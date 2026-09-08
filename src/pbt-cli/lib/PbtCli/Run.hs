{-# LANGUAGE OverloadedStrings #-}

{- | Implementations of the @pbt-cli@ commands.

Every command returns an 'ExitCode' rather than calling @exitWith@ itself, so
the exit-code contract lives in one place and is testable:

* @0@ — success
* @1@ — tests failed, or nothing matched (no suites, empty tree)
* @2@ — usage error: unknown suite, or a suite that cannot do what was asked
* @3@ — discovery failed (the root is not a readable directory)
* @4@ — @cabal@ could not be found or could not be started
* @5@ — some other I\/O failure

@5@ exists so that @1@ keeps meaning what it says. An unrelated I\/O error — a
write failure part-way through @suites --json@, an @EMFILE@ while forking
cabal — must not reach a CI consumer looking like a failing test run.
-}
module PbtCli.Run (
  execute,

  -- * Selection (exposed for testing)
  groupByProject,
  selectSuites,

  -- * Exit codes
  exitOk,
  exitFailed,
  exitUsage,
  exitDiscovery,
  exitCabal,
  exitIoError,
  classifyIoError,
) where

import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless)
import Data.Aeson (Value (..), decodeStrict')
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import PbtCli.Cabal (
  CabalMissing (..),
  Invocation,
  TestOptions (..),
  findCabal,
  noTestOptions,
  renderCommand,
  runCapture,
  runInherit,
  runStreaming,
  testInvocation,
 )
import PbtCli.Discover (
  Discovery (..),
  SuiteRef (..),
  TestSuite (..),
  compatibleOnly,
  discover,
  entryPointText,
  flattenSuites,
  isCompatible,
 )
import PbtCli.Doctor (doctor)
import PbtCli.Events (Event (..), decodeEvent, eventsFrom, isJsonObjectLine)
import PbtCli.Options (
  Command (..),
  DoctorOpts (..),
  Filters (..),
  RunOpts (..),
  RunOutput (..),
  SuitesFormat (..),
  SuitesOpts (..),
  TestsFormat (..),
  TestsOpts (..),
  ThreatModelsOpts (..),
 )
import PbtCli.Render (
  encodeJsonCompact,
  encodeJsonPretty,
  renderEventWith,
  renderSuiteBanner,
  renderSuiteNames,
  renderSuitesTable,
  renderSuitesTsv,
  renderTestList,
  renderTestTree,
  tagEventWithSuite,
  testNameIndex,
 )
import System.Directory (doesDirectoryExist, makeAbsolute)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError, isPermissionError, isResourceVanishedError)

exitOk, exitFailed, exitUsage, exitDiscovery, exitCabal, exitIoError :: ExitCode
exitOk = ExitSuccess
exitFailed = ExitFailure 1
exitUsage = ExitFailure 2
exitDiscovery = ExitFailure 3
exitCabal = ExitFailure 4
exitIoError = ExitFailure 5

{- | Which exit code an 'IOException' deserves.

Shared with @main@, which classifies a failure of its own final
@hFlush stdout@ the same way -- otherwise a write error would be reported with
one code inside 'execute' and another at shutdown.

* A vanished resource means stdout closed under us: the consumer of a pipe
  stopped reading, which is what @pbt-cli suites | head@ does. It got what it
  asked for, so exit quietly.
* A missing or unreadable file is the one class that can mean cabal itself
  could not be started, so it keeps cabal's code.
* Everything else gets its own code. Blaming cabal was actively misleading --
  an encoding error while rendering a table used to be reported as "could not
  run cabal" -- but so is exit 1, which the contract reserves for a failing
  test run.
-}
classifyIoError :: IOException -> ExitCode
classifyIoError e
  | isResourceVanishedError e = exitOk
  | isDoesNotExistError e || isPermissionError e = exitCabal
  | otherwise = exitIoError

{- | Dispatch a parsed command.

A missing @cabal@ is caught here rather than per command: it is the one
failure every non-static command shares, and it deserves the same message and
exit code wherever it surfaces.
-}
execute :: Command -> IO ExitCode
execute cmd = run `catch` onCabalMissing `catch` onIOError
 where
  run = case cmd of
    Suites o -> doSuites o
    Tests o -> doTests o
    Run o -> doRun o
    ThreatModels o -> doThreatModels o
    Doctor o -> doctor (dcoRoot o)

  onCabalMissing (CabalMissing msg) = do
    hPutStrLn stderr ("pbt-cli: " <> msg)
    pure exitCabal

  onIOError (e :: IOException) = do
    let code = classifyIoError e
    if code == exitOk
      then pure ()
      else
        hPutStrLn stderr $
          if code == exitCabal
            then "pbt-cli: could not run cabal: " <> show e
            else "pbt-cli: " <> show e
    pure code

-- ---------------------------------------------------------------------------
-- suites
-- ---------------------------------------------------------------------------

doSuites :: SuitesOpts -> IO ExitCode
doSuites o =
  withDiscovery (suoRoot o) $ \d0 -> do
    let d = if suoCompatibleOnly o then compatibleOnly d0 else d0
    case suoFormat o of
      SuitesList -> TextIO.putStr (renderSuiteNames d)
      SuitesJson -> LBS8.putStr (encodeJsonPretty d)
      SuitesJsonCompact -> LBS8.putStrLn (encodeJsonCompact d)
      SuitesTable -> TextIO.putStr (renderSuitesTable d)
      SuitesTsv -> TextIO.putStr (renderSuitesTsv d)
    -- Same contract as list-test-suites.sh: a readable root with nothing to
    -- report is exit 1, distinct from a bad root (exit 3).
    pure (if null (flattenSuites d) then exitFailed else exitOk)

-- ---------------------------------------------------------------------------
-- tests
-- ---------------------------------------------------------------------------

doTests :: TestsOpts -> IO ExitCode
doTests o =
  withStreamingSuite (tsoRoot o) (tsoSuite o) $ \root sr -> do
    inv <-
      invocationFor
        root
        sr
        (filterOptions (tsoFilters o)){toListTestsJson = True}
    if tsoDryRun o
      then dryRun [inv]
      else do
        (code, ls) <- runCapture inv
        case [e | e@EventSuiteStarted{} <- eventsFrom ls] of
          (EventSuiteStarted tests raw : _) -> do
            case tsoFormat o of
              TestsList -> TextIO.putStr (renderTestList tests)
              TestsJson -> LBS8.putStr (encodeJsonPretty raw)
              TestsTree -> TextIO.putStr (renderTestTree tests)
            pure (if null tests then exitFailed else exitOk)
          _ -> do
            hPutStrLn stderr $
              "pbt-cli: "
                <> Text.unpack (tsName (srSuite sr))
                <> " produced no test tree. The suite is marked as streaming-capable, so "
                <> "this usually means it failed to build — cabal's output above says why."
            pure (worstOf code exitFailed)

-- ---------------------------------------------------------------------------
-- run
-- ---------------------------------------------------------------------------

doRun :: RunOpts -> IO ExitCode
doRun o =
  withDiscovery (rnoRoot o) $ \d0 -> do
    let d = if rnoCompatibleOnly o then compatibleOnly d0 else d0
        available = flattenSuites d
    case selectSuites available (rnoSuites o) of
      Left err -> usageError err available
      Right [] -> do
        hPutStrLn stderr "pbt-cli: no test suites found."
        pure exitFailed
      Right selected -> case incompatible selected of
        -- --stream and --json need the --streaming-json ingredient, which only
        -- convex-tasty-streaming provides. Refusing here beats running an
        -- upstream-tasty suite that would ignore the flag, print its usual
        -- console output and exit 0 -- leaving a consumer waiting for events
        -- that never come.
        (bad : more)
          | rnoOutput o /= RunConsole ->
              incompatibleError (bad : more)
        _ -> do
          root <- makeAbsolute (rnoRoot o)
          cabal <- findCabal
          case rnoOutput o of
            RunConsole -> consoleRun root cabal selected
            RunStream -> eventRun root cabal selected (Pretty (length selected > 1))
            RunJson -> eventRun root cabal selected Ndjson
 where
  incompatible = filter (not . isCompatible . srSuite)

  incompatibleError bad = do
    hPutStrLn stderr $
      "pbt-cli: "
        <> (if rnoOutput o == RunJson then "--json" else "--stream")
        <> " needs sc-testing-tools compatible suites, but these are not:"
    forM_ bad $ \sr ->
      hPutStrLn stderr $
        "  "
          <> Text.unpack (tsName (srSuite sr))
          <> " (entry point: "
          <> Text.unpack (entryPointText (tsEntryPoint (srSuite sr)))
          <> ")"
    hPutStrLn stderr "Add --compatible-only to skip them, or drop the output flag."
    pure exitUsage

  -- \| Console mode: one @cabal test@ per project file, stdio inherited.
  --
  --  Grouping is what keeps a whole-repository run to a couple of cabal calls.
  --
  consoleRun root cabal selected = do
    let opts = filterOptions (rnoFilters o)
        invs =
          [ testInvocation cabal root projectFile (map suiteName srs) opts
          | (projectFile, srs) <- groupByProject selected
          ]
    if rnoDryRun o
      then dryRun invs
      else worst <$> mapM runInherit invs

  -- \| Event modes: one @cabal test@ per suite.
  --
  --  Suites cannot be grouped here. Cabal runs grouped targets sequentially and
  --  the events of each carry no suite identity, so a grouped run would produce
  --  several indistinguishable @suite_started@ blocks. One invocation per suite
  --  costs a little cabal overhead and buys correct attribution.
  --
  eventRun root cabal selected mode = do
    let opts = (filterOptions (rnoFilters o)){toStreamingJson = True}
        invs = [(sr, testInvocation cabal root (srProjectFile sr) [suiteName sr] opts) | sr <- selected]
    if rnoDryRun o
      then dryRun (map snd invs)
      else worst <$> mapM (streamOne mode) invs

  streamOne mode (sr, inv) = do
    let suite = tsName (srSuite sr)
    case mode of
      Pretty True -> TextIO.putStrLn (renderSuiteBanner suite)
      _ -> pure ()
    names <- newIORef IntMap.empty
    runStreaming inv (emit mode suite names)

  emit mode suite names line = case mode of
    Ndjson
      -- Forward the suite's NDJSON, minus cabal's own chatter, with the suite
      -- named on every event so a multi-suite run stays unambiguous.
      | isJsonObjectLine line -> case decodeStrict' line of
          Just v -> LBS8.putStrLn (encodeJsonCompact (tagEventWithSuite suite v))
          Nothing -> pure ()
      | otherwise -> pure ()
    Pretty _ -> case decodeEvent line of
      Right ev -> do
        -- The tree arrives once, in suite_started, and later test_done events
        -- reference it by id; remember it so a test whose provider sets no
        -- description can still be named. See 'testNameIndex'.
        case ev of
          EventSuiteStarted tests _ -> writeIORef names (testNameIndex tests)
          _ -> pure ()
        index <- readIORef names
        forM_ (renderEventWith (`IntMap.lookup` index) ev) TextIO.putStrLn
      Left _ -> pure ()

  suiteName = Text.unpack . tsName . srSuite

{- | How @run@ should present a suite's streaming events.

The flag on 'Pretty' says whether to print a banner naming each suite, which
is only worth the noise when more than one suite is being run.
-}
data EventMode = Pretty Bool | Ndjson
  deriving (Eq)

{- | Group suites by the project file that owns them, preserving first-
appearance order.

Grouping is what lets one @cabal test@ call cover a whole project's suites.
Suites are named explicitly rather than using cabal's @all@ target because
@cabal.project.schema-gen@ imports @cabal.project@, so @all@ under a variant
project file would re-run every suite in the repository.
-}
groupByProject :: [SuiteRef] -> [(Maybe FilePath, [SuiteRef])]
groupByProject srs =
  [(p, [sr | sr <- srs, srProjectFile sr == p]) | p <- projectsInOrder]
 where
  projectsInOrder = dedupe (map srProjectFile srs)
  dedupe = foldr (\x acc -> x : filter (/= x) acc) []

{- | Resolve the requested suite names against what was discovered.

An empty request means "everything", which is what makes @pbt-cli run@ with no
arguments the run-all-tests command.
-}
selectSuites :: [SuiteRef] -> [Text] -> Either String [SuiteRef]
selectSuites available [] = Right available
selectSuites available names = traverse look names
 where
  look n = case [sr | sr <- available, tsName (srSuite sr) == n] of
    (sr : _) -> Right sr
    [] -> Left ("unknown test suite: " <> Text.unpack n)

-- ---------------------------------------------------------------------------
-- threat-models
-- ---------------------------------------------------------------------------

doThreatModels :: ThreatModelsOpts -> IO ExitCode
doThreatModels o =
  withStreamingSuite (tmoRoot o) (tmoSuite o) $ \root sr -> do
    inv <- invocationFor root sr noTestOptions{toListThreatModels = True}
    if tmoDryRun o
      then dryRun [inv]
      else do
        (code, ls) <- runCapture inv
        case threatModelPayloads ls of
          (payload : _) -> do
            if tmoJson o
              then LBS8.putStr (encodeJsonPretty (Object payload))
              else case KeyMap.lookup "threatModels" payload of
                Just (Array names) -> forM_ names printName
                _ -> pure ()
            pure exitOk
          [] -> do
            hPutStrLn stderr $
              "pbt-cli: "
                <> Text.unpack (tsName (srSuite sr))
                <> " did not report any threat models. Suites get "
                <> "--list-threat-models-json from defaultMainTestingInterface; a suite using "
                <> "plain defaultMainStreaming does not have it."
            pure (worstOf code exitFailed)
 where
  printName = \case
    String t -> TextIO.putStrLn t
    other -> LBS8.putStrLn (encodeJsonCompact other)

-- | The @{"threatModels": [...]}@ objects among a run's stdout lines.
threatModelPayloads :: [BS8.ByteString] -> [KeyMap.KeyMap Value]
threatModelPayloads ls =
  [ km
  | l <- filter isJsonObjectLine ls
  , Just (Object km) <- [decodeStrict' l]
  , KeyMap.member "threatModels" km
  ]

-- ---------------------------------------------------------------------------
-- Shared plumbing
-- ---------------------------------------------------------------------------

{- | Discover @root@ and hand the result to @k@, or fail with exit code 3.

Discovery is checked up front for every command, including the ones that end
up shelling out to cabal, so a mistyped @--root@ is reported as such instead of
surfacing as a confusing cabal error.
-}
withDiscovery :: FilePath -> (Discovery -> IO ExitCode) -> IO ExitCode
withDiscovery root k = do
  ok <- doesDirectoryExist root
  if not ok
    then do
      hPutStrLn stderr ("pbt-cli: " <> root <> " is not a directory")
      pure exitDiscovery
    else discover root >>= k

{- | Resolve a suite name and require that it is sc-testing-tools compatible.

The three commands that need structured output (@tests@, @stream@,
@threat-models@) all depend on ingredients only @convex-tasty-streaming@
provides, so pointing them at an upstream-tasty suite can only fail. Failing
here, with the suite's actual classification in the message, beats letting
cabal run a suite that will ignore the flag and exit 0.
-}
withStreamingSuite :: FilePath -> Text -> (FilePath -> SuiteRef -> IO ExitCode) -> IO ExitCode
withStreamingSuite root name k =
  withDiscovery root $ \d -> do
    let available = flattenSuites d
    case [sr | sr <- available, tsName (srSuite sr) == name] of
      [] -> usageError ("unknown test suite: " <> Text.unpack name) available
      (sr : _)
        | not (isCompatible (srSuite sr)) -> do
            hPutStrLn stderr $
              "pbt-cli: "
                <> Text.unpack name
                <> " is not sc-testing-tools compatible (entry point: "
                <> Text.unpack (entryPointText (tsEntryPoint (srSuite sr)))
                <> "). Structured discovery and streaming come from "
                <> "convex-tasty-streaming; use 'pbt-cli run "
                <> Text.unpack name
                <> "' to run it as a plain tasty suite."
            pure exitUsage
        | otherwise -> do
            absRoot <- makeAbsolute root
            k absRoot sr

invocationFor :: FilePath -> SuiteRef -> TestOptions -> IO Invocation
invocationFor root sr opts = do
  cabal <- findCabal
  pure (testInvocation cabal root (srProjectFile sr) [Text.unpack (tsName (srSuite sr))] opts)

filterOptions :: Filters -> TestOptions
filterOptions f =
  noTestOptions
    { toPattern = fltPattern f
    , toTestIds = fltTestIds f
    , toThreatModelName = fltThreatModelName f
    , toExtra = fltTestOption f
    }

dryRun :: [Invocation] -> IO ExitCode
dryRun invs = do
  mapM_ (putStrLn . renderCommand) invs
  pure exitOk

usageError :: String -> [SuiteRef] -> IO ExitCode
usageError msg available = do
  hPutStrLn stderr ("pbt-cli: " <> msg)
  unless (null available) $ do
    hPutStrLn stderr "known test suites:"
    forM_ available $ \sr ->
      hPutStrLn stderr $
        "  "
          <> Text.unpack (tsName (srSuite sr))
          <> (if isCompatible (srSuite sr) then "  (sc-testing-tools compatible)" else "")
  pure exitUsage

-- | The most severe of a list of exit codes; success only if all succeeded.
worst :: [ExitCode] -> ExitCode
worst = foldr worstOf exitOk

worstOf :: ExitCode -> ExitCode -> ExitCode
worstOf ExitSuccess b = b
worstOf a _ = a
