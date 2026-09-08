{-# LANGUAGE OverloadedStrings #-}

{- | Constructing and running the @cabal test@ invocations that back every
@pbt-cli@ command.

Two details here are load-bearing.

First, custom test options are passed with repeated singular
@--test-option=@ flags, never the plural @--test-options=@. Cabal splits the
plural form on whitespace, so a Tasty pattern containing a space — which is
the common case, since Tasty group names have spaces — would arrive at the
test binary as several mangled arguments. The singular form appends one
argument verbatim.

Second, nothing here goes through a shell. Arguments are handed to the process
as a list, so patterns, redeemer names and paths need no quoting and cannot be
re-interpreted. 'renderCommand' exists only to *show* a copy-pasteable
equivalent for @--dry-run@ and error messages.
-}
module PbtCli.Cabal (
  -- * Invocations
  Invocation (..),
  testInvocation,
  renderCommand,

  -- * Test options
  TestOptions (..),
  noTestOptions,
  testOptionArgs,
  wantsStructuredOutput,

  -- * Running
  runInherit,
  runCapture,
  runStreaming,

  -- * Environment
  findCabal,
  CabalMissing (..),
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString.Char8 qualified as BS8
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (catMaybes)
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (LineBuffering), hIsEOF, hSetBuffering)
import System.Process (
  CreateProcess (..),
  StdStream (CreatePipe, Inherit),
  proc,
  waitForProcess,
  withCreateProcess,
 )

-- | A resolved @cabal@ command line, ready to run.
data Invocation = Invocation
  { invProgram :: FilePath
  -- ^ absolute path to @cabal@.
  , invArgs :: [String]
  , invWorkingDir :: Maybe FilePath
  }
  deriving (Eq, Show)

{- | Thrown when @cabal@ is not on @PATH@; @pbt-cli@ is a wrapper, not a build
system, so it cannot do anything useful without it.
-}
newtype CabalMissing = CabalMissing String
  deriving (Show)

instance Exception CabalMissing

-- | Locate @cabal@, or fail with an actionable message.
findCabal :: IO FilePath
findCabal =
  findExecutable "cabal" >>= \case
    Just p -> pure p
    Nothing ->
      throwIO . CabalMissing $
        "cabal was not found on PATH. pbt-cli drives `cabal test`, so it needs a "
          <> "cabal installation (or a `nix develop` shell) to do anything beyond "
          <> "`pbt-cli suites`, which is purely static."

-- | The custom test options @pbt-cli@ knows how to forward to a suite.
data TestOptions = TestOptions
  { toStreamingJson :: Bool
  -- ^ @--streaming-json@: real-time NDJSON instead of console output.
  , toListTestsJson :: Bool
  -- ^ @--list-tests-json@: dump the test tree and exit without running.
  , toListThreatModels :: Bool
  -- ^ @--list-threat-models-json@: dump the threat models and exit.
  , toPattern :: Maybe String
  -- ^ Tasty's @-p@ pattern.
  , toTestIds :: Maybe String
  -- ^ @--test-id@, comma-separated ids from a previous discovery.
  , toThreatModelName :: Maybe String
  -- ^ @--threat-model-name@, comma-separated name prefixes.
  , toExtra :: [String]
  -- ^ anything else, passed through one argument at a time.
  }
  deriving (Eq, Show)

-- | No custom options: run the suite exactly as @cabal test@ would.
noTestOptions :: TestOptions
noTestOptions =
  TestOptions
    { toStreamingJson = False
    , toListTestsJson = False
    , toListThreatModels = False
    , toPattern = Nothing
    , toTestIds = Nothing
    , toThreatModelName = Nothing
    , toExtra = []
    }

{- | Render test options as @--test-option=@ flags.

Flags that take a value contribute two arguments (@--test-option=-p@ then
@--test-option=\<value\>@) because that is how Tasty's own parser reads them,
and it keeps values with spaces intact.
-}
testOptionArgs :: TestOptions -> [String]
testOptionArgs to =
  concat
    [ [one "--streaming-json" | toStreamingJson to]
    , [one "--list-tests-json" | toListTestsJson to]
    , [one "--list-threat-models-json" | toListThreatModels to]
    , pair "-p" (toPattern to)
    , pair "--test-id" (toTestIds to)
    , pair "--threat-model-name" (toThreatModelName to)
    , map one (toExtra to)
    ]
 where
  one v = "--test-option=" <> v
  pair flag = maybe [] (\v -> [one flag, one v])

{- | Does this invocation need the suite's stdout on our pipe?

True for every mode that parses NDJSON off it. Used to decide whether to
override the project's @test-show-details@.
-}
wantsStructuredOutput :: TestOptions -> Bool
wantsStructuredOutput to =
  toStreamingJson to || toListTestsJson to || toListThreatModels to

{- | Build a @cabal test@ invocation.

@targets@ are suite names; passing several is what lets @pbt-cli run@ launch a
whole project's suites in one cabal call. An empty target list means @all@,
cabal's own everything-target.

For the structured modes this forces @--test-show-details=direct@. Cabal's
default streams the suite's stdout through, but a target repository is free to
set @test-show-details: failures@ or @never@ in its @cabal.project@ (or
@cabal.project.local@, or @~\/.cabal\/config@), and then cabal captures that
stdout into a log under @dist-newstyle@ and our pipe receives nothing at all --
so @run --json@ would exit 0 having emitted no events. pbt-cli owns the pipe,
so it owns the setting; a later flag on the command line wins over the project
file, which makes this a safe unconditional override.

@direct@ rather than @streaming@: both forward the suite's stdout, but
@streaming@ adds cabal's own per-test decoration, and the NDJSON modes want the
suite's bytes and nothing else. Plain @run@ is left alone -- it is a
pass-through of cabal's console output, so the project's own preference is the
right one there.
-}
testInvocation
  :: FilePath
  -- ^ @cabal@ executable
  -> FilePath
  -- ^ working directory (the repository root)
  -> Maybe FilePath
  -- ^ @--project-file@, when the suites are not in the default project
  -> [String]
  -- ^ @cabal test@ targets
  -> TestOptions
  -> Invocation
testInvocation cabal root projectFile targets to =
  Invocation
    { invProgram = cabal
    , invArgs =
        ["test"]
          <> (if null targets then ["all"] else targets)
          <> catMaybes [("--project-file=" <>) <$> projectFile]
          <> ["--test-show-details=direct" | wantsStructuredOutput to]
          <> testOptionArgs to
    , invWorkingDir = Just root
    }

{- | A copy-pasteable shell rendering of an invocation.

For display only — the real invocation never touches a shell. Arguments are
single-quoted when they contain anything that a shell would treat specially.
-}
renderCommand :: Invocation -> String
renderCommand inv = unwords (map shellQuote ("cabal" : invArgs inv))
 where
  shellQuote s
    | all safe s && not (null s) = s
    | otherwise = "'" <> concatMap escapeQuote s <> "'"

  safe c = c `elem` ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789./_-=:," :: String)

  escapeQuote '\'' = "'\\''"
  escapeQuote c = [c]

{- | Run an invocation with the child's stdio connected straight to ours.

Used by @pbt-cli run@ so cabal's build progress and Tasty's console reporter
appear exactly as they would if the user had typed the cabal command.
-}
runInherit :: Invocation -> IO ExitCode
runInherit inv =
  withCreateProcess (processFor inv) $ \_ _ _ ph -> waitForProcess ph

{- | Run an invocation, collecting its stdout lines and letting stderr through.

stderr stays connected to ours on purpose: cabal reports build failures there,
and swallowing them would turn a compile error into a mystifying "no events"
result.
-}
runCapture :: Invocation -> IO (ExitCode, [BS8.ByteString])
runCapture inv = do
  ref <- newCollector
  code <- runStreaming inv (collect ref)
  ls <- takeCollected ref
  pure (code, ls)

{- | Run an invocation, handing each stdout line to a callback as it arrives.

The child's stdout is line-buffered and consumed incrementally, which is what
makes @pbt-cli stream@ actually live rather than a delayed dump at exit.
-}
runStreaming :: Invocation -> (BS8.ByteString -> IO ()) -> IO ExitCode
runStreaming inv onLine =
  withCreateProcess (processFor inv){std_out = CreatePipe} $ \_ mout _ ph ->
    case mout of
      Nothing -> waitForProcess ph
      Just h -> do
        hSetBuffering h LineBuffering
        pump h
        waitForProcess ph
 where
  pump h = do
    eof <- hIsEOF h
    if eof
      then pure ()
      else do
        l <- BS8.hGetLine h
        onLine l
        pump h

processFor :: Invocation -> CreateProcess
processFor inv =
  (proc (invProgram inv) (invArgs inv))
    { cwd = invWorkingDir inv
    , std_in = Inherit
    , std_err = Inherit
    }

-- A tiny append-only line collector, so runCapture can build its result
-- without leaning on lazy IO.
newtype Collector = Collector (IORef [BS8.ByteString])

newCollector :: IO Collector
newCollector = Collector <$> newIORef []

collect :: Collector -> BS8.ByteString -> IO ()
collect (Collector r) l = modifyIORef' r (l :)

takeCollected :: Collector -> IO [BS8.ByteString]
takeCollected (Collector r) = reverse <$> readIORef r
