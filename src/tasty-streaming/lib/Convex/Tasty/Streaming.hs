module Convex.Tasty.Streaming (
  streamingJsonReporter,
  listTestsJsonIngredient,
  streamingIngredients,
  defaultMainStreaming,
  defaultMainStreamingWithIngredients,
) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Monad (unless, when)
import Convex.Tasty.Streaming.QCStats (
  QCStatsRecorder,
  QCStatsStoreOption (..),
  lookupQCStatsByTestInfo,
  newQCStatsStore,
  storeQCStatsRecorder,
 )
import Convex.Tasty.Streaming.SrcLoc (PackageRootOpt (..), callerPackageRoot)
import Convex.Tasty.Streaming.TMSummary (
  CoverageIndexStorage (..),
  TMRecorder,
  TMStoreOption (..),
  TraceRecorder (..),
  lookupThreatModelSummary,
  newTMStore,
  storeRecorder,
 )
import Convex.Tasty.Streaming.TreeMap (buildTestMap)
import Convex.Tasty.Streaming.Types
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tagged (Tagged (..))
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hFlush, hPutStrLn, hSetBuffering, stderr, stdout)
import Test.Tasty (defaultMainWithIngredients, localOption)
import Test.Tasty.Ingredients (Ingredient (..))
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), lookupOption, mkFlagCLParser, safeRead)
import Test.Tasty.Runners (
  FailureReason (..),
  Outcome (..),
  Progress (..),
  Result (..),
  Status (..),
  TestTree (..),
  listingTests,
  parseOptions,
 )

-- | Command-line option to enable streaming JSON output
newtype StreamingJson = StreamingJson Bool
  deriving (Eq, Ord, Typeable)

instance IsOption StreamingJson where
  defaultValue = StreamingJson False
  parseValue = fmap StreamingJson . safeRead
  optionName = Tagged "streaming-json"
  optionHelp = Tagged "Enable streaming NDJSON test output to stdout"
  optionCLParser = mkFlagCLParser mempty (StreamingJson True)

-- | Command-line option to disable iteration trace collection
newtype NoTrace = NoTrace Bool
  deriving (Eq, Ord, Typeable)

instance IsOption NoTrace where
  defaultValue = NoTrace False
  parseValue = fmap NoTrace . safeRead
  optionName = Tagged "no-trace"
  optionHelp = Tagged "Disable iteration trace collection (only effective with --streaming-json)"
  optionCLParser = mkFlagCLParser mempty (NoTrace True)

-- | Command-line option to list tests as JSON without running them
newtype ListTestsJson = ListTestsJson Bool
  deriving (Eq, Ord, Typeable)

instance IsOption ListTestsJson where
  defaultValue = ListTestsJson False
  parseValue = fmap ListTestsJson . safeRead
  optionName = Tagged "list-tests-json"
  optionHelp = Tagged "List all tests as a JSON object and exit without running"
  optionCLParser = mkFlagCLParser mempty (ListTestsJson True)

-- | Command-line option to run only tests whose Tasty IDs are selected.
newtype TestIdFilter = TestIdFilter [Int]
  deriving (Eq, Ord, Typeable)

instance Monoid TestIdFilter where
  mempty = TestIdFilter []

instance Semigroup TestIdFilter where
  TestIdFilter a <> TestIdFilter b = TestIdFilter (a <> b)

instance IsOption TestIdFilter where
  defaultValue = TestIdFilter []
  parseValue raw = TestIdFilter <$> parseTestIds raw
   where
    parseTestIds s =
      let tokens = map trim (splitOn ',' s)
          nonEmpty = filter (not . null) tokens
       in traverse parseOne nonEmpty
    parseOne token =
      case safeRead token :: Maybe Int of
        Just n | n >= 0 -> Just n
        _ -> Nothing
    splitOn _ [] = []
    splitOn delim s = case break (== delim) s of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn delim rest
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
  optionName = Tagged "test-id"
  optionHelp = Tagged "Run only tests whose Tasty IDs match these values (comma-separated); discover IDs with --list-tests-json"

-- | Internal option carrying filtered->original test ID remapping.
newtype TestIdRemap = TestIdRemap (Maybe (IntMap Int))

instance IsOption TestIdRemap where
  defaultValue = TestIdRemap Nothing
  parseValue = const Nothing
  optionName = Tagged "test-id-remap"
  optionHelp = Tagged "internal: filtered-to-original test id remapping"

{- | Internal option carrying a shared 'IORef Bool' that is set to 'True' by
the streaming reporter when @--streaming-json@ is active.  The
'TraceRecorder' callback checks this before emitting any events.
-}
newtype StreamingEnabledRef = StreamingEnabledRef (Maybe (IORef Bool))

instance IsOption StreamingEnabledRef where
  defaultValue = StreamingEnabledRef Nothing
  parseValue = const Nothing
  optionName = Tagged "streaming-enabled-ref"
  optionHelp = Tagged "internal: streaming enabled flag"

{- | Internal option carrying a shared 'MVar ()' so the reporter and the
'TraceRecorder' use the same output lock, preventing interleaved NDJSON lines.
-}
newtype OutputLockRef = OutputLockRef (Maybe (MVar ()))

instance IsOption OutputLockRef where
  defaultValue = OutputLockRef Nothing
  parseValue = const Nothing
  optionName = Tagged "output-lock-ref"
  optionHelp = Tagged "internal: shared output lock"

{- | Internal option carrying a shared 'IORef' so the reporter can publish the
test map and the 'TraceRecorder' can read it back to resolve test IDs.
-}
newtype TestMapRef = TestMapRef (Maybe (IORef (IntMap TestInfo)))

instance IsOption TestMapRef where
  defaultValue = TestMapRef Nothing
  parseValue = const Nothing
  optionName = Tagged "test-map-ref"
  optionHelp = Tagged "internal: shared test map reference"

{- | The streaming JSON reporter ingredient.

When activated via @--streaming-json@, replaces console output with
newline-delimited JSON events streamed to stdout.
-}
streamingJsonReporter :: Ingredient
streamingJsonReporter = TestReporter
  [ Option (Proxy :: Proxy StreamingJson)
  , Option (Proxy :: Proxy NoTrace)
  , Option (Proxy :: Proxy QCStatsStoreOption)
  , Option (Proxy :: Proxy QCStatsRecorder)
  , Option (Proxy :: Proxy TestIdRemap)
  , Option (Proxy :: Proxy TMStoreOption)
  , Option (Proxy :: Proxy TMRecorder)
  , Option (Proxy :: Proxy TraceRecorder)
  , Option (Proxy :: Proxy CoverageIndexStorage)
  , Option (Proxy :: Proxy TestMapRef)
  , Option (Proxy :: Proxy StreamingEnabledRef)
  , Option (Proxy :: Proxy OutputLockRef)
  , Option (Proxy :: Proxy PackageRootOpt)
  ]
  $ \opts tree -> do
    let StreamingJson enabled = lookupOption opts
    if not enabled
      then Nothing
      else Just $ \statusMap -> do
        let TMStoreOption mStore = lookupOption opts
            QCStatsStoreOption mQCStatsStore = lookupOption opts
            TestMapRef mTestMapRef = lookupOption opts
            StreamingEnabledRef mEnabledRef = lookupOption opts
            CoverageIndexStorage coverageIndex = lookupOption opts

        -- Signal that streaming is active so the TraceRecorder callback
        -- (which checks the same IORef) actually emits events.
        -- When --no-trace is passed, leave the ref as False so that both
        -- trEnabled and recordIteration remain no-ops.
        let NoTrace noTrace = lookupOption opts
        case mEnabledRef of
          Just ref -> writeIORef ref (not noTrace)
          Nothing -> pure ()

        -- Set line buffering for streaming
        hSetBuffering stdout LineBuffering

        -- Use the shared output lock if provided, otherwise create a new one
        -- (backward compatibility when the reporter is used without
        -- defaultMainStreaming).
        let OutputLockRef mSharedLock = lookupOption opts
        outputLock <- maybe (newMVar ()) pure mSharedLock
        let emit evt = withMVar outputLock $ \_ -> emitEvent evt

        -- Build the test index -> metadata map
        let TestIdRemap mRemap = lookupOption opts
            remapId i = maybe i (IntMap.findWithDefault i i) mRemap
        testMap <- buildTestMap opts remapId tree

        -- Populate the shared test map ref so TraceRecorder can resolve IDs
        case mTestMapRef of
          Just ref -> writeIORef ref testMap
          Nothing -> pure ()

        -- Read the package root from the option set (populated by
        -- 'defaultMainStreaming' from the caller's 'HasCallStack'). Emitted
        -- once on SuiteStarted so consumers can resolve package-relative
        -- 'srcLoc.file' paths to absolute filesystem locations.
        let PackageRootOpt mPkgRoot = lookupOption opts

        -- Emit suite_started with full test list
        let testInfos = snd <$> IntMap.toAscList testMap
        emit $ SuiteStarted mPkgRoot testInfos coverageIndex

        -- Track results for final summary
        resultsVar <- newTVarIO ([] :: [(Int, Result)])

        -- Watch each test concurrently
        forConcurrently_ (IntMap.toAscList statusMap) $ \(idx, statusTVar) -> do
          -- Wait until the test starts
          atomically $ do
            status <- readTVar statusTVar
            case status of
              NotStarted -> retry
              _ -> pure ()

          -- Emit test_started
          emit $ TestStarted (remapId idx)

          -- Wait for completion, emitting progress events along the way
          let waitLoop lastSeen = do
                next <- atomically $ do
                  status <- readTVar statusTVar
                  case status of
                    NotStarted -> retry
                    Executing p ->
                      let cur = (progressText p, progressPercent p)
                       in if Just cur == lastSeen
                            then retry
                            else pure (Left p)
                    Done r -> pure (Right r)
                case next of
                  Left p -> do
                    emit $
                      TestProgress
                        { epId = remapId idx
                        , epMessage = Text.pack (progressText p)
                        , epPercent = progressPercent p
                        }
                    waitLoop (Just (progressText p, progressPercent p))
                  Right r -> pure r
          result <- waitLoop Nothing

          -- Record result
          atomically $ modifyTVar' resultsVar ((idx, result) :)

          -- Look up structured threat-model summary by "<group>/<name>" key
          let testInfo = IntMap.lookup idx testMap
              key = case testInfo of
                Just ti
                  | (parent : _) <- reverse (tiPath ti) ->
                      Text.unpack parent <> "/" <> Text.unpack (tiName ti)
                Just ti -> Text.unpack (tiName ti)
                Nothing -> ""
          mSummary <- case mStore of
            Just store -> lookupThreatModelSummary store key
            Nothing -> pure Nothing
          mMonitoring <- case (mQCStatsStore, testInfo) of
            (Just store, Just ti) -> lookupQCStatsByTestInfo store ti
            _ -> pure Nothing

          -- Emit test_done
          let outcome = case resultOutcome result of
                Success -> TestSuccess
                Failure reason ->
                  TestFailure $
                    FailureInfo
                      { fiReason = Text.pack $ showFailureReason reason
                      , fiMessage = Text.pack (resultDescription result)
                      }
          emit $
            TestDone
              { edId = remapId idx
              , edOutcome = outcome
              , edDuration = resultTime result
              , edDescription = Text.pack (resultDescription result)
              , edThreatModel = mSummary
              , edMonitoringStats = mMonitoring
              }

        -- Emit suite_done summary
        allResults <- readTVarIO resultsVar
        let passed = length [() | (_, r) <- allResults, isSuccess r]
        let failed = length allResults - passed

        -- Return the "finalize" callback
        pure $ \totalTime -> do
          emit $ SuiteDone passed failed totalTime
          pure (failed == 0)

-- | Emit a single NDJSON event line to stdout
emitEvent :: Event -> IO ()
emitEvent evt = do
  BL8.putStrLn (encode evt)
  hFlush stdout

-- | Check if a Result is a success
isSuccess :: Result -> Bool
isSuccess r = case resultOutcome r of
  Success -> True
  _ -> False

-- | Show a FailureReason as text
showFailureReason :: FailureReason -> String
showFailureReason TestFailed = "TestFailed"
showFailureReason (TestThrewException e) = "TestThrewException: " ++ show e
showFailureReason (TestTimedOut n) = "TestTimedOut: " ++ show n ++ "μs"
showFailureReason TestDepFailed = "TestDepFailed"

{- | Find the Tasty test ID for a test identified by group name and category.
Searches the test map for a 'TestInfo' whose path contains the group name
and whose name matches the category (e.g. \"Positive tests\", \"Negative tests\").
Returns @-1@ as a fallback when the test is not found.
-}
findTestId :: IntMap TestInfo -> String -> String -> Maybe Int
findTestId testMap group category =
  let categoryName = case category of
        "positive" -> "Positive tests"
        "negative" -> "Negative tests"
        other -> other
      matches =
        IntMap.toList $
          IntMap.filter
            ( \ti ->
                Text.pack group `elem` tiPath ti
                  && tiName ti == Text.pack categoryName
            )
            testMap
   in case matches of
        ((testId, _) : _) -> Just testId
        [] -> Nothing

{- | Ingredient that lists the test tree as JSON and exits without running tests.

Activated via @--list-tests-json@.
-}
listTestsJsonIngredient :: Ingredient
listTestsJsonIngredient = TestManager
  [ Option (Proxy :: Proxy ListTestsJson)
  , Option (Proxy :: Proxy TestIdRemap)
  , Option (Proxy :: Proxy PackageRootOpt)
  , Option (Proxy :: Proxy CoverageIndexStorage)
  ]
  $ \opts tree -> do
    let ListTestsJson enabled = lookupOption opts
        CoverageIndexStorage coverageIndex = lookupOption opts
    if not enabled
      then Nothing
      else Just $ do
        hSetBuffering stdout LineBuffering
        let PackageRootOpt mPkgRoot = lookupOption opts
            TestIdRemap mRemap = lookupOption opts
            remapId i = maybe i (IntMap.findWithDefault i i) mRemap
        testMap <- buildTestMap opts remapId tree
        let testInfos = snd <$> IntMap.toAscList testMap
        emitEvent $ SuiteStarted mPkgRoot testInfos coverageIndex
        pure True

-- | Option ingredient for selecting a subset of tests by Tasty ID.
testIdFilterIngredient :: Ingredient
testIdFilterIngredient =
  TestManager
    [Option (Proxy :: Proxy TestIdFilter)]
    (\_ _ -> Nothing)

-- | Default ingredients with streaming reporter added
streamingIngredients :: [Ingredient]
streamingIngredients = [listingTests, testIdFilterIngredient, listTestsJsonIngredient, streamingJsonReporter, consoleTestReporter]

filterTreeByPaths :: Set [String] -> TestTree -> Maybe TestTree
filterTreeByPaths selected = go []
 where
  go path tree = case tree of
    SingleTest name _t ->
      let fullPath = path <> [name]
       in if fullPath `Set.member` selected
            then Just tree
            else Nothing
    TestGroup name children ->
      let childPath = path <> [name]
          keptChildren = mapMaybe (go childPath) children
       in if null keptChildren
            then Nothing
            else Just (TestGroup name keptChildren)
    PlusTestOptions f subtree ->
      fmap (PlusTestOptions f) (go path subtree)
    WithResource spec mkTree ->
      Just (WithResource spec (\ioRes -> maybe (TestGroup "filtered-out" []) id (go path (mkTree ioRes))))
    AskOptions k ->
      Just (AskOptions (\opts -> maybe (TestGroup "filtered-out" []) id (go path (k opts))))
    After dep expr subtree ->
      fmap (After dep expr) (go path subtree)

expandSelectedTestIds :: IntMap TestInfo -> IntSet.IntSet -> IntSet.IntSet
expandSelectedTestIds testMap selectedIds =
  IntSet.union selectedIds prerequisiteIds
 where
  pathToId =
    Map.fromList
      [ (tiPath ti <> [tiName ti], tiId ti)
      | ti <- IntMap.elems testMap
      ]
  prerequisiteIds =
    IntSet.fromList
      [ positiveId
      | selectedId <- IntSet.toList selectedIds
      , Just positiveId <- [positiveTestIdFor selectedId]
      ]
  positiveTestIdFor selectedId = do
    ti <- IntMap.lookup selectedId testMap
    case tiPath ti of
      [] -> Nothing
      groupPath
        | last groupPath == Text.pack "Threat models"
            || last groupPath == Text.pack "Expected vulnerabilities" ->
            Map.lookup (init groupPath <> [Text.pack "Positive tests"]) pathToId
        | otherwise -> Nothing

{- | Drop-in replacement for 'defaultMain' that supports @--streaming-json@.

If you bypass this entry point and wire 'streamingIngredients' manually,
threat-model summaries will not appear in the JSON output unless you
also call
@'localOption' ('TMStoreOption' (Just store)) . 'localOption' ('storeRecorder' store)@
on your tree (with a freshly-allocated store from 'newTMStore').

== Package root capture

The @packageRoot@ field emitted on the @SuiteStarted@ event is captured
from the call site of 'defaultMainStreaming' (typically the user's
@Main.hs@) via 'HasCallStack'. The mechanism is implemented in
'Convex.Tasty.Streaming.SrcLoc.callerPackageRoot': read the top of
'callStack' to obtain both the GHC package identifier of the calling
module and the file path it was compiled from, then search the workspace
(starting at the process working directory) for a directory containing
both a matching @\<pkgname\>.cabal@ file and the relative source file.

This is correct in the common case where the @Main.hs@ entry point and
the tests it assembles live in the same cabal package. It is __not__
correct for cross-package test reuse — for example, if @Main.hs@ in
package /A/ pulls in test trees defined in package /B/'s library, those
tests will be attributed to package /A/, not /B/. We explicitly accept
this limitation; addressing it would require per-test source-location
metadata (e.g. via Template Haskell or CPP at every test definition
site), which is significantly more invasive.

If the resolution fails (e.g. the test is launched from a working
directory that does not contain the package, or the package name cannot
be extracted), @packageRoot@ is omitted from the JSON output (consistent
with the existing @Maybe@-as-absent-key convention).
-}
defaultMainStreaming :: (HasCallStack) => TestTree -> IO ()
defaultMainStreaming = defaultMainStreamingWithIngredients []

{- | Variant of 'defaultMainStreaming' that allows callers to prepend
additional ingredients (e.g. package-specific CLI option managers).

The same internal streaming wiring is always installed (threat-model
summary store, trace recorder, shared output lock, and package root
capture from call-site), then Tasty runs with:

@extraIngredients <> streamingIngredients@
-}
defaultMainStreamingWithIngredients :: (HasCallStack) => [Ingredient] -> TestTree -> IO ()
defaultMainStreamingWithIngredients extraIngredients tree = do
  -- Capture the package root from the user's call site BEFORE doing any
  -- other work. 'withFrozenCallStack' freezes our caller's call stack so
  -- that inside 'callerPackageRoot' the top frame is the user's Main.hs
  -- (the call site of 'defaultMainStreaming') rather than this internal
  -- invocation of 'callerPackageRoot'.
  mPkgRoot <- withFrozenCallStack callerPackageRoot
  let pkgRootOpt = PackageRootOpt (fmap Text.pack mPkgRoot)
  store <- newTMStore
  qcStatsStore <- newQCStatsStore
  testMapRef <- newIORef IntMap.empty
  testIdRemapRef <- newIORef IntMap.empty
  enabledRef <- newIORef False -- set to True by the reporter when --streaming-json is active
  -- Create a single shared output lock used by both the streaming reporter
  -- and the TraceRecorder so their NDJSON lines never interleave.
  outputLock <- newMVar ()
  -- Create a trace recorder that emits TestTrace events as NDJSON to stdout.
  -- The recorder reads the shared testMapRef (populated by the reporter at
  -- startup) to resolve the numeric Tasty test ID for each trace event.
  --
  -- Both 'trEnabled' and 'recordIteration' read the shared 'enabledRef',
  -- so when --streaming-json is NOT passed (or --no-trace is passed) the
  -- ref stays False: test bodies take the fast path and no NDJSON lines
  -- go to stdout.
  let traceRec =
        TraceRecorder
          { trEnabled = readIORef enabledRef
          , recordIteration = \group category covered iterationJson -> do
              enabled <- readIORef enabledRef
              when enabled $ do
                testMap <- readIORef testMapRef
                let testId = findTestId testMap group category
                withMVar outputLock $ \_ ->
                  emitEvent $
                    TestTrace
                      { ettTestId = fromMaybe (-1) testId
                      , ettCategory = Text.pack category
                      , ettTrace = iterationJson
                      , ettCovered = covered
                      }
          , findTestIdIO = \group category -> do
              testMap <- readIORef testMapRef
              pure $ findTestId testMap group category
          }
  let baseTree =
        localOption pkgRootOpt $
          localOption (QCStatsStoreOption (Just qcStatsStore)) $
            localOption (storeQCStatsRecorder qcStatsStore) $
              localOption (TMStoreOption (Just store)) $
                localOption (storeRecorder store) $
                  localOption (TestMapRef (Just testMapRef)) $
                    localOption (StreamingEnabledRef (Just enabledRef)) $
                      localOption (OutputLockRef (Just outputLock)) $
                        localOption traceRec tree

  opts <- parseOptions (extraIngredients <> streamingIngredients) baseTree
  let TestIdFilter requested = lookupOption opts
      requestedIdSet = IntSet.fromList requested

  tree' <-
    if IntSet.null requestedIdSet
      then pure baseTree
      else do
        fullMap <- buildTestMap opts id baseTree
        let unknown = IntSet.toAscList $ IntSet.difference requestedIdSet (IntSet.fromList (IntMap.keys fullMap))
        unless (null unknown) $ do
          hPutStrLn stderr $ "Unknown test id(s) for --test-id: " <> show unknown
          hPutStrLn stderr "Use --list-tests-json to discover valid test IDs for this suite."
          exitFailure

        let selectedIdSet = expandSelectedTestIds fullMap requestedIdSet
            selectedIds = IntSet.toAscList selectedIdSet
            selectedInfos = map (fullMap IntMap.!) selectedIds
            selectedPaths =
              Set.fromList
                [ map Text.unpack (tiPath ti <> [tiName ti])
                | ti <- selectedInfos
                ]
            remap = IntMap.fromAscList (zip [0 ..] selectedIds)

        writeIORef testIdRemapRef remap

        case filterTreeByPaths selectedPaths baseTree of
          Nothing -> do
            hPutStrLn stderr "No tests remained after applying --test-id selection."
            exitFailure
          Just filteredTree ->
            pure $ localOption (TestIdRemap (Just remap)) filteredTree

  defaultMainWithIngredients (extraIngredients <> streamingIngredients) tree'
