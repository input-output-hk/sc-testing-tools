{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{- | Which tests the streamed data names.

Suites in different groups can share a group name (all the CTF suites call
theirs "property-based testing"). Every such suite's traces used to be
streamed under the first one's test ids, and its threat-model summaries
recorded under keys its namesakes' summaries overwrote.
-}
module StreamAttributionSpec (
  streamAttributionTests,
) where

import Control.Concurrent.STM (atomically, readTVar, retry)
import Convex.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Convex.Tasty.Streaming.TMSummary (
  ThreatModelSummary (..),
  TraceRecorder (..),
  lookupThreatModelSummary,
  negativePropertyName,
  newTMStore,
  positivePropertyName,
  storeRecorder,
  threatModelGroupName,
 )
import Convex.Tasty.Streaming.TreeMap (annotateGroupPaths, buildTestMap, findTestId, testPath)
import Convex.Tasty.Streaming.Types (TestInfo (..))
import Convex.TestingInterface (RunOptions (threatModelFilter), propRunActionsWithOptions)
import Data.Foldable (for_, traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import PingPongSpec (PingPongModel)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (..))
import Test.Tasty.Runners (Status (..), launchTestTree)

streamAttributionTests :: RunOptions -> TestTree
streamAttributionTests runOpts =
  testGroup
    "stream attribution"
    [ testCase "same-named suites trace under their own tests" (tracesNameTheirTests runOpts)
    , testCase "same-named suites record their own threat-model summaries" (summariesNameTheirTests runOpts)
    ]

{- | Two copies of a suite, with the same group name, in different groups.
Every PingPong model runs, whatever @--threat-model-name@ selects for the
suites around them.
-}
sameNamedSuites :: RunOptions -> TestTree
sameNamedSuites runOpts =
  testGroup
    "contracts"
    [testGroup c [propRunActionsWithOptions @PingPongModel "property-based testing" allModels] | c <- ["a", "b"]]
 where
  allModels = runOpts{threatModelFilter = []}

suitePaths :: [[String]]
suitePaths = [["contracts", c, "property-based testing"] | c <- ["a", "b"]]

-- | Each suite names its own tests, both for its iterations and for its threat models' entries.
tracesNameTheirTests :: RunOptions -> Assertion
tracesNameTheirTests runOpts = do
  iterations <- newIORef Set.empty
  modelLookups <- newIORef Set.empty
  let recorder =
        TraceRecorder
          { trEnabled = pure True
          , recordIteration = \path category _ _ -> insert iterations (path, category)
          , findTestIdIO = \path -> Nothing <$ insert modelLookups path
          }
      tree = annotateGroupPaths . localOption recorder . localOption (QuickCheckTests 2) $ sameNamedSuites runOpts
  testMap <- buildTestMap mempty id tree
  runQuietly tree

  recorded <- readIORef iterations
  recorded
    @?= Set.fromList
      [ (path <> [property], category)
      | path <- suitePaths
      , (property, category) <- [(positivePropertyName, "positive"), (negativePropertyName, "negative")]
      ]
  for_ recorded $ \(path, _) ->
    assertBool ("no property at " <> show path) (isJust (findTestId testMap path))

  looked <- readIORef modelLookups
  Set.map (take 3) looked @?= Set.fromList suitePaths
  for_ looked $ \path ->
    assertBool ("no threat-model test case at " <> show path) (isJust (findTestId testMap path))

{- | Each per-model case records its summary where the reporter looks it up
for that case, instead of where its namesake in the other suite records too.
-}
summariesNameTheirTests :: RunOptions -> Assertion
summariesNameTheirTests runOpts = do
  store <- newTMStore
  let tree = annotateGroupPaths . localOption (storeRecorder store) . localOption (QuickCheckTests 2) $ sameNamedSuites runOpts
  testMap <- buildTestMap mempty id tree
  runQuietly tree

  let categoryGroups = map threatModelGroupName [minBound .. maxBound]
      modelCases = [(ti, group) | ti <- IntMap.elems testMap, group : _ <- [reverse (map Text.unpack (tiPath ti))], group `elem` categoryGroups]
  Set.fromList [take 3 (testPath ti) | (ti, _) <- modelCases] @?= Set.fromList suitePaths
  for_ modelCases $ \(ti, group) -> do
    summary <- lookupThreatModelSummary store (testPath ti)
    fmap (\s -> (Text.unpack (tmsName s), threatModelGroupName (tmsCategory s))) summary
      @?= Just (Text.unpack (tiName ti), group)

insert :: (Ord a) => IORef (Set a) -> a -> IO ()
insert ref x = atomicModifyIORef' ref (\s -> (Set.insert x s, ()))

-- | Run a tree to completion, reporting nothing.
runQuietly :: TestTree -> IO ()
runQuietly tree =
  launchTestTree mempty tree $ \statusMap -> do
    atomically $ traverse_ awaitDone statusMap
    pure (\_ -> pure ())
 where
  awaitDone status =
    readTVar status >>= \case
      Done _ -> pure ()
      _ -> retry
