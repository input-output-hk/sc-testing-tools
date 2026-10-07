{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

{- | Which tests the streamed traces name.

Suites in different groups can share a group name (all the CTF suites call
theirs "property-based testing"), and every such suite's traces used to be
streamed under the first one's test ids.
-}
module TraceAttributionSpec (
  traceAttributionTests,
) where

import Control.Concurrent.STM (atomically, readTVar, retry)
import Convex.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Convex.Tasty.Streaming.TMSummary (TraceRecorder (..))
import Convex.Tasty.Streaming.TreeMap (annotateGroupPaths, buildTestMap, findTestId)
import Convex.TestingInterface (RunOptions, propRunActionsWithOptions)
import Data.Foldable (for_, traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import PingPongSpec (PingPongModel)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (..))
import Test.Tasty.Runners (Status (..), launchTestTree)

traceAttributionTests :: RunOptions -> TestTree
traceAttributionTests runOpts =
  testGroup
    "trace attribution"
    [testCase "same-named suites trace under their own tests" (sameNamedSuites runOpts)]

{- | Two suites with the same group name, in different groups: each names its
own tests, both for its iterations and for its threat models' entries.
-}
sameNamedSuites :: RunOptions -> Assertion
sameNamedSuites runOpts = do
  iterations <- newIORef Set.empty
  modelLookups <- newIORef Set.empty
  let recorder =
        TraceRecorder
          { trEnabled = pure True
          , recordIteration = \path category _ _ -> insert iterations (path, category)
          , findTestIdIO = \path name -> Nothing <$ insert modelLookups (path, name)
          }
      suite = propRunActionsWithOptions @PingPongModel "property-based testing" runOpts
      tree =
        annotateGroupPaths . localOption recorder . localOption (QuickCheckTests 2) $
          testGroup "contracts" [testGroup "a" [suite], testGroup "b" [suite]]
      suitePaths = [["contracts", c, "property-based testing"] | c <- ["a", "b"]]
  testMap <- buildTestMap mempty id tree
  runQuietly tree

  recorded <- readIORef iterations
  recorded @?= Set.fromList [(path, category) | path <- suitePaths, category <- ["positive", "negative"]]
  for_ recorded $ \(path, category) ->
    assertBool
      ("no " <> category <> " property at " <> show path)
      (isJust (findTestId testMap path (propertyName category)))

  looked <- readIORef modelLookups
  Set.map (take 3 . fst) looked @?= Set.fromList suitePaths
  for_ looked $ \(path, name) ->
    assertBool
      ("no test case for threat model " <> show name <> " at " <> show path)
      (isJust (findTestId testMap path name))
 where
  propertyName = \case
    "positive" -> "Positive tests"
    _ -> "Negative tests"

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
