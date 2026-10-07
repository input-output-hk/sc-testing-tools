{- | Tests for naming a test by its group path: what a test body reads from
'GroupPathOpt' once the tree went through 'annotateGroupPaths', and how
'findTestId' resolves it against the map that 'buildTestMap' builds.
-}
module Convex.Tasty.TreeMapSpec (tests) where

import Convex.Tasty.HUnit (Assertion, testCase, (@?=))
import Convex.Tasty.Streaming.TreeMap (GroupPathOpt (..), annotateGroupPaths, buildTestMap, findTestId)
import Convex.Tasty.Streaming.Types (TestInfo (..))
import Data.IntMap.Strict qualified as IntMap
import Data.List (intercalate)
import Data.Text qualified as Text
import Test.Tasty (DependencyType (..), TestTree, after, askOption, localOption, sequentialTestGroup, testGroup, withResource)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (QuickCheckTests (..))

tests :: TestTree
tests =
  testGroup
    "TreeMap"
    [ testCase "annotateGroupPaths gives every subtree its group path" groupPathIsTiPath
    , testCase "findTestId tells same-named groups apart" findTestIdMatchesWholePath
    , testCase "findTestId returns the original id under --test-id" findTestIdKeepsRemappedIds
    ]

-- | A test that names itself after the group path it reads.
pathEcho :: TestTree
pathEcho = askOption $ \(GroupPathOpt path) -> HUnit.testCase (intercalate "/" path) (pure ())

{- | What a test body reads is the 'tiPath' of the tests next to it, through
every kind of node a tree can put in between.
-}
groupPathIsTiPath :: Assertion
groupPathIsTiPath = do
  let tree =
        annotateGroupPaths $
          testGroup
            "root"
            [ pathEcho
            , testGroup "a" [pathEcho, testGroup "b" [pathEcho]]
            , withResource (pure ()) (\_ -> pure ()) (\_ -> testGroup "c" [pathEcho])
            , sequentialTestGroup "d" AllFinish [pathEcho]
            , -- the shape of propRunActions: options asked for, then a group
              localOption (QuickCheckTests 1) (askOption (\(QuickCheckTests _) -> testGroup "e" [pathEcho]))
            , after AllFinish "a" (testGroup "f" [pathEcho])
            ]
  infos <- IntMap.elems <$> buildTestMap mempty id tree
  map (Text.unpack . tiName) infos @?= ["root", "root/a", "root/a/b", "root/c", "root/d", "root/e", "root/f"]
  map (Text.unpack . tiName) infos @?= map (intercalate "/" . map Text.unpack . tiPath) infos

{- | The bug this guards against: every suite's group was looked up by its
name alone, so all the suites called "property-based testing" resolved to
the first one's tests.
-}
findTestIdMatchesWholePath :: Assertion
findTestIdMatchesWholePath = do
  let suite = testGroup "property-based testing" [HUnit.testCase "Positive tests" (pure ()), HUnit.testCase "Negative tests" (pure ())]
      tree = testGroup "root" [testGroup "hello world" [suite], testGroup "tip-jar" [suite]]
  testMap <- buildTestMap mempty id tree
  findTestId testMap ["root", "hello world", "property-based testing", "Positive tests"] @?= Just 0
  findTestId testMap ["root", "tip-jar", "property-based testing", "Positive tests"] @?= Just 2
  findTestId testMap ["root", "tip-jar", "property-based testing", "Negative tests"] @?= Just 3
  findTestId testMap ["property-based testing", "Positive tests"] @?= Nothing
  findTestId testMap ["root", "tip-jar", "Positive tests"] @?= Nothing
  findTestId testMap [] @?= Nothing

-- | With @--test-id@ the map is keyed by the original ids, so those are what it resolves to.
findTestIdKeepsRemappedIds :: Assertion
findTestIdKeepsRemappedIds = do
  let tree = testGroup "root" [testGroup "a" [HUnit.testCase "Positive tests" (pure ())], testGroup "b" [HUnit.testCase "Positive tests" (pure ())]]
  testMap <- buildTestMap mempty (IntMap.fromList [(0, 7), (1, 9)] IntMap.!) tree
  findTestId testMap ["root", "b", "Positive tests"] @?= Just 9
