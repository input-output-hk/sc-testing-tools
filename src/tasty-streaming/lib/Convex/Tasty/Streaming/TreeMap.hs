module Convex.Tasty.Streaming.TreeMap (
  buildTestMap,
  findTestId,
  testPath,
  GroupPathOpt (..),
  annotateGroupPaths,
) where

import Convex.Tasty.Streaming.SrcLoc (SrcLocOpt (..))
import Convex.Tasty.Streaming.Types (TestInfo (..))
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find)
import Data.Tagged (Tagged (..))
import Data.Text qualified as Text
import Test.Tasty (localOption)
import Test.Tasty.Options (IsOption (..), OptionSet, lookupOption)
import Test.Tasty.Runners (Ap (..), TestTree (..), TreeFold (..), foldTestTree, trivialFold)

{- | Build a mapping from test indices to test metadata.

The indices correspond to the order tests appear during a fold of the TestTree,
which is the same order Tasty uses when building the StatusMap.

Test ids correspond to StatusMap indices when there's no test filtering with --test-id.
If --test-id options are used then the passed remapId will convert StatusMap indices to test indices.
-}
buildTestMap :: OptionSet -> (Int -> Int) -> TestTree -> IO (IntMap TestInfo)
buildTestMap opts remapId tree = do
  counterRef <- newIORef (0 :: Int)
  let Ap action = foldTestTree (mkFold remapId counterRef) opts tree
  action

mkFold :: (Int -> Int) -> IORef Int -> TreeFold (Ap IO (IntMap TestInfo))
mkFold remapId counterRef =
  (trivialFold :: TreeFold (Ap IO (IntMap TestInfo)))
    { foldSingle = \opts name _ -> Ap $ do
        idx <- remapId <$> readIORef counterRef
        modifyIORef' counterRef (+ 1)
        let SrcLocOpt mLoc = lookupOption opts
            info =
              TestInfo
                { tiId = idx
                , tiName = Text.pack name
                , tiPath = []
                , tiSrcLoc = mLoc
                }
        pure $ IntMap.singleton idx info
    , foldGroup = \_opts groupName children -> Ap $ do
        let Ap childAction = mconcat children
        childMap <- childAction
        let prependGroup ti = ti{tiPath = Text.pack groupName : tiPath ti}
        pure $ fmap prependGroup childMap
    , foldResource = \_ _ k ->
        k (error "Convex.Tasty.Streaming.TreeMap: resource not available during fold")
    }

{- | The id of the test at the given full path (see 'testPath'), if the map
has one. A test that @--test-id@ filtered out is not in the map.

The whole path has to match: tests in different groups can share both their
own name and their group's name (most @propRunActions@ suites call theirs
\"property-based testing\").
-}
findTestId :: IntMap TestInfo -> [String] -> Maybe Int
findTestId testMap path = tiId <$> find ((== wanted) . fullPath) (IntMap.elems testMap)
 where
  wanted = map Text.pack path

{- | A test's full path: the names of its groups, outermost first, then its
own name. What a test case records its threat-model summary under.
-}
testPath :: TestInfo -> [String]
testPath = map Text.unpack . fullPath

-- | 'testPath' as the test map spells it.
fullPath :: TestInfo -> [Text.Text]
fullPath ti = tiPath ti <> [tiName ti]

{- | Internal Tasty option carrying the names of the groups enclosing a
subtree, outermost first: the 'tiPath' that 'buildTestMap' gives the tests
directly inside it.

Set on every group by 'annotateGroupPaths', so that a test body can name its
own test for 'findTestId'. Not user-settable from the command line.
-}
newtype GroupPathOpt = GroupPathOpt [String]
  deriving (Eq, Show)

instance IsOption GroupPathOpt where
  defaultValue = GroupPathOpt []
  parseValue = const Nothing
  optionName = Tagged "internal-group-path"
  optionHelp = Tagged "Internal: names of the test groups enclosing a subtree"

-- | Set 'GroupPathOpt' on every group of the tree to that group's path.
annotateGroupPaths :: TestTree -> TestTree
annotateGroupPaths = go []
 where
  go path tree = case tree of
    SingleTest{} -> tree
    TestGroup name children ->
      let path' = path <> [name]
       in localOption (GroupPathOpt path') (TestGroup name (map (go path') children))
    PlusTestOptions f subtree -> PlusTestOptions f (go path subtree)
    WithResource spec k -> WithResource spec (go path . k)
    AskOptions k -> AskOptions (go path . k)
    After depType expr subtree -> After depType expr (go path subtree)
