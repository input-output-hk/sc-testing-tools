{-# LANGUAGE NamedFieldPuns #-}

module Convex.Tasty.Streaming.QCStats (
  QCStatsStore,
  QCStatsRecorder (..),
  QCStatsStoreOption (..),
  QCStatsPathIndex (..),
  newQCStatsStore,
  storeQCStatsRecorder,
  mkQCStatsPathIndex,
  resolveQCStatsPath,
  mkQCStatsKey,
  lookupQCStats,
  lookupQCStatsByTestInfo,
  recordQCStatsFromState,
) where

import Convex.Tasty.Streaming.SrcLoc (SrcLocRange (..))
import Convex.Tasty.Streaming.Types (
  MonitoringClassStat (..),
  MonitoringLabelStat (..),
  MonitoringStats (..),
  MonitoringTableEntry (..),
  MonitoringTableStat (..),
  TestInfo (..),
 )
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Tagged (Tagged (..))
import Data.Text qualified as T
import Test.QuickCheck.State qualified as QS
import Test.Tasty.Options (IsOption (..))

newtype QCStatsStore = QCStatsStore (IORef (Map String MonitoringStats))

newtype QCStatsRecorder = QCStatsRecorder
  { qcRecordStats :: String -> MonitoringStats -> IO ()
  }

newtype QCStatsStoreOption = QCStatsStoreOption (Maybe QCStatsStore)

newtype QCStatsPathIndex = QCStatsPathIndex (Map String (Maybe [T.Text]))

instance IsOption QCStatsRecorder where
  defaultValue = QCStatsRecorder (\_ _ -> pure ())
  parseValue = const Nothing
  optionName = Tagged "qc-stats-recorder"
  optionHelp = Tagged "internal: quickcheck monitoring stats recorder"

instance IsOption QCStatsStoreOption where
  defaultValue = QCStatsStoreOption Nothing
  parseValue = const Nothing
  optionName = Tagged "qc-stats-store"
  optionHelp = Tagged "internal: quickcheck monitoring stats store handle"

instance IsOption QCStatsPathIndex where
  defaultValue = QCStatsPathIndex Map.empty
  parseValue = const Nothing
  optionName = Tagged "qc-stats-path-index"
  optionHelp = Tagged "internal: quickcheck monitoring stats path resolver"

newQCStatsStore :: IO QCStatsStore
newQCStatsStore = QCStatsStore <$> newIORef Map.empty

storeQCStatsRecorder :: QCStatsStore -> QCStatsRecorder
storeQCStatsRecorder (QCStatsStore ref) = QCStatsRecorder $ \key stats ->
  atomicModifyIORef' ref $ \m -> (Map.insert key stats m, ())

lookupQCStats :: QCStatsStore -> String -> IO (Maybe MonitoringStats)
lookupQCStats (QCStatsStore ref) key =
  Map.lookup key <$> readIORef ref

lookupQCStatsByTestInfo :: QCStatsStore -> TestInfo -> IO (Maybe MonitoringStats)
lookupQCStatsByTestInfo (QCStatsStore ref) ti =
  case tiSrcLoc ti of
    Nothing -> pure Nothing
    Just loc -> do
      m <- readIORef ref
      pure $ Map.lookup (mkQCStatsKey loc (tiPath ti) (tiName ti)) m

mkQCStatsPathIndex :: [TestInfo] -> QCStatsPathIndex
mkQCStatsPathIndex infos = QCStatsPathIndex (foldr insertInfo Map.empty infos)
 where
  insertInfo ti m =
    case tiSrcLoc ti of
      Nothing -> m
      Just loc ->
        let identity = mkQCStatsKey loc [] (tiName ti)
            path = tiPath ti
         in case Map.lookup identity m of
              Nothing -> Map.insert identity (Just path) m
              Just (Just existingPath)
                | existingPath == path -> m
                | otherwise -> Map.insert identity Nothing m
              Just Nothing -> m

resolveQCStatsPath :: QCStatsPathIndex -> Maybe SrcLocRange -> String -> Maybe [T.Text]
resolveQCStatsPath (QCStatsPathIndex index) mLoc testName = do
  loc <- mLoc
  Map.lookup (mkQCStatsKey loc [] (T.pack testName)) index >>= id

recordQCStatsFromState :: QCStatsRecorder -> Maybe SrcLocRange -> Maybe [T.Text] -> String -> QS.State -> IO ()
recordQCStatsFromState recorder mLoc mPath testName st =
  for_ ((,) <$> mLoc <*> mPath) $ \(loc, path) ->
    qcRecordStats recorder (mkQCStatsKey loc path (T.pack testName)) (fromState st)

srcLocKey :: SrcLocRange -> String
srcLocKey SrcLocRange{slrFile, slrStartLine, slrStartCol, slrEndLine, slrEndCol} =
  T.unpack slrFile
    <> ":"
    <> show slrStartLine
    <> ":"
    <> show slrStartCol
    <> ":"
    <> show slrEndLine
    <> ":"
    <> show slrEndCol

mkQCStatsKey :: SrcLocRange -> [T.Text] -> T.Text -> String
mkQCStatsKey loc pathParts testName =
  srcLocKey loc <> "#" <> concatMap encodePart (pathParts <> [testName])
 where
  encodePart part =
    show (T.length part) <> ":" <> T.unpack part <> "|"

fromState :: QS.State -> MonitoringStats
fromState st =
  MonitoringStats
    { msNumTests = total
    , msNumDiscarded = QS.numDiscardedTests st
    , msLabels =
        sortOn
          (Down . mlsCount)
          [ MonitoringLabelStat
              { mlsLabels = map T.pack names
              , mlsCount = count
              , mlsPercent = pct count
              }
          | (names, count) <- Map.toList (QS.labels st)
          ]
    , msClasses =
        sortOn
          (Down . mcsCount)
          [ MonitoringClassStat
              { mcsName = T.pack className
              , mcsCount = count
              , mcsPercent = pct count
              }
          | (className, count) <- Map.toList (QS.classes st)
          ]
    , msTables =
        [ MonitoringTableStat
            { mtsName = T.pack tableName
            , mtsEntries =
                sortOn
                  (Down . mteCount)
                  [ MonitoringTableEntry
                      { mteValue = T.pack entryName
                      , mteCount = count
                      }
                  | (entryName, count) <- Map.toList tableEntries
                  ]
            }
        | (tableName, tableEntries) <- Map.toList (QS.tables st)
        ]
    }
 where
  total = QS.numSuccessTests st
  pct count
    | total <= 0 = 0
    | otherwise = (fromIntegral count * 100) / fromIntegral total
