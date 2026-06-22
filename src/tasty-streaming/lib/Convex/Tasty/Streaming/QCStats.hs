{-# LANGUAGE NamedFieldPuns #-}

module Convex.Tasty.Streaming.QCStats (
  QCStatsKey (..),
  QCStatsStore,
  QCStatsRecorder (..),
  QCStatsStoreOption (..),
  newQCStatsStore,
  storeQCStatsRecorder,
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

data QCStatsKey = QCStatsKey
  { qskSrcLoc :: SrcLocRange
  , qskTestName :: T.Text
  }
  deriving (Eq, Ord, Show)

newtype QCStatsStore = QCStatsStore (IORef (Map QCStatsKey MonitoringStats))

newtype QCStatsRecorder = QCStatsRecorder
  { qcRecordStats :: QCStatsKey -> MonitoringStats -> IO ()
  }

newtype QCStatsStoreOption = QCStatsStoreOption (Maybe QCStatsStore)

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

newQCStatsStore :: IO QCStatsStore
newQCStatsStore = QCStatsStore <$> newIORef Map.empty

storeQCStatsRecorder :: QCStatsStore -> QCStatsRecorder
storeQCStatsRecorder (QCStatsStore ref) = QCStatsRecorder $ \key stats ->
  atomicModifyIORef' ref $ \m -> (Map.insert key stats m, ())

lookupQCStats :: QCStatsStore -> QCStatsKey -> IO (Maybe MonitoringStats)
lookupQCStats (QCStatsStore ref) key =
  Map.lookup key <$> readIORef ref

lookupQCStatsByTestInfo :: QCStatsStore -> TestInfo -> IO (Maybe MonitoringStats)
lookupQCStatsByTestInfo (QCStatsStore ref) ti =
  case tiSrcLoc ti of
    Nothing -> pure Nothing
    Just loc -> do
      m <- readIORef ref
      pure $ Map.lookup (mkQCStatsKey loc (tiName ti)) m

mkQCStatsKey :: SrcLocRange -> T.Text -> QCStatsKey
mkQCStatsKey loc testName = QCStatsKey{qskSrcLoc = loc, qskTestName = testName}

recordQCStatsFromState :: QCStatsRecorder -> Maybe SrcLocRange -> String -> QS.State -> IO ()
recordQCStatsFromState recorder mLoc testName st =
  for_ mLoc $ \loc ->
    qcRecordStats recorder (mkQCStatsKey loc (T.pack testName)) (fromState st)

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
