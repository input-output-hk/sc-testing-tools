{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Convex.Tasty.Streaming.TMSummary (
  ThreatModelSummary (..),
  ThreatModelCategory (..),
  Fault (..),
  faultLabel,
  threatModelGroupName,
  TMStore,
  TMRecorder (..),
  TMStoreOption (..),
  TraceRecorder (..),
  CoverageIndexStorage (..),
  newTMStore,
  storeRecorder,
  lookupThreatModelSummary,
) where

import Convex.Tasty.Streaming.SrcLoc (SrcLocRange)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, withText, (.:), (.:?), (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Test.Tasty.Options (IsOption (..))

{- | Which list of a suite's @ThreatModelsFor@ instance a threat model was run
from. It decides how the counts in a 'ThreatModelSummary' are to be read: the
same 'tmsFailed' (the attack's mutated transaction still validated) is a
vulnerability for a 'Claimed' model, the required outcome for an 'Expected'
one, and a known, tolerated artifact for an 'Accepted' one. A consumer that
alerts on @failed > 0@ must therefore filter on the category first.
-}
data ThreatModelCategory
  = -- | From @threatModels@: the contract is claimed secure against it; a detection fails the test.
    Claimed
  | -- | From @expectedVulnerabilities@: the contract is known vulnerable; no detection fails the test.
    Expected
  | -- | From @acceptedFindings@: the finding is a known benign artifact. Judged like @Expected@ - a detection is the required outcome.
    Accepted
  | -- | From @candidateModels@: run for information. Reported, but never failed for not applying.
    Surveyed
  | -- | From @notApplicable@: reviewed as not applying here; it applying at all fails the test.
    NotApplicable
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON ThreatModelCategory where
  toJSON = \case
    Claimed -> "claimed"
    Expected -> "expected"
    Accepted -> "accepted"
    Surveyed -> "surveyed"
    NotApplicable -> "not_applicable"

instance FromJSON ThreatModelCategory where
  parseJSON = withText "ThreatModelCategory" $ \case
    "claimed" -> pure Claimed
    "expected" -> pure Expected
    "accepted" -> pure Accepted
    "surveyed" -> pure Surveyed
    "not_applicable" -> pure NotApplicable
    other -> fail ("Unknown threat model category: " <> show other)

{- | The Tasty group that a category's per-model test cases live under. The
single source of both the group the test tree is built with and the names the
streaming reporter matches on, so that a category cannot end up in a group the
reporter does not recognise (@--test-id@ resolves a per-model test's
@Positive tests@ prerequisite from these names).
-}
threatModelGroupName :: ThreatModelCategory -> String
threatModelGroupName = \case
  Claimed -> "Threat models"
  Surveyed -> "Surveyed threat models"
  NotApplicable -> "Not applicable"
  Expected -> "Expected vulnerabilities"
  Accepted -> "Accepted findings"

{- | Whose fault a failing threat-model test case is.

Only one cell of the slot-by-outcome matrix is the contract's fault; most
red is a stale declaration. Saying which lets a reader — or a dashboard —
tell "the contract regressed" from "somebody needs to update the instance",
and stops a resolved vulnerability from reading as a broken contract.
-}
data Fault
  = -- | The script is at fault: fix the contract.
    Contract
  | -- | The @ThreatModelsFor@ instance is stale or wrong: edit the declaration.
    Declaration
  | -- | The attack could not be carried out: fix the generator, or accept a harness limit.
    Setup
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON Fault where
  toJSON = \case
    Contract -> "contract"
    Declaration -> "declaration"
    Setup -> "setup"

instance FromJSON Fault where
  parseJSON = withText "Fault" $ \case
    "contract" -> pure Contract
    "declaration" -> pure Declaration
    "setup" -> pure Setup
    other -> fail ("Unknown fault: " <> show other)

-- | The prefix a failure message leads with, so the fault is greppable.
faultLabel :: Fault -> String
faultLabel = \case
  Contract -> "CONTRACT"
  Declaration -> "DECLARATION"
  Setup -> "SETUP"

-- | Structured summary of a threat-model test case.
data ThreatModelSummary = ThreatModelSummary
  { tmsName :: !Text
  , tmsCategory :: !ThreatModelCategory
  , tmsTested :: !Int
  , tmsTotal :: !Int
  , tmsPassed :: !Int
  , tmsFailed :: !Int
  , tmsSkipped :: !Int
  , tmsSkippedPhase1 :: !Int
  , tmsErrors :: !Int
  , tmsFault :: !(Maybe Fault)
  -- ^ Set only when the case failed, saying whose fault it is.
  }
  deriving (Show, Eq, Generic)

instance ToJSON ThreatModelSummary where
  toJSON s =
    object
      [ "name" .= tmsName s
      , "category" .= tmsCategory s
      , "tested" .= tmsTested s
      , "total" .= tmsTotal s
      , "passed" .= tmsPassed s
      , "failed" .= tmsFailed s
      , "skipped" .= tmsSkipped s
      , "skipped_phase1" .= tmsSkippedPhase1 s
      , "errors" .= tmsErrors s
      , "fault" .= tmsFault s
      ]

instance FromJSON ThreatModelSummary where
  parseJSON = withObject "ThreatModelSummary" $ \o ->
    ThreatModelSummary
      <$> o .: "name"
      -- Required, as in the schema: defaulting a missing category to 'Claimed'
      -- would decode an accepted finding with @failed > 0@ as a vulnerability.
      <*> o .: "category"
      <*> o .: "tested"
      <*> o .: "total"
      <*> o .: "passed"
      <*> o .: "failed"
      <*> o .: "skipped"
      <*> o .: "skipped_phase1"
      <*> o .: "errors"
      -- Optional: only a failing case has a fault.
      <*> o .:? "fault"

-- | Mutable storage for threat-model summaries, owned by the reporter.
newtype TMStore = TMStore (IORef (Map String ThreatModelSummary))

{- | A recorder closure passed to test bodies via Tasty's option system.
The default no-op makes summaries silently dropped when the streaming
reporter is not active.
-}
newtype TMRecorder = TMRecorder
  { tmRecord :: String -> ThreatModelSummary -> IO ()
  }

{- | Internal option carrying the live store. Set by `defaultMainStreaming`
alongside the recorder so the reporter can read summaries back out.
-}
newtype TMStoreOption = TMStoreOption (Maybe TMStore)

instance IsOption TMRecorder where
  defaultValue = TMRecorder (\_ _ -> pure ())
  parseValue = const Nothing
  optionName = Tagged "tm-recorder"
  optionHelp = Tagged "internal: threat-model summary recorder"

instance IsOption TMStoreOption where
  defaultValue = TMStoreOption Nothing
  parseValue = const Nothing
  optionName = Tagged "tm-store"
  optionHelp = Tagged "internal: threat-model summary store handle"

-- | Allocate fresh storage. Call once per reporter run.
newTMStore :: IO TMStore
newTMStore = TMStore <$> newIORef Map.empty

-- | Build a recorder that writes into the given store.
storeRecorder :: TMStore -> TMRecorder
storeRecorder (TMStore ref) = TMRecorder $ \key s ->
  atomicModifyIORef' ref $ \m -> (Map.insert key s m, ())

-- | Look up a summary by key (does not delete).
lookupThreatModelSummary :: TMStore -> String -> IO (Maybe ThreatModelSummary)
lookupThreatModelSummary (TMStore ref) key =
  Map.lookup key <$> readIORef ref

{- | Callback for recording iteration traces as pre-serialized JSON.
Arguments: group name, category ("positive"\/"negative"), pre-serialized trace JSON.
Default is a no-op (zero overhead when streaming is not active).

When 'trEnabled' returns 'True', test bodies use the expensive traced code
path (building 'IterationTrace' values with UTxO snapshots, transaction
summaries, and JSON serialisation).  When it returns 'False' (the 'IsOption'
default), the cheap 'runActions' path is used instead, avoiding all that
work.

'trEnabled' is an 'IO' action so that the decision can be deferred until the
streaming reporter has parsed @--no-trace@ and written the shared 'IORef'.
-}
data TraceRecorder = TraceRecorder
  { trEnabled :: IO Bool
  -- ^ Whether test bodies should collect detailed traces.
  , recordIteration :: String -> String -> [SrcLocRange] -> Value -> IO ()
  -- ^ Emit a single iteration trace event.
  , findTestIdIO :: String -> String -> IO (Maybe Int)
  }

instance IsOption TraceRecorder where
  defaultValue = TraceRecorder (pure False) (\_ _ _ _ -> pure ()) (\_ _ -> pure Nothing)
  parseValue = const Nothing
  optionName = Tagged "trace-recorder"
  optionHelp = Tagged "internal: iteration trace recorder"

-- | Internal option carrying the coverage index, i.e. all the possible code range that can be reported as covered.
newtype CoverageIndexStorage = CoverageIndexStorage {getCoverageIndex :: [SrcLocRange]}

instance IsOption CoverageIndexStorage where
  defaultValue = CoverageIndexStorage []
  parseValue = const Nothing
  optionName = Tagged "coverage-index-storage"
  optionHelp = Tagged "internal: coverage index storage"
