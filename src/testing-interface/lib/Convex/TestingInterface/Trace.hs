{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Convex.TestingInterface.Trace (
  -- * Test run trace
  TestRunTrace (..),
  TestCategory (..),

  -- * Iteration trace
  IterationTrace (..),
  IterationStatus (..),

  -- * State transitions
  Transition (..),
  TransitionResult (..),

  -- * Transaction summary
  TxSummary (..),
  TxInputSummary (..),
  TxOutputSummary (..),

  -- * Value representation
  ValueSummary (..),
  AssetSummary (..),

  -- * Threat model trace
  ThreatModelTrace (..),
  ThreatModelTraceOutcome (..),

  -- * Redeemer tagging (Tier 2)
  RedeemerTag (..),
  RedeemerTagger (..),
) where

import Control.Applicative ((<|>))
import Convex.Tasty.Streaming.SrcLoc (SrcLocRange, groupRanges)
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import PlutusTx (Data)

{- | Complete trace of a test run (one QuickCheck property execution = N iterations).
Links to the Tasty test tree via 'trtTestId'.
-}
data TestRunTrace = TestRunTrace
  { trtTestId :: !Int
  -- ^ Tasty test ID (links to the @test_done@ event in the NDJSON stream)
  , trtTestName :: !Text
  -- ^ e.g. "Positive tests"
  , trtPath :: ![Text]
  -- ^ Tasty test path, e.g. @["MyContract", "Positive tests"]@
  , trtCategory :: !TestCategory
  , trtIterations :: ![IterationTrace]
  }
  deriving (Eq, Show, Generic)

-- | Whether this test run is a positive or negative test property.
data TestCategory
  = Positive
  | Negative
  deriving (Eq, Show, Generic)

instance ToJSON TestCategory where
  toJSON Positive = "positive"
  toJSON Negative = "negative"

-- | Trace of a single QuickCheck iteration within a test run.
data IterationTrace = IterationTrace
  { itIndex :: !Int
  -- ^ 0-based iteration number
  , itStatus :: !IterationStatus
  , itTransitions :: ![Transition]
  -- ^ Ordered sequence of actions performed
  , itThreatModels :: ![ThreatModelTrace]
  {- ^ Threat model results applied to this iteration's transactions.
  Only populated for positive tests.
  -}
  }
  deriving (Eq, Show, Generic)

-- | Outcome of a single iteration.
data IterationStatus
  = IterationSuccess
  | IterationFailure !Text
  | IterationDiscarded !Text
  deriving (Eq, Show, Generic)

{- | One step in an iteration: an action was performed, the model state changed,
and a transaction was (possibly) submitted.
-}
data Transition = Transition
  { trStepIndex :: !Int
  -- ^ 0-based index within the iteration
  , trAction :: !Text
  -- ^ @show@ of the @Action state@ value
  , trStateBefore :: !Value
  -- ^ @toJSON@ of the model state before @perform@
  , trStateAfter :: !Value
  -- ^ @toJSON@ of the model state after @perform@
  , trTransaction :: !(Maybe TxSummary)
  {- ^ The transaction produced, if any. @Nothing@ if @perform@ failed
  before building a transaction.
  -}
  , trResult :: !TransitionResult
  }
  deriving (Eq, Show, Generic)

-- | Whether the transaction was successfully submitted to the mockchain.
data TransitionResult
  = -- | TxId as text
    TransitionSuccess !Text
  | -- | Error description
    TransitionFailure !Text
  deriving (Eq, Show, Generic)

{- | Compact representation of a transaction for visualization.
Values are structured JSON for full fidelity.
-}
data TxSummary = TxSummary
  { txsId :: !(Maybe Text)
  -- ^ TxId if submitted successfully, @Nothing@ otherwise
  , txsInputs :: ![TxInputSummary]
  , txsOutputs :: ![TxOutputSummary]
  , txsMint :: !(Maybe ValueSummary)
  -- ^ Structured mint value, @Nothing@ if no minting
  , txsFee :: !Integer
  -- ^ Fee in lovelace
  , txsSigners :: ![Text]
  -- ^ Required signer key hashes
  , txsValidRange :: !(Maybe Text)
  -- ^ Rendered validity interval
  }
  deriving (Eq, Show, Generic)

-- | Summary of a transaction input.
data TxInputSummary = TxInputSummary
  { tisUtxo :: !Text
  -- ^ @"txid#index"@
  , tisAddress :: !Text
  -- ^ Bech32 or hex address
  , tisValue :: ValueSummary
  -- ^ Structured value (ada + tokens)
  , tisRedeemerRaw :: !(Maybe Text)
  {- ^ Hex (CBOR) of the spend redeemer's 'C.ScriptData', @Nothing@ for
  non-script inputs (no redeemer in the witness set).
  -}
  , tisRedeemerConstr :: !(Maybe Integer)
  {- ^ Constr index when the parsed redeemer 'Data' is @Constr n _@;
  @Nothing@ for non-Constr redeemers or non-script inputs.
  -}
  , tisRedeemerKind :: !(Maybe Text)
  {- ^ Tier 2: human-readable redeemer label (e.g. @"Pong"@) produced by
  the implementor's 'RedeemerTagger'. @Nothing@ when no tagger is
  supplied or the tagger declines to label this redeemer.
  -}
  , tisRedeemerPayload :: !(Maybe Value)
  {- ^ Tier 2: optional JSON payload accompanying 'tisRedeemerKind'.
  @Nothing@ unless the tagger explicitly returns one.
  -}
  }
  deriving (Eq, Show, Generic)

-- | Summary of a transaction output.
data TxOutputSummary = TxOutputSummary
  { tosUtxo :: !Text
  -- ^ @"txid#index"@ – the UTxO reference for this output
  , tosAddress :: !Text
  , tosValue :: !ValueSummary
  , tosDatum :: !(Maybe Text)
  -- ^ @"inline:\<hash\>"@, @"hash:\<hash\>"@, or @Nothing@
  }
  deriving (Eq, Show, Generic)

-- | Structured representation of a Cardano value for JSON serialization.
data ValueSummary = ValueSummary
  { vsLovelace :: !Integer
  , vsAssets :: ![AssetSummary]
  }
  deriving (Eq, Show, Generic)

data AssetSummary = AssetSummary
  { asPolicyId :: !Text
  , asName :: !Text
  , asQuantity :: !Integer
  }
  deriving (Eq, Show, Generic)

{- | A human-readable label for a redeemer, produced by a 'RedeemerTagger'
supplied by the implementor (Tier 2). When no tagger matches, both
'tisRedeemerKind' and 'tisRedeemerPayload' stay @Nothing@ and only Tier 1
('tisRedeemerRaw' / 'tisRedeemerConstr') is streamed.
-}
data RedeemerTag = RedeemerTag
  { rtKind :: !Text
  -- ^ Discriminator such as @"Ping"@ / @"Pong"@.
  , rtPayload :: !(Maybe Value)
  -- ^ Optional JSON payload for richer UI columns.
  }

{- | An opt-in function from a parsed Plutus 'Data' (the redeemer of a script
input) to an optional 'RedeemerTag'. The 'Monoid' instance picks the first
'Just' result, enabling composition of per-validator taggers.
-}
newtype RedeemerTagger = RedeemerTagger
  { applyRedeemerTagger :: Data -> Maybe RedeemerTag
  }

instance Semigroup RedeemerTagger where
  RedeemerTagger f <> RedeemerTagger g =
    RedeemerTagger (\d -> f d <|> g d)

instance Monoid RedeemerTagger where
  mempty = RedeemerTagger (const Nothing)

instance ToJSON ValueSummary where
  toJSON v =
    object
      [ "lovelace" .= vsLovelace v
      , "assets" .= vsAssets v
      ]

instance ToJSON AssetSummary where
  toJSON a =
    object
      [ "policyId" .= asPolicyId a
      , "name" .= asName a
      , "quantity" .= asQuantity a
      ]

{- | What happened when a threat model was applied to a specific transaction
in this iteration.
-}
data ThreatModelTrace = ThreatModelTrace
  { tmtName :: !Text
  -- ^ Name of the threat model (e.g. "unprotectedScriptOutput")
  , tmtTestId :: !Int
  -- ^ Test id of the threat model
  , tmtTargetTxIndex :: !Int
  -- ^ Index into 'itTransitions' identifying which transaction was targeted
  , tmtModifications :: ![Value]
  -- ^ Structured JSON descriptions of each modification applied
  , tmtOriginalTx :: !TxSummary
  -- ^ The original transaction before modification
  , tmtModifiedTx :: !(Maybe TxSummary)
  -- ^ The modified transaction, @Nothing@ if the modification couldn't produce a valid tx body
  , tmtOutcome :: !ThreatModelTraceOutcome
  -- ^ The outcome of running the threat model
  , tmtCovered :: ![SrcLocRange]
  -- ^ The code ranges covered by running this threat model
  }
  deriving (Eq, Show, Generic)

-- | Outcome of applying a threat model to a transaction.
data ThreatModelTraceOutcome
  = -- | Modified tx was correctly rejected by the ledger (good!)
    TMTOPassed
  | -- | Modified tx was ACCEPTED by the ledger (vulnerability found!)
    TMTOFailed !Text
  | -- | Couldn't test: rebalancing failed or precondition not met
    TMTOSkipped !Text
  | -- | Skipped: modified tx hit Phase 1 invalidation
    TMTOSkippedPhase1 !Text
  | -- | Unexpected error during threat model execution
    TMTOError !Text
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------
-- ToJSON instances
-- ---------------------------------------------------------------------

instance ToJSON TestRunTrace where
  toJSON t =
    object
      [ "testId" .= trtTestId t
      , "testName" .= trtTestName t
      , "path" .= trtPath t
      , "category" .= trtCategory t
      , "iterations" .= trtIterations t
      ]

instance ToJSON IterationTrace where
  toJSON t =
    object
      [ "index" .= itIndex t
      , "status" .= itStatus t
      , "transitions" .= itTransitions t
      , "threatModels" .= itThreatModels t
      ]

instance ToJSON IterationStatus where
  toJSON IterationSuccess =
    object ["status" .= ("success" :: Text)]
  toJSON (IterationFailure msg) =
    object ["status" .= ("failure" :: Text), "message" .= msg]
  toJSON (IterationDiscarded msg) =
    object ["status" .= ("discarded" :: Text), "message" .= msg]

instance ToJSON Transition where
  toJSON t =
    object
      [ "stepIndex" .= trStepIndex t
      , "action" .= trAction t
      , "stateBefore" .= trStateBefore t
      , "stateAfter" .= trStateAfter t
      , "transaction" .= trTransaction t
      , "result" .= trResult t
      ]

instance ToJSON TransitionResult where
  toJSON (TransitionSuccess txId) =
    object ["status" .= ("success" :: Text), "txId" .= txId]
  toJSON (TransitionFailure err) =
    object ["status" .= ("failure" :: Text), "error" .= err]

instance ToJSON TxSummary where
  toJSON t =
    object
      [ "id" .= txsId t
      , "inputs" .= txsInputs t
      , "outputs" .= txsOutputs t
      , "mint" .= txsMint t
      , "fee" .= txsFee t
      , "signers" .= txsSigners t
      , "validRange" .= txsValidRange t
      ]

instance ToJSON TxInputSummary where
  toJSON t =
    object
      [ "utxo" .= tisUtxo t
      , "address" .= tisAddress t
      , "value" .= tisValue t
      , "redeemerRaw" .= tisRedeemerRaw t
      , "redeemerConstr" .= tisRedeemerConstr t
      , "redeemerKind" .= tisRedeemerKind t
      , "redeemerPayload" .= tisRedeemerPayload t
      ]

instance ToJSON TxOutputSummary where
  toJSON t =
    object
      [ "utxo" .= tosUtxo t
      , "address" .= tosAddress t
      , "value" .= tosValue t
      , "datum" .= tosDatum t
      ]

instance ToJSON ThreatModelTrace where
  toJSON t =
    object
      [ "name" .= tmtName t
      , "testId" .= tmtTestId t
      , "targetTxIndex" .= tmtTargetTxIndex t
      , "modifications" .= tmtModifications t
      , "originalTx" .= tmtOriginalTx t
      , "modifiedTx" .= tmtModifiedTx t
      , "outcome" .= tmtOutcome t
      , "covered" .= groupRanges (tmtCovered t)
      ]

instance ToJSON ThreatModelTraceOutcome where
  toJSON TMTOPassed =
    object ["status" .= ("passed" :: Text)]
  toJSON (TMTOFailed reason) =
    object ["status" .= ("failed" :: Text), "reason" .= reason]
  toJSON (TMTOSkipped reason) =
    object ["status" .= ("skipped" :: Text), "reason" .= reason]
  toJSON (TMTOSkippedPhase1 reason) =
    object ["status" .= ("skipped_phase1" :: Text), "reason" .= reason]
  toJSON (TMTOError msg) =
    object ["status" .= ("error" :: Text), "message" .= msg]
