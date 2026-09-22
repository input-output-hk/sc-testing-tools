{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RewardWithdrawal.Spec.Prop (
  propBasedTests,
) where

import Convex.TestingInterface (RunOptions, TestingInterface (..), ThreatModelsFor (..), propRunActionsWithOptions)
import Convex.TestingInterface.Trace.RedeemerTag (autoRedeemerTag)
import Convex.ThreatModel.InvalidDatumIndex (invalidDatumIndexAttack)
import Convex.ThreatModel.LargeData (largeDataAttack)
import Convex.ThreatModel.LargeValue (largeValueAttack)
import Convex.ThreatModel.NegativeInteger (negativeIntegerAttack)
import Convex.ThreatModel.SignatoryRemoval (signatoryRemoval)
import Convex.ThreatModel.ValueUnderpayment (valueUnderpaymentAttack)
import Data.Aeson (ToJSON (..))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import RewardWithdrawal.Spec.Common (fixedOwner, registerCredential, withdrawZero)
import Test.QuickCheck (choose, frequency)
import Test.Tasty (TestTree, testGroup)

-------------------------------------------------------------------------------
-- Property-based tests for the RewardWithdrawal script
-------------------------------------------------------------------------------

propBasedTests :: RunOptions -> TestTree
propBasedTests runOpts =
  testGroup
    "property-based tests"
    [ propRunActionsWithOptions @RewardWithdrawalModel "Property-based test reward withdrawal script" runOpts
    ]

-------------------------------------------------------------------------------
-- RewardWithdrawal Testing Interface
-------------------------------------------------------------------------------

{- | Model of the RewardWithdrawal contract state for property-based testing.

  Configuration:
  - Owner: MockWallet.w1
  - The stake credential's script is registered exactly once, then triggered
    any number of times by zero-lovelace withdrawals (the "withdraw zero
    trick"), either on their own or while locking ADA at the script's own
    payment address under a @LockDatum@ the script vets.
-}
newtype RewardWithdrawalModel = RewardWithdrawalModel
  { _registered :: Bool
  {- ^ Whether the stake credential has been registered yet. The owner,
  parameters and script hash are fixed for the whole run and live in
  'RewardWithdrawal.Spec.Common', so this is the only state the model
  tracks.
  -}
  }
  deriving (Show, Eq, Generic)

instance ToJSON RewardWithdrawalModel where
  toJSON = toJSON . show

instance TestingInterface RewardWithdrawalModel where
  data Action RewardWithdrawalModel
    = -- \| Register the script-guarded stake credential
      Register
    | -- \| Trigger the script via a zero-lovelace withdrawal
      WithdrawZero
    | -- \| Trigger the script via a zero-lovelace withdrawal while locking
      -- this many lovelace at the script's payment address
      Lock Integer
    deriving (Show, Eq)

  initialize = pure RewardWithdrawalModel{_registered = False}

  arbitraryAction _ =
    frequency
      [ (1, pure Register)
      , (2, pure WithdrawZero)
      , (3, Lock <$> choose (2_000_000, 50_000_000))
      ]

  precondition vm Register = not (_registered vm)
  precondition vm WithdrawZero = _registered vm
  precondition vm (Lock amount) = _registered vm && amount > 0

  perform vm Register = do
    registerCredential
    pure vm{_registered = True}
  perform vm WithdrawZero = do
    withdrawZero fixedOwner Nothing
    pure vm
  perform vm (Lock amount) = do
    -- The datum claims exactly what the output holds, so the validator's
    -- amount check passes and the threat models below have a valid
    -- transaction to attack.
    withdrawZero fixedOwner (Just (amount, amount))
    pure vm

  validate _vm = pure True
  monitoring _ _ = id
  redeemerTagger = autoRedeemerTag (Proxy @())

instance ThreatModelsFor RewardWithdrawalModel where
  -- The 'Lock' transactions are the interesting ones here: they spend no
  -- script input at all, so the output-targeting attacks below only apply
  -- because the harness recognises the zero-lovelace withdrawal's Rewarding
  -- redeemer as a running validator ('requireScriptExecution'). The validator
  -- vets each lock output's datum constructor, owner, amount and value, so
  -- these attacks are expected to be rejected.
  --
  -- Notably absent: the input-shaped attacks ('doubleSatisfaction',
  -- 'inputDuplication', 'mutualExclusionAttack', 'unprotectedScriptOutput')
  -- need a script input, and nothing here ever spends one; the datum-bloat
  -- attacks need a list field and 'missingOutputDatumAttack' /
  -- 'outputDatumHashMissingAttack' a datum-hash output, and the lock datum
  -- has neither.
  threatModels =
    [ signatoryRemoval
    , invalidDatumIndexAttack
    , negativeIntegerAttack
    , valueUnderpaymentAttack
    , largeValueAttack
    ]

  -- largeDataAttack appends extra fields to the lock datum's constructor and
  -- the transaction still validates: the derived 'FromData' decoder for
  -- 'LockDatum' reads the two fields it knows and ignores any trailing ones.
  -- That is a benign artifact rather than an exploitable bug here - the
  -- owner, amount and value of the output are still checked in full, nothing
  -- ever consumes a lock output on-chain, and the only party paying for the
  -- bloated datum's min-UTxO is the locker themselves.
  acceptedFindings = [largeDataAttack]
