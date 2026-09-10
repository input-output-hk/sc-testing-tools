{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module RewardWithdrawal.Spec.Prop (
  propBasedTests,
) where

import Cardano.Api qualified as C
import Cardano.Api.Experimental.Certificate qualified as Ex
import Cardano.Ledger.Core qualified as Ledger
import Control.Lens ((^.))
import Control.Monad.Except (MonadError)
import Convex.BuildTx (execBuildTx, execBuildTxT)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadMockchain, queryProtocolParameters)
import Convex.CoinSelection (BalanceTxError, ChangeOutputPosition (TrailingChange))
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.MockChain.Defaults qualified as Defaults
import Convex.PlutusLedger.V1 (transPubKeyHash)
import Convex.TestingInterface (RunOptions, TestingInterface (..), ThreatModelsFor (..), propRunActionsWithOptions)
import Convex.TestingInterface.Trace.RedeemerTag (autoRedeemerTag)
import Convex.ThreatModel.InvalidDatumIndex (invalidDatumIndexAttack)
import Convex.ThreatModel.LargeData (largeDataAttack)
import Convex.ThreatModel.LargeValue (largeValueAttack)
import Convex.ThreatModel.NegativeInteger (negativeIntegerAttack)
import Convex.ThreatModel.SignatoryRemoval (signatoryRemoval)
import Convex.ThreatModel.ValueUnderpayment (valueUnderpaymentAttack)
import Convex.Wallet (Wallet, verificationKeyHash)
import Convex.Wallet.MockWallet qualified as MockWallet
import Data.Aeson (ToJSON (..))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import RewardWithdrawal.Scripts (rewardWithdrawalValidatorScript)
import RewardWithdrawal.Validator (LockDatum (..), RewardWithdrawalParams (..))
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
    payment address under a 'LockDatum' the script vets.
-}
data RewardWithdrawalModel = RewardWithdrawalModel
  { _registered :: Bool
  -- ^ Whether the stake credential has been registered yet
  , _owner :: Wallet
  -- ^ The party authorised to trigger the script
  , _params :: RewardWithdrawalParams
  -- ^ Cached contract parameters
  , _scriptHash :: C.ScriptHash
  -- ^ Cached script hash
  }
  deriving (Show, Eq, Generic)

fixedOwner :: Wallet
fixedOwner = MockWallet.w1

fixedParams :: RewardWithdrawalParams
fixedParams = RewardWithdrawalParams{rwpOwner = transPubKeyHash (verificationKeyHash fixedOwner)}

fixedScriptHash :: C.ScriptHash
fixedScriptHash =
  let validator = C.PlutusScript C.plutusScriptVersion (rewardWithdrawalValidatorScript fixedParams)
   in C.hashScript validator

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

  initialize =
    pure
      RewardWithdrawalModel
        { _registered = False
        , _owner = fixedOwner
        , _params = fixedParams
        , _scriptHash = fixedScriptHash
        }

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
    registerRewardWithdrawalPBT vm
    pure vm{_registered = True}
  perform vm WithdrawZero = do
    withdrawZeroPBT vm Nothing
    pure vm
  perform vm (Lock amount) = do
    withdrawZeroPBT vm (Just amount)
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

-------------------------------------------------------------------------------
-- Mockchain transactions
-------------------------------------------------------------------------------

{- | Register the model's script-guarded stake credential. On Conway, a
script-credentialed registration certificate must itself carry a script
witness, or the ledger rejects it with @MissingScriptWitnessesUTXOW@.
-}
registerRewardWithdrawalPBT
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => RewardWithdrawalModel
  -> m ()
registerRewardWithdrawalPBT RewardWithdrawalModel{_owner = owner, _params = params, _scriptHash = scriptHash} = do
  let ownerPkh = verificationKeyHash owner
      script = rewardWithdrawalValidatorScript params
      stakeCred = C.StakeCredentialByScript scriptHash
  pp <- queryProtocolParameters
  let cert = Ex.makeStakeAddressRegistrationCertificate stakeCred (C.unLedgerProtocolParameters pp ^. Ledger.ppKeyDepositL)
      registerTx =
        execBuildTx $ do
          BuildTx.addRequiredSignature ownerPkh
          BuildTx.addStakeScriptWitness cert stakeCred script ()
  _ <- tryBalanceAndSubmit mempty owner registerTx TrailingChange []
  pure ()

{- | Trigger the model's registered stake credential script via a zero-lovelace
withdrawal, optionally locking the given lovelace at the script's own payment
address under a 'LockDatum' naming the owner.
-}
withdrawZeroPBT
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => RewardWithdrawalModel
  -> Maybe Integer
  -> m ()
withdrawZeroPBT RewardWithdrawalModel{_owner = owner, _params = params, _scriptHash = scriptHash} lockAmount = do
  let ownerPkh = verificationKeyHash owner
      script = rewardWithdrawalValidatorScript params
  withdrawTxBody <-
    execBuildTxT $ do
      BuildTx.addRequiredSignature ownerPkh
      BuildTx.addScriptWithdrawal scriptHash 0 (BuildTx.buildScriptWitness script C.NoScriptDatumForStake ())
      case lockAmount of
        Nothing -> pure ()
        Just amount ->
          BuildTx.payToScriptInlineDatum
            Defaults.networkId
            scriptHash
            LockDatum{ldOwner = transPubKeyHash ownerPkh, ldAmount = amount}
            C.NoStakeAddress
            (C.lovelaceToValue (C.Coin amount))
  _ <- tryBalanceAndSubmit mempty owner withdrawTxBody TrailingChange []
  pure ()
