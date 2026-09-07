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
import Convex.PlutusLedger.V1 (transPubKeyHash)
import Convex.TestingInterface (RunOptions, TestingInterface (..), ThreatModelsFor (..), elements, propRunActionsWithOptions)
import Convex.TestingInterface.Trace.RedeemerTag (autoRedeemerTag)
import Convex.ThreatModel.SignatoryRemoval (signatoryRemoval)
import Convex.Wallet (Wallet, verificationKeyHash)
import Convex.Wallet.MockWallet qualified as MockWallet
import Data.Aeson (ToJSON (..))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import RewardWithdrawal.Scripts (rewardWithdrawalValidatorScript)
import RewardWithdrawal.Validator (RewardWithdrawalParams (..))
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
    trick").
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
    deriving (Show, Eq)

  initialize =
    pure
      RewardWithdrawalModel
        { _registered = False
        , _owner = fixedOwner
        , _params = fixedParams
        , _scriptHash = fixedScriptHash
        }

  arbitraryAction _ = elements [Register, WithdrawZero]

  precondition vm Register = not (_registered vm)
  precondition vm WithdrawZero = _registered vm

  perform vm Register = do
    registerRewardWithdrawalPBT vm
    pure vm{_registered = True}
  perform vm WithdrawZero = do
    withdrawZeroPBT vm
    pure vm

  validate _vm = pure True
  monitoring _ _ = id
  redeemerTagger = autoRedeemerTag (Proxy @())

instance ThreatModelsFor RewardWithdrawalModel where
  threatModels = [signatoryRemoval]

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

-- | Trigger the model's registered stake credential script via a zero-lovelace withdrawal.
withdrawZeroPBT
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => RewardWithdrawalModel
  -> m ()
withdrawZeroPBT RewardWithdrawalModel{_owner = owner, _params = params, _scriptHash = scriptHash} = do
  let ownerPkh = verificationKeyHash owner
      script = rewardWithdrawalValidatorScript params
  withdrawTxBody <-
    execBuildTxT $ do
      BuildTx.addRequiredSignature ownerPkh
      BuildTx.addScriptWithdrawal scriptHash 0 (BuildTx.buildScriptWitness script C.NoScriptDatumForStake ())
  _ <- tryBalanceAndSubmit mempty owner withdrawTxBody TrailingChange []
  pure ()
