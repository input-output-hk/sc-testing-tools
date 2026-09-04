{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RewardWithdrawal.Spec.Unit where

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
import Convex.MockChain.Utils (mockchainFails, mockchainSucceeds)
import Convex.PlutusLedger.V1 (transPubKeyHash)
import Convex.Tasty.HUnit (testCase)
import Convex.Utils (failOnError)
import Convex.Wallet (Wallet, verificationKeyHash)
import Convex.Wallet.MockWallet qualified as MockWallet
import RewardWithdrawal.Scripts (rewardWithdrawalValidatorScript)
import RewardWithdrawal.Validator (RewardWithdrawalParams (..))
import Test.Tasty (TestTree, testGroup)

-------------------------------------------------------------------------------
-- Unit tests for the RewardWithdrawal script
--
-- These exercise the "withdraw zero trick": a script attached to a stake
-- credential is triggered by a zero-lovelace withdrawal, purely to run its
-- validation logic against the whole transaction, without spending or
-- creating any UTxO.
-------------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup
    "unit tests"
    [ testCase
        "owner triggers the reward-withdrawal script via a zero-lovelace withdrawal"
        (mockchainSucceeds $ failOnError ownerWithdrawsZero)
    , testCase
        "Fail: zero-lovelace withdrawal without the owner's signature"
        (mockchainFails (failOnError missingOwnerSignature) (\_ -> pure ()))
    ]

{- | Register the given script-guarded stake credential. On Conway, a
script-credentialed registration certificate must itself carry a script
witness, or the ledger rejects it with @MissingScriptWitnessesUTXOW@.
-}
registerRewardWithdrawalCredential
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => Wallet
  -> C.Hash C.PaymentKey
  -> C.PlutusScript C.PlutusScriptV3
  -> C.StakeCredential
  -> m ()
registerRewardWithdrawalCredential payer ownerPkh script stakeCred = do
  pp <- queryProtocolParameters
  let cert = Ex.makeStakeAddressRegistrationCertificate stakeCred (C.unLedgerProtocolParameters pp ^. Ledger.ppKeyDepositL)
      registerTx =
        execBuildTx $ do
          BuildTx.addRequiredSignature ownerPkh
          BuildTx.addStakeScriptWitness cert stakeCred script ()
  _ <- tryBalanceAndSubmit mempty payer registerTx TrailingChange []
  pure ()

-------------------------------------------------------------------------------
-- Scenario: the stake credential's script is registered, then triggered by a
-- withdrawal of 0 lovelace in a transaction signed by the owner named in the
-- script's parameters. The script only checks 'txSignedBy', so it accepts.
-------------------------------------------------------------------------------
ownerWithdrawsZero
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => m ()
ownerWithdrawsZero = do
  let owner = MockWallet.w1
      ownerPkh = verificationKeyHash owner
      params = RewardWithdrawalParams{rwpOwner = transPubKeyHash ownerPkh}
      script = rewardWithdrawalValidatorScript params
      scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV3 script)
      stakeCred = C.StakeCredentialByScript scriptHash

  registerRewardWithdrawalCredential owner ownerPkh script stakeCred

  withdrawTxBody <-
    execBuildTxT $ do
      BuildTx.addRequiredSignature ownerPkh
      BuildTx.addScriptWithdrawal scriptHash 0 (BuildTx.buildScriptWitness script C.NoScriptDatumForStake ())

  _ <- tryBalanceAndSubmit mempty owner withdrawTxBody TrailingChange []
  pure ()

-------------------------------------------------------------------------------
-- Scenario: same as above, but the transaction is signed only by an
-- outsider, not by the owner named in the script's parameters. 'txSignedBy'
-- fails and the validator rejects with "OSM".
-------------------------------------------------------------------------------
missingOwnerSignature
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => m ()
missingOwnerSignature = do
  let owner = MockWallet.w1
      ownerPkh = verificationKeyHash owner
      outsider = MockWallet.w2
      outsiderPkh = verificationKeyHash outsider
      params = RewardWithdrawalParams{rwpOwner = transPubKeyHash ownerPkh}
      script = rewardWithdrawalValidatorScript params
      scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV3 script)
      stakeCred = C.StakeCredentialByScript scriptHash

  -- Registration is itself script-witnessed and must pass the same
  -- 'txSignedBy' check, so register via the owner: only the later
  -- withdrawal is meant to be missing the owner's signature.
  registerRewardWithdrawalCredential owner ownerPkh script stakeCred

  withdrawTxBody <-
    execBuildTxT $ do
      -- Intentionally sign only with a non-owner.
      BuildTx.addRequiredSignature outsiderPkh
      BuildTx.addScriptWithdrawal scriptHash 0 (BuildTx.buildScriptWitness script C.NoScriptDatumForStake ())

  _ <- tryBalanceAndSubmit mempty outsider withdrawTxBody TrailingChange []
  pure ()
