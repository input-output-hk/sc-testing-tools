{-# LANGUAGE NumericUnderscores #-}
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
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (mockchainFails, mockchainSucceeds)
import Convex.PlutusLedger.V1 (transPubKeyHash)
import Convex.Tasty.HUnit (testCase)
import Convex.Utils (failOnError)
import Convex.Wallet (Wallet, verificationKeyHash)
import Convex.Wallet.MockWallet qualified as MockWallet
import RewardWithdrawal.Scripts (rewardWithdrawalValidatorScript)
import RewardWithdrawal.Validator (LockDatum (..), RewardWithdrawalParams (..))
import Test.Tasty (TestTree, testGroup)

-------------------------------------------------------------------------------
-- Unit tests for the RewardWithdrawal script
--
-- These exercise the "withdraw zero trick": a script attached to a stake
-- credential is triggered by a zero-lovelace withdrawal, purely to run its
-- validation logic against the whole transaction, without spending any
-- script UTxO. The lock scenarios show the trick doing useful work: the
-- rewarding script vets outputs paid to its own payment address.
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
    , testCase
        "owner locks ADA at the script address under a matching lock datum"
        (mockchainSucceeds $ failOnError (ownerLocks 10_000_000 10_000_000))
    , testCase
        "Fail: lock datum amount does not match the output's value"
        (mockchainFails (failOnError (ownerLocks 10_000_000 9_000_000)) (\_ -> pure ()))
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

{- | The shared scenario: the owner's (MockWallet.w1's) script-guarded stake
credential is registered, then a zero-lovelace withdrawal triggering the
script is signed and paid for by @signer@, optionally also locking
@lockedAmount@ lovelace at the script's own payment address under a
'LockDatum' claiming @datumAmount@.
-}
withdrawZeroScenario
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => Wallet
  -> Maybe (Integer, Integer)
  -> m ()
withdrawZeroScenario signer lock = do
  let owner = MockWallet.w1
      ownerPkh = verificationKeyHash owner
      params = RewardWithdrawalParams{rwpOwner = transPubKeyHash ownerPkh}
      script = rewardWithdrawalValidatorScript params
      scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV3 script)
      stakeCred = C.StakeCredentialByScript scriptHash

  -- Registration is itself script-witnessed and must pass the same
  -- 'txSignedBy' check, so it always goes via the owner: only the
  -- withdrawal below is attributed to @signer@.
  registerRewardWithdrawalCredential owner ownerPkh script stakeCred

  withdrawTxBody <-
    execBuildTxT $ do
      BuildTx.addRequiredSignature (verificationKeyHash signer)
      BuildTx.addScriptWithdrawal scriptHash 0 (BuildTx.buildScriptWitness script C.NoScriptDatumForStake ())
      case lock of
        Nothing -> pure ()
        Just (lockedAmount, datumAmount) ->
          BuildTx.payToScriptInlineDatum
            Defaults.networkId
            scriptHash
            LockDatum{ldOwner = transPubKeyHash ownerPkh, ldAmount = datumAmount}
            C.NoStakeAddress
            (C.lovelaceToValue (C.Coin lockedAmount))

  _ <- tryBalanceAndSubmit mempty signer withdrawTxBody TrailingChange []
  pure ()

-- | The owner triggers the script. It only checks 'txSignedBy', so it accepts.
ownerWithdrawsZero
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => m ()
ownerWithdrawsZero = withdrawZeroScenario MockWallet.w1 Nothing

{- | The owner's withdrawal also pays @lockedAmount@ to the script's own
payment address under a 'LockDatum' claiming @datumAmount@. The validator
accepts only when the two agree.
-}
ownerLocks
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => Integer
  -> Integer
  -> m ()
ownerLocks lockedAmount datumAmount = withdrawZeroScenario MockWallet.w1 (Just (lockedAmount, datumAmount))

{- | The withdrawal is signed only by an outsider, not by the owner named in
the script's parameters. 'txSignedBy' fails and the validator rejects with
"OSM".
-}
missingOwnerSignature
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => m ()
missingOwnerSignature = withdrawZeroScenario MockWallet.w2 Nothing
