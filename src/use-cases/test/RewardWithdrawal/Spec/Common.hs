{- | Fixtures and transaction builders shared by the RewardWithdrawal unit
('RewardWithdrawal.Spec.Unit') and property ('RewardWithdrawal.Spec.Prop')
tests.

Both suites exercise the same "withdraw zero trick": a script attached to a
stake credential is triggered by a zero-lovelace withdrawal, purely to run
its validation logic against the whole transaction, without spending any
script UTxO. They differ only in how they sequence the two transactions
below, so the builders themselves live here.
-}
module RewardWithdrawal.Spec.Common (
  -- * Fixtures
  fixedOwner,
  fixedOwnerPkh,
  fixedScript,
  fixedScriptHash,
  fixedStakeCredential,

  -- * Transactions
  registerCredential,
  withdrawZero,
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
import Convex.Wallet (Wallet, verificationKeyHash)
import Convex.Wallet.MockWallet qualified as MockWallet
import RewardWithdrawal.Scripts (rewardWithdrawalValidatorScript)
import RewardWithdrawal.Validator (LockDatum (..), RewardWithdrawalParams (..))

-- | The party the validator is parameterised on, and the only one it accepts.
fixedOwner :: Wallet
fixedOwner = MockWallet.w1

fixedOwnerPkh :: C.Hash C.PaymentKey
fixedOwnerPkh = verificationKeyHash fixedOwner

{- | The validator applied to 'fixedOwner'.

Deliberately a CAF: 'rewardWithdrawalValidatorScript' applies the UPLC
parameter and reserialises on every call, and the property tests build a
transaction per action, so the script must be compiled once for the whole
run rather than recomputed per transaction.
-}
fixedScript :: C.PlutusScript C.PlutusScriptV3
fixedScript = rewardWithdrawalValidatorScript RewardWithdrawalParams{rwpOwner = transPubKeyHash fixedOwnerPkh}

fixedScriptHash :: C.ScriptHash
fixedScriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV3 fixedScript)

-- | The stake credential guarded by 'fixedScript'.
fixedStakeCredential :: C.StakeCredential
fixedStakeCredential = C.StakeCredentialByScript fixedScriptHash

{- | Register 'fixedStakeCredential'. On Conway, a script-credentialed
registration certificate must itself carry a script witness, or the ledger
rejects it with @MissingScriptWitnessesUTXOW@.

That witness runs the validator, which must pass the same 'txSignedBy'
check as any other trigger, so registration always goes via 'fixedOwner' —
only 'withdrawZero' can be attributed to someone else.
-}
registerCredential
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => m ()
registerCredential = do
  pp <- queryProtocolParameters
  let cert = Ex.makeStakeAddressRegistrationCertificate fixedStakeCredential (C.unLedgerProtocolParameters pp ^. Ledger.ppKeyDepositL)
      registerTx =
        execBuildTx $ do
          BuildTx.addRequiredSignature fixedOwnerPkh
          BuildTx.addStakeScriptWitness cert fixedStakeCredential fixedScript ()
  _ <- tryBalanceAndSubmit mempty fixedOwner registerTx TrailingChange []
  pure ()

{- | Trigger the registered credential's script with a zero-lovelace
withdrawal, signed and paid for by @signer@.

When @lock@ is @Just (lockedAmount, datumAmount)@ the transaction also pays
@lockedAmount@ lovelace to the script's own payment address under a
'LockDatum' claiming @datumAmount@ — the trick doing useful work, since the
rewarding script vets outputs paid to its payment address. The validator
accepts only when the two amounts agree.
-}
withdrawZero
  :: (MonadMockchain C.ConwayEra m, MonadFail m, MonadError (BalanceTxError C.ConwayEra) m)
  => Wallet
  -> Maybe (Integer, Integer)
  -> m ()
withdrawZero signer lock = do
  withdrawTxBody <-
    execBuildTxT $ do
      BuildTx.addRequiredSignature (verificationKeyHash signer)
      BuildTx.addScriptWithdrawal fixedScriptHash 0 (BuildTx.buildScriptWitness fixedScript C.NoScriptDatumForStake ())
      case lock of
        Nothing -> pure ()
        Just (lockedAmount, datumAmount) ->
          BuildTx.payToScriptInlineDatum
            Defaults.networkId
            fixedScriptHash
            LockDatum{ldOwner = transPubKeyHash fixedOwnerPkh, ldAmount = datumAmount}
            C.NoStakeAddress
            (C.lovelaceToValue (C.Coin lockedAmount))
  _ <- tryBalanceAndSubmit mempty signer withdrawTxBody TrailingChange []
  pure ()
