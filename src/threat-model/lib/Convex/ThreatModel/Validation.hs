{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Transaction validation for threat model testing
module Convex.ThreatModel.Validation (
  -- * Validation
  revalidateTx,
  applyModifierAndRebuild,

  -- * Validation predicates
  isPredicateFailure,
  isPhase2Failure,
  isMaxTxSizeError,
  isExUnitsExceeded,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Shelley.API qualified as Ledger
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Convex.Class (
  MockChainState,
  ValidationError (..),
  env,
  poolState,
 )
import Convex.CoinSelection (balanceForWallet)
import Convex.MockChain qualified as MockChain
import Convex.MockChain.Defaults qualified as Defaults
import Convex.NodeParams (NodeParams)
import Convex.ThreatModel.Core (
  ThreatModelT,
  getMockchainState,
  getNodeParams,
 )
import Convex.ThreatModel.TxModifier (TxModifier, applyModifier)
import Convex.Wallet (Wallet)
import Convex.Wallet qualified as Wallet
import Control.Lens ((^.))
import Data.Text (Text)
import Data.Text qualified as Text

-- | Re-validate a transaction against the current mockchain state
-- without modifying the actual mockchain
revalidateTx
  :: (MonadIO m, C.IsShelleyBasedEra era)
  => C.Tx era
  -> ThreatModelT era m (Either (ValidationError era) ())
revalidateTx tx = do
  state <- getMockchainState
  params <- getNodeParams

  -- Apply transaction to a copy of the state (read-only validation)
  pure $ case MockChain.applyTransaction params state tx of
    Left err -> Left err
    Right _ -> Right ()

-- | Apply a modifier to a transaction, rebuild it, and return the new transaction
applyModifierAndRebuild
  :: (MonadIO m, C.IsShelleyBasedEra era)
  => TxModifier era
  -> C.Tx era
  -> Wallet
  -> [C.ShelleyWitnessSigningKey]
  -> ThreatModelT era m (Either Text (C.Tx era))
applyModifierAndRebuild modifier tx wallet keys = do
  -- Apply the modifier to get modified TxBodyContent
  case applyModifier modifier tx of
    Left err -> pure $ Left err
    Right modifiedBodyContent -> do
      -- Get current state for balancing
      state <- getMockchainState

      -- Get wallet UTxOs
      let utxos = Wallet.walletUtxo wallet
          mempoolState = state ^. poolState
          utxoFromMempool = mempoolState ^. Ledger.lsUTxOState . Ledger._UTxOState . Ledger._1

      -- Balance the transaction
      case balanceForWallet C.shelleyBasedEra Defaults.networkId (error "pparams") utxos modifiedBodyContent of
        Left balanceErr -> pure $ Left $ "Balance error: " <> Text.pack (show balanceErr)
        Right balancedBodyContent -> do
          -- Create transaction body
          case C.createAndValidateTransactionBody C.shelleyBasedEra balancedBodyContent of
            Left txBodyErr -> pure $ Left $ "TxBody error: " <> Text.pack (show txBodyErr)
            Right txBody -> do
              -- Sign the transaction
              let keyWitnesses = map (C.makeShelleyKeyWitness C.shelleyBasedEra txBody) keys
                  signedTx = C.makeSignedTransaction keyWitnesses txBody
              pure $ Right signedTx

-- | Check if error is a predicate failure (phase-2 validation failure)
isPredicateFailure :: ValidationError era -> Bool
isPredicateFailure = \case
  PredicateFailures{} -> True
  _ -> False

-- | Check if error is a phase-2 failure
isPhase2Failure :: ValidationError era -> Bool
isPhase2Failure = isPredicateFailure

-- | Check if error is due to max transaction size exceeded
isMaxTxSizeError :: ValidationError era -> Bool
isMaxTxSizeError = \case
  PredicateFailures errs -> any checkMaxSize errs
  _ -> False
 where
  checkMaxSize err = "MaxTxSize" `Text.isInfixOf` Text.pack (show err)

-- | Check if error is due to execution units exceeded
isExUnitsExceeded :: ValidationError era -> Bool
isExUnitsExceeded = \case
  PredicateFailures errs -> any checkExUnits errs
  _ -> False
 where
  checkExUnits err = "ExUnits" `Text.isInfixOf` Text.pack (show err)
