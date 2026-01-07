{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | High-level DSL for writing threat model tests
module Convex.ThreatModel.DSL (
  -- * High-level threat testing
  shouldNotValidate,
  shouldValidate,
  allShouldFail,
  anyShouldFail,
  threat,
) where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (MonadIO)
import Convex.Class (ValidationError)
import Convex.ThreatModel.Core (
  ThreatModelT,
  ThreatResult (..),
  getOriginalTx,
  recordThreat,
 )
import Convex.ThreatModel.TxModifier (TxModifier)
import Convex.ThreatModel.Validation (applyModifierAndRebuild, revalidateTx)
import Convex.Wallet (Wallet)
import Convex.Wallet qualified as Wallet
import Data.Text (Text)
import Data.Text qualified as Text

-- | Assert that a modified transaction should not validate
shouldNotValidate
  :: (MonadIO m, C.IsShelleyBasedEra era)
  => Text
  -- ^ Description of the threat
  -> TxModifier era
  -- ^ Modification to apply
  -> (ValidationError era -> Bool)
  -- ^ Predicate to check the validation error
  -> ThreatModelT era m ()
shouldNotValidate description modifier errorPredicate = do
  originalTx <- getOriginalTx

  -- Apply modifier and rebuild transaction
  modifyResult <- applyModifierAndRebuild modifier originalTx Wallet.w1 []

  case modifyResult of
    Left err -> do
      -- Could not build modified transaction
      recordThreat $
        ThreatModificationFailed
          { trDescription = description
          , trError = err
          }
    Right modifiedTx -> do
      -- Validate the modified transaction
      validationResult <- revalidateTx modifiedTx

      case validationResult of
        Left validationError ->
          if errorPredicate validationError
            then
              -- Threat was properly detected
              recordThreat $
                ThreatDetected
                  { trDescription = description
                  , trOriginalTx = originalTx
                  , trModifiedTx = modifiedTx
                  , trValidationError = Text.pack (show validationError)
                  }
            else
              -- Wrong error type
              recordThreat $
                ThreatNotDetected
                  { trDescription = description <> " (wrong error type)"
                  , trOriginalTx = originalTx
                  , trModifiedTx = modifiedTx
                  , trExpectedError = "Expected error matching predicate, got: " <> Text.pack (show validationError)
                  }
        Right () ->
          -- Threat was not detected (vulnerability!)
          recordThreat $
            ThreatNotDetected
              { trDescription = description
              , trOriginalTx = originalTx
              , trModifiedTx = modifiedTx
              , trExpectedError = "Transaction should have been rejected"
              }

-- | Assert that an unmodified transaction still validates
shouldValidate
  :: (MonadIO m, C.IsShelleyBasedEra era)
  => Text
  -- ^ Description
  -> C.Tx era
  -- ^ Transaction to validate
  -> ThreatModelT era m ()
shouldValidate description tx = do
  validationResult <- revalidateTx tx

  case validationResult of
    Left validationError ->
      recordThreat $
        ThreatNotDetected
          { trDescription = description <> " (original tx should validate)"
          , trOriginalTx = tx
          , trModifiedTx = tx
          , trExpectedError = "Original transaction should validate, got error: " <> Text.pack (show validationError)
          }
    Right () ->
      -- Original transaction still validates (good)
      pure ()

-- | Assert that all modifications should fail validation
allShouldFail
  :: (MonadIO m, C.IsShelleyBasedEra era)
  => Text
  -- ^ Description
  -> [TxModifier era]
  -- ^ List of modifications
  -> (ValidationError era -> Bool)
  -- ^ Error predicate
  -> ThreatModelT era m ()
allShouldFail baseDescription modifiers errorPredicate = do
  mapM_ testModifier (zip [1 ..] modifiers)
 where
  testModifier (idx, modifier) =
    shouldNotValidate
      (baseDescription <> " [" <> Text.pack (show (idx :: Int)) <> "]")
      modifier
      errorPredicate

-- | Assert that at least one modification should fail validation
anyShouldFail
  :: (MonadIO m, C.IsShelleyBasedEra era)
  => Text
  -- ^ Description
  -> [TxModifier era]
  -- ^ List of modifications
  -> (ValidationError era -> Bool)
  -- ^ Error predicate
  -> ThreatModelT era m ()
anyShouldFail baseDescription modifiers errorPredicate = do
  -- For now, just test all of them like allShouldFail
  -- A more sophisticated implementation would check if ANY failed
  allShouldFail baseDescription modifiers errorPredicate

-- | Convenient alias for shouldNotValidate
threat
  :: (MonadIO m, C.IsShelleyBasedEra era)
  => Text
  -> TxModifier era
  -> (ValidationError era -> Bool)
  -> ThreatModelT era m ()
threat = shouldNotValidate
