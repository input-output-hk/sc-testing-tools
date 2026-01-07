{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | State transition threats for state machine contracts
module Convex.ThreatModel.Threats.StateTransition (
  -- * Generic state transition threats
  invalidStateTransition,

  -- * Datum manipulation
  changeInlineDatum,
) where

import Cardano.Api qualified as C
import Convex.ThreatModel.TxModifier (TxModifier, modifyOutput)
import PlutusLedgerApi.V1 qualified as PV1

-- | Create a threat that modifies the output datum to an invalid state
invalidStateTransition
  :: forall state era
   . (PV1.ToData state, C.IsShelleyBasedEra era)
  => state
  -- ^ Invalid next state
  -> Int
  -- ^ Output index containing the state
  -> TxModifier era
invalidStateTransition invalidState outputIdx =
  changeInlineDatum outputIdx invalidState

-- | Change the inline datum at a specific output index
changeInlineDatum
  :: forall datum era
   . (PV1.ToData datum, C.IsShelleyBasedEra era)
  => Int
  -- ^ Output index
  -> datum
  -- ^ New datum value
  -> TxModifier era
changeInlineDatum idx newDatum = modifyOutput idx $ \txOut ->
  case txOut of
    C.TxOut addr value (C.TxOutDatumInline babbageOnwards _) refScript ->
      let newScriptData = C.unsafeHashableScriptData $ C.fromPlutusData $ PV1.toData newDatum
       in C.TxOut addr value (C.TxOutDatumInline babbageOnwards newScriptData) refScript
    _ -> txOut -- If not inline datum, don't modify
