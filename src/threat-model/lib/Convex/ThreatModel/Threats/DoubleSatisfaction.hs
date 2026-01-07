{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Double satisfaction threats
module Convex.ThreatModel.Threats.DoubleSatisfaction (
  -- * Double satisfaction threats
  doubleSatisfactionRedirect,
  doubleSatisfactionExtraOutput,
) where

import Cardano.Api qualified as C
import Convex.ThreatModel.TxModifier (TxModifier, addOutput, changeOutputAddress, modifyOutput)

-- | Try to redirect a script output to an attacker's address
doubleSatisfactionRedirect
  :: C.IsShelleyBasedEra era
  => C.AddressInEra era
  -- ^ Attacker's address
  -> Int
  -- ^ Output index to redirect
  -> TxModifier era
doubleSatisfactionRedirect attackerAddr = changeOutputAddress attackerAddr

-- | Try to add an extra output to extract value
doubleSatisfactionExtraOutput
  :: C.IsShelleyBasedEra era
  => C.AddressInEra era
  -- ^ Attacker's address
  -> C.Value
  -- ^ Value to extract
  -> TxModifier era
doubleSatisfactionExtraOutput attackerAddr value =
  let extraOutput =
        C.TxOut
          attackerAddr
          (C.TxOutValueShelleyBased C.shelleyBasedEra (C.toMaryValue value))
          C.TxOutDatumNone
          C.ReferenceScriptNone
   in addOutput extraOutput
