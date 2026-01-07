{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Large value attack threats
module Convex.ThreatModel.Threats.LargeValue (
  -- * Value manipulation threats
  valueInflationAttacks,
  inflateValue,
) where

import Cardano.Api qualified as C
import Convex.ThreatModel.TxModifier (TxModifier, inflateOutputValue)

-- | Standard set of value inflation attacks
valueInflationAttacks :: C.IsShelleyBasedEra era => Int -> [TxModifier era]
valueInflationAttacks outputIdx =
  [ inflateValue outputIdx 1_000_000_000 -- Add 1000 Ada
  , inflateValue outputIdx 45_000_000_000_000 -- Add 45M Ada (max supply)
  ]

-- | Inflate the value at an output index
inflateValue
  :: C.IsShelleyBasedEra era
  => Int
  -- ^ Output index
  -> C.Quantity
  -- ^ Additional lovelace to add
  -> TxModifier era
inflateValue = inflateOutputValue
