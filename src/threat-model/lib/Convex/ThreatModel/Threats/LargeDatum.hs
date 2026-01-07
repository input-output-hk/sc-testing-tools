{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Large datum attack threats
module Convex.ThreatModel.Threats.LargeDatum (
  -- * Large datum attacks
  largeDatumAttacks,
  enlargeDatumTo,
) where

import Cardano.Api qualified as C
import Convex.ThreatModel.TxModifier (TxModifier, enlargeDatum)

-- | Standard set of large datum attacks
largeDatumAttacks :: C.IsShelleyBasedEra era => Int -> [TxModifier era]
largeDatumAttacks outputIdx =
  [ enlargeDatumTo outputIdx 10_000 -- 10KB
  , enlargeDatumTo outputIdx 50_000 -- 50KB
  , enlargeDatumTo outputIdx 100_000 -- 100KB (near max tx size)
  ]

-- | Enlarge datum at output index to target size
enlargeDatumTo
  :: C.IsShelleyBasedEra era
  => Int
  -- ^ Output index
  -> Int
  -- ^ Target size in bytes
  -> TxModifier era
enlargeDatumTo = enlargeDatum
