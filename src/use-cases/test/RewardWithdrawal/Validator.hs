{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module RewardWithdrawal.Validator where

import Data.Eq (Eq)
import GHC.Show (Show)
import PlutusLedgerApi.V3 (PubKeyHash)
import PlutusLedgerApi.V3.Contexts (ScriptContext (..), txSignedBy)
import PlutusTx (makeLift)
import PlutusTx.Builtins.Internal (unitval)
import PlutusTx.IsData.Class (UnsafeFromData (unsafeFromBuiltinData))
import PlutusTx.Prelude (BuiltinData, BuiltinUnit, not, otherwise, traceError)

-- | Compile-time parameters baked into the script at compilation.
newtype RewardWithdrawalParams = RewardWithdrawalParams
  { rwpOwner :: PubKeyHash
  -- ^ The only party allowed to trigger this reward-withdrawal script.
  }
  deriving stock (Show, Eq)

makeLift ''RewardWithdrawalParams

{-# INLINEABLE mkValidator #-}

{- | A reward-account script meant to be triggered via a zero-lovelace
  withdrawal (the "withdraw zero trick"): attaching it to a registered stake
  credential lets a transaction invoke arbitrary validation logic without
  spending or creating any UTxO. Here the logic is just "the owner signed".

  Registering the credential also invokes this script (as a certifying
  script rather than a rewarding one), so the check is purpose-agnostic.
-}
mkValidator :: RewardWithdrawalParams -> BuiltinData -> BuiltinUnit
mkValidator
  params
  ( unsafeFromBuiltinData ->
      ScriptContext
        { scriptContextTxInfo = txI
        }
    )
    | not (txSignedBy txI owner) = traceError "OSM" -- Owner's signature missing
    | otherwise = unitval
   where
    owner :: PubKeyHash
    owner = rwpOwner params

{-# INLINEABLE validator #-}
validator :: RewardWithdrawalParams -> BuiltinData -> BuiltinUnit
validator = mkValidator
