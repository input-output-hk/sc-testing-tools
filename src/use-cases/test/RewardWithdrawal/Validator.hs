{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module RewardWithdrawal.Validator where

import Data.Eq (Eq)
import GHC.Show (Show)
import PlutusLedgerApi.V1.Value (lovelaceValue)
import PlutusLedgerApi.V3 (
  Address (addressCredential),
  Credential (ScriptCredential),
  Datum (getDatum),
  Lovelace (Lovelace),
  OutputDatum (OutputDatum),
  PubKeyHash,
  ScriptHash,
  TxCert (TxCertRegStaking),
  TxInfo (txInfoOutputs),
  TxOut (txOutAddress, txOutDatum, txOutValue),
 )
import PlutusLedgerApi.V3.Contexts (ScriptContext (..), ScriptInfo (..), txSignedBy)
import PlutusTx (makeLift, unstableMakeIsData)
import PlutusTx.Builtins.Internal (unitval)
import PlutusTx.IsData.Class (FromData (fromBuiltinData), UnsafeFromData (unsafeFromBuiltinData))
import PlutusTx.List (all, filter)
import PlutusTx.Prelude (Bool (..), BuiltinData, BuiltinUnit, Integer, Maybe (..), not, otherwise, traceError, (&&), (==), (>))

-- | Compile-time parameters baked into the script at compilation.
newtype RewardWithdrawalParams = RewardWithdrawalParams
  { rwpOwner :: PubKeyHash
  -- ^ The only party allowed to trigger this reward-withdrawal script.
  }
  deriving stock (Show, Eq)

makeLift ''RewardWithdrawalParams

{- | Datum of a UTxO locked at the script's own payment address. The script
  never spends these; it only vets them on the way in (see 'mkValidator').
-}
data LockDatum = LockDatum
  { ldOwner :: PubKeyHash
  -- ^ Must be the owner named in the script's parameters
  , ldAmount :: Integer
  -- ^ The lovelace locked; must be positive and match the output's value exactly
  }
  deriving stock (Show, Eq)

unstableMakeIsData ''LockDatum

{-# INLINEABLE mkValidator #-}

{- | A reward-account script meant to be triggered via a zero-lovelace
  withdrawal (the "withdraw zero trick"): attaching it to a registered stake
  credential lets a transaction invoke arbitrary validation logic without
  spending or creating any UTxO. The logic here is:

  * the owner signed, and
  * every output paid to the script's own payment address carries an inline
    'LockDatum' naming that owner, whose amount is positive and equals the
    output's value (so the output holds exactly that much ADA and nothing
    else).

  The second rule is what makes this a meaningful withdraw-zero validator:
  the transaction spends no script input, yet the datum and value of the
  outputs it creates at the script address are still checked - by the
  rewarding script rather than by a spending one.

  Registering the credential also invokes this script (as a certifying
  script rather than a rewarding one), so both purposes are handled.
-}
mkValidator :: RewardWithdrawalParams -> BuiltinData -> BuiltinUnit
mkValidator
  params
  ( unsafeFromBuiltinData ->
      ScriptContext
        { scriptContextTxInfo = txI
        , scriptContextScriptInfo = info
        }
    )
    | not (txSignedBy txI owner) = traceError "OSM" -- Owner's signature missing
    | not (all validLockOutput (ownOutputs ownHash txI)) = traceError "BLO" -- Bad lock output
    | otherwise = unitval
   where
    owner :: PubKeyHash
    owner = rwpOwner params

    ownHash :: ScriptHash
    ownHash = case info of
      RewardingScript (ScriptCredential h) -> h
      CertifyingScript _ (TxCertRegStaking (ScriptCredential h) _) -> h
      _ -> traceError "PUR" -- Unexpected script purpose
    validLockOutput :: TxOut -> Bool
    validLockOutput o = case txOutDatum o of
      OutputDatum (getDatum -> fromBuiltinData -> Just LockDatum{ldOwner, ldAmount}) ->
        ldOwner == owner && ldAmount > 0 && txOutValue o == lovelaceValue (Lovelace ldAmount)
      _ -> False

{-# INLINEABLE ownOutputs #-}

-- | The transaction outputs paid to the given script hash as payment credential.
ownOutputs :: ScriptHash -> TxInfo -> [TxOut]
ownOutputs h txI = filter isOwn (txInfoOutputs txI)
 where
  isOwn o = case addressCredential (txOutAddress o) of
    ScriptCredential h' -> h' == h
    _ -> False

{-# INLINEABLE validator #-}
validator :: RewardWithdrawalParams -> BuiltinData -> BuiltinUnit
validator = mkValidator
