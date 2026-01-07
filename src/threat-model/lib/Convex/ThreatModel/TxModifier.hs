{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Transaction modifiers for threat model testing
module Convex.ThreatModel.TxModifier (
  -- * TxModifier type
  TxModifier (..),
  applyModifier,

  -- * Creating modifiers
  modifyTxBody,
  modifyTxBodyContent,

  -- * Output modifiers
  removeOutput,
  addOutput,
  changeOutputValue,
  changeOutputAddress,
  changeOutputDatum,
  modifyOutput,

  -- * Input modifiers
  removeInput,
  addInput,
  modifyInputs,

  -- * Datum modifiers
  enlargeDatum,

  -- * Value modifiers
  inflateOutputValue,

  -- * Combining modifiers
  noModification,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens ((%~), (&), (.~), (^.))
import Convex.CardanoApi.Lenses qualified as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import PlutusLedgerApi.V1.Value qualified as PV1

-- | A transaction modifier represents a transformation on a transaction
newtype TxModifier era = TxModifier
  { unTxModifier :: C.Tx era -> Either Text (C.TxBodyContent C.BuildTx era)
  }

-- | Apply a modifier to a transaction
applyModifier
  :: TxModifier era
  -> C.Tx era
  -> Either Text (C.TxBodyContent C.BuildTx era)
applyModifier = unTxModifier

-- | Compose two modifiers (apply first, then second)
instance Semigroup (TxModifier era) where
  TxModifier f <> TxModifier g = TxModifier $ \tx -> do
    bodyContent1 <- f tx
    -- For chaining, we need to convert back to Tx, but for now we'll apply both to the original
    -- This is a simplification - in practice, you'd need to rebuild the tx
    bodyContent2 <- g tx
    -- Apply second modifier's changes to first modifier's result
    pure bodyContent2

-- | Identity modifier
instance Monoid (TxModifier era) where
  mempty = noModification

-- | Create a modifier from a transaction body content transformation
modifyTxBodyContent
  :: (C.TxBodyContent C.BuildTx era -> Either Text (C.TxBodyContent C.BuildTx era))
  -> TxModifier era
modifyTxBodyContent f = TxModifier $ \tx ->
  case tx of
    C.Tx (C.ShelleyTxBody _ _ _ bodyContent _ _) _ ->
      f bodyContent

-- | Create a modifier from a transaction body transformation
modifyTxBody
  :: (C.TxBody era -> Either Text (C.TxBodyContent C.BuildTx era))
  -> TxModifier era
modifyTxBody f = TxModifier $ \tx ->
  case tx of
    C.Tx body _ -> f body

-- | No modification (identity)
noModification :: TxModifier era
noModification = TxModifier $ \tx ->
  case tx of
    C.Tx (C.ShelleyTxBody _ _ _ bodyContent _ _) _ ->
      Right bodyContent

-- | Remove an output at the given index
removeOutput :: C.IsShelleyBasedEra era => Int -> TxModifier era
removeOutput idx = modifyTxBodyContent $ \bodyContent ->
  let outputs = bodyContent ^. L.txOuts
      (before, after) = splitAt idx outputs
   in if idx < 0 || idx >= length outputs
        then Left $ "Output index out of bounds: " <> Text.pack (show idx)
        else
          Right $
            bodyContent
              & L.txOuts .~ (before ++ drop 1 after)

-- | Add an output
addOutput :: C.IsShelleyBasedEra era => C.TxOut C.CtxTx era -> TxModifier era
addOutput output = modifyTxBodyContent $ \bodyContent ->
  Right $ bodyContent & L.txOuts %~ (++ [output])

-- | Change the value of an output at the given index
changeOutputValue
  :: C.IsShelleyBasedEra era
  => Int
  -> C.Value
  -> TxModifier era
changeOutputValue idx newValue = modifyOutput idx $ \(C.TxOut addr _ datum refScript) ->
  C.TxOut addr (C.TxOutValueShelleyBased C.shelleyBasedEra (C.toMaryValue newValue)) datum refScript

-- | Change the address of an output at the given index
changeOutputAddress
  :: C.IsShelleyBasedEra era
  => Int
  -> C.AddressInEra era
  -> TxModifier era
changeOutputAddress idx newAddr = modifyOutput idx $ \(C.TxOut _ value datum refScript) ->
  C.TxOut newAddr value datum refScript

-- | Change the datum of an output at the given index
changeOutputDatum
  :: C.IsShelleyBasedEra era
  => Int
  -> C.TxOutDatum C.CtxTx era
  -> TxModifier era
changeOutputDatum idx newDatum = modifyOutput idx $ \(C.TxOut addr value _ refScript) ->
  C.TxOut addr value newDatum refScript

-- | Modify an output at the given index
modifyOutput
  :: C.IsShelleyBasedEra era
  => Int
  -> (C.TxOut C.CtxTx era -> C.TxOut C.CtxTx era)
  -> TxModifier era
modifyOutput idx f = modifyTxBodyContent $ \bodyContent ->
  let outputs = bodyContent ^. L.txOuts
      (before, rest) = splitAt idx outputs
   in case rest of
        [] -> Left $ "Output index out of bounds: " <> Text.pack (show idx)
        (output : after) ->
          Right $
            bodyContent
              & L.txOuts .~ (before ++ [f output] ++ after)

-- | Remove an input
removeInput :: C.IsShelleyBasedEra era => C.TxIn -> TxModifier era
removeInput txIn = modifyTxBodyContent $ \bodyContent ->
  let inputs = bodyContent ^. L.txIns
      filtered = filter (\(i, _) -> i /= txIn) inputs
   in if length filtered == length inputs
        then Left $ "Input not found: " <> Text.pack (show txIn)
        else Right $ bodyContent & L.txIns .~ filtered

-- | Add an input
addInput
  :: C.IsShelleyBasedEra era
  => C.TxIn
  -> C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era)
  -> TxModifier era
addInput txIn witness = modifyTxBodyContent $ \bodyContent ->
  Right $ bodyContent & L.txIns %~ ((txIn, witness) :)

-- | Modify the inputs
modifyInputs
  :: C.IsShelleyBasedEra era
  => ([(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))] -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))])
  -> TxModifier era
modifyInputs f = modifyTxBodyContent $ \bodyContent ->
  Right $ bodyContent & L.txIns %~ f

-- | Enlarge a datum at the given output index to the target size (in bytes)
-- by padding with zero bytes
enlargeDatum
  :: C.IsShelleyBasedEra era
  => Int
  -- ^ Output index
  -> Int
  -- ^ Target size in bytes
  -> TxModifier era
enlargeDatum idx targetSize = modifyOutput idx $ \txOut ->
  case txOut of
    C.TxOut addr value (C.TxOutDatumInline babbageOnwards scriptData) refScript ->
      let padding = replicate targetSize (0 :: Integer)
          paddedData = C.fromPlutusData $ PV1.toData padding
          newScriptData = C.unsafeHashableScriptData paddedData
       in C.TxOut addr value (C.TxOutDatumInline babbageOnwards newScriptData) refScript
    _ -> txOut -- If not inline datum, don't modify

-- | Inflate the value of an output by adding the given lovelace amount
inflateOutputValue
  :: C.IsShelleyBasedEra era
  => Int
  -- ^ Output index
  -> C.Quantity
  -- ^ Additional lovelace to add
  -> TxModifier era
inflateOutputValue idx additionalLovelace = modifyOutput idx $ \(C.TxOut addr value datum refScript) ->
  let newValue = case value of
        C.TxOutValueShelleyBased sbe v ->
          C.TxOutValueShelleyBased sbe (C.toMaryValue $ C.fromMaryValue v <> C.lovelaceToValue (C.Lovelace additionalLovelace))
        C.TxOutValueByron l ->
          C.TxOutValueByron (l + C.Lovelace additionalLovelace)
   in C.TxOut addr newValue datum refScript
