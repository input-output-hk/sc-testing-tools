{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Convex.TestingInterface.Trace.TxSummary (
  summarizeTx,
  summarizeTxBody,
  renderAddress,
  toValueSummary,
  renderAssetName,
  renderDatum,
) where

import Cardano.Api qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger (AsIx (AsIx))
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger (Redeemers (Redeemers))
import Cardano.Ledger.Conway.Scripts qualified as Conway (ConwayPlutusPurpose (ConwaySpending))
import Convex.TestingInterface.Trace (
  AddressLabeler (..),
  AddressType (..),
  AssetSummary (..),
  RedeemerTag (..),
  RedeemerTagger (..),
  TxInputSummary (..),
  TxOutputSummary (..),
  TxSummary (..),
  ValueSummary (..),
 )
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Word (Word32)
import GHC.Exts (toList)
import PlutusTx (Data (..))

{- | Summarize a full transaction, resolving inputs from the given UTxO set.
The 'RedeemerTagger' is applied to each script input's parsed redeemer
'Data' to optionally produce Tier 2 ('tisRedeemerKind' /
'tisRedeemerPayload') labels. Pass 'mempty' for Tier 1-only behaviour. The
'AddressLabeler' is applied to every address's credential hash to produce
'tisAddressLabel' / 'tosAddressLabel'.
-}
summarizeTx :: RedeemerTagger -> AddressLabeler -> C.Tx C.ConwayEra -> C.UTxO C.ConwayEra -> TxSummary
summarizeTx tagger labeler tx utxo =
  let body = C.getTxBody tx
      txId = C.getTxId body
      summary = summarizeTxBody tagger labeler body utxo
   in summary{txsId = Just (C.serialiseToRawBytesHexText txId)}

{- | Summarize a transaction body, resolving inputs from the given UTxO set.
Spend redeemers are read from the body's 'C.TxBodyScriptData' (always
available on a built 'C.TxBody') and surfaced per script input. The
'RedeemerTagger' supplies the optional Tier 2 labels, and the
'AddressLabeler' supplies the optional friendly address labels.
-}
summarizeTxBody :: RedeemerTagger -> AddressLabeler -> C.TxBody C.ConwayEra -> C.UTxO C.ConwayEra -> TxSummary
summarizeTxBody tagger labeler body (C.UTxO utxoMap) =
  let content = C.getTxBodyContent body

      -- Spend redeemers keyed by input position (matches 'C.txIns' order).
      redeemers = bodySpendRedeemers body

      -- Inputs (resolved from UTxO). The index is the original position in
      -- 'txIns content' so it lines up with the redeemer keys even when some
      -- inputs are unresolved and filtered out.
      inputTxIns = C.txIns content
      inputs =
        [ mkInputSummary tagger labeler ix txIn txOut (Map.lookup ix redeemers)
        | (ix, (txIn, _)) <- zip [0 ..] inputTxIns
        , Just txOut <- [Map.lookup txIn utxoMap]
        ]

      -- Outputs
      outputs = zipWith (mkOutputSummary labeler (C.getTxId body)) [0 ..] (C.txOuts content)

      -- Fee
      fee = case C.txFee content of
        C.TxFeeExplicit _ coin -> C.unCoin coin

      -- Mint
      mint = case C.txMintValue content of
        C.TxMintNone -> Nothing
        mv@C.TxMintValue{} ->
          let v = C.txMintValueToValue mv
           in if v == mempty then Nothing else Just (toValueSummary v)

      -- Required signers
      signers = case C.txExtraKeyWits content of
        C.TxExtraKeyWitnessesNone -> []
        C.TxExtraKeyWitnesses _ hashes -> map C.serialiseToRawBytesHexText hashes

      -- Validity range
      validRange =
        renderValidityRange
          (C.txValidityLowerBound content)
          (C.txValidityUpperBound content)
   in TxSummary
        { txsId = Nothing
        , txsInputs = inputs
        , txsOutputs = outputs
        , txsMint = mint
        , txsFee = fee
        , txsSigners = signers
        , txsValidRange = validRange
        }

{- | Build an input summary from its (0-based) position in the tx inputs, the
resolved 'C.TxOut', and the optional spend redeemer's 'C.ScriptData'.
The 'RedeemerTagger' is applied to the parsed Plutus 'Data' of the
redeemer to produce Tier 2 labels when present. The 'AddressLabeler' is
applied to the address's credential hash to produce 'tisAddressLabel'.
-}
mkInputSummary :: RedeemerTagger -> AddressLabeler -> Word32 -> C.TxIn -> C.TxOut C.CtxUTxO C.ConwayEra -> Maybe C.ScriptData -> TxInputSummary
mkInputSummary tagger labeler _ix txIn (C.TxOut addr val _datum _refScript) mRedeemer =
  let mTag = do
        sd <- mRedeemer
        let d = C.toPlutusData sd
        applyRedeemerTagger tagger d
   in TxInputSummary
        { tisUtxo = renderTxIn txIn
        , tisAddress = renderAddressInEra addr
        , tisAddressType = addressType addr
        , tisAddressLabel = addressCredentialHashHex addr >>= applyAddressLabeler labeler
        , tisValue = toValueSummary (C.txOutValueToValue val)
        , tisRedeemerRaw = redeemerToHex <$> mRedeemer
        , tisRedeemerConstr = mRedeemer >>= redeemerConstrIx
        , tisRedeemerKind = rtKind <$> mTag
        , tisRedeemerPayload = mTag >>= rtPayload
        }

{- | Build an output summary from a TxId, an index, and a TxOut. The
'AddressLabeler' is applied to the address's credential hash to produce
'tosAddressLabel'.
-}
mkOutputSummary :: AddressLabeler -> C.TxId -> Int -> C.TxOut C.CtxTx C.ConwayEra -> TxOutputSummary
mkOutputSummary labeler txId idx (C.TxOut addr val datum _refScript) =
  TxOutputSummary
    { tosUtxo = renderTxIn (C.TxIn txId (C.TxIx (fromIntegral idx)))
    , tosAddress = renderAddressInEra addr
    , tosAddressType = addressType addr
    , tosAddressLabel = addressCredentialHashHex addr >>= applyAddressLabeler labeler
    , tosValue = toValueSummary (C.txOutValueToValue val)
    , tosDatum = renderDatum datum
    }

-- ---------------------------------------------------------------------
-- Redeemer helpers
-- ---------------------------------------------------------------------

{- | Extract the spend-purpose redeemers from a 'C.TxBody', keyed by the
0-based position of the input in the tx body's spend input list. Mirrors
the destructure used by 'Convex.ThreatModel.Cardano.Api.redeemerOfTxIn':
the redeemers live in the 'C.TxBodyScriptData' carried by the
'C.ShelleyTxBody' constructor (cardano-api 10.x).
-}
bodySpendRedeemers :: C.TxBody C.ConwayEra -> Map Word32 C.ScriptData
bodySpendRedeemers body =
  case body of
    C.ShelleyTxBody _ _ _ scriptData _ _ -> scriptDataSpendRedeemers scriptData

-- | Project the spend redeemers out of a 'C.TxBodyScriptData' value.
scriptDataSpendRedeemers :: C.TxBodyScriptData C.ConwayEra -> Map Word32 C.ScriptData
scriptDataSpendRedeemers = \case
  C.TxBodyNoScriptData -> Map.empty
  C.TxBodyScriptData _ _ (Ledger.Redeemers rdmrs) ->
    Map.fromList
      [ (idx, C.getScriptData (C.fromAlonzoData d))
      | (Conway.ConwaySpending (Ledger.AsIx idx), (d, _exUnits)) <- Map.toList rdmrs
      ]

-- | Render a redeemer's 'C.ScriptData' as the hex of its CBOR encoding.
redeemerToHex :: C.ScriptData -> Text
redeemerToHex = TE.decodeUtf8 . Base16.encode . C.serialiseToCBOR

-- | Extract the Constr index when the redeemer parses to @Constr n _@.
redeemerConstrIx :: C.ScriptData -> Maybe Integer
redeemerConstrIx sd = case C.toPlutusData sd of
  Constr n _ -> Just n
  _ -> Nothing

-- ---------------------------------------------------------------------
-- Rendering helpers
-- ---------------------------------------------------------------------

-- | Render a TxIn as @"txid#index"@.
renderTxIn :: C.TxIn -> Text
renderTxIn (C.TxIn txId (C.TxIx ix)) =
  C.serialiseToRawBytesHexText txId <> "#" <> Text.pack (show ix)

-- | Render an AddressInEra as bech32 text.
renderAddressInEra :: C.AddressInEra C.ConwayEra -> Text
renderAddressInEra (C.AddressInEra C.ShelleyAddressInEra{} addr) = C.serialiseAddress addr
renderAddressInEra (C.AddressInEra C.ByronAddressInAnyEra{} addr) = Text.pack (show addr)

-- | Render a Shelley address as bech32 text.
renderAddress :: C.Address C.ShelleyAddr -> Text
renderAddress = C.serialiseAddress

{- | Classify a payment address's credential as a public key or script
address, so a client doesn't have to parse the address itself to find out.
Byron addresses are always key-based (Byron has no script credentials).
-}
addressType :: C.AddressInEra C.ConwayEra -> AddressType
addressType (C.AddressInEra C.ByronAddressInAnyEra{} _) = PublicKey
addressType (C.AddressInEra C.ShelleyAddressInEra{} (C.ShelleyAddress _ paymentCred _)) =
  case C.fromShelleyPaymentCredential paymentCred of
    C.PaymentCredentialByKey _ -> PublicKey
    C.PaymentCredentialByScript _ -> Script

{- | The raw hex of a payment address's credential hash (key or script hash),
for looking up a friendly label via 'AddressLabeler'. @Nothing@ for Byron
addresses.
-}
addressCredentialHashHex :: C.AddressInEra C.ConwayEra -> Maybe Text
addressCredentialHashHex (C.AddressInEra C.ByronAddressInAnyEra{} _) = Nothing
addressCredentialHashHex (C.AddressInEra C.ShelleyAddressInEra{} (C.ShelleyAddress _ paymentCred _)) =
  Just $ case C.fromShelleyPaymentCredential paymentCred of
    C.PaymentCredentialByKey h -> C.serialiseToRawBytesHexText h
    C.PaymentCredentialByScript h -> C.serialiseToRawBytesHexText h

-- | Build a structured ValueSummary from a cardano-api Value.
toValueSummary :: C.Value -> ValueSummary
toValueSummary val =
  let items = toList val -- [(AssetId, Quantity)]
      lovelace = sum [n | (C.AdaAssetId, C.Quantity n) <- items]
      assets = [toAssetSummary pid name qty | (C.AssetId pid name, C.Quantity qty) <- items]
   in ValueSummary
        { vsLovelace = lovelace
        , vsAssets = assets
        }

toAssetSummary :: C.PolicyId -> C.AssetName -> Integer -> AssetSummary
toAssetSummary pid name qty =
  AssetSummary
    { asPolicyId = C.serialiseToRawBytesHexText pid -- FULL hex, no truncation
    , asName = renderAssetName name -- UTF-8 or hex fallback
    , asQuantity = qty
    }

-- | Render an AssetName as text, trying UTF-8 decoding first.
renderAssetName :: C.AssetName -> Text
renderAssetName an =
  let C.UnsafeAssetName bs = an
   in if BS.null bs
        then "<empty>"
        else case TE.decodeUtf8' bs of
          Right t -> t
          Left _ -> C.serialiseToRawBytesHexText an

-- | Render a datum reference for a transaction output.
renderDatum :: C.TxOutDatum C.CtxTx C.ConwayEra -> Maybe Text
renderDatum C.TxOutDatumNone = Nothing
renderDatum (C.TxOutDatumHash _ h) = Just ("hash:" <> C.serialiseToRawBytesHexText h)
renderDatum (C.TxOutSupplementalDatum _ d) =
  Just ("supplemental:" <> C.serialiseToRawBytesHexText (C.hashScriptDataBytes d))
renderDatum (C.TxOutDatumInline _ d) =
  Just ("inline:" <> C.serialiseToRawBytesHexText (C.hashScriptDataBytes d))

-- | Render validity range as text. Returns @Nothing@ for unbounded ranges.
renderValidityRange
  :: C.TxValidityLowerBound C.ConwayEra
  -> C.TxValidityUpperBound C.ConwayEra
  -> Maybe Text
renderValidityRange lower upper =
  case (lower, upper) of
    (C.TxValidityNoLowerBound, C.TxValidityUpperBound _ Nothing) ->
      Nothing -- unbounded, no need to show
    _ ->
      Just (renderLower lower <> " - " <> renderUpper upper)
 where
  renderLower C.TxValidityNoLowerBound = "(-inf"
  renderLower (C.TxValidityLowerBound _ (C.SlotNo n)) = "[" <> Text.pack (show n)
  renderUpper (C.TxValidityUpperBound _ Nothing) = "+inf)"
  renderUpper (C.TxValidityUpperBound _ (Just (C.SlotNo n))) = Text.pack (show n) <> ")"
