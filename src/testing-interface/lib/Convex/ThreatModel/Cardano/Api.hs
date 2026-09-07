{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Convex.ThreatModel.Cardano.Api (
  -- * Types
  Era,
  LedgerEra,
  IsPlutusScriptInEra,

  -- * TxOut accessors
  addressOfTxOut,
  valueOfTxOut,
  datumOfTxOut,
  referenceScriptOfTxOut,

  -- * Redeemer and script data
  redeemerOfTxIn,
  mintedPlutusPolicies,
  recomputeScriptData,
  emptyTxBodyScriptData,
  addScriptData,
  updateRedeemer,
  addMintingRedeemer,
  recomputeScriptDataForMint,
  addDatum,
  toMaryAssetName,

  -- * Address utilities
  paymentCredentialToAddressAny,
  scriptAddressAny,
  keyAddressAny,
  isKeyAddressAny,

  -- * Datum/Redeemer conversion
  toCtxUTxODatum,
  txOutDatum,
  toScriptData,

  -- * Transaction utilities
  dummyTxId,
  makeTxOut,
  txSigners,
  mockWalletHashes,
  detectSigningWallet,
  txRequiredSigners,
  txInputs,
  txReferenceInputs,
  txOutputs,

  -- * Value utilities
  leqValue,
  projectAda,

  -- * Validation
  ValidityReport (..),
  TxValidity (..),
  validateTx,
  validateTxM,
  buildMockState,

  -- * Rebalancing
  rebalanceAndSignM,
  rebalanceAndSign,
  updateExecutionUnits,
  updateTxRedeemersWithExUnits,
  updateScriptDataExUnits,
  recalculateScriptIntegrityHash,
  recalculateTotalCollateral,
  getScriptLanguage,
  getTxFeeCoin,
  setTxFeeCoin,
  setTxOutputsList,
  adjustChangeOutputM,
  adjustChangeOutput,
  replaceAt,

  -- * Validity interval
  convValidityInterval,

  -- * UTxO utilities
  restrictUTxO,

  -- * Coverage
  extractCoverageFromValidationError,
  unescapeHaskellString,
  extractCoverageAnnotations,
) where

import Cardano.Api

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo.PParams (ppCollateralPercentageL)
import Cardano.Ledger.Alonzo.Scripts qualified as Ledger
import Cardano.Ledger.Alonzo.Tx (hashScriptIntegrity, mkScriptIntegrity)
import Cardano.Ledger.Alonzo.TxBody qualified as Ledger
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Api.Era qualified as Ledger (eraProtVerLow)
import Cardano.Ledger.Api.Tx.Body qualified as Ledger
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Conway.Rules (ConwayLedgerPredFailure (..), ConwayUtxoPredFailure (..), ConwayUtxosPredFailure (..), ConwayUtxowPredFailure (..))
import Cardano.Ledger.Conway.Scripts qualified as Conway
import Cardano.Ledger.Conway.TxBody qualified as Conway
import Cardano.Ledger.Keys (WitVKey (..), coerceKeyRole, hashKey)
import Cardano.Ledger.Mary.Value qualified as Mary
import Cardano.Ledger.Plutus.Language qualified as Plutus
import Cardano.Ledger.Shelley.API.Mempool (ApplyTxError (..))
import Cardano.Ledger.State (getScriptsHashesNeeded, getScriptsNeeded, getScriptsProvided)
import Cardano.Ledger.TxIn qualified as Ledger (TxIn)
import Cardano.Slotting.Slot ()
import Cardano.Slotting.Time (SlotLength, mkSlotLength)
import Control.Lens ((&), (.~), (^.), _1)
import Data.List (isPrefixOf)

import Convex.CardanoApi.Lenses qualified as L
import Convex.Class (
  ExUnitsError (..),
  MockChainState,
  MonadBlockchain (..),
  MonadMockchain (..),
  SendTxError (..),
  ValidationError (VExUnits),
  coverageData,
  env,
  getSlot,
  poolState,
  setTimeToValidRange,
 )
import Convex.MockChain (applyTransaction, initialState)
import Convex.NodeParams (NodeParams (..))
import Convex.Wallet (Wallet)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet (mockWallets)
import Data.ByteString.Short qualified as SBS
import Data.Either (isRight)
import Data.Foldable (foldrM)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Maybe.Strict
import Data.SOP.NonEmpty (NonEmpty (NonEmptyOne))
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word
import GHC.Exts (toList)
import Ouroboros.Consensus.Block (GenesisWindow (..))
import Ouroboros.Consensus.Cardano.Block (CardanoEras, StandardCrypto)
import Ouroboros.Consensus.HardFork.History qualified as History
import PlutusTx (ToData, toData)
import PlutusTx.Coverage (CoverageData, coverageDataFromLogMsg)

type Era = ConwayEra
type LedgerEra = ShelleyLedgerEra Era
type IsPlutusScriptInEra lang = (HasScriptLanguageInEra lang Era, IsPlutusScriptLanguage lang)

addressOfTxOut :: TxOut ctx Era -> AddressAny
addressOfTxOut (TxOut (AddressInEra ShelleyAddressInEra{} addr) _ _ _) = AddressShelley addr
addressOfTxOut (TxOut (AddressInEra ByronAddressInAnyEra{} addr) _ _ _) = AddressByron addr

valueOfTxOut :: TxOut ctx Era -> Value
valueOfTxOut (TxOut _ v _ _) = txOutValueToValue v

-- | Get the datum from a transaction output.
datumOfTxOut :: TxOut ctx Era -> TxOutDatum ctx Era
datumOfTxOut (TxOut _ _ datum _) = datum

referenceScriptOfTxOut :: TxOut ctx Era -> ReferenceScript Era
referenceScriptOfTxOut (TxOut _ _ _ rscript) = rscript

redeemerOfTxIn :: Tx Era -> TxIn -> Maybe ScriptData
redeemerOfTxIn tx txIn = redeemer
 where
  Tx (ShelleyTxBody _ Conway.ConwayTxBody{Conway.ctbSpendInputs = inputs} _ scriptData _ _) _ = tx

  redeemer = case scriptData of
    TxBodyNoScriptData -> Nothing
    TxBodyScriptData _ _ (Ledger.Redeemers rdmrs) ->
      getScriptData . fromAlonzoData . fst <$> Map.lookup (Conway.ConwaySpending idx) rdmrs

  idx = case Ledger.indexOf (Ledger.AsItem (toShelleyTxIn txIn)) inputs of
    SJust idx' -> idx'
    _ -> error "The impossible happened!"

{- | Plutus minting policies (with their minted/burned assets and the redeemer used) that
the given transaction already exercises. Each policy's script is resolved either from the
transaction's own witness set or, for reference-script mints, from a UTxO among the
chain state passed in. Native-script policies, and policies whose script can't be
resolved, are omitted.
-}
mintedPlutusPolicies :: Tx Era -> UTxO Era -> [(PolicyId, PolicyAssets, ScriptInAnyLang, ScriptData)]
mintedPlutusPolicies tx (UTxO utxoMap) =
  mapMaybe resolve (zip [0 ..] (Map.toAscList policyMap))
 where
  Tx (ShelleyTxBody _ Conway.ConwayTxBody{Conway.ctbMint = Mary.MultiAsset policyMap} witnessScripts scriptData _ _) _ = tx

  redeemerAt :: Word32 -> Maybe ScriptData
  redeemerAt idx = case scriptData of
    TxBodyNoScriptData -> Nothing
    TxBodyScriptData _ _ (Ledger.Redeemers rdmrs) ->
      getScriptData . fromAlonzoData . fst <$> Map.lookup (Conway.ConwayMinting (Ledger.AsIx idx)) rdmrs

  fromAlonzoScript :: Ledger.Script LedgerEra -> Maybe ScriptInAnyLang
  fromAlonzoScript = \case
    Ledger.NativeScript _ -> Nothing
    Ledger.PlutusScript ps -> Just $ case ps of
      Conway.ConwayPlutusV1 (Plutus.Plutus (Plutus.PlutusBinary bs)) ->
        toScriptInAnyLang $ PlutusScript PlutusScriptV1 (PlutusScriptSerialised bs)
      Conway.ConwayPlutusV2 (Plutus.Plutus (Plutus.PlutusBinary bs)) ->
        toScriptInAnyLang $ PlutusScript PlutusScriptV2 (PlutusScriptSerialised bs)
      Conway.ConwayPlutusV3 (Plutus.Plutus (Plutus.PlutusBinary bs)) ->
        toScriptInAnyLang $ PlutusScript PlutusScriptV3 (PlutusScriptSerialised bs)

  candidateScripts :: [ScriptInAnyLang]
  candidateScripts =
    mapMaybe fromAlonzoScript witnessScripts
      <> [ script
         | txout <- Map.elems utxoMap
         , ReferenceScript _ script <- [referenceScriptOfTxOut txout]
         ]

  hashOf :: ScriptInAnyLang -> ScriptHash
  hashOf (ScriptInAnyLang _ s) = hashScript s

  resolve :: (Word32, (Mary.PolicyID, Map.Map Mary.AssetName Integer)) -> Maybe (PolicyId, PolicyAssets, ScriptInAnyLang, ScriptData)
  resolve (idx, (Mary.PolicyID ledgerScriptHash, assetMap)) = do
    -- fromMaryPolicyID isn't re-exported from Cardano.Api in every version we support
    -- (it's an internal helper that only became public later), so convert manually.
    let scriptHash = fromShelleyScriptHash ledgerScriptHash
        policyId = PolicyId scriptHash
    redeemer <- redeemerAt idx
    scriptInAnyLang <- listToMaybe [s | s <- candidateScripts, hashOf s == scriptHash]
    let assets = PolicyAssets $ Map.fromList [(UnsafeAssetName (SBS.fromShort n), Quantity q) | (Mary.AssetName n, q) <- Map.toList assetMap]
    pure (policyId, assets, scriptInAnyLang, redeemer)

paymentCredentialToAddressAny :: PaymentCredential -> AddressAny
paymentCredentialToAddressAny t =
  AddressShelley $ makeShelleyAddress (Testnet $ NetworkMagic 1) t NoStakeAddress

-- | Construct a script address.
scriptAddressAny :: ScriptHash -> AddressAny
scriptAddressAny = paymentCredentialToAddressAny . PaymentCredentialByScript

-- | Construct a public key address.
keyAddressAny :: Hash PaymentKey -> AddressAny
keyAddressAny = paymentCredentialToAddressAny . PaymentCredentialByKey

-- | Check if an address is a public key address.
isKeyAddressAny :: AddressAny -> Bool
isKeyAddressAny = isKeyAddress . anyAddressInShelleyBasedEra (shelleyBasedEra @Era)

recomputeScriptData
  :: Maybe Word32 -- Index to remove
  -> (Word32 -> Word32)
  -> TxBodyScriptData Era
  -> TxBodyScriptData Era
recomputeScriptData _ _ TxBodyNoScriptData = TxBodyNoScriptData
recomputeScriptData i f (TxBodyScriptData era dats (Ledger.Redeemers rdmrs)) =
  TxBodyScriptData
    era
    dats
    (Ledger.Redeemers $ Map.mapKeys updatePtr $ Map.filterWithKey idxFilter rdmrs)
 where
  -- updatePtr = Ledger.hoistPlutusPurpose (\(Ledger.AsIx ix) -> Ledger.AsIx (f ix)) -- TODO: replace when hoistPlutusPurpose is available
  updatePtr = \case
    Conway.ConwayMinting (Ledger.AsIx ix) -> Conway.ConwayMinting (Ledger.AsIx (f ix))
    Conway.ConwaySpending (Ledger.AsIx ix) -> Conway.ConwaySpending (Ledger.AsIx (f ix))
    Conway.ConwayRewarding (Ledger.AsIx ix) -> Conway.ConwayRewarding (Ledger.AsIx (f ix))
    Conway.ConwayCertifying (Ledger.AsIx ix) -> Conway.ConwayCertifying (Ledger.AsIx (f ix))
    Conway.ConwayVoting (Ledger.AsIx ix) -> Conway.ConwayVoting (Ledger.AsIx (f ix))
    Conway.ConwayProposing (Ledger.AsIx ix) -> Conway.ConwayProposing (Ledger.AsIx (f ix))
  idxFilter (Conway.ConwaySpending (Ledger.AsIx idx)) _ = Just idx /= i
  idxFilter (Conway.ConwayMinting (Ledger.AsIx idx)) _ = Just idx /= i
  idxFilter (Conway.ConwayCertifying (Ledger.AsIx idx)) _ = Just idx /= i
  idxFilter (Conway.ConwayRewarding (Ledger.AsIx idx)) _ = Just idx /= i
  idxFilter (Conway.ConwayVoting (Ledger.AsIx idx)) _ = Just idx /= i
  idxFilter (Conway.ConwayProposing (Ledger.AsIx idx)) _ = Just idx /= i

emptyTxBodyScriptData :: TxBodyScriptData Era
emptyTxBodyScriptData = TxBodyScriptData AlonzoEraOnwardsConway (Ledger.TxDats mempty) (Ledger.Redeemers mempty)

addScriptData
  :: Word32
  -> Ledger.Data (ShelleyLedgerEra Era)
  -> (Ledger.Data (ShelleyLedgerEra Era), Ledger.ExUnits)
  -> TxBodyScriptData Era
  -> TxBodyScriptData Era
addScriptData ix dat rdmr TxBodyNoScriptData = addScriptData ix dat rdmr emptyTxBodyScriptData
addScriptData ix dat rdmr (TxBodyScriptData era (Ledger.TxDats dats) (Ledger.Redeemers rdmrs)) =
  TxBodyScriptData
    era
    (Ledger.TxDats $ Map.insert (Ledger.hashData dat) dat dats)
    (Ledger.Redeemers $ Map.insert (Conway.ConwaySpending (Ledger.AsIx ix)) rdmr rdmrs)

{- | Update only the redeemer for a spending input (does not modify TxDats)
Use this when the original UTxO has an inline datum to avoid adding orphaned datums
-}
updateRedeemer
  :: Word32
  -> (Ledger.Data (ShelleyLedgerEra Era), Ledger.ExUnits)
  -> TxBodyScriptData Era
  -> TxBodyScriptData Era
updateRedeemer ix rdmr TxBodyNoScriptData = updateRedeemer ix rdmr emptyTxBodyScriptData
updateRedeemer ix rdmr (TxBodyScriptData era dats (Ledger.Redeemers rdmrs)) =
  TxBodyScriptData
    era
    dats
    (Ledger.Redeemers $ Map.insert (Conway.ConwaySpending (Ledger.AsIx ix)) rdmr rdmrs)

-- | Add a minting redeemer to the script data (no datum needed for minting)
addMintingRedeemer
  :: Word32
  -> (Ledger.Data (ShelleyLedgerEra Era), Ledger.ExUnits)
  -> TxBodyScriptData Era
  -> TxBodyScriptData Era
addMintingRedeemer _ _ TxBodyNoScriptData = addMintingRedeemer 0 (error "no redeemer", Ledger.ExUnits 0 0) emptyTxBodyScriptData
addMintingRedeemer ix rdmr (TxBodyScriptData era dats (Ledger.Redeemers rdmrs)) =
  TxBodyScriptData
    era
    dats
    (Ledger.Redeemers $ Map.insert (Conway.ConwayMinting (Ledger.AsIx ix)) rdmr rdmrs)

-- | Like recomputeScriptData but only updates minting redeemer indices
recomputeScriptDataForMint
  :: Maybe Word32 -- Index to remove
  -> (Word32 -> Word32)
  -> TxBodyScriptData Era
  -> TxBodyScriptData Era
recomputeScriptDataForMint _ _ TxBodyNoScriptData = TxBodyNoScriptData
recomputeScriptDataForMint i f (TxBodyScriptData era dats (Ledger.Redeemers rdmrs)) =
  TxBodyScriptData
    era
    dats
    (Ledger.Redeemers $ Map.mapKeys updatePtr $ Map.filterWithKey idxFilter rdmrs)
 where
  updatePtr = \case
    Conway.ConwayMinting (Ledger.AsIx ix) -> Conway.ConwayMinting (Ledger.AsIx (f ix))
    other -> other -- Don't modify non-minting redeemers
  idxFilter (Conway.ConwayMinting (Ledger.AsIx idx)) _ = Just idx /= i
  idxFilter _ _ = True -- Keep all non-minting redeemers

-- | Convert cardano-api AssetName to ledger Mary.AssetName
toMaryAssetName :: AssetName -> Mary.AssetName
toMaryAssetName an = Mary.AssetName $ SBS.toShort $ serialiseToRawBytes an

addDatum
  :: Ledger.Data (ShelleyLedgerEra Era)
  -> TxBodyScriptData Era
  -> TxBodyScriptData Era
addDatum dat TxBodyNoScriptData = addDatum dat emptyTxBodyScriptData
addDatum dat (TxBodyScriptData era (Ledger.TxDats dats) rdmrs) =
  TxBodyScriptData
    era
    (Ledger.TxDats $ Map.insert (Ledger.hashData dat) dat dats)
    rdmrs

toCtxUTxODatum :: TxOutDatum CtxTx Era -> TxOutDatum CtxUTxO Era
toCtxUTxODatum d = case d of
  TxOutDatumNone -> TxOutDatumNone
  TxOutDatumHash s h -> TxOutDatumHash s h
  TxOutDatumInline s sd -> TxOutDatumInline s sd
  TxOutSupplementalDatum s _sd -> TxOutDatumHash s (hashScriptDataBytes _sd)

-- | Convert ScriptData to a `Test.QuickCheck.ContractModel.ThreatModel.Datum`.
txOutDatum :: ScriptData -> TxOutDatum CtxTx Era
txOutDatum d = TxOutDatumInline BabbageEraOnwardsConway (unsafeHashableScriptData d)

{- | Convert a Haskell value to ScriptData for use as a
`Test.QuickCheck.ContractModel.ThreatModel.Redeemer` or convert to a
`Test.QuickCheck.ContractModel.ThreatModel.Datum` with `txOutDatum`.
-}
toScriptData :: (ToData a) => a -> ScriptData
toScriptData = fromPlutusData . toData

-- | Used for new inputs.
dummyTxId :: TxId
dummyTxId =
  fromShelleyTxId $
    Ledger.txIdTxBody @LedgerEra $
      Ledger.mkBasicTxBody

makeTxOut :: AddressAny -> Value -> TxOutDatum CtxTx Era -> ReferenceScript Era -> TxOut CtxUTxO Era
makeTxOut addr value datum refScript =
  toCtxUTxOTxOut $
    TxOut
      (anyAddressInShelleyBasedEra shelleyBasedEra addr)
      (TxOutValueShelleyBased shelleyBasedEra (toMaryValue value))
      datum
      refScript

txSigners :: Tx Era -> [Hash PaymentKey]
txSigners (Tx _ wits) = [toHash wit | ShelleyKeyWitness _ (WitVKey wit _) <- wits]
 where
  toHash =
    PaymentKeyHash
      . hashKey
      . coerceKeyRole

mockWalletHashes :: [(Hash PaymentKey, Wallet)]
mockWalletHashes = map (\w -> (Wallet.verificationKeyHash w, w)) mockWallets

{- | Detect which mock wallet signed a transaction by examining its witnesses.
Returns an error message if no known mock wallet is found among the signers.
-}
detectSigningWallet :: Tx Era -> Either String Wallet
detectSigningWallet tx =
  case txSigners tx of
    [] -> Left "Transaction has no signers — cannot determine wallet for threat model"
    signers ->
      case mapMaybe (\h -> lookup h mockWalletHashes) signers of
        (w : _) -> Right w
        [] -> Left "Transaction signers do not match any known mock wallet"

-- | Get the required signers from the transaction body (not witnesses).
txRequiredSigners :: Tx Era -> [Hash PaymentKey]
txRequiredSigners (Tx (ShelleyTxBody _ body _ _ _ _) _) =
  map (PaymentKeyHash . coerceKeyRole) . Set.toList $ Conway.ctbReqSignerHashes body

txInputs :: Tx Era -> [TxIn]
txInputs tx = map fst $ txIns body
 where
  body = getTxBodyContent $ getTxBody tx

txReferenceInputs :: Tx Era -> [TxIn]
txReferenceInputs tx =
  case txInsReference body of
    TxInsReferenceNone -> []
    TxInsReference _ txins _ -> txins
 where
  body = getTxBodyContent $ getTxBody tx

txOutputs :: Tx Era -> [TxOut CtxTx Era]
txOutputs tx = txOuts body
 where
  body = getTxBodyContent $ getTxBody tx

-- | Check if a value is less or equal than another value.
leqValue :: Value -> Value -> Bool
leqValue v v' = all ((<= 0) . snd) (toList $ v <> negateValue v')

-- | Keep only the Ada part of a value.
projectAda :: Value -> Value
projectAda = lovelaceToValue . selectLovelace

{- | The outcome of validating a transaction, mirroring the ledger's two-phase
validation: Phase 2 (script execution) only runs when Phase 1 (structural
ledger rules) passes, so no other combinations exist.
-}
data TxValidity
  = -- | Phase 1 and Phase 2 both passed
    Valid
  | -- | Rejected by Phase 1 ledger rules; scripts were never executed
    Phase1Invalid
  | -- | Phase 1 passed, but a script rejected the transaction
    Phase2Invalid
  deriving stock (Ord, Eq, Show)

{- | The result of validating a transaction. In case of failure, it includes a list
  of reasons.
-}
data ValidityReport = ValidityReport
  { errors :: [String]
  , validity :: TxValidity
  }
  deriving stock (Ord, Eq, Show)

{- | Validate a transaction using Phase 2 (script execution) validation only.

This uses evaluateTransactionExecutionUnits to check if Plutus scripts would
accept or reject the transaction. It does NOT validate Phase 1 ledger rules
(fees, signatures, value preservation, etc.) because threat model modifications
alter the transaction body, invalidating signatures and fee calculations.

The purpose of threat models is to test script logic, not transaction construction.
-}
validateTx :: LedgerProtocolParameters Era -> Tx Era -> UTxO Era -> ValidityReport
validateTx pparams tx utxos =
  ValidityReport
    { errors = [show e | Left e <- Map.elems report]
    , validity = if all isRight (Map.elems report) then Valid else Phase2Invalid
    }
 where
  report =
    evaluateTransactionExecutionUnits
      ConwayEra
      systemStart
      (toLedgerEpochInfo eraHistory)
      pparams
      utxos
      (getTxBody tx)

  eraHistory :: EraHistory
  eraHistory = EraHistory (History.mkInterpreter summary)

  summary :: History.Summary (CardanoEras StandardCrypto)
  summary =
    History.Summary . NonEmptyOne $
      History.EraSummary
        { History.eraStart = History.initBound
        , History.eraEnd = History.EraUnbounded
        , History.eraParams =
            History.EraParams
              { History.eraEpochSize = epochSize
              , History.eraSlotLength = slotLength
              , History.eraSafeZone = History.UnsafeIndefiniteSafeZone
              , History.eraGenesisWin = genesisWindow
              }
        }

  epochSize :: EpochSize
  epochSize = EpochSize 100

  slotLength :: SlotLength
  slotLength = mkSlotLength 1

  systemStart :: SystemStart
  systemStart = SystemStart $ posixSecondsToUTCTime 0

  genesisWindow :: GenesisWindow
  genesisWindow = GenesisWindow 10

-- | Keep only UTxOs mentioned in the given transaction.
restrictUTxO :: Tx Era -> UTxO Era -> UTxO Era
restrictUTxO tx (UTxO utxo) =
  UTxO $
    Map.filterWithKey
      ( \k _ ->
          k `elem` map fst (txIns body)
            || k `elem` toInputList (txInsReference body)
      )
      utxo
 where
  body = getTxBodyContent $ getTxBody tx
  toInputList (TxInsReference _ ins _) = ins
  toInputList _ = []

convValidityInterval
  :: (TxValidityLowerBound era, TxValidityUpperBound era)
  -> ValidityInterval
convValidityInterval (lowerBound, upperBound) =
  ValidityInterval
    { invalidBefore = case lowerBound of
        TxValidityNoLowerBound -> SNothing
        TxValidityLowerBound _ s -> SJust s
    , invalidHereafter = case upperBound of
        TxValidityUpperBound _ Nothing -> SNothing
        TxValidityUpperBound _ (Just s) -> SJust s
    }

-- | Build a MockChainState from NodeParams, slot, and UTxO for validation
buildMockState
  :: NodeParams Era
  -> SlotNo
  -> UTxO Era
  -> MockChainState Era
buildMockState params slot utxo =
  initialState params
    & env . L.slot .~ slot
    & poolState . L.utxoState . L._UTxOState . _1 .~ toLedgerUTxO shelleyBasedEra utxo

{- | Check if an 'ApplyTxError' contains a Phase 2 (script execution) failure.

The only genuine Phase 2 signal in an 'ApplyTxError' is 'ValidationTagMismatch':
the ledger re-ran the scripts and their result contradicts the transaction's
'IsValid' flag.

'CollectErrors' is deliberately not treated as Phase 2: its cases
('NoRedeemer', 'NoWitness', 'NoCostModel', 'BadTranslation') mean script
execution never started, which is Phase 1 in nature. It is also unreachable
here: the mockchain collects scripts in 'constructValidated' before 'applyTx'
and returns such failures as 'MockchainError', never as 'ApplyTxError'.
-}
hasPhase2Failure :: ApplyTxError LedgerEra -> Bool
hasPhase2Failure (ApplyTxError failures) = any isPhase2 failures
 where
  isPhase2 (ConwayUtxowFailure (UtxoFailure (UtxosFailure ValidationTagMismatch{}))) = True
  isPhase2 _ = False

{- | Validate a transaction with full Phase 1 + Phase 2 validation inside MockchainT.

This uses 'applyTransaction' which performs complete ledger validation including:
- Fee adequacy
- Signature verification
- UTxO existence
- Value preservation
- Validity intervals
- Collateral requirements
- Script execution (Phase 2)
-}
validateTxM
  :: (MonadMockchain Era m)
  => NodeParams Era
  -> Tx Era
  -> UTxO Era
  -> m (ValidityReport, CoverageData)
validateTxM params tx utxo = do
  -- Validate at a slot within the transaction's own validity interval (like
  -- 'threatModelEnvs' does when replaying). Otherwise the ledger rejects the
  -- transaction with 'OutsideValidityIntervalUTxO' (Phase 1) whenever the
  -- current mockchain slot falls outside the interval, masking any Phase 2
  -- script failure.
  let txBodyContent = getTxBodyContent $ getTxBody tx
  setTimeToValidRange (txValidityLowerBound txBodyContent, txValidityUpperBound txBodyContent)
  slot <- getSlot
  let mockState = buildMockState params slot utxo
      NodeParams{npSystemStart, npEraHistory, npProtocolParameters} = params
  pure $ case applyTransaction params mockState tx of
    Left (ApplyTxFailure err)
      | hasPhase2Failure err ->
          let (covData, errors) =
                extractFromExUnits $
                  evaluateTransactionExecutionUnits
                    ConwayEra
                    npSystemStart
                    (toLedgerEpochInfo npEraHistory)
                    npProtocolParameters
                    utxo
                    (getTxBody tx)
           in (ValidityReport{errors, validity = Phase2Invalid}, covData)
      | otherwise ->
          (ValidityReport{errors = [show err], validity = Phase1Invalid}, mempty)
    Left (MockchainError (VExUnits (Phase2Error (ScriptErrorEvaluationFailed DebugPlutusFailure{dpfEvaluationError, dpfExecutionLogs})))) ->
      (ValidityReport{errors = [show dpfEvaluationError], validity = Phase2Invalid}, foldMap (coverageDataFromLogMsg . Text.unpack) dpfExecutionLogs)
    Left err -> (ValidityReport{errors = [show err], validity = Phase1Invalid}, mempty)
    Right (state', _) -> (ValidityReport{errors = [], validity = Valid}, state' ^. coverageData)

extractFromExUnits :: Map.Map k (Either ScriptExecutionError b) -> (CoverageData, [String])
extractFromExUnits = foldMap fromScriptResult . Map.elems
 where
  fromScriptResult (Left (ScriptErrorEvaluationFailed DebugPlutusFailure{dpfExecutionLogs, dpfEvaluationError})) =
    ( foldMap (coverageDataFromLogMsg . Text.unpack) dpfExecutionLogs
    , [show dpfEvaluationError]
    )
  fromScriptResult _ = (mempty, [])

{- | Re-balance fees, recalculate execution units, and re-sign a modified transaction.

After applying TxModifier operations, the transaction body changes which:
1. Invalidates the original signatures (body hash changed)
2. May require different fees (outputs changed)
3. May have invalid execution units (for added scripts)

This function:
1. Recalculates execution units for all scripts
2. Calculates the new required fee
3. Adjusts the change output (last output to wallet address) to compensate
4. Re-signs the transaction with the wallet's key
-}
rebalanceAndSignM
  :: (MonadMockchain Era m, MonadFail m)
  => Wallet
  -> Tx Era
  -> UTxO Era
  -> m (Tx Era)
rebalanceAndSignM wallet tx utxo = do
  result <- rebalanceAndSign wallet tx utxo
  case result of
    Left err -> fail err
    Right signedTx -> pure signedTx

{- | Like 'rebalanceAndSign' but returns Either instead of using MonadFail.

This is useful for threat model execution where we want to handle rebalancing
failures (e.g., "No change output found") as skipped tests rather than errors.
-}
rebalanceAndSign
  :: (MonadMockchain Era m)
  => Wallet
  -> Tx Era
  -> UTxO Era
  -> m (Either String (Tx Era))
rebalanceAndSign wallet tx utxo = do
  pparams <- Convex.Class.queryProtocolParameters
  networkId <- Convex.Class.queryNetworkId
  systemStart <- Convex.Class.querySystemStart
  eraHistory <- Convex.Class.queryEraHistory

  let walletAddr = Wallet.addressInEra networkId wallet

  -- First, recalculate execution units for all scripts in the transaction
  -- This is necessary because TxModifier may add scripts with ExecutionUnits 0 0
  let txWithUpdatedExUnits = updateExecutionUnits pparams systemStart eraHistory utxo tx

  {- A TxModifier that bloats an output's value or datum (e.g. adding junk
  tokens or extra datum fields) only adds what it's testing - it doesn't also
  top up that output's ADA to cover the larger minimum UTxO requirement its
  new size demands, which a real attacker constructing this transaction
  would have to do anyway. Do that now, so later steps (fee estimation, value
  balancing) see the transaction's true final shape (see
  'topUpUnderfundedOutputs').
  -}
  let txWithFundedOutputs = topUpUnderfundedOutputs pparams txWithUpdatedExUnits

  {- If a TxModifier introduced a Plutus script into a transaction that
  previously ran none, it now needs a collateral input and return output that
  didn't exist before. Give it that shape *before* estimating the fee below,
  so the fee calculation sees the transaction's true final size (see
  'ensureCollateralInputShape').
  -}
  let txWithCollateralShape = ensureCollateralInputShape utxo txWithFundedOutputs

  -- Create a temp tx with max fee to calculate the actual required fee
  let maxFee = Coin (2 ^ (32 :: Integer) - 1)
      tempTx = setTxFeeCoin maxFee txWithCollateralShape
      Tx tempBody _ = tempTx
      newFee =
        calculateMinTxFee
          shelleyBasedEra
          (unLedgerProtocolParameters pparams)
          utxo
          tempBody
          1

  {- Measure how far the transaction is from being value-conserved, once the
  recalculated fee is in place. This single number subsumes the old
  fee-increase-only case (a plain fee change is all the previous code
  compensated for) as well as any value a TxModifier added, removed, or
  resized elsewhere in the transaction (e.g. a duplicated or shrunk output)
  without a matching change on the input side. Passing this to
  'adjustChangeOutput' lets the change output absorb it generically, instead
  of every TxModifier having to hand-balance its own mutation.
  -}
  let txWithNewFee = setTxFeeCoin newFee txWithFundedOutputs
      Tx bodyWithNewFee _ = txWithNewFee
      residual =
        txOutValueToValue $
          evaluateTransactionBalance
            shelleyBasedEra
            (unLedgerProtocolParameters pparams)
            mempty
            mempty
            mempty
            utxo
            bodyWithNewFee

  -- Adjust the change output to absorb the residual (with the new fee already set)
  let currentOuts = txOutputs txWithNewFee
  case adjustChangeOutput pparams walletAddr residual currentOuts of
    Left err -> pure (Left err)
    Right adjustedOutputs -> do
      -- Apply the adjusted outputs
      let modifiedTx = setTxOutputsList adjustedOutputs txWithNewFee

      -- Recalculate total collateral based on new fee
      case recalculateTotalCollateral pparams utxo modifiedTx of
        Left err -> pure (Left err)
        Right txWithCollateral -> do
          -- Recalculate script integrity hash (after updating execution units)
          let finalTx = recalculateScriptIntegrityHash utxo pparams txWithCollateral

          -- Re-sign (strip old signatures and add new one)
          let Tx finalBody _ = finalTx
              unsignedTx = makeSignedTransaction [] finalBody
              signers = txSigners tx
              sign hash tx' = case lookup hash mockWalletHashes of
                Just w -> Right $ Wallet.signTx w tx'
                Nothing -> Left "Transaction was signed by an unknown wallet"
          pure $ foldrM sign unsignedTx signers

{- | Update execution units in a transaction by evaluating all scripts.

This computes the actual execution units required for each script and updates
the redeemers in the transaction with those values. This is necessary because
TxModifier operations like addPlutusScriptMint use ExecutionUnits 0 0 as
placeholders.
-}
updateExecutionUnits
  :: LedgerProtocolParameters Era
  -> SystemStart
  -> EraHistory
  -> UTxO Era
  -> Tx Era
  -> Tx Era
updateExecutionUnits pparams systemStart eraHistory utxo tx =
  let exUnitsMap =
        evaluateTransactionExecutionUnits
          ConwayEra
          systemStart
          (toLedgerEpochInfo eraHistory)
          pparams
          utxo
          (getTxBody tx)
      -- Extract only successful execution unit results
      successfulExUnits =
        Map.mapMaybe
          ( \case
              Right (_, exUnits) -> Just exUnits
              Left _ -> Nothing
          )
          exUnitsMap
   in updateTxRedeemersWithExUnits successfulExUnits tx

{- | Update the execution units in a transaction's redeemers.

This function takes a map from ScriptWitnessIndex to ExecutionUnits and updates
the corresponding redeemers in the transaction.
-}
updateTxRedeemersWithExUnits
  :: Map.Map ScriptWitnessIndex ExecutionUnits
  -> Tx Era
  -> Tx Era
updateTxRedeemersWithExUnits exUnitsMap (Tx (ShelleyTxBody era body scripts scriptData auxData validity) wits) =
  let scriptData' = updateScriptDataExUnits exUnitsMap scriptData
   in Tx (ShelleyTxBody era body scripts scriptData' auxData validity) wits

-- | Update execution units in TxBodyScriptData based on ScriptWitnessIndex map.
updateScriptDataExUnits
  :: Map.Map ScriptWitnessIndex ExecutionUnits
  -> TxBodyScriptData Era
  -> TxBodyScriptData Era
updateScriptDataExUnits _ TxBodyNoScriptData = TxBodyNoScriptData
updateScriptDataExUnits exUnitsMap (TxBodyScriptData eraWit dats (Ledger.Redeemers rdmrs)) =
  TxBodyScriptData eraWit dats (Ledger.Redeemers updatedRdmrs)
 where
  updatedRdmrs = Map.mapWithKey updateRedeemer' rdmrs

  updateRedeemer' :: Conway.ConwayPlutusPurpose Ledger.AsIx LedgerEra -> (Ledger.Data LedgerEra, Ledger.ExUnits) -> (Ledger.Data LedgerEra, Ledger.ExUnits)
  updateRedeemer' purpose (dat, _oldExUnits) =
    case purposeToScriptWitnessIndex purpose of
      Just idx -> case Map.lookup idx exUnitsMap of
        Just newExUnits -> (dat, toAlonzoExUnits newExUnits)
        Nothing -> (dat, _oldExUnits) -- Keep old if not in map
      Nothing -> (dat, _oldExUnits)

  -- Convert Conway purpose to cardano-api ScriptWitnessIndex
  purposeToScriptWitnessIndex :: Conway.ConwayPlutusPurpose Ledger.AsIx LedgerEra -> Maybe ScriptWitnessIndex
  purposeToScriptWitnessIndex (Conway.ConwaySpending (Ledger.AsIx ix)) = Just $ ScriptWitnessIndexTxIn ix
  purposeToScriptWitnessIndex (Conway.ConwayMinting (Ledger.AsIx ix)) = Just $ ScriptWitnessIndexMint ix
  purposeToScriptWitnessIndex (Conway.ConwayRewarding (Ledger.AsIx ix)) = Just $ ScriptWitnessIndexWithdrawal ix
  purposeToScriptWitnessIndex (Conway.ConwayCertifying (Ledger.AsIx ix)) = Just $ ScriptWitnessIndexCertificate ix
  purposeToScriptWitnessIndex (Conway.ConwayVoting (Ledger.AsIx ix)) = Just $ ScriptWitnessIndexVoting ix
  purposeToScriptWitnessIndex (Conway.ConwayProposing (Ledger.AsIx ix)) = Just $ ScriptWitnessIndexProposing ix

{- | Recalculate and update the script integrity hash in a transaction.

The script integrity hash commits to:
- The redeemers in the transaction
- The datums in the witness set
- The cost models for languages used (from protocol parameters)

After modifying a transaction (adding/removing inputs, changing redeemers/datums),
this hash becomes stale and must be recalculated.
-}
recalculateScriptIntegrityHash :: UTxO Era -> LedgerProtocolParameters Era -> Tx Era -> Tx Era
recalculateScriptIntegrityHash utxo pparams tx@(Tx (ShelleyTxBody era body scripts scriptData auxData validity) wits) =
  let
    pp = unLedgerProtocolParameters pparams
    ledgerUtxo = toLedgerUTxO shelleyBasedEra utxo
    ShelleyTx _ ledgerTx = tx
    scriptsProvided = getScriptsProvided ledgerUtxo ledgerTx
    scriptsNeeded = getScriptsHashesNeeded (getScriptsNeeded ledgerUtxo body)

    -- Compute new script integrity hash
    newHash = hashScriptIntegrity <$> mkScriptIntegrity pp ledgerTx scriptsProvided scriptsNeeded

    -- Update the body with new hash
    body' = body{Conway.ctbScriptIntegrityHash = newHash}
   in
    Tx (ShelleyTxBody era body' scripts scriptData auxData validity) wits

{- | Recalculate the total collateral and collateral return based on the new fee.

Total collateral = ceiling(fee * collateralPercentage / 100)
Collateral return = collateral input value - total collateral

This is needed because cardano-ledger is strict about collateral matching the fee.
When the fee increases (e.g., due to bloated datum), we need to:
1. Increase the total collateral field
2. Decrease the collateral return (to provide more collateral)

If the transaction runs a Plutus script (spending, minting, or otherwise) but
doesn't have any collateral inputs yet - e.g. a 'TxModifier' introduced a new
Plutus script, such as a minting policy, into a transaction that previously
ran no scripts at all - an existing ADA-only key-address input already
present in the transaction is reused as the collateral input. The same UTxO
can appear in both the regular input set and the collateral input set: on a
successful script run the collateral fields are simply ignored by the
ledger, so this "double duty" is safe and is what a real wallet without a
dedicated collateral reserve would do too.

Returns Left if no suitable collateral input is available, or if the chosen
collateral inputs don't have enough value to cover the required collateral.
The latter can happen when a TxModifier significantly increases the
transaction size (and thus the fee) - the original collateral may no longer
be sufficient.
-}
recalculateTotalCollateral :: LedgerProtocolParameters Era -> UTxO Era -> Tx Era -> Either String (Tx Era)
recalculateTotalCollateral pparams utxo tx@(Tx (ShelleyTxBody era body scripts scriptData auxData validity) wits)
  -- No Plutus script runs in this transaction: no collateral is required at all.
  | not (needsCollateral utxo body scripts) = Right tx
  | otherwise =
      case collateralInputsToUse utxo body of
        Nothing -> Left "Transaction runs a Plutus script but no ADA-only key-address input is available to use as collateral"
        Just collInputsSet ->
          -- Calculate total collateral input value
          let collOuts =
                [ txOut
                | txIn <- Set.toList collInputsSet
                , Just txOut <- [Map.lookup (fromShelleyTxIn txIn) (unUTxO utxo)]
                ]
           in case collOuts of
                -- collInputsSet is non-empty (it comes from 'collateralInputsToUse'), but
                -- none of its members resolve against the supplied UTxO set.
                [] -> Left "Transaction's collateral inputs do not resolve in the supplied UTxO set"
                -- Address to send any collateral return to, if a fresh return
                -- output needs to be created (i.e. there wasn't one already):
                -- the address of the collateral input itself.
                (TxOut fallbackReturnAddr _ _ _ : _) ->
                  let pp = unLedgerProtocolParameters pparams
                      collPerc = pp ^. ppCollateralPercentageL
                      Coin fee = Conway.ctbTxfee body
                      -- Calculate required total collateral: ceiling(fee * collateralPercentage / 100)
                      requiredColl@(Coin requiredCollAmount) = Coin $ ceiling (fromIntegral fee * fromIntegral collPerc / (100 :: Rational))
                      Coin collInputValue = sum [txOutValueToLovelace val | TxOut _ val _ _ <- collOuts]
                      -- Calculate new collateral return = input value - required collateral
                      newReturnAmount = collInputValue - requiredCollAmount
                      -- The output the leftover would be returned in, had we not yet
                      -- decided whether it's big enough to keep as its own output.
                      candidateReturnOut = case Conway.ctbCollateralReturn body of
                        SJust sizedOut ->
                          let TxOut addr _ datum rscript = fromShelleyTxOut shelleyBasedEra (CBOR.sizedValue sizedOut)
                           in TxOut addr (TxOutValueShelleyBased shelleyBasedEra (toMaryValue (lovelaceToValue (Coin newReturnAmount)))) datum rscript
                        SNothing ->
                          TxOut fallbackReturnAddr (TxOutValueShelleyBased shelleyBasedEra (toMaryValue (lovelaceToValue (Coin newReturnAmount)))) TxOutDatumNone ReferenceScriptNone
                      minReturnAda = calculateMinimumUTxO shelleyBasedEra pp candidateReturnOut
                      -- A leftover that's non-zero but still below the minimum ADA an
                      -- output must carry can't be returned as its own output (the
                      -- ledger would reject it with BabbageOutputTooSmallUTxO); in
                      -- that case forfeit the whole collateral input instead of
                      -- creating an under-funded return output.
                      canReturnLeftover = newReturnAmount == 0 || Coin newReturnAmount >= minReturnAda
                   in if newReturnAmount < 0
                        then Left $ "Insufficient collateral: inputs=" ++ show collInputValue ++ ", need=" ++ show requiredCollAmount
                        else
                          -- Update the collateral inputs, total collateral, and collateral return
                          let (actualTotalCollateral, newCollateralReturn)
                                | canReturnLeftover =
                                    (requiredColl, setCollateralReturn fallbackReturnAddr (Coin newReturnAmount) (Conway.ctbCollateralReturn body))
                                | otherwise = (Coin collInputValue, SNothing)
                              body' =
                                body
                                  { Conway.ctbCollateralInputs = collInputsSet
                                  , Conway.ctbTotalCollateral = SJust actualTotalCollateral
                                  , Conway.ctbCollateralReturn = newCollateralReturn
                                  }
                           in Right $ Tx (ShelleyTxBody era body' scripts scriptData auxData validity) wits

{- | Does this transaction run a Plutus script? A script can either be
witnessed directly (in @scripts@) or, without being witnessed at all,
attached as a reference script to one of the UTxOs the transaction spends
or references - so both have to be checked.
-}
needsCollateral :: UTxO Era -> Conway.TxBody LedgerEra -> [Ledger.AlonzoScript LedgerEra] -> Bool
needsCollateral utxo body scripts =
  any isPlutusScript scripts || any isPlutusRefScript resolvedInputs
 where
  isPlutusScript Ledger.PlutusScript{} = True
  isPlutusScript _ = False

  resolvedInputs =
    [ txOut
    | txIn <- Set.toList (Conway.ctbSpendInputs body) <> Set.toList (Conway.ctbReferenceInputs body)
    , Just txOut <- [Map.lookup (fromShelleyTxIn txIn) (unUTxO utxo)]
    ]

  isPlutusRefScript txOut = case referenceScriptOfTxOut txOut of
    ReferenceScript _ (ScriptInAnyLang (PlutusScriptLanguage _) _) -> True
    _ -> False

{- | The collateral inputs to use: the existing ones if there are any,
otherwise a single reused ADA-only key-address input (see
'recalculateTotalCollateral').
-}
collateralInputsToUse :: UTxO Era -> Conway.TxBody LedgerEra -> Maybe (Set.Set Ledger.TxIn)
collateralInputsToUse utxo body
  | not (Set.null existingCollateralInputs) = Just existingCollateralInputs
  | otherwise = Set.singleton <$> findAdaOnlyKeyInput utxo body
 where
  existingCollateralInputs = Conway.ctbCollateralInputs body

{- | An existing regular input that's ADA-only and at a key (non-script)
address, suitable for reuse as a collateral input.
-}
findAdaOnlyKeyInput :: UTxO Era -> Conway.TxBody LedgerEra -> Maybe Ledger.TxIn
findAdaOnlyKeyInput utxo body =
  listToMaybe
    [ txIn
    | txIn <- Set.toList (Conway.ctbSpendInputs body)
    , Just txOut@(TxOut _ val _ _) <- [Map.lookup (fromShelleyTxIn txIn) (unUTxO utxo)]
    , isKeyAddressAny (addressOfTxOut txOut)
    , isAdaOnlyValue (txOutValueToValue val)
    ]

isAdaOnlyValue :: Value -> Bool
isAdaOnlyValue v = lovelaceToValue (selectLovelace v) == v

{- | Ensure every output in a transaction carries at least the protocol's
minimum required ADA for its current size (its value's assets, its datum,
etc). A 'TxModifier' that bloats an output's value or datum (e.g. adding
junk tokens or extra datum fields) only adds what it's testing; it doesn't
separately account for the larger minimum UTxO requirement that bloat
demands. Without this, such a mutation fails Phase 1 with
@BabbageOutputTooSmallUTxO@ before the validator ever gets a chance to
accept or reject the bloat, wasting the test on a ledger bookkeeping
artifact instead of the validator's own logic - a real attacker constructing
this transaction would simply provide the required ADA, so the test should
too.

The shortfall for whichever outputs need it is made up generically once
'rebalanceAndSign' absorbs the transaction's overall value residual into the
wallet's change output.
-}
topUpUnderfundedOutputs :: LedgerProtocolParameters Era -> Tx Era -> Tx Era
topUpUnderfundedOutputs pparams tx = setTxOutputsList (map topUp (txOutputs tx)) tx
 where
  pp = unLedgerProtocolParameters pparams
  topUp out@(TxOut addr val datum refScript)
    | current >= required = out
    | otherwise =
        let newValue = txOutValueToValue val <> negateValue (lovelaceToValue current) <> lovelaceToValue required
         in TxOut addr (TxOutValueShelleyBased shelleyBasedEra (toMaryValue newValue)) datum refScript
   where
    required = calculateMinimumUTxO shelleyBasedEra pp out
    current = txOutValueToLovelace val

{- | If a transaction runs a Plutus script but has no collateral inputs yet,
pre-populate the collateral inputs and a placeholder collateral return output
(reusing an existing ADA-only key-address input, see
'recalculateTotalCollateral'). This exists purely so that a subsequent min-fee
calculation over the transaction sees its true final shape - including the
extra collateral return output a first-time collateral input requires -
before the fee is fixed; 'recalculateTotalCollateral' then overwrites the
placeholder amount with the precise one once the real fee is known. Without
this, the collateral return output would be added only after the fee had
already been set, undercounting the transaction's size and its minimum fee.

Does nothing if collateral inputs already exist, or if no Plutus script
needs to run.
-}
ensureCollateralInputShape :: UTxO Era -> Tx Era -> Tx Era
ensureCollateralInputShape utxo tx@(Tx (ShelleyTxBody era body scripts scriptData auxData validity) wits)
  | not (needsCollateral utxo body scripts) = tx
  | not (Set.null (Conway.ctbCollateralInputs body)) = tx
  | otherwise = case findAdaOnlyKeyInput utxo body of
      Nothing -> tx -- No candidate; let recalculateTotalCollateral report the error later.
      Just txIn ->
        case Map.lookup (fromShelleyTxIn txIn) (unUTxO utxo) of
          Nothing -> tx
          Just (TxOut addr val _ _) ->
            let body' =
                  body
                    { Conway.ctbCollateralInputs = Set.singleton txIn
                    , Conway.ctbCollateralReturn = SJust (mkSizedShelleyTxOut (TxOut addr val TxOutDatumNone ReferenceScriptNone))
                    }
             in Tx (ShelleyTxBody era body' scripts scriptData auxData validity) wits

{- | Update the collateral return output with a new Ada value. If there's no
existing collateral return output, a fresh one is created at the given
fallback address (used when a transaction gains a collateral input for the
first time and thus never had a return output to begin with).
-}
setCollateralReturn
  :: AddressInEra Era
  -> Coin
  -> StrictMaybe (CBOR.Sized (Ledger.TxOut LedgerEra))
  -> StrictMaybe (CBOR.Sized (Ledger.TxOut LedgerEra))
setCollateralReturn _ (Coin 0) _ = SNothing -- No return needed if all collateral is used
setCollateralReturn _ newCoin (SJust sizedOut) =
  let oldOut = CBOR.sizedValue sizedOut
      TxOut addr _ datum rscript = fromShelleyTxOut shelleyBasedEra oldOut
      -- Create new output with updated Ada value (preserve any non-Ada assets)
      newOut = TxOut addr (TxOutValueShelleyBased shelleyBasedEra (toMaryValue (lovelaceToValue newCoin))) datum rscript
   in SJust $ mkSizedShelleyTxOut newOut
setCollateralReturn fallbackAddr newCoin SNothing =
  let newOut = TxOut fallbackAddr (TxOutValueShelleyBased shelleyBasedEra (toMaryValue (lovelaceToValue newCoin))) TxOutDatumNone ReferenceScriptNone
   in SJust $ mkSizedShelleyTxOut newOut

-- | Convert a 'TxOut' into a sized ledger 'TxOut', as stored in a tx body.
mkSizedShelleyTxOut :: TxOut CtxTx Era -> CBOR.Sized (Ledger.TxOut LedgerEra)
mkSizedShelleyTxOut out =
  CBOR.mkSized (Ledger.eraProtVerLow @LedgerEra) (toShelleyTxOut shelleyBasedEra (toCtxUTxOTxOut out))

-- | Extract the Plutus language from a ledger script, if it's a Plutus script
getScriptLanguage :: Ledger.AlonzoScript LedgerEra -> Maybe Plutus.Language
getScriptLanguage script = case script of
  Ledger.NativeScript{} -> Nothing
  Ledger.PlutusScript ps -> Just $ Ledger.plutusScriptLanguage ps

-- | Get the fee from a transaction
getTxFeeCoin :: Tx Era -> Coin
getTxFeeCoin (Tx (ShelleyTxBody _ body _ _ _ _) _) = Conway.ctbTxfee body

-- | Set the fee in a transaction
setTxFeeCoin :: Coin -> Tx Era -> Tx Era
setTxFeeCoin fee (Tx (ShelleyTxBody era body scripts scriptData auxData validity) wits) =
  Tx (ShelleyTxBody era body{Conway.ctbTxfee = fee} scripts scriptData auxData validity) wits

-- | Set transaction outputs (helper that works at the Tx level)
setTxOutputsList :: [TxOut CtxTx Era] -> Tx Era -> Tx Era
setTxOutputsList newOuts (Tx (ShelleyTxBody era body scripts scriptData auxData validity) wits) =
  let newOutsSeq =
        Seq.fromList
          [ CBOR.mkSized
              (Ledger.eraProtVerLow @LedgerEra)
              (toShelleyTxOut shelleyBasedEra (toCtxUTxOTxOut out))
          | out <- newOuts
          ]
      body' = body{Conway.ctbOutputs = newOutsSeq}
   in Tx (ShelleyTxBody era body' scripts scriptData auxData validity) wits

{- | Adjust the last output going to wallet address by a value delta.

The delta is added to the change output's value: a negative Lovelace (or
other asset) component subtracts from it. This is used both to cover a plain
fee increase/decrease and, more generally, to absorb whatever residual value
imbalance ('rebalanceAndSign''s 'evaluateTransactionBalance' check) a
'TxModifier' introduced elsewhere in the transaction (e.g. an output that was
added, removed, or resized without a matching change on the input side).

Unlike every other output, the change output can't be fixed up afterwards by
'topUpUnderfundedOutputs': that helper's top-ups are themselves absorbed by a
further adjustment of the change output, so applying it to the change output
itself would just cancel back out. So the minimum-UTxO requirement is checked
right here, on the one output this function is the last thing to touch.
-}
adjustChangeOutputM
  :: (MonadFail m)
  => LedgerProtocolParameters Era
  -> AddressInEra Era
  -- ^ Wallet address to find change output
  -> Value
  -- ^ Value delta to apply to the change output
  -> [TxOut CtxTx Era]
  -- ^ Transaction outputs
  -> m [TxOut CtxTx Era]
adjustChangeOutputM pparams walletAddr delta outputs =
  case adjustChangeOutput pparams walletAddr delta outputs of
    Left err -> fail err
    Right result -> pure result

-- | Like 'adjustChangeOutput' but returns Either instead of using MonadFail.
adjustChangeOutput
  :: LedgerProtocolParameters Era
  -> AddressInEra Era
  -- ^ Wallet address to find change output
  -> Value
  -- ^ Value delta to apply to the change output
  -> [TxOut CtxTx Era]
  -- ^ Transaction outputs
  -> Either String [TxOut CtxTx Era]
adjustChangeOutput pparams walletAddr delta outputs = do
  -- Find last output to wallet address
  let indexed = zip [0 ..] outputs
      walletOutputs =
        [ (i, o)
        | (i, o@(TxOut addr _ _ _)) <- indexed
        , addr == walletAddr
        ]
  case listToMaybe (reverse walletOutputs) of
    Nothing -> Left "No change output found to wallet address"
    Just (idx, TxOut addr val datum refScript) -> do
      let oldValue = txOutValueToValue val
          newValue = oldValue <> delta
      if not (leqValue mempty newValue)
        then Left "Change output cannot cover the value shortfall introduced by the transaction modification"
        else do
          let newVal = TxOutValueShelleyBased shelleyBasedEra (toMaryValue newValue)
              newOutput = TxOut addr newVal datum refScript
              required = calculateMinimumUTxO shelleyBasedEra (unLedgerProtocolParameters pparams) newOutput
          if txOutValueToLovelace newVal < required
            then Left "Change output would fall below the minimum required ADA after rebalancing"
            else Right $ replaceAt idx newOutput outputs

-- | Replace element at index in a list
replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 x (_ : xs) = x : xs
replaceAt n x (y : ys) = y : replaceAt (n - 1) x ys

{- | Extract coverage data from a ValidationError string containing CovLoc annotations.
Handles the format found in Phase2 script evaluation errors where coverage
annotations appear as "CoverLocation (CovLoc {...})" or "CoverBool (CovLoc {...}) Bool"
-}
extractCoverageFromValidationError :: String -> CoverageData
extractCoverageFromValidationError errStr =
  mconcat $ map (coverageDataFromLogMsg . unescapeHaskellString) $ extractCoverageAnnotations errStr

-- | Unescape common Haskell string escapes (backslash-quote to quote, backslash-backslash to backslash)
unescapeHaskellString :: String -> String
unescapeHaskellString [] = []
unescapeHaskellString ('\\' : '"' : xs) = '"' : unescapeHaskellString xs
unescapeHaskellString ('\\' : '\\' : xs) = '\\' : unescapeHaskellString xs
unescapeHaskellString (x : xs) = x : unescapeHaskellString xs

{- | Extract all "CoverLocation (...)" and "CoverBool (...)" substrings from text.
Uses bracket counting to properly match nested parentheses.
-}
extractCoverageAnnotations :: String -> [String]
extractCoverageAnnotations [] = []
extractCoverageAnnotations s = case findCoverageStart s of
  Nothing -> []
  Just (prefix, rest) ->
    case extractBalancedParens rest of
      Nothing -> extractCoverageAnnotations (drop 1 s) -- skip and continue
      Just (content, remaining) ->
        (prefix ++ "(" ++ content ++ ")") : extractCoverageAnnotations remaining
 where
  -- Find "CoverLocation (" or "CoverBool (" prefix
  -- Returns the prefix and rest of string starting with '('
  -- "CoverLocation " is 14 chars, "CoverBool " is 10 chars
  findCoverageStart :: String -> Maybe (String, String)
  findCoverageStart [] = Nothing
  findCoverageStart str
    | "CoverLocation (" `isPrefixOf` str = Just ("CoverLocation ", drop 14 str) -- keep "(CovLoc..."
    | "CoverBool (" `isPrefixOf` str = Just ("CoverBool ", drop 10 str) -- keep "(CovLoc..."
    | otherwise = findCoverageStart (drop 1 str)

  -- Extract content within balanced parentheses
  -- Expects the string to start with '(' and returns content between matching parens
  extractBalancedParens :: String -> Maybe (String, String)
  extractBalancedParens ('(' : xs) = go' 1 [] xs
   where
    go' :: Integer -> [Char] -> [Char] -> Maybe ([Char], [Char])
    go' _ _ [] = Nothing
    go' n acc (c : cs)
      | c == '(' = go' (n + 1) (c : acc) cs
      | c == ')' =
          if n == 1
            then Just (reverse acc, cs)
            else go' (n - 1) (c : acc) cs
      | otherwise = go' n (c : acc) cs
  extractBalancedParens _ = Nothing
