{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Unit tests for the collateral recalculation in
"Convex.ThreatModel.Cardano.Api". The interesting rule is the ledger's
multi-asset collateral equation: the collateral balance (inputs minus return
output) must be pure ADA, so native tokens carried by collateral inputs have
to come back, in full, in the collateral return output - and forfeiting the
return output entirely is only possible for ADA-only collateral.
-}
module RebalanceSpec (rebalanceTests) where

import Cardano.Api qualified as C
import Cardano.Ledger.Api.Tx.Body qualified as Ledger (mkBasicTxBody)
import Cardano.Ledger.Api.Tx.Wits qualified as Ledger (AsIx (AsIx), Redeemers (Redeemers), TxDats (TxDats))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Scripts qualified as Conway (ConwayPlutusPurpose (ConwaySpending))
import Cardano.Ledger.Conway.TxBody qualified as Conway
import Cardano.Ledger.Core qualified as Ledger (TxBody)
import Cardano.Ledger.Plutus (ExUnits (..))
import Control.Lens ((^.))
import Convex.MockChain.Defaults qualified as Defaults
import Convex.NodeParams (ledgerProtocolParameters)
import Convex.ThreatModel.Cardano.Api (LedgerEra, dummyTxId, mkSizedShelleyTxOut, recalculateTotalCollateral)
import Convex.Utils (scriptAddressV1)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Exts (fromList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

{- | The transaction under test: a fee, one collateral input, and a single
Spending redeemer (so the transaction counts as running a Plutus script and
collateral is recalculated at all) - nothing else.
'recalculateTotalCollateral' is a pure function over the transaction body
and the given UTxO set; the transaction is never submitted to a ledger, so
it can stay this minimal.
-}
testTx :: Coin -> C.Tx C.ConwayEra
testTx = mkTestTx [] [collateralTxIn]

{- | Like 'testTx' but with the given spend inputs and no collateral inputs
yet, so 'recalculateTotalCollateral' has to pick a collateral input from
among the spend inputs.
-}
testTxSpending :: [C.TxIn] -> Coin -> C.Tx C.ConwayEra
testTxSpending spendIns = mkTestTx spendIns []

mkTestTx :: [C.TxIn] -> [C.TxIn] -> Coin -> C.Tx C.ConwayEra
mkTestTx spendIns collIns fee =
  let body =
        Ledger.mkBasicTxBody
          { Conway.ctbSpendInputs = Set.fromList (map C.toShelleyTxIn spendIns)
          , Conway.ctbCollateralInputs = Set.fromList (map C.toShelleyTxIn collIns)
          , Conway.ctbTxfee = fee
          }
      unitRedeemer = C.toAlonzoData (C.unsafeHashableScriptData (C.ScriptDataNumber 42))
      redeemers = Ledger.Redeemers (Map.singleton (Conway.ConwaySpending (Ledger.AsIx 0)) (unitRedeemer, ExUnits 0 0))
      scriptData = C.TxBodyScriptData C.AlonzoEraOnwardsConway (Ledger.TxDats mempty) redeemers
   in C.Tx (C.ShelleyTxBody C.ShelleyBasedEraConway body [] scriptData Nothing C.TxScriptValidityNone) []

collateralTxIn :: C.TxIn
collateralTxIn = txInAt 0

-- | Inputs of one dummy transaction; 'C.TxIn' orders them by index.
txInAt :: Word -> C.TxIn
txInAt = C.TxIn dummyTxId . C.TxIx

walletAddr :: C.AddressInEra C.ConwayEra
walletAddr = Wallet.addressInEra Defaults.networkId Wallet.w1

-- | Some script address, for inputs that must never be picked as collateral.
scriptAddr :: C.AddressInEra C.ConwayEra
scriptAddr = scriptAddressV1 Defaults.networkId (C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn)

{- | A UTxO set resolving the collateral input to an output at the wallet's
key address, carrying the given value.
-}
utxoWith :: C.Value -> C.UTxO C.ConwayEra
utxoWith v = C.UTxO (Map.singleton collateralTxIn (mkOut v))

-- | A UTxO set with the given outputs at the wallet's key address, indexed by 'txInAt'.
utxoOf :: [(Word, C.Value)] -> C.UTxO C.ConwayEra
utxoOf outs = C.UTxO (Map.fromList [(txInAt i, mkOut v) | (i, v) <- outs])

mkOut :: C.Value -> C.TxOut ctx C.ConwayEra
mkOut = mkOutAt walletAddr

mkOutAt :: C.AddressInEra C.ConwayEra -> C.Value -> C.TxOut ctx C.ConwayEra
mkOutAt addr v = C.TxOut addr (C.TxOutValueShelleyBased C.ShelleyBasedEraConway (C.toMaryValue v)) C.TxOutDatumNone C.ReferenceScriptNone

ada :: Integer -> C.Value
ada = C.lovelaceToValue . Coin

tokens :: C.Value
tokens = fromList [(C.AssetId testPolicy testAssetName, 5)]
 where
  testPolicy = either (error . show) id (C.deserialiseFromRawBytes C.AsPolicyId (BS.replicate 28 3))
  testAssetName = either (error . show) id (C.deserialiseFromRawBytes C.AsAssetName "TKN")

pparams :: C.LedgerProtocolParameters C.ConwayEra
pparams = Defaults.nodeParams ^. ledgerProtocolParameters

-- The default protocol parameters set collateralPercentage to 150, so the
-- 1 ADA fee used throughout requires 1.5 ADA of collateral.
feeOneAda :: Coin
feeOneAda = Coin 1_000_000

requiredColl :: Coin
requiredColl = Coin 1_500_000

rebalanceTests :: TestTree
rebalanceTests =
  testGroup
    "recalculateTotalCollateral"
    [ collateralRecalculationTests
    , collateralCandidateTests
    ]

collateralRecalculationTests :: TestTree
collateralRecalculationTests =
  testGroup
    "existing collateral input"
    [ testCase "ADA-only collateral returns the leftover" $
        case recalculateTotalCollateral pparams (utxoWith (C.lovelaceToValue 10_000_000)) (testTx feeOneAda) of
          Left err -> assertFailure err
          Right (C.Tx (C.ShelleyTxBody _ body _ _ _ _) _) -> do
            Conway.ctbTotalCollateral body @?= SJust requiredColl
            Conway.ctbCollateralReturn body @?= SJust (mkSizedShelleyTxOut (mkOut (C.lovelaceToValue 8_500_000)))
    , testCase "ADA-only collateral with dust leftover forfeits the whole input" $
        -- 1.6 ADA input - 1.5 ADA required = 0.1 ADA leftover, below the
        -- minimum ADA a return output must carry, so the return output is
        -- dropped and the whole input is declared as total collateral.
        case recalculateTotalCollateral pparams (utxoWith (C.lovelaceToValue 1_600_000)) (testTx feeOneAda) of
          Left err -> assertFailure err
          Right (C.Tx (C.ShelleyTxBody _ body _ _ _ _) _) -> do
            Conway.ctbTotalCollateral body @?= SJust (Coin 1_600_000)
            Conway.ctbCollateralReturn body @?= SNothing
    , testCase "token-carrying collateral returns the tokens in full" $
        case recalculateTotalCollateral pparams (utxoWith (C.lovelaceToValue 10_000_000 <> tokens)) (testTx feeOneAda) of
          Left err -> assertFailure err
          Right (C.Tx (C.ShelleyTxBody _ body _ _ _ _) _) -> do
            Conway.ctbTotalCollateral body @?= SJust requiredColl
            Conway.ctbCollateralReturn body @?= SJust (mkSizedShelleyTxOut (mkOut (C.lovelaceToValue 8_500_000 <> tokens)))
    , testCase "token-carrying collateral with dust leftover is rejected, not forfeited" $
        -- Forfeiting would pay the tokens as collateral, which the ledger
        -- rejects, and the 0.1 ADA leftover cannot fund the token-returning
        -- return output - so this transaction cannot be built at all.
        case recalculateTotalCollateral pparams (utxoWith (C.lovelaceToValue 1_600_000 <> tokens)) (testTx feeOneAda) of
          Left err -> assertBool ("error should name the tokens as the problem, got: " <> err) ("native tokens" `isInfixOf` err)
          Right _ -> assertFailure "expected recalculation to fail: token-carrying collateral cannot be forfeited"
    ]

{- | With no collateral inputs yet, 'recalculateTotalCollateral' reuses one of
the key-address spend inputs. These pin down which one: the richest first,
ADA-only ahead of token-carrying at equal lovelace, falling through to the
next candidate when the preferred one cannot be made to work, and never a
script-address input - regardless of 'C.TxIn' order, which is why the
preferred input is always placed at a higher index than a decoy here.
-}
collateralCandidateTests :: TestTree
collateralCandidateTests =
  testGroup
    "collateral input chosen among the spend inputs"
    [ testCase "the richest key input is chosen, not the first in TxIn order" $
        -- Both inputs cover 1.5 ADA of collateral; only the ranking picks index 1.
        expectCollateral (utxoOf [(0, ada 10_000_000), (1, ada 100_000_000)]) (testTxSpending [txInAt 0, txInAt 1] feeOneAda) $ \body -> do
          Conway.ctbCollateralInputs body @?= Set.singleton (C.toShelleyTxIn (txInAt 1))
          Conway.ctbTotalCollateral body @?= SJust requiredColl
          Conway.ctbCollateralReturn body @?= SJust (mkSizedShelleyTxOut (mkOut (ada 98_500_000)))
    , testCase "a richer token-carrying input beats a poorer ADA-only one" $
        expectCollateral (utxoOf [(0, ada 10_000_000), (1, ada 100_000_000 <> tokens)]) (testTxSpending [txInAt 0, txInAt 1] feeOneAda) $ \body -> do
          Conway.ctbCollateralInputs body @?= Set.singleton (C.toShelleyTxIn (txInAt 1))
          Conway.ctbCollateralReturn body @?= SJust (mkSizedShelleyTxOut (mkOut (ada 98_500_000 <> tokens)))
    , testCase "at equal lovelace an ADA-only input beats a token-carrying one" $
        expectCollateral (utxoOf [(0, ada 10_000_000 <> tokens), (1, ada 10_000_000)]) (testTxSpending [txInAt 0, txInAt 1] feeOneAda) $ \body -> do
          Conway.ctbCollateralInputs body @?= Set.singleton (C.toShelleyTxIn (txInAt 1))
          Conway.ctbCollateralReturn body @?= SJust (mkSizedShelleyTxOut (mkOut (ada 8_500_000)))
    , testCase "a token-carrying input with dust leftover falls through to a forfeitable ADA-only one" $
        -- 1.7 ADA + tokens ranks first but its 0.2 ADA leftover cannot fund the
        -- token-returning return output; the 1.6 ADA ADA-only input can be
        -- forfeited whole instead.
        expectCollateral (utxoOf [(0, ada 1_600_000), (1, ada 1_700_000 <> tokens)]) (testTxSpending [txInAt 0, txInAt 1] feeOneAda) $ \body -> do
          Conway.ctbCollateralInputs body @?= Set.singleton (C.toShelleyTxIn (txInAt 0))
          Conway.ctbTotalCollateral body @?= SJust (Coin 1_600_000)
          Conway.ctbCollateralReturn body @?= SNothing
    , testCase "a script-address input is never used as collateral" $
        let utxo = C.UTxO (Map.fromList [(txInAt 0, mkOutAt scriptAddr (ada 100_000_000)), (txInAt 1, mkOut (ada 10_000_000))])
         in expectCollateral utxo (testTxSpending [txInAt 0, txInAt 1] feeOneAda) $ \body ->
              Conway.ctbCollateralInputs body @?= Set.singleton (C.toShelleyTxIn (txInAt 1))
    , testCase "when every candidate fails, each one's own reason is reported" $
        -- 1 ADA cannot cover 1.5 ADA of collateral; 2 ADA + tokens can, but
        -- its 0.5 ADA leftover cannot fund the token-returning return output.
        case recalculateTotalCollateral pparams (utxoOf [(0, ada 1_000_000), (1, ada 2_000_000 <> tokens)]) (testTxSpending [txInAt 0, txInAt 1] feeOneAda) of
          Left err -> do
            assertBool ("error should report the ADA-only input's shortfall, got: " <> err) ("Insufficient collateral" `isInfixOf` err)
            assertBool ("error should report the token-carrying input's problem, got: " <> err) ("native tokens" `isInfixOf` err)
          Right _ -> assertFailure "expected recalculation to fail: neither input can serve as collateral"
    , testCase "no key-address input at all is reported as such" $
        let utxo = C.UTxO (Map.singleton (txInAt 0) (mkOutAt scriptAddr (ada 100_000_000)))
         in case recalculateTotalCollateral pparams utxo (testTxSpending [txInAt 0] feeOneAda) of
              Left err -> assertBool ("unexpected error: " <> err) ("no key-address input" `isInfixOf` err)
              Right _ -> assertFailure "expected recalculation to fail without a key-address input"
    ]

-- | Run 'recalculateTotalCollateral' and hand the resulting body to the assertions.
expectCollateral :: C.UTxO C.ConwayEra -> C.Tx C.ConwayEra -> (Ledger.TxBody LedgerEra -> IO ()) -> IO ()
expectCollateral utxo tx k = case recalculateTotalCollateral pparams utxo tx of
  Left err -> assertFailure err
  Right (C.Tx (C.ShelleyTxBody _ body _ _ _ _) _) -> k body
