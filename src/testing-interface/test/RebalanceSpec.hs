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
import Cardano.Ledger.Plutus (ExUnits (..))
import Control.Lens ((^.))
import Convex.MockChain.Defaults qualified as Defaults
import Convex.NodeParams (ledgerProtocolParameters)
import Convex.ThreatModel.Cardano.Api (dummyTxId, mkSizedShelleyTxOut, recalculateTotalCollateral)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Map qualified as Map
import Data.Set qualified as Set
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
testTx fee =
  let body =
        Ledger.mkBasicTxBody
          { Conway.ctbCollateralInputs = Set.singleton (C.toShelleyTxIn collateralTxIn)
          , Conway.ctbTxfee = fee
          }
      unitRedeemer = C.toAlonzoData (C.unsafeHashableScriptData (C.ScriptDataNumber 42))
      redeemers = Ledger.Redeemers (Map.singleton (Conway.ConwaySpending (Ledger.AsIx 0)) (unitRedeemer, ExUnits 0 0))
      scriptData = C.TxBodyScriptData C.AlonzoEraOnwardsConway (Ledger.TxDats mempty) redeemers
   in C.Tx (C.ShelleyTxBody C.ShelleyBasedEraConway body [] scriptData Nothing C.TxScriptValidityNone) []

collateralTxIn :: C.TxIn
collateralTxIn = C.TxIn dummyTxId (C.TxIx 0)

walletAddr :: C.AddressInEra C.ConwayEra
walletAddr = Wallet.addressInEra Defaults.networkId Wallet.w1

{- | A UTxO set resolving the collateral input to an output at the wallet's
key address, carrying the given value.
-}
utxoWith :: C.Value -> C.UTxO C.ConwayEra
utxoWith v = C.UTxO (Map.singleton collateralTxIn (mkOut v))

mkOut :: C.Value -> C.TxOut ctx C.ConwayEra
mkOut v = C.TxOut walletAddr (C.TxOutValueShelleyBased C.ShelleyBasedEraConway (C.toMaryValue v)) C.TxOutDatumNone C.ReferenceScriptNone

tokens :: C.Value
tokens = C.valueFromList [(C.AssetId testPolicy testAssetName, 5)]
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
