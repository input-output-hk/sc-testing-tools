{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Unit tests for 'runningPlutusScriptHashes' and 'scriptHashOfAddressAny'
in "Convex.ThreatModel.Cardano.Api" — the two halves of the guard that
'Convex.ThreatModel.guardedScriptOutputs' applies, whose rationale is
documented there.
-}
module RunningScriptsSpec (runningScriptsTests) where

import Cardano.Api qualified as C
import Cardano.Ledger.Api.Tx.Body qualified as Ledger (mkBasicTxBody)
import Cardano.Ledger.Conway.TxBody qualified as Conway (ctbSpendInputs)
import Cardano.Ledger.Hashes qualified as Ledger (ScriptHash)
import Convex.ThreatModel.Cardano.Api (isKeyAddressAny, keyAddressAny, makeTxOut, runningPlutusScriptHashes, scriptAddressAny, scriptHashOfAddressAny)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Data.Map qualified as Map
import Data.Set qualified as Set
import Scripts qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestTx (mkConwayTx, spendRedeemerAt, txInAt)

-- | The Plutus script the transaction under test actually spends.
spentScript :: C.Script C.PlutusScriptV3
spentScript = C.PlutusScript C.PlutusScriptV3 Scripts.pingPongValidatorScript

spentScriptHash :: C.ScriptHash
spentScriptHash = C.hashScript spentScript

{- | A different Plutus script, never spent here — it only ever appears as
the address of an output being paid into. This is the false positive the
guard exists to rule out.
-}
paidOnlyScriptHash :: C.ScriptHash
paidOnlyScriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV1 (C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn))

-- | A native script, spent by 'nativeTx'.
nativeScript :: C.Script C.SimpleScript'
nativeScript = C.SimpleScript (C.RequireAllOf [])

nativeScriptHash :: C.ScriptHash
nativeScriptHash = C.hashScript nativeScript

{- | A transaction spending the single input at index 0, carrying the given
script in its witness set so it counts as provided.
-}
mkTx :: C.TxBodyScriptData C.ConwayEra -> C.ScriptInEra C.ConwayEra -> C.Tx C.ConwayEra
mkTx scriptData scriptInEra =
  mkConwayTx
    Ledger.mkBasicTxBody{Conway.ctbSpendInputs = Set.singleton (C.toShelleyTxIn (txInAt 0))}
    [C.toShelleyScript scriptInEra]
    scriptData

-- | A UTxO set resolving the spent input to an output at the given address.
utxoAt :: C.AddressAny -> C.UTxO C.ConwayEra
utxoAt addr =
  C.UTxO $
    Map.singleton
      (txInAt 0)
      (makeTxOut addr (C.lovelaceToValue 2_000_000) C.TxOutDatumNone C.ReferenceScriptNone)

runningIn :: C.UTxO C.ConwayEra -> C.Tx C.ConwayEra -> Set.Set Ledger.ScriptHash
runningIn utxo tx = runningPlutusScriptHashes (C.toLedgerUTxO C.ShelleyBasedEraConway utxo) tx

{- | The test's stand-in for what 'Convex.ThreatModel.guardedScriptOutputs'
does per output: match the address's payment credential against the set.
-}
guards :: C.UTxO C.ConwayEra -> C.Tx C.ConwayEra -> C.AddressAny -> Bool
guards utxo tx addr = maybe False (`Set.member` runningIn utxo tx) (scriptHashOfAddressAny addr)

-- | Spending a Plutus script input: that script runs.
plutusTx :: C.Tx C.ConwayEra
plutusTx = mkTx (spendRedeemerAt 0) (C.ScriptInEra C.PlutusScriptV3InConway spentScript)

plutusUtxo :: C.UTxO C.ConwayEra
plutusUtxo = utxoAt (scriptAddressAny spentScriptHash)

-- | Spending a native script input: no Plutus code runs.
nativeTx :: C.Tx C.ConwayEra
nativeTx = mkTx (spendRedeemerAt 0) (C.ScriptInEra C.SimpleScriptInConway nativeScript)

nativeUtxo :: C.UTxO C.ConwayEra
nativeUtxo = utxoAt (scriptAddressAny nativeScriptHash)

walletAddr :: C.AddressAny
walletAddr = keyAddressAny (Wallet.verificationKeyHash Wallet.w1)

runningScriptsTests :: TestTree
runningScriptsTests =
  testGroup
    "running scripts"
    [ testGroup
        "runningPlutusScriptHashes"
        [ testCase "a spent Plutus script counts as running" $
            assertBool "expected the spent script's hash" $
              C.toShelleyScriptHash spentScriptHash `Set.member` runningIn plutusUtxo plutusTx
        , testCase "a spent native script does not count as running" $
            assertBool "expected no Plutus script" $
              Set.null (runningIn nativeUtxo nativeTx)
        , testCase "no redeemers means nothing runs" $
            assertBool "expected no Plutus script" $
              Set.null (runningIn plutusUtxo (mkTx C.TxBodyNoScriptData (C.ScriptInEra C.PlutusScriptV3InConway spentScript)))
        ]
    , testGroup
        "guarding an output's address"
        [ testCase "an output at the running script's address is guarded" $
            assertBool "expected guarded" $
              guards plutusUtxo plutusTx (scriptAddressAny spentScriptHash)
        , testCase "an output at a script that is only paid into is not guarded" $
            assertBool "expected unguarded" $
              not (guards plutusUtxo plutusTx (scriptAddressAny paidOnlyScriptHash))
        , testCase "an output at a key address is not guarded" $
            assertBool "expected unguarded" $
              not (guards plutusUtxo plutusTx walletAddr)
        , testCase "a native script guards nothing, even at its own address" $
            assertBool "expected unguarded" $
              not (guards nativeUtxo nativeTx (scriptAddressAny nativeScriptHash))
        , {- A guarded address always has a script payment credential, so the
          "not a key address" clause the output-mutation models used to apply
          on top of the guard can never reject anything — which is why those
          clauses were dropped from their predicates.
          -}
          testCase "a guarded address is never a key address" $
            let addr = scriptAddressAny spentScriptHash
             in assertBool "expected guarded and not a key address" $
                  guards plutusUtxo plutusTx addr && not (isKeyAddressAny addr)
        ]
    ]
