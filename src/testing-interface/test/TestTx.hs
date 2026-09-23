{- | Fixtures shared by the unit specs that build bare Conway transactions
by hand ('RebalanceSpec', 'ScriptDataSpec', 'RunningScriptsSpec').

These specs test pure functions over a transaction body and a UTxO set, so
they assemble a 'C.Tx' directly rather than going through the mockchain.
The point of sharing is 'mkConwayTx': it is the one place the
'C.ShelleyTxBody' shape is spelled out, so a cardano-api bump that changes
its arity touches one file instead of three.
-}
module TestTx (
  mkConwayTx,
  txInAt,
  spendRedeemerAt,
) where

import Cardano.Api qualified as C
import Cardano.Ledger.Api.Tx.Wits qualified as Ledger (AsIx (AsIx), Redeemers (Redeemers), TxDats (TxDats))
import Cardano.Ledger.Conway.Scripts qualified as Conway (ConwayPlutusPurpose (ConwaySpending))
import Cardano.Ledger.Core qualified as Ledger (Script, TxBody)
import Cardano.Ledger.Plutus (ExUnits (..))
import Convex.ThreatModel.Cardano.Api (LedgerEra, dummyTxId)
import Data.Map qualified as Map
import Data.Word (Word32)

{- | A transaction built from a ledger body, the scripts in its witness set
and its script data. No auxiliary data, no script-validity flag, no key
witnesses — none of these specs submit the result.
-}
mkConwayTx
  :: Ledger.TxBody LedgerEra
  -> [Ledger.Script LedgerEra]
  -> C.TxBodyScriptData C.ConwayEra
  -> C.Tx C.ConwayEra
mkConwayTx body scripts scriptData =
  C.Tx (C.ShelleyTxBody C.ShelleyBasedEraConway body scripts scriptData Nothing C.TxScriptValidityNone) []

-- | Inputs of one dummy transaction; 'C.TxIn' orders them by index.
txInAt :: Word -> C.TxIn
txInAt = C.TxIn dummyTxId . C.TxIx

{- | Script data holding a single Spending redeemer at the given index, so
the transaction counts as running a Plutus script. The payload is arbitrary
— nothing here evaluates it.
-}
spendRedeemerAt :: Word32 -> C.TxBodyScriptData C.ConwayEra
spendRedeemerAt ix =
  C.TxBodyScriptData C.AlonzoEraOnwardsConway (Ledger.TxDats mempty) $
    Ledger.Redeemers $
      Map.singleton
        (Conway.ConwaySpending (Ledger.AsIx ix))
        (C.toAlonzoData (C.unsafeHashableScriptData (C.ScriptDataNumber 42)), ExUnits 0 0)
