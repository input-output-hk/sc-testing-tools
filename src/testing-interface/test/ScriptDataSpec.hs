{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Unit tests for the redeemer re-indexing helpers in
"Convex.ThreatModel.Cardano.Api". Redeemers are keyed by (purpose, index),
where the index points into the item set of that purpose: spend inputs for
Spending, minted policy ids for Minting, reward accounts for Rewarding, and
so on. A modifier that adds or removes an item of one purpose must shift
only the redeemers of that purpose and leave every other purpose untouched,
otherwise the transaction fails phase 1 (MissingRedeemer/ExtraRedeemers)
and the attack is silently skipped instead of judged.
-}
module ScriptDataSpec (scriptDataTests) where

import Cardano.Api qualified as C
import Cardano.Ledger.Api.Tx.Body qualified as Ledger (mkBasicTxBody)
import Cardano.Ledger.Api.Tx.Wits qualified as Ledger (AsIx (AsIx), Redeemers (Redeemers), TxDats (TxDats))
import Cardano.Ledger.Conway.Scripts qualified as Conway (ConwayPlutusPurpose (..))
import Cardano.Ledger.Plutus (ExUnits (..))
import Convex.ThreatModel.Cardano.Api (recomputeScriptData, recomputeScriptDataForMint, txRunsPlutusScript)
import Data.Map qualified as Map
import Data.Word (Word32)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestTx (mkConwayTx)

type Purpose = Conway.ConwayPlutusPurpose Ledger.AsIx (C.ShelleyLedgerEra C.ConwayEra)

{- | Every purpose has a redeemer at index 0 and Spending and Minting also
have one at index 1, so shifting the wrong purpose is always observable.
The redeemer payload records the original key, so the test can also check
that the moved redeemers are the right ones and not just the right count.
-}
allPurposes :: [Purpose]
allPurposes =
  [ Conway.ConwaySpending (Ledger.AsIx 0)
  , Conway.ConwaySpending (Ledger.AsIx 1)
  , Conway.ConwayMinting (Ledger.AsIx 0)
  , Conway.ConwayMinting (Ledger.AsIx 1)
  , Conway.ConwayCertifying (Ledger.AsIx 0)
  , Conway.ConwayRewarding (Ledger.AsIx 0)
  ]

scriptData :: C.TxBodyScriptData C.ConwayEra
scriptData = redeemersOnly allPurposes

-- | Script data holding only the given redeemers.
redeemersOnly :: [Purpose] -> C.TxBodyScriptData C.ConwayEra
redeemersOnly ps =
  C.TxBodyScriptData
    C.AlonzoEraOnwardsConway
    (Ledger.TxDats mempty)
    (Ledger.Redeemers $ Map.fromList [(p, (C.toAlonzoData (payload p), ExUnits 0 0)) | p <- ps])

-- | A payload that identifies the redeemer's original purpose and index.
payload :: Purpose -> C.HashableScriptData
payload p = C.unsafeHashableScriptData $ C.ScriptDataList [C.ScriptDataBytes (tag p), C.ScriptDataNumber (fromIntegral (idx p))]
 where
  tag = \case
    Conway.ConwaySpending{} -> "spend"
    Conway.ConwayMinting{} -> "mint"
    Conway.ConwayRewarding{} -> "reward"
    Conway.ConwayCertifying{} -> "cert"
    _ -> "other"

idx :: Purpose -> Word32
idx = \case
  Conway.ConwaySpending (Ledger.AsIx i) -> i
  Conway.ConwayMinting (Ledger.AsIx i) -> i
  Conway.ConwayRewarding (Ledger.AsIx i) -> i
  Conway.ConwayCertifying (Ledger.AsIx i) -> i
  Conway.ConwayVoting (Ledger.AsIx i) -> i
  Conway.ConwayProposing (Ledger.AsIx i) -> i

-- | The redeemers of the result as a list of (new key, original key), in key order.
redeemerMoves :: C.TxBodyScriptData C.ConwayEra -> IO [(Purpose, Purpose)]
redeemerMoves = \case
  C.TxBodyNoScriptData -> assertFailure "expected script data"
  C.TxBodyScriptData _ _ (Ledger.Redeemers rdmrs) ->
    pure [(k, original (C.getScriptData (C.fromAlonzoData d))) | (k, (d, _)) <- Map.toList rdmrs]
 where
  original d = case [p | p <- allPurposes, C.getScriptData (payload p) == d] of
    [p] -> p
    _ -> error ("unrecognised redeemer payload: " <> show d)

-- | Shift used after inserting an item at index @i@.
insertAt :: Word32 -> Word32 -> Word32
insertAt i ix
  | ix >= i = ix + 1
  | otherwise = ix

-- | Shift used after removing the item at index @i@.
removeAt :: Word32 -> Word32 -> Word32
removeAt i ix
  | ix > i = ix - 1
  | otherwise = ix

-- | A minimal transaction carrying the given script data and nothing else.
txWithScriptData :: C.TxBodyScriptData C.ConwayEra -> C.Tx C.ConwayEra
txWithScriptData sd = mkConwayTx Ledger.mkBasicTxBody [] sd

spend, mint, reward, cert :: Word32 -> Purpose
spend = Conway.ConwaySpending . Ledger.AsIx
mint = Conway.ConwayMinting . Ledger.AsIx
reward = Conway.ConwayRewarding . Ledger.AsIx
cert = Conway.ConwayCertifying . Ledger.AsIx

scriptDataTests :: TestTree
scriptDataTests =
  testGroup
    "redeemer re-indexing"
    [ testGroup
        "txRunsPlutusScript"
        [ testCase "no script data means no script runs" $
            assertBool "expected False" $
              not (txRunsPlutusScript (txWithScriptData C.TxBodyNoScriptData))
        , testCase "script data without redeemers means no script runs" $
            assertBool "expected False" $
              not (txRunsPlutusScript (txWithScriptData (redeemersOnly [])))
        , testCase "a Rewarding redeemer alone counts as a script run" $
            assertBool "expected True" $
              txRunsPlutusScript (txWithScriptData (redeemersOnly [reward 0]))
        , testCase "a Minting redeemer alone counts as a script run" $
            assertBool "expected True" $
              txRunsPlutusScript (txWithScriptData (redeemersOnly [mint 0]))
        ]
    , testGroup
        "recomputeScriptData (spend inputs)"
        [ testCase "inserting a spend input shifts only Spending redeemers" $ do
            moves <- redeemerMoves (recomputeScriptData Nothing (insertAt 0) scriptData)
            moves
              @?= [ (spend 1, spend 0)
                  , (spend 2, spend 1)
                  , (mint 0, mint 0)
                  , (mint 1, mint 1)
                  , (cert 0, cert 0)
                  , (reward 0, reward 0)
                  ]
        , testCase "removing a spend input drops only that Spending redeemer" $ do
            moves <- redeemerMoves (recomputeScriptData (Just 0) (removeAt 0) scriptData)
            moves
              @?= [ (spend 0, spend 1)
                  , (mint 0, mint 0)
                  , (mint 1, mint 1)
                  , (cert 0, cert 0)
                  , (reward 0, reward 0)
                  ]
        , testCase "no script data stays no script data" $
            case recomputeScriptData (Just 0) (removeAt 0) C.TxBodyNoScriptData of
              C.TxBodyNoScriptData -> pure ()
              C.TxBodyScriptData{} -> assertFailure "expected TxBodyNoScriptData"
        ]
    , testGroup
        "recomputeScriptDataForMint (minted policies)"
        [ testCase "inserting a policy shifts only Minting redeemers" $ do
            moves <- redeemerMoves (recomputeScriptDataForMint Nothing (insertAt 0) scriptData)
            moves
              @?= [ (spend 0, spend 0)
                  , (spend 1, spend 1)
                  , (mint 1, mint 0)
                  , (mint 2, mint 1)
                  , (cert 0, cert 0)
                  , (reward 0, reward 0)
                  ]
        , testCase "removing a policy drops only that Minting redeemer" $ do
            moves <- redeemerMoves (recomputeScriptDataForMint (Just 1) (removeAt 1) scriptData)
            moves
              @?= [ (spend 0, spend 0)
                  , (spend 1, spend 1)
                  , (mint 0, mint 0)
                  , (cert 0, cert 0)
                  , (reward 0, reward 0)
                  ]
        ]
    ]
