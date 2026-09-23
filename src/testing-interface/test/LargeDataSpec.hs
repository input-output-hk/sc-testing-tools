{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "Convex.ThreatModel.LargeData".

The interesting property is what the attack does with a datum it cannot
bloat. 'bloatData' grows the two container shapes a generated @FromData@
parser produces (@Constr@ and @List@) and returns everything else unchanged;
an unchanged datum makes @changeDatumOf@ a no-op, and asserting
'Convex.ThreatModel.shouldNotValidate' on an unmodified transaction would
report a "vulnerability" for a transaction the attack never touched. The
end-to-end cases below pin down both halves of that: a list-encoded datum is
bloated and really validated, a bare integer datum is skipped.
-}
module LargeDataSpec (largeDataTests) where

import Cardano.Api qualified as C
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.Trans (lift)
import Convex.BuildTx (execBuildTx)
import Convex.BuildTx qualified as BuildTx
import Convex.Class (MonadMockchain)
import Convex.Class qualified
import Convex.CoinSelection (BalanceTxError, ChangeOutputPosition (TrailingChange))
import Convex.MockChain (runMockchain0IOWith)
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.MockChain.Defaults qualified as Defaults
import Convex.ThreatModel (
  SigningWallet (SignWith),
  ThreatModelOutcome (TMFailed, TMSkipped),
  mkThreatModelEnv,
  runThreatModelCheck,
 )
import Convex.ThreatModel.LargeData (bloatData, largeDataAttackWith)
import Convex.Wallet.MockWallet qualified as Wallet
import Data.ByteString qualified as BS
import Data.List (nub)
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import Scripts qualified
import Test.Tasty (TestTree, testGroup)

import Convex.Tasty.HUnit (assertFailure, testCase, (@?=))

largeDataTests :: TestTree
largeDataTests =
  testGroup
    "large data attack"
    [ testGroup
        "bloatData"
        [ -- A record's fields are heterogeneous, so there is no element type
          -- to mirror; a generated Constr parser either ignores the extra
          -- fields or rejects the field list outright, whatever they hold.
          testCase "appends fields to a Constr datum" $
            bloatData 2 (C.ScriptDataConstructor 1 [C.ScriptDataBytes "k"])
              @?= C.ScriptDataConstructor 1 [C.ScriptDataBytes "k", num 42, num 42]
        , -- A list-encoded datum (plutus-tx's @makeIsDataAsList@, or an Aiken
          -- type declared as a list) is a List on-chain, not a Constr, and is
          -- parsed with @pasList@ plus positional access - which ignores
          -- trailing elements exactly as a Constr parser ignores extra fields.
          testCase "appends elements to a List datum" $
            bloatData 2 (C.ScriptDataList [num 7])
              @?= C.ScriptDataList [num 7, num 7, num 7]
        , -- The junk mirrors an existing element rather than defaulting to a
          -- number: a strict 'FromData' instance traverses every member, so
          -- junk of the wrong type would make the whole datum unparseable and
          -- the validator would reject it for a reason unrelated to bloat.
          testCase "mirrors the element type of a List datum" $
            bloatData 2 (C.ScriptDataList [C.ScriptDataBytes "abc"])
              @?= C.ScriptDataList (replicate 3 (C.ScriptDataBytes "abc"))
        , testCase "falls back to a number for an empty List datum" $
            bloatData 2 (C.ScriptDataList []) @?= C.ScriptDataList [num 42, num 42]
        , -- Appending under keys already in the map could change what an
          -- existing lookup answers, so the added keys must be fresh: past
          -- the largest number key here.
          testCase "appends entries to a Map datum under fresh number keys" $
            bloatData 2 (C.ScriptDataMap [(num 3, num 9), (num 7, num 9)])
              @?= C.ScriptDataMap
                [(num 3, num 9), (num 7, num 9), (num 8, num 9), (num 9, num 9)]
        , -- With byte-string keys, a key longer than every existing one is
          -- fresh by construction, and the fixed-width suffix keeps the added
          -- keys distinct from each other.
          testCase "appends entries to a Map datum under fresh bytes keys" $
            assertBloatedMap 2 [(C.ScriptDataBytes "ab", C.ScriptDataBytes "v")] $ \added ->
              [k | (k@C.ScriptDataBytes{}, _) <- added] @?= map fst added
        , -- A key does not have to be an atom. 'Map AssetClass Integer' has
          -- Constr keys, and a key of some shape of our own (a number, say)
          -- would make a strict 'FromData' reject the whole datum, so the
          -- generated keys have to keep the template's structure and perturb
          -- a leaf inside it.
          testCase "appends entries to a Map datum under fresh Constr keys"
            $ assertBloatedMap
              2
              [(C.ScriptDataConstructor 0 [C.ScriptDataBytes "p", num 1], num 9)]
            $ \added ->
              [ idx
              | (C.ScriptDataConstructor idx [C.ScriptDataBytes{}, C.ScriptDataNumber{}], _) <- added
              ]
                @?= [0, 0]
        , -- A key with no leaf to perturb yields no type-correct fresh key,
          -- so the datum comes back unchanged and the attack skips rather
          -- than asserting against an untouched transaction.
          testCase "leaves a Map datum with unperturbable keys unchanged" $ do
            let leafless = C.ScriptDataMap [(C.ScriptDataConstructor 0 [], num 9)]
            bloatData 2 leafless @?= leafless
        , -- The junk is replicated up to 1000 times by the default attack, so
          -- its size must not scale with the datum's: a long byte string is
          -- truncated (still a byte string, so it still parses) rather than
          -- copied whole.
          testCase "does not scale junk size with the mirrored member" $ do
            let long = C.ScriptDataBytes (BS.replicate 400 0)
            case bloatData 3 (C.ScriptDataList [long]) of
              C.ScriptDataList (_ : added) -> do
                length added @?= 3
                [BS.length bs | C.ScriptDataBytes bs <- added] @?= replicate 3 8
              other -> assertFailure $ "expected a List, got " <> show other
        , -- ... and the member copied is the smallest, not the first.
          testCase "mirrors the smallest member of a List datum" $
            bloatData 1 (C.ScriptDataList [C.ScriptDataList [num 1, num 2], C.ScriptDataList []])
              @?= C.ScriptDataList
                [C.ScriptDataList [num 1, num 2], C.ScriptDataList [], C.ScriptDataList []]
        , -- The shapes that cannot be grown. Each must come back unchanged,
          -- because that is the signal 'largeDataAttackWith' turns into a
          -- skipped precondition.
          testCase "leaves a Number datum unchanged" $
            bloatData 2 (num 7) @?= num 7
        , testCase "leaves a Bytes datum unchanged" $
            bloatData 2 (C.ScriptDataBytes "abc") @?= C.ScriptDataBytes "abc"
        ]
    , testGroup
        "against a script output whose inline datum is"
        [ -- The bloated list datum has to survive being put back into the
          -- transaction (rebalancing, re-signing, Phase 1) for the attack to
          -- reach its assertion at all: a Phase 1 rejection would come back
          -- as 'TMSkippedPhase1' instead.
          testCase "list-encoded: the bloated datum is validated" $ do
            outcome <- runAttackOn ([1, 2] :: [Integer])
            case outcome of
              TMFailed _ -> pure ()
              other ->
                assertFailure $
                  "The sample validator ignores its datum, so it accepts a bloated "
                    <> "list datum and the attack should report a vulnerability, but got: "
                    <> show other
        , testCase "a map: the bloated datum is validated" $ do
            outcome <- runAttackOn (AssocMap.unsafeFromList [(1 :: Integer, 2 :: Integer)])
            case outcome of
              TMFailed _ -> pure ()
              other ->
                assertFailure $
                  "The sample validator ignores its datum, so it accepts a bloated "
                    <> "map datum and the attack should report a vulnerability, but got: "
                    <> show other
        , -- Regression test: 'bloatData' cannot grow a bare integer, so
          -- before the no-op check the attack asserted against a
          -- byte-for-byte unmodified transaction - which validates, because
          -- it was submitted successfully - and reported 'TMFailed' for every
          -- such contract.
          testCase "a bare integer: the attack is skipped, not reported" $ do
            outcome <- runAttackOn (42 :: Integer)
            outcome @?= TMSkipped
        ]
    ]
 where
  num = C.ScriptDataNumber

  -- \| Bloat a map datum and check the shared properties of the result: the
  --  original entries come first and unchanged, @n@ entries were added, their
  --  keys are fresh and distinct, and each carries a value mirroring the
  --  existing one. The per-shape key assertion is left to the caller.
  --
  assertBloatedMap n kvs assertKeys =
    case bloatData n (C.ScriptDataMap kvs) of
      C.ScriptDataMap bloated -> do
        take (length kvs) bloated @?= kvs
        let added = drop (length kvs) bloated
        length added @?= n
        map snd added @?= replicate n (snd (head kvs))
        length (nub (map fst bloated)) @?= length bloated
        assertKeys added
      other -> assertFailure $ "expected a Map, got " <> show other

{- | Run @largeDataAttackWith 10@ against a transaction that spends the sample
script and pays back to it, carrying @datum@ as the inline datum of its only
script output.

The sample validator only checks its redeemer - it looks at neither the datum
it is spending nor the transaction's outputs - so the datum can be any shape,
and the validator accepts whatever the attack does to it. That isolates the
attack's own behaviour: the outcome says what the attack did, not what a
particular validator thinks of it.
-}
runAttackOn :: (PlutusTx.ToData a) => a -> IO ThreatModelOutcome
runAttackOn datum = do
  (result, _finalState) <-
    runMockchain0IOWith Wallet.initialUTxOs Defaults.nodeParams $
      runExceptT $ do
        (tx, chainStateBefore) <- scenario
        lift $
          fst
            <$> runThreatModelCheck
              (SignWith Wallet.w1)
              (largeDataAttackWith 10)
              [mkThreatModelEnv tx chainStateBefore]
  either (assertFailure . ("Mockchain error: " <>) . show) pure result
 where
  scenario
    :: ( MonadMockchain C.ConwayEra m
       , MonadError (BalanceTxError C.ConwayEra) m
       , MonadFail m
       )
    => m (C.Tx C.ConwayEra, Convex.Class.MockChainState C.ConwayEra)
  scenario = do
    -- Lock funds at the sample script address
    lockTx <-
      tryBalanceAndSubmit
        mempty
        Wallet.w1
        ( execBuildTx
            ( BuildTx.payToScriptDatumHash
                Defaults.networkId
                (C.PlutusScript C.plutusScriptVersion Scripts.sampleValidatorScript)
                ()
                C.NoStakeAddress
                (C.lovelaceToValue 10_000_000)
            )
        )
        TrailingChange
        []

    -- Capture the chain state before the transaction under test
    chainStateBefore <- Convex.Class.getMockChainState

    -- Spend the script output and pay back to the script with the datum
    -- shape under test. The transaction under test therefore has exactly one
    -- script output with an inline datum, so the attack's 'pickAny' over the
    -- candidate outputs has nothing to choose between.
    let txIn = C.TxIn (C.getTxId (C.getTxBody lockTx)) (C.TxIx 0)
    spendTx <-
      tryBalanceAndSubmit
        mempty
        Wallet.w1
        ( execBuildTx $ do
            Scripts.spendSample (Scripts.SampleRedeemer True True) txIn
            BuildTx.payToScriptInlineDatum
              Defaults.networkId
              (C.hashScript (C.PlutusScript C.plutusScriptVersion Scripts.sampleValidatorScript))
              datum
              C.NoStakeAddress
              (C.lovelaceToValue 5_000_000)
        )
        TrailingChange
        []

    pure (spendTx, chainStateBefore)
