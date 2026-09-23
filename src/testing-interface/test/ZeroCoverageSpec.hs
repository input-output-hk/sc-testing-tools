{-# LANGUAGE OverloadedStrings #-}

{- | Unit tests for the zero-coverage policy of the per-model threat model
test cases ('zeroCoverageVerdict'): which combinations of "why was the model
never tested" and "what does the suite claim about it" fail the test case,
and which are only reported as status lines.
-}
module ZeroCoverageSpec (zeroCoverageTests) where

import Convex.Tasty.Streaming.TMSummary (Fault (..), ThreatModelCategory (..), ThreatModelSummary (..))
import Convex.TestingInterface (ZeroCoverageKind (..), skippedMessage, zeroCoverageKind, zeroCoverageVerdict)
import Convex.ThreatModel (ThreatModelOutcome (..), finalOutcome)
import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- | A summary of a model that was never tested, by skip and error counts.
neverTested :: Int -> Int -> Int -> ThreatModelSummary
neverTested skipped phase1 errors =
  ThreatModelSummary
    { tmsName = "Some Attack"
    , tmsCategory = Claimed
    , tmsTested = 0
    , tmsTotal = skipped + phase1 + errors
    , tmsPassed = 0
    , tmsFailed = 0
    , tmsSkipped = skipped
    , tmsSkippedPhase1 = phase1
    , tmsErrors = errors
    , tmsFault = Nothing
    }

preconditionNeverMet, oneEnvironmentalSkip, allErrors :: ThreatModelSummary
preconditionNeverMet = neverTested 100 0 0
-- A single environmental skip among the precondition misses: this used to
-- silence the coverage failure entirely.
oneEnvironmentalSkip = neverTested 99 1 0
allErrors = neverTested 0 0 100

rebalanceReason :: String
rebalanceReason = "Rebalancing failed: No change output found to wallet address"

-- | The verdict is a failure whose message contains every fragment.
fails :: String -> ThreatModelCategory -> ThreatModelSummary -> [String] -> [String] -> TestTree
fails name claim summary reasons fragments = testCase name $
  case zeroCoverageVerdict claim summary reasons of
    Left (_, msg) -> mapM_ (\f -> assertBool ("expected " <> show f <> " in failure:\n" <> msg) (f `isInfixOf` msg)) fragments
    Right steps -> assertFailure $ "expected a failure, got status lines:\n" <> unlines steps

-- | The verdict is a failure whose message contains none of the fragments.
failsWithout :: String -> ThreatModelCategory -> ThreatModelSummary -> [String] -> [String] -> TestTree
failsWithout name claim summary reasons fragments = testCase name $
  case zeroCoverageVerdict claim summary reasons of
    Left (_, msg) -> mapM_ (\f -> assertBool ("expected " <> show f <> " NOT in failure:\n" <> msg) (not (f `isInfixOf` msg))) fragments
    Right steps -> assertFailure $ "expected a failure, got status lines:\n" <> unlines steps

{- | The verdict is a failure attributed to the given fault. Which fault a
zero-coverage failure carries is the whole point of the distinction: a model
that never applied is a claim you cannot have, while one that applied but
could never be attacked is a limit of the generator or the harness.
-}
blames :: String -> ThreatModelCategory -> ThreatModelSummary -> Fault -> TestTree
blames name claim summary expected = testCase name $
  case zeroCoverageVerdict claim summary [] of
    Left (fault, _) -> fault @?= expected
    Right steps -> assertFailure $ "expected a failure, got status lines:\n" <> unlines steps

-- | The verdict is a set of status lines that together contain every fragment.
reports :: String -> ThreatModelCategory -> ThreatModelSummary -> [String] -> [String] -> TestTree
reports name claim summary reasons fragments = testCase name $
  case zeroCoverageVerdict claim summary reasons of
    Right steps -> mapM_ (\f -> assertBool ("expected " <> show f <> " in status lines:\n" <> unlines steps) (f `isInfixOf` unlines steps)) fragments
    Left (fault, msg) -> assertFailure $ "expected status lines, got a " <> show fault <> " failure:\n" <> msg

zeroCoverageTests :: TestTree
zeroCoverageTests =
  testGroup
    "zero-coverage policy"
    [ testGroup
        "precondition never met"
        [ reports "default list is only reported" Surveyed preconditionNeverMet [] ["SKIPPED: Precondition never met", "0/100 tests applicable"]
        , fails "explicit list fails" Claimed preconditionNeverMet [] ["Threat model never applied", "100 generated transactions", "'threatModels'"]
        , fails "expected vulnerability fails" Expected preconditionNeverMet [] ["Expected vulnerability never exercised", "'expectedVulnerabilities'"]
        ]
    , testGroup
        "attack never carried out"
        [ reports "default list warns loudly, with reasons" Surveyed oneEnvironmentalSkip [rebalanceReason] ["WARNING: zero attack coverage", "1 phase 1/rebalance skipped", rebalanceReason]
        , fails "explicit list fails, with reasons" Claimed oneEnvironmentalSkip [rebalanceReason] ["Threat model never tested", "99 precondition skipped, 1 phase 1/rebalance skipped", rebalanceReason]
        , fails "expected vulnerability fails, with reasons" Expected oneEnvironmentalSkip [rebalanceReason] ["Expected vulnerability never tested", rebalanceReason]
        , fails "reasons are capped at five" Claimed oneEnvironmentalSkip (map (\i -> "reason " <> show i) [1 .. 7 :: Int]) ["reason 5", "... and 2 more"]
        , failsWithout "the sixth reason is not listed" Claimed oneEnvironmentalSkip (map (\i -> "reason " <> show i) [1 .. 7 :: Int]) ["reason 6"]
        ]
    , testGroup
        "model errored"
        [ fails "an all-error run names the error, not the precondition" Claimed allErrors ["No signing wallet found"] ["Threat model never tested", "errored before it could attack anything", "100 errors", "No signing wallet found"]
        , failsWithout "an all-error run does not blame the precondition" Claimed allErrors ["No signing wallet found"] ["precondition held"]
        , reports "default list warns instead of failing" Surveyed allErrors ["No signing wallet found"] ["WARNING: zero attack coverage", "errored before it could attack anything"]
        , fails "expected vulnerability fails too" Expected allErrors ["No signing wallet found"] ["Expected vulnerability never tested", "errored before it could attack anything"]
        ]
    , testGroup
        "no reasons to report"
        [ fails "the sentence ends in a period" Claimed oneEnvironmentalSkip [] ["remove it."]
        , failsWithout "never a dangling colon" Claimed oneEnvironmentalSkip [] [":\n"]
        ]
    , testGroup
        -- 'zeroCoverageVerdict' fails a claimed model on a TMError, so a run
        -- that did carry out an attack must never be reported as one.
        "the runner's outcome ranking feeds the policy"
        [ testCase "a pass on one env survives an error on a later one" $
            finalOutcome True False (Just "no signing wallet") @?= TMPassed
        , testCase "an environmental skip outranks an error" $
            finalOutcome False True (Just "no signing wallet") @?= TMSkippedPhase1
        , testCase "an error speaks only when nothing else happened" $
            finalOutcome False False (Just "no signing wallet") @?= TMError "no signing wallet"
        , testCase "no error and nothing tested is a plain skip" $
            finalOutcome False False Nothing @?= TMSkipped
        , testCase "the two rankings agree on which case is an error" $
            case (finalOutcome False False (Just "boom"), zeroCoverageKind allErrors) of
              (TMError _, ModelErrored) -> pure ()
              other -> assertFailure ("rankings disagree: " <> show (fst other))
        , testCase "the two rankings agree on which case is environmental" $
            case (finalOutcome False True (Just "boom"), zeroCoverageKind oneEnvironmentalSkip) of
              (TMSkippedPhase1, AttackNeverCarriedOut) -> pure ()
              other -> assertFailure ("rankings disagree: " <> show (fst other))
        ]
    , testGroup
        "an accepted finding is judged exactly like an expected vulnerability"
        [ fails "zero coverage fails it too" Accepted preconditionNeverMet [] ["Accepted finding never applied"]
        , fails "the reasons are still listed" Accepted oneEnvironmentalSkip [rebalanceReason] [rebalanceReason]
        , blames "never applying is the declaration's fault" Accepted preconditionNeverMet Declaration
        , blames "never being attacked is the setup's fault" Accepted oneEnvironmentalSkip Setup
        ]
    , testGroup
        "a model declared not applicable is confirmed by zero coverage"
        [ reports "never applying is the passing outcome" NotApplicable preconditionNeverMet [] ["Confirmed not applicable"]
        ]
    , testGroup
        "zero coverage names whose fault it is"
        [ blames "a claim that never applied is the declaration's fault" Claimed preconditionNeverMet Declaration
        , blames "a claim that could not be attacked is the setup's fault" Claimed oneEnvironmentalSkip Setup
        , blames "a model that only errored is the setup's fault" Claimed allErrors Setup
        , blames "an expected vulnerability that never applied is the declaration's fault" Expected preconditionNeverMet Declaration
        , blames "an expected vulnerability that could not be attacked is the setup's fault" Expected oneEnvironmentalSkip Setup
        ]
    , testGroup
        "skipped status line"
        [ testCase "names the precondition when nothing else happened" $
            assertBool (skippedMessage preconditionNeverMet) ("Precondition never met" `isInfixOf` skippedMessage preconditionNeverMet)
        , testCase "names the attack when environmental skips occurred" $
            assertBool (skippedMessage oneEnvironmentalSkip) ("Attack never carried out" `isInfixOf` skippedMessage oneEnvironmentalSkip)
        , testCase "names the error when the model only errored" $
            assertBool (skippedMessage allErrors) ("Threat model errored" `isInfixOf` skippedMessage allErrors)
        , -- "applicable" would name the wrong fault: the model DID apply.
          testCase "does not claim nothing was applicable when the attack was blocked" $
            assertBool (skippedMessage oneEnvironmentalSkip) ("0/100 tests carried out" `isInfixOf` skippedMessage oneEnvironmentalSkip)
        , testCase "still says applicable when the precondition never held" $
            assertBool (skippedMessage preconditionNeverMet) ("0/100 tests applicable" `isInfixOf` skippedMessage preconditionNeverMet)
        ]
    ]
