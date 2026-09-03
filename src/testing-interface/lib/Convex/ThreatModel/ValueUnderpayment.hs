{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Threat model for detecting Value Underpayment vulnerabilities.

A Value Underpayment Attack exploits validators that don't properly verify
that the actual ADA value in an output matches the expected value based on
the datum. If a validator tracks a "balance" in the datum but doesn't verify
the actual ADA matches, an attacker can modify transactions to underpay.

== Example Vulnerability ==

Consider a bank contract where the account datum tracks a balance:

@
data AccountDatum = AccountDatum { balance :: Integer, owner :: PubKeyHash }
@

If the deposit action (IncreaseBalance) only checks that:
- The output datum has an increased balance
- But doesn't verify that the actual ADA value increased by the same amount

Then an attacker can "deposit" by increasing the datum balance without
adding any actual ADA to the output.

== Consequences ==

1. __Free balance increases__: Attacker gains balance without depositing funds
2. __Theft of pooled funds__: If the bank pays out based on datum balance,
   the attacker can withdraw more than they deposited
3. __Insolvency__: Multiple attackers can drain the bank's pooled funds

== Root Cause ==

Validators that:
- Track value in datum without verifying actual UTxO value matches
- Only check datum changes without checking corresponding value changes
- Allow balance increases without requiring matching fund increases

== Mitigation ==

A secure validator should:
- Verify output value matches expected value based on datum
- Check that fund_difference == balance_change for deposits/withdrawals
- Never rely solely on datum for balance tracking

This threat model tests if a script output can have its ADA value reduced
while keeping the datum unchanged. If the transaction still validates,
the validator has a Value Underpayment vulnerability.
-}
module Convex.ThreatModel.ValueUnderpayment (
  valueUnderpaymentAttack,
  valueUnderpaymentAttackWith,
  valueUnderpaymentAttackWithGen,
) where

import Cardano.Api qualified as C
import Convex.ThreatModel
import Test.QuickCheck (Gen, choose, shrinkRealFrac)

{- | Default value-underpayment attack. The reduction factor is drawn per
transaction from a curated range, so QuickCheck explores the parameter space
and shrinks counterexamples toward the smallest triggering value.
-}
valueUnderpaymentAttack :: ThreatModel ()
valueUnderpaymentAttack = valueUnderpaymentAttackWithGen (choose (0.01, 0.99))

{- | Value-underpayment attack with a fixed reduction factor. Keep using
this for deterministic regression tests and golden seeds.
-}
valueUnderpaymentAttackWith :: Double -> ThreatModel ()
valueUnderpaymentAttackWith = valueUnderpaymentAttackWithGen . pure

{- | Value-underpayment attack parameterised by a generator for the ADA
reduction factor (fraction of the original ADA removed from a script output).
This is the primitive the other two forms delegate to.
-}
valueUnderpaymentAttackWithGen :: Gen Double -> ThreatModel ()
valueUnderpaymentAttackWithGen reductionFactorGen =
  Named "Value Underpayment Attack" $ do
    reductionFactor <- forAllTM reductionFactorGen shrinkPositiveDouble

    -- Skip iterations where the draw is too small to be a meaningful attack.
    ensure (reductionFactor > 0)

    requireScriptInput

    {- The floor for the reduced ADA amount has to be each output's own
    protocol-mandated minimum, not a hardcoded guess: 'rebalanceAndSign' runs
    'Convex.ThreatModel.Cardano.Api.topUpUnderfundedOutputs' on every output
    before validation, which would silently restore ADA a reduction below
    that real minimum, masking a genuine underpayment vulnerability behind a
    false "still passes". Flooring at the real minimum here means the
    reduced output is never below it, so that top-up is a no-op and the
    deliberate reduction reaches the validator intact.
    -}
    ThreatModelEnv _ _ envPParams <- getThreatModelEnv
    let minRequiredAda out = C.calculateMinimumUTxO C.shelleyBasedEra (C.unLedgerProtocolParameters envPParams) (outputTxOut out)

    -- Get all outputs from the transaction
    outputs <- getTxOutputs

    -- Filter to script outputs (NOT key addresses)
    let scriptOutputs = filter (not . isKeyAddressAny . addressOf) outputs

    -- Precondition: there must be at least one script output
    threatPrecondition $ ensure (not $ null scriptOutputs)

    -- Further filter to outputs that have enough ADA to be reduced.
    let hasEnoughAda out = C.selectLovelace (valueOf out) > minRequiredAda out
        reducibleOutputs = filter hasEnoughAda scriptOutputs

    -- Precondition: there must be at least one script output with enough ADA
    threatPrecondition $ ensure (not $ null reducibleOutputs)

    -- Pick a target script output
    target <- pickAny reducibleOutputs

    -- Calculate reduced value
    let currentValue = valueOf target
        currentAda = C.selectLovelace currentValue
        requiredAda = minRequiredAda target
        -- Calculate reduced ADA, ensuring we don't go below the output's own
        -- minimum. Lovelace has a Num instance, so we can use numeric
        -- operations.
        reducedAda = max requiredAda (fromInteger $ round (fromIntegral currentAda * (1 - reductionFactor)))
        adaDifference = C.negateValue $ C.lovelaceToValue (currentAda - reducedAda)
        reducedValue = currentValue <> adaDifference

    counterexampleTM $
      paragraph
        [ "The transaction contains a script output at index"
        , show (outputIx target)
        , "."
        ]

    counterexampleTM $
      paragraph
        [ "Testing if the ADA value can be reduced from"
        , show currentAda
        , "to"
        , show reducedAda
        , "(reduction factor:"
        , show (reductionFactor * 100) ++ "%)"
        , "while keeping the datum unchanged."
        ]

    counterexampleTM $
      paragraph
        [ "If this validates, the script's value validation is insufficient."
        , "An attacker could exploit this to:"
        , "1) Increase their balance without depositing matching funds"
        , "2) Steal funds from pooled reserves"
        , "3) Create inconsistency between datum balance and actual UTxO value"
        ]

    tabulateTM "reduction %" [bucketPct reductionFactor]

    -- This SHOULD fail - if it validates, the contract is vulnerable
    -- The attack: reduce the ADA value but keep datum the same
    shouldNotValidate $ changeValueOf target reducedValue

-- | Shrink a positive 'Double' toward 0, discarding non-positive results.
shrinkPositiveDouble :: Double -> [Double]
shrinkPositiveDouble = filter (> 0) . shrinkRealFrac

-- | Coarse bucket for the reduction-factor distribution report.
bucketPct :: Double -> String
bucketPct r
  | r <= 0.25 = "01-25%"
  | r <= 0.50 = "26-50%"
  | r <= 0.75 = "51-75%"
  | otherwise = "76-99%"
