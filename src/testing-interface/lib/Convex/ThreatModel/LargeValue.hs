{-# LANGUAGE OverloadedStrings #-}

{- | Threat model for detecting Large Value Attack vulnerabilities.

A Large Value Attack exploits validators that don't properly validate the
structure of @Value@ in their outputs. If a validator allows spending from
a script output without checking what tokens are present in the output's value,
an attacker can "bloat" the value with additional junk tokens.

== Consequences ==

1. __Increased min-UTxO requirements__: Each unique token in a UTxO increases
   the minimum Ada required. Adding many junk tokens forces the victim to
   lock more Ada than intended.

2. __Serialization costs__: Large values increase transaction size, consuming
   more of the victim's fee budget when spending the UTxO.

3. __Permanent fund locking__: If the value is bloated sufficiently:

   - The transaction required to spend the UTxO may exceed protocol size limits
   - The serialized output may exceed the max-value-size protocol parameter

   In these cases, the UTxO becomes __permanently unspendable__ and funds
   are locked forever with no possibility of recovery.

== Root Cause ==

Validators that don't check the @Value@ structure of outputs being created.
For example, a validator that only checks:

@
traceIfFalse "insufficient payment" (valuePaidTo pkh >= expectedAmount)
@

This allows an attacker to include @expectedAmount + junkTokens@, satisfying
the check while bloating the output.

== Mitigation ==

A secure validator should either:

- Whitelist expected tokens (only allow known policy IDs)
- Check the token count (e.g., @length (flattenValue v) <= maxTokens@)
- Require exact value match (not just @>=@ comparison)
- Validate that outputs contain only expected assets

This threat model tests if a script output can have arbitrary tokens added
to its value via minting. If the transaction still validates, the validator
has a Large Value Attack vulnerability.
-}
module Convex.ThreatModel.LargeValue (
  largeValueAttack,
  largeValueAttackWith,
  largeValueAttackWithGen,
) where

import Cardano.Api qualified as C
import Convex.ThreatModel
import Convex.ThreatModel.TxModifier (addPlutusScriptMint, alwaysSucceedsMintingPolicy)
import Data.ByteString.Char8 qualified as BS
import GHC.Exts (fromList)
import Test.QuickCheck (Gen, choose, shrinkIntegral)

{- | Default large-value attack. The number of junk tokens is drawn per
transaction from a curated range, so QuickCheck explores the parameter space
and shrinks counterexamples toward the smallest triggering value.
-}
largeValueAttack :: ThreatModel ()
largeValueAttack = largeValueAttackWithGen (choose (1, 100))

{- | Large-value attack with a fixed junk-token count. Keep using this for
deterministic regression tests and golden seeds.
-}
largeValueAttackWith :: Int -> ThreatModel ()
largeValueAttackWith = largeValueAttackWithGen . pure

{- | Large-value attack parameterised by a generator for the number of junk
tokens minted and added to a script output. This is the primitive the other
two forms delegate to.
-}
largeValueAttackWithGen :: Gen Int -> ThreatModel ()
largeValueAttackWithGen numTokensGen =
  Named "Large Value Attack" $ do
    numTokens <- forAllTM numTokensGen shrinkPositive

    -- Skip iterations where the draw is too small to be a meaningful attack.
    ensure (numTokens >= 1)

    requireScriptInput

    -- Get all outputs from the transaction
    outputs <- getTxOutputs

    -- Filter to script outputs (NOT key addresses)
    let scriptOutputs = filter (not . isKeyAddressAny . addressOf) outputs

    -- Precondition: there must be at least one script output
    threatPrecondition $ ensure (not $ null scriptOutputs)

    -- Pick a target script output
    target <- pickAny scriptOutputs

    -- Create junk tokens by minting with the always-succeeds policy
    let policyId = C.PolicyId $ hashScript (C.PlutusScript C.PlutusScriptV2 alwaysSucceedsMintingPolicy)
        junkTokens =
          [ (C.UnsafeAssetName $ BS.pack $ "junk" ++ show i, C.Quantity 1)
          | i <- [1 .. numTokens]
          ]
        junkValue =
          fromList
            [ (C.AssetId policyId name, qty)
            | (name, qty) <- junkTokens
            ]
        bloatedValue = valueOf target <> junkValue

    counterexampleTM $
      paragraph
        [ "The transaction contains a script output at index"
        , show (outputIx target)
        , "."
        ]

    counterexampleTM $
      paragraph
        [ "Testing if"
        , show numTokens
        , "junk tokens can be minted and added to the output's value"
        , "while still passing validation."
        ]

    counterexampleTM $
      paragraph
        [ "If this validates, the script's value validation is permissive."
        , "An attacker could exploit this to:"
        , "1) Increase min-UTxO requirements, locking victim's Ada"
        , "2) Inflate transaction sizes, increasing spending costs"
        , "3) Potentially lock funds permanently if size limits are exceeded"
        ]

    tabulateTM "junk tokens" [bucket numTokens]

    -- Create mint modifiers for all junk tokens
    let mintModifiers =
          mconcat
            [ addPlutusScriptMint alwaysSucceedsMintingPolicy name qty (toScriptData ())
            | (name, qty) <- junkTokens
            ]

    -- This SHOULD fail - if it validates, the contract is vulnerable
    -- The attack: mint junk tokens AND add them to the target output
    shouldNotValidate $
      changeValueOf target bloatedValue
        <> mintModifiers

{- | Shrink a positive integer toward 1 (the smallest meaningful value),
never reaching 0.
-}
shrinkPositive :: Int -> [Int]
shrinkPositive = filter (>= 1) . shrinkIntegral

-- | Coarse bucket for the parameter distribution report.
bucket :: Int -> String
bucket n
  | n <= 10 = "001-010"
  | n <= 50 = "011-050"
  | n <= 100 = "051-100"
  | otherwise = "101+"
