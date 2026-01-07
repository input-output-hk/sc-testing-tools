# Convex Threat Model

A framework for adversarial testing of Cardano smart contracts using transaction modification and re-validation.

## Overview

The threat model framework allows you to test smart contracts against adversarial attacks by:
1. Capturing valid transactions from your MockChain tests
2. Applying malicious modifications to those transactions
3. Re-validating them to ensure they are properly rejected

This complements traditional property-based testing by focusing on security properties rather than functional correctness.

## Features

- **Transaction Modifiers**: Type-safe modifications to transactions (add/remove inputs/outputs, change datums, modify values)
- **Threat Library**: Pre-built threats for common attack vectors:
  - Invalid state transitions
  - Double satisfaction attacks
  - Large datum attacks
  - Value inflation attacks
- **MockChain Integration**: Seamlessly works with existing Convex MockChain infrastructure
- **Composable**: Threat modifiers can be combined and reused

## Quick Start

```haskell
import Convex.ThreatModel.Core
import Convex.ThreatModel.DSL
import Convex.ThreatModel.Threats.StateTransition

-- Test that invalid state transitions are rejected
testStateTransition :: IO ()
testStateTransition = do
  results <- runMockchain0IO Wallet.initialUTxOs $ do
    -- Setup: create a valid transaction
    tx <- setupMyContract

    -- Run threat tests
    runThreatModel tx $ do
      threat "Invalid state transition"
        (invalidStateTransition InvalidState 0)
        isPredicateFailure

  -- Check results
  print $ filter (not . isThreatDetected) results
```

## Architecture

The framework consists of several modules:

- **Core**: The `ThreatModelT` monad and core types
- **TxModifier**: Transaction modification primitives
- **Validation**: Re-validation using MockChain infrastructure
- **DSL**: High-level combinators for writing threat tests
- **Threats**: Library of common threat types

## Threat Types

### State Transition Threats

Test that state machine contracts reject invalid state transitions:

```haskell
invalidStateTransition :: state -> Int -> TxModifier era
```

### Double Satisfaction Threats

Test that contracts prevent value extraction or redirection:

```haskell
doubleSatisfactionRedirect :: AddressInEra era -> Int -> TxModifier era
doubleSatisfactionExtraOutput :: AddressInEra era -> Value -> TxModifier era
```

### Large Datum Threats

Test handling of abnormally large datums:

```haskell
largeDatumAttacks :: Int -> [TxModifier era]
```

### Value Inflation Threats

Test handling of extreme Ada amounts:

```haskell
valueInflationAttacks :: Int -> [TxModifier era]
```

## Writing Custom Threats

You can create custom threat modifiers using the transaction modification primitives:

```haskell
myCustomThreat :: TxModifier era
myCustomThreat =
  removeOutput 0 <>  -- Remove first output
  addOutput myMaliciousOutput  -- Add attacker's output
```

## Integration with TestingInterface

Threat model tests run as a separate test suite alongside your TestingInterface property tests:

```haskell
main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ testGroup "Property Tests"
      [ testProperty "My contract" (propRunActions @MyModel)
      ]
  , testGroup "Threat Model Tests"
      [ testCase "State transitions" myStateTransitionTests
      , testCase "Double satisfaction" myDoubleSatTests
      ]
  ]
```

## Examples

See the `test/ThreatModel/PingPong/` directory for complete examples of threat testing on the PingPong contract.

## Design

The framework is inspired by IOG's quickcheck-contractmodel threat model but adapted to work with Convex's MockChain infrastructure. Key design decisions:

- **Separation**: Threat testing is separate from TestingInterface
- **Type Safety**: Transaction modifications are type-safe
- **Composability**: Modifiers can be combined using Monoid/Semigroup
- **Read-Only Validation**: Threat tests don't modify the main mockchain state

## License

Apache-2.0
