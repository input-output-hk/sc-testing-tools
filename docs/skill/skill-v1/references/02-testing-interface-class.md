# Reference: The `TestingInterface` class

This reference is the API contract you must satisfy. Everything below is
extracted from `Convex.TestingInterface` in the `convex-testing-interface`
package.

## A. The class signature (full)

```haskell
class (Show state, Eq state, Show (Action state), ToJSON state)
  => TestingInterface state where

  -- | Actions that can be performed on the contract.
  -- One constructor per user-facing operation.
  data Action state

  -- | The initial state of the model, before any actions are performed.
  -- Runs once per QuickCheck iteration, at the start.
  initialize :: (MonadIO m) => TestingMonadT m state

  -- | Generate a random action given the current state.
  arbitraryAction :: state -> Gen (Action state)

  -- | Precondition: must hold before an action is executed.
  -- Default: True.  Override to drive negative testing.
  precondition :: state -> Action state -> Bool
  precondition _ _ = True

  -- | Execute the action on the mockchain AND return the new model state.
  perform :: (MonadIO m) => state -> Action state -> TestingMonadT m state

  -- | Optional invariant check after each action.  Default: True.
  validate :: (MonadIO m) => state -> TestingMonadT m Bool
  validate _ = pure True

  -- | Wrap the QuickCheck property with labels/counterexamples.  Default: id.
  monitoring :: state -> Action state -> Property -> Property
  monitoring _ _ = id

  -- | Discard negative tests that failed via off-chain user error?
  --   False (default) means BOTH off-chain failures and on-chain rejections
  --   are accepted as "the bad action was rejected".  Leave at default for v0.1.
  discardNegativeTestForUserExceptions :: Bool
  discardNegativeTestForUserExceptions = False
```

Minimal complete definition: `data Action`, `initialize`, `arbitraryAction`,
`perform`. The other four have defaults you should accept for v0.1.

## B. Each method explained

### `data Action state`

An associated data family. For each concrete `state` you define, you write one
data declaration:

```haskell
instance TestingInterface MyModel where
  data Action MyModel
    = DoX !Wallet
    | DoY !Wallet !C.Value
    deriving Show
  ...
```

- Each constructor is one user-facing operation.
- Include in each constructor enough data to **describe the action fully**:
  which wallet acts, which value, which datum/redeemer parameters. The
  generator (`arbitraryAction`) will fill these in randomly.
- You need `deriving Show` (required superclass). `Eq` is not required on
  actions but is harmless.
- Use strict fields (`!`) if you want.

### `initialize :: MonadIO m => TestingMonadT m state`

Runs once at the start of every QuickCheck iteration. Use it to:

- Deploy reference scripts or pay setup UTxOs onto the mockchain.
- Mint initial tokens, set initial datums.
- Return the initial value of your model state.

You are in `TestingMonadT`, which gives you `MonadBlockchain ConwayEra` and
`MonadMockchain ConwayEra` instances. Use the canonical
`tryBalanceAndSubmit` idiom (see `03-helpers-cheatsheet.md`).

If the contract requires no on-chain setup, the body can be just
`pure (MyModel ...)`.

The user's project likely already has off-chain helpers that produce the
deployment / setup transaction (ask the user which helper to call). Do not
guess where they live.

### `arbitraryAction :: state -> Gen (Action state)`

QuickCheck generator. Returns a `Gen (Action state)`. Use `oneof` for uniform
choice across constructors, or `frequency` to weight them.

**Before writing this method, read
`references/04-generator-precondition-perform.md`.** The relationship between
`arbitraryAction`, `precondition`, and `perform` is the single most common
source of silent test failures, and the framing there is what you need before
you start.

```haskell
arbitraryAction s = oneof
  [ DoX <$> elements [Wallet.w1, Wallet.w2]
  , DoY <$> elements [Wallet.w1, Wallet.w2] <*> genValue s
  ]
```

Available re-exports: `Gen`, `Arbitrary`, `oneof`, `frequency`, `elements`
from `Convex.TestingInterface`.

### `precondition :: state -> Action state -> Bool`

Default `True`. The framework uses this in two ways:

1. **Positive tests**: only actions where `precondition s a == True` are
   chosen for execution.
2. **Negative tests**: the framework finds an action where
   `precondition s a == False` and asserts the chain rejects it.

Pattern-match per constructor:

```haskell
precondition s (Withdraw w v) = balance s >= valueOf v && owner s == w
precondition _ _              = True
```

`precondition` is the seam between positive and negative testing. The closer
it mirrors what the on-chain validator actually enforces, the more useful
both directions of testing become.

**The full treatment — including the failure modes when `arbitraryAction`,
`precondition`, and `perform` are mis-tuned — is in
`references/04-generator-precondition-perform.md`. Read 04 before
implementing this method.**

### `perform :: state -> Action state -> TestingMonadT m state`

This is where most of the work lives.

For each action constructor:

1. Build the transaction body using whatever off-chain helpers the user's
   project provides (ask if unclear where they live). Wrap with
   `Convex.BuildTx.execBuildTx` if your helper is a `BuildTx` writer;
   otherwise it likely already returns a builder.
2. Submit it via `tryBalanceAndSubmit` (the canonical idiom — fails via
   `fail`/exception on error, which the framework catches and routes
   correctly for both positive and negative tests).
3. Return the **new** model state. Do not return the old `s`.

`perform` must run unconditionally — do not check `precondition` yourself
inside `perform` and skip the submit. The framework runs `perform` for both
positive and negative cases; for negative cases it expects the submit to
fail. See `references/04-generator-precondition-perform.md` §8–9.

```haskell
perform s (Deposit w val) = do
  let tx = execBuildTx (BuildTx.payToAddress (Wallet.addressInEra Defaults.networkId w) val)
  void $ tryBalanceAndSubmit mempty w tx TrailingChange []
  pure s { balance = balance s + val }
```

The argument `s` is the **current** model state (before the action). Use it
to look up any tracked data — e.g. which UTxO to spend, what redeemer to use.

### `validate :: state -> TestingMonadT m Bool`

Default `True`. Optional. Use it to check the chain matches the model after
each action. Typical pattern: query a script UTxO and compare its value
against `balance s`. For v0.1, leave at default.

### `monitoring :: state -> Action state -> Property -> Property`

Default `id`. Used for QuickCheck `label`, `classify`, `cover`, etc. For
v0.1, leave at default.

### `discardNegativeTestForUserExceptions :: Bool`

Default `False`. Leave at default for v0.1.

### Superclasses

```haskell
class (Show state, Eq state, Show (Action state), ToJSON state) => TestingInterface state where ...
```

- `Show state` — `deriving Show`.
- `Eq state` — `deriving Eq`.
- `Show (Action state)` — `deriving Show` on the `data Action` declaration.
- `ToJSON state` — from `Data.Aeson`. The framework uses this to dump model
  states into iteration traces.

For `ToJSON state`, the easiest path is:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data MyModel = MyModel { ... }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
```

If your model contains types that lack `ToJSON` instances (e.g. cardano-api
types), write an orphan and silence the warning at the top of the file:

```haskell
{-# OPTIONS_GHC -Wno-orphans #-}
```

## C. Positive vs negative testing — short summary

The framework runs two QuickCheck properties:

- **Positive tests** filter generated actions to `precondition = True`, run
  them via `perform`, and expect each to succeed.
- **Negative tests** find a generated action with `precondition = False`,
  run it via `perform`, and expect it to be rejected by the chain.

If your generator never produces `precondition = True` actions, the positive
side starves. If it never produces `precondition = False` actions, the
negative side is silently discarded.

**The conceptual heart of this — including the failure modes when the
generator is mis-tuned and how to bias it to cross the validity boundary
— is in `references/04-generator-precondition-perform.md`. Read 04 before
implementing `arbitraryAction`, `precondition`, or `perform`.**

## D. `ThreatModelsFor` class

```haskell
class TestingInterface state => ThreatModelsFor state where
  -- Threat models that should NOT find vulnerabilities (test passes if none found).
  threatModels :: [ThreatModel ()]
  threatModels = deleteFirstsBy <eqByName> allThreatModels (expectedVulnerabilities @state)

  -- Threat models EXPECTED to find vulnerabilities (inverted: test passes if a vuln IS found).
  expectedVulnerabilities :: [ThreatModel ()]
  expectedVulnerabilities = []
```

Notes:

- `expectedVulnerabilities` has **inverted** semantics: a passing test
  means the model **did** detect the vulnerability.
- **Threat-model selection is covered in `references/05-threat-models.md`.**
  Until you reach Step E2 of the SKILL workflow, leave the instance with
  empty lists:
  ```haskell
  instance ThreatModelsFor MyModel where
    threatModels = []
    expectedVulnerabilities = []
  ```
  Empty `threatModels` disables threat-model evaluation entirely; only the
  positive and negative tests run. Once the positive/negative suite passes,
  use 05 §C to pick models and 05 §D to wire them.

## E. `TestingMonadT`

```haskell
newtype TestingMonadT m a = TestingMonadT
  { unTestingMonadT :: ExceptT (BalanceTxError ConwayEra) (MockchainT ConwayEra m) a
  }
```

You almost never need to know this concretely. It provides:

- `MonadIO`
- `MonadFail`
- `MonadBlockchain ConwayEra` — `sendTx`, `utxoByTxIn`, `queryProtocolParameters`, …
- `MonadMockchain ConwayEra` — `getUtxo`, `nextSlot`, `setSlot`, `setPOSIXTime`, …
- `MonadError (BalanceTxError ConwayEra)`

In `initialize` and `perform` you simply use the helpers from
`03-helpers-cheatsheet.md`. The era is fixed to `ConwayEra`; your off-chain
helpers must produce Conway-compatible transaction builders.

## F. `propRunActions` — the runner

```haskell
propRunActions :: forall state. ThreatModelsFor state => String -> TestTree
```

Returns a tasty `TestTree`. The string is the group name shown in test output.
You must enable `TypeApplications` to call it (`@MyModel`).

```haskell
tests :: TestTree
tests = propRunActions @MyModel "My contract"
```

`propRunActions` builds a group containing:

1. `"Positive tests"` — a QuickCheck property.
2. `"Negative tests"` — a QuickCheck property.
3. (If `threatModels` non-empty) one test case per threat model.
4. (If `expectedVulnerabilities` non-empty) one test case per expected vuln.

For v0.1 with `threatModels = []`, only (1) and (2) appear.

## G. Worked skeleton — a fictional "Counter" contract

> **Shape illustration only — your contract differs.** This is a fictional
> Counter contract used to show how the pieces fit together. **Do not copy
> this code.** Use it to understand the structure: a model type with the
> required derivings, the `data Action ...` family living inside the
> instance, an `initialize`/`arbitraryAction`/`precondition`/`perform`
> quartet, and the `ThreatModelsFor` empty default. Your model, actions,
> generator bias, and precondition will all look different.

Below is a self-contained sketch showing every wired-up piece. It is **not** a
real contract — `counterScript` and `incrementTx`/`resetTx` are placeholders
that would live wherever your project keeps its off-chain helpers.

This skeleton requires the following `LANGUAGE` pragmas at the top of the
file: `DerivingStrategies`, `DeriveGeneric`, `DeriveAnyClass`,
`NumericUnderscores`, `OverloadedStrings`, plus `TypeApplications`,
`TypeFamilies`, `FlexibleInstances`, `MultiParamTypeClasses`,
`ScopedTypeVariables`. Your test file should start with these. If you
later see errors like `Illegal deriving strategy` or `lexical error in
numeric literal at character '_'`, the missing extension is the cause —
add `DerivingStrategies` or `NumericUnderscores` respectively.

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CounterSpec (tests) where

import           Control.Monad                  (void)
import qualified Cardano.Api                    as C
import           Convex.BuildTx                 (execBuildTx)
import           Convex.CoinSelection           (ChangeOutputPosition (..))
import qualified Convex.MockChain.CoinSelection as CoinSelection
import           Convex.TestingInterface        ( Action, TestingInterface (..)
                                                , TestingMonadT
                                                , ThreatModelsFor (..)
                                                , propRunActions
                                                , Gen, oneof
                                                )
import qualified Convex.Wallet.MockWallet       as Wallet
import           Data.Aeson                     (ToJSON)
import           GHC.Generics                   (Generic)
import           Test.Tasty                     (TestTree)

-- | Assume these come from your project's lib/.  Placeholders only.
-- import Scripts (counterScriptHash, incrementTx, resetTx)
counterScriptHash :: C.ScriptHash
counterScriptHash = undefined   -- supplied by lib/

-- BuildTx-writer actions, supplied by lib/.
incrementTx :: ( ) => () -- TxBuilder ConwayEra
incrementTx = undefined

resetTx :: ( ) => () -- TxBuilder ConwayEra
resetTx = undefined

-- | The model.  Tracks the counter value and who can reset it.
data CounterModel = CounterModel
  { counter :: !Integer
  , owner   :: !String         -- a label; use a real type in practice
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance TestingInterface CounterModel where
  data Action CounterModel
    = Increment
    | Reset
    deriving Show

  initialize = do
    -- Deploy / initialise the counter at value 0.
    -- e.g.  void $ tryBalanceAndSubmit mempty Wallet.w1 (execBuildTx deployTx) TrailingChange []
    pure CounterModel { counter = 0, owner = "w1" }

  arbitraryAction _s = oneof
    [ pure Increment
    , pure Reset
    ]

  precondition s Reset     = counter s > 0          -- only meaningful when > 0
  precondition _ Increment = True

  perform s Increment = do
    -- void $ tryBalanceAndSubmit mempty Wallet.w1 (execBuildTx incrementTx) TrailingChange []
    pure s { counter = counter s + 1 }

  perform s Reset = do
    -- void $ tryBalanceAndSubmit mempty Wallet.w1 (execBuildTx resetTx) TrailingChange []
    pure s { counter = 0 }

instance ThreatModelsFor CounterModel where
  threatModels = []

tests :: TestTree
tests = propRunActions @CounterModel "Counter"
```

The structural shapes worth noticing (your contract will differ on every
data point, but these scaffolding patterns are stable):

- The model has `deriving stock (Eq, Show, Generic)` plus
  `deriving anyclass (ToJSON)`.
- `data Action CounterModel` lives inside the `instance` body with
  `deriving Show`.
- `initialize` and `perform` return new model values in `TestingMonadT`.
- `precondition` is total — every constructor of `Action` is handled (use
  catch-all `_ -> True` if you have many).
- `ThreatModelsFor` with empty lists for v0.2.
- `propRunActions @CounterModel "Counter"` is the tasty `TestTree`.

The placeholders (`counterScriptHash`, `incrementTx`, `resetTx`) stand in
for whatever the user's project actually provides. Find the equivalents by
discovery; do not assume `lib/Scripts.hs` exists.

## H. Where the instances live (orphans)

**Recommendation**: keep the model type AND its instances
(`TestingInterface`, `Show (Action _)`, `ThreatModelsFor`) in the SAME
module. This avoids `-Worphans` entirely.

If the user has a strong reason to split — e.g. the instance lives in a
test target while the model is reused by library code — document the
split by adding `{-# OPTIONS_GHC -Wno-orphans #-}` to the instance
module with a one-line `-- justification: instance confined to test
target` comment. This is a default-with-escape-hatch; do not push users
toward either side beyond that.
