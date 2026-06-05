# Reference: The `TestingInterface` class

> **CARDINAL RULE — Deployment is an action, not setup.**
>
> Deploying the contract on-chain is THE initial action (or one of several, if multiple deployment shapes are tested). It MUST be part of the Action type — typically named `Start`, `Deploy`, `StartWithInlineDatum`, etc.
>
> `initialize` MUST be model-only: it sets up the in-memory bookkeeping and returns a zero state with `modelInitialized = False` (or equivalent flag). It MUST NOT submit any transaction, deploy any script, or touch the chain.
>
> The generator reads the model state and decides whether to emit a deploy action (when uninitialised) or a normal action (when initialised).
>
> If a proposed design has `initialize` deploying the contract, or has an Action type without a deploy action, the design is WRONG. STOP. Do not write code. Do not proceed. Re-read this rule.
>
> Every subagent dispatched from this skill MUST receive this rule verbatim in its prompt.

The API contract. Method signatures, semantics, superclass derivings,
and where instances should live. Loaded by subagents writing or
extending instance bodies.

## A. The class signature

```haskell
class (Show state, Eq state, Show (Action state), ToJSON state)
  => TestingInterface state where
  data Action state
  initialize     :: (MonadIO m) => TestingMonadT m state
  arbitraryAction :: state -> Gen (Action state)
  precondition   :: state -> Action state -> Bool
  precondition _ _ = True
  perform        :: (MonadIO m) => state -> Action state -> TestingMonadT m state
  validate       :: (MonadIO m) => state -> TestingMonadT m Bool
  validate _ = pure True
  monitoring     :: state -> Action state -> Property -> Property
  monitoring _ _ = id
  discardNegativeTestForUserExceptions :: Bool
  discardNegativeTestForUserExceptions = False
```

Minimal complete definition: `data Action`, `initialize`,
`arbitraryAction`, `perform`. Accept defaults for the other four
unless the user explicitly requests otherwise (and `monitoring` /
`validate` / `discardNegativeTestForUserExceptions` are out of scope —
see `SKILL.md §9`).

## B. `data Action state`

Associated data family. One declaration per concrete state, inside the
instance body. Each constructor is one user-facing operation. Include
in each constructor enough data to describe the action fully (wallet,
value, datum/redeemer parameters). The generator fills the fields in
randomly.

```haskell
instance TestingInterface MyModel where
  data Action MyModel
    = DoX !Wallet
    | DoY !Wallet !C.Value
    deriving Show
  ...
```

`deriving Show` is required (superclass constraint). `Eq` on actions
is not required but harmless. Strict fields (`!`) optional.

## C. `initialize :: MonadIO m => TestingMonadT m state`

Runs once at the start of every QuickCheck iteration. **Per the
Cardinal Rule (top of this file): `initialize` is model-only.** It
sets up the in-memory model and returns the zero state with
`modelInitialized = False` (or equivalent). It does NOT submit any tx,
deploy any script, mint any tokens, or pay any setup UTxO. All of that
belongs in a deploy Action (`Start`, `Deploy`, `StartWithInlineDatum`,
…) emitted by the generator when the model is uninitialised.

Typical body:

```haskell
initialize = pure MyModel
  { modelInitialized = False
  , …
  }
```

If the model needs no setup at all, `pure (MyModel ...)` is the entire
body.

## D. `arbitraryAction :: state -> Gen (Action state)`

QuickCheck generator. `oneof` for uniform choice; `frequency` to bias
toward / across the precondition boundary. **Before writing this
method, read `05-generators-and-implementation.md`.** This is the
single most common source of silent test failures.

```haskell
arbitraryAction s = oneof
  [ DoX <$> elements [Wallet.w1, Wallet.w2]
  , DoY <$> elements [Wallet.w1, Wallet.w2] <*> genValue s
  ]
```

Re-exports from `Convex.TestingInterface`: `Gen`, `Arbitrary`,
`oneof`, `frequency`, `elements`.

## E. `precondition :: state -> Action state -> Bool`

Default `True`. Two roles:

1. **Positive tests** execute only actions where `precondition s a =
   True` and expect them to succeed on-chain.
2. **Negative tests** find actions where `precondition s a = False`
   and assert the chain rejects them.

```haskell
precondition s (Withdraw w v) = balance s >= valueOf v && owner s == w
precondition _ _              = True
```

Discipline: total, pure, cheap (called many times during shrinking),
mirrors the validator's rules. Full failure-mode treatment in
`05-generators-and-implementation.md`.

## F. `perform :: state -> Action state -> TestingMonadT m state`

Where most of the work lives. For each `Action` constructor:

1. Build the transaction via the user's off-chain helpers (wrap with
   `Convex.BuildTx.execBuildTx` if the helper is a `BuildTx` writer).
2. Submit via `tryBalanceAndSubmit` (canonical idiom).
3. Return the **new** model state. Not `pure s`.

```haskell
perform s (Deposit w val) = do
  let tx = execBuildTx
        (BuildTx.payToAddress (Wallet.addressInEra Defaults.networkId w) val)
  void $ tryBalanceAndSubmit mempty w tx TrailingChange []
  pure s { balance = balance s + val }
```

**`perform` runs unconditionally.** Do not check `precondition` inside
`perform`. The framework runs it for both positive and negative cases;
negative cases expect the submit to fail. See
`05-generators-and-implementation.md §8–9`.

## G. Superclasses & derivings

```haskell
class (Show state, Eq state, Show (Action state), ToJSON state)
  => TestingInterface state where ...
```

- `Show state` — `deriving Show`.
- `Eq state` — `deriving Eq`.
- `Show (Action state)` — `deriving Show` on the data declaration.
- `ToJSON state` — from `Data.Aeson`. Used to dump model states into
  iteration traces.

Canonical derivings:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

data MyModel = MyModel { ... }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
```

If the model contains types lacking `ToJSON` (e.g. cardano-api types),
write an orphan and silence the warning at the top of the module:

```haskell
{-# OPTIONS_GHC -Wno-orphans #-}
```

## H. Where instances live (orphans)

**Default**: keep the model type AND its instances (`TestingInterface`,
`Show (Action _)`, `ThreatModelsFor`) in the SAME module. This avoids
`-Worphans` entirely.

If the user has a strong reason to split — e.g. the instance lives in a
test target while the model is reused by library code — document the
split by adding `{-# OPTIONS_GHC -Wno-orphans #-}` to the instance
module with a one-line `-- justification: instance confined to test
target` comment. This is a default-with-escape-hatch; do not push
users toward either side beyond that.

## I. `ThreatModelsFor` class

```haskell
class TestingInterface state => ThreatModelsFor state where
  threatModels            :: [ThreatModel ()]
  expectedVulnerabilities :: [ThreatModel ()]
  expectedVulnerabilities = []
```

`expectedVulnerabilities` has **inverted semantics**: a passing test
means the model **did** detect the vulnerability. Default empty.

Until the positive/negative suite passes, you may write the instance
with empty lists:

```haskell
instance ThreatModelsFor MyModel where
  threatModels = []
  expectedVulnerabilities = []
```

Empty `threatModels` disables threat-model evaluation entirely; only
positive and negative tests run. Pick models from
`06-threat-models.md` once the suite is green.

## J. `TestingMonadT`

```haskell
newtype TestingMonadT m a = TestingMonadT
  { unTestingMonadT
      :: ExceptT (BalanceTxError ConwayEra) (MockchainT ConwayEra m) a
  }
```

You rarely need this concretely. It provides `MonadIO`, `MonadFail`,
`MonadBlockchain ConwayEra`, `MonadMockchain ConwayEra`, and
`MonadError (BalanceTxError ConwayEra)`. Era is fixed to **Conway**;
off-chain helpers must produce Conway-compatible builders.

## K. `propRunActions` — the runner

```haskell
propRunActions :: forall state. ThreatModelsFor state => String -> TestTree
```

Returns a tasty `TestTree`. Enable `TypeApplications` to call it.

```haskell
tests :: TestTree
tests = propRunActions @MyModel "My contract"
```

Tree contains: `"Positive tests"`, `"Negative tests"`, one case per
threat model (if any), one case per `expectedVulnerabilities` entry (if
any).

## L. Required LANGUAGE pragmas

The test module needs at minimum:

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
```

Errors like `Illegal deriving strategy` or `lexical error in numeric
literal at character '_'` mean `DerivingStrategies` or
`NumericUnderscores` is missing.
