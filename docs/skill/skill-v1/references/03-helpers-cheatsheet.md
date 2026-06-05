# Reference: Helpers cheatsheet

Canonical idioms for use inside `initialize` and `perform`. Everything below
runs in `TestingMonadT m` (so you have `MonadIO`, `MonadFail`,
`MonadBlockchain ConwayEra`, `MonadMockchain ConwayEra`).

## A. Submitting a transaction (universal idiom)

```haskell
void $ tryBalanceAndSubmit mempty Wallet.w1 (execBuildTx myBuildAction) TrailingChange []
```

Piece by piece:

- **`tryBalanceAndSubmit`** — from `Convex.MockChain.CoinSelection`.
  Coin-selects inputs from the wallet, balances the transaction, signs it
  with the wallet's key, and submits. Calls `fail` on any error. Use this in
  setup/positive paths.

  Full type:
  ```haskell
  tryBalanceAndSubmit
    :: ( MonadMockchain era m, MonadError (BalanceTxError era) m
       , MonadFail m, C.IsBabbageBasedEra era )
    => Tracer m TxBalancingMessage
    -> Wallet
    -> TxBuilder era
    -> ChangeOutputPosition
    -> [C.ShelleyWitnessSigningKey]
    -> m (C.Tx era)
  ```

- **`mempty`** — the first argument is a `Tracer` for debug messages. `mempty`
  is the no-op tracer; use it unless you actively want trace output.

- **`Wallet.w1`** — the paying wallet. `Convex.Wallet.MockWallet` exports
  pre-funded mock wallets `w1` through `w10`. See §E below.

- **`execBuildTx myBuildAction`** — from `Convex.BuildTx`. Runs the `BuildTx`
  writer monad and produces a `TxBuilder ConwayEra`. `myBuildAction` is the
  user's off-chain helper (wherever they keep them — ask if unclear). If
  the helper already returns a `TxBuilder ConwayEra`, drop `execBuildTx`
  and pass it directly.

- **`TrailingChange`** — a `ChangeOutputPosition`. **Always use
  `TrailingChange`** for the testing interface. The other variants
  (`LeadingChange`, `BalancedChange`) interact poorly with some balancing
  scenarios and are not needed here.

- **`[]`** — extra signing keys (`[ShelleyWitnessSigningKey]`). Usually empty:
  the wallet's own key is added automatically. Pass a non-empty list only if
  the transaction requires additional signers beyond the wallet.

When you want to handle the failure yourself (e.g. inspect the error in a
negative-style assertion within `perform`):

```haskell
result <- CoinSelection.balanceAndSubmit mempty Wallet.w1 (execBuildTx myBuildAction) TrailingChange []
case result of
  Left err -> ...   -- inspect or log
  Right tx -> ...
```

`balanceAndSubmit` returns `Either (SendTxError era) (C.Tx era)` instead of
calling `fail`. The framework's negative-testing logic uses
`tryBalanceAndSubmit` style and catches the exception itself, so most user
code should stick with `tryBalanceAndSubmit`.

## B. Reading chain state — UTxO queries

To read the full mockchain UTxO and filter it:

```haskell
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import Convex.Class (getUtxo)

allUtxos <- C.fromLedgerUTxO C.shelleyBasedEra <$> getUtxo
```

- `getUtxo` (from `Convex.Class`) returns the ledger-level UTxO of the current
  mockchain era.
- `C.fromLedgerUTxO C.shelleyBasedEra` lifts it to a `C.UTxO ConwayEra` you
  can pattern-match with `C.unUTxO`.

Filtering UTxOs at a specific address:

```haskell
import qualified Data.Map as Map

let scriptUtxos =
      Map.filter (\(C.TxOut a _ _ _) -> a == myScriptAddress)
                 (C.unUTxO allUtxos)
```

Network ID (used to compute addresses from script hashes / verification keys):

```haskell
import qualified Convex.MockChain.Defaults as Defaults

let nid = Defaults.networkId
```

Wallet addresses are derived via:

```haskell
let w1Addr = Wallet.addressInEra Defaults.networkId Wallet.w1
```

## C. Time control

These come from `Convex.Class` (instances satisfied by `TestingMonadT`):

```haskell
import Convex.Class (nextSlot, setSlot, setPOSIXTime)
import qualified Cardano.Api as C

nextSlot                            -- advance one slot
setSlot   (C.SlotNo 100)            -- jump to a specific slot
setPOSIXTime (1700000000 * 1000)    -- jump to a specific POSIX time (ms)
```

If your validator depends on transaction validity intervals, advance time
between actions in `perform` to model real elapsed time.

## D. Common import block — paste-and-prune

A starter import block. **These are typical imports — your project may need
more or fewer.** Do not assume; check the user's existing test or library
code first, then prune or extend. The exact module names are stable; what
the user pulls in is their choice.

The matching `LANGUAGE` pragma block. Adjust based on your actual usage;
these cover the common cases (deriving strategies for the model, numeric
underscores in lovelace literals like `5_000_000`, type applications for
`propRunActions @MyModel`, type families for the associated `Action`
family):

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

```haskell
import           Control.Monad                  (void)
import qualified Cardano.Api                    as C
import qualified Cardano.Api.Shelley            as C
import           Convex.BuildTx                 (execBuildTx)
import qualified Convex.BuildTx                 as BuildTx
import           Convex.Class                   ( MonadBlockchain
                                                , MonadMockchain
                                                , getUtxo
                                                , nextSlot
                                                , setSlot
                                                , setPOSIXTime
                                                )
import           Convex.CoinSelection           (ChangeOutputPosition (..))
import qualified Convex.MockChain.CoinSelection as CoinSelection
import qualified Convex.MockChain.Defaults      as Defaults
import           Convex.TestingInterface
    ( Action
    , TestingInterface (..)
    , TestingMonadT
    , ThreatModelsFor (..)
    , propRunActions
    , Gen
    , oneof
    , frequency
    , elements
    )
-- For built-in threat models (see references/05-threat-models.md), import
-- the individual modules under Convex.ThreatModel.<Name>. Example:
--   import Convex.ThreatModel.UnprotectedScriptOutput (unprotectedScriptOutput)
--   import Convex.ThreatModel.DoubleSatisfaction      (doubleSatisfaction)
--   import Convex.ThreatModel.LargeData               (largeDataAttackWith)
-- Note: Convex.ThreatModel.All only re-exports `allThreatModels`; the
-- individual names are NOT re-exported through it.
import qualified Convex.Wallet.MockWallet       as Wallet
import           Data.Aeson                     (ToJSON)
import qualified Data.Map                       as Map
import           GHC.Generics                   (Generic)
import           Convex.Tasty.Streaming         (defaultMainStreaming)
import           Test.QuickCheck                (Property)
import           Test.Tasty                     (TestTree)
```

`Convex.Tasty.Streaming.defaultMainStreaming` is a drop-in for
`Test.Tasty.defaultMain` that adds the `--streaming-json` and
`--list-tests-json` ingredients needed for IDE / extension integration.
Use it in `test/Spec.hs` as the default entry point.

Key qualified names:

- `Convex.BuildTx` → re-imported as `BuildTx`. Functions like
  `BuildTx.payToAddress`, `BuildTx.spendPublicKeyOutput`, `BuildTx.mintPlutus`
  live here, but in practice you usually call the user's own off-chain
  helpers which wrap these primitives.
- `Convex.MockChain.CoinSelection` → `CoinSelection.tryBalanceAndSubmit`,
  `CoinSelection.balanceAndSubmit`.
- `Convex.Wallet.MockWallet` → `Wallet.w1` .. `Wallet.w10`,
  `Wallet.addressInEra`.

## E. Pre-funded mock wallets

`Convex.Wallet.MockWallet` exposes ten pre-funded wallets:
**`w1`, `w2`, `w3`, `w4`, `w5`, `w6`, `w7`, `w8`, `w9`, `w10`**.

Use them as funding sources in `tryBalanceAndSubmit` and as parties in your
generated actions:

```haskell
arbitraryAction _s = oneof
  [ Deposit  <$> elements [Wallet.w1, Wallet.w2, Wallet.w3]
  , Withdraw <$> elements [Wallet.w1, Wallet.w2, Wallet.w3]
  ]
```

To get a wallet's address:

```haskell
Wallet.addressInEra Defaults.networkId Wallet.w1
  :: C.AddressInEra ConwayEra
```

## E.5. Building tx in the `BuildTx` writer

The user's project likely already has functions like `payToScript`,
`spendScript`, etc., wired up — possibly as `BuildTx era ()` writer
actions, possibly as functions returning a fully-built `TxBuilder era`.
Look for them in the user's library (ask the user if unclear where to
look). Your `perform` will call these and, if they are `BuildTx` writer
actions, wrap them in `execBuildTx`:

```haskell
let txb = execBuildTx (userHelper arg1 arg2)
void $ tryBalanceAndSubmit mempty wallet txb TrailingChange []
```

If the helper already returns a `TxBuilder ConwayEra`, omit `execBuildTx`.

## E.6. Constructing Lovelace values

In modern `cardano-api` (10.17+), `Lovelace` is a type synonym for the
ledger's `Coin` newtype:

```haskell
type Lovelace = Coin                 -- from Cardano.Ledger.Coin, re-exported
newtype Coin  = Coin { unCoin :: Integer }
```

So `C.Coin :: Integer -> Coin` is the constructor; both `Coin` and
`Lovelace` are valid in type signatures. To get a `Value`:

```haskell
let ada = C.lovelaceToValue (C.Coin 5_000_000)
```

**Common mistake**: `C.Lovelace 5_000_000` — that constructor no longer
exists; `Lovelace` is just an alias. Use `C.Coin` instead.

## E.7. Finding the continuation output after a `perform` step

State-machine contracts hand off a UTxO at the script address from one
step to the next. After your `perform` submits a transaction, the next
step needs the `TxIn` of the new continuation output (the one carrying
the next datum/value at the script address). Two patterns work:

**(a) Scan the submitted tx body** — no extra query, but ties you to the
current `cardano-api` body-inspection surface:

```haskell
import qualified Cardano.Api as C

-- Adapt: `tx :: C.Tx ConwayEra` is what tryBalanceAndSubmit returned.
let body   = C.getTxBody tx
    txid   = C.getTxId body
    outs   = C.txOuts (C.getTxBodyContent body)
    pick i (C.TxOut a _ _ _) | a == scriptAddr = Just (C.TxIn txid (C.TxIx i))
    pick _ _                                   = Nothing
    contIn = listToMaybe [t | (i, o) <- zip [0..] outs, Just t <- [pick i o]]
```

**(b) Query the chain after submit** — simpler, era-agnostic, costs one
query:

```haskell
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Data.Map as Map
import           Convex.Class (getUtxo)

allUtxos <- C.fromLedgerUTxO C.shelleyBasedEra <$> getUtxo
let scriptOuts =
      Map.filter (\(C.TxOut a _ _ _) -> a == scriptAddr) (C.unUTxO allUtxos)
-- Pick the relevant one (often: most recent, or by datum match).
```

Trade-off: (a) is one fewer query but breaks if `TxBodyContent`'s output
shape shifts in a future cardano-api; (b) is robust but pays for a query
and you must disambiguate when several script UTxOs coexist. Treat both
as patterns to adapt, not snippets to copy verbatim.

## E.8. Inspecting types via `cabal repl`

When the skill or references don't show you the exact signature of
something — especially types from `sc-tools`, `sc-testing-tools`, or
`cardano-api` that the user pulls in via `source-repository-package` —
**don't guess**. Open a REPL loaded with the test-suite environment and
ask GHC:

```
NIX_CONFIG="system = x86_64-linux" nix develop -c \
  cabal repl --repl-options=-fno-code <project>:test:<testsuite-name>
```

Then at the `ghci>` prompt:

```
:info Coin
:info Lovelace
:info TxBodyContent
:info TestingInterface
:type tryBalanceAndSubmit
:quit
```

- `-fno-code` skips code generation, so the REPL loads in seconds.
- `:info TypeName` prints the type's definition, constructors, instances.
- `:type expr` prints the inferred type of an expression.

This is the primary way to inspect types from third-party / vendored
packages — faster than chasing source files, and always matches the exact
version your build resolves to.

## F. Common pitfalls

- **"No instance for `ToJSON MyModel`"** — add `deriving (Generic)` (with
  `DeriveGeneric`) and `deriving anyclass (ToJSON)` (with `DeriveAnyClass`),
  or write a manual instance. If the JSON-blocking type is from a third-party
  library, add `{-# OPTIONS_GHC -Wno-orphans #-}` and write an orphan
  `instance ToJSON …`.

- **"No instance for `Show (Action MyModel)`"** — add `deriving Show` to the
  `data Action MyModel = …` declaration *inside the instance body*. The
  `deriving` clause goes right after the constructors, just like a normal
  data declaration.

- **`perform` returning the old state** — every call must return the **new**
  state. `pure s` is almost always wrong. Build the new value:
  `pure s { field = newValue }`.

- **Using a change position other than `TrailingChange`** — don't. Only
  `TrailingChange` is the universal idiom for this framework. Other positions
  exist but are not supported by this skill.

- **Trying to compile outside Nix** — the `cabal build` will fail because GHC
  9.6.6 and the right cardano-api version are not in your PATH. Always
  prefix commands with `NIX_CONFIG="system = x86_64-linux" nix develop -c`.

- **Mismatched era** — `TestingMonadT` is fixed to `ConwayEra`. If a
  `tryBalanceAndSubmit` call has a type error mentioning `BabbageEra` vs
  `ConwayEra`, your project's off-chain helper is producing the wrong era.
  Ask the user to provide a Conway-compatible builder.

- **`precondition` not total** — `precondition` is `state -> Action state ->
  Bool`. If you only handle some constructors, GHC will warn (with
  `-Wincomplete-patterns`) and the unmatched cases will default to whatever
  the catch-all says. Always end with `precondition _ _ = True` (or `= False`)
  so you don't leak unintended behaviour.

- **Generating actions that can never satisfy `precondition`** — if no random
  `Action` ever satisfies `precondition s a` for the current `s`, the
  framework gives up that step. If this happens at the start (when `s` is
  the initial state), positive tests will run zero actions and look trivially
  green. Make sure at least one action is always producible from the initial
  state.
