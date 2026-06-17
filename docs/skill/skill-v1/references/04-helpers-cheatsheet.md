# Reference: Helpers cheatsheet

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

Canonical idioms for use inside `perform` (including the deploy
Action's `perform` branch — that's where on-chain deployment lives;
`initialize` is model-only, see the Cardinal Rule above). Everything
below runs in `TestingMonadT m` (so `MonadIO`, `MonadFail`,
`MonadBlockchain ConwayEra`, `MonadMockchain ConwayEra` are all
available). Loaded ad-hoc by any subagent writing Haskell.

## A. Submitting a transaction (universal idiom)

```haskell
void $ tryBalanceAndSubmit mempty Wallet.w1
         (execBuildTx myBuildAction) TrailingChange []
```

Piece by piece:

- **`tryBalanceAndSubmit`** (from `Convex.MockChain.CoinSelection`):
  coin-selects, balances, signs with the wallet's key, submits. Calls
  `fail` on any error. Use for setup and positive paths.
- **`mempty`** — first arg is a `Tracer`. `mempty` is no-op.
- **`Wallet.w1`** — the paying wallet. Ten pre-funded mock wallets
  `w1..w10` from `Convex.Wallet.MockWallet`.
- **`execBuildTx myBuildAction`** — runs the `BuildTx` writer and
  produces a `TxBuilder ConwayEra`. If the user's helper already
  returns a `TxBuilder`, drop `execBuildTx`.
- **`TrailingChange`** — a `ChangeOutputPosition`. **Always use
  `TrailingChange`** in this framework. Other variants exist; not
  supported here.
- **`[]`** — extra signing keys. Usually empty; the wallet's own key
  is added automatically.

When you want to inspect the failure rather than `fail`:

```haskell
result <- CoinSelection.balanceAndSubmit mempty Wallet.w1
            (execBuildTx myBuildAction) TrailingChange []
case result of
  Left err -> ...
  Right tx -> ...
```

The framework's negative-testing logic catches the exception itself,
so most user code should stick with `tryBalanceAndSubmit`.

## B. Reading chain state — UTxO queries

```haskell
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import Convex.Class (getUtxo)

allUtxos <- C.fromLedgerUTxO C.shelleyBasedEra <$> getUtxo
```

Filtering at a specific address:

```haskell
import qualified Data.Map as Map

let scriptUtxos =
      Map.filter (\(C.TxOut a _ _ _) -> a == myScriptAddress)
                 (C.unUTxO allUtxos)
```

Network ID + wallet addresses:

```haskell
import qualified Convex.MockChain.Defaults as Defaults

let nid = Defaults.networkId
let w1Addr = Wallet.addressInEra Defaults.networkId Wallet.w1
```

## C. Time control

```haskell
import Convex.Class (nextSlot, setSlot, setPOSIXTime)
import qualified Cardano.Api as C

nextSlot                            -- advance one slot
setSlot   (C.SlotNo 100)            -- jump to a specific slot
setPOSIXTime (1700000000 * 1000)    -- jump to POSIX time (ms)
```

If the validator depends on validity intervals, advance time between
actions in `perform`.

## D. Common import block

LANGUAGE pragmas (see `03-testing-interface-class.md §L`).

Framework / Cardano core:

```haskell
import           Control.Monad                  (void)
import qualified Cardano.Api                    as C
import qualified Cardano.Api.Shelley            as C
import           Convex.BuildTx                 (execBuildTx)
import qualified Convex.BuildTx                 as BuildTx
import           Convex.Class                   ( MonadBlockchain, MonadMockchain
                                                , getUtxo, nextSlot
                                                , setSlot, setPOSIXTime
                                                )
import           Convex.CoinSelection           (ChangeOutputPosition (..))
import qualified Convex.MockChain.CoinSelection as CoinSelection
import qualified Convex.MockChain.Defaults      as Defaults
import qualified Convex.Wallet.MockWallet       as Wallet
```

TestingInterface + tasty:

```haskell
import           Convex.TestingInterface
    ( Action, TestingInterface (..)
    , TestingMonadT, ThreatModelsFor (..)
    , propRunActions, Gen, oneof, frequency, elements
    )
import           Convex.Tasty.Streaming         (defaultMainStreaming)
import           Data.Aeson                     (ToJSON)
import qualified Data.Map                       as Map
import           GHC.Generics                   (Generic)
import           Test.QuickCheck                (Property)
import           Test.Tasty                     (TestTree)
```

Key qualified names:

- `Convex.BuildTx` → re-imported as `BuildTx`. Functions like
  `BuildTx.payToAddress`, `BuildTx.spendPublicKeyOutput`,
  `BuildTx.mintPlutus`. In practice you usually call the user's own
  off-chain helpers which wrap these.
- `Convex.MockChain.CoinSelection` → `CoinSelection.tryBalanceAndSubmit`,
  `CoinSelection.balanceAndSubmit`.
- `Convex.Wallet.MockWallet` → `Wallet.w1..w10`, `Wallet.addressInEra`.

## E. Per-attack import gotcha (threat models)

When wiring `ThreatModelsFor`, import each attack from its OWN module:

```haskell
import Convex.ThreatModel.UnprotectedScriptOutput (unprotectedScriptOutput)
import Convex.ThreatModel.DoubleSatisfaction      (doubleSatisfaction)
import Convex.ThreatModel.LargeData               (largeDataAttackWith)
```

**`Convex.ThreatModel.All` only re-exports `allThreatModels`** (the
full default list) — the individual names are NOT re-exported through
it. Trying `import Convex.ThreatModel.All (unprotectedScriptOutput)`
fails with `Module does not export ...`. Always go to the per-attack
module.

## F. Pre-funded mock wallets

`Convex.Wallet.MockWallet` exposes ten pre-funded wallets `w1..w10`.

```haskell
arbitraryAction _s = oneof
  [ Deposit  <$> elements [Wallet.w1, Wallet.w2, Wallet.w3]
  , Withdraw <$> elements [Wallet.w1, Wallet.w2, Wallet.w3]
  ]
```

Wallet address: `Wallet.addressInEra Defaults.networkId Wallet.w1 ::
C.AddressInEra ConwayEra`.

## G. Building tx in the `BuildTx` writer

The user's project likely has functions like `payToScript`,
`spendScript`, wired as either `BuildTx era ()` writer actions or
functions returning a fully-built `TxBuilder era`. If a writer, wrap
in `execBuildTx`; if already a `TxBuilder`, pass directly.

```haskell
let txb = execBuildTx (userHelper arg1 arg2)
void $ tryBalanceAndSubmit mempty wallet txb TrailingChange []
```

## H. Constructing Lovelace values

In `cardano-api` 10.17+, `Lovelace` is a type synonym for the ledger's
`Coin` newtype:

```haskell
type Lovelace = Coin
newtype Coin  = Coin { unCoin :: Integer }
```

```haskell
let ada = C.lovelaceToValue (C.Coin 5_000_000)
```

**Common mistake**: `C.Lovelace 5_000_000` — that constructor no
longer exists. Use `C.Coin`.

## I. Finding the continuation output

State-machine contracts hand off a UTxO at the script address from
one step to the next. Two patterns:

**(a) Scan the submitted tx body** (no extra query):

```haskell
let body   = C.getTxBody tx
    txid   = C.getTxId body
    outs   = C.txOuts (C.getTxBodyContent body)
    pick i (C.TxOut a _ _ _) | a == scriptAddr = Just (C.TxIn txid (C.TxIx i))
    pick _ _                                   = Nothing
    contIn = listToMaybe [t | (i, o) <- zip [0..] outs, Just t <- [pick i o]]
```

**(b) Query the chain after submit** (era-agnostic, costs one query):

```haskell
allUtxos <- C.fromLedgerUTxO C.shelleyBasedEra <$> getUtxo
let scriptOuts =
      Map.filter (\(C.TxOut a _ _ _) -> a == scriptAddr) (C.unUTxO allUtxos)
```

Trade-off: (a) is one fewer query but breaks if `TxBodyContent`'s
output shape shifts in a future cardano-api; (b) is robust but pays
for a query.

## J. Inspecting types via `cabal repl`

Don't guess third-party signatures. Open a REPL loaded with the
test-suite environment:

```
NIX_CONFIG="system = x86_64-linux" nix develop -c \
  cabal repl --repl-options=-fno-code <project>:test:<testsuite-name>
```

Then: `:info Coin`, `:info TestingInterface`, `:type tryBalanceAndSubmit`,
`:quit`. `-fno-code` skips code generation for faster loading.

## K. Common pitfalls

- **"No instance for `ToJSON MyModel`"** — add `deriving (Generic)`
  + `deriving anyclass (ToJSON)`, or write a manual instance. If a
  third-party type blocks it, add `{-# OPTIONS_GHC -Wno-orphans #-}`
  and write an orphan.
- **"No instance for `Show (Action MyModel)`"** — `deriving Show` on
  the `data Action MyModel = ...` declaration **inside the instance
  body**.
- **`perform` returning the old state** — `pure s` is almost always
  wrong. Build the new value: `pure s { field = newValue }`.
- **Using a change position other than `TrailingChange`** — don't.
- **Trying to compile outside Nix** — wrong GHC, wrong cardano-api.
  Always prefix with `NIX_CONFIG="system = x86_64-linux" nix develop
  -c`.
- **Mismatched era** — `TestingMonadT` is `ConwayEra`. `BabbageEra`
  vs `ConwayEra` errors mean the user's helper produces the wrong era.
- **`precondition` not total** — always end with a catch-all
  `precondition _ _ = True` (or `False`) so unmatched cases don't leak
  unintended behaviour.
- **Generator never satisfies `precondition`** — positive tests run
  zero actions and look trivially green. See
  `05-generators-and-implementation.md §4`.
