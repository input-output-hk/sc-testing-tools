# Reference: Project setup

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

Loaded by subagents in the Fresh phase (full setup) and in the
Setup-done phase (verification only). The main agent never reads this.

The skill assumes the trial repo is **Haskell + cabal** with either a
nix flake or the user's agreement to add one. Everything else
(directory layout, package count, test-dir location, module names) is
discovered, not assumed.

## A. Discovery checklist (Fresh phase)

A subagent runs this list and writes findings to `contract-sketch.md`.

- `cabal.project` at the root or one level down.
- `flake.nix` at the root (or note its absence).
- All `*.cabal` files. Record each `name:` field.
- Validator scripts: `.hs` files importing `PlutusTx`,
  `PlutusLedgerApi.V1/V2/V3`, or `.ak` files (Aiken).
- Off-chain helpers: `.hs` files importing `Convex.BuildTx` /
  `Cardano.Api` and producing `TxBodyContent BuildTx ...` or running in
  the `BuildTx` writer.
- Existing test directories (`test/`, `tests/`, or per-package).

The subagent writes a Discovery summary to `contract-sketch.md`. The
main agent asks the user to confirm validator-under-test, off-chain
helpers, and where new test files should live BEFORE any cabal edit.

## B. Verification checklist (Setup-done phase)

A subagent confirms the four items below are present and current. If
any are missing or stale, transition is blocked back to Fresh for that
item only.

1. GHC version pin matches §C.1.
2. CHaP repository block present (§C.2).
3. `sc-testing-tools` source-repo stanza pinned to `chore/without-p`
   with BOTH `src/testing-interface` AND `src/tasty-streaming` subdirs
   (§C.3 — the second subdir is the easy thing to miss).
4. `sc-tools` source-repo stanza (§C.4).
5. Test-suite stanza depends on `convex-testing-interface` AND
   `convex-tasty-streaming` (§D).

## C. Dependency facts (non-negotiable)

### C.1 GHC version

`with-compiler: ghc-9.6.6` in `cabal.project`.

### C.2 CHaP repository

```cabal
repository cardano-haskell-packages
  url: https://chap.intersectmbo.org/
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
    443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
    a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
    bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
    c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
    d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee
```

### C.3 `sc-testing-tools` source-repository-package

```cabal
source-repository-package
  type: git
  location: https://github.com/input-output-hk/sc-testing-tools.git
  tag: 017066310f1f5387ec2c7848a50dac8da0de291f
  subdir:
    src/tasty-streaming
    src/testing-interface
```

**Critical:** `convex-tasty-streaming` is a transitive dependency that
lives in the same monorepo and is not published to CHaP or Hackage. Its
`subdir` MUST be listed alongside `src/testing-interface` or the cabal
solver fails to locate it when resolving the test-suite's build-depends.
This is the single most common setup mistake.

The pinned commit must be on `chore/without-p`. The hash above is the
current pin.

### C.4 `sc-tools` source-repository-package

```cabal
source-repository-package
  type: git
  location: https://github.com/input-output-hk/sc-tools.git
  tag: c50e9edf2606d149820d41c2d4f82fae54eb21dd
  subdir:
    src/base
    src/coin-selection
    src/mockchain
    src/node-client
    src/optics
    src/wallet
```

The first `cabal build` will fail asking for `--sha256:` for each
`source-repository-package`; paste the value cabal prints into the
block and rebuild.

### C.5 `allow-newer`

```cabal
allow-newer: maestro-sdk:containers
```

Transitive-dep workaround. Required.

## D. Test-suite stanza

Append to the user-confirmed `.cabal` file. Replace `<project>` and
`<Name>` with confirmed names. Test directory matches what the user
agreed to in discovery.

```cabal
test-suite <project>-test
  default-language: Haskell2010
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          Spec.hs
  other-modules:    <Name>Spec
  ghc-options:
    -Wall -fobject-code -fno-ignore-interface-pragmas
    -fno-omit-interface-pragmas -fno-specialise -Wno-unused-packages
  build-depends:    <see below>
```

Required `build-depends` (at minimum):

```cabal
  , aeson, base >=4.14.0, containers, lens, mtl
  , cardano-api, cardano-ledger-conway
  , convex-base, convex-coin-selection, convex-mockchain
  , convex-tasty-streaming, convex-testing-interface, convex-wallet
  , plutus-ledger-api, plutus-tx
  , <project>
  , QuickCheck, tasty, tasty-hunit, tasty-quickcheck
```

`convex-tasty-streaming` in build-depends is required because
`Spec.hs` uses `defaultMainStreaming` (see §E).

## E. `test/Spec.hs` template

Use `defaultMainStreaming` by default. The `--streaming-json` and
`--list-tests-json` ingredients it adds are needed for IDE / extension
integration. Fall back to plain `defaultMain` only on explicit user
request.

```haskell
module Main where

import qualified <Name>Spec
import Convex.Tasty.Streaming (defaultMainStreaming)
import Test.Tasty (testGroup)

main :: IO ()
main = defaultMainStreaming
  $ testGroup "<project> tests" [<Name>Spec.tests]
```

`test/<Name>Spec.hs` must `module <Name>Spec (tests) where` and
export `tests :: TestTree`.

## F. Build & test commands

```
NIX_CONFIG="system = x86_64-linux" nix develop -c cabal update
NIX_CONFIG="system = x86_64-linux" nix develop -c cabal build <testsuite>
NIX_CONFIG="system = x86_64-linux" nix develop -c cabal test <testsuite>
```

`<testsuite>` is `all`, a package name, or `<package>:test:<suite-name>`.
Always prefix with `nix develop -c`; running outside Nix means GHC 9.6.6
and the cardano-api pin are not on PATH.

Test output prints directly to stdout (`test-show-details: direct`).
On failure, the subagent captures the last ~30 lines and includes them
in its `## decisions log` entry — not in the four-part return body
itself (that contract has no raw build output, per `SKILL.md §6`).

## G. Conflict handling

If discovery reveals a different `sc-tools` tag, a different GHC pin,
or existing `source-repository-package` entries pointing at forks, do
NOT silently change them. The subagent reports the conflict; the main
agent presents the user with at least two options (keep / adopt /
hybrid) and waits.

## H. Common red herring

"X is unusable due to missing dependencies: Y, Z, …" with a
suspiciously long list (10+ packages) is almost always a stale
`dist-newstyle/` from a previous build under a different GHC. Fix:
`cabal clean` or `rm -rf dist-newstyle`, then rebuild. The error
message lies about which dependency is missing.
