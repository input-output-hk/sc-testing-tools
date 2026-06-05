# Reference: Project setup (adaptive)

This reference is how you discover the user's project shape, confirm what's
there with the user, and — only after confirmation — propose or write build
wiring.

The skill's hard rule applies: **observe first, ask the user second, write
third.**

## A. The two hard requirements

These are the only project-shape assumptions the skill makes:

1. The project is **Haskell + cabal**.
2. The project has a **nix flake** (or the user agrees to add one — but ask
   before proposing to add it).

Everything else — directory layout, package count, test directory location,
module names — is variable. Discover, do not assume.

## B. Discovery — what to look for

Run this checklist on the user's project before doing anything else.

1. **`cabal.project`** — search at the root and one level down. Note its
   path. If absent, that itself is a finding to report.
2. **`flake.nix`** — search at the root. Note its path. If absent, report
   the finding; the user may have a different nix setup or none.
3. **`*.cabal` files** — find all of them. Multi-package projects may have
   one per subdirectory. Read each `name:` field — those are the
   user's package names you will refer to.
4. **Validator scripts** — search for `.hs` files importing any of:
   - `PlutusTx` / `PlutusTx.Prelude` / `PlutusTx.Compile`
   - `PlutusLedgerApi.V1`, `V2`, or `V3`
   - or `.ak` files (Aiken)
5. **Off-chain helpers** — `.hs` files that import any of:
   - `Convex.BuildTx`
   - `Cardano.Api`
   - and define functions returning `TxBodyContent BuildTx ...`,
     `TxBuilder ...`, or running inside the `BuildTx` writer monad.
6. **Existing test directory or test-suites** — `test/`, `tests/`, or any
   `*.cabal` with `test-suite ...` stanzas. There is **no canonical
   location** — projects vary.

After running this checklist, summarise findings to the user in this form:

> ## Discovery
>
> - cabal.project: found at `./cabal.project` (or: not found)
> - flake.nix: found at `./flake.nix` (or: not found)
> - Cabal packages: `<name>` at `./<path>.cabal`, ...
> - Candidate validator files: `lib/Foo.hs`, `validators/Bar.hs`
> - Candidate off-chain helpers: `lib/Tx.hs`
> - Existing tests: `test/Spec.hs` (or: none)
>
> ## Proposal
>
> I believe `lib/Foo.hs` is the validator under test. Is this correct?
> I propose creating the new test files at `test/...`. Where would you like them?

Use the wording **"I FOUND X. I BELIEVE its role is Y. Is this correct?"**
Discovery is observation, not action.

## C. The dependency facts (non-negotiable)

These hold regardless of project shape. If the user's `cabal.project` is
missing any of these, **propose** them to the user (showing the exact
lines to add) and wait for confirmation before writing.

1. **GHC version**: `with-compiler: ghc-9.6.6` in `cabal.project`.

2. **CHaP repository**:

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

3. **`sc-testing-tools` source-repository-package**:

   ```cabal
   source-repository-package
     type: git
     location: https://github.com/input-output-hk/sc-testing-tools.git
     tag: 017066310f1f5387ec2c7848a50dac8da0de291f
     subdir:
       src/tasty-streaming
       src/testing-interface
   ```

   Note: `convex-tasty-streaming` is a transitive dependency that lives in
   the same monorepo and is not published to CHaP or Hackage. Its `subdir`
   must be listed alongside `src/testing-interface` or the cabal solver
   will fail to locate it when resolving the test-suite's build-depends.

4. **`sc-tools` source-repository-package**:

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

   Troubleshooting: the first `cabal build` will fail asking for a
   `--sha256:` for each `source-repository-package`; paste the value
   cabal prints back into the block and rebuild.

5. **`allow-newer: maestro-sdk:containers`** (transitive dep workaround).

6. **Test-suite `build-depends`** must include at minimum:
   `base`, `convex-base`, `convex-coin-selection`, `convex-mockchain`,
   `convex-wallet`, `convex-testing-interface`, `convex-tasty-streaming`,
   `cardano-api`, `plutus-ledger-api`, `plutus-tx`, `QuickCheck`, `tasty`,
   `tasty-quickcheck`, `aeson`, `containers`. Plus the user's own
   library (so the test suite can call the off-chain helpers).
   `convex-tasty-streaming` is required because the default `Spec.hs`
   uses `defaultMainStreaming` (see §D.5) for IDE/extension integration.

**Never silently add.** Always show the user the current `cabal.project`
content and the proposed diff so they can see exactly what's changing.

## D. Fallback templates

Use these **only** when the user has no equivalent file in place AND has
confirmed that they want you to create one. Templates are fallback
starting points, not prescriptions. Show the user the template first.

### D.1 `cabal.project` (fallback)

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

with-compiler: ghc-9.6.6

index-state:
  , hackage.haskell.org 2026-02-09T00:00:00Z
  , cardano-haskell-packages 2026-02-09T00:00:00Z

write-ghc-environment-files: never

allow-newer: maestro-sdk:containers

test-show-details: direct
tests: True

packages:
  .

source-repository-package
  type: git
  location: https://github.com/input-output-hk/sc-testing-tools.git
  tag: 017066310f1f5387ec2c7848a50dac8da0de291f
  subdir:
    src/tasty-streaming
    src/testing-interface

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

### D.2 `flake.nix` (fallback)

Replace `<project>` with the user-confirmed project name.

```nix
{
  description = "<project> testing-interface scaffold";

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };
    nixpkgs.follows = "haskell-nix/nixpkgs-2411";
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    import ./nix/outputs.nix { inherit inputs system; }
  );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://sc-testing-tools.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "sc-testing-tools.cachix.org-1:EdJM0ldUx5PeP16xc1fjZ5oCGgryZJxf/Q1MHQ40M8s="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
```

**WARNING for the user**: if `accept-flake-config = true` is not in
`~/.config/nix/nix.conf` (or `/etc/nix/nix.conf`), nix will refuse the IOG
caches and rebuild GHC from source — multiple hours. Tell the user this
before the first `nix develop`.

### D.3 `nix/` files (fallback)

Use these only if the user wants a fresh flake skeleton. Replace
`<project>` where indicated.

`nix/pkgs.nix`:

```nix
{ inputs, system }:

import inputs.nixpkgs {
  inherit system;
  config = inputs.haskell-nix.config;
  overlays = [
    inputs.iohk-nix.overlays.crypto
    inputs.iohk-nix.overlays.cardano-lib
    inputs.haskell-nix.overlay
    inputs.iohk-nix.overlays.haskell-nix-crypto
    inputs.iohk-nix.overlays.haskell-nix-extra
  ];
}
```

`nix/project.nix` (replace `<project>`):

```nix
{ inputs, pkgs, lib }:

let
  cabalProject = pkgs.haskell-nix.cabalProject' (
    { config, pkgs, ... }:
    {
      name = "<project>";
      compiler-nix-name = lib.mkDefault "ghc966";
      src = lib.cleanSource ../.;
      flake.variants = { ghc966 = { }; };
      inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };
      cabalProjectLocal = "";
      modules = [ ];
    }
  );
in
cabalProject
```

`nix/shell.nix` (replace `<project>` in `name` and `PS1`):

```nix
{ inputs, pkgs, lib, project, utils, ghc, system }:

let
  allTools = {
    "ghc966".cabal = project.projectVariants.ghc966.tool "cabal" "latest";
    "ghc966".cabal-fmt = project.projectVariants.ghc966.tool "cabal-fmt" "latest";
    "ghc966".haskell-language-server = project.projectVariants.ghc966.tool "haskell-language-server" "latest";
    "ghc966".fourmolu = project.projectVariants.ghc966.tool "fourmolu" "latest";
    "ghc966".hlint = project.projectVariants.ghc966.tool "hlint" "latest";
  };

  tools = allTools.${ghc};

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {
    src = lib.cleanSources ../.;
    hooks = {
      cabal-fmt = { enable = true; package = tools.cabal-fmt; };
      fourmolu  = { enable = true; package = tools.fourmolu; args = [ ]; };
    };
  };

  commonPkgs = [
    tools.haskell-language-server
    tools.fourmolu
    tools.cabal
    tools.hlint
    tools.cabal-fmt
    pkgs.nixpkgs-fmt
    pkgs.bash
    pkgs.git
    pkgs.which
    pkgs.cacert
    pkgs.curl
    pkgs.zlib
  ];

  shell = project.shellFor {
    name = "<project>-${project.args.compiler-nix-name}";
    buildInputs = commonPkgs;
    withHoogle = true;
    shellHook = ''
      ${preCommitCheck.shellHook}
      export PS1="\n\[\033[1;32m\][<project>-shell:\w]\$\[\033[0m\] "
    '';
  };
in
shell
```

`nix/outputs.nix`:

```nix
{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };
  utils = import ./utils.nix { inherit pkgs lib; };
  project = import ./project.nix { inherit inputs pkgs lib; };

  mkShell = ghc: import ./shell.nix { inherit inputs pkgs lib project utils ghc system; };

  packages = { };

  devShells = rec {
    default = ghc966;
    ghc966 = mkShell "ghc966";
  };

  projectFlake = project.flake { };

  defaultHydraJobs = {
    ghc966 = projectFlake.hydraJobs.ghc966;
    inherit packages;
    inherit devShells;
    required = utils.makeHydraRequiredJob hydraJobs;
  };

  hydraJobsPerSystem = {
    "x86_64-linux"   = defaultHydraJobs;
    "aarch64-linux"  = defaultHydraJobs;
    "aarch64-darwin" = defaultHydraJobs;
  };

  hydraJobs = utils.flattenDerivationTree "-" hydraJobsPerSystem.${system};
in
{
  inherit devShells;
  inherit hydraJobs;
  inherit (projectFlake) apps;
  inherit (projectFlake) packages;
  project = project;
}
```

`nix/utils.nix`:

```nix
{ pkgs, lib }:

rec {
  flattenDerivationTree = separator: set:
    let
      recurse = name: name':
        flatten (if name == "" then name' else "${name}${separator}${name'}");
      flatten = name': value:
        let name = builtins.replaceStrings [":"] [separator] name'; in
          if lib.isDerivation value || lib.typeOf value != "set" then
            [{ inherit name value; }]
          else
            lib.concatLists (lib.mapAttrsToList (recurse name) value);
    in
      assert lib.typeOf set == "set";
      lib.listToAttrs (flatten "" set);

  mapAttrsValues = f: lib.mapAttrs (_name: f);

  makeHydraRequiredJob = hydraJobs:
    let
      cleanJobs = lib.filterAttrsRecursive
        (name: _: name != "recurseForDerivations")
        (removeAttrs hydraJobs [ "required" ]);
    in
      pkgs.releaseTools.aggregate {
        name = "required";
        meta.description = "All jobs required to pass CI";
        constituents = lib.collect lib.isDerivation cleanJobs;
      };
}
```

### D.4 Test-suite stanza (fallback)

Append to the user-confirmed `.cabal` file. Replace `<project>` and
`<Name>` with the user-confirmed names. The test directory (`test/`)
should match what the user agreed to in Step B.

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

  build-depends:
    , aeson
    , base                      >=4.14.0
    , cardano-api
    , cardano-ledger-conway
    , containers
    , convex-base
    , convex-coin-selection
    , convex-mockchain
    , convex-tasty-streaming
    , convex-testing-interface
    , convex-wallet
    , lens
    , mtl
    , <project>
    , plutus-ledger-api
    , plutus-tx
    , QuickCheck
    , tasty
    , tasty-hunit
    , tasty-quickcheck
```

### D.5 `test/Spec.hs` (fallback)

Use the streaming runner by default — the `--streaming-json` and
`--list-tests-json` flags are needed for IDE integration. Fall back to
plain `defaultMain` only on explicit user request.

```haskell
module Main where

import qualified <Name>Spec
import Convex.Tasty.Streaming (defaultMainStreaming)
import Test.Tasty (testGroup)

main :: IO ()
main = defaultMainStreaming $ testGroup "<project> tests" [<Name>Spec.tests]
```

`test/<Name>Spec.hs` must `module <Name>Spec (tests) where` and export
`tests :: TestTree`.

## E. Test location — no canonical answer

Some projects use `test/`. Some use `tests/`. Some put tests inside
sub-packages. Some have no existing test directory at all.

You **must** ask the user where new test files should live, even if `test/`
looks obvious. If the user has no test directory, propose `test/` as the
default and confirm before creating it.

## F. Build & test commands

```
NIX_CONFIG="system = x86_64-linux" nix develop -c cabal update
NIX_CONFIG="system = x86_64-linux" nix develop -c cabal build <testsuite>
NIX_CONFIG="system = x86_64-linux" nix develop -c cabal test <testsuite>
```

Where `<testsuite>` may be `all`, a package name, or a fully qualified
`<package>:test:<suite-name>`. Check the user-confirmed names before running.

The first `nix develop` of a fresh checkout can take a very long time if
the IOG cache substituters are not enabled. See §D.2.

Test output is printed directly to stdout (`test-show-details: direct`).
On failure, capture the last ~30 lines and share them with the user.

## G. Conflict handling

If discovery reveals a conflict — for example, the user's existing
`cabal.project` uses different `sc-tools` tags, or pins a different GHC
version, or already has `source-repository-package` entries pointing at
different forks — **do not silently change them**.

Show the conflict to the user:

> ## Conflict
>
> Current `cabal.project` contains:
>
> ```
> ...current snippet...
> ```
>
> The skill's expected version is:
>
> ```
> ...expected snippet...
> ```
>
> Options:
> 1. Keep the user's current version (may require adjusting other deps).
> 2. Adopt the skill's expected version (may require user buy-in on a
>    different `sc-tools` tag).
> 3. Hybrid: ...
>
> Which do you prefer?

Wait for the user's answer before changing anything.

If the user has a different nix setup (e.g. `niv`, plain `nixpkgs` overlay,
no flake at all), report that finding and ask. **Do not auto-convert.**

### G.1 Common build-failure red herrings

- **"X is unusable due to missing dependencies: Y, Z, …" wall on build.**
  If the dependency list looks suspiciously long (10+ packages) or
  includes things you definitely have, the culprit is usually a stale
  `dist-newstyle/` directory from a previous build with a different GHC
  or cabal config. Fix: run `cabal clean` or `rm -rf dist-newstyle` and
  rebuild. The error message is misleading — it almost never means a
  real dependency is missing.

## H. Per-touched-file log

After every file you touch, log a line in your running summary to the
user, in the form:

```
- <path>: created | modified | left alone (reason)
```

This trail makes it easy for the user to see exactly what happened.
