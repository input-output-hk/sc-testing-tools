# pbt-cli

A single self-contained binary that wraps `cabal test` for repositories built
on the sc-testing-tools property-based testing stack. It

- **discovers** test suites without compiling anything, and reports which are
  sc-testing-tools compatible,
- **runs** all of them, or just the ones you name — with `--stream` for live
  event rendering or `--json` for NDJSON,
- **lists** the tests inside one suite, authoritatively, and
- surfaces our custom test options (`--list-tests-json`, `--streaming-json`,
  `--test-id`, `--threat-model-name`) as first-class flags.

Every command prints a plain list by default and takes `--json` when you want
structure.

It supersedes `scripts/pre-fetch.sh` and the two tools behind it. Those needed
bash 4, Node, a `npm install`, and a checkout of the scripts; `pbt-cli` needs
only itself and `cabal`.

## Install

### From a release

Download the asset for your platform from the
[releases page](https://github.com/input-output-hk/sc-testing-tools/releases)
and mark it executable:

```sh
# pick one: linux-x64, linux-arm64, darwin-arm64
curl -L -o pbt-cli \
  https://github.com/input-output-hk/sc-testing-tools/releases/latest/download/pbt-cli-0.1.0.0-linux-x64
chmod +x pbt-cli
./pbt-cli --version
```

The binaries are dynamically linked against the platform's own libc, libgmp and
libffi; no Cardano system libraries (libsodium, secp256k1, blst) are needed,
because `pbt-cli` itself depends on none of the Cardano stack.

### From source

```sh
cabal install --project-file=cabal.project.pbt-cli exe:pbt-cli \
  --overwrite-policy=always
```

`cabal.project.pbt-cli` deliberately does **not** import the main
`cabal.project`: that would pull in cardano-api and plutus, and with them the
system libraries above. The quarantined project file solves in seconds against
Hackage only. `src/pbt-cli` is *also* listed in the main `cabal.project`, so
`cabal build pbt-cli` works in a normal `nix develop` shell.

## What "sc-testing-tools compatible" means

A suite is compatible when its `main-is` uses the streaming runner family:

| in `main-is` | classification |
| --- | --- |
| `defaultMainStreaming`, `defaultMainStreamingWithIngredients`, `defaultMainTestingInterface`, or an import of `Convex.Tasty.Streaming` / `Convex.TestingInterface` | `STREAMING` — compatible |
| plain `Test.Tasty.defaultMain` / `defaultMainWithIngredients` | `upstream` |
| a `main-is` with no recognised runner | `unknown` |
| a `main-is` whose source file is not on disk | `MISSING` |

Only `STREAMING` suites carry the `--streaming-json` and `--list-tests-json`
ingredients, since those come from `convex-tasty-streaming`. That is why
`tests`, `threat-models` and `run --stream` / `run --json` refuse a
non-compatible suite (exit `2`) instead of running something that would ignore
the flag and exit `0`. Plain `pbt-cli run` accepts any suite.

## Commands

```
pbt-cli suites   [ROOT] [--json|--compact|--table|--tsv] [--compatible-only]
pbt-cli tests    SUITE  [--json|--tree] [-p PAT] [--dry-run]
pbt-cli run      [SUITE...] [--stream|--json] [-p PAT] [--test-id IDS]
                            [--threat-model-name N] [--compatible-only] [--dry-run]
pbt-cli threat-models SUITE [--json] [--dry-run]
pbt-cli doctor   [ROOT]
pbt-cli --version
```

Defaults are plain lists — `suites` prints suite names, `tests` prints
`id<TAB>path`, `threat-models` prints names, and `run` passes cabal's own
console output through. `--json` on any of them gives you the structured form.

Every command takes `--root DIR` (`-C DIR`); `suites` and `doctor` also accept
the root positionally, the way `list-test-suites.sh` did.

### `suites` — discovery

No compilation, milliseconds. By default, just the names — so the output
composes:

```
$ pbt-cli suites
convex-tasty-streaming-test
convex-testing-interface-test
convex-auction-test
...

$ pbt-cli suites --compatible-only | wc -l
9
$ pbt-cli run $(pbt-cli suites --compatible-only)
```

`--table` adds the package, the compatibility column and `main-is`:

```
$ pbt-cli suites --table
root: /home/you/iog/sc-testing-tools

project: cabal.project
  SUITE                            PACKAGE                   PBT  ENTRY POINT  MAIN-IS
  -----                            -------                   ---  -----------  -------
  convex-tasty-streaming-test      convex-tasty-streaming    yes  STREAMING    test/Spec.hs
  convex-testing-interface-test    convex-testing-interface  yes  STREAMING    test/Spec.hs
  convex-auction-test              convex-use-cases          yes  STREAMING    test/Auction/Spec/Spec.hs
  ...

project: cabal.project.schema-gen
  SUITE                             PACKAGE            PBT  ENTRY POINT  MAIN-IS
  -----                             -------            ---  -----------  -------
  convex-schema-gen-streaming-test  convex-schema-gen  yes  STREAMING    test-streaming/Spec.hs
  convex-schema-gen-plain-test      convex-schema-gen  -    upstream     test-plain/Spec.hs

10 test suite(s), 9 sc-testing-tools compatible
```

`--json` emits the document described by
[`scripts/list-test-suites/list-test-suites.schema.json`](../../scripts/list-test-suites/list-test-suites.schema.json) —
the same schema the shell tool used, so existing consumers need no changes:

```json
{
  "root": "/home/you/iog/sc-testing-tools",
  "projects": [
    {
      "projectFile": "cabal.project",
      "packages": [
        {
          "name": "convex-use-cases",
          "cabalFile": "src/use-cases/convex-use-cases.cabal",
          "packageDir": "src/use-cases",
          "testSuites": [
            {
              "name": "convex-auction-test",
              "mainIs": "test/Auction/Spec/Spec.hs",
              "entryPoint": "STREAMING",
              "runTestsCommand": "cabal test convex-auction-test",
              "streamTestsCommand": "cabal test convex-auction-test --test-options=--streaming-json",
              "discoverCommand": "cabal test convex-auction-test --test-options=--list-tests-json",
              "hsSourceDirs": ["test"]
            }
          ]
        }
      ]
    }
  ],
  "orphans": []
}
```

Just the compatible ones, one name per line:

```sh
pbt-cli suites --compatible-only --tsv | cut -f1
```

`--compact` is single-line JSON for piping; `--tsv` is the legacy five-column
format (`suite`, `packageDir`, `mainPath`, `entryPoint`, `;`-joined
`hsSourceDirs`), with paths relative to the root in both.

### `run` — run the tests

```sh
pbt-cli run                              # every discovered suite
pbt-cli run convex-vesting-test          # one suite
pbt-cli run convex-vesting-test convex-auction-test
pbt-cli run --compatible-only            # skip upstream-tasty suites
pbt-cli run -p 'first bid'               # Tasty pattern
pbt-cli run convex-vesting-test --test-id 0,3,7
pbt-cli run --dry-run                    # print the cabal commands, run nothing
```

#### Output modes

| mode | output |
| --- | --- |
| *(default)* | cabal's and Tasty's own console output, passed straight through |
| `--stream` | the streaming events, rendered live |
| `--json` | the streaming events as NDJSON, each tagged with its `suite` |

```
$ pbt-cli run convex-pbt-cli-test convex-tasty-streaming-test --stream -p SrcLoc
== convex-pbt-cli-test ==
running 0 test(s)
OK: 0 passed, 0 failed in 0.001s
== convex-tasty-streaming-test ==
running 16 test(s)
  PASS  convex-tasty-streaming / SrcLoc / shim'd leaf has a location (0.000s)
  ...
```

```sh
pbt-cli run --json --compatible-only | jq 'select(.event == "test_done" and .success == false)'
```

`--json` forwards only the JSON lines, so cabal's own chatter is already gone —
this replaces the `jq -R 'fromjson? // empty'` dance the tasty-streaming README
describes. Each event gains a `suite` field:

```json
{"event":"test_started","id":0,"suite":"convex-pbt-cli-test"}
```

A `run` can cover many suites and the event schema carries no suite identity,
so without that field a consumer would see several indistinguishable
`suite_started` events. The
[streaming-events schema](../tasty-streaming/schema/streaming-events.schema.json)
does not set `additionalProperties: false`, so the added field is schema-valid
and a consumer that ignores unknown fields is unaffected.

`pbt-cli` decodes only the fields it renders and passes everything else through
untouched, so a suite built against a newer `convex-tasty-streaming` — with
event kinds this binary has never heard of — still streams correctly.

Both event modes need the `--streaming-json` ingredient, so they refuse a
selection containing an upstream-tasty suite rather than running one that would
ignore the flag, print its usual output and exit 0:

```
$ pbt-cli run --stream
pbt-cli: --stream needs sc-testing-tools compatible suites, but these are not:
  convex-schema-gen-plain-test (entry point: upstream)
Add --compatible-only to skip them, or drop the output flag.
```

#### How the cabal calls are grouped

In console mode, `run` groups the discovered suites by their project file and
issues one `cabal test` per group:

```
$ pbt-cli run --dry-run
cabal test convex-tasty-streaming-test convex-testing-interface-test convex-auction-test ...
cabal test convex-pbt-cli-test --project-file=cabal.project.pbt-cli
cabal test convex-schema-gen-streaming-test convex-schema-gen-plain-test ... --project-file=cabal.project.schema-gen
```

Suites are named explicitly rather than using cabal's `all` target, because
`cabal.project.schema-gen` imports `cabal.project` — so `cabal test all
--project-file=cabal.project.schema-gen` would re-run every suite in the
repository.

`--stream` and `--json` cannot group: cabal runs grouped targets sequentially
and their events carry no suite identity, so a grouped run would produce
several indistinguishable `suite_started` blocks. Those modes therefore invoke
cabal once per suite — a little more overhead, in exchange for correct
attribution.

### `tests` — what is in a suite

Authoritative — it builds the suite. By default, one `id<TAB>path` line per
test:

```
$ pbt-cli tests convex-pbt-cli-test
0	pbt-cli / Glob / isGlob / plain path is not a glob
1	pbt-cli / Glob / isGlob / star is a glob
2	pbt-cli / Glob / isGlob / question mark is a glob
3	pbt-cli / Glob / matchSegment / literal matches itself
```

```sh
pbt-cli tests convex-vesting-test            # id<TAB>path, one per line
pbt-cli tests convex-vesting-test --tree     # nested groups with ids
pbt-cli tests convex-vesting-test --json     # the suite's own suite_started payload
```

```
$ pbt-cli tests convex-pbt-cli-test --tree
pbt-cli/
  Glob/
    #12 expandGlob resolves a two-level pattern
    isGlob/
      #0 plain path is not a glob
      #1 star is a glob
```

The ids are what `--test-id` takes, so the usual loop is: discover ids, then
re-run the interesting ones.

### `threat-models` and `doctor`

```sh
pbt-cli threat-models convex-testing-interface-test          # one name per line
pbt-cli threat-models convex-testing-interface-test --json
```

One name per line by default, the suite's payload with `--json`.
`--list-threat-models-json` comes from `defaultMainTestingInterface`, so a
suite on plain `defaultMainStreaming` will report none.

```
$ pbt-cli doctor
pbt-cli — health check

[  OK  ] root          .
[  OK  ] cabal         cabal-install version 3.10.3.0
[  OK  ] ghc           The Glorious Glasgow Haskell Compilation System, version 9.6.6
[  OK  ] projects      3: cabal.project, cabal.project.pbt-cli, cabal.project.schema-gen
[  OK  ] test suites   10 discovered
[  OK  ] pbt suites    9 sc-testing-tools compatible
[  OK  ] orphans       none

All required dependencies present.
```

Only a missing *required* dependency or an unreadable root fails. `ghc` is a
warning, since cabal can be configured with a compiler that is not on `PATH`.

## Custom test options

| pbt-cli flag | reaches the suite as |
| --- | --- |
| `-p PAT`, `--pattern PAT` | `-p PAT` (Tasty pattern) |
| `--test-id IDS` | `--test-id IDS` |
| `--threat-model-name NAMES` | `--threat-model-name NAMES` |
| `--stream`, `--json` (on `run`) | `--streaming-json` |
| `--test-option OPT` (repeatable) | `OPT`, verbatim |

These are forwarded, not interpreted, so options a newer
`convex-tasty-streaming` adds work without a new `pbt-cli`.

They are passed with repeated singular `--test-option=` flags, never the plural
`--test-options=`: cabal splits the plural form on whitespace, and Tasty group
names contain spaces, so `-p 'auction tests'` would arrive mangled. Check what
will run with `--dry-run`:

```
$ pbt-cli run convex-vesting-test --dry-run -p 'auction tests' --test-id 0,3
cabal test convex-vesting-test --test-option=-p '--test-option=auction tests' --test-option=--test-id --test-option=0,3
```

Nothing goes through a shell — arguments are handed to the process as a list,
so patterns need no quoting. `--dry-run` output is quoted only so it can be
pasted into one.

The structured commands (`tests`, `threat-models`, `run --stream`,
`run --json`) also pass `--test-show-details=direct`, overriding whatever the
target repository configures. Cabal's default streams a suite's stdout through,
but a project that sets `test-show-details: failures` or `never` makes cabal
capture it into a log under `dist-newstyle` instead — and then pbt-cli's pipe
receives nothing and `run --json` exits 0 having emitted no events. pbt-cli
owns that pipe, so it owns the setting. Plain `pbt-cli run` is left alone: it
is a pass-through of cabal's console output, so your project's preference is
the right one there.

## Exit codes

| code | meaning |
| --- | --- |
| `0` | success |
| `1` | tests failed, or nothing matched (no suites, empty test tree) |
| `2` | usage error: unknown suite, or a suite that cannot do what was asked |
| `3` | discovery failed — the root is not a readable directory |
| `4` | `cabal` was not found on `PATH`, or could not be started |
| `5` | some other I/O failure |

`1` versus `3` is the contract `list-test-suites.sh` had: a readable root with
nothing to report is not the same as a bad root. `5` keeps `1` honest: a write
failure part-way through `suites --json`, or an `EMFILE` while forking cabal,
must not reach a CI consumer looking like a failing test run.

## Relationship to `scripts/`

`pbt-cli suites` is a native Haskell port of
[`scripts/list-test-suites/list-test-suites.sh`](../../scripts/list-test-suites/),
emitting the same JSON. The port is checked against the original by a
differential test (`test/PbtCli/ReferenceSpec.hs`) that runs the script over
this repository and compares both the JSON document and the TSV rows; it skips,
rather than fails, when the script cannot run (macOS ships bash 3.2, and the
script needs bash 4).

Two intentional differences:

- A trailing comma on a `packages:` entry is stripped. `cabal` accepts
  comma-separated lists; the shell version split on whitespace only and would
  silently drop such a package.
- `--tsv` paths are relative to the root, matching the JSON mode. The shell
  tool's `--tsv` did not relativise, so it emitted `./src/pkg` or an absolute
  path depending on how `ROOT` was spelled.

And two limitations shared with the reference:

- The `.cabal` reader is a line-oriented scanner, not cabal's own parser, so it
  does not evaluate conditional blocks (`if flag(…)`) or `common` stanza
  `import`s. A `main-is` that only appears inside one of those reads as
  `MISSING`. A suite affected by it can still be run with plain `pbt-cli run`,
  which does not need the entry point classified.
And one deliberate difference in the other direction: a package directory named
in `packages:` that is itself a **symlink** *is* reported by pbt-cli, and is not
by the reference. The scan does not follow symlinks — that is what stops a link
back to an ancestor looping — but `packages:` is an explicit instruction rather
than a search, so a package it names is honoured wherever it points. A
`packages:` entry that resolves *outside* the scanned root still cannot be
reported, since every path in the output is relative to that root; pbt-cli warns
on stderr rather than dropping it silently.

The Node/tree-sitter tool, [`scripts/list-tests/`](../../scripts/list-tests/),
has **no** counterpart here. It was the fast, approximate "Tier 1" tree for
rendering something on every keystroke, and it was always overridden by the
authoritative `--list-tests-json` tree — which is exactly what `pbt-cli tests`
asks for.

## Versioning and releases

The version lives in `convex-pbt-cli.cabal` and is what `--version` reports, so
an asset name and the binary inside it cannot drift apart.

Releases are tagged **`pbt-cli-v<version>`**:

```sh
git tag pbt-cli-v0.1.0.0
git push origin pbt-cli-v0.1.0.0
```

That triggers [`.github/workflows/pbt-cli-release.yaml`](../../.github/workflows/pbt-cli-release.yaml),
which creates the release, builds `exe:pbt-cli` for the four targets in
parallel (`fail-fast: false`, so one bad leg still ships the rest), uploads each
as `pbt-cli-<version>-<target>`, and finally promotes the release to `latest` —
but only if at least one asset was uploaded, so an all-legs-failed run never
points `releases/latest` at an empty release.

Bump the version in `convex-pbt-cli.cabal`, add a `CHANGELOG.md` entry, merge,
then tag.
