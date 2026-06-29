# Static test-suite discovery

`list-test-suites.sh` reads a Haskell repository's `*.cabal` and
`cabal.project` files and reports every test suite, its project structure, and
ready-to-run `cabal` commands — **without compiling anything**. It resolves
`cabal.project` imports, maps each package to the project file that owns it,
flags orphaned `.cabal` files, and classifies how each suite's `main-is` runs
tests.

The intended consumer is an editor or IDE test explorer (for example, a VS Code
extension) that needs the test tree quickly, before or instead of a build.

## You do not need `nix develop`

Run this from a plain system shell. It uses only standard Unix tools, and the
Nix development shell makes it *slower*, not faster: the same scan measures
around **1.2s inside `nix develop`** versus **~0.01s in a normal shell**,
because the dev shell wraps coreutils and bash behind heavier store paths and
PATH indirection. There is nothing to gain by running it under Nix.

## Dependencies

- `bash` 4+ (for `mapfile` and associative arrays)
- `awk`, `grep`, `sed`, `find`, `sort`, `dirname`

That is the whole list. The tool has **no `jq` dependency** — it writes JSON to
stdout, and `jq` is only useful on the consumer side for pretty-printing. (The
test harness, `test-list-test-suites.sh`, *does* require `jq`; the tool itself
does not.)

## Usage

```
list-test-suites.sh [ROOT] [--tsv]
```

- `ROOT` — directory to scan; defaults to `.`.
- `--tsv` — emit the legacy tab-separated format instead of JSON.
- Exit codes: `0` on success, `1` when a valid directory contains no test
  suites, `2` on a usage error or when `ROOT` is not a directory.

```bash
# scan the current repo (default ROOT = .)
scripts/list-test-suites/list-test-suites.sh

# pretty-print with jq
scripts/list-test-suites/list-test-suites.sh | jq .

# scan another repo
scripts/list-test-suites/list-test-suites.sh /path/to/some/haskell/repo

# legacy tab-separated output
scripts/list-test-suites/list-test-suites.sh --tsv
```

## Output

Default output is single-line JSON on stdout. Pipe through `jq .` to read it:

```json
{
  "root": "/home/alice/iog/pingpong",
  "projects": [
    {
      "projectFile": "cabal.project",
      "packages": [
        {
          "name": "pingpong",
          "cabalFile": "pingpong.cabal",
          "packageDir": ".",
          "testSuites": [
            {
              "name": "pingpong-test",
              "mainIs": "test/Spec.hs",
              "entryPoint": "upstream",
              "runTestsCommand": "cabal test pingpong-test",
              "streamTestsCommand": null,
              "discoverCommand": null
            }
          ]
        }
      ]
    }
  ],
  "orphans": []
}
```

### Entry-point classification

Each suite's `entryPoint` records how its `main-is` runs tests:

- `STREAMING` — uses the streaming runner family (`defaultMainStreaming`,
  `defaultMainStreamingWithIngredients`, or `defaultMainTestingInterface` — that
  is, anything from `Convex.Tasty.Streaming` or `Convex.TestingInterface`).
  These support structured test discovery and streaming results.
- `upstream` — plain `Test.Tasty.defaultMain` or `defaultMainWithIngredients`.
  Runnable, but without the streaming library's discovery.
- `unknown` — a `main-is` file is present but uses no recognized runner.
- `MISSING` — the `main-is` source file was not found on disk.

### Commands per suite

Each suite carries up to three commands:

- `runTestsCommand` — always present: `cabal test <suite>`, plus
  `--project-file=<file>` when the package is owned exclusively by a non-default
  project file (such as `cabal.project.schema-gen`).
- `streamTestsCommand` — STREAMING suites only; appends
  `--test-options=--streaming-json`. `null` otherwise.
- `discoverCommand` — STREAMING suites only; appends
  `--test-options=--list-tests-json` for a structured test tree without running
  anything. `null` otherwise.

The same suite object looks different depending on its `entryPoint`. Three
illustrative examples:

**STREAMING** — all three commands are populated:

```json
{
  "name": "vault-spec-test",
  "mainIs": "test/Spec.hs",
  "entryPoint": "STREAMING",
  "runTestsCommand": "cabal test vault-spec-test",
  "streamTestsCommand": "cabal test vault-spec-test --test-options=--streaming-json",
  "discoverCommand": "cabal test vault-spec-test --test-options=--list-tests-json"
}
```

**upstream** — only `runTestsCommand`; the streaming commands are `null`:

```json
{
  "name": "ledger-prop-test",
  "mainIs": "test/Main.hs",
  "entryPoint": "upstream",
  "runTestsCommand": "cabal test ledger-prop-test",
  "streamTestsCommand": null,
  "discoverCommand": null
}
```

**unknown** — a recognized `main-is`, but no known runner:

```json
{
  "name": "oracle-golden-test",
  "mainIs": "test/Golden.hs",
  "entryPoint": "unknown",
  "runTestsCommand": "cabal test oracle-golden-test",
  "streamTestsCommand": null,
  "discoverCommand": null
}
```

### Multiple projects and orphans

A repository can hold several `cabal.project*` files. Each package is reported
once, under the project file that introduces it: a variant that `import:`s a
base project shows only the packages it adds, not the inherited ones. Any
`.cabal` referenced by no project file is listed under `orphans` and also raises
a warning on stderr.

## Legacy `--tsv` mode

`--tsv` emits four tab-separated columns —
`<suite>`, `<package-dir>`, `<main-path>`, `<entry-point>`:

```
pingpong-test	.	test/Spec.hs	upstream
```

## Schema

`list-test-suites.schema.json` is a JSON Schema (draft 2020-12) describing the
output shape, including the invariant that `streamTestsCommand` and
`discoverCommand` are non-null only for STREAMING suites. Consumers can use it
for validation or type generation.

## Testing

```bash
bash scripts/list-test-suites/test-list-test-suites.sh
```

The harness runs 107 assertions over hermetic synthetic fixtures, this
repository, and the golden baseline in `testdata/golden.tsv`. It requires `jq`.

## Provenance

This script, its test harness, and the JSON Schema were generated entirely by an
LLM — Claude Opus 4.8 (Anthropic).

See the [list-tests README](../list-tests/README.md) for the sibling
`list-tests` (#65) tool, and the [project README](../../README.md) for the wider
`sc-testing-tools` libraries.
