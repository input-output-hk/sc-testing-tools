# list-tests

Static (no-compile) discovery of the **tasty test tree** inside each test
suite's `.hs` files, using
[`tree-sitter-haskell`](https://github.com/tree-sitter/tree-sitter-haskell)
via WASM. This is the **Tier-1** list-tests tool: a fast, best-effort
approximation of the test tree that needs no GHC compilation, so an editor or
VS Code extension can render *something* on every keystroke.

## Two-tier model

Test discovery has two tiers, and this tool is the *fast, approximate* one:

- **Tier 1 — this tool (fast, approximate).** Scans `.hs` files statically and
  guesses the tasty tree in milliseconds, without compiling. It does not need to
  be exact; it only needs to be plausible and instant.
- **Tier 2 — `--list-tests` / streaming (authoritative).** The real tree, from
  the streaming `--list-tests-json` runner. It requires a compiled project, but
  when it arrives it **overrides** whatever Tier 1 guessed.

Because Tier 2 always corrects Tier 1, every tree this tool emits is a guess to
be replaced once the authoritative tree is available. The two tools have
different output shapes, so the editor already knows which tier produced a tree —
there is no per-node "authoritative" flag (it would be a constant `false` and
carry no information). For the *individual* provisional nodes whose content will
change after `--list-tests`, see the [`pendingExpansion`](#pendingexpansion-and-roles) flag below.

## Why Node + tree-sitter (and not pure bash)

The sibling [`list-test-suites.sh`](../list-test-suites/list-test-suites.sh) (the list-test-suites tool) is pure,
zero-dependency bash because it only reads `*.cabal` / `cabal.project` files —
plain text with simple structure. Parsing Haskell *source* is a different
problem: it needs a real grammar that tolerates broken/incomplete code (the
buffer is mid-edit) and resolves applications, lists, qualified names, operators
and type applications. `tree-sitter-haskell` does exactly that, and it has no
usable CLI or Python binding available on the target machine — only the Node
runtime. So this one tool accepts Node + a WASM grammar as a dependency, while
list-test-suites stays zero-dep.

## Setup

```bash
# convenience wrapper (from the repo root)
./scripts/pre-fetch.sh install

# or directly with npm (from anywhere)
npm install --ignore-scripts --prefix scripts/list-tests
```

This restores `node_modules` with the two dependencies:
[`web-tree-sitter`](https://www.npmjs.com/package/web-tree-sitter) (the WASM
engine) and [`tree-sitter-haskell`](https://www.npmjs.com/package/tree-sitter-haskell)
(the grammar package, which ships the prebuilt
`tree-sitter-haskell.wasm`). The loader reads the grammar from
`node_modules/tree-sitter-haskell/tree-sitter-haskell.wasm`.

**`--ignore-scripts` is required.** The `tree-sitter-haskell` package has a
native postinstall build (`node-gyp-build`) that fails on this machine and is
unnecessary — the prebuilt `.wasm` ships in the package regardless of that
build. `--ignore-scripts` skips it; our own `package.json` has no risky
postinstall, so only that dependency's native step is suppressed.

Check whether the dependencies are present at any time:

```bash
./scripts/pre-fetch.sh --check-health
# or just this tool:
node scripts/list-tests/list-tests.js --check-health
```

## Relationship to `list-test-suites.sh` (the list-test-suites tool)

This tool **consumes** the per-suite JSON emitted by
[`../list-test-suites/list-test-suites.sh`](../list-test-suites/list-test-suites.sh). By default it invokes that
script itself, so a single command turns a repo into a full test tree. It needs
the `hsSourceDirs` field (added to list-test-suites) to resolve imported modules to files.

## Usage

```bash
# Run the list-test-suites tool internally, then extract every suite's tree.
# <ROOT> is the repo to scan; use `.` for the current repo:
node scripts/list-tests/list-tests.js .
node scripts/list-tests/list-tests.js /path/to/repo

# Use a captured test-suite discovery JSON instead of re-running the script:
node scripts/list-tests/list-tests.js --from-json suites.json

# Chain: feed test-suite discovery JSON on stdin as an override:
./scripts/list-test-suites/list-test-suites.sh . | node scripts/list-tests/list-tests.js

# Filter to one suite (handy while iterating):
node scripts/list-tests/list-tests.js . --suite convex-vesting-test

# Tune the cross-file follow backstop (see below; default 100):
node scripts/list-tests/list-tests.js . --limit 50

# Help:
node scripts/list-tests/list-tests.js --help
```

Flags:

| flag               | meaning                                                           |
| ------------------ | ----------------------------------------------------------------- |
| `<ROOT>`           | repo to scan (e.g. `.`); invokes `list-test-suites.sh` internally |
| `--from-json <f>`  | read test-suite discovery JSON from file `f` instead of running the script |
| `--suite <name>`   | restrict output to a single named suite                           |
| `--limit <N>`      | cross-file **follow** backstop (default 100), *not* tree depth    |
| `-h`, `--help`     | print usage and exit                                              |

`--limit` bounds only how many times a reference is **followed into another
file** (plus a visited-set for cycle detection). The test **tree depth is
unlimited** — only cross-file hops are capped. 100 is an effectively-unlimited
backstop against pathological repos; real suites need 1–17.

## Output

Pretty-printed JSON with shape `{ root, suites: [ ... ] }`. Each suite carries
`suite`, `package`, `entryFile`, `entryPoint`, and the extracted `tree`:

```json
{
  "root": "/path/to/repo",
  "grammar": "scripts/list-tests/node_modules/tree-sitter-haskell/tree-sitter-haskell.wasm",
  "suites": [
    {
      "suite": "convex-vesting-test",
      "package": "src/use-cases",
      "entryFile": "src/use-cases/test/Vesting/Spec/Spec.hs",
      "entryPoint": "STREAMING",
      "entryBinding": "main",
      "crossFileFollows": 2,
      "tree": { "kind": "group", "label": "vesting tests", "...": "..." }
    }
  ]
}
```

## Node shape & provenance

Every emitted node has:

| field              | meaning                                                              |
| ------------------ | ------------------------------------------------------------------- |
| `kind`             | `group` \| `test` \| `placeholder`                                  |
| `label`            | display label (string-literal arg, unquoted)                       |
| `source`           | `parsed` \| `synthesized` \| `dynamic`                              |
| `testingInterface` | `true` on every node in a TestingInterface synthesized subtree      |
| `role`             | semantic role within a TestingInterface subtree (enum; see below)  |
| `pendingExpansion` | `true` only on genuinely unknown/unenumerable nodes (dynamic placeholders, unresolved cross-package refs); `false` on the fully-known TestingInterface synthesized tree |
| `children`         | nested nodes — **present ONLY on `group` nodes**; leaves omit it    |
| `file`, `line`     | source location (root-relative), when known                       |
| `model`            | (synthesized prop tests) the TestingInterface model name           |
| `note`             | rationale for synthesized/dynamic nodes                            |

Only `kind`, `label`, and `source` are required; the rest are optional and
omitted when not applicable. (There is no `authoritative` field — it was removed
as a constant `false` that carried no information.)

`children` is a **group-only** property: it is present **only** on `group`
nodes, and its **absence is the signal that a node is a leaf**. Leaf nodes
(`kind: "test"` and `kind: "placeholder"`, including the synthesized
`Positive tests` / `Negative tests` and threat-model / expected-vulnerability
leaves) carry **no `children` key at all** — not even an empty `[]`. The schema
enforces this with a conditional (if `kind == "group"` then `children` is
required, else `children` is forbidden).

### `pendingExpansion` and roles

`pendingExpansion: true` marks a node whose real content is **genuinely unknown
statically** — we cannot enumerate it without running the slow `--list-tests`
command. It is set **only** on:

- unresolved cross-project / cross-package references (we can't find the file), and
- every `dynamic` placeholder (`map` / `let` / list-comprehension children, etc.).

The entire **TestingInterface synthesized subtree** carries
`pendingExpansion: false`. Its shape has been **verified** to match the real
tasty tree in labels, counts, nesting and **order** — so it is FULLY KNOWN
statically. This applies uniformly to the synthesized top group, the
`Positive tests` / `Negative tests` leaves, the `Threat models` and
`Expected vulnerabilities` groups, and every threat-model /
expected-vulnerability leaf. The leaf labels are approximate (the real rendered
name comes from `getThreatModelName`), but a rename is **not** an expansion, so
it does not make the node pending.

`testingInterface: true` marks every node that belongs to a TestingInterface
(`propRunActions*`) synthesized subtree — the top synthesized group **and** all
of its descendants. Plain parsed/literal nodes never carry it.

`role` carries the **semantic role** of a node inside a TestingInterface subtree
so the extension can label/icon it:

| `role`                            | applies to                                                |
| --------------------------------- | --------------------------------------------------------- |
| `positive`                        | the `Positive tests` leaf                                 |
| `negative`                        | the `Negative tests` leaf                                 |
| `threat-models-group`             | the `Threat models` group                                 |
| `expected-vulnerabilities-group`  | the `Expected vulnerabilities` group                      |
| `threat-model`                    | the individual leaves under **both** of the above groups  |

> **Note:** expected-vulnerability leaves are structurally identical to
> threat-model leaves (both are rendered via `getThreatModelName`, both are
> `testCaseSteps`). They therefore share `role: threat-model`; the semantic
> difference between "expected NOT to find a vuln" and "expected to FIND a vuln"
> is carried only by the **parent group's** role. There is no separate
> `expected-vulnerability` leaf role. Nodes with no special role omit `role`.

### The three node classes

1. **`parsed`** — literal tasty trees written out in source:
   `testGroup` / `testCase` / `testProperty` / `testCaseSteps` /
   `testCaseInfo` / `goldenVsFile` / `goldenVsString`. The label is the
   string-literal argument; children come from the literal list argument.
   Read directly from the syntax tree. High confidence.
2. **`synthesized`** — `propRunActions` / `propRunActionsWithOptions @Model`
   from the **TestingInterface** framework. The real tree is generated at
   *runtime*, so it cannot be read syntactically — but this repo *owns* the
   generator (see `propRunActionsWithOptions` in
   `src/testing-interface/lib/Convex/TestingInterface.hs`), so its shape is
   known. The tool reads the `ThreatModelsFor` instance's
   `threatModels = [...]` and `expectedVulnerabilities = [...]` lists and
   synthesizes a faithful subtree with this **exact shape and order** (the order
   is a hard invariant — getting it wrong makes the tree diverge from what
   `--list-tests` returns):

   ```
   "<groupName>"                       (top synthesized group; testingInterface, pendingExpansion=false)
     ├── "Positive tests"              (leaf, ALWAYS present, role=positive)
     ├── "Negative tests"              (leaf, ALWAYS present, role=negative)
     ├── "Threat models"              (group, ONLY if threatModels non-empty, role=threat-models-group)
     │     ├── <threatModels[0]>       (leaf, role=threat-model)
     │     └── ...                     (one per list element, in list order)
     └── "Expected vulnerabilities"   (group, ONLY if expectedVulnerabilities non-empty, role=expected-vulnerabilities-group)
           ├── <expectedVulnerabilities[0]>  (leaf, role=threat-model)
           └── ...                     (one per list element, in list order)
   ```

   - `<groupName>` is the string passed to `propRunActions` /
     `propRunActionsWithOptions`.
   - The literal labels are exactly `Positive tests`, `Negative tests`,
     `Threat models` (lowercase `m`), `Expected vulnerabilities` (lowercase `v`).
   - `Positive tests` and `Negative tests` are **always** present. The
     `Threat models` group appears only if `threatModels` is non-empty; the
     `Expected vulnerabilities` group only if `expectedVulnerabilities` is
     non-empty. With both lists empty, only the two leaves appear.
   - **Default `threatModels` (no explicit list).** Many instances do *not*
     write a `threatModels = [...]` literal — they rely on the library default,
     which is `deleteFirstsBy eqName allThreatModels (expectedVulnerabilities)`
     (see `Convex/TestingInterface.hs`). For these (binding absent, or RHS is the
     bare identifier `allThreatModels`/`All.allThreatModels`), the tool
     enumerates the default set from a **hardcoded mirror** of
     `allThreatModels` (`ALL_THREAT_MODELS` in `lib/synthesize.js`, 19 entries in
     source order) and **subtracts** any entry that also appears in that
     instance's `expectedVulnerabilities`. So default-set property tests now show
     their `Threat models` group too (e.g. `ctf purchase_offer`: 19 − 3 expected
     vulnerabilities = 16 leaves). These leaves are structurally identical to
     explicit-list leaves (`pendingExpansion: false`). **Keep-in-sync caveat:**
     `ALL_THREAT_MODELS` is a static mirror — if the Haskell `allThreatModels`
     list in `Convex/ThreatModel/All.hs` changes, update that array.
   - Leaf labels are a **best guess** from the source list-element text; the real
     rendered name comes from `getThreatModelName` (fallback
     `"Threat model <n>"` / `"Expected vulnerability <n>"`). A rename is not an
     expansion, so every leaf is still `pendingExpansion: false`. The **count
     and order** are kept exact — count + order are what keep the tree stable
     across the Tier-1/Tier-2 swap.
3. **`dynamic`** — placeholders for anything that cannot be resolved or
   enumerated statically: unresolved references, non-literal labels, and
   children built by `map` / `fmap` / `let` / list comprehensions. Emitted as
   a single `placeholder` node with an explanatory `note`; it **never
   fabricates children**.

### Override model

`source` records *how* a node was derived. This whole tree is the Tier-1
approximation; when the slow `--list-tests` / streaming result arrives, it is
the authoritative tree and **replaces** this output entirely. The two tools have
different output shapes, so a consumer can tell the tiers apart without any
per-node flag. For the individual provisional nodes that will visibly change
after `--list-tests`, use `pendingExpansion`. The provenance fields let a
consumer show the guess immediately and swap it out honestly.

## Cross-file resolution

References like `Vesting.Spec.Unit.unitTests` or unqualified
`import Escrow.Spec.Unit (unitTests)` are **followed** into their defining
files, resolved via the per-file import table (handles `import X`,
`import X qualified`, `import X qualified as Y`, `import X as Y`, and explicit
import lists) + the suite's `hsSourceDirs`.

- The **test tree depth is unlimited.**
- Only **cross-file follows** are bounded — default 100 (`--limit`), an
  effectively-unlimited backstop. Real suites need 1–17.
- A visited `(file, binding)` set provides cycle detection.

## Known limitations

This is deliberately a best-effort approximation (Tier 2 corrects it), so the
following are accepted trade-offs rather than bugs:

- **First clause only.** For a multi-clause top-level function, only the first
  clause's right-hand side is read. Fine for the typical single-clause tree
  binding; a limitation in general.
- **Non-literal structure becomes a placeholder.** A non-literal label
  (`testGroup name [...]`) or children produced by `map` / `let` / list
  comprehension cannot be enumerated statically and are emitted as a `dynamic`
  placeholder rather than fabricated.
- **Cross-module `ThreatModelsFor` is lightly exercised.** The `ThreatModelsFor`
  instance can live in a different module than the `propRunActions` call; the
  tool searches the model's defining module, but this path has had limited
  real-world coverage so far.

## Packaging / dependencies

**Required:**

- `node` (tested on v18.17.1).
- `web-tree-sitter` (the WASM engine) — an npm dependency.
- the `tree-sitter-haskell.wasm` grammar — shipped prebuilt inside the
  `tree-sitter-haskell` npm package, installed via `--ignore-scripts`.
- `../list-test-suites/list-test-suites.sh` (and its shell deps) for `<ROOT>`
  mode, where the suites are discovered before extraction.

**Optional:**

- `jq` — used only by the examples / test harness.

Install with `./scripts/pre-fetch.sh install` (or `npm install --ignore-scripts
--prefix scripts/list-tests`). `--ignore-scripts` is required: it skips the
`tree-sitter-haskell` package's native postinstall build, which fails on this
machine and is unnecessary because the prebuilt `.wasm` ships in the package.
The loader reads the grammar from
`node_modules/tree-sitter-haskell/tree-sitter-haskell.wasm`. `node_modules` is
git-ignored; `package.json` and `package-lock.json` are tracked.

```
scripts/list-tests/
├── list-tests.js          # CLI entry
├── lib/
│   ├── parser.js          # grammar load (from node_modules) + module parse/index
│   ├── ast.js             # tree-sitter-haskell node helpers
│   ├── imports.js         # per-file import table (qualified/aliases/lists)
│   ├── resolver.js        # module->file + follow budget + cycle detection
│   ├── extract.js         # core tasty recursive descent
│   ├── synthesize.js      # TestingInterface (propRunActions) synthesis
│   └── suites.js          # drive extraction over test-suite discovery JSON
├── node_modules/          # npm deps (git-ignored); grammar wasm lives under
│                          #   tree-sitter-haskell/tree-sitter-haskell.wasm
├── testdata/              # golden trees for the harness
├── list-tests.schema.json # output JSON Schema (draft 2020-12)
└── package.json
```

## Performance

A full scan of this repository takes roughly **1.5s** (most of which is loading
the WASM grammar once). Run it from a **plain system shell** — *not* inside
`nix develop`, which wraps coreutils behind heavier store paths and only slows
the surrounding tooling down.

## Testing

```bash
bash scripts/list-tests/test-list-tests.sh
```

The harness runs **68 assertions**: a smoke run, recursive structural
conformance over every emitted node (including `role` / `testingInterface` /
`pendingExpansion` invariants, and the group-only `children` rule — leaves carry
no `children` key, groups always do), value spot-checks (including the TestingInterface
`Positive tests` → `Negative tests` → `Threat models` → `Expected vulnerabilities`
labels and their order), and a diff of four deterministic suites against
committed golden trees. It validates output against
`list-tests.schema.json` (using a real JSON Schema validator if one is on
`PATH`, otherwise a `jq` structural fallback). Golden trees live in `testdata/`.
The harness requires `jq` and `node`; it does **not** compile any Haskell.

## Provenance

This tool, its library modules, test harness, and JSON Schema were generated
entirely by an LLM — Claude Opus 4.8 (Anthropic).

See the [list-test-suites README](../list-test-suites/README.md) for the sibling
`list-test-suites.sh` (the list-test-suites tool), and the [project README](../../README.md) for the wider
`sc-testing-tools` libraries.
