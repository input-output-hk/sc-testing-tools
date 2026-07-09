# convex-tasty-streaming

A Tasty ingredient that streams test results as **NDJSON** (newline-delimited JSON) to stdout. Designed for consumption by IDE extensions, VS Code test explorers, and external tooling that need real-time structured test output.

## Integration

### 1. Add the dependency

In your `.cabal` file, add `convex-tasty-streaming` to the test suite's `build-depends`:

```cabal
test-suite my-tests
  build-depends:
    , convex-tasty-streaming
    , tasty
    ...
```

### 2. Replace `defaultMain`

In your test entry point, swap `defaultMain` for `defaultMainStreaming`:

```haskell
-- Before
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain tests

-- After
import Convex.Tasty.Streaming (defaultMainStreaming)

main :: IO ()
main = defaultMainStreaming tests
```

`defaultMainStreaming` behaves identically to `defaultMain` by default. The streaming features are only activated when their CLI flags are passed. Normal console output is unchanged.

If you use a wrapper like `withCoverage`, just replace the `defaultMain` call inside it:

```haskell
main :: IO ()
main = withCoverage config $ \opts runOpts ->
  defaultMainStreaming (tests opts runOpts)
```

If you need package-specific Tasty ingredients (custom option managers,
listing modes, etc.), use `defaultMainStreamingWithIngredients` and pass
them explicitly:

```haskell
import Convex.Tasty.Streaming (defaultMainStreamingWithIngredients)

main :: IO ()
main = defaultMainStreamingWithIngredients [myIngredientA, myIngredientB] tests
```

### 3. Add the package to `cabal.project`

```
packages:
  src/tasty-streaming
  ...
```

## Usage

### Discover tests (no execution)

List the full test tree as structured JSON without running any tests:

```bash
cabal test convex-testing-interface-test --test-options="--list-tests-json"
```

Combine with Tasty's `-p` pattern flag to filter:

```bash
cabal test convex-testing-interface-test --test-options="--list-tests-json -p 'ping-pong'"
```

### Run selected tests by ID

You can run only specific tests by passing one or more Tasty IDs with
`--test-id` (comma-separated):

```bash
cabal test convex-testing-interface-test --test-options="--test-id 0"
```

```bash
cabal test convex-testing-interface-test --test-options="--test-id 0,3,7"
```

Recommended workflow:

1. Use `--list-tests-json` to discover IDs.
2. Re-run with `--test-id` using the IDs you want.

Behavior notes:

- Unknown IDs fail fast with a helpful error.
- For threat-model and expected-vulnerability tests, required prerequisites
  (such as `Positive tests`) are included automatically.
- In JSON outputs (`--list-tests-json` and `--streaming-json`), `--test-id` runs preserve the 
  original test IDs (so they match the IDs discovered via `--list-tests-json`), which may be 
  sparse rather than reindexed.

### Stream test results

Run tests with real-time NDJSON output instead of console output:

```bash
cabal test convex-testing-interface-test --test-options="--streaming-json"
```

Combine with pattern filtering:

```bash
cabal test convex-testing-interface-test --test-options="--streaming-json -p 'ping-pong'"
```

Combine streaming with ID filtering:

```bash
cabal test convex-testing-interface-test --test-options="--streaming-json --test-id 0,3"
```

## NDJSON Event Schema

Each line of output is a self-contained JSON object with an `event` field. Events are emitted in this order:

| Event            | When                           | Fields                                                          |
|------------------|--------------------------------|-----------------------------------------------------------------|
| `suite_started`  | Before any test runs           | `tests[]` — array of `{id, name, path}`                        |
| `test_started`   | A test begins executing        | `id`                                                            |
| `test_done`      | A test completes               | `id`, `success`, `duration`, `description`, optional `failure`  |
| `suite_done`     | After all tests finish         | `passed`, `failed`, `duration`                                  |

### `suite_started`

```json
{
  "event": "suite_started",
  "tests": [
    {"id": 0, "name": "First bid equals minimum bid", "path": ["auction tests", "unit tests"]},
    {"id": 1, "name": "Positive tests", "path": ["auction tests", "property-based tests"]}
  ]
}
```

- `id` — stable integer index, used to correlate `test_started` and `test_done` events
- `name` — the test's own name (leaf label in the Tasty tree)
- `path` — ordered list of group names from root to the test's parent

### `test_started`

```json
{"event": "test_started", "id": 0}
```

Emitted when the test transitions from queued to executing.

### `test_done`

Success:

```json
{"event": "test_done", "id": 0, "success": true, "duration": 0.217, "description": "First bid equals minimum bid"}
```

Failure:

```json
{
  "event": "test_done",
  "id": 1,
  "success": false,
  "duration": 0.456,
  "description": "Positive tests",
  "failure": {
    "reason": "TestFailed",
    "message": "Expected 1 but got 2"
  }
}
```

### `suite_done`

```json
{"event": "suite_done", "passed": 55, "failed": 0, "duration": 79.6}
```

## Redeemer data in `test_trace`

Each `test_trace` event carries, per transition, the transaction's
inputs as a list of `TxInputSummary` objects. For **script inputs**
(inputs protected by a Plutus validator), each summary now surfaces the
redeemer in two layers.

### Tier 1 — always on

Two fields are populated for every script input with no setup required:

- `redeemerRaw` — hex CBOR of the redeemer's `ScriptData`. `null` for
  non-script inputs (no redeemer in the witness set).
- `redeemerConstr` — the Plutus `Constr` index when the redeemer parses
  to `Constr n _`. `null` otherwise, and for non-script inputs.

This lets consumers (e.g. the VS Code extension) bucket script inputs by
constructor index without any per-spec opt-in.

### Tier 2 — opt-in via `redeemerTagger`

The library cannot know your redeemer ADT's shape — the compiled Plutus
script has lost the constructor names — so Tier 2 is opt-in. Override
the optional `redeemerTagger` method on your `TestingInterface`
instance to map a parsed Plutus `Data` to a human-readable label and an
optional JSON payload. When the tagger returns a label, two more fields
are populated:

- `redeemerKind` — a human-readable discriminator, e.g. `"Pong"`.
- `redeemerPayload` — optional JSON payload of the redeemer's inner
  data.

When not opted in (or the tagger declines a given redeemer), both are
`null`; Tier 1 fields are still present for script inputs. The default
`redeemerTagger` is a no-op, so existing specs keep streaming unchanged.

### Full `TxInputSummary` shape

```json
{
  "utxo":            "txid#idx",
  "address":         "bech32",
  "value":           { "lovelace": 0, "assets": [] },
  "redeemerRaw":     "hex-cbor" | null,
  "redeemerConstr":  0           | null,
  "redeemerKind":    "Pong"      | null,
  "redeemerPayload": <any-json>  | null
}
```

All seven fields are `required`; the four redeemer fields are nullable.

### Labelling helpers

Three helpers, from one-liner to fully manual. All are
re-exported from `Convex.TestingInterface` (defined in
`Convex.TestingInterface.Trace.RedeemerTag`).

**Auto label — one-liner** for nullary redeemers such as `Ping | Pong | Stop`:

```haskell
import Convex.TestingInterface (autoRedeemerTag)

instance TestingInterface MyModel where
  -- ...
  redeemerTagger = autoRedeemerTag (Proxy @MyRedeemer)
```

Decodes via `FromData` and labels with the Haskell `Show` of the decoded
value. The PingPong spec (`PingPongSpec.hs`) is the reference example —
`"Ping"`, `"Pong"`, `"Stop"` appear as `redeemerKind` on script inputs.

**Label with a function — explicit labelling** for redeemers that carry useful data:

```haskell
import Convex.TestingInterface (labelRedeemer, RedeemerTag (..))

redeemerTagger =
  labelRedeemer (Proxy @MyRedeemer) $ \case
    Buy n  -> RedeemerTag "Buy"   (Just (toJSON n))
    Cancel -> RedeemerTag "Cancel" Nothing
```

Decodes via `FromData` and applies your labelling function for kind and
optional payload.

**Raw `Data` matching — escape hatch** when you don't want to decode via
`FromData`. Construct a `RedeemerTagger` directly from a
`Data -> Maybe RedeemerTag` function:

```haskell
import Convex.TestingInterface.Trace (RedeemerTagger (..), RedeemerTag (..))

redeemerTagger = RedeemerTagger $ \d -> Just (RedeemerTag "custom" Nothing)
```

`RedeemerTagger` is a `Monoid` (first `Just` wins), so helpers can be
combined with `<>`.

## Parsing with jq

Since `cabal test` prints its own non-JSON lines to stdout (build info, "Running 1 test suites...", etc.), use this pattern to safely parse only the JSON lines:

```bash
jq -R 'fromjson? // empty'
```

This reads each line as a raw string (`-R`), tries to parse it as JSON (`fromjson?` — the `?` silently skips failures), and discards any leftovers (`// empty`).

### Examples

**Pretty-print all events:**

```bash
cabal test convex-testing-interface-test \
  --test-options="--streaming-json" 2>/dev/null \
  | jq -R 'fromjson? // empty'
```

**List the test tree (discovery only):**

```bash
cabal test convex-testing-interface-test \
  --test-options="--list-tests-json" 2>/dev/null \
  | jq -R 'fromjson? // empty | .tests[] | {id, name, path}'
```

**Show only failures:**

```bash
cabal test convex-testing-interface-test \
  --test-options="--streaming-json" 2>/dev/null \
  | jq -R 'fromjson? // empty | select(.event == "test_done" and .success == false)'
```

**Extract test names and durations as a table:**

```bash
cabal test convex-testing-interface-test \
  --test-options="--streaming-json" 2>/dev/null \
  | jq -r -R 'fromjson? // empty | select(.event == "test_done") | [.id, .duration, .description] | @tsv'
```

**Get the final summary:**

```bash
cabal test convex-testing-interface-test \
  --test-options="--streaming-json" 2>/dev/null \
  | jq -R 'fromjson? // empty | select(.event == "suite_done")'
```

**Count tests per top-level group:**

```bash
cabal test convex-testing-interface-test \
  --test-options="--list-tests-json" 2>/dev/null \
  | jq -R 'fromjson? // empty | .tests | group_by(.path[0]) | map({group: .[0].path[0], count: length})'
```

**Filter discovery by path:**

```bash
cabal test convex-testing-interface-test \
  --test-options="--list-tests-json -p 'ping-pong'" 2>/dev/null \
  | jq -R 'fromjson? // empty | .tests[] | {id, name, path}'
```

**Pick IDs, then run only those tests:**

```bash
# Discover IDs
cabal test convex-testing-interface-test \
  --test-options="--list-tests-json" 2>/dev/null \
  | jq -r -R 'fromjson? // empty | .tests[] | "\(.id)\t\(.name)"'

# Run selected IDs
cabal test convex-testing-interface-test \
  --test-options="--test-id 0,3"
```

**Stream only selected test IDs:**

```bash
cabal test convex-testing-interface-test \
  --test-options="--streaming-json --test-id 0,3" 2>/dev/null \
  | jq -R 'fromjson? // empty'
```

## JSON Schema

A [JSON Schema (draft 2020-12)](https://json-schema.org/specification) describing every NDJSON event emitted by the streaming reporter lives at `schema/streaming-events.schema.json` in this package. Use it for VS Code extension type generation, payload validation, or as machine-readable documentation of the event format.

### Regenerating the schema

After modifying any `ToJSON` instance on streaming or trace types, regenerate the schema:

```bash
cabal run --project-file=cabal.project.schema-gen gen-schema \
  > src/tasty-streaming/schema/streaming-events.schema.json
```

You need to regenerate whenever you change types in:

- `Convex.TestingInterface.Trace`
- `Convex.ThreatModel.TxModifier`
- `Convex.Tasty.Streaming.Types`
- `Convex.Tasty.Streaming.TMSummary`

### How it works

The `convex-schema-gen` package (in `src/schema-gen/`) defines `ToSchema` orphan instances (from `openapi3`) for all serialized types and converts them to JSON Schema. The `openapi3` dependency is quarantined in that package — it is never pulled into any library that users consume. A separate project file (`cabal.project.schema-gen`) includes `convex-schema-gen` without affecting the main Nix build.

## API Reference

| Export                     | Type         | Description                                                          |
|----------------------------|--------------|----------------------------------------------------------------------|
| `defaultMainStreaming`     | `TestTree -> IO ()` | Drop-in replacement for `defaultMain` with streaming support   |
| `defaultMainStreamingWithIngredients` | `[Ingredient] -> TestTree -> IO ()` | Same as `defaultMainStreaming`, but prepends custom ingredients before streaming defaults |
| `streamingJsonReporter`    | `Ingredient` | The `--streaming-json` reporter (real-time NDJSON during test runs)   |
| `listTestsJsonIngredient`  | `Ingredient` | The `--list-tests-json` manager (test discovery without execution)   |
| `streamingIngredients`     | `[Ingredient]` | All ingredients combined (listing + JSON discovery + streaming + console) |
