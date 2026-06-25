# Test-discovery scripts

Fast, **no-compile** discovery of a Cardano/Haskell repo's test structure, for
editors and CI. Two tools, plus a small wrapper to drive them.

| Tool | What it does |
| --- | --- |
| [`list-test-suites/`](./list-test-suites/) | Discovers cabal projects, packages, and their test **suites** from `*.cabal` / `cabal.project*`. |
| [`list-tests/`](./list-tests/) | Statically extracts the **tasty test tree** inside `.hs` files (tree-sitter, no compilation). |

`pre-fetch.sh` is a thin dispatcher over both — the subcommand names match the
folders.

## Quick start

```sh
# One-time: install list-tests Node deps (skips an unnecessary native build)
./scripts/pre-fetch.sh install

# Check that all required dependencies are present (aggregated over both tools)
./scripts/pre-fetch.sh --check-health

# Discover test suites in the current repo
./scripts/pre-fetch.sh list-test-suites .

# Discover the test tree inside .hs files
./scripts/pre-fetch.sh list-tests .

# Just one suite, pretty-printed
./scripts/pre-fetch.sh list-tests . --suite convex-vesting-test | jq .
```

Every command forwards its remaining arguments straight to the underlying tool,
so `--help` works per command:

```sh
./scripts/pre-fetch.sh list-test-suites --help
./scripts/pre-fetch.sh list-tests --help
```

## Commands

| Command | Forwards to |
| --- | --- |
| `install` | `npm install --ignore-scripts` in `list-tests/` |
| `list-test-suites [args]` | [`list-test-suites/list-test-suites.sh`](./list-test-suites/) |
| `list-tests [args]` | [`list-tests/list-tests.js`](./list-tests/) |
| `--check-health` | runs both tools' health checks, aggregated verdict |

## More detail

This file is only an index. **For full usage, options, output format, JSON
schema, and design notes, read the README inside each command's folder:**

- **list-test-suites** → [`list-test-suites/README.md`](./list-test-suites/README.md)
- **list-tests** → [`list-tests/README.md`](./list-tests/README.md)
