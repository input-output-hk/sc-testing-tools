# Changelog for `convex-pbt-cli`

All notable changes to `pbt-cli` are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/); releases are tagged
`pbt-cli-v<version>`.

## 0.1.0.0

First release.

### Added

- `pbt-cli suites` — no-compile discovery of cabal projects, packages and test
  suites, reporting which are sc-testing-tools compatible. A native Haskell
  port of `scripts/list-test-suites/list-test-suites.sh`, emitting the same
  JSON document. Prints one suite name per line by default, with `--json`,
  `--compact`, `--table` and `--tsv` for the fuller views and a
  `--compatible-only` filter.
- `pbt-cli run` — run every discovered suite or just the named ones. Console
  output by default, grouped into one `cabal test` call per project file;
  `--stream` renders the streaming events live and `--json` forwards them as
  NDJSON, each event tagged with the `suite` that produced it. Both event modes
  invoke cabal once per suite, so attribution is exact.
- `pbt-cli tests` — the authoritative test list for one suite: `id<TAB>path`
  lines by default, `--tree` for nested groups, `--json` for the suite's own
  payload.
- `pbt-cli threat-models` — the threat models a suite can run, one name per
  line or `--json`.
- `pbt-cli doctor` — dependency and repository health check.
- Custom test options as first-class flags: `-p`/`--pattern`, `--test-id`,
  `--threat-model-name`, and `--test-option` as a pass-through escape hatch.
- `--dry-run` on every command that shells out, printing the cabal invocation.
- A closed stdout (`pbt-cli suites | head`) exits quietly instead of reporting
  a failure.
- Release binaries for `linux-x64`, `linux-arm64` and `darwin-arm64`, built
  with no dependency on the Cardano stack or its system libraries. Intel macOS
  is not built for: it is considered obsolete in the Cardano ecosystem.
