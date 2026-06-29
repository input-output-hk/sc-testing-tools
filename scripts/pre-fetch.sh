#!/usr/bin/env bash
set -euo pipefail

# Resolve this script's own directory so the wrapper works from any CWD.
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

SUITES_TOOL="$HERE/list-test-suites/list-test-suites.sh"
TESTS_TOOL="$HERE/list-tests/list-tests.js"

print_help() {
  cat <<'EOF'
Usage: ./scripts/pre-fetch.sh <command> [options]
       ./scripts/pre-fetch.sh --check-health

Commands:
  install            Install list-tests Node dependencies (npm, no native build)
  list-test-suites   Discover cabal test suites (no compile)
  list-tests         Discover the tasty test tree inside .hs files (no compile)

Options:
  --check-health  Run BOTH tools' own dependency health checks and print an
                  aggregated verdict. Exits non-zero if either tool reports a
                  missing required dependency, else 0.

Run './scripts/pre-fetch.sh <command> --help' for command-specific options.
EOF
}

# Aggregated health check: run each tool's own --check-health, capture exit
# codes, and report a combined verdict.
run_check_health() {
  local rc_suites rc_tests

  echo "==================================================================="
  echo "  pre-fetch — aggregated dependency health check"
  echo "==================================================================="
  echo
  echo "------------------------------------------------------------------"
  echo "  list-test-suites (bash)"
  echo "------------------------------------------------------------------"
  bash "$SUITES_TOOL" --check-health && rc_suites=0 || rc_suites=$?

  echo
  echo "------------------------------------------------------------------"
  echo "  list-tests (node)"
  echo "------------------------------------------------------------------"
  node "$TESTS_TOOL" --check-health && rc_tests=0 || rc_tests=$?

  echo
  echo "==================================================================="
  echo "  Aggregated verdict"
  echo "==================================================================="
  if [[ $rc_suites -eq 0 ]]; then
    echo "  list-test-suites : OK (all required deps present)"
  else
    echo "  list-test-suites : FAIL (missing required deps)"
  fi
  if [[ $rc_tests -eq 0 ]]; then
    echo "  list-tests       : OK (all required deps present)"
  else
    echo "  list-tests       : FAIL (missing required deps)"
  fi
  echo

  if [[ $rc_suites -eq 0 && $rc_tests -eq 0 ]]; then
    echo "All tools healthy: required dependencies present."
    return 0
  else
    echo "One or more tools are missing required dependencies."
    return 1
  fi
}

# Top-level help: no args, -h, or --help.
if [[ $# -eq 0 || "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  print_help
  exit 0
fi

# Aggregated health check (accept both the flag and a bare subcommand form).
if [[ "${1:-}" == "--check-health" || "${1:-}" == "check-health" ]]; then
  run_check_health
  exit $?
fi

cmd="$1"
shift

case "$cmd" in
  install)
    # Install the list-tests Node dependencies. --ignore-scripts is REQUIRED:
    # the tree-sitter-haskell package's native postinstall build fails / is
    # unnecessary — the prebuilt grammar wasm ships in the package regardless.
    echo "Installing list-tests dependencies (skipping native build scripts)..."
    exec npm install --ignore-scripts --prefix "$HERE/list-tests" "$@"
    ;;
  list-test-suites)
    exec bash "$SUITES_TOOL" "$@"
    ;;
  list-tests)
    exec node "$TESTS_TOOL" "$@"
    ;;
  *)
    echo "pre-fetch.sh: unknown command '$cmd'" >&2
    print_help >&2
    exit 2
    ;;
esac
