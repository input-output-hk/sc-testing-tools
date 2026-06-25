#!/usr/bin/env bash
# test-list-test-suites.sh
#
# Independent, hermetic black-box test harness for scripts/list-test-suites/list-test-suites.sh
# (list-test-suites project discovery).  Does NOT compile anything; builds synthetic
# fixture repos in temp dirs.  Requires jq.
#
# Run:  bash scripts/list-test-suites/test-list-test-suites.sh
# Exit: 0 if all pass, 1 if any fail.

set -uo pipefail

# ---------------------------------------------------------------------------
# Config / preconditions
# ---------------------------------------------------------------------------
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT="$HERE/list-test-suites.sh"
REPO="$(cd "$HERE/../.." && pwd)"
# Golden TSV baseline. Prefer the stable copy committed inside the repo
# (scripts/list-test-suites/testdata/golden.tsv) so the harness passes on a fresh checkout
# without manual /tmp seeding. Fall back to the legacy /tmp location, and if
# neither exists, regenerate one at runtime from the current script so the
# harness is fully self-sufficient.
GOLDEN_TSV="$HERE/testdata/golden.tsv"
if [[ ! -f "$GOLDEN_TSV" ]]; then
  if [[ -f /tmp/issue64-test/golden.tsv ]]; then
    GOLDEN_TSV="/tmp/issue64-test/golden.tsv"
  else
    GOLDEN_TSV="$(mktemp)"
    (cd "$REPO" && bash "$SCRIPT" . --tsv 2>/dev/null) > "$GOLDEN_TSV"
  fi
fi

JQ="$(command -v jq || true)"
[[ -z "$JQ" && -x /home/horus/.nix-profile/bin/jq ]] && JQ=/home/horus/.nix-profile/bin/jq
if [[ -z "$JQ" ]]; then
  echo "FATAL: jq not found on PATH and not at /home/horus/.nix-profile/bin/jq" >&2
  exit 3
fi
if [[ ! -f "$SCRIPT" ]]; then
  echo "FATAL: script under test not found: $SCRIPT" >&2
  exit 3
fi

# Colors
if [[ -t 1 ]]; then
  C_GREEN=$'\033[32m'; C_RED=$'\033[31m'; C_YEL=$'\033[33m'; C_RST=$'\033[0m'
else
  C_GREEN=""; C_RED=""; C_YEL=""; C_RST=""
fi

PASS=0
FAIL=0
declare -a FAILURES=()

# ---------------------------------------------------------------------------
# Temp workspace + cleanup
# ---------------------------------------------------------------------------
TMPROOT="$(mktemp -d)"
cleanup() { rm -rf "$TMPROOT"; }
trap cleanup EXIT

# fresh fixture dir
fx() { local d; d="$(mktemp -d "$TMPROOT/fx.XXXXXX")"; echo "$d"; }

# Run the script under test. Captures stdout/stderr/exit into globals.
RUN_OUT=""; RUN_ERR=""; RUN_RC=0
run() {
  local _errf; _errf="$(mktemp "$TMPROOT/err.XXXXXX")"
  RUN_OUT="$(bash "$SCRIPT" "$@" 2>"$_errf")"
  RUN_RC=$?
  RUN_ERR="$(cat "$_errf")"
  rm -f "$_errf"
}

# run_in <fixture-dir> [extra-args...]
# Runs the tool from INSIDE the fixture dir with a RELATIVE ROOT (this exercises
# the ROOT="." code path that an absolute ROOT never hits). $SCRIPT is absolute,
# so the cd does not break locating it.
run_in() {
  local _dir="$1"; shift
  local _errf; _errf="$(mktemp "$TMPROOT/err.XXXXXX")"
  RUN_OUT="$(cd "$_dir" && bash "$SCRIPT" "$@" 2>"$_errf")"
  RUN_RC=$?
  RUN_ERR="$(cat "$_errf")"
  rm -f "$_errf"
}

# ---------------------------------------------------------------------------
# Assert framework
# ---------------------------------------------------------------------------
_pass() { PASS=$((PASS+1)); printf '%s  PASS%s %s\n' "$C_GREEN" "$C_RST" "$1"; }
_fail() {
  FAIL=$((FAIL+1))
  printf '%s  FAIL%s %s\n' "$C_RED" "$C_RST" "$1"
  [[ -n "${2:-}" ]] && printf '         %s\n' "$2"
  FAILURES+=("$1${2:+  ::  $2}")
}

assert_eq() { # id expected actual
  local id="$1" exp="$2" act="$3"
  if [[ "$exp" == "$act" ]]; then _pass "$id"
  else _fail "$id" "expected [$exp] got [$act]"; fi
}

assert_contains() { # id haystack needle
  local id="$1" hay="$2" needle="$3"
  if [[ "$hay" == *"$needle"* ]]; then _pass "$id"
  else _fail "$id" "string does not contain [$needle]; got: $(printf '%s' "$hay" | head -c 400)"; fi
}

assert_not_contains() { # id haystack needle
  local id="$1" hay="$2" needle="$3"
  if [[ "$hay" != *"$needle"* ]]; then _pass "$id"
  else _fail "$id" "string unexpectedly contains [$needle]"; fi
}

assert_exit_code() { # id expected actual
  local id="$1" exp="$2" act="$3"
  if [[ "$exp" == "$act" ]]; then _pass "$id"
  else _fail "$id" "expected exit $exp got $act"; fi
}

# assert_json id json filter expected  -> (json | filter) == expected
assert_json() {
  local id="$1" json="$2" filter="$3" exp="$4"
  local got
  got="$(printf '%s' "$json" | "$JQ" -r "$filter" 2>/dev/null)"
  local rc=$?
  if [[ $rc -ne 0 ]]; then _fail "$id" "jq filter failed: $filter"; return; fi
  if [[ "$got" == "$exp" ]]; then _pass "$id"
  else _fail "$id" "filter [$filter] expected [$exp] got [$got]"; fi
}

assert_valid_json() { # id json
  local id="$1" json="$2"
  if printf '%s' "$json" | "$JQ" . >/dev/null 2>&1; then _pass "$id"
  else _fail "$id" "output is not valid JSON"; fi
}

# ===========================================================================
# Section A: live-repo smoke tests
# ===========================================================================
echo "${C_YEL}== A. Live-repo smoke tests ==${C_RST}"

run "$REPO"
assert_exit_code "A1 default exits 0" 0 "$RUN_RC"
assert_valid_json "A1 default valid JSON" "$RUN_OUT"
A_JSON="$RUN_OUT"

# A2: --tsv byte-identical to golden.
# NOTE: golden.tsv was generated with ROOT="." from inside the repo, so it
# carries "./"-prefixed RELATIVE paths. We must reproduce that exact context
# (cwd=repo, ROOT="."); passing an absolute ROOT yields absolute paths.
if [[ -f "$GOLDEN_TSV" ]]; then
  A2_OUT="$(cd "$REPO" && bash "$SCRIPT" . --tsv 2>/dev/null)"
  if diff <(printf '%s\n' "$A2_OUT") "$GOLDEN_TSV" >/dev/null 2>&1; then
    _pass "A2 --tsv byte-identical to golden"
  else
    _fail "A2 --tsv byte-identical to golden" "diff vs $GOLDEN_TSV: $(diff <(printf '%s\n' "$A2_OUT") "$GOLDEN_TSV" | head -c 300)"
  fi
  # 7 STREAMING rows: tasty-streaming, 4 use-cases, testing-interface
  # (defaultMainTestingInterface -> streaming-capable), schema-gen-streaming.
  rows="$(printf '%s\n' "$A2_OUT" | grep -c 'STREAMING' || true)"
  assert_eq "A2 seven STREAMING rows" "7" "$rows"
else
  _fail "A2 --tsv byte-identical to golden" "golden file missing: $GOLDEN_TSV"
fi

# A3: projectFile list contains both
pfs="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].projectFile] | sort | join(",")')"
assert_contains "A3 has cabal.project" "$pfs" "cabal.project"
assert_contains "A3 has cabal.project.schema-gen" "$pfs" "cabal.project.schema-gen"

# A4: convex-testing-interface owned ONLY by the default project (reachable
# from cabal.project directly; NOT re-listed under the importing variant).
ti_owners="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[] | select(.packages[].name=="convex-testing-interface") | .projectFile] | sort | join(",")')"
assert_eq "A4 testing-interface only under default project" "cabal.project" "$ti_owners"

# A5: convex-schema-gen ONLY under schema-gen project, with its 2 test suites
# carrying the correct --project-file + discover flags (locks in the
# exclusively-non-default --project-file behaviour).
sg_owners="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[] | select(.packages[].name=="convex-schema-gen") | .projectFile] | sort | join(",")')"
assert_eq "A5 schema-gen only under schema-gen project" "cabal.project.schema-gen" "$sg_owners"

# Exactly 2 test suites.
sg_suite_count="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[] | select(.name=="convex-schema-gen") | .testSuites[]] | length')"
assert_eq "A5 schema-gen has 2 test suites" "2" "$sg_suite_count"

# Streaming suite: STREAMING -> all 3 commands, each carrying --project-file.
sg_stream_ep="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select(.name=="convex-schema-gen-streaming-test") | .entryPoint][0]')"
assert_eq "A5 schema-gen-streaming entryPoint STREAMING" "STREAMING" "$sg_stream_ep"
sg_stream_dc="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select(.name=="convex-schema-gen-streaming-test") | .discoverCommand][0]')"
assert_contains "A5 schema-gen-streaming discoverCommand has project-file" "$sg_stream_dc" "--project-file=cabal.project.schema-gen"
assert_contains "A5 schema-gen-streaming discoverCommand has list-tests-json" "$sg_stream_dc" "--test-options=--list-tests-json"
sg_stream_sc="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select(.name=="convex-schema-gen-streaming-test") | .streamTestsCommand][0]')"
assert_contains "A5 schema-gen-streaming streamTestsCommand has streaming-json" "$sg_stream_sc" "--test-options=--streaming-json"
assert_contains "A5 schema-gen-streaming streamTestsCommand has project-file" "$sg_stream_sc" "--project-file=cabal.project.schema-gen"
sg_stream_rc="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select(.name=="convex-schema-gen-streaming-test") | .runTestsCommand][0]')"
assert_contains "A5 schema-gen-streaming runTestsCommand has project-file" "$sg_stream_rc" "--project-file=cabal.project.schema-gen"
assert_not_contains "A5 schema-gen-streaming runTestsCommand no --test-options" "$sg_stream_rc" "--test-options"

# Plain suite: upstream -> only runTestsCommand; stream/discover are null.
sg_plain_ep="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select(.name=="convex-schema-gen-plain-test") | .entryPoint][0]')"
assert_eq "A5 schema-gen-plain entryPoint upstream" "upstream" "$sg_plain_ep"
assert_json "A5 schema-gen-plain discoverCommand null" "$A_JSON" '[.projects[].packages[].testSuites[] | select(.name=="convex-schema-gen-plain-test") | .discoverCommand][0]' "null"
assert_json "A5 schema-gen-plain streamTestsCommand null" "$A_JSON" '[.projects[].packages[].testSuites[] | select(.name=="convex-schema-gen-plain-test") | .streamTestsCommand][0]' "null"
sg_plain_rc="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select(.name=="convex-schema-gen-plain-test") | .runTestsCommand][0]')"
assert_contains "A5 schema-gen-plain runTestsCommand has project-file" "$sg_plain_rc" "--project-file=cabal.project.schema-gen"

# A6: orphans == []
assert_json "A6 orphans empty" "$A_JSON" '.orphans | length' "0"

# A7: every STREAMING suite discoverCommand ends with --test-options=--list-tests-json
bad_stream="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select(.entryPoint=="STREAMING") | select((.discoverCommand|endswith("--test-options=--list-tests-json"))|not)] | length')"
assert_eq "A7 STREAMING discoverCommand suffix" "0" "$bad_stream"

# A8: LOCK the testing-interface classification. Its main-is uses
# defaultMainTestingInterface (Convex.TestingInterface), which is
# streaming-capable -> MUST be STREAMING with all 3 commands. Guards against
# regressing the classifier to a false-negative ("unknown").
ti_ep="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select(.name=="convex-testing-interface-test") | .entryPoint][0]')"
assert_eq "A8 testing-interface-test entryPoint STREAMING" "STREAMING" "$ti_ep"
ti_stream="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select(.name=="convex-testing-interface-test") | .streamTestsCommand][0]')"
assert_contains "A8 testing-interface-test streamTestsCommand has streaming-json" "$ti_stream" "--test-options=--streaming-json"
ti_disc="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select(.name=="convex-testing-interface-test") | .discoverCommand][0]')"
assert_contains "A8 testing-interface-test discoverCommand has list-tests-json" "$ti_disc" "--test-options=--list-tests-json"

# A9: hsSourceDirs is emitted for EVERY suite, always as an array of strings.
# (Guards the new field's presence + type across the whole real repo output.)
a9_total="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[]] | length')"
a9_arrays="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select((.hsSourceDirs|type)=="array")] | length')"
assert_eq "A9 every suite has hsSourceDirs array" "$a9_total" "$a9_arrays"
a9_bad="$(printf '%s' "$A_JSON" | "$JQ" -r '[.projects[].packages[].testSuites[] | select((.hsSourceDirs|all(type=="string"))|not)] | length')"
assert_eq "A9 hsSourceDirs items all strings" "0" "$a9_bad"

# A9: spot-check known suites' hsSourceDirs values (relative, as written).
sg_stream_hsd="$(printf '%s' "$A_JSON" | "$JQ" -c '[.projects[].packages[].testSuites[] | select(.name=="convex-schema-gen-streaming-test") | .hsSourceDirs][0]')"
assert_eq "A9 schema-gen-streaming hsSourceDirs == [test-streaming]" '["test-streaming"]' "$sg_stream_hsd"
sg_plain_hsd="$(printf '%s' "$A_JSON" | "$JQ" -c '[.projects[].packages[].testSuites[] | select(.name=="convex-schema-gen-plain-test") | .hsSourceDirs][0]')"
assert_eq "A9 schema-gen-plain hsSourceDirs == [test-plain]" '["test-plain"]' "$sg_plain_hsd"
ti_hsd="$(printf '%s' "$A_JSON" | "$JQ" -c '[.projects[].packages[].testSuites[] | select(.name=="convex-testing-interface-test") | .hsSourceDirs][0]')"
assert_eq "A9 testing-interface hsSourceDirs == [test]" '["test"]' "$ti_hsd"
vest_hsd="$(printf '%s' "$A_JSON" | "$JQ" -c '[.projects[].packages[].testSuites[] | select(.name=="convex-vesting-test") | .hsSourceDirs][0]')"
assert_eq "A9 vesting hsSourceDirs == [test]" '["test"]' "$vest_hsd"

# ===========================================================================
# Section B: synthetic fixtures
# ===========================================================================
echo "${C_YEL}== B. Synthetic fixtures ==${C_RST}"

# --- B1 single pkg, streaming ---
d="$(fx)"; mkdir -p "$d/p"
cat > "$d/p/p.cabal" <<'EOF'
name: pkg-one
test-suite one-test
  main-is: Spec.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming undefined' > "$d/p/Spec.hs"
run "$d"
assert_json "B1 entryPoint STREAMING" "$RUN_OUT" '.projects[].packages[].testSuites[].entryPoint' "STREAMING"
b1dc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].discoverCommand')"
assert_contains "B1 discoverCommand has list-tests-json" "$b1dc" "--test-options=--list-tests-json"
b1sc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].streamTestsCommand')"
assert_contains "B1 streamTestsCommand has streaming-json" "$b1sc" "--test-options=--streaming-json"
b1rc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].runTestsCommand')"
assert_not_contains "B1 runTestsCommand no --test-options" "$b1rc" "--test-options"
# hs-source-dirs is "." in this fixture -> hsSourceDirs == ["."].
b1hsd="$(printf '%s' "$RUN_OUT" | "$JQ" -c '.projects[].packages[].testSuites[].hsSourceDirs')"
assert_eq "B1 hsSourceDirs == [.]" '["."]' "$b1hsd"
# --tsv: hs-source-dirs is the trailing column.
run --tsv "$d"
b1tsv_col="$(printf '%s' "$RUN_OUT" | awk -F'\t' 'NR==1{print $5}')"
assert_eq "B1 tsv trailing column is hs-source-dirs" "." "$b1tsv_col"

# --- B1b multiple hs-source-dirs: array order preserved + ';'-joined in TSV ---
d="$(fx)"; mkdir -p "$d/p/src" "$d/p/test"
cat > "$d/p/p.cabal" <<'EOF'
name: pkg-multi
test-suite multi-test
  main-is: Spec.hs
  hs-source-dirs: src/use-cases/test, test
EOF
echo 'main = defaultMainStreaming undefined' > "$d/p/test/Spec.hs"
run "$d"
assert_valid_json "B1b multi hs-source-dirs valid JSON" "$RUN_OUT"
b1bhsd="$(printf '%s' "$RUN_OUT" | "$JQ" -c '.projects[].packages[].testSuites[].hsSourceDirs')"
assert_eq "B1b hsSourceDirs preserves order" '["src/use-cases/test","test"]' "$b1bhsd"
run --tsv "$d"
b1btsv_col="$(printf '%s' "$RUN_OUT" | awk -F'\t' 'NR==1{print $5}')"
assert_eq "B1b tsv joins multiple dirs with ';'" "src/use-cases/test;test" "$b1btsv_col"

# --- B2 upstream defaultMain ---
d="$(fx)"; mkdir -p "$d/p"
cat > "$d/p/p.cabal" <<'EOF'
name: pkg-up
test-suite up-test
  main-is: Up.hs
  hs-source-dirs: .
EOF
printf 'import Test.Tasty\nmain = defaultMain tests\n' > "$d/p/Up.hs"
run "$d"
assert_json "B2 entryPoint upstream" "$RUN_OUT" '.projects[].packages[].testSuites[].entryPoint' "upstream"
# upstream -> no streaming-only ingredients: discover/stream are null.
assert_json "B2 discoverCommand null" "$RUN_OUT" '.projects[].packages[].testSuites[].discoverCommand' "null"
assert_json "B2 streamTestsCommand null" "$RUN_OUT" '.projects[].packages[].testSuites[].streamTestsCommand' "null"
b2rc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].runTestsCommand')"
assert_contains "B2 runTestsCommand is plain cabal test" "$b2rc" "cabal test up-test"
assert_not_contains "B2 runTestsCommand no --test-options" "$b2rc" "--test-options"

# --- B3 main-is file does not exist -> MISSING ---
d="$(fx)"; mkdir -p "$d/p"
cat > "$d/p/p.cabal" <<'EOF'
name: pkg-miss
test-suite miss-test
  main-is: Nope.hs
  hs-source-dirs: .
EOF
run "$d"
assert_json "B3 entryPoint MISSING" "$RUN_OUT" '.projects[].packages[].testSuites[].entryPoint' "MISSING"
assert_json "B3 discoverCommand null" "$RUN_OUT" '.projects[].packages[].testSuites[].discoverCommand' "null"
assert_json "B3 streamTestsCommand null" "$RUN_OUT" '.projects[].packages[].testSuites[].streamTestsCommand' "null"
b3rc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].runTestsCommand')"
assert_contains "B3 runTestsCommand present" "$b3rc" "cabal test miss-test"

# --- B4 main exists but calls neither -> unknown ---
d="$(fx)"; mkdir -p "$d/p"
cat > "$d/p/p.cabal" <<'EOF'
name: pkg-unk
test-suite unk-test
  main-is: Unk.hs
  hs-source-dirs: .
EOF
echo 'main = putStrLn "nothing here"' > "$d/p/Unk.hs"
run "$d"
assert_json "B4 entryPoint unknown" "$RUN_OUT" '.projects[].packages[].testSuites[].entryPoint' "unknown"
assert_json "B4 discoverCommand null" "$RUN_OUT" '.projects[].packages[].testSuites[].discoverCommand' "null"
assert_json "B4 streamTestsCommand null" "$RUN_OUT" '.projects[].packages[].testSuites[].streamTestsCommand' "null"
b4rc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].runTestsCommand')"
assert_contains "B4 runTestsCommand present" "$b4rc" "cabal test unk-test"

# --- B4b defaultMainTestingInterface (Convex.TestingInterface) -> STREAMING ---
# The testing-interface entry point is streaming-capable; classifier must label
# it STREAMING (not unknown) and emit all 3 commands.
d="$(fx)"; mkdir -p "$d/p"
cat > "$d/p/p.cabal" <<'EOF'
name: pkg-ti
test-suite ti-test
  main-is: TI.hs
  hs-source-dirs: .
EOF
printf 'import Convex.TestingInterface\nmain = defaultMainTestingInterface tests\n' > "$d/p/TI.hs"
run "$d"
assert_json "B4b entryPoint STREAMING" "$RUN_OUT" '.projects[].packages[].testSuites[].entryPoint' "STREAMING"
b4b_sc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].streamTestsCommand')"
assert_contains "B4b streamTestsCommand has streaming-json" "$b4b_sc" "--test-options=--streaming-json"
b4b_dc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].discoverCommand')"
assert_contains "B4b discoverCommand has list-tests-json" "$b4b_dc" "--test-options=--list-tests-json"
b4b_rc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].runTestsCommand')"
assert_not_contains "B4b runTestsCommand no --test-options" "$b4b_rc" "--test-options"

# --- B5 no cabal.project -> projectFile null, no orphans, no --project-file ---
d="$(fx)"; mkdir -p "$d/p"
cat > "$d/p/p.cabal" <<'EOF'
name: pkg-implicit
test-suite imp-test
  main-is: Spec.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming undefined' > "$d/p/Spec.hs"
run "$d"
assert_json "B5 single project projectFile null" "$RUN_OUT" '.projects[0].projectFile' "null"
assert_json "B5 only one project" "$RUN_OUT" '.projects | length' "1"
assert_json "B5 orphans empty" "$RUN_OUT" '.orphans | length' "0"
b5bc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].runTestsCommand')"
assert_not_contains "B5 runTestsCommand no --project-file" "$b5bc" "--project-file"

# --- B6 two projects, custom does NOT import default; exclusive ownership ---
d="$(fx)"; mkdir -p "$d/pkg-a" "$d/pkg-b"
cat > "$d/pkg-a/a.cabal" <<'EOF'
name: pkg-a
test-suite a-test
  main-is: A.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming x' > "$d/pkg-a/A.hs"
cat > "$d/pkg-b/b.cabal" <<'EOF'
name: pkg-b
test-suite b-test
  main-is: B.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming x' > "$d/pkg-b/B.hs"
printf 'packages:\n  pkg-a\n' > "$d/cabal.project"
printf 'packages:\n  pkg-b\n' > "$d/cabal.project.custom"
run "$d"
b6_a_bc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[] | select(.name=="pkg-a") | .testSuites[].runTestsCommand] | unique | join("|")')"
b6_b_bc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[] | select(.name=="pkg-b") | .testSuites[].runTestsCommand] | unique | join("|")')"
# pkg-b's fixture is STREAMING (defaultMainStreaming), so discoverCommand is
# non-null and also carries the flag; assert the always-present runTestsCommand.
b6_b_dc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[] | select(.name=="pkg-b") | .testSuites[].discoverCommand] | unique | join("|")')"
assert_not_contains "B6 pkg-a NO project-file flag" "$b6_a_bc" "--project-file"
assert_contains "B6 pkg-b runTestsCommand has flag" "$b6_b_bc" "--project-file=cabal.project.custom"
assert_contains "B6 pkg-b discoverCommand has flag" "$b6_b_dc" "--project-file=cabal.project.custom"

# --- B7 import resolution (variant imports default + adds extra) ---
d="$(fx)"; mkdir -p "$d/base1" "$d/base2" "$d/extra"
for p in base1 base2 extra; do
  cat > "$d/$p/$p.cabal" <<EOF
name: pkg-$p
test-suite $p-test
  main-is: M.hs
  hs-source-dirs: .
EOF
  echo 'main = defaultMainStreaming x' > "$d/$p/M.hs"
done
printf 'packages:\n  base1\n  base2\n' > "$d/cabal.project"
printf 'import: cabal.project\npackages:\n  extra\n' > "$d/cabal.project.variant"
run "$d"
# Ownership dedup: a base package reachable from the default project is owned
# there ONLY (not re-listed under the importing variant). Only the variant's
# UNIQUE package (extra) appears under the variant.
extra_owners="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[] | select(.packages[].name=="pkg-extra") | .projectFile] | sort | join(",")')"
base1_owners="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[] | select(.packages[].name=="pkg-base1") | .projectFile] | sort | join(",")')"
assert_eq "B7 imported base1 only under default" "cabal.project" "$base1_owners"
assert_json "B7 no orphans" "$RUN_OUT" '.orphans | length' "0"
# extra exclusively in variant -> only variant
assert_eq "B7 extra only under variant" "cabal.project.variant" "$extra_owners"

# --- B8 orphan detection ---
d="$(fx)"; mkdir -p "$d/pkg-a" "$d/pkg-orphan"
cat > "$d/pkg-a/a.cabal" <<'EOF'
name: pkg-a
test-suite a-test
  main-is: A.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming x' > "$d/pkg-a/A.hs"
cat > "$d/pkg-orphan/orph.cabal" <<'EOF'
name: pkg-orphan
test-suite orph-test
  main-is: O.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming x' > "$d/pkg-orphan/O.hs"
printf 'packages:\n  pkg-a\n' > "$d/cabal.project"
run "$d"
assert_json "B8 one orphan" "$RUN_OUT" '.orphans | length' "1"
assert_json "B8 orphan is orph.cabal" "$RUN_OUT" '.orphans[0].cabalFile' "pkg-orphan/orph.cabal"

# --- B9 glob packages: */*.cabal ---
d="$(fx)"; mkdir -p "$d/x" "$d/y"
for p in x y; do
  cat > "$d/$p/$p.cabal" <<EOF
name: pkg-$p
test-suite $p-test
  main-is: M.hs
  hs-source-dirs: .
EOF
  echo 'main = defaultMainStreaming z' > "$d/$p/M.hs"
done
printf 'packages: */*.cabal\n' > "$d/cabal.project"
run "$d"
b9names="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[].name] | sort | join(",")')"
assert_eq "B9 glob resolves both packages" "pkg-x,pkg-y" "$b9names"
assert_json "B9 no orphans" "$RUN_OUT" '.orphans | length' "0"

# --- B10 multi-line indented packages block ---
d="$(fx)"; mkdir -p "$d/m1" "$d/m2" "$d/m3"
for p in m1 m2 m3; do
  cat > "$d/$p/$p.cabal" <<EOF
name: pkg-$p
test-suite $p-test
  main-is: M.hs
  hs-source-dirs: .
EOF
  echo 'main = defaultMainStreaming z' > "$d/$p/M.hs"
done
printf 'packages:\n  m1\n  m2\n  m3\n' > "$d/cabal.project"
run "$d"
b10names="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[].name] | sort | join(",")')"
assert_eq "B10 multiline block resolves all" "pkg-m1,pkg-m2,pkg-m3" "$b10names"
assert_json "B10 no orphans" "$RUN_OUT" '.orphans | length' "0"

# --- B11 package with library only (no test-suite) -> included, testSuites: [] ---
d="$(fx)"; mkdir -p "$d/libpkg" "$d/withtest"
cat > "$d/libpkg/lib.cabal" <<'EOF'
name: pkg-lib
library
  exposed-modules: Foo
  hs-source-dirs: .
EOF
cat > "$d/withtest/wt.cabal" <<'EOF'
name: pkg-wt
test-suite wt-test
  main-is: M.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming z' > "$d/withtest/M.hs"
printf 'packages:\n  libpkg\n  withtest\n' > "$d/cabal.project"
run "$d"
assert_exit_code "B11 exits 0 (a suite exists)" 0 "$RUN_RC"
lib_suites="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[] | select(.name=="pkg-lib") | .testSuites | length] | unique | join(",")')"
assert_eq "B11 library pkg testSuites empty" "0" "$lib_suites"
lib_present="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[].name] | index("pkg-lib") != null')"
assert_eq "B11 library pkg present" "true" "$lib_present"
assert_json "B11 library pkg not orphaned" "$RUN_OUT" '.orphans | length' "0"

# --- B12 bare-directory packages token: `packages:\n  .` (root single pkg) ---
# Regression: a directory token (esp. ".") must resolve to the .cabal file(s)
# directly inside that directory and NOT be flagged as an orphan.
d="$(fx)"
cat > "$d/root.cabal" <<'EOF'
name: pkg-root
test-suite root-test
  main-is: M.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming z' > "$d/M.hs"
printf 'packages:\n  .\n' > "$d/cabal.project"
run "$d"
assert_json "B12 one project" "$RUN_OUT" '.projects | length' "1"
assert_json "B12 projectFile cabal.project" "$RUN_OUT" '.projects[0].projectFile' "cabal.project"
assert_json "B12 one package" "$RUN_OUT" '.projects[0].packages | length' "1"
b12name="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[0].packages[0].name')"
assert_eq "B12 package is pkg-root" "pkg-root" "$b12name"
assert_json "B12 no orphans" "$RUN_OUT" '.orphans | length' "0"
b12rc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].runTestsCommand')"
assert_eq "B12 runTestsCommand plain (default project)" "cabal test root-test" "$b12rc"
assert_not_contains "B12 runTestsCommand no --project-file" "$b12rc" "--project-file"

# --- B12b bare-directory token naming a SUBDIR: `packages:\n  sub` ---
# `sub` (no slash) and `sub/` (trailing slash) both mean the directory.
d="$(fx)"; mkdir -p "$d/sub"
cat > "$d/sub/foo.cabal" <<'EOF'
name: pkg-sub
test-suite sub-test
  main-is: M.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming z' > "$d/sub/M.hs"
printf 'packages:\n  sub/\n' > "$d/cabal.project"
run "$d"
b12bname="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[].name] | join(",")')"
assert_eq "B12b subdir token resolves" "pkg-sub" "$b12bname"
assert_json "B12b no orphans" "$RUN_OUT" '.orphans | length' "0"

# --- B13 RELATIVE ROOT: `packages:\n  .` exercised with ROOT="." ---
# Regression for the common real-world invocation (cd into repo, no ROOT arg).
# With ROOT="." the glob match is "././pkg.cabal"; norm_cabal_path must strip
# ALL leading "./" so the ownership key matches and it is NOT orphaned.
d="$(fx)"
cat > "$d/root.cabal" <<'EOF'
name: pkg-r13
test-suite r13-test
  main-is: M.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming z' > "$d/M.hs"
printf 'packages:\n  .\n' > "$d/cabal.project"

# B13 assertions, applied to both invocation styles (default ROOT and "." ROOT).
b13_assert() {
  local tag="$1"
  assert_json "B13 $tag one package" "$RUN_OUT" '.projects[0].packages | length' "1"
  local nm; nm="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[0].packages[0].name')"
  assert_eq "B13 $tag package is pkg-r13" "pkg-r13" "$nm"
  assert_json "B13 $tag no orphans" "$RUN_OUT" '.orphans | length' "0"
  assert_json "B13 $tag projectFile cabal.project" "$RUN_OUT" '.projects[0].projectFile' "cabal.project"
  local rc; rc="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].testSuites[].runTestsCommand')"
  assert_not_contains "B13 $tag runTestsCommand no --project-file" "$rc" "--project-file"
  assert_eq "B13 $tag empty stderr (no orphan warning)" "" "$RUN_ERR"
}

run_in "$d"          # default ROOT (no arg) from inside the fixture
b13_assert "default-root"
run_in "$d" .        # explicit ROOT="."
b13_assert "dot-root"

# --- B13b RELATIVE ROOT: bare subdir token `packages:\n  sub` with ROOT="." ---
d="$(fx)"; mkdir -p "$d/sub"
cat > "$d/sub/foo.cabal" <<'EOF'
name: pkg-r13b
test-suite r13b-test
  main-is: M.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming z' > "$d/sub/M.hs"
printf 'packages:\n  sub\n' > "$d/cabal.project"
run_in "$d"
b13b_name="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[].name] | join(",")')"
assert_eq "B13b subdir token resolves (relative ROOT)" "pkg-r13b" "$b13b_name"
assert_json "B13b no orphans (relative ROOT)" "$RUN_OUT" '.orphans | length' "0"

# ===========================================================================
# Section C: CLI / error handling
# ===========================================================================
echo "${C_YEL}== C. CLI / error handling ==${C_RST}"

# C1 empty dir -> exit 1, stderr mentions no test-suites
d="$(fx)"
run "$d"
assert_exit_code "C1 empty dir exit 1" 1 "$RUN_RC"
assert_contains "C1 stderr mentions no test-suite" "$RUN_ERR" "test-suite"

# C2 unknown flag -> exit 2
run "$REPO" --bogus
assert_exit_code "C2 unknown flag exit 2" 2 "$RUN_RC"

# C3 flag order independence (use a synthetic deterministic fixture)
d="$(fx)"; mkdir -p "$d/p"
cat > "$d/p/p.cabal" <<'EOF'
name: pkg-order
test-suite o-test
  main-is: M.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming z' > "$d/p/M.hs"
run --tsv "$d"; o1="$RUN_OUT"; rc1="$RUN_RC"
run "$d" --tsv; o2="$RUN_OUT"; rc2="$RUN_RC"
assert_eq "C3 tsv order-independent output" "$o1" "$o2"
assert_eq "C3 tsv order-independent rc" "$rc1" "$rc2"

# C4 nonexistent ROOT -> non-zero, sane (document behavior)
run "$TMPROOT/does-not-exist-xyz"
if [[ "$RUN_RC" -ne 0 ]]; then
  _pass "C4 nonexistent ROOT non-zero exit (rc=$RUN_RC)"
else
  _fail "C4 nonexistent ROOT non-zero exit" "got rc=0"
fi
# document: should not spew a bash stack trace / 'line NN:' errors
if printf '%s' "$RUN_ERR" | grep -qE 'line [0-9]+:|unbound variable|syntax error'; then
  _fail "C4 no bash stack spew" "stderr: $(printf '%s' "$RUN_ERR" | head -c 200)"
else
  _pass "C4 no bash stack spew (rc=$RUN_RC, msg=$(printf '%s' "$RUN_ERR" | head -c 80))"
fi

# ===========================================================================
# Section D: robustness probes (may reveal bugs; report, do not fix script)
# ===========================================================================
echo "${C_YEL}== D. Robustness probes ==${C_RST}"

# D1 path containing a space -> JSON must be valid + escaped
d="$(fx)"; mkdir -p "$d/my pkg"
cat > "$d/my pkg/foo.cabal" <<'EOF'
name: pkg-space
test-suite space-test
  main-is: Spec.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming undefined' > "$d/my pkg/Spec.hs"
run "$d"
assert_valid_json "D1 space-in-dir valid JSON" "$RUN_OUT"
d1pdir="$(printf '%s' "$RUN_OUT" | "$JQ" -r '.projects[].packages[].packageDir' 2>/dev/null)"
assert_eq "D1 packageDir preserves space" "my pkg" "$d1pdir"

# D2 mixed-case + trailing whitespace header
d="$(fx)"; mkdir -p "$d/p"
printf 'name: pkg-case\nTest-Suite  weird-test  \n  main-is: M.hs\n  hs-source-dirs: .\n' > "$d/p/p.cabal"
echo 'main = defaultMainStreaming z' > "$d/p/M.hs"
run "$d"
assert_valid_json "D2 mixed-case header valid JSON" "$RUN_OUT"
d2name="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[].testSuites[].name] | join(",")' 2>/dev/null)"
# Document: trailing whitespace should be trimmed -> "weird-test"
assert_eq "D2 mixed-case header parsed & name trimmed" "weird-test" "$d2name"

# D3 source-repository-package subdirs must NOT be local packages/orphans
d="$(fx)"; mkdir -p "$d/p"
cat > "$d/p/p.cabal" <<'EOF'
name: pkg-srp
test-suite srp-test
  main-is: M.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming z' > "$d/p/M.hs"
cat > "$d/cabal.project" <<'EOF'
packages:
  p

source-repository-package
  type: git
  location: https://example.com/x.git
  tag: deadbeef
  subdir:
    src/base
    src/foo
EOF
run "$d"
assert_json "D3 srp no orphans" "$RUN_OUT" '.orphans | length' "0"
d3pkgs="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[].name] | sort | join(",")')"
assert_eq "D3 only local package present" "pkg-srp" "$d3pkgs"

# ===========================================================================
# Section S: JSON Schema conformance
# (validates the tool's REAL output against scripts/list-test-suites/list-test-suites.schema.json
#  so the schema cannot silently drift from the tool)
# ===========================================================================
echo "${C_YEL}== S. JSON Schema conformance ==${C_RST}"

SCHEMA="$HERE/list-test-suites.schema.json"

# Pick a real JSON Schema validator if one is on PATH; else "" -> jq fallback.
SCHEMA_VALIDATOR=""
if command -v check-jsonschema >/dev/null 2>&1; then
  SCHEMA_VALIDATOR="check-jsonschema"
elif command -v ajv >/dev/null 2>&1; then
  SCHEMA_VALIDATOR="ajv"
elif python3 -c 'import jsonschema' >/dev/null 2>&1; then
  SCHEMA_VALIDATOR="python-jsonschema"
fi

# jq-based structural conformance: assert the KEY invariants on a JSON document.
# Echoes "OK" or "FAIL: <reasons>".
SCHEMA_JQ_FILTER='
def chk:
  (if (has("root") and has("projects") and has("orphans")) then empty else "missing top-level key" end),
  (if (.root|type)=="string" then empty else "root not string" end),
  (if (.projects|type)=="array" then empty else "projects not array" end),
  (if (.orphans|type)=="array" then empty else "orphans not array" end),
  (.orphans[] | if (has("cabalFile") and has("packageDir")) then empty else "orphan missing keys" end),
  ( .projects[].packages[].testSuites[] |
    . as $s |
    (if (["name","mainIs","entryPoint","runTestsCommand","streamTestsCommand","discoverCommand","hsSourceDirs"]|all(. as $k| $s|has($k))) then empty else "suite missing field(s)" end),
    (if (.hsSourceDirs|type)=="array" and (.hsSourceDirs|all(type=="string")) then empty else "hsSourceDirs not array of strings for \(.name)" end),
    (if (.entryPoint|IN("STREAMING","upstream","unknown","MISSING")) then empty else "bad entryPoint \(.entryPoint)" end),
    (if (.runTestsCommand|type)=="string" then empty else "runTestsCommand not string" end),
    (if .entryPoint=="STREAMING"
       then (if (.streamTestsCommand|type)=="string" and (.streamTestsCommand|test("--streaming-json")) and (.discoverCommand|type)=="string" and (.discoverCommand|test("--list-tests-json")) then empty else "STREAMING cmd invariant violated for \(.name)" end)
       else (if (.streamTestsCommand==null) and (.discoverCommand==null) then empty else "non-STREAMING null invariant violated for \(.name)" end)
     end)
  );
[chk] | if length==0 then "OK" else "FAIL: "+(join("; ")) end
'

# validate_schema <test-id> <json-string>
# Uses the real validator if available, otherwise the jq structural fallback.
validate_schema() {
  local id="$1" json="$2"
  if [[ -n "$SCHEMA_VALIDATOR" ]]; then
    local tf; tf="$(mktemp "$TMPROOT/doc.XXXXXX.json")"
    printf '%s' "$json" > "$tf"
    local rc=0 out=""
    case "$SCHEMA_VALIDATOR" in
      check-jsonschema)   out="$(check-jsonschema --schemafile "$SCHEMA" "$tf" 2>&1)"; rc=$? ;;
      ajv)                out="$(ajv validate -s "$SCHEMA" -d "$tf" 2>&1)"; rc=$? ;;
      python-jsonschema)  out="$(python3 -c 'import sys,json,jsonschema; jsonschema.validate(json.load(open(sys.argv[1])), json.load(open(sys.argv[2])))' "$tf" "$SCHEMA" 2>&1)"; rc=$? ;;
    esac
    rm -f "$tf"
    if [[ $rc -eq 0 ]]; then _pass "$id (via $SCHEMA_VALIDATOR)"; else _fail "$id (via $SCHEMA_VALIDATOR)" "$(printf '%s' "$out" | head -c 200)"; fi
  else
    local res; res="$(printf '%s' "$json" | "$JQ" -r "$SCHEMA_JQ_FILTER" 2>&1)"
    if [[ "$res" == "OK" ]]; then _pass "$id (jq structural)"; else _fail "$id (jq structural)" "$res"; fi
  fi
}

# S1 schema file itself is valid JSON.
if "$JQ" -e . "$SCHEMA" >/dev/null 2>&1; then
  _pass "S1 schema file is valid JSON"
else
  _fail "S1 schema file is valid JSON" "jq could not parse $SCHEMA"
fi

# S1 embedded example validates.
schema_example="$("$JQ" -c '.examples[0]' "$SCHEMA" 2>/dev/null)"
validate_schema "S1 embedded example validates" "$schema_example"

# S1 this repo's REAL output validates (exercises the STREAMING branch).
run "$REPO"
validate_schema "S1 this-repo output validates" "$RUN_OUT"

# S1 synthetic fixture with a NON-STREAMING (upstream) suite validates
# (exercises the else-branch of the null-iff-STREAMING invariant).
d="$(fx)"; mkdir -p "$d/up" "$d/st"
cat > "$d/up/up.cabal" <<'EOF'
name: pkg-s-up
test-suite up-test
  main-is: Up.hs
  hs-source-dirs: .
EOF
printf 'import Test.Tasty\nmain = defaultMain tests\n' > "$d/up/Up.hs"
cat > "$d/st/st.cabal" <<'EOF'
name: pkg-s-st
test-suite st-test
  main-is: St.hs
  hs-source-dirs: .
EOF
echo 'main = defaultMainStreaming z' > "$d/st/St.hs"
printf 'packages:\n  up\n  st\n' > "$d/cabal.project"
run "$d"
validate_schema "S1 non-STREAMING fixture validates" "$RUN_OUT"
# sanity: confirm the fixture really has both branches present
s1eps="$(printf '%s' "$RUN_OUT" | "$JQ" -r '[.projects[].packages[].testSuites[].entryPoint] | sort | join(",")')"
assert_eq "S1 fixture has upstream+STREAMING suites" "STREAMING,upstream" "$s1eps"

# ===========================================================================
# Summary
# ===========================================================================
echo
echo "${C_YEL}==================== SUMMARY ====================${C_RST}"
TOTAL=$((PASS+FAIL))
if [[ $FAIL -eq 0 ]]; then
  echo "${C_GREEN}ALL ${PASS}/${TOTAL} PASSED${C_RST}"
else
  echo "${C_RED}${PASS}/${TOTAL} passed, ${FAIL} FAILED${C_RST}"
  echo "Failures:"
  for f in "${FAILURES[@]}"; do
    echo "  ${C_RED}-${C_RST} $f"
  done
fi
echo "${C_YEL}================================================${C_RST}"

[[ $FAIL -eq 0 ]]
