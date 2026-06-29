#!/usr/bin/env bash
# test-list-tests.sh
#
# Black-box test harness for scripts/list-tests/list-tests.js (static, no-compile
# tasty test-tree discovery). Modelled on scripts/list-test-suites/test-list-test-suites.sh
# (the list-test-suites harness): helper assert functions, PASS/FAIL counts, non-zero exit on any
# failure. Does NOT compile any Haskell; it runs the Node tool against the real
# repo and diffs three deterministic suites against committed golden trees.
#
# Run:  bash scripts/list-tests/test-list-tests.sh
# Exit: 0 if all pass, 1 if any fail.
#
# ---------------------------------------------------------------------------
# ROOT NORMALIZATION
# ---------------------------------------------------------------------------
# The tool emits an absolute, machine-specific `root` path. To keep the golden
# trees portable, the committed goldens store `root` already normalized to the
# literal string "<ROOT>". Before diffing, this harness applies the SAME
# normalization to live tool output (jq: `.root = "<ROOT>"`). Every other path
# in the output (grammar, entryFile, per-node file) is relative to root and
# therefore already stable across machines.
#
# ---------------------------------------------------------------------------
# SCHEMA CONFORMANCE
# ---------------------------------------------------------------------------
# If a real JSON Schema validator is on PATH (check-jsonschema / ajv /
# python-jsonschema) it is used to validate the golden trees against
# list-tests.schema.json. Otherwise the harness falls back to a jq structural
# conformance walk (exactly like the list-test-suites harness) and notes that no formal
# validator is installed.

set -uo pipefail

# ---------------------------------------------------------------------------
# Config / preconditions
# ---------------------------------------------------------------------------
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOOL="$HERE/list-tests.js"
SCHEMA="$HERE/list-tests.schema.json"
TESTDATA="$HERE/testdata"
REPO="$(cd "$HERE/../.." && pwd)"

JQ="$(command -v jq || true)"
[[ -z "$JQ" && -x /home/horus/.nix-profile/bin/jq ]] && JQ=/home/horus/.nix-profile/bin/jq
if [[ -z "$JQ" ]]; then
  echo "FATAL: jq not found on PATH and not at /home/horus/.nix-profile/bin/jq" >&2
  exit 3
fi

NODE="$(command -v node || true)"
if [[ -z "$NODE" ]]; then
  echo "FATAL: node not found on PATH" >&2
  exit 3
fi
if [[ ! -f "$TOOL" ]]; then
  echo "FATAL: tool under test not found: $TOOL" >&2
  exit 3
fi
if [[ ! -f "$SCHEMA" ]]; then
  echo "FATAL: schema not found: $SCHEMA" >&2
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

assert_valid_json() { # id json
  local id="$1" json="$2"
  if printf '%s' "$json" | "$JQ" . >/dev/null 2>&1; then _pass "$id"
  else _fail "$id" "output is not valid JSON"; fi
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

# ---------------------------------------------------------------------------
# Run the tool for one suite, normalize root, capture stdout/exit.
# RUN_OUT / RUN_RC are set as globals.
# ---------------------------------------------------------------------------
RUN_OUT=""; RUN_RC=0
run_suite() { # <suite-name>
  local suite="$1"
  local errf; errf="$(mktemp "$TMPROOT/err.XXXXXX")"
  RUN_OUT="$( "$NODE" "$TOOL" "$REPO" --suite "$suite" 2>"$errf" | "$JQ" '.root = "<ROOT>"' )"
  RUN_RC="${PIPESTATUS[0]}"
  RUN_ERR="$(cat "$errf")"
  rm -f "$errf"
}

# ===========================================================================
# Section A: smoke — tool runs and emits valid JSON
# ===========================================================================
echo "${C_YEL}== A. Smoke: tool runs, emits valid JSON ==${C_RST}"

# A0: --help works and exits 0.
help_out="$( "$NODE" "$TOOL" --help 2>/dev/null )"; help_rc=$?
assert_eq "A0 --help exits 0" "0" "$help_rc"
assert_contains "A0 --help mentions usage" "$help_out" "USAGE"

# A1: full run (no --suite) over the whole repo emits valid JSON with required keys.
ALL_OUT="$( "$NODE" "$TOOL" "$REPO" 2>/dev/null | "$JQ" '.root = "<ROOT>"' )"
assert_valid_json "A1 full-run valid JSON" "$ALL_OUT"
assert_json "A1 root present (normalized)" "$ALL_OUT" '.root' "<ROOT>"
assert_json "A1 suites is array" "$ALL_OUT" '(.suites|type)' "array"
a1_nsuites="$(printf '%s' "$ALL_OUT" | "$JQ" -r '.suites | length')"
# The repo has 8 suites (see scripts/testdata/golden.tsv); require at least the 3 deterministic ones.
if [[ "$a1_nsuites" -ge 3 ]]; then _pass "A1 has >=3 suites (got $a1_nsuites)"
else _fail "A1 has >=3 suites" "got $a1_nsuites"; fi

# ===========================================================================
# Section B: structural conformance (recursive jq walk over every node)
# ===========================================================================
echo "${C_YEL}== B. Structural conformance ==${C_RST}"

# Collect every node object in the full output via `[.. | objects | select(has("source"))]`.
# B1: every node.kind in {group,test,placeholder}.
b1_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(has("source")) | select((.kind|IN("group","test","placeholder"))|not)] | length')"
assert_eq "B1 every node.kind in {group,test,placeholder}" "0" "$b1_bad"

# B2: every node.source in {parsed,synthesized,dynamic}.
b2_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(has("source")) | select((.source|IN("parsed","synthesized","dynamic"))|not)] | length')"
assert_eq "B2 every node.source in {parsed,synthesized,dynamic}" "0" "$b2_bad"

# B3: no node carries the removed `authoritative` field (R0).
b3_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(has("source")) | select(has("authoritative"))] | length')"
assert_eq "B3 no node has the removed authoritative field" "0" "$b3_bad"

# B4: every node has required fields kind, label, source.
b4_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(has("source")) | select((has("kind") and has("label") and has("source"))|not)] | length')"
assert_eq "B4 every node has kind/label/source" "0" "$b4_bad"

# B9: when present, role is one of the enum values.
b9_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(has("role")) | select((.role|IN("positive","negative","threat-models-group","expected-vulnerabilities-group","threat-model"))|not)] | length')"
assert_eq "B9 every node.role in enum" "0" "$b9_bad"

# B10: when present, testingInterface is boolean true.
b10_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(has("testingInterface")) | select(.testingInterface != true)] | length')"
assert_eq "B10 testingInterface (when present) == true" "0" "$b10_bad"

# B11: when present, pendingExpansion is a boolean (true or false).
b11_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(has("pendingExpansion")) | select((.pendingExpansion|type) != "boolean")] | length')"
assert_eq "B11 pendingExpansion (when present) is boolean" "0" "$b11_bad"

# B11a: NO synthesized TestingInterface node is pendingExpansion (fully known).
b11a_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(.testingInterface==true) | select(.pendingExpansion != false)] | length')"
assert_eq "B11a every synthesized TestingInterface node pendingExpansion==false" "0" "$b11a_bad"

# B12: testingInterface flag only appears on synthesized nodes.
b12_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(.testingInterface==true) | select(.source != "synthesized")] | length')"
assert_eq "B12 testingInterface only on synthesized nodes" "0" "$b12_bad"

# B13: every dynamic placeholder is pendingExpansion (R3).
b13_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(.source=="dynamic") | select(.pendingExpansion != true)] | length')"
assert_eq "B13 every dynamic node is pendingExpansion" "0" "$b13_bad"

# B5: label is string or null (never another type) on every node.
b5_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(has("source")) | select((.label|type) as $t | ($t=="string" or $t=="null")|not)] | length')"
assert_eq "B5 every node.label is string|null" "0" "$b5_bad"

# B6: `children` is a GROUP-ONLY property. When present it must be an array;
# leaves (kind "test"/"placeholder") MUST NOT carry a `children` key at all
# (its absence is the leaf signal), and every group MUST carry one.
b6_bad_children="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(has("source")) | select(has("children")) | select((.children|type)!="array")] | length')"
assert_eq "B6 children (when present) is array" "0" "$b6_bad_children"
# B6a: NO leaf node (kind "test" or "placeholder") has a `children` key.
b6_leaf_with_children="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(has("source")) | select(.kind=="test" or .kind=="placeholder") | select(has("children"))] | length')"
assert_eq "B6a test/placeholder leaves have NO children key" "0" "$b6_leaf_with_children"
# B6b: EVERY group node HAS a `children` key.
b6_group_without_children="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.. | objects | select(has("source")) | select(.kind=="group") | select(has("children")|not)] | length')"
assert_eq "B6b group nodes DO have a children key" "0" "$b6_group_without_children"

# B7: entryPoint of every suite is one of the 4 enum values.
b7_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.suites[] | select((.entryPoint|IN("STREAMING","upstream","unknown","MISSING"))|not)] | length')"
assert_eq "B7 every suite.entryPoint in enum" "0" "$b7_bad"

# B8: every suite has required keys.
b8_bad="$(printf '%s' "$ALL_OUT" | "$JQ" -r '[.suites[] | select((["suite","package","entryFile","entryPoint","tree"]|all(. as $k|true)) and ([.] | all(has("suite") and has("package") and has("entryFile") and has("entryPoint") and has("tree")))|not)] | length')"
assert_eq "B8 every suite has required keys" "0" "$b8_bad"

# ===========================================================================
# Section C: golden tree comparison (3 deterministic suites)
# ===========================================================================
echo "${C_YEL}== C. Golden tree comparison ==${C_RST}"

# golden_check <id> <suite-name> <golden-file>
golden_check() {
  local id="$1" suite="$2" gold="$3"
  if [[ ! -f "$gold" ]]; then
    _fail "$id" "golden file missing: $gold"
    return
  fi
  run_suite "$suite"
  if [[ "$RUN_RC" -ne 0 ]]; then
    _fail "$id" "tool exited $RUN_RC for suite $suite: $(printf '%s' "$RUN_ERR" | head -c 200)"
    return
  fi
  # Normalize BOTH sides through jq -S (sorted keys) so formatting/key-order
  # differences never produce false diffs; only structural/value diffs matter.
  local live_norm gold_norm
  live_norm="$(printf '%s' "$RUN_OUT" | "$JQ" -S .)"
  gold_norm="$("$JQ" -S . "$gold")"
  if diff <(printf '%s\n' "$gold_norm") <(printf '%s\n' "$live_norm") >/dev/null 2>&1; then
    _pass "$id"
  else
    _fail "$id" "diff (golden < / live >):
$(diff <(printf '%s\n' "$gold_norm") <(printf '%s\n' "$live_norm") | head -c 800)"
  fi
}

golden_check "C1 vesting golden matches"          "convex-vesting-test"               "$TESTDATA/golden-vesting.json"
golden_check "C2 schema-gen-streaming golden"     "convex-schema-gen-streaming-test"  "$TESTDATA/golden-schema-gen-streaming.json"
golden_check "C3 schema-gen-plain golden"         "convex-schema-gen-plain-test"      "$TESTDATA/golden-schema-gen-plain.json"
golden_check "C4 testing-interface golden"        "convex-testing-interface-test"     "$TESTDATA/golden-testing-interface.json"

# ===========================================================================
# Section D: value spot-checks
# ===========================================================================
echo "${C_YEL}== D. Value spot-checks ==${C_RST}"

# --- D-vesting ---
run_suite "convex-vesting-test"
V="$RUN_OUT"
assert_json "D vesting entryPoint STREAMING" "$V" '.suites[0].entryPoint' "STREAMING"
assert_json "D vesting top label" "$V" '.suites[0].tree.label' "vesting tests"
assert_json "D vesting top is group" "$V" '.suites[0].tree.kind' "group"

# "unit tests" group: parsed, with 22 parsed test children.
ut_count="$(printf '%s' "$V" | "$JQ" -r '[.. | objects | select(.kind=="group" and .label=="unit tests")][0].children | length')"
assert_eq "D vesting 'unit tests' has 22 children" "22" "$ut_count"
ut_parsed="$(printf '%s' "$V" | "$JQ" -r '[.. | objects | select(.kind=="group" and .label=="unit tests")][0].children | map(select(.kind=="test" and .source=="parsed")) | length')"
assert_eq "D vesting 'unit tests' 22 parsed test children" "22" "$ut_parsed"

# --- D-vesting TestingInterface synthesized subtree (R1-R4) ---
# Locate the synthesized propRunActions group (the TestingInterface top group).
TI='[.. | objects | select(.source=="synthesized" and .testingInterface==true and (.role|not) and .kind=="group")][0]'

# Top synthesized group flags: testingInterface + NOT pendingExpansion + model.
assert_json "D vesting TI top is testingInterface"  "$V" "$TI.testingInterface" "true"
assert_json "D vesting TI top NOT pendingExpansion" "$V" "$TI.pendingExpansion" "false"
assert_json "D vesting TI top label"                "$V" "$TI.label" "Property-based test vesting script"
assert_json "D vesting TI top model"                "$V" "$TI.model" "VestingModel"

# R4 ORDER INVARIANT: the synthesized group's direct children must be, in order:
# Positive tests -> Negative tests -> Threat models -> Expected vulnerabilities.
ti_order="$(printf '%s' "$V" | "$JQ" -r "$TI.children | map(.label) | join(\" | \")")"
assert_eq "D vesting TI children labels+ORDER" \
  "Positive tests | Negative tests | Threat models | Expected vulnerabilities" "$ti_order"

# Positive tests leaf: role=positive, leaf test, pendingExpansion==false.
assert_json "D vesting 'Positive tests' kind"  "$V" "$TI.children[0].kind" "test"
assert_json "D vesting 'Positive tests' role"  "$V" "$TI.children[0].role" "positive"
assert_json "D vesting 'Positive tests' NOT pendingExpansion" "$V" "$TI.children[0].pendingExpansion" "false"
# Negative tests leaf: role=negative, pendingExpansion==false.
assert_json "D vesting 'Negative tests' role"  "$V" "$TI.children[1].role" "negative"
assert_json "D vesting 'Negative tests' NOT pendingExpansion" "$V" "$TI.children[1].pendingExpansion" "false"

# "Threat models" group (role + 6 leaves, role=threat-model + pendingExpansion).
assert_json "D vesting 'Threat models' role"   "$V" '[.. | objects | select(.label=="Threat models")][0].role' "threat-models-group"
tm_count="$(printf '%s' "$V" | "$JQ" -r '[.. | objects | select(.kind=="group" and .label=="Threat models")][0].children | length')"
assert_eq "D vesting 'Threat models' has 6 children" "6" "$tm_count"
tm_src="$(printf '%s' "$V" | "$JQ" -r '[.. | objects | select(.kind=="group" and .label=="Threat models")][0].source')"
assert_eq "D vesting 'Threat models' is synthesized" "synthesized" "$tm_src"
tm_leaf_role="$(printf '%s' "$V" | "$JQ" -r '[.. | objects | select(.kind=="group" and .label=="Threat models")][0].children | all(.role=="threat-model" and .pendingExpansion==false)')"
assert_eq "D vesting 'Threat models' leaves role=threat-model+NOTpending" "true" "$tm_leaf_role"

# "Expected vulnerabilities" group (role + 1 leaf, role=threat-model, NOT pending).
assert_json "D vesting 'Expected vulnerabilities' role" "$V" '[.. | objects | select(.label=="Expected vulnerabilities")][0].role' "expected-vulnerabilities-group"
assert_json "D vesting 'Expected vulnerabilities' NOT pendingExpansion" "$V" '[.. | objects | select(.label=="Expected vulnerabilities")][0].pendingExpansion' "false"
ev_count="$(printf '%s' "$V" | "$JQ" -r '[.. | objects | select(.kind=="group" and .label=="Expected vulnerabilities")][0].children | length')"
assert_eq "D vesting 'Expected vulnerabilities' has 1 child" "1" "$ev_count"
ev_src="$(printf '%s' "$V" | "$JQ" -r '[.. | objects | select(.kind=="group" and .label=="Expected vulnerabilities")][0].source')"
assert_eq "D vesting 'Expected vulnerabilities' is synthesized" "synthesized" "$ev_src"
ev_leaf_role="$(printf '%s' "$V" | "$JQ" -r '[.. | objects | select(.kind=="group" and .label=="Expected vulnerabilities")][0].children[0].role')"
assert_eq "D vesting 'Expected vulnerabilities' leaf role=threat-model" "threat-model" "$ev_leaf_role"
ev_leaf_pending="$(printf '%s' "$V" | "$JQ" -r '[.. | objects | select(.kind=="group" and .label=="Expected vulnerabilities")][0].children[0].pendingExpansion')"
assert_eq "D vesting 'Expected vulnerabilities' leaf NOT pendingExpansion" "false" "$ev_leaf_pending"

# R4: the bogus "QuickCheck action sequences" placeholder must be GONE.
qc_present="$(printf '%s' "$V" | "$JQ" -r '[.. | objects | select(.label=="QuickCheck action sequences")] | length')"
assert_eq "D vesting 'QuickCheck action sequences' placeholder removed" "0" "$qc_present"

# --- D-schema-gen-streaming ---
run_suite "convex-schema-gen-streaming-test"
SGS="$RUN_OUT"
sgs_dummy="$(printf '%s' "$SGS" | "$JQ" -r '[.. | objects | select(.kind=="test" and .label=="dummy")] | length')"
assert_eq "D schema-gen-streaming exactly 1 'dummy' test" "1" "$sgs_dummy"
sgs_grp_children="$(printf '%s' "$SGS" | "$JQ" -r '.suites[0].tree.children | length')"
assert_eq "D schema-gen-streaming group has exactly 1 test" "1" "$sgs_grp_children"
sgs_grp_kind="$(printf '%s' "$SGS" | "$JQ" -r '.suites[0].tree.children[0].kind')"
assert_eq "D schema-gen-streaming child is a test" "test" "$sgs_grp_kind"

# --- D-schema-gen-plain ---
run_suite "convex-schema-gen-plain-test"
SGP="$RUN_OUT"
sgp_dummy="$(printf '%s' "$SGP" | "$JQ" -r '[.. | objects | select(.kind=="test" and .label=="dummy")] | length')"
assert_eq "D schema-gen-plain exactly 1 'dummy' test" "1" "$sgp_dummy"
sgp_grp_children="$(printf '%s' "$SGP" | "$JQ" -r '.suites[0].tree.children | length')"
assert_eq "D schema-gen-plain group has exactly 1 test" "1" "$sgp_grp_children"
sgp_grp_kind="$(printf '%s' "$SGP" | "$JQ" -r '.suites[0].tree.children[0].kind')"
assert_eq "D schema-gen-plain child is a test" "test" "$sgp_grp_kind"

# --- D-testing-interface DEFAULT allThreatModels set (list-tests / R6) ---
# `ctf purchase_offer` writes NO explicit threatModels list (only an explicit
# expectedVulnerabilities of 3 entries), so its synthesized property-tests group
# must use the library default: ALL_THREAT_MODELS (19) minus the 3 expected
# vulnerabilities = 16 leaves. This is the case R6 found diverging from reality.
run_suite "convex-testing-interface-test"
TIO="$RUN_OUT"
# Synthesized property-tests group sitting under "ctf purchase_offer".
PO='[.. | objects | select(.label=="ctf purchase_offer")][0]'
PO_TI="$PO | .. | objects | select(.testingInterface==true and .kind==\"group\" and (.role|not))"
# Direct children order must include the (now present) Threat models group.
po_order="$(printf '%s' "$TIO" | "$JQ" -r "[$PO_TI][0].children | map(.label) | join(\" | \")")"
assert_eq "D ti purchase_offer TI children labels+ORDER" \
  "Positive tests | Negative tests | Threat models | Expected vulnerabilities" "$po_order"
# The "Threat models" group EXISTS for this default-set instance.
PO_TM="$PO | .. | objects | select(.kind==\"group\" and .label==\"Threat models\")"
po_tm_present="$(printf '%s' "$TIO" | "$JQ" -r "[$PO_TM] | length")"
assert_eq "D ti purchase_offer has a 'Threat models' group" "1" "$po_tm_present"
# 16 leaves = 19 (ALL_THREAT_MODELS) - 3 (expectedVulnerabilities subtracted).
po_tm_count="$(printf '%s' "$TIO" | "$JQ" -r "[$PO_TM][0].children | length")"
assert_eq "D ti purchase_offer 'Threat models' has 16 children (19 default - 3 expected vulns)" "16" "$po_tm_count"
# Default-derived leaves are synthesized, role=threat-model, NOT pendingExpansion.
po_tm_src="$(printf '%s' "$TIO" | "$JQ" -r "[$PO_TM][0].source")"
assert_eq "D ti purchase_offer 'Threat models' is synthesized" "synthesized" "$po_tm_src"
po_tm_leaves_ok="$(printf '%s' "$TIO" | "$JQ" -r "[$PO_TM][0].children | all(.kind==\"test\" and .role==\"threat-model\" and .source==\"synthesized\" and .testingInterface==true and .pendingExpansion==false and (has(\"children\")|not))")"
assert_eq "D ti purchase_offer 'Threat models' leaves are TI threat-model leaves (no children)" "true" "$po_tm_leaves_ok"
# The 3 expected-vulnerability entries are SUBTRACTED out of the default set.
po_tm_no_evs="$(printf '%s' "$TIO" | "$JQ" -r "[$PO_TM][0].children | map(.label) | any(. == \"redeemerAssetSubstitution\" or . == \"timeBoundManipulation\" or (startswith(\"tokenForgeryAttack\"))) | not")"
assert_eq "D ti purchase_offer default set excludes the 3 expected-vuln entries" "true" "$po_tm_no_evs"
# Expected vulnerabilities group still has its 3 explicit leaves (unchanged path).
PO_EV="$PO | .. | objects | select(.kind==\"group\" and .label==\"Expected vulnerabilities\")"
po_ev_count="$(printf '%s' "$TIO" | "$JQ" -r "[$PO_EV][0].children | length")"
assert_eq "D ti purchase_offer 'Expected vulnerabilities' has 3 children" "3" "$po_ev_count"

# ===========================================================================
# Section E: JSON Schema conformance of the golden trees
# (uses a real validator if available; else jq structural fallback like list-test-suites)
# ===========================================================================
echo "${C_YEL}== E. JSON Schema conformance (goldens) ==${C_RST}"

# Pick a real JSON Schema validator if one is on PATH; else "" -> jq fallback.
SCHEMA_VALIDATOR=""
if command -v check-jsonschema >/dev/null 2>&1; then
  SCHEMA_VALIDATOR="check-jsonschema"
elif command -v ajv >/dev/null 2>&1; then
  SCHEMA_VALIDATOR="ajv"
elif python3 -c 'import jsonschema' >/dev/null 2>&1; then
  SCHEMA_VALIDATOR="python-jsonschema"
fi

# jq-based structural conformance walk: asserts the KEY schema invariants on a
# whole list-tests document. Echoes "OK" or "FAIL: <reasons>".
SCHEMA_JQ_FILTER='
def chknode:
  (if (has("kind") and has("label") and has("source")) then empty else "node missing required field" end),
  (if (has("authoritative")|not) then empty else "node has removed authoritative field" end),
  (if (.kind|IN("group","test","placeholder")) then empty else "bad node.kind \(.kind)" end),
  (if (.source|IN("parsed","synthesized","dynamic")) then empty else "bad node.source \(.source)" end),
  (if (has("role")|not) or (.role|IN("positive","negative","threat-models-group","expected-vulnerabilities-group","threat-model")) then empty else "bad node.role \(.role)" end),
  (if (has("testingInterface")|not) or (.testingInterface==true) then empty else "node.testingInterface not true" end),
  (if (has("pendingExpansion")|not) or ((.pendingExpansion|type)=="boolean") then empty else "node.pendingExpansion not boolean" end),
  (if ((.label|type) as $t | $t=="string" or $t=="null") then empty else "node.label not string|null" end),
  (if (has("file")|not) or ((.file|type)=="string") then empty else "node.file not string" end),
  (if (has("line")|not) or ((.line|type)=="number") then empty else "node.line not number" end),
  (if (has("children")|not) or ((.children|type)=="array") then empty else "node.children not array" end),
  (.children[]? | chknode);
def chk:
  (if (has("root") and has("suites")) then empty else "missing top-level key" end),
  (if (.root|type)=="string" then empty else "root not string" end),
  (if (.suites|type)=="array" then empty else "suites not array" end),
  ( .suites[] |
    . as $s |
    (if (["suite","package","entryFile","entryPoint","tree"]|all(. as $k| $s|has($k))) then empty else "suite missing field(s)" end),
    (if (.entryPoint|IN("STREAMING","upstream","unknown","MISSING")) then empty else "bad entryPoint \(.entryPoint)" end),
    (if (.tree==null) then empty else (.tree | chknode) end)
  );
[chk] | if length==0 then "OK" else "FAIL: "+(join("; ")) end
'

# validate_schema <id> <json-file>
validate_schema() {
  local id="$1" file="$2"
  if [[ -n "$SCHEMA_VALIDATOR" ]]; then
    local rc=0 out=""
    case "$SCHEMA_VALIDATOR" in
      check-jsonschema)   out="$(check-jsonschema --schemafile "$SCHEMA" "$file" 2>&1)"; rc=$? ;;
      ajv)                out="$(ajv validate -s "$SCHEMA" -d "$file" 2>&1)"; rc=$? ;;
      python-jsonschema)  out="$(python3 -c 'import sys,json,jsonschema; jsonschema.validate(json.load(open(sys.argv[1])), json.load(open(sys.argv[2])))' "$file" "$SCHEMA" 2>&1)"; rc=$? ;;
    esac
    if [[ $rc -eq 0 ]]; then _pass "$id (via $SCHEMA_VALIDATOR)"; else _fail "$id (via $SCHEMA_VALIDATOR)" "$(printf '%s' "$out" | head -c 200)"; fi
  else
    local res; res="$("$JQ" -r "$SCHEMA_JQ_FILTER" "$file" 2>&1)"
    if [[ "$res" == "OK" ]]; then _pass "$id (jq structural)"; else _fail "$id (jq structural)" "$res"; fi
  fi
}

if [[ -z "$SCHEMA_VALIDATOR" ]]; then
  echo "${C_YEL}  (note) no formal JSON Schema validator installed (check-jsonschema/ajv/python-jsonschema); using jq structural conformance.${C_RST}"
fi

# E1 schema file itself is valid JSON.
if "$JQ" -e . "$SCHEMA" >/dev/null 2>&1; then
  _pass "E1 schema file is valid JSON"
else
  _fail "E1 schema file is valid JSON" "jq could not parse $SCHEMA"
fi

# E2 embedded schema example conforms.
EX_FILE="$(mktemp "$TMPROOT/example.XXXXXX.json")"
"$JQ" -c '.examples[0]' "$SCHEMA" > "$EX_FILE" 2>/dev/null
validate_schema "E2 schema embedded example conforms" "$EX_FILE"

# E3..E5 the three golden trees conform to the schema.
validate_schema "E3 golden-vesting conforms"                "$TESTDATA/golden-vesting.json"
validate_schema "E4 golden-schema-gen-streaming conforms"   "$TESTDATA/golden-schema-gen-streaming.json"
validate_schema "E5 golden-schema-gen-plain conforms"       "$TESTDATA/golden-schema-gen-plain.json"
validate_schema "E7 golden-testing-interface conforms"      "$TESTDATA/golden-testing-interface.json"

# E6 live full-repo output conforms too (guards schema-vs-tool drift).
LIVE_FILE="$(mktemp "$TMPROOT/live.XXXXXX.json")"
printf '%s' "$ALL_OUT" > "$LIVE_FILE"
validate_schema "E6 live full-repo output conforms" "$LIVE_FILE"

# ===========================================================================
# Section F: portability of goldens
# ===========================================================================
echo "${C_YEL}== F. Golden portability ==${C_RST}"

for g in golden-vesting.json golden-schema-gen-streaming.json golden-schema-gen-plain.json golden-testing-interface.json; do
  gf="$TESTDATA/$g"
  groot="$("$JQ" -r '.root' "$gf" 2>/dev/null)"
  assert_eq "F $g root is normalized to <ROOT>" "<ROOT>" "$groot"
  # No absolute home/nix paths leaked anywhere in the golden.
  if grep -q "$HOME" "$gf" 2>/dev/null; then
    _fail "F $g contains no machine-specific absolute path" "found \$HOME in $gf"
  else
    _pass "F $g contains no machine-specific absolute path"
  fi
done

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
