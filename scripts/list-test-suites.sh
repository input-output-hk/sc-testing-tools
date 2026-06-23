#!/usr/bin/env bash
# list-test-suites.sh
#
# Discover cabal projects under a foreign Haskell repo and emit a structured
# description of every test-suite: how to build it and how to discover its
# tests, plus a list of orphan .cabal files not referenced by any project.
#
# Usage:
#   list-test-suites.sh [ROOT] [--tsv]
#
#   ROOT   Directory to scan (positional, default ".").
#   --tsv  Emit the legacy flat 4-column TSV instead of JSON.
#
# Default output is single-line JSON to stdout (pipe to `jq .` for pretty
# printing). This tool has NO jq dependency. Shape:
#   {
#     "root":     "<abs path>",
#     "projects": [ { "projectFile": <path|null>, "packages": [ ... ] } ],
#     "orphans":  [ { "cabalFile": ..., "packageDir": ... } ]
#   }
# Each package: { name, cabalFile, packageDir, testSuites[] }.
# Each suite:   { name, mainIs, entryPoint,
#                 runTestsCommand, streamTestsCommand, discoverCommand }.
#   - entryPoint        is one of: STREAMING | upstream | unknown | MISSING
#   - runTestsCommand   ALWAYS a string: `cabal test <suite> [--project-file=X]`.
#   - streamTestsCommand / discoverCommand are strings ONLY for STREAMING suites
#     (they append --test-options=--streaming-json / --list-tests-json, which
#     are ingredients provided only by our convex-tasty-streaming lib); they are
#     null for upstream/unknown/MISSING suites.
# Packages with zero test suites are INCLUDED with "testSuites": [].
# A package may appear under multiple projects (e.g. via "import:").
#
# --tsv output (legacy, tab-separated):
#   <suite-name>\t<package-dir>\t<main-path>\t<entry-point>
#
# Exit codes:
#   0  success
#   1  no test-suites found anywhere
#   2  usage error (unknown flag, etc.)

set -euo pipefail

# ---------------------------------------------------------------------------
# Argument parsing: [ROOT] [--tsv]
# ---------------------------------------------------------------------------
ROOT="."
ROOT_SET=0
EMIT_TSV=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --tsv)
      EMIT_TSV=1
      shift
      ;;
    -h|--help)
      sed -n '2,33p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    --*|-*)
      echo "error: unknown flag '$1'" >&2
      echo "usage: list-test-suites.sh [ROOT] [--tsv]" >&2
      exit 2
      ;;
    *)
      if [[ $ROOT_SET -eq 1 ]]; then
        echo "error: unexpected extra argument '$1'" >&2
        echo "usage: list-test-suites.sh [ROOT] [--tsv]" >&2
        exit 2
      fi
      ROOT="$1"
      ROOT_SET=1
      shift
      ;;
  esac
done

# A nonexistent or non-directory ROOT is a caller error (exit 2), distinct
# from the legitimate "valid dir but no test-suites" case (exit 1).
if [[ ! -d "$ROOT" ]]; then
  echo "error: ROOT '$ROOT' is not a directory" >&2
  echo "usage: list-test-suites.sh [ROOT] [--tsv]" >&2
  exit 2
fi

# ===========================================================================
# Shared discovery: find all .cabal files (used by both TSV and JSON paths).
# ===========================================================================
mapfile -t CABAL_FILES < <(
  find "$ROOT" \
    \( -name dist-newstyle -o -name tasty-investigate -o -name .git -o -name node_modules \) -prune -o \
    -name '*.cabal' -type f -print 2>/dev/null | sed 's,^\./,,' | sort
)

# ---------------------------------------------------------------------------
# awk program that parses one .cabal's test-suite stanzas and prints rows:
#   <suite>\t<pkg_dir>\t<main_path_or_MISSING>\t<PENDING|MISSING>
# (entry-point classification is done afterwards in bash via grep)
# This preserves the original script's parser behaviour exactly.
# ---------------------------------------------------------------------------
read -r -d '' AWK_PROG <<'AWK' || true
    function trim(s) { sub(/^[[:space:]]+/, "", s); sub(/[[:space:]]+$/, "", s); return s }

    function emit(    n, paths, i, p, found) {
      if (suite == "") return
      if (main_is == "") {
        print suite "\t" pkg_dir "\t" "MISSING" "\t" "MISSING"
        return
      }
      n = split(src_dirs, paths, /[[:space:],]+/)
      found = ""
      for (i = 1; i <= n; i++) {
        if (paths[i] == "") continue
        p = pkg_dir "/" paths[i] "/" main_is
        if (system("test -f \"" p "\"") == 0) { found = p; break }
      }
      print suite "\t" pkg_dir "\t" (found != "" ? found : "MISSING") "\t" "PENDING"
    }

    /^[a-zA-Z]/ {
      if (tolower($1) == "test-suite") {
        emit()
        suite = $2
        main_is = ""
        src_dirs = "."
        in_stanza = 1
        next
      } else if (in_stanza) {
        emit()
        suite = ""
        in_stanza = 0
      }
    }

    in_stanza && /^[[:space:]]+[Mm]ain-[Ii]s[[:space:]]*:/ {
      v = $0
      sub(/^[[:space:]]+[Mm]ain-[Ii]s[[:space:]]*:[[:space:]]*/, "", v)
      main_is = trim(v)
    }
    in_stanza && /^[[:space:]]+[Hh][Ss]-[Ss]ource-[Dd]irs[[:space:]]*:/ {
      v = $0
      sub(/^[[:space:]]+[Hh][Ss]-[Ss]ource-[Dd]irs[[:space:]]*:[[:space:]]*/, "", v)
      src_dirs = trim(v)
    }

    END { emit() }
AWK

# classify_entry <main_path>  -> echoes STREAMING|upstream|unknown
# STREAMING covers the whole streaming-capable family (all support
# --streaming-json / --list-tests-json):
#   - defaultMainStreaming / defaultMainStreamingWithIngredients
#     (from Convex.Tasty.Streaming)
#   - defaultMainTestingInterface (from Convex.TestingInterface, which is
#     defaultMainStreamingWithIngredients testingInterfaceIngredients)
# STREAMING is tested BEFORE the upstream defaultMain check so the streaming
# *WithIngredients runners are never mislabelled upstream.
classify_entry() {
  local main_path="$1"
  if grep -qE 'defaultMainStreamingWithIngredients|defaultMainStreaming|defaultMainTestingInterface|Convex\.Tasty\.Streaming|Convex\.TestingInterface' "$main_path" 2>/dev/null; then
    echo "STREAMING"
  elif grep -qE '\bdefaultMain\b|defaultMainWithIngredients' "$main_path" 2>/dev/null; then
    echo "upstream"
  else
    echo "unknown"
  fi
}

# parse_suites_for_cabal <cabal_file>
# emits rows: <suite>\t<pkg_dir>\t<main_path>\t<entryPoint>
parse_suites_for_cabal() {
  local cabal_file="$1"
  local pkg_dir
  pkg_dir="$(dirname "$cabal_file")"
  awk -v pkg_dir="$pkg_dir" "$AWK_PROG" "$cabal_file" \
  | while IFS=$'\t' read -r suite pdir main_path _pending; do
      if [[ "$main_path" == "MISSING" ]]; then
        printf '%s\t%s\t%s\t%s\n' "$suite" "$pdir" "MISSING" "MISSING"
      else
        local entry
        entry="$(classify_entry "$main_path")"
        printf '%s\t%s\t%s\t%s\n' "$suite" "$pdir" "$main_path" "$entry"
      fi
    done
}

# ===========================================================================
# --tsv mode: legacy flat output, byte-identical to the original script.
# ===========================================================================
if [[ $EMIT_TSV -eq 1 ]]; then
  # Replicate original grep-driven ordering / "no stanzas" check.
  mapfile -t hits < <(
    grep -rEHin '^test-suite[[:space:]]+' \
      --include='*.cabal' \
      --exclude-dir=dist-newstyle \
      --exclude-dir=tasty-investigate \
      --exclude-dir=.git \
      --exclude-dir=node_modules \
      "$ROOT" 2>/dev/null || true
  )
  if [[ ${#hits[@]} -eq 0 ]]; then
    echo "No test-suite stanzas found under $ROOT" >&2
    exit 1
  fi
  declare -A seen_cabal=()
  for h in "${hits[@]}"; do
    cabal_file="${h%%:*}"
    if [[ -n "${seen_cabal[$cabal_file]:-}" ]]; then continue; fi
    seen_cabal[$cabal_file]=1
    parse_suites_for_cabal "$cabal_file"
  done
  exit 0
fi

# ===========================================================================
# JSON mode.
# ===========================================================================

# --- helpers -------------------------------------------------------------

# extract_pkg_name <cabal_file> : echo the top-level name: field (or "").
extract_pkg_name() {
  local cabal_file="$1"
  grep -iE '^name:[[:space:]]*' "$cabal_file" 2>/dev/null \
    | head -n1 \
    | sed -E 's/^[Nn][Aa][Mm][Ee]:[[:space:]]*//; s/[[:space:]]+$//'
}

# rel_to_root <path> : path relative to $ROOT (strip leading "$ROOT/" or "./").
rel_to_root() {
  local p="$1"
  # normalise ./ prefix
  p="${p#./}"
  local r="${ROOT#./}"
  if [[ "$r" != "." && "$r" != "" ]]; then
    p="${p#$r/}"
  fi
  echo "$p"
}

# json_str <s> : emit a JSON-escaped string (with surrounding quotes).
# Hand-rolled (no jq dependency). Backslash MUST be escaped first.
json_str() {
  local s="$1"
  s="${s//\\/\\\\}"        # backslash (first!)
  s="${s//\"/\\\"}"        # double quote
  s="${s//$'\t'/\\t}"      # tab
  s="${s//$'\n'/\\n}"      # newline
  s="${s//$'\r'/\\r}"      # carriage return
  s="${s//$'\b'/\\b}"      # backspace
  s="${s//$'\f'/\\f}"      # form feed
  printf '"%s"' "$s"
}

# --- cabal.project discovery + parsing -----------------------------------

# Discover project files: cabal.project and cabal.project.* under $ROOT
# (top-level only; cabal only honours them at the project root).
mapfile -t PROJECT_FILES < <(
  find "$ROOT" -maxdepth 1 \( -name 'cabal.project' -o -name 'cabal.project.*' \) -type f 2>/dev/null \
    | grep -vE '\.(freeze|local)$' \
    | sort
)

# Map: cabal_file(abs-ish path as found) -> space-separated list of project files (rel).
declare -A CABAL_OWNERS=()
# Set of project files (rel) we will emit, in order.
declare -a PROJECT_REL_LIST=()

# Normalise a path the same way CABAL_FILES entries are normalised, so
# ownership-map keys match (otherwise a resolved cabal is wrongly orphaned).
# Strips a leading "./" and collapses any embedded "/./" segments and a
# trailing "/." (which arise from directory tokens like "." or "mydir/").
norm_cabal_path() {
  local p="$1"
  while [[ "$p" == *"/./"* ]]; do p="${p//\/.\//\/}"; done  # embedded /./
  p="${p%/.}"          # trailing /.
  while [[ "$p" == *"//"* ]]; do p="${p//\/\//\/}"; done    # collapse //
  while [[ "$p" == ./* ]]; do p="${p#./}"; done             # strip ALL leading ./
  echo "$p"
}

# Resolve a package-entry token to concrete .cabal file paths. A token can be:
#   - an explicit file:  foo/bar.cabal
#   - a glob:            */*.cabal, pkgs/*
#   - a bare DIRECTORY:  .  |  mydir  |  mydir/   -> the .cabal file(s) directly
#                        inside that directory (non-recursive, as cabal does).
# Echoes zero or more .cabal paths in CABAL_FILES-normalised form.
resolve_package_entry() {
  local proj_dir="$1" entry="$2"
  # entry is relative to the project file's directory.
  local target="$proj_dir/$entry"
  shopt -s nullglob
  local matches=()
  if [[ "$entry" == *"*"* || "$entry" == *.cabal ]]; then
    # glob or explicit .cabal path
    for m in $target; do
      [[ -f "$m" ]] && matches+=("$m")
    done
  fi
  if [[ ${#matches[@]} -eq 0 && -d "$target" ]]; then
    # bare directory token (incl. "." and trailing-slash forms): take the
    # .cabal file(s) directly inside, non-recursively.
    for m in "$target"/*.cabal; do
      [[ -f "$m" ]] && matches+=("$m")
    done
  fi
  shopt -u nullglob
  for m in "${matches[@]}"; do
    norm_cabal_path "$m"
  done
}

# Recursively collect packages declared in a project file, following import:.
# Args: <project_file> <accumulator-array-name-for-visited>
# Echoes resolved .cabal paths (one per line), possibly with duplicates.
declare -A _VISITED_PROJ=()
collect_project_packages() {
  local pfile="$1"
  local key="$pfile"
  if [[ -n "${_VISITED_PROJ[$key]:-}" ]]; then return; fi
  _VISITED_PROJ[$key]=1

  local pdir
  pdir="$(dirname "$pfile")"

  # Parse line-by-line. Track state for multi-line packages: block.
  local in_pkgs=0 in_srp=0
  while IFS= read -r line || [[ -n "$line" ]]; do
    # strip CR
    line="${line%$'\r'}"
    # strip comments (-- ...) ; cabal uses -- for line comments
    local stripped="${line%%--*}"

    # source-repository-package stanza: skip its indented body.
    if [[ "$stripped" =~ ^source-repository-package ]]; then
      in_srp=1; in_pkgs=0
      continue
    fi
    if [[ $in_srp -eq 1 ]]; then
      if [[ "$stripped" =~ ^[^[:space:]] ]]; then
        in_srp=0   # dedented: stanza ended; fall through to process this line
      else
        continue
      fi
    fi

    # import: <file>  -> recurse
    if [[ "$stripped" =~ ^[[:space:]]*[Ii]mport:[[:space:]]*(.+)$ ]]; then
      local imp
      imp="$(echo "${BASH_REMATCH[1]}" | sed -E 's/[[:space:]]+$//')"
      [[ -n "$imp" ]] && collect_project_packages "$pdir/$imp"
      in_pkgs=0
      continue
    fi

    # packages: ... (inline and/or start of block)
    if [[ "$stripped" =~ ^[[:space:]]*[Pp]ackages:[[:space:]]*(.*)$ ]]; then
      local rest="${BASH_REMATCH[1]}"
      in_pkgs=1
      # inline entries on same line
      for tok in $rest; do
        resolve_package_entry "$pdir" "$tok"
      done
      continue
    fi

    # any other top-level (column-0) field closes the packages block
    if [[ "$stripped" =~ ^[^[:space:]] ]]; then
      in_pkgs=0
      continue
    fi

    # indented continuation of a packages block
    if [[ $in_pkgs -eq 1 ]]; then
      for tok in $stripped; do
        resolve_package_entry "$pdir" "$tok"
      done
    fi
  done < "$pfile"
}

# Build ownership map.
if [[ ${#PROJECT_FILES[@]} -gt 0 ]]; then
  for pf in "${PROJECT_FILES[@]}"; do
    pf_rel="$(rel_to_root "$pf")"
    PROJECT_REL_LIST+=("$pf_rel")
    _VISITED_PROJ=()   # reset visited set per top-level project
    while IFS= read -r cab; do
      [[ -z "$cab" ]] && continue
      # de-duplicate owners per cabal
      cur="${CABAL_OWNERS[$cab]:-}"
      case " $cur " in
        *" $pf_rel "*) ;;
        *) CABAL_OWNERS[$cab]="${cur:+$cur }$pf_rel" ;;
      esac
    done < <(collect_project_packages "$pf" | sort -u)
  done
else
  # No cabal.project: synthetic implicit project owns ALL discovered .cabal files.
  PROJECT_REL_LIST+=("__IMPLICIT_NULL__")
  for cab in "${CABAL_FILES[@]}"; do
    CABAL_OWNERS[$cab]="__IMPLICIT_NULL__"
  done
fi

# --- orphan detection ----------------------------------------------------
declare -a ORPHANS=()
for cab in "${CABAL_FILES[@]}"; do
  if [[ -z "${CABAL_OWNERS[$cab]:-}" ]]; then
    ORPHANS+=("$cab")
    echo "warning: orphan .cabal not referenced by any project: $(rel_to_root "$cab")" >&2
  fi
done

# --- command synthesis ---------------------------------------------------
# Determine the --project-file flag (if any) for a package's owners.
# Echoes either "" (default project, no flag) or the chosen non-default
# project-file (relative). Warns to stderr on ambiguity.
project_flag_for_owners() {
  local owners="$1"
  # If owned by the default cabal.project (rel "cabal.project") -> no flag.
  for o in $owners; do
    if [[ "$o" == "cabal.project" || "$o" == "__IMPLICIT_NULL__" ]]; then
      echo ""
      return
    fi
  done
  # Otherwise exclusively non-default: pick deterministically (sorted first).
  local sorted
  sorted="$(printf '%s\n' $owners | sort)"
  local count
  count="$(printf '%s\n' $owners | wc -l | tr -d ' ')"
  local chosen
  chosen="$(printf '%s\n' "$sorted" | head -n1)"
  if [[ "$count" -gt 1 ]]; then
    echo "warning: package exclusively owned by multiple non-default project files ($owners); picking '$chosen'" >&2
  fi
  echo "$chosen"
}

# --- single-owner assignment (dedup by ownership) ------------------------
# A package is rendered under EXACTLY ONE project: the one that INTRODUCES it.
# Rule (mirrors project_flag_for_owners so flag + nesting stay consistent):
#   - If reachable from the default project (cabal.project or implicit null),
#     the default owns it.
#   - Otherwise the chosen (sorted-first) non-default variant owns it.
# CABAL_PRIMARY_OWNER[$cab] = the project file (rel) that renders this cabal.
declare -A CABAL_PRIMARY_OWNER=()
for cab in "${CABAL_FILES[@]}"; do
  owners="${CABAL_OWNERS[$cab]:-}"
  [[ -z "$owners" ]] && continue
  primary=""
  for o in $owners; do
    if [[ "$o" == "cabal.project" || "$o" == "__IMPLICIT_NULL__" ]]; then
      primary="$o"
      break
    fi
  done
  if [[ -z "$primary" ]]; then
    primary="$(project_flag_for_owners "$owners")"
  fi
  CABAL_PRIMARY_OWNER[$cab]="$primary"
done

# --- JSON assembly -------------------------------------------------------
# We hand-roll the JSON (no jq dependency); json_str() handles escaping.

# Track whether any test-suite exists at all (for exit code 1).
# Computed up-front because build_json runs in a command-substitution subshell
# (variable assignments there would not propagate back to this shell).
ANY_SUITE=0
for _cab in "${CABAL_FILES[@]}"; do
  if [[ -n "$(parse_suites_for_cabal "$_cab")" ]]; then
    ANY_SUITE=1
    break
  fi
done

# Pre-compute, per cabal file: name, suites rows.
# We'll emit projects[] by iterating PROJECT_REL_LIST and, for each, the cabal
# files it owns (in CABAL_FILES order for determinism).

emit_suite_json() {
  # args: suite_name main_is entry_point run_cmd stream_cmd discover_cmd
  # stream_cmd / discover_cmd may be the sentinel "__NULL__" -> JSON null.
  local sname="$1" mis="$2" ep="$3" rcmd="$4" scmd="$5" dcmd="$6"
  local sval dval
  if [[ "$scmd" == "__NULL__" ]]; then sval="null"; else sval="$(json_str "$scmd")"; fi
  if [[ "$dcmd" == "__NULL__" ]]; then dval="null"; else dval="$(json_str "$dcmd")"; fi
  printf '{"name":%s,"mainIs":%s,"entryPoint":%s,"runTestsCommand":%s,"streamTestsCommand":%s,"discoverCommand":%s}' \
    "$(json_str "$sname")" "$(json_str "$mis")" "$(json_str "$ep")" "$(json_str "$rcmd")" "$sval" "$dval"
}

# Build the whole compact JSON document and emit it on stdout.
build_json() {
  local abs_root
  abs_root="$(cd "$ROOT" 2>/dev/null && pwd || echo "$ROOT")"

  printf '{'
  printf '"root":%s,' "$(json_str "$abs_root")"
  printf '"projects":['

  local proj_first=1
  for prel in "${PROJECT_REL_LIST[@]}"; do
    [[ $proj_first -eq 0 ]] && printf ','
    proj_first=0

    printf '{'
    if [[ "$prel" == "__IMPLICIT_NULL__" ]]; then
      printf '"projectFile":null,'
    else
      printf '"projectFile":%s,' "$(json_str "$prel")"
    fi
    printf '"packages":['

    local pkg_first=1
    for cab in "${CABAL_FILES[@]}"; do
      local primary="${CABAL_PRIMARY_OWNER[$cab]:-}"
      [[ -z "$primary" ]] && continue
      # Render this cabal ONLY under its single introducing project (dedup).
      [[ "$primary" != "$prel" ]] && continue

      local pname pdir cab_rel
      pname="$(extract_pkg_name "$cab")"
      pdir="$(dirname "$cab")"
      cab_rel="$(rel_to_root "$cab")"
      local pdir_rel
      pdir_rel="$(rel_to_root "$pdir")"

      # project-file flag derives from the SAME primary owner, so flag and
      # nesting stay consistent: default owner -> no flag; non-default -> flag.
      local flagstr=""
      if [[ "$primary" != "cabal.project" && "$primary" != "__IMPLICIT_NULL__" ]]; then
        flagstr=" --project-file=$primary"
      fi

      [[ $pkg_first -eq 0 ]] && printf ','
      pkg_first=0

      printf '{'
      printf '"name":%s,' "$(json_str "$pname")"
      printf '"cabalFile":%s,' "$(json_str "$cab_rel")"
      printf '"packageDir":%s,' "$(json_str "$pdir_rel")"
      printf '"testSuites":['

      local suite_first=1
      while IFS=$'\t' read -r suite _pdir main_path entry; do
        [[ -z "$suite" ]] && continue
        ANY_SUITE=1

        # mainIs: relative to package dir if resolved, else the raw value.
        local mis
        if [[ "$main_path" == "MISSING" ]]; then
          mis="MISSING"
        else
          mis="${main_path#$pdir/}"
        fi

        local run_cmd="cabal test ${suite}${flagstr}"
        local stream_cmd discover_cmd
        # --streaming-json / --list-tests-json are ingredients only provided
        # by our convex-tasty-streaming lib (defaultMainStreaming). Upstream
        # defaultMain suites don't have them, so they get only the run command.
        case "$entry" in
          STREAMING)
            stream_cmd="cabal test ${suite}${flagstr} --test-options=--streaming-json"
            discover_cmd="cabal test ${suite}${flagstr} --test-options=--list-tests-json"
            ;;
          *)
            stream_cmd="__NULL__"
            discover_cmd="__NULL__"
            ;;
        esac

        [[ $suite_first -eq 0 ]] && printf ','
        suite_first=0
        emit_suite_json "$suite" "$mis" "$entry" "$run_cmd" "$stream_cmd" "$discover_cmd"
      done < <(parse_suites_for_cabal "$cab")

      printf ']}'
    done

    printf ']}'
  done

  printf '],'

  # orphans
  printf '"orphans":['
  local orph_first=1
  for cab in "${ORPHANS[@]:-}"; do
    [[ -z "$cab" ]] && continue
    [[ $orph_first -eq 0 ]] && printf ','
    orph_first=0
    printf '{"cabalFile":%s,"packageDir":%s}' \
      "$(json_str "$(rel_to_root "$cab")")" \
      "$(json_str "$(rel_to_root "$(dirname "$cab")")")"
  done
  printf ']'

  printf '}'
}

RAW_JSON="$(build_json)"

# Did we find any test-suite?
if [[ $ANY_SUITE -eq 0 ]]; then
  echo "No test-suite stanzas found under $ROOT" >&2
  exit 1
fi

# Emit compact single-line JSON. Pipe to `jq .` for pretty-printing.
printf '%s\n' "$RAW_JSON"
