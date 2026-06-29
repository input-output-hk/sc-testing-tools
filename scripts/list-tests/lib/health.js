// Dependency health check for list-tests.js.
//
// Prints one aligned line per dependency (status tag, name, info/version), and
// reports whether all REQUIRED dependencies are present.
//
// VERSION POLICY: versions are shown for INFO only. No minimum version is ever
// enforced — the ONLY failure cause is an ABSENT required dependency. Optional
// deps that are absent are reported (WARN) but never affect the result.

const path = require("path");
const fs = require("fs");
const { spawnSync } = require("child_process");

const { resolveGrammarWasm } = require("./parser");

// Pad a name into a fixed-width column so the report stays scannable.
function col(name, width) {
  name = String(name);
  return name.length >= width ? name : name + " ".repeat(width - name.length);
}

const NAME_WIDTH = 22;

// The install command that restores the two npm-satisfiable required deps
// (the web-tree-sitter engine and the prebuilt tree-sitter-haskell.wasm
// grammar). --ignore-scripts is REQUIRED: the tree-sitter-haskell package's
// native postinstall build fails / is unnecessary — the prebuilt wasm suffices.
const INSTALL_CMD = "npm install --ignore-scripts --prefix scripts/list-tests";
const INSTALL_CONVENIENCE = "./scripts/pre-fetch.sh install";
const INSTALL_HINT =
  "  -> run: " +
  INSTALL_CMD +
  "  (or: " +
  INSTALL_CONVENIENCE +
  ")\n" +
  "     --ignore-scripts skips an unnecessary failing native build step;\n" +
  "     the prebuilt wasm ships in the package.";

function lineOK(name, info) {
  return `[  OK  ] ${col(name, NAME_WIDTH)} ${info || ""}`.trimEnd();
}
function lineMissing(name, info) {
  return `[MISSING] ${col(name, NAME_WIDTH)} ${info || ""}`.trimEnd();
}
function lineWarn(name, info) {
  return `[ WARN ] ${col(name, NAME_WIDTH)} ${info || ""}`.trimEnd();
}

// Best-effort presence check for an external command via `command -v`.
function commandPresent(cmd) {
  try {
    const r = spawnSync("bash", ["-c", `command -v ${cmd}`], {
      encoding: "utf8",
    });
    return r.status === 0;
  } catch (_e) {
    return false;
  }
}

// Best-effort one-line version string for an external command (INFO only).
function commandVersion(cmd) {
  try {
    const r = spawnSync(cmd, ["--version"], { encoding: "utf8" });
    const out = (r.stdout || "").split("\n")[0].trim();
    return out || "(present)";
  } catch (_e) {
    return "(present)";
  }
}

// Read a package's version from its node_modules package.json, or null. Reads
// the file directly (some packages, e.g. web-tree-sitter, restrict the
// "./package.json" subpath via their "exports" map, so require.resolve fails).
function pkgVersion(pkgName) {
  try {
    const p = path.join(
      __dirname,
      "..",
      "node_modules",
      pkgName,
      "package.json"
    );
    if (!fs.existsSync(p)) return null;
    const j = JSON.parse(fs.readFileSync(p, "utf8"));
    return j.version || null;
  } catch (_e) {
    return null;
  }
}

// Run the full health check. Writes the report to stdout and returns an exit
// code: 0 if all required deps are present, 1 otherwise.
function runCheckHealth() {
  const lines = [];
  let missing = 0;

  lines.push("list-tests — dependency health check");
  lines.push("");
  lines.push("Required:");

  // node (always present — we're running under it).
  lines.push(lineOK("node", process.version));

  // web-tree-sitter module must load.
  let wtsVer = pkgVersion("web-tree-sitter");
  try {
    require("web-tree-sitter");
    lines.push(
      lineOK("web-tree-sitter", wtsVer ? wtsVer : "(present)")
    );
  } catch (_e) {
    missing++;
    lines.push(
      lineMissing("web-tree-sitter", "engine module not loadable")
    );
    lines.push(INSTALL_HINT);
  }

  // tree-sitter-haskell grammar wasm must resolve (vendored or node_modules).
  try {
    const wasm = resolveGrammarWasm();
    const where = wasm.includes(`${path.sep}vendor${path.sep}`)
      ? "vendored"
      : "node_modules";
    lines.push(lineOK("tree-sitter-haskell.wasm", `present (${where})`));
  } catch (_e) {
    missing++;
    lines.push(
      lineMissing("tree-sitter-haskell.wasm", "grammar wasm not found")
    );
    lines.push(INSTALL_HINT);
  }

  // The sibling list-test-suites.sh (spawned in <ROOT> mode).
  const sibling = path.resolve(
    __dirname,
    "..",
    "..",
    "list-test-suites",
    "list-test-suites.sh"
  );
  if (fs.existsSync(sibling)) {
    lines.push(
      lineOK("list-test-suites.sh", "present (required for <ROOT> mode)")
    );
  } else {
    missing++;
    lines.push(
      lineMissing(
        "list-test-suites.sh",
        `not found at ${sibling} (required for <ROOT> mode)`
      )
    );
  }

  lines.push("");
  lines.push("Optional:");

  // jq — used only by examples / the test harness.
  if (commandPresent("jq")) {
    lines.push(lineOK("jq", commandVersion("jq")));
  } else {
    lines.push(
      lineWarn("jq", "(optional — used by the test harness / examples only)")
    );
  }

  // tree-sitter-haskell npm package — the source of the prebuilt grammar wasm.
  const tshVer = pkgVersion("tree-sitter-haskell");
  if (tshVer) {
    lines.push(
      lineOK("tree-sitter-haskell", `${tshVer} (npm package; ships the prebuilt grammar wasm)`)
    );
  } else {
    lines.push(
      lineWarn(
        "tree-sitter-haskell",
        "(npm package — ships the prebuilt grammar wasm; install via --ignore-scripts)"
      )
    );
  }

  lines.push("");
  if (missing === 0) {
    lines.push("All required dependencies present.");
  } else {
    lines.push(`Missing ${missing} required dependencies.`);
    lines.push(
      "Prerequisite missing — run: " +
        INSTALL_CMD +
        "  (or: " +
        INSTALL_CONVENIENCE +
        ")"
    );
  }

  process.stdout.write(lines.join("\n") + "\n");
  return missing === 0 ? 0 : 1;
}

module.exports = { runCheckHealth };
