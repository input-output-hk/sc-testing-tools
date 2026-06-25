#!/usr/bin/env node
// list-tests — static (no-compile) tasty test-tree discovery for sc-tools.
//
// Tier-1 best-effort approximation of the tasty test tree inside each test
// suite's .hs files, extracted with tree-sitter-haskell (WASM). The authoritative
// tree comes from the streaming `--list-tests-json` output (Tier 2), which
// replaces this output entirely. The two tools have different output shapes, so
// the extension already knows which tier produced a tree.
//
// USAGE
//   node list-tests.js <ROOT>
//       Invoke scripts/list-test-suites/list-test-suites.sh <ROOT> (the
//       list-test-suites tool), parse its JSON, and extract a tree for every
//       test suite.
//
//   node list-tests.js --from-json <file> [<ROOT>]
//       Use the given test-suite discovery JSON file instead of invoking the
//       script. ROOT is taken from the JSON's `root`.
//
//   <test-suite discovery JSON on stdin> | node list-tests.js
//       If stdin is not a TTY and no <ROOT>/--from-json is given, read the
//       test-suite discovery JSON from stdin as an override.
//
// OPTIONS
//   --suite <name>   only extract the named suite (handy for testing)
//   --limit <N>      cross-file follow limit (default 100; tree depth unlimited)
//   --from-json <f>  read test-suite discovery JSON from file f instead of invoking the script
//   --check-health   verify required dependencies are present, then exit
//   -h, --help       show this help
//
// OUTPUT  (pretty-printed JSON)
//   { root, suites: [ { suite, package, entryFile, entryPoint, tree, note? } ] }

const path = require("path");
const fs = require("fs");
const { spawnSync } = require("child_process");

const { getParser, grammarPath, ModuleCache } = require("./lib/parser");
const { flattenSuites, extractSuite } = require("./lib/suites");

const LIST_TEST_SUITES_SH = path.resolve(
  __dirname,
  "..",
  "list-test-suites",
  "list-test-suites.sh"
);

function usage() {
  process.stdout.write(
    [
      "list-tests — static (no-compile) tasty test-tree discovery.",
      "",
      "USAGE",
      "  node list-tests.js <ROOT>",
      "  node list-tests.js --from-json <file>",
      "  <test-suite discovery JSON on stdin> | node list-tests.js",
      "",
      "OPTIONS",
      "  --suite <name>   only extract the named suite",
      "  --limit <N>      cross-file follow limit (default 100)",
      "  --from-json <f>  read test-suite discovery JSON from file instead of running the script",
      "  --check-health   verify required dependencies are present, then exit",
      "  -h, --help       show this help",
      "",
    ].join("\n")
  );
}

function parseArgs(argv) {
  const opts = {
    root: null,
    suite: null,
    limit: 100,
    fromJson: null,
    help: false,
    checkHealth: false,
  };
  for (let i = 2; i < argv.length; i++) {
    const a = argv[i];
    switch (a) {
      case "-h":
      case "--help":
        opts.help = true;
        break;
      case "--check-health":
        opts.checkHealth = true;
        break;
      case "--suite":
        opts.suite = argv[++i];
        break;
      case "--limit": {
        const n = parseInt(argv[++i], 10);
        if (Number.isNaN(n) || n < 0) {
          throw new Error(`--limit expects a non-negative integer, got '${argv[i]}'`);
        }
        opts.limit = n;
        break;
      }
      case "--from-json":
        opts.fromJson = argv[++i];
        break;
      default:
        if (a.startsWith("-")) {
          throw new Error(`unknown option: ${a}`);
        }
        if (opts.root === null) opts.root = a;
        else throw new Error(`unexpected extra argument: ${a}`);
    }
  }
  return opts;
}

// Read all of stdin synchronously, robustly handling EAGAIN on a slow pipe
// (fs.readFileSync(0) can throw/short-read when the producer is still writing).
function readStdin() {
  const CHUNK = 64 * 1024;
  const buf = Buffer.alloc(CHUNK);
  const chunks = [];
  while (true) {
    let bytes;
    try {
      bytes = fs.readSync(0, buf, 0, CHUNK, null);
    } catch (err) {
      if (err.code === "EAGAIN") {
        // Producer not ready yet; spin briefly and retry.
        continue;
      }
      if (err.code === "EOF") break;
      throw err;
    }
    if (bytes === 0) break;
    chunks.push(Buffer.from(buf.subarray(0, bytes)));
  }
  return Buffer.concat(chunks).toString("utf8");
}

// True if fd 0 is a pipe/FIFO or a regular file (i.e. data is being fed in),
// as opposed to an interactive terminal.
function stdinIsPiped() {
  if (process.stdin.isTTY) return false;
  try {
    const st = fs.fstatSync(0);
    return st.isFIFO() || st.isFile() || st.isSocket();
  } catch (_e) {
    return false;
  }
}

// Obtain test-suite discovery JSON: from --from-json, from stdin, or by invoking the script.
function obtainSuitesJson(opts) {
  if (opts.fromJson) {
    const txt = fs.readFileSync(opts.fromJson, "utf8");
    return JSON.parse(txt);
  }

  // No explicit root and stdin is piped -> read JSON from stdin.
  if (opts.root === null && stdinIsPiped()) {
    const txt = readStdin();
    if (txt.trim().length > 0) return JSON.parse(txt);
  }

  if (opts.root === null) {
    throw new Error(
      "no <ROOT> given and no test-suite discovery JSON on stdin/--from-json. See --help."
    );
  }

  const root = path.resolve(opts.root);
  if (!fs.existsSync(LIST_TEST_SUITES_SH)) {
    throw new Error(`list-test-suites.sh not found at ${LIST_TEST_SUITES_SH}`);
  }
  const res = spawnSync(LIST_TEST_SUITES_SH, [root], {
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  });
  if (res.error) {
    throw new Error(`failed to run list-test-suites.sh: ${res.error.message}`);
  }
  if (res.status !== 0) {
    throw new Error(
      `list-test-suites.sh exited ${res.status}: ${res.stderr || ""}`
    );
  }
  return JSON.parse(res.stdout);
}

async function main() {
  const opts = parseArgs(process.argv);
  if (opts.help) {
    usage();
    return;
  }
  if (opts.checkHealth) {
    const { runCheckHealth } = require("./lib/health");
    process.exit(runCheckHealth());
  }

  const json = obtainSuitesJson(opts);
  const root = json.root;
  if (!root) throw new Error("test-suite discovery JSON has no `root` field");

  let suites = flattenSuites(json);
  if (opts.suite) {
    suites = suites.filter((s) => s.suite === opts.suite);
    if (suites.length === 0) {
      throw new Error(`no suite named '${opts.suite}' in list-test-suites output`);
    }
  }

  const parser = await getParser();
  const moduleCache = new ModuleCache(parser);

  const out = {
    root,
    grammar: path.relative(root, grammarPath()),
    suites: [],
  };

  for (const suite of suites) {
    out.suites.push(extractSuite(suite, root, moduleCache, opts.limit));
  }

  process.stdout.write(JSON.stringify(out, null, 2) + "\n");
}

main().catch((err) => {
  process.stderr.write(`list-tests: ${err.message}\n`);
  process.exit(1);
});
