// Parser loading and module parsing/indexing.
//
// Loads the tree-sitter-haskell grammar wasm (from node_modules, where the
// prebuilt wasm ships inside the tree-sitter-haskell package; an optional
// vendor/ copy is still honoured if present) via web-tree-sitter (async API,
// 0.26.x), and parses .hs files into
// an indexed `Module` object: top-level binds + instances, ready for cross-file
// resolution.
//
// Versions:
//   web-tree-sitter     0.26.x
//   tree-sitter-haskell 0.23.x   (tree-sitter-haskell.wasm)

const path = require("path");
const fs = require("fs");

// NOTE: `web-tree-sitter` is required lazily inside getParser() rather than at
// module load. This keeps lightweight consumers — notably the health check,
// which only needs resolveGrammarWasm() — loadable even when the engine has not
// been installed yet, so they can report it MISSING gracefully instead of
// crashing on a top-level require.

const { parseImports } = require("./imports");
const { nodeField } = require("./ast");

// Resolve the grammar wasm: honour an optional vendored copy first (in case
// someone vendors it), then fall back to node_modules (the normal source — the
// prebuilt wasm ships inside the tree-sitter-haskell package). Returns the
// first that exists, else throws.
function resolveGrammarWasm() {
  const candidates = [
    path.join(__dirname, "..", "vendor", "tree-sitter-haskell.wasm"),
    path.join(
      __dirname,
      "..",
      "node_modules",
      "tree-sitter-haskell",
      "tree-sitter-haskell.wasm"
    ),
  ];
  for (const c of candidates) {
    if (fs.existsSync(c)) return c;
  }
  throw new Error(
    "tree-sitter-haskell grammar wasm not found. Looked in:\n  " +
      candidates.join("\n  ") +
      "\nRun: npm install --ignore-scripts --prefix scripts/list-tests  " +
      "(the --ignore-scripts flag skips a native build step that is " +
      "unnecessary — the prebuilt wasm ships in the package)."
  );
}

let _parser = null;
let _grammarPath = null;

// Initialise (idempotent) and return the shared Parser instance.
async function getParser() {
  if (_parser) return _parser;
  const { Parser, Language } = require("web-tree-sitter");
  _grammarPath = resolveGrammarWasm();
  await Parser.init();
  const Haskell = await Language.load(_grammarPath);
  _parser = new Parser();
  _parser.setLanguage(Haskell);
  return _parser;
}

// Path of the grammar wasm actually loaded (for diagnostics).
function grammarPath() {
  return _grammarPath;
}

// Index the top-level declarations of a parsed module.
//   binds:     Map<name, declNode>   (bind / function nodes)
//   instances: [{ name, patterns, node }]
function indexDecls(declsContainer, binds, instances) {
  if (!declsContainer) return;
  for (const child of declsContainer.namedChildren) {
    if (child.type === "bind") {
      const nm = nodeField(child, "name");
      if (nm && !binds.has(nm.text)) binds.set(nm.text, child);
    } else if (child.type === "function") {
      // function with patterns/clauses, e.g. `tests _opts runOpts = ...`.
      // First clause wins (multi-clause limitation, acceptable for Tier 1).
      const nm = nodeField(child, "name");
      if (nm && !binds.has(nm.text)) binds.set(nm.text, child);
    } else if (child.type === "instance") {
      const nm = nodeField(child, "name");
      const pats = nodeField(child, "patterns");
      instances.push({
        name: nm ? nm.text : null,
        patterns: pats ? pats.text.trim() : null,
        node: child,
      });
    }
  }
}

// A parsed Haskell module.
//   file:      absolute path
//   source:    file text
//   tree:      tree-sitter Tree
//   binds:     Map<name, declNode>
//   instances: [{ name, patterns, node }]
//   imports:   ImportTable (see lib/imports.js)
class ModuleCache {
  constructor(parser) {
    this.parser = parser;
    this.cache = new Map(); // absolute file -> Module
  }

  // Parse + index a module file (memoised by absolute path). Throws on read
  // failure; callers handle missing files before calling.
  parse(file) {
    const abs = path.resolve(file);
    if (this.cache.has(abs)) return this.cache.get(abs);

    const source = fs.readFileSync(abs, "utf8");
    const tree = this.parser.parse(source);

    const binds = new Map();
    const instances = [];

    // declarations sit directly under the root for tree-sitter-haskell.
    const declsField = nodeField(tree.rootNode, "declarations");
    indexDecls(declsField || tree.rootNode, binds, instances);

    const imports = parseImports(tree.rootNode);

    const mod = { file: abs, source, tree, binds, instances, imports };
    this.cache.set(abs, mod);
    return mod;
  }
}

module.exports = {
  getParser,
  grammarPath,
  resolveGrammarWasm,
  ModuleCache,
};
