// Per-file import table.
//
// tree-sitter-haskell node shape (verified):
//   import A.B.C                    -> (import module: (module ...))
//   import A.B.C qualified          -> (import module: (module ...))
//   import qualified A.B.C          -> (import module: (module ...))
//   import A.B.C qualified as X     -> (import module: (module ...) alias: (module ...))
//   import A.B.C as Y               -> (import module: (module ...) alias: (module ...))
//
// `module.text` is the dotted module name; `alias.text` (when present) is the
// local alias. Explicit import lists `import M (a, b)` expose
// names:(import_list name:(import_name variable:(variable))). We build:
//   - modules:        Set of imported module names
//   - aliases:        Map<alias, module name>
//   - unqualified:    Map<unqualified name, module name>  (explicit import lists)
//   - openModules:    [module name] imported WITHOUT an explicit list
//                     (i.e. `import M` / `import M hiding (...)`), whose every
//                     exported name is in scope unqualified — search these as a
//                     fallback for bare references.
//
// resolveModule(ref) maps a qualified reference's `module` text (which may be a
// full module name OR an alias) to a concrete module name.

const { nodeField } = require("./ast");

class ImportTable {
  constructor() {
    this.modules = new Set(); // imported module names (full dotted)
    this.aliases = new Map(); // alias -> module name
    this.unqualified = new Map(); // unqualified name -> module name
    this.openModules = []; // modules imported without an explicit list
  }

  // Map a reference's module component (full name or alias) to a module name.
  // Returns the resolved module name (possibly identical to input) or null if
  // it is neither a known import nor a known alias.
  resolveModule(refModule) {
    if (!refModule) return null;
    if (this.aliases.has(refModule)) return this.aliases.get(refModule);
    if (this.modules.has(refModule)) return refModule;
    // Unknown: still return it as-is so the file resolver can try base dirs.
    // (Defensive: import may be hidden behind CPP / re-exports we didn't index.)
    return refModule;
  }

  // For a bare (unqualified) reference name, return the module(s) it could come
  // from: the precise explicit-import module first (if any), then all open
  // modules as fallbacks. Caller searches these in order.
  candidateModulesFor(name) {
    const out = [];
    if (this.unqualified.has(name)) out.push(this.unqualified.get(name));
    for (const m of this.openModules) if (!out.includes(m)) out.push(m);
    return out;
  }
}

// Build an ImportTable from a parsed module's root node.
function parseImports(rootNode) {
  const table = new ImportTable();
  if (!rootNode) return table;

  function walk(node) {
    if (node.type === "import") {
      const modNode = nodeField(node, "module");
      const aliasNode = nodeField(node, "alias");
      const namesNode = nodeField(node, "names");
      if (modNode) {
        const moduleName = modNode.text.trim();
        table.modules.add(moduleName);
        if (aliasNode) {
          table.aliases.set(aliasNode.text.trim(), moduleName);
        }
        if (namesNode) {
          // Explicit import list. `hiding (...)` also produces a names list;
          // we cannot tell them apart by field alone, so we record explicit
          // variable names AND treat the module as open (fallback search). For
          // the common `import M (a, b)` this is precise; for `hiding` the
          // extra open-module fallback is harmless (it just scans more).
          for (const nm of namesNode.namedChildren) {
            if (nm.type !== "import_name") continue;
            const v = nm.childForFieldName("variable");
            if (v) table.unqualified.set(v.text.trim(), moduleName);
          }
          table.openModules.push(moduleName);
        } else {
          // No list: every exported name is unqualified-in-scope.
          table.openModules.push(moduleName);
        }
      }
      return; // imports have no nested imports
    }
    for (const child of node.namedChildren) walk(child);
  }

  walk(rootNode);
  return table;
}

module.exports = { ImportTable, parseImports };
