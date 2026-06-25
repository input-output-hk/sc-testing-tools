// Core tasty test-tree extraction (promoted from spike-63/extract2.js).
//
// Given a parsed entry module + the suite's base dirs, walk the tasty tree:
//   CLASS 1 (parsed)      : literal testGroup / testCase / testProperty / ...
//   CLASS 2 (synthesized) : propRunActions* (TestingInterface) — synthesize a
//                           faithful subtree from the ThreatModelsFor instance.
//   CLASS 3 (dynamic)     : unresolved references / non-literal labels ->
//                           placeholder with a note. Never fabricate children.
//
// Cross-file references are FOLLOWED via the import table + base dirs, bounded
// by a follow limit and a visited (file,binding) set (see lib/resolver.js).

const path = require("path");

const A = require("./ast");
const { FollowState, moduleNameToFile } = require("./resolver");
const { synthesizePropRunActions, PROP_RUN_FNS } = require("./synthesize");

// ---------------------------------------------------------------------------
// Tasty vocabulary
// ---------------------------------------------------------------------------

const GROUP_FNS = new Set(["testGroup"]);
const TEST_FNS = new Set([
  "testCase",
  "testProperty",
  "testCaseSteps",
  "testCaseInfo",
  "goldenVsFile",
  "goldenVsString",
]);

// defaultMain family: the IO entry the `main` binding calls. Their first/only
// test-tree argument is what we actually want to extract from.
const DEFAULT_MAIN_FNS = new Set([
  "defaultMain",
  "defaultMainStreaming",
  "defaultMainStreamingWithIngredients",
  "defaultMainTestingInterface",
  "defaultMainWithIngredients",
]);

function isTastyFn(name) {
  return GROUP_FNS.has(name) || TEST_FNS.has(name);
}

function isDefaultMainFn(name) {
  return DEFAULT_MAIN_FNS.has(name);
}

// Transparent tasty wrappers: functions that take a TestTree and return a
// TestTree (possibly behind option lambdas). We descend through their argument
// and never emit a placeholder for the bare wrapper name itself.
const TRANSPARENT_WRAPPERS = new Set([
  "askOption",
  "localOption",
  "adjustOption",
  "withCoverageIndices",
  "withResource",
  "after",
  "sequentialTestGroup",
  // tasty test modifiers that wrap a single TestTree (the LAST argument):
  "expectFail",
  "expectFailBecause",
  "ignoreTest",
  "ignoreTestBecause",
]);

function isTransparentWrapperFn(name) {
  return TRANSPARENT_WRAPPERS.has(name);
}

// ---------------------------------------------------------------------------
// Extraction context — everything an extraction run needs.
// ---------------------------------------------------------------------------

class ExtractContext {
  constructor({ root, moduleCache, baseDirs, follow }) {
    this.root = root; // absolute repo root (for relative file paths in output)
    this.moduleCache = moduleCache; // ModuleCache
    this.baseDirs = baseDirs; // absolute hs-source base dirs for this suite
    this.follow = follow; // FollowState
  }

  // file path relative to repo root, for provenance.
  rel(file) {
    return path.relative(this.root, file);
  }
}

// ---------------------------------------------------------------------------
// Node constructors
// ---------------------------------------------------------------------------

function dynamicPlaceholder(label, note) {
  return {
    kind: "placeholder",
    label,
    source: "dynamic",
    // R3: dynamic placeholders are provisional; the real/expanded content
    // arrives when the slow --list-tests command runs.
    pendingExpansion: true,
    note,
  };
}

// ---------------------------------------------------------------------------
// Core recursive descent.
//
// extractExpr(expr, ctxModule, ctx): array of test-tree nodes for `expr`.
//   - testGroup "x" [ ... ]        -> group, recurse elements
//   - testCase "x" ...             -> test leaf
//   - propRunActions* @M "lbl" ..  -> synthesized subtree (CLASS 2)
//   - defaultMain* <tree>          -> transparent, descend into its argument
//   - references to bindings        -> follow (CLASS 1) or placeholder (CLASS 3)
//   - parens / let_in / infix / lists -> descend transparently
// ---------------------------------------------------------------------------

function extractExpr(expr, ctxModule, ctx) {
  if (!expr) return [];

  switch (expr.type) {
    case "parens":
      return extractExpr(expr.childForFieldName("expression"), ctxModule, ctx);

    case "let_in":
      return extractExpr(expr.childForFieldName("expression"), ctxModule, ctx);

    case "lambda": {
      // `\(Opt o) -> tests ...` (e.g. behind askOption). Descend into the body,
      // which is the last named child that is NOT the `patterns` node.
      let body = null;
      for (const c of expr.namedChildren) {
        if (c.type !== "patterns") body = c;
      }
      return extractExpr(body, ctxModule, ctx);
    }

    case "infix": {
      const op = expr.childForFieldName("operator");
      const left = expr.childForFieldName("left_operand");
      const right = expr.childForFieldName("right_operand");

      // `f $ x` is function application `f x`. For the `$` operator we treat the
      // left operand as a function applied to the right operand, so that
      // `testCase "label" $ body` / `defaultMain $ testGroup ...` /
      // `withCoverageIndices [...] $ tests` are handled exactly like the
      // equivalent `apply` — crucially WITHOUT descending the right operand as
      // an independent expression (which would mine non-test helper calls in a
      // test's body and emit spurious placeholders).
      if (op && op.text.trim() === "$" && left && right) {
        const { head, args } = A.peelApply(left);
        const fnName = A.headVariableName(head);
        const allArgs = args.concat([right]);

        if (fnName && PROP_RUN_FNS.has(fnName)) {
          return [
            synthesizePropRunActions(expr, allArgs, ctxModule, ctx, fnName, {
              extractContext: ctx,
              resolveModuleFile: (m) => resolveModuleFile(m, ctxModule, ctx),
              dynamicPlaceholder,
            }),
          ];
        }
        if (fnName && isTastyFn(fnName)) {
          return [buildTastyNode(expr, fnName, allArgs, ctxModule, ctx)];
        }
        if (fnName && (isDefaultMainFn(fnName) || isTransparentWrapperFn(fnName))) {
          // transparent: the test tree is the right operand.
          return extractExpr(right, ctxModule, ctx);
        }
        // Unknown function applied via `$`: try following the head binding;
        // if that fails, descend the right operand (it may itself be a tree).
        const ref = resolveReference(left, ctxModule, ctx, { quiet: true });
        if (ref.resolved) return ref.nodes;
        return extractExpr(right, ctxModule, ctx);
      }

      // Other operators (e.g. `<>` combining two trees): descend both operands.
      const out = [];
      if (left) out.push(...extractExpr(left, ctxModule, ctx));
      if (right) out.push(...extractExpr(right, ctxModule, ctx));
      return out;
    }

    case "apply": {
      const { head, args } = A.peelApply(expr);
      const fnName = A.headVariableName(head);

      // CLASS 2: TestingInterface runtime generator.
      if (fnName && PROP_RUN_FNS.has(fnName)) {
        return [
          synthesizePropRunActions(expr, args, ctxModule, ctx, fnName, {
            extractContext: ctx,
            resolveModuleFile: (m) =>
              resolveModuleFile(m, ctxModule, ctx),
            dynamicPlaceholder,
          }),
        ];
      }

      // CLASS 1: literal tasty calls.
      if (fnName && isTastyFn(fnName)) {
        return [buildTastyNode(expr, fnName, args, ctxModule, ctx)];
      }

      // defaultMain* <tree>: transparent wrapper; descend into ALL args so the
      // test tree argument is found regardless of position.
      if (fnName && isDefaultMainFn(fnName)) {
        const out = [];
        for (const a of args) out.push(...extractExpr(a, ctxModule, ctx));
        return out;
      }

      // Known transparent wrappers (askOption, withCoverageIndices, ...): the
      // test tree is the LAST argument by convention; earlier args are options /
      // coverage indices / resources, not tests. Descend only the last arg.
      if (fnName && isTransparentWrapperFn(fnName)) {
        if (args.length === 0) return [];
        return extractExpr(args[args.length - 1], ctxModule, ctx);
      }

      // Wrappers like withCoverageIndices/askOption/localOption: not tasty,
      // not defaultMain. Descend into their args transparently so any tasty
      // value buried inside is still found. ALSO attempt to follow the head if
      // it names a binding (e.g. `propBasedTests runOpts`).
      const refNodes = resolveReference(expr, ctxModule, ctx, {
        quiet: true,
      });
      if (refNodes.resolved) return refNodes.nodes;

      // Head was not a followable binding: descend into args as a fallback.
      const out = [];
      for (const a of args) out.push(...extractExpr(a, ctxModule, ctx));
      if (out.length > 0) return out;
      // Nothing found anywhere -> emit the unresolved placeholder.
      return refNodes.nodes;
    }

    case "variable":
    case "qualified": {
      // A bare reference to a defaultMain* or transparent-wrapper function
      // (e.g. the left operand of `defaultMainStreaming $ testGroup ...` or
      // `askOption $ \o -> ...`) is not a followable test binding. Drop it.
      const refName = A.headVariableName(expr);
      if (refName && (isDefaultMainFn(refName) || isTransparentWrapperFn(refName)))
        return [];
      return resolveReference(expr, ctxModule, ctx, {}).nodes;
    }

    case "list": {
      const out = [];
      for (const el of A.listElements(expr)) {
        out.push(...extractExpr(el, ctxModule, ctx));
      }
      return out;
    }

    default:
      return [];
  }
}

// Build a tasty group/test node (CLASS 1 - parsed).
function buildTastyNode(applyNode, fnName, args, ctxModule, ctx) {
  let label = null;
  for (const a of args) {
    const t = A.stringLiteralText(a);
    if (t !== null) {
      label = t;
      break;
    }
  }
  const kind = GROUP_FNS.has(fnName) ? "group" : "test";

  // Non-literal label on a group/test -> dynamic (Tier 1 cannot enumerate).
  const node = {
    kind,
    label,
    source: "parsed",
    file: ctx.rel(ctxModule.file),
    line: A.lineOf(applyNode),
  };
  if (label === null) {
    node.note = "non-literal label; not statically determinable";
  }

  if (kind === "group") {
    node.children = [];
    const listArg = A.findListArg(args);
    if (listArg) {
      for (const el of A.listElements(listArg)) {
        node.children.push(...extractExpr(el, ctxModule, ctx));
      }
    } else {
      // group whose children come from a non-list expression (map / fmap /
      // list-comprehension / variable): not statically enumerable.
      node.children.push(
        dynamicPlaceholder(
          label || "group children",
          "group children are not a literal list; not statically enumerable"
        )
      );
    }
  }
  return node;
}

// Resolve a module name to a file + parsed module via import table + base dirs.
// Returns { file, module } or null.
function resolveModuleFile(moduleRef, ctxModule, ctx) {
  const resolved = ctxModule.imports.resolveModule(moduleRef);
  const file = moduleNameToFile(resolved || moduleRef, ctx.baseDirs);
  if (!file) return null;
  return { file, module: ctx.moduleCache.parse(file) };
}

// Resolve a reference node (variable / qualified / apply-with-var-head) to its
// binding and recurse, following one cross-file hop when needed.
//
// Returns { resolved: bool, nodes: [...] }. When `quiet`, an unresolved binding
// still produces a placeholder in `nodes` but `resolved:false` lets the caller
// decide whether to fall back to descending args.
function resolveReference(node, ctxModule, ctx, { quiet }) {
  const tgt = A.refTarget(node);
  const label = node.text.trim();

  if (!tgt) {
    return {
      resolved: false,
      nodes: [dynamicPlaceholder(label, "could not interpret as a reference")],
    };
  }

  // Figure out which module the binding lives in.
  let targetModule = ctxModule;
  let crossFile = false;
  if (tgt.module) {
    const r = resolveModuleFile(tgt.module, ctxModule, ctx);
    if (!r) {
      return {
        resolved: false,
        nodes: [
          dynamicPlaceholder(
            label,
            `unresolved reference: module ${tgt.module} not found under base dirs`
          ),
        ],
      };
    }
    targetModule = r.module;
    crossFile = targetModule.file !== ctxModule.file;
  }

  let bindNode = targetModule.binds.get(tgt.name);

  // Unqualified reference not found locally: it may be imported unqualified
  // from another module (e.g. `import Escrow.Spec.Unit (unitTests)`). Search
  // candidate modules from the import table.
  if (!bindNode && !tgt.module) {
    for (const moduleName of ctxModule.imports.candidateModulesFor(tgt.name)) {
      const r = resolveModuleFile(moduleName, ctxModule, ctx);
      if (!r) continue;
      const cand = r.module.binds.get(tgt.name);
      if (cand) {
        targetModule = r.module;
        crossFile = targetModule.file !== ctxModule.file;
        bindNode = cand;
        break;
      }
    }
  }

  if (!bindNode) {
    return {
      resolved: false,
      nodes: [
        dynamicPlaceholder(
          label,
          `unresolved reference: binding '${tgt.name}'${
            tgt.module ? " in module " + tgt.module : " in current module or imports"
          } not found`
        ),
      ],
    };
  }

  // Cycle detection on (file, binding).
  if (ctx.follow.seen(targetModule.file, tgt.name)) {
    return {
      resolved: true,
      nodes: [
        dynamicPlaceholder(label, "cyclic reference; following stopped"),
      ],
    };
  }

  // Cross-file follow budget (tree depth itself is unlimited).
  if (crossFile) {
    if (!ctx.follow.hasBudget()) {
      return {
        resolved: false,
        nodes: [
          dynamicPlaceholder(
            label,
            `reference not followed (cross-file follow limit ${ctx.follow.limit} reached)`
          ),
        ],
      };
    }
    ctx.follow.spend();
  }

  const expr = A.bindExpression(bindNode);
  if (!expr) {
    return {
      resolved: false,
      nodes: [
        dynamicPlaceholder(
          label,
          `binding '${tgt.name}' resolved but has no simple RHS expression`
        ),
      ],
    };
  }

  ctx.follow.mark(targetModule.file, tgt.name);
  const nodes = extractExpr(expr, targetModule, ctx);
  return { resolved: true, nodes };
}

// ---------------------------------------------------------------------------
// Entry-point discovery.
//
// Strategy:
//   1. Prefer `main` and follow its defaultMain* argument (matches real code,
//      e.g. `main = defaultMainStreaming (tests opts runOpts)`).
//   2. Else a `tests` binding.
//   3. Else any top-level bind whose RHS is a literal testGroup application.
// ---------------------------------------------------------------------------

function findEntryExpression(mod) {
  // 1. main = defaultMain* <tree>
  if (mod.binds.has("main")) {
    const expr = A.bindExpression(mod.binds.get("main"));
    if (expr) return { expr, from: "main" };
  }
  // 2. tests
  if (mod.binds.has("tests")) {
    const expr = A.bindExpression(mod.binds.get("tests"));
    if (expr) return { expr, from: "tests" };
  }
  // 3. any bind whose RHS head is testGroup.
  for (const [name, bindNode] of mod.binds) {
    const expr = A.bindExpression(bindNode);
    if (!expr) continue;
    if (expr.type === "apply") {
      const { head } = A.peelApply(expr);
      if (GROUP_FNS.has(A.headVariableName(head))) return { expr, from: name };
    }
    if (expr.type === "infix") {
      // defaultMain $ testGroup ... at top level.
      const right = expr.childForFieldName("right_operand");
      if (right && right.type === "apply") {
        const { head } = A.peelApply(right);
        if (GROUP_FNS.has(A.headVariableName(head)))
          return { expr, from: name };
      }
    }
  }
  return null;
}

// Extract the full tree for an entry module. Returns a single node, an array of
// nodes, or null when no entry could be located.
function extractTree(entryModule, ctx) {
  const entry = findEntryExpression(entryModule);
  if (!entry) return { tree: null, entryBinding: null };

  ctx.follow.mark(entryModule.file, entry.from);
  const nodes = extractExpr(entry.expr, entryModule, ctx);
  const tree = nodes.length === 0 ? null : nodes.length === 1 ? nodes[0] : nodes;
  return { tree, entryBinding: entry.from };
}

module.exports = {
  ExtractContext,
  FollowState,
  extractTree,
  extractExpr,
  findEntryExpression,
  dynamicPlaceholder,
  GROUP_FNS,
  TEST_FNS,
  DEFAULT_MAIN_FNS,
};
