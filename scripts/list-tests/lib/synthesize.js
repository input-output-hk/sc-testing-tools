// CLASS 2 synthesis: TestingInterface (propRunActions*).
//
// propRunActions / propRunActionsWithOptions generate their tasty subtree at
// RUNTIME, so it cannot be parsed. But we OWN the generator and know its shape,
// so we read its INPUTS syntactically and synthesize a faithful subtree.
//
// Synthesized SHAPE (verified from src/testing-interface/lib/Convex/
// TestingInterface.hs :: propRunActionsWithOptions). ORDER is a HARD invariant:
//   "<groupName>"                       (top synthesized group; testingInterface)
//     ├─ "Positive tests"               (leaf, ALWAYS, role=positive)
//     ├─ "Negative tests"               (leaf, ALWAYS, role=negative)
//     ├─ "Threat models"                (group, iff threatModels non-empty,
//     │                                  role=threat-models-group)
//     │    └─ <one leaf per threatModels elem, role=threat-model, pending>
//     └─ "Expected vulnerabilities"     (group, iff expectedVulnerabilities
//                                        non-empty, role=expected-vulnerabilities-group)
//          └─ <one leaf per expectedVulnerabilities elem, role=threat-model, pending>
//
// All synthesized nodes carry source:"synthesized" and testingInterface:true.
// The whole subtree has been VERIFIED to match the real tasty tree in labels,
// counts, nesting and ORDER, so every synthesized node carries
// pendingExpansion:false: the shape is FULLY KNOWN statically. (Approximate leaf
// names are mere renames, not expansions, so they do not justify pending.)
//
// NOTE: expected-vulnerability leaves are structurally identical to threat-model
// leaves (both rendered via getThreatModelName, both testCaseSteps). The leaf
// role is therefore `threat-model` in BOTH groups; the semantic difference is
// carried by the parent group's role. There is NO separate
// `expected-vulnerability` leaf role.

const path = require("path");

const A = require("./ast");

const PROP_RUN_FNS = new Set(["propRunActions", "propRunActionsWithOptions"]);

// Mirror of src/testing-interface/lib/Convex/ThreatModel/All.hs :: allThreatModels
// (19 entries, in order). KEEP IN SYNC: if that Haskell list changes, update this
// array. Each label is the SOURCE-TEXT form an explicit-list leaf would render via
// A.elementLabel (so the deleteFirstsBy/expectedVulnerabilities subtraction below
// can match by exact string). The tokenForgeryAttack entry is the full applied
// expression, matching how it appears in allThreatModels (and in instances'
// expectedVulnerabilities lists).
const ALL_THREAT_MODELS = [
  "datumListBloatAttack",
  "datumByteBloatAttack",
  "doubleSatisfaction",
  "duplicateListEntryAttack",
  "inputDuplication",
  "invalidDatumIndexAttack",
  "largeDataAttack",
  "largeValueAttack",
  "missingOutputDatumAttack",
  "mutualExclusionAttack",
  "negativeIntegerAttack",
  "outputDatumHashMissingAttack",
  "redeemerAssetSubstitution",
  "selfReferenceInjection",
  "signatoryRemoval",
  "timeBoundManipulation",
  "tokenForgeryAttack simpleAlwaysSucceedsMintingPolicyV2 simpleTestAssetName",
  "unprotectedScriptOutput",
  "valueUnderpaymentAttack",
];

// Locate the `binding` decl (bind/function) for `bindName` inside an instance,
// or null if absent.
function findInstanceBind(instanceNode, bindName) {
  const decls = instanceNode.childForFieldName("declarations");
  if (!decls) return null;
  for (const d of decls.namedChildren) {
    if (d.type === "bind" || d.type === "function") {
      const nm = d.childForFieldName("name");
      if (nm && nm.text === bindName) return d;
    }
  }
  return null;
}

// Find a `binding = [ e1, e2, ... ]` decl inside an instance and return a
// readable label per list element.
function readInstanceListBind(instanceNode, bindName) {
  const target = findInstanceBind(instanceNode, bindName);
  if (!target) return [];
  const expr = A.bindExpression(target);
  if (!expr || expr.type !== "list") return [];
  return A.listElements(expr).map(A.elementLabel);
}

// Decide how an instance's `threatModels` binding should be interpreted:
//   { kind: "explicit", list: [...] }  — an explicit `threatModels = [...]` list
//                                         literal; use it verbatim (UNCHANGED).
//   { kind: "default" }                — the library DEFAULT applies: either the
//                                         binding is ABSENT, or its RHS is the
//                                         bare identifier `allThreatModels`
//                                         (optionally module-qualified). The
//                                         effective list is ALL_THREAT_MODELS
//                                         minus expectedVulnerabilities (applied
//                                         by the caller).
// Anything else (e.g. a non-list, non-allThreatModels expression we can't read)
// is treated as "default" too — the safest faithful approximation, since the
// real default is what the compiler would use when no usable override is given.
function classifyThreatModelsBind(instanceNode) {
  const target = findInstanceBind(instanceNode, "threatModels");
  if (!target) return { kind: "default" }; // (a) binding absent
  const expr = A.bindExpression(target);
  if (expr && expr.type === "list") {
    return { kind: "explicit", list: A.listElements(expr).map(A.elementLabel) };
  }
  // (b) RHS is the bare identifier `allThreatModels` (plain or qualified, e.g.
  //     `All.allThreatModels`). refTarget peels parens/qualified for us.
  const ref = expr ? A.refTarget(expr) : null;
  if (ref && ref.name === "allThreatModels") return { kind: "default" };
  // Fallback: an RHS we can't statically read as a list — mirror the library
  // default rather than emitting NO group.
  return { kind: "default" };
}

// Locate the `instance ThreatModelsFor <model>` node, searching:
//   1. the call-site module (ctxModule), then
//   2. the module where <model> is defined/imported (best-effort).
// Returns { instance, module } or null.
function findThreatModelsForInstance(model, ctxModule, helpers) {
  const matches = (inst) =>
    inst.name === "ThreatModelsFor" &&
    (model === null || inst.patterns === model);

  // 1. current module.
  const local = ctxModule.instances.find(matches);
  if (local) return { instance: local.node, module: ctxModule };

  if (model === null) return null;

  // 2. the model's defining module. We don't have a type-name -> module map,
  //    so try the imported modules of the call-site whose file defines a
  //    matching instance. This catches the common case where the
  //    ThreatModelsFor instance lives beside the data declaration in another
  //    module imported here.
  for (const moduleName of ctxModule.imports.modules) {
    const r = helpers.resolveModuleFile(moduleName);
    if (!r) continue;
    const inst = r.module.instances.find(matches);
    if (inst) return { instance: inst.node, module: r.module };
  }
  return null;
}

// Build one synthesized threat-model / expected-vulnerability leaf.
//   bestGuessLabel : the source-text guess for the leaf (e.g. "mutualExclusionAttack")
//   idx            : 1-based index into the list (for the fallback label note)
//   note           : human-readable note
function threatModelLeaf(bestGuessLabel, note) {
  return {
    kind: "test",
    label: bestGuessLabel,
    source: "synthesized",
    testingInterface: true,
    role: "threat-model",
    pendingExpansion: false,
    note,
  };
}

// Synthesize the propRunActions* subtree.
//   applyNode : the full apply node (for line/file provenance)
//   args      : peeled args (may include type_application @Model + string label)
//   ctxModule : call-site module
//   ctx       : ExtractContext (for ctx.rel)
//   fnName    : the propRunActions* function name
//   helpers   : { resolveModuleFile, dynamicPlaceholder }
function synthesizePropRunActions(applyNode, args, ctxModule, ctx, fnName, helpers) {
  let model = null;
  let label = null;
  for (const a of args) {
    const m = A.typeAppName(a);
    if (m && model === null) model = m;
    const s = A.stringLiteralText(a);
    if (s !== null && label === null) label = s;
  }
  if (label === null) label = "property-based testing";

  const node = {
    kind: "group",
    label,
    source: "synthesized",
    testingInterface: true,
    pendingExpansion: false,
    note: `runtime-generated by ${fnName} (TestingInterface); shape/order known statically`,
    model,
    file: ctx.rel(ctxModule.file),
    line: A.lineOf(applyNode),
    children: [],
  };

  // R4: Positive tests + Negative tests are ALWAYS present, in this order,
  // BEFORE the threat-model / expected-vulnerability groups.
  node.children.push({
    kind: "test",
    label: "Positive tests",
    source: "synthesized",
    testingInterface: true,
    role: "positive",
    pendingExpansion: false,
    note: "positive property run (testProperty)",
  });
  node.children.push({
    kind: "test",
    label: "Negative tests",
    source: "synthesized",
    testingInterface: true,
    role: "negative",
    pendingExpansion: false,
    note: "negative property run (testProperty)",
  });

  const found = findThreatModelsForInstance(model, ctxModule, helpers);

  // expectedVulnerabilities has NO synthesized default (library default = []),
  // so we only ever read an explicit list for it.
  const expectedVulns = found
    ? readInstanceListBind(found.instance, "expectedVulnerabilities")
    : [];

  // threatModels: explicit list literal is used verbatim; otherwise the library
  // DEFAULT applies and the effective list is ALL_THREAT_MODELS with any entry
  // whose label also appears in expectedVulnerabilities removed (mirroring
  // `deleteFirstsBy eqName allThreatModels (expectedVulnerabilities @state)`).
  let threatModels = [];
  let threatModelsFromDefault = false;
  if (found) {
    const cls = classifyThreatModelsBind(found.instance);
    if (cls.kind === "explicit") {
      threatModels = cls.list;
    } else {
      threatModelsFromDefault = true;
      const removed = new Set(expectedVulns);
      threatModels = ALL_THREAT_MODELS.filter((tm) => !removed.has(tm));
    }
  }

  // "Threat models" group — ONLY if threatModels is non-empty.
  if (threatModels.length > 0) {
    const noteFor = (i) =>
      threatModelsFromDefault
        ? `from default allThreatModels set (mirror of Convex/ThreatModel/All.hs, minus expectedVulnerabilities); real rendered name comes from getThreatModelName (fallback "Threat model ${i + 1}")`
        : `best-guess label from source; real rendered name comes from getThreatModelName (fallback "Threat model ${i + 1}")`;
    node.children.push({
      kind: "group",
      label: "Threat models",
      source: "synthesized",
      testingInterface: true,
      role: "threat-models-group",
      pendingExpansion: false,
      children: threatModels.map((tmLabel, i) =>
        threatModelLeaf(tmLabel, noteFor(i))
      ),
    });
  }

  // "Expected vulnerabilities" group — ONLY if expectedVulnerabilities is
  // non-empty. Leaves are structurally identical to threat-model leaves, so
  // they use role=threat-model too; the group role carries the distinction.
  if (expectedVulns.length > 0) {
    node.children.push({
      kind: "group",
      label: "Expected vulnerabilities",
      source: "synthesized",
      testingInterface: true,
      role: "expected-vulnerabilities-group",
      pendingExpansion: false,
      children: expectedVulns.map((vLabel, i) =>
        threatModelLeaf(
          vLabel,
          `best-guess label from source; real rendered name comes from getThreatModelName (fallback "Expected vulnerability ${i + 1}")`
        )
      ),
    });
  }

  return node;
}

module.exports = {
  PROP_RUN_FNS,
  ALL_THREAT_MODELS,
  synthesizePropRunActions,
  readInstanceListBind,
  findInstanceBind,
  classifyThreatModelsBind,
  findThreatModelsForInstance,
};
