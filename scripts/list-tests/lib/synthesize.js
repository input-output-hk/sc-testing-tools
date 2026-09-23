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
//     ├─ "Threat models"                (group, iff threatModels non-empty)
//     ├─ "Surveyed threat models"       (group, iff candidateModels non-empty)
//     ├─ "Expected vulnerabilities"     (group, iff expectedVulnerabilities non-empty)
//     ├─ "Accepted findings"            (group, iff acceptedFindings non-empty)
//     └─ "Not applicable"               (group, iff notApplicable non-empty)
//          └─ <one leaf per slot element, role=threat-model, in list order>
//
// One group per non-empty slot of the ThreatModelsFor instance, each with the
// role named in list-tests.schema.json. `threatModels` defaults to [];
// `candidateModels` carries the default set (every model not spoken for by
// another slot). The last three slots hold (model, "reason") tuples, of which
// only the model half names a test case.
//
// All synthesized nodes carry source:"synthesized" and testingInterface:true.
// The whole subtree has been VERIFIED to match the real tasty tree in labels,
// counts, nesting and ORDER, so every synthesized node carries
// pendingExpansion:false: the shape is FULLY KNOWN statically. (Approximate leaf
// names are mere renames, not expansions, so they do not justify pending.)
//
// NOTE: leaves are structurally identical across all five groups (all rendered
// via getThreatModelName, all testCaseSteps). The leaf role is therefore
// `threat-model` everywhere; the semantic difference is carried by the parent
// group's role. There are NO per-slot leaf roles.

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
// Read a list-valued instance bind as labels. `unwrap` maps each list
// element to the node that names the model: identity for a plain list of
// models, and the tuple's first component for the triaged slots, which hold
// `(model, "reason")` pairs.
function readInstanceListBind(instanceNode, bindName, unwrap = (el) => el) {
  const target = findInstanceBind(instanceNode, bindName);
  if (!target) return [];
  const expr = A.bindExpression(target);
  if (!expr || expr.type !== "list") return [];
  return A.listElements(expr).map((el) => A.elementLabel(unwrap(el)));
}

// The triaged slots hold `(model, "reason")` tuples; only the model half
// names a test case. Anything that is not a tuple reads as a bare model,
// which keeps this working if a slot is ever simplified back.
const tupleHead = (el) =>
  (el.type === "tuple" ? el.namedChildren.filter((c) => c.type !== "comment")[0] : null) || el;

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

  // The three triaged slots hold (model, "reason") tuples; only the model
  // half names a test case.
  const expectedVulns = found
    ? readInstanceListBind(found.instance, "expectedVulnerabilities", tupleHead)
    : [];
  const acceptedFindings = found
    ? readInstanceListBind(found.instance, "acceptedFindings", tupleHead)
    : [];
  const notApplicable = found
    ? readInstanceListBind(found.instance, "notApplicable", tupleHead)
    : [];

  // threatModels: an explicit list is used verbatim; the library default is
  // now [] (a claim you have not made), NOT the whole set.
  // An absent binding, or an RHS we cannot read as a list, claims nothing —
  // which is the library default. Synthesizing the whole set here would
  // invent a coverage claim the instance may not make.
  const threatModels = found
    ? readInstanceListBind(found.instance, "threatModels")
    : [];

  // candidateModels carries the survey, and is where the default set lives
  // now: every model not already spoken for by another slot. Matching is
  // textual, mirroring `defaultThreatModelsExcluding` only approximately —
  // the library compares rendered names, so a parameterised override such as
  // `largeDataAttackWith 10` does not mask `largeDataAttack` here.
  let candidateModels = [];
  let candidatesFromDefault = false;
  if (found) {
    if (findInstanceBind(found.instance, "candidateModels")) {
      candidateModels = readInstanceListBind(found.instance, "candidateModels");
    } else {
      candidatesFromDefault = true;
      // Compare by head identifier: the library excludes by rendered name, so
      // `tokenForgeryAttack mp asset` and a bare `tokenForgeryAttack` are the
      // same model. (A `...With n` variant still reads as a different head,
      // which is the remaining gap.)
      const head = (label) => String(label).split(/\s+/)[0];
      const spokenFor = new Set(
        [...threatModels, ...expectedVulns, ...acceptedFindings, ...notApplicable].map(head)
      );
      candidateModels = ALL_THREAT_MODELS.filter((tm) => !spokenFor.has(head(tm)));
    }
  }

  // One group per non-empty slot, in the order the runner builds them.
  const slotGroup = (label, role, entries, fallbackName, note) => {
    if (entries.length === 0) return;
    node.children.push({
      kind: "group",
      label,
      source: "synthesized",
      testingInterface: true,
      role,
      pendingExpansion: false,
      children: entries.map((tmLabel, i) =>
        threatModelLeaf(
          tmLabel,
          `${note}; real rendered name comes from getThreatModelName (fallback "${fallbackName} ${i + 1}")`
        )
      ),
    });
  };

  slotGroup(
    "Threat models",
    "threat-models-group",
    threatModels,
    "Threat model",
    "best-guess label from source"
  );
  slotGroup(
    "Surveyed threat models",
    "surveyed-threat-models-group",
    candidateModels,
    "Candidate model",
    candidatesFromDefault
      ? "from the default candidateModels set (mirror of Convex/ThreatModel/All.hs, minus models spoken for by another slot)"
      : "best-guess label from source"
  );
  slotGroup(
    "Expected vulnerabilities",
    "expected-vulnerabilities-group",
    expectedVulns,
    "Expected vulnerability",
    "best-guess label from source"
  );
  slotGroup(
    "Accepted findings",
    "accepted-findings-group",
    acceptedFindings,
    "Accepted finding",
    "best-guess label from source"
  );
  slotGroup(
    "Not applicable",
    "not-applicable-group",
    notApplicable,
    "Not applicable",
    "best-guess label from source"
  );

  return node;
}

module.exports = {
  PROP_RUN_FNS,
  ALL_THREAT_MODELS,
  synthesizePropRunActions,
  readInstanceListBind,
  findInstanceBind,
  findThreatModelsForInstance,
};
