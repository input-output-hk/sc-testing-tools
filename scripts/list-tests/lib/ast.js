// Low-level tree-sitter-haskell AST helpers.
//
// Encapsulates the verified node-type facts (see plan issue-63) so the rest of
// the tool is insulated from grammar quirks:
//   - application = `apply` (curried; function:/argument:)
//   - string literal = `literal` > `string` (.text INCLUDES quotes)
//   - list `[...]` = `list` (items under `element:`)
//   - `$`/operators = `infix` (left_operand/operator/right_operand)
//   - type application `@Model` = `type_application` (field type:)
//   - qualified name = `qualified` (module: has TRAILING dot; id:)
//   - top-level def: `bind` (no params) / `function` (with params); both expose
//     match -> expression RHS.

// Safe childForFieldName.
function nodeField(node, field) {
  return node && node.childForFieldName ? node.childForFieldName(field) : null;
}

// Peel a curried `apply` into { head, args } in source order.
// `head` is the innermost function node (variable / qualified / etc.).
//
// tree-sitter-haskell represents `f a b c` as
//   (apply function: (apply function: (apply function: (variable f) argument: a) ...))
// i.e. the outermost apply holds the LAST argument.
function peelApply(node) {
  const args = [];
  let cur = node;
  while (cur && cur.type === "apply") {
    const arg = cur.childForFieldName("argument");
    if (arg) args.push(arg);
    cur = cur.childForFieldName("function");
  }
  args.reverse();
  return { head: cur, args };
}

// Name of an application head if it's a plain or qualified variable, else null.
function headVariableName(node) {
  if (!node) return null;
  if (node.type === "variable") return node.text;
  if (node.type === "qualified") {
    const id = node.childForFieldName("id");
    if (id) return id.text;
  }
  return null;
}

// Strip a single trailing dot (qualified.module text carries one).
function stripTrailingDot(s) {
  return s ? s.replace(/\.$/, "") : s;
}

// For a reference node (variable / qualified / parens / apply-with-var-head),
// return { name, module|null } describing the referenced binding, or null.
function refTarget(node) {
  if (!node) return null;
  if (node.type === "variable") return { name: node.text, module: null };
  if (node.type === "qualified") {
    const id = node.childForFieldName("id");
    const mod = node.childForFieldName("module");
    if (!id) return null;
    return {
      name: id.text,
      module: mod ? stripTrailingDot(mod.text) : null,
    };
  }
  if (node.type === "parens") {
    return refTarget(node.childForFieldName("expression"));
  }
  if (node.type === "apply") {
    const { head } = peelApply(node);
    return refTarget(head);
  }
  return null;
}

// Strip surrounding double-quotes from a Haskell string literal's text.
function stripQuotes(s) {
  if (s.length >= 2 && s[0] === '"' && s[s.length - 1] === '"') {
    return s.slice(1, -1);
  }
  return s;
}

// String-literal text (unquoted) for a node, else null.
// String literal = `literal` wrapping `string`.
function stringLiteralText(node) {
  if (!node) return null;
  if (node.type === "literal") {
    const inner = node.namedChildren.find((c) => c.type === "string");
    if (inner) return stripQuotes(inner.text);
  }
  if (node.type === "string") return stripQuotes(node.text);
  return null;
}

// Type-application argument `@Model` -> model name text, else null.
function typeAppName(node) {
  if (node && node.type === "type_application") {
    const t = node.childForFieldName("type");
    if (t) return t.text.trim();
  }
  return null;
}

// The `match expression:` RHS of a bind/function decl. For multi-clause
// functions only the first clause's match is read (acceptable for Tier 1).
function bindExpression(node) {
  if (!node) return null;
  const match = node.childForFieldName("match");
  if (match) {
    const expr = match.childForFieldName("expression");
    if (expr) return expr;
  }
  return null;
}

// First `list` node among args (the children list of a testGroup).
function findListArg(args) {
  return args.find((a) => a && a.type === "list") || null;
}

// Items of a list node, ignoring comments.
function listElements(listNode) {
  return listNode.namedChildren.filter((c) => c.type !== "comment");
}

// 1-based source line of a node.
function lineOf(node) {
  return node.startPosition.row + 1;
}

// Produce a readable label from a list-element expression:
//   `largeValueAttackWith 10`           -> "largeValueAttackWith 10"
//   `tokenForgeryAttack mp asset`       -> "tokenForgeryAttack mp asset"
//   bare `mutualExclusionAttack`        -> "mutualExclusionAttack"
function elementLabel(node) {
  if (node.type === "apply") {
    const { head, args } = peelApply(node);
    const headName = headVariableName(head) || head.text.trim();
    const argTexts = args.map((a) => a.text.trim());
    return [headName, ...argTexts].join(" ");
  }
  return node.text.trim();
}

module.exports = {
  nodeField,
  peelApply,
  headVariableName,
  stripTrailingDot,
  refTarget,
  stripQuotes,
  stringLiteralText,
  typeAppName,
  bindExpression,
  findListArg,
  listElements,
  lineOf,
  elementLabel,
};
