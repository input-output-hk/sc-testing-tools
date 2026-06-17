---
name: testing-interface
description: Phase-aware entry to adding or maintaining a property-based on-chain test suite for a Cardano (Haskell + cabal + nix) contract using the convex-testing-interface framework, with the cardinal rule that deployment is always an action and initialize is always model-only. The skill probes the trial repo first and enters at one of six phases (Fresh, Setup-done, Implemented, Threat-models-wired, Green-maintenance, Red-repair) rather than walking a fixed linear tutorial. Core mental model is two test channels (positive + negative) driven by a model-chain mirror, where the test unit is a ROUND of actions: positive rounds where every action passes, negative rounds where the last action probes a guard. The generator reads the model state to branch on STRUCTURAL feasibility (can I build this Action at all?), never on semantic validity (does the contract accept it?) — that goes in precondition. Declarative threat models layer on top as shadow/parallel-world attacks. Handles initial setup, TestingInterface authoring, threat-model selection and wiring, and steady-state maintenance verbs (add action, extend model, swap threat model, triage failure). Mandates concept-level Haskell comments above every TestingInterface function.
version: 2.0.4
---

# Skill: testing-interface (phase-aware)

## 0. Reference rules — read first

The source of truth for this framework is the `main` branch of
`input-output-hk/sc-testing-tools`:

  https://github.com/input-output-hk/sc-testing-tools/tree/main

- Do not read or import from forks.
- If you clone it locally, work from `main`.

Encouraged: browse the example specs under
`src/testing-interface/test/` on `main` — Sample, Bounty,
AikenHelloWorld, AikenBank, AikenVesting, AikenSellNft,
AikenMultisigTreasury (v1/v2/v3), AikenTipJar (v1/v2),
AikenKingOfCardano, AikenLending, AikenPurchaseOffer, Scripts, Spec.
Note: none of these is a "pingpong" example.

### Forbidden references

- Any URL on a fork of `github.com/input-output-hk/sc-testing-tools`
  (or any fork of `github.com/input-output-hk/sc-tools`).
- Any fork or mirror of either repo.
- Any locally-cloned copy of either repo on disk other than via the
  `source-repository-package` cabal mechanism.

### Propagation to subagents

When you delegate any task to a subagent, the first thing in the
subagent's prompt MUST be the entire text of this §0 (from "## 0.
Reference rules — read first" through the end of this propagation
paragraph), pasted verbatim. No paraphrasing. No summarising. No "see
the skill". The rules do not transfer through skill loading; they
transfer through prompt text. This is a hard obligation, not a
suggestion.

## 0.5 Cardinal rule

Overrides every other design suggestion in this skill, every reference
file, every example. If anything contradicts it, this rule wins.

> **CARDINAL RULE — Deployment is an action, not setup.**
>
> Deploying the contract on-chain is THE initial action (or one of several, if multiple deployment shapes are tested). It MUST be part of the Action type — typically named `Start`, `Deploy`, `StartWithInlineDatum`, etc.
>
> `initialize` MUST be model-only: it sets up the in-memory bookkeeping and returns a zero state with `modelInitialized = False` (or equivalent flag). It MUST NOT submit any transaction, deploy any script, or touch the chain.
>
> The generator reads the model state and decides whether to emit a deploy action (when uninitialised) or a normal action (when initialised).
>
> If a proposed design has `initialize` deploying the contract, or has an Action type without a deploy action, the design is WRONG. STOP. Do not write code. Do not proceed. Re-read this rule.
>
> Every subagent dispatched from this skill MUST receive this rule verbatim in its prompt.

## 1. What this skill does

REMINDER: see §0.5 Cardinal rule. Deployment is an action.
`initialize` is model-only. No exceptions.

Guides an agent through installing, authoring, and maintaining a
`TestingInterface` property-test suite for a Cardano contract. The skill
is a **small state machine**: a phase-0 probe classifies the trial repo
into one of six phases, and the skill routes to the reference file and
step list for that phase. It works equally well for a first-time
onboard and for a returning session that wants one focused maintenance
verb. The main agent plans and confirms; every read, edit, build, and
test run is delegated to a subagent.

### 1.1 The two channels (core mental model)

A `TestingInterface` suite drives the contract through **two channels
at once**:

1. **Positive channel.** Generator emits an action, precondition=True,
   `perform` submits, framework asserts the chain accepts.
2. **Negative channel.** Generator emits the same shape, precondition=
   False, `perform` still submits, framework asserts the chain rejects.

Threat models are a **third, separate attack channel** layered on the
positive channel — declarative tweaks to already-valid txs, distinct
from negative testing.

Model↔chain mirror: model decides validity (precondition); chain
enforces for real; the two must agree. **The generator must not gate
on semantic validity** — if it filters to only valid actions, the
negative channel starves silently. (The generator IS allowed to read
the model for *structural* feasibility; see §1.2 and
`references/05-generators-and-implementation.md`.)

### 1.2 Test unit: rounds, not single actions

The framework's test unit is a **round** (trace) of actions
`r = [a₁, …, aₙ]`, in two shapes:

- **Positive round.** Every action passes its precondition and the
  chain accepts each in sequence.
- **Negative round.** First N−1 are positive (precondition True, chain
  accepts); the **last** is negative (precondition False, chain MUST
  reject). The negative tail proves the contract guards against bad
  inputs given a real history.

```
Positive round:   [a1✓, a2✓, a3✓, a4✓]     all accepted
Negative round:   [a1✓, a2✓, a3✓, BAD✗]    last must be rejected
                  └────positive tail────┘ └── attack tail ──┘
```

The negative channel doesn't generate failing actions in isolation —
it generates a valid history then probes one bad action at the end.
Traces, not actions, are the unit of testing.

## 2. Phase-0 probe

REMINDER: see §0.5 — even the probe subagent receives the rule
verbatim in its dispatch prompt.

The first thing the agent does. Spawn ONE short subagent running the
checks listed in `references/00-phase-detection.md`, cheapest first:

1. `.skill-state.md` present? Read its `current phase` hint.
2. Detect build wrapper (`flake.nix` → nix; else cabal).
3. Grep `cabal.project` for the `sc-testing-tools` source-repo stanza
   on `main`.
4. Grep `.cabal` test stanzas for `convex-testing-interface` dep.
5. Grep `test/**/*.hs` for `import Convex.TestingInterface`.
6. Grep for `instance TestingInterface` and `instance ThreatModelsFor`.
7. `cabal build --dry-run` the test target only if the state file is
   absent or stale.

The subagent returns a phase label per the mapping table in
`references/00-phase-detection.md`. The main agent trusts the
classification and moves to §3.

## 3. Phase routing table

| Phase | Reference files | Entry section |
|---|---|---|
| Fresh | `01-onboarding-blueprint`, `02-project-setup`, `03-testing-interface-class`, `05-generators-and-implementation`, `06-threat-models` | §4 Fresh |
| Setup-done | `02-project-setup` (verify only), `03-testing-interface-class`, `05-generators-and-implementation` | §4 Setup-done |
| Implemented | `06-threat-models` | §4 Implemented |
| Threat-models-wired | (none) | §4 Threat-models-wired |
| Green-maintenance | per maintenance verb (see §4 Green-maintenance) | §4 Green-maintenance |
| Red-repair | the module that broke + the reference for the broken thing | §4 Red-repair |

Helpers cheatsheet (`04-helpers-cheatsheet.md`) is loaded ad-hoc by any
subagent writing Haskell. The main agent never loads it directly.

## 4. Phases

REMINDER: see §0.5 Cardinal rule — every phase, every subagent.

### Fresh

REMINDER: see §0.5 Cardinal rule.

- **Detection.** No `sc-testing-tools` cabal pin; no `Convex.TestingInterface`
  import; no `.skill-state.md`.
- **Hard intro gate.** Fresh phase ONLY: before any other action, ask
  exactly once:

  > "New to convex-testing-interface, or just need me to get on with it?"

  Options (exact wording):
  - `Quick intro first` — load `01-onboarding-blueprint`, paraphrase
    tailored to the discovered contract.
  - `Skip, just work` — straight to bootstrap; skip the blueprint.
  - `Figure it out` — agent decides from signals (user statements,
    repo state, prior `convex-testing-interface` usage anywhere).

  No other phase offers this intro. Record the choice in
  `.skill-state.md` decisions log.
- **Entry.** Branch on the gate. If `Quick intro first` (or
  `Figure it out` → unfamiliar), load `01-onboarding-blueprint`.
- **Steps.** discover contract → sketch state model →
  `02-project-setup` (cabal pin, test stanza) → wire class skeleton →
  implement one action at a time, building between each →
  `06-threat-models` (pick from menu, wire `ThreatModelsFor`) →
  run `propRunActions` → commit only on user request.
- **HARD GATE — before writing any code, verify:**
  - [ ] The Action type includes at least one deploy/Start action.
  - [ ] `initialize` does NOT call any function that submits a tx or
        deploys a script.
  - [ ] The generator branches on `modelInitialized` (or equivalent)
        to emit the deploy action when uninitialised.

  If any checkbox is unticked, the design violates the Cardinal Rule
  (§0.5). STOP. Revise. Re-confirm with the user. Do NOT delegate to a
  subagent until all three boxes are ticked.
- **Exits.** → Setup-done after cabal pin + test stanza compile;
  → Threat-models-wired after first green run; → abort on user decline.

### Setup-done

REMINDER: see §0.5 — `initialize` stays model-only; Action type
includes a deploy variant.

- **Detection.** Cabal pin present, test target builds, no
  `TestingInterface` instance exists yet.
- **Entry.** Subagent verifies `02-project-setup` is current (pin tag
  matches, both subdirs listed); then load `03-testing-interface-class`
  and `05-generators-and-implementation`.
- **Steps.** discover contract (if `contract-sketch.md` missing) → sketch
  model → wire skeleton → implement actions one-at-a-time → build green.
- **Exits.** → Implemented once instance typechecks and `propRunActions`
  builds; → Fresh only if cabal pin reverted.

### Implemented

REMINDER: see §0.5 — before threat models, re-verify `initialize` is
model-only and a deploy Action exists.

- **Detection.** `TestingInterface` instance compiles; no
  `ThreatModelsFor` instance.
- **Entry.** Load `06-threat-models`.
- **Steps.** walk decision tree → propose model selection → confirm with
  user → wire `ThreatModelsFor` → rebuild → run.
- **Exits.** → Threat-models-wired on green; → Red-repair on failure.

### Threat-models-wired

REMINDER: see §0.5 Cardinal rule.

- **Detection.** Both instances present, last build green, tests ran at
  least once (state file records a green run).
- **Entry.** Brief confirmation; usually transitions immediately.
- **Steps.** optional first interpretation of QuickCheck output, sanity
  check shrinking, suggest first maintenance verb.
- **Exits.** → Green-maintenance.

### Green-maintenance

REMINDER: see §0.5 — maintenance verbs MUST NOT migrate deploy logic
into `initialize`. If asked, refuse and re-state the rule.

- **Detection.** Both instances present, last build + test run green,
  user is back for a focused change.
- **Entry.** Ask user which maintenance verb. Do exactly one verb per
  invocation, then stop.
- **Maintenance verbs** (one per invocation):
  - **Add action** — extend `Action` GADT; update `arbitraryAction`,
    `precondition`, `perform`. Ref: `05-generators-and-implementation`.
  - **Extend model** — add a state field; update `initialize` and every
    `perform` branch. Ref: `03-testing-interface-class` + `05-...`.
  - **Add threat model** — append to `threatModels`; note whether it
    newly fires. Ref: `06-threat-models`.
  - **Swap threat model** — replace one entry. Ref: `06-threat-models`.
  - **Investigate skipped case** — locate `precondition` returning False
    excessively; propose tweak; don't auto-apply. Ref: `05-...`.
  - **Triage failure** — classify (model vs contract vs flaky
    generator); propose smallest reproducer; don't auto-fix unless
    obvious. Ref: `05-...` (+ `06-...` if threat model failed).
  Always rebuild + rerun after the verb; update state file.
- **Exits.** → Red-repair on any failure; stays in Green-maintenance
  otherwise.

### Red-repair

REMINDER: see §0.5 — if `initialize` does on-chain work or there is no
deploy Action, fix that first, before any other repair.

- **Detection.** Last build or test failed, or a change broke a
  previously green suite.
- **Entry.** Read `.skill-state.md` failure note; subagent reads only
  the failing module + the immediately relevant reference.
- **Steps.** localise (build error vs property counterexample vs
  threat-model counterexample) → minimal fix → rebuild → rerun.
- **Exits.** → Green-maintenance on green; stays otherwise.

## 5. Dashboard notepad protocol

`.skill-state.md` at the trial-repo root: gitignored, ≤60 lines, FIXED
section order. Subagents update at step boundaries; main agent only
reads it. **Read** at session start and after every subagent return.
**Update** only via subagent — each returns a `state-file diff` (see
§6) which is the patch to apply.

**Schema** (fixed section order):

```markdown
# current phase
<phase-label> <ISO-date>
build-wrapper: <nix | cabal>
## user profile
<author | auditor | explorer | unknown>
## contract sketch
<relative path to contract-sketch.md>
## files touched
- <path>
- <path>
## last build
green <ISO-date>     # or: red <ISO-date>: <one-line reason>
## decisions log
- <one-line decision>
- <one-line decision>
## open question
<single line, or empty>
```

`build-wrapper:` is set by the phase-0 probe (`00-phase-detection.md`):
`nix` = route build/test through `nix develop -c …`; `cabal` = invoke
cabal directly. If the file grows past 60 lines, a subagent trims
oldest `files touched` and `decisions log` entries.

Two sibling docs live alongside: `contract-sketch.md` (validators,
datums, redeemers, invariants) and `model-decisions.md` (append-only
log of model choices and why). All three are gitignored.

## 6. Delegation rules

Cardinal Rule (§0.5), restated verbatim — MUST be pasted into every
subagent prompt:

> **CARDINAL RULE — Deployment is an action, not setup.**
>
> Deploying the contract on-chain is THE initial action (or one of several, if multiple deployment shapes are tested). It MUST be part of the Action type — typically named `Start`, `Deploy`, `StartWithInlineDatum`, etc.
>
> `initialize` MUST be model-only: it sets up the in-memory bookkeeping and returns a zero state with `modelInitialized = False` (or equivalent flag). It MUST NOT submit any transaction, deploy any script, or touch the chain.
>
> The generator reads the model state and decides whether to emit a deploy action (when uninitialised) or a normal action (when initialised).
>
> If a proposed design has `initialize` deploying the contract, or has an Action type without a deploy action, the design is WRONG. STOP. Do not write code. Do not proceed. Re-read this rule.
>
> Every subagent dispatched from this skill MUST receive this rule verbatim in its prompt.

**Main agent** never edits files, builds, or reads source code. It
only reads the three state docs in §5 and plans / confirms / dispatches.

**Subagents** do everything else: read source, edit cabal files, write
Haskell, run `cabal build`, run `cabal test`.

**Subagent prompt template (mandatory parts):**

1. The entire text of §0 above, pasted verbatim.
2. The entire §0.5 Cardinal Rule blockquote above, pasted verbatim.
   Same propagation model as §0 — rule transfers through prompt text,
   not skill loading. No paraphrasing. A prompt missing §0.5 is
   malformed; rewrite before dispatch.
3. The current phase label.
4. The one task for this subagent.
5. The state-file update the subagent must perform on completion.

**Subagent return contract — four parts, exactly:**

1. **Outcome** — one line: `success` | `failure` | `blocked`.
2. **Changed** — up to 5 bullets listing files changed or commands run.
3. **Blocker** — one line, or `none`.
4. **State-file diff** — the lines added/removed in `.skill-state.md`.

No raw `cabal` output is returned; the subagent summarises. The main
agent does not re-read files the subagent already summarised in the
same step (**no-reread rule**). At every step boundary the main agent
compresses prior tool chatter into one paragraph and continues from the
state file.

### 6.1 Implementation hygiene — mandatory commenting

Every `TestingInterface` function (and `threatModels` in
`ThreatModelsFor`) MUST carry a Haskell comment block explaining the
**concept** — not the syntax, not a paraphrase. Scope: `initialize`,
`arbitraryAction`, `precondition`, `perform`, `threatModels`, plus a
per-clause one-liner inside `precondition` stating the *rule*. Subagent
prompts MUST include this rule alongside §0 and §0.5 and reference the
templates in `references/05-generators-and-implementation.md §11`.
Explain the *why* (two-channel / model-chain / shadow-attack /
cardinal rule), not the *what*. A function lacking its concept comment
is incomplete work.

### 6.2 Progress tracking (main agent only)

- If the main/orchestrator agent has a TODO-list tool (`mcp_todowrite`
  or equivalent), it MUST track phase steps and substeps. Mark
  `in_progress` on start, `completed` on finish; only one
  `in_progress` at a time. Subagents do NOT touch the TODO list.
- If no such tool is available, skip silently.

## 7. Question budget

The agent talks to the human **directly in the chat**. No file-based
handoff (no REPORT.md, no ANSWERS.md). Ask in chat when genuinely
stuck. No fixed quota. Ask only when a decision is genuinely ambiguous
and the answer changes the next step.

- Do not ask if a sensible default is documented in a reference file.
- Do ask before a new dependency, rewriting the model, deleting tests,
  or committing.
- Batch related questions; phrase as a closed choice when possible.
- Never ask twice in a session; if the answer is in `.skill-state.md`
  or `model-decisions.md`, use it.

### 7.1 The `Figure it out` rule (hard)

Every multiple-choice question presented to the human MUST include a
`Figure it out` option with this exact wording:

- **label:** `Figure it out`
- **description:** `Decide this one yourself based on what you've
  found. Future questions still on the table.`

Don't paraphrase the label or description.

**Exception.** Questions that fundamentally require the human —
identity, ownership, intent — skip the option. E.g. "Is this your
contract or are you auditing?" / "Should I commit?". If in doubt,
include `Figure it out`.

## 8. Reference index

- `00-phase-detection.md` — probe checklist + phase mapping. Loaded first.
- `01-onboarding-blueprint.md` — conditional framing for Fresh when user is unfamiliar.
- `02-project-setup.md` — cabal pin, test stanza, Spec.hs template; Setup-done verification.
- `03-testing-interface-class.md` — class methods, superclass derivings, orphan guidance.
- `04-helpers-cheatsheet.md` — `tryBalanceAndSubmit`, `TrailingChange`, imports, per-attack import gotcha.
- `05-generators-and-implementation.md` — the triad, beam metaphor, eight-point checklist.
- `06-threat-models.md` — 18 attacks + 8 variants, decision tree, wiring, `expectedVulnerabilities`.

## 9. Out of scope

- Custom threat models (`06-threat-models` menu is the full surface).
- `RunOptions` tuning, `monitoring` overrides, `validate` post-step checks.
- Plutus script coverage (`withCoverage` / `CoverageConfig` / HPC).
- Editor extension wiring; multi-package monorepo orchestration beyond
  a single trial repo; migration tooling from older test suites.

If the user asks for any of these, name the boundary and stop.