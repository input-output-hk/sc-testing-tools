---
name: testing-interface
description: Phase-aware entry to adding or maintaining a property-based on-chain test suite for a Cardano (Haskell + cabal + nix) contract using the convex-testing-interface framework. The skill probes the trial repo first and enters at one of six phases (Fresh, Setup-done, Implemented, Threat-models-wired, Green-maintenance, Red-repair) rather than walking a fixed linear tutorial. Handles initial setup, TestingInterface authoring, threat-model selection and wiring, and steady-state maintenance verbs (add action, extend model, swap threat model, triage failure).
version: 1.0.0
---

# Skill: testing-interface (phase-aware)

## 0. Reference rules — read first

The ONLY allowed source of truth for this framework is the branch
`chore/without-p` of `input-output-hk/sc-testing-tools`:

  https://github.com/input-output-hk/sc-testing-tools/tree/chore/without-p

- Do not read any other branch of that repo (`main`, etc.).
- Do not read repo history (`git log`, `git show`, commit URLs).
- Do not read or import from forks.
- If you clone it locally, pin to a single commit on `chore/without-p`
  and never `git checkout` / `git branch` away.

Encouraged: browse the example specs under
`src/testing-interface/test/` on that branch — Sample, Bounty,
AikenHelloWorld, AikenBank, AikenVesting, AikenSellNft,
AikenMultisigTreasury (v1/v2/v3), AikenTipJar (v1/v2),
AikenKingOfCardano, AikenLending, AikenPurchaseOffer, Scripts, Spec.
Note: none of these is a "pingpong" example.

### Forbidden git operations on the workspace repo

Allowed: `git status`, `git add`, `git commit`, `git diff` of currently
uncommitted work, `git diff --staged`.

Forbidden — these reveal history or other branches:
- `git log`, `git show`, `git reflog`, `git rev-list`
- `git diff <ref>`, `git diff <branch>..<branch>`
- `git checkout <ref>`, `git checkout <branch>`, `git switch`
- `git branch` (any form that lists or creates branches)
- `git stash` (any form)
- `git worktree`
- Reading `.git/` contents directly
- Any web/API call that fetches branch lists, commit lists, or PR
  history of the workspace repo

### Forbidden references

- Any URL on `github.com/input-output-hk/sc-testing-tools` whose path is
  not under `tree/chore/without-p/` or `blob/chore/without-p/` or
  `raw.githubusercontent.com/.../chore/without-p/`. No `main`, no other
  branches, no commit SHAs, no PR pages, no issues, no compare views.
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

## 1. What this skill does

Guides an agent through installing, authoring, and maintaining a
`TestingInterface` property-test suite for a Cardano contract. The skill
is a **small state machine**: a phase-0 probe classifies the trial repo
into one of six phases, and the skill routes to the reference file and
step list for that phase. It works equally well for a first-time
onboard and for a returning session that wants one focused maintenance
verb. The main agent plans and confirms; every read, edit, build, and
test run is delegated to a subagent.

## 2. Phase-0 probe

The very first thing the agent does. Spawn ONE short subagent that runs
the checks listed in `references/00-phase-detection.md`, cheapest first:

1. Is `.skill-state.md` present? Read its `current phase` hint.
2. Grep `cabal.project` for the `sc-testing-tools` source-repo stanza on
   `chore/without-p`.
3. Grep `.cabal` test stanzas for `convex-testing-interface` dep.
4. Grep `test/**/*.hs` for `import Convex.TestingInterface`.
5. Grep for `instance TestingInterface` definitions.
6. Grep for `instance ThreatModelsFor` definitions.
7. `cabal build --dry-run` the test target only if the state file is
   absent or stale (last build hint older than the most-recent edit).

The subagent returns a phase label per the mapping table in
`references/00-phase-detection.md`. The main agent does not re-read
those files; it trusts the classification and moves to §3.

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

### Fresh

- **Detection.** No `sc-testing-tools` cabal pin; no `Convex.TestingInterface`
  import; no `.skill-state.md`.
- **Entry.** If user signals unfamiliarity (asks "what does this skill do",
  or repo shows no prior `convex-testing-interface` usage), load
  `01-onboarding-blueprint` and personalise. Otherwise skip the intro.
- **Steps.** discover contract → sketch state model →
  `02-project-setup` (cabal pin, test stanza) → wire class skeleton →
  implement one action at a time, building between each →
  `06-threat-models` (pick from menu, wire `ThreatModelsFor`) →
  run `propRunActions` → commit only on user request.
- **Exits.** → Setup-done after cabal pin + test stanza compile;
  → Threat-models-wired after first green run; → abort on user decline.

### Setup-done

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

- **Detection.** `TestingInterface` instance compiles; no
  `ThreatModelsFor` instance.
- **Entry.** Load `06-threat-models`.
- **Steps.** walk decision tree → propose model selection → confirm with
  user → wire `ThreatModelsFor` → rebuild → run.
- **Exits.** → Threat-models-wired on green; → Red-repair on failure.

### Threat-models-wired

- **Detection.** Both instances present, last build green, tests ran at
  least once (state file records a green run).
- **Entry.** Brief confirmation; usually transitions immediately.
- **Steps.** optional first interpretation of QuickCheck output, sanity
  check shrinking, suggest first maintenance verb.
- **Exits.** → Green-maintenance.

### Green-maintenance

- **Detection.** Both instances present, last build + test run green,
  user is back for a focused change.
- **Entry.** Ask user which maintenance verb. Do exactly one verb per
  invocation, then stop.
- **Maintenance verbs** (one per invocation):
  - **Add action** — extend `Action` GADT; update `arbitraryAction`,
    `precondition`, `perform`. Ref: `05-generators-and-implementation`.
  - **Extend model** — add a state field; update `initialize` and
    every `perform` branch. Ref: `03-testing-interface-class` +
    `05-generators-and-implementation`.
  - **Add threat model** — append to `threatModels`; note whether it
    newly fires. Ref: `06-threat-models`.
  - **Swap threat model** — replace one entry with another. Ref:
    `06-threat-models`.
  - **Investigate skipped case** — locate `precondition` returning
    False excessively; propose tweak; do not auto-apply. Ref:
    `05-generators-and-implementation`.
  - **Triage failure** — classify (model bug vs contract bug vs
    flaky generator); propose smallest reproducer; do not auto-fix
    unless obvious. Ref: `05-generators-and-implementation`
    (+ `06-threat-models` if threat model failed).
  Always rebuild + rerun after the verb; update state file.
- **Exits.** → Red-repair on any failure; stays in Green-maintenance
  otherwise.

### Red-repair

- **Detection.** Last build or test failed, or current change broke a
  previously green suite.
- **Entry.** Read `.skill-state.md` failure note; then a subagent
  reads only the failing module and the immediately relevant reference.
- **Steps.** localise (build error vs property counterexample vs
  threat-model counterexample) → minimal fix → rebuild → rerun.
- **Exits.** → Green-maintenance on green; stays in Red-repair otherwise.

## 5. Dashboard notepad protocol

The agent maintains `.skill-state.md` at the trial-repo root. It is
gitignored, ≤60 lines, and has a FIXED section order. Subagents update
it at step boundaries; the main agent only reads it.

**When to read**: at the start of every session, after every subagent
return.

**When to update**: only via subagent. Every subagent returns a
`state-file diff` block (see §6) which is the patch to apply.

**Schema** (fixed section order):

```markdown
# current phase
<phase-label> <ISO-date>
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

If the file grows past 60 lines, a subagent trims oldest `files touched`
and `decisions log` entries.

Two sibling docs live alongside: `contract-sketch.md` (free-form bullets
about validators, datums, redeemers, invariants) and `model-decisions.md`
(append-only bullet log of model choices and why). All three are
gitignored.

## 6. Delegation rules

**Main agent** never edits files, never builds, never reads source code.
It only reads the three state documents in §5 and plans / confirms /
dispatches.

**Subagents** do everything else: read source modules, edit cabal files,
write Haskell, run `cabal build`, run `cabal test`.

**Subagent prompt template (mandatory parts):**

1. The entire text of §0 above, pasted verbatim.
2. The current phase label.
3. The one task for this subagent.
4. The state-file update the subagent must perform on completion.

**Subagent return contract — four parts, exactly:**

1. **Outcome** — one line: `success` | `failure` | `blocked`.
2. **Changed** — up to 5 bullets listing files changed or commands run.
3. **Blocker** — one line, or `none`.
4. **State-file diff** — the lines added/removed in `.skill-state.md`.

No raw `cabal` output is returned. The subagent summarises. The main
agent must not re-read files the subagent already summarised in the same
step (**no-reread rule**). At every step boundary the main agent
compresses prior tool chatter into one paragraph and continues from the
state file.

## 7. Question budget

No fixed quota. Ask only when a decision is genuinely ambiguous and the
answer changes the next step.

- Do not ask if a sensible default exists and is documented in a
  reference file.
- Do ask before introducing a new dependency, before rewriting the model,
  before deleting tests, and before committing.
- Batch related questions into one message.
- Phrase as a closed choice (A/B/C) when possible.
- Never ask the same question twice in a session; if the answer is in
  `.skill-state.md` or `model-decisions.md`, use it.

## 8. Reference index

- `00-phase-detection.md` — probe checklist + phase mapping. Loaded first.
- `01-onboarding-blueprint.md` — conditional framing for Fresh when user is unfamiliar.
- `02-project-setup.md` — cabal pin, test stanza, Spec.hs template; Setup-done verification.
- `03-testing-interface-class.md` — class methods, superclass derivings, orphan guidance.
- `04-helpers-cheatsheet.md` — `tryBalanceAndSubmit`, `TrailingChange`, imports, per-attack import gotcha.
- `05-generators-and-implementation.md` — the triad, beam metaphor, eight-point checklist.
- `06-threat-models.md` — 18 attacks + 8 variants, decision tree, wiring, `expectedVulnerabilities`.

## 9. Out of scope

- Writing custom threat models (the menu in `06-threat-models` is the
  full surface this skill exposes).
- `RunOptions` tuning, `monitoring` overrides, `validate` post-step
  invariant checks.
- Plutus script coverage via `withCoverage` / `CoverageConfig` / HPC.
- VS Code or other editor extension wiring.
- Multi-package monorepo orchestration beyond a single trial repo.
- Migration tooling from older test suites.

If the user asks for any of these, name the boundary and stop.
