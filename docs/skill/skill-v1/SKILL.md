---
name: testing-interface
description: Use this skill when the user wants to add a property-based on-chain test suite to a Cardano (Haskell + cabal + nix) contract project using the convex-testing-interface framework. Covers project discovery, modeling the contract, writing the TestingInterface instance (Action, initialize, arbitraryAction, precondition, perform), selecting and wiring built-in threat models, and wiring the test-suite build.
version: 0.3.0
---

# Skill: Add a `TestingInterface` property-test suite to a Cardano contract

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

## 1. Purpose

This skill guides you (a Claude agent) to add a `TestingInterface`-based
property-test suite to an existing Cardano contract project. The skill is
**adaptive**: you do not assume a project shape. You discover what the user
has, confirm with them, then adapt. You do not write threat models in
this version of the skill.

## 2. When to use this skill

- The user wants property-based on-chain tests for their Cardano contract.
- Their project is Haskell + cabal + nix-flake based (these are the only hard
  project assumptions).
- They have at least one Plutus or Aiken validator script.

If any of these is unclear, ask the user before doing anything.

## 3. High-level workflow — adaptive

The general pattern at every step: observe, propose, confirm with the user,
then act. Do not chain multiple irreversible writes ahead of confirmation.
"Adaptive" means: if your assumption about the project differs from
reality, ask before writing.

### Step A — Discover

Survey the user's project. Look for, but do not assume:

- The Cabal project file (`cabal.project`).
- The flake (`flake.nix`) and any `nix/*.nix` files.
- All package descriptors (`*.cabal`). Note all of them — multi-package
  projects are common.
- Validator scripts: `.hs` files referencing `PlutusTx.compile`,
  `PlutusLedgerApi.V*`, or `.ak` files (Aiken). Look in `lib/`, `src/`,
  `validators/`, `onchain/`, or wherever they happen to live.
- Off-chain tx-building helpers: `.hs` files importing `Convex.BuildTx`,
  `Cardano.Api`, or producing `TxBodyContent BuildTx ...`.
- Any existing test directory (`test/`, `tests/`, or per-package).

Do **not** assume a layout. See `references/01-project-setup.md` §B for the
discovery checklist.

### Step B — Confirm with the user

Summarise what you found and ask the user to confirm, in the spirit of:

> I found the following files: ...
> I believe X is the validator under test and Y and Z are the off-chain
> helpers. Is this correct? Are there contracts in this project I should NOT
> test? Where should the new test files live?

Get the user's answers before continuing. **Do not guess.** Discovery is
observation; deciding what to test is the user's choice.

### Step C — Map the contract domain in plain language

Before touching any Haskell, share your understanding with the user:

> Based on the validator and helpers, my understanding of this contract is:
> - States: ...
> - Parties: ...
> - Actions a user can take: ...
> - Rules / invariants the validator enforces: ...
>
> Please correct anything I have wrong.

This step is critical. An incorrect mental model here poisons everything
downstream — your generator will produce nonsense, your precondition will
disagree with reality, and your tests will silently test nothing. Get
confirmation before moving on.

### Step D — Propose the model and actions

Convert the user-confirmed mental model into a Haskell sketch:

> I propose:
>
> ```haskell
> data ContractModel = ContractModel { ... } deriving (Eq, Show, Generic)
>
> data Action ContractModel
>   = A1 ...
>   | A2 ...
> ```
>
> Reasoning: ...
>
> Questions: ...

Ask the user to confirm. Do not start writing the `TestingInterface`
instance until they have agreed on the model shape and the action
constructors.

### Step E — Implement `TestingInterface` — minimum

Implement `Action`, `initialize`, `arbitraryAction`, `precondition`,
`perform`. The `ThreatModelsFor` instance has `threatModels = []` and
`expectedVulnerabilities = []`.

- See `references/02-testing-interface-class.md` for the API.
- See `references/04-generator-precondition-perform.md` for the conceptual
  heart of this skill.

**READ 04 BEFORE WRITING THE GENERATOR.** The triad is the single most
common source of silent test failures.

### Step E2 — Select and wire threat models

With the positive/negative suite passing, populate the `ThreatModelsFor`
instance using the decision tree in `references/05-threat-models.md`
§C. Walk the user through your selection in your usual confirmation
loop before writing.

- Leave `expectedVulnerabilities = []` unless the user explicitly asks
  for the inverted semantics (see §E of the threat-models reference).
- After populating, re-run `cabal test`. Each threat model becomes its
  own test case in the tree (`Threat model: <name>`). Expect: each
  passes (= attack rejected). If any FAIL, surface the counter-example
  to the user — that is a real vulnerability finding, not a test bug.

### Step F — Set up build wiring

Add a test-suite stanza if needed; ensure `cabal.project` has the required
`source-repository-package` entries for `sc-tools` and `sc-testing-tools`.
See `references/01-project-setup.md`.

**Ask before modifying any existing config file.** Show the user the diff
you propose. For a `cabal.project` that already exists, show the current
content and the proposed change side by side so the user can see exactly
what will happen.

### Step G — Build & run

```
NIX_CONFIG="system = x86_64-linux" nix develop -c cabal build <testsuite>
NIX_CONFIG="system = x86_64-linux" nix develop -c cabal test <testsuite>
```

Iterate on errors. Report every milestone — successful build, first test
run, first failing iteration, first green run.

### Step H — Wrap up

- Stage only project artefacts: `test/**`, the modified `*.cabal`, the
  modified `cabal.project`.
- Do NOT stage skill scratch: `.claude/`, REPORT/ANSWERS-style files, or
  any agent metadata.
- Draft a conventional-commits commit message (e.g.
  `feat(test): add TestingInterface property test suite`). Apply one
  commit.
- Do not push. Do not propose changes to the skill itself.
- Stop and report the resulting `git log -1 --oneline` only. Do not run
  wider `git log`.

## 4. The triad — read this before Step E

The conceptual heart of this skill. Five bullets here; the full treatment is
in `references/04-generator-precondition-perform.md`.

- The generator (`arbitraryAction`) is a *beam* of candidate `Action`s.
- It must stay inside the ledger's well-formedness envelope **AND** cross the
  model's `precondition` line in both directions.
- `precondition` does NOT filter the generator. It LABELS each candidate:
  `True` → expect on-chain success → positive test; `False` → expect on-chain
  rejection → negative test.
- `perform` submits unconditionally and returns the new model state.
- If your generator only emits valid actions (or only invalid ones), one
  half of your test suite silently runs nothing.

Get this wrong and the test suite will look green while testing nothing.
That is the single most expensive failure mode in this framework. Read
`references/04-generator-precondition-perform.md`.

## 5. Iterating when things go wrong

- **Type-check failures** → share the error with the user, propose a fix,
  ask if the proposed fix is appropriate. Common: missing `ToJSON`,
  `Show (Action ...)`, era mismatch (the monad is fixed to `ConwayEra`).
- **Build failures** → ensure `flake.nix`, `nix/`, and `cabal.project` are
  set up; consult `references/01-project-setup.md`.
- **Test runs but no negative tests fire / "discard"** → your generator
  emits only `precondition = True` candidates (or only ledger-malformed
  ones). See `references/04-generator-precondition-perform.md` §4.
- **Positive tests fail immediately** → either your generator emits
  ledger-malformed values, or your `precondition` disagrees with the
  validator. See `references/04-generator-precondition-perform.md` §4.
- **Test hangs / shrinks forever** → `precondition` may be expensive or
  non-total. See `references/04-generator-precondition-perform.md` §7.

When anything is unclear, ask the user before guessing.

## 6. What this skill deliberately omits

If the user asks for any of the following, ask the user before proceeding:

- `validate` post-step invariant checks.
- `monitoring` (QuickCheck labels / coverage classifications).
- `RunOptions` (custom protocol params, coverage, verbosity, `maxActions`).
- `disableNegativeTesting`.
- `discardNegativeTestForUserExceptions = True`.
- The `mockchainSucceedsWithOptions` direct-test style.
- Plutus script coverage via `withCoverage` / `CoverageConfig`.

## 7. Reference index

- `references/01-project-setup.md` — project discovery, dependency facts,
  fallback templates, build wiring.
- `references/02-testing-interface-class.md` — class signatures and method
  semantics.
- `references/03-helpers-cheatsheet.md` — canonical API idioms.
- `references/04-generator-precondition-perform.md` — **the conceptual core.
  Read before writing `arbitraryAction`, `precondition`, or `perform`.**
- `references/05-threat-models.md` — built-in catalog, decision tree, wiring, expectedVulnerabilities, custom-model pointers.
