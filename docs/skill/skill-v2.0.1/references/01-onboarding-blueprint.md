# Reference: Onboarding blueprint (conditional)

This file is loaded ONLY in the Fresh phase, and ONLY when the Fresh
intro gate (see `SKILL.md §4 Fresh`) resolves to `Quick intro first`
(or `Figure it out` → unfamiliar). Otherwise skip entirely and go
straight to `02-project-setup.md`.

## A. When to load this file

Load it when ALL of these are true:

- Phase-0 probe returned **Fresh**.
- The Fresh intro gate returned `Quick intro first`, OR the gate
  returned `Figure it out` and one of these unfamiliarity signals
  fires:
  - User explicitly says "I don't know what this is", "explain it
    first", "what does this skill do", or similar.
  - Repo contains zero prior usage of `convex-testing-interface`
    anywhere (no imports, no scaffolding stubs, no docs).
  - User is in the **explorer** profile (see §D).

Skip this file entirely when:

- Phase is anything other than Fresh.
- The Fresh intro gate returned `Skip, just work`.
- User profile is **auditor** (they know the framework already).

## B. The four blueprint sections

Personalise each to the validator found in `contract-sketch.md`. The
goal is one short paragraph per section, with at least one concrete name
from the user's contract in each. Do NOT recite these verbatim — that
defeats the personalisation.

### B.1 What `convex-testing-interface` is

A property-test harness that drives the contract through **two channels
at once**:

- **Positive channel.** Generator → `precondition=True` → `perform`
  submits → chain accepts. Confirms valid actions succeed.
- **Negative channel.** Same generator → `precondition=False` →
  `perform` submits anyway → chain rejects. Confirms invalid actions
  fail.

A tiny model (a Haskell record + an `Action` GADT) decides which side a
generated action falls on. The mockchain enforces validity for real.
The two should agree on every step.

A separate, declarative **attack channel** (threat models) is layered
on top: it takes valid transactions and mutates them to verify the
contract rejects the mutated versions.

Personalisation hook: name the validator from the contract sketch
("...drives your `<ValidatorName>` validator through both channels...").

### B.2 Why property testing over unit tests for validators

A validator is a function from transaction context to Bool. Unit tests
cover the action sequences you thought of. Property tests cover sequences
you didn't — and shrink failing ones down to a minimal counterexample.
For on-chain code, this is the difference between "I tested the happy
path" and "I tested 100 random interleavings of every action my contract
exposes".

Personalisation hook: name two plausible action sequences from the
sketch that a hand-written unit test would probably miss.

### B.3 The model↔chain mirror, in service of the two channels

You author one record type (the model state), one `Action` GADT (one
constructor per user-facing operation), and three methods:

- `arbitraryAction` — generator. **Intentionally dumb about validity.**
  Produces actions of both shapes (valid AND semantically-invalid).
  This is what keeps the negative channel alive.
- `precondition` — the validity oracle. Returns `True` for actions the
  model considers valid; `False` for actions it considers invalid.
  Splits generated candidates into the two channels.
- `perform` — submits the tx unconditionally and returns the new model
  state. Runs in `TestingMonadT`.

The framework runs everything twice — on the model (precondition) and
on the chain (`perform`) — and asserts they agree. Disagreement is
either a model bug, a contract bug, or a generator bug.

Personalisation hook: name the likely state fields ("balance", "owner",
"deadline") and one or two plausible actions.

### B.4 Threat models (the third channel)

Once the positive/negative suite is green, the skill wires a list of
declarative attacks (output redirection, signer removal, value
underpayment, datum bloat, double satisfaction, etc.). The framework
applies each attack to every successfully-submitted transaction and
flags any attack the validator fails to reject. You pick from a menu
of 26; you do not write attacks from scratch in this skill.

Threat models target **structural** attacks (malformed data, missing
fields, oversized payloads, asset substitution). **Semantic** attacks
("call this method when you shouldn't be allowed to") belong in the
negative channel above, not here. See
`references/05-generators-and-implementation.md` for the boundary.

Personalisation hook: name one threat model obviously relevant to the
contract ("...your contract has a continuation script output, so
`unprotectedScriptOutput` will be in the menu...").

## C. Calibration questions

Ask AT MOST two before starting work. Batch them. Skip any whose answer
is already in `.skill-state.md` or visible in the repo.

1. **User profile.** "Are you the author of this contract, an auditor
   reviewing it, or learning the framework?" (See §D for what each
   answer changes.)
2. **Trust level for defaults.** "Should I propose the conservative
   threat-model set without asking line-by-line, or walk through each
   pick?"

Record both in `.skill-state.md` under `## user profile` and
`## decisions log` respectively.

## D. User profile calibration

The profile changes verbosity and the default threat-model count. It
does NOT change the phase machine.

- **Author.** Building the contract themselves. Wants speed; accepts
  defaults; iterates fast. Default threat-model set is the conservative
  five from `06-threat-models §C`. Skip this onboarding blueprint
  unless they ask. Confirm-then-act loops are short.
- **Auditor.** Reviewing someone else's contract. Wants explicit
  invariants, more threat models, careful model choices. Always skip
  this blueprint (they know it). Default threat-model set is the full
  decision-tree menu, not the conservative five. Confirm-then-act loops
  are thorough; show the diff before every write.
- **Explorer.** Learning the framework. Always load this blueprint.
  One action per `perform`-loop iteration; explain what each method
  does as it's written. Default threat-model set is the conservative
  five plus a one-line explanation of each.
- **Unknown.** Treat as author until calibrated; ask the profile
  question early.

## E. How one test round runs

A single QuickCheck round looks like this:

1. **`initialize` runs ONCE** at the start of the round. It deploys
   any scripts the suite needs, funds the test wallets, and returns
   the **initial model state** — the state already reflects whatever
   bootstrap was needed to make the contract testable.
2. **Framework loop.** Starting from the initial state, the framework
   repeats: `arbitraryAction state → precondition state action →
   perform state action → new state`. Each iteration appends one
   action to a growing trace.
3. **Trace** = `[action₁, …, actionₙ]`.
4. **End of trace** → invariant checks (model invariants, optional
   `validate` post-step checks).
5. **Failure** → QuickCheck shrinks the trace to a minimal
   counterexample.

### Consequences (rules)

- By the time `arbitraryAction` runs, `initialize` has **already
  happened**. The model state passed to `arbitraryAction` already
  reflects bootstrap.
- **Do NOT include bootstrap/deploy actions in the action vocabulary.**
  That work belongs in `initialize`. A `Seed` / `Deploy` constructor in
  the `Action` GADT is almost always wrong.
- The structural-branching rule from
  `references/05-generators-and-implementation.md` applies to
  **steady-state shapes** (e.g. "no active auction" vs "auction
  running" — both reachable post-init), NOT to pre-init bootstrap.

### Worked example — PingPong

- `initialize`: deploy the validator, kick off the thread token,
  return `PingPongModel { msThread = Just Pinged }` (or whatever the
  contract's first reachable state is).
- Action vocabulary: `{DoPing, DoPong, DoStop}`. **No `Seed`.**
- `arbitraryAction` generates from this vocabulary freely;
  `precondition` decides which are valid given `msThread`.

If the contract genuinely supports being deployed-but-not-kicked-off,
that pre-kickoff state is part of the steady-state shape and the agent
handles it in `arbitraryAction` — but `initialize` still owns the
deploy. **`initialize` = whatever one-time setup makes the contract
testable at all**, no more, no less.

## F. What to do next

After delivering the blueprint (or skipping it), proceed to
`02-project-setup.md`. The discover step (find validators, draft
`contract-sketch.md`) is delegated to a subagent BEFORE writing any
cabal changes — the sketch is what the blueprint personalises against
and what the model proposal builds on.
