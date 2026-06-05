# Reference: Onboarding blueprint (conditional)

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
at once**, where each test is a **round of actions**, not a single
action:

- **Positive round.** Every action satisfies `precondition=True`,
  `perform` submits each, the chain accepts each. Confirms a valid
  history is reachable.
- **Negative round.** First N−1 actions are positive (precondition
  True, accepted); the **last** action is negative (precondition
  False, chain MUST reject). The negative tail proves the contract
  guards against bad inputs given a real history.

A tiny model (a Haskell record + an `Action` GADT) decides which side a
generated action falls on. The mockchain enforces validity for real.
The two should agree on every step.

```
Positive round:   [a1✓, a2✓, a3✓, a4✓]     all accepted
Negative round:   [a1✓, a2✓, a3✓, BAD✗]    last must be rejected
                  └────positive tail────┘ └── attack tail ──┘
```

The negative channel doesn't generate a failing action in isolation —
it generates a valid history then probes one bad action at the end.
That's why traces (rounds), not actions, are the unit of testing.

A separate, declarative **attack channel** (threat models) is layered
on top: it takes valid transactions and mutates them to verify the
contract rejects the mutated versions.

The shape of the two-channel split, in one picture:

```
                    generator
                       │
                  pick an Action
                       │
                       ▼
                 precondition?
                  ┌────┴────┐
              True│         │False
                  ▼         ▼
              perform     perform
                  │         │
                  ▼         ▼
              chain         chain
              accepts       rejects
             (positive)    (negative)
```

One generator feeds both channels. The precondition is the routing
decision, not a filter. In both branches `perform` runs and submits;
the framework then asserts the chain's verdict matches the model's
verdict. Disagreement on either side is a bug.

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

Picture each threat model as a parallel-world shadow of a successful
positive transaction:

```
   positive run (real)            shadow run (per threat model)
   ────────────────────           ────────────────────────────
   build tx ───► submit ───►      build tx ──► mutate ──► submit
                  │                   (twist X)         │
                  ▼                                     ▼
              accepted                              rejected
              (good)                                (good — guard held)
```

Every positive test that passes spawns N shadow tests, one per
applicable threat model. Each shadow asks: *"if I twist this one
knob, does the validator still reject?"* A shadow that gets accepted
= a vulnerability.

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

A single QuickCheck round (a **trace**) looks like this:

1. **`initialize` runs ONCE** at the start of the round. It sets up
   the in-memory model only (zero state, `modelInitialized = False`).
   It does NOT submit any tx. Deployment happens via an Action emitted
   by the generator. See §E.1.
2. **Framework loop.** Starting from that initial state, the framework
   repeats: `arbitraryAction state → precondition state action →
   perform state action → new state`. Each iteration appends one
   action to the growing trace.
3. **Trace shape**: `[a₁, …, aₙ]`. Positive trace = all actions pass;
   negative trace = the first N−1 are positive, the last is negative
   (precondition False; chain must reject).
4. **End of trace** → invariant checks (model invariants, optional
   `validate` post-step checks).
5. **Failure** → QuickCheck shrinks the trace to a minimal
   counterexample.

### E.1 Where deployment lives — one correct shape

There is one correct shape. Deployment is always an Action.
`initialize` is always model-only. The Cardinal Rule at the top of
this file states this in full; do not deviate.

- The Action type includes at least one deploy variant —
  `Start`, `Deploy`, `StartWithInlineDatum`, `StartWithDatumHash`,
  whatever your contract calls for. Multiple deploy variants are fine
  when distinct deployment shapes are worth testing.
- `initialize` returns the zero state (e.g. `modelInitialized = False`).
  No on-chain calls.
- `arbitraryAction` reads the model: when uninitialised, emit a deploy
  Action; once initialised, emit the normal vocabulary.

Worked example: see `references/05-generators-and-implementation.md
§1a` (the PingPong canonical example) and the WRONG/RIGHT pair in
§11.1 of that same file.

### E.2 The structural-vs-semantic rule (preview)

The generator IS allowed to read the model state. It branches on
**structural feasibility** — "can I build this Action at all given
what the model knows right now?" — never on **semantic validity** —
"will the contract accept this Action right now?". The latter is
`precondition`'s job. Examples of structural branching:

- `not initialised` → only `Start*` actions make sense yet.
- `scriptUtxoCount < 2` → can't construct `SpendTwoScriptInputs`.
- `datumStyle == Hash` → can't construct an action that requires
  inline-datum shape.

Full rule, examples, and the right/wrong patterns:
`references/05-generators-and-implementation.md §1a`.

## F. What to do next

After delivering the blueprint (or skipping it), proceed to
`02-project-setup.md`. The discover step (find validators, draft
`contract-sketch.md`) is delegated to a subagent BEFORE writing any
cabal changes — the sketch is what the blueprint personalises against
and what the model proposal builds on.
