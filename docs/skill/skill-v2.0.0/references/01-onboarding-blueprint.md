# Reference: Onboarding blueprint (conditional)

This file is loaded ONLY in the Fresh phase, and ONLY when the user
signals unfamiliarity. Detection criteria are in §C. If neither fires,
skip this file entirely and go straight to `02-project-setup.md`.

## A. When to load this file

Load it when ALL of these are true:

- Phase-0 probe returned **Fresh**.
- One of these unfamiliarity signals fires:
  - User explicitly says "I don't know what this is", "explain it
    first", "what does this skill do", or similar.
  - The repo contains zero prior usage of `convex-testing-interface`
    anywhere (no imports, no historical scaffolding stubs, no
    documentation references).
  - The user is in the **explorer** profile (see §D).

Skip this file entirely when:

- Phase is anything other than Fresh.
- User profile is **auditor** (they already know the framework; they
  want the harness wired up, not a tutorial).
- User opens with a concrete task ("add a test suite", "wire threat
  models") and the repo shows they've done this before.

## B. The four blueprint sections

Personalise each to the validator found in `contract-sketch.md`. The
goal is one short paragraph per section, with at least one concrete name
from the user's contract in each. Do NOT recite these verbatim — that
defeats the personalisation.

### B.1 What `convex-testing-interface` is

A property-test harness that drives two things in lockstep: a tiny model
of the contract's intended behaviour (a Haskell record + an `Action`
GADT) and the real mockchain that runs the on-chain validator. The
framework generates random action sequences, runs them on both sides,
and flags every disagreement.

Personalisation hook: name the validator from the contract sketch
("...drives your `<ValidatorName>` validator in lockstep with a small
model...").

### B.2 Why property testing over unit tests for validators

A validator is a function from transaction context to Bool. Unit tests
cover the action sequences you thought of. Property tests cover sequences
you didn't — and shrink failing ones down to a minimal counterexample.
For on-chain code, this is the difference between "I tested the happy
path" and "I tested 100 random interleavings of every action my contract
exposes".

Personalisation hook: name two plausible action sequences from the
sketch that a hand-written unit test would probably miss.

### B.3 The model↔chain mirror

The skill has the agent author one record type (the model state), one
`Action` GADT (one constructor per user-facing operation), and three
methods: `arbitraryAction` (generator), `precondition` (which actions
the model considers valid), and `perform` (runs the action on-chain AND
returns the new model state). The framework runs everything twice — on
the model and on the chain — and your test passes when they agree.

Personalisation hook: name the likely state fields ("balance",
"owner", "deadline" — whichever the sketch suggests) and one or two
plausible actions.

### B.4 Threat models

Once the positive/negative suite is green, the skill wires a list of
declarative attacks (output redirection, signer removal, value
underpayment, datum bloat, double satisfaction, etc.). The framework
applies each attack to every transaction your suite generates and flags
any attack the validator fails to reject. You pick from a menu of 26;
you do not write attacks from scratch in this skill.

Personalisation hook: name one threat model that is obviously relevant
to the contract ("...your contract has a continuation script output, so
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

## E. What to do next

After delivering the blueprint (or skipping it), proceed to
`02-project-setup.md`. The discover step (find validators, draft
`contract-sketch.md`) is delegated to a subagent BEFORE writing any
cabal changes — the sketch is what the blueprint personalises against
and what the model proposal builds on.
