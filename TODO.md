# Threat-model harness follow-ups

Parked findings from the code reviews of the rebalancing/withdrawals work
(September 2026). The load-bearing correctness fixes from those reviews are
done; everything below is real but deliberately deferred. Items link to the
code as of `feat/improvements`.

## Correctness / robustness

## Design

## ThreatModelsFor redesign (agreed, not yet implemented)

Supersedes the former "`usesDefaultTms` heuristic" and "key the outcome map
by group" items, which were symptoms. Agreed September 2026 after reviewing
the class against the user journey rather than against this repo's own
fixtures (those are all post-triage, several of them testing the harness
itself, so they are not evidence of how the class should read).

**The journey the class has to serve.** A user starts with an empty instance
and all defaults, reads the results, and triages each model into a slot.
Later the script evolves, or the harness changes what "applies" means (see
`43ee17c0`, which made the output-mutation attacks start applying to
withdrawal transactions), and the run must show clearly *what* changed and
*why*.

### One list, expectation per entry

Replaces `threatModels` / `expectedVulnerabilities` / `acceptedFindings`:

```haskell
resists        tm          -- must apply, must resist (a coverage claim)
ifApplicable   tm          -- run it, report, never fail (the untriaged default)
notApplicable  tm "why"    -- must NOT apply; tell me if it starts
vulnerableTo   tm "why"    -- must be detected; "resolved" when it stops
accepts        tm "why"    -- informational only
```

Default: `ifApplicable` over every model in `allThreatModels`.

This deletes `usesDefaultTms` outright rather than replacing it: leniency
becomes a property of the entry, stated by the user, instead of something
the runner infers by comparing name lists against an evaluated default. It
also makes two things expressible that are not today — a hand-picked model
that is allowed not to apply, and a reviewed "does not apply here" verdict.
The latter is the slot 9 fixtures currently fake with `threatModels = []`
plus a prose comment, which throws away a prediction that can later break.

The `"why"` strings travel into the summary and the NDJSON. Today every
triage rationale lives in Haskell comments (`AikenBankSpec` has six lines
justifying one accepted finding) where no report can reach it.

### Attribute the fault

Only one cell below is the contract's fault; most red is a stale
declaration. Every failure names which of the three it is:

| | detected | blocked | never applied | never attacked / errored |
|---|---|---|---|---|
| `resists` | FAIL **contract** | pass | FAIL **declaration** | FAIL **setup** |
| `ifApplicable` | FAIL **contract** (triage me) | pass | report | warn |
| `notApplicable` | FAIL **contract**, note stale decl. | FAIL **declaration** | pass | pass |
| `vulnerableTo` | pass | FAIL **declaration** (resolved) | FAIL **declaration** | FAIL **setup** |
| `accepts` | report | warn (stale) | report | report |

A resolved `vulnerableTo` still fails — a green CI would be a lie and the
programmer must act — but the message must say the contract improved and the
instance is what needs editing. Compare today's, which reads as if the
attack failed:

```
Expected vulnerability NOT found in 100 tested tests
```

with:

```
RESOLVED - this vulnerability is no longer detected (100/100 attacks blocked).
Good news for the contract; this declaration is now stale.
DECLARATION: move it from 'vulnerableTo' to 'resists', or remove it.
Declared because: "CTF exercise ships with this bug deliberately"
```

`accepts` never fails: it claims nothing, so green is not a lie there.

### Reporting mechanics

- Lead each failure body with a greppable `CONTRACT:` / `DECLARATION:` /
  `SETUP:` label.
- Keep the Tasty groups keyed by slot, **not** by fault. Grouping by fault
  would migrate a model between groups as its outcome changes, wrecking the
  run-to-run diffability this whole design is for.
- Add a `fault` field to the streaming JSON beside the `category` field, so
  dashboards can route contract regressions separately from declarations
  that need updating.

### Prerequisite: stable identity

Diffing runs over time needs a key that is not a display string. Today
`ThreatModelResults` is keyed by the bare `Named` name, parameterized
variants share one (`largeValueAttackWith 10` vs `1000` are both "Large
Value Attack"), names are `Maybe` and get patched positionally by
`nameFallbacks` behind a partial `modelName` that `error`s, and
`defaultThreatModelsExcluding` matches by name across the
parameterless/parameterized boundary. Give models a `ThreatModelId` distinct
from the display name and key the outcome map, the exclusion match and the
CLI filter on it.

### Decided edge cases

- `notApplicable` + detected: lead with the contract finding (the urgent
  one), note the stale declaration second.
- `ifApplicable` + detected on day one: red is correct even before any
  triage; word it as a triage prompt, not an accusation. An empty instance
  is a red starting state by design.

### Out of scope

Detecting that a model is *new* (added to `allThreatModels` by a library
release) needs cross-run memory. Rejected: a checked-in baseline file rots
and adds review churn. New models get called out in release notes instead.

## Cleanups

## Deferred by decision (revisit conditions, not bugs)

- **An output-mutation model for outputs policed by another script**: the
  tightened `guardedScriptOutputs` precondition matches an output against
  its own payment credential, so it drops the cases the old
  transaction-wide `requireScriptExecution` caught where a *different*
  running script is the one responsible for the output - a validator V
  paying onward to an unrelated script W, or a thread-token minting policy
  vetting the datum and value paid into a validator. Both are real
  vulnerabilities if that script's output check is buggy, and both are now
  skipped. They are a different accusation from "W failed to protect its
  own output", which is why they were not folded back into these models;
  the harness cannot tell statically which running script inspects which
  output. Revisit as a model of its own if a use-case routes funds between
  scripts or enforces initial output state from a minting policy. (Note
  the multi-validator case is *not* affected: there the policy id and the
  validator hash are equal, so the output still matches.)

- **Token sourcing in `rebalanceAndSign`** (make a negative native-token
  residual satisfiable by adding wallet-owned UTxOs holding that token as
  inputs): rejected for now — no scenario has token-holding wallets, so it
  would be untestable plumbing, and it requires `rebalanceAndSign` to return
  an augmented UTxO set through all three runners. Revisit if a use-case
  with wallet-held native tokens appears.

- **`mutualExclusionAttack` fabricating its own second script input**:
  rejected as unsound — an attacker-funded sibling makes aggregate-value
  validators (Vesting/Escrow/Auction) validate legitimately, so every result
  would be a false positive. The reasoning is documented above the
  precondition in `ThreatModel/MutualExclusion.hs`; the ≥2-script-inputs
  precondition is a hard applicability bound, and contracts whose
  transactions never spend two script inputs should simply not list the
  model.
