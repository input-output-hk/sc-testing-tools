# Reference: The triad — `arbitraryAction` / `precondition` / `perform`

The conceptual core of the skill. Misunderstanding the relationship
between these three is the single most common way a `TestingInterface`
property test silently tests nothing. Loaded by subagents in the Fresh
and Setup-done phases, and in the Green-maintenance verbs **add
action**, **extend model**, **investigate skipped case**, and
**triage failure**.

## 1. The three roles

- **`arbitraryAction`** — a *candidate generator*. Given the current
  model state, produces an `Action`. The framework routes each
  candidate into positive or negative test based on `precondition`.
- **`precondition`** — a *labeller*. Given state + candidate, returns
  `True` if the model thinks the action should succeed on-chain,
  `False` if it thinks the action should fail. **It does not filter
  the generator.** It labels what the generator produced.
- **`perform`** — an *executor*. Submits the transaction and returns
  the new model state. Runs in `TestingMonadT`. It does **not** decide
  whether to submit — it always submits.

The three are independent. Each has a separate job. Confusing them is
what breaks the test suite.

## 1a. Where to branch: structural vs semantic

The single most common mistake in `arbitraryAction` is branching by
**semantic** validity ("will the contract accept this right now?")
instead of **structural** feasibility ("can I even build this Action
from the model data?"). Get this wrong and the negative channel
silently dies.

### The rule

1. **The generator MUST read the model state.** It is handed the
   model precisely so it can decide what is *structurally
   constructible* right now. Reading the model is not the bug;
   gating on semantic validity is.

2. **Structural branching is broader than "bootstrap".** Any of these
   are structural — the Action literally cannot be built:
   - `not initialised` → only `Start*` actions are constructible
     (everything else references a script UTxO that doesn't exist).
   - `scriptUtxoCount < 2` → `SpendTwoScriptInputs` references two
     UTxOs that aren't there.
   - `datumStyle == Hash` → an action that requires inline-datum
     shape is unconstructible.

3. **Semantic branching belongs in `precondition`.** When the action
   IS fully constructible from the model data, but the contract
   should reject it given current state (e.g. "Ping while already
   Pinged"), the generator must produce it and `precondition` must
   return `False`. That's how the negative channel sees it.

4. **Refined heuristic:**
   - *"Can I build a meaningful `Action` value at all given what the
     model knows?"* → if no, branch in the generator.
   - *"Can I build it, but the contract should reject?"* → leave it
     to `precondition`.

5. **`initialize` is for model bookkeeping, not necessarily on-chain
   deployment.** See `01-onboarding-blueprint.md §E.1`:
   - If the contract has multiple deployment shapes worth testing
     (inline vs hash datum, with/without ref script, single vs many
     script UTxOs), deployment IS an action — `StartWithInlineDatum`,
     `StartWithDatumHash`, … — and the generator emits them when the
     model says we're uninitialised.
   - If the contract has only ONE deployment shape and there's
     nothing to test about it, `initialize` may also deploy on-chain
     and skip `Start*` actions. Both shapes are valid.

Hiding semantic gates in the generator **starves the negative-testing
channel**: the framework never gets to verify the contract rejects
bad-but-well-formed actions.

### Worked canonical example (PingPong)

This mirrors `PingPongSpec.hs` in `convex-testing-interface`. Model
is roughly:

```haskell
data PingPongModel = PingPongModel
  { modelState           :: !PongState   -- Pinged | Ponged | Stopped
  , modelScriptUtxoCount :: !Int
  , modelDatumStyle      :: !DatumStyle  -- Inline | Hash
  , modelInitialized     :: !Bool
  }
```

Actions: `StartWithInlineDatum | StartWithDatumHash | PlayRound Redeemer | DeployExtraScriptUtxo | SpendTwoScriptInputs Redeemer`.

```haskell
arbitraryAction PingPongModel{modelInitialized, modelScriptUtxoCount, modelState, modelDatumStyle} =
  if not modelInitialized
    then QC.elements [StartWithInlineDatum, StartWithDatumHash]
    else
      let allRedeemers = [Ping, Pong, Stop]
          genSingle = PlayRound <$> QC.elements allRedeemers
          genDual   = SpendTwoScriptInputs <$> QC.elements allRedeemers
          canDual   = modelScriptUtxoCount >= 2
          canDeploy = modelDatumStyle == Inline
                   && modelScriptUtxoCount == 1
                   && modelState /= Stopped
      in QC.frequency $ catMaybes
           [ Just (3, genSingle)
           , guard canDual    *> Just (1, genDual)
           , guard canDeploy  *> Just (1, pure DeployExtraScriptUtxo)
           ]
```

Reading each branch:

- `not modelInitialized` → only `Start*` is **structurally
  constructible**; nothing else references existing script state yet.
- `canDual = modelScriptUtxoCount >= 2` → `SpendTwoScriptInputs`
  literally needs two script UTxOs to reference; **structural**.
- `canDeploy` checks `Inline` style and exactly one current UTxO —
  these are **structural** preconditions of `DeployExtraScriptUtxo`
  itself, not of whether the contract should accept it.
- `PlayRound` is emitted with ALL three redeemers (`Ping`, `Pong`,
  `Stop`) regardless of `modelState`. Whether the contract should
  accept `PlayRound Ping` right now is a **semantic** question —
  that's `precondition`'s job, not the generator's.

### Anti-pattern: semantic gating in the generator

The true mistake is filtering by what the contract WILL accept:

```haskell
arbitraryAction s = case msThread s of
    Just Pinged   -> oneof [pure DoPong, pure DoStop]   -- pre-filters
    Just Ponged   -> oneof [pure DoPing, pure DoStop]   -- by semantic
    Just Stopped  -> oneof [pure DoStop]                -- validity
```

`DoPing`, `DoPong`, `DoStop` are all fully constructible — they
need nothing from the model that isn't always there. The case
split is **semantic**, not structural. The framework now never sees
`DoPing` while `msThread = Just Pinged`, so it never verifies the
contract rejects that case. Negative channel dies silently.

### Right pattern

```haskell
arbitraryAction PingPongModel{modelInitialized, ..} =
  if not modelInitialized
    then QC.elements [StartWithInlineDatum, StartWithDatumHash]
    -- structural: can't PlayRound without a script UTxO yet
    else QC.elements [PlayRound Ping, PlayRound Pong, PlayRound Stop]
    -- ALL three, regardless of state — precondition filters

precondition s (PlayRound Ping) = modelState s == Ponged   -- semantic
precondition s (PlayRound Pong) = modelState s == Pinged   -- semantic
precondition s (PlayRound Stop) = modelState s `elem` [Pinged, Ponged]
```

The generator reads the model (good — it branches on whether we
can build an Action at all). It does NOT read the model to decide
whether the contract should accept the Action (that's the negative
channel's food).

## 1b. Actions vs attacks (vocabulary discipline)

The `Action` GADT encodes the **contract's intended vocabulary** — the
operations a legitimate user, or an attacker using legitimate
operations, would invoke. Examples: `DoPing`, `Withdraw`, `Bid`,
`Cancel`.

**Do NOT include malformed-data variants** in the action type. They
are a category error.

- ✗ `MalformedRedeemer`, `GarbageDatum`, `OversizedPayload`,
  `NegativeAmount`. These are structural attacks.
- ✓ `DoPing`, `Withdraw <amount>`, `Bid <signer> <amount>`. These are
  operations.

Where each kind belongs:

| Kind | Lives in | Channel |
|---|---|---|
| Well-formed valid op | action type, `precondition=True` | positive |
| Well-formed but contract-should-reject op | action type, `precondition=False` | negative |
| Structurally malformed input | NOT in action type | threat models (`06-...`) |

The threat-model catalog in `references/06-threat-models.md` covers
~18 structural attacks (malformed data, missing fields, oversized
payloads, asset substitution, negative integers, etc.). If a needed
attack is genuinely outside that catalog, a custom threat model is
required — that is out of scope for this skill (`SKILL.md §9`).

## 2. The beam metaphor

`arbitraryAction` is a beam aimed at the space of possible
transactions. The beam must satisfy three properties simultaneously:

1. **Stay inside the ledger envelope.** Every generated action must
   produce a transaction the ledger accepts as well-formed (positive
   ada, fits protocol params, balances). Candidates the ledger rejects
   *before* the validator runs are wasted.
2. **Cross the precondition line in both directions.** The beam must
   include actions the model considers valid (`precondition = True`)
   AND actions it considers invalid (`precondition = False`). If only
   one side is produced, the other side of testing starves silently.
3. **Stay near the interesting boundary.** Most generation effort
   should cluster near the validity threshold. Wild noise across the
   whole integer range is mostly wasted.

`precondition` is the line down the middle of the beam. It does NOT
shape the beam. `perform` is the executor; it submits
unconditionally, regardless of which side the candidate came from.

## 3. The failure-mode matrix

| Symptom | Cause | Fix |
|---|---|---|
| Negative tests "all pass" but you wrote 0 negative-handling logic | Generator emits only `precondition = True` candidates | Widen generator to cross the validity boundary |
| Positive tests "all pass" but QuickCheck reports 0 actions per run | Generator emits only `precondition = False` candidates | Narrow generator toward the valid range |
| Many "discard" / "no tests run" messages | Generator emits ledger-malformed candidates (negative ada, etc.) | Constrain to ledger-valid values |
| Positive test fails immediately with "validator rejected" | Precondition too loose | Tighten precondition to match the validator |
| Negative test fails "Expected failure but it succeeded" | Precondition too tight, OR validator is permissive | Inspect: model says no, chain says yes. Decide which is wrong. |
| Test hangs / shrinks forever | `precondition` is expensive or non-total | Make it pure, cheap, total |
| `perform` short-circuits when precondition is False | You added `when (precondition s a)` inside `perform` | **Remove it.** See §6. |

## 4. Worked generator example (BoundedCounter, fictional)

A `BoundedCounter` UTxO with action `Add k` where the validator
accepts iff `n + k ∈ [0, 100]`.

**Generator A — NAIVE.**

```haskell
arbitraryAction _ = Add <$> arbitrary
```

Most candidates fall outside the ledger envelope (huge negatives →
negative ada). Most discards. Few hit the boundary.

**Generator B — SAFE BUT USELESS.**

```haskell
arbitraryAction (CounterModel n) = Add <$> choose (0, 100 - n)
```

Every action satisfies `precondition`. Negative side produces zero
candidates. Negative tests "pass" trivially.

**Generator C — CORRECT.**

```haskell
arbitraryAction (CounterModel n) = frequency
  [ (3, Add <$> choose (-n, 100 - n))         -- in-range
  , (2, Add <$> choose (101 - n, 200 - n))    -- overshoots upper
  , (1, Add <$> choose (-n - 50, -n - 1))     -- undershoots lower
  , (1, Add <$> choose (-5, 5))               -- near zero
  ]
```

Crosses the line in both directions, clusters near the boundary,
stays inside the ledger envelope.

## 5. The matching precondition and perform

```haskell
precondition (CounterModel n) (Add k) =
  let new = n + k in new >= 0 && new <= 100

perform s@(CounterModel n) (Add k) = do
  void $ tryBalanceAndSubmit mempty Wallet.w1
           (execBuildTx (userAddTxHelper k)) TrailingChange []
  pure (CounterModel (n + k))
```

`perform` submits unconditionally. The state update is optimistic; on
a failed negative test the framework discards this return value
anyway (see §7).

## 6. Practical generation tactics

- **`frequency`, not `oneof`, when one branch should dominate.**
  Typical weighting: in-range 3, just-over 2, just-under 1.
- **Numeric fields with threshold `T`** — combine `choose (0, T)` +
  `choose (T+1, 2*T)` + `choose (negate T, -1)`. Tune ratios so
  neither side starves.
- **Boolean / enum where one value is "valid here"** — `frequency`
  with non-canonical weighted ~1/3, not 1/2 (overshoots negative)
  and not 1/10 (negative side starves).
- **Multi-field actions — vary one field at a time across the
  boundary.** Don't maximise all dimensions of badness simultaneously;
  that produces actions failing for many reasons and obscures
  shrinking.
- **Use the model state.** Bias generation by what `s` currently
  contains. Only generate `Withdraw` when funds exist; weight the
  boundary-crossing branch higher when the model is near the limit.
- **Avoid `arbitrary :: Gen Integer`.** Constrain the range.

## 7. State drift after a negative-test step

When a negative step runs (`precondition = False`, framework expects
on-chain submit to fail):

- `perform` is still called and submits.
- If the submit fails, the framework counts this as a successful
  negative outcome and discards `perform`'s return value.
- The model state from a failing step is never observed by the
  framework — negative tests end after one bad action; there is no
  next step that consumes the state.

Mental model: **write `perform` for the success case.** Assume the
submit went through, compute the new state optimistically, return it.
On failure the framework discards your return value. You do not need
to wrap `tryBalanceAndSubmit` in error handling for negative-test
purposes.

## 8. Common misimplementations of `perform`

- **"Skip submit if precondition is False"** — destroys negative
  testing. Every negative test trivially "passes" because no submit
  happened.
- **"Always return `s` unchanged"** — positive testing can no longer
  observe state evolution between steps. Sequences test nothing
  meaningful beyond "no submit threw".
- **"Use `balanceAndSubmit` and ignore the `Either`"** — silent
  failures. State advances even on failed submit, desynchronising
  model from chain.
- **Correct.** Use `tryBalanceAndSubmit`; let it throw on failure;
  the framework catches.

## 9. Pre-build checklist (eight points)

Before `cabal build`:

1. Does `arbitraryAction` produce ledger-valid candidates (positive
   amounts, sensible sizes)?
2. At least some candidates with `precondition = True`?
3. At least some candidates with `precondition = False`?
4. Most generation clusters near the validity boundary, not far away?
5. Is `precondition` total — every `(state, action)` covered?
6. Is `precondition` pure, cheap, mirrors the validator?
7. Does `perform` submit unconditionally — no `precondition` guard?
8. Does `perform` return the **new** model state on the success path?

Yes to all eight → the triad is correct. Otherwise fix before
running. Running an incorrectly-tuned suite gives green output that
means nothing.

## 10. Maintenance-verb specifics

### Add action

- Add a constructor to `data Action MyModel`.
- Add a branch to `arbitraryAction` (respect the beam metaphor).
- Extend `precondition` — keep it total (catch-all `_ _ = True`).
- Add a `perform` branch (submit unconditionally, return new state).
- Rebuild + rerun. Note the new action in `model-decisions.md`.

### Extend model

- Add a field to the state record.
- Update `initialize` to populate the new field.
- Update every `perform` branch to maintain the new field.
- Decide if any existing `precondition` clause should read it.
- Rebuild + rerun. Note the rationale in `model-decisions.md`.

### Investigate skipped case

- Identify which `precondition` clause is returning `False` too
  often. The signal is QuickCheck reporting many discards or a
  branch never firing.
- Two fixes possible: tighten the generator branch that produces
  these (so it produces fewer or aims at a different threshold), or
  loosen the precondition (only if the model is wrong, not the
  generator).
- Propose to the user; do not auto-apply.

### Triage failure

- Classify: build error (Red-repair), positive-test counterexample
  (model bug or contract bug), negative-test "expected failure but
  succeeded" (validator more permissive than model), threat-model
  counterexample (genuine vulnerability finding).
- Reproduce with the shrunk counterexample QuickCheck prints.
- For positive failures: inspect the failing `perform` step; ask
  whether the validator's rejection matches the model's expectation.
- For threat-model failures: this is a real finding. Surface it to
  the user; do not auto-fix.

## 11. Commenting style (mandatory)

Every `TestingInterface` function (and `threatModels`) MUST have a
Haskell comment block above it explaining the **concept** the function
embodies — not the syntax, not a paraphrase of the code. The future
reader should understand the testing philosophy from comments alone.
This is the rule named in `SKILL.md §6.1`; the templates below are
what subagents and the main agent paste in, then specialise to the
contract under test.

The rule: explain the *why* and the *concept*, not the *what*. Don't
restate the code in English.

### 11.1 Templates

**`initialize`:**

```haskell
-- Sets up the in-memory model for a test round. Returns the model's
-- zero state. If the contract has multiple deployment shapes worth
-- testing, initialize leaves `initialised = False` and the on-chain
-- deploy happens via a Start action emitted by the generator. If the
-- contract has only one deployment shape, initialize may also deploy
-- on-chain and return a post-bootstrap model. Pick whichever is true
-- for the contract you're testing.
```

**`arbitraryAction`:**

```haskell
-- Proposes the next Action. Branches on STRUCTURAL feasibility only:
-- "can I build this Action from what the model knows right now?"
-- Never on SEMANTIC validity ("is this Action allowed by the contract?")
-- -- that goes in precondition. Keeping the semantic gate out of the
-- generator is what feeds the negative-testing channel.
```

**`precondition`:**

```haskell
-- Encodes the contract's semantic rules: for each Action, returns True
-- iff the validator SHOULD accept it given current state. The framework
-- runs the same action against both the chain (perform) and this model;
-- the two verdicts must agree. Disagreement = bug somewhere.
```

Plus a **per-clause** one-liner explaining the *rule* (not the
boolean expression). Example shape:

```haskell
-- DoPong only valid in Pinged state -- after a Ping has been sent.
precondition s DoPong = msThread s == Just Pinged
```

**`perform`:**

```haskell
-- Builds and submits the on-chain tx for this Action, then returns the
-- updated model state. The model transition must mirror what the
-- contract does on-chain. If the action is invalid (precondition False),
-- perform still submits it -- the framework expects rejection.
```

**`threatModels`:**

```haskell
-- Shadow attacks: each model takes a positive tx that succeeded and
-- mutates it (twists exactly one property), then resubmits. The
-- contract must reject the mutated form. Selected here because each
-- probes a guarantee this contract claims to enforce:
--   unprotectedScriptOutput -- script outputs always reach back to script
--   largeDataAttackWith 10  -- datum/redeemer strictness
--   ...
```

### 11.2 Application rules

- Use the templates as a starting point; specialise wording to the
  contract (name the validator, the state fields, the actions).
- A function without its concept comment is incomplete work. A
  subagent returning code without these comments must redo the task.
- Keep per-clause `precondition` comments to ONE short line each — the
  *rule*, not the predicate.
- Do NOT comment trivial helper functions or imports under this rule;
  it applies to the TI methods and `threatModels` only.
