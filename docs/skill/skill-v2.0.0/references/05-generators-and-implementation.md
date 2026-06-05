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
