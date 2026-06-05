# Reference: The generator / precondition / perform triad

## 1. Why this file exists

This is the single most important file in the skill. Misunderstanding the
relationship between `arbitraryAction`, `precondition`, and `perform` is the
most common way a `TestingInterface` property test silently tests nothing.
Read this before you write any of the three methods.

## 2. The three roles

- **`arbitraryAction`** — a *candidate generator*. Given the current model
  state, produces an `Action`. The framework will then decide whether to
  route it into a positive or negative test based on `precondition`.

- **`precondition`** — a *labeller*. Given the model state and a candidate
  action, returns `True` if the model thinks the action should succeed
  on-chain, `False` if it thinks the action should fail on-chain. **It does
  not filter the generator.** It labels what the generator produced so the
  framework can route it.

- **`perform`** — an *executor*. Submits the transaction (via
  `tryBalanceAndSubmit` or similar) and returns the new model state. It
  runs in `TestingMonadT`. It does **not** decide whether to submit — it
  always submits.

The three are independent. Each has a separate job. Confusing them is what
breaks the test suite.

## 3. The beam metaphor

Think of `arbitraryAction` as a beam aimed at the space of possible
transactions.

```
                   space of all transactions
       ┌──────────────────────────────────────────────┐
       │                                              │
       │   ledger-malformed (rejected before          │
       │   validator runs, useless candidates)        │
       │                                              │
       │   ┌────────────────────────────────────────┐ │
       │   │   ledger envelope                      │ │
       │   │   (well-formed: positive ada, fits     │ │
       │   │    protocol params, balances)          │ │
       │   │                                        │ │
       │   │       valid    │    invalid            │ │
       │   │   (precondition│ (precondition=False)  │ │
       │   │    =True)      │                       │ │
       │   │                │                       │ │
       │   │     ░░░░░░░░░░░│░░░░░░░░░░░░           │ │
       │   │       beam = arbitraryAction          │ │
       │   │     ░░░░░░░░░░░│░░░░░░░░░░░░           │ │
       │   │                │                       │ │
       │   │                ↑                       │ │
       │   │        the precondition line          │ │
       │   └────────────────────────────────────────┘ │
       └──────────────────────────────────────────────┘
```

The beam must satisfy three properties simultaneously:

1. **Stay inside the ledger envelope.** Every generated action must produce
   a transaction the ledger will accept as structurally well-formed
   (positive ada amounts, fits protocol params, decodes correctly,
   balances). Candidates the ledger rejects *before* the validator runs are
   wasted — they test nothing about your contract.

2. **Cross the precondition line in both directions.** The beam must
   include actions the model considers valid (`precondition = True`) and
   actions the model considers invalid (`precondition = False`). If it only
   produces one side, the other side of testing starves silently.

3. **Stay near the interesting boundary.** Most generation effort should
   be near the validity threshold. Wild noise across the whole integer
   range is mostly wasted — values far from the boundary are usually
   trivially valid or trivially invalid. Cluster generation just-above, at,
   and just-below the model's validity threshold.

`precondition` is the line down the middle of the beam. It does **not**
shape the beam. It labels each candidate so the framework can route it.

`perform` is the executor. It does not know or care which side of the line
the candidate came from. It submits unconditionally.

## 4. The failure-mode matrix

| Symptom                                                              | Cause                                                              | Fix                                                                                                |
| -------------------------------------------------------------------- | ------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------- |
| Negative tests "all pass" but you wrote 0 negative-handling logic    | Generator emits only `precondition = True` candidates              | Widen generator to cross the validity boundary; add a branch that overshoots the threshold        |
| Positive tests "all pass" but QuickCheck reports 0 actions per run   | Generator emits only `precondition = False` candidates             | Narrow generator toward the valid range; weight the valid branch higher                            |
| Many "discard" / "no tests run" messages                              | Generator emits ledger-malformed candidates (negative ada, etc.)   | Constrain to ledger-valid values: positive integers, valid encodings, sensible value sizes        |
| Positive test fails immediately with "validator rejected"             | Precondition too loose — model thinks action is valid but isn't    | Tighten precondition to match what the validator actually enforces                                |
| Negative test fails with "Expected failure for invalid action but it succeeded" | Precondition too tight, OR the validator is genuinely permissive   | Inspect: model says no, chain says yes. Decide which is wrong. May indicate a real validator bug. |
| Test hangs / shrinks forever                                         | `precondition` is expensive or non-total                           | Make `precondition` pure, cheap, and total (cover every constructor)                              |
| `perform` returns wrong state on failure                             | `perform` updated state from a failed submit                       | Don't update state on a failed submit, or let the framework discard via negative-test routing     |
| `perform` short-circuits when precondition is False                  | You added a `when (precondition s a)` guard inside `perform`       | **Remove it.** `perform` must submit unconditionally — see §9.                                    |

## 5. Worked example — the BoundedCounter (fictional)

Suppose the user's contract is a `BoundedCounter`: a UTxO at a script
address holding a single integer counter `n`. There is one action:
`Add k` where the validator accepts the update iff the new counter value
`n + k` stays in `[0, 100]`. Anything outside that range is rejected
on-chain.

### The model

```haskell
data CounterModel = CounterModel { value :: !Integer }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
```

### The action

```haskell
data Action CounterModel = Add !Integer
  deriving Show
```

### Three candidate generators — only one is right

**Generator A — NAIVE.**

```haskell
arbitraryAction _ = Add <$> arbitrary
```

What goes wrong: `arbitrary :: Gen Integer` returns values across the full
signed range, including huge negatives that translate to negative ada
amounts and ledger-rejected transactions. Most candidates never reach the
validator. Worse: the few that do happen to be valid are scattered and
unlikely to land near the boundary `n + k = 0` or `n + k = 100`. The test
runs but reports a lot of discards and exercises very little.

**Generator B — SAFE BUT USELESS.**

```haskell
arbitraryAction (CounterModel n) = Add <$> choose (0, 100 - n)
```

What goes wrong: every generated action satisfies `precondition` (because
`0 ≤ n + k ≤ 100` always holds). The positive side gets exercised. The
negative side **silently produces zero candidates** — the framework's
`suchThatMaybe` keeps trying to find a `precondition = False` action,
fails, and the negative test discards. You see "Negative tests passed"
but nothing was actually tested.

**Generator C — CORRECT.**

```haskell
arbitraryAction (CounterModel n) = frequency
  [ (3, Add <$> choose (-n, 100 - n))         -- in-range: precondition True
  , (2, Add <$> choose (101 - n, 200 - n))    -- overshoots upper bound
  , (1, Add <$> choose (-n - 50, -n - 1))     -- undershoots lower bound
  , (1, Add <$> choose (-5, 5))               -- always near zero
  ]
```

What this achieves:

- All branches produce ledger-well-formed values (the `Add k` action with
  a moderate `k` translates to a transaction the ledger accepts as
  well-formed even when the validator will reject it).
- The first branch lands inside the precondition line (positive tests
  exercise these).
- The second and third branches land outside it (negative tests exercise
  these).
- All branches cluster near the boundary, so the test pressures the
  validator at its decision threshold rather than wasting effort at
  trivial extremes.

### The precondition

```haskell
precondition (CounterModel n) (Add k) =
  let new = n + k in new >= 0 && new <= 100
```

Total (one constructor, one case), pure, O(1). Mirrors the validator
exactly. Drift between the precondition and the validator would surface as
a test failure — which is precisely the value the framework provides.

### The perform

```haskell
perform s@(CounterModel n) (Add k) = do
  void $ tryBalanceAndSubmit
    mempty
    Wallet.w1
    (execBuildTx (userAddTxHelper k))
    TrailingChange
    []
  pure (CounterModel (n + k))
```

Notice: `perform` does **not** check `precondition` or test `k`'s range.
It submits unconditionally. If the validator rejects, `tryBalanceAndSubmit`
throws (or returns a `Left` if used via `balanceAndSubmit`), and the
framework catches that. The state update `(n + k)` is the optimistic
update; on a failed negative test the framework discards this final state
anyway (see §8).

## 6. Practical generation tactics

- **Use `frequency`, not `oneof`, when one branch should dominate.** The
  in-range branch typically wants higher weight than the boundary-crossing
  branches; weight 3:2:1 or similar.

- **For numeric fields with a validity threshold `T`,** generate from a
  combination like:
  ```haskell
  frequency
    [ (3, choose (0, T))           -- inside
    , (1, choose (T + 1, 2 * T))   -- just-outside-high
    , (1, choose (negate T, -1))   -- just-outside-low
    ]
  ```
  Tune ratios so neither side starves.

- **For booleans / enums where one value is "the valid choice in this
  state,"** use `frequency` with the non-canonical choice weighted around
  1/3 — not 1/2 (overshoots negative) and not 1/10 (negative side starves).

- **For multi-field actions, vary one field at a time across the
  boundary.** Don't try to maximise all dimensions of badness
  simultaneously; that produces actions that fail for many reasons at once
  and obscure shrinking. Pick which field is "the interesting one" for
  this branch and keep the others well-formed.

- **Use the model state.** The whole point of `arbitraryAction :: state -> Gen ...`
  is that you can bias generation by what the state currently contains.
  E.g. only generate `Withdraw` when the model says funds exist; or weight
  the boundary-crossing branch higher when the model is near the limit.

- **Avoid `arbitrary :: Gen Integer` directly.** Constrain the range. The
  ledger envelope is finite; respect it.

## 7. Precondition discipline

- **Pure.** No `IO`, no random generation. `precondition` is called many
  times during shrinking and must be repeatable.

- **Total.** Every `(state, action)` pair must return either `True` or
  `False`. Never `bottom`, never partial. If you have many constructors and
  most are unconditionally valid, end with `precondition _ _ = True` (or
  `False`) explicitly.

- **Cheap.** Called constantly during shrinking — keep it `O(state size)`
  at worst, preferably `O(1)`.

- **Aligned with the validator.** The model's notion of "valid" should
  match the on-chain validator's notion of "valid". Drift between the two
  IS a bug — either in the model, in the validator, or in the
  precondition. The test surfaces this drift; that is the framework's
  value proposition.

## 8. State drift after a negative-test step

When a negative test step runs (`precondition = False`, the framework
expects the on-chain submit to fail):

- `perform` is still called. It calls `tryBalanceAndSubmit`.
- If the submit fails (validator rejects, balancing fails, or any
  exception): the framework counts this as a successful negative test
  outcome and discards the final state from that step.
- The model state from that failing step is **never observed** by the
  framework — the negative test ends after this single bad-action
  attempt; there is no next step that would consume the model state.

In practice this means: `perform`'s return value on a failing submit
doesn't matter (the framework discards it). The safest mental model is to
**write `perform` for the success case** — assume the submit went through,
compute the new state optimistically, return it. If the submit failed,
nothing downstream uses your return value.

You do not need to wrap `tryBalanceAndSubmit` in error handling for
negative-test purposes. The framework handles that at a higher level.

## 9. Common misimplementations of `perform`

- **"I'll skip submit if precondition is False."**
  ```haskell
  perform s a = do
    if precondition s a
      then submit a >> pure (update s a)
      else pure s              -- WRONG
  ```
  This **destroys negative testing**. The framework's negative test relies
  on `perform` actually attempting the submit so the chain can reject it.
  If you short-circuit, every negative test "passes" trivially because no
  submit ever happened — but you've tested nothing.

- **"I'll always return `state` unchanged."**
  ```haskell
  perform s _ = do
    submit ...
    pure s                     -- WRONG
  ```
  Positive testing can no longer observe state evolution between steps.
  `validate` will pass against a stale model. Sequences of actions test
  nothing meaningful beyond "no submit threw."

- **"I'll use `balanceAndSubmit` and ignore the `Either`."**
  ```haskell
  perform s a = do
    _ <- balanceAndSubmit ... -- ignoring the Left case
    pure (update s a)         -- WRONG
  ```
  Silent failures. The state advances even when the submit failed,
  desynchronising model from chain.

- **"I'll use `tryBalanceAndSubmit` and that's it."**
  ```haskell
  perform s a = do
    void $ tryBalanceAndSubmit mempty w (execBuildTx (helper a)) TrailingChange []
    pure (update s a)         -- CORRECT
  ```
  `tryBalanceAndSubmit` throws on failure. The framework catches the
  exception, treats it as the expected rejection for a negative test (or
  surfaces it as a positive-test failure), and routes accordingly. This
  is the canonical pattern. The state update reflects the success
  hypothesis; on failure the framework discards it.

## 10. A checklist before you press build

Before you `cabal build` your new test suite, ask yourself:

1. Does `arbitraryAction` produce candidates the **ledger** will accept
   (positive amounts, sensible sizes)?
2. Does `arbitraryAction` produce at least some candidates with
   `precondition = True`?
3. Does `arbitraryAction` produce at least some candidates with
   `precondition = False`?
4. Does most of the generation effort cluster near the validity boundary
   (not far away from it)?
5. Is `precondition` total — every `(state, action)` covered?
6. Is `precondition` pure, cheap, and matches the validator's rules?
7. Does `perform` submit unconditionally — no `precondition` guard inside?
8. Does `perform` return the **new** model state on the success path?

If you can answer yes to all eight, the triad is configured correctly. If
you cannot, fix it before running the build — running an
incorrectly-tuned test suite gives green output that means nothing.
