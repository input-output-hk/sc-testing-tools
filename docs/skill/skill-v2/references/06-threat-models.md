# Reference: Threat models — selection and wiring

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

How to pick built-in threat models for the contract under test, how
to wire them into the `ThreatModelsFor` instance, and the inverted
`expectedVulnerabilities` semantics. Loaded by subagents in the
Implemented phase and in the Green-maintenance verbs **add threat
model** and **swap threat model**.

Custom threat models are out of scope for this skill (§F is the only
treatment). All facts here come from `chore/without-p` of
`sc-testing-tools`. The authoritative module is
[`Convex.ThreatModel.All`](https://github.com/input-output-hk/sc-testing-tools/blob/chore/without-p/src/testing-interface/lib/Convex/ThreatModel/All.hs);
the per-attack modules under
[`Convex.ThreatModel.*`](https://github.com/input-output-hk/sc-testing-tools/tree/chore/without-p/src/testing-interface/lib/Convex/ThreatModel)
expose the parameterised `…With` variants.

## §A. The parallel-world idea

Before any catalog or decision tree, anchor the mental model.

A threat model is not a different kind of test — it is a **shadow** of
a positive test. Take a transaction that succeeded in the positive
channel, twist exactly one property of it, resubmit:

```
   positive run (real)            shadow run (per threat model)
   ────────────────────           ────────────────────────────
   build tx ───► submit ───►      build tx ──► mutate ──► submit
                  │                   (twist X)         │
                  ▼                                     ▼
              accepted                              rejected
              (good)                                (good — guard held)
```

Interpretation:

- If the validator **still rejects** the mutated tx → the contract is
  guarded against that attack class. The shadow test passes.
- If the validator **accepts** the mutated tx → that is a
  vulnerability. The shadow test fails and surfaces it.

Threat models are a **layer on top** of the positive channel, not a
replacement for the positive/negative split. They re-use the
successful positive txs as raw material and probe them from new attack
angles. The positive/negative work in
`references/05-generators-and-implementation.md` still has to be in
place; threat models extend it, they do not substitute for it.

Each model targets **one specific knob** — large datum, missing output
datum, value underpayment, asset substitution, redirected continuation
output, dropped signer, widened validity range, and so on. One model
= one twist = one guarantee being probed. That is why a catalog of 18+
exists rather than a single mega-attack: granularity makes a failure
diagnostic.

### §A.1 What this implies operationally

- After every successful `perform` submission in the positive suite,
  the framework re-runs the same transaction with one or more tweaks
  (output redirection, signer removal, value inflation, datum
  injection, …).
- If the tweaked tx still validates, that's a found vulnerability.
- Threat models are **the structural-attack channel** — orthogonal to
  positive/negative testing. They target malformed data, missing
  fields, oversized payloads, asset substitution, negative integers,
  etc. **Semantic** attacks ("call this method when you shouldn't be
  allowed to") belong in the negative channel, not here; see
  `references/05-generators-and-implementation.md §1a–§1b`.
- Bolted on automatically once you declare models in `threatModels`
  and run `propRunActions`. You do not call them yourself.
- Each model can pass / skip (preconditions not met) / fail
  (vulnerability found). The framework **early-stops** a model on its
  first failure for that QuickCheck run.
- Models in `expectedVulnerabilities` have inverted semantics and
  never early-stop — see §E.

## §B. Catalog

18 built-in attacks in
[`allThreatModels`](https://github.com/input-output-hk/sc-testing-tools/blob/chore/without-p/src/testing-interface/lib/Convex/ThreatModel/All.hs)
as non-parameterised `ThreatModel ()` values. Several have
parameterised `…With` siblings exposed by individual modules.
`tokenForgeryAttack` is NOT in `allThreatModels` (it needs a
minting-policy argument). Total: 18 default + 8 parameterised = 26
callable forms.

### B.1 Output redirection

| name | description | applies when |
|---|---|---|
| `unprotectedScriptOutput` | Redirects a continuation script output to a signer's key address (preserves datum). | The tx spends a script input and produces a continuation output to the same script address. |
| `inputDuplication` | Adds a duplicate of an existing input to the tx. | Any tx with inputs. |
| `selfReferenceInjection` / `selfReferenceInjectionWith :: Bool -> ThreatModel ()` | Replaces an address-like field inside an inline datum with the script's own credential. | Script input + continuation output with inline datum that has at least one credential-shaped subterm. `With True` enables verbose counterexamples. |

### B.2 Token / value attacks

| name | description | applies when |
|---|---|---|
| `tokenForgeryAttack` / `tokenForgeryAttackWith :: ScriptData -> PlutusScript lang -> AssetName -> ThreatModel ()` | Mints additional tokens under a supplied policy and adds them to a key-address output. | Contract has (or could be paired with) a minting policy. **Not** in `allThreatModels`. |
| `largeValueAttack` / `largeValueAttackWith :: Int -> ThreatModel ()` | Mints N unique junk tokens (default 50) using an always-succeeds policy and stuffs them into a script output. | Tx has at least one script output. |
| `valueUnderpaymentAttack` / `valueUnderpaymentAttackWith :: Double -> ThreatModel ()` | Reduces ADA on a script output by the given factor (default 0.5). | Tx has a script output carrying more than ~2 ADA. |
| `redeemerAssetSubstitution` | Substitutes asset identifiers referenced in redeemers. | Contract uses redeemers that name assets/policies. |

### B.3 Authorization bypass

| name | description | applies when |
|---|---|---|
| `signatoryRemoval` | Drops a required signer from the tx body. | Tx has at least one entry in `txInfoSignatories`. |
| `timeBoundManipulation` | Widens the tx validity range. | Tx has a non-trivial `txInfoValidRange`. |

### B.4 Data injection (datum / redeemer shape attacks)

| name | description | applies when |
|---|---|---|
| `largeDataAttack` / `largeDataAttackWith :: Int -> ThreatModel ()` | Appends N extra fields (default 1000) of `ScriptDataNumber 42` to an inline-datum constructor. | Script input + script output with inline datum whose top-level shape is `ScriptDataConstructor`. |
| `datumByteBloatAttack` / `datumByteBloatAttackWith :: Int -> ThreatModel ()` | Inflates the first list-item bytestring inside a datum to N bytes (default 10 000). | Inline datum contains a non-empty list whose first item is a ByteString-like value. |
| `datumListBloatAttack` / `datumListBloatAttackWith :: Int -> Int -> ThreatModel ()` | Appends N items of M bytes to every list field in an inline datum (defaults 5 × 100). | Inline datum contains at least one list field. |
| `duplicateListEntryAttack` | Duplicates the first entry of every non-empty list field. | Inline datum contains a non-empty list. |
| `negativeIntegerAttack` | Replaces integer fields with negative values. | Datum or redeemer carries integer fields where sign matters. |
| `invalidDatumIndexAttack` | Targets datum-lookup-by-index patterns with out-of-range indices. | Validator selects datum entries by index. |
| `missingOutputDatumAttack` | Omits a required output datum. | Script outputs carry datums the validator expects. |
| `outputDatumHashMissingAttack` | Omits the datum hash on an output. | Script outputs use datum-hash references. |

### B.5 Composite / satisfaction

| name | description | applies when |
|---|---|---|
| `doubleSatisfaction` | Duplicates a script input and checks whether a single output satisfies both. | Tx has at least one script input that could be confused with another contract's payment requirement. |
| `mutualExclusionAttack` | Tests ordering / race-condition assumptions by permuting or pairing inputs. | Validator depends on input ordering or "one-of-N" exclusion. |
| `invalidScriptPurposeAttack :: PlutusScript lang -> ThreatModel ()` | Probes script-purpose confusion (e.g. invoking the validator under a wrong purpose such as minting vs spending) against the supplied validator script. | Validator branches on `ScriptPurpose` or implicitly assumes a single purpose. Must supply the validator script. |

## §C. Decision tree

Walk these five probes against the validator. For each "yes", add the
listed models. After the probes there is a default-on bucket and a
few situational additions.

### Probe 1 — Continuation outputs (UTxO continues at script address)?

- `unprotectedScriptOutput` — skip if no continuation output.
- `inputDuplication` — almost always cheap; skip only if you've
  already proven the validator inspects input multiplicity.
- `selfReferenceInjection` — only if the datum carries an
  address-like field. Skip if no datum field is an address.

### Probe 2 — Validator inspects `txInfoSignatories`?

- `signatoryRemoval` — skip if the tx never carries required signers.

### Probe 3 — Mints tokens (or could attacker mint using this contract's policies)?

- `tokenForgeryAttack <policy> <assetName>` — skip if no minting
  policy in scope; must supply policy + asset name (not parameter-free).
- `redeemerAssetSubstitution` — skip if redeemers don't reference
  asset identifiers.

### Probe 4 — Validator checks `txInfoValidRange` (POSIX time bounds)?

- `timeBoundManipulation` — skip if the tx never sets validity bounds.

### Probe 5 — Permissive datum / redeemer parsing?

Symptoms: `unsafeFromBuiltinData` with no field-count check, list
fields, integer fields where signs matter, bytestring fields without
length checks.

- `largeDataAttackWith N` (N=10 for fast runs; default 1000) — skip
  if datum is a sealed product with strict field-count parsing.
- `datumByteBloatAttackWith N` — skip if datum contains no
  bytestrings inside lists.
- `negativeIntegerAttack` — skip if datums/redeemers carry no
  integers.
- `duplicateListEntryAttack` — skip if validator already enforces
  uniqueness (e.g. uses a set abstraction).
- `largeValueAttackWith N` — skip if output value structure is
  whitelisted.

### Default-on for any contract handling Ada / native-token outputs

- `valueUnderpaymentAttack` (or `valueUnderpaymentAttackWith N`) —
  skip only if the validator does exact value equality rather than
  `>=`.

### Situational additions

- `doubleSatisfaction` — for any contract whose validity could be
  confused with another's. Common in marketplaces, escrows,
  multi-output payouts.
- `mutualExclusionAttack` — when the validator depends on a specific
  input ordering or a "one-of-N" invariant.

### State-machine contracts with continuation outputs (PingPong-shape)

Contracts that consume a script UTxO and produce a continuation
output carrying a new datum (PingPong, state-machine validators,
auction step machines, multisig advance-state, …) benefit from a
specific bundle on top of the conservative defaults:

- `invalidDatumIndexAttack` — probes datum-lookup-by-index patterns
  with out-of-range indices.
- `invalidScriptPurposeAttack <validatorScript>` — supplies the
  validator's own script and exercises script-purpose confusion
  (e.g. spending vs minting purpose). Requires the validator script
  value at suite-construction time.
- `missingOutputDatumAttack` — omits a required output datum on a
  script output the validator expects.
- `outputDatumHashMissingAttack` — omits the datum hash on a
  hash-datum-style output.

These four are in the canonical `PingPongSpec.hs` and any contract
that maintains state through continuation outputs with datums should
include them.

### Conservative default set

When in doubt, start with this and let QuickCheck flag irrelevant
models as SKIPPED:

```haskell
[ unprotectedScriptOutput
, doubleSatisfaction
, signatoryRemoval
, valueUnderpaymentAttack
, largeDataAttackWith 10
]
```

For state-machine / continuation-output contracts, extend with:

```haskell
, invalidDatumIndexAttack
, invalidScriptPurposeAttack myValidatorScript
, missingOutputDatumAttack
, outputDatumHashMissingAttack
```

## §D. Wiring

After confirming the selection with the user, populate the previously
empty `ThreatModelsFor` instance.

```haskell
import Convex.ThreatModel                          (ThreatModel)
import Convex.ThreatModel.UnprotectedScriptOutput  (unprotectedScriptOutput)
import Convex.ThreatModel.DoubleSatisfaction       (doubleSatisfaction)
import Convex.ThreatModel.SignatoryRemoval         (signatoryRemoval)
import Convex.ThreatModel.LargeData                (largeDataAttackWith)

instance ThreatModelsFor MyModel where
  threatModels =
    [ unprotectedScriptOutput
    , doubleSatisfaction
    , signatoryRemoval
    , largeDataAttackWith 10
    ]
  expectedVulnerabilities = []
```

Notes:

- Per-attack modules live under `Convex.ThreatModel.<Name>` on
  [chore/without-p](https://github.com/input-output-hk/sc-testing-tools/tree/chore/without-p/src/testing-interface/lib/Convex/ThreatModel).
- `Convex.ThreatModel.All` only re-exports `allThreatModels`;
  individual names must be imported from per-attack modules. See
  `04-helpers-cheatsheet.md §E`.
- Framework auto-runs each model against every tx produced by
  `perform`. Nothing else needed beyond declaring the list.
- After populating, re-run `cabal test`. Each model becomes its own
  test case in the tasty tree (`Threat model: <name>`). Expect each
  to PASS (= attack was correctly rejected). A FAIL is a real
  vulnerability finding.

## §E. `expectedVulnerabilities`

**Inverted semantics.** A test in `expectedVulnerabilities` passes
if the attack SUCCEEDS — i.e. the vulnerability is consistently
exploitable.

When to populate:

- CTF-style intentionally-vulnerable contracts.
- Documenting known issues for regression testing (catches accidental
  fixes that change the surface area).
- Verifying a vulnerability exists *before* writing the fix.

Behavioural difference from `threatModels`:

- `threatModels` early-stops on first failure for a model.
- `expectedVulnerabilities` runs the model against ALL transactions
  in the positive suite — to confirm consistency, not a one-off.
- Output is quieter: no verbose transaction dumps.

**Skill default**: leave `expectedVulnerabilities = []`. Populate
only when the user explicitly asks. If they do, ask which models and
why before writing.

Examples that exercise this field heavily:
[`AikenVestingSpec.hs`](https://github.com/input-output-hk/sc-testing-tools/blob/chore/without-p/src/testing-interface/test/AikenVestingSpec.hs),
[`AikenSellNftSpec.hs`](https://github.com/input-output-hk/sc-testing-tools/blob/chore/without-p/src/testing-interface/test/AikenSellNftSpec.hs),
[`AikenMultisigTreasurySpec.hs`](https://github.com/input-output-hk/sc-testing-tools/blob/chore/without-p/src/testing-interface/test/AikenMultisigTreasurySpec.hs)
(and `V2` / `V3` variants),
[`AikenKingOfCardanoSpec.hs`](https://github.com/input-output-hk/sc-testing-tools/blob/chore/without-p/src/testing-interface/test/AikenKingOfCardanoSpec.hs),
[`AikenTipJarSpec.hs`](https://github.com/input-output-hk/sc-testing-tools/blob/chore/without-p/src/testing-interface/test/AikenTipJarSpec.hs).

## §F. Custom threat models

Out of scope for this skill. If the user needs a bespoke attack
pattern, point them at the `ThreatModel` monad and its combinators
(`anyInput`, `anyOutput`, `pickAny`, `shouldValidate`,
`shouldNotValidate`, `Named`, plus `TxModifier` `Monoid` composition
with `<>`). Documented under "Writing Custom Threat Models" in the
[chore/without-p README](https://github.com/input-output-hk/sc-testing-tools/blob/chore/without-p/README.md).
