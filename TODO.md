# Threat-model harness follow-ups

Parked findings from the code reviews of the rebalancing/withdrawals work
(September 2026). The load-bearing correctness fixes from those reviews are
done; everything below is real but deliberately deferred. Items link to the
code as of `feat/improvements`.

## Correctness / robustness

- [ ] **`recalculateTotalCollateral` reports only the first candidate's
  error** (`ThreatModel/Cardano/Api.hs`, `firstRight`). When every collateral
  candidate fails, the message surfaced via the QuickCheck table,
  `tmceRebalanceError` and the report is the richest candidate's, hiding a
  later candidate's more specific failure (e.g. "Insufficient collateral:
  inputs=1000000" while a 2-ADA token-carrying input actually failed on the
  return output's minimum ADA). Fix: join the errors of all candidates, or
  report the one that got furthest.

## Design

- [ ] **`requireScriptExecution` accepts any redeemer, not just the target
  output's guardian** (`ThreatModel.hs`). The output-mutation attacks it
  gates only need *some* Plutus script to run, so on a transaction whose sole
  script is, say, a thread-token minting policy that never looks at outputs,
  while a key input is paid into spending validator V for the first time,
  mutating the V output "still validates" vacuously and is reported as a
  vulnerability of V. No scenario in the repo triggers this today (the
  RewardWithdrawal validator does inspect its outputs). Options: require some
  redeemer's script hash to equal the target output's payment credential, or
  document that such setup transactions belong in `acceptedFindings`.

- [ ] **Replace the `usesDefaultTms` name-comparison heuristic with an
  explicit signal** (`TestingInterface.hs`). Recognizing "did the user
  override `threatModels`?" by comparing name lists (a) evaluates
  `defaultThreatModelsExcluding`, so an unnamed model in
  `expectedVulnerabilities`/`acceptedFindings` hits the "Unexpected unnamed
  threat model" error even for a fully-overridden instance, and (b)
  misclassifies an explicit list that happens to equal the default, silently
  dropping its coverage claim. Express the intent in the class instead, e.g.
  a defaulted `threatModelsAreExplicit :: Bool` or a `ThreatModelSelection`
  type.

- [ ] **Decide a coverage policy for environmental skips** (`ThreatModel.hs`
  `runThreatModelM'`, and `TestingInterface.hs` `threatModelTestCase`/
  `expectedVulnTestCase`). Two faces of the same question:

  1. Direct M-runners: rebalance failures skip the env with only a
     QuickCheck table entry (visible on completed runs only), so a "secure
     against X" property (e.g. `BountySpec`, `AikenKingOfCardanoSpec`) can
     pass while some envs were never attacked; if *every* env fails, the run
     at least fails as an unexplained "Gave up!".
  2. Tasty test cases: the vacuity check is guarded by
     `numSkippedPhase1 + numErrors == 0`, so a claimed model that is 100%
     *environmentally* skipped (e.g. its attack never rebalances) stays
     green forever with the soft SKIPPED step — permanent systematic zero
     coverage is indistinguishable from transient flakiness, and a single
     environmental skip disables the coverage-claim failure entirely.

  Consider counting environmental skips and failing (or warning loudly) when
  attack coverage stays at zero — mind the trade-off recorded at the skip
  sites: hard-failing per-iteration would regress the skip-not-fail
  semantics for harness limitations.

- [ ] **Key the shared threat-model outcome map by group, not just name**
  (`TestingInterface.hs`, `positiveTest*`/`tmRecord`). Outcomes are collected
  in one map keyed by the model's `Named` name, so a parameterized variant in
  `acceptedFindings`/`expectedVulnerabilities` sharing a name with an
  explicitly listed threat model (e.g. `largeValueAttackWith 10` vs
  `largeValueAttackWith 1000` — both "Large Value Attack") leaks its
  `TMFailed` outcomes into the "Threat models" test case and fails it, while
  `alreadyFailed` then suppresses the innocent listed model. Mitigated today
  (the default list excludes same-named models), but key by (group, name) or
  reject duplicate names at setup.

- [ ] **Consider `-Werror=missing-fields`** (cabal `lang` stanza). Adding a
  field to `ThreatModelEnv` compiled every record construction into a runtime
  bottom with only a warning; this repo builds cleanly today, so promoting
  the warning is cheap insurance.

## Cleanups

- [ ] **RewardWithdrawal spec duplication** (`src/use-cases/test/
  RewardWithdrawal/Spec/{Unit,Prop}.hs`). The register/withdraw-zero tx
  builders exist in ~5 copies across the two files; the model recompiles the
  validator via UPLC parameter application on every `perform`; three of the
  four model fields are constants. Share the builders (a `Spec.Common`
  module), add a top-level applied-script CAF, shrink the model to the one
  real state bit (`_registered`).

- [ ] **Dead exports**: `adjustChangeOutputM` and `getTxFeeCoin`
  (`ThreatModel/Cardano/Api.hs`) have no callers — their twin
  `rebalanceAndSignM` was already removed for the same reason.

- [ ] **`mkWithdrawalSummary` duplicates `mkInputSummary`'s four-field
  redeemer projection** (`Trace/TxSummary.hs`).

- [ ] **Deduplicate the outcome-status schema blocks**
  (`src/schema-gen/lib/Convex/SchemaGen.hs`). The `skippedPhase1` block is a
  verbatim copy of the adjacent `skipped` block differing only in the enum
  string, and `failed`/`err` follow the same status-plus-companion-field
  template; each new `ThreatModelTraceOutcome` variant means another ~9-line
  copy that can silently drift from the JSON encoder. Extract a local helper
  (e.g. `statusVariant :: Text -> [Text] -> Schema`), which also serves the
  identically patterned `IterationStatus` instance above it.

## Deferred by decision (revisit conditions, not bugs)

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
