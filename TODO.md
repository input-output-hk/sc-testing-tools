# Threat-model harness follow-ups

Parked findings from the code reviews of the rebalancing/withdrawals work
(September 2026). The load-bearing correctness fixes from those reviews are
done; everything below is real but deliberately deferred. Items link to the
code as of `feat/improvements`.

## Correctness / robustness

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

## Cleanups

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
