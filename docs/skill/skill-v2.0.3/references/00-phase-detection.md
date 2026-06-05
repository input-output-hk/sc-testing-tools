# Reference: Phase-0 probe

The probe is the first action of every session. It classifies the trial
repo into one of six phases and the main agent routes from there. The
probe runs as a single subagent. Cheapest checks first; stop early when
the classification is unambiguous.

## A. Short-circuit on the notepad

Before any grep or build, check `.skill-state.md` at the trial-repo root.

- **File absent.** No prior session. Continue with the rest of the probe.
- **File present.** Read its `# current phase` line and its `## last
  build` line. These are HINTS, not the final classification. Continue
  the probe; if the rest of the evidence agrees, return the hinted
  phase immediately. If it disagrees, trust the evidence and overwrite
  the notepad.
- **Resume case.** When the file is present and the user opens with
  "continue", "resume", "where were we", or similar, the notepad is the
  primary source. Read all six sections and brief the user with a
  three-line summary: current phase, last build status, open question
  (if any). Then proceed to that phase's entry section in `SKILL.md §4`.

The notepad never overrides §0 guardrails. If a stale notepad records
forbidden git operations or off-branch references, ignore those entries
and continue.

## B. The seven checks, cheapest first

Each check returns one of: `present`, `absent`, `error`. The phase
mapping in §D consumes the tuple of results.

1. **`.skill-state.md` exists.** Simple file-exists probe at the
   trial-repo root.
2. **Build-wrapper detection.** Look for `flake.nix` at the repo root.
   - **Present** → build wrapper is `nix`. Every subsequent build / test
     command must be routed through `nix develop -c <cmd>`. **Inherit
     any `NIX_CONFIG` from the user's environment; do not override it.**
   - **Absent** → look for `cabal.project`. If present, build wrapper is
     `cabal` and commands are invoked directly. If both are absent, the
     repo isn't a Haskell project; mark this check `error` and stop.
   The chosen wrapper is recorded in `.skill-state.md` on the
   `build-wrapper:` line (schema in `SKILL.md §5`).
3. **`cabal.project` contains the `sc-testing-tools` source-repo
   stanza.** Grep for `sc-testing-tools` near a `source-repository-package`
   block; confirm the tag is on the `chore/without-p` branch by matching
   the pinned commit hash documented in `02-project-setup.md §C.3`.
4. **A `.cabal` test stanza depends on `convex-testing-interface`.**
   Grep `*.cabal` files for that build-dep inside a `test-suite` stanza.
5. **A `.hs` file under `test/` imports `Convex.TestingInterface`.**
   Grep for the exact import line.
6. **Some module defines `instance TestingInterface` for a concrete
   state type, and the same state type has an `instance ThreatModelsFor`
   declaration.** Grep for both; note the state type name.
7. **Last build status.** Only run when the notepad is absent or the
   most-recent file modification time under `src/` or `test/` is newer
   than the notepad's `## last build` line. Run `cabal build --dry-run`
   against the test target (via the wrapper chosen in check 2). If the
   plan resolves cleanly, classification proceeds; if the plan fails the
   phase is **Red-repair** regardless of the other results.

Do NOT run a full `cabal build` during the probe. Dry-run only.

## C. What each check tells you

| Check fails | What it means |
|---|---|
| 2 | No build system at all. Not a Haskell project. Stop. |
| 3 | No cabal pin yet. Fresh phase or partial setup. |
| 4 | Cabal pin exists but no test target wired. Setup-done if 3 ok. |
| 5 | Test target exists but no test module imports framework. Setup-done. |
| 6 (TestingInterface absent) | Framework imported but no instance written. Setup-done. |
| 6 (ThreatModelsFor absent) | Instance exists but no threat models wired. Implemented. |
| 7 | Build is broken regardless of structure. Red-repair. |

Check 2 never determines a phase by itself — it records the wrapper
and the rest of the probe uses it for command routing.

## D. Mapping table

Read the result tuple top-to-bottom; the first row that matches wins.

- check 7 = error → **Red-repair**
- check 3 = absent → **Fresh**
- check 6 `TestingInterface` = absent → **Setup-done**
- check 6 `ThreatModelsFor` = absent → **Implemented**
- both check-6 instances present, notepad records green test run →
  **Green-maintenance**
- both check-6 instances present, no green test run recorded →
  **Threat-models-wired**

When the notepad disagrees with the evidence on the green/red
distinction, the evidence wins. The probe subagent overwrites the
notepad's `# current phase` line as part of its state-file diff.

## E. Decision flow (bullets, not flowchart)

- Check 1. If absent, jump to check 2. If present, read all sections
  (including `build-wrapper:`), hold the hints, continue.
- Check 2. Record wrapper. If neither `flake.nix` nor `cabal.project`
  exists → blocker, stop.
- Check 3. If absent → Fresh; emit and return.
- Check 4. If absent → Fresh (pin without test target); emit and return.
- Check 5. If absent → Setup-done; emit and return.
- Check 6. If `TestingInterface` absent → Setup-done. If only
  `ThreatModelsFor` absent → Implemented. Emit and return.
- Check 7. Only run if notepad absent or stale. Dry-run fails →
  Red-repair; emit and return.
- All checks pass and build plan clean → consult notepad's
  `## last build`. Green run recorded → Green-maintenance.
  Otherwise → Threat-models-wired.

## F. What the probe subagent returns

The probe is a regular subagent and obeys the four-part return contract
in `SKILL.md §6`. Concretely:

- **Outcome.** `success` with the chosen phase label inline, e.g.
  `success — phase: Implemented`.
- **Changed.** Empty bullet list (the probe writes only the state file).
- **Blocker.** `none` unless a check returned `error` for a non-build
  reason (e.g. unreadable cabal file).
- **State-file diff.** The lines to update in `.skill-state.md`:
  `# current phase`, `build-wrapper:` (always — set from check 2),
  `## last build` (only if check 7 ran), optionally an entry to
  `## decisions log` (e.g. `- probe: classified as Implemented`).

The main agent then opens the relevant section of `SKILL.md §4` and
loads the reference files from the routing table in `SKILL.md §3`.
