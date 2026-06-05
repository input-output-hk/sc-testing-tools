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
2. **`cabal.project` contains the `sc-testing-tools` source-repo
   stanza.** Grep for `sc-testing-tools` near a `source-repository-package`
   block; confirm the tag is on the `chore/without-p` branch by matching
   the pinned commit hash documented in `02-project-setup.md §C.3`.
3. **A `.cabal` test stanza depends on `convex-testing-interface`.**
   Grep `*.cabal` files for that build-dep inside a `test-suite` stanza.
4. **A `.hs` file under `test/` imports `Convex.TestingInterface`.**
   Grep for the exact import line.
5. **Some module defines `instance TestingInterface` for a concrete
   state type.** Grep for `instance TestingInterface`. Note the state
   type name for use in check 6.
6. **The same state type has an `instance ThreatModelsFor` declaration.**
   Grep for `instance ThreatModelsFor <Name>` matching the type from
   check 5.
7. **Last build status.** Only run this when the notepad is absent or
   the most-recent file modification time under `src/` or `test/` is
   newer than the notepad's `## last build` line. Run
   `cabal build --dry-run` against the test target. If the plan
   resolves cleanly, classification proceeds; if the plan fails the
   phase is **Red-repair** regardless of the other results.

Do NOT run a full `cabal build` during the probe. Dry-run only.

## C. What each check tells you

| Check fails | What it means |
|---|---|
| 2 | No cabal pin yet. Fresh phase or partial setup. |
| 3 | Cabal pin exists but no test target wired. Setup-done if 2 ok, else Fresh. |
| 4 | Test target exists but no test module imports the framework. Setup-done. |
| 5 | Framework imported but no instance written. Setup-done. |
| 6 | Instance exists but no threat models wired. Implemented. |
| 7 | Build is broken regardless of structure. Red-repair. |

## D. Mapping table

Read the result tuple top-to-bottom; the first row that matches wins.

- check 7 = error → **Red-repair**
- check 2 = absent → **Fresh**
- check 5 = absent → **Setup-done**
- check 6 = absent → **Implemented**
- check 6 = present and notepad's `## last build` = `green ...` and
  records at least one green test run → **Green-maintenance**
- check 6 = present, no green test run recorded → **Threat-models-wired**

When the notepad disagrees with the evidence on the green/red
distinction, the evidence wins. The probe subagent overwrites the
notepad's `# current phase` line as part of its state-file diff.

## E. Decision flow (bullets, not flowchart)

- Check 1. If absent, jump straight to check 2. If present, read all
  six sections, hold the hint, continue.
- Check 2. If absent → Fresh; emit phase and return.
- Check 3. If absent → Fresh (the pin alone is not enough); emit and
  return.
- Check 4. If absent → Setup-done; emit and return.
- Check 5. If absent → Setup-done; emit and return.
- Check 6. If absent → Implemented; emit and return.
- Check 7. Only run if the notepad is absent or stale. If the dry-run
  plan fails → Red-repair; emit and return.
- All checks present and build plan clean → consult notepad's
  `## last build` and decisions log. Green run recorded →
  Green-maintenance. Otherwise → Threat-models-wired.

## F. What the probe subagent returns

The probe is a regular subagent and obeys the four-part return contract
in `SKILL.md §6`. Concretely:

- **Outcome.** `success` with the chosen phase label inline, e.g.
  `success — phase: Implemented`.
- **Changed.** Empty bullet list (the probe writes only the state file).
- **Blocker.** `none` unless a check returned `error` for a non-build
  reason (e.g. unreadable cabal file).
- **State-file diff.** The lines to update in `.skill-state.md`:
  `# current phase`, `## last build` (only if check 7 ran),
  optionally an entry to `## decisions log`
  (e.g. `- probe: classified as Implemented`).

The main agent then opens the relevant section of `SKILL.md §4` and
loads the reference files from the routing table in `SKILL.md §3`.
