# testing-interface skill

A skill (instruction set) that guides a coding agent through adding a
`convex-testing-interface` property-test suite to a Cardano contract repo.

The format follows the Agent Skills convention (`SKILL.md` + a `references/`
folder), so it works with any agent that loads local skills or instruction
sets from a directory in your repo.

## Install it

### Let your agent do it for you

Paste this into your agent:

```
Install the testing-interface skill into this repo.

Fetch docs/skill/skill-v1/ from
https://github.com/input-output-hk/sc-testing-tools
and put it in this repo at the location you use for local
skills or instruction sets (e.g. .claude/skills/,
.opencode/skills/, or whatever your tool uses).
Name the destination folder `testing-interface` and include
SKILL.md and the references/ folder.

Then load the skill and follow it. Help me work on the
property-test suite for the contract in this repo.
```

### Or install it manually

Clone this repo and copy `docs/skill/skill-v1/` into your agent's
skills directory under the name `testing-interface`. Restart your
agent if it was already running.

## What it does

The skill probes the target repo, classifies it into one of six phases
(fresh, setup-done, implemented, threat-models-wired, green-maintenance,
red-repair), and walks the agent through the right next steps for that
phase. It enforces the Cardinal Rule (deployment is an action, not setup
— `initialize` is model-only) and the two-channel test architecture
(positive and negative rounds).

## Source of truth

The framework this skill teaches lives at
[input-output-hk/sc-testing-tools](https://github.com/input-output-hk/sc-testing-tools)
on the `main` branch. The reference example is `PingPongSpec.hs` under
`src/testing-interface/test/`.
