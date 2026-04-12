# Claude Code Entry — Goby

Read `AGENTS.md` for product invariants. This file adds Claude-Code-specific workflow.

## Defaults

- Respond in Japanese, terse. Explain *why* for fixes; skip restating *what*.
- Use the `codex-reviewed-stepwise-dev-flow` Skill for any non-trivial code change.
- Before reading source, consult `REPO_MANIFEST.md` to pick the owning crate.

## Task-Specific Skills (load only when the trigger fits)

- `goby-navigate` — starting any code task: which files to read, which to skip, impact radius.
- `goby-verify` — running checks and reading failure output with minimum scope.
- `goby-invariants` — before commit: spec / examples / diagnostics sync gate.

## Hard Rules

- No destructive git ops (`reset --hard`, `checkout --`, force push) without explicit ask.
- Preserve diagnostics wording/spans unless the change is explicitly about diagnostics.
- Spec/examples/PLAN updates must ship in the same change as syntax or semantic changes.
