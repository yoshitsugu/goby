# Goby Agent Guidance (Global)

This is the minimum durable guidance for all agents in this repository.
Keep this file product-focused. Developer/process preferences should live in each
developer's `$HOME/.codex` setup, not in this repository.

## Summary

- Goby is an early-stage statically typed functional language with effects.
- Breaking changes are acceptable when they improve clarity and consistency.
- Readability, explicit boundaries, and practical tooling are top priorities.

## Always-Global Invariants

- Treat `doc/LANGUAGE_SPEC.md` as the source of truth for current language behavior.
- Treat `doc/PLAN.md` as roadmap/decision context.
- If syntax/semantics/core terminology changes, update in the same change:
  - `doc/LANGUAGE_SPEC.md`
  - `doc/PLAN.md`
  - relevant `examples/*.gb`
- Keep `README.md` high-level; do not move detailed spec text there.
- Prefer root-cause fixes at the correct ownership boundary over one-off patches.
- Preserve behavior/diagnostics unless behavior change is explicitly intended.

## Minimal Quality Gate

- Required for meaningful code changes:
  - `cargo fmt`
  - `cargo check`
  - `cargo test`
- Prefer focused tests during iteration, then run the broader gate before handoff.

## Context Expansion Order

1. `REPO_MANIFEST.md` (repo map)
2. Relevant source files for the task
3. Source files
4. Broader docs only if still blocked

For changed-files-first workflows, use your local `$HOME/.codex` tooling.

## Restart Safety

- Keep `doc/STATE.md` concise and execution-oriented.
- At milestones, record locked decisions, open questions, and immediate next steps.
