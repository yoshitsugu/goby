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

## Test Runner Choice

- `goby-core` tests are fast (`cargo test -p goby-core --lib` finishes in
  under a second).
- `goby-wasm` runs wasmtime + cranelift JIT per case, so its full lib suite
  and the `wasm_exports_and_smoke` / acceptance integration tests under
  `crates/goby-wasm/tests/` take 5–10+ minutes under `cargo test`. When the
  full suite is needed, use **`cargo nextest run -p goby-wasm`** instead —
  nextest spawns a separate test binary per case, which sidesteps wasmtime's
  internal locking and cuts wall time dramatically. If `cargo nextest` is
  not installed (or not available on the current host), fall back to plain
  `cargo test -p goby-wasm` and accept the longer wall time.
- When the change is localised, narrow the run with
  `cargo test -p goby-wasm --lib <module>::tests` (e.g.
  `planning::tests` finishes 16 cases in well under a second).
