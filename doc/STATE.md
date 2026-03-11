# Goby Project State Snapshot

Last updated: 2026-03-11

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- Active work is `doc/PLAN.md` Track F: maintainability hardening.
- `Milestone F1` (`goby-wasm` split) is complete.
- Current target is `Milestone F2`: start splitting `crates/goby-core/src/typecheck.rs`
  by responsibility without changing behavior.

## Current State

- `goby-wasm` runtime split completed so far:
  - `crates/goby-wasm/src/runtime_value.rs`
  - `crates/goby-wasm/src/runtime_env.rs`
  - `crates/goby-wasm/src/runtime_flow.rs`
  - `crates/goby-wasm/src/runtime_eval.rs`
  - `crates/goby-wasm/src/runtime_resolver.rs`
  - `crates/goby-wasm/src/runtime_dispatch.rs`
  - `crates/goby-wasm/src/runtime_decl.rs`
  - `crates/goby-wasm/src/runtime_exec.rs`
  - `crates/goby-wasm/src/runtime_replay.rs`
- `crates/goby-wasm/src/lib.rs` now mainly holds the public codegen entrypoint,
  fallback orchestration glue, and remaining helper methods that were not part of F1 extraction scope.
- Next maintainability target is `crates/goby-core/src/typecheck.rs`, which still mixes:
  - type environment construction
  - import / stdlib / intrinsic validation
  - effect dependency validation
  - expression / statement checking
- `F2.1` is now landed:
  - `crates/goby-core/src/typecheck_env.rs` owns `Ty`, `TypeEnv`, `ResumeContext`,
    effect-map structs, and related internal binding data.
  - `crates/goby-core/src/typecheck.rs` now consumes that module instead of defining those internals inline.
- Runtime model to preserve while refactoring:
  - `Out<T> = Done | Suspend | Escape | Err`
  - `Escape::WithScope { with_id, value }`
  - lexical nearest-handler dispatch
  - parity between fallback runtime behavior and current tests

## Verified

- `cargo fmt`
- `cargo check`
- `cargo test -p goby-wasm`
- `cargo test --workspace`

## Next Work

- Start `Milestone F2` in `goby-core`:
  - move import / stdlib / intrinsic policy validation into a dedicated internal module
  - keep `typecheck_module_with_context` as the top-level orchestrator
  - preserve current diagnostics and test corpus while moving code
- After the first `typecheck.rs` split lands, continue with later F2 steps and then `parser.rs`.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
