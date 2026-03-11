# Goby Project State Snapshot

Last updated: 2026-03-11

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- Active work is `doc/PLAN.md` Track F: maintainability hardening.
- Current target is shrinking `crates/goby-wasm/src/lib.rs` by moving
  `RuntimeOutputResolver` responsibilities into focused modules without changing behavior.

## Current State

- `goby-wasm` runtime split completed so far:
  - `crates/goby-wasm/src/runtime_value.rs`
  - `crates/goby-wasm/src/runtime_env.rs`
  - `crates/goby-wasm/src/runtime_flow.rs`
  - `crates/goby-wasm/src/runtime_eval.rs`
  - `crates/goby-wasm/src/runtime_resolver.rs`
  - `crates/goby-wasm/src/runtime_dispatch.rs`
- `crates/goby-wasm/src/lib.rs` still contains the deeper orchestration layer:
  - `apply_cont`
  - declaration execution / callable dispatch
  - remaining evaluator/orchestration glue
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

- Continue Track F in `goby-wasm`:
  - extract `apply_cont` and declaration-execution helpers from `crates/goby-wasm/src/lib.rs`
  - keep changes behavior-preserving and modularity-only
  - rerun the same quality gate after each extraction
- After `goby-wasm` F1 work, move on to `goby-core` responsibility splits:
  - `typecheck.rs`
  - `parser.rs`

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
