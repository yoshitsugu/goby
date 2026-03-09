# Goby Project State Snapshot

Last updated: 2026-03-09

This file is a restart-safe snapshot for resuming work after context reset.

## Current Status

- 4.7 scoped handler exit semantics are implemented.
- Current shipped behavior:
  - if a handler clause finishes without `resume`, evaluation exits only the
    current `with ... in ...` scope,
  - the clause result becomes the whole-`with` result,
  - explicit `resume` progression and continuation-consumed runtime errors still
    work.
- Active docs are in sync:
  - `doc/LANGUAGE_SPEC.md`
  - `doc/PLAN.md`
  - `examples/effect.gb`

## Runtime Shape

- Active value-path runtime uses:
  - `Out<T> = Done | Suspend | Escape | Err`
  - `Escape::WithScope { with_id, value }`
  - `FinishKind::WithBody { with_id }`
  - `FinishKind::HandlerBody { token_idx, produce_value, with_id }`
- Value-position `with` runs on the active `eval_expr` / `eval_stmts` /
  `apply_cont` path and consumes matching scoped exits at the `with` boundary.
- Statement-position `with` still uses the legacy unit stmt-sequence wrapper for
  replay, but scoped exit is absorbed at that `with` boundary instead of
  aborting the whole program.

## Verified

- `cargo fmt`
- `cargo clippy -p goby-wasm -- -D warnings`
- `cargo test -p goby-wasm`
- `cargo check`

## Next Work

- Continue from `doc/PLAN.md` Step 3 Phase 5 (sub-steps 5b-1/5b-2/5b-5/5b-6 done).
- Remaining Phase 5 targets:
  - `execute_ast_stmt_sequence` still called from `execute_unit_expr_ast` Expr::With
    body (line ~3598) — depends on `pending_caller_cont_stack`; migration requires
    Option B (migrate execute_unit_expr_ast to return `Out<()>`).
  - `eval_expr_ast_outcome` and `complete_ast_value_outcome` still active in
    `eval_expr_ast` internals and `execute_unit_ast_stmt` Binding/Assign arms.
  - Old types `AstContinuation`, `AstContinuationFrame`, `AstValueContinuation`,
    `AstEvalOutcome`, `HandlerContinuationState` still present.
  - `pending_value_continuations` field still in RuntimeOutputResolver.
  - `runtime_aborted` / `set_runtime_abort_once` / `has_abort_without_error`
    still used in execute_unit_expr_ast Expr::With arm.
- Quality gate passing: cargo fmt, cargo clippy -p goby-wasm -- -D warnings,
  cargo test --workspace all clean as of session 236.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
