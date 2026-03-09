# Goby Project State Snapshot

Last updated: 2026-03-09 (session 237)

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

- Continue from `doc/PLAN.md` Step 3 Phase 5.
- Remaining Phase 5 targets:
  - `eval_expr_ast_outcome` and `complete_ast_value_outcome` still active in
    `eval_expr_ast` internals and legacy fallback helper paths.
  - Old types `AstContinuation`, `AstContinuationFrame`, `AstValueContinuation`,
    `AstEvalOutcome`, `HandlerContinuationState` still present.
  - `pending_value_continuations` field still in RuntimeOutputResolver.
  - `runtime_aborted` / `set_runtime_abort_once` / `has_abort_without_error`
    still used in legacy `eval_ast_side_effect` / ingest path.
- Next restart point:
  - Assess next migration target: `apply_named_value_call_args_ast_outcome` or
    `apply_named_value_call_ast_outcome` migration to Out path, OR
    begin 5b-inner (`eval_expr_ast_outcome` internal self-calls → `eval_expr`).
  - Same execution flow rule: record expected breakages before code changes;
    roll back if breakages are not controllable within a narrow scope.
  - implementation rule:
    - add/route via `Out` helper first,
    - keep legacy AST/Option fallback until tests prove parity.

## Completed in Last Session (2026-03-09)

  - `apply_named_value_call_args_out` now uses an Out-first path that mirrors
    `apply_named_value_call_out` ordering:
    handler dispatch → `__goby_` intrinsic → `eval_decl_as_value_with_args_out`
    → legacy AST fallback. Multi-arg calls now properly propagate
    Suspend/Escape instead of silently discarding them via the Option path.
  - Added `dispatch_handler_method_as_value_with_args_flow` (Out-returning
    multi-arg handler dispatch, mirrors single-arg `_flow` version).
  - Added `debug_assert!(arg_values.len() >= 2)` to document call invariant.
  - commit: bef7b9a

## Previously Completed

  - `execute_unit_expr_ast` `Expr::With` migrated to `eval_stmts` +
    `FinishKind::WithBody`; legacy `execute_ast_stmt_sequence` removed.
  - `execute_unit_ast_stmt` Binding/Assign arms migrated from
    `eval_expr_ast_outcome` to `eval_expr` (`Out` path).
  - `execute_unit_ast_stmt` return type migrated to `Out<()>`; legacy
    `execute_unit_ast_stmt_outcome` removed and `eval_stmts` fallback now consumes
    `Out<()>` directly.
  - `execute_unit_expr_ast` return type migrated to `Out<()>`; key call sites and
    legacy fallback branches now consume `Out` instead of `Option`.
  - `eval_ast_side_effect` print/println/pipeline side-effect branches now use
    `eval_expr` (`Out` path) instead of `eval_expr_ast` (`Option` path).
  - `eval_ast_side_effect` `Case` / `If` handling now uses `eval_expr` (`Out` path).
  - `eval_expr` `Case` / `If` fall back to `execute_unit_expr_ast` for unsupported
    selected branches.
  - `apply_named_value_call_out` evaluates declaration bodies via
    `eval_decl_as_value_with_args_out` before falling back to legacy AST path.

- Quality gate passing: `cargo fmt`, `cargo clippy -p goby-wasm -- -D warnings`,
  `cargo test -p goby-wasm` (unit 209 passed, integration 6 passed), `cargo check`.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
