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

- Continue from `doc/PLAN.md` Step 3 Phase 5.
- Remaining Phase 5 targets:
  - `eval_expr_ast_outcome` and `complete_ast_value_outcome` still active in
    `eval_expr_ast` internals and legacy fallback helper paths.
  - Old types `AstContinuation`, `AstContinuationFrame`, `AstValueContinuation`,
    `AstEvalOutcome`, `HandlerContinuationState` still present.
  - `pending_value_continuations` field still in RuntimeOutputResolver.
  - `runtime_aborted` / `set_runtime_abort_once` / `has_abort_without_error`
    still used in legacy `eval_ast_side_effect` / ingest path.
- Next restart point (first step):
  - target only `apply_named_value_call_args_out` migration to `Out` path
    (no broad `eval_expr_ast` rewrite in the same step).
  - execution flow for the next step:
    - record expected breakages under this file as:
      `## Expected breakages (current step)` before code changes.
    - first write down "what is expected to break" (tests, paths, runtime behavior).
    - then implement the design-oriented change.
    - if breakages are not controllable/understandable, roll back immediately to the
      last stable commit and split into a smaller change.
  - implementation rule:
    - add/route via `Out` helper first,
    - keep legacy AST/Option fallback until tests prove parity.
  - run these focused tests before full suite:
    - `declaration_value_call_replays_nested_binding_progression`
    - `typed_mode_matches_fallback_for_declaration_value_call_progression`
    - `resolves_runtime_output_for_standalone_case_with_effectful_arm_bodies`
    - `typed_mode_matches_fallback_for_resume_success_path`
    - `resume_replays_binding_value_continuation_into_following_statements`
    - `resume_replays_multi_arg_named_call_arguments`
- Completed in this session:
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
    `eval_expr` (`Out` path) instead of `eval_expr_ast` (`Option` path), so
    `Suspend` / `Escape` / runtime error propagation is preserved on the active
    runtime path.
  - `eval_ast_side_effect` `Case` / `If` handling now evaluates condition and
    selected branch via `eval_expr` (`Out` path), while preserving unit-position
    fallback execution for unsupported selected branches.
  - `eval_expr` `Case` / `If` now preserve legacy parity by falling back to
    `execute_unit_expr_ast` for unsupported selected branches and returning
    `RuntimeValue::Unit` on success.
  - `apply_named_value_call_out` now evaluates declaration bodies via
    `eval_stmts` (`Out` path) through `eval_decl_as_value_with_args_out` before
    falling back to legacy AST `Option` path, improving continuation-safe value
    call behavior on the active runtime path.
- Quality gate passing: `cargo fmt`, `cargo clippy -p goby-wasm -- -D warnings`,
  `cargo test -p goby-wasm` (unit 209 passed, integration 6 passed), `cargo check`.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
