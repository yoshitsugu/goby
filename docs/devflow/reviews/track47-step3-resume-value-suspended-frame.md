# Step Review

## Step

- Track 4.7 Step 3.2d nested `resume (...)` suspended-frame slice.

## Contradiction Check

- After multi-arg calls, the AST runtime still treated `Expr::Resume` as a
  direct bridge operation even when its value expression suspended.
- That contradicted the ongoing move toward a single outcome-driven continuation
  path for nested resumable expressions.
- This slice removes that contradiction by making `resume (...)` itself a
  frame-backed evaluator step.

## Extensibility Check

- `ResumeValue` is a small continuation shape with clear responsibility: once
- the nested value expression finishes, invoke the active continuation bridge in
  outcome form.
- That is a better base for future nested resume/effect combinations than
  keeping a separate non-outcome bridge alive.

## Maintainability Check

- `Expr::Resume` now uses the same `eval_expr_ast_outcome(...)` /
  `complete_ast_value_outcome(...)` path as other migrated shapes.
- Removing the old non-outcome resume bridge helpers reduces one more parallel
  execution path.
- Full `cargo test -p goby-wasm` stayed green after the change.

## Security Check

- No new capability surface or external I/O was added.
- Remaining risk is migration completeness: non-direct callees and other mixed
  call/effect shapes still need the same treatment.

## Issues Found

- None beyond the expected scope limit; the slice intentionally reuses existing
  double-resume coverage instead of adding a large new matrix immediately.

## Fixes Applied

- Added `ResumeValue` as a unified continuation-frame shape.
- Routed `Expr::Resume` through the outcome consumer path.
- Removed obsolete non-outcome resume bridge helpers.
- Re-ran `cargo fmt` and `cargo test -p goby-wasm`.
