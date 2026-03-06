# Step Review

## Step

- Track 4.7 Step 3 `BinOp` continuation seam cleanup.

## Contradiction Check

- `BinOp` checkpoint capture had already moved to the outcome-aware path, but
  shared continuation replay still evaluated the right operand through legacy
  `eval_expr_ast(...)`.
- That left `BinOp` split across two models in a way the newer migrated shapes
  no longer were.
- This slice removes that contradiction without changing the continuation
  surface.

## Extensibility Check

- No new continuation kind was added.
- The cleanup reuses `eval_expr_ast_outcome(...)` plus
  `complete_ast_value_outcome(...)`, which keeps future nested shapes on the
  same suspension consumer boundary.

## Maintainability Check

- `BinOp` replay is now easier to reason about because both checkpoint capture
  and replay-time right-operand evaluation use the same outcome-aware contract.
- Full `cargo test -p goby-wasm` stayed green.

## Security Check

- No external capability or I/O surface changed.
- Residual risk remains architectural only: other leftover compatibility seams
  should continue to be collapsed one at a time.

## Issues Found

- None beyond the intended cleanup target.

## Fixes Applied

- Replaced the `BinOpLeft` shared replay branch's direct `eval_expr_ast(...)`
  call with `eval_expr_ast_outcome(...)`.
- Routed replay-time suspended outcomes through
  `complete_ast_value_outcome(...)`.
- Re-ran `cargo fmt`, focused `binop` replay coverage, and full
  `cargo test -p goby-wasm`.
