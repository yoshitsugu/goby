# Step Review

## Step

- Track 4.7 Step 3.2d binop legacy replay cleanup.

## Contradiction Check

- After the first `BinOp` suspended-frame slice, the migrated shape could suspend through the new
  outcome path, but legacy `eval_expr_ast` still captured operand replay checkpoints.
- That left `BinOp` in the same partially dual-routed state that `single-arg named call` had
  before Step 3.2c.
- This slice removes that contradiction by deleting the old `eval_expr_ast` checkpoint capture for
  `BinOp`.

## Extensibility Check

- `BinOp` now follows the same migration pattern as `single-arg named call`, which makes the next
  branch/control-flow migration more mechanically predictable.
- The remaining shared replay seam is narrow and temporary, so future cleanup can continue shape by
  shape instead of requiring another broad refactor.
- Existing parity tests for left/right operand replay still lock the externally visible behavior.

## Maintainability Check

- Checkpoint capture for the migrated `BinOp` shape now lives in one place:
  `eval_expr_ast_outcome`.
- Removing the old capture points lowers the chance that fallback and outcome-aware paths diverge as
  continuation behavior evolves.
- Full `cargo test -p goby-wasm` stayed green after the cleanup.

## Security Check

- No new I/O or external capability surface was introduced.
- Remaining risk is migration incompleteness, not safety: branch/control-flow shapes still use older
  replay mechanisms.
- Invalid continuation error behavior remains unchanged.

## Issues Found

- None beyond the expected migration overlap; the main task was removing old `BinOp` checkpoint
  capture without regressing already-landed statement-tail and single-arg behavior.

## Fixes Applied

- Removed legacy `BinOpLeft` / `BinOpRight` checkpoint capture from `eval_expr_ast`.
- Kept `BinOp` checkpoint capture on the outcome-aware path and verified parity/regression tests.
- Re-ran `cargo fmt` and `cargo test -p goby-wasm`.
