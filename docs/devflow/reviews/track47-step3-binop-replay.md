# Step Review

## Step

- Track 4.7 Step 3 direct `BinOp` operand replay slice.

## Contradiction Check

- After the single-arg call slice, nested value-position replay still contradicted the intended
  contract for arithmetic/equality expressions: `next 0 + 4` and `4 + next 0` resumed the operand
  value but did not replay the interrupted binary expression itself.
- This slice removes that contradiction for direct `BinOp` operand shapes by carrying dedicated
  left/right binop value continuations.
- The scope remains intentionally partial: broader branch and general expression-tree checkpoints
  are still open.

## Extensibility Check

- `AstValueContinuationKind::{BinOpLeft, BinOpRight}` extends the same continuation mechanism used
  by the single-arg call slice rather than inventing a second nested replay path.
- Separating left and right replay keeps the continuation payload minimal and makes later extension
  to other asymmetric expression forms more straightforward.
- The shared `apply_binop_runtime_value` helper provides a single place to preserve existing
  arithmetic/equality semantics across normal evaluation and replay.

## Maintainability Check

- The implementation is narrow and reuses the existing value-continuation stack, avoiding another
  independent checkpoint subsystem.
- Both left and right operand regressions are covered, plus typed/fallback parity for the combined
  shape.
- Existing Step 3 slices remained green under the full `cargo test -p goby-wasm` run.

## Security Check

- No new I/O or capability surface was introduced.
- The main risk remains semantic incompleteness, not safety.
- Existing invalid `resume` behavior stays on the same deterministic error paths.

## Issues Found

- `BinOpKind` is not `Copy`, so the initial continuation payload construction attempted to move out
  of a shared reference and failed to compile.
- Without dedicated left/right payloads, a resumed operand had no precise way to continue through
  the interrupted binary expression.

## Fixes Applied

- Added `BinOpLeft` and `BinOpRight` value continuation kinds.
- Added `apply_binop_runtime_value` so replay and normal execution share the same operator logic.
- Cloned `BinOpKind` where needed to satisfy ownership rules.
- Added fallback and typed parity tests for left/right operand replay.
- Re-ran `cargo test -p goby-wasm` after the compile fix.
