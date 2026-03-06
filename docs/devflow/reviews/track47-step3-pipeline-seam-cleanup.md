# Step Review

## Step

- Track 4.7 Step 3 pipeline continuation seam cleanup.

## Contradiction Check

- `PipelineCall` had a suspended-frame continuation shape already, but replay
  still reconstructed a string expression and re-entered `apply_pipeline(...)`.
- That contradicted the newer call-like shapes, which now replay through
  outcome-aware AST helpers directly.
- This slice removes that split without broadening the continuation model.

## Extensibility Check

- No new continuation kind was added.
- Reusing the named-call outcome helper keeps pipeline replay aligned with the
  same boundary used by other migrated call-like shapes.

## Maintainability Check

- Pipeline evaluation is now easier to trace because both expression-time and
  replay-time paths use the same outcome-aware helper.
- Full `cargo test -p goby-wasm` stayed green.

## Security Check

- No capability or I/O surface changed.
- Remaining risk is architectural only: a few legacy evaluation seams may still
  exist elsewhere and should continue to be removed incrementally.

## Issues Found

- None beyond the intended cleanup target.

## Fixes Applied

- Added an outcome-aware pipeline helper that delegates to the named-call AST
  path directly.
- Switched `Expr::Pipeline` and `PipelineCall` replay to that helper.
- Re-ran `cargo fmt`, focused pipeline replay coverage, and full
  `cargo test -p goby-wasm`.
