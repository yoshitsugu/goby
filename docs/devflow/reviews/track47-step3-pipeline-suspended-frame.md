# Step Review

## Step

- Track 4.7 Step 3.2d pipeline-value suspended-frame slice.

## Contradiction Check

- After named calls, receiver-method calls, and nested `resume (...)`,
  `Expr::Pipeline` still used the old direct evaluator path for value-position
  progression.
- That left mixed call/effect chains as a visible gap in the unified
  continuation model.
- This slice resolves that gap narrowly for `value |> callee`.

## Extensibility Check

- `PipelineCall` is a compact frame shape: it carries only the target callee
  name and reuses the resumed left-hand value.
- The slice keeps pipeline migration intentionally small and does not attempt to
  solve every side-effecting pipeline form at once.
- That fits the compact-turn plan and still extends the model into a new
  expression family.

## Maintainability Check

- `Expr::Pipeline` now uses the same outcome/consumer boundary as the other
  migrated value-position shapes.
- The resumed path reuses existing `apply_pipeline(...)` semantics instead of
  creating another bespoke dispatch layer.
- Full `cargo test -p goby-wasm` stayed green.

## Security Check

- No new capability surface or external I/O was added.
- Remaining risk is still migration completeness: the remaining work is about
  shrinking legacy replay seams, not new safety exposure.

## Issues Found

- None beyond expected scope limits; the slice intentionally targets only the
  value-position pipeline family.

## Fixes Applied

- Added `PipelineCall` as a unified continuation-frame shape.
- Routed AST `Expr::Pipeline` through the outcome consumer path.
- Added fallback/typed parity coverage for pipeline value replay.
- Re-ran `cargo fmt` and `cargo test -p goby-wasm`.
