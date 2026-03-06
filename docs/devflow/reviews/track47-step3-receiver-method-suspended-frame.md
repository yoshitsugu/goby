# Step Review

## Step

- Track 4.7 Step 3.2d one-arg receiver/method-call suspended-frame slice.

## Contradiction Check

- After migrating named calls, `resume (...)`, `if`, and `case`, non-direct
  callees still lagged behind on the legacy direct-eval path.
- That would have left `receiver.method value` style calls as an inconsistent
  hole in the unified continuation model.
- This slice resolves that narrowly for the smallest useful non-direct shape:
  one receiver/method call with one argument.

## Extensibility Check

- `ReceiverMethodCall` is still a compact frame shape: it captures only the
  receiver/module name and member name while reusing the resumed argument value.
- The new helper centralizes the outcome-aware dispatch logic for that shape,
  which is a better base for later pipeline or mixed chain migration.
- The slice does not try to solve all method-call forms at once.

## Maintainability Check

- Declaration value evaluation now completes via
  `complete_ast_value_outcome(...)`, which reduces the risk of newly migrated
  nested shapes silently falling back to the old value evaluator.
- The receiver-method resumed path reuses the same outcome-aware dispatch helper
  as the initial call site.
- Full `cargo test -p goby-wasm` stayed green.

## Security Check

- No new capability surface or external I/O was added.
- Remaining risk is still architectural incompleteness: pipelines and other
- mixed call/effect chains remain outside this slice.

## Issues Found

- The initial qualified-call test exposed that the parser represents this source
  shape as `MethodCall`, not `Call(Qualified, ...)`.
- The fix was to migrate the actual AST shape instead of forcing the wrong one.

## Fixes Applied

- Added `ReceiverMethodCall` as a unified continuation-frame shape.
- Added outcome-aware receiver/method dispatch helper reuse.
- Routed declaration value completion through `complete_ast_value_outcome(...)`.
- Added fallback/typed parity coverage for receiver/method-call argument replay.
- Re-ran `cargo fmt` and `cargo test -p goby-wasm`.
