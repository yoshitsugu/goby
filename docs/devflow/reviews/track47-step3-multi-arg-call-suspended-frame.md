# Step Review

## Step

- Track 4.7 Step 3.2d multi-arg direct named-call suspended-frame slice.

## Contradiction Check

- After `if` and `case`, control-flow used the unified suspended-frame consumer,
  but broader call shapes still relied on the older direct `flatten_named_call`
  evaluation path.
- That would have left nested call-chain progression lagging behind the new
  continuation model.
- This slice resolves that mismatch narrowly by migrating only multi-arg direct
  named-call argument replay.

## Extensibility Check

- `MultiArgNamedCall` captures only what the next evaluator step needs:
  already-evaluated args, remaining arg expressions, and the callee name.
- The shape is still direct and bounded; it does not try to solve arbitrary
  callee expressions or `resume (op ...)` in the same turn.
- That keeps the slice aligned with the compact-turn plan while extending the
  same consumer boundary to a meaningful next call form.

## Maintainability Check

- `eval_named_call_args_outcome(...)` centralizes incremental arg-list
  evaluation instead of re-implementing the same replay logic at each call
  site.
- The resumed path falls back into `apply_named_value_call_ast(...)`, so call
  semantics stay in one place while only the checkpoint transport changes.
- Full `cargo test -p goby-wasm` stayed green after the slice.

## Security Check

- No new external capability or I/O surface was introduced.
- Remaining risk is still architectural incompleteness: non-direct callees,
  mixed call/effect chains, and `resume (op ...)` remain outside this slice.

## Issues Found

- The first draft only failed due to incorrect expected test output; runtime
  behavior itself matched the implemented semantics.

## Fixes Applied

- Added `MultiArgNamedCall` as a unified continuation-frame shape.
- Added `eval_named_call_args_outcome(...)` for incremental multi-arg evaluation.
- Added fallback/typed parity coverage for multi-arg named-call replay.
- Re-ran `cargo fmt` and `cargo test -p goby-wasm`.
