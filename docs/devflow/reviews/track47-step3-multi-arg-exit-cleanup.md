# Step Review

## Step

- Track 4.7 Step 3 multi-arg named-call exit cleanup.

## Contradiction Check

- Multi-arg direct named calls already evaluated their argument list through the
  outcome-aware path, but their final application step still used a separate
  direct wrapper.
- That left call-like migrated shapes with one more unnecessary split between
  expression-time and replay-time exit paths.
- This slice removes that split without changing runtime semantics.

## Extensibility Check

- No new continuation kind or new suspension rule was added.
- A shared outcome-aware helper for already evaluated argument slices makes
  future call-shape cleanup easier because both direct and replay paths can exit
  through the same boundary.

## Maintainability Check

- Multi-arg named-call flow is now easier to follow because there is one fewer
  special-case helper edge.
- Full `cargo test -p goby-wasm` stayed green.

## Security Check

- No external capability or I/O surface changed.
- Remaining risk is limited to architectural cleanup still in progress.

## Issues Found

- None beyond the intended cleanup target.

## Fixes Applied

- Added an outcome-aware helper for applying named calls from an evaluated
  argument slice.
- Switched both direct multi-arg named calls and `MultiArgNamedCall` replay to
  that helper.
- Re-ran `cargo fmt`, focused multi-arg replay coverage, and full
  `cargo test -p goby-wasm`.
