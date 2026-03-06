# Step Review

## Step

- Track 4.7 Step 3 single-arg continuation seam cleanup.

## Contradiction Check

- After several migrated shapes landed, the remaining single-arg shared replay
  seam still treated `Suspended(...)` as an internal error on the old path.
- That contradicted the newer model where suspended outcomes are normal control
  flow and should be consumed uniformly.
- This slice removes that contradiction without adding another frame shape.

## Extensibility Check

- No new continuation kind was added.
- The cleanup instead reuses the same outcome-consumer boundary that the newer
  shapes already use, which is the right direction for the remaining cleanup.

## Maintainability Check

- The single-arg shared replay seam is now less special-cased.
- This makes the runtime easier to reason about because `Suspended(...)` no
  longer flips from "normal" to "internal error" depending on which side path
  hit it.
- Full `cargo test -p goby-wasm` stayed green.

## Security Check

- No capability surface or external I/O changed.
- Residual risk remains architectural, not security-related: some compatibility
  seams still exist and should continue to be collapsed carefully.

## Issues Found

- None beyond the intended cleanup target.

## Fixes Applied

- Removed the single-arg legacy-only internal-error handling for suspended
  continuation replay.
- Reused `complete_ast_value_outcome(...)` on that seam instead.
- Re-ran `cargo fmt` and `cargo test -p goby-wasm`.
