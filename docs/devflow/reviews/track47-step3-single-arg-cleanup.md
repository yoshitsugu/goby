# Step Review

## Step

- Track 4.7 Step 3.2c single-arg legacy replay cleanup.

## Contradiction Check

- After Step 3.2b, `single-arg named call` could suspend via the new outcome path, but the legacy
  `eval_expr_ast` path still captured the same shape with token-side replay.
- That kept the migrated shape partially dual-routed, which contradicted the plan to shrink old
  paths immediately after each migration.
- This slice removes that direct contradiction by stopping legacy plain named-call capture and
  reducing the remaining overlap to one narrow shared replay guard.

## Extensibility Check

- The migrated single-arg shape now depends on the new outcome path for checkpoint capture, which
  is the right base for migrating `BinOp` next.
- Remaining broader replay forms are untouched, so the cleanup does not blur shape boundaries.
- The small shared guard left in `execute_saved_value_continuation` is a contained compatibility
  seam rather than another source of checkpoint capture growth.

## Maintainability Check

- Removing the old `eval_expr_ast` capture points cuts one layer of duplicated single-arg replay
  behavior.
- Routing the remaining compatibility path back through `apply_named_value_call_ast_outcome`
  reduces logic drift between old and new execution paths.
- Full `cargo test -p goby-wasm` coverage stayed green after the cleanup, which is the main guard
  against accidental regression in existing Step 3 slices.

## Security Check

- No new I/O or external capability surface was added.
- The main residual risk is still architectural partiality: `BinOp` and other nested shapes remain
  on older replay mechanisms.
- Runtime error behavior for invalid continuation states stayed unchanged.

## Issues Found

- None beyond the expected migration overlap: the main task was deleting the right old capture
  points without disturbing statement-tail and binop replay.

## Fixes Applied

- Removed legacy `SingleArgNamedCall` checkpoint capture from `eval_expr_ast`.
- Kept the remaining shared replay path narrow and delegated it through the outcome-aware named-call
  application helper.
- Re-ran `cargo fmt` and `cargo test -p goby-wasm`.
