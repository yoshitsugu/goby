# Goby Project State Snapshot

Last updated: 2026-04-30 (Perceus M10 complete — Track E closed)

## Current Focus

Perceus M10 is **complete**. M0–M10 are shipped; Track E (Perceus memory
management) is closed. Refcount + free-list + reuse is the steady-state
allocator model.

Remaining work is in other tracks (see `doc/PLAN.md` §4.1, §4.3–§4.7).

## Perceus M10 Summary

M10 closed two design issues that kept M9 partially red:

- **DI-1 (Option B):** `classify_decl_return_ownership` infers `Owned` vs
  `Borrowed` from body IR and seeds call-site ownership before `drop_insert`.
- **DI-2:** `insert_drop_at_tail` preserves tail-call shape for direct `Var`
  callee calls via pre-call `Dup/Drop`, keeps conservative temp-wrap for
  `GlobalRef`/intrinsics/indirect calls.
- **Conditional `list.map` ownership:** results are `Owned` only when the
  callback is proven owned-returning.
- **`ListGet` projection-borrow liveness:** parent list stays live while a
  projected child reference is live.

Acceptance:

- `perceus_real_world_driver_drops_intermediates_and_reuses_per_round` passes
  (`total_bytes=37016`, `reuse_hits=200`, `free_list_hits>0`).
- `alloc-baseline/real_world_driver.gb` passes at ceiling `150049`.
- `cargo test --workspace --release` is green.

## Known Red / Green State

Green after M10 closure:

- `cargo test --workspace --release` — pass.
- `cargo test --workspace --release alloc_baseline` — pass.
- M9/M10 acceptance tests:
  - `perceus_m9_simple_list_drop_increments_free_list_hits`
  - `perceus_m9_build_list_has_chunks_when_dropped`
  - `perceus_real_world_driver_drops_intermediates_and_reuses_per_round`
- `goby-core perceus` tests, including return-ownership and tail-drop tests.
- `goby-cli cli_integration::run_command_debug_alloc_stats_emits_stats_line_for_general_lowered_program`
- Graphemes / split / walk / rr3 regression set.
- `refcount_reuse_loop_owned_param_seed_reuses_assign_index`

## Next Step

Return to other active tracks (see `doc/PLAN.md` §4):

- Track D: `goby lint` (unused binding, shadowed effect op).
- Track OOB: list index out-of-bounds error messages.
- Track RR: runtime resource failure diagnostics.
- Track Float: `Float` / Wasm `f64` support.
- Review backlog: typecheck env clone strategy, call-graph closure, etc.
