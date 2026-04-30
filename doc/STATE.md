# Goby Project State Snapshot

Last updated: 2026-04-30 (Perceus M10 in progress — Step 2 implemented)

## Current Focus

Perceus M10 is in progress. M0–M8 are shipped, M9 is partially landed
through the 3a + 3c stopgaps in `d7c4092`, and M10 is open to close
DI-1 / DI-2 plus the remaining M9 acceptance items.

Primary plan reference: `doc/PLAN_PERCEUS.md` §4.99 / §4.100.

## Current Decision

Step 1a/1b/1c landed in `9a9f502`. Step 2 is implemented in the
working tree and should be committed next.

The selected design changed during implementation: `__goby_list_map`
is not seeded as unconditionally `Owned`. The outer list is fresh, but
dropping it also drops its elements, so the result is classified
`Owned` only when the visible callback argument is known to return
owned values.

Implemented direction:

- Keep Step 1a/1b/1c in one commit.
- Classify `list.map` / `__goby_list_map` call results as `Owned`
  only when the callback return is proven owned, such as
  `fn n -> "${n + 1}"`.
- Model `ListGet(parent, index)` as creating a projection borrow: the
  parent list remains live while any projected child reference derived
  from it remains live.
- Do not weaken the cli integration allocation-stat assertion.
- Resolve DI-2 by splitting tail drop placement into C1/C2/C3:
  direct `Var` callee calls that mention the dropped name only in args
  keep the tail call shape via `Dup(name); Drop(name); Call(...)`;
  `GlobalRef` calls, runtime intrinsics, and calls that mention the name
  as callee keep the conservative temp wrap.

Deferred alternative: treating `ListGet` as an ownership transfer with
a matching `Dup` on the source. That may be useful later, but it is
more invasive than projection-borrow liveness and is not required for
M10 closure.

## Current Working Tree

Files currently touched by M10 Step 2 work:

- `crates/goby-core/src/perceus.rs`
  - updated `insert_drop_at_tail` to preserve C2 direct tail calls with
    pre-call `Dup`/`Drop`.
  - left runtime intrinsics and C3 closure/indirect call handling in the
    temp-wrap form and documented why.
  - added focused C2/C3/intrinsic unit tests for the tail-drop shape.
- `doc/PLAN_PERCEUS.md`
  - records Step 2 completion and checks the DI-2 acceptance item.
- `doc/STATE.md`
  - records the implemented Step 2 policy and next step.

## Known Red / Green State

Green after Step 2:

- M9 acceptance tests:
  - `perceus_m9_simple_list_drop_increments_free_list_hits`
  - `perceus_m9_build_list_has_chunks_when_dropped`
  - `perceus_real_world_driver_drops_intermediates_and_reuses_per_round`
- `goby-core perceus` tests, including the return-ownership tests.
- `goby-cli cli_integration::run_command_debug_alloc_stats_emits_stats_line_for_general_lowered_program`
- Graphemes / split / walk / rr3 regression set.
- `cargo test --workspace --release alloc_baseline`.
- `cargo test -p goby-core tail_drop_for_name`.
- Step 2 wasm checks:
  - `compile_module_scan_loop_lowering_eliminates_walk_self_call_in_wasm`
  - `perceus_real_world_driver_drops_intermediates_and_reuses_per_round`
  - `refcount_reuse_loop_owned_param_seed_reuses_assign_index`

The critical projection-borrow shape is:

```gb
rolls = list.map lines graphemes
row2  = list.get rolls 2
list.each row2 println
```

`row2` is a projected child reference into `rolls`; dropping `rolls`
immediately after `list.get rolls 2` can free memory that `row2` still
uses. The implemented rule delays the parent drop when a projected
child remains live.

## Remaining M10 Work

- [x] Step 1c: implement conditional `list.map` ownership plus
      projection-borrow liveness for `ListGet`.
- [x] Step 2: fix DI-2 with tail-call-safe drop placement in
      `insert_drop_at_tail` (C1 / C2 / C3 split in
      `doc/PLAN_PERCEUS.md` §4.100).
- [ ] Step 3: add
      `crates/goby-wasm/tests/fixtures/alloc-baseline/real_world_driver.gb`
      and record its ceiling in `crates/goby-wasm/tests/alloc_baseline.txt`.
- [ ] Step 4: move the BUGS.md M9 entry to resolved, update
      `doc/PLAN.md` §4.2, refresh this state file, and run the final
      quality gate.

Optional evidence, not a closure gate: AoC2025 day 4 part 2 under
`--max-memory-mb 256`.

## Next Step

Commit Step 2, then start Step 3:
`crates/goby-wasm/tests/fixtures/alloc-baseline/real_world_driver.gb`
plus its `alloc_baseline.txt` ceiling.

Gate evidence:

- `cargo fmt --all --check` — pass.
- `cargo check` — pass.
- `cargo test` — pass.
- `cargo test -p goby-core tail_drop_for` — pass.
- `cargo test -p goby-core borrowed_callee_arg_is_dropped_by_owner_after_call` — pass.
- `cargo test --release -p goby-wasm compile_module_scan_loop_lowering_eliminates_walk_self_call_in_wasm` — pass.
- `cargo test --release -p goby-wasm perceus_m9_real_world_driver_drops_intermediates_and_reuses_per_round` — pass.
- `cargo test --release -p goby-wasm refcount_reuse_loop_owned_param_seed_reuses_assign_index` — pass.
