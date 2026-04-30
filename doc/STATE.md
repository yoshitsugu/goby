# Goby Project State Snapshot

Last updated: 2026-04-30 (Perceus M10 in progress — Step 1c implemented)

## Current Focus

Perceus M10 is in progress. M0–M8 are shipped, M9 is partially landed
through the 3a + 3c stopgaps in `d7c4092`, and M10 is open to close
DI-1 / DI-2 plus the remaining M9 acceptance items.

Primary plan reference: `doc/PLAN_PERCEUS.md` §4.99 / §4.100.

## Current Decision

Step 1c has landed in the working tree. Step 1a/1b/1c should be
committed together; the final formatting and workspace gates are green.

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

Deferred alternative: treating `ListGet` as an ownership transfer with
a matching `Dup` on the source. That may be useful later, but it is
more invasive than projection-borrow liveness and is not required for
M10 closure.

## Current Working Tree

Files currently touched by M10 Step 1 work:

- `crates/goby-core/src/perceus.rs`
  - added `classify_decl_return_ownership`,
    `return_ownership_comp`, `return_ownership_value`,
    `callee_decl_return_class`, conditional map-result ownership, and
    projection-borrow-aware tail-drop placement.
  - keeps `HEAP_RETURNING_INTRINSICS` for unconditional seeds only;
    `__goby_list_map` is handled at the call site instead.
  - changed `ownership_classify_module` to return
    `(param_ownership, decl_returns)`.
  - threaded `decl_returns` through `classify_comp`,
    `classify_bound_comp`, and `ownership_classify_decl`.
  - changed `classify_owned_result` to use decl-return ownership
    instead of `IrType`.
  - deleted `type_is_known_heap`.
  - added return-ownership unit tests.
- `crates/goby-wasm/src/gen_lower/emit.rs`
  - exports aux decls when `expose_perceus_test_exports` is set, so
    Perceus acceptance tests can call helper declarations directly.
- `doc/PLAN_PERCEUS.md`
  - records the GFP return-ownership fixpoint, conditional
    `list.map` ownership, and Step 1c projection-borrow direction.
- `doc/STATE.md`
  - records the implemented Step 1c policy.

## Known Red / Green State

Green after Step 1c:

- M9 acceptance tests:
  - `perceus_m9_simple_list_drop_increments_free_list_hits`
  - `perceus_m9_build_list_has_chunks_when_dropped`
  - `perceus_m9_real_world_driver_drops_intermediates_and_reuses_per_round`
- `goby-core perceus` tests, including the return-ownership tests.
- `goby-cli cli_integration::run_command_debug_alloc_stats_emits_stats_line_for_general_lowered_program`
- Graphemes / split / walk / rr3 regression set.
- `cargo test --workspace --release alloc_baseline`.

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
- [ ] Step 2: fix DI-2 with tail-call-safe drop placement in
      `insert_drop_at_tail` (C1 / C2 / C3 split in
      `doc/PLAN_PERCEUS.md` §4.100).
- [ ] Step 3: add
      `crates/goby-wasm/tests/fixtures/alloc-baseline/m9_real_world_driver.gb`
      and record its ceiling in `crates/goby-wasm/tests/alloc_baseline.txt`.
- [ ] Step 4: move the BUGS.md M9 entry to resolved, update
      `doc/PLAN.md` §4.2, refresh this state file, and run the final
      quality gate.

Optional evidence, not a closure gate: AoC2025 day 4 part 2 under
`--max-memory-mb 256`.

## Next Step

Commit Step 1a/1b/1c together.

Gate evidence:

- `cargo fmt --all --check` — pass.
- `cargo check` — pass.
- `cargo test --workspace --release` — pass.
