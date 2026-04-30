# Goby Project State Snapshot

Last updated: 2026-04-30 (Perceus M10 reopen — regression net landed,
naive fix reverted as unsafe)

## Current Focus

Perceus M10 closure remains **reopened**. Re-closure work is **in
progress**:

- A regression net (5 tests) is in place to pin the boundary that any
  future fix must respect.
- An attempted Perceus-IR-layer fix was implemented and **reverted**:
  it produced a use-after-free in the existing test
  `recursive_multi_part_interpolated_print_after_graphemes_executes`
  (`debug` rendered as `debtg`). The fix did not correctly distinguish
  immutable `Let` parents from `LetMut` parents and would race with
  later `mut := ...` re-assignments.
- The 138×138 real-world driver still exhausts Wasm memory under
  both 256 MiB and the default 1 GiB ceiling.
- The remaining leak likely lives below the Perceus IR layer (see
  "Hypothesis" below).

Other tracks remain queued (see `doc/PLAN.md` §4.1, §4.3–§4.7).
Perceus M10 stays the highest priority until re-closed.

## What landed in this session

1. **Regression net (5 tests).** The pre-fix red set is documented
   and pinned to a stable harness so any future fix has both a
   positive guard and explicit safety boundaries.

   - `crates/goby-core/src/perceus.rs` (3 unit tests around the
     boundary the fix must respect):
     - `return_ownership_local_shadowed_global_ref_with_owned_local_is_owned`
       — record-field projection of an Owned local must classify
       Owned. **Currently `#[ignore]`d**: this is the forward-
       looking spec and can be enabled only once the Let / LetMut
       distinction lands (otherwise `mut`-cell projections become
       unsafe — see "Why the naive fix was reverted").
     - `return_ownership_local_shadowed_global_ref_with_borrowed_local_is_borrowed`
       — same shape over a Borrowed local must stay Borrowed (no
       spurious Drop on a Borrowed parent).
     - `return_ownership_global_ref_without_local_shadow_remains_borrowed`
       — true module-path GlobalRef (a function value) must keep
       the conservative Borrowed classification.

   - `crates/goby-wasm/src/compile_tests.rs` (2 runtime guards,
     both `#[ignore]` until the leak is fully eliminated):
     - `perceus_m10_graphemes_single_call_emits_free_list_hits` —
       a single `graphemes "abc"` call must drive `free_list_hits
       > 0` so a future regression of `graphemes`'s
       return-ownership classification is caught at the smallest
       possible scale.
     - `perceus_m10_list_map_graphemes_138_lines_runs_under_256mib`
       — the BUGS.md reduction (`lines = read_lines (); rows =
       list.map lines graphemes`) over a 138×138 stdin grid must
       run under a 256 MiB `WasmMemoryConfig`.

2. **Documentation update — `return_ownership_value::GlobalRef`.**
   The `GlobalRef` arm in `return_ownership_value`
   (`crates/goby-core/src/perceus.rs`) now carries an explicit
   comment recording the local-shadowing pitfall, the
   `recursive_multi_part_interpolated_print_after_graphemes_executes`
   regression that the naive promotion produces, and a forward
   pointer to this STATE entry. The conservative `Borrowed` default
   stays in place pending a future Let / LetMut distinction.

3. **Test harness improvements.** The 138-line repro test runs
   under an explicit `WasmMemoryConfig { max_pages: 4096, ..
   RUNTIME_MEMORY_CONFIG }` so it exercises the same 256 MiB
   ceiling as `goby run --max-memory-mb 256` without spawning the
   CLI. Both new runtime tests live in `compile_tests.rs` and
   follow the existing pattern of
   `execute_runtime_module_with_stdin_config_and_options_captured`
   + `parse_alloc_stats_field`.

## Why the naive fix was reverted

`crates/goby-core/src/resolved.rs:419` lowers `local.field`
record-field projections as `ValueExpr::GlobalRef { module:
<local_name>, name: <field_name> }`. The IR therefore conflates two
very different shapes:

- a **module-path** function-value reference, and
- a **local-shadowed** field projection.

A first attempt promoted the `GlobalRef` value-position arm to
`env.get(module)` so that a Let-bound local of class Owned would
propagate Owned to its field projection. That made
`stdlib::graphemes` (`final.parts`) classify Owned and fixed the
`graphemes` IR-level Drop placement. But the same code path is
exercised by the `mut`-cell shape inside `graphemes` itself — and
the existing test
`recursive_multi_part_interpolated_print_after_graphemes_executes`
caught it immediately: a per-grapheme `String` was freed by the
caller's Drop while the `mut` cell was still being re-assigned,
corrupting the next round's grapheme buffer (`debug` rendered as
`debtg`).

A correct fix must distinguish:

- Immutable `Let` parents, where Owned promotion of a field
  projection is sound (the parent is consumed at decl exit, the
  field is the parent's heap slot transferred to the caller).
- `LetMut` parents, where the parent cell may receive further
  `:=` re-assignments after the projection is read, so the field
  cannot be transferred without breaking subsequent re-assigns.

The runtime-layer leak audit (below) may also reveal that the IR-
level fix alone is insufficient even for Let parents, in which case
the M10 scope question becomes "is this a Perceus IR fix at all, or
is it a runtime-allocator unification fix?".

## Problem

Even with the naive IR fix in place (now reverted for safety), the
138-line repro still exhausted memory:

- `perceus_m10_graphemes_single_call_emits_free_list_hits` reported
  `total_bytes=0 peak_bytes=0 free_list_hits=0 reuse_hits=0` for a
  program that **does** allocate at runtime (it produces `3`,
  i.e. the grapheme list `["a", "b", "c"]` was constructed).
- `perceus_m10_list_map_graphemes_138_lines_runs_under_256mib`
  still tripped `E-MEMORY-EXHAUSTION` at 256 MiB and at the default
  1 GiB ceiling, despite the IR (when the naive fix was applied)
  showing `Drop(rows)` and `graphemes` classified Owned.
- A grid sweep (`n × n`, `n ∈ {10, 30, 50, 70, 80, 138}`) shows
  successful execution up to `n = 50` (`total_bytes ≤ 584` —
  unrealistically low for ~2500 grapheme cells) and abrupt 1 GiB
  exhaustion at `n ≥ 70`.

## Hypothesis

The runtime-layer counter `GLOBAL_ALLOC_BYTES_TOTAL` is incremented
only by `emit_alloc_from_top` (`crates/goby-wasm/src/gen_lower/emit.rs:5448`),
which is the Perceus-aware refcounted heap path. The fact that
`total_bytes=0` while a non-trivial grapheme list is in fact
materialised at runtime suggests that **a substantial fraction of
list / string / handler-state allocations bypass the Perceus
refcounted heap altogether** and live in a separate buffer (host
bump area, WASI/legacy allocator, or handler-continuation arena).

Likely culprits, in priority order:

1. `__goby_string_each_grapheme` and friends — host-side intrinsics
   that build per-grapheme strings outside the refcounted heap.
2. `__goby_list_push_string` — used inside the `graphemes` handler
   body to extend `step.parts`. The IR already emits the call as
   `Call { callee: Var("__goby_list_push_string"), .. }`, but if
   the runtime helper allocates from a separate heap, those chunks
   accumulate without ever reaching `__goby_drop`.
3. Effect-handler continuation / resume-frame allocation. Each
   `yield grapheme step -> resume (...)` builds a fresh
   `GraphemeState` record in the resume value. If those records
   are allocated on a continuation-local buffer that is never
   walked by `__goby_drop`, the per-grapheme leak is O(n) per call
   and O(n²) across `list.map lines graphemes`.

The asymmetric jump between `n = 50` (OK) and `n = 70` (1 GiB
exhaustion) is consistent with an O(n²) leak whose constant factor
is large enough to saturate the linear memory once per-grapheme
state crosses ~5000 elements.

## Verification plan for the hypothesis

The next investigation should:

1. Audit which intrinsics on the `graphemes` / `list.map` /
   handler-resume hot path call `emit_alloc_from_top` (and
   therefore are visible to `total_bytes`) versus those that do
   not.
2. For each intrinsic that does not, decide whether routing it
   through `emit_alloc_from_top` (so `__goby_drop` can free it) is
   safe and within M10 scope, or whether the fix belongs in a new
   milestone targeting host-intrinsic allocator unification.
3. If unification is out of M10 scope, surface that as a
   PLAN_PERCEUS update and re-scope M10 to "Perceus IR-level fix +
   regression net" (already landed) and a successor milestone for
   the runtime-layer leak.
4. Independently, design the Let / LetMut distinction in
   `return_ownership_value` (and matching boundary in
   `classify_call_result_ownership`) so the
   `return_ownership_local_shadowed_global_ref_with_owned_local_is_owned`
   guard can be flipped back on without breaking
   `recursive_multi_part_interpolated_print_after_graphemes_executes`.

## Known Red / Green State

Red after this session:

- `perceus_m10_graphemes_single_call_emits_free_list_hits` —
  `#[ignore]`d. Reproduces the runtime-layer leak at minimal
  scale.
- `perceus_m10_list_map_graphemes_138_lines_runs_under_256mib` —
  `#[ignore]`d. Reproduces the BUGS.md exhaustion under 256 MiB.
- `return_ownership_local_shadowed_global_ref_with_owned_local_is_owned`
  — `#[ignore]`d. Forward-looking spec for the Let-only case.
- Original BUGS.md 2026-04-30 entry: 138×138 driver via stdin
  under `--max-memory-mb 256`.

Green after this session:

- `cargo test --workspace --release` — pass (the four `#[ignore]`d
  tests do not count toward red).
- Full `perceus::tests` module — pass (16 active
  `return_ownership_*` tests; the 17th is intentionally
  `#[ignore]`d as forward-looking spec).
- All previously-green tests from the prior M10 closure remain
  green. Specifically the existing 20×20 acceptance test
  `perceus_real_world_driver_drops_intermediates_and_reuses_per_round`
  and the `alloc-baseline/real_world_driver.gb` (6×10 fixture)
  test still pass, and
  `recursive_multi_part_interpolated_print_after_graphemes_executes`
  is back to green after the naive-fix revert.

## Next Step

Stay on Perceus M10 until the runtime-layer leak is either fixed
inside M10 scope or explicitly deferred:

1. Run the runtime-allocator audit described in "Verification plan"
   above to localise which intrinsic / handler construct allocates
   outside the Perceus refcounted heap.
2. Independently design the Let / LetMut distinction so the
   `return_ownership_local_shadowed_global_ref_with_owned_local_is_owned`
   guard can be enabled.
3. Decide M10 scope: either land the runtime-layer fix here or
   open a successor milestone in `doc/PLAN_PERCEUS.md`. Update
   §4.100 acceptance accordingly.
4. Once 138×138 is green under 256 MiB, un-`#[ignore]` the two
   runtime guards added this session, and mirror the closure into
   `doc/BUGS.md` (Open → Resolved) and
   `memory/project_perceus_status.md`.

After M10 fully closes, return to other active tracks (`doc/PLAN.md`
§4):

- Track D: `goby lint` (unused binding, shadowed effect op).
- Track OOB: list index out-of-bounds error messages.
- Track RR: runtime resource failure diagnostics.
- Track Float: `Float` / Wasm `f64` support.
- Review backlog: typecheck env clone strategy, call-graph
  closure, etc.
