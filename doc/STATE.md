# Goby Project State Snapshot

Last updated: 2026-05-01 (effect handler-use classifier started)

## Current Focus

The effect-runtime roadmap has been revised in docs:

- `doc/PLAN.md` now records the lexical-capability / stable-prompt /
  continuation-object direction.
- `doc/PLAN_IR.md` now treats WB-4 as the general effect-runtime track and
  moves WasmFX to an optional future emitter role.
- Ordinary `mut` semantics are preserved; multi-shot continuations that capture
  ordinary mutable locals should be rejected until an explicit branch-local
  state surface exists.
- The first implementation-prep slice has landed in
  `crates/goby-wasm/src/effect_handler_legality.rs`: handler-use records now
  distinguish abortive, direct one-shot, delayed one-shot, sequential
  multi-shot, reentrant-looking, and multi-shot-with-mutable-capture shapes.
  The existing one-shot optimized-path gate remains compatible with the old
  `has_unsupported` / `all_one_shot_tail_resumptive` queries.

Perceus M10's reopened IR boundary and the M11 138x138 memory
exhaustion are fixed. The temporary Perceus execution plan is closed
and removed; durable state now lives in `doc/PLAN.md` §4.2,
`doc/BUGS.md`, and the regression tests.

- `return_ownership_value` now distinguishes immutable `Let` parents
  from `LetMut` parents for local-shadowed `GlobalRef` field
  projections.
- `return_ownership_local_shadowed_global_ref_with_owned_local_is_owned`
  is active and green.
- A new `LetMut` regression keeps owned mutable parents conservative,
  preventing the reverted use-after-free shape
  (`recursive_multi_part_interpolated_print_after_graphemes_executes`,
  where `debug` previously rendered as `debtg`).
- Host imports that return escaping Goby values now allocate in the
  Perceus refcounted heap instead of the monotonic host bump arena.
- The active 138×138 `read_lines () -> list.map graphemes`
  reduction now runs under the 256 MiB test ceiling.
- The full BUGS.md real-world-driver shape now has an active
  138×138 stdin regression and runs under the same 256 MiB ceiling.
- Fold-prepend lowering now handles non-empty accumulators and
  case-recursive reverse-prepend helpers, avoiding repeated
  list-spread copies in `flatten`.

Other tracks remain queued (see `doc/PLAN.md` §4.1, §4.3–§4.7).

## What landed in this session

1. **Regression net (5 tests).** The pre-fix red set is documented
   and pinned to a stable harness so any future fix has both a
   positive guard and explicit safety boundaries.

   - `crates/goby-core/src/perceus.rs` (3 unit tests around the
     boundary the fix must respect):
     - `return_ownership_local_shadowed_global_ref_with_owned_local_is_owned`
       — record-field projection of an Owned local must classify
       Owned. Active and green after the Let / LetMut distinction.
     - `return_ownership_local_shadowed_global_ref_with_borrowed_local_is_borrowed`
       — same shape over a Borrowed local must stay Borrowed (no
       spurious Drop on a Borrowed parent).
     - `return_ownership_global_ref_without_local_shadow_remains_borrowed`
       — true module-path GlobalRef (a function value) must keep
       the conservative Borrowed classification.
     - `return_ownership_local_shadowed_global_ref_with_owned_let_mut_is_borrowed`
       — same lowering over an Owned `LetMut` parent must stay
       Borrowed so later `mut := ...` re-assignments cannot observe
       a freed field.

   - `crates/goby-wasm/src/compile_tests.rs` (2 active M11 runtime
     guards):
     - `perceus_m11_graphemes_single_call_reports_host_alloc_stats`
       — a single `graphemes "abc"` call must report non-zero
       `total_bytes`, proving host-created values are visible to
       alloc stats instead of disappearing into the host bump arena.
     - `perceus_m11_list_map_graphemes_138_lines_runs_under_256mib`
       — the BUGS.md reduction (`lines = read_lines (); rows =
       list.map lines graphemes`) over a 138×138 stdin grid must
       run under a 256 MiB `WasmMemoryConfig`.

2. **Documentation update — `return_ownership_value::GlobalRef`.**
   The `GlobalRef` arm in `return_ownership_value`
   (`crates/goby-core/src/perceus.rs`) now carries the active
   local-shadowing rule: immutable owned locals promote to `Owned`,
   `LetMut` and true module-path references stay `Borrowed`.

3. **Test harness improvements.** The 138-line repro test runs
   under an explicit `WasmMemoryConfig { max_pages: 4096, ..
   RUNTIME_MEMORY_CONFIG }` so it exercises the same 256 MiB
   ceiling as `goby run --max-memory-mb 256` without spawning the
   CLI. Both new runtime tests live in `compile_tests.rs` and
   follow the existing pattern of
   `execute_runtime_module_with_stdin_config_and_options_captured`
   + `parse_alloc_stats_field`.

4. **M11 host allocator unification.** Host-created escaping strings,
   list headers, and list chunks are allocated via a Rust-side mirror
   of `emit_alloc_from_top`: refcount word initialized to 1, global
   heap cursor updated, and `GLOBAL_ALLOC_BYTES_TOTAL` incremented.
   Generated Wasm now reloads the global heap cursor/floor after host
   imports so later Wasm allocations cannot overwrite host-created
   objects.

5. **M11 full-driver lowering closure.** The `flatten` shape used by
   the BUGS.md driver now avoids O(n^4) repeated list-spread copies:
   inline `fold row acc (fn a c -> [c, ..a])` lowers to one
   reverse-fold prefix plus a single concat with `acc`, and
   case-recursive reverse-prepend helpers lower the same way.

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

## Root cause (localized 2026-04-30)

The leak is a **runtime-layer leak of the host bump arena**, not a
Perceus IR-level Drop placement bug. The Wasm linear memory has two
disjoint allocators that share the page budget, and only one of them
is freed:

| Allocator | Site | Freed by | Counted in `total_bytes` |
|-----------|------|----------|--------------------------|
| Perceus refcounted heap | `emit_alloc_from_top` (`crates/goby-wasm/src/gen_lower/emit.rs:5319`) | `__goby_drop` (refcount→0 → free list) | yes (`emit.rs:5448`) |
| Host bump arena | `alloc_from_host_bump` (`crates/goby-wasm/src/wasm_exec.rs:867`) | **never** (cursor is monotonic; only `memory.grow` extends it) | **no** |

Every `goby:runtime/track-e` host import allocates from the host bump
arena and never increments `GLOBAL_ALLOC_BYTES_TOTAL`. There is no
`free` path: `alloc_from_host_bump` only does
`bump.compare_exchange_weak` forward, and on capacity pressure calls
`ensure_linear_memory_capacity` → `memory.grow`. That is exactly how
`E-MEMORY-EXHAUSTION` is reached at the 1 GiB / 256 MiB ceiling without
the Perceus stats showing any pressure.

The seven host imports that route through this arena are registered
in `crates/goby-wasm/src/wasm_exec.rs:215-330` and listed in
`crates/goby-wasm/src/host_runtime.rs:75-83`:

| Host import | Implementation | Allocates per call |
|-------------|----------------|--------------------|
| `__goby_value_to_string` | `value_to_string_host` (`wasm_exec.rs:354`) | one tagged String header (`encode_string_in_host_bump`) |
| `__goby_string_each_grapheme_count` | `grapheme_count_host` | none (returns count) |
| `__goby_string_each_grapheme_state` | `grapheme_state_host_inner` (`wasm_exec.rs:397`) | one fresh `(len, bytes)` String header **only when `span.start ∈ 1..=3`** (offsets that have no safe in-place 4-byte slot); `span.start == 0` reuses the source header, `span.start ≥ 4` mutates the source bytes in place |
| `__goby_string_concat` | `string_concat_host` | one `(len, bytes)` String per call |
| `__goby_list_join_string` | `list_join_string_host` | one `(len, bytes)` String per call |
| `__goby_string_graphemes_list` | `graphemes_list_host` (`wasm_exec.rs:1016`) | full chunked list header + N chunks + N grapheme strings — **O(N) bytes per call, never freed** |
| `__goby_string_split_lines` | `split_lines_host` (`wasm_exec.rs:1036`) | full chunked list header + chunks + N line strings — **O(input size) bytes per call, never freed** |

For the 138×138 reduction
(`perceus_m10_list_map_graphemes_138_lines_runs_under_256mib`), the
hot path is:

1. `lines = read_lines ()` calls `__goby_string_split_lines`. The
   entire `List String` (header + chunks + 138 line strings ≈
   `138 × (4 + 138)` bytes for the line bodies plus list metadata)
   is allocated **once** in the host bump arena. It is not freed at
   end-of-function. (`split_lines_host` allocates via
   `alloc_list_string_host` → `alloc_from_host_bump`.) This is a
   one-shot O(n²) cost in line-count × line-width.
2. `rows = list.map lines graphemes` then calls the Goby-level
   `graphemes` for each line. The stdlib `graphemes`
   (`stdlib/goby/string.gb:127`) does **not** call
   `__goby_string_graphemes_list`; it iterates with
   `__goby_string_each_grapheme value initial` (the 2-arg form,
   which is `StringEachGraphemeState` — a host import) and pushes
   each grapheme into a Perceus-managed `List String` via
   `__goby_list_push_string` (in-Wasm, `emit_list_push_string_helper`,
   `emit.rs:7698`).
3. For each grapheme yielded by the host, the host **may** allocate
   a fresh String in the bump arena (only the `span.start ∈ 1..=3`
   case). For ASCII input `"."` ×138 this is a small per-line cost,
   but for the full real-world driver shape (`splitlines (read())`
   of a typical text file with multi-byte graphemes) the per-call
   cost becomes O(line_length).
4. Even if step 3 is small, step 1 alone is large enough that the
   *combined* live host-bump occupancy across a session of
   N = 138 lines × repeated runs / closures is what tips the 256 MiB
   ceiling — and crucially, **the bump cursor never retreats even
   when `lines` and every `rows[i]` go out of scope.**

This explains the observations in `## Problem` directly:

- `total_bytes = 0` for `perceus_m10_graphemes_single_call_emits_free_list_hits`:
  the regression test calls `read()` (consumes empty stdin → host bump
  alloc only for the `Read` effect plumbing) and `graphemes "abc"`,
  whose body uses `__goby_string_each_grapheme` 2-arg = host import,
  not the Perceus heap. The grapheme list is built in-Wasm via
  `__goby_list_push_string`, which does increment `total_bytes`. So
  observing `total_bytes = 0` means execution **aborted before**
  `emit_alloc_stats_line` ran — most likely a `memory.grow` failure
  inside the host bump path tripped `RUNTIME_ERROR_MEMORY_EXHAUSTION`
  and the `_start` epilogue never reached the alloc-stats line.
  (`emit_alloc_stats_line` writes the line *before* the WASI exit, but
  `set_runtime_error_once` + `emit_abort` short-circuits the epilogue.)
- `n = 50` succeeds, `n = 70` fails at 1 GiB: the host bump arena
  starts at `host_bump_start = initial_linear_memory_bytes -
  host_bump_reserved_bytes`. Once it crosses
  `host_bump_reserved_bytes = 49152`, every subsequent host import
  triggers `memory.grow`. Because `__goby_string_split_lines` alone
  allocates `O(n × line_width) + list_meta` per call and never frees,
  the cliff between 50 and 70 is just where cumulative host bump
  consumption × the constant factor saturates the 1 GiB ceiling.
- The Perceus IR-level fix to `return_ownership_value::GlobalRef`
  (the naive promotion) is **orthogonal**: it can only affect what
  Perceus's `__goby_drop` walks, and `__goby_drop` does not touch
  host bump memory. So no IR-level Drop placement change can release
  host-bump-resident `lines`/`rows[i]` strings.

## Implication for Perceus scope

The remaining 138×138 memory exhaustion is **not** a Perceus IR-layer
fix. Two independent fixes were identified:

1. **Runtime-allocator unification** — either (a) route the seven host
   imports through the Perceus refcounted heap so `__goby_drop` can
   walk them, or (b) give the host bump arena a free path
   (per-call-frame reset, or refcounted host strings/lists). Option
   (a) requires the host imports to write into refcount-prefixed
   slots that `__goby_drop` understands; option (b) requires deciding
   the lifetime story for host-bump-resident pointers when they
   escape into Goby values.
2. **Let / LetMut distinction in `return_ownership_value`** — landed
   independently to flip
   `return_ownership_local_shadowed_global_ref_with_owned_local_is_owned`
   on without breaking
   `recursive_multi_part_interpolated_print_after_graphemes_executes`.
   This was unblocked from (1) and is the originally-scoped M10 work.

The former Perceus M10 scope closes on (2) plus the regression net; (1)
landed as M11 host-intrinsic allocator unification.

## Verification status

The audit listed in the previous session's "Verification plan" is
complete; results are summarized in `## Root cause` above. Concretely:

- All seven `goby:runtime/track-e` host imports were traced to
  `alloc_from_host_bump` and confirmed to **not** call
  `emit_alloc_from_top` and **not** to update
  `GLOBAL_ALLOC_BYTES_TOTAL`.
- `__goby_list_push_string` (the in-Wasm path used by `graphemes`'s
  handler body) was confirmed to use `emit_alloc_from_top` and is
  therefore visible to Perceus drop / free-list reuse.
- `RecordLit` (used to build `GraphemeState` in the resume value) was
  confirmed to use `emit_alloc_from_top`. Effect-handler
  resume-frame allocation is therefore **not** an additional
  off-heap source — Hypothesis 3 from the prior session is
  retracted.

Open verification work, in priority order:

1. Quantify host-bump consumption per call to `split_lines_host` and
   `graphemes_list_host` for the 138×138 reduction (instrument
   `alloc_from_host_bump` to log cumulative bytes; confirm the cliff
   between `n = 50` and `n = 70` matches the predicted O(n²) shape).
2. Confirm `total_bytes = 0` is "aborted before alloc-stats line",
   not "Perceus heap genuinely unused" — capture stderr including
   the abort prefix, or add a `#[ignore]`d test that prints a
   pre-exit marker before the alloc-stats line.

## Known Red / Green State

Red after this session:

- None for the M10/M11 Perceus closure.

Green after this session:

- `cargo test --release -p goby-core perceus` — pass; all
  `return_ownership_*` tests are active, including the Let-only
  owned projection and the LetMut conservative regression.
- Focused `goby-wasm` regression tests pass:
  `recursive_multi_part_interpolated_print_after_graphemes_executes`,
  `rr3`, `wb3_m7`, and
  `compile_module_scan_loop_lowering_eliminates_walk_self_call_in_wasm`.
- M11 runtime reductions pass:
  `perceus_m11_graphemes_single_call_reports_host_alloc_stats`,
  `perceus_m11_list_map_graphemes_138_lines_runs_under_256mib`,
  `perceus_m11_real_world_driver_138_grid_runs_under_256mib`, and
  `host_string_concat`.
- All previously-green tests from the prior M10 closure remain
  green. Specifically the existing 20×20 acceptance test
  `perceus_real_world_driver_drops_intermediates_and_reuses_per_round`
  and the `alloc-baseline/real_world_driver.gb` (6×10 fixture)
  test still pass, and
  `recursive_multi_part_interpolated_print_after_graphemes_executes`
  is back to green after the naive-fix revert.

## Next Step

Continue effect runtime preparation from the classifier into lexical target
metadata. The next useful slice is to attach stable handler-operation identity
to handler installation / effect operation use sites, so later lowering
consumes capability metadata instead of raw operation-name matching.

Other active tracks remain available (`doc/PLAN.md` §4):

- Track D: `goby lint` (unused binding, shadowed effect op).
- Track OOB: list index out-of-bounds error messages.
- Track RR: runtime resource failure diagnostics.
- Track Float: `Float` / Wasm `f64` support.
- Review backlog: typecheck env clone strategy, call-graph
  closure, etc.
