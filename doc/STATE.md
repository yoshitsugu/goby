# Goby Project State Snapshot

Last updated: 2026-04-08

## Current Focus

Next track: **Track RR** (`PLAN.md §4.8c`) - runtime resource failure diagnostics and resilience.

Locked ideal goal for RR:

- make "write the obvious recursive/list-building program first" a rational
  default for Goby users;
- push resilience into shared lowering/runtime boundaries instead of accumulating
  source-shape-specific exceptions;
- keep the remaining limits explicit, attributable, and honestly diagnosed.

RR execution reminder:

- at the start of each RR implementation step, review the locked ideal goal
  above and reject any step that cannot be justified as progress toward that
  shared design.

Immediate next steps:

- **RR-2**: decompose the current "solve2-style" failure into representative
  owned buckets before attempting another runtime/lowering fix.
  - preserve separate repros for:
    - self tail recursion,
    - non-tail recursive scanning,
    - recursive list spread / concat growth,
    - callback-assisted recursion (`fold` in the hot path).
- **RR-3**: target recursion resilience only at the boundary that RR-2 shows is
  actually dominant.
  - current evidence says self tail recursion alone is not enough; the hotter
    path is `fold -> should_prune_cell/check_around_rolls ->
    collect_prune_positions/count_valid_roll`.
- **RR-4**: improve list-spread resilience for recursive list builders such as
  `[x, ..rest]` after ownership is clearer.
- keep RR-1 diagnostics best-effort and explicit about uncertainty
  (`likely stack pressure`, `memory exhaustion`, `unknown runtime trap`) as
  later resilience work lands.

## Recently Completed

- **Track RR, RR-2 exploration note** (recorded, 2026-04-08; reverted, no code change kept).
  - tried a narrow lowering/emission experiment for aux-decl self tail recursion
    only.
  - the experiment was reverted because it broke existing Wasm validation / runtime
    behavior for recursive list-pruning programs such as
    `iterative_grid_pruning_after_render_executes_without_heap_cursor_corruption`
    and `aoc2025/04/solve2.gb`.
  - the failed slice still established two useful facts:
    - self tail recursion alone is not the dominant issue for the `solve2.gb`
      class of failures;
    - with named Goby frames now present in Wasm backtraces, the hot failing path
      is visible as `fold -> should_prune_cell/check_around_rolls ->
      collect_prune_positions/count_valid_roll`, which points more strongly to
      non-tail recursive scanning and list-building than to `check` alone.
- **Track RR, RR-0 / RR-1** (complete, 2026-04-08).
  - added a CLI regression for the minimal recursive list-spread memory bug.
  - classified runtime traps in `goby-wasm` with user-facing English messages
    and normalized CLI rendering.
  - emitted Wasm function names for Goby-generated functions so unknown-trap
    secondary detail now identifies frames like `goby!check` instead of only
    raw function indices.
  - fixed direct-call heap-cursor synchronization so:
    - caller-owned live heap values are flushed before every direct call,
    - heap-returning callees still refresh the caller cursor after the call.
  - verified this against:
    - the new recursive list-spread regression,
    - `cc4_mutable_write_capture_via_each_executes_correctly`,
    - `iterative_grid_pruning_after_render_executes_without_heap_cursor_corruption`,
    - `cargo clippy -- -D warnings`,
    - `cargo test --workspace`.
- **Track RR planning** (recorded, 2026-04-08). Added `PLAN.md §4.8c` to prioritize
  user-facing diagnostics first, then recursion/list-spread/runtime-limit resilience work.
- **Bug capture** (recorded, 2026-04-08). Added a minimal non-AoC reproduction to `doc/BUGS.md`
  for runtime-`Read` recursive list-spread memory exhaustion.
- **Track CL**: Closure-captured mutable list fix (complete, 2026-04-07). `lower_value_ctx` for effect/call args; cell-promoted `AssignIndex` write-back.
- **Track MLF**: Mutable list runtime execution fix (complete, 2026-04-07). Unified `RuntimeLocals` store.
- **Track LM**: Mutable list element assignment (complete, 2026-04-06). `a[i] := v`, `a[i][j] := v`.
- **Track CC**: Closure capture (complete, 2026-04-02). CC0–CC6, shared-cell model.

## Architecture State

| Layer | Status |
|---|---|
| Parser (`parser_stmt.rs`) | Stable. Includes list-index assignment (`AssignTarget`) |
| Resolver (`resolved.rs`) | Stable. `ResolvedTarget` for assignment targets |
| Typechecker | Stable. Validates assignment targets, effects, generics |
| IR (`ir.rs`) | Stable. Includes `CompExpr::AssignIndex` |
| IR lowering (`ir_lower.rs`) | Stable. `doc/PLAN_IR.md` is the architectural reference |
| Wasm backend | Stable. Closures, effects, list mutation implemented |
| Effect handlers | Non-tail / multi-resume produces `BackendLimitation` |
| GC / reclamation | Out of scope. Bump allocator only |

## Known Deferred Items

- Span ownership for multiline block arguments (`PLAN.md §4.5`)
- `Float` type (`PLAN.md §4.7`)
- Migrate effect runtime dispatch to compiled `EffectId`/`OpId` tables (`PLAN.md §5`)

## Open Questions

- For RR-3, what is the smallest honest boundary that can reduce stack pressure
  for non-tail recursive scans without destabilizing existing call semantics?
- For RR-4, is the first real win in list representation, list-spread lowering,
  concat runtime behavior, or memory-limit tuning after ownership is clearer?

## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — top-level roadmap
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — current language specification
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering boundary design reference
- [`doc/BUGS.md`](BUGS.md) — known bug tracker
