# Goby Project State Snapshot

Last updated: 2026-04-08

## Current Focus

Next slice: **Track RR, RR-3** (`PLAN.md §4.8c`) - recursion resilience at the shared
non-tail scan boundary.

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

- **RR-4**: keep recursive list spread / concat growth as a separate ownership
  track after RR-3, since RR-2 kept it in runtime data representation/list
  concat ownership rather than recursion lowering.
  - preserve the RR-3 boundary split: recursion lowering now owns the scan
    buckets, while list-spread resilience should be solved in list/runtime
    ownership rather than by adding more recursion-specific rewrites.
- keep RR-1 diagnostics best-effort and explicit about uncertainty
  (`likely stack pressure`, `memory exhaustion`, `unknown runtime trap`) as
  later resilience work lands.

## Recently Completed

- **Track RR, RR-3 tight-stack proof** (complete, 2026-04-09).
  - kept the RR-3 fix at the shared scan boundary: restricted self-recursive
    Int scans now lower to loop-form backend IR instead of self `DeclCall`.
  - proved the compile path end-to-end: emitted Wasm validates and the looped
    helper body no longer directly calls itself.
  - proved the execution boundary under the RR low-stack configuration for both
    the primary non-tail scan bucket and the callback-assisted scan bucket.
  - RR-3 is now closed for the currently locked representative scan buckets;
    remaining resilience work moves to RR-4 list-spread ownership.

- **Track RR, RR-3 emitted-Wasm proof** (partial, 2026-04-09).
  - added backend-IR loop support plus a lowering rewrite for a restricted
    self-recursive Int scan shape in `gen_lower/lower.rs`.
  - switched loop emission to `loop (result i64)` so the specialized scan shape
    produces valid Wasm without relying on an outer result block.
  - verified both that supported scan shapes lower to `Loop` and that the
    compiled Wasm helper body validates and eliminates direct self `call`.
- **Track RR, RR-2 representative decomposition** (complete, 2026-04-08).
  - added focused Goby-owned representative repro tests in
    `crates/goby-wasm/src/runtime_rr_tests.rs` for:
    - self tail recursion under a tight stack limit,
    - non-tail recursive scan,
    - recursive list spread / concat growth,
    - callback-assisted recursion in the scan hot path.
  - locked the RR-2 ownership decision:
    - self tail recursion belongs to shared recursion lowering/runtime stack
      behavior, but is not the default next slice by itself;
    - non-tail recursive scans are the primary RR-3 target;
    - callback-assisted recursion currently stays bundled with that same RR-3
      scan boundary rather than a separate callback-only fix;
    - recursive list spread remains RR-4 work owned by list representation /
      concat behavior, not by recursion lowering.
  - explicitly rejected these first-fix boundaries:
    - Wasm limit tuning alone,
    - symbol-specific rewrites for `count_valid_roll` /
      `collect_prune_positions`,
    - self-tail-only lowering as the default next slice.
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
- Why does the current loop-form lowering still lack a confirmed tight-stack
  runtime win: remaining Wasm emission shape, stack-limit calibration, or some
  other execution-boundary cost?
- Within that RR-3 boundary, should callback-assisted scans be improved by the
  same rule automatically, or do they expose a second shared sub-boundary once
  the scan case is modeled?
- For RR-4, is the first real win in list representation, list-spread lowering,
  concat runtime behavior, or memory-limit tuning after ownership is clearer?

## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — top-level roadmap
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — current language specification
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering boundary design reference
- [`doc/BUGS.md`](BUGS.md) — known bug tracker
