# Goby Project State Snapshot

Last updated: 2026-04-09

## Current Focus

Next slice: **Track RR, RR-5** (`PLAN.md §4.5`) - direct-call group execution
model on top of normalized `TailDeclCall` backend IR.

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

- **RR-5**: continue the planned generic tail-call optimization track now that
  the first shared tail-position analysis and direct tail-call normalization
  slices are in place.
  - keep the RR-3/RR-4 shared-boundary discipline: prefer reusable control-flow
    rules over symbol-specific recursion rewrites.
  - next RR-5 task: define the smallest honest grouped execution model that can
    run `TailDeclCall` in constant stack for direct known-declaration calls.
  - keep self-tail recursion only as one member of that direct-call group
    model, not the headline feature claim.
  - keep current diagnostics explicit for shapes that still fall outside the
    optimized subset.
- keep RR-1 diagnostics best-effort and explicit about uncertainty
  (`likely stack pressure`, `memory exhaustion`, `unknown runtime trap`) as
  later resilience work lands.

## Recently Completed

- **Track RR, RR-5 shared tail-position boundary** (partial, 2026-04-09).
  - added `goby_core::tail_analysis` as a compiler-owned IR analysis for tail
    position propagation through `let`, `seq`, `if`, and `case`.
  - made nested computation boundaries explicit via configurable lambda /
    handler-clause policies so future TCO work can state exactly which bodies it
    analyzes.
  - locked focused tests for direct tail-call collection in declaration mode and
    non-tail treatment of nested lambda bodies for resumptive-handler mode.
  - rewired `goby-wasm` handler legality to use the shared analysis, proving
    the new RR-5 boundary in existing behavior without widening the accepted
    resumptive subset.
  - this slice establishes RR-5 milestone M1 ownership but does not yet
    normalize or execute generic tail calls in constant stack.

- **Track RR, RR-5 direct tail-call normalization** (partial, 2026-04-09).
  - normalized eligible direct top-level declaration calls in tail position to
    `WasmBackendInstr::TailDeclCall` rather than leaving them indistinguishable
    from ordinary `DeclCall`.
  - propagated that normalization through tail `if` / `case` joins while
    keeping non-tail statement positions on the ordinary direct-call form.
  - kept emitter behavior unchanged for now, so `TailDeclCall` still executes
    like a normal direct Wasm call until the direct-call group execution model
    lands.
  - locked focused lowering tests for:
    - root tail-position direct calls,
    - non-tail `seq` statements staying ordinary,
    - `case` arm tails normalizing to the tail-call marker.

- **Track RR, RR-4 builder-backed list-spread lowering** (complete, 2026-04-09).
  - fixed the historical recursive `[x, ..rest]` memory-exhaustion bug without
    changing the external `List` representation.
  - restricted self-recursive list builders now lower to a loop plus internal
    growable builder ops instead of recursively materializing singleton prefix
    lists and concatenating the full tail at every step.
  - compile coverage now proves the emitted Wasm validates and no longer
    directly self-calls in the specialized helper body.
  - runtime coverage now locks both the original `doc/BUGS.md` repro and a
    larger builder-shaped variant as successful executions.
  - a follow-up slice widened the same RR-4 ownership to inline empty-acc
    `fold` callbacks that prepend with `[x, ..acc]`, replacing callback-side
    concat chains with a reverse traversal over the source list.
  - compile coverage now locks that main-level rewrite via
    `compile_module_inline_fold_prepend_lowering_rewrites_concat_chain_in_main`.
  - runtime coverage now locks the inline callback success case via
    `rr4_inline_fold_prepend_builder_executes_after_specialized_lowering`.
  - the final RR-4 slice rewrote supported named/local callback prepend shapes
    onto the same reverse-fold boundary, closing the remaining `fold seed []
    prepend` style bucket without changing user-visible `List` semantics.
  - compile coverage now locks the named/local callback rewrite via
    `compile_module_named_fold_prepend_lowering_rewrites_decl_callback_chain_in_main`
    and
    `compile_module_local_alias_fold_prepend_lowering_rewrites_decl_callback_chain_in_main`.
  - runtime coverage now locks both named/local callback executions via
    `rr4_named_callback_list_spread_chain_executes_after_callback_rewrite` and
    `rr4_local_named_callback_list_spread_chain_executes_after_callback_rewrite`.
  - RR-4 is complete for the currently locked representative list-spread /
    prepend-builder buckets.

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
- **Track RR planning** (recorded, 2026-04-08). Added `PLAN.md §4.5` to prioritize
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

- Multiline/body-relative span ownership for diagnostics (`doc/PLAN.md`, review follow-ups and spec-detail notes)
- `Float` type (`PLAN.md §4.3`)
- Migrate effect runtime dispatch to compiled `EffectId`/`OpId` tables (`PLAN.md §5`)

## Open Questions

- Should the first generic TCO slice target direct self-tail calls only, or
  should it include mutually tail-recursive direct calls from the start so the
  eventual language-level claim stays stable?
- Which direct-call forms should the first normalization slice accept once
  `tail_analysis` has marked tail position: only direct declaration names, or
  also resolvable local aliases to those names?
- Should the first constant-stack execution model handle only self
  `TailDeclCall`, or grouped sibling direct calls from the start so RR-5 does
  not stall at a self-recursion-only boundary?
- For unsupported tail-call shapes, what is the stable contract: explicit
  compile-time rejection, ordinary non-TCO execution, or backend-specific
  capability reporting?

## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — top-level roadmap
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — current language specification
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering boundary design reference
- [`doc/BUGS.md`](BUGS.md) — known bug tracker
