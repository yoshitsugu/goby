# Goby Project State Snapshot

Last updated: 2026-04-08

## Current Focus

Next track: **Track RR** (`PLAN.md §4.8c`) - runtime resource failure diagnostics and resilience.

Immediate next steps:

- **RR-2**: improve recursion resilience without asking users to rewrite natural
  recursive code first.
  - start by identifying tail-recursive shapes that can be lowered to loops or
    otherwise made less stack-hungry in the general-lowered Wasm path.
- **RR-3**: improve list-spread resilience for recursive list builders such as
  `[x, ..rest]`.
  - investigate ownership in lowering/runtime first, so the fix target stays in
    Goby rather than in user programs.
- keep RR-1 diagnostics best-effort and explicit about uncertainty
  (`likely stack pressure`, `memory exhaustion`, `unknown runtime trap`) as
  later resilience work lands.

## Recently Completed

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

- For RR-2, what is the smallest honest lowering/runtime boundary that can reduce
  stack pressure without destabilizing existing call semantics?
- For RR-3, is the first real win in list representation, list-spread lowering,
  or memory-limit tuning after ownership is clearer?

## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — top-level roadmap
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — current language specification
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering boundary design reference
- [`doc/BUGS.md`](BUGS.md) — known bug tracker
