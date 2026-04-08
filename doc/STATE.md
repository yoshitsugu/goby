# Goby Project State Snapshot

Last updated: 2026-04-08

## Current Focus

Next track: **Track RR** (`PLAN.md §4.8c`) - runtime resource failure diagnostics and resilience.

Immediate next steps:

- **RR-0**: convert the new `doc/BUGS.md` recursive list-spread reproduction into a regression test.
- **RR-1**: improve `goby-cli` / `goby-wasm` runtime error reporting so resource-related failures
  surface as a user-facing explanation instead of a raw Wasm backtrace.
- keep the first message layer best-effort and explicit about uncertainty
  (`likely stack pressure`, `memory exhaustion`, `unknown runtime trap`).

## Recently Completed

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

- For RR-1, how much trap classification can be made reliable from Wasmtime error surfaces
  versus Goby-owned explicit runtime error codes?
- For RR-2/RR-3, which limit is the better first target after diagnostics:
  recursion depth, recursive list-spread allocation shape, or both in parallel?

## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — top-level roadmap
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — current language specification
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering boundary design reference
- [`doc/BUGS.md`](BUGS.md) — known bug tracker
