# Goby Project State Snapshot

Last updated: 2026-04-07

## Current Focus

No active track. See `doc/PLAN.md` for next candidates.

## Recently Completed

- **Track MLF**: Mutable list runtime execution fix (complete, 2026-04-07). Unified `RuntimeLocals` store.
- **Track LM**: Mutable list element assignment (complete, 2026-04-06). `a[i] := v`, `a[i][j] := v`.
- **Track CC**: Closure capture (complete, 2026-04-02). CC0–CC6, shared-cell model.
- **Track TD**: Typed diagnostic spans (complete, 2026-04-05). TD0–TD5.

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
## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — top-level roadmap
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — current language specification
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering boundary design reference
- [`doc/BUGS.md`](BUGS.md) — known bug tracker
