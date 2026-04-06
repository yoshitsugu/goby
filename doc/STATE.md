# Goby Project State Snapshot

Last updated: 2026-04-07

## Current Focus

No active track. See `doc/PLAN.md` for next candidates.

## Recently Completed

### Track MLF: Mutable List Runtime Execution Fix (complete, 2026-04-07)

All milestones complete (MLF-0 through MLF-3, including MLF-2 fallback runtime value
unification). `RuntimeLocals` now uses a single `values: HashMap<String, RuntimeValue>` store;
`ListIntEvaluator` and shape-specific type maps have been removed. `IntEvaluator` reads from
`RuntimeLocals` via `int_view()` at the entry point. Rooted mutable-list updates route through
the `GeneralLowered` Wasm path for all well-typed programs.

### Track MLF staged milestone (2026-04-06)

Complete for the routing slice: mutable rooted list updates no longer fall through to
fallback/static execution, and the exact `goby run` reproduction now succeeds.

### Track LM: Mutable List Element Assignment (2026-04-06)

Complete. `a[i] := v` and `a[i][j] := v` work end-to-end (LM0–LM4).

### Track CC: Closure Capture (2026-04-02)

CC0–CC6 complete. The `GeneralLowered` Wasm path fully implements closure capture
semantics. The fallback/interpreter runtime now matches the same semantics.

### Track TD: Typed Diagnostic Spans (2026-04-05)

Complete (TD0–TD5). Remaining: multiline/body-relative expression span ownership (deferred).

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
