# Goby Project State Snapshot

Last updated: 2026-04-06

## Current Focus

No designated active track at this point.

Candidates for the next track:

1. **List index precedence fix** (`PLAN.md §2.1`) — `f xs[0]` currently parses as
   `(f xs)[0]` instead of `f (xs[0])`; the parser's postfix precedence is wrong.
2. **Track Float** (`PLAN.md §4.7`) — add a first-class `Float` type.
3. **Track EP** (`PLAN.md §4.9`) — effect row polymorphism for HOF effect propagation.

## Recently Completed

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

- Parser precedence bug: `f xs[0]` parses as `(f xs)[0]` (`PLAN.md §2.1`)
- Span ownership for multiline block arguments (`PLAN.md §4.5`)
- `Float` type (`PLAN.md §4.7`)
- Migrate effect runtime dispatch to compiled `EffectId`/`OpId` tables (`PLAN.md §5`)
- Nested list evaluation in the fallback runtime (`doc/BUGS.md`)

## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — top-level roadmap
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — current language specification
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering boundary design reference
- [`doc/BUGS.md`](BUGS.md) — known bug tracker
