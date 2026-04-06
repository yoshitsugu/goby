# Goby Project State Snapshot

Last updated: 2026-04-06

## Current Focus

### Track MLF: Mutable List Runtime Execution Fix

Current staged status:

- rooted mutable-list updates now classify as a semantic runtime capability and route to the
  `GeneralLowered` Goby-owned Wasm path even for `Print`-only programs.
- shared IR now carries pure `list.get` reads inside interpolation, so `goby run` succeeds for
  update-followed-by-interpolation shapes such as `println("${a[1][0]},${a[1][1]}")` after
  `a[1][1] := 30`.
- remaining work is the longer-term fallback/runtime recursive-aggregate convergence described in
  `doc/PLAN_MUT_LIST_FIX.md` MLF-2.

## Recently Completed

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
- Recursive aggregate convergence in the fallback runtime after the mutable-list routing fix
  (`doc/PLAN_MUT_LIST_FIX.md` MLF-2)

## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — top-level roadmap
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — current language specification
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering boundary design reference
- [`doc/BUGS.md`](BUGS.md) — known bug tracker
