# Goby Project State Snapshot

Last updated: 2026-04-06

## Current Focus

Track LM (Mutable List Element Assignment) is complete.
There is no designated active track at this point.

Candidates for the next track:

1. **Track TD2 continuation** (`doc/PLAN_ERROR.md`) — remaining typed diagnostic span
   coverage; multiline/body-relative expression ownership is still deferred.
2. **List index precedence fix** (`PLAN.md §2.1`) — `f xs[0]` currently parses as
   `(f xs)[0]` instead of `f (xs[0])`; the parser's postfix precedence is wrong.
3. **Track Float** (`PLAN.md §4.7`) — add a first-class `Float` type.

## Recently Completed

### Track LM: Mutable List Element Assignment (2026-04-06)

`a[i] := v` and `a[i][j] := v` work end-to-end through the full Goby compiler pipeline.

- **LM0**: Semantics locked in `LANGUAGE_SPEC.md` §3.
- **LM1a–c**: `AssignTarget` / `ResolvedTarget` AST extensions; parser handles single-level
  and nested index forms; all ~15 construction sites updated.
- **LM2**: Typechecker rejects immutable roots, undeclared roots, non-List receivers,
  type mismatches, and non-Int index expressions via `check_assign_target_chain` (7 tests).
- **LM3a**: IR `CompExpr::AssignIndex` node added with fmt/validate/closure-capture arms.
- **LM3b**: `ir_lower.rs` lowers `ResolvedTarget::ListIndex` → `AssignIndex` via
  `lower_list_index_assign` (ANF-hoists effectful index expressions).
- **LM3c**: Wasm backend `BackendIntrinsic::ListSet` (alloc + path-copy) and
  `lower_assign_index` supporting arbitrary nesting depth.
- **LM4**: `examples/mut_list.gb`; 4 runtime integration tests covering single-level,
  multi-index, value semantics, and two-level nested update.

Final test counts: 688 goby-core + 560 goby-wasm + 62 integration — all passing.

### Track CC: Closure Capture (2026-04-02)

CC0–CC6 complete. The `GeneralLowered` Wasm path fully implements closure capture
semantics. The fallback/interpreter runtime now matches the same semantics.

### Track TD2: Typed Diagnostic Spans (as of 2026-04-05)

Diagnostic spans for effect-op and `resume` argument mismatches are implemented in
`goby-core`. Remaining: honest span ownership for multiline/body-relative expressions
(deferred).

## Architecture State

| Layer | Status |
|---|---|
| Parser (`parser_stmt.rs`) | Parses list-index assignment via `AssignTarget` |
| Resolver (`resolved.rs`) | Represents assignment targets via `ResolvedTarget` |
| Typechecker | Validates list-index assignment via `check_assign_target_chain` |
| IR (`ir.rs`) | `CompExpr::AssignIndex` implemented |
| IR lowering (`ir_lower.rs`) | `lower_list_index_assign` with path-copy semantics |
| Wasm backend | `BackendIntrinsic::ListSet` + `lower_assign_index` implemented |
| Effect handlers | Non-tail / multi-resume produces `BackendLimitation` |
| Resolved → IR boundary | Stable. `doc/PLAN_IR.md` is the architectural reference |
| GC / reclamation | Out of scope. Bump allocator only |

## Known Deferred Items

- Parser precedence bug: `f xs[0]` parses as `(f xs)[0]` (`PLAN.md §2.1`)
- Span ownership for multiline block arguments (`doc/PLAN_ERROR.md`)
- `Float` type (`PLAN.md §4.7`)
- Migrate effect runtime dispatch to compiled `EffectId`/`OpId` tables (`PLAN.md §5`)
- Nested list evaluation in the fallback runtime (`doc/BUGS.md`)

## Key Entry Points

- [`doc/PLAN.md`](PLAN.md) — top-level roadmap
- [`doc/PLAN_ERROR.md`](PLAN_ERROR.md) — typed diagnostic span plan (next candidate)
- [`doc/PLAN_LIST_MUT.md`](PLAN_LIST_MUT.md) — completed LM detail plan
- [`doc/LANGUAGE_SPEC.md`](LANGUAGE_SPEC.md) — current language specification
- [`doc/PLAN_IR.md`](PLAN_IR.md) — IR lowering boundary design reference
- [`doc/BUGS.md`](BUGS.md) — known bug tracker
