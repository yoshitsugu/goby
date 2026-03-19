# Goby Project State Snapshot

Last updated: 2026-03-19

## Current Focus

- `doc/PLAN_IR.md` is the active roadmap.
- IR0 through IR11 are landed.
- The IR-lowering roadmap is complete.

## Immediate Next Steps

1. Keep future lowering work aligned with `resolved form -> shared IR -> backend`.
2. Treat backend limitations as backend limitations rather than restoring AST-shaped recognizers.
3. Continue Track E from `E3 -> E4 -> E5`: implement backend execution for explicit grapheme/list intrinsics without widening planner fallback.
4. Reopen `doc/PLAN_IR.md` only if a genuinely new architectural gap appears.

## Restart Notes

- Read `doc/PLAN_IR.md` first.
- Mutation lowering is landed through shared IR: `mut` bindings lower to `CompExpr::LetMut`, assignment lowers to `CompExpr::Assign`, and non-local assignment targets are rejected during IR lowering.
- Backend-boundary convergence is landed for the current representative slice: native fallback no longer rejects `mut` bindings without assignment, and assignment is reported as an explicit native backend limitation rather than a generic unsupported statement.
- General Wasm lowering classification now checks emitter support instead of assuming every lowered backend IR sequence is emit-ready.
- Wasm compile-path tests now run structural validation with `wasmparser::Validator`, which caught and now guards against invalid-stack-shape regressions.
- Runtime-I/O plans that delegate to general backend emission append an explicit final `Drop`, matching the `_start : () -> ()` Wasm contract.
- Non-fused `CallHelper` emission is landed for `string.split`, `list.get`, and `string.length`, backed by a downward bump-allocation ABI for runtime strings/lists in general Wasm lowering.
- Non-fused helper shapes now execute end to end in wasmtime for `split -> drop` and `split -> list.get -> alias -> println`.
- Track E E1/E2 are landed:
  - the grapheme/list backend work is locked to a narrow intrinsic-aware stdlib-decl execution path rather than arbitrary handler support in general Wasm lowering,
  - backend lowering now uses explicit backend intrinsics instead of stringly helper-name dispatch for new grapheme-track primitives,
  - `__goby_string_each_grapheme` and `__goby_list_push_string` lower structurally but remain emitter-unsupported until E3/E4.
- Track E bridge slice is landed for the current selective-import `goby/string.graphemes` runtime-`Read` path:
  - runtime-I/O classification now routes `read -> graphemes -> print/index` programs to `InterpreterBridge` instead of generic `Unsupported`,
  - CLI `run` executes that subset through seeded-stdin interpreter runtime rather than pretending it is Wasm-lowerable,
  - end-to-end regression coverage now locks emoji-family grapheme behavior for the bridge path.
- Track E E4 substrate is partially landed:
  - `__goby_list_push_string` now emits through the shared tagged list/string ABI in general Wasm lowering,
  - regression coverage locks a runtime-`Read` helper chain `split -> __goby_list_push_string -> list.get -> print`,
  - remaining work is E3 semantic grapheme iteration, not list accumulation layout.
- Track E E3 semantic-authority groundwork is landed:
  - Unicode Extended Grapheme Cluster segmentation now lives behind a dedicated backend/runtime helper module,
  - imported `goby/string.graphemes` evaluation and `__goby_string_each_grapheme` runtime intrinsic execution both use that shared authority,
  - remaining E3 work is emitter-side execution, not deciding semantics in multiple places.
- Remaining helper work is incremental family expansion on top of the emitter ABI, not a reason to restore planner or AST-shaped fallback.
- The IR-lowering roadmap is complete; follow-up work should stay within the converged lowering architecture.
- Then inspect:
  - `crates/goby-wasm/src/fallback.rs`
  - `crates/goby-wasm/src/lower.rs`
  - `doc/PLAN_IR.md`
