# Goby Project State Snapshot

Last updated: 2026-03-18

## Current Focus

- Next active work: **Track G — Shared Typed IR Boundary**
  - Detailed architecture and milestone tracker: `doc/PLAN_IR.md`
  - Roadmap entry: `doc/PLAN.md` §4.7

## Immediate Next Steps

1. Create shared IR module skeletons in `goby-core`.
2. Add an IR text/debug printer and basic IR validation helpers.
3. Lower a minimal pure typed subset from AST into IR.
4. Do not add new ad-hoc AST-shape branches to runtime-I/O planning while Track G is active.

## Decisions To Carry Forward

- Shared IR must become the semantic boundary for both native lowering and, eventually, portable fallback execution.
- `goby-wasm` may perform backend-local analysis over IR, but it should not own source-language semantic canonicalization.
- Existing typed-continuation metadata in `crates/goby-wasm/src/lower.rs` is transitional; it should migrate into shared IR ownership or become a backend analysis product derived from shared IR.
- Runtime-I/O support should move away from source-shape classification and toward IR-based capability analysis.

## Deferred Work Still Relevant Later

- `PLAN_STANDARD_LIBRARY.md`: finish stdlib-driven `goby/string.split` and retire the runtime builtin path
- `PLAN.md` Track D5: `goby lint`
- `PLAN.md` Track D6c: shared grammar asset
- `PLAN.md` D6b-ts: Tree-sitter grammar after D6c

## Restart Notes

- `doc/PLAN_IR.md` is now the detailed execution document for Track G, including checkbox milestones `G1` through `G10`.
- `doc/PLAN.md` was intentionally trimmed to keep only active or still-relevant planning items.
