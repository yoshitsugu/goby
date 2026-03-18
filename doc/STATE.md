# Goby Project State Snapshot

Last updated: 2026-03-18 (G10 in progress)

## Current Focus

- Next active work: **Track G — Shared Typed IR Boundary**
  - Detailed architecture and milestone tracker: `doc/PLAN_IR.md`
  - Roadmap entry: `doc/PLAN.md` §4.7
  - G1–G9 complete; next is G10 finish-up

## Immediate Next Steps

1. G10: Remove the remaining Track G transitional seams.
   - Keep `crates/goby-wasm/src/wasm_exec_plan.rs` as the single formal
     backend-local semantic layer over shared IR; do not reintroduce separate
     ad-hoc adapters for planning, runtime, and lowering.
   - Decide whether backend-local typed-continuation evidence in
     `crates/goby-wasm/src/lower.rs` should migrate into shared IR ownership or
     remain a backend analysis derived from shared IR.
   - Audit runtime-I/O and fallback diagnostics for any remaining wording that
     implies source-shape classification rather than IR/backend capability.
2. Do not add new ad-hoc AST-shape branches to runtime-I/O planning while Track G is active.

## Decisions To Carry Forward

- Shared IR must become the semantic boundary for both native lowering and, eventually, portable fallback execution.
- `goby-wasm` may perform backend-local analysis over IR, but it should not own source-language semantic canonicalization.
- Existing typed-continuation metadata in `crates/goby-wasm/src/lower.rs` is transitional; handler/resume gating now derives from IR-based lowering-plan metadata, and the remaining evidence payload should migrate into shared IR ownership or become a backend analysis product derived from shared IR.
- Runtime-I/O support should move away from source-shape classification and toward IR-based capability analysis.
- Portable fallback now enters through shared-IR-derived runtime artifacts for
  `main`, runtime declaration lookup, simple evaluators, and native capability
  checks; raw parsed AST fallback remains only as an internal compatibility path
  when IR lowering is unavailable.
- `crates/goby-wasm/src/wasm_exec_plan.rs` is now the named backend-local layer
  over shared IR for Wasm execution/planning inputs. Further Track G cleanup
  should consolidate backend consumers onto that layer instead of adding new
  parallel adapters.

## Deferred Work Still Relevant Later

- `PLAN_STANDARD_LIBRARY.md`: finish stdlib-driven `goby/string.split` and retire the runtime builtin path
- `PLAN.md` Track D5: `goby lint`
- `PLAN.md` Track D6c: shared grammar asset
- `PLAN.md` D6b-ts: Tree-sitter grammar after D6c

## Restart Notes

- `doc/PLAN_IR.md` is now the detailed execution document for Track G, including checkbox milestones `G1` through `G10`.
- `doc/PLAN.md` was intentionally trimmed to keep only active or still-relevant planning items.
