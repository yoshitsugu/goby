# Goby Project State Snapshot

Last updated: 2026-03-18 (G9 in progress)

## Current Focus

- Next active work: **Track G — Shared Typed IR Boundary**
  - Detailed architecture and milestone tracker: `doc/PLAN_IR.md`
  - Roadmap entry: `doc/PLAN.md` §4.7
  - G1–G8 complete
  - G9 is in progress: module-centric fallback entrypoints now cover compile,
    execute, parity, and runtime test call sites

## Immediate Next Steps

1. G9: Portable fallback should align to the shared IR boundary.
   - Reduce fallback execution internals that still require raw parsed AST
     bodies (`runtime_resolver`, runtime decl/dispatch metadata, fallback
     capability checks).
   - Keep fallback/native parity testable from the same shared-IR semantic
     handoff without reintroducing body/`parsed_body` plumbing at call
     boundaries.
2. Do not add new ad-hoc AST-shape branches to runtime-I/O planning while Track G is active.

## Decisions To Carry Forward

- Shared IR must become the semantic boundary for both native lowering and, eventually, portable fallback execution.
- `goby-wasm` may perform backend-local analysis over IR, but it should not own source-language semantic canonicalization.
- Existing typed-continuation metadata in `crates/goby-wasm/src/lower.rs` is transitional; handler/resume gating now derives from IR-based lowering-plan metadata, and the remaining evidence payload should migrate into shared IR ownership or become a backend analysis product derived from shared IR.
- Runtime-I/O support should move away from source-shape classification and toward IR-based capability analysis.
- Module-centric wrappers are now the public/test-facing fallback boundary in
  `goby-wasm`; remaining raw AST handoff is an internal migration detail to
  shrink under G9.

## Deferred Work Still Relevant Later

- `PLAN_STANDARD_LIBRARY.md`: finish stdlib-driven `goby/string.split` and retire the runtime builtin path
- `PLAN.md` Track D5: `goby lint`
- `PLAN.md` Track D6c: shared grammar asset
- `PLAN.md` D6b-ts: Tree-sitter grammar after D6c

## Restart Notes

- `doc/PLAN_IR.md` is now the detailed execution document for Track G, including checkbox milestones `G1` through `G10`.
- `doc/PLAN.md` was intentionally trimmed to keep only active or still-relevant planning items.
