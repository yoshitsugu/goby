# Goby Project State Snapshot

Last updated: 2026-03-31

## Current Focus

**Track CC / CC1** (next): Analysis and IR ownership.
CC0 (semantics lock) is complete — `doc/LANGUAGE_SPEC.md` is the sole semantic authority
for closure capture. CC1 will introduce capture classification and the shared mutable-cell
model at the IR ownership boundary.

Landed in this slice:

- shared closure-capture analysis now lives in `crates/goby-core/src/closure_capture.rs`
- callable-environment metadata is explicit for both zero-capture and capturing lambdas
- captured `mut` bindings are assigned stable shared `MutableStorageId`s before Wasm lowering
- `goby-wasm` now consumes the shared callable-environment metadata instead of its old
  backend-local free-variable traversal when rejecting unsupported capturing lambdas

## Recently Completed

- **Track CC / CC1 slice 1** (2026-03-31): Added shared closure-capture analysis and callable-environment ownership metadata in `goby-core`; Wasm lowering now rejects capture through that shared metadata path instead of backend-local free-variable analysis.
- **`fn`-only anonymous functions** (2026-03-31): Pipe-lambda `|x| ->` removed. `fn x -> expr` is the only form. Parser rejects old syntax; formatter, tooling, docs, and all tests updated.
- **Track H / HOF milestone series** (complete through HOF-M7): `fn` keyword, multi-param lambdas, effectful callbacks, `fold`, end-to-end acceptance gate.
- **Track E** (HOF type checking, 2026-03-27): Callback arity mismatches rejected at `goby-cli check`.
- **Track F** (stdlib `int.to_string`, 2026-03-25): End-to-end; direct calls and named callback use covered.
- **Track ER** (compiler error reporting, 2026-03-29): Unresolved/ambiguous names, import diagnostics, CLI/LSP parity.

## Immediate Next Steps

Next track candidates are in `doc/PLAN.md` §4:

- **Track CC: Closure Capture** (§4.6): CC1 slice 1 is in. Next step is runtime representation and lowering follow-up: closure record/env layout, mutable-cell runtime helpers, and using callable-env metadata for non-empty environments.
- **Track D follow-ups** (§4.1): D5 (`goby lint` unused-binding rule), D6c shared grammar asset.
- **Track WB-3B** (deferred): WasmFX typed continuations — on hold until WebAssembly stack switching reaches Phase 4.
- **Float support** (§4.7): `Float` type backed by Wasm `f64`; semantics to be locked before coding.

## Architecture State

- Resolved-form → shared IR boundary is stable (IR0–IR11 done).
- Wasm backend lowering design is locked in `doc/PLAN_IR.md`:
  - Phase WB-1: pure control flow and operators ✓
  - Phase WB-2: pattern matching and structured data ✓
  - Phase WB-3: function values and effect handlers (direct-call lowering, one-shot tail-resumptive) ✓
  - Phase WB-3B (future): WasmFX stack switching when proposal reaches Phase 4
- `GeneralLowered` coverage includes:
  - Pure control flow: `If`, `BinOp`, `Interp`, `LetMut`, `Assign`
  - Pattern matching: `Case` with literal/list patterns
  - Structured data: `ListLit`, `TupleLit`, `RecordLit`
  - Decl calls / recursion / higher-order funcref calls
  - Backend effect dispatch (typed `BackendEffectOp` / `BackendPrintOp`)
  - stdlib `list.each` / `list.map` / `list.fold`
  - Effect handlers: `Handle` / `WithHandler` / tail `Resume` (one-shot tail-resumptive subset)
  - Function values: `Lambda` (no-capture only); stdlib `graphemes` via wrapper AuxDecl
  - Host intrinsics: `StringGraphemesList` (`__goby_string_graphemes_list`)
- Known limitations:
  - non-tail / multi-resume handlers → `BackendLimitation` error
  - lambda with free variables (closure capture) still returns `UnsupportedForm` on the Wasm path, but the rejection now comes from shared callable-environment metadata rather than backend-local free-variable traversal
  - inline capturing lambda passed to HOF callbacks (e.g. `fold (fn acc x -> acc + x + bias)`) → `UnsupportedForm` on Wasm path — Track CC (CC4) will implement
  - interpreter path: capturing lambdas work but use snapshot semantics for `mut` captures (not the spec's shared-cell model); will be corrected as part of Track CC

## Key Entry Points

- `doc/PLAN_IR.md` — Wasm backend lowering design and phase plan
- `doc/PLAN_CLOSURE_CAPTURE.md` — closure capture design and milestones
- `crates/goby-core/src/closure_capture.rs` — shared closure-capture analysis and callable-environment metadata
- `crates/goby-wasm/src/gen_lower/lower.rs` — `lower_comp` / `lower_value`
- `crates/goby-wasm/src/gen_lower/emit.rs` — Wasm instruction emission
- `crates/goby-wasm/src/gen_lower/backend_ir.rs` — backend IR instruction set
