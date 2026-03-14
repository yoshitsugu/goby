# Goby Project State Snapshot

Last updated: 2026-03-15

## Current Focus

- Semantics planning update: `can` is now specified as "unhandled effects that escape a function body"
- Inline `with` handler clauses now target: unique bare operation names by default, qualified `Effect.operation` only when disambiguation is needed
- Next implementation-planning target: align parser/typechecker/examples/stdlib with the new `can` semantics
- Track D — Developer Tooling Foundation remains the next broader roadmap track after the `can` semantics slice

## Locked Decisions Carried Forward

- Runtime-I/O ownership stays split as:
  - `runtime_io_plan.rs` for classification/planning
  - `backend.rs` for dynamic WASI stdin/stdout lowering helpers
  - CLI as path selector only
- `goby run` must follow planner classification, not codegen error text
- No planner-bypassing runtime-I/O branching in `crates/goby-wasm/src/lib.rs`
- `InterpreterBridge` remains only as a temporary extension point; current reachable surface is empty
- `fetch_env_var` still reads the compiler-process environment at compile time for direct-style and effect-boundary programs.
  Open policy question remains tracked by `TODO(F-sweep)` in `lower.rs`.

## Current Runtime-I/O Boundary

Dynamic (`DynamicWasiIo`):

- echo shapes for `read()` / `read_line()`
- local alias / forwarded alias echo variants
- trailing static print suffixes after echo
- `read` + `split(text, "\n")` + `each` when the callback is output-passthrough
- callback alias / delimiter alias chain variants of that split family
- trailing static print suffixes after `each`
- transformed split-callback family: `|line| -> println "${line}!"` with static
  prefix/suffix decorations

Unsupported:

- runtime-read transforms outside the DynamicWasiIo subset
- important fixed examples:
  - `decorated = "${text}!"` after `text = read()`
  - mixed `read_line()` then `read()`
  - repeated `read()` programs that depend on post-exhaustion behavior in codegen

NotRuntimeIo:

- complex static-output programs with bindings still fall through to fallback resolution
  instead of `StaticOutput` collapse

## Next Slice

`can` semantics alignment is the next default starting point.

Immediate planning goals:

1. Remove the old effect-member-`can` model in implementation.
2. Rebuild effect checking around one residual-effect model for expressions and function bodies.
3. Add regression coverage for handled-vs-unhandled function effects and inline handler clause resolution before broader refactors.
