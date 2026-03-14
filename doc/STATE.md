# Goby Project State Snapshot

Last updated: 2026-03-14

## Current Focus

- Active track: Track F
- Current goal: shrink the remaining `InterpreterBridge` surface
- `cargo test` is green

## Current Runtime-I/O Boundary

Dynamic:

- echo shapes for `read()` / `read_line()`
- local alias / forwarded alias echo variants
- trailing static print suffixes after echo
- `read` + `split(text, "\n")` + `each` when the callback is output-passthrough
- callback alias / delimiter alias chain variants of that split family
- trailing static print suffixes after `each`

InterpreterBridge:

- the narrow transformed split-callback family
- canonical regression shape:
  `text = read(); lines = split(text, "\n"); each lines (|line| -> println "${line}!")`

Unsupported:

- runtime-read transforms outside that bridge subset
- important fixed examples:
  - `decorated = "${text}!"` after `text = read()`
  - mixed `read_line()` then `read()`
  - repeated `read()` programs that depend on post-exhaustion behavior in codegen

NotRuntimeIo:

- complex static-output programs with bindings still fall through to fallback resolution
  instead of `StaticOutput` collapse

## Important Current Facts

- `runtime_io_plan.rs` owns runtime-I/O classification.
- `backend.rs` owns dynamic WASI stdin/stdout lowering helpers.
- `compile_module` does not consume compiler-process stdin for `Read` programs.
- `execute_module_with_stdin` is temporary and only valid for `InterpreterBridge`.
- `goby run` follows planner classification, not codegen error text.
- stdlib `goby/string.graphemes` is implemented.

## Next Slice

- Try to move the canonical bridge-only transformed split-callback shape
  (`|line| -> println "${line}!"`) into `DynamicWasiIo`.

Constraints:

- do not add planner-bypassing runtime-I/O branching in `crates/goby-wasm/src/lib.rs`
- prefer extending `RuntimeIoPlan` / backend ownership rather than adding one-off matcher hacks
- keep at least one explicit regression on the bridge until a matching dynamic lowering exists

## If Blocked

- finish the remaining Track F non-feature work:
  - document the execution contract around `DynamicWasiIo` vs `StaticOutput`
  - sweep for any other compile-time fallback paths that could observe host runtime state
