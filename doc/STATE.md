# Goby Project State Snapshot

Last updated: 2026-03-14

## Current Focus

- Active track: Track F
- Current goal: Track F remaining non-feature work (docs, sweep, Phase F5)
- `cargo test` is green

## Current Runtime-I/O Boundary

Dynamic (DynamicWasiIo):

- echo shapes for `read()` / `read_line()`
- local alias / forwarded alias echo variants
- trailing static print suffixes after echo
- `read` + `split(text, "\n")` + `each` when the callback is output-passthrough
- callback alias / delimiter alias chain variants of that split family
- trailing static print suffixes after `each`
- **transformed split-callback family**: `|line| -> println "${line}!"` with static
  prefix/suffix decorations (promoted from InterpreterBridge)

InterpreterBridge:

- **effectively empty**: the transformed split-callback family was the only
  bridge-only shape and has been promoted to DynamicWasiIo.
- The bridge path remains in the code but currently has no program shapes that
  route through it.

Unsupported:

- runtime-read transforms outside the DynamicWasiIo subset
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
- `execute_module_with_stdin` is temporary and only valid for `InterpreterBridge`
  (currently no programs route through it).
- `InterpreterBridge` detection has been removed from `classify_runtime_io`;
  the variant and CLI fallback arm are retained as extension points only.
- `goby run` follows planner classification, not codegen error text.
- stdlib `goby/string.graphemes` is implemented.
- `fetch_env_var` reads the **compiler-process** environment at compile time for
  direct-style and effect-boundary programs (both paths bake the value into emitted
  output before Wasm is finalized).  See `TODO(F-sweep)` in `lower.rs` for the
  open question on whether this should be a documented user guarantee or restricted.

## Next Slice

Track F remaining work (Phase F5):

1. Document the execution contract around `DynamicWasiIo` vs `StaticOutput` vs
   `InterpreterBridge` in `doc/LANGUAGE_SPEC.md` or inline in code.
2. Sweep for compile-time fallback call sites that could observe host environment
   during compilation; encode findings as tests or explicit TODOs.
3. Consider whether `belongs_on_interpreter_bridge` / `split_lines_each_bridge_plan`
   should be removed or retained as dead-code scaffolding for future shapes.
4. Update runnable stdin examples such as `examples/read.gb` if needed.

Constraints:

- do not add planner-bypassing runtime-I/O branching in `crates/goby-wasm/src/lib.rs`
- prefer extending `RuntimeIoPlan` / backend ownership rather than adding one-off matcher hacks
