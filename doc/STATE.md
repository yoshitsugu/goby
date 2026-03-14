# Goby Project State Snapshot

Last updated: 2026-03-14

## Current Focus

- Active track: Track F — **Phase F5 complete**
- Track F is now closed; all acceptance criteria met
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

## Phase F5 Completed Work

1. Removed dead bridge detection code (`belongs_on_interpreter_bridge` and related
   functions) from `runtime_io_plan.rs`; `InterpreterBridge` variant retained as
   extension point with NOTE comments in CLI and `execute_module_with_stdin`.
2. Added execution runtime requirements to `LANGUAGE_SPEC.md` section 7: WASI Preview 1
   requirement, stdin not available at compile time, Print-only vs Read program behavior.
3. Strengthened doc comments in `classify_runtime_io`: safety contract, Print is
   compile-time-safe, Read is runtime-host-dependent, WASI Preview 1 minimum runtime.
4. Compile-time host-observation sweep: `Read` guarded by `allow_live_stdin=false`;
   `fetch_env_var` reads compiler-process env at compile time (see `TODO(F-sweep)` in
   `lower.rs` for open policy question).
5. Updated `examples/read.gb` with a comment explaining current runability status.

## Next Slice

Track F is complete. Next active track: Track D (Developer Tooling Foundation) or
Track E (`Float` / Wasm `f64` support).

Constraints carried forward:

- do not add planner-bypassing runtime-I/O branching in `crates/goby-wasm/src/lib.rs`
- prefer extending `RuntimeIoPlan` / backend ownership rather than adding one-off matcher hacks
