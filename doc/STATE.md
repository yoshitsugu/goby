# Goby Project State Snapshot

Last updated: 2026-03-15

## Current Focus

- `can` semantics alignment: **completed** (2026-03-15)
- Next track: **Track D — Developer Tooling Foundation** (formatter, linter, LSP)

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

Track D — Developer Tooling Foundation is the next starting point.

Near-term scope:

1. `goby fmt` / `goby fmt --check` (deterministic formatting).
2. `goby lint` (high-signal checks + machine-readable output).
3. `goby-lsp` MVP (diagnostics, hover, definition).
4. Cross-editor syntax regression tests for existing highlight packs.
