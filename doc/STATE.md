# Goby Project State Snapshot

Last updated: 2026-03-13

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- Planning slice recorded for Track D developer tooling.
- `doc/PLAN.md` now contains a phased `goby-lsp` implementation plan under Active Track D.
- `doc/PLAN.md` also records Active Track E for `Float` support backed by Wasm `f64`.
- Higher-order named function references are now documented explicitly
  (for example: `map xs add_ten`).
- Track F Phase F1 containment is now implemented:
  compile-time fallback no longer consumes stdin for `Read` programs during `goby run`.
- Track F capability split has advanced:
  `goby run` now executes stdin-backed `Read` programs through the runtime resolver
  instead of failing once compile-time fallback is blocked.
- Track F dynamic Wasm work has started:
  the exact `print(read())` shape now compiles to a WASI Wasm module that imports
  `fd_read` and `fd_write`.
- The same dynamic Wasm path now also covers close variants:
  `println(read())` and `text = read(); print/println text`.
- The first `read_line()`-based dynamic Wasm shapes now work too:
  `print(read_line())` and `line = read_line(); println line`.
- The original `read` + `split(text, "\n")` + `each ... println`
  sample shape now also compiles to dynamic WASI Wasm instead of requiring the
  interpreter-backed runtime bridge.
- That structured stdin Wasm shape now also accepts a nearby spelling where the
  newline delimiter is first bound to a local variable before calling `split`.
- Runtime-I/O shape detection now lives in a dedicated `goby-wasm` planning module
  instead of staying inline in the top-level `lib.rs` orchestration path.
- That planning module now explicitly classifies current `main` shapes into
  dynamic-Wasm candidates, temporary interpreter-bridge cases, or non-runtime-I/O code.
- CLI `run` now uses that runtime-I/O classification instead of matching a codegen
  diagnostic string to decide when to execute the temporary stdin-backed bridge.

## Current State

- Compatibility cleanup backlog has been closed and removed from `doc/PLAN.md`.
- Next actionable tooling sequence is:
  - machine-readable diagnostic/span hardening in `goby-core`,
  - `crates/goby-lsp` workspace scaffold,
  - LSP diagnostics/hover/definition MVP.
- Numeric-type expansion is now captured as a separate follow-on track:
  - lock `Float` literal/coercion/equality semantics,
  - implement parser/typechecker/runtime/Wasm support,
  - sync docs/examples/tooling once behavior lands.
- Current language docs/examples/tests now make explicit that named functions can be
  passed directly where a function-typed argument is expected.
- `goby-wasm::compile_module` now runs compile-time fallback output resolution in a
  "no live stdin" mode and surfaces an explicit codegen error if `Read.read` or
  `Read.read_line` would consume compiler-process stdin.
- `goby-wasm` now exposes a runtime execution path with seeded stdin so the CLI can
  execute `Read` programs without compile-time stdin capture.
- `goby-core` parser now lowers parenthesized multi-arg calls like `split(a, b)` into
  the same left-associative call shape as spaced calls, which unblocks runtime handling
  of selective-import stdlib helpers in this path.
- CLI and Wasm regression tests now cover both containment and runtime stdin execution
  for the `read` + `split` + `each` shape.
- `goby-wasm` backend now has a first dynamic stdin/stdout Wasm path for the simple
  read-all echo case (`print(read())`), while more complex `Read` programs still use
  the interpreter-backed runtime execution bridge.
- That dynamic path is now generalized across a few equivalent output spellings, but it
  still only handles direct echo-style `read` / `read_line` output shapes plus the
  newline-splitting `each println` family in a narrow set of local-binding forms.
- Runtime-I/O planning ownership has moved into a dedicated `goby-wasm` module, and
  dynamic runtime-I/O Wasm emission now follows that same ownership boundary instead
  of being re-mapped from planner cases inside `crates/goby-wasm/src/lib.rs`.
- `compile_module` now consults the runtime-I/O classifier before compile-time fallback,
  so bridge cases are rejected by planning rather than only by fallback execution.
- `goby-wasm` now exposes a small public runtime-I/O execution-kind query so callers can
  align execution-path decisions with planner output instead of error-message text.
- compile-time fallback now receives structured runtime-resolution results internally,
  so `compile_module` no longer inspects formatted `"runtime error: ..."` text to detect
  stdin-related bridge cases.
- `execute_module_with_stdin` is now explicitly limited to interpreter-bridge programs,
  so dynamic-Wasm shapes are no longer silently executable through the temporary bridge.
- Wasm smoke/regression coverage now separates the two runtime-I/O paths explicitly:
  bridge-only stdin execution is tested with a planner-classified bridge shape, while
  dynamic-Wasm shapes are checked to reject the temporary interpreter bridge entrypoint.
- `RuntimeIoPlan` now owns both classification and dynamic-Wasm emission for the
  currently supported stdin/stdout runtime-I/O cases.
- `RuntimeIoPlan` echo planning now also accepts a one-hop local forwarding shape,
  so `text = read(); copied = text; print copied` and the analogous `read_line`
  variant no longer fall back to the temporary interpreter bridge.

## Verified

- `cargo fmt`
- `cargo check`
- `cargo test`

## Next Work

- Continue Track F toward dynamic Wasm/WASI stdin support:
  extend the new `fd_read`/`fd_write` backend work beyond the simple echo shape so
  more `Read` programs stop depending on the interpreter-backed runtime execution path.
- Likely next backend slice:
  widen the new structured stdin shape support beyond the exact `split(..., "\n")`
  + `each println` pattern so nearby variants do not immediately fall back to the
  interpreter runtime.
- Decide whether the new runtime execution path should become an explicit internal API
  boundary before dynamic Wasm support lands, or remain a temporary CLI-only bridge.
- Continue Track F by expanding `RuntimeIoPlan` expressiveness, not by adding new
  planner-bypassing runtime-I/O branches in `crates/goby-wasm/src/lib.rs`.
- Keep Track D queued after the runtime I/O containment/runtime split work is in a stable state.
- Once Track F runtime support lands, sync `doc/LANGUAGE_SPEC.md` and runnable stdin examples.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
