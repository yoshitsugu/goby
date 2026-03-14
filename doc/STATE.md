# Goby Project State Snapshot

Last updated: 2026-03-14 (F3c complete + string.graphemes)

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- Planning slice recorded for Track D developer tooling.
- `doc/PLAN.md` now contains a phased `goby-lsp` implementation plan under Active Track D.
- `doc/PLAN.md` also records Active Track E for `Float` support backed by Wasm `f64`.
- Stdlib `goby/string` now exports `graphemes : String -> List String`,
  implemented by reusing the existing grapheme intrinsic path instead of adding a
  separate runtime builtin.
- Track F Phase F4 deterministic coverage has started:
  runtime/env tests now pin `read` exhaustion and mixed `read_line` + `read`
  semantics, and runtime-output/bridge tests pin seeded-stdin behavior including
  EOF handling on repeated reads.
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
- Echo-style dynamic Wasm lowering now also covers trailing static literal output,
  so shapes like `println(read_line()); print "done"` stay on the dynamic Wasm path.
- F3b boundary work is now complete:
  runtime-read programs outside known dynamic-Wasm plans and the remaining narrow
  bridge subset are classified as `Unsupported` with explicit user-facing errors.
- F3c backend cleanup is now complete:
  runtime-I/O Wasm emission now shares backend-owned memory planning, module
  skeleton setup, and stdout helper building blocks across echo/read-line/split paths.
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
- `RuntimeIoPlan` newline-splitting `each println` planning now also accepts a
  one-hop forwarding of the `split` result, so `copied = lines; each copied ...`
  compiles to dynamic Wasm and the bridge-only regression path now requires a
  more complex two-hop variant.
- The public runtime-I/O execution-kind query is now owned by the planner module
  and re-exported from `goby-wasm`, so classification policy lives with the
  runtime-I/O planner rather than in `lib.rs`.
- The remaining runtime-I/O bridge policy in `execute_module_with_stdin` and the
  compile-time stdin-blocking decision are now also expressed through planner-owned
  classification helpers instead of ad-hoc matches in `lib.rs`.
- `RuntimeIoPlan` matching now factors read-binding, alias-binding, delimiter-binding,
  and split-binding detection through small helpers, reducing duplicated AST-shape
  logic across the echo and split planning paths.
- Echo and newline-splitting runtime-I/O planning now follow simple local alias chains
  instead of stopping after a single forwarded binding, while bridge-only regression
  coverage has moved to nearby unsupported callback-body shapes such as `each ... print`.
- Echo runtime-I/O planning can now also carry trailing static literal
  `print` / `println` suffixes after the dynamic read echo step, so nearby
  shapes like `print(read()); println "done"` no longer need the interpreter bridge.
- Echo runtime-I/O planning now also resolves local `print` / `println` aliases,
  including simple forwarded aliases, so shapes like
  `printer = print; text = read(); printer text` also compile to dynamic Wasm.
- `classify_runtime_io` no longer treats every unmatched read-program as
  `InterpreterBridge`; unmatched runtime-read shapes now split into:
  - the intentionally narrow remaining bridge subset
    (`read` + newline `split` + transformed `each` callback family),
  - `Unsupported` for other runtime-read shapes with no current dynamic lowering.
- `goby-core` now resolves workspace-root-relative `examples/` and `stdlib/`
  paths through a shared helper instead of assuming a fixed checkout path,
  which restores full `cargo test` stability after repo relocation.
- The newline-splitting runtime-I/O planner/back-end path now models the callback output
  mode directly, so `each lines print` and `each lines println` both compile to dynamic
  Wasm, including the named-function callback spelling in addition to the direct lambda form.
- That same split-callback planner path now also accepts the equivalent passthrough
  interpolation spelling `|line| -> println "${line}"`, while still leaving genuinely
  transformed output shapes such as `|line| -> println "${line}!"` on the temporary bridge.
- Split-callback planning now also resolves local output-function aliases such as
  `printer = println; each lines printer` and simple forwarded forms like
  `writer = printer; each lines writer`, so those cases no longer depend on the bridge.
- The same callback alias resolution now also covers equivalent lambda-call spellings like
  `each lines (|line| -> printer line)`, including simple forwarded aliases, so planner
  coverage follows the named-function-reference direction rather than only bare callback vars.
- Split planning now scans the binding sequence around `split(...)` instead of requiring a
  fixed `read -> delim -> split` layout, so nearby input-side forms like delimiter alias chains
  (`newline = "\n"; delim = newline`) and callback aliases placed before the `split` binding
  also compile to dynamic Wasm.
- `SplitLinesEach` runtime-I/O planning can now carry trailing static literal
  `print` / `println` suffixes after the `each` step, so sample-shaped programs like
  `...; each lines println; println "test"; print "done"` no longer have to stay on
  the interpreter bridge just because they append fixed output afterward.
- Track F Phase F2b and F2c are now complete:
  `StaticOutput(String)` and `Unsupported` are now first-class `RuntimeIoClassification`
  and `RuntimeIoExecutionKind` variants. `plan_static_output` detects programs whose every
  statement is a `print`/`println` call with a string-literal argument and routes them
  through `compile_module_wasm_or_error` directly. The fallback `resolve_main_runtime_output_for_compile`
  is retained for more complex static-output programs (variable args, arithmetic, etc.)
  that require interpreter evaluation. `Unsupported` is a placeholder variant whose
  assignment is deferred to F3b.
- Track F Phase F2c is complete: the bridge boundary is clarified via doc comments and
  integration tests. `execute_module_with_stdin` is doc-commented as "temporary bridge,
  CLI-only, marked for shrinkage". `classify_runtime_io` now has explicit variant boundary
  documentation. `Unsupported` assignment in `classify_runtime_io` deferred to F3b.
- Track F Phase F3a is complete: the exit condition was already met by prior commits
  (no direct AST-shape conditionals in `lib.rs`). Stopping rule documented in
  `classify_runtime_io` doc comment. Five `DynamicWasiIo` round-trip tests added
  (classification + valid Wasm) covering all four Echo combinations and `SplitLinesEach`.
- Track F Phase F3b is complete:
  - echo-path planner coverage expanded to suffix-output and local output-alias forms.
  - `Unsupported` is now assigned by `classify_runtime_io` for runtime-read programs
    outside both dynamic lowering and the narrow temporary bridge subset.
  - compile/bridge entrypoints now surface explicit `Unsupported` diagnostics instead
    of silently falling through to compile-time fallback or generic bridge errors.
- Track F Phase F3c is complete:
  - `backend.rs` now owns shared runtime-I/O memory planning and WASI module setup helpers.
  - echo/read-line/split-lines lowering paths reuse common stdin/stdout instruction helpers.
  - planning/lowering/caller ownership is cleaner without changing planner policy.

## Verified

- `cargo fmt`
- `cargo check`
- `cargo test -p goby-wasm`
- `cargo test`

## Next Work

- Continue Track F by expanding `RuntimeIoPlan` expressiveness, not by adding new
  planner-bypassing runtime-I/O branches in `crates/goby-wasm/src/lib.rs`.
- Track explicit shrinkage of temporary bridge coverage as matching dynamic lowerings land.
- Phase F4 deterministic tests
  expand boundary and runtime-I/O classification coverage around the now-explicit
  `DynamicWasiIo` / `InterpreterBridge` / `Unsupported` split.
- Keep Track D queued after the runtime I/O containment/runtime split work is in a stable state.
- Once Track F runtime support lands, sync `doc/LANGUAGE_SPEC.md` and runnable stdin examples.

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
