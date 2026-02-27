# Goby Project State Snapshot

Last updated: 2026-02-27

This file is a restart-safe snapshot for resuming work after context reset.

## 1. Current Architecture

- Rust Cargo workspace (root `Cargo.toml`).
- Crates:
  - `crates/goby-core` (language core: AST/parser/typechecker/IR).
  - `crates/goby-cli` (CLI entrypoint).
  - `crates/goby-wasm` (Wasm backend).

## 2. Locked MVP Decisions

- First backend target is Wasm.
- Effect system is parse-only in MVP.
- Entry function is `main` only.
- `main` type is `void -> void`.
- CLI commands:
  - `run`: parse + typecheck + requires `main`
  - `check`: parse + typecheck (no runtime entry requirement)
- Statement separator is newline or `;`.
- Indentation-based blocks:
  - tabs and spaces are both accepted.
- Function calls support both `f x` and `f(x)`.
- MVP built-ins:
  - `print`
  - `string.concat`
- `map` is out of MVP scope.
- `examples/basic_types.gb` is parse/typecheck target only (not runtime entry target).

## 3. Known Open Decisions

- Wasm runtime path:
  - define how generated Wasm is executed in MVP (`wasmtime` vs embedded runtime strategy).
- Assignment/binding semantics:
  - exact rule for block-local `a = ...` still needs a formal spec entry.
- Operator precedence/associativity table:
  - still not frozen.

## 4. Existing Documents and Their Roles

- `README.md`: user-facing project overview.
- `AGENTS.md`: contributor/agent workflow and coding instructions.
- `MVP.md`: implementation plan and acceptance criteria.
- `doc/PLAN.md`: evolving language plan (locked + open items).

## 5. Current Example Files

- `examples/hello.gb`
- `examples/basic_types.gb`
- `examples/function.gb`

## 6. Immediate Next Steps (Execution Order)

1. Lock Wasm execution approach in `MVP.md`.
2. Replace current wasm-file emission in `run` with actual execution pipeline.
3. Add expression-level lowering from Goby AST/body into Wasm function bodies.

## 7. Resume Commands

- Check workspace build:
  - `cargo check`
- Run tests:
  - `cargo test`
- Current CLI run shape:
  - `cargo run -p goby-cli -- run examples/hello.gb`

## 8. Progress Since Scaffold

- Implemented minimal parser foundation in `crates/goby-core`:
  - `ast.rs` and `parser.rs`
  - top-level declaration parsing for current examples
- Added parser tests:
  - parses `examples/hello.gb`
  - parses `examples/basic_types.gb`
- Implemented initial CLI parse flow in `crates/goby-cli`:
  - loads source file
  - parses with `goby_core::parse_module`
  - reports declaration count
- Verified with:
  - `cargo test`
  - `cargo run -p goby-cli -- run examples/hello.gb`
  - `cargo run -p goby-cli -- check examples/basic_types.gb`

## 9. Progress Since Minimal Parser

- Added minimal typechecking in `crates/goby-core`:
  - `types.rs`: basic function type parsing (`->`) and effect suffix stripping (`can ...`)
  - `typecheck.rs`: duplicate declaration check and `main : void -> void` MVP check
- Updated CLI flow in `crates/goby-cli`:
  - parse -> typecheck -> success output
- Added/updated tests:
  - function type parsing with effect annotation
  - example-level typecheck for `hello.gb` and `basic_types.gb`

## 10. Progress Since Minimal Typecheck

- Updated `goby-cli` command contract:
  - `run <file.gb>` now requires `main`
  - `check <file.gb>` added for parse/typecheck-only workflows
- This aligns CLI behavior with MVP intent:
  - `hello.gb` is a run target
  - `basic_types.gb` is a check target

## 11. Progress Since Initial CLI Contract Hardening

- Added minimal Wasm codegen in `crates/goby-wasm`:
  - `compile_module` emits a valid Wasm module exporting `main`
  - returns error when `main` is missing
- Updated `goby-cli run`:
  - parse + typecheck + codegen
  - writes output wasm file next to input (`<name>.wasm`)
- Added wasm backend unit test:
  - validates emitted module magic header

## 12. Progress Since Initial Wasm Emission

- Refactored `goby-cli` internals for maintainability:
  - separated argument parsing (`parse_args`) from execution (`run`)
  - unified error handling via `CliError` (usage vs runtime)
  - preserved command behavior (`run` / `check`)

## 13. Progress Since CLI Internal Refactor

- Improved `goby-cli` testability:
  - extracted iterator-based `parse_args_from` for unit testing
  - added CLI argument parser unit tests (run/unknown/extra cases)
- Improved `goby-wasm` maintainability:
  - changed minimal wasm bytes to a named constant slice
  - simplified module byte materialization with `to_vec()`

## 14. Progress Since CLI Refactor Step 2

- Added minimal execution phase to `goby-cli run`:
  - after wasm emission, attempts `wasmtime run --invoke main <file.wasm>`
  - if `wasmtime` is unavailable, prints a skip message and exits successfully
- Added CLI unit test for wasm output path computation

## 15. Progress Since CLI Refactor Step 3

- Refined `wasmtime` execution behavior in `goby-cli run`:
  - switched from `output()` to `status()` so runtime stdout/stderr is not swallowed
  - improved failure diagnostics with explicit exit status reporting
  - factored availability check into `is_wasmtime_available`
- Expanded CLI arg parser tests:
  - missing command
  - missing file

## 16. Progress Since CLI Refactor Step 4

- Simplified wasm execution control flow in `goby-cli`:
  - replaced string-sentinel error branching with explicit `ExecutionOutcome`
- `execute_wasm` now returns:
  - `Executed`
  - `SkippedNoWasmtime` (when `wasmtime` is not installed)

## 17. Progress Since Wasm Execution Refactor

- Added first expression-aware Wasm lowering path in `goby-wasm`:
  - when `main` body is `print "..."`, emits a WASI `fd_write`-based module via WAT
  - otherwise falls back to minimal empty-main module
- Added new wasm backend test for print-literal codegen.
- Added `wat` dependency in `goby-wasm` and updated lockfile.

## 18. Progress Since Wasm Lowering Refactor

- Fixed memory layout safety in print-literal Wasm generation:
  - moved static write buffers to non-overlapping offsets (`iovec`, `nwritten`, text)
  - removed potential overlap for longer print strings
- Added bounds-safe string-length conversion (`usize` -> `u32`) with explicit error.
- Added regression test for long print literals.

## 19. Progress Since Wasm Lowering Refactor Step 2

- Improved wasm backend maintainability by removing print-lowering magic numbers:
  - extracted static offsets into named constants (`IOVEC_OFFSET`, `NWRITTEN_OFFSET`, `TEXT_OFFSET`)
  - extracted iovec serialization into `encode_iovec`
- Added focused unit test for iovec little-endian encoding.

## 20. Progress Since Wasm Lowering Step 3

- Added shared print-body analysis in `goby-core` (`analysis.rs`):
  - resolves `print "..."` literals
  - resolves `print <identifier>` when identifier is a local string binding
  - reports invalid print argument forms
- Updated `typecheck` to validate print argument string-ness using shared analysis.
- Updated `goby-wasm` to reuse shared analysis instead of custom print parsing.
- Added `examples/print_local.gb` for local-binding print flow.

## 21. Progress Since Shared Analysis Refactor

- Strengthened print analysis correctness:
  - no longer stops at first print line
  - validates all lines and rejects unsupported multiple-print bodies
- Added regression test for multiple print-call rejection.

## 22. Progress Since Shared Analysis Refactor Step 2

- Hardened assignment detection in print analysis:
  - `=` is treated as binding only when it is not part of `==`
  - prevents accidental misclassification of equality-like expressions
- Added regression test to ensure equality-like lines are not interpreted as local bindings.

## 23. Progress Since Shared Analysis Refactor Step 3

- Extended string expression resolution for print analysis:
  - supports `string.concat(<string-expr>, <string-expr>)`
  - supports nested concat and local string bindings
- Added new sample:
  - `examples/print_concat.gb`
- Added analysis tests for concat-based print resolution.

## 24. Progress Since Shared Analysis Refactor Step 4

- Refactored `analysis.rs` for maintainability:
  - extracted local-binding update logic
  - extracted print-argument resolution logic
  - centralized error message constants for print analysis
- Kept behavior equivalent while improving readability and future edit safety.

## 25. Progress Since Shared Analysis Refactor Step 5

- Improved print-call parsing robustness:
  - accepts whitespace after `print` (including tabs), not only a single space
- Made comment handling explicit in body analysis:
  - lines beginning with `#` are skipped intentionally
- Added regression tests for tab-delimited print syntax and inline comment skipping.

## 26. Progress Since Shared Analysis Refactor Step 6

- Locked indentation mixing behavior for MVP in `doc/PLAN.md`:
  - any leading space/tab marks an indented line
  - mixed tabs/spaces in one block are accepted in MVP
- Added parser regression test to ensure mixed-indentation block lines parse as one declaration body.
