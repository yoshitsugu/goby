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

- Indentation mixing rule details:
  - behavior when tabs and spaces are mixed in one logical block is not fully specified.
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

1. Lock indentation mixing behavior in `doc/PLAN.md`.
2. Lock Wasm execution approach in `MVP.md`.
3. Implement initial Wasm codegen path in `crates/goby-wasm`.
4. Replace current parse+typecheck CLI flow with typecheck + codegen + run pipeline.

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
