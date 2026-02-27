# Goby MVP Implementation Plan

This MVP focuses on running `examples/hello.gb` and `examples/basic_types.gb`
without implementing a full effect system.

## 1. MVP Goal

Build a minimal end-to-end Goby pipeline that can:

- parse `.gb` source files,
- typecheck core syntax and basic types,
- execute `main` when present,
- compile to Wasm as the first backend target,
- run `hello.gb` successfully and typecheck `basic_types.gb`.

## 2. Scope

### In Scope

- File loading for a single `.gb` file.
- Lexer + parser for currently used syntax:
  - top-level function/value declarations,
  - type annotations with `:` and `->`,
  - function definitions with `=`,
  - statement separation with newline or `;`,
  - indentation-based blocks,
  - indentation is recognized from contiguous tabs or contiguous spaces,
  - basic expressions (literals, identifiers, binary `+`, function calls),
  - function calls in both styles: `f x` and `f(x)`,
  - tuples `(a, b)`.
- Basic type checker:
  - `Int`, `String`, `void`,
  - function types,
  - tuple types,
  - simple type inference for local bindings.
- Minimal Wasm backend:
  - lower typed AST to a small Wasm-oriented IR,
  - emit runnable Wasm module for `main`,
  - support integer addition and string handling needed by examples.
- Built-ins required by examples:
  - `print`,
  - `string.concat`.

### Out of Scope (for this MVP)

- Effect checking/inference (no full effect system yet).
- Pattern matching, ADTs, records, modules, packages.
- Optimizations and additional backends beyond Wasm.
- Advanced diagnostics and LSP.

## 3. Effect Handling Strategy for MVP

Since examples include `can Print`, the parser should accept effect annotations,
but the checker/runtime should treat them as metadata only for now.

Policy:

- Parse and keep effect annotation nodes in AST.
- Do not perform effect validation in MVP.
- Ignore unknown effect names in MVP.

This keeps source compatibility while postponing the full effect system.

## 4. MVP Locked Decisions

- Function call style: both `f x` and `f(x)` are valid.
  - Use `f(x)` when precedence needs to be explicit.
- Type annotation syntax is unified to `name : Type` (not `name = Type`).
- Indentation:
  - both tabs and spaces are accepted,
  - contiguous tabs or contiguous spaces count as indentation.
- Runtime entrypoint:
  - `goby-cli run <file.gb>` uses `main` by default,
  - `main` type is restricted to `void -> void` in MVP.
- CLI commands:
  - `goby-cli run <file.gb>`: parse + typecheck + requires `main`
  - `goby-cli check <file.gb>`: parse + typecheck only
- Backend:
  - Wasm is the first backend target for MVP.
- Effect system handling in MVP:
  - parse-only for `can ...`,
  - no effect checking,
  - unknown effects are ignored.
- Built-ins in MVP:
  - `print`,
  - `string.concat`,
  - `map` is out of scope.
- `examples/basic_types.gb`:
  - no `main` required,
  - no `--entry` option in MVP,
  - behavior is verified through parser/typechecker and dedicated tests.

## 5. Milestones

### M1: Frontend Skeleton

- Define AST and source span model.
- Implement lexer and parser for the MVP grammar subset.
- Add parser snapshot/unit tests for `hello.gb` and `basic_types.gb`.

Exit criteria:

- Both files parse into AST without errors.

### M2: Type Checking Core

- Implement symbol tables and type environments.
- Validate function signatures and expression types.
- Support local binding inference inside blocks.
- Handle tuple and function types used by examples.

Exit criteria:

- `hello.gb` and `basic_types.gb` pass type checking.

### M3: Wasm Backend Core

- Implement typed IR -> Wasm lowering for MVP subset.
- Generate runnable Wasm module with `main` entry.
- Add built-ins: `print`, `string.concat`.

Exit criteria:

- `cargo run -p goby-cli -- run examples/hello.gb` compiles to Wasm and runs `main`.
- `main` must typecheck as `void -> void`.

### M4: CLI and Reliability

- CLI shape:
  - `goby-cli run <file.gb>`
  - `goby-cli check <file.gb>`
- Add clear parse/type/codegen/runtime error categories.
- Add regression tests for current examples.

Exit criteria:

- One-command run path works for `hello.gb`.
- `basic_types.gb` is covered by parse/typecheck tests.
- CI-level command (`cargo test`) is green.

## 6. Suggested Implementation Order (Codebase)

1. `crates/goby-core/src/ast.rs`
2. `crates/goby-core/src/lexer.rs`
3. `crates/goby-core/src/parser.rs`
4. `crates/goby-core/src/types.rs` + `crates/goby-core/src/typecheck.rs`
5. `crates/goby-wasm/src/` (IR lowering + codegen modules)
6. `crates/goby-core/src/builtins.rs`
7. `crates/goby-cli/src/main.rs` (CLI wiring)

## 7. Acceptance Criteria

- `examples/hello.gb` runs and prints expected output.
- `examples/basic_types.gb` parses and typechecks.
- Effect annotations are accepted syntactically but not enforced semantically.
- New contributors can run/verify the MVP path with:
  - `cargo run -p goby-cli -- run examples/hello.gb`
  - `cargo run -p goby-cli -- check examples/basic_types.gb`
  - `cargo test`

## 8. Immediate Next Tasks

1. Reflect locked MVP grammar and runtime decisions in `doc/PLAN.md`.
2. Implement parser first, before checker/codegen.
3. Add golden tests based on current `examples/*.gb`.
