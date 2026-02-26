# Goby Project Instructions

This document guides AI assistants working on the Goby programming language.
For the evolving language draft and open decisions, see [doc/PLAN.md](./doc/PLAN.md).

## Project Overview

Goby is a statically typed, immutable-first functional language with an effect system.
The first backend target is Wasm.

Target direction:

- Keep readability and simplicity as a core language value.
- Add practical expressiveness only where it clearly improves developer ergonomics.
- Build a strong toolchain from early stages.

**Important**: Goby is in an early design/prototyping stage. Breaking changes are acceptable if they improve language clarity and consistency.

## Design Principles

1. **Readability First**: Syntax should be easy to read and reason about.
2. **Functional Core**: Prefer pure functions and immutable data by default.
3. **Typed Safety**: Maintain strong static typing with clear diagnostics.
4. **Practical Effects**: Effect tracking should be explicit enough to be useful, without overwhelming users.
5. **Tooling as Product**: Compiler, formatter, diagnostics, and developer workflow are first-class concerns.
6. **Balanced Language Positioning**: If trade-offs appear, favor designs that preserve clarity and predictability while allowing practical expressiveness where it provides clear user value.

## Goby Syntax Quick Reference (Current Draft)

The current draft is based on files in `examples/` and `doc/PLAN.md`.

- Source files use `.gb`.
- Function definitions use `=`.
- Type annotations use `:` and `->`.
- Statements are separated by newline or `;`.
- Function-local scopes and blocks are indentation-based (spaces or tabs).
- Block expressions return the last expression value.
- Effects are declared with `can <Effect>` (example: `can Print`).
- Anonymous functions support `|x| -> expr` and shorthand forms like `_ * 10`.

## Full Reference Policy

`doc/PLAN.md` is the active design reference.

**Instruction**: When changing language syntax, semantics, or core terminology:

- update `doc/PLAN.md`,
- update or add relevant examples in `examples/`,
- keep `README.md` user-facing (high-level only, not detailed spec text).

## Project Structure

- `crates/goby-core`: core language implementation
- `crates/goby-cli`: CLI entrypoint and command wiring
- `crates/goby-wasm`: Wasm backend
- `examples/`: Goby sample programs (`.gb`)
- `doc/`: language planning and design notes

## Build and Test Commands

Use Cargo commands from repository root:

- `cargo check`
- `cargo test`
- `cargo run -p goby-cli -- run <file.gb>`
- `cargo fmt`
- `cargo clippy -- -D warnings`

When making non-trivial changes, run at least `cargo check` and `cargo test`.

## Coding Standards

### Rust (compiler/tooling code)

- Prefer clear, explicit code over clever abstractions.
- Keep modules small and responsibilities focused.
- Avoid panics in normal compiler flows; return structured errors.
- Use enums for AST/errors where possible.

### Formatting

- Use `cargo fmt` for formatting.
- Keep naming idiomatic Rust (`snake_case` for functions/modules, `CamelCase` for types).

### Documentation

- Document externally visible behavior and non-obvious internals.
- If parser/typechecker behavior changes, add or update examples that demonstrate the behavior.

### Testing

- Add unit tests for lexer/parser/typechecker behavior as they are introduced.
- Add regression tests for previously failing language cases.
- Prefer small, focused tests tied to a single behavior.

## Change Workflow Expectations

For language-facing changes:

1. Update `doc/PLAN.md` (spec intent and status).
2. Update `examples/` (concrete syntax/behavior).
3. Update implementation in `crates/`.
4. Run formatting and tests.

For restart safety:

1. Treat `doc/STATE.md` as a save-point document.
2. At meaningful milestones (or before stopping work), update `doc/STATE.md`
   with:
   - newly locked decisions,
   - open questions,
   - immediate next steps.
3. Keep `doc/STATE.md` concise and execution-oriented so work can resume after
   context reset without ambiguity.

If there is tension between approaches, choose the option that keeps the language easy to read and reason about while still enabling practical, high-value expressiveness.
