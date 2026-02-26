# Goby (Working Title)

Goby is a new programming language project with a clear, practical goal:
build a readable functional language with a modern developer experience.

## Language Concept

- Functional-first language design.
- All data is immutable by default.
- Built-in effect system.
- Statically typed.
- Prioritize readability over advanced or highly complex language features.
- Strong tooling is a core part of the project vision.
- Built with deep respect for the ideas and communities behind Gleam and Ante.

## Repository Layout

- `crates/goby-core`: core language implementation (AST, parser, type checker, IR)
- `crates/goby-cli`: command-line interface
- `crates/goby-wasm`: Wasm backend
- `examples/`: small sample programs
- `doc/PLAN.md`: language design notes, draft spec details, and open decisions
- `doc/STATE.md`: restart-safe snapshot of current decisions and next steps

## Language Specification

Detailed and evolving language rules are intentionally documented in
`doc/PLAN.md` instead of this README.

## Current Status

The project is in early design and prototyping.
The first backend target is Wasm.
Open decisions and next steps are tracked in `doc/PLAN.md`.
