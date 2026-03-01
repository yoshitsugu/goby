# Goby ꒰( ˙𐃷˙ )꒱

Goby is a new programming language project with a clear, practical goal:
build a readable functional language with a modern developer experience.

## Disclaimer

This project is an early-stage hobby POC.

- Goby is both a language experiment and an AI-assisted development experiment.
- Most implementation work is done with AI assistance.
- The current goal is exploration and learning, not production-grade polish.
- Open decisions and active direction are tracked in `doc/PLAN.md`.

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
- `examples/`: small sample programs (see `examples/README.md`)
- `doc/PLAN.md`: language design notes, draft spec details, and open decisions
- `doc/STATE.md`: restart-safe snapshot of current decisions and next steps

## Language Specification

Detailed and evolving language rules are intentionally documented in
`doc/PLAN.md` instead of this README.

## License

To match the project goals, licensing is split by artifact type:

- **Code**: permissive (free to use, modify, and redistribute) under **MIT-0**.
- **Language concept/spec text and design docs**: **CC BY-NC-ND 4.0**.
- See `LICENSE-CODE` and `LICENSE-DOCS` for details.

Notes:

- This split is intentional: code reuse is encouraged, while direct copy/republish of
  concept/spec documents is restricted.
- Copyright protects concrete expression (text/code), not abstract ideas.
