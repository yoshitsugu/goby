# Goby ꒰( ˙𐃷˙ )꒱

Goby is a new programming language project with a clear, practical goal:
build a readable functional language with a modern developer experience.

## Disclaimer

This project is an early-stage hobby POC.

- Goby is both a language experiment and an AI-assisted development experiment.
- Most implementation work is done with AI assistance.
- The current goal is exploration and learning, not production-grade polish.
- Latest language spec is tracked in `doc/LANGUAGE_SPEC.md`.
- Open decisions and active direction are tracked in `doc/PLAN.md`.

## Getting Started

Clone the repository:

```bash
git clone https://github.com/yoshitsugu/goby
cd goby
```

Install the Goby CLI (`goby`) with Cargo:

```bash
cargo install --path crates/goby-cli
```

Create a minimal program:

```bash
cat > hello.gb <<'EOF'
main : Unit -> Unit
main = print "Hello Goby"
EOF
```

Run it:

```bash
goby run hello.gb
```

If `wasmtime` is not installed, install it first so `goby run` can execute generated Wasm.

## Language Concept

- Functional-first language design.
- All data is immutable by default.
- `List` is the default ordered collection surface. The runtime is free to use
  a sequence-backed representation, and the intended practical operations for
  ordinary scripts are indexed read (`xs[i]` / `list.get`), immutable point
  update (`list.set`), callback-style traversal (`list.each`), and list
  pattern matching (`[]`, `[x, ..rest]`, exact-length forms).
- Built-in effect system.
- Statically typed.
- On the documented compiled Wasm path, Goby now has generic TCO for
  statically resolvable direct tail calls among known top-level declarations.
  The current guarantee includes self recursion, sibling/mutual direct calls,
  supported tail control-flow joins, and statically resolvable local aliases;
  indirect/higher-order calls and non-tail recursion remain outside the
  guarantee.
- Wasm-first implementation direction: the current backend target is Wasm, and
  `goby run` is intended to execute generated Wasm rather than a separate
  language-level interpreter runtime.
- Prioritize readability over advanced or highly complex language features.
- Strong tooling is a core part of the project vision.
- Built with deep respect for the ideas and communities behind Gleam and Ante.

## Repository Layout

- `crates/goby-core`: core language implementation (AST, parser, type checker, IR)
- `crates/goby-cli`: command-line interface
- `crates/goby-wasm`: Wasm backend
- `examples/`: small sample programs (see `examples/README.md`)
- `doc/README.md`: documentation guide and reading order
- `doc/LANGUAGE_SPEC.md`: latest language specification
- `doc/PLAN.md`: active planning reference and open decisions
- `doc/STATE.md`: restart-safe snapshot of current decisions and next steps

## Language Specification

Detailed and evolving language rules are intentionally documented in
`doc/LANGUAGE_SPEC.md` instead of this README. For an overview of all project docs, start at
`doc/README.md`.

## License

MIT License. See `LICENSE` for details.
