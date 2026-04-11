# REPO_MANIFEST

Repository-level summary for quick orientation.

## Repo Summary
- Language project: Goby (typed functional language with effects)
- Primary backend target: Wasm
- Preferred context flow: summary -> changed source -> targeted expansion

## High-Value Paths
- `crates/goby-core` (89 files): Language core: parser, typechecker, IR
- `crates/goby-wasm` (49 files): Wasm lowering, emitter, runtime bridge
- `crates/goby-cli` (17 files): CLI entrypoints and command wiring
- `crates/goby-lsp` (2 files): Language server
- `stdlib/goby` (7 files): Stdlib surface modules
- `examples` (56 files): Language behavior samples/regressions
- `doc` (8 files): Spec, plans, state
- `tooling` (28 files): Editor syntax/tool integrations

## Global Docs
- `AGENTS.md` [ok]
- `doc/LANGUAGE_SPEC.md` [ok]
- `doc/PLAN.md` [ok]
- `doc/STATE.md` [ok]

## Agent Entry
- Read `AGENTS.md` first for global invariants.
- Use your local `$HOME/.codex` workflow for process-specific context shaping.
