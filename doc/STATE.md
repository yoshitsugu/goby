# Goby Project State Snapshot

Last updated: 2026-02-28 (session 12, uncommitted)

This file is a restart-safe snapshot for resuming work after context reset.

## 1. Current Architecture

- Rust Cargo workspace (root `Cargo.toml`).
- Crates:
  - `crates/goby-core` (language core: AST/parser/typechecker/IR).
  - `crates/goby-cli` (CLI entrypoint).
  - `crates/goby-wasm` (Wasm backend).

## 2. Locked MVP Decisions

- First backend target is Wasm.
- Effects are parse-only metadata in MVP.
- Entry function is `main` only.
- `main` type is `Unit -> Unit`.
- CLI commands:
  - `run`: parse + typecheck + requires `main`.
  - `check`: parse + typecheck (no runtime entry requirement).
- `run` execution pipeline:
  - emit `<input>.wasm`
  - execute via external `wasmtime run --invoke main <output.wasm>`
  - if `wasmtime` is missing, skip execution with an informational message.
- Statement separator is newline or `;`.
- Generic type application syntax is locked to Haskell-style spacing:
  - `List Int`
  - `TypeX a b`
  - nested grouping via parentheses, for example `TypeX (TypeY a b) c`.
- Indentation-based blocks accept tabs and spaces.
- Function calls support both `f x` and `f(x)`.
- Block-local binding semantics are locked:
  - `name = expr` is a binding only for assignment `=`, never for `==`.
  - bindings are visible to subsequent statements in the same declaration body.
  - same-name re-binding in one body is allowed and shadows earlier bindings.
  - bindings are declaration-local (do not escape declarations).
- Operator precedence/associativity is locked:
  - low -> high: `|>` < `+` < `*` < call/application.
  - `|>`, `+`, `*` are left-associative.
  - MVP parser rule: `+` and `*` require spaces on both sides.
  - pipeline callee must be an identifier (`expr |> f`).
- MVP built-ins:
  - `print`
  - `string.concat`
  - `map` (required for `examples/function.gb` parity).
- Type diagnostics quality bar is locked for MVP:
  - diagnostics are non-empty plain text.
  - declaration name is included when known.
  - mismatch diagnostics include both expected and actual type names.
  - composite types are shown in full form (for example, `List Int`, `(String, Int)`).
  - line/column reporting is not required in MVP.
- `examples/basic_types.gb` is parse/typecheck target only (not runtime entry target).
- `examples/function.gb` is a fixed canonical run target and must be preserved as-is.
- `examples/function.gb` expected runtime output is locked:
  - `90`
  - `[30, 40, 50]`
  - `[60, 70]`
  - `something`
  - `15`

## 3. Known Open Decisions

- None currently tracked for the locked MVP subset.
- MVP implementation for the locked subset is complete.

## 4. Recent Milestones

- `0c24614`: fixed AST unit-call fallback in Wasm runtime path (`goby-wasm`).
- `b468f78`: locked binding and precedence rules in `doc/PLAN.md` and synced state.
- `cf107c5`: added parser/typecheck regression tests for locked MVP rules.
- `96672df`: locked MVP comment syntax policy in `doc/PLAN.md` and `doc/STATE.md`.
- 2026-02-28 (session 10, uncommitted): validated `check/run` acceptance path and full `cargo check/test/clippy`, and added parser regression coverage for line-end comments and `#!`.
- 2026-02-28 (session 11, uncommitted): implemented Haskell-style generic type-application parsing in `goby-core` type parsing/typecheck and added regression tests.
- 2026-02-28 (session 12, uncommitted): revalidated acceptance path and full workspace checks; marked locked MVP implementation as complete.

## 5. Current Example Files

- `examples/hello.gb`
- `examples/basic_types.gb`
- `examples/function.gb`
- `examples/generic_types.gb`

## 6. Immediate Next Steps (Execution Order)

1. Keep CLI/E2E regression checks green for MVP acceptance path:
   - `cargo run -p goby-cli -- check examples/function.gb`
   - `cargo run -p goby-cli -- run examples/function.gb`
   - locked output contract for `function.gb`.
2. Treat remaining language/design items as post-MVP evolution work.

## 7. Resume Commands

- `cargo check`
- `cargo test`
- `cargo clippy -- -D warnings`
- `cargo run -p goby-cli -- run examples/function.gb`

## 8. Deferred TODO

- Declaration-side generic parameter binders are intentionally deferred
  (design memo only; not in active execution steps).
