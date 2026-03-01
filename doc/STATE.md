# Goby Project State Snapshot

Last updated: 2026-03-01 (session 19, uncommitted)

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
  - Callee can be a bare identifier or a qualified name (`Mod.fn x`, `Mod.fn(x)`).
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
- `main` type annotation is required for `run`; optional for `check`.
  - Declarations without annotations (e.g. `effect.gb`'s `main`) pass `check`.
  - The Wasm backend enforces the annotation at compile time (for `run`).

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
- 2026-02-28 (session 13, uncommitted): re-audited all `examples/` files and added an example-driven feature checklist + spec-detail memo to `doc/PLAN.md`.
- 2026-02-28 (session 14, uncommitted): added an incremental implementation plan for the next `import.gb` slice in `doc/PLAN.md`.
- 2026-02-28 (session 15, uncommitted): completed the initial `import.gb` slice
  (import parsing + minimal built-in module resolver + typecheck integration) and verified full checks.
- 2026-02-28 (session 16, uncommitted): finalized import collision policy
  (ambiguous imported names error only when referenced) and synced plan/state docs.
- 2026-02-28 (session 17, committed ddbf19e): completed `effect.gb` slice:
  - `effect`/`handler` top-level block parsing and AST nodes.
  - `Stmt::Using` with indentation-aware body parsing.
  - effect member registration in type env (qualified and bare keys).
  - handler name registration as `Unknown` in type env.
  - relaxed `main` annotation requirement (`check` no longer requires it).
  - fixed `try_parse_call` to accept qualified callee (`Mod.fn x`, `Mod.fn(x)`).
  - added `parses_effect_gb_declarations` regression test.
  - `check` now passes for all `examples/*.gb` including `effect.gb`.
- 2026-02-28 (session 18, uncommitted): completed `type.gb` runtime slice:
  - added `RuntimeValue::Record { constructor, fields }` variant to Wasm evaluator.
  - added `record_values` field to `RuntimeLocals` (stores `RuntimeValue::Record` directly).
  - implemented `Expr::RecordConstruct` evaluation (recursive field evaluation).
  - implemented `Expr::Qualified` evaluation (field access for records; union constructor fallback).
  - simplified `execute_unit_call_ast` parameter binding via `locals.store`.
  - `run examples/type.gb` now outputs `John`.
  - all `examples/*.gb` pass `check`; `function.gb` and `type.gb` pass `run`.
- 2026-03-01 (session 19, uncommitted): completed all remaining runtime slices — **all `examples/*.gb` now pass `run`**:
  - `control_flow.gb`: added `CasePattern`/`CaseArm` AST nodes, `Expr::Case`/`Expr::If` variants, multi-line lookahead parsing (`parse_multiline_expr`), `unescape_string` for `\n`/`\t`/`\\`/`\"`, `RuntimeValue::Bool`, `BinOpKind::Eq` eval; outputs `Five!`, `50`, `30`.
  - `import.gb`: added `fetch_env_var` (Call + MethodCall), `string.split` → `RuntimeValue::ListString`, `.join` → `RuntimeValue::String`; outputs `foo`, `bar` (with `GOBY_PATH=foo,bar`).
  - `effect.gb`: added `active_handlers: HashMap<String, usize>` to `RuntimeOutputResolver`, `Stmt::Using` save/install/execute/restore pattern, `find_handler_method_for_effect`/`find_handler_method_by_name`/`dispatch_handler_method`/`dispatch_handler_method_as_value`/`execute_decl_as_side_effect` helpers; outputs `13`, `hello` (with `GOBY_PATH=hello`).
  - 140 total tests pass; `cargo clippy -- -D warnings` clean.

## 5. Current Example Files

- `examples/hello.gb`
- `examples/basic_types.gb`
- `examples/function.gb`
- `examples/generic_types.gb`
- `examples/import.gb`
- `examples/control_flow.gb`
- `examples/type.gb`
- `examples/effect.gb`

## 6. Immediate Next Steps (Execution Order)

All MVP example targets are complete. All `examples/*.gb` pass both `check` and `run`.

Post-MVP options (not yet planned):
- Real Wasm code generation (replace compile-time interpreter with actual Wasm emission).
- Declaration-side generic parameter binders.
- Handler type-checking (effect-safety / unhandled-effect diagnostics).
- REPL or interactive mode.

## 7. Resume Commands

- `cargo check`
- `cargo test`
- `cargo clippy -- -D warnings`
- `cargo run -p goby-cli -- run examples/function.gb`

## 8. Deferred TODO

- Declaration-side generic parameter binders are intentionally deferred
  (design memo only; not in active execution steps).
- `HandlerMethod.body` is stored as a raw `String`; future handler type-checking
  would benefit from `Option<Vec<Stmt>>` similar to `Declaration.parsed_body`.
- Effect-safety / unhandled-effect diagnostics are deferred (out of scope for check slice).
