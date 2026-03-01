# Goby Project State Snapshot

Last updated: 2026-03-01 (session 22)

This file is a restart-safe snapshot for resuming work after context reset.

## 1. Current Architecture

- Rust Cargo workspace (root `Cargo.toml`).
- Crates:
  - `crates/goby-core` (language core: AST/parser/typechecker).
  - `crates/goby-cli` (CLI entrypoint).
  - `crates/goby-wasm` (Wasm backend / compile-time interpreter).

## 2. Locked MVP Decisions

- First backend target is Wasm.
- Runtime model: compile-time interpreter — `resolve_main_runtime_output` runs the program in
  Rust at Wasm compile time and embeds the output as a static string in the Wasm binary.
- Entry function is `main` only.
- `main` type is `Unit -> Unit`; annotation required for `run`, optional for `check`.
- CLI commands:
  - `run`: parse + typecheck + requires `main` + emits Wasm + executes via `wasmtime`.
  - `check`: parse + typecheck (no runtime entry requirement).
- Statement separator is newline or `;`.
- Generic type application syntax: Haskell-style spacing (`List Int`, `TypeX a b`).
- Indentation-based blocks accept tabs and spaces (mixing allowed in MVP).
- Function calls: `f x` and `f(x)`; callee can be bare identifier or qualified name.
- Block-local binding semantics: `name = expr` is binding only; re-binding shadows; declaration-local.
- Operator precedence: `|>` < `+` < `*` < call/application; all left-associative.
  - `+` and `*` require spaces on both sides in MVP.
- `using` handler syntax: comma-separated (`using HandlerA, HandlerB`).
- `active_handlers` is a `BTreeMap<String, usize>` (deterministic alphabetical dispatch order).
- String escape sequences: `\n`, `\t`, `\\`, `\"` expanded at parse time via `unescape_string`.
- `case` arm separator: ` -> `; parsed by `split_case_arm` (safe for lambda bodies).
- `CasePattern` variants: `IntLit(i64)`, `StringLit(String)`, `BoolLit(bool)`, `Wildcard`.
- `HandlerMethod.parsed_body: Option<Vec<Stmt>>` pre-populated at parse time (no per-dispatch re-parse).
- MVP built-ins: `print`, `string.concat`, `map`, `fetch_env_var`, `string.split`, `list.join`.
- `examples/function.gb` expected runtime output (locked):
  - `90`
  - `[30, 40, 50]`
  - `[60, 70]`
  - `something`
  - `15`

## 3. Known Open Decisions

- None for the locked MVP subset — implementation is complete.
- Post-MVP open items tracked in `doc/PLAN.md` §3 and §6.
- Post-MVP effect implementation direction is now fixed in `doc/PLAN.md` §2.3:
  - deep handlers with one-shot resumptions,
  - selective CPS + evidence passing lowering,
  - compiled `EffectId`/`OpId` dispatch (no map lookup on hot path),
  - phased Wasm lowering (portable trampoline first, typed-continuation optimization later).

## 4. Recent Milestones

- `0c24614`: fixed AST unit-call fallback in Wasm runtime path.
- `b468f78`: locked binding and precedence rules.
- `cf107c5`: parser/typecheck regression tests for locked MVP rules.
- `96672df`: locked MVP comment syntax policy.
- 2026-02-28 (session 10): validated `check/run` acceptance path and parser regression coverage.
- 2026-02-28 (session 11): Haskell-style generic type-application parsing.
- 2026-02-28 (session 12): revalidated acceptance path; marked locked MVP implementation complete.
- 2026-02-28 (session 13): re-audited all `examples/` and added example-driven feature checklist.
- 2026-02-28 (session 14): added incremental implementation plan for `import.gb` slice.
- 2026-02-28 (session 15): `import.gb` slice — import parsing + built-in resolver + typecheck.
- 2026-02-28 (session 16): import collision policy (ambiguous names error only when referenced).
- 2026-02-28 (session 17, commit ddbf19e): `effect.gb` slice — effect/handler/using parse+typecheck; all `examples/*.gb` pass `check`.
- 2026-02-28 (session 18, commit 7962891): `type.gb` runtime — record construction, field access; `run examples/type.gb` outputs `John`.
- 2026-03-01 (session 19, commit c8e669b): all remaining runtime slices:
  - `control_flow.gb`: `CasePattern`/`CaseArm`/`Expr::Case`/`Expr::If`, multi-line lookahead,
    `unescape_string`, `RuntimeValue::Bool`, `BinOpKind::Eq` eval. Outputs: `Five!`, `50`, `30`.
  - `import.gb`: `fetch_env_var`, `string.split` → `ListString`, `.join` → `String`.
    Outputs (with `GOBY_PATH=foo,bar`): `foo`, `bar`.
  - `effect.gb`: `active_handlers: BTreeMap`, `Stmt::Using` save/install/execute/restore,
    handler dispatch helpers. Outputs (with `GOBY_PATH=hello`): `13`, `hello`.
  - **All `examples/*.gb` pass both `check` and `run`.**
- 2026-03-01 (session 20, commits e446b94–7a5f586): Codex-review-driven quality fixes:
  - H1: removed dead `matches!(fn_name.as_str(), _)` always-true guard.
  - H2: `ENV_MUTEX` for env-var test thread-safety; `remove_var` before assert.
  - H3: `HandlerMethod.parsed_body` cached at parse time (removed per-dispatch `parse_body_stmts`).
  - M1: `split_case_arm` replaces `split_once(" -> ")` in case arm parser.
  - M2: `active_handlers: HashMap` → `BTreeMap`; removed `Vec<usize>` collect.
  - M3: `arg_val.to_expression_text()` only in string-fallback branch of `execute_unit_call_ast`.
  - L1: `CasePattern::BoolLit(bool)` — `True`/`False` patterns in `case`.
  - L2: `Stmt::Using` fully handled in `dispatch_handler_method_as_value`.
  - Bonus: bare handler value dispatch in `eval_expr_ast` for non-Int/non-List arguments.
  - 4 new regression tests; 147 total tests pass; `cargo clippy -- -D warnings` clean.
- 2026-03-01 (session 21): algebraic-effects implementation survey and plan lock:
  - Added research-backed post-MVP effect runtime direction to `doc/PLAN.md` (§2.3).
  - Added references for OCaml 5 one-shot continuations, evidence-passing translation, and WasmFX.
- 2026-03-01 (session 22, commit 796a896): Step 1 of `can` semantic checking:
  - `validate_effect_declarations`: duplicate `effect` name now rejected.
  - `validate_effect_clause`: unknown effect name in `can` clause now rejected.
  - `builtin_effect_names() -> &'static [&'static str]` returns `["Print"]`.
  - 6 new tests; 2 existing tests updated. 144 → 150 tests pass.

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

Post-MVP work in progress: `can` semantic checking (Step 1 complete).

Resume checks:
```
cargo check
cargo test
cargo clippy -- -D warnings
cargo run -p goby-cli -- run examples/function.gb
cargo run -p goby-cli -- run examples/effect.gb  # requires GOBY_PATH=hello
```

Post-MVP candidate work (see `doc/PLAN.md` §4):
1. Real Wasm code generation (replace compile-time interpreter with actual Wasm instruction emission).
2. Effect runtime redesign (one-shot deep handlers + selective CPS/evidence passing).
3. Better error diagnostics (line/column, effect-safety errors).
4. More standard library (`Result`, `Option`, etc.).
5. `else if` chaining.
6. Handler type-checking (effect-safety / unhandled-effect diagnostics).

## 7. Resume Commands

```
cargo check
cargo test
cargo clippy -- -D warnings
cargo run -p goby-cli -- run examples/function.gb
```

## 8. Deferred TODO

- Declaration-side generic parameter binders (design memo only).
- `HandlerMethod` body type-checking (currently stored as raw `String` alongside `parsed_body: Option<Vec<Stmt>>`; future handler type-checking would benefit from full inference).
- Effect-safety / unhandled-effect diagnostics (out of scope for MVP).
- Record update syntax and pattern matching on record fields.
- `else if` chaining in `if` expressions (not supported in MVP; documented).
- Real Wasm code generation (current model embeds static string; no instruction-level emit yet).
- REPL / interactive mode.
