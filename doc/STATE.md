# Goby Project State Snapshot

Last updated: 2026-03-01 (session 25)

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
- 2026-03-01 (session 23, commit b889ee0): Step 2 — unhandled effect op detection:
  - `EffectMap`: handler→effect + effect→ops mapping.
  - `check_unhandled_effects_in_expr`: full Expr tree walk for uncovered effect ops.
  - `TypeEnv::is_effect_op`: detects effect-registered globals (Resolved + Ambiguous).
  - `ops_from_can_clause`: seeds covered_ops from function's own `can` clause.
  - Codex-review fixes: lambda param shadowing (H1), pipeline callee (H2), MethodCall (H3), Ambiguous binding (M1), L1/L2 style.
  - 11 new tests. 150 → 161 tests pass.
- 2026-03-01 (session 24): Step 3 — effectful function call requires `using` handler:
  - `build_required_effects_map`: maps decl name → required effect list (from `can` clause).
  - `check_callee_required_effects`: checks a named callee's required effects against `covered_ops`.
  - `check_unhandled_effects_in_expr` extended with `required_effects_map` param; `Expr::Call` + `Expr::Pipeline` now call `check_callee_required_effects`.
  - `macro_rules! recurse!` introduced to reduce boilerplate with 6-param signature.
  - `#[allow(clippy::too_many_arguments)]` on `check_body_stmts`.
  - 4 new tests. 161 → 165 tests pass.
  - `doc/PLAN.md` §5 items marked complete.
- 2026-03-01 (session 25): Better error diagnostics — Steps 1–3 (Codex-reviewed):
  - Step 1 (commit 4b17f70): `ParseError` gains `col: usize`; `is_indented` error computes
    col from raw-line indent width; EOF-after-annotation error points to annotation line;
    `Display`/`Error` impl; CLI format → `"file:line:col: parse error: msg"` (GCC form).
  - Step 2 (commit b098afc): `Declaration` gains `line: usize` (annotation line when present,
    definition line otherwise); set by parser via single immutable `decl_line = i + 1`.
  - Step 3 (commit eb2820f): `Span { line, col }` added to `ast.rs` (re-exported from lib.rs);
    `TypecheckError` gains `span: Option<Span>` + `Display`/`Error`; 3 declaration-level
    sites get `Some(Span { line: decl.line, col: 1 })` (duplicate decl, main annotation
    errors, param count mismatch); remaining 29 sites use `span: None`; CLI typecheck format
    → `"file: typecheck error in X at line Y:Z: msg"` via Display; TODO comments for
    `EffectDecl`/`TypeDeclaration` span gap and CLI format asymmetry.
  - 165 tests pass throughout; `cargo clippy -- -D warnings` clean.
  - Work directory: `.claude-work/` → moved to
    `/home/yoshitsugu/.claude/workspaces/home_yoshitsugu_src_gitlab.com_yoshitsugu_goby/`.
- 2026-03-02 (session 26): Better error diagnostics — Steps 4–5 (Codex-reviewed):
  - Step 4 (commit 6f4f4f0): `format_snippet(source, line, col) -> String` in goby-cli;
    ParseError path appends `\n{snippet}` (no trailing newline when snippet empty);
    TypecheckError path appends `\n{snippet}` when `span` is `Some` and non-empty;
    `col: 1` sentinel lands caret at line start (acceptable for unknown column);
    4 unit tests for `format_snippet`; 169 tests pass.
  - Step 5 (commit 37a621f): 4 ParseError.line/col tests in parser.rs; 3 TypecheckError.span
    tests in typecheck.rs (duplicate decl, non-function main type, wrong-function-type main);
    176 tests pass; `cargo clippy -- -D warnings` clean.
  - Diagnostics improvement phase complete.

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

Diagnostics improvement complete (session 26). All 5 steps done; 176 tests pass.

Resume checks:
```
cargo check
cargo test
cargo clippy -- -D warnings
```

Post-MVP candidates (see `doc/PLAN.md` §4):
1. Real Wasm code generation (actual instruction emission, remove compile-time interpreter).
2. Effect runtime redesign (one-shot deep handlers + selective CPS/evidence passing).
3. More standard library (`Result`, `Option`, etc.).
4. `else if` chaining.

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
