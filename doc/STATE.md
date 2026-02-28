# Goby Project State Snapshot

Last updated: 2026-02-28 (session 7, uncommitted)

This file is a restart-safe snapshot for resuming work after context reset.

## 1. Current Architecture

- Rust Cargo workspace (root `Cargo.toml`).
- Crates:
  - `crates/goby-core` (language core: AST/parser/typechecker/IR).
  - `crates/goby-cli` (CLI entrypoint).
  - `crates/goby-wasm` (Wasm backend).

## 2. Locked MVP Decisions

- First backend target is Wasm.
- Effect system is parse-only in MVP.
- Entry function is `main` only.
- `main` type is `Unit -> Unit`.
- CLI commands:
  - `run`: parse + typecheck + requires `main`
  - `check`: parse + typecheck (no runtime entry requirement)
- `run` Wasm execution pipeline:
  - emit `<input>.wasm`
  - execute via external `wasmtime run --invoke main <output.wasm>`
  - if `wasmtime` is missing, skip execution with an informational message
- Statement separator is newline or `;`.
- Indentation-based blocks:
  - tabs and spaces are both accepted.
- Function calls support both `f x` and `f(x)`.
- Block-local binding semantics are locked:
  - `name = expr` is a binding only for assignment `=`, never for `==`.
  - bindings are visible to subsequent statements in the same declaration body.
  - same-name re-binding in one body is allowed and shadows earlier bindings.
- Operator precedence/associativity is locked:
  - low -> high: `|>` < `+` < `*` < call/application.
  - `|>`, `+`, `*` are left-associative.
  - MVP parser rule: `+` and `*` require spaces on both sides.
- MVP built-ins:
  - `print`
  - `string.concat`
- `map` is required runtime scope for `examples/function.gb` parity.
- `examples/basic_types.gb` is parse/typecheck target only (not runtime entry target).
- `examples/function.gb` is a fixed canonical run target and must be preserved as-is.
- `examples/function.gb` expected runtime output is locked:
  - `90`
  - `[30, 40, 50]`
  - `[60, 70]`
  - `something`
  - `15`

## 3. Known Open Decisions

- Comment syntax policy:
  - currently `# ...` in examples; formal language-spec wording is not frozen.

## 4. Existing Documents and Their Roles

- `README.md`: user-facing project overview.
- `AGENTS.md`: contributor/agent workflow and coding instructions.
- `doc/MVP.md`: implementation plan and acceptance criteria.
- `doc/PLAN.md`: evolving language plan (locked + open items).

## 5. Current Example Files

- `examples/hello.gb`
- `examples/basic_types.gb`
- `examples/function.gb`

## 6. Immediate Next Steps (Execution Order)

1. ~~Fix `to_str_repr()` precedence loss~~ — DONE (session 2).
2. ~~`string.concat` を 2 引数固定でパーサー・評価器を統一~~ — DONE (session 2).
3. ~~型チェックで宣言型 vs 本体戻り型の検証を追加~~ — DONE (session 2).
4. ~~`to_str_repr()` 依存を撤廃し、`Expr` を直接評価する AST evaluator に移行~~ — DONE (session 6, Track B Step 1).

### Function.gb Runtime Parity Checklist

- [x] Step 1: Freeze current failure boundary with tests
  - Add regression coverage that locks expected output for `examples/function.gb`:
    - `90`
    - `[30, 40, 50]`
    - `[60, 70]`
- [x] Step 2: Refactor runtime path to evaluate all `main` lines sequentially
  - Remove single-line/special-case execution paths and unify block evaluation flow.
- [x] Step 3: Implement pipeline operator (`|>`) in evaluator/codegen subset
  - Support `x |> f` as `f x` for the locked MVP subset.
- [x] Step 4: Implement lambda forms used in `function.gb`
  - Support both `|n| -> n * 10` and `_ * 10` in `map` call positions.
- [x] Step 5: Implement runtime `map` for `List Int` subset
  - Support `List Int -> (Int -> Int) -> List Int` evaluation for current examples.
- [x] Step 6: Implement `print` formatting for `List Int`
  - Emit bracketed comma-separated output (`[30, 40, 50]` style).
- [x] Step 7: Align typecheck and diagnostics with runtime support
  - Update accepted forms and error messages to match the implemented subset.
- [x] Step 8: Final validation
  - Verify with:
    - `cargo check`
    - `cargo test`
    - `cargo run -p goby-cli -- run examples/function.gb`

## 7. Resume Commands

- Check workspace build:
  - `cargo check`
- Run tests:
  - `cargo test`
- Current CLI run shape:
  - `cargo run -p goby-cli -- run examples/hello.gb`

## 8. Early Session History (sessions 1–4, collapsed)

Sessions 1–4 built the project from scaffold to a working runtime:

- **Sessions 1–2**: parser, typechecker, CLI (`run`/`check`), Wasm codegen
  skeleton, `string.concat`, `to_str_repr()` precedence fix, declared
  return-type validation.
- **Session 3**: `List Int` runtime, `map`, pipeline (`|>`), lambda forms
  (`|n| -> ...`, `_ * ...`), `List Int` print formatting, unit-returning
  function call support with callable arguments.
- **Session 4**: Codex review P0/P1 fixes — `MethodCall` in
  `needs_parens_as_subexpr`, `ty_name()` recursive formatting, silent
  binding-clear removal, `split_top_level_comma` / `parse_string_concat_call`
  consolidated into `goby-core/src/str_util.rs`.

All function.gb parity steps (checklist in §6) were completed by end of
session 3. All 73+ tests passed from session 4 onwards.

## 38. Progress Since AST Expression Parser Introduction (Phase 4a/4b)

Incremental migration from string-based evaluation to AST-based evaluation. All 62 tests pass. Changes are uncommitted.

### Added / Modified Files

- `crates/goby-core/src/ast.rs`
  - Added `Expr`, `Stmt`, and `BinOpKind` AST node types.
  - Added `Expr::to_str_repr()` — temporary bridge from AST back to string for the legacy evaluator.
  - Added `Declaration::parsed_body: Option<Vec<Stmt>>` field.

- `crates/goby-core/src/parser.rs`
  - Added `parse_body_stmts()` and `parse_expr()` to parse declaration bodies into AST.
  - `parse_module` now populates `parsed_body` automatically.

- `crates/goby-core/src/typecheck.rs`
  - Added `check_expr()` — type inference for `Expr` nodes (Int / String / List / Fun / Unknown).
  - Added `check_body_stmts()` — skeleton for statement-level type checking (local bindings only for now).
  - Added `TypeEnv::with_local()`.

- `crates/goby-wasm/src/lib.rs` (Phase 4a)
  - Extended `RuntimeOutputResolver::resolve()` with `parsed_stmts: Option<&[Stmt]>`.
  - Added `ingest_ast_statement()`, `eval_ast_side_effect()`, `eval_ast_value()`.
  - `main` body now uses the AST path when available, falling back to the string path.

- `crates/goby-wasm/src/lib.rs` (Phase 4b — added this session)
  - `EvaluatedFunction::parsed_stmts` is now used in the evaluation path.
  - Added `execute_unit_ast_stmt()` for AST-based execution of unit-returning function bodies.
  - `execute_unit_call()` uses the AST path when `parsed_stmts` is present, otherwise falls back to the string path.

### All Known Issues Resolved (session 2)

1. ✅ `to_str_repr()` now wraps `Expr::Call` args and `Expr::BinOp` operands in parentheses when needed.
   - Added `needs_parens_as_subexpr()` helper in `ast.rs`.
   - 3 regression tests added in `ast::tests`.

2. ✅ Type checker now validates body type vs declared return type.
   - `check_body_stmts` receives `declared_return_ty: Option<Ty>` and compares the last expression statement's inferred type.
   - `Ty::Unknown` is tolerated (insufficient type info).
   - 2 regression tests added in `typecheck::tests`.

3. ✅ `string.concat` is now strictly 2-argument everywhere.
   - Parser: `parts.len() != 2` → `None`.
   - `analysis.rs` and `goby-wasm`: 3rd+ arg detection via second `split_top_level_comma` call.
   - Regression tests added in `parser::tests` and `analysis::tests`.

### Resume Steps

```
cargo test    # confirm all 69 tests pass
```

## 39. Progress Since Codex Review P0 Fixes (session 3)

Three P0 bugs identified by Codex review were fixed. All 73 tests pass.

### Fix 1: Non-function type annotation bypassed typecheck (`typecheck.rs`)

- `declared_return_ty` derivation now handles both function types (`Int -> Int` → result type)
  and non-function types (`Int`, `String` → annotation type directly).
- Before: `x : Int; x = "hello"` passed silently.
- After: rejected with `body type 'String' does not match declared return type 'Int'`.
- 2 regression tests added.

### Fix 2: Silent binding clear on eval failure (`goby-wasm/src/lib.rs`)

- `ingest_ast_statement` and `execute_unit_ast_stmt` now propagate `None` via `?`
  instead of calling `locals.clear(name)` and returning `Some(())`.
- Caller (`resolve()`) falls back to the string path when `None` is returned.

### Fix 3: Register `print` builtin in `TypeEnv` (`typecheck.rs`)

- `build_type_env` now inserts `print : Unknown -> Unit` into globals.
- `check_expr` for `Call` can now infer `print(x)` → `Ty::Unit` from the environment.
- Before: `f : Int -> Int` body ending with `print "x"` passed silently (`Ty::Unknown`).
- After: rejected with `body type 'Unit' does not match declared return type 'Int'`.
- 2 regression tests added.

### Remaining P1 items (not yet fixed)

1. Constant-declaration type check for non-function annotations (`x : Int; x = "hello"`) — **FIXED above**
2. `ty_name()` shows `"List"` without element type in error messages.
3. `parse_string_concat_call` duplicated in `analysis.rs` and `goby-wasm/src/lib.rs`.
4. `MethodCall` missing from `needs_parens_as_subexpr()`.
5. Function parameters not registered in `TypeEnv` (e.g. `double x = x + x` → `x` is `Unknown`).

## 40. Progress Since Refactoring Phase (session 4)

Two commits: refactoring cleanup + P1 item fixes. All 80 tests pass.

### Refactoring commit (`51fea1a`)

- Removed dead `_ if callee == "print"` branch in `check_expr` Pipeline arm.
- Extracted `annotation_return_ty()` helper in `typecheck.rs`.
- Collapsed nested `if let` guards with `filter()` and let-chains (clippy).
- Propagated `None` in `execute_unit_call` string-path `Statement::Binding` (last silent-clear site).

### P1 fixes commit (`866e594`)

- `ast.rs`: `Expr::MethodCall` added to `needs_parens_as_subexpr()`.
- `typecheck.rs`: `ty_name()` now returns `String` and formats `List Int` recursively.
- `goby-wasm`: `bind_local()` propagates `None` on eval failure; no more silent binding clears anywhere.
- `goby-core/src/str_util.rs`: new module with `split_top_level_comma` and `parse_string_concat_call`; duplicate implementations in `analysis.rs` and `goby-wasm` removed.

### Remaining known gaps (P2)

All P2 gaps from session 4 have been resolved in session 5 (Track A commit `2abe600`).

## 42. Progress Since Codex P0/P1 Review Fixes (session 5, commit 2)

Commit `a5b43b9`. 89 tests pass, clippy clean.

### P0-1 (`parser.rs`)
`split_top_level_definition` now calls `is_assignment_eq` when scanning for the definition `=`, so `==` in the body no longer confuses the splitter.

### P1-1 (`typecheck.rs`)
`typecheck_module` validates `decl.params.len() == ft.arguments.len()` before zipping. A single `Unit` parameter may be omitted from the definition (idiomatic `main : Unit -> Unit; main = ...`).

### P1-2 (`typecheck.rs`)
`ty_from_annotation` now recurses on `parts[0]` when `parts.len() == 1` inside a parenthesised annotation, so `(Int)` → `Ty::Int` instead of `Ty::Unknown`.

### P1-3 (`str_util.rs`, `parser.rs`, `typecheck.rs`)
`split_top_level_commas` (Vec-returning, string-literal + bracket-safe) extracted to `str_util.rs`. Duplicate local implementations in `parser.rs` and `typecheck.rs` removed. `split_top_level_comma` extended to track `[`/`]` depth.

### Next work

- Migrate evaluator fully to AST path (remove `to_str_repr()` dependency).
- Wasm codegen: emit actual Wasm bytecode for integer arithmetic (remove WAT dependency).

## 41. Progress Since Track A (session 5)

Track A: all three items completed in commit `2abe600`. 84 tests pass, clippy clean.

### A1: Register function parameters in TypeEnv

- Added `params: Vec<String>` field to `Declaration` in `ast.rs`.
- `split_top_level_definition` in `parser.rs` now returns parameter names (tokens between function name and `=`).
- `typecheck_module` derives per-parameter types from the function type annotation and passes them to `check_body_stmts`.
- `check_body_stmts` receives `param_tys: &[(&str, Ty)]` and pre-populates `local_env.locals`.
- Effect: `greet : String -> Int; greet name = name` now correctly identifies `name : String` vs declared return `Int` and rejects it.
- 2 regression tests added (`rejects_function_body_type_mismatch_via_param`, `accepts_function_body_with_param_matching_return_type`).

### A2: Tuple type annotation body type check

- `ty_from_annotation` now parses `(A, B, ...)` into `Ty::Tuple([Ty::A, Ty::B, ...])`.
- Added `split_annotation_commas` helper in `typecheck.rs`.
- `ty_name` for `Ty::Tuple` now returns `"(A, B)"` instead of plain `"Tuple"`.
- Effect: `pair : (String, Int); pair = 42` is now correctly rejected.
- 2 regression tests added (`rejects_tuple_annotation_body_mismatch`, `accepts_matching_tuple_annotation_body`).

### A3: Tighten split_top_level_comma visibility

- `str_util::split_top_level_comma` changed from `pub` to `pub(crate)`.
- `parse_string_concat_call` remains `pub` as it is used by `goby-wasm`.

## 43. Progress Since Track B Step 1 (session 6)

Native AST expression evaluator implemented in `crates/goby-wasm/src/lib.rs`. 89 tests pass, clippy clean.

### Changes

- Added `RuntimeLocals::get(&str) -> Option<RuntimeValue>` — unified lookup helper.
- Added `RuntimeOutputResolver::eval_expr_ast(expr, locals, callables, evaluators, depth) -> Option<RuntimeValue>` — evaluates `Expr` nodes directly without `to_str_repr()`.
  - Handles: `IntLit`, `StringLit`, `Var`, `BinOp` (Add/Mul), `MethodCall` (`string.concat` 2-arg), `ListLit`, `Call` (Int and ListInt function paths), `Pipeline`.
  - Unsupported: `Lambda` (top-level), `TupleLit`, other `MethodCall` forms → returns `None` (caller falls back).
  - Depth guard: reuses `MAX_EVAL_DEPTH`.
- Updated `eval_ast_value` — now calls `eval_expr_ast` instead of `to_str_repr()` + `eval_value_with_context`.
- Updated `execute_unit_ast_stmt`:
  - `Stmt::Binding` arm: calls `eval_expr_ast` directly.
  - `print <arg>` arm: calls `eval_expr_ast` directly.
  - `value |> print` arm: calls `eval_expr_ast` directly.
  - Other expression statements: still falls back to `to_str_repr()` + `execute_unit_call` (unit-returning function calls via string path).

### Remaining `to_str_repr()` usage

Only one call site remains: the `_ =>` fallback arm in `execute_unit_ast_stmt` for unit-returning function calls (e.g., `callback_after_print (...)`). This is intentional for now — a follow-up can extend `eval_expr_ast` to return Unit and remove the last `to_str_repr()` call.

### Verification

- `cargo test`: 89 tests pass (6 + 73 + 10).
- `cargo clippy --all-targets --all-features -- -D warnings`: clean.
- `cargo run -p goby-cli -- run examples/hello.gb`: `Hello Goby!`
- `cargo run -p goby-cli -- run examples/function.gb`: `90 / [30, 40, 50] / [60, 70] / something / 15`

## 44. Progress Since Codex P1 Review Fixes (session 6, commit 2)

Commit `26df9b2`. 89 tests pass, clippy clean.

### P1-A (`goby-wasm/src/lib.rs`)
`execute_unit_ast_stmt` now accepts `depth: usize` and propagates it to
all `eval_expr_ast` calls inside it (instead of resetting to `0`).
`execute_unit_call` passes `i + 1` as depth when iterating `parsed_stmts`,
so nested unit-function calls respect `MAX_EVAL_DEPTH`.

### P1-B (`goby-wasm/src/lib.rs`)
`Expr::Call` arm in `eval_expr_ast`: added comment documenting that a
non-`Var` callee (curried call, lambda application) is not yet supported
by the native evaluator and returns `None` intentionally.

### P1-C (`goby-wasm/src/lib.rs`)
`apply_pipeline` now accepts `_depth: usize`. The AST path passes
`depth + 1`; the string path passes `0`. A comment notes that `_depth`
is not propagated into `IntEvaluator`'s own depth counter — known
limitation until the string path is removed.
