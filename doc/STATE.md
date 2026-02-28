# Goby Project State Snapshot

Last updated: 2026-02-28 (session 3)

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

- Assignment/binding semantics:
  - exact rule for block-local `a = ...` still needs a formal spec entry.
- Operator precedence/associativity table:
  - still not frozen.

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
4. 将来: `to_str_repr()` 依存を撤廃し、`Expr` を直接評価する AST evaluator に移行。

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

## 8. Progress Since Scaffold

- Implemented minimal parser foundation in `crates/goby-core`:
  - `ast.rs` and `parser.rs`
  - top-level declaration parsing for current examples
- Added parser tests:
  - parses `examples/hello.gb`
  - parses `examples/basic_types.gb`
- Implemented initial CLI parse flow in `crates/goby-cli`:
  - loads source file
  - parses with `goby_core::parse_module`
  - reports declaration count
- Verified with:
  - `cargo test`
  - `cargo run -p goby-cli -- run examples/hello.gb`
  - `cargo run -p goby-cli -- check examples/basic_types.gb`

## 9. Progress Since Minimal Parser

- Added minimal typechecking in `crates/goby-core`:
  - `types.rs`: basic function type parsing (`->`) and effect suffix stripping (`can ...`)
  - `typecheck.rs`: duplicate declaration check and `main : Unit -> Unit` MVP check
- Updated CLI flow in `crates/goby-cli`:
  - parse -> typecheck -> success output
- Added/updated tests:
  - function type parsing with effect annotation
  - example-level typecheck for `hello.gb` and `basic_types.gb`

## 10. Progress Since Minimal Typecheck

- Updated `goby-cli` command contract:
  - `run <file.gb>` now requires `main`
  - `check <file.gb>` added for parse/typecheck-only workflows
- This aligns CLI behavior with MVP intent:
  - `hello.gb` is a run target
  - `basic_types.gb` is a check target

## 11. Progress Since Initial CLI Contract Hardening

- Added minimal Wasm codegen in `crates/goby-wasm`:
  - `compile_module` emits a valid Wasm module exporting `main`
  - returns error when `main` is missing
- Updated `goby-cli run`:
  - parse + typecheck + codegen
  - writes output wasm file next to input (`<name>.wasm`)
- Added wasm backend unit test:
  - validates emitted module magic header

## 12. Progress Since Initial Wasm Emission

- Refactored `goby-cli` internals for maintainability:
  - separated argument parsing (`parse_args`) from execution (`run`)
  - unified error handling via `CliError` (usage vs runtime)
  - preserved command behavior (`run` / `check`)

## 13. Progress Since CLI Internal Refactor

- Improved `goby-cli` testability:
  - extracted iterator-based `parse_args_from` for unit testing
  - added CLI argument parser unit tests (run/unknown/extra cases)
- Improved `goby-wasm` maintainability:
  - changed minimal wasm bytes to a named constant slice
  - simplified module byte materialization with `to_vec()`

## 14. Progress Since CLI Refactor Step 2

- Added minimal execution phase to `goby-cli run`:
  - after wasm emission, attempts `wasmtime run --invoke main <file.wasm>`
  - if `wasmtime` is unavailable, prints a skip message and exits successfully
- Added CLI unit test for wasm output path computation

## 15. Progress Since CLI Refactor Step 3

- Refined `wasmtime` execution behavior in `goby-cli run`:
  - switched from `output()` to `status()` so runtime stdout/stderr is not swallowed
  - improved failure diagnostics with explicit exit status reporting
  - factored availability check into `is_wasmtime_available`
- Expanded CLI arg parser tests:
  - missing command
  - missing file

## 16. Progress Since CLI Refactor Step 4

- Simplified wasm execution control flow in `goby-cli`:
  - replaced string-sentinel error branching with explicit `ExecutionOutcome`
- `execute_wasm` now returns:
  - `Executed`
  - `SkippedNoWasmtime` (when `wasmtime` is not installed)

## 17. Progress Since Wasm Execution Refactor

- Added first expression-aware Wasm lowering path in `goby-wasm`:
  - when `main` body is `print "..."`, emits a WASI `fd_write`-based module via WAT
  - otherwise falls back to minimal empty-main module
- Added new wasm backend test for print-literal codegen.
- Added `wat` dependency in `goby-wasm` and updated lockfile.

## 18. Progress Since Wasm Lowering Refactor

- Fixed memory layout safety in print-literal Wasm generation:
  - moved static write buffers to non-overlapping offsets (`iovec`, `nwritten`, text)
  - removed potential overlap for longer print strings
- Added bounds-safe string-length conversion (`usize` -> `u32`) with explicit error.
- Added regression test for long print literals.

## 19. Progress Since Wasm Lowering Refactor Step 2

- Improved wasm backend maintainability by removing print-lowering magic numbers:
  - extracted static offsets into named constants (`IOVEC_OFFSET`, `NWRITTEN_OFFSET`, `TEXT_OFFSET`)
  - extracted iovec serialization into `encode_iovec`
- Added focused unit test for iovec little-endian encoding.

## 20. Progress Since Wasm Lowering Step 3

- Added shared print-body analysis in `goby-core` (`analysis.rs`):
  - resolves `print "..."` literals
  - resolves `print <identifier>` when identifier is a local string binding
  - reports invalid print argument forms
- Updated `typecheck` to validate print argument string-ness using shared analysis.
- Updated `goby-wasm` to reuse shared analysis instead of custom print parsing.
- Added `examples/print/local_binding.gb` for local-binding print flow.

## 21. Progress Since Shared Analysis Refactor

- Strengthened print analysis correctness:
  - no longer stops at first print line
  - validates all lines and rejects unsupported multiple-print bodies
- Added regression test for multiple print-call rejection.

## 22. Progress Since Shared Analysis Refactor Step 2

- Hardened assignment detection in print analysis:
  - `=` is treated as binding only when it is not part of `==`
  - prevents accidental misclassification of equality-like expressions
- Added regression test to ensure equality-like lines are not interpreted as local bindings.

## 23. Progress Since Shared Analysis Refactor Step 3

- Extended string expression resolution for print analysis:
  - supports `string.concat(<string-expr>, <string-expr>)`
  - supports nested concat and local string bindings
- Added new sample:
  - `examples/print/concat.gb`
- Added analysis tests for concat-based print resolution.

## 24. Progress Since Shared Analysis Refactor Step 4

- Refactored `analysis.rs` for maintainability:
  - extracted local-binding update logic
  - extracted print-argument resolution logic
  - centralized error message constants for print analysis
- Kept behavior equivalent while improving readability and future edit safety.

## 25. Progress Since Shared Analysis Refactor Step 5

- Improved print-call parsing robustness:
  - accepts whitespace after `print` (including tabs), not only a single space
- Made comment handling explicit in body analysis:
  - lines beginning with `#` are skipped intentionally
- Added regression tests for tab-delimited print syntax and inline comment skipping.

## 26. Progress Since Shared Analysis Refactor Step 6

- Locked indentation mixing behavior for MVP in `doc/PLAN.md`:
  - any leading space/tab marks an indented line
  - mixed tabs/spaces in one block are accepted in MVP
- Added parser regression test to ensure mixed-indentation block lines parse as one declaration body.

## 27. Progress Since Shared Analysis Refactor Step 7

- Locked MVP Wasm execution path in docs to match current CLI behavior:
  - `run` emits `<input>.wasm`
  - attempts execution via external `wasmtime`
  - gracefully skips execution when `wasmtime` is unavailable

## 28. Progress Since Shared Analysis Refactor Step 8

- Realigned MVP docs for `function.gb` implementation track:
  - clarified that `map` belongs to MVP parse/typecheck scope
  - kept runtime/codegen support for `map` explicitly out of current MVP runtime scope
  - locked `examples/function.gb` as a `check` acceptance target

## 29. Progress Since Shared Analysis Refactor Step 9

- Confirmed `examples/function.gb` with `main` now passes:
  - `cargo run -p goby-cli -- check examples/function.gb`
- Temporarily relaxed typecheck by removing print-string-only enforcement from `typecheck.rs`:
  - this unblocks `function.gb` in `check` mode while AST/type work is still pending
- Confirmed runtime is still blocked at codegen:
  - `cargo run -p goby-cli -- run examples/function.gb` fails with current print-lowering limitation

## 30. Progress Since Shared Analysis Refactor Step 10

- Added explicit Wasm codegen diagnostics for unsupported forms:
  - `print List` is reported as unsupported
  - pipeline operator `|>` is now reported as unsupported
  - higher-order/anonymous-function usage is now reported as unsupported
- Added focused `goby-wasm` unit tests for each diagnostic path.

## 31. Progress Since Shared Analysis Refactor Step 11

- Extended Wasm print lowering with a first `Int` path:
  - supports `print` of simple integer expressions in `main` (literal/identifier and `+`/`*` combinations)
  - reuses existing WASI `fd_write` module emission by converting resolved int values to text
- Updated `goby-wasm` tests:
  - replaced the `print Int` unsupported diagnostic case with a positive codegen case for local int bindings.

## 32. Progress Since Shared Analysis Refactor Step 12

- Extended integer expression lowering to follow simple top-level Int function calls:
  - infers single-parameter usage from declaration bodies for MVP subset
  - resolves `main` local bindings that call Int-returning functions
  - emits printable output through existing WASI `fd_write` path
- Updated `examples/function.gb` to the runnable integer-function subset.
- Added `goby-wasm` regression test to ensure `examples/function.gb` compiles as a valid Wasm module.

## 33. Progress Since Shared Analysis Refactor Step 13

- Re-locked policy that `examples/function.gb` is canonical and must not be simplified.
- Updated planning/state docs to treat `function.gb` runtime parity as required work.
- Recorded concrete parity gap as of 2026-02-27:
  - `cargo run -p goby-cli -- run examples/function.gb` currently prints only `90`.
  - remaining required behaviors are `map`, anonymous function forms, pipeline, and `List Int` print output.

## 34. Progress Since Function Runtime Parity Step 5

- Added `List Int` runtime evaluation subset in `goby-wasm` analysis path:
  - supports `map <list-expr> (|n| -> <int-expr>)`
  - supports placeholder lambda shorthand in map position (`_ * 10` form)
  - supports list-function call parsing with spaced list literals (for forms like `mul_tens [3, 4, 5]`)
- Removed the old blanket higher-order rejection guard so supported `map` + lambda subset can be analyzed.
- Added regression tests that lock these forms through codegen diagnostics:
  - named-lambda map print path reports `print List` unsupported (Step 6 still pending)
  - placeholder-lambda map print path reports `print List` unsupported
  - list-function call with spaced literal argument reports `print List` unsupported

## 35. Progress Since Function Runtime Parity Step 6

- Refactored `goby-wasm` runtime output resolution to evaluate `main` sequentially and collect all print outputs in order.
- Implemented pipeline handling in runtime subset (`x |> f`) and supported `|> print` output emission for `main` statements.
- Implemented `List Int` print formatting as bracketed comma-separated output.
- Updated diagnostics so `print Int` / `print List` are treated as supported in the current runtime subset.
- Replaced previous unsupported-form regression tests with parity-lock tests:
  - pipeline print output resolution test (`[1, 2, 3]`)
  - canonical `examples/function.gb` output lock:
    - `90`
    - `[30, 40, 50]`
    - `[60, 70]`

## 36. Progress Since Function-Argument Runtime Support

- Extended runtime output resolution to execute `Unit`-returning function calls in `main` expression statements.
- Added support for passing `Int -> Int` callables as function arguments and invoking them inside function bodies (`f 10`).
- Added callable argument forms:
  - inline lambda (`|n| -> n + 5`)
  - named function reference (`add_ten`)
- Hardened parameter inference by ignoring identifiers inside string literals.
- Updated `function.gb` runtime parity lock output:
  - `90`
  - `[30, 40, 50]`
  - `[60, 70]`
  - `something`
  - `15`
- Added focused regression for function-argument call runtime output.
- Verified end-to-end validation commands:
  - `cargo fmt --all`
  - `cargo clippy --all-targets --all-features -- -D warnings`
  - `cargo test --all-features`
  - `cargo run -p goby-cli -- run examples/function.gb`

## 37. Progress Since Typecheck Tightening Step 1

- Strengthened `goby-core` typecheck validation for top-level annotations:
  - rejects legacy `void` spelling in any type annotation and requires `Unit`
  - validates `can` clause syntax (effect list must be non-empty identifiers)
  - reports invalid function type annotation shape when `->` is malformed
- Added focused typecheck regressions for:
  - rejecting `void` in non-`main` declarations
  - rejecting empty `can` effect list
  - rejecting invalid effect identifiers
  - accepting non-function annotations such as tuple types
- Validation run:
  - `cargo fmt`
  - `cargo test`
  - `cargo run -p goby-cli -- run examples/function.gb`

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

### Next work

- Address P1 items above.
- Migrate evaluator fully to AST path (remove `to_str_repr()` dependency).
- Wasm codegen: emit actual Wasm bytecode for integer arithmetic (remove WAT dependency).
