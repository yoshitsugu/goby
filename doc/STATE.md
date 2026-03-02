# Goby Project State Snapshot

Last updated: 2026-03-02 (session 32)

This file is a restart-safe snapshot for resuming work after context reset.

## 1. Current Architecture

- Rust Cargo workspace (root `Cargo.toml`).
- Crates:
  - `crates/goby-core` (language core: AST/parser/typechecker).
  - `crates/goby-cli` (CLI entrypoint).
- `crates/goby-wasm` (Wasm backend with native+fallback dual path).

## 2. Locked MVP Decisions

- First backend target is Wasm.
- Runtime model (current): prefer native lowering (`supports_native_codegen` + `lower`),
  fallback to compile-time interpreter (`resolve_main_runtime_output`) for unsupported subsets.
- Entry function is `main` only.
- `main` type is `Unit -> Unit`; annotation required for `run`, optional for `check`.
- CLI commands:
  - `run`: parse + typecheck + requires `main` + emits Wasm + executes via `wasmtime run --invoke main`.
    (Phase 8: migrate to WASI-standard `_start` export; drop `--invoke main`.)
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
- 2026-03-02 (session 27, commit 8136d61): bare/qualified/pipeline effect call dispatch in main body (§4.1):
  - `eval_ast_side_effect` (main/top-level AST path) previously dropped bare effect ops inside `using`.
  - Added `Expr::Qualified` arm: `find_handler_method_for_effect` → dispatch → string fallthrough.
  - Added bare-name handler lookup in `Expr::Var` arm (before `execute_unit_call_ast`).
  - Added bare-name handler lookup in `Pipeline` arm (`"msg" |> log` now dispatches).
  - Fixed `execute_decl_as_side_effect` depth: 1 → 0 (top-level consistency).
  - depth=0 for all new `dispatch_handler_method` calls.
  - 5 new regression tests; 181 total tests pass; `cargo clippy -- -D warnings` clean.
- 2026-03-02 (session 28, commit f07a85b): effect op arg type checking at typecheck time (§4.1.1):
  - `check_unhandled_effects_in_expr` `Expr::Call` arm: checks arg type against `Ty::Fun { params }` from `env.lookup`.
  - `Expr::Pipeline` arm: same check (`callee: String`, `value: Box<Expr>` as arg).
  - Both arms use let-chains; skip when type is `Ty::Unknown` (no false positives for unknown vars).
  - `recurse!(value)` in Pipeline arm placed after type check (avoids double traversal).
  - TODO §4.1.1 comments on MethodCall and Qualified-callee deferred paths.
  - 5 new tests: 2 reject (Call + Pipeline wrong type), 2 accept (Call + Pipeline correct type), 1 neutral.
  - 181 → 186 total tests pass; `cargo clippy -- -D warnings` clean.
- 2026-03-02 (session 28, commit cf14974): positional single-field record constructor sugar (BUG-002):
  - `Ctor(value)` now accepted as sugar for `Ctor(field: value)` when record has exactly one field.
  - `check_expr` Expr::Call arm: detects single-field constructor via `lookup_record_by_constructor`, rewrites to RecordConstruct.
  - `eval_expr_ast` Expr::Call arm: new `single_field_constructor_field` helper scans `module.type_declarations`.
  - 3 new tests; 186 → 189 total tests pass; `cargo clippy -- -D warnings` clean.
  - `doc/BUGS.md` created; BUG-001 (handler for unknown effect) and BUG-002 (positional constructor, Fixed) documented.
- 2026-03-02 (session 28, commit 6ba076f): VSCode syntax highlighting (§4.4 phase 1):
  - `tooling/syntax/textmate/goby.tmLanguage.json`: canonical TextMate grammar (10 token categories).
  - `tooling/vscode-goby/`: VS Code extension package (package.json, language-configuration.json, grammar copy, README).
  - `tooling/syntax/testdata/highlight_sample.gb`: manual test fixture covering all categories.
  - `tooling/syntax/README.md`: token category table + "Inspect Editor Tokens and Scopes" instructions.
  - Vim and Emacs support deferred to follow-up phase.
- 2026-03-02 (session 29): Real Wasm codegen planning document added:
  - `doc/PLAN_WASM.md` created with phased migration from compile-time interpreter to instruction-level codegen.
  - Plan includes: AST subset per phase, wasm-encoder module layout, value representation (`Int`/`String`/`List Int`),
    fallback coexistence/retirement timing, concrete file-level task breakdown, DoD checks, and risk mitigations.
- 2026-03-02 (session 30, commits adda49e..6af6e5e): Real Wasm codegen Phase A progress:
  - Phase 0: native/fallback scaffold landed (`backend.rs`, `layout.rs`, `lower.rs`, `fallback.rs`), `compile_module` dispatch gate wired.
  - Phase 1: wasm-encoder static print emitter landed (`print "..."` subset, no `wat` generation path for native subset).
  - Phase 2: native expression subset expanded (`Int`/`Bool`, `+`, `*`, `==`, bindings, print).
  - Phase 3: native direct first-order function call subset landed (`Expr::Call` to named decls in supported bodies).
  - Phase 4: native `List Int` print + pipeline print landed.
  - Phase 5: native `if`/`case` subset landed; `examples/control_flow.gb` now asserted to use native path in tests.
  - Quality gates kept green at each step: `cargo fmt`, `cargo test`, `cargo clippy -- -D warnings`.
- 2026-03-02 (session 31, commits 89f53ee, d9a4efd, d3de0e3):
  - `doc/PLAN_WASM.md` / `doc/STATE.md` refreshed to reflect completed native subset milestones.
  - Native capability checker now exposes reason codes via
    `fallback::native_unsupported_reason(&Module) -> Option<&'static str>`.
  - Added tests asserting native-path selection for `examples/control_flow.gb` and reason-code behavior:
    - supported example (`hello.gb`) => `None`
    - unsupported example (`effect.gb`) => `Some("main_annotation_not_unit_to_unit")`
  - Hardened native call checks: unsupported callee forms return fallback deterministically.
  - Attempted 2-arg call support (`f a b`) was intentionally rolled back in this session to keep
    parser-shape/capability/lowerer consistency; scheduled for next controlled step (Phase 6).
- 2026-03-02 (session 32):
  - Phase 6 re-entry completed for direct-call arity expansion:
    - parser now builds left-associative spaced call chains (`f a b c` => `(((f a) b) c)`),
    - native lower/evaluator supports flattened direct named calls with arbitrary arity
      (subject to declaration param-count match),
    - fallback capability checker now returns explicit reason codes for unsupported call forms
      (arity mismatch, non-native callee body, non-declaration target, etc.).
  - Added regression tests:
    - parser left-associative multi-arg call shape,
    - native emitter selection for 4-arg direct call subset,
    - explicit fallback reason on `examples/function.gb` (lambda/HOF path).
  - Quality gates green: `cargo fmt`, `cargo check`, `cargo test`, `cargo clippy -- -D warnings`.
- 2026-03-02 (session 33): Phase 6.1 safety-first progress
  - Added reason-code regression coverage for native capability fallback:
    - table-driven call reason cases (`call_callee_not_direct_name`, `call_arity_mismatch`,
      `call_target_not_declaration`, `call_target_body_not_native_supported`),
    - deterministic-priority test: lambda/HOF reason wins over arity mismatch when both coexist.
  - Updated `crates/goby-wasm/src/fallback.rs` call-reason order:
    - declaration-body support check now runs before arity mismatch check for direct calls.
  - Replaced ad-hoc fallback reason literals with shared constants in
    `crates/goby-wasm/src/fallback.rs`.
  - Removed legacy string-heuristic unsupported analyzer from `crates/goby-wasm/src/lib.rs`:
    - removed `find_unsupported_form` / `UnsupportedFormAnalyzer` and related helper code.
  - Added example path-coverage matrix test in `goby-wasm`:
    - native: `hello.gb`, `control_flow.gb`,
    - fallback: `effect.gb`, `function.gb`.
  - Quality gates green after changes:
    - `cargo test -p goby-wasm`,
    - `cargo check`,
    - `cargo test`,
    - `cargo clippy -- -D warnings`.
- 2026-03-02 (session 34): Phase 6.1 helper-boundary tightening
  - Added shared direct-call target resolver in `crates/goby-wasm/src/call.rs`:
    - `resolve_direct_call_target(module, name, arity) -> Result<&Declaration, DirectCallTargetError>`.
  - Switched `fallback.rs` and `lower.rs` to use the shared resolver for
    declaration lookup + arity checks (removes duplicated per-file logic).
  - Preserved fallback reason precedence:
    - `call_target_not_declaration` remains explicit for unknown callees,
    - lambda/HOF (`call_target_body_not_native_supported`) still wins over arity mismatch when both apply.
  - Validation: targeted reason/path tests, full `cargo test -p goby-wasm`,
    workspace `cargo check/test/clippy`, and `cargo run -p goby-cli -- run examples/function.gb` all green.
- 2026-03-02 (session 35): shared-call helper test lock
  - Added unit tests in `crates/goby-wasm/src/call.rs` for:
    - multi-arg flattening (`flatten_named_call`),
    - direct-call target resolution outcomes
      (`Ok`, `NotDeclaration`, `ArityMismatch`).
  - Validation rerun: `cargo test -p goby-wasm`, workspace `cargo check/test/clippy` all green.
- 2026-03-02 (session 36): shared print-call helper extraction
  - Added `extract_direct_print_call_arg` + `PrintCallError` to `crates/goby-wasm/src/call.rs`
    and covered with unit tests.
  - Updated `fallback.rs` and `lower.rs` to reuse the shared print-call helper
    (direct print detection and arity-1 semantics no longer duplicated).
  - Reason behavior preserved via existing and new tests:
    - non-print direct call still routes to value-expression checks,
    - print arity mismatch still reports `print_arity_not_one`,
    - non-direct call shape still reports `call_callee_not_direct_name`.
  - Validation rerun: targeted tests, full `cargo test -p goby-wasm`, workspace
    `cargo check/test/clippy` all green.
- 2026-03-02 (session 37): fallback reason enum migration (compat layer)
  - `crates/goby-wasm/src/fallback.rs` now defines typed reason enum:
    - `UnsupportedReason` with `as_str()` mapping.
  - Added `native_unsupported_reason_kind(&Module) -> Option<UnsupportedReason>`
    while preserving existing string API:
    - `native_unsupported_reason(&Module) -> Option<&'static str>`.
  - Existing reason-code tests remain unchanged and green (string-compat maintained).
  - Validation rerun: `cargo test -p goby-wasm`, workspace `cargo check/test/clippy` all green.
- 2026-03-02 (session 38): enum reason API adoption in tests
  - Updated wasm native/fallback capability tests to assert both:
    - typed reason API (`native_unsupported_reason_kind`),
    - string compatibility API (`native_unsupported_reason`).
  - Path matrix test now validates typed and string reason expectations together.
  - Validation rerun: `cargo test -p goby-wasm`, workspace `cargo check/test/clippy` all green.
- 2026-03-02 (session 39): shared case-pattern support helper
  - Added `crates/goby-wasm/src/support.rs` with shared helper:
    - `is_supported_case_pattern(&CasePattern) -> bool`.
  - Reused helper from both:
    - `fallback.rs` (`UnsupportedReason::UnsupportedCasePattern` checks),
    - `lower.rs` (native value-expression capability checks for `Expr::Case`).
  - Added helper unit test (`support::tests::supports_current_native_case_patterns`).
  - Validation rerun: targeted tests, full `cargo test -p goby-wasm`, workspace
    `cargo check/test/clippy` all green.
- 2026-03-02 (session 40): shared list-item/binop support helpers
  - Expanded `crates/goby-wasm/src/support.rs` with:
    - `is_supported_list_item_expr(&Expr) -> bool`,
    - `is_supported_binop_kind(&BinOpKind) -> bool`,
    - focused helper tests.
  - Reused these helpers in both `fallback.rs` and `lower.rs` capability checks.
  - Resolved clippy cleanups (`redundant_closure`) and reran full gates.
  - Validation rerun: `cargo test -p goby-wasm`, workspace `cargo check/test/clippy` all green.
- 2026-03-02 (session 41): typed reason checks expanded
  - Migrated call-reason capability tests in `crates/goby-wasm/src/lib.rs` to
    typed expectations (`UnsupportedReason`) as primary assertions.
  - Kept string compatibility checks in parallel by deriving expected strings via
    `UnsupportedReason::as_str`.
  - Added typed assertion for lambda/HOF-over-arity priority test.
  - Validation rerun: `cargo test -p goby-wasm`, workspace `cargo check/test/clippy` all green.
- 2026-03-02 (session 42): Phase 6 completion gates finalized
  - Added explicit Phase-6 boundary tests in `crates/goby-wasm/src/lib.rs`:
    - first-order subset derived from `function.gb` uses native path,
    - unused HOF declaration does not block native path,
    - transitively required HOF declaration forces fallback with typed reason
      `CallTargetBodyNotNativeSupported`.
  - Revalidated full quality gates:
    - `cargo test -p goby-wasm`,
    - `cargo check`,
    - `cargo test`,
    - `cargo clippy -- -D warnings`.
  - Phase 6/6.1 marked complete in `doc/PLAN_WASM.md`; focus moves to Phase 7 sign-off.
- 2026-03-02 (session 43): Phase 7 sign-off completed
  - Migrated `compile_print_module` from `wat` to `wasm-encoder`
    (`backend::WasmProgramBuilder::emit_static_print_module`).
  - Fixed pre-existing Wasm spec violation in `backend.rs`: code section (id=10)
    was emitted after data section (id=11); swapped to correct order.
  - Removed `wat = "1"` dependency from `crates/goby-wasm/Cargo.toml`.
  - Deleted `minimal_main_module()` and its byte-constant; `compile_module` now
    returns `Err(CodegenError)` for modules with unsupported-but-non-printable `main`.
  - Replaced byte-equality fallback test assertions with `supports_native_codegen`
    + `assert_valid_wasm_module` checks (version byte added to helper).
  - Added module-level doc to `fallback.rs` documenting the native subset and
    intentional fallback boundaries; added `///` docs to public `CodegenError`
    and `compile_module`.
  - 215 tests pass; `cargo doc` warning-free; `cargo clippy -- -D warnings` clean.
  - Phase 7 / Phase A marked complete in `doc/PLAN_WASM.md`.

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

Wasm Phase A (Phases 0–7) complete. 215 tests green, clippy clean.

Resume checks:
```
cargo check
cargo test
cargo clippy -- -D warnings
```

Execution focus (in order):
1. **Phase 8**: WASI-standard `_start` entrypoint — `backend.rs` + `goby-cli` (small, self-contained).
   - Replace `exports.export("main", ...)` with `exports.export("_start", ...)` in `backend.rs`.
   - Drop `--invoke main` from `execute_wasm` in `goby-cli/src/main.rs`.
   - Validates that `wasmer run` and `wasmtime run` (no flags) both work out of the box.
2. Lambda/HOF native lowering (Phase B prerequisite).
3. Effect runtime redesign (one-shot deep handlers + selective CPS/evidence passing).
4. `resolve_main_runtime_output` retirement (blocked on lambda + effect native support).

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
