# Goby Project State Snapshot

Last updated: 2026-03-03 (session 73)

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
  - `run`: parse + typecheck + requires `main` + emits Wasm + executes via `wasmtime run <path>`.
    Wasm module exports `_start` (WASI Preview 1 standard); no `--invoke` flags needed.
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
- 2026-03-02 (session 28, commit 6ba076f): Editor syntax highlighting (§4.5):
  - `tooling/syntax/textmate/goby.tmLanguage.json`: canonical TextMate grammar (10 token categories).
  - `tooling/vscode-goby/`: VS Code extension package (package.json, language-configuration.json, grammar copy, README).
  - `tooling/vim/syntax/goby.vim` + `tooling/vim/ftdetect/goby.vim`: Vim syntax pack + ftdetect rule.
  - `tooling/emacs/goby-mode.el`: Emacs major mode with font-lock rules and `auto-mode-alist` for `.gb`.
  - `tooling/syntax/testdata/highlight_sample.gb`: manual test fixture covering all categories.
  - `tooling/syntax/README.md`: token category table + install instructions.
  - All three editors (VSCode, Vim, Emacs) confirmed present; cross-editor regression tests deferred.
- 2026-03-02 (session 29): Real Wasm codegen planning document added:
  - Wasm migration plan document created with phased migration from compile-time interpreter to instruction-level codegen.
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
  - Wasm migration plan doc and `doc/STATE.md` refreshed to reflect completed native subset milestones.
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
  - Phase 6/6.1 marked complete in the Wasm migration plan doc; focus moved to Phase 7 sign-off.
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
  - Phase 7 / Phase A marked complete in the Wasm migration plan doc.
- 2026-03-02 (session 44): Phase 8 sign-off completed (commits 832805d, 581775c)
  - `backend.rs`: renamed export from `"main"` to `"_start"` (WASI Preview 1 standard).
  - Added `find_wasm_section` helper + `exports_start_entrypoint` test in `lib.rs`:
    scans export section (id=0x07) only for length-prefixed names to avoid false matches.
  - `goby-cli/src/main.rs`: removed `.arg("--invoke").arg("main")` from `execute_wasm`.
  - `cli_integration.rs`: updated fake-wasmtime guard from `--invoke main` positional check
    to `$1 = "run" && -n $2`, matching the new `wasmtime run <path>` invocation shape.
  - 215 tests pass; `cargo clippy -- -D warnings` clean.
  - Phase 8 marked complete in the Wasm migration plan doc.
- 2026-03-02 (session 45): Wasm test layout cleanup for migration-plan §6
  - Added integration test file:
    `crates/goby-wasm/tests/wasm_exports_and_smoke.rs`.
  - Moved smoke/export checks from `src/lib.rs` tests to integration coverage:
    - `_start` export-section assertion,
    - unsupported-main codegen error check,
    - basic wasm-header smoke checks (`print` literal, `function.gb` compile).
  - Updated migration-plan §6 test work-item to done.
  - Validation: `cargo test -p goby-wasm`, `cargo check`, `cargo test`,
    `cargo clippy -- -D warnings` all green.
- 2026-03-02 (session 46): Wasm migration plan closure
  - Marked PLAN_WASM as complete for its defined scope (Phase 0-8).
  - Updated baseline behavior note: unsupported non-printable fallback now returns
    `Err(CodegenError)` (no `minimal_main_module` path).
  - Reframed §9 as post-plan handoff (out of scope for this closed plan):
    full interpreter retirement remains follow-up work (lambda/HOF + effect runtime).
- 2026-03-02 (session 47): Lambda/HOF native lowering (`function.gb`) landed
  - `crates/goby-wasm/src/lower.rs`:
    - added callable native values (named callable + lambda closure + partial application),
    - added generic callable application path for `Expr::Call` (not direct-name-only),
    - added native `map` builtin evaluation on `List Int` with callable mapper,
    - enabled callback parameter invocation (`f 10`) in native declaration bodies.
  - `crates/goby-wasm/src/fallback.rs`:
    - accepts `Expr::Lambda` as native-supported value expression,
    - accepts direct `map` calls as native-supported value calls,
    - keeps `CallTargetBodyNotNativeSupported` for unsupported declaration bodies (for example `using`).
  - `crates/goby-wasm/src/lib.rs` tests:
    - `function.gb` switched to native-capable expectation,
    - transitive HOF declaration case switched from fallback to native,
    - call-reason coverage now keeps unsupported-body reason via `using`-based fixtures.
  - Validation: `cargo fmt`, `cargo test -p goby-wasm`, `cargo check`, `cargo test` all green.
- 2026-03-02 (session 48): Standard-library foundation planning track started
  - Added detailed execution document: `doc/PLAN_STANDARD_LIBRARY.md`.
  - Scope is explicitly staged:
    - resolver skeleton,
    - typechecker import integration (with legacy builtin fallback),
    - stdlib seed modules under `stdlib/goby/`,
    - `goby/stdio` module planning and builtin `print` migration path,
    - CLI stdlib-root wiring,
    - diagnostics hardening.
  - Progress for this track will be managed against the checkpoints and DoD in
    `doc/PLAN_STANDARD_LIBRARY.md`.
- 2026-03-02 (session 49): `goby/stdio` + stdlib-only `@embed` policy added to plan
  - `doc/PLAN_STANDARD_LIBRARY.md` updated with:
    - `stdlib/goby/stdio.gb` as a first-class module target,
    - explicit `print` migration sequence through `goby/stdio`,
    - stdlib-only `@embed` design rule (initial target: `@embed Print`),
    - diagnostics/test/checkpoint requirements for `@embed` scope enforcement.
  - `doc/PLAN.md` §4.3 updated with locked planning constraint:
    - `@embed` is stdlib-only (not allowed in user/non-stdlib modules),
    - `print` migration proceeds via stdlib bridge with temporary bare-print compatibility.
  - Execution style locked: proceed via incremental step-by-step plan in
    `doc/PLAN_STANDARD_LIBRARY.md` §7, completing one small step at a time.
- 2026-03-02 (session 50): `PLAN_STANDARD_LIBRARY` Step0 baseline lock complete
  - Added baseline regression tests in `crates/goby-core/src/typecheck.rs` for:
    - plain import (`import goby/string`) qualified symbol use,
    - alias import (`import goby/list as l`) qualified symbol use,
    - selective import (`import goby/env ( fetch_env_var )`) bare symbol use,
    - bare builtin `print` availability without import.
  - No language/runtime behavior changes intended in this step.
  - Quality gates re-run and green:
    - `cargo check`
    - `cargo test` (220 tests passed)
    - `cargo clippy -- -D warnings`
  - `clippy` cleanup included one non-functional let-chain refactor in
    `crates/goby-wasm/src/fallback.rs` (`collapsible_if`).
- 2026-03-02 (session 51): `PLAN_STANDARD_LIBRARY` Step1 resolver shell complete
  - Added new module: `crates/goby-core/src/stdlib.rs`.
  - Added resolver API shell:
    - `StdlibResolver::new(root: PathBuf)`,
    - `StdlibResolver::resolve_module(module_path: &str) -> Result<..., StdlibResolveError>`,
    - `StdlibResolver::module_file_path(module_path: &str) -> Result<PathBuf, ...>`.
  - Added shell data types: `ResolvedStdlibModule`, `StdlibResolveError`.
  - Added unit tests for path mapping only:
    - `goby/string` -> `<root>/goby/string.gb`,
    - nested module path mapping,
    - invalid module path rejection.
  - No integration call sites changed yet (`typecheck` import flow unchanged).
- 2026-03-02 (session 52): `PLAN_STANDARD_LIBRARY` Step3-5 complete (resolver extraction + import integration)
  - `crates/goby-core/src/stdlib.rs`:
    - Implemented `resolve_module` pipeline:
      - module path -> file path,
      - read source (`ReadFailed`/`ModuleNotFound`),
      - parse source (`ParseFailed`),
      - export map extraction from top-level declarations.
    - Export extraction policy:
      - duplicate top-level export names => `DuplicateExport`,
      - missing type annotation on exported declaration => `ExportTypeMissing`.
    - Added resolver regression tests for:
      - successful module resolution/export extraction,
      - module-not-found,
      - parse-failed,
      - duplicate-export,
      - export-type-missing.
  - `crates/goby-core/src/typecheck.rs`:
    - Added resolver-backed import export lookup helper with builtin fallback.
    - Step4: `validate_imports` now uses resolver-first lookup, builtin fallback on module-not-found.
    - Step5: `inject_imported_symbols` now uses the same resolver-first lookup path.
    - Added typecheck integration tests for:
      - resolver-first precedence over builtin exports,
      - builtin fallback when file-based stdlib module is missing,
      - parse-failure path surfaced as import-resolution error.
  - Quality gates green:
    - `cargo check`
    - `cargo test` (231 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 53): `PLAN_STANDARD_LIBRARY` Step6-8 complete (stdlib seed + stdio + `@embed` gate)
  - Step6 (`stdlib/goby` seed modules):
    - Added `stdlib/goby/string.gb` with `concat`, `split`, and file-only marker export `length`.
    - Added `stdlib/goby/list.gb` with `join`.
    - Added `stdlib/goby/env.gb` with `fetch_env_var`.
    - Added regression test ensuring file-based stdlib symbol (`goby/string.length`) resolves/typechecks (resolver-first path active).
  - Step7 (`goby/stdio` module):
    - Added `stdlib/goby/stdio.gb` with:
      - `@embed effect Print`
      - `print : String -> Unit can Print`
    - Added typecheck regression test for `import goby/stdio ( print )`.
  - Step8 (stdlib-only `@embed` parsing gate):
    - AST/parser:
      - added `Module.embed_declarations` and `EmbedDecl`,
      - parser now accepts `@embed effect <EffectName>` at top level,
      - parser rejects malformed `@embed` declarations.
    - Typechecker:
      - added `typecheck_module_with_context(module, source_path, stdlib_root)` entrypoint,
      - `typecheck_module` keeps compatibility and delegates with no context,
      - `validate_embed_declarations` enforces:
        - `@embed` allowed only when `source_path` is under stdlib root,
        - duplicate embedded effect names rejected.
    - CLI:
      - switched to `typecheck_module_with_context` and passes current source path.
    - Added regression tests:
      - stdlib-path `@embed` accepted,
      - user-path `@embed` rejected,
      - legacy no-context API remains compatible (restriction enforced on context-aware path),
      - duplicate embedded effect names rejected.
  - Quality gates green:
    - `cargo check`
    - `cargo test` (240 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 54): `PLAN_STANDARD_LIBRARY` Step9-10 complete (`@embed Print` metadata + CLI stdlib root wiring)
  - Step9 (`@embed Print` stdio bridge metadata):
    - `ResolvedStdlibModule` now includes `embedded_effects: Vec<String>`.
    - Resolver extracts `@embed effect <Name>` metadata from parsed stdlib modules.
    - Added resolver regression test confirming `goby/stdio` surfaces `embedded_effects = [\"Print\"]`.
    - Bare builtin `print` compatibility preserved while allowing `import goby/stdio ( print )` call sites.
  - Step10 (CLI stdlib root wiring):
    - Typechecker import resolution now receives stdlib root from context (no hardcoded resolver root in import flow).
    - CLI resolves stdlib root via:
      - `GOBY_STDLIB_ROOT` override when set,
      - repo-default `stdlib/` path otherwise.
    - CLI now returns explicit runtime errors for invalid stdlib root overrides:
      - path does not exist,
      - path is not a directory.
    - Added CLI integration tests for:
      - default stdlib root usage (`goby/string.length` file-only symbol),
      - invalid `GOBY_STDLIB_ROOT` error path.
  - Quality gates green:
    - `cargo check`
    - `cargo test` (242 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 55): `PLAN_STANDARD_LIBRARY` Step11-12 complete (diagnostics hardening + print migration checkpoint)
  - Step11 diagnostics hardening:
    - Improved import diagnostics for unknown modules to include attempted stdlib file path.
    - Improved parser diagnostics for malformed `@embed` declarations:
      - invalid target now reports expected shape `@embed effect <EffectName>`,
      - invalid embedded effect name has explicit error text.
    - Refined resolver diagnostics:
      - added dedicated `DuplicateEmbeddedEffect` error kind (no longer overloading `DuplicateExport`).
    - Added regression tests for the above diagnostics.
  - Step12 print migration handoff checkpoint (current active behavior):
    - `goby/stdio.print` is importable and callable (`import goby/stdio ( print )`).
    - bare builtin `print` compatibility is preserved during migration.
    - `@embed` parsing/typecheck support is active and path-restricted in context-aware flows:
      - accepted under stdlib root,
      - rejected outside stdlib root,
      - duplicate embedded effect names rejected.
  - Quality gates green:
    - `cargo check`
    - `cargo test` (244 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 56): `PLAN_RESUME` Step 0 complete (spec/keyword lock + parser contract tests)
  - Spec lock:
    - `doc/PLAN.md` now explicitly locks `resume` surface contract for Step 0.
    - `doc/PLAN_RESUME.md` Step 0 marked DONE with landed parser-contract scope.
  - Parser guardrails:
    - reserved keyword check added for top-level declaration name `resume`.
    - handler parameter name `resume` now rejected with dedicated parse error text.
  - Parser contract tests added:
    - reject top-level declaration name `resume`,
    - reject handler parameter name `resume`,
    - accept handler-body `resume Unit` shape (still call-form pre-`Expr::Resume`).
  - Quality gates green:
    - `cargo check`
    - `cargo test` (251 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 57): `PLAN_RESUME` Step 1 complete (AST/parser + malformed resume diagnostics)
  - `Expr::Resume { value }` added to `goby-core` AST.
  - Parser now recognizes `resume <expr>` as a dedicated expression form.
  - Reserved keyword behavior tightened in expression parsing:
    - bare identifier `resume` no longer parses as `Expr::Var`.
  - Dedicated parse diagnostics added for malformed resume syntax:
    - `malformed \`resume\` expression: expected \`resume <expr>\``.
    - enforced in declaration and handler-method body parsing paths.
  - Parser tests updated/added:
    - `resume Unit` parses to `Expr::Resume`,
    - malformed `resume` is rejected (`parse_expr` + `parse_module` diagnostics),
    - handler-body contract test updated from call-form to `Expr::Resume`.
  - Temporary Step1 bridge behavior:
    - typechecker treats `Expr::Resume` as `Ty::Unknown` (to be tightened in Step 2),
    - wasm runtime evaluator treats `Expr::Resume` as unsupported (`None`) pending Step 3.
  - Quality gates green:
    - `cargo check`
    - `cargo test` (255 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 58): `PLAN_RESUME` Step 2 complete (typecheck context + resume diagnostics)
  - Added handler-method operation context resolution from `effect` signatures
    (`op : A -> B` gives handler param type `A`, resume expected type `B`).
  - Added dedicated `resume` typecheck pass for both declaration and handler bodies.
  - New diagnostics:
    - `resume_outside_handler`,
    - `resume_arg_type_mismatch`,
    - `resume_in_unknown_operation_context`.
  - Handler method body checks now enforce `resume` argument compatibility with
    the handled operation return type when known.
  - Added 4 resume typecheck regression tests:
    - outside handler rejection,
    - arg mismatch rejection,
    - unknown operation context rejection,
    - matching type acceptance.
  - Quality gates green:
    - `cargo check`
    - `cargo test` (259 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 59): `PLAN_RESUME` Step 3 runtime checkpoint (interpreter bridge)
  - `goby-wasm` runtime now tracks explicit resume state:
    - `HandlerFrame`,
    - `Continuation` (with one-shot consumed bit),
    - `ResumeToken` (captures resumed value).
  - `Expr::Resume` runtime handling added in evaluator:
    - evaluates resume argument,
    - consumes current one-shot token,
    - returns value to effect operation call site.
  - Effect call evaluation order adjusted for value-position calls:
    - active handler dispatch is attempted before Int/List function fast paths,
      so effect ops with Int/List arguments can resume correctly.
  - One-shot guard behavior added:
    - second `resume` on the same token fails deterministically (`None` path).
  - New runtime regression tests in `goby-wasm`:
    - `resume` returns value to effect call site,
    - one-shot guard rejects double resume.
  - Quality gates green:
    - `cargo check`
    - `cargo test` (261 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 60): `PLAN_RESUME` Step 3 complete (runtime error surfacing + continuation snapshot restore)
  - Runtime resume behavior refinements in `goby-wasm`:
    - `resolve_main_runtime_output` now surfaces runtime errors in output text
      (`runtime error: ...`) instead of silent `None` when resume misuse occurs.
    - `Expr::Resume` now reports explicit errors for:
      - no active continuation,
      - already-consumed continuation.
    - token-stack mismatch guard added for defensive runtime consistency checks.
  - Continuation frame usage tightened:
    - handler snapshot from captured continuation frames is reinstalled on `resume`.
    - resume token origin resolution now prefers currently-active handlers
      (avoids ambiguous global handler-method name scan).
  - Added/updated runtime tests:
    - double resume now asserts explicit runtime error message,
    - resume outside handler runtime error surfacing,
    - resume return-to-call-site path remains covered.
  - Quality gates green:
    - `cargo check`
    - `cargo test` (262 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 61): Step1-3 self-review follow-up fixes (runtime consistency + tests)
  - `goby-wasm` resume/runtime fixes:
    - removed `resume`-time mutation of `active_handlers` to prevent handler-context leakage.
    - refactored handler dispatch through shared `dispatch_handler_method_core` to eliminate duplicated token lifecycle logic.
    - resume token now carries resolved `handler_decl_idx` and validates token-handler consistency on `resume`.
    - removed unused continuation frame/handler stack plumbing from fallback runtime path.
  - Added regression tests:
    - qualified resume call with overlapping handler method names dispatches to target handler.
    - repeated qualified calls after `resume` keep handler context stable (`2\n2` case).
  - Quality gates green:
    - `cargo fmt`
    - `cargo check`
    - `cargo test` (264 tests passed)
- 2026-03-02 (session 62): `PLAN_RESUME` Step 4 complete (nearest-handler stack semantics)
  - Runtime handler dispatch in `goby-wasm` now uses lexical stack order:
    - replaced `active_handlers: BTreeMap<effect, handler>` with `active_handler_stack: Vec<handler_idx>`,
    - handler lookup now walks stack from nearest to outermost (LIFO),
    - removed alphabetical fallback capture behavior.
  - `using` scope management now uses push/pop by lexical nesting in:
    - top-level AST ingestion,
    - unit AST execution,
    - handler-body core dispatch path.
  - Resume path consistency:
    - effect-call boundary still restores handler context even on error path (`truncate` to entry depth).
  - Updated/added runtime tests:
    - renamed deterministic overlap test to nearest-handler behavior (`from-beta` expected),
    - added nested-`using` same-effect test (`inner` expected).
  - Plan docs synchronized:
    - `doc/PLAN.md`: runtime section now states lexical nearest-handler semantics active.
    - `doc/PLAN_RESUME.md`: Step 4 marked DONE.
  - Quality gates green:
    - `cargo fmt`
    - `cargo check`
    - `cargo test` (265 tests passed)
- 2026-03-02 (session 63): `PLAN_RESUME` Step 5 complete (validation + regression stabilization)
  - Added focused runtime regression for no-resume abortive path:
    - `no_resume_in_value_position_takes_abortive_path` in `goby-wasm`.
  - Existing focused cases confirmed green:
    - single resume success,
    - double resume runtime error,
    - nested `using` nearest-handler selection.
  - `doc/PLAN_RESUME.md` updated: Step 5 marked DONE.
  - Quality gates green:
    - `cargo check`
    - `cargo test` (266 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 64): `PLAN_RESUME` Step 6 complete (consumer-track stdlib enablement)
  - Iteration API direction:
    - kept effect/handler `yield` + `resume` pattern as the iterator-like baseline.
    - `examples/iterator.gb` updated to a stable fallback-runtime-compatible lock sample
      (three value-position `yield` calls via local bindings).
  - Coverage updates:
    - added `goby-wasm` runtime lock test for `iterator.gb` output
      (`locks_runtime_output_for_iterator_gb` => `tick\ntick\ntick`),
    - added `iterator.gb` into `goby-core` `typechecks_examples` regression suite.
  - Intrinsic re-evaluation result:
    - no `__goby_*` names are currently implemented/consumed in compiler/runtime code,
    - kept `__goby_*` as deferred bridge convention in `doc/PLAN_STANDARD_LIBRARY.md`
      (do not introduce runtime intrinsics in this Step 6 scope).
  - Plan docs synchronized:
    - `doc/PLAN_RESUME.md`: Step 6 marked DONE with decisions above.
    - `examples/README.md`: iterator sample behavior documented.
  - Quality gates green:
    - `cargo fmt`
    - `cargo check`
    - `cargo test` (267 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 65): `PLAN_RESUME` Step 7.1 complete (execution-style planning metadata)
  - Added new wasm planning module:
    - `crates/goby-wasm/src/planning.rs`
    - introduces `LoweringPlan` + `LoweringStyle` (`DirectStyle` / `EffectBoundary`).
  - Planning classification signals implemented:
    - `can` effect clause in type annotation,
    - `Stmt::Using` presence,
    - `Expr::Resume` presence,
    - missing `parsed_body` (conservative `EffectBoundary`).
  - Transitive propagation implemented:
    - declaration call graph over direct named calls,
    - callers of boundary declarations are marked `EffectBoundary`.
  - Added handler-context metadata flag:
    - `handler_resume_present` (any handler method contains `resume`).
  - Lowerer integration:
    - `lower::try_emit_native_module` now consults `LoweringPlan`;
      native lowering proceeds only when `main` is `DirectStyle`.
  - Added unit tests for planning:
    - pure direct-style classification,
    - `can` boundary + caller propagation,
    - `using` boundary,
    - `resume` boundary + caller propagation,
    - handler-resume presence flag.
  - Quality gates green:
    - `cargo fmt`
    - `cargo check`
    - `cargo test` (268 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 66): `PLAN_RESUME` Step 7.2 complete (evidence payload shape contract)
  - Extended wasm planning metadata with evidence payload types:
    - `EffectId`, `OpId`, `EffectOperationRef`,
    - `DeclarationEvidenceRequirement` (style + required effects + referenced ops),
    - `EvidencePayloadShape` (operation table + declaration requirements).
  - Evidence construction details:
    - effect declarations indexed to stable placeholder IDs (`EffectId` / `OpId`),
    - `can` clauses mapped into declaration required-effect IDs,
    - referenced operations collected from declaration bodies
      (qualified calls + bare-name candidates via op-name index).
  - Lowerer integration:
    - `lower::try_emit_native_module` now reads evidence-shape metadata
      (internal observability/contract usage), while keeping pure-path gating from 7.1.
  - Added planning tests for 7.2 contract:
    - operation-table ID assignment and lookup,
    - declaration evidence requirement contents from `can` + operation references.
  - Plan docs synchronized:
    - `doc/PLAN_RESUME.md`: Step 7.2 marked DONE.
  - Quality gates green:
    - `cargo fmt`
    - `cargo check`
    - `cargo test` (270 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 67): `PLAN_RESUME` Step 7.3 complete (direct-style lowerer path stabilization)
  - Threaded `LoweringPlan` through `lower.rs` native evaluator environment.
  - Enforced direct-style-only declaration evaluation/materialization in:
    - variable-to-callable resolution,
    - named declaration application/evaluation paths.
  - Added lowerer regression tests:
    - native lowering rejects main when transitive call graph includes `can` boundary declaration,
    - native lowering accepts representative pure direct-style function-call subset.
  - Plan docs synchronized:
    - `doc/PLAN_RESUME.md`: Step 7.3 marked DONE.
  - Quality gates green:
    - `cargo fmt`
    - `cargo check`
    - `cargo test` (276 tests passed)
    - `cargo clippy -- -D warnings`
- 2026-03-02 (session 68): Step 7.1-7.3 self-review follow-up
  - Fixed review item #1 (native capability/lower path consistency):
    - `fallback::native_unsupported_reason_kind` now also checks
      `build_lowering_plan(module).is_direct_style("main")`,
      returning `call_target_body_not_native_supported` when `main` is not direct-style.
    - Added regression coverage for `can`-only effect-boundary call graph
      (`tick : Int -> Int can Tick`) to ensure capability checker rejects native path.
  - Deferred review notes to revisit at Step 7 completion:
    - remove or formalize current evidence observability no-op reads
      in `lower::try_emit_native_module` (`let _ = (...)` block),
    - decide deterministic ordering policy for `EvidencePayloadShape::checksum`
      if checksum is promoted beyond debug/observability use.
- 2026-03-02 (session 69): `PLAN_RESUME` Step 7.4 complete (effect-boundary lowering skeleton)
  - Added explicit native lowerer entry contract:
    - `NativeLoweringResult::{Emitted, EffectBoundaryHandoff, NotLowered}`.
  - Added `EffectBoundaryHandoff` metadata payload carrying:
    - `main_style`,
    - `handler_resume_present`,
    - evidence summary (`operation_table_len`, `requirements_len`, `checksum`).
  - `compile_module` now consumes native-lowering result with an explicit
    effect-boundary handoff branch before routing to fallback runtime.
  - Added lowerer regression assertion that effect-boundary call graphs produce
    `EffectBoundaryHandoff` via the new lowerer API.
- 2026-03-02 (session 70): `PLAN_RESUME` Step 7.5 complete (regression + observability hooks)
  - Added internal planning observability API:
    - `LoweringPlan::declaration_lowering_modes()`
    - `DeclarationLoweringMode { declaration_name, style }`
    - output is name-sorted for deterministic inspection.
  - Extended `EffectBoundaryHandoff` payload to include declaration-mode snapshot,
    enabling per-declaration mode inspection at explicit handoff points.
  - Added planning regression coverage:
    - multi-hop transitive boundary propagation (`main -> mid -> fx(can ...)`),
    - declaration-mode snapshot content/ordering check.
  - Added lowerer test assertion that effect-boundary handoff exposes declaration-mode snapshot.
- 2026-03-02 (session 71): `PLAN_RESUME` Step 7.6 complete (Step-7 closeout)
  - Confirmed Step-7 completion criteria:
    - planning metadata + boundary classification implemented with regression coverage,
    - pure-path native lowering remains green,
    - explicit effect-boundary handoff exists in lowering pipeline.
  - Closed previously deferred Step-7 review notes:
    - removed no-op observability reads from `lower::try_emit_native_module_with_handoff`,
    - made `EvidencePayloadShape::checksum` deterministic by folding
      declaration requirements in declaration-name order.
  - Added regression test:
    - checksum remains stable under declaration reordering.
- 2026-03-02 (session 72): Step 7 post-review hardening pass
  - Removed dual native-gating dependency in `compile_module`:
    - native emit now trusts `lower::try_emit_native_module_with_handoff`
      (`NativeLoweringResult::Emitted`) as single source of truth.
  - Improved boundary-handoff diagnostics:
    - if fallback runtime output resolution fails after `EffectBoundaryHandoff`,
      error now includes effect-boundary context + evidence observability summary.
  - Renamed evidence summary primitive from `checksum` to `fingerprint_hint`
    (explicitly non-integrity, observability-only semantics).
  - Planning performance/coverage updates:
    - removed per-node reconstruction of seen-op set in `collect_operation_refs`,
    - added multi-hop propagation + evidence-metadata composite regression test.
- 2026-03-03 (session 73): State sync refresh
  - Updated `doc/STATE.md` summary fields to latest repository state:
    - `Last updated` header,
    - current example list includes `examples/iterator.gb`,
    - Phase-A quality snapshot updated to 281 tests green.
- 2026-03-03 (session 74): `PLAN_RESUME` Step 8 kickoff (8.1-8.3 scaffold)
  - Added Step8 execution-mode gate skeleton in `goby-wasm` lowerer:
    - `EffectExecutionMode` (`PortableFallback` / `TypedContinuationOptimized`),
    - compile-time runtime profile probe via `GOBY_WASM_RUNTIME_PROFILE`
      (`wasmtime` / `wasmer` / `unknown`),
    - first-failure fallback reason reporting (`RuntimeProfileNotSupported`, etc.).
  - Extended effect-boundary handoff observability payload with:
    - selected execution mode,
    - selected-mode fallback reason,
    - compile-time runtime profile,
    - optional typed-continuation IR artifact slot.
  - Added internal typed-continuation IR scaffold (Step 8.2/8.3 base):
    - operation table snapshot,
    - main declaration required-effect IDs,
    - main declaration referenced-operation refs,
    - one-shot resume flag.
  - Added compile-time gate feature:
    - `crates/goby-wasm/Cargo.toml` feature `typed-continuation-optimized` (default off).
  - Updated fallback boundary diagnostics in `compile_module` to include
    Step8 mode-selection context.
  - Validation:
    - `cargo fmt`
    - `cargo test -p goby-wasm` (62 unit + 5 integration tests passed)
- 2026-03-03 (session 75): `PLAN_RESUME` Step 8.4 bridge wiring (mode-aware runtime continuation path)
  - Added mode-aware runtime output resolver entry:
    - `resolve_main_runtime_output_with_mode(..., execution_mode)`,
    - `compile_module` now threads selected boundary handoff mode into runtime output resolution.
  - Added continuation bridge points in `RuntimeOutputResolver`:
    - `begin_handler_continuation_bridge`,
    - `resume_through_active_continuation_bridge`,
    - `finish_handler_continuation_bridge`.
  - `Expr::Resume` path now routes through bridge layer so Step8 optimized mode can
    plug continuation re-entry while preserving current fallback behavior.
  - Error contract preserved across modes:
    - `resume used without an active continuation`,
    - `resume continuation already consumed`,
    - `internal resume token handler mismatch`.
  - Added Step8.4 parity regression tests (`PortableFallback` vs `TypedContinuationOptimized`):
    - resume success path,
    - double-resume one-shot error path,
    - nearest-handler qualified dispatch path.
  - Validation:
    - `cargo fmt`
    - `cargo test -p goby-wasm` (68 unit + 5 integration tests passed)
- 2026-03-03 (session 76): `PLAN_RESUME` Step 8.5 parity oracle hardening
  - Added mode parity oracle in `goby-wasm` tests:
    - compares `stdout` and runtime error kind IDs separately
      (instead of full raw output string only).
  - Added runtime error kind mapping for parity assertions:
    - `continuation_missing`,
    - `continuation_consumed`,
    - `token_handler_mismatch`,
    - `token_stack_mismatch`.
  - Extended Step8 parity coverage for required cases:
    - resume success path,
    - no-resume abortive path,
    - double-resume deterministic error path,
    - nearest/qualified handler dispatch path.
  - Validation:
    - `cargo fmt`
    - `cargo test -p goby-wasm` (68 unit + 5 integration tests passed)
- 2026-03-03 (session 77): Step8.4-8.5 self-review follow-up fixes
  - Added missing Step8.5 parity coverage for nested same-effect nearest-handler dispatch
    (`PortableFallback` vs `TypedContinuationOptimized`).
  - Reduced bridge-path duplication in `RuntimeOutputResolver` by collapsing
    mode branches to single shared token-bridge code paths (still mode-aware).
  - Hardened parity assertions to fail on unmapped runtime error kinds
    (`unknown_runtime_error`) instead of allowing silent parity pass.
  - Validation:
    - `cargo fmt`
    - `cargo test -p goby-wasm` (70 unit + 5 integration tests passed)
- 2026-03-03 (session 78): `PLAN_RESUME` Step 8.6 guardrails kickoff (runtime override kill-switch)
  - Added runtime override kill-switch for mode selection in `goby-wasm` lowerer:
    - env `GOBY_WASM_FORCE_PORTABLE_FALLBACK=1|true|yes|on` forces
      `EffectExecutionMode::PortableFallback` with highest priority.
    - fallback reason now reports `ForcedPortableOverride`.
  - Added lowerer unit test to assert forced override precedence over runtime profile/gate/construct checks.
  - Added integration regression test to assert compile diagnostics surface
    `selected_mode_fallback_reason=Some(ForcedPortableOverride)` when override is active.
  - Updated Step8.6 plan text to document current env knob name.
  - Validation:
    - `cargo fmt`
    - `cargo test -p goby-wasm`
- 2026-03-03 (session 79): `PLAN_RESUME` Step 8.6 performance harness wiring
  - Added explicit Step8.6 performance acceptance harness in `goby-wasm` tests:
    - ignored test `step8_perf_acceptance_resume_heavy_samples`,
    - 3 representative resume-heavy samples,
    - 5 warmup + 30 measured runs per mode/sample,
    - p50/p95 slowdown assertion (`typed <= fallback * 1.03`).
  - Added helper utilities for percentile-based microbenchmark assertions in test module.
  - Updated Step8.6 plan text with concrete harness command.
  - Validation:
    - `cargo fmt`
    - `cargo test -p goby-wasm`
    - `cargo test -p goby-wasm step8_perf_acceptance_resume_heavy_samples -- --ignored --nocapture`
- 2026-03-03 (session 80): Step8 review follow-up polish
  - Addressed medium review items:
    - fixed parity test source typo (`main =` indentation in resume-success case),
    - documented no-resume abortive-path output contract in parity test comments,
    - documented that Step8.5 parity/perf currently run on shared bridge path by design.
  - Addressed low review item:
    - renamed lowerer helper arg `_module` -> `module` for clarity.
  - Validation:
    - `cargo fmt`
    - `cargo test -p goby-wasm`

## 5. Current Example Files

- `examples/hello.gb`
- `examples/basic_types.gb`
- `examples/function.gb`
- `examples/generic_types.gb`
- `examples/import.gb`
- `examples/control_flow.gb`
- `examples/type.gb`
- `examples/effect.gb`
- `examples/iterator.gb`

## 6. Immediate Next Steps (Execution Order)

Wasm Phase A (Phases 0–8) complete. 281 tests green.

Resume checks:
```
cargo check
cargo test
cargo clippy -- -D warnings
```

Execution focus (in order):
1. Standard-library foundation implementation (`doc/PLAN_STANDARD_LIBRARY.md`, Phase A -> E + stdio/print + stdlib-only `@embed` checkpoints).
2. Effect runtime redesign (one-shot deep handlers + selective CPS/evidence passing).
3. `resolve_main_runtime_output` retirement (blocked on effect-native support and remaining unsupported forms).

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
- Real Wasm next milestone: extend native coverage beyond current subset (lambda/HOF + effect runtime),
- Real Wasm next milestone: extend native coverage beyond current subset (effect runtime and remaining unsupported expression forms),
  then retire remaining fallback execution paths.
- REPL / interactive mode.
