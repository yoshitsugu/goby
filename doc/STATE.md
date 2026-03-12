# Goby Project State Snapshot

Last updated: 2026-03-12

This file is a restart-safe snapshot for resuming work after context reset.

## Current Focus

- Active work is again `doc/PLAN.md` Track F: maintainability hardening.
- `Milestone F1` (`goby-wasm` split) is complete.
- `Milestone F2` (`goby-core` typecheck split) is complete.
- `Milestone F3` (`goby-core` parser split) is complete.
- `Milestone F4` (post-extraction cleanup) is complete.
- New focus is the second maintainability pass:
  - `Milestone F6`: split residual responsibilities from `typecheck.rs`
  - `Milestone F7`: decompose `typecheck_check.rs`
  - `Milestone F8`: boundary hardening and post-split cleanup

## Current State

- `goby-wasm` runtime split completed so far:
  - `crates/goby-wasm/src/runtime_value.rs`
  - `crates/goby-wasm/src/runtime_env.rs`
  - `crates/goby-wasm/src/runtime_flow.rs`
  - `crates/goby-wasm/src/runtime_eval.rs`
  - `crates/goby-wasm/src/runtime_resolver.rs`
  - `crates/goby-wasm/src/runtime_dispatch.rs`
  - `crates/goby-wasm/src/runtime_decl.rs`
  - `crates/goby-wasm/src/runtime_exec.rs`
  - `crates/goby-wasm/src/runtime_replay.rs`
- `crates/goby-wasm/src/lib.rs` now mainly holds the public codegen entrypoint,
  fallback orchestration glue, and the largest remaining fallback-runtime concentration point after F1.
- `crates/goby-core/src/typecheck.rs` is now reduced to phase orchestration plus
  residual validation/helpers after the `F2` split, but still owns environment construction,
  annotation validation, and type-conversion helpers that are now planned for extraction in `F6`.
- `F3.1` is landed:
  - `crates/goby-core/src/parser_util.rs` owns shared parser predicates and split helpers.
- `F3.2` is landed:
  - `crates/goby-core/src/parser_top.rs` owns top-level import/embed/type/effect/declaration-header parsing.
- `F3.3` is landed:
  - `crates/goby-core/src/parser_stmt.rs` owns declaration-body statement parsing, multiline `with` / `case` / `if` block handling, handler-body parsing, and statement-oriented binding/assignment splitting.
  - `crates/goby-core/src/parser.rs` now keeps `parse_module` orchestration instead of mixing top-level and statement parsing in one file.
- `F3.4` is landed:
  - `crates/goby-core/src/parser_expr.rs` owns expression parsing, interpolation parsing, application/method parsing, and expression split helpers.
  - `crates/goby-core/src/parser_pattern.rs` owns case/list pattern parsing shared by statement parsing.
  - `crates/goby-core/src/parser.rs` is now a thin public entry layer for `parse_module`, `parse_body_stmts`, and `parse_expr`.
  - `Milestone F4` cleanup is complete:
    - expression-focused parser tests now live in `crates/goby-core/src/parser_expr.rs`
    - list/case-pattern unit tests now live in `crates/goby-core/src/parser_pattern.rs`
    - statement/body parsing tests now live in `crates/goby-core/src/parser_stmt.rs`
    - top-level parser syntax tests now live in `crates/goby-core/src/parser_top.rs`
    - shared parser example-test fixture now lives in `crates/goby-core/src/parser_test_support.rs`
    - `crates/goby-core/src/parser.rs` is now limited to public entrypoints plus parse-module integration/error-span coverage
- `F2.1` is now landed:
  - `crates/goby-core/src/typecheck_env.rs` owns `Ty`, `TypeEnv`, `ResumeContext`,
    effect-map structs, and related internal binding data.
  - `crates/goby-core/src/typecheck.rs` now consumes that module instead of defining those internals inline.
- `F2.2` is now landed:
  - `crates/goby-core/src/typecheck_validate.rs` owns import resolution helpers,
    stdlib-root policy, embed/intrinsic validation, imported effect/type collection,
    and import-backed symbol injection.
  - `crates/goby-core/src/typecheck.rs` now calls that module instead of carrying those helpers inline.
- `F2.3` is now landed:
  - `crates/goby-core/src/typecheck_effect.rs` owns effect declaration validation,
    member `can`-clause validation, effect dependency cycle checks, and effect-map builders.
  - `crates/goby-core/src/typecheck.rs` now consumes those effect-phase helpers instead of defining them inline.
- `F2.4` is now landed:
  - `crates/goby-core/src/typecheck_check.rs` owns expression inference, statement checking,
    resume validation, branch-consistency checks, and type rendering helpers.
  - `crates/goby-core/src/typecheck.rs` now calls that module instead of carrying the checking core inline.
- Design-review follow-up for Track F second pass:
  - `crates/goby-wasm/src/lib.rs` still contains the main `RuntimeOutputResolver` impl and several helper clusters (`flatten_direct_call`, pipeline/string helpers, print-only codegen helper).
  - `crates/goby-core/src/typecheck.rs` still contains env-building, annotation/effect-clause validation, and type-conversion helpers.
  - `crates/goby-core/src/typecheck_check.rs` is now the largest remaining semantic concentration point and is planned to split by checking concern rather than by arbitrary size.
- `F5` progress:
  - `crates/goby-wasm/src/runtime_support.rs` now owns direct-call flattening, selective-import symbol lookup, simple pipeline parsing, and string-expression helpers.
  - `crates/goby-wasm/src/print_codegen.rs` now owns the print-only Wasm emission helper.
  - `crates/goby-wasm/src/runtime_apply.rs` now owns declaration/value-call/binop helper methods extracted from the tail of `RuntimeOutputResolver`.
  - `crates/goby-wasm/src/runtime_expr.rs` now owns expression/value evaluation, imported declaration value resolution, effect-conflict lookup, and continuation-to-value completion helpers.
  - `crates/goby-wasm/src/runtime_unit.rs` now owns unit-position expression execution and side-effect dispatch centered on `execute_unit_expr_ast`.
  - `crates/goby-wasm/src/runtime_entry.rs` now owns `resolve_main_runtime_output*` helper entrypoints and runtime evaluator construction.
  - `Milestone F5` can be treated as complete; `crates/goby-wasm/src/lib.rs` now serves as compile entrypoint/state definition/test host rather than the fallback-runtime dump site.
- `F2.5` is now landed:
  - `typecheck_module_with_context` now sequences explicit validation, checking-state preparation,
    and declaration-body checking phases through dedicated helpers.
  - `Milestone F2` can be treated as complete; next work moves to `parser.rs`.
- `F6.1` is now landed:
  - `crates/goby-core/src/typecheck_build.rs` owns type-environment construction, constructor/effect symbol injection, and global symbol insertion.
  - `crates/goby-core/src/typecheck.rs` now calls that build-phase module instead of carrying environment construction inline.
- `F6.2` is now landed:
  - `crates/goby-core/src/typecheck_annotation.rs` owns declaration/main annotation validation, handler-type validation, `can`-clause parsing helpers, and declaration annotation shape helpers.
  - `crates/goby-core/src/typecheck.rs` now consumes that annotation-phase module instead of carrying annotation parsing inline.
- `F6.3` is now landed:
  - `crates/goby-core/src/typecheck_types.rs` owns type-declaration validation and type-expression to internal-`Ty` conversion helpers.
  - `typecheck_build`, `typecheck_validate`, `typecheck_effect`, and `typecheck_annotation` now reuse that shared conversion layer instead of importing through `typecheck.rs`.
- `F6.4` is now landed:
  - `crates/goby-core/src/typecheck_phase.rs` owns validation/checking phase structs and the sequencing helpers used by `typecheck_module_with_context`.
  - `crates/goby-core/src/typecheck.rs` is now reduced to the public typecheck API, shared error type, and the identifier predicate reused by other phase modules.
- `F7.1` is now landed:
  - `crates/goby-core/src/typecheck_resume.rs` owns resume-context validation, local-env replay for handler bodies, and generic resume-substitution helpers.
  - `crates/goby-core/src/typecheck_check.rs` now consumes that module instead of carrying resume validation inline.
- `F7.2` is now landed:
  - `crates/goby-core/src/typecheck_effect_usage.rs` owns effect-usage checking, handler coverage validation, and required-effect enforcement.
  - `crates/goby-core/src/typecheck_check.rs` now calls that module instead of mixing effect semantics into the general checking file.
- `F7.3` is now landed:
  - `crates/goby-core/src/typecheck_ambiguity.rs` owns ambiguity/name-collision validation and related constructor/tuple-member checks.
  - `crates/goby-core/src/typecheck_branch.rs` owns branch-consistency checking for `if`/`case`/block forms.
- `F7.4` is now landed:
  - `crates/goby-core/src/typecheck_unify.rs` owns type substitution, unification, fresh type-variable instantiation, and shared type-hole diagnostics.
  - `typecheck_resume` and `typecheck_effect_usage` now depend on that common unification layer instead of coupling through one checking phase file.
- `F7.5` is now landed:
  - `crates/goby-core/src/typecheck_check.rs` now keeps `check_expr` and `check_body_stmts` as thin top-level entrypoints over smaller helper layers.
  - statement-sequence checking, per-statement validation, and return-type validation are now explicit internal steps instead of being inlined in one long entry function.
- `F8.1` is now landed:
  - `crates/goby-core/src/typecheck_stmt.rs` owns statement/body checking, local mutability validation, and declared return-type enforcement.
  - `crates/goby-core/src/typecheck_render.rs` owns `Ty` rendering and focused formatting regression tests.
  - `crates/goby-core/src/typecheck_check.rs` is narrowed to expression inference, branch merge helpers, list-spread checks, and expression-shape helpers reused by other checking modules.
- `F8.2` is now landed:
  - expression-inference, tuple-member access, and list-spread regression tests now live beside `crates/goby-core/src/typecheck_check.rs`.
  - `crates/goby-core/src/typecheck.rs` is less of a generic test host and more focused on top-level typecheck integration coverage.
- `F8.3` is now landed:
  - compile/native-lowering regression tests now live in `crates/goby-wasm/src/compile_tests.rs`.
  - `crates/goby-wasm/src/lib.rs` is narrowed toward runtime behavior/parity coverage instead of mixing compile smoke tests with runtime test ownership.
- `F8.4` is now landed:
  - runtime parity/perf helpers now live in `crates/goby-wasm/src/runtime_parity.rs`.
  - `crates/goby-wasm/src/lib.rs` no longer hosts parity outcome decoding, perf sampling, or mode-comparison assertions inline.
- `F8.5` is now landed:
  - runtime-output, stdin-backed output resolution, and output-lock regression tests now live in `crates/goby-wasm/src/runtime_output_tests.rs`.
  - `crates/goby-wasm/src/lib.rs` is narrowed further toward compile smoke tests plus remaining runtime-behavior integration coverage.
- `F8.6` is now landed:
  - resume, continuation replay, nearest-handler parity, and resume perf-acceptance tests now live in `crates/goby-wasm/src/runtime_resume_tests.rs`.
  - `crates/goby-wasm/src/lib.rs` no longer acts as the default host for continuation-path parity coverage.
- `F8.7` is now landed:
  - residual runtime behavior, data-shape replay, and block-evaluation regression tests now live in `crates/goby-wasm/src/runtime_behavior_tests.rs`.
  - `crates/goby-wasm/src/lib.rs` test ownership is now narrowed to compile smoke coverage.
- Runtime model to preserve while refactoring:
  - `Out<T> = Done | Suspend | Escape | Err`
  - `Escape::WithScope { with_id, value }`
  - lexical nearest-handler dispatch
  - parity between fallback runtime behavior and current tests
- Boundary decisions to preserve in F8:
  - `typecheck_check.rs` should stay expression-centric.
  - `typecheck_stmt.rs` should own statement sequencing and return-type enforcement.
  - `typecheck_render.rs` should own user-visible internal `Ty` formatting.
  - module-specific regression tests should live beside their owned implementation when they do not require top-level typecheck integration coverage.
  - `goby-wasm/src/lib.rs` should not regain compile/native-lowering smoke tests that fit dedicated test modules.
  - `goby-wasm/src/lib.rs` should not regain parity/perf helper infrastructure when a focused test-support module can own it.
  - `goby-wasm/src/lib.rs` should not regain broad runtime-output/output-lock regression coverage when a dedicated runtime-output test module can own it.
  - `goby-wasm/src/lib.rs` should not regain broad resume/continuation replay regression coverage when a dedicated runtime-resume test module can own it.
  - `goby-wasm/src/lib.rs` should not regain general runtime behavior/data-shape regression coverage when dedicated runtime test modules can own it.

## Verified

- `cargo fmt`
- `cargo check`
- `cargo test -p goby-core`
- `cargo test -p goby-wasm`
- `cargo test --workspace`

## Next Work

- `F8.1`, `F8.2`, `F8.3`, `F8.4`, `F8.5`, `F8.6`, and `F8.7` are complete.
- Continue `Milestone F8` with:
  - boundary review for remaining large files (`goby-wasm/src/lib.rs`, `typecheck_check.rs`)
  - relocation of any remaining subsystem-specific regression tests to owned modules
  - cleanup of temporary cross-module helpers that are no longer carrying distinct logic

## Notes

- Devflow records live outside the repo:
  `/home/yoshitsugu/.codex/devflow/goby-c372fa22bba4/`
