# Goby Project State Snapshot

Last updated: 2026-03-05

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
- Legacy `using` / top-level `handler ... for ...` syntax is rejected by
  `goby-cli check/run` by default (2026-03-04); migration diagnostics point to
  canonical `with` / `with_handler`.
- String escape sequences: `\n`, `\t`, `\\`, `\"` expanded at parse time via `unescape_string`.
- String interpolation `${...}` is parsed into `Expr::InterpolatedString`.
  Runtime stringifies embedded expression values when materializing the final `String`.
- `case` arm separator: ` -> `; parsed by `split_case_arm` (safe for lambda bodies).
- `case` arms support both inline and block bodies:
  - inline: `pattern -> expr`
  - block: `pattern ->` + deeper-indented statements, where the last expression is the arm value.
- Current implementation `CasePattern` variants:
  `IntLit(i64)`, `StringLit(String)`, `BoolLit(bool)`, `EmptyList`,
  `ListPattern { items, tail }`, `Wildcard`.
- List `case` patterns are implemented:
  - `[]`
  - fixed/prefix list patterns (e.g. `[1]`, `[_, _]`, `[a, b]`)
  - head/tail with rest bind/ignore (e.g. `[a, ..b]`, `[4, ..]`)
  - `_` is wildcard/non-binding in list items and tail binder position (`.._`).
- Native Wasm capability checker still treats list patterns as unsupported,
  so list-pattern `case` executes via fallback runtime path.
- MVP built-ins: `print`, `map`, `fetch_env_var`, `string.split`, `list.join`.
- `print` is an internal runtime-resolved operation; `DefaultStdioPrintHandler`-equivalent behavior
  is compiler/runtime-owned and not required to appear as a user-visible stdlib handler definition.
- `examples/function.gb` expected runtime output (locked):
  - `90`
  - `[30, 40, 50]`
  - `[60, 70]`
  - `something`
  - `15`

## 3. Known Open Decisions

- MVP locked subset remains complete for currently implemented syntax.
- Remaining open point for list `case` patterns:
  - native lowering support (capability checker + native evaluator path).
- Post-MVP open items tracked in `doc/PLAN.md` §3 and §6.
- Post-MVP effect implementation direction is now fixed in `doc/PLAN.md` §2.3:
  - deep handlers with one-shot resumptions,
  - selective CPS + evidence passing lowering,
  - compiled `EffectId`/`OpId` dispatch (no map lookup on hot path),
  - phased Wasm lowering (portable trampoline first, typed-continuation optimization later).
- `PLAN_EFFECT_RENEWAL` completion status:
  - P6 removal is complete (2026-03-04).
  - parser/runtime/typecheck legacy compatibility paths are removed.
  - follow-up work moved to post-MVP tracks in `doc/PLAN.md`.

## 4. Recent Milestones

Recent (detailed):

- 2026-03-05 (session 162): stdlib migration progress toward unified `Iterator.yield`.
  - migrated to `effect Iterator a b / yield : a -> b -> (Bool, b)`:
    - `stdlib/goby/iterator.gb`,
    - `stdlib/goby/string.gb`,
    - `stdlib/goby/int.gb`.
  - runtime and typecheck remain backward-compatible with legacy iterator shapes
    during migration window (as planned in PR3 bridge policy).
  - `stdlib/goby/list.gb` migration is still pending due tuple-result consumption ergonomics
    in source-level control flow (no tuple pattern destructuring yet).
- 2026-03-05 (session 161): runtime iterator contract bridge (PR3 slice) landed.
  - runtime intrinsic `__goby_string_each_grapheme` now supports unified iterator
    contract path:
    - `yield : String -> state -> (Bool, state)` for state-thread mode,
    - `(Bool, Unit)` path for 1-arg count mode with early-stop support.
  - compatibility bridge kept for migration:
    - legacy `yield : String -> Int` one-arg mode remains accepted,
    - legacy `yield_state` state-thread mode remains accepted.
  - runtime evaluator now supports non-empty tuple values (`RuntimeValue::Tuple`)
    required for `(Bool, state)` resume payloads.
  - handler dispatch core now supports multi-argument method binding, enabling
    intrinsic-driven two-arg `yield` calls.
  - regression coverage added:
    - unified iterator state-thread runtime path,
    - unified iterator early-stop runtime path,
    - stdlib-root typecheck acceptance for unified intrinsic usage shape.
- 2026-03-05 (session 160): handler-clause fresh instantiation + type-hole/type-param consistency fixes.
  - effect member type variables are now instantiated freshly per handler clause,
    and clause parameter locals + `resume` expected type share the same instantiated signature.
  - handler clause body checks now receive instantiated parameter types instead of all-`Unknown`.
  - fixed type-hole semantics: `_` in type position is now treated as an anonymous
    hole per occurrence (independent fresh vars), not a shared named type variable.
  - fixed effect-member type validation: unknown/free type variables not declared in
    effect header are now rejected.
  - regression tests added for:
    - unknown effect type parameter rejection,
    - independent `_` holes in multi-arg effect member signatures,
    - independent instantiation across multiple calls to same generic effect op.
- 2026-03-05 (session 159): effect-member generic unification core (PR2 slice) landed.
  - typecheck now uses substitution/unification for effect operation argument checks
    in handler-covered scopes, instead of strict equality-only matching.
  - multi-argument generic constraints are validated as one call-site constraint set
    (for example `a -> a -> Unit` rejects `Int` + `String` mix).
  - `resume` argument validation now accepts generic operation return signatures
    when they unify with actual resume values (and still rejects incompatible shapes).
  - regression tests added for:
    - generic effect op argument acceptance/rejection,
    - generic resume return acceptance/rejection.
- 2026-03-05 (session 158): generic effect header groundwork (PR1 slice) landed.
  - parser now accepts effect declarations with optional type parameters
    (`effect Iterator a b`), while preserving existing `effect Name` syntax.
  - parser now rejects invalid/duplicate effect type parameter names.
  - AST `EffectDecl` now carries `type_params`.
  - conflict-signature comparison for effect declarations now includes type-parameter
    list in addition to member signatures.
  - docs/examples synced:
    - `doc/LANGUAGE_SPEC.md` effect declaration syntax updated.
    - `doc/PLAN.md` Step 0/1 and PR1 checklist marked complete.
    - `examples/effect_generic.gb` added.
- 2026-03-05 (session 157): effect dependency cycle diagnostics hardened.
  - typecheck now rejects cycles in effect-member `can` dependencies
    (including self-cycles and multi-effect cycles).
  - added regression tests for `A -> B -> A` and `A -> A`.
  - docs synced:
    - `doc/LANGUAGE_SPEC.md` §5
    - `doc/PLAN.md` §2.3
- 2026-03-05 (session 156): operation-level effect dependency rules introduced.
  - effect member signatures now support dependency declarations via `can`
    (e.g. `trace : String -> Unit can Print`), validated in typecheck.
  - handler clause bodies now enforce dependency-constrained effect usage:
    allowed = currently covered effects + dependencies declared by the handled op.
  - `with` path now always validates inline handler bodies through the same
    unhandled-effect analysis path.
  - Wasm lowering plan now expands declaration required effects through
    operation-declared dependencies with deterministic topological ordering
    (source-order tie-break via DFS traversal order).
  - spec/planning docs synced:
    - `doc/LANGUAGE_SPEC.md` §5
    - `doc/EFFECT_HANDLER_RESOLUTION.md` §4.1, §6.1
    - `doc/PLAN.md` §2.3 notes
- 2026-03-05 (session 155): callable dispatch diagnostics hardened.
  - fallback runtime now reports deterministic runtime errors for unsupported
    callable argument shapes (`[E-CALLABLE-DISPATCH]`) in:
    - `goby/list.each` callback argument dispatch,
    - callable-parameter declaration calls.
  - added regression tests:
    `reports_callable_dispatch_error_for_list_each_non_callable_callback` and
    `reports_callable_dispatch_error_for_decl_callable_param_non_callable_arg`.
- 2026-03-05 (session 154): effectful callback coverage for
  `goby/list.each` import variants completed.
  - runtime regression tests added for effectful callback dispatch under
    plain/alias/selective imports:
    `resolves_runtime_output_for_effectful_callback_with_list_each_plain_import`,
    `resolves_runtime_output_for_effectful_callback_with_list_each_alias_import`,
    `resolves_runtime_output_for_effectful_callback_with_list_each_selective_import`.
- 2026-03-05 (session 153): lambda-as-function-argument runtime import-variant
  coverage completed for `list.each`.
  - fallback runtime now dispatches `goby/list.each` callbacks for plain import
    (`list.each`), alias import (`l.each`), and selective import (`each`).
  - added runtime regression tests:
    `resolves_runtime_output_for_list_each_with_plain_import`,
    `resolves_runtime_output_for_list_each_with_alias_import`,
    `resolves_runtime_output_for_list_each_with_selective_import`.
- 2026-03-05 (session 152): lambda-as-function-argument import-variant
  typecheck coverage completed.
  - added `goby/list.each` regression tests for import modes:
    plain (`import goby/list`), alias (`import goby/list as l`), selective
    (`import goby/list ( each )`).
- 2026-03-05 (session 151): lambda-as-function-argument runtime parity
  (list.each-style + captured closure slice) completed.
  - fallback runtime now executes curried declaration call chains with callable
    parameters in Unit side-effect position.
  - added runtime regression tests:
    `resolves_runtime_output_for_list_each_style_callback_dispatch` and
    `resolves_runtime_output_for_unit_callback_argument_inline_lambda_with_capture`.
  - `stdlib/goby/list.gb` now includes `iter`/`each` based on effect+handler
    structure (`ListYield` + `with handler ... in`).
- 2026-03-05 (session 150): lambda-as-function-argument runtime parity
  (Unit callback slice) completed.
  - fallback runtime now resolves declaration-local callable parameters for
    inline lambdas and named functions in side-effect position.
  - added regression tests:
    `resolves_runtime_output_for_unit_callback_argument_inline_lambda` and
    `resolves_runtime_output_for_unit_callback_argument_named_function`.
- 2026-03-05: effect-op argument type checking in handler scopes completed.
  - typecheck now rejects mismatched arg types for bare/qualified/method-style/pipeline effect-op calls.
  - regression tests added for qualified and method-style mismatches.
  - `examples/effect.gb` updated to keep example suite type-correct under stricter checks.
- 2026-03-05 (session 149): `Print` operation split finalized (`print`/`println`) with
  embedded default handler behavior and regression coverage.
- 2026-03-05 (sessions 148-146): control-flow typing/runtime parity improvements:
  - `if`/`case` branch type unification,
  - multiline `if`/`case` expression coverage in parser/runtime,
  - `case` arm block execution/typecheck support.
- 2026-03-05 (session 145): list-pattern semantics clarified to prefix matching and matcher refactor.
- 2026-03-04 (sessions 144-140): list-pattern implementation completion + import/type/effect
  selective import semantics stabilization.
- 2026-03-04 (sessions 139-138): Unit-arg call parsing fixes (`f ()`) and `int.parse`
  naming/behavior alignment.

Older milestone records are intentionally compressed out of this file to keep restart
context focused. Full chronology remains in git history.

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

Baseline gates before/after each implementation slice:

```
cargo fmt
cargo check
cargo test
cargo clippy -- -D warnings
```

Execution focus (aligned with `doc/PLAN.md`):

1. Stdlib runtime bridge generalization (reduce symbol-specific fallback branches).
2. Tooling foundation (`fmt`/`lint`/`lsp`) with stable diagnostics surface.
3. Native lowering coverage expansion for remaining unsupported expression/effect paths.

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
- Real Wasm next milestone: extend native coverage beyond current subset (lambda/HOF + effect runtime and remaining unsupported expression forms), then retire remaining fallback execution paths.
- REPL / interactive mode.
