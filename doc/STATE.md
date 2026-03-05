# Goby Project State Snapshot

Last updated: 2026-03-05 (session 149)

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

- 2026-03-05 (session 149): Print effect operation split (`print` + `println`) implemented
  - Stdlib/prelude surface:
    - `effect Print` now exposes both `print : String -> Unit` and
      `println : String -> Unit`.
    - updated `stdlib/goby/stdio.gb` and `stdlib/goby/prelude.gb`.
  - Runtime fallback dispatch:
    - embedded default handler `__goby_embeded_effect_stdout_handler` now
      handles both operations.
    - `Print.print` keeps no-newline behavior.
    - `Print.println` ensures trailing `\n` (adds one only when missing).
  - Tests:
    - added typechecker regression for implicit-prelude `println` call acceptance.
    - added Wasm fallback regressions for explicit-handler precedence and
      embedded-default behavior of `Print.println`.

- 2026-03-05 (session 148): `if` / `case` branch result type unification in typechecker
  - Type inference updates:
    - `check_expr` now merges branch result types for `if` / `case`
      (compatible types are preserved; incompatible known types collapse to `Unknown`).
  - Type checking updates:
    - explicit branch-type consistency check added for nested expressions.
    - mismatched known branch types now fail with diagnostics:
      - `if branch type mismatch: then is ... else is ...`
      - `case branch type mismatch: ... vs ...`
    - checks are applied through declaration bodies, including multiline `case/if` RHS and block expressions.
  - Tests:
    - added regressions for `if` mismatch rejection and `case` mismatch rejection.
  - Validation:
    - targeted `cargo test -p goby-core` regressions for new checks.

- 2026-03-05 (session 147): multiline `case`/`if` AST coverage + effectful branch runtime support
  - Parser (`parse_body_stmts`) now parses multiline control-flow expressions in additional positions:
    - standalone statement form (`case ...` / `if ...`),
    - binding RHS (`x = case ...`, `x = if ...`),
    - mutable binding / assignment RHS (`mut x = ...`, `x := ...`).
  - Runtime fallback (`goby-wasm`) now evaluates effectful branch bodies in unit contexts:
    - `case` / `if` / `Expr::Block` branches can execute side-effect expressions such as `print` and handled effect ops.
    - top-level expression statements now route through AST unit-evaluation before string fallback.
  - User-visible regression resolved:
    - programs shaped like `b = case ...; print b` now run successfully instead of failing with
      `fallback runtime output could not be resolved`.
    - standalone `case` with block-arm `print` calls now runs successfully.
  - Validation:
    - `cargo check`
    - `cargo test`
    - targeted runtime regression runs via `goby-cli run` on `/tmp` copies.

- 2026-03-05 (session 146): `case` arm block implementation completed
  - AST:
    - added expression-level `Expr::Block(Vec<Stmt>)`.
  - Parser:
    - `case` arm now accepts both `pattern -> expr` and `pattern ->` + indented block body.
    - malformed arm-block shapes are rejected (`->` without indented body, comment/blank-only block).
  - Typechecker:
    - block expressions are checked with sequential local-env extension.
    - block expressions must end with an expression (`block expression must end with an expression`).
    - `case` arm checks now recurse with per-arm pattern-binding env.
  - Runtime/codegen:
    - native/fallback evaluators support `Expr::Block` evaluation (sequential statements + tail expr result).
  - Tests/docs/examples:
    - added parser/typecheck/runtime/CLI tests for `case` arm blocks.
    - added `examples/case_arm_block.gb`.
    - updated `doc/LANGUAGE_SPEC.md`, `doc/PLAN.md`, and `examples/README.md`.
  - Validation:
    - `cargo check`
    - targeted tests for parser/typecheck/runtime/CLI arm-block behavior.

- 2026-03-05 (session 145): list-pattern semantics/robustness follow-up
  - Spec wording aligned to actual semantics:
    - `[p1, p2, ...]` is prefix match (`len >= item_count`), not exact-length match.
  - Parser tightened:
    - list-pattern bool items (`[True]`, `[False]`) are now rejected in MVP.
  - Runtime matcher refactor:
    - duplicated int/string list-pattern matching logic extracted into helpers
      to reduce drift risk.
  - Validation:
    - `cargo test`
    - `cargo run -p goby-cli -- run examples/list_case.gb`

- 2026-03-04 (session 144): list `case` pattern completion (`..` syntax)
  - AST:
    - replaced `ListCons` with generalized `ListPattern { items, tail }`.
  - Parser:
    - supports fixed/prefix list patterns and head-literal forms:
      - `[1]`, `[_, _]`, `[4, ..]`, `[a, ..b]`
    - `_` is wildcard (non-binding) in list item and tail positions.
    - duplicate non-wildcard binders are rejected (e.g. `[x, ..x]`).
  - Typechecker:
    - list-pattern binder extension generalized for item binders and tail binders.
    - list-pattern scrutinee validation remains (`List _` or `Unknown`).
  - Runtime fallback:
    - `Expr::Case` matcher supports generalized list-pattern matching for
      `List Int` and `List String`.
  - Docs/examples:
    - updated `doc/LANGUAGE_SPEC.md`, `doc/PLAN.md`, `examples/list_case.gb`.
  - Validation:
    - `cargo test`
    - `cargo run -p goby-cli -- run examples/list_case.gb`

- 2026-03-04 (session 143): list-pattern support scope correction in docs
  - Validation against concrete samples confirmed current subset support only:
    - `[]`
    - `[a, ..b]`
  - Confirmed unsupported examples:
    - `[1]`
    - `[4, ..]`
    - `[_, _]`
  - Updated docs to avoid over-claiming full list-pattern coverage:
    - `doc/LANGUAGE_SPEC.md`
    - `doc/PLAN.md`
    - `doc/STATE.md`
  - Immediate next step:
    - extend `CasePattern` and matcher/typechecker to support fixed-length and head-literal list patterns.

- 2026-03-04 (session 142): list `case` pattern implementation (`[]`, `[head, ..tail]`)
  - AST:
    - added `CasePattern::EmptyList` and `CasePattern::ListCons { head, tail }`.
  - Parser:
    - `split_case_arm` now splits on top-level ` -> ` so list patterns with spaces are accepted,
    - added list-pattern parsing for `[]` and `[head, ..tail]`,
    - list-cons wildcard `_` is non-binding (`[_, ..xs]`),
    - duplicate binders are rejected at parse time (`[x, ..x]`).
    - malformed patterns are rejected.
  - Typechecker:
    - per-arm local bindings for list-cons pattern:
      - `head : a`,
      - `tail : List a`,
    - non-list scrutinee with list patterns now reports a type error.
  - Runtime:
    - fallback runtime `Expr::Case` now supports list-pattern matching with arm-local bindings.
  - Native/lowering:
    - capability checker remains conservative and marks list patterns unsupported.
  - Tests:
    - added parser/typecheck/runtime regressions for list patterns.
  - Examples/docs:
    - added `examples/list_case.gb`,
    - updated `examples/README.md`,
    - synced `doc/PLAN.md` status from planned -> implemented for parser/typechecker slices.
  - Validation:
    - `cargo fmt`
    - `cargo test`

- 2026-03-04 (session 141): list `case` pattern syntax/plan/state lock
  - Updated `doc/LANGUAGE_SPEC.md`:
    - `case` pattern forms now include list patterns:
      - `[]`
      - `[head, ..tail]` (arm-local bindings).
  - Updated `doc/PLAN.md`:
    - added parser/typechecker intent for list patterns,
    - added malformed-pattern rejection targets,
    - locked typing intent for `[head, ..tail]` (`head : a`, `tail : List a`).
  - Updated `doc/STATE.md`:
    - recorded this lock as next implementation slice.
  - Immediate next steps:
    - AST extension (`CasePattern` list variants),
    - parser support for `[]` and `[head, ..tail]`,
    - case-arm env extension in typechecker/runtime evaluators,
    - regression tests for parser/typecheck/runtime.

- 2026-03-04 (session 139): `f ()` Unit-arg call parse fix + naming-convention lock
  - Parser disambiguation updated:
    - `read_line ()` now parses as `Expr::Call` with Unit argument,
      instead of being misclassified as record-constructor syntax.
    - Constructor syntax is now convention-gated to `CamelCase(...)` / `CamelCase (...)`.
  - Naming checks in parser tightened:
    - top-level declaration names must start with lowercase (`_` allowed for intrinsic/internal names),
    - `effect` names must be `CamelCase`,
    - `type` names and type constructors must be `CamelCase`.
  - Added regressions:
    - parser tests for `read_line ()` call shape and `CamelCase` constructor-with-space parsing,
    - parser tests for lowercase effect/type rejection and uppercase top-level declaration rejection,
    - typecheck test for `read_line ()` in `main`,
    - wasm runtime test covering `read_line ()` parse+typecheck+stdin execution path.
  - Verified:
    - `cargo test` all pass after parser + typecheck + wasm updates.

- 2026-03-04 (session 138): `int.parse` rename + overflow-to-error behavior
  - stdlib module path renamed:
    - `stdlib/goby/integer.gb` -> `stdlib/goby/int.gb`.
  - parse API name stabilized for `Int` naming consistency:
    - `int.parse : String -> Int can StringParseError`.
  - Overflow behavior aligned with failure contract:
    - `parse` now uses negative accumulation with boundary checks,
    - out-of-range numbers delegate to `invalid_integer` instead of runtime overflow failure.
  - Docs/examples synced to `goby/int` + `int.parse`.

- 2026-03-04 (session 137): `int.parse` + parse-error effect in stdlib
  - `stdlib/goby/int.gb` now declares:
    - `effect StringParseError`
    - `invalid_integer : String -> Int`
    - `parse : String -> Int can StringParseError`
  - `parse` parses base-10 integer strings with optional leading `-`.
    - success examples: `"42"`, `"-7"`
    - failure examples: `""`, `"-"`, `"12x"`, `"+"`
  - failure path is delegated to handler via `invalid_integer value`.
  - Added sample usage file:
    - `examples/to_integer.gb` (`with_handler` for `invalid_integer`).
  - Updated docs:
    - `doc/LANGUAGE_SPEC.md` runtime notes for `int.parse` contract.
    - `doc/PLAN.md` locked-MVP notes for failure-path policy.

- 2026-03-04 (session 136): prelude `Read` effect runtime bridge (stdin) first implementation
  - `stdlib/goby/prelude.gb` now declares:
    - `effect Read` with:
      - `read : Unit -> String`
      - `read_line : Unit -> String`
    - `@embed Read __goby_embeded_effect_stdin_handler`.
  - Typechecker intrinsic allow-list now accepts
    `__goby_embeded_effect_stdin_handler` for stdlib `@embed`.
  - Wasm fallback runtime now supports embedded `Read` dispatch:
    - `Read.read ()`: returns remaining stdin and consumes it,
    - `Read.read_line ()`: reads one line and trims one trailing terminator
      (`\n`, `\r\n`, `\r`),
    - both ops share one per-execution stdin buffer/cursor.
  - Added regression tests:
    - implicit prelude `can Read` acceptance in typechecker,
    - runtime behavior for `Read.read_line` newline trimming and EOF,
    - runtime interleaving behavior (`read_line` + `read` + EOF reread).
  - Added `examples/read.gb` as minimal stdin usage sample.

- 2026-03-04 (session 135): Unit value literal `()` first implementation
  - Parser now accepts empty tuple syntax `()` as Unit value literal.
  - Typechecker now treats `Expr::TupleLit([])` as `Ty::Unit`.
  - Wasm fallback evaluator now resolves `Expr::TupleLit([])` to `RuntimeValue::Unit`.
  - Added regression tests:
    - parser: `parse_expr(\"()\")` and `parse_expr(\"resume ()\")`,
    - typecheck: `main : Unit -> Unit` with `main = ()`,
    - wasm runtime: `main = ()` resolves to no output.
  - `doc/LANGUAGE_SPEC.md` updated:
    - `()` is canonical Unit value spelling,
    - legacy expression-form `Unit` remains temporarily accepted.

- 2026-03-04 (session 134): syntax-change workflow reminder added (highlight sync)
  - Added explicit workflow rule in `doc/PLAN.md`:
    - syntax changes must include syntax-highlighting impact check
      (`textmate`, `vscode`, `emacs`, `vim`).
  - Added maintenance rule in `doc/README.md`:
    - list of highlight files to review/update when syntax evolves.

- 2026-03-04 (session 133): reserved-token hardening follow-up (parser + spec sync)
  - Parser behavior tightened for `effect` member declarations:
    - malformed member lines now fail fast with parse diagnostics
      (no silent member drop),
    - reserved-token member names are explicitly rejected.
  - Added parser regression tests for:
    - reserved effect member name rejection,
    - malformed effect member signature rejection.
  - `doc/LANGUAGE_SPEC.md` updated with explicit reserved-token policy and
    current reserved-token set.

- 2026-03-04 (session 132): Print effect operation split plan locked (`print` + `println`)
  - Added planning constraint to `doc/PLAN.md`:
    - `Print` effect exposes two ops: `print` (no newline), `println` (ensures trailing newline),
    - runtime/dispatch split and regression requirements are documented.
  - Immediate next step:
    - implement stdlib `effect Print` surface update and runtime dispatch split.

- 2026-03-04 (session 131): Unit value spelling migration plan locked (`Unit` -> `()`)
  - Added implementation constraint to `doc/PLAN.md`:
    - target syntax is `()` as canonical Unit value spelling in expression position,
    - staged migration policy is explicit: accept+warn first, then reject legacy value-form `Unit`.
  - Updated `doc/LANGUAGE_SPEC.md` to explicitly document:
    - current implementation still accepts value-form `Unit`,
    - migration target is `()` for Unit values.
  - Immediate next step:
    - implement parser acceptance for `()` Unit literal with compatibility diagnostics.

- 2026-03-04 (session 130): implicit prelude bootstrap (`goby/prelude`) implementation
  - Added `stdlib/goby/prelude.gb` as the implicit bootstrap module:
    - declares `effect Print`,
    - declares `@embed Print __goby_embeded_effect_stdout_handler`.
  - Typechecker import pipeline now supports implicit prelude loading (when present):
    - effective imports include `goby/prelude` automatically unless explicitly imported,
    - import symbol injection and embedded-default metadata collection both consume
      the same effective-import set.
  - `Print` can-clause acceptance now works via prelude-provided embedded defaults
    (not builtin effect-name table).
  - Wasm runtime fallback now collects embedded defaults from imported stdlib modules
    (including implicit prelude), not only local module embeds.
  - Added regression coverage:
    - implicit prelude acceptance for `main : Unit -> Unit can Print`,
    - explicit context-root prelude behavior,
    - missing-prelude rejection in context-root tests,
    - runtime fallback resolution via implicit prelude metadata.
  - Validation completed:
    - `cargo test -p goby-core`
    - `cargo test -p goby-wasm`
    - `cargo test`

- 2026-03-04 (session 129): `@embed` handler-model migration (`PLAN_EMBED`) implementation
  - Locked syntax transition completed:
    - canonical `@embed` form is now `@embed <EffectName> <HandlerName>`,
    - legacy `@embed effect <EffectName>` is parser-rejected with explicit migration diagnostic.
  - Core metadata model migrated from effect-name list to effect->handler mapping:
    - `EmbedDecl` now stores `effect_name` + `handler_name`,
    - stdlib resolver now exposes `embedded_defaults`.
  - Typechecker updates:
    - `@embed` remains stdlib-path gated and requires in-module `effect` declaration,
    - embedded handler name must start with `__goby_embeded_effect_`,
    - embedded handler target must be a known runtime intrinsic,
    - `main` can-clause allows embedded-default effects from local/imported stdlib metadata,
      while non-main unresolved effect usage remains rejected.
  - Runtime updates (`goby-wasm` fallback resolver):
    - added embedded-default dispatch for `Print.print` via
      `__goby_embeded_effect_stdout_handler`,
    - explicit `with` / `with_handler` dispatch precedence is preserved over embedded defaults.
  - Stdlib/docs sync:
    - `stdlib/goby/stdio.gb` updated to
      `@embed Print __goby_embeded_effect_stdout_handler`,
    - `stdlib/goby/iterator.gb` obsolete embed declaration removed,
    - `examples/README.md` and `doc/PLAN_STANDARD_LIBRARY.md` embed syntax references updated.
  - Embed-default intrinsic set (current):
    - `__goby_embeded_effect_stdout_handler`.
  - Remaining open question:
    - whether additional embedded default handlers beyond stdout should be introduced,
      and if so, which effects/handlers are first candidates.
  - Immediate next steps:
    - wire imported-stdlib embedded-default metadata into runtime fallback path (currently local-module embed metadata only),
    - decide whether `Print` remains a builtin effect name or is fully normalized through stdlib embed defaults.
  - Validation completed:
    - `cargo test -p goby-core`
    - `cargo test -p goby-wasm`

- 2026-03-04 (session 128): stdlib mutable-syntax migration for `mut`/`:=`
  - Updated `stdlib/goby/string.gb` to replace same-scope rebinding updates with
    explicit mutable variables and assignment:
    - `n = ...` update path -> `mut n` + `n := ...`
    - `final = ...` update paths -> `mut final` + `final := ...`
  - This keeps stdlib behavior aligned with the new rule that same-scope
    redeclaration is rejected and mutation must use `:=`.
  - Validation completed:
    - `cargo fmt`
    - `cargo check`
    - `cargo test`
    - `cargo clippy -- -D warnings`

- 2026-03-04 (session 127): mutable variable syntax implementation (`mut` / `:=`)
  - Implemented mutable statement surface in core AST/parser:
    - `mut <name> = <expr>`
    - `<name> := <expr>`
  - Typechecker now enforces mutable-variable rules in declaration bodies:
    - assignment target must be declared,
    - assignment target must be mutable,
    - assignment value type must be compatible with the declared variable type,
    - same-scope redeclaration is rejected with `:=` guidance.
  - Runtime/fallback execution paths support mutation statements.
  - Native-lowering capability keeps mutation forms on fallback path.
  - Tests updated/added:
    - parser body parse coverage for `mut`/`:=`,
    - typechecker acceptance/rejection coverage for mutable assignment rules.
  - Docs sync:
    - `doc/PLAN.md` mutable syntax section moved from planned to implemented.
  - Validation completed:
    - `cargo fmt`
    - `cargo check`
    - `cargo test`
    - `cargo clippy -- -D warnings`

- 2026-03-04 (session 126): `PLAN_EFFECT_RENEWAL` P6 closure docs sync
  - Updated `doc/PLAN.md` wording to remove remaining active-implementation
    references to legacy `using` paths in effect-system sections.
  - Updated `doc/BUGS.md`:
    - marked BUG-001 (legacy `handler ... for ...` unknown effect) as
      obsolete/closed after syntax removal.
  - Updated `PLAN_EFFECT_RENEWAL (archived)`:
    - P6 status promoted from in-progress to complete,
    - Immediate Next Step section replaced with closure note.

- 2026-03-04 (session 125): `PLAN_EFFECT_RENEWAL` P6-R4 cleanup (naming + validation closure pass)
  - Renamed remaining test identifiers/messages that still used `using` terminology
    to canonical `with` / `with_handler` wording in:
    - `crates/goby-core/src/typecheck.rs`
    - `crates/goby-wasm/src/lib.rs`
  - Updated runtime comment wording in `goby-wasm` to remove legacy `using` phrasing.
  - Re-ran mandatory validation flow after cleanup:
    - `cargo fmt`
    - `cargo check`
    - `cargo test`
    - `cargo clippy -- -D warnings`

- 2026-03-04 (session 124): `PLAN_EFFECT_RENEWAL` P6-R3 schema pruning (`handler_declarations` removal)
  - Removed legacy top-level handler schema from core AST:
    - deleted `Module.handler_declarations`,
    - deleted `HandlerDecl` / legacy top-level handler struct exports.
  - Updated parser/module construction and parser tests to reflect schema removal.
  - Updated `goby-wasm` inline handler runtime representation to local type
    (`RuntimeHandlerMethod`), decoupled from removed core legacy schema.
  - Validation completed:
    - `cargo check`
    - `cargo test`

- 2026-03-04 (session 123): `PLAN_EFFECT_RENEWAL` P6-R2 first implementation slice (legacy AST/runtime path shrink)
  - Removed active legacy `using` AST surface:
    - deleted `Stmt::Using` variant from `crates/goby-core/src/ast.rs`.
    - parser continues to reject legacy `using` / top-level `handler ... for ...` with migration hints.
  - Removed legacy typechecker dependencies:
    - removed `module.handler_declarations`-driven handler-method resume checks,
    - removed `using`-scope branches in resume/intrinsic/ambiguity/effect checks,
    - effect coverage now follows canonical `with` / `with_handler` path only.
  - Removed `goby-wasm` legacy statement/runtime paths:
    - deleted `Stmt::Using` handling branches in `planning`, `lower`, `fallback`, and runtime resolver,
    - removed runtime legacy handler-stack lookups via `module.handler_declarations`,
    - simplified continuation-token structure for inline-handler-only flow.
  - Codebase verification for removal target:
    - `rg -n "Stmt::Using|handler_declarations" crates/goby-core crates/goby-wasm`
      now reports parser/schema references only (no active runtime/typecheck usage).
  - Validation completed:
    - `cargo check`
    - `cargo test`
    - `cargo fmt`
    - `cargo clippy -- -D warnings`

- 2026-03-04 (session 122): `PLAN_EFFECT_RENEWAL` immediate-next-step planning refresh
  - Expanded `PLAN_EFFECT_RENEWAL (archived)` §6 from a short note into an ordered
    execution slice (`P6-R2`) with concrete scope:
    - goby-core AST/parser cleanup (`Stmt::Using` legacy path removal direction),
    - goby-core typechecker cleanup (`handler_declarations` legacy dependency removal),
    - goby-wasm runtime/lowering cleanup (inline-handler-only dispatch path),
    - tests/docs migration targets and explicit done criteria.
  - Added follow-up slice definition (`P6-R3`) for final schema pruning and closure.
  - Validation note:
    - planning/documentation update only (no compiler/runtime behavior change in this step).

- 2026-03-04 (session 121): mutable variable feature planning and sample doc sync
  - Added mutable variable implementation plan to `doc/PLAN.md`:
    - reserved keyword `mut`,
    - mutable declaration `mut x = ...`,
    - assignment syntax `x := ...`,
    - required error cases and target diagnostics.
  - Added `examples/mut.gb` and polished its English comments.
  - Validation note:
    - doc/example updates only (no compiler/runtime behavior change in this step).

- 2026-03-04 (session 120): effect-renewal review follow-up fixes (parser/typecheck wording + tab-compat)
  - Parser legacy syntax rejection hardening:
    - legacy `using` detection now handles tab/space after keyword,
    - legacy top-level `handler` rejection now handles tab/space after keyword.
  - Typecheck diagnostics wording aligned with canonical syntax:
    - unhandled-effect messages now reference enclosing `with`/`with_handler` scope
      instead of legacy `using` wording.
  - Added parser regressions:
    - rejects tab-separated legacy `handler` declarations,
    - rejects tab-separated legacy `using` statements.
  - Validation completed:
    - `cargo fmt -- --check`
    - `cargo test -p goby-core`
    - `cargo test -p goby-cli`

- 2026-03-04 (session 119): `PLAN_EFFECT_RENEWAL` P6 cleanup (`goby-wasm` planning legacy fallback removal)
  - Simplified lowering-plan resume signal in `crates/goby-wasm/src/planning.rs`:
    - removed legacy `module.handler_declarations` fallback traversal from
      `handler_resume_present`,
    - detection now relies on expression-level handler usage in declaration bodies
      (`with` / `with_handler`) only.
  - Runtime/lowering tests remain green under canonical syntax fixtures.
  - Validation completed:
    - `cargo fmt -- --check`
    - `cargo clippy -- -D warnings`
    - `cargo test -p goby-wasm`
    - `cargo test`

- 2026-03-04 (session 118): `PLAN_EFFECT_RENEWAL` P6 cleanup (CLI legacy scan removal)
  - Removed redundant CLI-side legacy syntax scan after parser-level rejection landed:
    - deleted `analyze_legacy_syntax_usage` / `count_using_stmts`,
    - removed associated unit test that only validated zero-count behavior.
  - Runtime behavior stays aligned with current model:
    - legacy syntax is rejected at parse time with migration diagnostics,
    - CLI no longer performs a second legacy-count gate on parsed modules.
  - Validation completed:
    - `cargo fmt -- --check`
    - `cargo clippy -- -D warnings`
    - `cargo test -p goby-cli`
    - `cargo test`

- 2026-03-04 (session 117): `PLAN_EFFECT_RENEWAL` P6 parser-level strict rejection + wasm planning alignment
  - Enforced parser-level rejection for legacy effect syntax:
    - top-level `handler ... for ...` now fails parse with migration hint,
    - legacy `using` syntax now fails parse with migration hint.
  - Updated parser/typecheck/CLI tests to match parser-first rejection messaging
    and canonical `with_handler` fixtures.
  - Updated `goby-wasm` lowering-plan resume signal:
    - `handler_resume_present` now detects `resume` inside expression-level
      handler clauses (`with_handler` / `with` handler values),
    - retained compatibility fallback for legacy `handler_declarations` traversal.
  - Validation completed:
    - `cargo test -p goby-core`
    - `cargo test -p goby-cli`
    - `cargo test -p goby-wasm`
    - `cargo fmt -- --check`
    - `cargo clippy -- -D warnings`
    - `cargo test`

- 2026-03-04 (session 116): `PLAN_EFFECT_RENEWAL` P6 prep (legacy fixture reduction, wasm continued)
  - Further reduced `using`-based fixtures in `crates/goby-wasm`:
    - converted additional runtime parity/dispatch fixtures in `src/lib.rs` to
      canonical `with_handler` forms,
    - converted effect-mode gate fixtures in `src/lower.rs` to `with_handler`.
  - Updated fallback reason fixture declarations in `src/lib.rs` from
    `uses_using*` examples to `uses_with_handler*` equivalents while preserving
    expected unsupported-reason assertions.
  - Planning tests:
    - renamed coverage to `marks_with_handler_declaration_as_effect_boundary`,
    - retained legacy top-level handler fixture for
      `records_handler_resume_presence` (still keyed off `handler_declarations`).
  - Validation completed:
    - `cargo test -p goby-wasm`
    - `cargo fmt`
    - `cargo clippy -- -D warnings`
    - `cargo test`

- 2026-03-04 (session 115): `PLAN_EFFECT_RENEWAL` P6 prep (legacy fixture reduction, continued)
  - Further migrated `goby-wasm` fixtures to canonical `with_handler` syntax:
    - typed/fallback parity cases,
    - nearest-handler dispatch parity cases,
    - resume success/abortive/double-resume runtime samples.
  - Migrated effect-boundary gating tests in `goby-wasm` `lower.rs` from
    legacy `handler ... for`/`using` to `with_handler`.
  - Updated planning test coverage wording and fixture style where possible
    (`marks_with_handler_declaration_as_effect_boundary`).
  - Kept one planning regression fixture on legacy top-level handler
    (`records_handler_resume_presence`) because current signal source is
    still `handler_declarations`.
  - Validation completed:
    - `cargo test -p goby-wasm`
    - `cargo test`

- 2026-03-04 (session 114): `PLAN_EFFECT_RENEWAL` P6 prep (goby-wasm test fixture migration)
  - Migrated representative legacy effect fixtures in `crates/goby-wasm/src/lib.rs`
    to canonical `with_handler` form, including:
    - bare/qualified/pipeline effect dispatch tests in main body,
    - nearest-handler precedence tests,
    - intrinsic each-grapheme runtime tests,
    - positional constructor dispatch and resume runtime path tests.
  - Legacy-path runtime coverage remains for selected regression scenarios
    (for example inline-vs-legacy precedence checks), pending parser/runtime
    removal phase.
  - Validation completed:
    - `cargo test -p goby-wasm`

- 2026-03-04 (session 113): `PLAN_EFFECT_RENEWAL` P6 prep (goby-core test fixture migration)
  - Migrated `crates/goby-core/src/typecheck.rs` effect-related fixtures from
    legacy top-level handlers/`using` to canonical `with_handler` in most cases:
    - effect-op coverage tests,
    - effectful-call coverage tests,
    - effect-op argument type tests,
    - stdlib intrinsic state-thread fixture.
  - Kept two `resume_potential_multi_shot` regression tests on legacy handler
    declarations temporarily, because current static multi-resume rejection still
    depends on that path.
  - Validation completed:
    - `cargo test -p goby-core`
    - `cargo test`

- 2026-03-04 (session 112): `PLAN_EFFECT_RENEWAL` P6 prep (stdlib migration off legacy syntax)
  - Migrated `stdlib/goby/string.gb` from legacy top-level handlers + `using`
    to canonical `with_handler ... in ...` usage in:
    - `grapheme_count`
    - `split_with_empty_delimiter`
    - `split_with_single_delimiter`
  - This removes stdlib parser dependency on:
    - top-level `handler ... for ...`
    - `using` statements
  - Validation completed:
    - `cargo test -p goby-core`
    - `cargo test`

- 2026-03-04 (session 111): `PLAN_EFFECT_RENEWAL` P6 first slice (CLI default rejection)
  - Removed legacy syntax warn/deny mode split from CLI:
    - removed warning mode and `GOBY_LEGACY_EFFECT_SYNTAX` gating behavior,
    - `goby-cli check/run` now fail by default when legacy syntax usage is detected.
  - Updated CLI regression coverage:
    - legacy syntax now fails in default mode for both `check` and `run`,
    - legacy env var presence no longer changes acceptance behavior.
  - Documentation sync:
    - `doc/PLAN.md`, `PLAN_EFFECT_RENEWAL (archived)`, and
      `doc/EFFECT_RENEWAL_MIGRATION.md` now describe default rejection status.
  - Validation completed:
    - `cargo test -p goby-cli`
    - `cargo test -p goby-core`

- 2026-03-04 (session 110): state snapshot refresh (renewal progress + next focus sync)
  - Refreshed restart snapshot after `PLAN_EFFECT_RENEWAL` P4/P5 progress:
    - P4 examples/docs migration landed (`effect.gb`, `iterator.gb`, docs sync),
    - P5 compatibility layer landed in CLI:
      - warning mode by default for legacy `handler ... for ...` and `using`,
      - `GOBY_LEGACY_EFFECT_SYNTAX=deny` rejects legacy syntax.
  - Updated execution focus to reflect current phase boundary:
    - next primary work is P6 migration/removal preparation using deny mode rollout path.
  - No additional behavior change in this snapshot-only update.

- 2026-03-04 (session 109): `PLAN_EFFECT_RENEWAL` P5 deny mode for legacy syntax
  - Added CLI legacy syntax mode switch via environment variable:
    - `GOBY_LEGACY_EFFECT_SYNTAX=deny`
      rejects legacy `handler ... for ...` and `using` usage with migration hint.
    - default behavior remains warning mode.
  - Refactored legacy syntax detection into shared usage analysis:
    - counts both top-level legacy handlers and nested `using` occurrences.
  - Added regression coverage:
    - unit test for mode resolution from env,
    - integration test for deny-mode failure on `check`.
  - Validation (mandatory flow) completed:
    - `cargo fmt`
    - `cargo clippy -- -D warnings`
    - `cargo test`

- 2026-03-04 (session 108): `PLAN_EFFECT_RENEWAL` P5 warning coverage expansion (`run`)
  - Extended CLI legacy-warning regression coverage to `run` path:
    - added integration test asserting `run` also emits legacy warnings for
      `handler ... for ...` and `using` syntax.
  - This keeps warning behavior consistent across both CLI entrypoints:
    - `check`
    - `run`
  - Validation (mandatory flow) completed:
    - `cargo fmt`
    - `cargo clippy -- -D warnings`
    - `cargo test`

- 2026-03-04 (session 107): `PLAN_EFFECT_RENEWAL` P5 first implementation (legacy syntax warnings)
  - Added legacy syntax warnings in `goby-cli check/run` success path:
    - warns when top-level legacy `handler ... for ...` declarations are present,
    - warns when legacy `using` statements are present (includes counted usages),
    - warning text points to `doc/EFFECT_RENEWAL_MIGRATION.md`.
  - Added CLI regression coverage:
    - unit test for nested `using` statement counting,
    - integration test asserting warning lines are emitted for legacy syntax input.
  - Validation (mandatory flow) completed:
    - `cargo fmt`
    - `cargo clippy -- -D warnings`
    - `cargo test`

- 2026-03-04 (session 106): `PLAN_EFFECT_RENEWAL` P5 kickoff (migration guide draft)
  - Added migration mapping document:
    - `doc/EFFECT_RENEWAL_MIGRATION.md`
      (`handler ... for` / `using` -> `handler` value + `with` / `with_handler`).
  - Linked migration guide from planning docs:
    - `PLAN_EFFECT_RENEWAL (archived)` P5 section,
    - `doc/PLAN.md` effect-renewal migration policy subsection.
  - No runtime/compiler behavior change in this step.
  - Validation (mandatory flow) completed:
    - `cargo fmt`
    - `cargo clippy -- -D warnings`
    - `cargo test`

- 2026-03-04 (session 105): `PLAN_EFFECT_RENEWAL` P4 iterator capture example hardening
  - Updated `examples/iterator.gb` to demonstrate lexical capture in handler values:
    - local `prefix` binding is captured inside `with_handler` clause body,
    - sample output now shows transformed values (`tick:a`, `tick:b`, `tick:c`).
  - Synced example docs and runtime lock expectation:
    - `examples/README.md` output bullets updated,
    - `goby-wasm` `locks_runtime_output_for_iterator_gb` expectation updated.
  - Validation (mandatory flow) completed:
    - `cargo fmt`
    - `cargo clippy -- -D warnings`
    - `cargo test`

- 2026-03-04 (session 104): `PLAN_EFFECT_RENEWAL` P4 doc sync (`PLAN.md` canonical wording)
  - Updated `doc/PLAN.md` MVP summary and effect-system wording to match renewal direction:
    - canonical handler application is `with` / `with_handler`,
    - legacy `using` is explicitly marked as temporary compatibility syntax,
    - effect coverage wording now refers to enclosing handler scope (with compatibility note).
  - No compiler/runtime behavior changes in this step (documentation alignment only).
  - Validation (mandatory flow) completed:
    - `cargo fmt`
    - `cargo clippy -- -D warnings`
    - `cargo test`

- 2026-03-04 (session 103): `PLAN_EFFECT_RENEWAL` P4 iterator example migration
  - Migrated `examples/iterator.gb` from legacy top-level `handler ... for` + `using`
    to canonical `with_handler ... in ...` form.
  - To keep current typecheck compatibility for handler-expression op resolution,
    the sample now declares local `effect Iterator` instead of relying on imported
    effect metadata for handler-expression validation.
  - Updated examples index text:
    - `examples/README.md` now describes `iterator.gb` as a `with_handler` sample.
  - Validation (mandatory flow) completed:
    - `cargo fmt`
    - `cargo clippy -- -D warnings`
    - `cargo test`

- 2026-03-04 (session 102): `PLAN_EFFECT_RENEWAL` P4 first step (effect example migration start)
  - Migrated `examples/effect.gb` away from legacy `using` in executable `main` flow:
    - uses handler-value model (`h = handler ...`) with `with h in ...`,
    - keeps inline sugar example (`with_handler ... in ...`) for canonical syntax guidance.
  - Kept example runtime-stable under current fallback constraints:
    - `cargo run -p goby-cli -- check examples/effect.gb` passes,
    - `cargo run -p goby-cli -- run examples/effect.gb` executes and prints expected output.
  - Verified renewal runtime regressions still pass after example migration:
    - `cargo test -p goby-wasm with_handler_`.

- 2026-03-04 (session 101): `PLAN_EFFECT_RENEWAL` P3 runtime hardening (capture + precedence parity)
  - `goby-wasm` inline handler runtime model now captures lexical evaluation context:
    - `handler` expression values retain captured `RuntimeLocals` and local callable bindings,
    - handler method execution starts from captured context before binding operation params.
  - Added runtime parity/precedence regressions:
    - `with_handler_captures_lexical_local_in_runtime` (lexical capture),
    - `nested_with_handler_prefers_nearest_inline_handler` (nearest-handler rule),
    - `inline_handler_overrides_legacy_using_handler` (inline-first precedence),
    - `with_handler_dispatches_qualified_effect_call_in_runtime` (qualified dispatch in `with`).
  - Validation:
    - `cargo test -p goby-wasm`
    - `cargo check`
    - `cargo clippy -- -D warnings`

- 2026-03-04 (session 100): `PLAN_EFFECT_RENEWAL` P3 runtime first step (`with` / inline handler execution in fallback runtime)
  - `goby-wasm` fallback runtime now evaluates renewal AST directly for:
    - `Expr::Handler` as runtime handler value,
    - `Expr::With` execution with lexical push/pop of active inline handlers,
    - handler dispatch lookup precedence: nearest inline handler first, then legacy named handlers.
  - Inline-dispatch plumbing added:
    - runtime-side inline handler representation derived from `HandlerClause`,
    - resume-token bridge accepts inline-dispatch tokens without legacy handler-index mismatch errors.
  - Runtime evaluator value model extended with `RuntimeValue::Unit` so handler bodies like
    `resume Unit` evaluate successfully in the AST runtime path.
  - Added and validated runtime regressions:
    - `with_handler_dispatches_effect_operation_in_runtime`
    - `with_handler_variable_dispatches_effect_operation_in_runtime`
  - Validation:
    - `cargo test -p goby-wasm`
    - `cargo check`

- 2026-03-04 (session 99): `PLAN_EFFECT_RENEWAL` P2 hardening (negative diagnostics coverage)
  - Added additional typecheck regressions for renewal error paths:
    - ambiguous operation in `with_handler` clause (same op name in multiple effects) is rejected,
    - `with <expr>` rejects non-handler expressions with explicit diagnostic.
  - Kept diagnostic assertions resilient to wording drift while checking key semantic markers
    (`ambiguous`, operation name, handler-value requirement).
  - Validation:
    - `cargo check`
    - `cargo test -p goby-core`

- 2026-03-04 (session 98): `PLAN_EFFECT_RENEWAL` P2 second slice (`Handler(...)` type semantics)
  - `Handler(...)` type annotations now have semantic validation:
    - must include at least one effect,
    - arguments must be identifier effect names,
    - unknown effects in `Handler(...)` are rejected.
  - Added `Ty::Handler { covered_ops }` compatibility bridge:
    - declaration return-type checking now uses `are_compatible` (not raw `==`),
    - `Handler(E1, E2, ...)` compares against handler-value covered op sets
      with order-insensitive effect-list semantics.
  - Added Handler-type regressions:
    - accept `Unit -> Handler(Log)` with matching handler value,
    - accept order-insensitive `Handler(Env, Log)` matching,
    - reject mismatched handler effect set,
    - reject unknown effect in `Handler(...)` annotation.
  - Validation:
    - `cargo check`
    - `cargo test -p goby-core`

- 2026-03-04 (session 97): `PLAN_EFFECT_RENEWAL` P2 first slice (`with` effect coverage + handler-op diagnostics)
  - Typechecker now performs `with`-body effect coverage using handler-expression-derived covered ops:
    - `with_handler ... in ...` and `with <handler-var> in ...` both feed coverage into body checks.
  - Added strict handler-expression op resolution diagnostics:
    - unknown op in handler expression => compile-time error,
    - ambiguous op across multiple effects => compile-time error message aligned with renewal plan wording.
  - Added internal `Ty::Handler { covered_ops }` to carry handler values through local bindings.
  - Added compact reserved-keyword enforcement in parser for renewal keywords:
    - `resume`, `with`, `with_handler`, `in`, `handler`, `effect`.
  - Added typecheck regressions:
    - accept effect op calls in `with_handler` body handling,
    - accept `with <handler-var> in ...` handling,
    - reject unknown operation in handler expression.
  - Validation:
    - `cargo check`
    - `cargo test -p goby-core`

- 2026-03-04 (session 96): `PLAN_EFFECT_RENEWAL` P1 parser/AST kickoff
  - `goby-core` AST extended with renewal syntax nodes:
    - `Expr::Handler { clauses }`,
    - `Expr::With { handler, body }`,
    - `HandlerClause { name, params, body, parsed_body }`.
  - `goby-core` parser support added for:
    - handler-expression binding form: `x = handler ...`,
    - `with <handler_expr> in ...`,
    - `with_handler ... in ...` sugar (parsed as `Expr::With` + inline `Expr::Handler`).
  - Type parser (`types.rs`) now accepts compact type-application form used by renewal:
    - `Handler(Env, Log)` and `Handler(Env,Log)` parse equivalently.
  - Regression tests added:
    - parser tests for handler-expression binding / `with` / `with_handler`,
    - type parser tests for compact `Handler(...)` syntax spacing variants.
  - Compatibility follow-up:
    - `goby-wasm` and `goby-core` exhaustiveness updated for new `Expr` variants.
    - current typechecker behavior for new renewal nodes is intentionally conservative
      (`Ty::Unknown` / no-op traversal) until P2 type rules land.
  - Validation:
    - `cargo check`
    - `cargo test -p goby-core`

- 2026-03-04 (session 95): `PLAN_EFFECT_RENEWAL` P0 sync into `doc/PLAN.md`
  - Added canonical effect-renewal lock section in `doc/PLAN.md` §2.3:
    - `handler` as expression/value with lexical capture,
    - `Handler(E...)` type model (order-insensitive multi-effect equivalence),
    - `with <handler> in <body>` + `with_handler ... in ...` sugar,
    - untyped clause headers (`op x -> ...`) with effect-signature-driven typing,
    - ambiguity conflict rejection + canonical diagnostic wording,
    - one-shot `resume`/double-resume rejection policy,
    - reserved keyword set (`with`, `with_handler`, `in`, `handler`, `effect`),
    - aggressive legacy bridge policy (short warning window then default rejection).
  - `PLAN_EFFECT_RENEWAL` first step (P0 spec synchronization) is now complete.

- 2026-03-04 (session 94): effect renewal planning sync (`PLAN_EFFECT_RENEWAL`) and priority lock
  - Replaced prior inline-clause `with` migration framing with handler-value model:
    - `handler` as expression/value with lexical capture,
    - `with <handler> in <body>` as canonical application form,
    - `with_handler ... in ...` as inline sugar.
  - Locked decisions recorded in `PLAN_EFFECT_RENEWAL (archived)`:
    - reject double `resume` (keep one-shot contract),
    - handler clause headers remain untyped (`op x -> ...`), with typing sourced from `effect` declarations,
    - `Handler(E1,E2)` and `Handler(E1, E2)` are equivalent (whitespace-insensitive parsing),
    - ambiguity diagnostic style aligned with existing typecheck wording (`... is ambiguous ...`),
    - aggressive legacy migration policy (short bridge phase, then legacy rejected by default).
  - Execution priority updated: effect renewal work is now the highest-priority track.

- 2026-03-03 (session 93): ExtraStep C C4 phase-1 wiring (Iterator state-thread path):
  - Added `Iterator.yield_state` contract and `GraphemeState` shape in `stdlib/goby/iterator.gb`.
  - Extended runtime intrinsic `__goby_string_each_grapheme` with state-thread mode:
    - one-arg mode unchanged (`yield` dispatch, returns grapheme count),
    - two-arg mode dispatches `yield_state` and threads resumed state value.
  - Reworked `stdlib/goby/string.gb` `split` implementation toward iterator-driven flow:
    - empty-delimiter and single-grapheme-delimiter paths now use handler-driven state threading,
    - multi-grapheme delimiter still falls back to runtime `string.split` (C4/C5 still open).
  - Added regression tests:
    - `goby-core`: stdlib acceptance for two-arg grapheme intrinsic state-thread mode.
    - `goby-wasm`: runtime output test for two-arg grapheme intrinsic state-thread mode.
  - Known gap:
    - `stdlib/goby/string.gb` still depends on runtime `string.split` for multi-grapheme delimiter.
    - `List` field types in record declarations are not yet accepted by isolated stdlib module
      typecheck (`check stdlib/goby/string.gb`), so this remains a tracked type-system gap.

- 2026-03-03 (session 92): ExtraStep C C2/C3 completion:
  - `doc/PLAN_STANDARD_LIBRARY.md` ExtraStep C now has locked split semantics and grapheme policy.
  - Added intrinsic `__goby_string_each_grapheme` to typechecker known intrinsic set.
  - Expanded typechecker/runtime intrinsic set with:
    - `__goby_list_push_string`.
  - Runtime bridge supports Unicode grapheme segmentation + `Iterator.yield` dispatch
    (`__goby_string_each_grapheme` returns yielded grapheme count).
  - String equality uses language operator `==` (`String == String -> Bool`) rather than
    a dedicated `__goby_string_eq` intrinsic.
  - Added regression tests in `goby-core` (stdlib-root intrinsic acceptance) and `goby-wasm`
    (runtime output for grapheme iteration / string equality / list push).
  - stdlib `string.split` migration and runtime builtin retirement remain pending (C4/C5).

- 2026-03-03 (session 90): string interpolation implementation landed end-to-end:
  - `Expr::InterpolatedString` + `InterpolatedPart` added in `goby-core` AST.
  - Parser supports `${ expr }` in string literals (including quoted strings inside interpolation expressions).
  - Typechecker treats interpolated literals as `String` and recursively validates embedded expressions.
  - Wasm runtime/native evaluator now evaluates interpolation segments and stringifies values.
  - Added parser/typechecker/runtime regression tests for interpolation.

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
- 2026-03-03 (session 81): Step8 continuation bridge divergence + profile evidence
  - Runtime bridge divergence (Step8 core progress):
    - `PortableFallback` and `TypedContinuationOptimized` now use distinct
      continuation token stacks in `RuntimeOutputResolver`:
      - fallback: `resume_tokens: Vec<ResumeToken>`,
      - typed: `optimized_resume_tokens: Vec<OptimizedResumeToken>`.
    - begin/resume/finish bridge points now dispatch to mode-specific token
      paths while preserving identical runtime error contract.
  - Lowerer/profile evidence hardening:
    - `lower` tests now derive expected runtime profile from compile-time
      `GOBY_WASM_RUNTIME_PROFILE` (unknown/wasmtime/wasmer aware).
    - Added successful test runs with compile-time profile builds:
      - `GOBY_WASM_RUNTIME_PROFILE=wasmtime ...`,
      - `GOBY_WASM_RUNTIME_PROFILE=wasmer ...`.
  - Validation:
    - `cargo fmt`
    - `cargo test -p goby-wasm`
    - `GOBY_WASM_RUNTIME_PROFILE=wasmtime cargo test -p goby-wasm lower::tests::native_lowerer_rejects_main_when_call_graph_contains_effect_boundary -- --exact`
    - `GOBY_WASM_RUNTIME_PROFILE=wasmer cargo test -p goby-wasm lower::tests::native_lowerer_rejects_main_when_call_graph_contains_effect_boundary -- --exact`
- 2026-03-03 (session 82): Step8.6 profile-matrix full suite evidence
  - Executed full `goby-wasm` test suite under compile-time runtime profiles:
    - `GOBY_WASM_RUNTIME_PROFILE=wasmtime cargo test -p goby-wasm`,
    - `GOBY_WASM_RUNTIME_PROFILE=wasmer cargo test -p goby-wasm`.
  - Results:
    - both profile runs passed (`71 passed, 0 failed, 1 ignored` unit tests + `6 passed` integration tests).
- 2026-03-03 (session 83): Open-question sync + runtime resume diagnostics hardening
  - Open Question #1 decision synchronized in plans:
    - no explicit `discontinue` syntax in current phase,
    - abortive behavior remains "not calling `resume`",
    - `discontinue` tracked as deferred future extension in `doc/PLAN.md`.
  - Strengthened runtime resume misuse diagnostics with explicit error IDs + hints:
    - `[E-RESUME-MISSING]`,
    - `[E-RESUME-CONSUMED]`,
    - `[E-RESUME-HANDLER-MISMATCH]`,
    - `[E-RESUME-STACK-MISMATCH]`.
  - Updated parity error-kind extraction to recognize error IDs (while remaining
    backward compatible with legacy message prefixes).
  - Updated resume runtime tests to assert error-ID presence instead of brittle full-string equality.
  - Validation:
    - `cargo fmt`
    - `cargo test -p goby-wasm`
- 2026-03-03 (session 84): Open Question #2 decision + conservative resume local inference
  - Decision: `resume` return type inference adopts conservative middle path:
    - infer local binding type only when handler operation result type is
      non-generic and directly known,
    - keep generic/complex cases as `Ty::Unknown`.
  - `goby-core` typechecker update:
    - binding inference in resume-aware stmt walk now uses resume context for
      non-generic cases (`infer_binding_ty_with_resume_context`),
    - added generic-type detection guard (`ty_contains_type_var`) to avoid
      premature inference in generic contexts.
  - Added unit tests:
    - non-generic resume context infers concrete binding type,
    - generic resume context keeps `Ty::Unknown`.
  - Plan docs synchronized:
    - `doc/PLAN.md` and `doc/PLAN_RESUME.md` now record this inference policy.
  - Validation:
    - `cargo fmt`
    - `cargo test -p goby-core`
- 2026-03-03 (session 85): Open Question #1 lightweight static guard + plan sync
  - `goby-core` typechecker update:
    - added conservative static multi-shot guard in handler methods:
      - counts syntactic `resume` expressions in each handler method body,
      - rejects methods with more than one `resume` via
        `resume_potential_multi_shot`.
    - scope is intentionally lightweight; precise control-flow-sensitive checks
      remain deferred.
  - Added regression tests:
    - rejects sequential double-`resume` in a handler method,
    - rejects branch-separated double-`resume` conservatively in this phase.
  - Plan/docs synchronization:
    - `doc/PLAN.md`: typecheck contract now includes
      `resume_potential_multi_shot` and lightweight-policy note.
    - `doc/PLAN_RESUME.md`: Open Questions section resolved with this policy.
    - `doc/PLAN_STANDARD_LIBRARY.md`: status updated to
      `Step 0-12 complete; ExtraStep A/B pending`.
  - Validation:
    - `cargo fmt`
    - `cargo test -p goby-core`
- 2026-03-03 (session 86): `PLAN_RESUME` archive + future-work consolidation
  - Archived `doc/PLAN_RESUME.md` to `PLAN_RESUME (archived)` after Step0-8 completion.
  - Consolidated remaining `resume` future tasks into `doc/PLAN.md`:
    - control-flow-sensitive multi-shot static analysis (post current conservative guard),
    - possible explicit `discontinue` as a future extension proposal,
    - default mode switch policy remains a dedicated follow-up decision.
  - Kept `doc/PLAN.md` as the active design source of truth.
- 2026-03-03 (session 87): `PLAN_STANDARD_LIBRARY` ExtraStep A progress (A1-A10 complete)
  - `goby-core` parser updated:
    - canonical embed syntax `@embed <EffectName>` is accepted,
    - legacy `@embed effect <EffectName>` is kept as temporary compatibility syntax.
  - `goby-core` typechecker updated:
    - embedded effect must be declared in the same module (`effect X` required for `@embed X`),
    - existing stdlib-root-only path restriction remains enforced.
  - `stdlib/goby/stdio.gb` migrated to canonical model:
    - `effect Print` declaration + `@embed Print`.
  - Resolver/typecheck/parser tests updated for canonical syntax and new validation behavior.
  - `doc/PLAN_STANDARD_LIBRARY.md` ExtraStep A checklist updated:
    - A1-A10 marked complete,
    - A11/A12 kept as final doc+gate closeout steps.
  - Validation in this session:
    - `cargo fmt`
    - `cargo check`
    - `cargo test -p goby-core`
- 2026-03-03 (session 88): `PLAN_STANDARD_LIBRARY` ExtraStep A closeout (A11-A12 complete)
  - Docs synchronized for landed behavior:
    - `doc/PLAN.md` now records canonical embed form and in-module effect requirement
      (`@embed <EffectName>` + local `effect <EffectName>`),
    - `doc/PLAN_STANDARD_LIBRARY.md` initial embed model updated to canonical form.
  - ExtraStep A checklist status:
    - A1-A12 all complete.
  - Final quality gates passed:
    - `cargo fmt`
    - `cargo check`
    - `cargo test`
    - `cargo clippy -- -D warnings`
- 2026-03-03 (session 89): `PLAN_STANDARD_LIBRARY` ExtraStep B complete (intrinsic bridge)
  - Locked intrinsic bridge set:
    - `__goby_string_length : String -> Int`
    - `__goby_env_fetch_env_var : String -> String`
  - `goby-core` typechecker updates:
    - intrinsic symbol types injected into type environment for stdlib bridge calls,
    - user-space `__goby_*` declaration/call usage rejected in context-aware checks,
    - unknown `__goby_*` names in stdlib modules rejected with explicit diagnostics.
  - `goby-wasm` runtime updates:
    - evaluator/lowering bridge supports intrinsic execution for string length and env fetch.
  - Stdlib module migration:
    - `stdlib/goby/string.gb`: `length` now calls `__goby_string_length`,
    - `stdlib/goby/env.gb`: `fetch_env_var` now calls `__goby_env_fetch_env_var`.
  - Test coverage additions:
    - `goby-core`: intrinsic namespace policy (allow/reject/unknown) regressions,
    - `goby-wasm`: runtime-output tests for both intrinsic calls.
  - Final quality gates passed:
    - `cargo fmt`
    - `cargo check`
    - `cargo test`
    - `cargo clippy -- -D warnings`
- 2026-03-04 (session 140): import semantics update (type/effect selective import + eager ambiguity error)
  - `import` behavior clarified and implemented as value/type/effect aware.
    - selective import now accepts mixed names: values, type names, and effect names.
  - Typechecker updates:
    - import validation now checks selective names against module exports + type names + effect names,
    - imported type names are included in type declaration validation,
    - imported effect declarations/names respect selective lists,
    - import-origin name resolution now fails eagerly when globals become ambiguous
      (no deferred "only on use" behavior).
  - Stdlib resolver metadata extended with exported type names for validation.

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
cargo fmt
cargo check
cargo test
cargo clippy -- -D warnings
```

Validation policy (every implementation step):
- Always run `cargo fmt`, `cargo clippy -- -D warnings`, and `cargo test`.

Execution focus (in order):
1. `PLAN_EFFECT_RENEWAL` implementation (highest priority):
   - P0-P4 complete (spec + parser/AST + typecheck + runtime parity + examples/docs migration),
   - P5 complete (compatibility/deprecation window closed in CLI),
   - P6 in progress: remove legacy parser/runtime paths and remaining internal dependencies.
2. Effect runtime redesign follow-up (one-shot deep handlers + selective CPS/evidence passing), including
   post-`PLAN_RESUME` items now tracked in `doc/PLAN.md`.
3. `resolve_main_runtime_output` retirement (blocked on effect-native support and remaining unsupported forms).
4. Standard-library intrinsic retirement planning (`__goby_*` debt reduction path) after effect/runtime feature expansion.

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
