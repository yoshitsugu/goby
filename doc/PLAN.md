# Goby Language Plan (Draft)

This document tracks:

- what is already visible from examples,
- what must be fixed for MVP,
- what can be postponed.

## 1. Confirmed in Current Draft

Based on `examples/*.gb`:

- `.gb` source file extension
- Function definition syntax: `name args = expr`
- Type annotation syntax: `name : A -> B`
- Type inference for omitted annotations
- Statement terminator: newline or `;`
- Indentation-based block expressions
- Function-local scopes and blocks are defined by indentation
  (spaces and tabs are both allowed)
- Last expression is the return value
- Effect annotation syntax: `can Print`
- Anonymous functions: `|x| -> ...` and placeholder shorthand (`_ * 10`)
- Basic shown types: `Int`, `String`, tuple `(A, B)`, `List T`

## 2. MVP Decisions and Remaining Open Items

### 2.0 Locked for Current MVP

- Function calls support both `f x` and `f(x)`.
  - spaced application supports multiple args (`f a b c`) and is parsed left-associatively
    (`(((f a) b) c)`).
  - `f(x)` is used when precedence needs to be explicit.
- Block-local binding semantics are fixed for MVP:
  - `name = expr` is a local binding statement only when `name` is an identifier and `=` is assignment (not `==`).
  - bindings are visible to subsequent statements in the same declaration body.
  - re-binding the same name in the same body is allowed; the newer binding shadows the older one from that point onward.
  - bindings are declaration-local and do not escape to other declarations.
- Operator precedence/associativity is fixed for MVP:
  - precedence (low -> high): pipeline `|>` < addition `+` < multiplication `*` < call/application (`f x`, `f(x)`, `receiver.method(...)`).
  - `|>`, `+`, `*` are left-associative.
  - for MVP parser compatibility, infix `+` and `*` require spaces on both sides.
  - pipeline callee must be an identifier (`expr |> f`).
- Type annotation style is unified to `name : Type`.
- Statement terminators are newline or `;`.
- Comment syntax is fixed for MVP:
  - single-line comment marker is `#`.
  - line-end comments are allowed (for example: `x = 1 # note`).
  - `#` starts a comment only outside string literals; inside strings it is a normal character.
  - block comment syntax is not included in MVP.
  - `#!` has no special shebang handling in MVP; it is treated as a normal `#` comment.
- Generic type application syntax is fixed to Haskell-style spacing:
  - single-parameter examples: `List Int`.
  - multi-parameter examples: `TypeX a b`.
  - nested applications requiring grouping use parentheses: `TypeX (TypeY a b) c`.
  - diagnostics should render type applications in the same style.
- Indentation-based blocks accept both tabs and spaces.
  - for MVP, any line with at least one leading space or tab is treated as an indented block line.
  - mixing tabs and spaces in the same block is allowed in MVP (no normalization/error rule yet).
- CLI commands:
  - `goby-cli run <file.gb>` uses `main` entrypoint only and executes generated Wasm via external `wasmtime`.
    - if `wasmtime` is not installed, execution is skipped with an informational message.
    - current invocation: `wasmtime run <file.wasm>` with WASI-standard `_start` export.
  - `goby-cli check <file.gb>` performs parse/typecheck without runtime entrypoint.
- `main` type is restricted to `Unit -> Unit` for MVP.
  - `main` type annotation is required for `run`; optional for `check`.
- Legacy `void` type spelling is rejected in type annotations.
- First backend target is Wasm.
- Effects and handlers are available in MVP runtime via `effect`, `handler`, `with`, and `with_handler`.
  - handler dispatch follows lexical nearest-handler stack order.
  - `can` clauses are validated (declared effect or built-in effect names).
- Canonical handler application syntax is `with <handler_expr> in <body>` and
  `with_handler ... in ...` (inline sugar).
- Legacy syntax (`handler ... for ...`, `using`) is no longer accepted by the
  language parser as of 2026-03-04.
  - CLI commands (`check` / `run`) therefore reject such sources by default.
- MVP built-ins: `print`, `map`, `fetch_env_var`, `string.split`, `list.join`.
  - `print` execution is resolved by compiler/runtime internals (default stdio print path),
    not by a user-visible stdlib handler definition.
- `examples/basic_types.gb` is a parse/typecheck target, not a runnable entrypoint target.
  - no `main` addition and no `--entry` option in MVP.
- `examples/function.gb` is a canonical MVP run target and must be preserved as-is.
  - no simplification/downgrade of `examples/function.gb` is allowed.
  - `goby-cli run examples/function.gb` target output is:
    - `90`
    - `[30, 40, 50]`
    - `[60, 70]`
    - `something`
    - `15`
- String escape sequences in string literals: `\n`, `\t`, `\\`, `\"`.
- `case` expression syntax: multi-line lookahead, arm separator ` -> `, wildcard `_`.
  - `CasePattern` supports: `IntLit`, `StringLit`, `BoolLit` (`True`/`False`), `Wildcard`.
  - `else if` chaining is not supported in MVP.
- `if ... else ...` expression: indentation-based two-branch form.
- `==` equality operator: produces `Bool` at runtime.
- Handler dispatch: lexical nearest-handler stack walk (no alphabetical fallback).
- Runtime execution model: prefer native lowering for the supported subset; fallback to
  compile-time interpreter (`resolve_main_runtime_output`) for unsupported forms.

### 2.1 Syntax and Parsing

- **String interpolation `${}`** (implemented).
  - Parser supports `${ expr }` inside string literals and lowers it to `Expr::InterpolatedString`.
  - Typechecker treats interpolated literals as `String`.
  - Runtime/codegen evaluates each segment and stringifies embedded expression values.
- **Tuple index access `expr.N`** (post-MVP).
  - Syntax `a.0`, `a.1` is shown in `examples/basic_types.gb` but is not yet parsed.
  - `parse_method_call` rejects numeric method names (`is_identifier` fails on digits).
  - Implementation requires: new `Expr::TupleIndex { expr, index: usize }` variant,
    parser recognition of `expr.N` where N is a non-negative integer literal,
    typechecker check that the receiver is a tuple type with sufficient arity,
    and runtime/codegen evaluation.

### 2.2 Types and Checking

- TODO (Deferred): declaration-side generic parameter binders
  (for example, `id : a -> a` with explicit binders).
- Type annotation placement rules (required vs optional locations).
- Type error diagnostics quality bar is fixed for MVP:
  - diagnostics must be non-empty and human-readable plain text.
  - when a declaration is known, diagnostics must include the declaration name.
  - type mismatch diagnostics must include both expected and actual type names.
  - composite types should be rendered with full shape (for example: `List Int`, `(String, Int)`), not collapsed labels.
  - line/column reporting is not required in MVP.

### 2.3 Effect System

- Current implemented checks:
  - `can` effect names must be declared (or built-in).
  - uncovered effect operation calls are rejected unless covered by enclosing handler scope
    (`with` / `with_handler`).
  - calls to `can`-annotated functions require an appropriate enclosing handler scope.
- Current runtime behavior:
  - effect operations dispatch through installed handlers.
  - bare-name dispatch falls back to deterministic effect-name order (temporary MVP behavior).
- How to represent multiple effects (`can Print + Read` or other syntax) — deferred.
- Effect propagation rules for higher-order functions — deferred.
- Effect diagnostics UX polish (wording/format consistency) — deferred.

#### Effect Renewal Syntax/Typing Lock (P0, 2026-03-04)

This section mirrors the locked renewal decisions in
`doc/PLAN_EFFECT_RENEWAL.md` and is the canonical language-spec entry for the
new handler-value model.

- Effect declaration syntax:
  - `effect <CamelCaseName>` with indented operation signatures.
  - newline-separated and `;`-separated declaration forms are both accepted.
- Handler is an expression/value:
  - `handler` evaluates to a handler value and may capture lexical bindings.
  - handler clauses use untyped headers only: `op x -> ...`.
  - operation/argument typing is sourced from effect declaration signatures.
- Handler type:
  - `Handler(E)` for single-effect handler values.
  - `Handler(E1, E2, ...)` for multi-effect handler values.
  - effect-list order is type-equivalent (`Handler(A, B)` == `Handler(B, A)`).
  - parser/tokenization is whitespace-insensitive around commas
    (`Handler(E1,E2)` and `Handler(E1, E2)` are equivalent).
- Handler application:
  - canonical form is `with <handler_expr> in <body>`.
  - `with` accepts exactly one handler value.
  - `with_handler ... in ...` is syntax sugar for inline handler construction +
    `with`.
- Dispatch/conflict policy:
  - dynamic operation dispatch uses nearest active handler (lexical stack order).
  - duplicate operation clauses in one handler are rejected.
  - if one multi-effect handler introduces overlapping operation names across
    effects, reject as ambiguity.
  - canonical ambiguity diagnostic:
    `operation '<op>' is ambiguous across effects in Handler(...): <EffectA>, <EffectB>`.
- Resume policy:
  - one-shot continuation contract remains active.
  - double `resume` in the same handler invocation is rejected.
- Reserved keywords for renewal syntax:
  - `with`, `with_handler`, `in`, `handler`, `effect`.
- Migration policy:
  - bridge-window acceptance for top-level `handler ... for ...` and `using` has
    ended (2026-03-04).
  - current status: parser/runtime/typecheck legacy compatibility paths are removed;
    legacy forms remain migration-guide-only.
  - migration mapping examples are documented in
    `doc/EFFECT_RENEWAL_MIGRATION.md`.

#### Post-MVP Implementation Direction (locked 2026-03-01)

- Adopt **deep handlers + one-shot resumptions** as the baseline semantics.
  - Rationale: this matches the efficient path used by OCaml 5 and keeps runtime costs low for the common case.
  - Multi-shot resumptions are deferred; if added later, they must be explicit (clone/copy semantics) and opt-in.
- Replace name-based runtime handler lookup with **compiled IDs**:
  - intern `EffectId` and `OpId` at compile time,
  - compile each handler into a compact operation table indexed by `OpId`,
  - resolve operations by lexical handler stack walk (nearest enclosing handler wins).
- Use **selective CPS + evidence passing** in lowering:
  - keep pure/no-effect functions in direct style,
  - lower only effectful call paths and handler boundaries to continuation/evidence form,
  - pass handler evidence explicitly instead of global maps in hot paths.
- Wasm lowering strategy (phased):
  - Phase A: explicit continuation objects + trampoline/state-machine execution (portable on current Wasm MVP engines).
  - Phase B: optional optimization path on engines with typed continuations/stack-switching support.
- Performance guardrails for implementation:
  - no `HashMap`/`BTreeMap` lookup on hot operation dispatch paths,
  - dispatch target should be `O(handler_depth)` frame walk + `O(1)` op-table index,
  - continuation capture/resume should avoid full-stack copying on one-shot path.

#### `resume` Surface Contract (Step 0 lock, 2026-03-02)

- `resume` is a reserved keyword for upcoming effect-resumption syntax.
- Parser contract is locked:
  - top-level declaration name `resume` is rejected.
  - handler parameter name `resume` is rejected.
- Decision (2026-03-03):
  - do not introduce explicit `discontinue` syntax in the current phase.
  - keep abortive behavior as "handler does not call `resume`".
  - track explicit `discontinue` only as a possible later-phase language extension.

#### `resume` Parser/AST Contract (Step 1 done, 2026-03-02)

- `Expr::Resume { value }` is added to AST.
- Parser recognizes `resume <expr>` as a dedicated expression form.
- Bare `resume` (no argument) is rejected with parse diagnostics:
  - `malformed \`resume\` expression: expected \`resume <expr>\``.

#### `resume` Typecheck Contract (Step 2 done, 2026-03-02)

- `resume` is rejected outside handler method bodies.
- In handler method bodies, `resume` argument type is checked against the handled
  operation return type.
- Dedicated diagnostics are active:
  - `resume_outside_handler`
  - `resume_arg_type_mismatch`
  - `resume_in_unknown_operation_context`
  - `resume_potential_multi_shot` (conservative syntactic guard: more than one
    `resume` in a handler method body is rejected in the current phase)
- Decision update (2026-03-03):
  - `resume` result type inference is conservative:
    - allow local inference only when handler operation result type is
      non-generic and directly available in resume context,
    - keep `Ty::Unknown` for generic/complex cases.
  - multi-shot static rejection policy is intentionally lightweight:
    - reject obvious syntactic multi-resume usage in the same handler method,
    - defer control-flow-sensitive precision improvements to a later phase.

#### `resume` Runtime Bridge (Step 4 update, 2026-03-02)

- Interpreter-path runtime now has explicit `ResumeToken`/`Continuation` carrier
  objects and one-shot consume tracking.
- `Expr::Resume` evaluates in handler context, marks the current token consumed,
  and returns a resume value to the effect call site.
- Runtime surfaces explicit resume misuse errors:
  - `resume used without an active continuation`
  - `resume continuation already consumed`
- Runtime dispatch now uses lexical handler stack semantics:
  - nearest enclosing handler wins (LIFO stack walk),
  - no alphabetical effect-name fallback in runtime operation capture.

#### `resume` Plan Status (2026-03-03)

- `PLAN_RESUME` implementation track (Step 0-8) is complete and archived:
  - `doc/old/PLAN_RESUME.md`.
- Remaining `resume`-related work is now tracked here as future development:
  - improve multi-shot static rejection from current conservative syntactic guard
    to control-flow-sensitive analysis,
  - reconsider explicit `discontinue` only as a future language extension,
  - if/when switching default execution mode from `PortableFallback` to
    optimized mode, do so in a dedicated follow-up with separate review.

### 2.4 Standard Library Surface (MVP)

- Core modules to ship first (`Int`, `String`, `List`, `Env`) — minimal built-ins implemented.
- Naming conventions for stdlib functions — established.
- Minimal collection API for immutable workflows — deferred.

### 2.5 Runtime / Compiler Scope (MVP)

- Current model: native Wasm code generation for the supported subset, with deterministic
  fallback behavior for unsupported forms.
- Real Wasm migration plan (Phase 0-8) is completed; implementation record is archived.
- `goby-cli run examples/basic_types.gb` currently fails with a codegen error because current
  codegen/runtime paths do not handle tuple index access (`expr.N`) yet.
- Error location strategy (line/column reporting) — deferred.

### 2.6 Tooling Scope (MVP)

- Minimum CLI commands (`goby-cli run`, `goby-cli check`) — complete.
- Project layout and package metadata format (Cargo workspace with `goby-core`, `goby-cli`, `goby-wasm`) — complete.
- Tooling direction is now explicit:
  - prioritize early developer-environment support (syntax highlight, formatter, linter, LSP),
  - keep diagnostics/messages stable enough to be consumed by editor tooling.

## 3. Later-Phase Decisions

- Module/package ecosystem and remote dependency management.
- Advanced effects (async, state, cancellation, effect-safety diagnostics).
- Potential explicit `discontinue` syntax as a clarity-focused effect-control extension
  (deferred; not part of current `resume` rollout).
- Pattern matching exhaustiveness and advanced ADTs.
- Interoperability/FFI strategy.
- Governance model, RFC process, compatibility policy.
- Real Wasm code generation (replace compile-time interpreter).
- Declaration-side generic parameter binders.
- Handler clause body type-checking improvements (effect-safety and diagnostics polish).
- `else if` chaining in `if` expressions.
- REPL or interactive mode.

## 4. Next Phase Plan

All MVP example targets are complete. The next work is post-MVP:

- Keep regression checks green for the locked MVP subset.
- Treat remaining design items as post-MVP evolution work.
- Track all new syntax requests as explicit change proposals.
- Candidate next focus areas:
  1. Real Wasm code generation (actual instruction emission, remove compile-time interpreter).
  2. Effect runtime redesign (one-shot deep handlers + selective CPS/evidence passing).
  3. Standard-library foundation for self-hosted Goby libraries.
  4. Early developer tooling foundation (LSP, syntax highlighting, linter, formatter).

### 4.1 Short-Term Patch: bare/qualified/pipeline effect call dispatch in `main` body — DONE (2026-03-02, commit 8136d61)

`eval_ast_side_effect` now dispatches bare calls, qualified calls, and pipeline calls
through active `with` / `with_handler` handlers, matching the behavior of
`execute_unit_ast_stmt`.

- Added `Expr::Qualified` arm: `find_handler_method_for_effect` → dispatch → string fallthrough.
- Added bare-name handler lookup in `Expr::Var` arm (before `execute_unit_call_ast`).
- Added bare-name handler lookup in `Pipeline` arm (`"msg" |> log` dispatches).
- Fixed `execute_decl_as_side_effect` depth: 1 → 0 (top-level consistency).
- 5 new regression tests; 181 total tests pass.

### 4.2 Mutable Variable Syntax (`mut` / `:=`) — Implemented (2026-03-04)

Goal: support explicit mutable local variables while preserving immutable-by-default
behavior in declaration bodies.

Proposed surface syntax:

- Mutable declaration: `mut a = 1`
- Mutable update: `a := 2`
- Immutable declaration remains: `b = 4`

Lock decisions for this change:

- `mut` is a reserved keyword.
- `=` remains declaration syntax; assignment uses `:=`.
- Variables are immutable by default unless declared with `mut`.

Implementation status:

1. Parser/AST
   - Added statement variants for mutable declaration and assignment.
   - Parses `mut <name> = <expr>`.
   - Parses `<name> := <expr>`.
2. Typechecker
   - Tracks declaration-body mutability and enforces assignment rules.
   - Rejects same-scope redeclaration and guides users to `:=`.
3. Runtime/lowering
   - Fallback/runtime evaluators execute mutable declaration and assignment.
   - Native lowering keeps these forms on fallback path (unsupported in direct subset).
4. Tests and docs
   - Added parser/typechecker/runtime regression coverage.
   - `examples/mut.gb` remains the canonical sample.

Required error cases (must pass):

- `a = 10` after `mut a = ...` in the same scope is rejected
  (redeclaration; users should use `:=`).
- `b := 3` when `b` was declared without `mut` is rejected.
- `x := 1` when `x` is undeclared is rejected.

Diagnostic style targets (wording aligned with existing plain errors):

- `duplicate declaration \`a\` in the same scope; use \`:=\` for mutation`
- `cannot assign to immutable variable \`b\`; declare it with \`mut\` first`
- `cannot assign to undeclared variable \`x\``

### 4.1.1 Effect op argument type checking in handler scopes

#### Background

`catch "NoCoffeeError"` compiles without error even when `catch : Error -> Unit` (the
handler expects an `Error` record, not a `String`). At runtime the handler receives a
`String`, tries to access `.message` field, gets `None`, and silently produces no output.

Root causes identified (2026-03-02):

1. **No argument-type check on effect op calls.** The typechecker only checks that
   an effect op name is covered by an enclosing handler scope; it does not check that the
   argument type matches the op's declared signature.
2. **Runtime evaluator cannot construct record values from positional-style calls.**
   `Error "NoCoffeeError"` is parsed as `Call { callee: Var("Error"), arg: StringLit }`,
   not as `RecordConstruct`. `eval_ast_value` returns `None` for this form when `Error`
   is not a declared Goby function, causing silent no-output rather than an error.

#### Goal

Make the above class of errors a **compile-time typecheck error**, not a silent runtime
failure.  The diagnostic should fire whenever a handler scope (`with` / `with_handler`) contains an effect op call
whose argument type is statically incompatible with the op's declared signature.

#### Scope and constraints

- Effect op signatures are declared as `op_name: ArgType -> ReturnType` in `effect` blocks.
- Only single-argument ops are in scope (MVP constraint: all current effect ops take one arg).
- Full type inference is out of scope; check only the cases where the arg type is
  unambiguously known at typecheck time (literal types: `Int`, `String`, `Bool`; known record
  constructors).
- Do not break any existing passing test.
- `cargo clippy -- -D warnings` must remain clean.

#### Design

**No new data structures needed.**
`inject_effect_symbols` already calls `parse_function_type` on each `EffectMember.type_annotation`
and stores the result as `Ty::Fun { params, result }` in `TypeEnv`. The arg type is already
available via `env.lookup(op_name)` — no new map or re-parsing required.

**Step 1: Add argument type check in `check_unhandled_effects_in_expr`, `Expr::Call` arm.**

Insertion point: after the existing coverage guard and `check_callee_required_effects` call,
when `callee` is `Expr::Var(name)` and `env.is_effect_op(name)`:

```
if let Ty::Fun { params, .. } = env.lookup(name) {
    if let Some(expected) = params.first() {
        if *expected != Ty::Unknown {
            let actual = check_expr(arg, env);
            if actual != Ty::Unknown && actual != *expected {
                return Err(TypecheckError {
                    message: format!(
                        "effect operation `{name}` expects argument of type `{expected}` \
                         but got `{actual}`"
                    ),
                    ...
                });
            }
        }
    }
}
```

**Step 2: Apply the same check in the `Expr::Pipeline` arm.**

`"NoCoffeeError" |> catch` also passes a typed argument to an effect op and must be checked.

**Step 3: Regression tests (4 cases).**

1. Negative (Call): `catch "x"` when `catch : Error -> Unit` → error including op name,
   expected type, actual type.
2. Negative (Pipeline): `"x" |> catch` when `catch : Error -> Unit` → error.
3. Positive: `catch (Error(message: "x"))` when `catch : Error -> Unit` → accepted.
4. Neutral: op with annotation that fails to parse → no error (check silently skipped).

Error message convention: use `"effect operation"` (not `"effect op"`) to match existing
diagnostics in the codebase.

#### Out of scope (deferred)

- Full type inference for arbitrary expressions.
- Multi-argument ops.
- Return type checking of effect ops.
- Checking handler method body types against op signatures.

### 4.2 Refactoring Candidates (Wasm Track) — DONE (2026-03-02 sessions 33-40)

- Legacy fallback analyzer extraction from `crates/goby-wasm/src/lib.rs` completed.
- Fallback reason literals migrated to shared constants/typed enum mapping.
- Capability/support helper split completed (`call.rs`, `support.rs`, and focused tests).

### 4.2.1 Lambda/HOF native lowering for `function.gb` subset — DONE (2026-03-02)

- Native lower/evaluator now supports function values:
  - lambda closure values (`Expr::Lambda`),
  - declaration references as callable values,
  - partial application for named callables.
- Native evaluator supports HOF calls used by `examples/function.gb`:
  - `map` with lambda/function argument,
  - callback-style parameter calls (`f 10`) inside declarations.
- Native capability checker now accepts `examples/function.gb`.
  - `function.gb` moved from fallback matrix to native matrix in tests.
  - `call_target_body_not_native_supported` remains for declarations with unsupported forms
    such as `with_handler` blocks.

### 4.3 Standard-Library Foundation (self-hosted direction)

Goal: prepare infrastructure so core standard libraries can be authored primarily in Goby,
while preserving practical monorepo workflow during early phases.

Planning constraints:

- Standard libraries should be implemented in Goby itself by default.
- Libraries should be importable as `import goby/(library_name)`.
- At least for the near term, stdlib and compiler/runtime remain in the same monorepo.

Preparation steps:

1. Define source layout and naming for Goby stdlib modules in-repo.
   - Decide canonical directory for Goby stdlib sources.
   - Map module path `goby/x` to that directory deterministically.
2. Extend import resolution to load Goby source modules from the stdlib location.
   - Keep existing built-in module behavior working during migration.
   - Add clear diagnostics for unresolved/ambiguous stdlib module paths.
3. Introduce minimal bootstrap boundary between language built-ins and Goby stdlib.
   - Keep only runtime-primitive operations as built-ins.
   - Move composable library logic to Goby modules incrementally.
4. Add regression coverage for stdlib import behavior.
   - Positive: `import goby/x` resolves and typechecks from in-repo Goby source.
   - Negative: missing module path reports stable diagnostics.
5. Validate monorepo workflow.
   - Ensure `cargo check`, `cargo test`, and example `goby-cli check/run` flows continue to work
     with stdlib sources in-tree.

Additional planning constraint (locked 2026-03-02):

- Introduce `@embed` declarations as a **stdlib-only** feature for runtime-bridged effects
  (initial target: `@embed Print` for `goby/stdio`).
- Canonical stdlib embed form is `@embed <EffectName>` with an in-module `effect <EffectName>`
  declaration requirement (legacy `@embed effect <EffectName>` is temporarily accepted for compatibility).
- `@embed` is not allowed in user Goby modules or non-stdlib libraries.
- `print` migration should proceed via `goby/stdio` + stdlib `@embed Print` bridge,
  with temporary compatibility for existing bare `print`.
- Stdlib intrinsic bridge policy (ExtraStep B lock, 2026-03-03):
  - active intrinsic set:
    - `__goby_string_length : String -> Int`
    - `__goby_env_fetch_env_var : String -> String`
  - `__goby_*` names are reserved and stdlib-only,
  - unknown `__goby_*` names in stdlib are rejected explicitly.

### 4.4 Early Developer Tooling Plan

Goal: align implementation priorities with the language vision that strong tooling is a core
project value, and make Goby practical in editors early.

Scope to prioritize:

- LSP server (at least hover, go-to-definition, diagnostics, document symbols).
- Syntax highlighting (TextMate grammar first; Tree-sitter grammar as optional follow-up).
- Linter (`goby lint`) with stable machine-readable output format for editor integration.
- Formatter (`goby fmt`) with deterministic output and CI-friendly check mode.

Implementation steps:

1. Tooling protocol/contracts baseline.
   - Define canonical diagnostic schema (file, line, column, code, message, severity).
   - Freeze a minimal set of error codes for parser/typechecker/lint.
2. Formatter MVP.
   - Implement a deterministic formatter for currently supported syntax.
   - Add `goby fmt` and `goby fmt --check`.
   - Add golden-file tests to prevent formatting drift.
3. Linter MVP.
   - Implement `goby lint` for high-signal checks (unused bindings, shadowing clarity, style traps).
   - Output both human-readable and machine-readable formats.
4. Syntax highlighting packs.
   - Provide a `.tmLanguage` grammar for immediate editor adoption.
   - Keep grammar in monorepo with snapshot tests on representative `.gb` snippets.
5. LSP MVP.
   - Build `goby-lsp` over parser/typechecker diagnostics.
   - Implement diagnostics + hover + definition first; defer heavy features.
   - Verify behavior on `examples/*.gb` and stdlib Goby modules.
6. Editor integration and release flow.
   - Publish a minimal VS Code extension wrapper (syntax + LSP wiring).
   - Keep tooling artifacts versioned in the same monorepo at this stage.

### 4.5 Editor Syntax Highlight Rollout Plan (VSCode / Emacs / Vim) — DONE (2026-03-02)

All three editor highlight packs are implemented and present in the monorepo:

- `tooling/syntax/textmate/goby.tmLanguage.json`: canonical TextMate grammar (10 token categories).
- `tooling/vscode-goby/`: VS Code extension (package.json, language-configuration.json, grammar, README).
- `tooling/vim/syntax/goby.vim` + `tooling/vim/ftdetect/goby.vim`: Vim syntax pack + ftdetect.
- `tooling/emacs/goby-mode.el`: Emacs major mode (font-lock + `auto-mode-alist` for `.gb`).
- `tooling/syntax/testdata/highlight_sample.gb`: manual test fixture covering all token categories.

Remaining (deferred):

- Cross-editor regression tests (shared fixture → expected token scopes/groups in CI).

## 5. Spec Detail Notes

### Still Open (Post-MVP)

- Effects/handlers: lexical nearest-handler runtime semantics are active; post-MVP work is to move from interpreter/runtime lookup to compiled `EffectId`/`OpId` tables.
- Effects/handlers (`resume` follow-up): tighten multi-shot static analysis beyond
  the current conservative "multiple syntactic `resume`" rejection.
- Effects/handlers (`resume` extension): evaluate explicit `discontinue` syntax
  as a separate language-design proposal.
- Effect namespace rules: qualified vs unqualified calls, unhandled-effect diagnostics format.
- Type annotation placement: where annotations are required vs optional outside current MVP subset.
- Tuple/record roadmap: record update syntax, pattern matching on record fields.
- Import system: filesystem-backed/local package resolution, dependency graph rules.
- Equality/comparison: operator set (`!=`, `<`, `>`, etc.) and type constraints.

## 6. Research References (2026-03-01 survey)

- OCaml manual (effect handlers): one-shot continuations are cheaper than multi-shot and are the default operational model.
  - <https://caml.inria.fr/pub/distrib/ocaml-5.0/ocaml-5.0-refman.html#sec281>
- Retrofitting Effect Handlers onto OCaml (Sivaramakrishnan et al., PLDI 2021): runtime design with fibers and one-shot continuations integrated into a production compiler/runtime.
  - <https://arxiv.org/abs/2104.00250>
- Effect Handlers, Evidently (Xie and Leijen): evidence-passing translation strategy for efficient handlers.
  - <https://arxiv.org/abs/2106.00160>
- WasmFX: Typed Continuations and Stack Switching for WebAssembly (Hillerstrom et al., ICFP 2024): practical path for direct-style effect handlers on Wasm backends.
  - <https://arxiv.org/abs/2403.01036>
