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

1. General lambda-as-function-argument runtime parity (`list.each`-class cases).
2. Stdlib runtime bridge generalization (reduce symbol-specific fallback branches).
3. Tooling foundation (`fmt`/`lint`/`lsp`) with stable diagnostics surface.

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
