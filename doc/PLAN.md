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
  - `goby-cli check <file.gb>` performs parse/typecheck without runtime entrypoint.
- `main` type is restricted to `Unit -> Unit` for MVP.
- Legacy `void` type spelling is rejected in type annotations.
- First backend target is Wasm.
- Effects are parse-only metadata in MVP.
  - unknown effect names are ignored.
  - `can` clauses must be syntactically valid (non-empty list of identifiers).
- MVP built-ins are `print` and `string.concat`.
  - `map` is required for `examples/function.gb` run parity.
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
- Current status:
  - as of 2026-02-28, `check` passes for `examples/function.gb`.
  - as of 2026-02-28, `run` parity is implemented for the locked subset and prints:
    - `90`
    - `[30, 40, 50]`
    - `[60, 70]`
    - `something`
    - `15`
  - as of 2026-02-28, workspace validation is green:
    - `cargo check`
    - `cargo test`
    - `cargo clippy -- -D warnings`
  - MVP implementation for the locked subset is complete.

### 2.1 Syntax and Parsing

- No additional open syntax/parsing items are currently tracked for MVP.

### 2.2 Types and Checking

- TODO (Deferred): declaration-side generic parameter binders
  (for example, `id : a -> a` with explicit binders).
- Type annotation placement rules (required vs optional locations).
- Tuple and record roadmap (records in MVP or not).
- Type error diagnostics quality bar is fixed for MVP:
  - diagnostics must be non-empty and human-readable plain text.
  - when a declaration is known, diagnostics must include the declaration name.
  - type mismatch diagnostics must include both expected and actual type names.
  - composite types should be rendered with full shape (for example: `List Int`, `(String, Int)`), not collapsed labels.
  - line/column reporting is not required in MVP.

### 2.3 Effect System

- How to represent multiple effects (`can Print + Read` or other syntax).
- Effect propagation rules for higher-order functions.

### 2.4 Standard Library Surface (MVP)

- Core modules to ship first (`Int`, `String`, `List`, maybe `Result/Option`).
- Naming conventions for stdlib functions (`string.concat` style consistency).
- Minimal collection API for immutable workflows.

### 2.5 Runtime / Compiler Scope (MVP)

- First execution target is Wasm backend.
- Smallest end-to-end pipeline to ship first.
- Error location strategy (line/column reporting).

### 2.6 Tooling Scope (MVP)

- Minimum CLI commands (for example: `goby-cli run`, `goby-cli check`, `goby fmt`).
- Project layout and package metadata format (Cargo workspace with `goby-core`, `goby-cli`, `goby-wasm`).
- Formatter policy and non-configurable defaults.

## 3. Later-Phase Decisions

- Module/package ecosystem and remote dependency management.
- Advanced effects (async, state, cancellation).
- Pattern matching exhaustiveness and advanced ADTs.
- Interoperability/FFI strategy.
- Governance model, RFC process, compatibility policy.

## 4. Next Phase Plan

- Keep regression checks green for the locked MVP subset.
- Treat remaining design items as post-MVP evolution work.
- Track all new syntax requests as explicit change proposals.

## 5. Example-Driven Feature Checklist

Status baseline (2026-02-28):
- `check` passes: `hello.gb`, `basic_types.gb`, `function.gb`, `generic_types.gb`,
  `print/local_binding.gb`, `print/concat.gb`, `parser/mixed_indent.gb`.
- `check` fails: `control_flow.gb`, `effect.gb`, `import.gb`.

### 5.1 Core Syntax/Typing Used by Stable Examples

- [x] Top-level declarations with optional type annotations (`name : Type`, `name = expr`)
- [x] Function types (`A -> B`), including parenthesized function arguments (`(A -> B) -> C`)
- [x] Haskell-style type application in annotations (`List Int`, `TypeX a b`, `TypeX (TypeY a b) c`)
- [x] Integer/string literals, tuple/list literals, local bindings, block-last-value return
- [x] Calls (`f x`, `f(x)`), method call subset (`string.concat(a, b)`), pipeline (`|>`)
- [x] Lambda forms (`|x| -> ...`, `_ * 10`)
- [x] Comments (`#`, line-end comments, shebang-as-comment), mixed tabs/spaces indentation
- [x] MVP diagnostics baseline (non-empty, declaration name when known, expected/actual types)

### 5.2 Runtime/CLI Behaviors Required by Examples

- [x] `check` command (parse + typecheck without runtime entry requirement)
- [x] `run` command with `main` entrypoint only
- [x] `main : Unit -> Unit` enforcement for runnable programs
- [x] Wasm emit + `wasmtime` execution path
- [x] Locked `examples/function.gb` output parity

### 5.3 Features Referenced by Non-MVP Examples (Post-MVP Backlog)

- [x] `import` syntax and minimal module resolution
  (`import goby/x`, alias `as`, selective import `(...)`) for built-in modules
  (`goby/string`, `goby/list`, `goby/env`) used by `examples/import.gb`.
- [ ] `effect` declarations and effect member signatures
- [ ] `handler ... for ...` syntax and handler scope semantics
- [ ] `using` handler application syntax (single/multiple handlers)
- [ ] Multiple effects in `can` with semantic checking (currently parse/type surface only)
- [ ] `case` expressions with pattern arms and wildcard `_`
- [ ] `if ... else ...` (or strict desugaring rule to `case`) and `==` typing rules

## 6. Spec Detail Notes (Need Clarification Before Implementation)

- Import system:
  module path grammar (`goby/...` vs local paths), lookup order, shadowing, and cyclic import diagnostics.
- Effects/handlers:
  effect namespace rules, qualified vs unqualified calls (`Log.log` vs `log`),
  handler resolution order, and unhandled-effect diagnostics format.
- Control flow:
  exact grammar/precedence for multiline `case`/`if`, exhaustiveness requirements,
  and whether `if` is mandatory sugar or first-class syntax in the AST.
- Equality/comparison:
  operator set for MVP+ (`==`, `!=`, etc.), precedence relative to `+/*/|>`,
  and type constraints for comparable values.
- Type annotation placement:
  where annotations are required vs optional outside current MVP subset.
- Tuple/record roadmap:
  whether records are introduced before module/effect expansion and minimal syntax shape.
- Import system (post-slice remaining scope):
  filesystem-backed/local package resolution, dependency graph rules, and cache/build integration.

## 7. Incremental Implementation Plan (Next Slice: `import.gb`)

Goal:
- make `cargo run -p goby-cli -- check examples/import.gb` pass with a minimal, explicit import model.

Phase 1: Scope lock (minimum useful subset)
- support only:
  - `import goby/x`
  - `import goby/x as alias`
  - `import goby/x (name1, name2)`
- keep `run` behavior unchanged in this slice; focus on parser/typecheck path.
- keep `main` requirements unchanged.

Phase 2: Parser/AST extension
- add import declarations to module AST.
- parse the three import forms above.
- preserve backward compatibility for all existing non-import examples.

Phase 3: Minimal module resolver
- introduce a fixed built-in module table for:
  - `goby/string`
  - `goby/list`
  - `goby/env`
- implement alias binding and selective import symbol exposure.
- add diagnostics for unknown module and unknown imported symbol.

Phase 4: Typecheck integration
- inject imported symbols into type environment.
- verify imported and qualified calls used by `examples/import.gb`.
- keep diagnostics explicit and declaration-oriented.

Phase 5: Validation and docs
- run:
  - `cargo run -p goby-cli -- check examples/import.gb`
  - `cargo check`
  - `cargo test`
  - `cargo clippy -- -D warnings`
- update this plan checklist and `doc/STATE.md` with decisions and remaining gaps.

Out of scope for this slice:
- filesystem-backed package resolution
- remote dependencies
- effect/control-flow feature integration changes
