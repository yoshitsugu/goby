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
- Type annotation style is unified to `name : Type`.
- Statement terminators are newline or `;`.
- Indentation-based blocks accept both tabs and spaces.
  - for MVP, any line with at least one leading space or tab is treated as an indented block line.
  - mixing tabs and spaces in the same block is allowed in MVP (no normalization/error rule yet).
- CLI commands:
  - `goby-cli run <file.gb>` uses `main` entrypoint only and executes generated Wasm via external `wasmtime`.
    - if `wasmtime` is not installed, execution is skipped with an informational message.
  - `goby-cli check <file.gb>` performs parse/typecheck without runtime entrypoint.
- `main` type is restricted to `void -> void` for MVP.
- First backend target is Wasm.
- Effects are parse-only metadata in MVP.
  - unknown effect names are ignored.
- MVP built-ins are `print` and `string.concat`.
  - `map` is included for parse/typecheck (`check`) scope.
  - `map` runtime/codegen support is postponed.
- `examples/basic_types.gb` is a parse/typecheck target, not a runnable entrypoint target.
  - no `main` addition and no `--entry` option in MVP.
- `examples/function.gb` is an MVP run target for the current integer-only function subset.
  - `goby-cli run examples/function.gb` is supported for integer call/lowering flow.
  - higher-order/pipeline/list-print runtime support remains out of scope.
- Current status:
  - `check` and `run` for `examples/function.gb` are passing in the current subset.
  - unsupported runtime forms are still diagnosed explicitly (pipeline, higher-order usage, list print).

### 2.1 Syntax and Parsing

- Final rule for assignment/binding inside blocks (`a = 10` semantics).
- Comment syntax policy (currently `# ...` in examples).
- Operator precedence and associativity table.

### 2.2 Types and Checking

- Canonical generic syntax (`List Int` and future multi-parameter generics).
- Type annotation placement rules (required vs optional locations).
- Tuple and record roadmap (records in MVP or not).
- Type error message format and minimum diagnostics quality bar.

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

## 4. Immediate Execution Plan

- Freeze a short "MVP spec subset" from current examples.
- Advance expression/function lowering for `run` coverage beyond the current print-only path.
- Keep codegen diagnostics explicit for unsupported forms (`List` print, pipeline, and higher-order calls).
- Track all new syntax requests as explicit change proposals.
