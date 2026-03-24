# Goby IR Lowering Plan

Last updated: 2026-03-24

This document is the active architecture reference for Goby's compilation pipeline
and the long-term plan for Wasm backend lowering.

Related documents:

- `doc/LANGUAGE_SPEC.md` — source of truth for language syntax and semantics.
- `doc/PLAN.md` — top-level roadmap.
- `doc/PLAN_STANDARD_LIBRARY.md` — stdlib split/grapheme work (C4–C8).
- `doc/STATE.md` — current focus and restart notes.

---

## 1. Compilation Pipeline (Stable)

```
Parsed AST
  → typecheck / name resolution
  → ResolvedModule / ResolvedDecl / ResolvedExpr   (crates/goby-core/src/resolved.rs)
  → shared IR                                       (crates/goby-core/src/ir.rs)
  → backend IR / WasmBackendInstr                   (crates/goby-wasm/src/gen_lower/)
  → Wasm binary / interpreter
```

IR0–IR11 are complete. The resolved-form → shared IR boundary is stable.
Semantically equivalent surface spellings converge before the IR boundary.

**Locked design rules:**

1. Do not add backend-specific AST recognizers for constructs that belong in IR.
2. Semantically equivalent surface forms must produce equivalent IR.
3. Backend limitations are expressed as backend limitations, not IR-construction failures.
4. Fused patterns in `lower.rs` (`SplitEachPrint`, `SplitGetPrint`, `graphemes-get-print`)
   are deletion targets, not extension points. Each WB phase identifies which ones it makes
   obsolete.

---

## 2. Current IR → Wasm Coverage

`lower_comp` in `crates/goby-wasm/src/gen_lower/lower.rs` currently handles 5 of 13
`CompExpr` variants. All others fall through to `UnsupportedForm`.

`Call` is handled only for effect calls and backend intrinsics. Calls to ordinary
top-level declarations (including recursive stdlib functions like `map`/`each`) return
`UnsupportedForm`. This is the key gap that blocks WB-2A.

### CompExpr (13 variants)

| Variant | Status | Target phase |
|---------|--------|-------------|
| `Value` | ✅ handled | — |
| `Let` | ✅ handled | — |
| `Seq` | ✅ handled | — |
| `PerformEffect` | ✅ handled (Print/Read effects) | — |
| `Call` | ✅ handled (effect, intrinsic, decl calls, indirect) | WB-2A ✓ |
| `If` | ✅ handled | WB-1 ✓ |
| `LetMut` | ✅ handled | WB-1 ✓ |
| `Assign` | ✅ handled | WB-1 ✓ |
| `Case` | ✅ handled (literal/list patterns) | WB-2B ✓ |
| `Handle` | ✅ handled (one-shot tail-resumptive subset) | WB-3 ✓ |
| `WithHandler` | ✅ handled (one-shot tail-resumptive subset) | WB-3 ✓ |
| `Resume` | ✅ handled (tail position only → `return_call`) | WB-3 ✓ |

### ValueExpr (12 variants)

| Variant | Status | Target phase |
|---------|--------|-------------|
| `Unit` | ✅ handled | — |
| `IntLit` | ✅ handled | — |
| `BoolLit` | ✅ handled | — |
| `StrLit` | ✅ handled | — |
| `Var` | ✅ handled | — |
| `GlobalRef` | ✅ handled | — |
| `BinOp` | ✅ handled | WB-1 ✓ |
| `Interp` | ✅ handled | WB-1 ✓ |
| `ListLit` | ✅ handled | WB-2B ✓ |
| `TupleLit` | ✅ handled | WB-2B ✓ |
| `RecordLit` | ✅ handled | WB-2B ✓ |
| `Lambda` | ✅ handled (no-capture + wrapper AuxDecl for funcref values) | WB-3 ✓ |

---

## 3. Why the Phase Order Is Fixed

### WB-3 (effects) must be designed before WB-3's Lambda

`Lambda` and `Handle`/`WithHandler` share one design question: how are captured variables
passed to a function emitted as a separate Wasm function? The effect handler calling
convention is more constrained (it must integrate with `PerformEffect` dispatch and the
`EffectId`/`OpId` boundary), so it must be locked first. `Lambda` follows the same
convention.

### WB-2A (decl calls) must precede WB-2B (Case/data)

`stdlib/goby/list.gb` `map` and `each` are recursive functions that take function arguments.
They use `Case` but also call `each`/`map` recursively and call the `f` argument. Even after
`Case` is lowered (WB-2B), `map`/`each` will still fail because ordinary decl calls are
unsupported. WB-2A must land first.

### WB-3 legality analysis precedes WB-3 emission

The language spec (`doc/LANGUAGE_SPEC.md` §resume) defines multi-resume progression as
valid: a handler clause may call `resume` multiple times, each one restarting the continuation
from the next resumable point. Phase WB-3A (direct-call lowering) covers only the one-shot
tail-resumptive subset. The emit layer must therefore first determine whether a given
`WithHandler`/`Resume` is in the safe subset; programs outside that subset must produce an
explicit `BackendLimitation` error, not a silent wrong result or miscompilation.

---

## 4. Effect Handler Lowering: Strategy Survey

### 4.1 Why this is the hard problem

`WithHandler`/`Resume`/`PerformEffect` require non-local control transfer: when an effect
operation is performed, execution transfers to the nearest enclosing handler, then optionally
resumes from the call site. Wasm's structured control flow does not natively support this.

### 4.2 Strategy A — Selective CPS (chosen for WB-3A)

**Primary sources:**
- Daan Leijen, "Type Directed Compilation of Row-Typed Algebraic Effects", POPL 2017.
  MSR preprint: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/algeff.pdf
- Daniel Hillerström, Sam Lindley, Robert Atkey, KC Sivaramakrishnan,
  "Continuation Passing Style for Effect Handlers", FSCD 2017.
  https://homepages.inf.ed.ac.uk/slindley/papers/handlers-cps.pdf
- wasm_of_ocaml (Tarides / Jane Street):
  https://tarides.com/blog/2023-11-01-webassembly-support-for-ocaml-introducing-wasm-of-ocaml/

Only functions in the dynamic scope of a handler that performs non-tail resumptions are
CPS-transformed. The continuation becomes an explicit function parameter.

For **one-shot tail-resumptive** handlers (the only pattern in Goby's current stdlib),
selective CPS degenerates to direct-call lowering: the continuation is never heap-allocated,
`resume` at the tail of a handler clause becomes a Wasm `return_call`. No overhead.

**Scope gate:** this fast path is only legal when the handler is provably one-shot and
tail-resumptive (legality analysis, see §5 WB-3). The language spec permits multi-resume;
the fast path is a subset optimisation, not a semantic redefinition.

### 4.3 Strategy B — Evidence Passing

**Primary sources:**
- Ningning Xie and Daan Leijen, "Generalized Evidence Passing for Effect Handlers", ICFP 2021.
  DOI: 10.1145/3473576
  MSR preprint: https://www.microsoft.com/en-us/research/wp-content/uploads/2021/03/multip-tr-v2.pdf
- Koka language, `src/Compile/Options.hs`: `--target=wasm` routes through C backend
  (`target=C Wasm`), compiled via Emscripten (`emcc`). No direct Wasm backend exists in Koka.
  Source: https://github.com/koka-lang/koka

Evidence is threaded as an implicit extra parameter through all functions in handler scope.
O(1) handler lookup. Designed for C as a compilation target (Koka: C → Emscripten → Wasm).
More parameter-plumbing overhead per call than Strategy A for the one-shot case.

**Ruling:** not chosen for WB-3A. Higher implementation complexity than selective CPS for
no benefit given Goby's current one-shot usage pattern.

### 4.4 Strategy C — WasmFX / Stack Switching (target for WB-3B)

**Primary sources:**
- Luna Phipps-Costin, Andreas Rossberg, Arjun Guha, Daan Leijen, Daniel Hillerström,
  KC Sivaramakrishnan, Matija Pretnar, Sam Lindley,
  "Continuing WebAssembly with Effect Handlers", OOPSLA 2023.
  DOI: 10.1145/3622814
  arXiv preprint: https://arxiv.org/abs/2308.08347
  WasmFX project: https://wasmfx.dev/
- WebAssembly stack-switching proposal (W3C CG, **Phase 3** as of 2026-03):
  https://github.com/WebAssembly/proposals (stack switching listed under Phase 3)
  https://github.com/WebAssembly/stack-switching
  Explainer: https://github.com/WebAssembly/stack-switching/blob/main/proposals/stack-switching/Explainer.md
- Wasmtime implementation tracking (x64 initial, other ISAs deferred):
  https://github.com/bytecodealliance/wasmtime/issues/10248

Adds `suspend <tag>` / `resume <ft> (on $tag handler_block)` instructions to Wasm.
Maps 1:1 to Goby's IR:

```
PerformEffect { effect, op, args }  →  suspend $op_tag
WithHandler { handler, body }       →  resume $ft (on $op_tag handler_block)
Resume { value }                    →  (implicit: handled by suspend/resume semantics)
```

No source-level transformation required. Supports multi-resume natively.

**Status (2026-03):** Phase 3 (implementation phase). Wasmtime has x64 implementation;
ARM64 and other ISAs are deferred. Not available in browsers. The WasmFX fork of Wasmtime
(`https://github.com/wasmfx/wasmfxtime`) is the current research prototype.

**Key property:** Goby's IR is intentionally kept structurally 1:1 with WasmFX instruction
semantics. Migrating from WB-3A to WB-3B requires replacing only the emit layer, not IR or
the lowering pipeline.

### 4.5 Strategy D — Asyncify (ruled out)

**Source:** https://kripken.github.io/blog/wasm/2019/07/16/asyncify.html

A Wasm-binary-to-binary rewrite pass. Requires Binaryen as post-compilation dependency.
Does not integrate into a hand-written emitter. 50–100% binary size overhead. Cannot express
multi-shot continuations or compositional handler nesting. **Ruled out.**

---

## 5. Implementation Phases and Milestones

### Phase WB-1: Pure control flow and operators

No dependency on effect handler design or data layout decisions.

**Scope:**

| Construct | Wasm mapping | Notes |
|-----------|-------------|-------|
| `CompExpr::If` | `if/else/end` | Condition: tagged i64 bool → `i32` for Wasm `if` |
| `ValueExpr::BinOp` — integer arithmetic | `i64.add`, `i64.sub`, `i64.mul`, `i64.div_s` | Untag before op, retag result |
| `ValueExpr::BinOp` — integer comparison | `i64.eq`, `i64.ne`, `i64.lt_s`, `i64.le_s`, `i64.gt_s`, `i64.ge_s` | Result: tagged bool |
| `ValueExpr::BinOp` — string equality | `__goby_string_eq` helper | Reuse existing string ABI |
| `ValueExpr::Interp` | sequential `__goby_string_concat` helper calls | Concat pairs left-to-right |
| `CompExpr::LetMut` | allocate new mutable `local.set` slot | Same model as native fallback |
| `CompExpr::Assign` | `local.set` to existing mutable slot | Same model as native fallback |

**Entry files:**
- `crates/goby-wasm/src/gen_lower/lower.rs` — add match arms to `lower_comp` / `lower_value`
- `crates/goby-wasm/src/gen_lower/emit.rs` — add Wasm instruction emission for new `WasmBackendInstr` variants
- `crates/goby-wasm/src/gen_lower/backend_ir.rs` — add `If`, `BinOp`, etc. to `WasmBackendInstr`

**Milestones:**

- [x] WB-1-M1. `ValueExpr::BinOp` (arithmetic and comparison) lowered and emitted
  - done when: `2 + 3` and `x == y` programs classify as `GeneralLowered` and execute correctly
  - regression: unit test in `lower.rs` + execution test in `lib.rs`
- [x] WB-1-M2. `ValueExpr::Interp` lowered and emitted
  - done when: string interpolation programs classify as `GeneralLowered`
  - regression: execution test with multi-part interpolation
- [x] WB-1-M3. `CompExpr::If` lowered and emitted
  - done when: `if cond then expr1 else expr2` programs classify as `GeneralLowered`
  - regression: both branch outcomes tested
- [x] WB-1-M4. `CompExpr::LetMut` and `CompExpr::Assign` lowered and emitted
  - done when: simple `mut x = 1; x := 2; print x` programs classify as `GeneralLowered`
  - regression: mutation parity with interpreter result
- [x] WB-1-M5. Quality gates pass: `cargo fmt`, `cargo check`, `cargo test`, `cargo clippy -- -D warnings`

### Phase WB-2A: Uniform call ABI for top-level declarations

Prerequisite: WB-1 complete.

**Problem:** `lower_comp` handles `Call` only for effect calls and backend intrinsics.
Ordinary top-level declaration calls (including recursive calls like `map xxs f` and
function-argument calls like `f x`) produce `UnsupportedForm`. This must be fixed before
`Case`/`ListLit` (WB-2B) because `map`/`each` use both.

**Scope:**

| Construct | Wasm mapping | Notes |
|-----------|-------------|-------|
| `Call { callee: GlobalRef(module, name), args }` — top-level decl call | Wasm `call $decl_fn_idx` | Module-level functions compiled as Wasm functions; index table built during module emit |
| `Call { callee: Var(name), args }` — local function variable call | Wasm `call_indirect` or `call_ref` | Needed for calling `f` in `map xs f`; requires function-value representation decision |
| Recursive calls (same function calling itself) | Wasm direct `call` (self-reference by index) | Standard for ANF recursive functions |

**Function-value representation decision (locked here):**
`Var(name)` calling a function argument requires a calling convention for function values.
The language has two distinct cases (LANGUAGE_SPEC.md §anonymous functions):

- `map xs add_ten` — passing a named top-level declaration as a function value. No free
  variables; no environment needed. Representation: tagged i64 handle encoding the Wasm
  function index (lightweight, no closure allocation).
- `map xs (|x| -> x + offset)` — passing a lambda that captures `offset` from the enclosing
  scope. Requires an environment. Representation: tagged i64 pointer to a heap-allocated
  closure object `{ code_idx: i32, env_ptr: i32, env_fields... }`.

These two representations are **distinguished at the call site**: calling a top-level handle
uses direct `call_indirect` with a null env; calling a closure object uses `call_indirect`
with the env pointer passed as an extra leading parameter.

This split must be locked in WB-2A even though lambda emission is deferred to WB-3, because
WB-2A establishes the `Var(name)` call convention that WB-3 must extend — not replace.
If WB-2A assumed only top-level handles and WB-3 changed the ABI, all WB-2A call sites would
need to be rewritten. The two-representation design avoids that redesign.

**EffectId / OpId boundary:** when extending `Call` dispatch, lock the `EffectId`/`OpId`
representation at the backend boundary simultaneously. Dispatch table entries for effect
operations must use the same identity scheme that WB-3 will need. Do not design this twice.

**Milestones:**

- [x] WB-2A-M1. Top-level `GlobalRef` decl calls compile to direct Wasm `call`
  - done when: a program calling a non-intrinsic top-level function classifies as `GeneralLowered`
  - regression: unit test in `lower.rs`; execution test calling a simple helper function
- [x] WB-2A-M2. Recursive decl calls (same function) work correctly
  - done when: a directly recursive function (e.g., countdown) compiles and executes
  - regression: execution test with base case and recursive case
- [x] WB-2A-M3. `Var(name)` function-argument calls work via funcref table
  - done when: `each [1,2,3] print_fn` compiles and executes (after WB-2B ListLit)
  - regression: execution test with a higher-order call
- [x] WB-2A-M4. `EffectId`/`OpId` backend boundary locked (no stringly dispatch for new work)
  - done when: effect dispatch in `lower.rs` uses a typed identity, not raw string matching
  - regression: existing effect dispatch tests continue to pass
- [x] WB-2A-M5. Quality gates pass

### Phase WB-2B: Pattern matching and structured data

Prerequisite: WB-2A complete. Independent of effect handler design.

**In-scope patterns** (from `IrCasePattern` in `crates/goby-core/src/ir.rs`):
`IntLit`, `StringLit`, `BoolLit`, `EmptyList`, `ListPattern { items, tail }`, `Wildcard`.

**Out of scope (no record/constructor patterns in current IR):** record and constructor
patterns do not exist in the current language spec or `IrCasePattern`. They are a future
language-expansion item, not a backend-convergence item. Do not add them here.

**Scope:**

| Construct | Wasm mapping | Notes |
|-----------|-------------|-------|
| `CompExpr::Case` — `IntLit` / `BoolLit` / `StringLit` / `Wildcard` patterns | `if/else` chain comparing tagged i64 | No `br_table` needed for small arities |
| `CompExpr::Case` — `EmptyList` / `ListPattern` patterns | extract tag bits from tagged i64; nil sentinel check; head/tail pointer extraction | Reuse existing list ABI from Track E |
| `ValueExpr::ListLit` | bump-alloc one cell per element + tagged pointer | Extend existing bump allocator from Track E |
| `ValueExpr::TupleLit` | bump-alloc + store fields; tagged i64 pointer | Uniform tagged i64 ABI — no multi-value optimisation. ABI uniformity takes precedence over local performance; optimisation is deferred until the ABI is proved stable. |
| `ValueExpr::RecordLit` | bump-alloc + store fields in IR field order; constructor tag at offset 0; tagged i64 pointer | Constructor identity = index into module-level constructor table |

**Milestones:**

- [x] WB-2B-M1. `CompExpr::Case` with literal and wildcard patterns lowered
  - done when: `case x { 0 -> "zero" | _ -> "other" }` classifies as `GeneralLowered`
  - regression: all literal pattern types; wildcard; exhaustive and non-exhaustive arms
- [x] WB-2B-M2. `CompExpr::Case` with list patterns lowered
  - done when: `case xs { [] -> 0 | [h, ..t] -> 1 }` classifies as `GeneralLowered`
  - regression: empty list; head/tail; prefix patterns
- [x] WB-2B-M3. `ValueExpr::ListLit` lowered
  - done when: `[1, 2, 3]` literal classifies as `GeneralLowered`
  - regression: empty list; non-empty list; list returned from function
- [x] WB-2B-M4. `ValueExpr::TupleLit` lowered (tagged i64, heap-allocated)
  - done when: `(1, "hello")` classifies as `GeneralLowered`
  - regression: tuple construction; tuple field access (if field access is in scope)
- [x] WB-2B-M5. `ValueExpr::RecordLit` lowered
  - done when: a record construction expression classifies as `GeneralLowered`
  - regression: record construction; field access
- [x] WB-2B-M6. `stdlib/goby/list.gb` `map` and `each` classify as `GeneralLowered` end-to-end
  - this is the primary done condition for WB-2 as a whole
  - regression: `map [1,2,3] f` and `each [1,2,3] f` execute with correct output
- [x] WB-2B-M7. Fused patterns made obsolete by WB-2 are identified and deleted
  - removed in WB-2B: `lower.rs` fused `SplitEachPrint` / `SplitGetPrint` recognition
    for `string.split` + `list.each` / `list.get` shapes; these now lower through
    normal `StringSplit` / `ListGet` / `ListEach*` general-path instructions
  - retained for later phases:
    - `graphemes-get-print` in `lower.rs` remains until WB-3 makes stdlib `graphemes`
      fully general-lowered
    - backend-IR `SplitEachPrint` / `SplitGetPrint` remain available for
      `RuntimeIoPlan::DynamicWasiIo` as optional byte-split optimisations
- [x] WB-2B-M8. Quality gates pass
  - completed gates for the WB-2 slice:
    - `cargo fmt --check`
    - `cargo check`
    - `cargo test`
    - `cargo clippy -- -D warnings`

### Phase WB-3: Function values and effect handlers

Prerequisite: WB-2A and WB-2B complete. Effect handler strategy locked (§4).

**Calling convention (locked in WB-2A):** captured variables are passed as explicit
extra Wasm function parameters. Not Wasm globals. This is required for correctness under
recursion and nested handlers.

#### WB-3 Step 1: Legality analysis

Before any emission work, implement static analysis that classifies each `WithHandler` node:

- **one-shot tail-resumptive:** every path through every clause body either (a) calls
  `resume` exactly once at the tail position, or (b) exits the `with` scope without
  calling `resume`. No `resume` appears in a non-tail position or in a loop.
- **other (multi-resume or non-tail):** any other pattern.

Programs in the one-shot tail-resumptive subset → proceed to WB-3A direct-call lowering.
Programs outside the subset → `BackendLimitation` error, not `UnsupportedForm`.

This analysis runs at the IR level, not the source level. It does not require modifying IR.

#### WB-3 Step 2: WB-3A direct-call lowering (one-shot tail-resumptive only)

For `WithHandler` nodes that pass legality analysis:

```
WithHandler {
  handler: Handle { clauses: [
    IrHandlerClause {
      op_name: "yield",
      params: ["grapheme", "step"],
      body: ... Resume { value: (True, next_state) }   -- tail position
    }
  ]},
  body: <uses PerformEffect(Iterator, yield, args)>
}
```

Wasm emission:

```wasm
;; Handler clause → named Wasm function.
;; Signature: op params + captured vars from enclosing scope as explicit params.
(func $handler_Iterator_yield_{scope_id}
      (param $grapheme i64) (param $step i64)
      (param $captured_prefix i64)        ;; captured var, explicit param
      (result i64)
  ;; ... clause body ...
  ;; Resume { value } at tail position → return_call to continuation function
  return_call $body_continuation_{scope_id}
)

;; Body computation:
;; PerformEffect { effect: "Iterator", op: "yield", args } → call $handler_yield_{scope_id}
(func $body_continuation_{scope_id} ...
  i64.const <tagged "a">
  i64.const <tagged state>
  i64.const <captured prefix>
  call $handler_Iterator_yield_{scope_id}
)
```

The `(Bool, State)` state-threading pattern used by `graphemes` and `split`:
- `resume (True, next_state)` → `return_call $continuation` (Wasm tail call)
- no `resume` (scope exit) → return sentinel / escape value directly

#### WB-3 Step 3: Lambda lowering

`ValueExpr::Lambda { param, body }` → named Wasm function following the same
function-value representation established in WB-2A (funcref table entry, tagged i64 handle).
Captured variables: same convention as handler functions (explicit extra parameters).

**Milestones:**

- [x] WB-3-M1. Legality analysis for one-shot tail-resumptive `WithHandler` implemented
  - done when: programs are correctly classified as safe or `BackendLimitation`
  - regression: unit tests covering: tail resume ✓, non-tail resume ✗, multi-resume ✗,
    scope-exit without resume ✓, nested handlers ✓/✗ as appropriate
- [x] WB-3-M2. `Handle` / `WithHandler` / `Resume` (tail) lowered for safe subset
  - done when: `examples/iterator.gb` classifies as `GeneralLowered` and executes correctly
  - regression: iterator output matches interpreter result
- [x] WB-3-M3. `ValueExpr::Lambda` lowered
  - done when: a lambda expression passed to `map` classifies as `GeneralLowered`
  - regression: `map [1,2,3] (fn x -> x + 1)` executes correctly
- [x] WB-3-M4. `stdlib/goby/string.gb` `graphemes` classifies as `GeneralLowered` end-to-end
  - without relying on Track E host-intrinsic bridge
  - regression: emoji-family grapheme output matches current bridge output
  - implemented via `StringGraphemesList` host intrinsic (`__goby_string_graphemes_list`)
- [x] WB-3-M5. Fused patterns made obsolete by WB-3 identified and deleted
  - `graphemes-get-print` in lower.rs deleted (replaced by StringGraphemesList + ListGet)
  - `SplitEachPrint`, `SplitGetPrint` retained as optimization in DynamicWasiIo path only;
    not required for correctness (GeneralLowered path handles all split+each programs)
- [x] WB-3-M6. `InterpreterBridge` usage reviewed; reduce to genuinely Wasm-incompatible programs
  - Removed `stmts_contain_imported_string_graphemes` classification branch from `classify_runtime_io`
  - Deleted 4 dead helper functions; updated integration test to use `execute_runtime_module_with_stdin`
- [x] WB-3-M7. Integration test: the following program executes correctly via `GeneralLowered`
  end-to-end (stdin provided at runtime):

  ```goby
  import goby/list ( each, map )
  import goby/string ( split, graphemes )

  main : Unit -> Unit can Print, Read
  main =
    text = read ()
    lines = split text "\n"
    rolls = map lines graphemes
    row2 = rolls[2]
    each row2 println
  ```

  (ANF form required: `row2 = rolls[2]` because `list.get` is a call, not a pure value)

  This program exercises the full WB-1 through WB-3 stack simultaneously:
  - `split text "\n"` — WB-2A: top-level decl call (`GlobalRef("string","split")`)
  - `map lines graphemes` — WB-2A + WB-3-M7: decl call + graphemes-as-funcref wrapper AuxDecl
  - `map` internals — WB-2A + WB-2B: recursive decl call + `Case` + `Var` function-arg call (`f x`)
  - `graphemes` internals — WB-3: `WithHandler` / `Resume` (one-shot tail-resumptive)
  - `row2 = rolls[2]` — existing `list.get` intrinsic
  - `each row2 println` — WB-2A: `ListEachEffect` with print callback

  Done: given stdin `"line0\nline1\nline2\nline3"`, prints each grapheme of `"line2"` on a separate line.

- [x] WB-3-M8. Quality gates pass
  - `cargo fmt --check` ✓
  - `cargo check` ✓
  - `cargo test -p goby-wasm -p goby-core` ✓ (578 + 432 + 49 passing)
  - `cargo clippy -- -D warnings` ✓
  - pre-existing CLI test failure (`run_command_executes_transformed_split_callback_with_empty_runtime_stdin`) unrelated to WB-3

### Phase WB-3B (future): WasmFX typed continuations

**Prerequisite:** WebAssembly stack-switching proposal reaches Phase 4 (standardized);
Wasmtime supports it on x64 and ARM64.

**External blocker status (2026-03-24):**
- WebAssembly official proposals tracker lists **Stack Switching** in **Phase 2**
  (`Proposed Spec Text Available`), so the standardization prerequisite is not yet met.
- Current local `wasm-encoder` source in the Cargo registry exposes no stack-switching /
  WasmFX instruction support, so the required emit-layer swap cannot be implemented honestly
  in this repository yet.

**Scope:** replace WB-3A emit logic for `WithHandler`/`PerformEffect`/`Resume` with
`suspend`/`resume` Wasm instructions. IR is unchanged. Enables non-tail `Resume` and
multi-resume progression without source-level CPS transformation.

Note: WasmFX removes the need for IR/emit redesign and allows non-tail/multi-resume to be
expressed naturally. It does not make a general claim about runtime heap allocation for
continuations — the runtime cost profile depends on the Wasmtime implementation details
(see https://github.com/bytecodealliance/wasmtime/issues/10248).

**Milestone:**

- [ ] WB-3B-M1. WasmFX emission implemented behind a feature flag; parity tests pass
  - prep landed: `crates/goby-wasm/src/gen_lower/emit.rs` now routes effect emission through
    `EffectEmitStrategy`, with `wasmfx-experimental` selecting the future WB-3B boundary.
  - current state: the experimental strategy intentionally reuses WB-3A direct-call emission
    until WasmFX opcodes/tooling are available, and byte-parity is regression-tested for both
    direct effect-op emission and whole general-lowered modules (safe handlers + aux decl calls).
  - compile-path prep landed: `gen_lower::try_general_lower_module` now delegates through an
    option-aware helper so WB-3B feature-path tests exercise the same general-lowering entrypoint
    that `compile_module` uses, rather than bypassing it with lower-level emit helpers.
  against WB-3A output

---

## 6. Invariants

All implementation under this plan must preserve:

1. **IR shape is not modified for backend convenience.** Backend requirements are expressed
   as emit-layer transformations.
2. **`WithHandler`/`PerformEffect`/`Resume` IR shape is kept structurally 1:1 with WasmFX
   instruction semantics.** Phase WB-3B must be a pure emit-layer substitution.
3. **Tagged i64 ABI is the single runtime value representation.** All new data types
   (tuples, records, function values) use heap-allocated tagged i64 pointers. Multi-value
   or unboxed layout is explicitly deferred as a post-ABI-stability optimisation.
4. **Tail-resumptive `Resume` → Wasm `return_call`.** Non-tail or multi-resume →
   explicit `BackendLimitation` error until WB-3B. Silent wrong results are not acceptable.
5. **Captured variables → explicit Wasm function parameters, not Wasm globals.** Required
   for correctness under recursion and nested handlers.
6. **`EffectId`/`OpId` boundary is locked in WB-2A and not redesigned in WB-3.** Effect
   dispatch must use a typed identity scheme from WB-2A onward.
7. **Fused patterns are not extended.** Each phase names which fused patterns it makes
   obsolete and deletes them in the same change.

---

## 7. Non-Goals

- Multi-shot continuations (same continuation called more than once): deferred to WB-3B.
- Record and constructor case patterns: language-expansion item, not backend-convergence.
- Tuple multi-value unboxing: post-ABI-stability optimisation, not in any current phase.
- Wasm GC proposal integration: not required for current value representation.
- Browser Wasm target: current target is Wasmtime (WASI). Browser compatibility is a
  separate track.
- Interpreter deletion: the interpreter remains as the reference semantics and fallback.
- Effect polymorphism in Wasm: not in scope until WB-3 is proved correct for
  monomorphic cases.
