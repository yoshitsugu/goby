# Goby Project State Snapshot

Last updated: 2026-03-28

## Current Focus

- IR0–IR11 complete. `doc/PLAN_IR.md` now contains the Wasm backend lowering design (§4–§5).
- Unicode grapheme backend support is complete: backend intrinsics, host runtime
  execution, and stdlib handoff work landed end-to-end.
- Phase WB-1 complete (2026-03-24): `If`, `BinOp`, `Interp`, `LetMut`, `Assign` all lowered and emitted.
- Phase WB-2A complete (2026-03-24): top-level `DeclCall`, recursion, funcref-table indirect calls, typed backend effect identities.
- Phase WB-2B complete (2026-03-24): `Case` literal/wildcard/list patterns, `ListLit`, `TupleLit`, `RecordLit`, stdlib `list.each` / `list.map`.
- Phase WB-3 complete (2026-03-25): `Handle` / `WithHandler` / tail `Resume` lowered for one-shot tail-resumptive subset; `ValueExpr::Lambda` lowered; `graphemes` end-to-end via `StringGraphemesList` host intrinsic; `InterpreterBridge` graphemes classification removed; the representative runtime-`Read` composed stdlib path is now covered by classification and execution regressions.
  - M1: legality analysis implemented.
  - M2: safe handler lowering complete; `examples/iterator.gb` executes via `GeneralLowered`.
  - M3: `ValueExpr::Lambda` lowered; `map [1,2,3] (fn x -> x+1)` executes correctly.
  - M4: `StringGraphemesList` host intrinsic; graphemes classifies as `GeneralLowered` end-to-end.
  - M5: `graphemes-get-print` fused pattern deleted; `SplitEachPrint`/`SplitGetPrint` retained in `DynamicWasiIo` path only.
  - M6: `InterpreterBridge` graphemes classification removed; 4 dead helper functions deleted.
  - M7: `graphemes`-as-funcref wrapper AuxDecl plus regression coverage for the representative
    runtime-`Read` composed path (`read -> split -> map(graphemes) -> list.get -> each(println)`)
    and required alias/import variants.
  - M8: quality gates pass (`cargo fmt`, `cargo check`, `cargo test`, `cargo clippy -- -D warnings`).
- General call-argument ANF normalization complete (2026-03-25): ordinary shared-IR
  `Call` lowering now hoists non-value callees/arguments into left-to-right `let`
  bindings, so inline forms such as `each (rolls[2]) println` lower and execute
  through the same path as explicit staging binds.
- CLI/runtime stdin ownership corrected (2026-03-26): `goby run` no longer pre-consumes
  interactive stdin for `GeneralLowered` programs that do not perform `Read`.
  `goby-wasm` now exposes a narrow stdin-requirement predicate so CLI execution keeps
  seeded stdin for true `Read` / `InterpreterBridge` cases while allowing lambda-only
  `GeneralLowered` programs to run immediately in interactive shells.
- Boolean operator surface extended (2026-03-26): `||` and unary `!` are now supported.
  Parser precedence is locked so comparisons bind tighter than `&&` / `||`, and the
  active examples/spec now reflect the implemented operator set.
- Track H complete (2026-03-26):
  - tuple member access no longer leaks into `gen_lower/emit` as pseudo-local names such as
    `pair.0`; `examples/bugs/runtime_read_tuple_member.gb` now runs and prints `1`.
  - runtime-`Read` helper closure-capture fixtures no longer collapse into the generic runtime-I/O
    unsupported-shape error; `goby run examples/bugs/runtime_read_captured_lambda.gb` now reports a
    precise closure-capture limitation from the general-lowering boundary.
  - `doc/BUGS.md` is now empty of active entries; the `examples/bugs/` fixtures remain as
    regressions for the resolved Track H cases.
- Track H H6 complete (2026-03-26): shared-IR lowering now ANF-normalizes non-value `if`
  conditions before constructing `CompExpr::If`.
  - helper-call conditions such as `if is_two n` no longer fail with
    `if condition must be a pure value expression in shared IR today`.
  - runtime-`Read` modules that previously collapsed this failure into the generic
    runtime-I/O unsupported-shape error now classify and execute via `GeneralLowered`.
  - focused regressions landed in `goby-core::ir_lower` and `goby-wasm::lib` tests.
- Track E complete (2026-03-27): ordinary call validation now rejects resolved
  higher-order callback mismatches during typecheck instead of deferring them to runtime.
  - shared matcher extraction landed in `typecheck_unify.rs`; callback compatibility now reuses
    fresh type-variable instantiation plus the existing substitution/unification machinery.
  - ordinary `Expr::Call` chains now validate function-typed arguments in a dedicated
    `typecheck_call` module, preserving the existing phase split (`ensure_known_call_targets_in_expr`
    for unresolved calls, `check_expr` for inference).
  - focused regressions now cover:
    - `each [1, 2] println` rejection with a higher-order mismatch diagnostic,
    - `each ["a", "b"] println` acceptance,
    - `map [1, 2] println` rejection,
    - qualified callback mismatch,
    - generic callback acceptance,
    - partially applied callback mismatch.
  - current implementation targets ordinary nested `Expr::Call` chains; if future pipeline surface
    feeds function-valued arguments through a distinct validation path, extend Track E from there
    rather than special-casing symbols.
- `List.fold` planning refined (2026-03-28):
  - `doc/PLAN.md` now treats `goby/list.fold` as the next planned stdlib feature built on a
    shared higher-order call path rather than a symbol-specific compiler/runtime exception.
  - the plan now locks the intended observable semantics as left fold under the public name `fold`,
    adds milestone checklists (`FOLD-M1`..`FOLD-M5`), and makes the completion criteria explicit.
  - the next implementation work should start from `FOLD-M1`: lock callback shape, long-term API
    rationale, and temporary callback/effect policy before changing lowering/runtime code.
- Dependency lock stabilized (2026-03-28):
  - workspace now patches `crypto-common 0.1.7` from `vendor/crypto-common-0.1.7` so its
    `generic-array` pin can move from `0.14.7` to `0.14.9`.
  - `Cargo.lock` now resolves `generic-array v0.14.9`, removing the repeated
    `Adding generic-array v0.14.7 (available: v0.14.9)` notice during lockfile refresh.
  - current tradeoff: the vendored patch emits upstream deprecation warnings about
    `generic-array 1.x`, but `cargo check` / `cargo test` remain green and no Goby code depends on
    this crate directly.
- WB-3B prep slice in progress (2026-03-24): `gen_lower/emit.rs` now has an
  `EffectEmitStrategy` boundary and `wasmfx-experimental` feature flag so future WasmFX work can
  replace the emit path without redesigning IR/lowering; current strategies are parity-tested to
  emit identical bytes for supported effect ops and representative general-lowered modules
  (safe handler-only main, helper decl + read path).
- WB-3B compile-path prep extended (2026-03-24): general lowering now exposes an internal
  option-aware Wasm emission helper so strategy parity tests run through the same
  `try_general_lower_module` entrypoint used by `compile_module`.
- WB-3B remains externally blocked as of 2026-03-24:
  - WebAssembly official proposals tracker does not yet satisfy the Phase 4 restart condition.
  - Local `wasm-encoder` source exposes no stack-switching/WasmFX instruction support.
- WB-3B is on hold until all restart conditions in `doc/PLAN_IR.md` Phase WB-3B are met.
- **WB-3 is complete.** All 13 `CompExpr` variants and all 12 `ValueExpr` variants are handled
  in the `GeneralLowered` path within the currently supported subsets, including the representative
  runtime-`Read` composed stdlib path.

## Track Priority

**No active stdlib split-retirement work remains.**
C4 plus S1/S2/S3 are complete: source-level `split` ownership is fully stdlib-driven, and the
fallback/runtime-output path no longer carries a source-level legacy `string.split` branch.

## Immediate Next Steps

**Track ER: Compiler Error Reporting (active track):**
Precise unresolved-name / import diagnostics. `ER0` and `ER1` are complete in
`doc/PLAN_ERROR.md`:
- ownership split is locked (`goby-core` diagnoses; CLI/LSP render),
- shared expression-span helpers now live in `crates/goby-core/src/typecheck_span.rs`,
- current parser behavior is audited and regression-locked: statement spans exist, nested
  callee token spans generally do not,
- pipeline callee token spans are explicitly deferred until an AST/data-model change adds
  ownership for that location.
Start implementation from `ER2`: migrate unresolved bare-name diagnostics in
`typecheck_stmt.rs` and adjacent checker paths to use the shared helper layer, then classify
any remaining `span: None` outcomes explicitly instead of leaving them implicit.
Immediate acceptance target: the representative `map`-not-imported case underlines the `map`
token in both CLI and LSP.

**Track stdlib (C4-S1) — complete (2026-03-24):**
`cargo run -p goby-cli -- check stdlib/goby/string.gb` now succeeds.
The typechecker accepts the required `List String` state initialization shape and preserves
outer `mut` locals through stdlib-style `with ... in` bodies.

**Track stdlib (C4-S2) — complete (2026-03-25):**
Stabilize the shared iterator state contract by keeping `GraphemeState` in
`stdlib/goby/iterator.gb` as the canonical declaration and removing duplicated local copies
from `stdlib/goby/string.gb`.
Exit criterion: no duplicated `Iterator` / `GraphemeState` declarations remain across stdlib modules.

**Track stdlib (C4-S3) — complete (2026-03-25):**
`stdlib/goby/string.gb` now handles multi-grapheme delimiters through the stdlib-owned path, and
`split` no longer calls `string.split(value, sep)` for any delimiter case.
Focused Wasm execution coverage now locks a representative multi-delimiter case with
leading/consecutive/trailing empty segments preserved.

**Track stdlib (C4-S4) — complete (2026-03-25):**
Fixed `split text ""` Wasm trap via `StringGraphemesList` redirect (commit a9f85fa3).
`StringSplit` intrinsic retained for non-empty delimiter (stdlib `split_with_*` functions use
record field access unsupported by emit). Refactored stdlib `split`/`split_multi_parts`/
`split_with_multi_delimiter` to use `let` bindings (required by `ir_lower.rs` constraints).
Added tests: empty-delimiter ASCII/emoji, single-char delimiter, leading/trailing empty segments,
`split "" "" -> []`, Unicode multi-grapheme delimiter execution, and `examples/import.gb`
compile-path parity.

**Track stdlib (S1/S2/S3) — complete (2026-03-25):**
The remaining generic evaluator gaps behind stdlib `split` helper execution are closed.
Declaration/body execution now uses the shared fallback path for imported/local stdlib helpers,
parser statement recovery now preserves multiline indented RHS/`resume` continuations needed by
`stdlib/goby/string.gb`, and the runtime-output fallback no longer relies on source-level
`string.split` special cases in `runtime_decl.rs`, `runtime_expr.rs`, or `runtime_resolver.rs`.
Focused split regressions pass for selective import, canonical qualified call, empty delimiter,
and Unicode multi-grapheme delimiter behavior after branch removal. Backend `StringSplit`
remains only as a Wasm lowering/runtime detail; it is no longer part of source-level fallback
semantics.

**Track WB-3B (future, deferred):**
WasmFX typed continuations — currently on hold.
Restart only when the external prerequisites in `doc/PLAN_IR.md` Phase WB-3B are satisfied.

**Track H (complete, 2026-03-26):**
Runtime-lowering correctness at the shared IR / general-lowering boundary for runtime-`Read`
programs.
Completed slices:
- `H1`/`H2`: tuple projection canonicalization + backend lowering
- `H3`/`H4`: unsupported-reason plumbing + precise closure-capture diagnostics in the
  `execute_runtime_module_with_stdin` path
- `H6`: ANF lowering for non-value `if` conditions
- `H7`: compile/CLI-path parity for precise unsupported reasons
Constraints remain:
  - no syntax change
  - no new `runtime_io_plan.rs` shape recognizers

**Track runtime memory (complete, 2026-03-27):**
Host-owned temporary string/list allocations no longer collide with compiled static-string data
in general-lowered Wasm modules.
Completed slices:
- `gen_lower/emit.rs` now reserves the host bump region at the top of linear memory instead of
  placing static strings there.
- host bump capacity increased to handle representative `graphemes` + recursive multi-part
  interpolated `println` workloads without truncating or corrupting output.
- focused `goby-wasm` regression coverage now locks the `split -> map(graphemes) -> recursive
  interpolated println` shape.

**Track E (complete, 2026-03-27):**
Higher-order function-type checking.
All E1–E5 milestones complete. Callback positions such as `each xs println` are rejected during
`goby-cli check` with a resolved-name-but-wrong-function-type diagnostic. The shared matcher in
`typecheck_unify.rs` handles named, qualified, generic, and partially applied callbacks uniformly.
Regressions cover direct/qualified/named/generic/partial-application callback cases. Future
non-`Expr::Call` caller paths can extend the same shared matcher boundary if needed.

**Track fold (complete, 2026-03-28):**
All FOLD-M1..M5 milestones complete.
- `FOLD-M1`: callback shape locked — `fold : List a -> b -> (b -> a -> b) -> b` (curried)
- `FOLD-M2`: lower.rs baseline tests for 2-arg callback lowering
- `FOLD-M3`: `IndirectCall { arity: u8 }` generalization; arity=2 adds `(i64,i64)->i64`
  Wasm type; no fold-specific compiler branch; 2-arg IndirectCall integration tests pass
- `FOLD-M4`: `fold` added to `stdlib/goby/list.gb` as ordinary recursive stdlib code
- `FOLD-M5`: semantic coverage (sum, empty-list, single-element, non-commutative order,
  fold+each combination); `examples/fold.gb` added; `doc/LANGUAGE_SPEC.md` updated
All 540 goby-wasm tests pass.
- only then add lower-level regressions for the underlying higher-order call-path gap
Constraints:
- no `fold`-specific intrinsic
- no symbol-name special casing in lowering/runtime
- no API choice justified only by a temporary backend quirk

**Track runtime execution (2026-03-26):**
The interactive-shell stdin hang is fixed for lambda-only `GeneralLowered` programs.
Representative CLI regression coverage now keeps stdin open while asserting that
`map`/`each` output still completes without EOF, and existing `Read` / grapheme stdin
execution regressions remain green.

**Track operators (complete, 2026-03-26):**
`||` and unary `!` now parse, typecheck, format, and execute.
Comparison precedence is regression-locked above logical conjunction/disjunction, and
`examples/operators.gb` now runs cleanly through the CLI using string interpolation.

**Track F (complete, 2026-03-25):**
`goby/int.to_string : Int -> String` is implemented end-to-end.
- canonical decimal text is provided through stdlib (`0`, `123`, `-7`)
- direct calls and named callback use (`map xs int.to_string`) are covered in
  typecheck, fallback runtime resolution, and compiled Wasm execution tests
- fallback callable capture now canonicalizes imported qualified callbacks to
  module paths so alias spelling does not leak into imported stdlib execution

Residual:
- qualified iterator handler clauses currently remain outside formatter
  idempotence/runtime-output/general-lowered smoke locking; the affected tests
  are explicitly `ignore`d until that surface is supported consistently

## Architecture State

- Resolved-form → shared IR boundary is stable (IR0–IR11 done).
- Wasm backend lowering design is locked in `doc/PLAN_IR.md`:
  - Phase WB-1: pure control flow and operators ✓
  - Phase WB-2: pattern matching and structured data ✓
  - Phase WB-3: function values and effect handlers (direct-call lowering, one-shot tail-resumptive) ✓
  - Phase WB-3B (future): WasmFX stack switching when proposal reaches Phase 4
- All `CompExpr` and `ValueExpr` variants have backend lowering coverage, including the
  representative runtime-`Read` composed stdlib path used as the acceptance shape for WB-3-M7.
- `GeneralLowered` coverage includes:
  - Pure control flow: `If`, `BinOp`, `Interp`, `LetMut`, `Assign`
  - Pattern matching: `Case` with literal/list patterns
  - Structured data: `ListLit`, `TupleLit`, `RecordLit`
  - Decl calls / recursion / higher-order funcref calls
  - Backend effect dispatch (typed `BackendEffectOp` / `BackendPrintOp`)
  - stdlib `list.each` / `list.map`
  - Effect handlers: `Handle` / `WithHandler` / tail `Resume` (one-shot tail-resumptive subset)
  - Function values: `Lambda` (no-capture only); stdlib `graphemes` via wrapper AuxDecl
  - Host intrinsics: `StringGraphemesList` (`__goby_string_graphemes_list`)
- Fused patterns deleted or retained as optimization only:
  - `graphemes-get-print` deleted (WB-3-M5)
  - `SplitEachPrint` / `SplitGetPrint` retained in `DynamicWasiIo` path as optimization (correctness not required)
- WB-3 exit state:
  - non-tail / multi-resume handlers produce `BackendLimitation` error (not silent miscompilation)
  - lambda with free variables (closure capture) produces `UnsupportedForm` (WB-3B deferred)
  - this remains an execution limitation, not a runtime-I/O-shape limitation; next work is to
    surface that distinction explicitly in diagnostics and internal lowering results

## Key Entry Points

- `doc/PLAN_IR.md` — Wasm backend lowering design and phase plan
- `crates/goby-wasm/src/gen_lower/lower.rs` — `lower_comp` / `lower_value`
- `crates/goby-wasm/src/gen_lower/emit.rs` — Wasm instruction emission
- `crates/goby-wasm/src/gen_lower/backend_ir.rs` — backend IR instruction set
