# Goby Project State Snapshot

Last updated: 2026-03-31

## Current Focus

Track H: Higher-Order Callback Reliability and Multi-Arg Lambda Surface (HOF-M1 through M7 complete).

- Track HOF M1 complete (2026-03-29):
  - `examples/hof_fold_print.gb`, `examples/hof_fold_print.in`, and
    `examples/hof_fold_print.out` now lock the end-to-end acceptance shape for the active HOF track.
  - the fixture uses the existing named two-argument callback surface to isolate the
    runtime contract before `fn acc x -> ...` lands.
  - focused regressions also lock the inline-curried callback failure.
- Track HOF M2 complete (2026-03-29):
  - ordinary higher-order callback checking now validates inline lambdas against the expected
    callback function type instead of inferring them as `Unknown -> ...`.
  - focused regressions lock arity mismatch and result-shape mismatch diagnostics.
- Track HOF M3 complete (2026-03-30):
  - effectful callback policy confirmed: covered effectful callback path is supported,
    not rejected by `goby check` or at runtime.
  - `goby check examples/hof_fold_print.gb` passes; runtime acceptance gate passes.
  - HOF effect propagation rules remain intentionally deferred per §2.3.
- Track HOF M4 complete (2026-03-30):
  - `fn a b -> expr` is now the canonical multi-parameter anonymous function syntax.
  - desugaring at parse time: `fn a b -> expr` → nested `|a| -> |b| -> expr` AST; no IR change.
  - `|a| -> |b| -> ...` curried spelling is now rejected by the parser.
  - `fn` is reserved as a keyword.
  - formatter emits `fn a b -> expr` for nested lambdas.
  - `doc/LANGUAGE_SPEC.md` updated.
  - block-body `fn` form deferred to HOF-M6.
- Track HOF M5 complete (2026-03-30):
  - `fn a b -> expr` confirmed to lower through the same callable model as `|x| -> expr`;
    no new parallel branch in IR or runtime.
  - `map` + `fn` form and `each` + `fn` form parity tests added to `runtime_output_tests.rs`.
  - `fold` + inline `fn acc x ->` form: typechecks and parses; runtime execution deferred
    (general lowering does not yet support inline Lambda IR for multi-param fold callbacks;
    same limitation applies to named callbacks, not specific to fn form).
    Named callback runtime gate remains `executes_hof_fold_print_example_with_locked_stdin_and_stdout`.
  - block-body `fn a b ->` with indented body deferred to HOF-M6.
- Track HOF M6 complete (2026-03-30):
  - `fn` keyword added to all syntax-highlighting tooling:
    `tooling/syntax/textmate/goby.tmLanguage.json`,
    `tooling/vscode-goby/syntaxes/goby.tmLanguage.json`,
    `tooling/emacs/goby-mode.el`,
    `tooling/vim/syntax/goby.vim`,
    `tooling/nvim/syntax/goby.vim`.
  - `tooling/syntax/testdata/highlight_sample.gb` updated with `fn x y -> x + y` example.
  - README tables updated in `tooling/syntax/README.md`, `tooling/vscode-goby/README.md`,
    `tooling/emacs/README.md`.
- Track HOF M7 complete (2026-03-31):
  - end-to-end acceptance gate passes: `cat examples/hof_fold_print.in | cargo run -p goby-cli -- run examples/hof_fold_print.gb` stdout matches `examples/hof_fold_print.out` exactly.
  - CLI diagnostic messages (`parsed and typechecked`, `generated wasm:`, `wasmtime not found`) moved from stdout to stderr; all CLI integration tests updated accordingly.
  - `run_command_hof_fold_print_acceptance_gate` integration test added to `cli_integration.rs`.
  - `parser_expr.rs` clippy `manual-strip` lint fixed (`strip_prefix`).
  - Track H is fully complete.

Earlier completed work (for reference):

- IR0–IR11, WB-1/WB-2/WB-3 complete; `doc/PLAN_IR.md` contains the Wasm backend lowering design.
- WB-3B on hold: externally blocked; WebAssembly stack switching proposal has not reached Phase 4.
- Track E complete (2026-03-27): HOF callback typechecking; shared matcher in `typecheck_unify.rs`.
- Track ER complete (2026-03-29): error reporting with CLI/LSP parity; see `doc/PLAN_ERROR.md`.
- Track fold complete (2026-03-28): `fold : List a -> b -> (b -> a -> b) -> b` in `stdlib/goby/list.gb`.
- Track F complete (2026-03-25): `goby/int.to_string`.
- Track operators complete (2026-03-26): `||`, unary `!`, comparison precedence locked.
- Track C4/S1–S4 complete (2026-03-25): stdlib `split` ownership fully stdlib-driven.

## Track Priority

**No active stdlib split-retirement work remains.**
C4 plus S1/S2/S3 are complete: source-level `split` ownership is fully stdlib-driven, and the
fallback/runtime-output path no longer carries a source-level legacy `string.split` branch.

## Immediate Next Steps

**Track H: Higher-Order Callback Reliability and Multi-Arg Lambda Surface — complete (2026-03-31):**
All HOF-M1 through HOF-M7 milestones are complete. The acceptance gate
`cat examples/hof_fold_print.in | cargo run -p goby-cli -- run examples/hof_fold_print.gb`
produces stdout matching `examples/hof_fold_print.out` exactly.

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
