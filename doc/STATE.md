# Goby Project State Snapshot

Last updated: 2026-05-08 (Track Float E1–E4 + E5-A landed; E5-B —
`Float, Float -> Float` / `Float, Float -> Bool` Wasm emission — is
the next active line. Track PC remains queued behind user design
review.)

## Current Focus

**Track Float: `Float` / Wasm `f64` Support** (`doc/PLAN.md` §4.4).

Locked surface (LANGUAGE_SPEC §3): literal grammar `<int>.<frac>` only
(`1.0`, `0.5`, `-3.25`); `Float` is a distinct primitive with no implicit
`Int`/`Float` coercion; `+`/`-`/`*`/`/`/`==`/`<`/`<=`/`>`/`>=` overload
on operand type; `Int / Int` keeps integer division; runtime semantics
follow IEEE 754 (NaN ≠ NaN, division by zero produces ±Infinity / NaN);
printing follows Haskell `show` (`1.0`, `Infinity`, `-Infinity`, `NaN`,
`-0.0`, `0.0`).

Phase progress:

- **E1** complete (`9c6e5e3`): semantics lock, doc updates only.
- **E2** complete (`80ba5c9`): `FloatBits(u64)` newtype, `Expr::FloatLit`,
  `ResolvedExpr::FloatLit`, `ValueExpr::FloatLit`, `Ty::Float`,
  `parse_float_literal` (qualified-access still wins for `pair.0`),
  exhaustive-match pass across `goby-core` + `goby-wasm`.
- **E3** complete (`fe143db`): typecheck operator dispatch
  (`Float, Float -> Float` for arithmetic, `Float, Float -> Bool` for
  comparisons), typed IR variants (`IrBinOp::Float{Add,Sub,Mul,Div,Eq,
  Lt,Gt,Le,Ge}`), shallow lowering dispatcher in `ir_lower`, and a
  workspace-wide wasm safety net: every `goby_wasm` public compile /
  execute entry point and `runtime_io_execution_kind` reject any module
  containing a `Float` literal with a clear "Float values are not yet
  runnable on the wasm path (Track Float Phase E5)" `CodegenError` so a
  var-rooted Float operand cannot silently flow through integer wasm
  ops.
- **E4** complete: Wasm runtime value representation lock — heap-boxed
  `f64` plus a new `TAG_FLOAT` (`0xB`). The current 4-bit tag + 60-bit
  payload cannot carry arbitrary IEEE 754 bit patterns, so `Float`
  values must live behind a heap pointer alongside `List` / `Tuple` /
  `Cell` payloads. `gen_lower/value.rs` now exposes `encode_float_ptr` /
  `decode_float_ptr` (Cell-shaped tagged pointer) and the pure bit
  helpers `float_bits_to_i64` / `i64_to_float_bits`. Heap allocation
  itself, `f64.const` / `f64.add` / ... emission, and the interpreter
  fallback are deferred to E5; the E3 wasm safety net (rejection of
  modules containing Float literals) is intentionally still in place.
- **E5** in progress (split into E5-A → E5-D in `doc/PLAN.md` §4.4):
  - **E5-A** complete: Float literal end-to-end on the Wasm path.
    `WasmBackendInstr::AllocFloatBox { bits_instrs }` allocates the
    8-byte `(bits: i64)` box and emits a TAG_FLOAT-tagged pointer.
    `__goby_dup` / `__goby_drop` learn TAG_FLOAT (drop is an
    *independent* branch from Cell — raw IEEE bits must not be
    recursively dropped — but reuses the Cell free-list slot).
    `format_float` in `gen_lower/value.rs` renders Haskell-`show`
    form and is shared between the host TAG_FLOAT decoder
    (`wasm_exec::format_tagged_value`) and the future interpreter
    fallback. The Phase E3 module-level safety net is removed; the
    GeneralLowered capability gate gains a `has_float_in_comp` clause
    so `Float` programs no longer fall into the static-output /
    native-fallback path that cannot render TAG_FLOAT.
  - **E5-B** active: replace the deferred `IrBinOp::Float*` codegen
    errors with real `unbox → f64.<op> → rebox` Wasm emission. Stack
    order for non-commutative ops must be locked via scratch locals,
    and `==` follows IEEE 754 (`f64.eq`) — not bit-eq.
  - **E5-C** queued: interpreter fallback (`RuntimeValue::Float`,
    `NativeValue::Float`, runtime equality and printing through
    `format_float`).
  - **E5-D** queued: parity acceptance + minimal
    `examples/float_basics.gb` and removal of the E5-B interim guard
    test.
- **E6** queued: examples / formatter idempotence / LSP hover / final
  PLAN + STATE sync.

**Track PC** (`doc/PLAN.md` §4.6) remains queued — design exploration is
in progress in `tmp/pc.md` (`Parser` shape, effect protocol,
backtracking, error type). PC-2 is still gated on §3.3 multi-shot /
branch-local state.

## Known Red / Green State

Green:

- `cargo test -p goby-core --lib`: 916 passed / 2 ignored.
- `cargo check --workspace`: warning-free.
- `cargo nextest run -p goby-wasm -E 'not test(fold_m5_string_accumulator)'`:
  the regular wasm suite passes.

Red / ignored:

- Pre-existing `#[ignore]`d perceus / compile_tests entries from M10
  closure (see `doc/PLAN.md` §4.2).
- `goby-wasm` lib test `tests::fold_m5_string_accumulator` is a known
  CPU-bound hang on this checkout; track it via `doc/BUGS.md` and skip
  it in nextest runs.

## Next Step

**Primary (Track Float E5-B):**

Replace the deferred `IrBinOp::Float{Add,Sub,Mul,Div,Eq,Lt,Gt,Le,Ge}`
codegen errors in `gen_lower/emit.rs` with real Wasm emission:

1. For each Float operand on the stack (TAG_FLOAT-tagged i64),
   `& 0xFFFF_FFFF` to obtain the box pointer, `i64.load offset=0`
   to read the IEEE 754 bits, `f64.reinterpret_i64` to materialise
   the `f64`. Store left/right into scratch locals first so the
   `f64`s end up on the stack in the original `left, right` order
   for non-commutative ops (`f64.sub`, `f64.div`, `f64.lt`, ...).
2. Arithmetic (`Add`/`Sub`/`Mul`/`Div`): emit `f64.<op>`, then
   `i64.reinterpret_f64`, then a fresh `WasmBackendInstr::AllocFloatBox`
   with `bits_instrs = [the reinterpreted i64 already on the stack]`.
   Reusing the same `AllocFloatBox` variant keeps allocation shape
   identical between literals and computed results.
3. Comparisons (`Eq`/`Lt`/`Gt`/`Le`/`Ge`): emit `f64.<cmp>` to get
   an i32, then `bool_from_i32!` to a TAG_BOOL i64. `==` follows
   IEEE 754 (`f64.eq`); do **not** use bit-eq, otherwise
   `NaN == NaN` would surface as True and `-0.0 == 0.0` would
   surface as False, both contradicting `LANGUAGE_SPEC §3`.
4. Re-enable `compile_tests::compile_module_rejects_var_rooted_float_arithmetic_until_phase_e5b`
   (currently `#[ignore]`) — the helper `my_add a b = a + b` plus
   `print "${my_add 1.0 2.0}"` should now succeed end-to-end and
   render `3.0`. Replace its rejection assertion with an execution
   parity assertion, or fold it into the E5-D parity acceptance.

**Parallel (queued, parallelizable):**

- **PC-0 design lock** (`doc/PLAN.md` §4.6): once the user finishes the
  design review in `tmp/pc.md`, motivating fixtures (`parser_number`,
  `parser_arith`, `parser_json_lite`) land as failing examples.
- **§3.3 multi-shot classification + branch-local state surface**: the
  only remaining hard PC-2 blocker.

**Other queued tracks (lower priority):**

- Track OOB (out-of-bounds handling polish).
- Track D D5/D6 follow-ups (`goby lint`).
- Track RR-6 limit tuning.

**Parallel known-red cleanup (lowest priority):**

- Pre-existing typecheck regressions in 8 example files
  (`case_arm_block.gb`, `function_reference.gb`, `list_set.gb`,
  `list_spread.gb`, `mut.gb`, `string_graphemes.gb`, `tco.gb`,
  `to_integer.gb`) reproduce on the `975863e` baseline. Triage
  separately with a `doc/BUGS.md` entry per case.

## Recently Closed (Reference Only)

- **Effect row polymorphism** (Track EP, 2026-05-07): row-polymorphic
  callback signatures (`can ..., {e}`) propagate effects through stdlib
  HOFs and user-defined HOFs; closed callback rows reject effectful
  lambdas; diagnostic wording distinguishes "missing effect in closed
  row" from "row variable cannot be unified" (LANGUAGE_SPEC §5).
  Implementation pieces: row representation (`unify_effect_rows`,
  `apply_row_substitution`), lambda inference
  (`infer_expr_effects`, `infer_curried_lambda_body_effects`,
  `infer_call_effects_at_site`), and the decoupling of `decl_can_ops`
  from `covered_ops` so callback effects surface to the row variable
  instead of being swallowed by an outer `can`. 13 `ep3_*` acceptance
  tests in `crates/goby-core/src/typecheck.rs` plus the EP-2 acceptance
  set pin the contract.
- **Track E (Perceus)**: M0–M11 complete. Durable design in
  `doc/PLAN.md` §4.2; runtime-allocator unification, `LetMut`-aware
  `return_ownership_value`, and the 138×138 stdin acceptance test all
  shipped.
- **Generic TCO (RR-5)** and **Sequence-backed `List`**: published
  contracts in `doc/LANGUAGE_SPEC.md`.
- **WB-4C lexical handler metadata**: shared IR carries `EffectOpId`;
  handler-clause lowering and legality analysis use `effect + op`
  identity when available. Lexical target records for `WithHandler` /
  `PerformEffect` remain a follow-up slice.
