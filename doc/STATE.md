# Goby Project State Snapshot

Last updated: 2026-04-18 (bitwise XOR `^` landed)

## Current Focus

**The `^` (bitwise XOR) prerequisite is landed; Perceus M1 acceptance harness
(checksum capture, un-ignoring the compile test) is the next work item.**
The pipeline now parses/type-checks/lowers/emits `^` end-to-end; the M1 goal
program still fails to compile (stack overflow), which is the next investigation
target (separate from the `^` work).

---

## Perceus M1 — what is already in place

The groundwork commit `bdf7d327 "Add closed literal hoisting groundwork"`
landed the structural pieces for M1:

- `crates/goby-core/src/closed_literals.rs` — `is_closed_literal` /
  `collect_closed_literals`, conservatively accepting `ListLit` / `TupleLit`
  / `RecordLit` whose leaves are scalar literals or nested closed aggregates
  (no `Var`, no spread tail, no `BinOp`).
- `crates/goby-wasm/src/gen_lower/emit.rs` — `STATIC_REFCOUNT_SENTINEL =
  u64::MAX`, `StaticHeapValue` intern pool, `alloc_static_list / tuple /
  record`, sentinel refcount write at the header slot.
- `crates/goby-wasm/src/gen_lower/lower.rs` — detects closed literals in a
  `Value` position and emits `WasmBackendInstr::PushStaticHeap` instead of
  per-call allocation.
- `examples/refcount_reuse_loop.gb` — normative goal program per
  `doc/PLAN_PERCEUS.md` §1.1 (length 4096, 5000 iterations).
- `crates/goby-wasm/tests/wasm_exports_and_smoke.rs` — two integration tests:
  `refcount_reuse_loop_example_parses` (active) and
  `refcount_reuse_loop_example_compiles` (currently `#[ignore]` citing
  "Perceus goal harness is added in M1").

Baseline `cargo check -p goby-core -p goby-wasm` is green with this
groundwork.

## Perceus M1 prerequisite landed on 2026-04-18: bitwise XOR (`^`)

The normative goal program uses bitwise XOR:

```goby
xor_fold : List Int -> Int -> Int
xor_fold xs acc =
  case xs
    [] -> acc
    [x, ..rest] -> xor_fold rest (acc ^ x)
```

`doc/PLAN_PERCEUS.md` §1.1 marks this source as **normative** — implementers
must not "equivalent"-rewrite it, because the acceptance checksum is pinned
to the exact allocation and evaluation pattern. `^` was therefore added
end-to-end before resuming M1 proper.

Changes landed (Int-only; no Bool XOR, no AND/OR/shift, no constant folding
in `closed_literals.rs`):

- AST / IR: `BinOpKind::BitXor` in `ast.rs` and `IrBinOp::BitXor` in `ir.rs`,
  plus `"^"` in the Debug / formatter / `to_str_repr` renderers.
- Parser: `split_top_level_binop(_, '^')` arm in `parser_expr.rs::parse_expr`
  between `&&` and `==`, so `^` binds tighter than `==` but looser than `&&`
  and `a ^ b == c` parses as `(a ^ b) == c`.
- Typecheck: `(BitXor, Int, Int) => Int` arm in `typecheck_check.rs`, modeled
  on `Mod`.
- IR lowering: `ir_lower.rs::lower_binop` and
  `goby-wasm/src/wasm_exec_plan.rs::ir_binop_to_ast` updated.
- Wasm emit: `gen_lower/emit.rs` emits `i64.xor` + `retag_int!` (tag bits
  cancel under XOR, so the tag is reinstalled with `PAYLOAD_MASK | TAG_INT`).
  `lower.rs` native interpreter has the matching `Int ^ Int => Int` arm.
- Smoke: `wasm_exports_and_smoke.rs::bitxor_smoke_basic` and
  `bitxor_smoke_associativity_and_precedence`.
- Spec: `doc/LANGUAGE_SPEC.md` operator precedence line updated and `^` added
  to the left-associative list, with a one-line note that it is bitwise XOR
  on `Int`.

## Immediate Next Actions

1. Investigate the `refcount_reuse_loop_example_compiles` stack overflow
   surfaced under `-- --ignored`. With `^` in place the failure is no longer
   a missing-operator artefact; it indicates either a lowering infinite
   recursion or a genuinely deep recursive pass. Reproduce with a reduced
   input (e.g. length 16 + few iterations) and bisect the lowering pipeline.
2. Once the compile path is healthy, capture the `goby check
   examples/refcount_reuse_loop.gb` checksum, record it as an `assert_eq!`
   literal in the integration test, and un-ignore the compile-only case.
   Keep the `goby run` case ignored citing `doc/PLAN_PERCEUS.md` M5.
3. Extend `tooling/` syntax highlight definitions to cover `^` (tracked as a
   TODO under `doc/PLAN.md` §4.2.1).

## Verification snapshot (2026-04-18)

- `cargo fmt --all` and `cargo check --all-targets` — workspace green.
- `cargo test -p goby-core` — 697 pass, 0 failed, 2 ignored (pre-existing).
- `cargo test -p goby-wasm` — 66 + 2 new pass, 0 failed, 3 ignored.
- `cargo test -p goby-wasm bitxor_smoke` — 2 pass (basic + associativity /
  precedence).
- `cargo test -p goby-wasm refcount_reuse_loop_example_parses` — pass.
- `cargo test -p goby-wasm refcount_reuse_loop_example_compiles --
  --ignored` — still aborts with stack overflow. Root cause is no longer the
  missing `^` operator; see Immediate Next Actions.

## Architecture State

| Layer | Status |
|---|---|
| Parser | Stable (`^` added) |
| Resolver | Stable |
| Typechecker | Stable (`^`: Int × Int → Int) |
| IR (`ir.rs`) | Stable (`IrBinOp::BitXor` present) |
| IR lowering (`ir_lower.rs`) | Stable |
| Wasm backend | memory64 complete; Perceus M1 groundwork + `^` emission landed |
| Effect handlers | Non-tail / multi-resume still produces `BackendLimitation` |
| GC / reclamation | Bump allocator only; Perceus M1 acceptance harness is the next focus |
