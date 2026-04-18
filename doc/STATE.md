# Goby Project State Snapshot

Last updated: 2026-04-18

## Current Focus

**Perceus M1 work is started but currently blocked on a missing source-language
operator (`^`, bitwise XOR) required by the normative goal program.**

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

## Perceus M1 — the blocker discovered on 2026-04-18

The normative goal program uses bitwise XOR:

```goby
xor_fold : List Int -> Int -> Int
xor_fold xs acc =
  case xs
    [] -> acc
    [x, ..rest] -> xor_fold rest (acc ^ x)
```

`doc/PLAN_PERCEUS.md` §1.1 explicitly marks this source as **normative**:
implementers must not "equivalent"-rewrite it, because the acceptance
checksum is pinned to the exact allocation and evaluation pattern.

However, `^` is not implemented anywhere in the pipeline:

- Lexer / parser — no token and no `split_top_level_binop(_, '^')` arm. A
  grep for `Caret`, `"^"` and `'^'` in `goby-core/src` returns nothing.
- AST (`BinOpKind`) — enumerates only `Or And Add Sub Mul Div Mod Eq Lt Gt
  Le Ge`. No bitwise variant at all.
- IR (`IrBinOp`) — same shape.
- Wasm lowering — no `i64.xor` emission path.

Observed behaviour:

- `goby check examples/refcount_reuse_loop.gb` → passes (parser silently
  accepts a surface form, typecheck succeeds).
- `goby run examples/refcount_reuse_loop.gb` → fails at codegen with
  `fallback runtime output could not be resolved`.
- Reducing the inputs to `build 0 16 []` + 20 iterations also hangs under
  `goby run`, confirming the failure is in the compile / lowering path and
  not a long Wasm runtime loop.
- Reverting `crates/goby-wasm/src/gen_lower/{emit,lower,backend_ir}.rs` to
  the pre-groundwork commit `9b2a6613` still reproduces the failure — the
  M1 groundwork is not the regression source; `^` was never implemented.

Because the M1 acceptance criteria (`cargo test -p goby-wasm` green,
recorded checksum from `goby check`, ignored-`goby run` case citing M5) all
depend on `examples/refcount_reuse_loop.gb` compiling at least up to the
parts that do not need refcounting, M1 cannot close without `^`.

## Decided next slice (started, not committed)

Add `^` (bitwise XOR) as an `Int → Int → Int` operator before resuming
Perceus M1 proper. User approved this direction ("option A"). The
implementation plan lives in the workspace, not in the repo:

- `~/.claude/workspaces/home_yoshitsugu_src_github_com_yoshitsugu_goby/
  implementation-plan-perceus-m1-bitxor.md`
- `~/.claude/workspaces/home_yoshitsugu_src_github_com_yoshitsugu_goby/
  progress-log-perceus-m1-bitxor.md`

Planned step list (summary):

1. Add `BinOpKind::BitXor` + `IrBinOp::BitXor`, extend Debug / formatter
   renderers to `"^"`.
2. Parser: `split_top_level_binop(_, '^')` between the `&&` arm and the
   `==` arm of `parser_expr.rs::parse_expr` (so `a ^ b == c` parses as
   `(a ^ b) == c`). Mirror the change in the span-aware parser.
3. Typecheck: treat `BitXor` exactly like `Mod` (both operands `Int`,
   result `Int`, no effects).
4. IR lowering + closure_capture + any other exhaustive match sites —
   driven by `cargo check -p goby-core` failures.
5. Wasm backend: emit `i64.xor` for `IrBinOp::BitXor`.
6. Minimal smoke test in `wasm_exports_and_smoke.rs` that compiles and
   runs a tiny module using `^`.
7. Document `^` under operators in `doc/LANGUAGE_SPEC.md`; leave
   `doc/PLAN_PERCEUS.md` unchanged (already uses the operator).

Out of scope for this slice: Bool XOR, bitwise AND/OR/shift, constant
folding of `^` inside `closed_literals.rs`, and the Perceus M1 acceptance
harness itself (will be resumed once `^` lands).

## Immediate Next Actions

1. Run the Codex plan review for `implementation-plan-perceus-m1-bitxor.md`
   (was started and interrupted; restart cleanly in the next session).
2. Execute Step 1 of the plan (AST + IR variants + Debug renderers),
   verify with `cargo check -p goby-core`.
3. Progress through steps 2–7; keep `cargo fmt && cargo check && cargo
   test` workspace-green before each commit, and re-run
   `goby-invariants` before the spec doc update commit.
4. After `^` lands, return to Perceus M1 proper: capture the checksum
   via `goby check examples/refcount_reuse_loop.gb`, record it as an
   `assert_eq!` literal in the integration test, and un-ignore the
   compile-only case (keep the `goby run` case ignored citing
   `doc/PLAN_PERCEUS.md` M5).

## Verification snapshot (2026-04-18)

- `cargo check -p goby-core -p goby-wasm` — pass.
- `cargo test -p goby-wasm refcount_reuse_loop_example_parses` — pass.
- `cargo test -p goby-wasm refcount_reuse_loop_example_compiles --
  --ignored` — aborts ("stack overflow" message from the test harness;
  root cause is the missing `^` operator surfacing as a compile / lower
  failure, not a real recursion-depth issue).
- `goby check examples/refcount_reuse_loop.gb` — pass (parser is too
  permissive around `^`).
- `goby run examples/refcount_reuse_loop.gb` — fails with
  `fallback runtime output could not be resolved`.

## Architecture State

| Layer | Status |
|---|---|
| Parser | Stable (but missing `^`; blocks Perceus M1) |
| Resolver | Stable |
| Typechecker | Stable |
| IR (`ir.rs`) | Stable (missing `BitXor` variant) |
| IR lowering (`ir_lower.rs`) | Stable |
| Wasm backend | memory64 complete; Perceus M1 groundwork landed |
| Effect handlers | Non-tail / multi-resume still produces `BackendLimitation` |
| GC / reclamation | Bump allocator only; Perceus M1 preparing |
