# Goby Project State Snapshot

Last updated: 2026-04-18 (Perceus M1 acceptance harness landed)

## Current Focus

**Perceus M1 acceptance harness is complete.**
`refcount_reuse_loop_example_compiles` is now un-ignored and passes.
The next milestone is **M2: refcount emission and reuse loop**.

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
  `refcount_reuse_loop_example_compiles` (now active — M1 acceptance harness
  landed on 2026-04-18).

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

## Perceus M1 acceptance harness landed on 2026-04-18

Root cause of the previous stack-overflow/hang: modules with non-main
user-defined declarations (e.g. `step`, `build`, `xor_fold`) were
incorrectly classified as `NotRequiringRuntimeCapability` by the general-lower
gate in `gen_lower/mod.rs`, because the gate only checked `main`'s direct body
for `AssignIndex` / Lambda / etc. The fix adds `has_non_main_user_decls` as an
additional gate condition so programs with helper functions are always routed
through the general-lower path (rather than the interpreter fallback, which
would attempt to evaluate `step initial 0 5000` at compile time).

## Immediate Next Actions

1. Perceus M2: refcount emission — when a list cell's refcount reaches 1 at
   an `AssignIndex`, reuse the cell in-place instead of copying. Add the
   refcount decrement / reuse-or-copy logic in the Wasm runtime.
2. Extend `tooling/` syntax highlight definitions to cover `^` (tracked as a
   TODO under `doc/PLAN.md` §4.2.1).

## Verification snapshot (2026-04-18, M1 harness)

- `cargo fmt --all` and `cargo check --all-targets` — workspace green.
- `cargo test -p goby-core` — 697 pass, 0 failed, 2 ignored.
- `cargo test -p goby-wasm` (lib + integration, no --ignored) — all pass, 3 ignored.
- `cargo test -p goby-wasm refcount_reuse_loop_example_compiles` — pass (un-ignored).
- `cargo test -p goby-wasm refcount_reuse_loop_example_parses` — pass.

## Architecture State

| Layer | Status |
|---|---|
| Parser | Stable (`^` added) |
| Resolver | Stable |
| Typechecker | Stable (`^`: Int × Int → Int) |
| IR (`ir.rs`) | Stable (`IrBinOp::BitXor` present) |
| IR lowering (`ir_lower.rs`) | Stable |
| Wasm backend | memory64 complete; Perceus M1 groundwork + `^` emission + M1 harness landed |
| Effect handlers | Non-tail / multi-resume still produces `BackendLimitation` |
| GC / reclamation | Bump allocator only; Perceus M2 is the next focus |
