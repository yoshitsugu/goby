# Goby Project State Snapshot

Last updated: 2026-04-19 (Perceus M3 complete: __goby_drop full child-drop, acceptance test green)

## Current Focus

**Perceus M3 complete.** All M3 deliverables landed:

- Free-list head table in linear memory (`HEAP_BASE` 56 → 408), `SizeClass` enum,
  `emit_alloc_with_flag` (free-list pop + bump fallback), `emit_free_list_push`.
- `__goby_drop` Wasm function with full per-type child-drop:
  - TAG_CHUNK: drops each item slot, pushes chunk to `SizeClass::Chunk` free-list
  - TAG_LIST: reads n_chunks, synthesises TAG_CHUNK-tagged ptrs and recurses
  - TAG_TUPLE: reads arity word, drops each element slot
  - TAG_CELL: drops contained value, pushes to Cell free-list
  - TAG_RECORD / TAG_CLOSURE: `freed_bytes` accounting only (arity not in
    payload at runtime; full child-drop deferred to M4 with layout change)
- `free_list_hits` counter wired; `peak_bytes` tracked via `freed_bytes`.
- `EmitOptions::expose_perceus_test_exports` flag added; emits
  `__test_alloc_list_1chunk` / `__test_drop_ptr` / `__goby_drop` exports for
  the acceptance test.
- Acceptance test `drop_frees_unique_list_and_subsequent_alloc_gets_free_list_hit`
  passes: allocates 1-chunk list, drops it, re-allocates, asserts
  `GLOBAL_FREE_LIST_HITS_OFFSET > 0`.

Next: **Perceus M4** — static drop insertion (`DropRef` IR node, ownership
analysis pass, `__goby_dup` helper).

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

1. **Perceus M4:** `DropRef` IR node, ownership analysis pass
   (`perceus_ownership.rs`), `__goby_dup` helper emission.
2. Extend `tooling/` syntax highlight definitions to cover `^` (tracked as a
   TODO under `doc/PLAN.md` §4.2.1).

## Verification snapshot (2026-04-18, M2 debug-alloc-stats slice)

- `cargo fmt --all` — pass.
- `cargo check --all-targets` — pass.
- `cargo test` — pass (workspace green).
- `cargo test -p goby-cli --test cli_integration run_command_debug_alloc_stats_emits_stats_line_for_general_lowered_program`
  — pass.

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
| Wasm backend | memory64 complete; Perceus M1 + M2 + M3 complete |
| Effect handlers | Non-tail / multi-resume still produces `BackendLimitation` |
| GC / reclamation | Bump allocator + refcount + free-list + `__goby_drop` (M3 complete); M4 (static drop insertion) next |
