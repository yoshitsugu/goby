# Goby Project State Snapshot

Last updated: 2026-04-19 (Perceus M3 Step 1–5 in progress: layout, SizeClass, __goby_drop)

## Current Focus

**Perceus M3 in progress.** Free-list head table added to linear memory layout
(`HEAP_BASE` 56 → 408), `SizeClass` enum and `free_list_head_offset` helper
implemented, `emit_alloc_with_flag` and `emit_free_list_push` helpers added,
`__goby_drop` Wasm function emitted per module (sentinel check, refcount
decrement, Cell free-list push, freed_bytes tracking). Remaining: acceptance
test, full per-type child-drop, peak_bytes wiring.

Steps 1–4 (layout slots, refcount prefix in allocator, alloc counter, stats
epilogue, `EmitOptions.debug_alloc_stats`) and Steps 5–6 (`CompileOptions`,
public API, CLI `--debug-alloc-stats` flag + parse tests) have been implemented.
The stats line (`alloc-stats: total_bytes=N peak_bytes=M free_list_hits=H`) is
emitted by the Wasm binary at `_start` exit when `debug_alloc_stats=true` and
the module is on the memory64 / GeneralLowered path.

The previously failing CLI integration now passes. The fix had three parts:
- `crates/goby-wasm/src/wasm_exec.rs` now captures WASI stderr separately on the
  Goby-owned runtime path and forwards it to process stderr when
  `debug_alloc_stats=true`.
- `crates/goby-cli/tests/cli_integration.rs` now exercises a real
  `GeneralLowered` program instead of a file-based example.
- `free_list_hits` is emitted as the published M2 placeholder value `0`
  until actual reuse wiring lands in the next milestone.

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

1. **Perceus M3 remaining:** acceptance test `drop_frees_unique_list` (Step 7),
   full per-type child-drop helpers (list chunks, tuple, record, closure),
   peak_bytes wiring against freed_bytes (Step 6).
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
| Wasm backend | memory64 complete; Perceus M1 + M2 complete; M3 in progress (free-list layout, SizeClass, __goby_drop) |
| Effect handlers | Non-tail / multi-resume still produces `BackendLimitation` |
| GC / reclamation | Bump allocator + refcount header + alloc stats landed; free-list table + drop runtime (M3) in progress |
