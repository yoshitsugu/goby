# Goby Project State Snapshot

Last updated: 2026-04-21 (Perceus M4 shipped as a conservative slice; residency test deferred to M4.5)

## Current Focus

**Perceus M3 complete. M4 landed as a conservative slice.** All M3
deliverables remain in tree; M4 adds ownership classification + a partial
§3.10 drop-insertion pass that only fires in cases where the
consume-vs-borrow distinction is unambiguous. Full last-use analysis,
If/Case branch balancing on general bindings, and the
`perceus_loop_residency` gate are reopened under M4.5 borrow inference.

See `doc/PLAN_PERCEUS.md` §M4 "As-shipped scope note" for the full list
of what the slice does and does not do, and why.

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
- M4 groundwork landed on 2026-04-20:
  - shared IR now has `CompExpr::Dup` / `CompExpr::Drop`
  - IR formatting/validation and traversal helpers understand those nodes
  - general Wasm lowering maps them to backend refcount ops
  - emitter now generates module-local `__goby_dup` alongside `__goby_drop`
  - Perceus pipeline-order assertion helper added and called from general-lower
    and runtime-entry drivers
- M4 conservative slice landed on 2026-04-21:
  - `goby-core::perceus::run_perceus_passes` classifies every function
    parameter and fresh-heap `let` as `Owned`, and inserts `Drop`/`Dup` in
    the unambiguous cases:
    - `let x = <fresh-heap>` with `use_count == 0`: `Drop(x)` at the top
      of the body. Any other use count leaves the binding alone — a
      single mid-body consume (e.g. `Call(process, [x])`) would otherwise
      collide with a post-body Drop and double-free.
    - unused `case` pattern bindings: `Drop(pat_var)` at arm entry
    - owned bindings live across a `WithHandler` body: `Dup` before the
      handler boundary (Case pattern binds and Handle clause params are
      excluded from the live-across set)
    - `Call`-site: owned args transfer ownership to the callee; the caller
      emits no post-call `Drop` for those args
    - owned parameters: `Drop` only for params that are never referenced
      in the body
  - 11 perceus unit tests cover pipeline wiring, fresh-heap Drop placement,
    nested-Let / Seq-Call double-free regressions, call-site transfer,
    partial application, closure capture, case-arm pattern drop, and
    WithHandler live-across Dup
  - the rewrite is wired into general-lower for user decls and stdlib-loaded
    aux decls
  - intentionally deferred to M4.5 (borrow inference):
    - parameter last-use analysis (requires borrow/consume marks on
      `Var` occurrences; intrinsics like `length`, `list_get` are borrow
      positions in today's backend, so uniform "every use consumes"
      produces use-after-free)
    - If/Case branch balancing on non-pattern bindings
    - Dup for non-last uses in multi-use scopes
    - `perceus_loop_residency` gate (M3's
      `drop_frees_unique_list_and_subsequent_alloc_gets_free_list_hit`
      remains the current residency proxy)

Next: **Perceus M4.5** — add borrow/consume classification on `Var`
occurrences, then re-open the deferred items above.

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

1. **Perceus M4.5:** introduce borrow/consume marks on `ValueExpr::Var`
   occurrences, extend `ownership_classify` with the fixpoint from
   `PLAN_PERCEUS.md` §M4.5 C1, then re-open parameter last-use Drop,
   If/Case branch balancing on general bindings, and the
   `perceus_loop_residency` gate.
2. Extend `tooling/` syntax highlight definitions to cover `^` (tracked as a
   TODO under `doc/PLAN.md` §4.2.1).

## Verification snapshot (2026-04-21, M4 conservative slice landed)

- `cargo fmt --all --check` — pass.
- `cargo check` — pass.
- `cargo test --workspace` — pass (goby-core 708, goby-wasm 686, goby-cli 54, all green).
- `cargo test -p goby-core --lib perceus` — 9 passed.

## Verification snapshot (2026-04-20, M4 first insertion slice)

- `cargo fmt --all --check` — pass.
- `cargo check` — pass.
- `cargo test` — pass (workspace green).

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
| Wasm backend | memory64 complete; Perceus M1 + M2 + M3 complete; M4 runtime helpers (`Dup`/`Drop` lowering, `__goby_dup`) present |
| Effect handlers | Non-tail / multi-resume still produces `BackendLimitation` |
| GC / reclamation | Bump allocator + refcount + free-list + `__goby_drop`; Perceus M4 conservative drop-insertion pass live; parameter last-use + branch balancing + loop residency gate reopened under M4.5 |
