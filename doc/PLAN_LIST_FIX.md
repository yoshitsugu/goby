# PLAN_LIST_FIX — Resolve the `each` + nested `AssignIndex` memory blowup

Transient plan. Closes the open bug in `doc/BUGS.md` ("`goby run` exhausts
Wasm memory when a tail-recursive driver repeatedly returns a list-of-lists
updated through nested `AssignIndex` inside an `each` callback"). Delete
this file once all milestones land and the BUGS.md entry moves to the
resolved section.

Long-term structural questions (type-level uniqueness, alternate list
representations, CLI surface evolution, embedder portability) are out of
scope here and recorded in `doc/PLAN.md` §3.1 and §3.2.

## User-facing requirements driving this plan

1. **Scaling.** Performance must not degrade super-linearly in the amount
   of data the program touches.
2. **Ceiling.** The runtime must not abort with `E-MEMORY-EXHAUSTION`
   before the host's resource budget is actually close to exhausted.

Track A closes the reported bug; Track B ensures that working sets
larger than the current emitter-baked 64 MiB cap also run to completion.

## Background (current implementation, as of 2026-04-14)

- IR node: `CompExpr::AssignIndex { root, path, value }` at
  `crates/goby-core/src/ir.rs:149`. Created by
  `crates/goby-core/src/ir_lower.rs:990`.
- Lowering: `lower_assign_index` at
  `crates/goby-wasm/src/gen_lower/lower.rs:1661` — descent via `ListGet`,
  ascent via copy-on-write `ListSet`.
- `ListSet` helper: `emit_list_set_helper` at
  `crates/goby-wasm/src/gen_lower/emit.rs:5450`. Copies header table
  and the touched chunk.
- `each` / `fold` lowering: goes through `BackendIntrinsic::ListFold`
  (`backend_ir.rs:138`). `each` is expressed as `fold` that discards
  the accumulator. Prepend-builder callbacks are already specialised
  via `lower_supported_inline_list_fold_prepend_builder`
  (`lower.rs:1303`-ish). **The `each` + nested `:=` shape is not yet
  specialised.**
- Memory ceiling: `DEFAULT_WASM_MEMORY_CONFIG` at
  `crates/goby-wasm/src/memory_config.rs:37` — `initial_pages: 4`,
  `max_pages: 1024` (64 MiB), `host_bump_reserved_bytes: 49_152`.
  `memory_type()` at `memory_config.rs:26` sets `memory64: false`.
- Host ceiling enforcement: `ensure_linear_memory_capacity`
  (`crates/goby-wasm/src/wasm_exec.rs:639`) compares against
  `memory_config.max_pages` and sets `RUNTIME_ERROR_MEMORY_EXHAUSTION`
  (code at `wasm_exec.rs:56`). Host-side byte accounting is `u32`
  throughout.
- CLI entry: `crates/goby-cli/src/main.rs` — `run()` at line 169,
  `parse_args` at 319/323, `execute_wasm` at 413.
- Execution environment targeted by this plan: `wasmtime` only.
  `wasmtime` 43.0.1 (in-tree) supports memory64 at Tier 1.

## Goals (measurable)

1. **G0 — Reported bug resolved.** The minimal repro committed to
   `doc/BUGS.md` (50 × 50 grid, 5000 iterations, executed via
   `printf 'x\n' | goby run repro.gb`) completes without
   `E-MEMORY-EXHAUSTION` under the default CLI settings and prints
   `0`. Both repros are wired into the automated test suite so the
   regression cannot silently return. G0 is the primary success
   condition for this plan; G1–G3 are the structural properties that
   make G0 durable.
2. **G1 — Scaling.** For a program that performs *K* cell-updates on
   an *R × C* grid through `each xs (fn v -> root[..] := rhs)`, peak
   steady-state allocation is O(R + C), independent of *K*. Verified
   by an `examples/` regression that OOM's on `main` but completes
   after M1.
3. **G2 — Ceiling.** `E-MEMORY-EXHAUSTION` fires at the *lower* of
   `--max-memory-mb` (or `GOBY_MAX_MEMORY_MB`) and the CLI's built-in
   default (1 GiB). The emitter contributes no hardcoded ceiling below
   that. Verified by a CLI smoke test.
4. **G3 — Preserved semantics.** `:=`, `each`, list equality, list
   pattern matching, and existing diagnostic wording / spans are
   unchanged on the existing test suite. Verified by `cargo test` and
   `examples/` stdout comparison.

## Non-goals (local to this plan)

- No new list representation, no type-system changes, no runtime
  uniqueness bits, no wasmer support, no surface-syntax changes.

## Track A — In-place lowering for the `each` + `:=` shape

Syntactic recognition at the IR level. Narrow by design: when a
structural solution from `doc/PLAN.md` §3.1 lands, delete the matcher
in one commit.

**Recognised shape (IR):**

```
CompExpr::Call {
  callee: GlobalRef { module: "list", name: "fold" }  // or the Var/alias equivalents
                                                      // and the bare-name forms
                                                      // already handled around
                                                      // lower.rs:1300
  args: [
    <source list>,                  // arg 0: the list being iterated
    <unit or literal accumulator>,  // arg 1: accumulator discarded by each
    <callback lambda>,              // arg 2: (acc, v) -> body
  ]
}
```

where `<callback lambda>` is a direct `fn acc v -> body` whose body
has the sequential shape `<AssignIndex root[..] := rhs> ; acc` (where
`;` is the IR's sequence/block form used today by `each`'s stdlib
expansion), and where:

- `root` is a `mut` binding whose declaration is visible in the
  enclosing scope of the `fold` call;
- `root` is not captured by any **other** lambda in the enclosing
  function body;
- the RHS `rhs` does not itself read `root` through a second path that
  could alias (conservative: reject if `root` appears textually inside
  `rhs`).

**Lowering:** emit the descent `ListGet` chain for `root[..]`, then a
single `ListSetInPlace` intrinsic that overwrites the element inside
the existing chunk. Skip the ascent / `StoreLocal { name: root }`. The
callback still returns `acc` so `ListFold`'s contract is preserved.

**Fallback:** any condition fails → today's lowering unchanged.

**Documented limitation:** recognition is syntactic, so a semantically
equivalent rewrite (manual recursion instead of `each`, explicit
`let`-binding of `root` mid-body) silently loses the optimization.
This is accepted. Do not pre-emptively expand the matcher; grow it
only in response to a *second* real-program report.

## Track B — Move the ceiling out of the emitter

Independent of the bug (Track A alone closes the repro). Exists so
that working sets larger than 64 MiB become a runtime policy question
rather than a compile-time cap.

- Emitter switches to `MemoryType { memory64: true, … }`; address-typed
  operations migrate from i32 to i64. Tagged values on the operand
  stack are already i64 — only linear-memory addresses change.
- The compiled-in `max_pages` ceiling is removed in favour of CLI
  policy: `--max-memory-mb <N>` (env fallback `GOBY_MAX_MEMORY_MB`),
  default 1 GiB, `0` means "defer to host / `wasmtime::StoreLimits`
  default only."
- Forced-trap tests that rely on `max_pages == initial_pages` stay;
  they are re-anchored to memory64 page counts, not deleted.

## Milestones

Each milestone is an independent shippable unit reviewed through the
`codex-reviewed-stepwise-dev-flow` skill. Cost labels: S = hours,
M = day(s), L = week(s).

### M1 — In-place lowering ✅ DONE (2026-04-14)

All sub-steps complete. G0 verified: `each_assign_index_in_place_bugs_md_minimal_repro_completes`
passes at 50 × 50 × 5000 under the default 64 MiB ceiling.

Key implementation:
- `BackendIntrinsic::ListSetInPlace` added to `backend_ir.rs`
- `emit_list_set_in_place_helper` added to `emit.rs`; `needs_helper_state` updated
- `lower_list_each_mutating_assign` added to `lower.rs` — intercepts
  `GlobalRef { module: "list", name: "each" }` calls at the lowering site
- Unit tests: single-level recognition + near-miss fallback; runtime repro test

### M2 — CLI ceiling surface ✅ DONE (2026-04-15)

All sub-steps complete. `--max-memory-mb` / `GOBY_MAX_MEMORY_MB`
override the 1 GiB default; the trap body reports the configured
ceiling and source. See commits 6b744123, b9f7a603, 073f42e2,
e6040e63.

#### M2.1 — Config split (S) — DONE

- [x] `crates/goby-wasm/src/memory_config.rs`: introduce
      `TEST_MEMORY_CONFIG` (same values as the current
      `DEFAULT_WASM_MEMORY_CONFIG`) and rename the runtime-facing one
      to `RUNTIME_MEMORY_CONFIG` with `max_pages: 16_384` (= 1 GiB).
      Keep `DEFAULT_WASM_MEMORY_CONFIG` as a deprecated alias of
      `TEST_MEMORY_CONFIG` for any remaining call sites; plan to
      delete the alias in M2.4.
- [x] Update `default_memory_config_locks_bounded_growth_defaults`
      test (`memory_config.rs:48`) to cover both configs.
- [x] Grep for every `DEFAULT_WASM_MEMORY_CONFIG` use and classify:
      test harness → `TEST_MEMORY_CONFIG`; runtime entry → pick up
      from CLI resolution (see M2.2).

#### M2.2 — CLI flag and env fallback (S) — DONE

- [x] `--max-memory-mb` flag + `GOBY_MAX_MEMORY_MB` env, precedence
      CLI > env > 1 GiB default; `0` defers to host (with a finite
      `StoreLimits` cap, per M3.3 decision).

#### M2.3 — Improved trap body (S) — DONE

- [x] Trap body reports "configured ceiling: N MiB (source: …)".

#### M2.4 — Tests (S) — DONE

- [x] CLI smoke + env-var + default-reachable tests; forced-trap
      tests pinned to `TEST_MEMORY_CONFIG`.

### M3 — memory64 proof-of-concept ✅ DONE (2026-04-15)

#### M3.1 — Skeleton toggle (S) — DONE

- [x] `WasmMemoryConfig::memory64: bool` added.
      `wasmtime::Config::wasm_memory64(true)` enabled unconditionally.

#### M3.2 — One example under memory64 (S) — DONE

- [x] `memory64_flag_hello_gb_executes_correctly` test in
      `runtime_output_tests.rs` runs `examples/hello.gb` end-to-end
      with `memory64: true`.

#### M3.3 — Host-refusal semantics validation (S) — DONE

- [x] Decision recorded in `doc/PLAN.md` §3.2: `--max-memory-mb=0`
      keeps a finite `RUNTIME_MEMORY_CONFIG` cap until `StoreLimits`
      wiring exists.

#### M3.4 — Address-site inventory (S) — DONE

- [x] `doc/PLAN_LIST_FIX_M4_SITES.md` written (494 hits in emit.rs,
      82 in backend.rs, 5 categories A–E).

### M4 — Full emitter migration to memory64 (cost: L)

Execute the M3 inventory. Memory64 becomes the default; the wasm32
path is retired.

#### M4.1 — `ptr_*` helper layer (S) — DONE (commit 964679cd, 2f0990df)

- [x] Introduced `PtrWidth` enum and `ptr_load` / `ptr_store` /
      `ptr_load_8u` / `ptr_const` / `ptr_add` / `ptr_sub` / `ptr_mul` /
      `ptr_div_u` / `ptr_eq` / `ptr_lt_u` / `ptr_neg_one` /
      `ptr_extend_to_i64` in `crates/goby-wasm/src/gen_lower/ptr.rs`.
      `PtrWidth::from_memory64(bool)` selects W32/W64. Threaded through
      `HeapEmitState::ptr_width`.
- [x] 12 unit tests build a minimal Wasm module per helper and run it
      through `wasmparser::Validator` (the `Instruction` enum does not
      implement `PartialEq`, so direct equality assertions are not
      possible — validation is the practical substitute).
- [x] M4.1b: scratch local pool widened to i64 when memory64 is on
      (`scratch_val_type` in both main and aux function blocks).

#### M4.2.0 — Layout preparation — DONE (alternative approach, 2026-04-17)

**Discovered during M4.1**: the four global slots in
`crates/goby-wasm/src/layout.rs` are each 4 bytes (i32) and packed
contiguously:

| Const                              | Offset | Stores         | Pointer-typed? |
| ---------------------------------- | ------ | -------------- | -------------- |
| `GLOBAL_HEAP_CURSOR_OFFSET`        | 12     | heap cursor    | YES (address)  |
| `GLOBAL_HEAP_FLOOR_OFFSET`         | 16     | heap floor     | YES (address)  |
| `GLOBAL_RUNTIME_ERROR_OFFSET`      | 20     | error code     | NO (u32 enum)  |
| `GLOBAL_HOST_BUMP_CURSOR_OFFSET`   | 24     | host bump end  | YES (address)  |

**Resolution (alternative to slot widening):** Slot layout kept at 4 bytes.
All global-slot read/write sites use `I32Store`/`I32Load` with
`I32WrapI64`/`I64ExtendI32U` conversions at the boundary. This avoids
layout corruption without widening offsets. `cargo test` green.

- [x] Global slot I/O sites fixed with conversion instructions instead of
      slot widening. Layout constants unchanged.

#### M4.2 — Route every inventoried site through the helpers — DONE (commits eef821f0, e18eb148, e62ac0be, 2026-04-16)

- [x] All `I32Load*`/`I32Store*` address sites migrated to `ptr_load`/`ptr_store`.
- [x] `backend.rs` excluded: all I32Load/I32Store there are WASI Preview 1 iovec ABI (always i32).
- [x] `I32Const <addr>` → `ptr_const` complete.
- [x] `I32Add`/`I32Sub` on addresses → `ptr_add`/`ptr_sub` complete.
- [x] Value-level i32 arithmetic (chunk indices, tag bits, payload counts) unchanged.
- [x] `cargo test -p goby-wasm` green after each sub-walk.

#### M4.3 — Scratch layout — DONE (decision: keep blanket promotion)

**Status:** M4.1b already promoted the *entire* scratch pool to i64
when `memory64` is on (`scratch_val_type`). This blanket promotion is
correct for address-typed slots but over-allocates for the
value-typed scratch (`s_word_off`, chunk indices, payload counts).
The over-allocation is harmless for correctness but wastes locals.

- [x] Decision: keep blanket i64 promotion. No split chosen.
- [x] `HeapEmitState` doc comment updated.

#### M4.4 — Host side (`wasm_exec.rs`) — DONE (commit 2ce3c06c, 2026-04-17)

- [x] Global slot read/write uses `I32WrapI64`/`I64ExtendI32U` conversions at boundary.
- [x] WASI ABI boundary stays i32 (`fd_write` and friends). Audited and confirmed.

#### M4.5 — Flip the default — DONE (commit 2ce3c06c, 2026-04-17)

- [x] `RUNTIME_MEMORY_CONFIG.memory64 = true` set.
- [x] `TEST_MEMORY_CONFIG.memory64 = true` も設定済み（計画と異なる）。
      OOMテストは `--max-memory-mb` ベースで動作確認済み。
- [x] `WasmMemoryConfig.memory64` フィールド保持。

#### M4.6 — Tests — DONE (2026-04-17)

- [x] OOM tests verify `E-MEMORY-EXHAUSTION` under memory64.
- [x] Full `cargo test` green. Existing program stdout unchanged.

#### M4.7 — Comment / doc audit — DONE (2026-04-17)

- [x] Stale wasm32/64 MiB references in comments updated or removed.
- [x] `doc/STATE.md` updated to reflect memory64 default and completed sub-milestones.

**Exit criteria (G2, G3):** `cargo test` green ✓; program stdout unchanged ✓;
`--max-memory-mb` bounds as specified ✓.

### M5 — Invariant and documentation sync (cost: S)

- [x] `doc/LANGUAGE_SPEC.md`: no wording references Wasm memory target — no change needed.
- [x] `doc/STATE.md`: updated with locked decisions (memory64 default, 1 GiB ceiling, in-place lowering).
- [x] `doc/PLAN.md`: §3 items settled by this plan noted.
- [ ] `doc/BUGS.md`: confirm G0 regression test green, move open entry to resolved. (pending)
- [ ] Run `goby-invariants` checklist before final commit.
- [ ] **Delete this file (`doc/PLAN_LIST_FIX.md`).**

**Exit criteria:** invariant gate clean; BUGS.md entry closed;
`doc/PLAN_LIST_FIX.md` removed from the tree.

## Dependency graph

```
M1 (in-place)  ──► independent; closes the reported bug.
M2 (CLI)       ──► independent; ships on wasm32.
M3 (mem64 PoC) ──► depends on M2 (CLI policy used to exercise ceilings).
M4 (mem64)     ──► depends on M3.
M5 (docs)      ──► depends on M1..M4.
```

M1 and M2 can run in either order or in parallel. M3 → M4 → M5 is
strict.

## Quality gate (per milestone boundary)

- [ ] `cargo fmt`
- [ ] `cargo check`
- [ ] `cargo test`
- [ ] No regression in diagnostic wording / spans except where
      explicitly listed in the milestone.
