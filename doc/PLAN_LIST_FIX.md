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

#### M4.2.0 — Layout preparation (PREREQUISITE for M4.2) (S)

**Discovered during M4.1**: the four global slots in
`crates/goby-wasm/src/layout.rs` are each 4 bytes (i32) and packed
contiguously:

| Const                              | Offset | Stores         | Pointer-typed? |
| ---------------------------------- | ------ | -------------- | -------------- |
| `GLOBAL_HEAP_CURSOR_OFFSET`        | 12     | heap cursor    | YES (address)  |
| `GLOBAL_HEAP_FLOOR_OFFSET`         | 16     | heap floor     | YES (address)  |
| `GLOBAL_RUNTIME_ERROR_OFFSET`      | 20     | error code     | NO (u32 enum)  |
| `GLOBAL_HOST_BUMP_CURSOR_OFFSET`   | 24     | host bump end  | YES (address)  |

Under memory64, replacing `I32Load` → `I64Load` at the address-typed
slots will read 8 bytes from a 4-byte cell and corrupt the next slot.
Therefore the slot layout must be widened **before** the M4.2
instruction substitution touches global-slot accesses.

- [ ] Widen the three address-typed slots to 8 bytes. Proposed new
      layout (alignment 3, i.e. 8-byte aligned):

      ```
      offset 16  GLOBAL_HEAP_CURSOR_OFFSET     (8 bytes, i64 address)
      offset 24  GLOBAL_HEAP_FLOOR_OFFSET      (8 bytes, i64 address)
      offset 32  GLOBAL_HOST_BUMP_CURSOR_OFFSET (8 bytes, i64 address)
      offset 40  GLOBAL_RUNTIME_ERROR_OFFSET   (4 bytes, u32 — unchanged width)
      offset 48  HEAP_BASE                     (was 28)
      ```

      Keep `IOVEC_OFFSET = 0` (16 bytes) and `NWRITTEN_OFFSET` aligned
      with the existing WASI ABI usage; only the post-WASI region
      changes.
- [ ] Update host-side `wasm_exec.rs`:
  - `read_heap_cursor_slot` / `write_*_cursor_slot` read/write 8 bytes
    via `u64::from_le_bytes` / `to_le_bytes` instead of 4-byte `u32`.
  - The active-data initializers at lines ~1140 and ~1309 must seed
    8-byte little-endian values for the address slots and keep the
    4-byte value for `GLOBAL_RUNTIME_ERROR_OFFSET`.
  - `current_linear_memory_bytes` and host bump arithmetic become
    `u64` (this overlaps with M4.4 — execute the slot-related parts
    here and defer the rest of M4.4 to its own step).
- [ ] Decide: keep both layouts (one per `PtrWidth`) or commit
      unconditionally to the wide layout for both wasm32 and memory64.
      **Recommendation:** commit unconditionally. The wasted 12 bytes
      under wasm32 are negligible, and a single layout removes a
      conditional that would otherwise contaminate every emitter site
      that touches a global slot. `TEST_MEMORY_CONFIG` (wasm32) keeps
      working with wider slots because `I32Load` reads only the low 4
      bytes of a little-endian i64 address that is bounded by 4 GiB —
      validate this assumption with one targeted test before locking
      the decision.
- [ ] Add a doc comment block at the top of `layout.rs` explaining
      that the slot widths are pointer-width-conservative (always 8
      bytes for address slots, regardless of `memory64`).
- [ ] done when: `cargo test -p goby-wasm` green with
      `RUNTIME_MEMORY_CONFIG.memory64 = false` (no behaviour change
      yet — only slot widths and the host I/O width change).

#### M4.2 — Route every inventoried site through the helpers (M)

**Order matters.** Execute the sub-walks in this sequence so that the
module remains valid and tests stay green between commits:

1. **Slot-access sites first** (depends on M4.2.0 being complete).
   Replace `I32Load` / `I32Store` at the four `GLOBAL_*_OFFSET` sites
   in `emit.rs` and `backend.rs` with `ptr_load` / `ptr_store` for
   address slots; leave `GLOBAL_RUNTIME_ERROR_OFFSET` as `I32Load` /
   `I32Store` (the error code is `u32`, not an address).
2. **Heap-payload load/store sites** (object headers, list chunks,
   tuple slots). Address operand becomes pointer-width; payload value
   keeps its existing type. `MemArg.align` increases to 3 only for
   slots that now hold i64 addresses.
3. **`I32Const <addr>` sites.** Use `ptr_const` only when the constant
   feeds an address operand (the next instruction is `ptr_load` /
   `ptr_store` / `ptr_add`). **Tag bits, payload counts, and
   `TAG_INT` constants stay i32.**
4. **`I32Add` / `I32Sub` on addresses.** Identify by checking that
   one operand traces back to a `ptr_const` or a `ptr_load` from a
   global address slot.

Disambiguation rule (write this down before walking the sites):
> A constant or arithmetic op is "address-typed" iff the value
> participates in a load/store address computation. If it feeds
> `I32And` (tag mask), `I32ShrS` (untag), comparison against
> `TAG_INT`, or a payload write, it is value-typed and stays i32.

- [ ] Walk the M3.4 checklist. For each category:
  - `I32Load*` / `I32Store*` on linear memory → `ptr_load` /
    `ptr_store`.
  - `I32Const <addr>` → `ptr_const`.
  - `I32Add` on addresses → `ptr_add`.
  - `MemoryFill` / `MemoryCopy` → emit the size argument via the
    correct `i32`/`i64` per `PtrWidth`.
  - `MemorySize` / `MemoryGrow` → produce/consume `i64` when
    `memory64`.
- [ ] **Value-level i32 arithmetic unrelated to addresses must not
      change** (chunk indices, payload counts, tag bits).
- [ ] After each sub-walk, `cargo test -p goby-wasm`. If red, revert
      the sub-walk before moving on.

#### M4.3 — Scratch layout (S)

**Status:** M4.1b already promoted the *entire* scratch pool to i64
when `memory64` is on (`scratch_val_type`). This blanket promotion is
correct for address-typed slots but over-allocates for the
value-typed scratch (`s_word_off`, chunk indices, payload counts).
The over-allocation is harmless for correctness but wastes locals.

- [ ] Decide: keep blanket i64 promotion (simple, costs a few extra
      locals per function) **or** split scratch into
      `scratch_addr: Vec<u32>` (i64 under memory64) and
      `scratch_val: Vec<u32>` (always i32). **Recommendation:** keep
      blanket promotion until profiling shows it matters; revisit
      only if Wasm module size grows materially.
- [ ] If the split is chosen: update `emit_list_set_helper` and
      siblings (`s_src_header`, `s_dst_chunk_ptr`, `s_word_off` —
      grep for `let s_` in `emit.rs`) to draw from the correct pool.
- [ ] Document the final layout in the doc comment on
      `HeapEmitState`.

#### M4.4 — Host side (`wasm_exec.rs`) (S)

**Note:** The slot-related `u32`→`u64` widening for the three
address-typed global slots is folded into M4.2.0. M4.4 covers the
remaining host-side address arithmetic that does not touch a global
slot.

- [ ] `current_linear_memory_bytes` (`wasm_exec.rs:633`):
      return `u64`, not `u32`.
- [ ] `ensure_linear_memory_capacity` and callers: take `required_end:
      u64`, compare against `max_pages` as `u64`.
- [ ] Replace all `u32::try_from(bytes)` truncations on addresses
      with `u64` arithmetic; only at host-import boundaries that
      interact with host data structures do we narrow.
- [ ] **WASI ABI boundary stays i32.** `fd_write` and friends take
      32-bit pointers per Preview 1 even when the linear memory is
      memory64. Audit the import signatures in `host_runtime.rs` and
      every `Caller::get_export` site that reads/writes WASI argument
      memory: those addresses must be narrowed with
      `u32::try_from(addr).map_err(|_| trap)?` rather than passed as
      `u64`. List the audited sites in the commit message.

#### M4.5 — Flip the default (S)

- [ ] `memory_config.rs`: `RUNTIME_MEMORY_CONFIG.memory64 = true`;
      `memory_type()` emits `memory64: true` when set.
- [ ] **Keep `TEST_MEMORY_CONFIG.memory64 = false`.** The forced-trap
      tests (`heap_only_recursive_tuple_allocation_grows_past_initial_pages`,
      `host_string_concat_grows_linear_memory_past_initial_pages`)
      depend on a small 64 MiB ceiling that is trivial to hit; under
      memory64 the tests would either need to allocate gigabytes or
      be rewritten against `--max-memory-mb`. Decision: keep wasm32
      for tests so the codepath that produces an actual `memory.grow`
      failure is still exercised by CI.
- [ ] **Do not remove the M3.1 internal toggle.** `WasmMemoryConfig`
      keeps its `memory64` field so `TEST_MEMORY_CONFIG` can stay on
      wasm32 while `RUNTIME_MEMORY_CONFIG` flips to memory64. The
      toggle becomes a permanent dual-mode switch, not a transitional
      flag.

#### M4.6 — Tests (S)

- [ ] Re-anchor `heap_only_recursive_tuple_allocation_grows_past_initial_pages`
      (`lib.rs:1985`) and
      `host_string_concat_grows_linear_memory_past_initial_pages`
      (`wasm_exec.rs:1191`) to the config chosen in M4.5. Verify
      they still raise `E-MEMORY-EXHAUSTION`.
- [ ] New positive test: allocate a single list of > 64 MiB under
      the default `RUNTIME_MEMORY_CONFIG`. Confirm success.
- [ ] Re-run the full `cargo test` matrix. Existing program stdout
      must be byte-identical (module bytes will differ).

#### M4.7 — Comment / doc audit (S)

- [ ] `rg '4\s*GiB|64\s*MiB|wasm32|max_pages' crates/ doc/` (the
      search, not the tool). Update comments / docstrings /
      diagnostic bodies that promise wasm32 limits. Replace with
      "host-limited (memory64)" phrasing.

**Exit criteria (G2, G3):** `cargo test` green; existing program
stdout unchanged; a > 64 MiB allocation completes under defaults;
`--max-memory-mb` still bounds as specified.

### M5 — Invariant and documentation sync (cost: S)

- [ ] `doc/LANGUAGE_SPEC.md`: update if any wording references the
      Wasm memory target.
- [ ] `doc/STATE.md`: record locked decisions (memory64 default,
      1 GiB default ceiling, in-place lowering for `each` + `:=`) and
      next steps per `AGENTS.md` restart-safety.
- [ ] `doc/PLAN.md`: mark any §3 items this plan settled;
      §3.1 / §3.2 remain for the unresolved structural questions.
- [ ] `doc/BUGS.md`: confirm the G0 regression test added in M1.4
      is green on the current `main`, then move the open entry to
      the resolved section with date and a one-line note pointing
      at M1. The entry must not be moved unless the regression test
      exists and passes.
- [ ] Run the `goby-invariants` skill checklist before the final
      commit of the track.
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
