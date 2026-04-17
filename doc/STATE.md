# Goby Project State Snapshot

Last updated: 2026-04-17

## Current Focus

**M4 — memory64 full migration (i32 → i64 pointer widths)**

---

## M4 Completion Status

### Completed sub-milestones

- **M4.1** (`ptr.rs` helpers): `PtrWidth` enum + `ptr_load/store/const/add/...` helpers added to `gen_lower/ptr.rs`. All pointer operations now dispatched through these helpers.
- **M4.1b** (i64 scratch locals): All `i32_scratch` pool locals declared as `ValType::I64` when `memory64 = true`. `HeapEmitState.pw()` shorthand wired in.
- **M4.2.0** (`HeapEmitState.pw()` shorthand): Added `pw()` accessor on `HeapEmitState`; propagated through all callers.
- **M4.2.1** (`backend.rs`): Excluded — all `I32Load`/`I32Store` in `backend.rs` are for WASI Preview 1 iovec ABI (always i32, never pointer-width).
- **M4.2.2** (emit_chunked_* in emit_instrs): All chunked-list emitters converted to `ptr_load`/`ptr_store`.
- **M4.2.3** (remaining `I32Load*`/`I32Store*` in `emit.rs`): 0 bare `I32Load`/`I32Store` remain for address operations.
- **M4.3** (address-typed `I32Const` → `ptr_const`): Complete. All fixed address constants use `ptr_const(pw, ...)`.
- **M4.4** (`I32Add`/`Sub` on addresses → `ptr_add`/`ptr_sub`): Complete.
- **M4.5** (`MemoryGrow`/`MemorySize`): Already pointer-width-correct.
- **M4.6** (flip `memory64` default to `true`): `RUNTIME_MEMORY_CONFIG.memory64 = true` and `TEST_MEMORY_CONFIG.memory64 = true` are both permanently set.
- **Data section fix**: Active data segment offsets use `ConstExpr::i64_const` when `memory64 = true`.
- **Global slot I/O fix** (critical): `heap_cursor`, `heap_floor`, `runtime_error`, `host_bump_cursor` are always 4-byte i32 fields at fixed offsets in linear memory. Fixed all read/write sites to use `I32Store`/`I32Load` (not `ptr_store`/`ptr_load`), with `I32WrapI64`/`I64ExtendI32U` conversions as needed. This eliminated the runtime trap from 8-byte writes corrupting adjacent global slots.
- **`bool_from_i64!` macro**: Added for `IrBinOp::Eq` in W64 — `s_result` scratch local is `i64` in W64, so we need `I64Or` (not `I64ExtendI32U + I64Or`).
- **`emit_push_tagged_ptr` fix**: Added `pw` parameter; `I64ExtendI32U` emitted only in W32 (in W64 the ptr local is already i64).
- **`BlockType::Result(ptr_val_type(pw))`**: Fixed two `If` blocks in `emit_list_concat_helper` and `emit_filter_map_helper` that yielded ptr-width values but declared `ValType::I32`.
- **Tail-call dispatcher scratch locals**: Fixed to use `ValType::I64` when `memory64 = true`.
- **`_pw` layout functions**: Added `chunk_alloc_size_pw`, `header_alloc_size_pw`, `header_n_chunks_offset_pw`, `header_chunk_ptr_offset_pw`, `chunk_item_offset_pw`, `meta_slot_bytes`, `ptr_slot_bytes` — all sensitive to `PtrWidth`.

### Known Remaining Issues (as of 2026-04-15)

**Test status**:
- `cargo fmt` — pass
- `cargo check` — pass
- `cargo test` — workspace passes except for **1 failing `goby-wasm` test**

#### Remaining issue: host temp allocations after grapheme-heavy runtime path

`cargo test -p goby-wasm recursive_multi_part_interpolated_print_after_graphemes_executes -- --test-threads=1`
still fails with:

- `stdout utf8: invalid utf-8 sequence of 1 bytes from index 405`

Observed shape:
- `read() -> split() -> list.map graphemes` succeeds
- subsequent recursive interpolated `println` output becomes corrupted
- most other general-lowered read/split/grapheme/string-equality tests now pass

Likely root cause:
- one remaining host-temp / Wasm-heap boundary sync gap after `StringGraphemesList`-driven allocations
- corruption appears only after host-side grapheme allocations are followed by many interpolated print temporaries

---

## Immediate Next Actions

1. Reproduce `recursive_multi_part_interpolated_print_after_graphemes_executes` in isolation.
2. Trace remaining host-temp / heap-floor desync after grapheme host imports.
3. Re-run `cargo test -p goby-wasm --lib -- --test-threads=1`.
4. Re-run workspace `cargo test`.
5. If green, update `doc/PLAN.md` to mark M4 complete.

---

## Architecture State

| Layer | Status |
|---|---|
| Parser | Stable |
| Resolver | Stable |
| Typechecker | Stable |
| IR (`ir.rs`) | Stable |
| IR lowering (`ir_lower.rs`) | Stable |
| Wasm backend | M4 nearly complete — workspace green except 1 `goby-wasm` runtime test |
| Effect handlers | Non-tail / multi-resume produces `BackendLimitation` |
| GC / reclamation | Out of scope. Bump allocator only |
