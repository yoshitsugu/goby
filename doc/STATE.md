# Goby Project State Snapshot

Last updated: 2026-04-17 (post-fix)

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

### Resolved in this slice

#### `recursive_multi_part_interpolated_print_after_graphemes_executes` fixed

Root cause:
- `emit_string_split_helper` still used a hard-coded `* 4` when allocating list header chunk-pointer slots.
- Under `memory64`, chunk pointers are 8-byte slots, so this under-allocated the header and `ptr_store` overflowed into adjacent static-string memory.
- The overflow corrupted static string blobs (notably interpolation literals), which then surfaced as `stdout utf8` failures during `StringConcat`.

Fix:
- In `emit_string_split_helper`, replaced `ptr_const(pw, 4)` with `ptr_const(pw, ptr_slot_bytes(pw) as u64)` for header allocation size.

**Current test status**:
- `cargo fmt` — pass
- `cargo check` — pass
- `cargo test` — pass (workspace)

---

## Immediate Next Actions

1. Update `doc/PLAN.md` M4 checklist/status to reflect workspace-green state.
2. Decide whether to keep or trim now-redundant M4 migration guard comments in `emit.rs`.
3. Continue next roadmap slice (post-M4 tasks) from `doc/PLAN.md`.

---

## Architecture State

| Layer | Status |
|---|---|
| Parser | Stable |
| Resolver | Stable |
| Typechecker | Stable |
| IR (`ir.rs`) | Stable |
| IR lowering (`ir_lower.rs`) | Stable |
| Wasm backend | M4 memory64 migration slice is workspace-green (`fmt/check/test`) |
| Effect handlers | Non-tail / multi-resume produces `BackendLimitation` |
| GC / reclamation | Out of scope. Bump allocator only |
