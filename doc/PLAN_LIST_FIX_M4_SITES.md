# M4 Address-Site Inventory

Produced during M3.4 (2026-04-15). M4 walks this checklist top-to-bottom when
migrating the Wasm emitter from i32 to i64 addresses under memory64.

## Source files in scope

- `crates/goby-wasm/src/gen_lower/emit.rs` — primary emitter (~494 address-related hits)
- `crates/goby-wasm/src/backend.rs` — low-level encoding helpers (~82 hits)
- `crates/goby-wasm/src/wasm_exec.rs` — host-side page accounting

## Categories

### A. `I32Load*` / `I32Store*` — linear-memory read/write via MemArg

These load/store tagged values, pointers, headers, and chunk entries from linear
memory. All are address-based and must migrate to `I64Load*` / `I64Store*` under
memory64.

| File | Count | Key functions |
|---|---|---|
| `emit.rs` | 140 (I32Load 69 + I32Store 71) | `emit_chunked_list_item_store`, `emit_chunked_load`, `emit_chunked_load_const`, `emit_chunked_store_address`, `emit_string_split_helper`, `emit_list_*`, `emit_alloc_helper` (internal bump allocator) |
| `backend.rs` | 23 (I32Load 7 + I32Store 16) | Low-level header/payload encode/decode helpers |

**Action:** Replace with `I64Load*` / `I64Store*` via `ptr_load` / `ptr_store`
helpers (to be introduced in M4.1).

### B. `I32Const` used as an address or offset

`I32Const` is used for both pure value arithmetic (tag bits, counts, offsets) and
address constants. Only the address/offset uses must change.

| File | Count | Notes |
|---|---|---|
| `emit.rs` | 350 | Subset are addresses; tag/payload I32Consts are NOT changed |
| `backend.rs` | 59 | Similar mix |

**Address-typed I32Const sites (must migrate):**
- `GLOBAL_HOST_BUMP_CURSOR_OFFSET as i32` — slot zero address in linear memory
- `HOST_BUMP_RESERVED_BYTES as i32` — used in subtract-from-top address calc
- `RUNTIME_MEMORY_CONFIG.host_bump_start() as i32` — initial cursor seed
- `WASM_PAGE_BYTES as i32` — page-size constant in byte→page arithmetic
- Any `I32Const` pushed immediately before `I32Load*`/`I32Store*` as a base

**Value-typed I32Const sites (must NOT change):**
- Tag discriminants (e.g. `TAG_INT`, `TAG_LIST`)
- Chunk-size constants (`CHUNK_SIZE`)
- Boolean flags, length fields, `align` values inside `MemArg`

**Action:** Introduce `ptr_const(addr: u64) -> Instruction` helper. Walk each
`I32Const` site and annotate with address vs value intent (can be done via the
surrounding opcode pattern).

### C. `I32Add` / `I32Sub` on addresses

Used for pointer arithmetic: advancing bump cursor, computing header base,
computing end-of-chunk address.

| File | Count | Notes |
|---|---|---|
| `emit.rs` | 164 | Many are value arithmetic (chunk index, count); address ones are adjacent to I32Load/Store |
| `backend.rs` | 10 | Mostly header/payload offset arithmetic |

**Action:** Replace address-arithmetic `I32Add`/`I32Sub` with `ptr_add`/`ptr_sub`
helpers. Value-arithmetic uses (chunk index, list length, count comparisons) are
unchanged.

### D. `MemoryGrow` / `MemorySize`

Both sites are in the bump allocator helper in `emit.rs`.

| File | Line | Function | Notes |
|---|---|---|---|
| `emit.rs` | 3735 | `emit_alloc_helper` (inner grow block) | `MemoryGrow(0)` — returns old page count as i32; under memory64 returns i64 |
| `emit.rs` | 3775 | `emit_alloc_helper` (rebase cursor) | `MemorySize(0)` — returns current page count as i32; under memory64 returns i64 |

**Context:** The grow block computes `delta_pages` as an i32, then calls
`MemoryGrow`. After the grow, `MemorySize` is used to rebase the cursor. Both the
operand to `MemoryGrow` and the result of `MemorySize` become i64 under memory64.

**Action:**
1. The `delta_pages` computation (currently i32 div-ceil) must produce an i64.
2. `MemoryGrow(0)` result (`-1` on failure) must be compared as i64.
3. `MemorySize(0)` result multiplied by `WASM_PAGE_BYTES` must use i64 multiply.
4. The subsequent `I32Sub HOST_BUMP_RESERVED_BYTES` must become i64.

### E. `WASM_PAGE_BYTES` multiplications (address arithmetic)

`WASM_PAGE_BYTES` appears in three contexts in `emit.rs` and in `wasm_exec.rs`.

| File | Line | Context |
|---|---|---|
| `emit.rs` | 3731 | `(WASM_PAGE_BYTES - 1) as i32` — div-ceil mask for delta_pages calc |
| `emit.rs` | 3733 | `WASM_PAGE_BYTES as i32` — divisor for delta_pages |
| `emit.rs` | 3746 | `WASM_PAGE_BYTES as i32` — multiply old page count to get base byte address |
| `emit.rs` | 3776 | `WASM_PAGE_BYTES as i32` — multiply MemorySize result to get byte address |
| `wasm_exec.rs` | 673 | `pages * WASM_PAGE_BYTES` — host-side `u64` multiply (already u64, OK) |
| `wasm_exec.rs` | 693 | `missing.div_ceil(WASM_PAGE_BYTES)` — host-side u64 div-ceil (already OK) |

**Action:** The four `emit.rs` sites must use `i64` arithmetic. `wasm_exec.rs`
host-side uses are already `u64` and do not change.

## Non-address I32 arithmetic (must NOT change)

These categories use `i32` for value semantics, not addresses. They must be
preserved as-is after M4.

- Tag bit masking and extraction (`& 0b111`, `>> 3`)
- Integer payload encode/decode (`value >> 3`, `value << 3 | TAG_INT`)
- Chunk-index arithmetic and comparisons
- List-length, string-length fields stored as payload i32 counts
- Boolean result values (`0` / `1`)
- `MemArg.align` and `MemArg.offset` fields (not instructions, not changed)

## M4 walk order (recommended)

1. **M4.1** — Introduce `ptr_load`, `ptr_store`, `ptr_const`, `ptr_add`, `ptr_sub`,
   `ptr_mul` helpers in a new `gen_lower/ptr.rs`. Unit-test each in both i32 and i64 mode.
2. **M4.2** — Walk category A (I32Load/Store), file by file. `cargo test -p goby-wasm` after each function group.
3. **M4.3** — Walk category B (address-typed I32Const). Use surrounding-opcode pattern to identify.
4. **M4.4** — Walk category C (I32Add/Sub on addresses).
5. **M4.5** — Walk category D+E (MemoryGrow/MemorySize + WASM_PAGE_BYTES multiply).
6. **M4.6** — Flip `RUNTIME_MEMORY_CONFIG.memory64 = true`. Run full suite.
7. **M4.7** — Comment/doc audit (`rg '4\s*GiB|64\s*MiB|wasm32|max_pages' crates/ doc/`).

## Checklist status

- [x] A: I32Load*/I32Store* → ptr_load/ptr_store
- [x] B: Address-typed I32Const → ptr_const
- [x] C: Address-arithmetic I32Add/Sub → ptr_add/ptr_sub
- [x] D: MemoryGrow/MemorySize → i64 operand/result
- [x] E: WASM_PAGE_BYTES multiplies in emit.rs → i64
- [x] Flip RUNTIME_MEMORY_CONFIG.memory64 = true
- [x] Comment/doc audit

Archive note (2026-04-18):
- This inventory is complete and now serves as the historical checklist for the
  memory64 migration that later unblocked Perceus refcount/reuse work.
