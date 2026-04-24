pub(crate) const IOVEC_OFFSET: u32 = 0;
pub(crate) const NWRITTEN_OFFSET: u32 = 8;
/// Persistent global heap cursor stored in linear memory at this offset.
/// All functions (main and aux decls) share this single cursor so that
/// callee heap allocations and host intrinsic allocations do not overwrite
/// caller-allocated data.
/// Layout: bytes [12..16) = i32 top-down heap cursor (starts at STATIC_STRING_LIMIT).
pub(crate) const GLOBAL_HEAP_CURSOR_OFFSET: u32 = 12;
/// Persistent global heap floor stored in linear memory at this offset.
/// Callees update this when memory growth moves the current top-down segment,
/// and callers must reload it before performing further heap allocations.
/// Layout: bytes [16..20) = i32 current heap floor.
pub(crate) const GLOBAL_HEAP_FLOOR_OFFSET: u32 = 16;
/// Shared runtime error code slot used by Goby-owned Wasm execution.
/// Layout: bytes [20..24) = i32 error code, where 0 means "no runtime error".
pub(crate) const GLOBAL_RUNTIME_ERROR_OFFSET: u32 = 20;
/// Host bump allocator cursor (exclusive end) shared from host runtime.
/// Layout: bytes [24..28) = i32 host bump cursor.
/// Wasm-side top-down allocation reads this to avoid overlapping host-owned
/// allocations after memory growth.
pub(crate) const GLOBAL_HOST_BUMP_CURSOR_OFFSET: u32 = 24;
// bytes [28..32): 4-byte pad to align the following i64 slots to 8-byte boundary.
/// Running total of bytes allocated by emit_alloc_from_top (i64, zero-init).
/// Layout: bytes [32..40).
pub(crate) const GLOBAL_ALLOC_BYTES_TOTAL_OFFSET: u32 = 32;
/// Peak live bytes (i64, zero-init; wired in M3).
/// Layout: bytes [40..48).
pub(crate) const GLOBAL_PEAK_BYTES_OFFSET: u32 = 40;
/// Free-list hit counter (i64, zero-init; wired in M3).
/// Layout: bytes [48..56).
pub(crate) const GLOBAL_FREE_LIST_HITS_OFFSET: u32 = 48;
/// Running total of bytes returned to free lists (i64, zero-init; wired in M3).
/// Used to compute peak live bytes: peak = max(total_bytes - freed_bytes, peak).
/// Layout: bytes [56..64).
pub(crate) const GLOBAL_FREED_BYTES_OFFSET: u32 = 56;
/// Reuse-hit counter: incremented each time drop_reuse retains a unique token
/// (refcount == 1 path). Distinct from free_list_hits (which counts alloc-side
/// free-list pops). Layout: bytes [64..72).
pub(crate) const GLOBAL_REUSE_HITS_OFFSET: u32 = 64;

// ---------------------------------------------------------------------------
// Free-list head pointer table (M3).
//
// Each slot is an i64 payload pointer (0 = empty list). The intrusive link
// word lives at payload_ptr - 8 (the refcount slot, reused as next-pointer
// when the object is on the free list).
//
// Size-class layout (§3.2 of PLAN_PERCEUS.md):
//
//   Slot  Offset  Class
//   0     72      chunk            (one fixed size per CHUNK_SIZE)
//   1     80      header[1]
//   2     88      header[2]
//   3     96      header[4]
//   4     104     header[8]
//   5     112     header[16]
//   6     120     header[32]
//   7     128     header[64]
//   8     136     header[128]
//   9     144     tuple[1]
//   10    152     tuple[2]
//   11    160     tuple[3]
//   12    168     tuple[4]
//   13    176     tuple[5]
//   14    184     tuple[6]
//   15    192     tuple[7]
//   16    200     tuple[8]
//   17    208     record[1]
//   18    216     record[2]
//   19    224     record[3]
//   20    232     record[4]
//   21    240     record[5]
//   22    248     record[6]
//   23    256     record[7]
//   24    264     record[8]
//   25    272     closure[0]
//   26    280     closure[1]
//   27    288     closure[2]
//   28    296     closure[3]
//   29    304     closure[4]
//   30    312     closure[5]
//   31    320     closure[6]
//   32    328     closure[7]
//   33    336     closure[8]
//   34    344     cell
//   35    352     string[8]
//   36    360     string[16]
//   37    368     string[32]
//   38    376     string[64]
//   39    384     string[128]
//   40    392     string[256]
//   41    400     string[512]
//   42    408     string[large]    (exact-size overflow; not recycled cross-class)
//   (no slot 43; large allocs not freed to a list in M3)
//
// Total: 43 slots × 8 bytes = 344 bytes, occupying bytes [72..416).
// ---------------------------------------------------------------------------

pub(crate) const FREE_LIST_TABLE_BASE: u32 = 72;
pub(crate) const FREE_LIST_SLOT_CHUNK: u32 = FREE_LIST_TABLE_BASE; // slot 0

// header[k] slots: k ∈ {1,2,4,8,16,32,64,128} → slots 1..=8
pub(crate) const FREE_LIST_SLOT_HEADER_BASE: u32 = FREE_LIST_TABLE_BASE + 8; // slot 1 = header[1]

// tuple[a] slots: a ∈ {1..8} → slots 9..=16
pub(crate) const FREE_LIST_SLOT_TUPLE_BASE: u32 = FREE_LIST_TABLE_BASE + 72; // slot 9 = tuple[1]

// record[a] slots: a ∈ {1..8} → slots 17..=24
pub(crate) const FREE_LIST_SLOT_RECORD_BASE: u32 = FREE_LIST_TABLE_BASE + 136; // slot 17 = record[1]

// closure[s] slots: s ∈ {0..8} → slots 25..=33
pub(crate) const FREE_LIST_SLOT_CLOSURE_BASE: u32 = FREE_LIST_TABLE_BASE + 200; // slot 25 = closure[0]

// cell: slot 34
pub(crate) const FREE_LIST_SLOT_CELL: u32 = FREE_LIST_TABLE_BASE + 272; // slot 34

// string[b] slots: b ∈ {8,16,32,64,128,256,512,large} → slots 35..=42
pub(crate) const FREE_LIST_SLOT_STRING_BASE: u32 = FREE_LIST_TABLE_BASE + 280; // slot 35 = string[8]

/// Number of free-list head slots.
pub(crate) const FREE_LIST_SLOT_COUNT: u32 = 43;

/// First byte after the free-list table; this is where runtime heap data begins.
pub(crate) const HEAP_BASE: u32 = FREE_LIST_TABLE_BASE + FREE_LIST_SLOT_COUNT * 8; // = 408

pub(crate) const RUNTIME_ERROR_NONE: u32 = 0;
pub(crate) const RUNTIME_ERROR_MEMORY_EXHAUSTION: u32 = 1;

#[derive(Debug, Clone, Copy)]
pub(crate) struct MemoryLayout {
    pub iovec_offset: u32,
    pub nwritten_offset: u32,
    pub heap_base: u32,
}

impl Default for MemoryLayout {
    fn default() -> Self {
        Self {
            iovec_offset: IOVEC_OFFSET,
            nwritten_offset: NWRITTEN_OFFSET,
            heap_base: HEAP_BASE,
        }
    }
}
