pub(crate) const IOVEC_OFFSET: u32 = 0;
pub(crate) const NWRITTEN_OFFSET: u32 = 8;
/// Persistent global heap cursor stored in linear memory at this offset.
/// All functions (main and aux decls) share this single cursor so that
/// callee heap allocations do not overwrite caller-allocated data.
/// Layout: bytes [12..16) = i32 bump cursor (starts at STATIC_STRING_LIMIT).
pub(crate) const GLOBAL_HEAP_CURSOR_OFFSET: u32 = 12;
pub(crate) const HEAP_BASE: u32 = 24;

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
