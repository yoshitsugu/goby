pub(crate) const IOVEC_OFFSET: u32 = 0;
pub(crate) const NWRITTEN_OFFSET: u32 = 8;
pub(crate) const HEAP_BASE: u32 = 16;
pub(crate) const TEXT_OFFSET: u32 = HEAP_BASE;

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
