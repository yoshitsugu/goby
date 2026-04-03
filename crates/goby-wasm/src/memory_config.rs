use wasm_encoder::MemoryType;

pub(crate) const WASM_PAGE_BYTES: u32 = 65_536;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct WasmMemoryConfig {
    pub initial_pages: u32,
    pub max_pages: u32,
    pub host_bump_reserved_bytes: u32,
    pub max_wasm_stack_bytes: usize,
}

impl WasmMemoryConfig {
    pub(crate) const fn initial_linear_memory_bytes(self) -> u32 {
        self.initial_pages * WASM_PAGE_BYTES
    }

    pub(crate) const fn max_linear_memory_bytes(self) -> u32 {
        self.max_pages * WASM_PAGE_BYTES
    }

    pub(crate) const fn host_bump_start(self) -> u32 {
        self.initial_linear_memory_bytes() - self.host_bump_reserved_bytes
    }

    pub(crate) fn memory_type(self) -> MemoryType {
        MemoryType {
            minimum: self.initial_pages as u64,
            maximum: Some(self.max_pages as u64),
            memory64: false,
            shared: false,
            page_size_log2: None,
        }
    }
}

pub(crate) const DEFAULT_WASM_MEMORY_CONFIG: WasmMemoryConfig = WasmMemoryConfig {
    initial_pages: 4,
    max_pages: 1024,
    host_bump_reserved_bytes: 49_152,
    max_wasm_stack_bytes: 64 * 1024 * 1024,
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_memory_config_locks_bounded_growth_defaults() {
        assert_eq!(DEFAULT_WASM_MEMORY_CONFIG.initial_pages, 4);
        assert_eq!(
            DEFAULT_WASM_MEMORY_CONFIG.initial_linear_memory_bytes(),
            262_144
        );
        assert_eq!(DEFAULT_WASM_MEMORY_CONFIG.max_pages, 1024);
        assert_eq!(
            DEFAULT_WASM_MEMORY_CONFIG.max_linear_memory_bytes(),
            67_108_864
        );
        assert_eq!(DEFAULT_WASM_MEMORY_CONFIG.host_bump_start(), 212_992);
    }
}
