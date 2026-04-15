use wasm_encoder::MemoryType;

pub const WASM_PAGE_BYTES: u32 = 65_536;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WasmMemoryConfig {
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

/// Test config: 64 MiB ceiling (matches the historical default).
/// Use in unit tests and wasm-encode-only helpers that do not grow memory dynamically.
pub(crate) const TEST_MEMORY_CONFIG: WasmMemoryConfig = WasmMemoryConfig {
    initial_pages: 4,
    max_pages: 1024,
    host_bump_reserved_bytes: 49_152,
    max_wasm_stack_bytes: 64 * 1024 * 1024,
};

/// Runtime config: 1 GiB ceiling used when emitting modules for actual execution.
/// All modules emitted for `goby run` declare this maximum so the CLI can raise
/// the effective ceiling up to 1 GiB via `--max-memory-mb` / `GOBY_MAX_MEMORY_MB`.
pub const RUNTIME_MEMORY_CONFIG: WasmMemoryConfig = WasmMemoryConfig {
    initial_pages: 4,
    max_pages: 16_384,
    host_bump_reserved_bytes: 49_152,
    max_wasm_stack_bytes: 64 * 1024 * 1024,
};

/// Deprecated alias for `TEST_MEMORY_CONFIG`.  Do not add new uses.
pub(crate) const DEFAULT_WASM_MEMORY_CONFIG: WasmMemoryConfig = TEST_MEMORY_CONFIG;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memory_config_locks_64_mib_ceiling() {
        assert_eq!(TEST_MEMORY_CONFIG.initial_pages, 4);
        assert_eq!(TEST_MEMORY_CONFIG.initial_linear_memory_bytes(), 262_144);
        assert_eq!(TEST_MEMORY_CONFIG.max_pages, 1024);
        assert_eq!(TEST_MEMORY_CONFIG.max_linear_memory_bytes(), 67_108_864);
        assert_eq!(TEST_MEMORY_CONFIG.host_bump_start(), 212_992);
    }

    #[test]
    fn runtime_memory_config_locks_1_gib_ceiling() {
        assert_eq!(RUNTIME_MEMORY_CONFIG.initial_pages, 4);
        assert_eq!(
            RUNTIME_MEMORY_CONFIG.initial_linear_memory_bytes(),
            262_144
        );
        assert_eq!(RUNTIME_MEMORY_CONFIG.max_pages, 16_384);
        assert_eq!(
            RUNTIME_MEMORY_CONFIG.max_linear_memory_bytes(),
            1_073_741_824
        );
        assert_eq!(RUNTIME_MEMORY_CONFIG.host_bump_start(), 212_992);
    }
}
