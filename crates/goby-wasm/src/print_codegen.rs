use crate::{CodegenError, backend, layout};

pub(crate) fn compile_print_module(text: &str) -> Result<Vec<u8>, CodegenError> {
    backend::WasmProgramBuilder::new(layout::MemoryLayout::default()).emit_static_print_module(text)
}
