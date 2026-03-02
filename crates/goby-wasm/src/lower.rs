use goby_core::Module;

use crate::{CodegenError, backend::WasmProgramBuilder, layout::MemoryLayout};

pub(crate) fn try_emit_native_module(module: &Module) -> Result<Option<Vec<u8>>, CodegenError> {
    if module.declarations.is_empty() {
        return Err(CodegenError {
            message: "module has no declarations".to_string(),
        });
    }

    let builder = WasmProgramBuilder::new(MemoryLayout::default());
    let layout = builder.layout();
    let _ = (
        layout.iovec_offset,
        layout.nwritten_offset,
        layout.heap_base,
    );

    Ok(None)
}
