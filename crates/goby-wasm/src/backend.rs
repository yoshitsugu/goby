use crate::layout::MemoryLayout;

pub(crate) struct WasmProgramBuilder {
    layout: MemoryLayout,
}

impl WasmProgramBuilder {
    pub(crate) fn new(layout: MemoryLayout) -> Self {
        Self { layout }
    }

    pub(crate) fn layout(&self) -> MemoryLayout {
        self.layout
    }
}
