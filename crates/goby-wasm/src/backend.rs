use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, ImportSection, Instruction, MemArg, MemorySection, MemoryType, Module,
    TypeSection, ValType,
};

use crate::{CodegenError, layout::MemoryLayout};

const WASM_PAGE_BYTES: u32 = 65_536;

pub(crate) struct WasmProgramBuilder {
    layout: MemoryLayout,
}

impl WasmProgramBuilder {
    pub(crate) fn new(layout: MemoryLayout) -> Self {
        Self { layout }
    }

    pub(crate) fn emit_static_print_module(&self, text: &str) -> Result<Vec<u8>, CodegenError> {
        let text_len = u32::try_from(text.len()).map_err(|_| CodegenError {
            message: "print literal is too large to encode".to_string(),
        })?;
        let text_ptr = i32::try_from(self.layout.heap_base).map_err(|_| CodegenError {
            message: "text offset does not fit in i32".to_string(),
        })?;
        let iovec_offset = i32::try_from(self.layout.iovec_offset).map_err(|_| CodegenError {
            message: "iovec offset does not fit in i32".to_string(),
        })?;
        let nwritten_offset =
            i32::try_from(self.layout.nwritten_offset).map_err(|_| CodegenError {
                message: "nwritten offset does not fit in i32".to_string(),
            })?;
        let text_len_i32 = i32::try_from(text_len).map_err(|_| CodegenError {
            message: "print literal length does not fit in i32".to_string(),
        })?;

        let mut module = Module::new();

        let mut types = TypeSection::new();
        let fd_write_type = types.len();
        types.ty().function(
            [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32],
        );
        let main_type = types.len();
        types.ty().function([], []);
        module.section(&types);

        let mut imports = ImportSection::new();
        imports.import(
            "wasi_snapshot_preview1",
            "fd_write",
            EntityType::Function(fd_write_type),
        );
        module.section(&imports);

        let mut functions = FunctionSection::new();
        functions.function(main_type);
        module.section(&functions);

        let mut memories = MemorySection::new();
        memories.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memories);

        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("_start", ExportKind::Func, 1);
        module.section(&exports);

        // Code section (id=10) must come before Data section (id=11) per Wasm spec §5.5.2.
        let mut code = CodeSection::new();
        let mut function = Function::new([]);
        function.instruction(&Instruction::I32Const(iovec_offset));
        function.instruction(&Instruction::I32Const(text_ptr));
        function.instruction(&Instruction::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        function.instruction(&Instruction::I32Const(iovec_offset + 4));
        function.instruction(&Instruction::I32Const(text_len_i32));
        function.instruction(&Instruction::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Const(iovec_offset));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Const(nwritten_offset));
        function.instruction(&Instruction::Call(0));
        function.instruction(&Instruction::Drop);
        function.instruction(&Instruction::End);
        code.function(&function);
        module.section(&code);

        let mut data = DataSection::new();
        data.active(0, &ConstExpr::i32_const(text_ptr), text.as_bytes().to_vec());
        module.section(&data);

        Ok(module.finish())
    }

    pub(crate) fn emit_read_all_to_stdout_module(
        &self,
        append_newline: bool,
    ) -> Result<Vec<u8>, CodegenError> {
        let buffer_ptr = i32::try_from(self.layout.heap_base).map_err(|_| CodegenError {
            message: "heap base does not fit in i32".to_string(),
        })?;
        let iovec_offset = i32::try_from(self.layout.iovec_offset).map_err(|_| CodegenError {
            message: "iovec offset does not fit in i32".to_string(),
        })?;
        let nread_offset =
            i32::try_from(self.layout.nwritten_offset).map_err(|_| CodegenError {
                message: "nread offset does not fit in i32".to_string(),
            })?;
        let buffer_len =
            i32::try_from(WASM_PAGE_BYTES - self.layout.heap_base).map_err(|_| CodegenError {
                message: "stdin buffer length does not fit in i32".to_string(),
            })?;
        let newline_ptr = i32::try_from(self.layout.heap_base - 1).map_err(|_| CodegenError {
            message: "newline pointer does not fit in i32".to_string(),
        })?;

        let mut module = Module::new();

        let mut types = TypeSection::new();
        let fd_io_type = types.len();
        types.ty().function(
            [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32],
        );
        let main_type = types.len();
        types.ty().function([], []);
        module.section(&types);

        let mut imports = ImportSection::new();
        imports.import(
            "wasi_snapshot_preview1",
            "fd_read",
            EntityType::Function(fd_io_type),
        );
        imports.import(
            "wasi_snapshot_preview1",
            "fd_write",
            EntityType::Function(fd_io_type),
        );
        module.section(&imports);

        let mut functions = FunctionSection::new();
        functions.function(main_type);
        module.section(&functions);

        let mut memories = MemorySection::new();
        memories.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memories);

        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("_start", ExportKind::Func, 2);
        module.section(&exports);

        let mut code = CodeSection::new();
        let mut function = Function::new([]);

        // Prepare one iovec for stdin read into the heap buffer.
        function.instruction(&Instruction::I32Const(iovec_offset));
        function.instruction(&Instruction::I32Const(buffer_ptr));
        function.instruction(&Instruction::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        function.instruction(&Instruction::I32Const(iovec_offset + 4));
        function.instruction(&Instruction::I32Const(buffer_len));
        function.instruction(&Instruction::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));

        // fd_read(stdin=0, &iovec, 1, &nread)
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Const(iovec_offset));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Const(nread_offset));
        function.instruction(&Instruction::Call(0));
        function.instruction(&Instruction::Drop);

        // Reuse the same iovec for stdout, but shrink len to the bytes actually read.
        function.instruction(&Instruction::I32Const(iovec_offset + 4));
        function.instruction(&Instruction::I32Const(nread_offset));
        function.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        function.instruction(&Instruction::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));

        // fd_write(stdout=1, &iovec, 1, &nwritten)
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Const(iovec_offset));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Const(nread_offset));
        function.instruction(&Instruction::Call(1));
        function.instruction(&Instruction::Drop);
        if append_newline {
            function.instruction(&Instruction::I32Const(iovec_offset));
            function.instruction(&Instruction::I32Const(newline_ptr));
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            function.instruction(&Instruction::I32Const(iovec_offset + 4));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Const(iovec_offset));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Const(nread_offset));
            function.instruction(&Instruction::Call(1));
            function.instruction(&Instruction::Drop);
        }
        function.instruction(&Instruction::End);

        code.function(&function);
        module.section(&code);

        if append_newline {
            let mut data = DataSection::new();
            data.active(0, &ConstExpr::i32_const(newline_ptr), b"\n".to_vec());
            module.section(&data);
        }

        Ok(module.finish())
    }
}
