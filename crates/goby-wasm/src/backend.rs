use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, ImportSection, Instruction, MemArg, MemorySection, MemoryType, Module,
    TypeSection, ValType,
};

use crate::{
    CodegenError,
    layout::MemoryLayout,
    runtime_io_plan::{OutputReadMode, StaticPrintSuffix},
};

const WASM_PAGE_BYTES: u32 = 65_536;

struct RuntimeIoMemoryPlan {
    buffer_ptr: i32,
    iovec_offset: i32,
    nread_offset: i32,
    buffer_len: i32,
    newline_ptr: i32,
    suffix_base: usize,
    suffix_payloads: Vec<Vec<u8>>,
}

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
        suffix_prints: &[StaticPrintSuffix],
    ) -> Result<Vec<u8>, CodegenError> {
        let plan = self.runtime_io_memory_plan(suffix_prints)?;
        self.build_runtime_io_module(&plan, &[], append_newline, |function, plan| {
            emit_prepare_iovec(
                function,
                plan.iovec_offset,
                plan.buffer_ptr,
                plan.buffer_len,
            );
            emit_fd_read_stdin(function, plan.iovec_offset, plan.nread_offset);
            emit_set_iovec_len_from_nread(function, plan.iovec_offset, plan.nread_offset);
            emit_fd_write_stdout(function, plan.iovec_offset, plan.nread_offset);
            if append_newline {
                emit_write_newline(
                    function,
                    plan.iovec_offset,
                    plan.nread_offset,
                    plan.newline_ptr,
                );
            }
            emit_static_suffix_writes(
                function,
                plan.iovec_offset,
                plan.nread_offset,
                plan.suffix_base,
                &plan.suffix_payloads,
            )
        })
    }

    pub(crate) fn emit_read_line_to_stdout_module(
        &self,
        append_newline: bool,
        suffix_prints: &[StaticPrintSuffix],
    ) -> Result<Vec<u8>, CodegenError> {
        let plan = self.runtime_io_memory_plan(suffix_prints)?;
        self.build_runtime_io_module(
            &plan,
            &[(2, ValType::I32)],
            append_newline,
            |function, plan| {
                emit_prepare_iovec(
                    function,
                    plan.iovec_offset,
                    plan.buffer_ptr,
                    plan.buffer_len,
                );
                emit_fd_read_stdin(function, plan.iovec_offset, plan.nread_offset);

                function.instruction(&Instruction::I32Const(plan.nread_offset));
                function.instruction(&Instruction::I32Load(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                function.instruction(&Instruction::LocalSet(1));
                function.instruction(&Instruction::I32Const(0));
                function.instruction(&Instruction::LocalSet(0));

                function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                function.instruction(&Instruction::LocalGet(0));
                function.instruction(&Instruction::LocalGet(1));
                function.instruction(&Instruction::I32GeU);
                function.instruction(&Instruction::BrIf(1));

                emit_load_buffer_byte(function, plan.buffer_ptr);
                function.instruction(&Instruction::LocalGet(0));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::I32Load8U(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }));
                function.instruction(&Instruction::I32Const(10));
                function.instruction(&Instruction::I32Eq);
                function.instruction(&Instruction::BrIf(1));

                emit_load_buffer_byte(function, plan.buffer_ptr);
                function.instruction(&Instruction::LocalGet(0));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::I32Load8U(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }));
                function.instruction(&Instruction::I32Const(13));
                function.instruction(&Instruction::I32Eq);
                function.instruction(&Instruction::BrIf(1));

                function.instruction(&Instruction::LocalGet(0));
                function.instruction(&Instruction::I32Const(1));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(0));
                function.instruction(&Instruction::Br(0));
                function.instruction(&Instruction::End);
                function.instruction(&Instruction::End);

                function.instruction(&Instruction::I32Const(plan.iovec_offset + 4));
                function.instruction(&Instruction::LocalGet(0));
                function.instruction(&Instruction::I32Store(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                emit_fd_write_stdout(function, plan.iovec_offset, plan.nread_offset);
                if append_newline {
                    emit_write_newline(
                        function,
                        plan.iovec_offset,
                        plan.nread_offset,
                        plan.newline_ptr,
                    );
                }
                emit_static_suffix_writes(
                    function,
                    plan.iovec_offset,
                    plan.nread_offset,
                    plan.suffix_base,
                    &plan.suffix_payloads,
                )
            },
        )
    }

    /// Emit a WASI module that reads all of stdin, splits on `\n`, and for each
    /// line emits `prefix + line_content + suffix` (with an optional newline from
    /// `append_newline` appended after the suffix).
    ///
    /// `prefix` and `suffix` are static strings known at compile time.  They are
    /// stored in the data section.  Empty strings are valid (nothing is written for
    /// that component).
    ///
    /// Locals used: local 0 = scan pos, local 1 = nread, local 2 = line_start.
    pub(crate) fn emit_read_split_lines_each_transform_module(
        &self,
        prefix: &str,
        suffix: &str,
        append_newline: bool,
        suffix_prints: &[StaticPrintSuffix],
    ) -> Result<Vec<u8>, CodegenError> {
        let prefix_bytes = prefix.as_bytes();
        let suffix_bytes = suffix.as_bytes();

        // Compute memory layout for static strings.
        // We place (from low to high, before heap_base):
        //   [newline_ptr] = heap_base - 1             (1 byte: '\n')
        //   [suffix_str]  = heap_base - 1 - suffix_len  (suffix_len bytes)
        //   [prefix_str]  = heap_base - 1 - suffix_len - prefix_len (prefix_len bytes)
        let suffix_payload_bytes: Vec<Vec<u8>> = encode_static_print_suffix_payloads(suffix_prints);
        let suffix_payloads_total: usize = suffix_payload_bytes.iter().map(Vec::len).sum();

        let prefix_len =
            i32::try_from(prefix_bytes.len()).map_err(|_| CodegenError {
                message: "transform prefix is too long".to_string(),
            })?;
        let suffix_len =
            i32::try_from(suffix_bytes.len()).map_err(|_| CodegenError {
                message: "transform suffix is too long".to_string(),
            })?;

        // heap_base - 1 is the newline byte slot (already used by newline writer)
        let newline_ptr =
            i32::try_from(self.layout.heap_base - 1).map_err(|_| CodegenError {
                message: "newline pointer does not fit in i32".to_string(),
            })?;
        // suffix string slot: just below newline
        let suffix_str_ptr = newline_ptr - suffix_len;
        // prefix string slot: just below suffix
        let prefix_str_ptr = suffix_str_ptr - prefix_len;

        // Guard: prefix and suffix must not overlap the iovec/nread region [0..nwritten_offset+4].
        let iovec_end =
            i32::try_from(self.layout.nwritten_offset + 4).map_err(|_| CodegenError {
                message: "iovec end offset does not fit in i32".to_string(),
            })?;
        if prefix_str_ptr < iovec_end {
            return Err(CodegenError {
                message: format!(
                    "transform prefix+suffix combined ({} bytes) is too long: \
                     static string region would overlap iovec/nread area",
                    prefix_bytes.len() + suffix_bytes.len()
                ),
            });
        }

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
        let buffer_len = i32::try_from(
            usize::try_from(WASM_PAGE_BYTES - self.layout.heap_base)
                .map_err(|_| CodegenError {
                    message: "stdin buffer length does not fit in usize".to_string(),
                })?
                .checked_sub(suffix_payloads_total)
                .ok_or_else(|| CodegenError {
                    message: "static print suffixes leave no room for stdin buffer".to_string(),
                })?,
        )
        .map_err(|_| CodegenError {
            message: "stdin buffer length does not fit in i32".to_string(),
        })?;
        let suffix_payload_base = usize::try_from(WASM_PAGE_BYTES)
            .map_err(|_| CodegenError {
                message: "wasm page size does not fit in usize".to_string(),
            })?
            .checked_sub(suffix_payloads_total)
            .ok_or_else(|| CodegenError {
                message: "static print suffix payloads overflow wasm memory page".to_string(),
            })?;

        // Build module
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

        // Locals: 0=pos, 1=nread, 2=line_start
        let locals = [(3u32, ValType::I32)];
        let mut code = CodeSection::new();
        let mut function = Function::new(locals.iter().copied());

        // --- Read all of stdin ---
        emit_prepare_iovec(&mut function, iovec_offset, buffer_ptr, buffer_len);
        emit_fd_read_stdin(&mut function, iovec_offset, nread_offset);

        // nread = memory[nread_offset]
        function.instruction(&Instruction::I32Const(nread_offset));
        function.instruction(&Instruction::I32Load(MemArg { offset: 0, align: 2, memory_index: 0 }));
        function.instruction(&Instruction::LocalSet(1)); // local 1 = nread

        // pos = 0; line_start = 0
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::LocalSet(0));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::LocalSet(2));

        // if nread == 0 { /* no output */ } else { ... }
        function.instruction(&Instruction::LocalGet(1));
        function.instruction(&Instruction::I32Eqz);
        function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        function.instruction(&Instruction::Else);

        // Block/Loop: scan for '\n' boundaries
        // Outer block: used to break out of the loop
        function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty)); // block 0
        function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty)); // loop 0

        // if pos >= nread: break out
        function.instruction(&Instruction::LocalGet(0));
        function.instruction(&Instruction::LocalGet(1));
        function.instruction(&Instruction::I32GeU);
        function.instruction(&Instruction::BrIf(1)); // break block 0

        // byte = memory[buffer_ptr + pos]
        function.instruction(&Instruction::I32Const(buffer_ptr));
        function.instruction(&Instruction::LocalGet(0));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::I32Load8U(MemArg { offset: 0, align: 0, memory_index: 0 }));
        // if byte == '\n'
        function.instruction(&Instruction::I32Const(10));
        function.instruction(&Instruction::I32Eq);
        function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

        // --- Emit: prefix + line_slice + suffix + optional_newline ---
        // Write prefix (if non-empty)
        if !prefix_bytes.is_empty() {
            emit_static_write(&mut function, iovec_offset, nread_offset, prefix_str_ptr, prefix_len);
        }
        // Write line_slice: [buffer_ptr + line_start .. pos)
        // iovec[0] = buffer_ptr + line_start
        function.instruction(&Instruction::I32Const(iovec_offset));
        function.instruction(&Instruction::I32Const(buffer_ptr));
        function.instruction(&Instruction::LocalGet(2));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));
        // iovec[1] = pos - line_start
        function.instruction(&Instruction::I32Const(iovec_offset + 4));
        function.instruction(&Instruction::LocalGet(0));
        function.instruction(&Instruction::LocalGet(2));
        function.instruction(&Instruction::I32Sub);
        function.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));
        emit_fd_write_stdout(&mut function, iovec_offset, nread_offset);
        // Write suffix (if non-empty)
        if !suffix_bytes.is_empty() {
            emit_static_write(&mut function, iovec_offset, nread_offset, suffix_str_ptr, suffix_len);
        }
        // Append newline if requested
        if append_newline {
            emit_write_newline(&mut function, iovec_offset, nread_offset, newline_ptr);
        }

        // line_start = pos + 1
        function.instruction(&Instruction::LocalGet(0));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalSet(2));

        function.instruction(&Instruction::End); // end if byte == '\n'

        // pos += 1
        function.instruction(&Instruction::LocalGet(0));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalSet(0));
        function.instruction(&Instruction::Br(0)); // continue loop
        function.instruction(&Instruction::End); // end loop
        function.instruction(&Instruction::End); // end block 0

        // After loop: emit final line if line_start < nread (last line without trailing '\n')
        // if line_start < nread:
        function.instruction(&Instruction::LocalGet(2));
        function.instruction(&Instruction::LocalGet(1));
        function.instruction(&Instruction::I32LtU);
        function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        // Write prefix
        if !prefix_bytes.is_empty() {
            emit_static_write(&mut function, iovec_offset, nread_offset, prefix_str_ptr, prefix_len);
        }
        // Write last line slice [line_start .. nread)
        function.instruction(&Instruction::I32Const(iovec_offset));
        function.instruction(&Instruction::I32Const(buffer_ptr));
        function.instruction(&Instruction::LocalGet(2));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));
        function.instruction(&Instruction::I32Const(iovec_offset + 4));
        function.instruction(&Instruction::LocalGet(1));
        function.instruction(&Instruction::LocalGet(2));
        function.instruction(&Instruction::I32Sub);
        function.instruction(&Instruction::I32Store(MemArg { offset: 0, align: 2, memory_index: 0 }));
        emit_fd_write_stdout(&mut function, iovec_offset, nread_offset);
        // Write suffix
        if !suffix_bytes.is_empty() {
            emit_static_write(&mut function, iovec_offset, nread_offset, suffix_str_ptr, suffix_len);
        }
        if append_newline {
            emit_write_newline(&mut function, iovec_offset, nread_offset, newline_ptr);
        }
        function.instruction(&Instruction::End); // end if last line

        // Static suffix_prints after the each
        emit_static_suffix_writes(
            &mut function,
            iovec_offset,
            nread_offset,
            suffix_payload_base,
            &suffix_payload_bytes,
        )?;

        function.instruction(&Instruction::End); // end else (nread != 0)
        function.instruction(&Instruction::End); // end function

        code.function(&function);
        module.section(&code);

        // Data section: newline, suffix_str, prefix_str, then suffix_payloads
        let mut data = DataSection::new();
        // Always emit newline slot (used when append_newline=true or for consistency)
        data.active(0, &ConstExpr::i32_const(newline_ptr), b"\n".to_vec());
        if !suffix_bytes.is_empty() {
            data.active(0, &ConstExpr::i32_const(suffix_str_ptr), suffix_bytes.to_vec());
        }
        if !prefix_bytes.is_empty() {
            data.active(0, &ConstExpr::i32_const(prefix_str_ptr), prefix_bytes.to_vec());
        }
        let mut sp = i32::try_from(suffix_payload_base).map_err(|_| CodegenError {
            message: "suffix payload base does not fit in i32".to_string(),
        })?;
        for payload in &suffix_payload_bytes {
            data.active(0, &ConstExpr::i32_const(sp), payload.clone());
            sp += i32::try_from(payload.len()).map_err(|_| CodegenError {
                message: "suffix payload length does not fit in i32".to_string(),
            })?;
        }
        module.section(&data);

        Ok(module.finish())
    }

    pub(crate) fn emit_read_split_lines_each_print_module(
        &self,
        append_newline: bool,
        suffix_prints: &[StaticPrintSuffix],
    ) -> Result<Vec<u8>, CodegenError> {
        let plan = self.runtime_io_memory_plan(suffix_prints)?;
        self.build_runtime_io_module(&plan, &[], append_newline, |function, plan| {
            emit_prepare_iovec(
                function,
                plan.iovec_offset,
                plan.buffer_ptr,
                plan.buffer_len,
            );
            emit_fd_read_stdin(function, plan.iovec_offset, plan.nread_offset);

            function.instruction(&Instruction::I32Const(plan.nread_offset));
            function.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            function.instruction(&Instruction::I32Eqz);
            function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            function.instruction(&Instruction::Else);

            emit_set_iovec_len_from_nread(function, plan.iovec_offset, plan.nread_offset);
            emit_fd_write_stdout(function, plan.iovec_offset, plan.nread_offset);
            if append_newline {
                emit_write_newline(
                    function,
                    plan.iovec_offset,
                    plan.nread_offset,
                    plan.newline_ptr,
                );
            }
            emit_static_suffix_writes(
                function,
                plan.iovec_offset,
                plan.nread_offset,
                plan.suffix_base,
                &plan.suffix_payloads,
            )?;

            function.instruction(&Instruction::End);
            Ok(())
        })
    }
}

impl WasmProgramBuilder {
    fn runtime_io_memory_plan(
        &self,
        suffix_prints: &[StaticPrintSuffix],
    ) -> Result<RuntimeIoMemoryPlan, CodegenError> {
        let suffix_payloads = encode_static_print_suffix_payloads(suffix_prints);
        let suffix_bytes_total: usize = suffix_payloads.iter().map(Vec::len).sum();
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
        let buffer_len = i32::try_from(
            usize::try_from(WASM_PAGE_BYTES - self.layout.heap_base)
                .map_err(|_| CodegenError {
                    message: "stdin buffer length does not fit in usize".to_string(),
                })?
                .checked_sub(suffix_bytes_total)
                .ok_or_else(|| CodegenError {
                    message: "static print suffixes leave no room for stdin buffer".to_string(),
                })?,
        )
        .map_err(|_| CodegenError {
            message: "stdin buffer length does not fit in i32".to_string(),
        })?;
        let newline_ptr = i32::try_from(self.layout.heap_base - 1).map_err(|_| CodegenError {
            message: "newline pointer does not fit in i32".to_string(),
        })?;
        let suffix_base = usize::try_from(WASM_PAGE_BYTES)
            .map_err(|_| CodegenError {
                message: "wasm page size does not fit in usize".to_string(),
            })?
            .checked_sub(suffix_bytes_total)
            .ok_or_else(|| CodegenError {
                message: "static print suffix payloads overflow wasm memory page".to_string(),
            })?;
        Ok(RuntimeIoMemoryPlan {
            buffer_ptr,
            iovec_offset,
            nread_offset,
            buffer_len,
            newline_ptr,
            suffix_base,
            suffix_payloads,
        })
    }

    fn build_runtime_io_module<F>(
        &self,
        plan: &RuntimeIoMemoryPlan,
        locals: &[(u32, ValType)],
        append_newline: bool,
        build_body: F,
    ) -> Result<Vec<u8>, CodegenError>
    where
        F: FnOnce(&mut Function, &RuntimeIoMemoryPlan) -> Result<(), CodegenError>,
    {
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
        let mut function = Function::new(locals.iter().copied());
        build_body(&mut function, plan)?;
        function.instruction(&Instruction::End);
        code.function(&function);
        module.section(&code);

        emit_runtime_io_data_section(
            &mut module,
            append_newline.then_some(plan.newline_ptr),
            plan.suffix_base,
            &plan.suffix_payloads,
        )?;

        Ok(module.finish())
    }
}

fn emit_prepare_iovec(
    function: &mut Function,
    iovec_offset: i32,
    buffer_ptr: i32,
    buffer_len: i32,
) {
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
}

fn emit_fd_read_stdin(function: &mut Function, iovec_offset: i32, nread_offset: i32) {
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32Const(iovec_offset));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Const(nread_offset));
    function.instruction(&Instruction::Call(0));
    function.instruction(&Instruction::Drop);
}

fn emit_set_iovec_len_from_nread(function: &mut Function, iovec_offset: i32, nread_offset: i32) {
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
}

fn emit_fd_write_stdout(function: &mut Function, iovec_offset: i32, nread_offset: i32) {
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Const(iovec_offset));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Const(nread_offset));
    function.instruction(&Instruction::Call(1));
    function.instruction(&Instruction::Drop);
}

fn emit_write_newline(
    function: &mut Function,
    iovec_offset: i32,
    nread_offset: i32,
    newline_ptr: i32,
) {
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
    emit_fd_write_stdout(function, iovec_offset, nread_offset);
}

fn emit_load_buffer_byte(function: &mut Function, buffer_ptr: i32) {
    function.instruction(&Instruction::I32Const(buffer_ptr));
}

/// Write a static byte slice (known at compile time) to stdout via a single fd_write call.
/// `ptr` and `len` must be i32 values suitable for Wasm memory addressing.
fn emit_static_write(
    function: &mut Function,
    iovec_offset: i32,
    nread_offset: i32,
    ptr: i32,
    len: i32,
) {
    function.instruction(&Instruction::I32Const(iovec_offset));
    function.instruction(&Instruction::I32Const(ptr));
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I32Const(iovec_offset + 4));
    function.instruction(&Instruction::I32Const(len));
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    emit_fd_write_stdout(function, iovec_offset, nread_offset);
}

fn emit_static_suffix_writes(
    function: &mut Function,
    iovec_offset: i32,
    nread_offset: i32,
    suffix_base: usize,
    suffix_payloads: &[Vec<u8>],
) -> Result<(), CodegenError> {
    let mut suffix_ptr = i32::try_from(suffix_base).map_err(|_| CodegenError {
        message: "static print suffix base does not fit in i32".to_string(),
    })?;
    for payload in suffix_payloads {
        let payload_len = i32::try_from(payload.len()).map_err(|_| CodegenError {
            message: "static print suffix length does not fit in i32".to_string(),
        })?;
        function.instruction(&Instruction::I32Const(iovec_offset));
        function.instruction(&Instruction::I32Const(suffix_ptr));
        function.instruction(&Instruction::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        function.instruction(&Instruction::I32Const(iovec_offset + 4));
        function.instruction(&Instruction::I32Const(payload_len));
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
        suffix_ptr += payload_len;
    }
    Ok(())
}

fn emit_runtime_io_data_section(
    module: &mut Module,
    newline_ptr: Option<i32>,
    suffix_base: usize,
    suffix_payloads: &[Vec<u8>],
) -> Result<(), CodegenError> {
    if newline_ptr.is_none() && suffix_payloads.is_empty() {
        return Ok(());
    }
    let mut data = DataSection::new();
    if let Some(newline_ptr) = newline_ptr {
        data.active(0, &ConstExpr::i32_const(newline_ptr), b"\n".to_vec());
    }
    let mut suffix_ptr = i32::try_from(suffix_base).map_err(|_| CodegenError {
        message: "static print suffix base does not fit in i32".to_string(),
    })?;
    for payload in suffix_payloads {
        data.active(0, &ConstExpr::i32_const(suffix_ptr), payload.clone());
        suffix_ptr += i32::try_from(payload.len()).map_err(|_| CodegenError {
            message: "static print suffix length does not fit in i32".to_string(),
        })?;
    }
    module.section(&data);
    Ok(())
}

fn encode_static_print_suffix_payloads(suffix_prints: &[StaticPrintSuffix]) -> Vec<Vec<u8>> {
    suffix_prints
        .iter()
        .map(|suffix| {
            let mut bytes = suffix.text.as_bytes().to_vec();
            if matches!(suffix.output_mode, OutputReadMode::Println) && !bytes.ends_with(b"\n") {
                bytes.push(b'\n');
            }
            bytes
        })
        .collect()
}
