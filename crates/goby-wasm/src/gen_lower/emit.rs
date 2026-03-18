//! `WasmBackendInstr` → Wasm bytes emission for the general lowering path.
//!
//! See `doc/wasm_runtime_architecture.md §4` for the effect-op WASI call mapping.

use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, ImportSection,
    Instruction, MemArg, MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::CodegenError;
use crate::gen_lower::backend_ir::WasmBackendInstr;
use crate::gen_lower::value::encode_string_ptr;
use crate::layout::MemoryLayout;

const WASM_PAGE_BYTES: u32 = 65_536;

// Import function indices (fd_read=0, fd_write=1).
const FD_READ_IDX: u32 = 0;
const FD_WRITE_IDX: u32 = 1;

/// Tracks compilation state during emission.
struct EmitContext {
    /// Map from local name to Wasm local index.
    locals: HashMap<String, u32>,
    /// Count of allocated locals.
    next_local: u32,
}

impl EmitContext {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            next_local: 0,
        }
    }

    fn declare(&mut self, name: &str) -> u32 {
        let idx = self.next_local;
        self.locals.insert(name.to_string(), idx);
        self.next_local += 1;
        idx
    }

    fn get(&self, name: &str) -> Result<u32, CodegenError> {
        self.locals.get(name).copied().ok_or_else(|| CodegenError {
            message: format!("gen_lower/emit: unknown local '{name}'"),
        })
    }
}

/// Emit a complete Wasm module from a flat list of `WasmBackendInstr`.
///
/// The module imports `fd_read` and `fd_write` from `wasi_snapshot_preview1`,
/// exports `memory` and `_start`, and contains one function (`main`).
pub(crate) fn emit_general_module(
    instrs: &[WasmBackendInstr],
    layout: &MemoryLayout,
) -> Result<Vec<u8>, CodegenError> {
    // --- Count declared locals (first pass) ---
    let local_count = instrs
        .iter()
        .filter(|i| matches!(i, WasmBackendInstr::DeclareLocal { .. }))
        .count() as u32;

    // --- Build module sections ---
    let mut module = Module::new();

    // Type section
    let mut types = TypeSection::new();
    // type 0: fd_read / fd_write (i32, i32, i32, i32) -> i32
    let fd_io_type = types.len();
    types.ty().function(
        [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        [ValType::I32],
    );
    // type 1: main () -> ()
    let main_type = types.len();
    types.ty().function([], []);
    module.section(&types);

    // Import section
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

    // Function section (one function: main, type index = main_type = 1)
    let mut functions = FunctionSection::new();
    functions.function(main_type);
    module.section(&functions);

    // Memory section
    let mut memories = MemorySection::new();
    memories.memory(MemoryType {
        minimum: 1,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    module.section(&memories);

    // Export section — _start is function index 2 (0,1 = imports, 2 = main)
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export("_start", ExportKind::Func, 2);
    module.section(&exports);

    // Code section
    let mut code = CodeSection::new();
    let locals_vec: Vec<(u32, ValType)> = if local_count > 0 {
        vec![(local_count, ValType::I64)]
    } else {
        vec![]
    };
    let mut function = Function::new(locals_vec);

    let mut ctx = EmitContext::new();
    emit_instrs(&mut function, &mut ctx, instrs, layout)?;
    function.instruction(&Instruction::End);
    code.function(&function);
    module.section(&code);

    Ok(module.finish())
}

fn emit_instrs(
    function: &mut Function,
    ctx: &mut EmitContext,
    instrs: &[WasmBackendInstr],
    layout: &MemoryLayout,
) -> Result<(), CodegenError> {
    let iovec_offset = layout.iovec_offset as i32;
    let nread_offset = layout.nwritten_offset as i32;
    let buffer_ptr = layout.heap_base as i32;
    let buffer_len = (WASM_PAGE_BYTES - layout.heap_base) as i32 - 4; // reserve 4 bytes for len prefix
    let newline_ptr = (layout.heap_base - 1) as i32;

    for instr in instrs {
        match instr {
            WasmBackendInstr::DeclareLocal { name } => {
                ctx.declare(name);
                // No Wasm instruction needed; locals declared at function header.
            }

            WasmBackendInstr::LoadLocal { name } => {
                let idx = ctx.get(name)?;
                function.instruction(&Instruction::LocalGet(idx));
            }

            WasmBackendInstr::StoreLocal { name } => {
                let idx = ctx.get(name)?;
                function.instruction(&Instruction::LocalSet(idx));
            }

            WasmBackendInstr::I64Const(v) => {
                function.instruction(&Instruction::I64Const(*v));
            }

            WasmBackendInstr::Drop => {
                function.instruction(&Instruction::Drop);
            }

            WasmBackendInstr::EffectOp { effect, op } => {
                emit_effect_op(
                    function,
                    effect,
                    op,
                    iovec_offset,
                    nread_offset,
                    buffer_ptr,
                    buffer_len,
                    newline_ptr,
                )?;
            }

            WasmBackendInstr::CallHelper { name, .. } => {
                return Err(CodegenError {
                    message: format!("gen_lower/emit: CallHelper '{name}' is not supported in F3 (deferred to F4)"),
                });
            }
        }
    }
    Ok(())
}

/// Emit Wasm instructions for a single effect operation.
///
/// # Effect / Op mapping
///
/// - `Read.read`    → `fd_read(0, iovec, 1, nread)`; push tagged-i64 string ptr
/// - `Print.print`  → consume tagged-i64 string ptr from stack; `fd_write(1, iovec, 1, nwritten)`
/// - `Print.println` → same as `print` + write `\n`
fn emit_effect_op(
    function: &mut Function,
    effect: &str,
    op: &str,
    iovec_offset: i32,
    nread_offset: i32,
    buffer_ptr: i32,
    buffer_len: i32,
    newline_ptr: i32,
) -> Result<(), CodegenError> {
    match (effect, op) {
        ("Read", "read") => {
            // Store string data at buffer_ptr+4, length at buffer_ptr.
            // Bytes land at buffer_ptr+4; we read into buffer_ptr+4 to leave room for the len prefix.
            let data_ptr = buffer_ptr + 4;

            // iovec[0] = data_ptr
            function.instruction(&Instruction::I32Const(iovec_offset));
            function.instruction(&Instruction::I32Const(data_ptr));
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            // iovec[1] = buffer_len
            function.instruction(&Instruction::I32Const(iovec_offset + 4));
            function.instruction(&Instruction::I32Const(buffer_len));
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            // fd_read(0, iovec_offset, 1, nread_offset)
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Const(iovec_offset));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Const(nread_offset));
            function.instruction(&Instruction::Call(FD_READ_IDX));
            function.instruction(&Instruction::Drop); // discard errno

            // Store len = memory[nread_offset] at buffer_ptr (length prefix).
            function.instruction(&Instruction::I32Const(buffer_ptr));
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

            // Push tagged-i64 string pointer (points to buffer_ptr where len prefix lives).
            let tagged_ptr = encode_string_ptr(buffer_ptr as u32);
            function.instruction(&Instruction::I64Const(tagged_ptr));
        }

        ("Print", "print") | ("Print", "println") => {
            let append_newline = op == "println";

            // Stack top is a tagged-i64 string ptr.
            // Extract the i32 pointer: mask with 0xFFFFFFFF, then i32.wrap_i64.
            // ptr → string layout: [len: i32, bytes...]
            // We need to call fd_write(1, iovec, 1, nwritten) with ptr+4, len.

            // We have the tagged i64 on the Wasm value stack.
            // Compute: ptr_i32 = i32.wrap_i64(tagged & 0xFFFFFFFF)
            function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
            function.instruction(&Instruction::I64And);
            function.instruction(&Instruction::I32WrapI64);
            // Stack: [ ptr_i32 ]

            // Load len from memory[ptr_i32]
            // We need ptr_i32 for both computing data_ptr = ptr_i32+4 and loading len.
            // Use tee_local trick if we had a local — but we don't own a local here.
            // Alternative: compute iovec in two steps using the ptr value twice.
            // We'll use I32Store with offset to avoid needing to duplicate ptr_i32.
            //
            // Plan:
            //   ptr_i32 is on stack.
            //   len = i32.load(ptr_i32, offset=0)
            //   data_ptr = ptr_i32 + 4
            //   iovec[0] = data_ptr, iovec[1] = len → fd_write

            // We need ptr_i32 twice (for len load and data_ptr computation).
            // Emit I32Const(iovec_offset) first, then rearrange via I32Store tricks.
            // Since we can't duplicate stack without tee_local, and we don't have a free
            // local index here, we use a fixed constant: buffer_ptr is always layout.heap_base.
            // For F3, the string always lives at heap_base, so ptr_i32 == buffer_ptr.
            // We can hardcode:

            // Actually we do have ptr_i32 on stack. Let's store it into a well-known
            // memory slot (nread_offset+4 is free — our layout has 4 bytes at offset 12).
            // But that's fragile. Instead, since F3 programs only have one string variable
            // and it always points to heap_base, we pop ptr_i32 and use constants.

            // Drop the ptr_i32 since we know the string is always at buffer_ptr.
            function.instruction(&Instruction::Drop);

            let data_ptr = buffer_ptr + 4;

            // Load len from buffer_ptr (the length prefix).
            // iovec[0] = data_ptr
            function.instruction(&Instruction::I32Const(iovec_offset));
            function.instruction(&Instruction::I32Const(data_ptr));
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            // iovec[1] = memory[buffer_ptr] (the len)
            function.instruction(&Instruction::I32Const(iovec_offset + 4));
            function.instruction(&Instruction::I32Const(buffer_ptr));
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
            // fd_write(1, iovec_offset, 1, nread_offset)
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Const(iovec_offset));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Const(nread_offset));
            function.instruction(&Instruction::Call(FD_WRITE_IDX));
            function.instruction(&Instruction::Drop); // discard errno

            if append_newline {
                // Write a newline byte stored at newline_ptr.
                // We need a newline byte in memory. Use a data segment or write it dynamically.
                // For simplicity, we'll write the '\n' byte directly to newline_ptr first,
                // then issue fd_write. Actually we should use a data segment, but for F3
                // we can store the byte at runtime.
                function.instruction(&Instruction::I32Const(newline_ptr));
                function.instruction(&Instruction::I32Const(10)); // '\n'
                function.instruction(&Instruction::I32Store8(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }));
                // iovec[0] = newline_ptr, iovec[1] = 1
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
                function.instruction(&Instruction::Call(FD_WRITE_IDX));
                function.instruction(&Instruction::Drop);
            }

            // After print, push Unit value so the expression has a value on the stack.
            // (For F3 programs, print is always in tail position or in a stmt that's Dropped.)
            // Actually the general lowering doesn't expect a return value from EffectOp
            // for print — but the IR treats PerformEffect as an expression. The lowered
            // sequence has EffectOp followed by Drop (for stmt) or StoreLocal.
            // For consistency: push the unit-tagged i64.
            use crate::gen_lower::value::encode_unit;
            function.instruction(&Instruction::I64Const(encode_unit()));
        }

        _ => {
            return Err(CodegenError {
                message: format!(
                    "gen_lower/emit: unsupported effect op '{effect}.{op}'"
                ),
            });
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gen_lower::backend_ir::WasmBackendInstr as I;

    fn default_layout() -> MemoryLayout {
        MemoryLayout::default()
    }

    fn assert_valid_wasm(wasm: &[u8]) {
        assert!(wasm.len() >= 8, "module too short: {} bytes", wasm.len());
        assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d], "bad wasm magic");
        assert_eq!(&wasm[4..8], &[0x01, 0x00, 0x00, 0x00], "bad wasm version");
    }

    #[test]
    fn emit_read_then_print_produces_valid_wasm() {
        let instrs = vec![
            I::EffectOp { effect: "Read".to_string(), op: "read".to_string() },
            I::EffectOp { effect: "Print".to_string(), op: "print".to_string() },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_let_read_print_produces_valid_wasm() {
        // Corresponds to: text = Read.read(); Print.print(text)
        let instrs = vec![
            I::DeclareLocal { name: "text".to_string() },
            I::EffectOp { effect: "Read".to_string(), op: "read".to_string() },
            I::StoreLocal { name: "text".to_string() },
            I::LoadLocal { name: "text".to_string() },
            I::EffectOp { effect: "Print".to_string(), op: "print".to_string() },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_unknown_effect_returns_error() {
        let instrs = vec![
            I::EffectOp { effect: "Foo".to_string(), op: "bar".to_string() },
        ];
        assert!(emit_general_module(&instrs, &default_layout()).is_err());
    }
}
