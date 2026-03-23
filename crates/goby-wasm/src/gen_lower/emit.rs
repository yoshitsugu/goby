//! `WasmBackendInstr` → Wasm bytes emission for the general lowering path.
//!
//! This module owns the concrete WASI import mapping for runtime `Read`/`Print`
//! effect operations.

use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, ImportSection, Instruction, MemArg, MemorySection, MemoryType, Module,
    TypeSection, ValType,
};

use crate::CodegenError;
use crate::gen_lower::backend_ir::{BackendIntrinsic, SplitIndexOperand, WasmBackendInstr};
use goby_core::ir::IrBinOp;

use crate::gen_lower::value::{
    TAG_BOOL, TAG_INT, TAG_LIST, TAG_STRING, encode_string_ptr, encode_unit,
};
use crate::host_runtime::{
    HOST_INTRINSIC_IMPORTS, IntrinsicExecutionBoundary, host_import_for_intrinsic,
};
use crate::layout::MemoryLayout;

const WASM_PAGE_BYTES: u32 = 65_536;

// Import function indices begin with the WASI pair and may be extended with
// `goby-wasm` owned Track E host intrinsics.
const FD_READ_IDX: u32 = 0;
const FD_WRITE_IDX: u32 = 1;
const HOST_IMPORT_BASE_IDX: u32 = 2;

// Number of I32 scratch locals used by the fused split instructions.
// scratch[0] = str_ptr, scratch[1] = str_len, scratch[2] = pos,
// scratch[3] = line_start, scratch[4] = target/remaining index
const SPLIT_FUSED_SCRATCH_I32: u32 = 5;
// Generic effects need two i32 scratch locals:
// scratch[0] = generic string ptr / read_line scan pos
// scratch[1] = read_line total len
const GENERIC_EFFECT_SCRATCH_I32: u32 = 2;
// Non-fused helper emission needs two transient i64 locals for helper arguments.
const HELPER_SCRATCH_I64: u32 = 2;
// Helper emission i32 scratch layout:
// scratch[0] = text/string/list ptr
// scratch[1] = text/string/list len
// scratch[2] = sep ptr / decoded index
// scratch[3] = sep len
// scratch[4] = scan pos
// scratch[5] = segment start
// scratch[6] = item count
// scratch[7] = auxiliary pointer
// scratch[8] = list ptr
// scratch[9] = allocation size temp
// scratch[10] = copy or match index
// scratch[11] = alloc cursor (persistent)
// scratch[12] = heap floor (persistent)
const HELPER_SCRATCH_I32: u32 = 13;
const HELPER_ALLOC_CURSOR_OFFSET: u32 = 11;
const HELPER_HEAP_FLOOR_OFFSET: u32 = 12;

#[derive(Debug, Clone)]
struct StaticStringPool {
    ptrs: HashMap<String, i32>,
    segments: Vec<(i32, Vec<u8>)>,
    bytes_used: u32,
}

impl StaticStringPool {
    /// Build a pool from a single instruction list (main only, no aux decls).
    fn build(instrs: &[WasmBackendInstr]) -> Result<Self, CodegenError> {
        Self::build_from_all(std::iter::once(instrs))
    }

    /// Build a pool from main instructions plus any number of aux decl instruction lists.
    fn build_from_all<'a>(
        all_slices: impl Iterator<Item = &'a [WasmBackendInstr]>,
    ) -> Result<Self, CodegenError> {
        let mut ptrs = HashMap::new();
        let mut segments = Vec::new();
        let mut cursor = WASM_PAGE_BYTES;

        // collect_all_instrs is defined later in this file, after all helper fns.
        // To avoid forward-reference issues, inline the recursion here.
        fn visit_instrs<'a>(instrs: &'a [WasmBackendInstr], out: &mut Vec<&'a WasmBackendInstr>) {
            for instr in instrs {
                out.push(instr);
                if let WasmBackendInstr::If {
                    then_instrs,
                    else_instrs,
                } = instr
                {
                    visit_instrs(then_instrs, out);
                    visit_instrs(else_instrs, out);
                }
            }
        }
        let mut all_instrs = Vec::new();
        for slice in all_slices {
            visit_instrs(slice, &mut all_instrs);
        }

        for instr in all_instrs {
            let WasmBackendInstr::PushStaticString { text } = instr else {
                continue;
            };
            if ptrs.contains_key(text) {
                continue;
            }

            let text_len = u32::try_from(text.len()).map_err(|_| CodegenError {
                message: "gen_lower/emit: static string is too large to encode".to_string(),
            })?;
            let blob_len = 4u32.checked_add(text_len).ok_or_else(|| CodegenError {
                message: "gen_lower/emit: static string blob size overflow".to_string(),
            })?;
            cursor = cursor.checked_sub(blob_len).ok_or_else(|| CodegenError {
                message: "gen_lower/emit: static string pool overflow".to_string(),
            })?;
            let ptr = i32::try_from(cursor).map_err(|_| CodegenError {
                message: "gen_lower/emit: static string pointer does not fit in i32".to_string(),
            })?;

            let mut blob = Vec::with_capacity(blob_len as usize);
            blob.extend_from_slice(
                &i32::try_from(text_len)
                    .map_err(|_| CodegenError {
                        message: "gen_lower/emit: static string length does not fit in i32"
                            .to_string(),
                    })?
                    .to_le_bytes(),
            );
            blob.extend_from_slice(text.as_bytes());

            ptrs.insert(text.clone(), ptr);
            segments.push((ptr, blob));
        }

        Ok(Self {
            ptrs,
            segments,
            bytes_used: WASM_PAGE_BYTES - cursor,
        })
    }

    fn ptr(&self, text: &str) -> Result<i32, CodegenError> {
        self.ptrs.get(text).copied().ok_or_else(|| CodegenError {
            message: format!("gen_lower/emit: missing static string pool entry for '{text}'"),
        })
    }

    fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }
}

/// Tracks compilation state during emission.
struct EmitContext {
    /// Map from local name to Wasm local index.
    locals: HashMap<String, u32>,
    /// Count of allocated locals.
    next_local: u32,
    /// Map from declaration name to Wasm function index (WB-2A).
    /// Built by `emit_general_module` before emitting any function body.
    decl_func_indices: HashMap<String, u32>,
}

impl EmitContext {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            next_local: 0,
            decl_func_indices: HashMap::new(),
        }
    }

    fn with_decl_indices(decl_func_indices: HashMap<String, u32>) -> Self {
        Self {
            locals: HashMap::new(),
            next_local: 0,
            decl_func_indices,
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

    fn decl_func_idx(&self, decl_name: &str) -> Result<u32, CodegenError> {
        self.decl_func_indices
            .get(decl_name)
            .copied()
            .ok_or_else(|| CodegenError {
                message: format!("gen_lower/emit: unknown declaration '{decl_name}' in DeclCall"),
            })
    }

    /// Reset local state between functions (keep decl_func_indices shared).
    fn reset_locals(&mut self) {
        self.locals.clear();
        self.next_local = 0;
    }
}

#[derive(Clone, Copy)]
struct HelperEmitState {
    i64_base: u32,
    i32_base: u32,
    alloc_cursor_local: u32,
    heap_floor_local: u32,
}

fn align_up_i32(value: i32, align: i32) -> i32 {
    debug_assert!((align as u32).is_power_of_two());
    (value + (align - 1)) & !(align - 1)
}

fn align_down_i32(value: i32, align: i32) -> i32 {
    debug_assert!((align as u32).is_power_of_two());
    value & !(align - 1)
}

/// Collect all instructions in a potentially-nested instruction list into a flat vec.
///
/// `WasmBackendInstr::If` contains nested `then_instrs` / `else_instrs`. All pre-scan
/// functions (scratch counts, static string pool, etc.) must recurse into these to avoid
/// emitting locals or data segments that reference missing declarations.
fn collect_all_instrs(instrs: &[WasmBackendInstr]) -> Vec<&WasmBackendInstr> {
    let mut result = Vec::new();
    for instr in instrs {
        result.push(instr);
        if let WasmBackendInstr::If {
            then_instrs,
            else_instrs,
        } = instr
        {
            result.extend(collect_all_instrs(then_instrs));
            result.extend(collect_all_instrs(else_instrs));
        }
    }
    result
}

fn required_i32_scratch_count(instrs: &[WasmBackendInstr]) -> u32 {
    let all = collect_all_instrs(instrs);
    let split = all.iter().any(|i| {
        matches!(
            i,
            WasmBackendInstr::SplitEachPrint { .. } | WasmBackendInstr::SplitGetPrint { .. }
        )
    });
    let generic_effects = all.iter().any(|i| {
        matches!(
            i,
            WasmBackendInstr::EffectOp { effect, op }
                if (effect == "Print" && (op == "print" || op == "println"))
                    || (effect == "Read" && op == "read_line")
        )
    });

    let split_count = if split { SPLIT_FUSED_SCRATCH_I32 } else { 0 };
    let generic_count = if generic_effects {
        GENERIC_EFFECT_SCRATCH_I32
    } else {
        0
    };
    let helper_count = if all
        .iter()
        .any(|i| matches!(i, WasmBackendInstr::Intrinsic { .. }))
    {
        HELPER_SCRATCH_I32
    } else {
        0
    };
    split_count.max(generic_count).max(helper_count)
}

fn required_i64_scratch_count(instrs: &[WasmBackendInstr]) -> u32 {
    use goby_core::ir::IrBinOp;
    let all = collect_all_instrs(instrs);
    let needs_scratch = all.iter().any(|i| match i {
        WasmBackendInstr::Intrinsic { .. } => true,
        WasmBackendInstr::BinOp { op } => {
            matches!(op, IrBinOp::Mul | IrBinOp::Div | IrBinOp::Mod)
        }
        _ => false,
    });
    if needs_scratch { HELPER_SCRATCH_I64 } else { 0 }
}

/// Returns true if any `SplitEachPrint` with println is present (needs newline data segment).
fn needs_newline_data(instrs: &[WasmBackendInstr]) -> bool {
    let all = collect_all_instrs(instrs);
    all.iter().any(|i| {
        matches!(i, WasmBackendInstr::SplitEachPrint { op, .. } if op == "println")
            || matches!(i, WasmBackendInstr::SplitGetPrint { op, .. } if op == "println")
    })
}

/// Emit a complete Wasm module from a flat list of `WasmBackendInstr`.
///
/// The module imports `fd_read` and `fd_write` from `wasi_snapshot_preview1`,
/// exports `memory` and `_start`, and contains one function (`main`).
/// A lowered auxiliary declaration (non-`main` top-level function) ready for emission.
///
/// `param_names` holds the ordered parameter names as they appear in the IR declaration.
/// Each parameter is a tagged i64 and occupies a Wasm local at indices 0..param_names.len()-1.
/// `instrs` is the lowered body; it may contain `DeclCall` which will be resolved
/// against the module-wide `decl_name → func_idx` table during `emit_general_module`.
#[derive(Debug, Clone)]
pub(crate) struct AuxDecl {
    pub(crate) decl_name: String,
    pub(crate) param_names: Vec<String>,
    pub(crate) instrs: Vec<WasmBackendInstr>,
}

pub(crate) fn supports_instrs(instrs: &[WasmBackendInstr]) -> bool {
    collect_all_instrs(instrs).iter().all(|instr| match instr {
        WasmBackendInstr::Intrinsic { intrinsic } => match intrinsic.execution_boundary() {
            IntrinsicExecutionBoundary::HostImport | IntrinsicExecutionBoundary::InWasm => true,
        },
        // DeclCall is fully supported: emit_general_module_with_aux builds the
        // decl_name → func_idx table and threads it through EmitContext.
        WasmBackendInstr::DeclCall { .. } => true,
        _ => true,
    })
}

pub(crate) fn emit_general_module(
    instrs: &[WasmBackendInstr],
    layout: &MemoryLayout,
) -> Result<Vec<u8>, CodegenError> {
    emit_general_module_with_aux(instrs, &[], layout)
}

/// Emit a Wasm module containing `main` (`instrs`) and zero or more auxiliary declarations.
///
/// `aux_decls` are compiled as Wasm functions placed *after* `main` in the function section
/// so that the `_start` export index (`main`) stays stable regardless of how many aux decls
/// are added.  The `decl_name → func_idx` table is built here and threaded through
/// `emit_instrs` via `EmitContext`.
pub(crate) fn emit_general_module_with_aux(
    instrs: &[WasmBackendInstr],
    aux_decls: &[AuxDecl],
    layout: &MemoryLayout,
) -> Result<Vec<u8>, CodegenError> {
    let static_strings = StaticStringPool::build_from_all(
        std::iter::once(instrs).chain(aux_decls.iter().map(|d| d.instrs.as_slice())),
    )?;
    if layout.heap_base + static_strings.bytes_used + 4 > WASM_PAGE_BYTES {
        return Err(CodegenError {
            message: "gen_lower/emit: static string pool leaves no room for runtime stdin buffer"
                .to_string(),
        });
    }

    // --- Pre-scan for main: count declared I64 locals and detect I32 scratch needs ---
    // Use collect_all_instrs to recurse into nested If branches.
    let all_main_instrs = collect_all_instrs(instrs);
    let main_named_i64_count = all_main_instrs
        .iter()
        .filter(|i| matches!(i, WasmBackendInstr::DeclareLocal { .. }))
        .count() as u32;
    let main_helper_i64_scratch_count = required_i64_scratch_count(instrs);
    let main_i64_count = main_named_i64_count + main_helper_i64_scratch_count;
    let main_i32_scratch_count = required_i32_scratch_count(instrs);
    let main_i32_base = main_i64_count;

    // Check whether any instruction list (main or aux) uses host intrinsics.
    let all_slices_iter =
        || std::iter::once(instrs).chain(aux_decls.iter().map(|d| d.instrs.as_slice()));
    let uses_host_intrinsics = all_slices_iter().any(|slice| {
        collect_all_instrs(slice).iter().any(|instr| {
            matches!(
                instr,
                WasmBackendInstr::Intrinsic { intrinsic }
                    if intrinsic.execution_boundary() == IntrinsicExecutionBoundary::HostImport
            )
        })
    });
    let used_host_imports: Vec<_> = if uses_host_intrinsics {
        HOST_INTRINSIC_IMPORTS.to_vec()
    } else {
        Vec::new()
    };

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
    let host_intrinsic_type_indices: Vec<u32> = used_host_imports
        .iter()
        .map(|import| {
            let idx = types.len();
            types.ty().function(
                import.params().iter().copied(),
                import.results().iter().copied(),
            );
            idx
        })
        .collect();
    // type N: main () -> ()
    let main_type = types.len();
    types.ty().function([], []);

    // types for aux decls: each takes (param_names.len() × i64) → i64
    let aux_type_indices: Vec<u32> = aux_decls
        .iter()
        .map(|decl| {
            let idx = types.len();
            let params: Vec<ValType> = vec![ValType::I64; decl.param_names.len()];
            types.ty().function(params, [ValType::I64]);
            idx
        })
        .collect();
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
    for (import, type_idx) in used_host_imports
        .iter()
        .zip(host_intrinsic_type_indices.iter().copied())
    {
        imports.import(
            import.module(),
            import.name(),
            EntityType::Function(type_idx),
        );
    }
    module.section(&imports);

    // Function section: main first, then aux decls.
    // Keeping main first means _start index = HOST_IMPORT_BASE_IDX + used_host_imports.len()
    // regardless of how many aux decls are added.
    let main_func_idx = HOST_IMPORT_BASE_IDX + used_host_imports.len() as u32;
    let mut functions = FunctionSection::new();
    functions.function(main_type);
    for aux_type_idx in &aux_type_indices {
        functions.function(*aux_type_idx);
    }
    module.section(&functions);

    // Build decl_name → func_idx table for DeclCall resolution.
    // aux decls start at main_func_idx + 1.
    let decl_func_indices: HashMap<String, u32> = aux_decls
        .iter()
        .enumerate()
        .map(|(i, decl)| (decl.decl_name.clone(), main_func_idx + 1 + i as u32))
        .collect();

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

    // Export section — `_start` is always main (index stable across aux additions).
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export("_start", ExportKind::Func, main_func_idx);
    module.section(&exports);

    // Code section — main function first.
    let mut code = CodeSection::new();
    {
        let mut locals_vec: Vec<(u32, ValType)> = Vec::new();
        if main_i64_count > 0 {
            locals_vec.push((main_i64_count, ValType::I64));
        }
        if main_i32_scratch_count > 0 {
            locals_vec.push((main_i32_scratch_count, ValType::I32));
        }
        let mut function = Function::new(locals_vec);
        let mut ctx = EmitContext::with_decl_indices(decl_func_indices.clone());
        emit_instrs(
            &mut function,
            &mut ctx,
            instrs,
            layout,
            main_named_i64_count,
            main_helper_i64_scratch_count,
            main_i32_base,
            &static_strings,
        )?;
        function.instruction(&Instruction::End);
        code.function(&function);
    }

    // Aux decl functions (after main).
    for decl in aux_decls {
        let aux_all = collect_all_instrs(&decl.instrs);
        let aux_named_i64_count = aux_all
            .iter()
            .filter(|i| matches!(i, WasmBackendInstr::DeclareLocal { .. }))
            .count() as u32;
        let aux_helper_i64_scratch_count = required_i64_scratch_count(&decl.instrs);
        // Aux decl params are Wasm function parameters (i64 each), not DeclareLocal slots.
        // In Wasm, function params are locals[0..param_names.len()-1]; body locals follow after.
        let aux_param_count = decl.param_names.len() as u32;
        let aux_i64_count = aux_named_i64_count + aux_helper_i64_scratch_count;
        let aux_i32_scratch_count = required_i32_scratch_count(&decl.instrs);
        let aux_i32_base = aux_param_count + aux_i64_count;

        let mut locals_vec: Vec<(u32, ValType)> = Vec::new();
        if aux_i64_count > 0 {
            locals_vec.push((aux_i64_count, ValType::I64));
        }
        if aux_i32_scratch_count > 0 {
            locals_vec.push((aux_i32_scratch_count, ValType::I32));
        }
        let mut function = Function::new(locals_vec);

        // Params occupy local indices 0..param_names.len()-1 in Wasm; body locals follow.
        // Pre-register param names in EmitContext so LoadLocal/StoreLocal resolve correctly.
        let mut ctx = EmitContext::with_decl_indices(decl_func_indices.clone());
        for param_name in &decl.param_names {
            // Wasm assigns local indices for params in declaration order before any body locals.
            // We mirror that: declare each param name with an explicit local index.
            ctx.locals.insert(param_name.clone(), ctx.next_local);
            ctx.next_local += 1;
        }
        // After params, body locals start from aux_param_count.
        // named_i64_count passed to emit_instrs counts only DeclareLocal in body.
        emit_instrs(
            &mut function,
            &mut ctx,
            &decl.instrs,
            layout,
            aux_named_i64_count,
            aux_helper_i64_scratch_count,
            aux_i32_base,
            &static_strings,
        )?;
        function.instruction(&Instruction::End);
        code.function(&function);
    }
    module.section(&code);

    // Data section: newline byte for println (if needed).
    let needs_newline =
        needs_newline_data(instrs) || aux_decls.iter().any(|d| needs_newline_data(&d.instrs));
    if needs_newline || !static_strings.is_empty() {
        let newline_ptr = (layout.heap_base - 1) as i32;
        let mut data = DataSection::new();
        if needs_newline {
            data.active(0, &ConstExpr::i32_const(newline_ptr), b"\n".to_vec());
        }
        for (ptr, bytes) in &static_strings.segments {
            data.active(0, &ConstExpr::i32_const(*ptr), bytes.clone());
        }
        module.section(&data);
    }

    Ok(module.finish())
}

fn emit_instrs(
    function: &mut Function,
    ctx: &mut EmitContext,
    instrs: &[WasmBackendInstr],
    layout: &MemoryLayout,
    named_i64_count: u32,
    helper_i64_scratch_count: u32,
    i32_base: u32,
    static_strings: &StaticStringPool,
) -> Result<(), CodegenError> {
    let iovec_offset = layout.iovec_offset as i32;
    let nread_offset = layout.nwritten_offset as i32;
    let buffer_ptr = layout.heap_base as i32;
    let buffer_len = (WASM_PAGE_BYTES - layout.heap_base - static_strings.bytes_used) as i32 - 4; // reserve 4 bytes for len prefix
    let newline_ptr = (layout.heap_base - 1) as i32;
    let helper_i64_base = named_i64_count;
    // `helper_state` is constructed only when Intrinsic instructions are present.
    // Intrinsics require HELPER_SCRATCH_I32 i32 locals (alloc cursor / heap floor pattern).
    // BinOp Mul/Div/Mod need only i64 scratch; those are provided via `helper_i64_base` directly
    // in `emit_bin_op`, without requiring the full HelperEmitState.
    let has_intrinsic = instrs
        .iter()
        .any(|i| matches!(i, WasmBackendInstr::Intrinsic { .. }));
    let helper_state = if has_intrinsic && helper_i64_scratch_count > 0 {
        let alloc_cursor_local = i32_base + HELPER_ALLOC_CURSOR_OFFSET;
        let heap_floor_local = i32_base + HELPER_HEAP_FLOOR_OFFSET;
        let initial_cursor = align_down_i32(
            i32::try_from(WASM_PAGE_BYTES - static_strings.bytes_used).map_err(|_| {
                CodegenError {
                    message: "gen_lower/emit: helper allocation cursor does not fit in i32"
                        .to_string(),
                }
            })?,
            8,
        );
        let initial_floor = align_up_i32(buffer_ptr + 4, 8);
        function.instruction(&Instruction::I32Const(initial_cursor));
        function.instruction(&Instruction::LocalSet(alloc_cursor_local));
        function.instruction(&Instruction::I32Const(initial_floor));
        function.instruction(&Instruction::LocalSet(heap_floor_local));
        Some(HelperEmitState {
            i64_base: helper_i64_base,
            i32_base,
            alloc_cursor_local,
            heap_floor_local,
        })
    } else {
        None
    };

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

            WasmBackendInstr::PushStaticString { text } => {
                let ptr = static_strings.ptr(text)?;
                function.instruction(&Instruction::I64Const(encode_string_ptr(ptr as u32)));
            }

            WasmBackendInstr::Drop => {
                function.instruction(&Instruction::Drop);
            }

            WasmBackendInstr::BinOp { op } => {
                // Pass the i64 scratch base (starts at named_i64_count) when available.
                let i64_scratch_base = if helper_i64_scratch_count > 0 {
                    Some(helper_i64_base)
                } else {
                    None
                };
                emit_bin_op(function, op, i64_scratch_base)?;
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
                    i32_base,
                    helper_state.as_ref(),
                )?;
            }

            WasmBackendInstr::Intrinsic { intrinsic } => {
                emit_helper_call(function, ctx, *intrinsic, i32_base, helper_state.as_ref())?;
            }

            WasmBackendInstr::SplitEachPrint {
                text_local,
                sep_bytes,
                effect,
                op,
            } => {
                if sep_bytes.len() != 1 {
                    return Err(CodegenError {
                        message: format!(
                            "gen_lower/emit: SplitEachPrint requires 1-byte separator, got {} bytes",
                            sep_bytes.len()
                        ),
                    });
                }
                if effect != "Print" || (op != "print" && op != "println") {
                    return Err(CodegenError {
                        message: format!(
                            "gen_lower/emit: SplitEachPrint unsupported callback '{effect}.{op}'"
                        ),
                    });
                }
                let text_idx = ctx.get(text_local)?;
                emit_split_each_print(
                    function,
                    text_idx,
                    sep_bytes[0],
                    op == "println",
                    i32_base,
                    iovec_offset,
                    nread_offset,
                    newline_ptr,
                );
            }

            WasmBackendInstr::SplitGetPrint {
                text_local,
                sep_bytes,
                index,
                op,
            } => {
                if sep_bytes.len() != 1 {
                    return Err(CodegenError {
                        message: format!(
                            "gen_lower/emit: SplitGetPrint requires 1-byte separator, got {} bytes",
                            sep_bytes.len()
                        ),
                    });
                }
                if op != "print" && op != "println" {
                    return Err(CodegenError {
                        message: format!("gen_lower/emit: SplitGetPrint unsupported op '{op}'"),
                    });
                }
                let text_idx = ctx.get(text_local)?;
                emit_split_get_print(
                    function,
                    ctx,
                    text_idx,
                    sep_bytes[0],
                    index,
                    op == "println",
                    i32_base,
                    iovec_offset,
                    nread_offset,
                    newline_ptr,
                )?;
            }
            WasmBackendInstr::If {
                then_instrs,
                else_instrs,
            } => {
                // Convert tagged Bool on stack to i32: extract payload bit 0.
                function.instruction(&Instruction::I64Const(1));
                function.instruction(&Instruction::I64And);
                function.instruction(&Instruction::I32WrapI64);
                // Wasm if/else/end block: both branches produce one tagged i64.
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                    wasm_encoder::ValType::I64,
                )));
                emit_instrs(
                    function,
                    ctx,
                    then_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                )?;
                function.instruction(&Instruction::Else);
                emit_instrs(
                    function,
                    ctx,
                    else_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                )?;
                function.instruction(&Instruction::End);
            }

            WasmBackendInstr::DeclCall { decl_name } => {
                let func_idx = ctx.decl_func_idx(decl_name)?;
                function.instruction(&Instruction::Call(func_idx));
            }
        }
    }
    Ok(())
}

fn emit_update_heap_floor_from_buffer(
    function: &mut Function,
    heap_floor_local: u32,
    buffer_ptr: i32,
    nread_offset: i32,
) {
    function.instruction(&Instruction::I32Const(nread_offset));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I32Const(buffer_ptr + 4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(7));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(!7));
    function.instruction(&Instruction::I32And);
    function.instruction(&Instruction::LocalSet(heap_floor_local));
}

fn emit_update_heap_floor_from_local_len(
    function: &mut Function,
    heap_floor_local: u32,
    buffer_ptr: i32,
    len_local: u32,
) {
    function.instruction(&Instruction::LocalGet(len_local));
    function.instruction(&Instruction::I32Const(buffer_ptr + 4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(7));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(!7));
    function.instruction(&Instruction::I32And);
    function.instruction(&Instruction::LocalSet(heap_floor_local));
}

fn emit_helper_call(
    function: &mut Function,
    ctx: &EmitContext,
    intrinsic: BackendIntrinsic,
    i32_base: u32,
    helper_state: Option<&HelperEmitState>,
) -> Result<(), CodegenError> {
    let helper_state = helper_state.ok_or_else(|| CodegenError {
        message: format!("gen_lower/emit: intrinsic '{intrinsic:?}' requires helper scratch state"),
    })?;
    match intrinsic {
        BackendIntrinsic::StringSplit => emit_string_split_helper(function, helper_state),
        BackendIntrinsic::ListGet => emit_list_get_helper(function, helper_state),
        BackendIntrinsic::StringLength => emit_string_length_helper(function, helper_state),
        BackendIntrinsic::StringEachGraphemeCount
        | BackendIntrinsic::StringEachGraphemeState
        | BackendIntrinsic::StringConcat => {
            let host_import = host_import_for_intrinsic(intrinsic).ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: missing host import mapping for intrinsic '{intrinsic:?}'"
                ),
            })?;
            let host_offset = HOST_INTRINSIC_IMPORTS
                .iter()
                .position(|candidate| candidate == &host_import)
                .ok_or_else(|| CodegenError {
                    message: format!(
                        "gen_lower/emit: missing host import index for '{host_import:?}'"
                    ),
                })? as u32;
            function.instruction(&Instruction::Call(HOST_IMPORT_BASE_IDX + host_offset));
            Ok(())
        }
        BackendIntrinsic::ListPushString => emit_list_push_string_helper(function, helper_state),
    }?;
    let _ = (ctx, i32_base);
    Ok(())
}

fn emit_decode_string_ptr(
    function: &mut Function,
    helper_state: &HelperEmitState,
    source_i64_local: u32,
    ptr_local: u32,
    len_local: u32,
) {
    function.instruction(&Instruction::LocalGet(source_i64_local));
    function.instruction(&Instruction::I64Const((TAG_STRING as i64) << 60));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const((TAG_STRING as i64) << 60));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(source_i64_local));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(ptr_local));

    function.instruction(&Instruction::LocalGet(ptr_local));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(len_local));

    let _ = helper_state;
}

fn emit_push_tagged_ptr(function: &mut Function, ptr_local: u32, tag: u8) {
    function.instruction(&Instruction::LocalGet(ptr_local));
    function.instruction(&Instruction::I64ExtendI32U);
    function.instruction(&Instruction::I64Const((tag as i64) << 60));
    function.instruction(&Instruction::I64Or);
}

fn emit_alloc_from_top(
    function: &mut Function,
    helper_state: &HelperEmitState,
    size_local: u32,
    result_local: u32,
) {
    function.instruction(&Instruction::LocalGet(helper_state.alloc_cursor_local));
    function.instruction(&Instruction::LocalGet(size_local));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::I32Const(!7));
    function.instruction(&Instruction::I32And);
    function.instruction(&Instruction::LocalTee(result_local));
    function.instruction(&Instruction::LocalGet(helper_state.heap_floor_local));
    function.instruction(&Instruction::I32LtU);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(result_local));
    function.instruction(&Instruction::LocalSet(helper_state.alloc_cursor_local));
}

fn emit_store_list_item(
    function: &mut Function,
    list_ptr_local: u32,
    item_count_local: u32,
    item_ptr_local: u32,
) {
    function.instruction(&Instruction::LocalGet(list_ptr_local));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(item_count_local));
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    emit_push_tagged_ptr(function, item_ptr_local, TAG_STRING);
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
}

fn emit_copy_string_slice_to_alloc(
    function: &mut Function,
    source_ptr_local: u32,
    start_local: u32,
    end_local: u32,
    dest_ptr_local: u32,
    copy_idx_local: u32,
) {
    function.instruction(&Instruction::LocalGet(end_local));
    function.instruction(&Instruction::LocalGet(start_local));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::LocalSet(copy_idx_local));

    function.instruction(&Instruction::LocalGet(dest_ptr_local));
    function.instruction(&Instruction::LocalGet(copy_idx_local));
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));

    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(copy_idx_local));

    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(copy_idx_local));
    function.instruction(&Instruction::LocalGet(end_local));
    function.instruction(&Instruction::LocalGet(start_local));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::LocalGet(dest_ptr_local));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(copy_idx_local));
    function.instruction(&Instruction::I32Add);

    function.instruction(&Instruction::LocalGet(source_ptr_local));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(start_local));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(copy_idx_local));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load8U(MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I32Store8(MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));

    function.instruction(&Instruction::LocalGet(copy_idx_local));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(copy_idx_local));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
}

fn emit_string_split_helper(
    function: &mut Function,
    helper_state: &HelperEmitState,
) -> Result<(), CodegenError> {
    let text_i64 = helper_state.i64_base;
    let sep_i64 = helper_state.i64_base + 1;
    let s_text_ptr = helper_state.i32_base;
    let s_text_len = helper_state.i32_base + 1;
    let s_sep_ptr = helper_state.i32_base + 2;
    let s_sep_len = helper_state.i32_base + 3;
    let s_pos = helper_state.i32_base + 4;
    let s_start = helper_state.i32_base + 5;
    let s_item_count = helper_state.i32_base + 6;
    let s_aux_ptr = helper_state.i32_base + 7;
    let s_list_ptr = helper_state.i32_base + 8;
    let s_alloc_size = helper_state.i32_base + 9;
    let s_iter = helper_state.i32_base + 10;

    function.instruction(&Instruction::LocalSet(sep_i64));
    function.instruction(&Instruction::LocalSet(text_i64));

    emit_decode_string_ptr(function, helper_state, text_i64, s_text_ptr, s_text_len);
    emit_decode_string_ptr(function, helper_state, sep_i64, s_sep_ptr, s_sep_len);

    function.instruction(&Instruction::LocalGet(s_sep_len));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_text_len));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_list_ptr);

    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_item_count));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_start));

    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_text_len));
    function.instruction(&Instruction::I32GtU);
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_iter));

    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_sep_len));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_text_len));
    function.instruction(&Instruction::I32LeU);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::LocalGet(s_sep_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Br(2));
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_text_ptr));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load8U(MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalGet(s_sep_ptr));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load8U(MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I32Ne);
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_start));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_aux_ptr);
    emit_copy_string_slice_to_alloc(function, s_text_ptr, s_start, s_pos, s_aux_ptr, s_iter);
    emit_store_list_item(function, s_list_ptr, s_item_count, s_aux_ptr);
    function.instruction(&Instruction::LocalGet(s_item_count));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_item_count));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_sep_len));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalSet(s_start));
    function.instruction(&Instruction::Br(1));
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_text_len));
    function.instruction(&Instruction::LocalGet(s_start));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_aux_ptr);
    emit_copy_string_slice_to_alloc(function, s_text_ptr, s_start, s_text_len, s_aux_ptr, s_iter);
    emit_store_list_item(function, s_list_ptr, s_item_count, s_aux_ptr);
    function.instruction(&Instruction::LocalGet(s_item_count));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_item_count));

    function.instruction(&Instruction::LocalGet(s_list_ptr));
    function.instruction(&Instruction::LocalGet(s_item_count));
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    emit_push_tagged_ptr(function, s_list_ptr, TAG_LIST);
    Ok(())
}

fn emit_list_get_helper(
    function: &mut Function,
    helper_state: &HelperEmitState,
) -> Result<(), CodegenError> {
    let list_i64 = helper_state.i64_base;
    let index_i64 = helper_state.i64_base + 1;
    let s_list_ptr = helper_state.i32_base;
    let s_list_len = helper_state.i32_base + 1;
    let s_index = helper_state.i32_base + 2;

    function.instruction(&Instruction::LocalSet(index_i64));
    function.instruction(&Instruction::LocalSet(list_i64));

    function.instruction(&Instruction::LocalGet(list_i64));
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(index_i64));
    function.instruction(&Instruction::I64Const((TAG_INT as i64) << 60));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const((TAG_INT as i64) << 60));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(index_i64));
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64Shl);
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64ShrS);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(s_index));

    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32LtS);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(list_i64));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(s_list_ptr));

    function.instruction(&Instruction::LocalGet(s_list_ptr));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_list_len));

    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_list_ptr));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    Ok(())
}

fn emit_string_length_helper(
    function: &mut Function,
    helper_state: &HelperEmitState,
) -> Result<(), CodegenError> {
    let string_i64 = helper_state.i64_base;
    let s_ptr = helper_state.i32_base;
    let s_len = helper_state.i32_base + 1;
    function.instruction(&Instruction::LocalSet(string_i64));
    emit_decode_string_ptr(function, helper_state, string_i64, s_ptr, s_len);
    function.instruction(&Instruction::LocalGet(s_len));
    function.instruction(&Instruction::I64ExtendI32S);
    function.instruction(&Instruction::I64Const((TAG_INT as i64) << 60));
    function.instruction(&Instruction::I64Or);
    Ok(())
}

fn emit_list_push_string_helper(
    function: &mut Function,
    helper_state: &HelperEmitState,
) -> Result<(), CodegenError> {
    let list_i64 = helper_state.i64_base;
    let string_i64 = helper_state.i64_base + 1;
    let s_list_ptr = helper_state.i32_base;
    let s_list_len = helper_state.i32_base + 1;
    let s_string_ptr = helper_state.i32_base + 2;
    let s_string_len = helper_state.i32_base + 3;
    let s_new_list_ptr = helper_state.i32_base + 7;
    let s_alloc_size = helper_state.i32_base + 9;
    let s_iter = helper_state.i32_base + 10;

    function.instruction(&Instruction::LocalSet(string_i64));
    function.instruction(&Instruction::LocalSet(list_i64));

    function.instruction(&Instruction::LocalGet(list_i64));
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    emit_decode_string_ptr(
        function,
        helper_state,
        string_i64,
        s_string_ptr,
        s_string_len,
    );

    function.instruction(&Instruction::LocalGet(list_i64));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(s_list_ptr));

    function.instruction(&Instruction::LocalGet(s_list_ptr));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_list_len));

    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_new_list_ptr);

    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_iter));

    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::LocalGet(s_new_list_ptr));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);

    function.instruction(&Instruction::LocalGet(s_list_ptr));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));

    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_new_list_ptr));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    emit_push_tagged_ptr(function, s_string_ptr, TAG_STRING);
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));

    function.instruction(&Instruction::LocalGet(s_new_list_ptr));
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));

    emit_push_tagged_ptr(function, s_new_list_ptr, TAG_LIST);
    Ok(())
}

/// Emit Wasm instructions for `WasmBackendInstr::BinOp`.
///
/// Both operands are expected on the stack as tagged i64 values (left deeper, right on top).
/// The result is a single tagged i64 pushed onto the stack.
///
/// # Add / Sub
/// `(left_tagged ± right_tagged) & PAYLOAD_MASK | TAG_INT_SHIFT` is correct because any carry
/// into the tag region (bits 60–63) is removed by the PAYLOAD_MASK before retagging.
///
/// # Mul / Div / Mod
/// The tag bits corrupt the product/quotient, so both operands must be sign-extended to plain
/// i64 before the operation. Two scratch i64 locals (`i64_scratch_base` and `+1`) are used.
/// `Mod` uses `i64.rem_s` (truncated division, same as Rust `%`).
///
/// # Eq / Lt / Gt / Le / Ge
/// Both tagged Int values have the same upper 4 bits (TAG_INT = 0x1), so the signed 64-bit
/// comparison of the tagged words equals the signed comparison of the 60-bit payloads.
/// Eq is also correct for Bool values (same tag). The other comparisons are **only** correct
/// for Int operands; passing Bool values gives an unspecified but deterministic result
/// (Bool values have TAG_BOOL=0x2, which is larger than any Int's upper 4 bits).
///
/// # And
/// Bool: `TAG_BOOL<<60 | payload`. `i64.and` on two tagged Bools gives `TAG_BOOL<<60 | (a & b)`
/// because `TAG_BOOL & TAG_BOOL == TAG_BOOL` (0x2 & 0x2 = 0x2). No retagging needed.
///
/// `i64_scratch_base` is the index of the first scratch i64 local (allocated when
/// `required_i64_scratch_count > 0`). Mul/Div/Mod require two consecutive scratch i64 locals.
fn emit_bin_op(
    function: &mut Function,
    op: &IrBinOp,
    i64_scratch_base: Option<u32>,
) -> Result<(), CodegenError> {
    const PAYLOAD_MASK: i64 = (1i64 << 60) - 1;
    const TAG_INT_SHIFT: i64 = (TAG_INT as i64) << 60;
    const TAG_BOOL_SHIFT: i64 = (TAG_BOOL as i64) << 60;

    // Helper: retag an i64 arithmetic result as Int (mask + tag).
    macro_rules! retag_int {
        ($f:expr) => {
            $f.instruction(&Instruction::I64Const(PAYLOAD_MASK));
            $f.instruction(&Instruction::I64And);
            $f.instruction(&Instruction::I64Const(TAG_INT_SHIFT));
            $f.instruction(&Instruction::I64Or);
        };
    }

    // Helper: convert an i32 comparison result (0 or 1) to a tagged Bool i64.
    macro_rules! bool_from_i32 {
        ($f:expr) => {
            $f.instruction(&Instruction::I64ExtendI32U);
            $f.instruction(&Instruction::I64Const(TAG_BOOL_SHIFT));
            $f.instruction(&Instruction::I64Or);
        };
    }

    // For Mul/Div/Mod, we must untag both operands before operating because
    // tag bits corrupt the product/quotient. We use two scratch i64 locals:
    //   scratch[0] = sign-extended right operand
    //   scratch[1] = sign-extended left operand
    // Both are stashed, sign-extended, then brought back via LocalGet.
    macro_rules! require_scratch {
        ($base:expr, $op_name:literal) => {
            match $base {
                Some(base) => (base, base + 1),
                None => {
                    return Err(CodegenError {
                        message: format!("BinOp::{} requires helper scratch i64 locals", $op_name),
                    });
                }
            }
        };
    }

    match op {
        IrBinOp::Add => {
            // (left_tagged + right_tagged) may carry into the tag region (bits 60-63),
            // but PAYLOAD_MASK removes those bits before TAG_INT_SHIFT is OR-ed in.
            // Modular 60-bit arithmetic is correct.
            function.instruction(&Instruction::I64Add);
            retag_int!(function);
            Ok(())
        }
        IrBinOp::Sub => {
            // (left_tagged - right_tagged): tag bits cancel (TAG - TAG = 0) in bits 60-63,
            // but borrow could affect them; PAYLOAD_MASK removes tag-region bits before retagging.
            function.instruction(&Instruction::I64Sub);
            retag_int!(function);
            Ok(())
        }
        IrBinOp::Mul => {
            // Tag bits corrupt the product — must sign-extend both operands first.
            // Stack before: [..., left_tagged, right_tagged]  (right is on top)
            // Sign-extension of 60-bit payload: local.set s; local.get s; i64.const 4;
            //   i64.shl; i64.const 4; i64.shr_s
            let (scratch0, scratch1) = require_scratch!(i64_scratch_base, "Mul");
            // Stash right_tagged, sign-extend → scratch0.
            function.instruction(&Instruction::LocalSet(scratch0));
            function.instruction(&Instruction::LocalGet(scratch0));
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64Shl);
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64ShrS);
            function.instruction(&Instruction::LocalSet(scratch0));
            // Stash left_tagged, sign-extend → scratch1.
            function.instruction(&Instruction::LocalSet(scratch1));
            function.instruction(&Instruction::LocalGet(scratch1));
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64Shl);
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64ShrS);
            function.instruction(&Instruction::LocalSet(scratch1));
            // Compute left_sext * right_sext and retag.
            function.instruction(&Instruction::LocalGet(scratch1));
            function.instruction(&Instruction::LocalGet(scratch0));
            function.instruction(&Instruction::I64Mul);
            retag_int!(function);
            Ok(())
        }
        IrBinOp::Div => {
            let (scratch0, scratch1) = require_scratch!(i64_scratch_base, "Div");
            function.instruction(&Instruction::LocalSet(scratch0));
            function.instruction(&Instruction::LocalGet(scratch0));
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64Shl);
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64ShrS);
            function.instruction(&Instruction::LocalSet(scratch0));
            function.instruction(&Instruction::LocalSet(scratch1));
            function.instruction(&Instruction::LocalGet(scratch1));
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64Shl);
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64ShrS);
            function.instruction(&Instruction::LocalSet(scratch1));
            function.instruction(&Instruction::LocalGet(scratch1));
            function.instruction(&Instruction::LocalGet(scratch0));
            function.instruction(&Instruction::I64DivS);
            retag_int!(function);
            Ok(())
        }
        IrBinOp::Mod => {
            // i64.rem_s follows truncated division semantics (same as Rust %).
            let (scratch0, scratch1) = require_scratch!(i64_scratch_base, "Mod");
            function.instruction(&Instruction::LocalSet(scratch0));
            function.instruction(&Instruction::LocalGet(scratch0));
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64Shl);
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64ShrS);
            function.instruction(&Instruction::LocalSet(scratch0));
            function.instruction(&Instruction::LocalSet(scratch1));
            function.instruction(&Instruction::LocalGet(scratch1));
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64Shl);
            function.instruction(&Instruction::I64Const(4));
            function.instruction(&Instruction::I64ShrS);
            function.instruction(&Instruction::LocalSet(scratch1));
            function.instruction(&Instruction::LocalGet(scratch1));
            function.instruction(&Instruction::LocalGet(scratch0));
            function.instruction(&Instruction::I64RemS);
            retag_int!(function);
            Ok(())
        }
        IrBinOp::Eq => {
            // Two tagged Ints are equal iff their tagged representations are equal
            // (same tag bits + same payload). i64.eq is correct without untagging.
            function.instruction(&Instruction::I64Eq);
            bool_from_i32!(function);
            Ok(())
        }
        IrBinOp::Lt => {
            // left_tagged = TAG_INT<<60 | left_payload (both have identical upper 4 bits = 0001).
            // As 64-bit signed numbers, both are positive (bit 63 = 0). The signed comparison
            // of the 64-bit values equals the signed comparison of the 60-bit payloads
            // because the upper 4 bits are identical.
            function.instruction(&Instruction::I64LtS);
            bool_from_i32!(function);
            Ok(())
        }
        IrBinOp::Gt => {
            function.instruction(&Instruction::I64GtS);
            bool_from_i32!(function);
            Ok(())
        }
        IrBinOp::Le => {
            function.instruction(&Instruction::I64LeS);
            bool_from_i32!(function);
            Ok(())
        }
        IrBinOp::Ge => {
            function.instruction(&Instruction::I64GeS);
            bool_from_i32!(function);
            Ok(())
        }
        IrBinOp::And => {
            // Bool: TAG_BOOL<<60 | 0 (false) or TAG_BOOL<<60 | 1 (true).
            // (TAG<<60 | a) & (TAG<<60 | b) = TAG<<60 | (a & b).
            // TAG_BOOL & TAG_BOOL = TAG_BOOL (0x2 & 0x2 = 0x2). Correctly tagged.
            function.instruction(&Instruction::I64And);
            Ok(())
        }
    }
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
    i32_base: u32,
    helper_state: Option<&HelperEmitState>,
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

            if let Some(helper_state) = helper_state {
                emit_update_heap_floor_from_buffer(
                    function,
                    helper_state.heap_floor_local,
                    buffer_ptr,
                    nread_offset,
                );
            }

            // Push tagged-i64 string pointer (points to buffer_ptr where len prefix lives).
            let tagged_ptr = encode_string_ptr(buffer_ptr as u32);
            function.instruction(&Instruction::I64Const(tagged_ptr));
        }

        ("Read", "read_line") => {
            // This currently supports the single-read-line shapes that still
            // route through `RuntimeIoPlan`. The result string is backed
            // by the same stdin buffer used for `Read.read`.
            let data_ptr = buffer_ptr + 4;
            let scan_idx_local = i32_base;
            let total_len_local = i32_base + 1;

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
            function.instruction(&Instruction::Drop);

            function.instruction(&Instruction::I32Const(nread_offset));
            function.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            function.instruction(&Instruction::LocalSet(total_len_local));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(scan_idx_local));

            function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
            function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
            function.instruction(&Instruction::LocalGet(scan_idx_local));
            function.instruction(&Instruction::LocalGet(total_len_local));
            function.instruction(&Instruction::I32GeU);
            function.instruction(&Instruction::BrIf(1));

            function.instruction(&Instruction::I32Const(data_ptr));
            function.instruction(&Instruction::LocalGet(scan_idx_local));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::I32Load8U(MemArg {
                offset: 0,
                align: 0,
                memory_index: 0,
            }));
            function.instruction(&Instruction::I32Const(10));
            function.instruction(&Instruction::I32Eq);
            function.instruction(&Instruction::BrIf(1));

            function.instruction(&Instruction::I32Const(data_ptr));
            function.instruction(&Instruction::LocalGet(scan_idx_local));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::I32Load8U(MemArg {
                offset: 0,
                align: 0,
                memory_index: 0,
            }));
            function.instruction(&Instruction::I32Const(13));
            function.instruction(&Instruction::I32Eq);
            function.instruction(&Instruction::BrIf(1));

            function.instruction(&Instruction::LocalGet(scan_idx_local));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(scan_idx_local));
            function.instruction(&Instruction::Br(0));
            function.instruction(&Instruction::End);
            function.instruction(&Instruction::End);

            function.instruction(&Instruction::I32Const(buffer_ptr));
            function.instruction(&Instruction::LocalGet(scan_idx_local));
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));

            if let Some(helper_state) = helper_state {
                emit_update_heap_floor_from_local_len(
                    function,
                    helper_state.heap_floor_local,
                    buffer_ptr,
                    scan_idx_local,
                );
            }

            function.instruction(&Instruction::I64Const(encode_string_ptr(buffer_ptr as u32)));
        }

        ("Print", "print") | ("Print", "println") => {
            let append_newline = op == "println";
            let ptr_local = i32_base;

            // Stack top is a tagged-i64 string ptr.
            function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
            function.instruction(&Instruction::I64And);
            function.instruction(&Instruction::I32WrapI64);
            function.instruction(&Instruction::LocalSet(ptr_local));

            // iovec[0] = data_ptr
            function.instruction(&Instruction::I32Const(iovec_offset));
            function.instruction(&Instruction::LocalGet(ptr_local));
            function.instruction(&Instruction::I32Const(4));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            // iovec[1] = memory[ptr_local] (the len)
            function.instruction(&Instruction::I32Const(iovec_offset + 4));
            function.instruction(&Instruction::LocalGet(ptr_local));
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
                // then issue fd_write. Actually we should use a data segment, but for the
                // current plain read+print subset we can store the byte at runtime.
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
            // For the current plain read+print subset, print is always in tail position
            // or in a stmt that's Dropped.
            // Actually the general lowering doesn't expect a return value from EffectOp
            // for print — but the IR treats PerformEffect as an expression. The lowered
            // sequence has EffectOp followed by Drop (for stmt) or StoreLocal.
            // For consistency: push the unit-tagged i64.
            use crate::gen_lower::value::encode_unit;
            function.instruction(&Instruction::I64Const(encode_unit()));
        }

        _ => {
            return Err(CodegenError {
                message: format!("gen_lower/emit: unsupported effect op '{effect}.{op}'"),
            });
        }
    }
    Ok(())
}

/// Emit the fused split-each-print loop.
///
/// At call time, the Wasm value stack must be empty (this is a statement-level instruction).
/// The string in `text_local_idx` (tagged I64) is split on `sep_byte`. For each segment,
/// `fd_write` is called (and optionally a newline write for println).
///
/// I32 scratch locals:
/// - `i32_base + 0`: str_ptr
/// - `i32_base + 1`: str_len
/// - `i32_base + 2`: pos
/// - `i32_base + 3`: line_start
///
/// Leaves an `encode_unit()` I64 on the stack as the result value.
#[allow(clippy::too_many_arguments)]
fn emit_split_each_print(
    function: &mut Function,
    text_local_idx: u32,
    sep_byte: u8,
    append_newline: bool,
    i32_base: u32,
    iovec_offset: i32,
    nread_offset: i32,
    newline_ptr: i32,
) {
    let s_str_ptr = i32_base; // scratch[0]: str_ptr (i32)
    let s_str_len = i32_base + 1; // scratch[1]: str_len (i32)
    let s_pos = i32_base + 2; // scratch[2]: scan position (i32)
    let s_line_start = i32_base + 3; // scratch[3]: line start (i32)

    // --- Extract str_ptr and str_len from text_local (tagged I64) ---
    // str_ptr = lower 32 bits of tagged I64
    function.instruction(&Instruction::LocalGet(text_local_idx));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(s_str_ptr));

    // str_len = mem[str_ptr] (the len prefix)
    function.instruction(&Instruction::LocalGet(s_str_ptr));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_str_len));

    // pos = 0; line_start = 0
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_line_start));

    // if str_len == 0: skip everything
    function.instruction(&Instruction::LocalGet(s_str_len));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Else);

    // Scan loop: for pos in 0..str_len
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty)); // block (break target)
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty)); // loop

    // if pos >= str_len: break
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_str_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1)); // break block

    // byte = mem[str_ptr + 4 + pos]  (data starts at str_ptr+4)
    function.instruction(&Instruction::LocalGet(s_str_ptr));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load8U(MemArg {
        offset: 4,
        align: 0,
        memory_index: 0,
    }));

    // if byte == sep_byte: emit slice and advance line_start
    function.instruction(&Instruction::I32Const(sep_byte as i32));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // Emit slice [line_start .. pos)
    emit_write_slice(
        function,
        s_str_ptr,
        s_pos,
        s_line_start,
        append_newline,
        iovec_offset,
        nread_offset,
        newline_ptr,
    );
    // line_start = pos + 1
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_line_start));
    function.instruction(&Instruction::End); // end if sep

    // pos += 1
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&Instruction::Br(0)); // continue loop

    function.instruction(&Instruction::End); // end loop
    function.instruction(&Instruction::End); // end block

    // After loop: emit final segment if line_start < str_len (last line w/o trailing sep)
    function.instruction(&Instruction::LocalGet(s_line_start));
    function.instruction(&Instruction::LocalGet(s_str_len));
    function.instruction(&Instruction::I32LtU);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_write_slice(
        function,
        s_str_ptr,
        s_str_len, // end = str_len
        s_line_start,
        append_newline,
        iovec_offset,
        nread_offset,
        newline_ptr,
    );
    function.instruction(&Instruction::End); // end if last segment

    function.instruction(&Instruction::End); // end else (str_len != 0)

    // Result: push unit value
    function.instruction(&Instruction::I64Const(encode_unit()));
}

fn emit_abort(function: &mut Function) {
    function.instruction(&Instruction::Unreachable);
}

fn emit_split_index_operand(
    function: &mut Function,
    ctx: &EmitContext,
    index: &SplitIndexOperand,
) -> Result<(), CodegenError> {
    match index {
        SplitIndexOperand::Const(value) => {
            let encoded =
                crate::gen_lower::value::encode_int(*value).map_err(|e| CodegenError {
                    message: format!("gen_lower/emit: invalid SplitGetPrint index literal: {e}"),
                })?;
            function.instruction(&Instruction::I64Const(encoded));
        }
        SplitIndexOperand::Local(name) => {
            let idx = ctx.get(name)?;
            function.instruction(&Instruction::LocalGet(idx));
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_split_get_print(
    function: &mut Function,
    ctx: &EmitContext,
    text_local_idx: u32,
    sep_byte: u8,
    index: &SplitIndexOperand,
    append_newline: bool,
    i32_base: u32,
    iovec_offset: i32,
    nread_offset: i32,
    newline_ptr: i32,
) -> Result<(), CodegenError> {
    let s_str_ptr = i32_base;
    let s_str_len = i32_base + 1;
    let s_pos = i32_base + 2;
    let s_line_start = i32_base + 3;
    let s_target = i32_base + 4;

    emit_split_index_operand(function, ctx, index)?;
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64Shl);
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64ShrS);
    function.instruction(&Instruction::I64Const(0));
    function.instruction(&Instruction::I64LtS);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    emit_split_index_operand(function, ctx, index)?;
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64Shl);
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64ShrS);
    function.instruction(&Instruction::I64Const(i64::from(i32::MAX)));
    function.instruction(&Instruction::I64GtS);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    emit_split_index_operand(function, ctx, index)?;
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64Shl);
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64ShrS);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(s_target));

    function.instruction(&Instruction::LocalGet(text_local_idx));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(s_str_ptr));

    function.instruction(&Instruction::LocalGet(s_str_ptr));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_str_len));

    function.instruction(&Instruction::LocalGet(s_str_len));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_line_start));

    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));

    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_str_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::LocalGet(s_str_ptr));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Load8U(MemArg {
        offset: 4,
        align: 0,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I32Const(sep_byte as i32));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

    function.instruction(&Instruction::LocalGet(s_target));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_write_slice(
        function,
        s_str_ptr,
        s_pos,
        s_line_start,
        append_newline,
        iovec_offset,
        nread_offset,
        newline_ptr,
    );
    function.instruction(&Instruction::I32Const(-1));
    function.instruction(&Instruction::LocalSet(s_target));
    function.instruction(&Instruction::Br(3));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(s_target));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::LocalSet(s_target));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_line_start));
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&Instruction::Br(0));

    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_target));
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::I32LtS);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(s_target));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_write_slice(
        function,
        s_str_ptr,
        s_str_len,
        s_line_start,
        append_newline,
        iovec_offset,
        nread_offset,
        newline_ptr,
    );
    function.instruction(&Instruction::Else);
    emit_abort(function);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::End);
    function.instruction(&Instruction::I64Const(encode_unit()));
    Ok(())
}

/// Emit fd_write for the slice [line_start .. end) of the string at str_ptr.
/// The string data starts at str_ptr + 4 (after the len prefix).
fn emit_write_slice(
    function: &mut Function,
    s_str_ptr: u32,
    s_end: u32, // local index for end position (pos or str_len)
    s_line_start: u32,
    append_newline: bool,
    iovec_offset: i32,
    nread_offset: i32,
    newline_ptr: i32,
) {
    // iovec[0] = str_ptr + 4 + line_start  (data pointer for this slice)
    function.instruction(&Instruction::I32Const(iovec_offset));
    function.instruction(&Instruction::LocalGet(s_str_ptr));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_line_start));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));

    // iovec[1] = end - line_start  (length of this slice)
    function.instruction(&Instruction::I32Const(iovec_offset + 4));
    function.instruction(&Instruction::LocalGet(s_end));
    function.instruction(&Instruction::LocalGet(s_line_start));
    function.instruction(&Instruction::I32Sub);
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
    function.instruction(&Instruction::Drop);

    if append_newline {
        // Write newline byte (pre-populated in data section at newline_ptr).
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gen_lower::backend_ir::{SplitIndexOperand, WasmBackendInstr as I};
    use wasmparser::Validator;

    fn default_layout() -> MemoryLayout {
        MemoryLayout::default()
    }

    fn assert_valid_wasm(wasm: &[u8]) {
        assert!(wasm.len() >= 8, "module too short: {} bytes", wasm.len());
        assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d], "bad wasm magic");
        assert_eq!(&wasm[4..8], &[0x01, 0x00, 0x00, 0x00], "bad wasm version");
        Validator::new()
            .validate_all(wasm)
            .expect("module should pass wasm validation");
    }

    #[test]
    fn emit_read_then_print_produces_valid_wasm() {
        let instrs = vec![
            I::EffectOp {
                effect: "Read".to_string(),
                op: "read".to_string(),
            },
            I::EffectOp {
                effect: "Print".to_string(),
                op: "print".to_string(),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout()).expect("emit should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_let_read_print_produces_valid_wasm() {
        // Corresponds to: text = Read.read(); Print.print(text)
        let instrs = vec![
            I::DeclareLocal {
                name: "text".to_string(),
            },
            I::EffectOp {
                effect: "Read".to_string(),
                op: "read".to_string(),
            },
            I::StoreLocal {
                name: "text".to_string(),
            },
            I::LoadLocal {
                name: "text".to_string(),
            },
            I::EffectOp {
                effect: "Print".to_string(),
                op: "print".to_string(),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout()).expect("emit should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_unknown_effect_returns_error() {
        let instrs = vec![I::EffectOp {
            effect: "Foo".to_string(),
            op: "bar".to_string(),
        }];
        assert!(emit_general_module(&instrs, &default_layout()).is_err());
    }

    #[test]
    fn emit_split_each_println_produces_valid_wasm() {
        // Corresponds to: text = Read.read(); SplitEachPrint(text, "\n", Print, println)
        let instrs = vec![
            I::DeclareLocal {
                name: "text".to_string(),
            },
            I::EffectOp {
                effect: "Read".to_string(),
                op: "read".to_string(),
            },
            I::StoreLocal {
                name: "text".to_string(),
            },
            I::SplitEachPrint {
                text_local: "text".to_string(),
                sep_bytes: b"\n".to_vec(),
                effect: "Print".to_string(),
                op: "println".to_string(),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit SplitEachPrint should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_split_get_println_produces_valid_wasm() {
        let instrs = vec![
            I::DeclareLocal {
                name: "text".to_string(),
            },
            I::EffectOp {
                effect: "Read".to_string(),
                op: "read".to_string(),
            },
            I::StoreLocal {
                name: "text".to_string(),
            },
            I::SplitGetPrint {
                text_local: "text".to_string(),
                sep_bytes: b"\n".to_vec(),
                index: SplitIndexOperand::Const(1),
                op: "println".to_string(),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit SplitGetPrint should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_split_get_print_with_local_index_produces_valid_wasm() {
        let instrs = vec![
            I::DeclareLocal {
                name: "text".to_string(),
            },
            I::DeclareLocal {
                name: "idx".to_string(),
            },
            I::EffectOp {
                effect: "Read".to_string(),
                op: "read".to_string(),
            },
            I::StoreLocal {
                name: "text".to_string(),
            },
            I::I64Const(crate::gen_lower::value::encode_int(0).unwrap()),
            I::StoreLocal {
                name: "idx".to_string(),
            },
            I::SplitGetPrint {
                text_local: "text".to_string(),
                sep_bytes: b"\n".to_vec(),
                index: SplitIndexOperand::Local("idx".to_string()),
                op: "print".to_string(),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit SplitGetPrint should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_static_string_print_produces_valid_wasm() {
        let instrs = vec![
            I::PushStaticString {
                text: "done".to_string(),
            },
            I::EffectOp {
                effect: "Print".to_string(),
                op: "print".to_string(),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit static string print should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn supports_list_push_string_intrinsic() {
        let instrs = vec![
            I::PushStaticString {
                text: "left\nright".to_string(),
            },
            I::PushStaticString {
                text: "\n".to_string(),
            },
            I::Intrinsic {
                intrinsic: BackendIntrinsic::StringSplit,
            },
            I::PushStaticString {
                text: "tail".to_string(),
            },
            I::Intrinsic {
                intrinsic: BackendIntrinsic::ListPushString,
            },
            I::PushStaticString {
                text: "2".to_string(),
            },
        ];
        assert!(supports_instrs(&instrs));
    }

    #[test]
    fn supports_track_e_host_grapheme_intrinsics() {
        let instrs = vec![
            I::PushStaticString {
                text: "a👨‍👩‍👧‍👦b".to_string(),
            },
            I::Intrinsic {
                intrinsic: BackendIntrinsic::StringEachGraphemeCount,
            },
            I::Drop,
        ];
        assert!(supports_instrs(&instrs));
    }

    #[test]
    fn emit_track_e_host_imports_for_grapheme_intrinsics() {
        let instrs = vec![
            I::PushStaticString {
                text: "a👨‍👩‍👧‍👦b".to_string(),
            },
            I::Intrinsic {
                intrinsic: BackendIntrinsic::StringEachGraphemeCount,
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit host-backed grapheme intrinsic should succeed");
        assert_valid_wasm(&wasm);
        assert!(
            wasm.windows(b"goby:runtime/track-e".len())
                .any(|w| w == b"goby:runtime/track-e"),
            "expected Track E host import module name in Wasm import section"
        );
        assert!(
            wasm.windows(b"__goby_string_each_grapheme_count".len())
                .any(|w| w == b"__goby_string_each_grapheme_count"),
            "expected grapheme count host import name in Wasm import section"
        );
    }

    #[test]
    fn emit_list_push_string_produces_valid_wasm() {
        let instrs = vec![
            I::PushStaticString {
                text: "left\nright".to_string(),
            },
            I::PushStaticString {
                text: "\n".to_string(),
            },
            I::Intrinsic {
                intrinsic: BackendIntrinsic::StringSplit,
            },
            I::PushStaticString {
                text: "tail".to_string(),
            },
            I::Intrinsic {
                intrinsic: BackendIntrinsic::ListPushString,
            },
            I::I64Const(crate::gen_lower::value::encode_int(2).unwrap()),
            I::Intrinsic {
                intrinsic: BackendIntrinsic::ListGet,
            },
            I::EffectOp {
                effect: "Print".to_string(),
                op: "print".to_string(),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit ListPushString helper chain should succeed");
        assert_valid_wasm(&wasm);
    }

    // --- WB-1 Step 2: BinOp emission ---

    #[test]
    fn emit_binop_add_produces_valid_wasm() {
        use goby_core::ir::IrBinOp;
        // Emit: I64Const(encode_int(2)), I64Const(encode_int(3)), BinOp(Add), EffectOp(Print,print), Drop
        // This simulates `2 + 3` with a Print effect so the module is valid.
        let instrs = vec![
            I::I64Const(crate::gen_lower::value::encode_int(2).unwrap()),
            I::I64Const(crate::gen_lower::value::encode_int(3).unwrap()),
            I::BinOp { op: IrBinOp::Add },
            I::EffectOp {
                effect: "Print".to_string(),
                op: "print".to_string(),
            },
            I::Drop,
        ];
        let wasm =
            emit_general_module(&instrs, &default_layout()).expect("emit BinOp Add should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_binop_eq_comparison_produces_valid_wasm() {
        use goby_core::ir::IrBinOp;
        let instrs = vec![
            I::I64Const(crate::gen_lower::value::encode_int(5).unwrap()),
            I::I64Const(crate::gen_lower::value::encode_int(5).unwrap()),
            I::BinOp { op: IrBinOp::Eq },
            I::EffectOp {
                effect: "Print".to_string(),
                op: "print".to_string(),
            },
            I::Drop,
        ];
        let wasm =
            emit_general_module(&instrs, &default_layout()).expect("emit BinOp Eq should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_binop_sub_produces_valid_wasm() {
        use goby_core::ir::IrBinOp;
        let instrs = vec![
            I::I64Const(crate::gen_lower::value::encode_int(10).unwrap()),
            I::I64Const(crate::gen_lower::value::encode_int(3).unwrap()),
            I::BinOp { op: IrBinOp::Sub },
            I::Drop,
        ];
        let wasm =
            emit_general_module(&instrs, &default_layout()).expect("emit BinOp Sub should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_binop_lt_produces_valid_wasm() {
        use goby_core::ir::IrBinOp;
        let instrs = vec![
            I::I64Const(crate::gen_lower::value::encode_int(1).unwrap()),
            I::I64Const(crate::gen_lower::value::encode_int(2).unwrap()),
            I::BinOp { op: IrBinOp::Lt },
            I::Drop,
        ];
        let wasm =
            emit_general_module(&instrs, &default_layout()).expect("emit BinOp Lt should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_binop_and_produces_valid_wasm() {
        use goby_core::ir::IrBinOp;
        let instrs = vec![
            I::I64Const(crate::gen_lower::value::encode_bool(true)),
            I::I64Const(crate::gen_lower::value::encode_bool(false)),
            I::BinOp { op: IrBinOp::And },
            I::Drop,
        ];
        let wasm =
            emit_general_module(&instrs, &default_layout()).expect("emit BinOp And should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_binop_mul_with_scratch_produces_valid_wasm() {
        use goby_core::ir::IrBinOp;
        // Mul requires scratch i64 locals (required_i64_scratch_count returns 2 for BinOp::Mul).
        // We avoid EffectOp here because Print.print needs i32 scratch locals which would
        // require HELPER_SCRATCH_I32 i32 locals — not needed for Mul alone.
        let instrs = vec![
            I::I64Const(crate::gen_lower::value::encode_int(3).unwrap()),
            I::I64Const(crate::gen_lower::value::encode_int(4).unwrap()),
            I::BinOp { op: IrBinOp::Mul },
            I::Drop,
        ];
        let wasm =
            emit_general_module(&instrs, &default_layout()).expect("emit BinOp Mul should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_if_both_branches_produce_valid_wasm() {
        // if true then 1 else 2 (with Read effect wrapper to satisfy has_runtime_read_effect)
        let instrs = vec![
            I::EffectOp {
                effect: "Read".to_string(),
                op: "read".to_string(),
            },
            I::Drop,
            I::I64Const(crate::gen_lower::value::encode_bool(true)),
            I::If {
                then_instrs: vec![I::I64Const(crate::gen_lower::value::encode_int(1).unwrap())],
                else_instrs: vec![I::I64Const(crate::gen_lower::value::encode_int(2).unwrap())],
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout()).expect("emit If should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_nested_if_produces_valid_wasm() {
        // if true then (if false then 1 else 2) else 3
        let instrs = vec![
            I::EffectOp {
                effect: "Read".to_string(),
                op: "read".to_string(),
            },
            I::Drop,
            I::I64Const(crate::gen_lower::value::encode_bool(true)),
            I::If {
                then_instrs: vec![
                    I::I64Const(crate::gen_lower::value::encode_bool(false)),
                    I::If {
                        then_instrs: vec![I::I64Const(
                            crate::gen_lower::value::encode_int(1).unwrap(),
                        )],
                        else_instrs: vec![I::I64Const(
                            crate::gen_lower::value::encode_int(2).unwrap(),
                        )],
                    },
                ],
                else_instrs: vec![I::I64Const(crate::gen_lower::value::encode_int(3).unwrap())],
            },
            I::Drop,
        ];
        let wasm =
            emit_general_module(&instrs, &default_layout()).expect("emit nested If should succeed");
        assert_valid_wasm(&wasm);
    }
}
