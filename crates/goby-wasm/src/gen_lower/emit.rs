//! `WasmBackendInstr` → Wasm bytes emission for the general lowering path.
//!
//! This module owns the concrete WASI import mapping for runtime `Read`/`Print`
//! effect operations.

use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, ElementSection, Elements, EntityType, ExportKind,
    ExportSection, Function, FunctionSection, ImportSection, Instruction, MemArg, MemorySection,
    Module, RefType, TableSection, TableType, TypeSection, ValType,
};

use crate::CodegenError;
use crate::gen_lower::backend_ir::{
    BackendEffectOp, BackendIntrinsic, BackendPrintOp, BackendReadOp, SplitIndexOperand,
    WasmBackendInstr,
};
use goby_core::ir::IrBinOp;

use crate::gen_lower::value::{
    TAG_BOOL, TAG_CELL, TAG_CLOSURE, TAG_INT, TAG_LIST, TAG_RECORD, TAG_STRING, TAG_TUPLE,
    encode_string_ptr, encode_unit,
};
use crate::host_runtime::{
    HOST_BUMP_RESERVED_BYTES, HOST_INTRINSIC_IMPORTS, IntrinsicExecutionBoundary,
    host_import_for_intrinsic,
};
use crate::layout::{
    GLOBAL_HEAP_CURSOR_OFFSET, GLOBAL_RUNTIME_ERROR_OFFSET, MemoryLayout,
    RUNTIME_ERROR_MEMORY_EXHAUSTION,
};
use crate::memory_config::{DEFAULT_WASM_MEMORY_CONFIG, WASM_PAGE_BYTES};

const STATIC_STRING_LIMIT: u32 =
    DEFAULT_WASM_MEMORY_CONFIG.initial_linear_memory_bytes() - HOST_BUMP_RESERVED_BYTES;

// Import function indices begin with the WASI pair and may be extended with
// `goby-wasm` owned grapheme host intrinsics.
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
const HELPER_SCRATCH_I64: u32 = 3;
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
//
// Additional i32 locals may be appended after this fixed helper pool. Those
// extra locals are reserved for heap-base spill slots used by nested composite
// allocations (`ListLit`, `TupleLit`, `RecordLit`, `CreateClosure`) so parent
// allocation pointers survive child heap allocations.
const HELPER_SCRATCH_I32: u32 = 13;
const HELPER_ALLOC_CURSOR_OFFSET: u32 = 11;
const HELPER_HEAP_FLOOR_OFFSET: u32 = 12;

// Named offsets into the HELPER_SCRATCH_I32 pool (relative to i32_base).
// These are used across emit_string_split_helper, emit_helper_call, emit_instrs
// (ListLit/TupleLit/RecordLit), emit_case_match (ListPattern), and
// emit_list_push_string_helper.
const HS_TEXT_PTR: u32 = 0; // text/string/list ptr
const HS_TEXT_LEN: u32 = 1; // text/string/list len
const HS_SEP_PTR: u32 = 2; // sep ptr / decoded index
const HS_SEP_LEN: u32 = 3; // sep len
const HS_SCAN_POS: u32 = 4; // scan pos
const HS_SEG_START: u32 = 5; // segment start
const HS_ITEM_COUNT: u32 = 6; // item count
const HS_AUX_PTR: u32 = 7; // auxiliary pointer (tail ptr in ListPattern, list ptr in ListLit)
const HS_LIST_PTR: u32 = 8; // secondary list ptr
const HS_ALLOC_SIZE: u32 = 9; // allocation size temp
const HS_ITER: u32 = 10; // copy or match index

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
        let mut cursor = STATIC_STRING_LIMIT;

        let mut all_instrs = Vec::new();
        for slice in all_slices {
            all_instrs.extend(collect_all_instrs(slice));
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
            bytes_used: STATIC_STRING_LIMIT - cursor,
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
    /// Map from declaration name to Wasm function index (for direct `DeclCall`).
    /// Built by `emit_general_module` before emitting any function body.
    decl_func_indices: HashMap<String, u32>,
    /// Map from declaration name to funcref table slot index (for `PushFuncHandle`).
    /// Slot 0 = first aux decl, slot 1 = second aux decl, etc.
    decl_table_slots: HashMap<String, u32>,
    /// Type index for `(i64) -> i64` in the Wasm type section, used by `IndirectCall { arity: 1 }`.
    /// `None` when the module has no arity-1 `IndirectCall` instructions.
    indirect_call_type_idx_1: Option<u32>,
    /// Type index for `(i64, i64) -> i64` in the Wasm type section, used by `IndirectCall { arity: 2 }`.
    /// `None` when the module has no arity-2 `IndirectCall` instructions.
    indirect_call_type_idx_2: Option<u32>,
    /// Type index for `(i64, i64, i64) -> i64` in the Wasm type section, used by
    /// closure-backed indirect calls with two user-visible arguments.
    indirect_call_type_idx_3: Option<u32>,
    /// Map from record constructor name to module-local numeric tag used in `RecordLit`.
    record_ctor_tags: HashMap<String, u32>,
}

impl EmitContext {
    /// Build an `EmitContext` seeded with the module-level lookup tables shared across
    /// all function bodies (main and every aux decl).  `locals` starts empty; callers
    /// that need to pre-register Wasm function parameters insert them afterwards.
    fn with_module_tables(
        decl_func_indices: HashMap<String, u32>,
        decl_table_slots: HashMap<String, u32>,
        indirect_call_type_idx_1: Option<u32>,
        indirect_call_type_idx_2: Option<u32>,
        indirect_call_type_idx_3: Option<u32>,
        record_ctor_tags: HashMap<String, u32>,
    ) -> Self {
        Self {
            locals: HashMap::new(),
            next_local: 0,
            decl_func_indices,
            decl_table_slots,
            indirect_call_type_idx_1,
            indirect_call_type_idx_2,
            indirect_call_type_idx_3,
            record_ctor_tags,
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

    fn decl_table_slot(&self, decl_name: &str) -> Result<u32, CodegenError> {
        self.decl_table_slots
            .get(decl_name)
            .copied()
            .ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: unknown declaration '{decl_name}' in PushFuncHandle"
                ),
            })
    }

    fn record_ctor_tag(&self, constructor: &str) -> Result<u32, CodegenError> {
        self.record_ctor_tags
            .get(constructor)
            .copied()
            .ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: unknown record constructor '{constructor}' in RecordLit"
                ),
            })
    }

    /// Return the Wasm type-section index for a `call_indirect` with `arity` i64 arguments.
    ///
    /// Returns `Err` when the required type index was not registered (meaning no instruction
    /// of that arity was detected during module scanning) or when `arity` is unsupported.
    fn indirect_call_type_idx(&self, arity: u8) -> Result<u32, CodegenError> {
        match arity {
            1 => self.indirect_call_type_idx_1.ok_or_else(|| CodegenError {
                message:
                    "gen_lower/emit: arity-1 IndirectCall used but indirect_call_type_idx_1 not registered"
                        .to_string(),
            }),
            2 => self.indirect_call_type_idx_2.ok_or_else(|| CodegenError {
                message:
                    "gen_lower/emit: arity-2 IndirectCall used but indirect_call_type_idx_2 not registered"
                        .to_string(),
            }),
            3 => self.indirect_call_type_idx_3.ok_or_else(|| CodegenError {
                message:
                    "gen_lower/emit: arity-3 IndirectCall used but indirect_call_type_idx_3 not registered"
                        .to_string(),
            }),
            _ => Err(CodegenError {
                message: format!("gen_lower/emit: IndirectCall with unsupported arity {arity}"),
            }),
        }
    }
}

#[derive(Clone, Copy)]
struct HelperEmitState {
    i64_base: u32,
    i32_base: u32,
    alloc_cursor_local: u32,
    heap_floor_local: u32,
    function_returns_i64: bool,
}

/// Extract `helper_state`, returning a `CodegenError` if it is absent.
///
/// `context` names the instruction or operation that requires scratch state,
/// and appears verbatim in the error message so callers do not need to repeat
/// the error-construction boilerplate.
///
/// `HelperEmitState` is `Copy`, so callers pass the `Option<HelperEmitState>` value directly.
fn require_helper_state(
    helper_state: Option<HelperEmitState>,
    context: &str,
) -> Result<HelperEmitState, CodegenError> {
    helper_state.ok_or_else(|| CodegenError {
        message: format!("gen_lower/emit: {context} requires helper scratch state"),
    })
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
        match instr {
            WasmBackendInstr::If {
                then_instrs,
                else_instrs,
            } => {
                result.extend(collect_all_instrs(then_instrs));
                result.extend(collect_all_instrs(else_instrs));
            }
            WasmBackendInstr::CaseMatch { arms, .. } => {
                for arm in arms {
                    result.extend(collect_all_instrs(&arm.body_instrs));
                }
            }
            WasmBackendInstr::ListLit { element_instrs } => {
                for elem in element_instrs {
                    result.extend(collect_all_instrs(elem));
                }
            }
            WasmBackendInstr::TupleLit { element_instrs } => {
                for elem in element_instrs {
                    result.extend(collect_all_instrs(elem));
                }
            }
            WasmBackendInstr::RecordLit { field_instrs, .. } => {
                for field in field_instrs {
                    result.extend(collect_all_instrs(field));
                }
            }
            WasmBackendInstr::ListEachEffect { list_instrs, .. } => {
                result.extend(collect_all_instrs(list_instrs));
            }
            WasmBackendInstr::ListEach {
                list_instrs,
                func_instrs,
            }
            | WasmBackendInstr::ListMap {
                list_instrs,
                func_instrs,
            } => {
                result.extend(collect_all_instrs(list_instrs));
                result.extend(collect_all_instrs(func_instrs));
            }
            WasmBackendInstr::CreateClosure {
                func_handle_instrs,
                slot_instrs,
            } => {
                result.extend(collect_all_instrs(func_handle_instrs));
                for slot in slot_instrs {
                    result.extend(collect_all_instrs(slot));
                }
            }
            WasmBackendInstr::AllocMutableCell { init_instrs } => {
                result.extend(collect_all_instrs(init_instrs));
            }
            WasmBackendInstr::StoreCellValue {
                cell_ptr_instrs,
                value_instrs,
            } => {
                result.extend(collect_all_instrs(cell_ptr_instrs));
                result.extend(collect_all_instrs(value_instrs));
            }
            _ => {}
        }
    }
    result
}

/// Returns true when `instrs` require the `HelperEmitState` bump-allocator / i32 scratch pool.
///
/// This is the single source of truth for the helper-state gate used both in
/// `emit_instrs` and `required_i32_scratch_count`.  A function needs helper state when it
/// contains any of:
/// - An `Intrinsic` instruction (uses the full HELPER_SCRATCH_I32 pool).
/// - A `ListLit` instruction (uses bump allocator via `emit_alloc_from_top`).
/// - A `TupleLit` instruction (uses bump allocator via `emit_alloc_from_top`).
/// - A `RecordLit` instruction (uses bump allocator via `emit_alloc_from_top`).
/// - A `CaseMatch` arm with a `ListPattern` (needs bump allocator for tail sub-list).
/// - A `CaseMatch` arm with an `EmptyList` pattern (needs one i32 to load the list pointer).
fn needs_helper_state(instrs: &[WasmBackendInstr]) -> bool {
    use crate::gen_lower::backend_ir::BackendCasePattern;
    collect_all_instrs(instrs).iter().any(|i| {
        matches!(
            i,
            WasmBackendInstr::Intrinsic { .. }
                | WasmBackendInstr::ListLit { .. }
                | WasmBackendInstr::TupleLit { .. }
                | WasmBackendInstr::RecordLit { .. }
                | WasmBackendInstr::ListEachEffect { .. }
                | WasmBackendInstr::ListEach { .. }
                | WasmBackendInstr::ListMap { .. }
                | WasmBackendInstr::CreateClosure { .. }
                | WasmBackendInstr::AllocMutableCell { .. }
                // DeclCall / IndirectCall may trigger callee heap allocations; the global cursor
                // must be synchronized before and after the call, which requires alloc_cursor_local.
                | WasmBackendInstr::DeclCall { .. }
                | WasmBackendInstr::IndirectCall { .. }
        )
    }) || {
        // Check for ListPattern or EmptyList patterns in CaseMatch arms.
        fn has_heap_pattern(instrs: &[WasmBackendInstr]) -> bool {
            for instr in instrs {
                match instr {
                    WasmBackendInstr::CaseMatch { arms, .. } => {
                        for arm in arms {
                            if matches!(
                                &arm.pattern,
                                BackendCasePattern::ListPattern { .. }
                                    | BackendCasePattern::EmptyList
                            ) {
                                return true;
                            }
                            if has_heap_pattern(&arm.body_instrs) {
                                return true;
                            }
                        }
                    }
                    WasmBackendInstr::If {
                        then_instrs,
                        else_instrs,
                    } => {
                        if has_heap_pattern(then_instrs) || has_heap_pattern(else_instrs) {
                            return true;
                        }
                    }
                    WasmBackendInstr::ListLit { element_instrs } => {
                        if element_instrs.iter().any(|e| has_heap_pattern(e)) {
                            return true;
                        }
                    }
                    WasmBackendInstr::TupleLit { element_instrs } => {
                        if element_instrs.iter().any(|e| has_heap_pattern(e)) {
                            return true;
                        }
                    }
                    WasmBackendInstr::RecordLit { field_instrs, .. } => {
                        if field_instrs.iter().any(|e| has_heap_pattern(e)) {
                            return true;
                        }
                    }
                    WasmBackendInstr::CreateClosure {
                        func_handle_instrs,
                        slot_instrs,
                    } => {
                        if has_heap_pattern(func_handle_instrs) {
                            return true;
                        }
                        if slot_instrs.iter().any(|e| has_heap_pattern(e)) {
                            return true;
                        }
                    }
                    WasmBackendInstr::AllocMutableCell { init_instrs } => {
                        if has_heap_pattern(init_instrs) {
                            return true;
                        }
                    }
                    WasmBackendInstr::StoreCellValue {
                        cell_ptr_instrs,
                        value_instrs,
                    } => {
                        if has_heap_pattern(cell_ptr_instrs) || has_heap_pattern(value_instrs) {
                            return true;
                        }
                    }
                    _ => {}
                }
            }
            false
        }
        has_heap_pattern(instrs)
    }
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
            WasmBackendInstr::EffectOp {
                op: BackendEffectOp::Print(_)
            } | WasmBackendInstr::EffectOp {
                op: BackendEffectOp::Read(BackendReadOp::ReadLine)
            }
        )
    });

    let split_count = if split { SPLIT_FUSED_SCRATCH_I32 } else { 0 };
    let generic_count = if generic_effects {
        GENERIC_EFFECT_SCRATCH_I32
    } else {
        0
    };
    let helper_count = if needs_helper_state(instrs) {
        HELPER_SCRATCH_I32 + required_heap_base_spill_count(instrs)
    } else {
        0
    };
    split_count.max(generic_count).max(helper_count)
}

fn required_heap_base_spill_count(instrs: &[WasmBackendInstr]) -> u32 {
    instrs
        .iter()
        .map(required_heap_base_spill_count_instr)
        .max()
        .unwrap_or(0)
}

fn required_heap_base_spill_count_instr(instr: &WasmBackendInstr) -> u32 {
    match instr {
        WasmBackendInstr::If {
            then_instrs,
            else_instrs,
        } => required_heap_base_spill_count(then_instrs)
            .max(required_heap_base_spill_count(else_instrs)),
        WasmBackendInstr::CaseMatch { arms, .. } => arms
            .iter()
            .map(|arm| required_heap_base_spill_count(&arm.body_instrs))
            .max()
            .unwrap_or(0),
        WasmBackendInstr::ListLit { element_instrs } => {
            1 + element_instrs
                .iter()
                .map(|child| required_heap_base_spill_count(child))
                .max()
                .unwrap_or(0)
        }
        WasmBackendInstr::TupleLit { element_instrs } => {
            1 + element_instrs
                .iter()
                .map(|child| required_heap_base_spill_count(child))
                .max()
                .unwrap_or(0)
        }
        WasmBackendInstr::RecordLit { field_instrs, .. } => {
            1 + field_instrs
                .iter()
                .map(|child| required_heap_base_spill_count(child))
                .max()
                .unwrap_or(0)
        }
        WasmBackendInstr::ListEachEffect { list_instrs, .. } => {
            required_heap_base_spill_count(list_instrs)
        }
        WasmBackendInstr::ListEach {
            list_instrs,
            func_instrs,
        }
        | WasmBackendInstr::ListMap {
            list_instrs,
            func_instrs,
        } => required_heap_base_spill_count(list_instrs)
            .max(required_heap_base_spill_count(func_instrs)),
        WasmBackendInstr::CreateClosure {
            func_handle_instrs,
            slot_instrs,
        } => {
            1 + required_heap_base_spill_count(func_handle_instrs).max(
                slot_instrs
                    .iter()
                    .map(|child| required_heap_base_spill_count(child))
                    .max()
                    .unwrap_or(0),
            )
        }
        WasmBackendInstr::AllocMutableCell { init_instrs } => {
            required_heap_base_spill_count(init_instrs)
        }
        _ => 0,
    }
}

fn required_i64_scratch_count(instrs: &[WasmBackendInstr]) -> u32 {
    use goby_core::ir::IrBinOp;
    let all = collect_all_instrs(instrs);
    let needs_scratch = all.iter().any(|i| match i {
        WasmBackendInstr::Intrinsic { .. }
        | WasmBackendInstr::ListEach { .. }
        | WasmBackendInstr::ListMap { .. }
        | WasmBackendInstr::IndirectCall { .. } => true,
        WasmBackendInstr::BinOp { op } => {
            matches!(op, IrBinOp::Mul | IrBinOp::Div | IrBinOp::Mod)
        }
        _ => false,
    });
    if needs_scratch { HELPER_SCRATCH_I64 } else { 0 }
}

/// Returns true if any print-with-newline instruction is present (needs newline data segment).
fn needs_newline_data(instrs: &[WasmBackendInstr]) -> bool {
    let all = collect_all_instrs(instrs);
    all.iter().any(|i| {
        matches!(
            i,
            WasmBackendInstr::SplitEachPrint {
                op: BackendPrintOp::Println,
                ..
            }
        ) || matches!(
            i,
            WasmBackendInstr::SplitGetPrint {
                op: BackendPrintOp::Println,
                ..
            }
        ) || matches!(
            i,
            WasmBackendInstr::ListEachEffect {
                op: BackendPrintOp::Println,
                ..
            }
        )
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EffectEmitStrategy {
    Wb3DirectCall,
    Wb3BWasmFxExperimental,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct EmitOptions {
    pub(crate) effect_emit_strategy: EffectEmitStrategy,
    pub(crate) memory_config: crate::memory_config::WasmMemoryConfig,
}

impl Default for EmitOptions {
    fn default() -> Self {
        Self {
            effect_emit_strategy: default_effect_emit_strategy(),
            memory_config: DEFAULT_WASM_MEMORY_CONFIG,
        }
    }
}

fn default_effect_emit_strategy() -> EffectEmitStrategy {
    if cfg!(feature = "wasmfx-experimental") {
        EffectEmitStrategy::Wb3BWasmFxExperimental
    } else {
        EffectEmitStrategy::Wb3DirectCall
    }
}

fn initialize_helper_state_locals(
    function: &mut Function,
    layout: &MemoryLayout,
    i32_base: u32,
    static_strings: &StaticStringPool,
    is_main: bool,
) -> Result<(), CodegenError> {
    let buffer_ptr = layout.heap_base as i32;
    let alloc_cursor_local = i32_base + HELPER_ALLOC_CURSOR_OFFSET;
    let heap_floor_local = i32_base + HELPER_HEAP_FLOOR_OFFSET;
    let initial_cursor = align_down_i32(
        i32::try_from(STATIC_STRING_LIMIT - static_strings.bytes_used).map_err(|_| {
            CodegenError {
                message: "gen_lower/emit: helper allocation cursor does not fit in i32".to_string(),
            }
        })?,
        8,
    );
    let initial_floor = align_up_i32(buffer_ptr + 4, 8);
    if is_main {
        // main initializes the global heap cursor in linear memory, then loads it locally.
        function.instruction(&Instruction::I32Const(GLOBAL_HEAP_CURSOR_OFFSET as i32));
        function.instruction(&Instruction::I32Const(initial_cursor));
        function.instruction(&Instruction::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        function.instruction(&Instruction::I32Const(GLOBAL_RUNTIME_ERROR_OFFSET as i32));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        function.instruction(&Instruction::I32Const(GLOBAL_HEAP_CURSOR_OFFSET as i32));
        function.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
    } else {
        // aux decls load the current global cursor (may have been advanced by caller).
        function.instruction(&Instruction::I32Const(GLOBAL_HEAP_CURSOR_OFFSET as i32));
        function.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
    }
    function.instruction(&Instruction::LocalSet(alloc_cursor_local));
    function.instruction(&Instruction::I32Const(initial_floor));
    function.instruction(&Instruction::LocalSet(heap_floor_local));
    Ok(())
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
    emit_general_module_with_aux_and_options(instrs, aux_decls, layout, EmitOptions::default())
}

pub(crate) fn emit_general_module_with_aux_and_options(
    instrs: &[WasmBackendInstr],
    aux_decls: &[AuxDecl],
    layout: &MemoryLayout,
    options: EmitOptions,
) -> Result<Vec<u8>, CodegenError> {
    let static_strings = StaticStringPool::build_from_all(
        std::iter::once(instrs).chain(aux_decls.iter().map(|d| d.instrs.as_slice())),
    )?;
    if layout.heap_base + static_strings.bytes_used + 4 > STATIC_STRING_LIMIT {
        return Err(CodegenError {
            message: "gen_lower/emit: static string pool leaves no room for runtime stdin buffer"
                .to_string(),
        });
    }
    let mut record_ctor_tags = HashMap::new();
    for instr in std::iter::once(instrs).chain(aux_decls.iter().map(|d| d.instrs.as_slice())) {
        for nested in collect_all_instrs(instr) {
            if let WasmBackendInstr::RecordLit { constructor, .. } = nested
                && !record_ctor_tags.contains_key(constructor)
            {
                let next = record_ctor_tags.len() as u32;
                record_ctor_tags.insert(constructor.clone(), next);
            }
        }
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

    // Detect which IndirectCall arities are used, and also whether ListEach/ListMap
    // (which use arity-1 indirect call internally) are present.
    let uses_indirect_call_1 = all_slices_iter().any(|slice| {
        collect_all_instrs(slice).iter().any(|i| {
            matches!(
                i,
                WasmBackendInstr::IndirectCall { arity: 1 }
                    | WasmBackendInstr::ListEach { .. }
                    | WasmBackendInstr::ListMap { .. }
            )
        })
    });
    let uses_indirect_call_2 = all_slices_iter().any(|slice| {
        collect_all_instrs(slice).iter().any(|i| {
            matches!(
                i,
                WasmBackendInstr::IndirectCall { arity: 2 }
                    | WasmBackendInstr::IndirectCall { arity: 1 }
                    | WasmBackendInstr::ListEach { .. }
                    | WasmBackendInstr::ListMap { .. }
            )
        })
    });
    let uses_indirect_call_3 = all_slices_iter().any(|slice| {
        collect_all_instrs(slice)
            .iter()
            .any(|i| matches!(i, WasmBackendInstr::IndirectCall { arity: 2 }))
    });
    // `uses_indirect_call` is true when any kind of indirect call requires a funcref table.
    let uses_indirect_call = uses_indirect_call_1 || uses_indirect_call_2 || uses_indirect_call_3;
    let indirect_call_type_idx_1: Option<u32> = if uses_indirect_call_1 {
        let idx = types.len();
        types.ty().function([ValType::I64], [ValType::I64]);
        Some(idx)
    } else {
        None
    };
    let indirect_call_type_idx_2: Option<u32> = if uses_indirect_call_2 {
        let idx = types.len();
        types
            .ty()
            .function([ValType::I64, ValType::I64], [ValType::I64]);
        Some(idx)
    } else {
        None
    };
    let indirect_call_type_idx_3: Option<u32> = if uses_indirect_call_3 {
        let idx = types.len();
        types
            .ty()
            .function([ValType::I64, ValType::I64, ValType::I64], [ValType::I64]);
        Some(idx)
    } else {
        None
    };

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

    // Build decl_name → table slot index for PushFuncHandle resolution.
    // Table slot N corresponds to aux decl N (0-based); main is excluded from the table.
    let decl_table_slots: HashMap<String, u32> = aux_decls
        .iter()
        .enumerate()
        .map(|(i, decl)| (decl.decl_name.clone(), i as u32))
        .collect();

    // Table section (Wasm section order: Type, Import, Function, Table, Memory, ...).
    // Table is only emitted when IndirectCall is used and there are aux decls to populate it.
    // Slot N corresponds to aux decl N; main is excluded (incompatible type).
    if uses_indirect_call && !aux_decls.is_empty() {
        let mut tables = TableSection::new();
        tables.table(TableType {
            element_type: RefType::FUNCREF,
            minimum: aux_decls.len() as u64,
            maximum: None,
            table64: false,
            shared: false,
        });
        module.section(&tables);
    }

    // Memory section
    let mut memories = MemorySection::new();
    memories.memory(options.memory_config.memory_type());
    module.section(&memories);

    // Export section — `_start` is always main (index stable across aux additions).
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export("_start", ExportKind::Func, main_func_idx);
    module.section(&exports);

    // Element section (Wasm section order: ..., Export, Element, Code, ...).
    // Populate table slot N with the Wasm function index for aux decl N.
    if uses_indirect_call && !aux_decls.is_empty() {
        let aux_func_indices: Vec<u32> = (0..aux_decls.len() as u32)
            .map(|i| main_func_idx + 1 + i)
            .collect();
        let offset = ConstExpr::i32_const(0);
        let mut elements = ElementSection::new();
        // `table_index: None` forces MVP encoding (table 0, funcref type).
        elements.active(
            None,
            &offset,
            Elements::Functions(std::borrow::Cow::Borrowed(&aux_func_indices)),
        );
        module.section(&elements);
    }

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
        let mut ctx = EmitContext::with_module_tables(
            decl_func_indices.clone(),
            decl_table_slots.clone(),
            indirect_call_type_idx_1,
            indirect_call_type_idx_2,
            indirect_call_type_idx_3,
            record_ctor_tags.clone(),
        );
        // main is the root caller; no outer caller needs to reload the cursor, so no epilogue sync.
        emit_function_body(
            &mut function,
            &mut ctx,
            instrs,
            layout,
            main_named_i64_count,
            main_helper_i64_scratch_count,
            main_i32_base,
            &static_strings,
            options,
            false, // emit_epilogue_cursor_sync
        )?;
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
        let mut ctx = EmitContext::with_module_tables(
            decl_func_indices.clone(),
            decl_table_slots.clone(),
            indirect_call_type_idx_1,
            indirect_call_type_idx_2,
            indirect_call_type_idx_3,
            record_ctor_tags.clone(),
        );
        for param_name in &decl.param_names {
            // Wasm assigns local indices for params in declaration order before any body locals.
            // We mirror that: declare each param name with an explicit local index.
            ctx.locals.insert(param_name.clone(), ctx.next_local);
            ctx.next_local += 1;
        }
        // Aux decls write the final alloc cursor back to the global slot before returning
        // so callers that use call_indirect (not DeclCall) can reload the updated cursor.
        emit_function_body(
            &mut function,
            &mut ctx,
            &decl.instrs,
            layout,
            aux_named_i64_count,
            aux_helper_i64_scratch_count,
            aux_i32_base,
            &static_strings,
            options,
            true, // emit_epilogue_cursor_sync
        )?;
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

#[allow(clippy::too_many_arguments)]
fn emit_instrs(
    function: &mut Function,
    ctx: &mut EmitContext,
    instrs: &[WasmBackendInstr],
    layout: &MemoryLayout,
    named_i64_count: u32,
    helper_i64_scratch_count: u32,
    i32_base: u32,
    static_strings: &StaticStringPool,
    options: EmitOptions,
    function_returns_i64: bool,
) -> Result<(), CodegenError> {
    emit_instrs_with_heap_depth(
        function,
        ctx,
        instrs,
        layout,
        named_i64_count,
        helper_i64_scratch_count,
        i32_base,
        static_strings,
        options,
        function_returns_i64,
        0,
    )
}

#[allow(clippy::too_many_arguments)]
fn emit_instrs_with_heap_depth(
    function: &mut Function,
    ctx: &mut EmitContext,
    instrs: &[WasmBackendInstr],
    layout: &MemoryLayout,
    named_i64_count: u32,
    helper_i64_scratch_count: u32,
    i32_base: u32,
    static_strings: &StaticStringPool,
    options: EmitOptions,
    function_returns_i64: bool,
    heap_base_depth: u32,
) -> Result<(), CodegenError> {
    let iovec_offset = layout.iovec_offset as i32;
    let nread_offset = layout.nwritten_offset as i32;
    let buffer_ptr = layout.heap_base as i32;
    let buffer_len =
        (STATIC_STRING_LIMIT - layout.heap_base - static_strings.bytes_used) as i32 - 4; // reserve 4 bytes for len prefix
    let newline_ptr = (layout.heap_base - 1) as i32;
    let helper_i64_base = i32_base
        .checked_sub(helper_i64_scratch_count)
        .ok_or_else(|| CodegenError {
            message: "gen_lower/emit: helper i64 scratch base underflow".to_string(),
        })?;
    // `helper_state` is constructed when bump allocator / i32 scratch is needed.
    // BinOp Mul/Div/Mod need only i64 scratch; those are provided via `helper_i64_base` directly
    // in `emit_bin_op`, without requiring the full HelperEmitState.
    // `needs_helper_state` is the single source of truth for when the helper pool is required.
    let helper_state = if needs_helper_state(instrs) {
        let alloc_cursor_local = i32_base + HELPER_ALLOC_CURSOR_OFFSET;
        let heap_floor_local = i32_base + HELPER_HEAP_FLOOR_OFFSET;
        Some(HelperEmitState {
            i64_base: helper_i64_base,
            i32_base,
            alloc_cursor_local,
            heap_floor_local,
            function_returns_i64,
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

            WasmBackendInstr::EffectOp { op } => {
                emit_effect_op(
                    function,
                    op,
                    iovec_offset,
                    nread_offset,
                    buffer_ptr,
                    buffer_len,
                    newline_ptr,
                    i32_base,
                    helper_state.as_ref(),
                    options.effect_emit_strategy,
                )?;
            }

            WasmBackendInstr::Intrinsic { intrinsic } => {
                emit_helper_call(function, ctx, *intrinsic, i32_base, helper_state.as_ref())?;
            }

            WasmBackendInstr::SplitEachPrint {
                text_local,
                sep_bytes,
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
                let text_idx = ctx.get(text_local)?;
                emit_split_each_print(
                    function,
                    text_idx,
                    sep_bytes[0],
                    op.append_newline(),
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
                let text_idx = ctx.get(text_local)?;
                emit_split_get_print(
                    function,
                    ctx,
                    text_idx,
                    sep_bytes[0],
                    index,
                    op.append_newline(),
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
                emit_instrs_with_heap_depth(
                    function,
                    ctx,
                    then_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                    heap_base_depth,
                )?;
                function.instruction(&Instruction::Else);
                emit_instrs_with_heap_depth(
                    function,
                    ctx,
                    else_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                    heap_base_depth,
                )?;
                function.instruction(&Instruction::End);
            }

            WasmBackendInstr::DeclCall { decl_name } => {
                let func_idx = ctx.decl_func_idx(decl_name)?;
                emit_heap_aware_direct_call(function, func_idx, helper_state.as_ref());
            }

            WasmBackendInstr::CaseMatch {
                scrutinee_local,
                arms,
            } => {
                emit_case_match(
                    function,
                    ctx,
                    scrutinee_local,
                    arms,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    &helper_state,
                    options,
                    function_returns_i64,
                )?;
            }

            WasmBackendInstr::ListLit { element_instrs } => {
                let hs = require_helper_state(helper_state, "ListLit")?;
                let s_list_ptr = hs.i32_base + HELPER_SCRATCH_I32 + heap_base_depth;
                let s_alloc_ptr = hs.i32_base + HS_AUX_PTR;
                let s_alloc_size = hs.i32_base + HS_ALLOC_SIZE;
                let n = element_instrs.len() as i32;
                // alloc_size = 4 + 8 * n
                function.instruction(&Instruction::I32Const(4 + 8 * n));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, &hs, s_alloc_size, s_alloc_ptr);
                function.instruction(&Instruction::LocalGet(s_alloc_ptr));
                function.instruction(&Instruction::LocalSet(s_list_ptr));
                // Write length header: i32 at [s_list_ptr + 0]
                function.instruction(&Instruction::LocalGet(s_list_ptr));
                function.instruction(&Instruction::I32Const(n));
                function.instruction(&Instruction::I32Store(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                // Write each element: i64 at [s_list_ptr + 4 + i*8]
                for (i, elem_instrs) in element_instrs.iter().enumerate() {
                    // address = s_list_ptr + 4 + i*8
                    function.instruction(&Instruction::LocalGet(s_list_ptr));
                    function.instruction(&Instruction::I32Const(4 + 8 * i as i32));
                    function.instruction(&Instruction::I32Add);
                    // push element value (tagged i64)
                    emit_instrs_with_heap_depth(
                        function,
                        ctx,
                        elem_instrs,
                        layout,
                        named_i64_count,
                        helper_i64_scratch_count,
                        i32_base,
                        static_strings,
                        options,
                        function_returns_i64,
                        heap_base_depth + 1,
                    )?;
                    function.instruction(&Instruction::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                // Push tagged list pointer as result
                emit_push_tagged_ptr(function, s_list_ptr, TAG_LIST);
            }

            WasmBackendInstr::TupleLit { element_instrs } => {
                let hs = require_helper_state(helper_state, "TupleLit")?;
                let s_tuple_ptr = hs.i32_base + HELPER_SCRATCH_I32 + heap_base_depth;
                let s_alloc_ptr = hs.i32_base + HS_AUX_PTR;
                let s_alloc_size = hs.i32_base + HS_ALLOC_SIZE;
                let n = element_instrs.len() as i32;
                function.instruction(&Instruction::I32Const(4 + 8 * n));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, &hs, s_alloc_size, s_alloc_ptr);
                function.instruction(&Instruction::LocalGet(s_alloc_ptr));
                function.instruction(&Instruction::LocalSet(s_tuple_ptr));
                function.instruction(&Instruction::LocalGet(s_tuple_ptr));
                function.instruction(&Instruction::I32Const(n));
                function.instruction(&Instruction::I32Store(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                for (i, elem_instrs) in element_instrs.iter().enumerate() {
                    function.instruction(&Instruction::LocalGet(s_tuple_ptr));
                    function.instruction(&Instruction::I32Const(4 + 8 * i as i32));
                    function.instruction(&Instruction::I32Add);
                    emit_instrs_with_heap_depth(
                        function,
                        ctx,
                        elem_instrs,
                        layout,
                        named_i64_count,
                        helper_i64_scratch_count,
                        i32_base,
                        static_strings,
                        options,
                        function_returns_i64,
                        heap_base_depth + 1,
                    )?;
                    function.instruction(&Instruction::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                emit_push_tagged_ptr(function, s_tuple_ptr, TAG_TUPLE);
            }

            WasmBackendInstr::RecordLit {
                constructor,
                field_instrs,
            } => {
                let hs = require_helper_state(helper_state, "RecordLit")?;
                let ctor_tag = ctx.record_ctor_tag(constructor)? as i64;
                let s_record_ptr = hs.i32_base + HELPER_SCRATCH_I32 + heap_base_depth;
                let s_alloc_ptr = hs.i32_base + HS_AUX_PTR;
                let s_alloc_size = hs.i32_base + HS_ALLOC_SIZE;
                let n = field_instrs.len() as i32;
                function.instruction(&Instruction::I32Const(8 + 8 * n));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, &hs, s_alloc_size, s_alloc_ptr);
                function.instruction(&Instruction::LocalGet(s_alloc_ptr));
                function.instruction(&Instruction::LocalSet(s_record_ptr));
                function.instruction(&Instruction::LocalGet(s_record_ptr));
                function.instruction(&Instruction::I64Const(ctor_tag));
                function.instruction(&Instruction::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
                for (i, field_instrs) in field_instrs.iter().enumerate() {
                    function.instruction(&Instruction::LocalGet(s_record_ptr));
                    function.instruction(&Instruction::I32Const(8 + 8 * i as i32));
                    function.instruction(&Instruction::I32Add);
                    emit_instrs_with_heap_depth(
                        function,
                        ctx,
                        field_instrs,
                        layout,
                        named_i64_count,
                        helper_i64_scratch_count,
                        i32_base,
                        static_strings,
                        options,
                        function_returns_i64,
                        heap_base_depth + 1,
                    )?;
                    function.instruction(&Instruction::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                emit_push_tagged_ptr(function, s_record_ptr, TAG_RECORD);
            }

            WasmBackendInstr::PushFuncHandle { decl_name } => {
                let table_slot = ctx.decl_table_slot(decl_name)?;
                function.instruction(&Instruction::I64Const(
                    crate::gen_lower::value::encode_func_handle(table_slot),
                ));
            }

            WasmBackendInstr::IndirectCall { arity } => {
                emit_indirect_call_dispatch(
                    function,
                    ctx,
                    helper_i64_base,
                    *arity,
                    helper_state.as_ref(),
                )?;
            }

            WasmBackendInstr::ListEachEffect { list_instrs, op } => {
                let _hs = require_helper_state(helper_state, "ListEachEffect")?;
                emit_instrs(
                    function,
                    ctx,
                    list_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                emit_list_each_effect(
                    function,
                    op.append_newline(),
                    i32_base,
                    iovec_offset,
                    nread_offset,
                    newline_ptr,
                );
            }

            WasmBackendInstr::ListEach {
                list_instrs,
                func_instrs,
            } => {
                let hs = require_helper_state(helper_state, "ListEach")?;
                let type_idx = ctx.indirect_call_type_idx(1)?;
                let closure_type_idx = ctx.indirect_call_type_idx(2)?;
                emit_instrs(
                    function,
                    ctx,
                    list_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                emit_instrs(
                    function,
                    ctx,
                    func_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                emit_list_each(function, &hs, type_idx, closure_type_idx);
            }

            WasmBackendInstr::ListMap {
                list_instrs,
                func_instrs,
            } => {
                let hs = require_helper_state(helper_state, "ListMap")?;
                let type_idx = ctx.indirect_call_type_idx(1)?;
                let closure_type_idx = ctx.indirect_call_type_idx(2)?;
                emit_instrs(
                    function,
                    ctx,
                    list_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                emit_instrs(
                    function,
                    ctx,
                    func_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                emit_list_map(function, &hs, type_idx, closure_type_idx)?;
            }

            WasmBackendInstr::TupleGet { tuple_local, index } => {
                // Load the tagged tuple pointer from the local.
                let tuple_slot = ctx.get(tuple_local)?;
                function.instruction(&Instruction::LocalGet(tuple_slot));
                // Unwrap the tagged i64 to an i32 pointer.
                // Tag occupies the high 4 bits; the payload in the low 32 bits is the pointer.
                function.instruction(&Instruction::I32WrapI64);
                // Compute byte offset: 4 (arity header) + 8 * index.
                let field_offset = 8i32
                    .checked_mul(*index as i32)
                    .ok_or_else(|| CodegenError {
                        message: format!(
                            "gen_lower/emit: TupleGet index {} overflows i32 byte offset",
                            index
                        ),
                    })?;
                let byte_offset = 4i32.checked_add(field_offset).ok_or_else(|| CodegenError {
                    message: format!(
                        "gen_lower/emit: TupleGet index {} overflows i32 byte offset",
                        index
                    ),
                })?;
                function.instruction(&Instruction::I32Const(byte_offset));
                function.instruction(&Instruction::I32Add);
                // Load the i64 field value.
                function.instruction(&Instruction::I64Load(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }

            // -------------------------------------------------------------------
            // Closure record and mutable cell instructions
            // -------------------------------------------------------------------
            WasmBackendInstr::CreateClosure {
                func_handle_instrs,
                slot_instrs,
            } => {
                // Layout: (func_handle: i64, slots: [i64; N]) = 8 + 8*N bytes.
                //
                // The closure base pointer is saved to a depth-indexed spill local immediately
                // after allocation so nested heap allocations cannot corrupt the outer pointer.
                let hs = require_helper_state(helper_state, "CreateClosure")?;
                let s_closure_ptr = hs.i32_base + HELPER_SCRATCH_I32 + heap_base_depth;
                let s_aux = hs.i32_base + HS_AUX_PTR; // temp for alloc, then discarded
                let s_alloc_size = hs.i32_base + HS_ALLOC_SIZE;
                let n = slot_instrs.len() as i32;
                // alloc_size = 8 + 8 * n  (8 bytes for func_handle, 8 bytes per slot)
                function.instruction(&Instruction::I32Const(8 + 8 * n));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                // Allocate into HS_AUX_PTR, then copy to the dedicated HS_CLOSURE_BASE_PTR.
                emit_alloc_from_top(function, &hs, s_alloc_size, s_aux);
                function.instruction(&Instruction::LocalGet(s_aux));
                function.instruction(&Instruction::LocalSet(s_closure_ptr));
                // Store func_handle (TAG_FUNC i64) at offset 0.
                function.instruction(&Instruction::LocalGet(s_closure_ptr));
                emit_instrs_with_heap_depth(
                    function,
                    ctx,
                    func_handle_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                    heap_base_depth + 1,
                )?;
                function.instruction(&Instruction::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
                // Store each slot at offset 8 + 8*i.
                // slot_instrs may allocate (overwriting HS_AUX_PTR), but s_closure_ptr is safe.
                for (i, slot_instrs_i) in slot_instrs.iter().enumerate() {
                    function.instruction(&Instruction::LocalGet(s_closure_ptr));
                    function.instruction(&Instruction::I32Const(8 + 8 * i as i32));
                    function.instruction(&Instruction::I32Add);
                    emit_instrs_with_heap_depth(
                        function,
                        ctx,
                        slot_instrs_i,
                        layout,
                        named_i64_count,
                        helper_i64_scratch_count,
                        i32_base,
                        static_strings,
                        options,
                        function_returns_i64,
                        heap_base_depth + 1,
                    )?;
                    function.instruction(&Instruction::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                // Push TAG_CLOSURE-tagged pointer using the dedicated closure base slot.
                emit_push_tagged_ptr(function, s_closure_ptr, TAG_CLOSURE);
            }

            WasmBackendInstr::LoadClosureSlot {
                closure_local,
                slot_index,
            } => {
                // Load the TAG_CLOSURE pointer from the local, decode it to i32, add slot offset.
                let slot = ctx.get(closure_local)?;
                function.instruction(&Instruction::LocalGet(slot));
                function.instruction(&Instruction::I32WrapI64);
                let byte_offset = 8i32
                    .checked_add(8i32.checked_mul(*slot_index as i32).ok_or_else(|| {
                        CodegenError {
                            message: format!(
                                "gen_lower/emit: LoadClosureSlot index {} overflows",
                                slot_index
                            ),
                        }
                    })?)
                    .ok_or_else(|| CodegenError {
                        message: format!(
                            "gen_lower/emit: LoadClosureSlot index {} overflows byte offset",
                            slot_index
                        ),
                    })?;
                function.instruction(&Instruction::I32Const(byte_offset));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::I64Load(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }

            WasmBackendInstr::AllocMutableCell { init_instrs } => {
                // Allocate 8 bytes, store init value at offset 0, push TAG_CELL-tagged ptr.
                //
                // Safety note: same HS_AUX_PTR clobber risk as CreateClosure if
                // init_instrs contains a nested heap allocation. The lowering pass must ensure
                // init_instrs does not allocate; typically init_instrs is a single
                // I64Const or LoadLocal, never a heap-allocating sequence.
                let hs = require_helper_state(helper_state, "AllocMutableCell")?;
                let s_cell_ptr = hs.i32_base + HS_AUX_PTR;
                let s_alloc_size = hs.i32_base + HS_ALLOC_SIZE;
                function.instruction(&Instruction::I32Const(8));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, &hs, s_alloc_size, s_cell_ptr);
                // Store init value at offset 0.
                function.instruction(&Instruction::LocalGet(s_cell_ptr));
                emit_instrs(
                    function,
                    ctx,
                    init_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                function.instruction(&Instruction::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
                // Push TAG_CELL-tagged pointer as result.
                emit_push_tagged_ptr(function, s_cell_ptr, TAG_CELL);
            }

            WasmBackendInstr::LoadCellValue => {
                // Stack top: TAG_CELL-tagged i64 pointer.
                // Decode to i32 pointer, load i64 at offset 0.
                function.instruction(&Instruction::I32WrapI64);
                function.instruction(&Instruction::I64Load(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }

            WasmBackendInstr::StoreCellValue {
                cell_ptr_instrs,
                value_instrs,
            } => {
                // Emit cell pointer (TAG_CELL i64), decode to i32 pointer.
                emit_instrs(
                    function,
                    ctx,
                    cell_ptr_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                function.instruction(&Instruction::I32WrapI64);
                // Emit the new value (i64) to store.
                emit_instrs(
                    function,
                    ctx,
                    value_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                // Store value into cell at offset 0.
                function.instruction(&Instruction::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
                // No stack result: the caller (Assign lowering) emits the Unit value.
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
        BackendIntrinsic::ValueToString
        | BackendIntrinsic::StringEachGraphemeCount
        | BackendIntrinsic::StringEachGraphemeState
        | BackendIntrinsic::StringConcat
        | BackendIntrinsic::StringGraphemesList
        | BackendIntrinsic::StringSplitLines => {
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
        BackendIntrinsic::ListConcat => emit_list_concat_helper(function, helper_state),
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

/// Emit the body of one Wasm function: helper-state prologue, instructions, optional
/// epilogue cursor sync, and the terminating `End` instruction.
///
/// `emit_epilogue_cursor_sync`: pass `true` for aux decls (callers need the updated cursor
/// after `call_indirect`); pass `false` for `main`/`_start` (no caller to reload from it).
#[allow(clippy::too_many_arguments)]
fn emit_function_body(
    function: &mut Function,
    ctx: &mut EmitContext,
    instrs: &[WasmBackendInstr],
    layout: &MemoryLayout,
    named_i64_count: u32,
    helper_i64_scratch_count: u32,
    i32_base: u32,
    static_strings: &StaticStringPool,
    options: EmitOptions,
    emit_epilogue_cursor_sync: bool,
) -> Result<(), CodegenError> {
    let has_heap = needs_helper_state(instrs);
    if has_heap {
        initialize_helper_state_locals(
            function,
            layout,
            i32_base,
            static_strings,
            !emit_epilogue_cursor_sync,
        )?;
    }
    emit_instrs(
        function,
        ctx,
        instrs,
        layout,
        named_i64_count,
        helper_i64_scratch_count,
        i32_base,
        static_strings,
        options,
        emit_epilogue_cursor_sync,
    )?;
    if has_heap && emit_epilogue_cursor_sync {
        let alloc_cursor_local = i32_base + HELPER_ALLOC_CURSOR_OFFSET;
        emit_sync_cursor_to_global(function, alloc_cursor_local);
    }
    function.instruction(&Instruction::End);
    Ok(())
}

/// Emit a direct Wasm `call` with heap-cursor sync.
///
/// Writes the local alloc cursor to the global slot before the call, then reloads it
/// after, so that any heap allocations inside the callee are visible to the caller.
/// When `helper_state` is `None` (caller has no heap state), the sync is skipped.
fn emit_heap_aware_direct_call(
    function: &mut Function,
    func_idx: u32,
    helper_state: Option<&HelperEmitState>,
) {
    if let Some(hs) = helper_state {
        emit_sync_cursor_to_global(function, hs.alloc_cursor_local);
    }
    function.instruction(&Instruction::Call(func_idx));
    if let Some(hs) = helper_state {
        emit_return_if_runtime_error(function, hs);
        emit_sync_cursor_from_global(function, hs.alloc_cursor_local);
    }
}

fn emit_return_if_runtime_error(function: &mut Function, helper_state: &HelperEmitState) {
    function.instruction(&Instruction::I32Const(GLOBAL_RUNTIME_ERROR_OFFSET as i32));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Else);
    if helper_state.function_returns_i64 {
        function.instruction(&Instruction::I64Const(0));
    }
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);
}

/// Emit `i32.const GLOBAL_HEAP_CURSOR_OFFSET; local.get alloc_cursor_local; i32.store`
/// to flush the local alloc cursor to the global persistent slot before a `call_indirect`.
fn emit_sync_cursor_to_global(function: &mut Function, alloc_cursor_local: u32) {
    function.instruction(&Instruction::I32Const(GLOBAL_HEAP_CURSOR_OFFSET as i32));
    function.instruction(&Instruction::LocalGet(alloc_cursor_local));
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
}

/// Emit `i32.const GLOBAL_HEAP_CURSOR_OFFSET; i32.load; local.set alloc_cursor_local`
/// to reload the alloc cursor from the global slot after a `call_indirect`.
fn emit_sync_cursor_from_global(function: &mut Function, alloc_cursor_local: u32) {
    function.instruction(&Instruction::I32Const(GLOBAL_HEAP_CURSOR_OFFSET as i32));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(alloc_cursor_local));
}

/// Emit a `call_indirect` dispatch that handles both plain funcrefs and capturing closures.
///
/// The callee is already stored in `callee_local` (i64 tagged value).
/// The arguments are already stored in `arg_locals` (in left-to-right order, i64 each).
///
/// Emitted Wasm (pseudo-code):
/// ```text
/// if callee_local >> 60 == TAG_CLOSURE {   // sync + If
///   [callee_local, arg_locals..., load_func_ptr(callee_local)]  call_indirect closure_type
/// } else {
///   [arg_locals..., callee_low32]  call_indirect plain_type
/// }                                // End + sync
/// ```
///
/// The heap-cursor is synced around the `If/End` block when `hs` is `Some`.
fn emit_callable_dispatch(
    function: &mut Function,
    callee_local: u32,
    arg_locals: &[u32],
    plain_type_idx: u32,
    closure_type_idx: u32,
    hs: Option<&HelperEmitState>,
) {
    // TAG_CLOSURE check: callee >> 60 == TAG_CLOSURE
    function.instruction(&Instruction::LocalGet(callee_local));
    function.instruction(&Instruction::I64Const(60));
    function.instruction(&Instruction::I64ShrU);
    function.instruction(&Instruction::I64Const(i64::from(TAG_CLOSURE)));
    function.instruction(&Instruction::I64Eq);
    if let Some(hs) = hs {
        emit_sync_cursor_to_global(function, hs.alloc_cursor_local);
    }
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        wasm_encoder::ValType::I64,
    )));
    // Closure branch: push callee (env ptr), then args, then func_ptr from closure[0].
    function.instruction(&Instruction::LocalGet(callee_local));
    for &arg in arg_locals {
        function.instruction(&Instruction::LocalGet(arg));
    }
    function.instruction(&Instruction::LocalGet(callee_local));
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::CallIndirect {
        type_index: closure_type_idx,
        table_index: 0,
    });
    function.instruction(&Instruction::Else);
    // Plain branch: push args, then callee low32 as table slot.
    for &arg in arg_locals {
        function.instruction(&Instruction::LocalGet(arg));
    }
    function.instruction(&Instruction::LocalGet(callee_local));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::CallIndirect {
        type_index: plain_type_idx,
        table_index: 0,
    });
    function.instruction(&Instruction::End);
    if let Some(hs) = hs {
        emit_sync_cursor_from_global(function, hs.alloc_cursor_local);
    }
}

fn emit_indirect_call_dispatch(
    function: &mut Function,
    ctx: &EmitContext,
    helper_i64_base: u32,
    arity: u8,
    hs: Option<&HelperEmitState>,
) -> Result<(), CodegenError> {
    if arity == 0 || u32::from(arity + 1) > HELPER_SCRATCH_I64 {
        return Err(CodegenError {
            message: format!("gen_lower/emit: IndirectCall with unsupported arity {arity}"),
        });
    }

    let callee_local = helper_i64_base;
    let mut arg_locals = Vec::with_capacity(usize::from(arity));
    for offset in 1..=u32::from(arity) {
        arg_locals.push(helper_i64_base + offset);
    }

    // Stack before spill: [arg0, ..., argN-1, callee]. Pop in reverse order so the
    // scratch locals preserve left-to-right argument order for shared callable dispatch.
    function.instruction(&Instruction::LocalSet(callee_local));
    for &arg_local in arg_locals.iter().rev() {
        function.instruction(&Instruction::LocalSet(arg_local));
    }

    let plain_type_idx = ctx.indirect_call_type_idx(arity)?;
    let closure_type_idx = ctx.indirect_call_type_idx(arity + 1)?;
    emit_callable_dispatch(
        function,
        callee_local,
        &arg_locals,
        plain_type_idx,
        closure_type_idx,
        hs,
    );
    if let Some(hs) = hs {
        function.instruction(&Instruction::LocalSet(callee_local));
        emit_return_if_runtime_error(function, hs);
        function.instruction(&Instruction::LocalGet(callee_local));
    }
    Ok(())
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
    function.instruction(&Instruction::LocalGet(helper_state.heap_floor_local));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::LocalTee(result_local));
    function.instruction(&Instruction::LocalGet(size_local));
    function.instruction(&Instruction::I32LtU);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // `result_local` currently holds available bytes between the top-down cursor
    // and the heap floor. Grow by the extra bytes required for this allocation.
    function.instruction(&Instruction::LocalGet(size_local));
    function.instruction(&Instruction::LocalGet(result_local));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::I32Const((WASM_PAGE_BYTES - 1) as i32));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(WASM_PAGE_BYTES as i32));
    function.instruction(&Instruction::I32DivU);
    function.instruction(&Instruction::LocalTee(result_local));
    function.instruction(&Instruction::MemoryGrow(0));
    function.instruction(&Instruction::I32Const(-1));
    function.instruction(&Instruction::I32Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_memory_exhaustion_abort(function, helper_state);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(result_local));
    function.instruction(&Instruction::I32Const(WASM_PAGE_BYTES as i32));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::LocalSet(result_local));
    function.instruction(&Instruction::LocalGet(helper_state.alloc_cursor_local));
    function.instruction(&Instruction::LocalGet(result_local));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(helper_state.alloc_cursor_local));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(helper_state.alloc_cursor_local));
    function.instruction(&Instruction::LocalGet(size_local));
    function.instruction(&Instruction::I32Sub);
    function.instruction(&Instruction::I32Const(!7));
    function.instruction(&Instruction::I32And);
    function.instruction(&Instruction::LocalTee(result_local));
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
    let s_text_ptr = helper_state.i32_base + HS_TEXT_PTR;
    let s_text_len = helper_state.i32_base + HS_TEXT_LEN;
    let s_sep_ptr = helper_state.i32_base + HS_SEP_PTR;
    let s_sep_len = helper_state.i32_base + HS_SEP_LEN;
    let s_pos = helper_state.i32_base + HS_SCAN_POS;
    let s_start = helper_state.i32_base + HS_SEG_START;
    let s_item_count = helper_state.i32_base + HS_ITEM_COUNT;
    let s_aux_ptr = helper_state.i32_base + HS_AUX_PTR;
    let s_list_ptr = helper_state.i32_base + HS_LIST_PTR;
    let s_alloc_size = helper_state.i32_base + HS_ALLOC_SIZE;
    let s_iter = helper_state.i32_base + HS_ITER;

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
    let s_list_ptr = helper_state.i32_base + HS_TEXT_PTR;
    let s_list_len = helper_state.i32_base + HS_TEXT_LEN;
    let s_index = helper_state.i32_base + HS_SEP_PTR;

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
    let s_ptr = helper_state.i32_base + HS_TEXT_PTR;
    let s_len = helper_state.i32_base + HS_TEXT_LEN;
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
    let s_list_ptr = helper_state.i32_base + HS_TEXT_PTR;
    let s_list_len = helper_state.i32_base + HS_TEXT_LEN;
    let s_string_ptr = helper_state.i32_base + HS_SEP_PTR;
    let s_string_len = helper_state.i32_base + HS_SEP_LEN;
    let s_new_list_ptr = helper_state.i32_base + HS_AUX_PTR;
    let s_alloc_size = helper_state.i32_base + HS_ALLOC_SIZE;
    let s_iter = helper_state.i32_base + HS_ITER;

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

fn emit_list_concat_helper(
    function: &mut Function,
    helper_state: &HelperEmitState,
) -> Result<(), CodegenError> {
    let tail_i64 = helper_state.i64_base;
    let prefix_i64 = helper_state.i64_base + 1;
    let s_prefix_ptr = helper_state.i32_base + HS_TEXT_PTR;
    let s_prefix_len = helper_state.i32_base + HS_TEXT_LEN;
    let s_tail_ptr = helper_state.i32_base + HS_SEP_PTR;
    let s_tail_len = helper_state.i32_base + HS_SEP_LEN;
    let s_new_list_ptr = helper_state.i32_base + HS_AUX_PTR;
    let s_alloc_size = helper_state.i32_base + HS_ALLOC_SIZE;
    let s_iter = helper_state.i32_base + HS_ITER;

    function.instruction(&Instruction::LocalSet(tail_i64));
    function.instruction(&Instruction::LocalSet(prefix_i64));

    for list_i64 in [prefix_i64, tail_i64] {
        function.instruction(&Instruction::LocalGet(list_i64));
        function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
        function.instruction(&Instruction::I64And);
        function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
        function.instruction(&Instruction::I64Ne);
        function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        emit_abort(function);
        function.instruction(&Instruction::End);
    }

    function.instruction(&Instruction::LocalGet(prefix_i64));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(s_prefix_ptr));

    function.instruction(&Instruction::LocalGet(tail_i64));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(s_tail_ptr));

    function.instruction(&Instruction::LocalGet(s_prefix_ptr));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_prefix_len));

    function.instruction(&Instruction::LocalGet(s_tail_ptr));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_tail_len));

    function.instruction(&Instruction::LocalGet(s_prefix_len));
    function.instruction(&Instruction::LocalGet(s_tail_len));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_new_list_ptr);

    function.instruction(&Instruction::LocalGet(s_new_list_ptr));
    function.instruction(&Instruction::LocalGet(s_prefix_len));
    function.instruction(&Instruction::LocalGet(s_tail_len));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));

    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::LocalGet(s_prefix_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::LocalGet(s_new_list_ptr));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);

    function.instruction(&Instruction::LocalGet(s_prefix_ptr));
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

    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::LocalGet(s_tail_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::LocalGet(s_new_list_ptr));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_prefix_len));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);

    function.instruction(&Instruction::LocalGet(s_tail_ptr));
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
        IrBinOp::Or => {
            // Bool: TAG_BOOL<<60 | 0 (false) or TAG_BOOL<<60 | 1 (true).
            // (TAG<<60 | a) | (TAG<<60 | b) = TAG<<60 | (a | b).
            function.instruction(&Instruction::I64Or);
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
#[allow(clippy::too_many_arguments)]
fn emit_effect_op(
    function: &mut Function,
    op: &BackendEffectOp,
    iovec_offset: i32,
    nread_offset: i32,
    buffer_ptr: i32,
    buffer_len: i32,
    newline_ptr: i32,
    i32_base: u32,
    helper_state: Option<&HelperEmitState>,
    strategy: EffectEmitStrategy,
) -> Result<(), CodegenError> {
    match strategy {
        // The future stack-switching path is specified as an emit-layer swap only. Until the toolchain can
        // encode WasmFX suspend/resume instructions, the experimental path keeps
        // byte-for-byte parity with the shipped direct-call emitter.
        EffectEmitStrategy::Wb3DirectCall | EffectEmitStrategy::Wb3BWasmFxExperimental => {
            emit_effect_op_wb3_direct(
                function,
                op,
                iovec_offset,
                nread_offset,
                buffer_ptr,
                buffer_len,
                newline_ptr,
                i32_base,
                helper_state,
            )
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_effect_op_wb3_direct(
    function: &mut Function,
    op: &BackendEffectOp,
    iovec_offset: i32,
    nread_offset: i32,
    buffer_ptr: i32,
    buffer_len: i32,
    newline_ptr: i32,
    i32_base: u32,
    helper_state: Option<&HelperEmitState>,
) -> Result<(), CodegenError> {
    match op {
        BackendEffectOp::Read(BackendReadOp::Read) => {
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

        BackendEffectOp::Read(BackendReadOp::ReadLine) => {
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

        BackendEffectOp::Print(print_op) => {
            let append_newline = print_op.append_newline();
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

/// Emit `list.each`: for each element in a list, call a funcref via `call_indirect` and
/// discard the result.  The two top-of-stack i64 values are consumed (list, func).
///
/// Stack before: `[..., list_tagged: i64, func_tagged: i64]`
/// Stack after:  `[..., unit: i64]`
///
/// I32 scratch slots used: HS_LIST_PTR (8), HS_ITEM_COUNT (6), HS_ITER (10).
/// I64 scratch locals used: i64_base+0 (func handle), i64_base+1 (current element).
fn emit_list_each(
    function: &mut Function,
    hs: &HelperEmitState,
    indirect_call_type_idx: u32,
    closure_call_type_idx: u32,
) {
    let s_func = hs.i64_base; // i64 scratch: func handle
    let s_elem = hs.i64_base + 1; // i64 scratch: current element
    let s_list_ptr = hs.i32_base + HS_LIST_PTR; // i32 scratch: list pointer
    let s_list_len = hs.i32_base + HS_ITEM_COUNT; // i32 scratch: list length
    let s_iter = hs.i32_base + HS_ITER; // i32 scratch: loop counter

    // Stack: [list_tagged, func_tagged]
    function.instruction(&Instruction::LocalSet(s_func));

    // Decode list pointer from tagged i64 (lower 32 bits)
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(s_list_ptr));

    // list_len = mem[list_ptr]
    function.instruction(&Instruction::LocalGet(s_list_ptr));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_list_len));

    // iter = 0
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_iter));

    // Loop: for iter in 0..list_len
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));

    // if iter >= list_len: break
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));

    // elem = mem[list_ptr + 4 + iter * 8]
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
    function.instruction(&Instruction::LocalSet(s_elem));

    emit_callable_dispatch(
        function,
        s_func,
        &[s_elem],
        indirect_call_type_idx,
        closure_call_type_idx,
        Some(hs),
    );
    function.instruction(&Instruction::LocalSet(s_elem));
    emit_return_if_runtime_error(function, hs);

    // iter += 1
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Br(0)); // continue loop

    function.instruction(&Instruction::End); // end loop
    function.instruction(&Instruction::End); // end block

    // Result: tagged Unit
    function.instruction(&Instruction::I64Const(encode_unit()));
}

/// Emit `list.map`: for each element in a list, call a funcref via `call_indirect` and
/// collect results into a new heap-allocated list.
///
/// Stack before: `[..., list_tagged: i64, func_tagged: i64]`
/// Stack after:  `[..., result_list_tagged: i64]`
///
/// I32 scratch slots: HS_LIST_PTR (8), HS_ITEM_COUNT (6), HS_ITER (10),
///                    HS_AUX_PTR (7) for result list ptr, HS_ALLOC_SIZE (9).
/// I64 scratch locals: i64_base+0 (func handle), i64_base+1 (current element / result).
fn emit_list_map(
    function: &mut Function,
    hs: &HelperEmitState,
    indirect_call_type_idx: u32,
    closure_call_type_idx: u32,
) -> Result<(), CodegenError> {
    let s_func = hs.i64_base;
    let s_elem = hs.i64_base + 1;
    let s_list_ptr = hs.i32_base + HS_LIST_PTR;
    let s_list_len = hs.i32_base + HS_ITEM_COUNT;
    let s_iter = hs.i32_base + HS_ITER;
    let s_result_ptr = hs.i32_base + HS_AUX_PTR;
    let s_alloc_size = hs.i32_base + HS_ALLOC_SIZE;

    // Stack: [list_tagged, func_tagged]
    function.instruction(&Instruction::LocalSet(s_func));

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

    // alloc_size = 4 + 8 * list_len
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_alloc_size));

    // Allocate from top of heap: result_ptr = alloc_cursor - alloc_size (aligned down to 8)
    emit_alloc_from_top(function, hs, s_alloc_size, s_result_ptr);

    // Write list length header
    function.instruction(&Instruction::LocalGet(s_result_ptr));
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));

    // iter = 0
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_iter));

    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));

    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));

    // elem = mem[list_ptr + 4 + iter * 8]
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
    function.instruction(&Instruction::LocalSet(s_elem));

    emit_callable_dispatch(
        function,
        s_func,
        &[s_elem],
        indirect_call_type_idx,
        closure_call_type_idx,
        Some(hs),
    );
    function.instruction(&Instruction::LocalSet(s_elem));
    emit_return_if_runtime_error(function, hs);

    // result_ptr[4 + iter * 8] = mapped
    function.instruction(&Instruction::LocalGet(s_result_ptr));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Const(8));
    function.instruction(&Instruction::I32Mul);
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalGet(s_elem));
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));

    // iter += 1
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Br(0));

    function.instruction(&Instruction::End); // end loop
    function.instruction(&Instruction::End); // end block

    // Push tagged list pointer as result
    emit_push_tagged_ptr(function, s_result_ptr, TAG_LIST);
    Ok(())
}

/// Emit `list.each` with an effect callback (fused `ListEachEffect`).
///
/// Stack before: `[..., list_tagged: i64]`
/// Stack after:  `[..., unit: i64]`
///
/// Iterates the list, printing each tagged-string element via fd_write.
/// I32 scratch slots: offset 0 (ptr_local for Print), HS_LIST_PTR (8), HS_ITEM_COUNT (6),
/// HS_ITER (10).
fn emit_list_each_effect(
    function: &mut Function,
    append_newline: bool,
    i32_base: u32,
    iovec_offset: i32,
    nread_offset: i32,
    newline_ptr: i32,
) {
    let ptr_local = i32_base; // for Print code: str_ptr
    let s_list_ptr = i32_base + HS_LIST_PTR; // i32 scratch: list pointer
    let s_list_len = i32_base + HS_ITEM_COUNT; // i32 scratch: list length
    let s_iter = i32_base + HS_ITER; // i32 scratch: loop counter

    // Stack: [list_tagged]
    // Decode list pointer
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(s_list_ptr));

    // list_len = mem[list_ptr]
    function.instruction(&Instruction::LocalGet(s_list_ptr));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_list_len));

    // iter = 0
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::LocalSet(s_iter));

    // if list_len == 0: skip
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Else);

    // Loop
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));

    // if iter >= list_len: break
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&Instruction::I32GeU);
    function.instruction(&Instruction::BrIf(1));

    // elem = mem[list_ptr + 4 + iter * 8] (tagged-i64 string)
    // Decode tagged string ptr into ptr_local
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
    // Decode tagged string ptr (lower 32 bits)
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::LocalSet(ptr_local));

    // iovec[0] = ptr_local + 4 (data bytes)
    function.instruction(&Instruction::I32Const(iovec_offset));
    function.instruction(&Instruction::LocalGet(ptr_local));
    function.instruction(&Instruction::I32Const(4));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    // iovec[1] = mem[ptr_local] (length)
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
    function.instruction(&Instruction::Drop);

    if append_newline {
        function.instruction(&Instruction::I32Const(newline_ptr));
        function.instruction(&Instruction::I32Const(10));
        function.instruction(&Instruction::I32Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));
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

    // iter += 1
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Add);
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Br(0)); // continue

    function.instruction(&Instruction::End); // end loop
    function.instruction(&Instruction::End); // end block
    function.instruction(&Instruction::End); // end else (list_len != 0)

    // Result: tagged Unit
    function.instruction(&Instruction::I64Const(encode_unit()));
}

/// Emit a `CaseMatch` instruction: scrutinee into scratch, then an if/else arm chain.
///
/// Each arm tests the scrutinee against its pattern and emits the arm body.
/// The last arm must be `Wildcard` (unconditional). If the last arm is not
/// `Wildcard`, an `unreachable` is emitted after the chain.
///
/// Result type: every arm must leave exactly one tagged i64 on the Wasm stack.
/// The entire construct is wrapped in a block/br so all arm results go to the
/// same merge point (Wasm `block (result i64) ... end`).
#[allow(clippy::too_many_arguments)]
fn emit_case_match(
    function: &mut Function,
    ctx: &mut EmitContext,
    scrutinee_local: &str,
    arms: &[crate::gen_lower::backend_ir::CaseArmInstr],
    layout: &MemoryLayout,
    named_i64_count: u32,
    helper_i64_scratch_count: u32,
    i32_base: u32,
    static_strings: &StaticStringPool,
    helper_state: &Option<HelperEmitState>,
    options: EmitOptions,
    function_returns_i64: bool,
) -> Result<(), CodegenError> {
    use crate::gen_lower::backend_ir::BackendCasePattern;

    if arms.is_empty() {
        return Err(CodegenError {
            message: "gen_lower/emit: CaseMatch has no arms".to_string(),
        });
    }

    // The scrutinee value is already stored in the named local (DeclareLocal + eval + StoreLocal
    // are emitted by lower_case *before* the CaseMatch instruction).
    let scrutinee_idx = ctx.get(scrutinee_local)?;

    // Wrap the whole match in a block with result i64, so `br 0` exits with a value.
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Result(
        ValType::I64,
    )));

    let mut remaining_arms = arms;
    while let Some((arm, rest)) = remaining_arms.split_first() {
        remaining_arms = rest;

        match &arm.pattern {
            BackendCasePattern::Wildcard => {
                // Unconditional: emit body directly, then br out of block.
                emit_instrs(
                    function,
                    ctx,
                    &arm.body_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                function.instruction(&Instruction::Br(0));
                // Any arms after Wildcard are unreachable; stop emitting.
                break;
            }

            BackendCasePattern::IntLit(n) => {
                let encoded =
                    crate::gen_lower::value::encode_int(*n).map_err(|e| CodegenError {
                        message: format!("gen_lower/emit: CaseMatch IntLit out of range: {e}"),
                    })?;
                // Untag scrutinee and literal, compare.
                // Comparison: tag-strip both then i64.eq
                // Actually simpler: compare the full tagged i64 directly (tag + payload unique).
                function.instruction(&Instruction::LocalGet(scrutinee_idx));
                function.instruction(&Instruction::I64Const(encoded));
                function.instruction(&Instruction::I64Eq);
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                emit_instrs(
                    function,
                    ctx,
                    &arm.body_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                function.instruction(&Instruction::Br(1)); // br out of enclosing block
                function.instruction(&Instruction::End); // end if
            }

            BackendCasePattern::BoolLit(b) => {
                let encoded = crate::gen_lower::value::encode_bool(*b);
                function.instruction(&Instruction::LocalGet(scrutinee_idx));
                function.instruction(&Instruction::I64Const(encoded));
                function.instruction(&Instruction::I64Eq);
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                emit_instrs(
                    function,
                    ctx,
                    &arm.body_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                function.instruction(&Instruction::Br(1));
                function.instruction(&Instruction::End);
            }

            BackendCasePattern::StrLit(pattern_text) => {
                // Inline byte-wise string comparison (no host import needed).
                // Algorithm:
                //   pat_ptr, pat_len = decode_string(static pattern)
                //   scr_ptr, scr_len = decode_string(scrutinee)
                //   match = (pat_len == scr_len) && byte_loop_eq(pat, scr, pat_len)
                let hs = require_helper_state(*helper_state, "StrLit case pattern")?;
                // i32 scratch locals (borrowing from helper pool):
                // We need 4 i32 locals: pat_ptr, pat_len, scr_ptr, scr_len, + loop_iter
                // Use the generic helper i32 slots starting at hs.i32_base.
                // These slots are also used by Intrinsic helpers, but CaseMatch patterns
                // are not interleaved with running intrinsics, so reuse is safe.
                let s_pat_ptr = hs.i32_base + HS_TEXT_PTR;
                let s_pat_len = hs.i32_base + HS_TEXT_LEN;
                let s_scr_ptr = hs.i32_base + HS_SEP_PTR;
                let s_scr_len = hs.i32_base + HS_SEP_LEN;
                let s_loop_iter = hs.i32_base + HS_SCAN_POS;

                // Push static string for pattern.
                let pat_ptr = static_strings.ptr(pattern_text)?;
                function.instruction(&Instruction::I32Const(pat_ptr));
                function.instruction(&Instruction::LocalSet(s_pat_ptr));
                function.instruction(&Instruction::I32Const(pat_ptr));
                function.instruction(&Instruction::I32Load(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                function.instruction(&Instruction::LocalSet(s_pat_len));

                // Decode scrutinee string pointer.
                // First verify it's a string tag:
                function.instruction(&Instruction::LocalGet(scrutinee_idx));
                function.instruction(&Instruction::I64Const((TAG_STRING as i64) << 60));
                function.instruction(&Instruction::I64And);
                function.instruction(&Instruction::I64Const((TAG_STRING as i64) << 60));
                function.instruction(&Instruction::I64Eq);
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                // Is a string: decode ptr
                function.instruction(&Instruction::LocalGet(scrutinee_idx));
                function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
                function.instruction(&Instruction::I64And);
                function.instruction(&Instruction::I32WrapI64);
                function.instruction(&Instruction::LocalSet(s_scr_ptr));
                function.instruction(&Instruction::LocalGet(s_scr_ptr));
                function.instruction(&Instruction::I32Load(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                function.instruction(&Instruction::LocalSet(s_scr_len));
                // Compare lengths:
                function.instruction(&Instruction::LocalGet(s_pat_len));
                function.instruction(&Instruction::LocalGet(s_scr_len));
                function.instruction(&Instruction::I32Eq);
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                // Same length: byte loop
                function.instruction(&Instruction::I32Const(0));
                function.instruction(&Instruction::LocalSet(s_loop_iter));
                function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty)); // block_outer
                function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty)); // loop
                // if iter >= len → break out (full match)
                function.instruction(&Instruction::LocalGet(s_loop_iter));
                function.instruction(&Instruction::LocalGet(s_pat_len));
                function.instruction(&Instruction::I32GeU);
                function.instruction(&Instruction::BrIf(1)); // br block_outer → match succeeded
                // load pat byte
                function.instruction(&Instruction::LocalGet(s_pat_ptr));
                function.instruction(&Instruction::I32Const(4));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalGet(s_loop_iter));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::I32Load8U(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }));
                // load scr byte
                function.instruction(&Instruction::LocalGet(s_scr_ptr));
                function.instruction(&Instruction::I32Const(4));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalGet(s_loop_iter));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::I32Load8U(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                }));
                function.instruction(&Instruction::I32Ne);
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                // Bytes differ → jump out of loop + if → no match
                function.instruction(&Instruction::Br(3)); // br out of loop + block_outer + if(same len)
                function.instruction(&Instruction::End); // end if (bytes differ)
                // iter++
                function.instruction(&Instruction::LocalGet(s_loop_iter));
                function.instruction(&Instruction::I32Const(1));
                function.instruction(&Instruction::I32Add);
                function.instruction(&Instruction::LocalSet(s_loop_iter));
                function.instruction(&Instruction::Br(0)); // loop back
                function.instruction(&Instruction::End); // end loop
                function.instruction(&Instruction::End); // end block_outer
                // Strings are equal → emit body + br out
                emit_instrs(
                    function,
                    ctx,
                    &arm.body_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                function.instruction(&Instruction::Br(2)); // br out of: if(same len) + outer block
                function.instruction(&Instruction::End); // end if (same len)
                function.instruction(&Instruction::End); // end if (is string)
            }

            BackendCasePattern::EmptyList => {
                // Emit true iff scrutinee is a List with len == 0.
                // Check: tag bits == TAG_LIST
                function.instruction(&Instruction::LocalGet(scrutinee_idx));
                function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
                function.instruction(&Instruction::I64And);
                function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
                function.instruction(&Instruction::I64Eq);
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                // Extract list pointer (one i32 local from helper pool), load len at offset 0.
                // `needs_helper_state` returns true for any function with an EmptyList pattern,
                // so helper_state is always Some here.  The error path is unreachable in practice.
                let s_ptr = helper_state
                    .map(|hs| hs.i32_base + HS_TEXT_PTR)
                    .ok_or_else(|| CodegenError {
                        message: "gen_lower/emit: EmptyList pattern requires helper state (internal error: needs_helper_state should have set this up)".to_string(),
                    })?;
                function.instruction(&Instruction::LocalGet(scrutinee_idx));
                function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
                function.instruction(&Instruction::I64And);
                function.instruction(&Instruction::I32WrapI64);
                function.instruction(&Instruction::LocalSet(s_ptr));
                function.instruction(&Instruction::LocalGet(s_ptr));
                function.instruction(&Instruction::I32Load(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                function.instruction(&Instruction::I32Eqz);
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                emit_instrs(
                    function,
                    ctx,
                    &arm.body_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                function.instruction(&Instruction::Br(2)); // out of: if(len==0) + if(is list) + block
                function.instruction(&Instruction::End); // end if (len == 0)
                function.instruction(&Instruction::End); // end if (is list)
            }

            BackendCasePattern::ListPattern { items, tail } => {
                use crate::gen_lower::backend_ir::BackendListPatternItem;
                let hs = require_helper_state(*helper_state, "ListPattern")?;
                let s_list_ptr = hs.i32_base + HS_TEXT_PTR;
                let s_list_len = hs.i32_base + HS_TEXT_LEN;
                let s_alloc_size = hs.i32_base + HS_ALLOC_SIZE;
                let s_iter = hs.i32_base + HS_ITER;
                let s_tail_ptr = hs.i32_base + HS_AUX_PTR;
                let n_items = items.len() as i32;

                // Check tag == TAG_LIST
                function.instruction(&Instruction::LocalGet(scrutinee_idx));
                function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
                function.instruction(&Instruction::I64And);
                function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
                function.instruction(&Instruction::I64Eq);
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                // Is a list: extract ptr, load len
                function.instruction(&Instruction::LocalGet(scrutinee_idx));
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

                // Length check
                function.instruction(&Instruction::LocalGet(s_list_len));
                function.instruction(&Instruction::I32Const(n_items));
                if tail.is_some() {
                    function.instruction(&Instruction::I32GeU);
                } else {
                    function.instruction(&Instruction::I32Eq);
                }
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

                // Item matching: for each item, validate/bind
                // Mismatch → br 0 (exit if_len, fall through if_list → fall through arm)
                for (i, item) in items.iter().enumerate() {
                    let elem_offset = 4 + 8 * i as i32;
                    match item {
                        BackendListPatternItem::Bind(name) => {
                            let name_local = ctx.get(name)?;
                            function.instruction(&Instruction::LocalGet(s_list_ptr));
                            function.instruction(&Instruction::I32Const(elem_offset));
                            function.instruction(&Instruction::I32Add);
                            function.instruction(&Instruction::I64Load(MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));
                            function.instruction(&Instruction::LocalSet(name_local));
                        }
                        BackendListPatternItem::IntLit(n) => {
                            let encoded = crate::gen_lower::value::encode_int(*n).map_err(|e| {
                                CodegenError {
                                    message: format!(
                                        "gen_lower/emit: ListPattern IntLit out of range: {e}"
                                    ),
                                }
                            })?;
                            // Load element; compare. Mismatch → exit if_len.
                            function.instruction(&Instruction::LocalGet(s_list_ptr));
                            function.instruction(&Instruction::I32Const(elem_offset));
                            function.instruction(&Instruction::I32Add);
                            function.instruction(&Instruction::I64Load(MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));
                            function.instruction(&Instruction::I64Const(encoded));
                            function.instruction(&Instruction::I64Ne);
                            function.instruction(&Instruction::BrIf(0)); // exit if_len
                        }
                        BackendListPatternItem::StrLit(_) => {
                            // String item literal matching not yet supported.
                            return Err(CodegenError {
                                message:
                                    "gen_lower/emit: ListPattern StrLit item not yet supported"
                                        .to_string(),
                            });
                        }
                        BackendListPatternItem::Wildcard => {
                            // Nothing to do.
                        }
                    }
                }

                // Tail binding: allocate sub-list of (s_list_len - n_items) elements
                if let Some(tail_name) = tail {
                    let tail_local = ctx.get(tail_name)?;
                    // alloc_size = 4 + 8 * (s_list_len - n_items)
                    // = 4 + 8*s_list_len - 8*n_items
                    function.instruction(&Instruction::LocalGet(s_list_len));
                    function.instruction(&Instruction::I32Const(n_items));
                    function.instruction(&Instruction::I32Sub);
                    function.instruction(&Instruction::I32Const(8));
                    function.instruction(&Instruction::I32Mul);
                    function.instruction(&Instruction::I32Const(4));
                    function.instruction(&Instruction::I32Add);
                    function.instruction(&Instruction::LocalSet(s_alloc_size));
                    emit_alloc_from_top(function, &hs, s_alloc_size, s_tail_ptr);
                    // Write length = s_list_len - n_items
                    function.instruction(&Instruction::LocalGet(s_tail_ptr));
                    function.instruction(&Instruction::LocalGet(s_list_len));
                    function.instruction(&Instruction::I32Const(n_items));
                    function.instruction(&Instruction::I32Sub);
                    function.instruction(&Instruction::I32Store(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }));
                    // Copy elements [n_items..s_list_len] into tail sub-list
                    // s_iter = 0
                    function.instruction(&Instruction::I32Const(0));
                    function.instruction(&Instruction::LocalSet(s_iter));
                    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                    // if iter >= (s_list_len - n_items) → break
                    function.instruction(&Instruction::LocalGet(s_iter));
                    function.instruction(&Instruction::LocalGet(s_list_len));
                    function.instruction(&Instruction::I32Const(n_items));
                    function.instruction(&Instruction::I32Sub);
                    function.instruction(&Instruction::I32GeU);
                    function.instruction(&Instruction::BrIf(1));
                    // dst addr = s_tail_ptr + 4 + iter*8
                    function.instruction(&Instruction::LocalGet(s_tail_ptr));
                    function.instruction(&Instruction::I32Const(4));
                    function.instruction(&Instruction::I32Add);
                    function.instruction(&Instruction::LocalGet(s_iter));
                    function.instruction(&Instruction::I32Const(8));
                    function.instruction(&Instruction::I32Mul);
                    function.instruction(&Instruction::I32Add);
                    // src value = s_list_ptr + 4 + (iter + n_items)*8
                    function.instruction(&Instruction::LocalGet(s_list_ptr));
                    function.instruction(&Instruction::I32Const(4));
                    function.instruction(&Instruction::I32Add);
                    function.instruction(&Instruction::LocalGet(s_iter));
                    function.instruction(&Instruction::I32Const(n_items));
                    function.instruction(&Instruction::I32Add);
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
                    // iter++
                    function.instruction(&Instruction::LocalGet(s_iter));
                    function.instruction(&Instruction::I32Const(1));
                    function.instruction(&Instruction::I32Add);
                    function.instruction(&Instruction::LocalSet(s_iter));
                    function.instruction(&Instruction::Br(0));
                    function.instruction(&Instruction::End); // end loop
                    function.instruction(&Instruction::End); // end block
                    // Bind tail local to tagged list ptr
                    emit_push_tagged_ptr(function, s_tail_ptr, TAG_LIST);
                    function.instruction(&Instruction::LocalSet(tail_local));
                }

                // Emit body + br out of: if_len (0) + if_list (1) + outer block (2)
                emit_instrs(
                    function,
                    ctx,
                    &arm.body_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    options,
                    function_returns_i64,
                )?;
                function.instruction(&Instruction::Br(2));
                function.instruction(&Instruction::End); // end if_len
                function.instruction(&Instruction::End); // end if_list
            }
        }
    }

    // If the last arm was not Wildcard, emit unreachable as a safety net.
    if !matches!(arms.last(), Some(a) if a.pattern == BackendCasePattern::Wildcard) {
        function.instruction(&Instruction::Unreachable);
        // Still need to produce a value for the block type; push a dummy.
        // (unreachable makes this dead code, but wasm-encoder requires a type-valid block)
        function.instruction(&Instruction::I64Const(0));
    }

    function.instruction(&Instruction::End); // end block (result i64)
    Ok(())
}

fn emit_abort(function: &mut Function) {
    function.instruction(&Instruction::Unreachable);
}

fn emit_memory_exhaustion_abort(function: &mut Function, helper_state: &HelperEmitState) {
    function.instruction(&Instruction::I32Const(GLOBAL_RUNTIME_ERROR_OFFSET as i32));
    function.instruction(&Instruction::I32Const(
        RUNTIME_ERROR_MEMORY_EXHAUSTION as i32,
    ));
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    if helper_state.function_returns_i64 {
        function.instruction(&Instruction::I64Const(0));
    }
    function.instruction(&Instruction::Return);
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
#[allow(clippy::too_many_arguments)]
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
    use crate::gen_lower::backend_ir::{
        BackendEffectOp, BackendPrintOp, BackendReadOp, SplitIndexOperand, WasmBackendInstr as I,
    };
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
                op: BackendEffectOp::Read(BackendReadOp::Read),
            },
            I::EffectOp {
                op: BackendEffectOp::Print(BackendPrintOp::Print),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout()).expect("emit should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn wasmfx_experimental_strategy_matches_direct_effect_emission_for_current_ops() {
        let instrs = vec![
            I::EffectOp {
                op: BackendEffectOp::Read(BackendReadOp::Read),
            },
            I::EffectOp {
                op: BackendEffectOp::Print(BackendPrintOp::Println),
            },
            I::Drop,
        ];
        let direct = emit_general_module_with_aux_and_options(
            &instrs,
            &[],
            &default_layout(),
            EmitOptions {
                effect_emit_strategy: EffectEmitStrategy::Wb3DirectCall,
                memory_config: DEFAULT_WASM_MEMORY_CONFIG,
            },
        )
        .expect("direct emit should succeed");
        let wasmfx = emit_general_module_with_aux_and_options(
            &instrs,
            &[],
            &default_layout(),
            EmitOptions {
                effect_emit_strategy: EffectEmitStrategy::Wb3BWasmFxExperimental,
                memory_config: DEFAULT_WASM_MEMORY_CONFIG,
            },
        )
        .expect("experimental wasmfx emit should succeed");
        assert_eq!(
            wasmfx, direct,
            "current effect ops should remain byte-identical across emit strategies"
        );
        assert_valid_wasm(&wasmfx);
    }

    #[test]
    fn emit_let_read_print_produces_valid_wasm() {
        // Corresponds to: text = Read.read(); Print.print(text)
        let instrs = vec![
            I::DeclareLocal {
                name: "text".to_string(),
            },
            I::EffectOp {
                op: BackendEffectOp::Read(BackendReadOp::Read),
            },
            I::StoreLocal {
                name: "text".to_string(),
            },
            I::LoadLocal {
                name: "text".to_string(),
            },
            I::EffectOp {
                op: BackendEffectOp::Print(BackendPrintOp::Print),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout()).expect("emit should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_split_each_println_produces_valid_wasm() {
        // Corresponds to: text = Read.read(); SplitEachPrint(text, "\n", Print, println)
        let instrs = vec![
            I::DeclareLocal {
                name: "text".to_string(),
            },
            I::EffectOp {
                op: BackendEffectOp::Read(BackendReadOp::Read),
            },
            I::StoreLocal {
                name: "text".to_string(),
            },
            I::SplitEachPrint {
                text_local: "text".to_string(),
                sep_bytes: b"\n".to_vec(),
                op: BackendPrintOp::Println,
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
                op: BackendEffectOp::Read(BackendReadOp::Read),
            },
            I::StoreLocal {
                name: "text".to_string(),
            },
            I::SplitGetPrint {
                text_local: "text".to_string(),
                sep_bytes: b"\n".to_vec(),
                index: SplitIndexOperand::Const(1),
                op: BackendPrintOp::Println,
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
                op: BackendEffectOp::Read(BackendReadOp::Read),
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
                op: BackendPrintOp::Print,
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
                op: BackendEffectOp::Print(BackendPrintOp::Print),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit static string print should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn heap_allocation_reports_runtime_error_when_growth_hits_maximum() {
        let element_instrs = (0..40_000)
            .map(|n| {
                vec![I::I64Const(
                    crate::gen_lower::value::encode_int(n).expect("int encode"),
                )]
            })
            .collect::<Vec<_>>();
        let instrs = vec![
            I::ListLit { element_instrs },
            I::Drop,
            I::PushStaticString {
                text: "done".to_string(),
            },
            I::EffectOp {
                op: BackendEffectOp::Print(BackendPrintOp::Print),
            },
            I::Drop,
        ];
        let wasm = emit_general_module_with_aux_and_options(
            &instrs,
            &[],
            &default_layout(),
            EmitOptions {
                effect_emit_strategy: EffectEmitStrategy::Wb3DirectCall,
                memory_config: crate::memory_config::WasmMemoryConfig {
                    initial_pages: DEFAULT_WASM_MEMORY_CONFIG.initial_pages,
                    max_pages: DEFAULT_WASM_MEMORY_CONFIG.initial_pages,
                    host_bump_reserved_bytes: DEFAULT_WASM_MEMORY_CONFIG.host_bump_reserved_bytes,
                    max_wasm_stack_bytes: DEFAULT_WASM_MEMORY_CONFIG.max_wasm_stack_bytes,
                },
            },
        )
        .expect("low-max emit should still produce a module");
        let err = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
            .expect_err("heap growth beyond the configured maximum should fail");
        assert_eq!(
            err,
            "runtime error: memory exhausted [E-MEMORY-EXHAUSTION]: allocation exceeded the configured Wasm memory limit"
        );
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
    fn supports_list_concat_intrinsic() {
        let instrs = vec![
            I::ListLit {
                element_instrs: vec![vec![I::I64Const(
                    crate::gen_lower::value::encode_int(1).unwrap(),
                )]],
            },
            I::ListLit {
                element_instrs: vec![vec![I::I64Const(
                    crate::gen_lower::value::encode_int(2).unwrap(),
                )]],
            },
            I::Intrinsic {
                intrinsic: BackendIntrinsic::ListConcat,
            },
            I::Drop,
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
    fn emit_grapheme_host_imports_for_intrinsics() {
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
            "expected grapheme host import module name in Wasm import section"
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
                op: BackendEffectOp::Print(BackendPrintOp::Print),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit ListPushString helper chain should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_list_concat_produces_valid_wasm() {
        let instrs = vec![
            I::ListLit {
                element_instrs: vec![vec![I::I64Const(
                    crate::gen_lower::value::encode_int(1).unwrap(),
                )]],
            },
            I::ListLit {
                element_instrs: vec![vec![I::I64Const(
                    crate::gen_lower::value::encode_int(2).unwrap(),
                )]],
            },
            I::Intrinsic {
                intrinsic: BackendIntrinsic::ListConcat,
            },
            I::EffectOp {
                op: BackendEffectOp::Print(BackendPrintOp::Print),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit ListConcat helper chain should succeed");
        assert_valid_wasm(&wasm);
    }

    // --- BinOp emission ---

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
                op: BackendEffectOp::Print(BackendPrintOp::Print),
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
                op: BackendEffectOp::Print(BackendPrintOp::Print),
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
                op: BackendEffectOp::Read(BackendReadOp::Read),
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
                op: BackendEffectOp::Read(BackendReadOp::Read),
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

    #[test]
    fn emit_case_match_int_lit_and_wildcard_produces_valid_wasm() {
        use crate::gen_lower::backend_ir::{BackendCasePattern, CaseArmInstr};
        use crate::gen_lower::value::{encode_int, encode_unit};
        let x_val = encode_int(2).unwrap();
        let one_val = encode_int(1).unwrap();
        let instrs = vec![
            I::DeclareLocal {
                name: "x".to_string(),
            },
            I::I64Const(x_val),
            I::StoreLocal {
                name: "x".to_string(),
            },
            I::CaseMatch {
                scrutinee_local: "x".to_string(),
                arms: vec![
                    CaseArmInstr {
                        pattern: BackendCasePattern::IntLit(1),
                        body_instrs: vec![I::I64Const(one_val)],
                    },
                    CaseArmInstr {
                        pattern: BackendCasePattern::Wildcard,
                        body_instrs: vec![I::I64Const(encode_unit())],
                    },
                ],
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit CaseMatch int+wildcard should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_tuple_lit_produces_valid_wasm() {
        let instrs = vec![
            I::TupleLit {
                element_instrs: vec![
                    vec![I::I64Const(crate::gen_lower::value::encode_int(1).unwrap())],
                    vec![I::PushStaticString {
                        text: "hello".to_string(),
                    }],
                ],
            },
            I::Drop,
        ];
        let wasm =
            emit_general_module(&instrs, &default_layout()).expect("emit TupleLit should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_record_lit_produces_valid_wasm() {
        let instrs = vec![
            I::RecordLit {
                constructor: "Pair".to_string(),
                field_instrs: vec![
                    vec![I::I64Const(crate::gen_lower::value::encode_int(1).unwrap())],
                    vec![I::PushStaticString {
                        text: "hello".to_string(),
                    }],
                ],
            },
            I::Drop,
        ];
        let wasm =
            emit_general_module(&instrs, &default_layout()).expect("emit RecordLit should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_tuple_get_produces_valid_wasm() {
        // Build a (1, 2) tuple, store it in a local, then load field 0.
        let instrs = vec![
            I::DeclareLocal {
                name: "t".to_string(),
            },
            I::TupleLit {
                element_instrs: vec![
                    vec![I::I64Const(crate::gen_lower::value::encode_int(1).unwrap())],
                    vec![I::I64Const(crate::gen_lower::value::encode_int(2).unwrap())],
                ],
            },
            I::StoreLocal {
                name: "t".to_string(),
            },
            I::TupleGet {
                tuple_local: "t".to_string(),
                index: 0,
            },
            I::Drop,
        ];
        let wasm =
            emit_general_module(&instrs, &default_layout()).expect("emit TupleGet should succeed");
        assert_valid_wasm(&wasm);
    }
}
