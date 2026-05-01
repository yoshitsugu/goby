//! `WasmBackendInstr` → Wasm bytes emission for the general lowering path.
//!
//! This module owns the concrete WASI import mapping for runtime `Read`/`Print`
//! effect operations.

use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, ElementSection, Elements, EntityType, ExportKind,
    ExportSection, Function, FunctionSection, ImportSection, Instruction, MemArg, MemorySection,
    Module, NameMap, NameSection, RefType, TableSection, TableType, TypeSection, ValType,
};

use crate::CodegenError;
use crate::gen_lower::backend_ir::{
    BackendAllocInit, BackendEffectOp, BackendIntrinsic, BackendPrintOp, BackendReadOp,
    SplitIndexOperand, StaticHeapValue, WasmBackendInstr,
};
use crate::gen_lower::ptr::{
    PtrWidth, ptr_add, ptr_and, ptr_const, ptr_div_u, ptr_eq, ptr_eqz, ptr_extend_to_i64, ptr_ge_u,
    ptr_gt_u, ptr_le_u, ptr_load, ptr_load_8u, ptr_lt_s, ptr_lt_u, ptr_mul, ptr_ne, ptr_neg_one,
    ptr_rem_u, ptr_shr_u, ptr_store, ptr_store_8, ptr_sub, ptr_val_type, ptr_wrap_from_i64,
};
use goby_core::ir::IrBinOp;

use crate::gen_lower::value::{
    TAG_BOOL, TAG_CELL, TAG_CHUNK, TAG_CLOSURE, TAG_FUNC, TAG_INT, TAG_LIST, TAG_RECORD,
    TAG_STRING, TAG_TUPLE, encode_bool, encode_int, encode_list_ptr, encode_record_ptr,
    encode_string_ptr, encode_tuple_ptr, encode_unit,
};
use crate::host_runtime::{
    HOST_BUMP_RESERVED_BYTES, HOST_INTRINSIC_IMPORTS, IntrinsicExecutionBoundary,
    host_import_for_intrinsic,
};
use crate::layout::{
    GLOBAL_ALLOC_BYTES_TOTAL_OFFSET, GLOBAL_FREE_LIST_HITS_OFFSET, GLOBAL_FREED_BYTES_OFFSET,
    GLOBAL_HEAP_CURSOR_OFFSET, GLOBAL_HEAP_FLOOR_OFFSET, GLOBAL_HOST_BUMP_CURSOR_OFFSET,
    GLOBAL_PEAK_BYTES_OFFSET, GLOBAL_REUSE_HITS_OFFSET, GLOBAL_RUNTIME_ERROR_OFFSET, MemoryLayout,
    RUNTIME_ERROR_MEMORY_EXHAUSTION,
};
#[cfg(test)]
use crate::memory_config::TEST_MEMORY_CONFIG;
use crate::memory_config::{RUNTIME_MEMORY_CONFIG, WASM_PAGE_BYTES};
use crate::size_class::SizeClass;

// STATIC_STRING_LIMIT uses initial_linear_memory_bytes(), which depends only on
// initial_pages (= 4 in both TEST and RUNTIME configs, invariant by design).
const STATIC_STRING_LIMIT: u32 =
    RUNTIME_MEMORY_CONFIG.initial_linear_memory_bytes() - HOST_BUMP_RESERVED_BYTES;
const STATIC_REFCOUNT_SENTINEL: u64 = u64::MAX;

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

// ---------------------------------------------------------------------------
// Chunked Sequence (Candidate B) layout constants and arithmetic helpers.
//
// List heap layout (header):
//   [ptr + 0] : i32  total_len   — total element count across all chunks
//   [ptr + 4] : i32  n_chunks    — number of chunks
//   [ptr + 8] : u32  chunk_ptr[0]
//   ...
//   [ptr + 8 + (n_chunks-1)*4] : u32  chunk_ptr[n_chunks-1]
//
// Each chunk (must be allocated at 8-byte alignment for I64Load/Store align:3):
//   [chunk + 0]                       : i32  len  (valid items, <= CHUNK_SIZE)
//   [chunk + 4 + item_idx*8]          : i64  item[item_idx]  (tagged value)
//
// Design constraints:
//   - Refcount-tracked. Chunks are recycled via the Chunk free-list on last drop.
//     List headers and variable-size values are not free-listed; they fall back
//     to top-of-heap bump allocation. No structural sharing.
//   - Chunk allocations must be 8-byte aligned so that item I64Load/Store
//     instructions with align:3 are valid.
//   - CHUNK_SIZE is a named constant, not a magic number.
// ---------------------------------------------------------------------------

/// Number of items per chunk. Fixed at compile time.
pub(crate) const CHUNK_SIZE: u32 = 32;

/// Width in bytes of a pointer (chunk_ptr slot) in the list header.
#[inline(always)]
fn ptr_slot_bytes(pw: PtrWidth) -> u32 {
    match pw {
        PtrWidth::W32 => 4,
        PtrWidth::W64 => 8,
    }
}

/// Width in bytes of a metadata field (total_len, n_chunks, chunk.len).
/// Under memory64 we widen these to 8 bytes so that ptr_store (I64Store) works.
#[inline(always)]
fn meta_slot_bytes(pw: PtrWidth) -> u32 {
    match pw {
        PtrWidth::W32 => 4,
        PtrWidth::W64 => 8,
    }
}

/// Byte size of one chunk allocation: len field + CHUNK_SIZE × 8-byte items.
#[inline(always)]
pub(crate) fn chunk_alloc_size_pw(pw: PtrWidth) -> u32 {
    meta_slot_bytes(pw) + CHUNK_SIZE * 8
}

/// Byte size of a list header with `n_chunks` chunk pointers.
/// Layout: meta_bytes total_len + meta_bytes n_chunks + n_chunks × ptr_bytes chunk ptrs.
#[inline(always)]
pub(crate) fn header_alloc_size_pw(pw: PtrWidth, n_chunks: u32) -> u32 {
    2 * meta_slot_bytes(pw) + n_chunks * ptr_slot_bytes(pw)
}

/// Byte offset of the `total_len` field inside a list header.
#[inline(always)]
pub(crate) fn header_total_len_offset() -> u32 {
    0
}

/// Byte offset of the `n_chunks` field inside a list header.
#[inline(always)]
pub(crate) fn header_n_chunks_offset_pw(pw: PtrWidth) -> u32 {
    meta_slot_bytes(pw)
}

/// Byte offset of `chunk_ptr[chunk_idx]` inside a list header.
#[inline(always)]
pub(crate) fn header_chunk_ptr_offset_pw(pw: PtrWidth, chunk_idx: u32) -> u32 {
    2 * meta_slot_bytes(pw) + chunk_idx * ptr_slot_bytes(pw)
}

/// Byte offset of `item[item_idx]` inside a chunk (after the len field).
#[inline(always)]
pub(crate) fn chunk_item_offset_pw(pw: PtrWidth, item_idx: u32) -> u32 {
    meta_slot_bytes(pw) + item_idx * 8
}

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
            match instr {
                WasmBackendInstr::PushStaticString { text } => {
                    intern_static_string(text, &mut ptrs, &mut segments, &mut cursor)?
                }
                WasmBackendInstr::PushStaticHeap { value } => {
                    collect_static_heap_strings(value, &mut ptrs, &mut segments, &mut cursor)?
                }
                _ => {}
            }
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

fn collect_static_heap_strings(
    value: &StaticHeapValue,
    ptrs: &mut HashMap<String, i32>,
    segments: &mut Vec<(i32, Vec<u8>)>,
    cursor: &mut u32,
) -> Result<(), CodegenError> {
    match value {
        StaticHeapValue::Str(text) => intern_static_string(text, ptrs, segments, cursor),
        StaticHeapValue::List(items) | StaticHeapValue::Tuple(items) => {
            for item in items {
                collect_static_heap_strings(item, ptrs, segments, cursor)?;
            }
            Ok(())
        }
        StaticHeapValue::Record { fields, .. } => {
            for (_, value) in fields {
                collect_static_heap_strings(value, ptrs, segments, cursor)?;
            }
            Ok(())
        }
        StaticHeapValue::Int(_) | StaticHeapValue::Bool(_) | StaticHeapValue::Unit => Ok(()),
    }
}

fn intern_static_string(
    text: &str,
    ptrs: &mut HashMap<String, i32>,
    segments: &mut Vec<(i32, Vec<u8>)>,
    cursor: &mut u32,
) -> Result<(), CodegenError> {
    if ptrs.contains_key(text) {
        return Ok(());
    }

    let text_len = u32::try_from(text.len()).map_err(|_| CodegenError {
        message: "gen_lower/emit: static string is too large to encode".to_string(),
    })?;
    let blob_len = 4u32.checked_add(text_len).ok_or_else(|| CodegenError {
        message: "gen_lower/emit: static string blob size overflow".to_string(),
    })?;
    *cursor = cursor.checked_sub(blob_len).ok_or_else(|| CodegenError {
        message: "gen_lower/emit: static string pool overflow".to_string(),
    })?;
    let ptr = i32::try_from(*cursor).map_err(|_| CodegenError {
        message: "gen_lower/emit: static string pointer does not fit in i32".to_string(),
    })?;

    let mut blob = Vec::with_capacity(blob_len as usize);
    blob.extend_from_slice(
        &i32::try_from(text_len)
            .map_err(|_| CodegenError {
                message: "gen_lower/emit: static string length does not fit in i32".to_string(),
            })?
            .to_le_bytes(),
    );
    blob.extend_from_slice(text.as_bytes());

    ptrs.insert(text.to_string(), ptr);
    segments.push((ptr, blob));
    Ok(())
}

#[derive(Debug, Clone)]
struct StaticHeapPool {
    ptrs: HashMap<StaticHeapValue, i32>,
    segments: Vec<(i32, Vec<u8>)>,
    bytes_used: u32,
}

impl StaticHeapPool {
    fn build_from_all<'a>(
        all_slices: impl Iterator<Item = &'a [WasmBackendInstr]>,
        static_strings: &StaticStringPool,
        record_ctor_tags: &HashMap<String, u32>,
        pw: PtrWidth,
    ) -> Result<Self, CodegenError> {
        let mut values = Vec::new();
        for slice in all_slices {
            for instr in collect_all_instrs(slice) {
                if let WasmBackendInstr::PushStaticHeap { value } = instr {
                    values.push(value.clone());
                }
            }
        }

        let mut builder = StaticHeapPoolBuilder {
            ptrs: HashMap::new(),
            segments: Vec::new(),
            cursor: STATIC_STRING_LIMIT
                .checked_sub(static_strings.bytes_used)
                .ok_or_else(|| CodegenError {
                    message: "gen_lower/emit: static heap overlaps static string pool".to_string(),
                })?,
            static_strings,
            record_ctor_tags,
            pw,
        };

        for value in values {
            builder.intern(&value)?;
        }

        Ok(Self {
            ptrs: builder.ptrs,
            segments: builder.segments,
            bytes_used: STATIC_STRING_LIMIT - builder.cursor - static_strings.bytes_used,
        })
    }

    fn ptr(&self, value: &StaticHeapValue) -> Result<i32, CodegenError> {
        self.ptrs.get(value).copied().ok_or_else(|| CodegenError {
            message: "gen_lower/emit: missing static heap pointer".to_string(),
        })
    }

    fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }
}

struct StaticHeapPoolBuilder<'a> {
    ptrs: HashMap<StaticHeapValue, i32>,
    segments: Vec<(i32, Vec<u8>)>,
    cursor: u32,
    static_strings: &'a StaticStringPool,
    record_ctor_tags: &'a HashMap<String, u32>,
    pw: PtrWidth,
}

impl<'a> StaticHeapPoolBuilder<'a> {
    fn intern(&mut self, value: &StaticHeapValue) -> Result<i32, CodegenError> {
        if let Some(ptr) = self.ptrs.get(value) {
            return Ok(*ptr);
        }

        let ptr = match value {
            StaticHeapValue::List(items) => self.alloc_static_list(items)?,
            StaticHeapValue::Tuple(items) => self.alloc_static_tuple(items)?,
            StaticHeapValue::Record {
                constructor,
                fields,
            } => self.alloc_static_record(constructor, fields)?,
            StaticHeapValue::Int(_)
            | StaticHeapValue::Bool(_)
            | StaticHeapValue::Str(_)
            | StaticHeapValue::Unit => {
                return Err(CodegenError {
                    message: "gen_lower/emit: scalar static heap value cannot be interned"
                        .to_string(),
                });
            }
        };
        self.ptrs.insert(value.clone(), ptr);
        Ok(ptr)
    }

    fn alloc_static_list(&mut self, items: &[StaticHeapValue]) -> Result<i32, CodegenError> {
        let n = u32::try_from(items.len()).map_err(|_| CodegenError {
            message: "gen_lower/emit: static list literal too large".to_string(),
        })?;
        let n_chunks = n.div_ceil(CHUNK_SIZE);
        let mut chunk_ptrs = Vec::new();

        for c in (0..n_chunks).rev() {
            let elem_start = (c * CHUNK_SIZE) as usize;
            let elem_end = ((c + 1) * CHUNK_SIZE).min(n) as usize;
            let chunk_len = (elem_end - elem_start) as u64;
            let payload_size = chunk_alloc_size_pw(self.pw);
            let chunk_ptr = self.alloc_payload(payload_size, 8)?;
            let mut bytes = vec![0; payload_size as usize];
            self.write_meta(&mut bytes, 0, chunk_len)?;
            for (idx, item) in items[elem_start..elem_end].iter().enumerate() {
                let encoded = self.encode_static_value(item)?;
                let start = chunk_item_offset_pw(self.pw, idx as u32) as usize;
                bytes[start..start + 8].copy_from_slice(&encoded.to_le_bytes());
            }
            self.segments.push((chunk_ptr, bytes));
            chunk_ptrs.push((c, chunk_ptr));
        }

        let header_ptr = self.alloc_payload(header_alloc_size_pw(self.pw, n_chunks), 8)?;
        let mut header = vec![0; header_alloc_size_pw(self.pw, n_chunks) as usize];
        self.write_meta(&mut header, header_total_len_offset() as usize, n as u64)?;
        self.write_meta(
            &mut header,
            header_n_chunks_offset_pw(self.pw) as usize,
            n_chunks as u64,
        )?;
        for (chunk_idx, chunk_ptr) in chunk_ptrs {
            self.write_ptr(
                &mut header,
                header_chunk_ptr_offset_pw(self.pw, chunk_idx) as usize,
                chunk_ptr as u64,
            )?;
        }
        self.segments.push((header_ptr, header));
        Ok(header_ptr)
    }

    fn alloc_static_tuple(&mut self, items: &[StaticHeapValue]) -> Result<i32, CodegenError> {
        let payload_size = meta_slot_bytes(self.pw)
            + 8 * u32::try_from(items.len()).map_err(|_| CodegenError {
                message: "gen_lower/emit: static tuple literal too large".to_string(),
            })?;
        let ptr = self.alloc_payload(payload_size, 8)?;
        let mut bytes = vec![0; payload_size as usize];
        self.write_meta(&mut bytes, 0, items.len() as u64)?;
        for (idx, item) in items.iter().enumerate() {
            let encoded = self.encode_static_value(item)?;
            let start = (meta_slot_bytes(self.pw) as usize) + idx * 8;
            bytes[start..start + 8].copy_from_slice(&encoded.to_le_bytes());
        }
        self.segments.push((ptr, bytes));
        Ok(ptr)
    }

    fn alloc_static_record(
        &mut self,
        constructor: &str,
        fields: &[(String, StaticHeapValue)],
    ) -> Result<i32, CodegenError> {
        let payload_size = 8 + 8 * u32::try_from(fields.len()).map_err(|_| CodegenError {
            message: "gen_lower/emit: static record literal too large".to_string(),
        })?;
        let ptr = self.alloc_payload(payload_size, 8)?;
        let mut bytes = vec![0; payload_size as usize];
        let ctor_tag = *self
            .record_ctor_tags
            .get(constructor)
            .ok_or_else(|| CodegenError {
                message: format!("gen_lower/emit: missing ctor tag for '{constructor}'"),
            })? as u64;
        bytes[0..8].copy_from_slice(&ctor_tag.to_le_bytes());
        for (idx, (_, value)) in fields.iter().enumerate() {
            let encoded = self.encode_static_value(value)?;
            let start = 8 + idx * 8;
            bytes[start..start + 8].copy_from_slice(&encoded.to_le_bytes());
        }
        self.segments.push((ptr, bytes));
        Ok(ptr)
    }

    fn encode_static_value(&mut self, value: &StaticHeapValue) -> Result<i64, CodegenError> {
        match value {
            StaticHeapValue::Int(n) => encode_int(*n).map_err(|err| CodegenError {
                message: err.to_string(),
            }),
            StaticHeapValue::Bool(b) => Ok(encode_bool(*b)),
            StaticHeapValue::Str(text) => {
                let ptr = self.static_strings.ptr(text)?;
                Ok(encode_string_ptr(ptr as u32))
            }
            StaticHeapValue::Unit => Ok(encode_unit()),
            StaticHeapValue::List(_) => Ok(encode_list_ptr(self.intern(value)? as u32)),
            StaticHeapValue::Tuple(_) => Ok(encode_tuple_ptr(self.intern(value)? as u32)),
            StaticHeapValue::Record { .. } => Ok(encode_record_ptr(self.intern(value)? as u32)),
        }
    }

    fn alloc_payload(&mut self, payload_size: u32, align: u32) -> Result<i32, CodegenError> {
        let total_size = payload_size
            .checked_add(ptr_slot_bytes(self.pw))
            .ok_or_else(|| CodegenError {
                message: "gen_lower/emit: static heap size overflow".to_string(),
            })?;
        let next = align_down_i32(
            i32::try_from(
                self.cursor
                    .checked_sub(total_size)
                    .ok_or_else(|| CodegenError {
                        message: "gen_lower/emit: static heap exhausted initial memory".to_string(),
                    })?,
            )
            .map_err(|_| CodegenError {
                message: "gen_lower/emit: static heap cursor does not fit in i32".to_string(),
            })?,
            align as i32,
        );
        let aligned_next = u32::try_from(next).map_err(|_| CodegenError {
            message: "gen_lower/emit: static heap alignment underflow".to_string(),
        })?;
        self.cursor = aligned_next;

        let header_addr = aligned_next;
        let payload_addr = header_addr + ptr_slot_bytes(self.pw);
        let header_bytes = match self.pw {
            PtrWidth::W32 => 0xFFFF_FFFFu32.to_le_bytes().to_vec(),
            PtrWidth::W64 => STATIC_REFCOUNT_SENTINEL.to_le_bytes().to_vec(),
        };
        self.segments.push((header_addr as i32, header_bytes));
        Ok(payload_addr as i32)
    }

    fn write_meta(&self, bytes: &mut [u8], offset: usize, value: u64) -> Result<(), CodegenError> {
        match self.pw {
            PtrWidth::W32 => {
                let value = u32::try_from(value).map_err(|_| CodegenError {
                    message: "gen_lower/emit: static meta value exceeds u32".to_string(),
                })?;
                bytes[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
            }
            PtrWidth::W64 => bytes[offset..offset + 8].copy_from_slice(&value.to_le_bytes()),
        }
        Ok(())
    }

    fn write_ptr(&self, bytes: &mut [u8], offset: usize, value: u64) -> Result<(), CodegenError> {
        match self.pw {
            PtrWidth::W32 => {
                let value = u32::try_from(value).map_err(|_| CodegenError {
                    message: "gen_lower/emit: static pointer exceeds u32".to_string(),
                })?;
                bytes[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
            }
            PtrWidth::W64 => bytes[offset..offset + 8].copy_from_slice(&value.to_le_bytes()),
        }
        Ok(())
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
    /// Map from declaration name to whether the result may reference
    /// Wasm-owned heap allocations that must survive after the call returns.
    decl_returns_wasm_heap: HashMap<String, bool>,
    /// Map from declaration name to whether executing the callee may advance
    /// the shared heap state even when its return value is immediate.
    decl_uses_heap: HashMap<String, bool>,
    /// Map from declaration name to funcref table slot index (for `PushFuncHandle`).
    /// Slot 0 = first aux decl, slot 1 = second aux decl, etc.
    decl_table_slots: HashMap<String, u32>,
    /// Wasm function index of the module-local `__goby_dup` helper.
    goby_dup_func_idx: u32,
    /// Wasm function index of the module-local `__goby_drop` helper.
    goby_drop_func_idx: u32,
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
    /// Optional tail-call execution metadata for the function currently being emitted.
    tail_call_mode: Option<TailCallMode>,
}

#[derive(Debug, Clone)]
enum TailCallMode {
    Group(TailCallGroupState),
}

#[derive(Debug, Clone)]
struct TailCallGroupState {
    tag_local: u32,
    shared_arg_locals: Vec<u32>,
    arg_scratch_base: u32,
    member_tags: HashMap<String, u32>,
    member_arities: HashMap<String, usize>,
}

#[derive(Debug, Clone)]
pub(crate) struct TailCallGroupPlan {
    pub(crate) dispatcher_name: String,
    pub(crate) member_names: Vec<String>,
    member_tags: HashMap<String, u32>,
    member_arities: HashMap<String, usize>,
    pub(crate) max_arity: usize,
}

impl EmitContext {
    /// Build an `EmitContext` seeded with the module-level lookup tables shared across
    /// all function bodies (main and every aux decl).  `locals` starts empty; callers
    /// that need to pre-register Wasm function parameters insert them afterwards.
    fn with_module_tables(
        decl_func_indices: HashMap<String, u32>,
        decl_returns_wasm_heap: HashMap<String, bool>,
        decl_uses_heap: HashMap<String, bool>,
        decl_table_slots: HashMap<String, u32>,
        goby_dup_func_idx: u32,
        goby_drop_func_idx: u32,
        indirect_call_type_idx_1: Option<u32>,
        indirect_call_type_idx_2: Option<u32>,
        indirect_call_type_idx_3: Option<u32>,
        record_ctor_tags: HashMap<String, u32>,
    ) -> Self {
        Self {
            locals: HashMap::new(),
            next_local: 0,
            decl_func_indices,
            decl_returns_wasm_heap,
            decl_uses_heap,
            decl_table_slots,
            goby_dup_func_idx,
            goby_drop_func_idx,
            indirect_call_type_idx_1,
            indirect_call_type_idx_2,
            indirect_call_type_idx_3,
            record_ctor_tags,
            tail_call_mode: None,
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

    fn decl_returns_wasm_heap(&self, decl_name: &str) -> Result<bool, CodegenError> {
        self.decl_returns_wasm_heap
            .get(decl_name)
            .copied()
            .ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: missing return ownership for declaration '{decl_name}'"
                ),
            })
    }

    fn decl_uses_heap(&self, decl_name: &str) -> Result<bool, CodegenError> {
        self.decl_uses_heap
            .get(decl_name)
            .copied()
            .ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: missing heap-usage metadata for declaration '{decl_name}'"
                ),
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
struct EmitScratchState {
    i64_base: u32,
    i32_base: u32,
    function_returns_i64: bool,
}

#[derive(Clone, Copy)]
struct HeapEmitState {
    scratch: EmitScratchState,
    alloc_cursor_local: u32,
    heap_floor_local: u32,
    ptr_width: crate::gen_lower::ptr::PtrWidth,
}

impl HeapEmitState {
    #[inline(always)]
    fn pw(self) -> PtrWidth {
        self.ptr_width
    }
}

/// Extract scratch state, returning a `CodegenError` if it is absent.
///
/// `context` names the instruction or operation that requires scratch state,
/// and appears verbatim in the error message so callers do not need to repeat
/// the error-construction boilerplate.
///
/// `EmitScratchState` is `Copy`, so callers pass the `Option<EmitScratchState>` value directly.
fn require_scratch_state(
    scratch_state: Option<EmitScratchState>,
    context: &str,
) -> Result<EmitScratchState, CodegenError> {
    scratch_state.ok_or_else(|| CodegenError {
        message: format!("gen_lower/emit: {context} requires helper scratch state"),
    })
}

fn require_heap_state(
    heap_state: Option<HeapEmitState>,
    context: &str,
) -> Result<HeapEmitState, CodegenError> {
    heap_state.ok_or_else(|| CodegenError {
        message: format!("gen_lower/emit: {context} requires heap helper state"),
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
pub(crate) fn collect_all_instrs(instrs: &[WasmBackendInstr]) -> Vec<&WasmBackendInstr> {
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
            WasmBackendInstr::Loop { body_instrs } => {
                result.extend(collect_all_instrs(body_instrs));
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
            WasmBackendInstr::ListBuilderPush { value_instrs, .. } => {
                result.extend(collect_all_instrs(value_instrs));
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
            WasmBackendInstr::AllocReuse { init, .. } => match init {
                BackendAllocInit::ListLit { element_instrs }
                | BackendAllocInit::TupleLit { element_instrs } => {
                    for elem in element_instrs {
                        result.extend(collect_all_instrs(elem));
                    }
                }
                BackendAllocInit::RecordLit { field_instrs, .. } => {
                    for field in field_instrs {
                        result.extend(collect_all_instrs(field));
                    }
                }
                BackendAllocInit::Interp(_) | BackendAllocInit::Retain => {}
            },
            WasmBackendInstr::ListReverseFoldPrepend {
                list_instrs,
                prefix_element_instrs,
                ..
            } => {
                result.extend(collect_all_instrs(list_instrs));
                for elem in prefix_element_instrs {
                    result.extend(collect_all_instrs(elem));
                }
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

/// Returns true when `instrs` require heap-cursor state.
pub(crate) fn needs_helper_state(instrs: &[WasmBackendInstr]) -> bool {
    use crate::gen_lower::backend_ir::BackendCasePattern;
    collect_all_instrs(instrs).iter().any(|i| {
        matches!(
            i,
            WasmBackendInstr::ListLit { .. }
                | WasmBackendInstr::ListBuilderNew { .. }
                | WasmBackendInstr::ListBuilderPush { .. }
                | WasmBackendInstr::ListBuilderFinish { .. }
                | WasmBackendInstr::TupleLit { .. }
                | WasmBackendInstr::RecordLit { .. }
                | WasmBackendInstr::AllocReuse { .. }
                | WasmBackendInstr::ListReverseFoldPrepend { .. }
                | WasmBackendInstr::CreateClosure { .. }
                | WasmBackendInstr::AllocMutableCell { .. }
                | WasmBackendInstr::Intrinsic {
                    intrinsic: BackendIntrinsic::StringSplit
                        | BackendIntrinsic::ValueToString
                        | BackendIntrinsic::StringEachGraphemeCount
                        | BackendIntrinsic::StringEachGraphemeState
                        | BackendIntrinsic::StringConcat
                        | BackendIntrinsic::StringGraphemesList
                        | BackendIntrinsic::StringSplitLines
                        | BackendIntrinsic::ListPushString
                        | BackendIntrinsic::ListSet
                        | BackendIntrinsic::ListSetInPlace
                        | BackendIntrinsic::ListConcat
                        | BackendIntrinsic::ListFold
                        | BackendIntrinsic::ListMap
                }
                // DeclCall / IndirectCall may trigger callee heap allocations; the global cursor
                // must be synchronized before and after the call, which requires alloc_cursor_local.
                | WasmBackendInstr::DeclCall { .. }
                | WasmBackendInstr::TailDeclCall { .. }
                | WasmBackendInstr::IndirectCall { .. }
        )
    }) || {
        // Check for heap-allocating list patterns in CaseMatch arms.
        fn has_heap_pattern(instrs: &[WasmBackendInstr]) -> bool {
            for instr in instrs {
                match instr {
                    WasmBackendInstr::CaseMatch { arms, .. } => {
                        for arm in arms {
                            if matches!(&arm.pattern, BackendCasePattern::ListPattern { .. }) {
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
                    WasmBackendInstr::Loop { body_instrs } => {
                        if has_heap_pattern(body_instrs) {
                            return true;
                        }
                    }
                    WasmBackendInstr::ListBuilderPush { value_instrs, .. } => {
                        if has_heap_pattern(value_instrs) {
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
                    WasmBackendInstr::AllocReuse { init, .. } => match init {
                        BackendAllocInit::ListLit { element_instrs }
                        | BackendAllocInit::TupleLit { element_instrs } => {
                            if element_instrs.iter().any(|e| has_heap_pattern(e)) {
                                return true;
                            }
                        }
                        BackendAllocInit::RecordLit { field_instrs, .. } => {
                            if field_instrs.iter().any(|e| has_heap_pattern(e)) {
                                return true;
                            }
                        }
                        BackendAllocInit::Interp(_) | BackendAllocInit::Retain => {}
                    },
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

fn needs_scratch_state(instrs: &[WasmBackendInstr]) -> bool {
    use crate::gen_lower::backend_ir::BackendCasePattern;
    use goby_core::ir::IrBinOp;

    if needs_helper_state(instrs) {
        return true;
    }

    collect_all_instrs(instrs).iter().any(|instr| match instr {
        WasmBackendInstr::BinOp { op } => matches!(op, IrBinOp::Eq),
        WasmBackendInstr::RefCountDropReuse { .. } => true,
        WasmBackendInstr::Intrinsic {
            intrinsic:
                BackendIntrinsic::ListGet
                | BackendIntrinsic::StringLength
                | BackendIntrinsic::ListLength,
        } => true,
        WasmBackendInstr::CaseMatch { arms, .. } => arms.iter().any(|arm| {
            matches!(
                arm.pattern,
                BackendCasePattern::StrLit(_) | BackendCasePattern::EmptyList
            )
        }),
        _ => false,
    })
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
    let helper_count = if needs_scratch_state(instrs) {
        HELPER_SCRATCH_I32
            + if needs_helper_state(instrs) {
                required_heap_base_spill_count(instrs)
            } else {
                0
            }
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
        WasmBackendInstr::Loop { body_instrs } => required_heap_base_spill_count(body_instrs),
        WasmBackendInstr::ListBuilderPush { value_instrs, .. } => {
            required_heap_base_spill_count(value_instrs)
        }
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
        WasmBackendInstr::AllocReuse { init, .. } => match init {
            BackendAllocInit::ListLit { element_instrs }
            | BackendAllocInit::TupleLit { element_instrs } => {
                1 + element_instrs
                    .iter()
                    .map(|child| required_heap_base_spill_count(child))
                    .max()
                    .unwrap_or(0)
            }
            BackendAllocInit::RecordLit { field_instrs, .. } => {
                1 + field_instrs
                    .iter()
                    .map(|child| required_heap_base_spill_count(child))
                    .max()
                    .unwrap_or(0)
            }
            BackendAllocInit::Interp(_) => 0,
            BackendAllocInit::Retain => 1,
        },
        WasmBackendInstr::ListReverseFoldPrepend {
            list_instrs,
            prefix_element_instrs,
            ..
        } => required_heap_base_spill_count(list_instrs).max(
            prefix_element_instrs
                .iter()
                .map(|child| required_heap_base_spill_count(child))
                .max()
                .unwrap_or(0),
        ),
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
    use crate::gen_lower::backend_ir::BackendCasePattern;
    use goby_core::ir::IrBinOp;
    let all = collect_all_instrs(instrs);
    let needs_scratch = all.iter().any(|i| match i {
        WasmBackendInstr::Intrinsic { .. }
        | WasmBackendInstr::RefCountDropReuse { .. }
        | WasmBackendInstr::ListReverseFoldPrepend { .. }
        | WasmBackendInstr::ListBuilderPush { .. }
        | WasmBackendInstr::IndirectCall { .. } => true,
        WasmBackendInstr::CaseMatch { arms, .. } => arms.iter().any(|arm| {
            matches!(
                arm.pattern,
                BackendCasePattern::ListPattern { tail: Some(_), .. }
            )
        }),
        WasmBackendInstr::BinOp { op } => {
            matches!(op, IrBinOp::Mul | IrBinOp::Div | IrBinOp::Mod | IrBinOp::Eq)
        }
        _ => false,
    });
    if needs_scratch { HELPER_SCRATCH_I64 } else { 0 }
}

fn collect_tail_decl_targets(instrs: &[WasmBackendInstr]) -> Vec<String> {
    collect_all_instrs(instrs)
        .into_iter()
        .filter_map(|instr| match instr {
            WasmBackendInstr::TailDeclCall { decl_name } => Some(decl_name.clone()),
            _ => None,
        })
        .collect()
}

pub(crate) fn compute_tail_call_group_plans(aux_decls: &[AuxDecl]) -> Vec<TailCallGroupPlan> {
    let decl_index_by_name: HashMap<String, usize> = aux_decls
        .iter()
        .enumerate()
        .map(|(idx, decl)| (decl.decl_name.clone(), idx))
        .collect();
    let edges: Vec<Vec<usize>> = aux_decls
        .iter()
        .map(|decl| {
            collect_tail_decl_targets(&decl.instrs)
                .into_iter()
                .filter_map(|target| decl_index_by_name.get(&target).copied())
                .collect()
        })
        .collect();

    let mut undirected_edges: Vec<Vec<usize>> = vec![Vec::new(); aux_decls.len()];
    let mut participates = vec![false; aux_decls.len()];
    for (src_idx, targets) in edges.iter().enumerate() {
        for &target_idx in targets {
            participates[src_idx] = true;
            participates[target_idx] = true;
            undirected_edges[src_idx].push(target_idx);
            if src_idx != target_idx {
                undirected_edges[target_idx].push(src_idx);
            }
        }
    }

    let mut plans = Vec::new();
    let mut visited = vec![false; aux_decls.len()];
    let mut group_idx = 0usize;
    for start_idx in 0..aux_decls.len() {
        if visited[start_idx] || !participates[start_idx] {
            continue;
        }
        let mut stack = vec![start_idx];
        let mut component = Vec::new();
        visited[start_idx] = true;
        while let Some(node) = stack.pop() {
            component.push(node);
            for &neighbor in &undirected_edges[node] {
                if !visited[neighbor] {
                    visited[neighbor] = true;
                    stack.push(neighbor);
                }
            }
        }
        component.sort_unstable();
        let member_names: Vec<String> = component
            .iter()
            .map(|&idx| aux_decls[idx].decl_name.clone())
            .collect();
        let member_tags: HashMap<String, u32> = member_names
            .iter()
            .enumerate()
            .map(|(tag, name)| (name.clone(), tag as u32))
            .collect();
        let member_arities: HashMap<String, usize> = component
            .iter()
            .map(|&idx| {
                let d = &aux_decls[idx];
                let arity = d.param_names.len() + if d.reuse_param_name.is_some() { 1 } else { 0 };
                (d.decl_name.clone(), arity)
            })
            .collect();
        let max_arity = member_arities.values().copied().max().unwrap_or(0);
        plans.push(TailCallGroupPlan {
            dispatcher_name: format!("__tail_group_dispatch_{group_idx}"),
            member_names,
            member_tags,
            member_arities,
            max_arity,
        });
        group_idx += 1;
    }
    plans
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
    pub(crate) returns_wasm_heap: bool,
    pub(crate) instrs: Vec<WasmBackendInstr>,
    /// If `Some(name)`, this decl has a hidden trailing `i64` reuse-token parameter.
    /// The name is registered as a wasm local at index `param_names.len()`.
    pub(crate) reuse_param_name: Option<String>,
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
    pub(crate) debug_alloc_stats: bool,
    /// Emit test-only exported Wasm functions for Perceus M3 acceptance testing.
    /// Adds `__test_alloc_list_1chunk` and `__test_drop_ptr` exports.
    /// Must NOT be set in production modules.
    pub(crate) expose_perceus_test_exports: bool,
}

impl Default for EmitOptions {
    fn default() -> Self {
        Self {
            effect_emit_strategy: default_effect_emit_strategy(),
            memory_config: RUNTIME_MEMORY_CONFIG,
            debug_alloc_stats: false,
            expose_perceus_test_exports: false,
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
    total_static_reserved_bytes: u32,
    is_main: bool,
    pw: PtrWidth,
) -> Result<(), CodegenError> {
    let buffer_ptr = layout.heap_base as i32;
    let alloc_cursor_local = i32_base + HELPER_ALLOC_CURSOR_OFFSET;
    let heap_floor_local = i32_base + HELPER_HEAP_FLOOR_OFFSET;
    let initial_cursor = align_down_i32(
        i32::try_from(STATIC_STRING_LIMIT - total_static_reserved_bytes).map_err(|_| {
            CodegenError {
                message: "gen_lower/emit: helper allocation cursor does not fit in i32".to_string(),
            }
        })?,
        8,
    );
    let initial_floor = align_up_i32(buffer_ptr + 4, 8);
    // Global heap-slot fields are always stored as i32 (4 bytes) regardless of pw.
    // The slot addresses are ptr-width (i64 in W64), but the values are i32.
    // After loading an i32, extend to ptr-width for use as a local address variable.
    let global_mem_arg = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    if is_main {
        // main initializes the global heap cursor in linear memory, then loads it locally.
        function.instruction(&ptr_const(pw, GLOBAL_HEAP_CURSOR_OFFSET as u64));
        function.instruction(&Instruction::I32Const(initial_cursor));
        function.instruction(&Instruction::I32Store(global_mem_arg));
        function.instruction(&ptr_const(pw, GLOBAL_RUNTIME_ERROR_OFFSET as u64));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Store(global_mem_arg));
        function.instruction(&ptr_const(pw, GLOBAL_HEAP_FLOOR_OFFSET as u64));
        function.instruction(&Instruction::I32Const(initial_floor));
        function.instruction(&Instruction::I32Store(global_mem_arg));
        // Zero-initialize the i64 allocation-stats slots.
        for offset in [
            GLOBAL_ALLOC_BYTES_TOTAL_OFFSET,
            GLOBAL_PEAK_BYTES_OFFSET,
            GLOBAL_FREE_LIST_HITS_OFFSET,
            GLOBAL_REUSE_HITS_OFFSET,
        ] {
            function.instruction(&ptr_const(pw, offset as u64));
            function.instruction(&Instruction::I64Const(0));
            function.instruction(&Instruction::I64Store(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
        }
        function.instruction(&ptr_const(pw, GLOBAL_HEAP_CURSOR_OFFSET as u64));
        function.instruction(&Instruction::I32Load(global_mem_arg));
        if pw == PtrWidth::W64 {
            function.instruction(&Instruction::I64ExtendI32U);
        }
        function.instruction(&Instruction::LocalSet(alloc_cursor_local));
        function.instruction(&ptr_const(pw, GLOBAL_HEAP_FLOOR_OFFSET as u64));
        function.instruction(&Instruction::I32Load(global_mem_arg));
        if pw == PtrWidth::W64 {
            function.instruction(&Instruction::I64ExtendI32U);
        }
    } else {
        // aux decls load the current global cursor/floor (may have been advanced by caller).
        function.instruction(&ptr_const(pw, GLOBAL_HEAP_CURSOR_OFFSET as u64));
        function.instruction(&Instruction::I32Load(global_mem_arg));
        if pw == PtrWidth::W64 {
            function.instruction(&Instruction::I64ExtendI32U);
        }
        function.instruction(&Instruction::LocalSet(alloc_cursor_local));
        function.instruction(&ptr_const(pw, GLOBAL_HEAP_FLOOR_OFFSET as u64));
        function.instruction(&Instruction::I32Load(global_mem_arg));
        if pw == PtrWidth::W64 {
            function.instruction(&Instruction::I64ExtendI32U);
        }
    }
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
        WasmBackendInstr::DeclCall { .. } | WasmBackendInstr::TailDeclCall { .. } => true,
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
    let pw = if options.memory_config.memory64 {
        PtrWidth::W64
    } else {
        PtrWidth::W32
    };
    let static_strings = StaticStringPool::build_from_all(
        std::iter::once(instrs).chain(aux_decls.iter().map(|d| d.instrs.as_slice())),
    )?;
    let mut record_ctor_tags = HashMap::new();
    for instr in std::iter::once(instrs).chain(aux_decls.iter().map(|d| d.instrs.as_slice())) {
        for nested in collect_all_instrs(instr) {
            match nested {
                WasmBackendInstr::RecordLit { constructor, .. } => {
                    if !record_ctor_tags.contains_key(constructor) {
                        let next = record_ctor_tags.len() as u32;
                        record_ctor_tags.insert(constructor.clone(), next);
                    }
                }
                WasmBackendInstr::PushStaticHeap {
                    value:
                        StaticHeapValue::Record {
                            constructor,
                            fields: _,
                        },
                } => {
                    if !record_ctor_tags.contains_key(constructor) {
                        let next = record_ctor_tags.len() as u32;
                        record_ctor_tags.insert(constructor.clone(), next);
                    }
                }
                _ => {}
            }
        }
    }
    let static_heap = StaticHeapPool::build_from_all(
        std::iter::once(instrs).chain(aux_decls.iter().map(|d| d.instrs.as_slice())),
        &static_strings,
        &record_ctor_tags,
        pw,
    )?;
    let total_static_reserved_bytes = static_strings.bytes_used + static_heap.bytes_used;
    if layout.heap_base + total_static_reserved_bytes + 4 > STATIC_STRING_LIMIT {
        return Err(CodegenError {
            message: "gen_lower/emit: static pools leave no room for runtime stdin buffer"
                .to_string(),
        });
    }
    let tail_group_plans = compute_tail_call_group_plans(aux_decls);
    let tail_group_plan_by_member: HashMap<String, TailCallGroupPlan> = tail_group_plans
        .iter()
        .flat_map(|plan| {
            plan.member_names
                .iter()
                .cloned()
                .map(move |member| (member, plan.clone()))
        })
        .collect();

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
    // Under memory64 the "i32 scratch" pool is declared as i64 so that all
    // address/size locals hold i64 values.  The pool offset (i32_base) is unchanged.
    let scratch_val_type = if options.memory_config.memory64 {
        ValType::I64
    } else {
        ValType::I32
    };

    // Check whether any instruction list (main or aux) uses host intrinsics.
    let all_slices_iter =
        || std::iter::once(instrs).chain(aux_decls.iter().map(|d| d.instrs.as_slice()));
    let uses_host_intrinsics = all_slices_iter().any(|slice| {
        collect_all_instrs(slice).iter().any(|instr| {
            matches!(
                instr,
                WasmBackendInstr::Intrinsic { intrinsic }
                    if intrinsic.execution_boundary() == IntrinsicExecutionBoundary::HostImport
                        || *intrinsic == BackendIntrinsic::StringSplit
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
            let param_count = decl.param_names.len()
                + if decl.reuse_param_name.is_some() {
                    1
                } else {
                    0
                };
            let params: Vec<ValType> = vec![ValType::I64; param_count];
            types.ty().function(params, [ValType::I64]);
            idx
        })
        .collect();
    let tail_group_dispatch_type_indices: Vec<u32> = tail_group_plans
        .iter()
        .map(|plan| {
            let idx = types.len();
            let params: Vec<ValType> = vec![ValType::I64; 1 + plan.max_arity];
            types.ty().function(params, [ValType::I64]);
            idx
        })
        .collect();

    // Detect which IndirectCall arities are used by emitted instructions/intrinsics.
    let uses_indirect_call_1 = all_slices_iter().any(|slice| {
        collect_all_instrs(slice).iter().any(|i| {
            matches!(
                i,
                WasmBackendInstr::IndirectCall { arity: 1 }
                    | WasmBackendInstr::Intrinsic {
                        intrinsic: BackendIntrinsic::ListMap
                    }
            )
        })
    });
    let uses_indirect_call_2 = all_slices_iter().any(|slice| {
        collect_all_instrs(slice).iter().any(|i| {
            matches!(
                i,
                WasmBackendInstr::IndirectCall { arity: 2 }
                    | WasmBackendInstr::IndirectCall { arity: 1 }
                    | WasmBackendInstr::Intrinsic {
                        intrinsic: BackendIntrinsic::ListFold
                    }
                    | WasmBackendInstr::Intrinsic {
                        intrinsic: BackendIntrinsic::ListMap
                    }
            )
        })
    });
    let uses_indirect_call_3 = all_slices_iter().any(|slice| {
        collect_all_instrs(slice).iter().any(|i| {
            matches!(
                i,
                WasmBackendInstr::IndirectCall { arity: 2 }
                    | WasmBackendInstr::Intrinsic {
                        intrinsic: BackendIntrinsic::ListFold
                    }
            )
        })
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

    // type for __goby_dup / __goby_drop: (i64) -> ()
    let goby_rc_type_idx = types.len();
    types.ty().function([ValType::I64], []);

    // type for __test_alloc_list_1chunk / __test_reuse_hits: () -> i64
    let test_alloc_type_idx = if options.expose_perceus_test_exports {
        let idx = types.len();
        types.ty().function([], [ValType::I64]);
        idx
    } else {
        0 // unused
    };
    // type for __test_drop_reuse_ptr: (i64) -> i64  (tagged_ptr -> reuse token)
    let test_drop_reuse_type_idx = if options.expose_perceus_test_exports {
        let idx = types.len();
        types.ty().function([ValType::I64], [ValType::I64]);
        idx
    } else {
        0 // unused
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
    // __goby_dup and __goby_drop are emitted as the last runtime helpers in the module
    // (after dispatchers).
    let goby_dup_func_idx: u32;
    let goby_drop_func_idx: u32;
    let mut functions = FunctionSection::new();
    functions.function(main_type);
    for aux_type_idx in &aux_type_indices {
        functions.function(*aux_type_idx);
    }
    for dispatch_type_idx in &tail_group_dispatch_type_indices {
        functions.function(*dispatch_type_idx);
    }
    {
        let idx = main_func_idx + 1 + aux_decls.len() as u32 + tail_group_plans.len() as u32;
        goby_dup_func_idx = idx;
        goby_drop_func_idx = idx + 1;
        functions.function(goby_rc_type_idx);
        functions.function(goby_rc_type_idx);
    }
    // Test-only functions: __test_alloc_list_1chunk, __test_drop_ptr,
    //                       __test_drop_reuse_ptr, __test_reuse_hits
    let test_alloc_func_idx = goby_drop_func_idx + 1;
    let test_drop_func_idx = goby_drop_func_idx + 2;
    let test_drop_reuse_func_idx = goby_drop_func_idx + 3;
    let test_reuse_hits_func_idx = goby_drop_func_idx + 4;
    if options.expose_perceus_test_exports {
        functions.function(test_alloc_type_idx); // () -> i64
        functions.function(goby_rc_type_idx); // (i64) -> ()
        functions.function(test_drop_reuse_type_idx); // (i64) -> i64
        functions.function(test_alloc_type_idx); // () -> i64
    }
    module.section(&functions);

    // Build decl_name → func_idx table for DeclCall resolution.
    // aux decls start at main_func_idx + 1.
    let decl_func_indices: HashMap<String, u32> = aux_decls
        .iter()
        .enumerate()
        .map(|(i, decl)| (decl.decl_name.clone(), main_func_idx + 1 + i as u32))
        .collect();
    let tail_group_dispatch_indices: HashMap<String, u32> = tail_group_plans
        .iter()
        .enumerate()
        .map(|(i, plan)| {
            (
                plan.dispatcher_name.clone(),
                main_func_idx + 1 + aux_decls.len() as u32 + i as u32,
            )
        })
        .collect();
    let decl_returns_wasm_heap: HashMap<String, bool> = aux_decls
        .iter()
        .map(|decl| (decl.decl_name.clone(), decl.returns_wasm_heap))
        .collect();
    let decl_uses_heap: HashMap<String, bool> = aux_decls
        .iter()
        .map(|decl| (decl.decl_name.clone(), needs_helper_state(&decl.instrs)))
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
    if options.expose_perceus_test_exports {
        exports.export(
            "__test_alloc_list_1chunk",
            ExportKind::Func,
            test_alloc_func_idx,
        );
        exports.export("__test_drop_ptr", ExportKind::Func, test_drop_func_idx);
        exports.export(
            "__test_drop_reuse_ptr",
            ExportKind::Func,
            test_drop_reuse_func_idx,
        );
        exports.export(
            "__test_reuse_hits",
            ExportKind::Func,
            test_reuse_hits_func_idx,
        );
        exports.export("__goby_drop", ExportKind::Func, goby_drop_func_idx);
        // M10: expose user aux decls so perceus tests can call them
        // directly (e.g. `build_list` in
        // `perceus_m9_build_list_has_chunks_when_dropped`). Without this,
        // wasmtime's `instance.get_typed_func(name)` cannot resolve aux
        // decls — the name section alone is not sufficient.
        for (i, decl) in aux_decls.iter().enumerate() {
            exports.export(
                &decl.decl_name,
                ExportKind::Func,
                main_func_idx + 1 + i as u32,
            );
        }
    }
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
            locals_vec.push((main_i32_scratch_count, scratch_val_type));
        }
        // Allocate 3 extra i64 scratch locals for the debug_alloc_stats epilogue:
        // [0] current stat value being converted, [1] buf write cursor, [2] digit loop temp.
        let stats_scratch_base = main_i64_count + main_i32_scratch_count;
        if options.debug_alloc_stats {
            locals_vec.push((3, ValType::I64));
        }
        let mut function = Function::new(locals_vec);
        let mut ctx = EmitContext::with_module_tables(
            decl_func_indices.clone(),
            decl_returns_wasm_heap.clone(),
            decl_uses_heap.clone(),
            decl_table_slots.clone(),
            goby_dup_func_idx,
            goby_drop_func_idx,
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
            &static_heap,
            total_static_reserved_bytes,
            options,
            false, // emit_epilogue_cursor_sync
            None,
            stats_scratch_base,
        )?;
        code.function(&function);
    }

    // Aux decl functions (after main). Tail-call-group members keep their public
    // wrapper signatures; shared constant-stack execution lives in dispatcher helpers.
    for decl in aux_decls {
        if let Some(group_plan) = tail_group_plan_by_member.get(&decl.decl_name) {
            let dispatcher_func_idx = *tail_group_dispatch_indices
                .get(&group_plan.dispatcher_name)
                .ok_or_else(|| CodegenError {
                    message: format!(
                        "gen_lower/emit: missing dispatcher function index for '{}'",
                        group_plan.dispatcher_name
                    ),
                })?;
            let member_tag =
                *group_plan
                    .member_tags
                    .get(&decl.decl_name)
                    .ok_or_else(|| CodegenError {
                        message: format!(
                            "gen_lower/emit: missing group tag for tail-call member '{}'",
                            decl.decl_name
                        ),
                    })?;
            let mut function = Function::new(Vec::new());
            function.instruction(&Instruction::I64Const(member_tag as i64));
            for param_idx in 0..decl.param_names.len() as u32 {
                function.instruction(&Instruction::LocalGet(param_idx));
            }
            for _ in decl.param_names.len()..group_plan.max_arity {
                function.instruction(&Instruction::I64Const(0));
            }
            function.instruction(&Instruction::Call(dispatcher_func_idx));
            function.instruction(&Instruction::End);
            code.function(&function);
            continue;
        }

        let aux_all = collect_all_instrs(&decl.instrs);
        let aux_named_i64_count = aux_all
            .iter()
            .filter(|i| matches!(i, WasmBackendInstr::DeclareLocal { .. }))
            .count() as u32;
        let aux_helper_i64_scratch_count = required_i64_scratch_count(&decl.instrs);
        let aux_param_count = decl.param_names.len() as u32
            + if decl.reuse_param_name.is_some() {
                1
            } else {
                0
            };
        let aux_i64_count = aux_named_i64_count + aux_helper_i64_scratch_count;
        let aux_i32_scratch_count = required_i32_scratch_count(&decl.instrs);
        let aux_i32_base = aux_param_count + aux_i64_count;

        let mut locals_vec: Vec<(u32, ValType)> = Vec::new();
        if aux_i64_count > 0 {
            locals_vec.push((aux_i64_count, ValType::I64));
        }
        if aux_i32_scratch_count > 0 {
            locals_vec.push((aux_i32_scratch_count, scratch_val_type));
        }
        let mut function = Function::new(locals_vec);

        let mut ctx = EmitContext::with_module_tables(
            decl_func_indices.clone(),
            decl_returns_wasm_heap.clone(),
            decl_uses_heap.clone(),
            decl_table_slots.clone(),
            goby_dup_func_idx,
            goby_drop_func_idx,
            indirect_call_type_idx_1,
            indirect_call_type_idx_2,
            indirect_call_type_idx_3,
            record_ctor_tags.clone(),
        );
        for param_name in &decl.param_names {
            ctx.locals.insert(param_name.clone(), ctx.next_local);
            ctx.next_local += 1;
        }
        if let Some(reuse_tok) = &decl.reuse_param_name {
            ctx.locals.insert(reuse_tok.clone(), ctx.next_local);
            ctx.next_local += 1;
        }
        emit_function_body(
            &mut function,
            &mut ctx,
            &decl.instrs,
            layout,
            aux_named_i64_count,
            aux_helper_i64_scratch_count,
            aux_i32_base,
            &static_strings,
            &static_heap,
            total_static_reserved_bytes,
            options,
            true,
            None,
            0, // stats_scratch_base unused for aux decls
        )?;
        code.function(&function);
    }
    for group_plan in &tail_group_plans {
        emit_tail_call_group_dispatcher(
            &mut code,
            group_plan,
            aux_decls,
            &decl_func_indices,
            &decl_returns_wasm_heap,
            &decl_uses_heap,
            &decl_table_slots,
            goby_dup_func_idx,
            goby_drop_func_idx,
            indirect_call_type_idx_1,
            indirect_call_type_idx_2,
            indirect_call_type_idx_3,
            &record_ctor_tags,
            &static_strings,
            &static_heap,
            total_static_reserved_bytes,
            layout,
            options,
        )?;
    }
    emit_goby_dup_function(&mut code, pw);
    // Emit __goby_drop as the last function in the code section.
    emit_goby_drop_function(&mut code, layout, pw, goby_drop_func_idx);

    // Emit test-only exports when requested.
    if options.expose_perceus_test_exports {
        emit_test_alloc_list_1chunk(&mut code, layout, pw);
        emit_test_drop_ptr(&mut code, goby_drop_func_idx);
        emit_test_drop_reuse_ptr(&mut code, goby_drop_func_idx, pw);
        emit_test_reuse_hits(&mut code, pw);
    }
    module.section(&code);

    let mut names = NameSection::new();
    names.module("goby");
    let mut function_names = NameMap::new();
    function_names.append(main_func_idx, "main");
    for (i, decl) in aux_decls.iter().enumerate() {
        function_names.append(main_func_idx + 1 + i as u32, &decl.decl_name);
    }
    for (i, plan) in tail_group_plans.iter().enumerate() {
        function_names.append(
            main_func_idx + 1 + aux_decls.len() as u32 + i as u32,
            &plan.dispatcher_name,
        );
    }
    function_names.append(goby_dup_func_idx, "__goby_dup");
    function_names.append(goby_drop_func_idx, "__goby_drop");
    if options.expose_perceus_test_exports {
        function_names.append(test_alloc_func_idx, "__test_alloc_list_1chunk");
        function_names.append(test_drop_func_idx, "__test_drop_ptr");
        function_names.append(test_drop_reuse_func_idx, "__test_drop_reuse_ptr");
        function_names.append(test_reuse_hits_func_idx, "__test_reuse_hits");
    }
    names.functions(&function_names);
    module.section(&names);

    // Data section: newline byte for println (if needed).
    let needs_newline =
        needs_newline_data(instrs) || aux_decls.iter().any(|d| needs_newline_data(&d.instrs));
    if needs_newline || !static_strings.is_empty() || !static_heap.is_empty() {
        let newline_ptr = (layout.heap_base - 1) as i32;
        let data_offset = |addr: i32| -> ConstExpr {
            if options.memory_config.memory64 {
                ConstExpr::i64_const(addr as i64)
            } else {
                ConstExpr::i32_const(addr)
            }
        };
        let mut data = DataSection::new();
        if needs_newline {
            data.active(0, &data_offset(newline_ptr), b"\n".to_vec());
        }
        for (ptr, bytes) in &static_heap.segments {
            data.active(0, &data_offset(*ptr), bytes.clone());
        }
        for (ptr, bytes) in &static_strings.segments {
            data.active(0, &data_offset(*ptr), bytes.clone());
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
    static_heap: &StaticHeapPool,
    options: EmitOptions,
    function_returns_i64: bool,
    self_tail_loop_depth: u32,
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
        static_heap,
        options,
        function_returns_i64,
        0,
        self_tail_loop_depth,
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
    static_heap: &StaticHeapPool,
    options: EmitOptions,
    function_returns_i64: bool,
    heap_base_depth: u32,
    self_tail_loop_depth: u32,
) -> Result<(), CodegenError> {
    let pw = PtrWidth::from_memory64(options.memory_config.memory64);
    let iovec_offset = layout.iovec_offset as i32;
    let nread_offset = layout.nwritten_offset as i32;
    let buffer_ptr = layout.heap_base as i32;
    let buffer_len = (STATIC_STRING_LIMIT
        - layout.heap_base
        - static_strings.bytes_used
        - static_heap.bytes_used) as i32
        - 4; // reserve 4 bytes for len prefix
    let newline_ptr = (layout.heap_base - 1) as i32;
    let helper_i64_base = i32_base
        .checked_sub(helper_i64_scratch_count)
        .ok_or_else(|| CodegenError {
            message: "gen_lower/emit: helper i64 scratch base underflow".to_string(),
        })?;
    let scratch_state = if needs_scratch_state(instrs) {
        Some(EmitScratchState {
            i64_base: helper_i64_base,
            i32_base,
            function_returns_i64,
        })
    } else {
        None
    };
    let helper_state = if needs_helper_state(instrs) {
        let alloc_cursor_local = i32_base + HELPER_ALLOC_CURSOR_OFFSET;
        let heap_floor_local = i32_base + HELPER_HEAP_FLOOR_OFFSET;
        Some(HeapEmitState {
            scratch: scratch_state.expect("heap state requires scratch state"),
            alloc_cursor_local,
            heap_floor_local,
            ptr_width: crate::gen_lower::ptr::PtrWidth::from_memory64(
                options.memory_config.memory64,
            ),
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

            WasmBackendInstr::PushStaticHeap { value } => {
                let ptr = static_heap.ptr(value)?;
                let encoded = match value {
                    StaticHeapValue::List(_) => encode_list_ptr(ptr as u32),
                    StaticHeapValue::Tuple(_) => encode_tuple_ptr(ptr as u32),
                    StaticHeapValue::Record { .. } => encode_record_ptr(ptr as u32),
                    StaticHeapValue::Int(_)
                    | StaticHeapValue::Bool(_)
                    | StaticHeapValue::Str(_)
                    | StaticHeapValue::Unit => {
                        return Err(CodegenError {
                            message: "gen_lower/emit: PushStaticHeap expects aggregate value"
                                .to_string(),
                        });
                    }
                };
                function.instruction(&Instruction::I64Const(encoded));
            }

            WasmBackendInstr::PushStaticString { text } => {
                let ptr = static_strings.ptr(text)?;
                function.instruction(&Instruction::I64Const(encode_string_ptr(ptr as u32)));
            }

            WasmBackendInstr::RefCountDup => {
                function.instruction(&Instruction::Call(ctx.goby_dup_func_idx));
            }

            WasmBackendInstr::RefCountDrop => {
                function.instruction(&Instruction::Call(ctx.goby_drop_func_idx));
            }

            WasmBackendInstr::RefCountDropReuse { token_local } => {
                let scratch = require_scratch_state(scratch_state, "RefCountDropReuse")?;
                let token_idx = ctx.get(token_local)?;
                emit_ref_count_drop_reuse(function, token_idx, scratch.i64_base, ctx, pw);
            }

            WasmBackendInstr::AllocReuse {
                token_local,
                size_class,
                init,
            } => {
                let hs = require_heap_state(helper_state, "AllocReuse")?;
                let token_idx = ctx.get(token_local)?;
                emit_alloc_reuse(
                    function,
                    ctx,
                    token_idx,
                    *size_class,
                    init,
                    &hs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    static_heap,
                    options,
                    function_returns_i64,
                    heap_base_depth,
                    self_tail_loop_depth,
                )?;
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
                emit_bin_op(
                    function,
                    op,
                    i64_scratch_base,
                    scratch_state,
                    PtrWidth::from_memory64(options.memory_config.memory64),
                )?;
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
                    PtrWidth::from_memory64(options.memory_config.memory64),
                )?;
            }

            WasmBackendInstr::Intrinsic { intrinsic } => {
                emit_helper_call(
                    function,
                    ctx,
                    *intrinsic,
                    i32_base,
                    scratch_state.as_ref(),
                    helper_state.as_ref(),
                    PtrWidth::from_memory64(options.memory_config.memory64),
                )?;
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
                    PtrWidth::from_memory64(options.memory_config.memory64),
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
                    PtrWidth::from_memory64(options.memory_config.memory64),
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
                    static_heap,
                    options,
                    function_returns_i64,
                    heap_base_depth,
                    self_tail_loop_depth + 1,
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
                    static_heap,
                    options,
                    function_returns_i64,
                    heap_base_depth,
                    self_tail_loop_depth + 1,
                )?;
                function.instruction(&Instruction::End);
            }

            WasmBackendInstr::Loop { body_instrs } => {
                function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Result(
                    wasm_encoder::ValType::I64,
                )));
                emit_instrs_with_heap_depth(
                    function,
                    ctx,
                    body_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    static_heap,
                    options,
                    function_returns_i64,
                    heap_base_depth,
                    self_tail_loop_depth + 1,
                )?;
                function.instruction(&Instruction::End);
            }

            WasmBackendInstr::ContinueLoop { relative_depth } => {
                function.instruction(&Instruction::Br(*relative_depth));
            }

            WasmBackendInstr::DeclCall { decl_name } => {
                let func_idx = ctx.decl_func_idx(decl_name)?;
                let returns_wasm_heap = ctx.decl_returns_wasm_heap(decl_name)?;
                let callee_uses_heap = ctx.decl_uses_heap(decl_name)?;
                emit_heap_aware_direct_call(
                    function,
                    func_idx,
                    helper_state.as_ref(),
                    returns_wasm_heap,
                    callee_uses_heap,
                );
            }

            WasmBackendInstr::TailDeclCall { decl_name } => match &ctx.tail_call_mode {
                Some(TailCallMode::Group(group_state))
                    if group_state.member_tags.contains_key(decl_name) =>
                {
                    let target_arity = *group_state
                            .member_arities
                            .get(decl_name)
                            .ok_or_else(|| CodegenError {
                                message: format!(
                                    "gen_lower/emit: missing arity for tail-call group member '{decl_name}'"
                                ),
                            })?;
                    for offset in (0..target_arity).rev() {
                        function.instruction(&Instruction::LocalSet(
                            group_state.arg_scratch_base + offset as u32,
                        ));
                    }
                    for offset in 0..target_arity {
                        function.instruction(&Instruction::LocalGet(
                            group_state.arg_scratch_base + offset as u32,
                        ));
                        function.instruction(&Instruction::LocalSet(
                            group_state.shared_arg_locals[offset],
                        ));
                    }
                    function.instruction(&Instruction::I64Const(
                        *group_state
                            .member_tags
                            .get(decl_name)
                            .expect("tail-call group tag") as i64,
                    ));
                    function.instruction(&Instruction::LocalSet(group_state.tag_local));
                    function.instruction(&Instruction::Br(self_tail_loop_depth));
                }
                _ => {
                    let func_idx = ctx.decl_func_idx(decl_name)?;
                    let returns_wasm_heap = ctx.decl_returns_wasm_heap(decl_name)?;
                    let callee_uses_heap = ctx.decl_uses_heap(decl_name)?;
                    emit_heap_aware_direct_call(
                        function,
                        func_idx,
                        helper_state.as_ref(),
                        returns_wasm_heap,
                        callee_uses_heap,
                    );
                }
            },

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
                    static_heap,
                    &scratch_state,
                    &helper_state,
                    options,
                    function_returns_i64,
                    self_tail_loop_depth,
                )?;
            }

            WasmBackendInstr::ListLit { element_instrs } => {
                // Chunked Sequence (Candidate B) layout.
                // Header: [total_len: i32][n_chunks: i32][chunk_ptr*: u32]
                // Chunk:  [len: i32][item*: i64]  (8-byte aligned by bump allocator)
                let hs = require_heap_state(helper_state, "ListLit")?;
                let pw = hs.pw();
                // s_list_ptr is a heap-depth-specific spill slot (survives nested allocs).
                let s_header_ptr = hs.scratch.i32_base + HELPER_SCRATCH_I32 + heap_base_depth;
                let s_alloc_ptr = hs.scratch.i32_base + HS_AUX_PTR;
                let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE;
                let n = element_instrs.len() as u32;
                let n_chunks = n.div_ceil(CHUNK_SIZE);

                // --- Allocate header ---
                function.instruction(&ptr_const(pw, header_alloc_size_pw(pw, n_chunks) as u64));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, &hs, s_alloc_size, s_alloc_ptr);
                function.instruction(&Instruction::LocalGet(s_alloc_ptr));
                function.instruction(&Instruction::LocalSet(s_header_ptr));

                // Write header.total_len = n
                function.instruction(&Instruction::LocalGet(s_header_ptr));
                function.instruction(&ptr_const(pw, n as u64));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: header_total_len_offset() as u64,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                // Write header.n_chunks
                function.instruction(&Instruction::LocalGet(s_header_ptr));
                function.instruction(&ptr_const(pw, n_chunks as u64));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: header_n_chunks_offset_pw(pw) as u64,
                        align: 2,
                        memory_index: 0,
                    },
                ));

                // --- Allocate each chunk and emit its elements ---
                for c in 0..n_chunks {
                    let elem_start = (c * CHUNK_SIZE) as usize;
                    let elem_end = ((c + 1) * CHUNK_SIZE).min(n) as usize;
                    let chunk_len = (elem_end - elem_start) as u64;

                    // Allocate chunk (bump allocator guarantees 8-byte alignment)
                    function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
                    function.instruction(&Instruction::LocalSet(s_alloc_size));
                    emit_alloc_from_top(function, &hs, s_alloc_size, s_alloc_ptr);

                    // Store chunk ptr into header at slot c
                    function.instruction(&Instruction::LocalGet(s_header_ptr));
                    function.instruction(&Instruction::LocalGet(s_alloc_ptr));
                    function.instruction(&ptr_store(
                        pw,
                        MemArg {
                            offset: header_chunk_ptr_offset_pw(pw, c) as u64,
                            align: 2,
                            memory_index: 0,
                        },
                    ));

                    // Write chunk.len
                    function.instruction(&Instruction::LocalGet(s_alloc_ptr));
                    function.instruction(&ptr_const(pw, chunk_len));
                    function.instruction(&ptr_store(
                        pw,
                        MemArg {
                            offset: 0,
                            align: 2,
                            memory_index: 0,
                        },
                    ));

                    // Write each element in this chunk
                    for (item_idx, elem_instrs) in
                        element_instrs[elem_start..elem_end].iter().enumerate()
                    {
                        // Re-load chunk ptr from the header slot each iteration.
                        // Element emission may allocate and reuse generic scratch locals.
                        // dst address = chunk_ptr + 4 + item_idx * 8
                        function.instruction(&Instruction::LocalGet(s_header_ptr));
                        function.instruction(&ptr_load(
                            pw,
                            MemArg {
                                offset: header_chunk_ptr_offset_pw(pw, c) as u64,
                                align: 2,
                                memory_index: 0,
                            },
                        ));
                        function.instruction(&ptr_const(
                            pw,
                            chunk_item_offset_pw(pw, item_idx as u32) as u64,
                        ));
                        function.instruction(&ptr_add(pw));
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
                            static_heap,
                            options,
                            function_returns_i64,
                            heap_base_depth + 1,
                            self_tail_loop_depth,
                        )?;
                        function.instruction(&Instruction::I64Store(MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }));
                    }
                }

                // Push tagged header pointer as result
                emit_push_tagged_ptr(function, s_header_ptr, TAG_LIST, pw);
            }

            WasmBackendInstr::ListBuilderNew {
                header_ptr_local,
                header_cap_local,
                n_chunks_local,
                chunk_ptr_local,
                total_len_local,
                initial_capacity,
            } => {
                // Chunked Sequence Builder — New.
                // Allocates an initial chunk and a header with `initial_capacity` chunk-ptr slots.
                // State stored in 5 named i64 locals (as raw u32 or i32 values):
                //   header_ptr_local : u32 — ptr to current header allocation
                //   header_cap_local : i32 — number of chunk-ptr slots in current header
                //   n_chunks_local   : i32 — number of chunks currently used
                //   chunk_ptr_local  : u32 — ptr to current (last) chunk being filled
                //   total_len_local  : i32 — total elements pushed so far
                let hs = require_heap_state(helper_state, "ListBuilderNew")?;
                let pw = hs.pw();
                let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE;
                let s_alloc_ptr = hs.scratch.i32_base + HS_AUX_PTR;
                let s_header_cap = hs.scratch.i32_base + HS_TEXT_PTR;
                let s_n_chunks = hs.scratch.i32_base + HS_TEXT_LEN;
                let s_total_len = hs.scratch.i32_base + HS_SEP_PTR;

                // header_cap = max(initial_capacity, 1)
                let init_cap = (*initial_capacity).max(1);
                function.instruction(&ptr_const(pw, init_cap as u64));
                function.instruction(&Instruction::LocalSet(s_header_cap));
                // n_chunks = 0, total_len = 0
                function.instruction(&ptr_const(pw, 0));
                function.instruction(&Instruction::LocalSet(s_n_chunks));
                function.instruction(&ptr_const(pw, 0));
                function.instruction(&Instruction::LocalSet(s_total_len));

                // Allocate header
                function.instruction(&ptr_const(pw, header_alloc_size_pw(pw, init_cap) as u64));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, &hs, s_alloc_size, s_alloc_ptr);
                // Store header_ptr
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    header_ptr_local,
                    s_alloc_ptr,
                    pw,
                )?;

                // Allocate initial chunk
                function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, &hs, s_alloc_size, s_alloc_ptr);
                // Write chunk.len = 0
                function.instruction(&Instruction::LocalGet(s_alloc_ptr));
                function.instruction(&ptr_const(pw, 0));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                // Store chunk_ptr
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    chunk_ptr_local,
                    s_alloc_ptr,
                    pw,
                )?;
                // Store header_cap, n_chunks, total_len
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    header_cap_local,
                    s_header_cap,
                    pw,
                )?;
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    n_chunks_local,
                    s_n_chunks,
                    pw,
                )?;
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    total_len_local,
                    s_total_len,
                    pw,
                )?;
            }

            WasmBackendInstr::ListBuilderPush {
                header_ptr_local,
                header_cap_local,
                n_chunks_local,
                chunk_ptr_local,
                total_len_local,
                value_instrs,
            } => {
                // Chunked Sequence Builder — Push.
                //
                // Logic:
                //   chunk_len = mem[chunk_ptr + 0]
                //   if chunk_len == CHUNK_SIZE:
                //     // current chunk full — grow header if needed, allocate new chunk
                //     if n_chunks == header_cap:
                //       // double header capacity
                //       new_header = alloc(header_alloc_size_pw(pw, header_cap * 2))
                //       copy n_chunks chunk ptrs from old header to new header
                //       header_ptr = new_header; header_cap *= 2
                //     header[8 + n_chunks*4] = chunk_ptr (finalize current chunk in header)
                //     n_chunks += 1
                //     new_chunk = alloc(chunk_alloc_size()); mem[new_chunk] = 0
                //     chunk_ptr = new_chunk; chunk_len = 0
                //   // append value to current chunk
                //   chunk[4 + chunk_len * 8] = value
                //   mem[chunk_ptr + 0] = chunk_len + 1
                //   total_len += 1
                let hs = require_heap_state(helper_state, "ListBuilderPush")?;
                let pw = hs.pw();
                let scratch = require_scratch_state(scratch_state, "ListBuilderPush")?;
                let value_i64 = scratch.i64_base;
                // i32 scratch slots:
                let s_header_ptr = hs.scratch.i32_base + HS_TEXT_PTR;
                let s_header_cap = hs.scratch.i32_base + HS_TEXT_LEN;
                let s_n_chunks = hs.scratch.i32_base + HS_SEP_PTR;
                let s_chunk_ptr = hs.scratch.i32_base + HS_SEG_START;
                let s_total_len = hs.scratch.i32_base + HS_ITEM_COUNT;
                let s_chunk_len = hs.scratch.i32_base + HS_AUX_PTR;
                let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE;
                let s_new_ptr = hs.scratch.i32_base + HS_LIST_PTR;
                let s_iter = hs.scratch.i32_base + HS_ITER;

                // Load all builder state
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    header_ptr_local,
                    s_header_ptr,
                    pw,
                )?;
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    header_cap_local,
                    s_header_cap,
                    pw,
                )?;
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    n_chunks_local,
                    s_n_chunks,
                    pw,
                )?;
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    chunk_ptr_local,
                    s_chunk_ptr,
                    pw,
                )?;
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    total_len_local,
                    s_total_len,
                    pw,
                )?;

                // chunk_len = mem[chunk_ptr]
                function.instruction(&Instruction::LocalGet(s_chunk_ptr));
                function.instruction(&ptr_load(
                    pw,
                    MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                function.instruction(&Instruction::LocalSet(s_chunk_len));

                // if chunk_len == CHUNK_SIZE: chunk is full, rotate to new chunk
                function.instruction(&Instruction::LocalGet(s_chunk_len));
                function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
                function.instruction(&ptr_eq(pw));
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

                // -- inner if: n_chunks == header_cap → grow header --
                function.instruction(&Instruction::LocalGet(s_n_chunks));
                function.instruction(&Instruction::LocalGet(s_header_cap));
                function.instruction(&ptr_eq(pw));
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

                // new_header_cap = header_cap * 2
                function.instruction(&Instruction::LocalGet(s_header_cap));
                function.instruction(&ptr_const(pw, 2));
                function.instruction(&ptr_mul(pw));
                function.instruction(&Instruction::LocalSet(s_header_cap));
                // alloc new header: 2*meta_bytes + new_cap * ptr_bytes
                function.instruction(&Instruction::LocalGet(s_header_cap));
                function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
                function.instruction(&ptr_mul(pw));
                function.instruction(&ptr_const(pw, 2 * meta_slot_bytes(pw) as u64));
                function.instruction(&ptr_add(pw));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, &hs, s_alloc_size, s_new_ptr);
                // copy n_chunks chunk ptrs from old to new header
                function.instruction(&ptr_const(pw, 0));
                function.instruction(&Instruction::LocalSet(s_iter));
                function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                function.instruction(&Instruction::LocalGet(s_iter));
                function.instruction(&Instruction::LocalGet(s_n_chunks));
                function.instruction(&ptr_ge_u(pw));
                function.instruction(&Instruction::BrIf(1));
                // dst = new_header + header_chunk_ptr_offset_pw(pw, 0) + iter * ptr_bytes
                function.instruction(&Instruction::LocalGet(s_new_ptr));
                function.instruction(&Instruction::LocalGet(s_iter));
                function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
                function.instruction(&ptr_mul(pw));
                function.instruction(&ptr_add(pw));
                // src = old_header + header_chunk_ptr_offset_pw(pw, 0) + iter * ptr_bytes
                function.instruction(&Instruction::LocalGet(s_header_ptr));
                function.instruction(&Instruction::LocalGet(s_iter));
                function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
                function.instruction(&ptr_mul(pw));
                function.instruction(&ptr_add(pw));
                function.instruction(&ptr_load(
                    pw,
                    MemArg {
                        offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                // iter += 1
                function.instruction(&Instruction::LocalGet(s_iter));
                function.instruction(&ptr_const(pw, 1));
                function.instruction(&ptr_add(pw));
                function.instruction(&Instruction::LocalSet(s_iter));
                function.instruction(&Instruction::Br(0));
                function.instruction(&Instruction::End); // end loop
                function.instruction(&Instruction::End); // end block
                // header_ptr = new_header
                function.instruction(&Instruction::LocalGet(s_new_ptr));
                function.instruction(&Instruction::LocalSet(s_header_ptr));
                function.instruction(&Instruction::End); // end if (grow header)

                // Store current chunk_ptr into header[chunk_ptr_offset(n_chunks)]
                // dst = header_ptr + 2*meta_bytes + n_chunks * ptr_bytes
                function.instruction(&Instruction::LocalGet(s_header_ptr));
                function.instruction(&Instruction::LocalGet(s_n_chunks));
                function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
                function.instruction(&ptr_mul(pw));
                function.instruction(&ptr_add(pw));
                function.instruction(&Instruction::LocalGet(s_chunk_ptr));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                // n_chunks += 1
                function.instruction(&Instruction::LocalGet(s_n_chunks));
                function.instruction(&ptr_const(pw, 1));
                function.instruction(&ptr_add(pw));
                function.instruction(&Instruction::LocalSet(s_n_chunks));
                // alloc new chunk; write chunk.len = 0
                function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, &hs, s_alloc_size, s_new_ptr);
                function.instruction(&Instruction::LocalGet(s_new_ptr));
                function.instruction(&ptr_const(pw, 0));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                // chunk_ptr = new_chunk; chunk_len = 0
                function.instruction(&Instruction::LocalGet(s_new_ptr));
                function.instruction(&Instruction::LocalSet(s_chunk_ptr));
                function.instruction(&ptr_const(pw, 0));
                function.instruction(&Instruction::LocalSet(s_chunk_len));

                function.instruction(&Instruction::End); // end if (chunk full)

                // Persist current builder state before evaluating the pushed value.
                // Value emission may allocate and reuse generic helper scratch locals.
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    header_ptr_local,
                    s_header_ptr,
                    pw,
                )?;
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    header_cap_local,
                    s_header_cap,
                    pw,
                )?;
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    n_chunks_local,
                    s_n_chunks,
                    pw,
                )?;
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    chunk_ptr_local,
                    s_chunk_ptr,
                    pw,
                )?;
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    total_len_local,
                    s_total_len,
                    pw,
                )?;

                // Evaluate value
                emit_instrs_with_heap_depth(
                    function,
                    ctx,
                    value_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    static_heap,
                    options,
                    function_returns_i64,
                    heap_base_depth + 1,
                    self_tail_loop_depth,
                )?;
                function.instruction(&Instruction::LocalSet(value_i64));

                // Re-load state after value evaluation; nested heap expressions may reuse
                // helper i32 scratch locals and clobber temporary builder locals.
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    header_ptr_local,
                    s_header_ptr,
                    pw,
                )?;
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    header_cap_local,
                    s_header_cap,
                    pw,
                )?;
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    n_chunks_local,
                    s_n_chunks,
                    pw,
                )?;
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    chunk_ptr_local,
                    s_chunk_ptr,
                    pw,
                )?;
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    total_len_local,
                    s_total_len,
                    pw,
                )?;
                function.instruction(&Instruction::LocalGet(s_chunk_ptr));
                function.instruction(&ptr_load(
                    pw,
                    MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                function.instruction(&Instruction::LocalSet(s_chunk_len));

                // chunk[meta_bytes + chunk_len * 8] = value
                function.instruction(&Instruction::LocalGet(s_chunk_ptr));
                function.instruction(&Instruction::LocalGet(s_chunk_len));
                function.instruction(&ptr_const(pw, 8));
                function.instruction(&ptr_mul(pw));
                function.instruction(&ptr_add(pw));
                function.instruction(&Instruction::LocalGet(value_i64));
                function.instruction(&Instruction::I64Store(MemArg {
                    offset: chunk_item_offset_pw(pw, 0) as u64,
                    align: 3,
                    memory_index: 0,
                }));

                // chunk.len = chunk_len + 1
                function.instruction(&Instruction::LocalGet(s_chunk_ptr));
                function.instruction(&Instruction::LocalGet(s_chunk_len));
                function.instruction(&ptr_const(pw, 1));
                function.instruction(&ptr_add(pw));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    },
                ));

                // total_len += 1
                function.instruction(&Instruction::LocalGet(s_total_len));
                function.instruction(&ptr_const(pw, 1));
                function.instruction(&ptr_add(pw));
                function.instruction(&Instruction::LocalSet(s_total_len));

                // Save back all state
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    header_ptr_local,
                    s_header_ptr,
                    pw,
                )?;
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    header_cap_local,
                    s_header_cap,
                    pw,
                )?;
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    n_chunks_local,
                    s_n_chunks,
                    pw,
                )?;
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    chunk_ptr_local,
                    s_chunk_ptr,
                    pw,
                )?;
                emit_store_raw_ptr_into_named_i64_local(
                    function,
                    ctx,
                    total_len_local,
                    s_total_len,
                    pw,
                )?;
            }

            WasmBackendInstr::ListBuilderFinish {
                header_ptr_local,
                n_chunks_local,
                total_len_local,
            } => {
                // Chunked Sequence Builder — Finish.
                // Stores the current (last) chunk_ptr into the header and writes total_len +
                // n_chunks into the header, then returns a tagged header pointer.
                //
                // NOTE: chunk_ptr_local is stored in a named i64 local in the caller context.
                // We need to finalize: store the last chunk into header[8 + n_chunks * 4],
                // then write header.total_len and header.n_chunks.
                let hs = require_heap_state(helper_state, "ListBuilderFinish")?;
                let pw = hs.pw();
                let s_header_ptr = hs.scratch.i32_base + HS_TEXT_PTR;
                let s_n_chunks = hs.scratch.i32_base + HS_TEXT_LEN;
                let s_total_len = hs.scratch.i32_base + HS_SEP_PTR;
                let s_chunk_ptr = hs.scratch.i32_base + HS_AUX_PTR;

                // Load state
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    header_ptr_local,
                    s_header_ptr,
                    pw,
                )?;
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    n_chunks_local,
                    s_n_chunks,
                    pw,
                )?;
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    total_len_local,
                    s_total_len,
                    pw,
                )?;

                // We need chunk_ptr_local to finalize the last chunk. Derive its local name from
                // the naming convention used in lower.rs (__rr4_list_chunk_ptr).
                // Access via the sibling local: its name differs only in the suffix.
                // Since header_ptr_local is e.g. "__rr4_list_header_ptr", chunk_ptr_local is
                // "__rr4_list_chunk_ptr". We must load it from ctx.
                // The chunk_ptr is stored in the builder locals; reconstruct its name by
                // replacing "header_ptr" suffix with "chunk_ptr".
                let chunk_ptr_local_name = header_ptr_local.replace("header_ptr", "chunk_ptr");
                emit_load_raw_ptr_from_named_i64_local(
                    function,
                    ctx,
                    &chunk_ptr_local_name,
                    s_chunk_ptr,
                    pw,
                )?;

                // Store last chunk_ptr into header[chunk_ptr_offset(n_chunks)]
                // Only do this if total_len > 0
                function.instruction(&Instruction::LocalGet(s_total_len));
                function.instruction(&ptr_const(pw, 0));
                function.instruction(&ptr_eq(pw));
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                function.instruction(&Instruction::Else);
                function.instruction(&Instruction::LocalGet(s_header_ptr));
                function.instruction(&Instruction::LocalGet(s_n_chunks));
                function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
                function.instruction(&ptr_mul(pw));
                function.instruction(&ptr_add(pw));
                function.instruction(&Instruction::LocalGet(s_chunk_ptr));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                function.instruction(&Instruction::LocalGet(s_n_chunks));
                function.instruction(&ptr_const(pw, 1));
                function.instruction(&ptr_add(pw));
                function.instruction(&Instruction::LocalSet(s_n_chunks));
                function.instruction(&Instruction::End); // end if (total_len > 0)

                // Write header.total_len and header.n_chunks
                function.instruction(&Instruction::LocalGet(s_header_ptr));
                function.instruction(&Instruction::LocalGet(s_total_len));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: header_total_len_offset() as u64,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                function.instruction(&Instruction::LocalGet(s_header_ptr));
                function.instruction(&Instruction::LocalGet(s_n_chunks));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: header_n_chunks_offset_pw(pw) as u64,
                        align: 2,
                        memory_index: 0,
                    },
                ));

                // Push tagged header pointer as result
                emit_push_tagged_ptr(function, s_header_ptr, TAG_LIST, pw);
            }

            WasmBackendInstr::TupleLit { element_instrs } => {
                let hs = require_heap_state(helper_state, "TupleLit")?;
                let s_tuple_ptr = hs.scratch.i32_base + HELPER_SCRATCH_I32 + heap_base_depth;
                let s_alloc_ptr = hs.scratch.i32_base + HS_AUX_PTR;
                let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE;
                let pw = hs.pw();
                let n = element_instrs.len() as u64;
                // Tuple layout: meta_bytes arity + n * 8 bytes items
                function.instruction(&ptr_const(pw, meta_slot_bytes(pw) as u64 + 8 * n));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, &hs, s_alloc_size, s_alloc_ptr);
                function.instruction(&Instruction::LocalGet(s_alloc_ptr));
                function.instruction(&Instruction::LocalSet(s_tuple_ptr));
                // Write arity
                function.instruction(&Instruction::LocalGet(s_tuple_ptr));
                function.instruction(&ptr_const(pw, n));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                for (i, elem_instrs) in element_instrs.iter().enumerate() {
                    function.instruction(&Instruction::LocalGet(s_tuple_ptr));
                    function.instruction(&ptr_const(pw, meta_slot_bytes(pw) as u64 + 8 * i as u64));
                    function.instruction(&ptr_add(pw));
                    emit_instrs_with_heap_depth(
                        function,
                        ctx,
                        elem_instrs,
                        layout,
                        named_i64_count,
                        helper_i64_scratch_count,
                        i32_base,
                        static_strings,
                        static_heap,
                        options,
                        function_returns_i64,
                        heap_base_depth + 1,
                        self_tail_loop_depth,
                    )?;
                    function.instruction(&Instruction::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                emit_push_tagged_ptr(function, s_tuple_ptr, TAG_TUPLE, pw);
            }

            WasmBackendInstr::RecordLit {
                constructor,
                field_instrs,
            } => {
                let hs = require_heap_state(helper_state, "RecordLit")?;
                let pw = hs.pw();
                let ctor_tag = ctx.record_ctor_tag(constructor)? as i64;
                let s_record_ptr = hs.scratch.i32_base + HELPER_SCRATCH_I32 + heap_base_depth;
                let s_alloc_ptr = hs.scratch.i32_base + HS_AUX_PTR;
                let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE;
                let n = field_instrs.len() as u64;
                // Record layout: 8-byte ctor_tag + n * 8 bytes fields
                function.instruction(&ptr_const(pw, 8 + 8 * n));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, &hs, s_alloc_size, s_alloc_ptr);
                function.instruction(&Instruction::LocalGet(s_alloc_ptr));
                function.instruction(&Instruction::LocalSet(s_record_ptr));
                // Write ctor tag (always i64 regardless of pw)
                function.instruction(&Instruction::LocalGet(s_record_ptr));
                function.instruction(&Instruction::I64Const(ctor_tag));
                function.instruction(&Instruction::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
                for (i, field_instrs) in field_instrs.iter().enumerate() {
                    function.instruction(&Instruction::LocalGet(s_record_ptr));
                    function.instruction(&ptr_const(pw, 8 + 8 * i as u64));
                    function.instruction(&ptr_add(pw));
                    emit_instrs_with_heap_depth(
                        function,
                        ctx,
                        field_instrs,
                        layout,
                        named_i64_count,
                        helper_i64_scratch_count,
                        i32_base,
                        static_strings,
                        static_heap,
                        options,
                        function_returns_i64,
                        heap_base_depth + 1,
                        self_tail_loop_depth,
                    )?;
                    function.instruction(&Instruction::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                emit_push_tagged_ptr(function, s_record_ptr, TAG_RECORD, pw);
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
                    pw,
                )?;
            }

            WasmBackendInstr::ListReverseFoldPrepend {
                list_instrs,
                item_local,
                prefix_element_instrs,
            } => {
                let hs = require_heap_state(helper_state, "ListReverseFoldPrepend")?;
                let item_local_idx = ctx.get(item_local)?;
                emit_instrs(
                    function,
                    ctx,
                    list_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    static_heap,
                    options,
                    function_returns_i64,
                    self_tail_loop_depth,
                )?;
                emit_list_reverse_fold_prepend(
                    function,
                    ctx,
                    &hs,
                    item_local_idx,
                    prefix_element_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    static_heap,
                    &options,
                    function_returns_i64,
                )?;
            }

            WasmBackendInstr::TupleGet { tuple_local, index } => {
                // Load the tagged tuple pointer from the local.
                let tuple_slot = ctx.get(tuple_local)?;
                function.instruction(&Instruction::LocalGet(tuple_slot));
                // Unwrap the tagged i64 to a pointer of the correct width.
                // Tag occupies the high 4 bits; the payload in the low 32 bits is the pointer.
                function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
                function.instruction(&Instruction::I64And);
                if let Some(wrap) = ptr_wrap_from_i64(pw) {
                    function.instruction(&wrap);
                }
                // Compute byte offset: meta_slot_bytes(pw) (arity header) + 8 * index.
                let field_offset =
                    (8u64)
                        .checked_mul(*index as u64)
                        .ok_or_else(|| CodegenError {
                            message: format!(
                                "gen_lower/emit: TupleGet index {} overflows byte offset",
                                index
                            ),
                        })?;
                let byte_offset = (meta_slot_bytes(pw) as u64)
                    .checked_add(field_offset)
                    .ok_or_else(|| CodegenError {
                        message: format!(
                            "gen_lower/emit: TupleGet index {} overflows byte offset",
                            index
                        ),
                    })?;
                function.instruction(&ptr_const(pw, byte_offset));
                function.instruction(&ptr_add(pw));
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
                let hs = require_heap_state(helper_state, "CreateClosure")?;
                let pw = hs.pw();
                let s_closure_ptr = hs.scratch.i32_base + HELPER_SCRATCH_I32 + heap_base_depth;
                let s_aux = hs.scratch.i32_base + HS_AUX_PTR; // temp for alloc, then discarded
                let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE;
                let n = slot_instrs.len() as u64;
                // alloc_size = 8 + 8 * n  (8 bytes for func_handle, 8 bytes per slot)
                function.instruction(&ptr_const(pw, 8 + 8 * n));
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
                    static_heap,
                    options,
                    function_returns_i64,
                    heap_base_depth + 1,
                    self_tail_loop_depth,
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
                    function.instruction(&ptr_const(pw, 8 + 8 * i as u64));
                    function.instruction(&ptr_add(pw));
                    emit_instrs_with_heap_depth(
                        function,
                        ctx,
                        slot_instrs_i,
                        layout,
                        named_i64_count,
                        helper_i64_scratch_count,
                        i32_base,
                        static_strings,
                        static_heap,
                        options,
                        function_returns_i64,
                        heap_base_depth + 1,
                        self_tail_loop_depth,
                    )?;
                    function.instruction(&Instruction::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                // Push TAG_CLOSURE-tagged pointer using the dedicated closure base slot.
                emit_push_tagged_ptr(function, s_closure_ptr, TAG_CLOSURE, pw);
            }

            WasmBackendInstr::LoadClosureSlot {
                closure_local,
                slot_index,
            } => {
                // Load the TAG_CLOSURE pointer from the local, decode to ptr width, add slot offset.
                let slot = ctx.get(closure_local)?;
                function.instruction(&Instruction::LocalGet(slot));
                function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
                function.instruction(&Instruction::I64And);
                if let Some(wrap) = ptr_wrap_from_i64(pw) {
                    function.instruction(&wrap);
                }
                let byte_offset = 8u64
                    .checked_add(8u64.checked_mul(*slot_index as u64).ok_or_else(|| {
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
                function.instruction(&ptr_const(pw, byte_offset));
                function.instruction(&ptr_add(pw));
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
                let hs = require_heap_state(helper_state, "AllocMutableCell")?;
                let pw = hs.pw();
                let s_cell_ptr = hs.scratch.i32_base + HS_AUX_PTR;
                let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE;
                function.instruction(&ptr_const(pw, 8));
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
                    static_heap,
                    options,
                    function_returns_i64,
                    self_tail_loop_depth,
                )?;
                function.instruction(&Instruction::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
                // Push TAG_CELL-tagged pointer as result.
                emit_push_tagged_ptr(function, s_cell_ptr, TAG_CELL, pw);
            }

            WasmBackendInstr::LoadCellValue => {
                // Stack top: TAG_CELL-tagged i64 pointer.
                // Decode to ptr-width pointer, load i64 at offset 0.
                function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
                function.instruction(&Instruction::I64And);
                if let Some(wrap) = ptr_wrap_from_i64(pw) {
                    function.instruction(&wrap);
                }
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
                // Emit cell pointer (TAG_CELL i64), decode to ptr-width pointer.
                emit_instrs(
                    function,
                    ctx,
                    cell_ptr_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    static_heap,
                    options,
                    function_returns_i64,
                    self_tail_loop_depth,
                )?;
                function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
                function.instruction(&Instruction::I64And);
                if let Some(wrap) = ptr_wrap_from_i64(pw) {
                    function.instruction(&wrap);
                }
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
                    static_heap,
                    options,
                    function_returns_i64,
                    self_tail_loop_depth,
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
    pw: PtrWidth,
) {
    // WASI writes a 4-byte i32 nread value — use I32Load always.
    // Address must be ptr-width (i64 in W64), but the loaded value is i32.
    function.instruction(&ptr_const(pw, nread_offset as u64));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I64ExtendI32U);
    }
    function.instruction(&ptr_const(pw, (buffer_ptr + 4) as u64));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, 7));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, !7u64));
    function.instruction(match pw {
        PtrWidth::W32 => &Instruction::I32And,
        PtrWidth::W64 => &Instruction::I64And,
    });
    function.instruction(&Instruction::LocalSet(heap_floor_local));
    emit_clamp_heap_floor_to_host_bump_cursor(function, heap_floor_local, pw);
}

fn emit_update_heap_floor_from_local_len(
    function: &mut Function,
    heap_floor_local: u32,
    buffer_ptr: i32,
    len_local: u32,
    pw: PtrWidth,
) {
    function.instruction(&Instruction::LocalGet(len_local));
    function.instruction(&ptr_const(pw, (buffer_ptr + 4) as u64));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, 7));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, !7u64));
    function.instruction(match pw {
        PtrWidth::W32 => &Instruction::I32And,
        PtrWidth::W64 => &Instruction::I64And,
    });
    function.instruction(&Instruction::LocalSet(heap_floor_local));
    emit_clamp_heap_floor_to_host_bump_cursor(function, heap_floor_local, pw);
}

fn emit_clamp_heap_floor_to_host_bump_cursor(
    function: &mut Function,
    heap_floor_local: u32,
    pw: PtrWidth,
) {
    function.instruction(&Instruction::LocalGet(heap_floor_local));
    function.instruction(&ptr_const(pw, GLOBAL_HOST_BUMP_CURSOR_OFFSET as u64));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I64ExtendI32U);
    }
    function.instruction(&ptr_lt_u(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&ptr_const(pw, GLOBAL_HOST_BUMP_CURSOR_OFFSET as u64));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I64ExtendI32U);
    }
    function.instruction(&Instruction::LocalSet(heap_floor_local));
    function.instruction(&Instruction::End);
}

fn emit_helper_call(
    function: &mut Function,
    ctx: &EmitContext,
    intrinsic: BackendIntrinsic,
    i32_base: u32,
    scratch_state: Option<&EmitScratchState>,
    heap_state: Option<&HeapEmitState>,
    pw: PtrWidth,
) -> Result<(), CodegenError> {
    match intrinsic {
        BackendIntrinsic::StringSplit => emit_string_split_helper(
            function,
            heap_state.ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: intrinsic '{intrinsic:?}' requires heap helper state"
                ),
            })?,
        ),
        BackendIntrinsic::ListGet => emit_list_get_helper(
            function,
            scratch_state.ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: intrinsic '{intrinsic:?}' requires helper scratch state"
                ),
            })?,
            pw,
        ),
        BackendIntrinsic::StringLength => emit_string_length_helper(
            function,
            scratch_state.ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: intrinsic '{intrinsic:?}' requires helper scratch state"
                ),
            })?,
            pw,
        ),
        BackendIntrinsic::ValueToString
        | BackendIntrinsic::StringEachGraphemeCount
        | BackendIntrinsic::StringEachGraphemeState
        | BackendIntrinsic::StringConcat
        | BackendIntrinsic::ListJoinString
        | BackendIntrinsic::StringGraphemesList
        | BackendIntrinsic::StringSplitLines => {
            if let Some(hs) = heap_state {
                emit_sync_cursor_to_global(function, hs.alloc_cursor_local, hs.pw());
                emit_sync_floor_to_global(function, hs.heap_floor_local, hs.pw());
            }
            let host_offset = host_import_offset_for_intrinsic(intrinsic)?;
            function.instruction(&Instruction::Call(HOST_IMPORT_BASE_IDX + host_offset));
            if let Some(hs) = heap_state {
                emit_return_if_runtime_error(function, hs);
                emit_sync_cursor_from_global(function, hs.alloc_cursor_local, hs.pw());
                emit_sync_floor_from_global(function, hs.heap_floor_local, hs.pw());
                emit_clamp_heap_floor_to_host_bump_cursor(function, hs.heap_floor_local, hs.pw());
                emit_sync_floor_to_global(function, hs.heap_floor_local, hs.pw());
            }
            Ok(())
        }
        BackendIntrinsic::ListPushString => emit_list_push_string_helper(
            function,
            heap_state.ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: intrinsic '{intrinsic:?}' requires heap helper state"
                ),
            })?,
        ),
        BackendIntrinsic::ListSet => emit_list_set_helper(
            function,
            heap_state.ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: intrinsic '{intrinsic:?}' requires heap helper state"
                ),
            })?,
        ),
        BackendIntrinsic::ListSetInPlace => emit_list_set_in_place_helper(
            function,
            heap_state.ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: intrinsic '{intrinsic:?}' requires heap helper state"
                ),
            })?,
        ),
        BackendIntrinsic::ListConcat => emit_list_concat_helper(
            function,
            heap_state.ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: intrinsic '{intrinsic:?}' requires heap helper state"
                ),
            })?,
            ctx.goby_drop_func_idx,
        ),
        BackendIntrinsic::ListLength => emit_list_length_helper(
            function,
            scratch_state.ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: intrinsic '{intrinsic:?}' requires helper scratch state"
                ),
            })?,
            pw,
        ),
        BackendIntrinsic::ListFold => {
            let hs = heap_state.ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: intrinsic '{intrinsic:?}' requires heap helper state"
                ),
            })?;
            let type_idx = ctx.indirect_call_type_idx(2)?;
            let closure_type_idx = ctx.indirect_call_type_idx(3)?;
            emit_list_fold_helper(function, hs, type_idx, closure_type_idx)
        }
        BackendIntrinsic::ListMap => {
            let hs = heap_state.ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: intrinsic '{intrinsic:?}' requires heap helper state"
                ),
            })?;
            let type_idx = ctx.indirect_call_type_idx(1)?;
            let closure_type_idx = ctx.indirect_call_type_idx(2)?;
            emit_list_map(function, hs, type_idx, closure_type_idx)
        }
    }?;
    let _ = (ctx, i32_base);
    Ok(())
}

fn host_import_offset_for_intrinsic(intrinsic: BackendIntrinsic) -> Result<u32, CodegenError> {
    let host_import = host_import_for_intrinsic(intrinsic).ok_or_else(|| CodegenError {
        message: format!(
            "gen_lower/emit: missing host import mapping for intrinsic '{intrinsic:?}'"
        ),
    })?;
    HOST_INTRINSIC_IMPORTS
        .iter()
        .position(|candidate| candidate == &host_import)
        .map(|idx| idx as u32)
        .ok_or_else(|| CodegenError {
            message: format!("gen_lower/emit: missing host import index for '{host_import:?}'"),
        })
}

fn emit_decode_string_ptr(
    function: &mut Function,
    helper_state: &EmitScratchState,
    source_i64_local: u32,
    ptr_local: u32,
    len_local: u32,
    pw: PtrWidth,
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
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(ptr_local));

    // Load string length prefix: always i32 regardless of pw (string len is 4-byte field).
    // In W64: address is i64 (from ptr_local) but the stored value is still i32.
    // Use I32Load (not I64Load) to read the 4-byte length field.
    // After loading i32, optionally extend to i64 for W64 scratch local.
    function.instruction(&Instruction::LocalGet(ptr_local));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    // W64: scratch local is i64, so zero-extend the i32 length value
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I64ExtendI32U);
    }
    function.instruction(&Instruction::LocalSet(len_local));

    let _ = helper_state;
}

fn emit_load_raw_ptr_from_named_i64_local(
    function: &mut Function,
    ctx: &EmitContext,
    local_name: &str,
    scratch_local: u32,
    pw: PtrWidth,
) -> Result<(), CodegenError> {
    let idx = ctx.get(local_name)?;
    function.instruction(&Instruction::LocalGet(idx));
    match pw {
        PtrWidth::W32 => {
            function.instruction(&Instruction::I32WrapI64);
        }
        PtrWidth::W64 => {
            // named i64 local already holds a raw i64 address (no tag bits here)
        }
    }
    function.instruction(&Instruction::LocalSet(scratch_local));
    Ok(())
}

fn emit_store_raw_ptr_into_named_i64_local(
    function: &mut Function,
    ctx: &EmitContext,
    local_name: &str,
    scratch_local: u32,
    pw: PtrWidth,
) -> Result<(), CodegenError> {
    let idx = ctx.get(local_name)?;
    function.instruction(&Instruction::LocalGet(scratch_local));
    match pw {
        PtrWidth::W32 => {
            function.instruction(&Instruction::I64ExtendI32U);
        }
        PtrWidth::W64 => {
            // scratch_local is already i64; store directly
        }
    }
    function.instruction(&Instruction::LocalSet(idx));
    Ok(())
}

fn emit_compare_decoded_strings(
    function: &mut Function,
    left_ptr_local: u32,
    left_len_local: u32,
    right_ptr_local: u32,
    right_len_local: u32,
    iter_local: u32,
    result_local: u32,
    pw: PtrWidth,
) {
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(result_local));
    function.instruction(&Instruction::LocalGet(left_len_local));
    function.instruction(&Instruction::LocalGet(right_len_local));
    function.instruction(&ptr_eq(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&Instruction::LocalSet(result_local));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(iter_local));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(iter_local));
    function.instruction(&Instruction::LocalGet(left_len_local));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));
    // left_ptr + 4 (length field) + iter
    function.instruction(&Instruction::LocalGet(left_ptr_local));
    function.instruction(&ptr_const(pw, 4)); // string length prefix is 4 bytes
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(iter_local));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load_8u(
        pw,
        MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        },
    ));
    // right_ptr + 4 + iter
    function.instruction(&Instruction::LocalGet(right_ptr_local));
    function.instruction(&ptr_const(pw, 4));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(iter_local));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load_8u(
        pw,
        MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        },
    ));
    // byte comparison: ptr_load_8u returns i32 (W32) or i64 (W64)
    match pw {
        PtrWidth::W32 => function.instruction(&Instruction::I32Ne),
        PtrWidth::W64 => function.instruction(&Instruction::I64Ne),
    };
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(result_local));
    function.instruction(&Instruction::Br(2));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(iter_local));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(iter_local));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
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
    static_heap: &StaticHeapPool,
    total_static_reserved_bytes: u32,
    options: EmitOptions,
    emit_epilogue_cursor_sync: bool,
    tail_call_mode: Option<TailCallMode>,
    // Base index of the 3 extra i64 scratch locals reserved for the debug_alloc_stats epilogue.
    // Only meaningful when `!emit_epilogue_cursor_sync && options.debug_alloc_stats`.
    stats_scratch_base: u32,
) -> Result<(), CodegenError> {
    ctx.tail_call_mode = tail_call_mode;
    let has_heap = needs_helper_state(instrs);
    if has_heap {
        let pw = PtrWidth::from_memory64(options.memory_config.memory64);
        initialize_helper_state_locals(
            function,
            layout,
            i32_base,
            total_static_reserved_bytes,
            !emit_epilogue_cursor_sync,
            pw,
        )?;
    }
    if ctx.tail_call_mode.is_some() {
        function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Result(
            wasm_encoder::ValType::I64,
        )));
        emit_instrs(
            function,
            ctx,
            instrs,
            layout,
            named_i64_count,
            helper_i64_scratch_count,
            i32_base,
            static_strings,
            static_heap,
            options,
            emit_epilogue_cursor_sync,
            0,
        )?;
        function.instruction(&Instruction::End);
    } else {
        emit_instrs(
            function,
            ctx,
            instrs,
            layout,
            named_i64_count,
            helper_i64_scratch_count,
            i32_base,
            static_strings,
            static_heap,
            options,
            emit_epilogue_cursor_sync,
            0,
        )?;
    }
    if has_heap && emit_epilogue_cursor_sync {
        let alloc_cursor_local = i32_base + HELPER_ALLOC_CURSOR_OFFSET;
        let heap_floor_local = i32_base + HELPER_HEAP_FLOOR_OFFSET;
        let pw = PtrWidth::from_memory64(options.memory_config.memory64);
        emit_sync_cursor_to_global(function, alloc_cursor_local, pw);
        emit_sync_floor_to_global(function, heap_floor_local, pw);
    }
    // is_main == !emit_epilogue_cursor_sync. Stats epilogue is memory64-only
    // (Perceus plan §3.1: refcount is i64, Goby is memory64-only).
    if !emit_epilogue_cursor_sync && options.memory_config.memory64 {
        let pw = PtrWidth::W64;
        if options.debug_alloc_stats {
            emit_alloc_stats_line(function, layout, pw, stats_scratch_base);
        }
    }
    function.instruction(&Instruction::End);
    ctx.tail_call_mode = None;
    Ok(())
}

/// Emit instructions that write the frozen alloc-stats line to fd=2 (stderr).
///
/// Format: `alloc-stats: total_bytes=N peak_bytes=M free_list_hits=H\n`
///
/// Uses a scratch window above `HEAP_BASE` as a stats buffer.
/// The extra gap keeps the text buffer disjoint from the nearby global
/// bookkeeping slots while remaining safe to overwrite at `_start` exit:
/// stdin has already been consumed and the heap cursor is no longer in use by
/// the epilogue.
///
/// `stats_scratch_base`: index of the first of 3 consecutive i64 scratch locals
/// allocated in the caller's function local slot table.
fn emit_alloc_stats_line(
    function: &mut Function,
    layout: &MemoryLayout,
    pw: PtrWidth,
    stats_scratch_base: u32,
) {
    let s_val = stats_scratch_base;
    let s_ptr = stats_scratch_base + 1;
    let s_tmp = stats_scratch_base + 2;

    // Keep the stats buffer away from the low-memory global bookkeeping slots.
    let buf_start = i64::from(layout.heap_base) + 64;

    // Initialize s_ptr = buf_start
    function.instruction(&Instruction::I64Const(buf_start));
    function.instruction(&Instruction::LocalSet(s_ptr));

    // Write prefix: "alloc-stats: total_bytes="
    for &b in b"alloc-stats: total_bytes=" {
        function.instruction(&Instruction::LocalGet(s_ptr));
        function.instruction(&Instruction::I64Const(b as i64));
        function.instruction(&Instruction::I64Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));
        function.instruction(&Instruction::LocalGet(s_ptr));
        function.instruction(&Instruction::I64Const(1));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::LocalSet(s_ptr));
    }

    // Load total_bytes into s_val and convert
    function.instruction(&ptr_const(pw, GLOBAL_ALLOC_BYTES_TOTAL_OFFSET as u64));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_val));
    emit_i64_to_decimal(function, s_val, s_ptr, s_tmp);

    // " peak_bytes="
    for &b in b" peak_bytes=" {
        function.instruction(&Instruction::LocalGet(s_ptr));
        function.instruction(&Instruction::I64Const(b as i64));
        function.instruction(&Instruction::I64Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));
        function.instruction(&Instruction::LocalGet(s_ptr));
        function.instruction(&Instruction::I64Const(1));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::LocalSet(s_ptr));
    }

    // Load peak_bytes into s_val and convert
    function.instruction(&ptr_const(pw, GLOBAL_PEAK_BYTES_OFFSET as u64));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_val));
    emit_i64_to_decimal(function, s_val, s_ptr, s_tmp);

    // " free_list_hits="
    for &b in b" free_list_hits=" {
        function.instruction(&Instruction::LocalGet(s_ptr));
        function.instruction(&Instruction::I64Const(b as i64));
        function.instruction(&Instruction::I64Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));
        function.instruction(&Instruction::LocalGet(s_ptr));
        function.instruction(&Instruction::I64Const(1));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::LocalSet(s_ptr));
    }

    // Load free_list_hits into s_val and convert
    function.instruction(&ptr_const(pw, GLOBAL_FREE_LIST_HITS_OFFSET as u64));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_val));
    emit_i64_to_decimal(function, s_val, s_ptr, s_tmp);

    // " reuse_hits="
    for &b in b" reuse_hits=" {
        function.instruction(&Instruction::LocalGet(s_ptr));
        function.instruction(&Instruction::I64Const(b as i64));
        function.instruction(&Instruction::I64Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));
        function.instruction(&Instruction::LocalGet(s_ptr));
        function.instruction(&Instruction::I64Const(1));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::LocalSet(s_ptr));
    }

    // Load reuse_hits into s_val and convert
    function.instruction(&ptr_const(pw, GLOBAL_REUSE_HITS_OFFSET as u64));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_val));
    emit_i64_to_decimal(function, s_val, s_ptr, s_tmp);

    // '\n'
    function.instruction(&Instruction::LocalGet(s_ptr));
    function.instruction(&Instruction::I64Const(b'\n' as i64));
    function.instruction(&Instruction::I64Store8(MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalGet(s_ptr));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Add);
    function.instruction(&Instruction::LocalSet(s_ptr));

    // len = s_ptr - buf_start
    // iovec[0] = buf_start (i32), iovec[1] = len (i32)
    // fd_write(2, iovec_offset, 1, nwritten_offset)
    let iovec_offset = layout.iovec_offset as i32;
    let nwritten_offset = layout.nwritten_offset as i32;

    // iovec[0] = buf_start pointer (i32 field), iovec[1] = byte length (i32 field).
    // Addresses for I32Store must match the module's pointer width.
    function.instruction(&ptr_const(pw, iovec_offset as u64));
    function.instruction(&Instruction::I32Const(buf_start as i32));
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&ptr_const(pw, (iovec_offset + 4) as u64));
    function.instruction(&Instruction::LocalGet(s_ptr));
    function.instruction(&Instruction::I64Const(buf_start));
    function.instruction(&Instruction::I64Sub);
    function.instruction(&Instruction::I32WrapI64);
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    // fd_write(fd=2, iovec_ptr, iovs_len=1, nwritten_ptr) — all args are i32 in WASI Preview 1.
    function.instruction(&Instruction::I32Const(2)); // stderr
    function.instruction(&Instruction::I32Const(iovec_offset));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Const(nwritten_offset));
    function.instruction(&Instruction::Call(FD_WRITE_IDX));
    function.instruction(&Instruction::Drop); // discard errno
}

/// Emit instructions to convert the i64 in `val_local` to decimal ASCII,
/// appending the digits at the address in `ptr_local` (advancing `ptr_local`).
/// Uses `tmp_local` as a scratch register.
fn emit_i64_to_decimal(function: &mut Function, val_local: u32, ptr_local: u32, tmp_local: u32) {
    // Special case: 0 → write '0'
    function.instruction(&Instruction::LocalGet(val_local));
    function.instruction(&Instruction::I64Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(ptr_local));
    function.instruction(&Instruction::I64Const(b'0' as i64));
    function.instruction(&Instruction::I64Store8(MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalGet(ptr_local));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Add);
    function.instruction(&Instruction::LocalSet(ptr_local));
    function.instruction(&Instruction::Else);
    // Non-zero: write digits right-to-left into a 20-byte window starting at ptr_local,
    // then copy the used portion to the front.
    // tmp_local = ptr_local + 19 (rightmost slot)
    function.instruction(&Instruction::LocalGet(ptr_local));
    function.instruction(&Instruction::I64Const(19));
    function.instruction(&Instruction::I64Add);
    function.instruction(&Instruction::LocalSet(tmp_local));
    // digit extraction loop
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(val_local));
    function.instruction(&Instruction::I64Eqz);
    function.instruction(&Instruction::BrIf(1));
    // digit = val % 10 + '0', store at tmp_local, tmp_local--
    function.instruction(&Instruction::LocalGet(tmp_local));
    function.instruction(&Instruction::LocalGet(val_local));
    function.instruction(&Instruction::I64Const(10));
    function.instruction(&Instruction::I64RemU);
    function.instruction(&Instruction::I64Const(b'0' as i64));
    function.instruction(&Instruction::I64Add);
    function.instruction(&Instruction::I64Store8(MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalGet(tmp_local));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Sub);
    function.instruction(&Instruction::LocalSet(tmp_local));
    function.instruction(&Instruction::LocalGet(val_local));
    function.instruction(&Instruction::I64Const(10));
    function.instruction(&Instruction::I64DivU);
    function.instruction(&Instruction::LocalSet(val_local));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End); // loop
    function.instruction(&Instruction::End); // block
    // Copy digits from [tmp_local+1 .. ptr_local+20) to [ptr_local .. ptr_local+count).
    // val_local is now 0 after the digit loop — reuse it to hold the fixed end-of-window.
    function.instruction(&Instruction::LocalGet(ptr_local));
    function.instruction(&Instruction::I64Const(20));
    function.instruction(&Instruction::I64Add);
    function.instruction(&Instruction::LocalSet(val_local)); // val_local = end_of_window (fixed)
    // src = tmp_local + 1
    function.instruction(&Instruction::LocalGet(tmp_local));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Add);
    function.instruction(&Instruction::LocalSet(tmp_local)); // tmp = src
    // copy loop: exit when src >= end_of_window (fixed)
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(tmp_local));
    function.instruction(&Instruction::LocalGet(val_local)); // fixed end
    function.instruction(&Instruction::I64GeU);
    function.instruction(&Instruction::BrIf(1));
    // *ptr_local = *tmp_local (src); advance both
    function.instruction(&Instruction::LocalGet(ptr_local));
    function.instruction(&Instruction::LocalGet(tmp_local));
    function.instruction(&Instruction::I64Load8U(MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I64Store8(MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalGet(ptr_local));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Add);
    function.instruction(&Instruction::LocalSet(ptr_local));
    function.instruction(&Instruction::LocalGet(tmp_local));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Add);
    function.instruction(&Instruction::LocalSet(tmp_local));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End); // loop
    function.instruction(&Instruction::End); // block
    function.instruction(&Instruction::End); // if
}

#[allow(clippy::too_many_arguments)]
fn emit_tail_call_group_dispatcher(
    code: &mut CodeSection,
    group_plan: &TailCallGroupPlan,
    aux_decls: &[AuxDecl],
    decl_func_indices: &HashMap<String, u32>,
    decl_returns_wasm_heap: &HashMap<String, bool>,
    decl_uses_heap: &HashMap<String, bool>,
    decl_table_slots: &HashMap<String, u32>,
    goby_dup_func_idx: u32,
    goby_drop_func_idx: u32,
    indirect_call_type_idx_1: Option<u32>,
    indirect_call_type_idx_2: Option<u32>,
    indirect_call_type_idx_3: Option<u32>,
    record_ctor_tags: &HashMap<String, u32>,
    static_strings: &StaticStringPool,
    static_heap: &StaticHeapPool,
    total_static_reserved_bytes: u32,
    layout: &MemoryLayout,
    options: EmitOptions,
) -> Result<(), CodegenError> {
    let member_by_name: HashMap<&str, &AuxDecl> = aux_decls
        .iter()
        .map(|decl| (decl.decl_name.as_str(), decl))
        .collect();
    let mut named_local_bases = HashMap::new();
    let mut next_named_local_base = 1 + group_plan.max_arity as u32;
    let mut total_named_i64_count = 0u32;
    let mut helper_i64_scratch_count = 0u32;
    let mut i32_scratch_count = 0u32;
    let mut needs_heap = false;
    for member_name in &group_plan.member_names {
        let decl = member_by_name
            .get(member_name.as_str())
            .copied()
            .ok_or_else(|| CodegenError {
                message: format!(
                    "gen_lower/emit: missing aux decl for tail-call group member '{member_name}'"
                ),
            })?;
        let named_count = collect_all_instrs(&decl.instrs)
            .into_iter()
            .filter(|instr| matches!(instr, WasmBackendInstr::DeclareLocal { .. }))
            .count() as u32;
        named_local_bases.insert(member_name.clone(), next_named_local_base);
        next_named_local_base += named_count;
        total_named_i64_count += named_count;
        helper_i64_scratch_count =
            helper_i64_scratch_count.max(required_i64_scratch_count(&decl.instrs));
        i32_scratch_count = i32_scratch_count.max(required_i32_scratch_count(&decl.instrs));
        needs_heap |= needs_helper_state(&decl.instrs);
    }
    let group_arg_scratch_count = group_plan.max_arity as u32;
    let dispatcher_param_count = 1 + group_plan.max_arity as u32;
    let total_i64_count =
        total_named_i64_count + group_arg_scratch_count + helper_i64_scratch_count;
    let i32_base = dispatcher_param_count + total_i64_count;

    // Under memory64 the "i32 scratch" pool is declared as i64 (same as main/aux functions).
    let scratch_val_type = if options.memory_config.memory64 {
        ValType::I64
    } else {
        ValType::I32
    };
    let mut locals_vec: Vec<(u32, ValType)> = Vec::new();
    if total_i64_count > 0 {
        locals_vec.push((total_i64_count, ValType::I64));
    }
    if i32_scratch_count > 0 {
        locals_vec.push((i32_scratch_count, scratch_val_type));
    }
    let mut function = Function::new(locals_vec);

    if needs_heap {
        let pw = PtrWidth::from_memory64(options.memory_config.memory64);
        initialize_helper_state_locals(
            &mut function,
            layout,
            i32_base,
            total_static_reserved_bytes,
            false,
            pw,
        )?;
    }
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Result(
        wasm_encoder::ValType::I64,
    )));

    emit_tail_call_group_dispatch_chain(
        &mut function,
        group_plan,
        &member_by_name,
        &named_local_bases,
        decl_func_indices,
        decl_returns_wasm_heap,
        decl_uses_heap,
        decl_table_slots,
        goby_dup_func_idx,
        goby_drop_func_idx,
        indirect_call_type_idx_1,
        indirect_call_type_idx_2,
        indirect_call_type_idx_3,
        record_ctor_tags,
        static_strings,
        static_heap,
        layout,
        options,
        total_named_i64_count,
        helper_i64_scratch_count,
        i32_base,
        0,
    )?;

    function.instruction(&Instruction::End);
    if needs_heap {
        let alloc_cursor_local = i32_base + HELPER_ALLOC_CURSOR_OFFSET;
        let heap_floor_local = i32_base + HELPER_HEAP_FLOOR_OFFSET;
        let pw = PtrWidth::from_memory64(options.memory_config.memory64);
        emit_sync_cursor_to_global(&mut function, alloc_cursor_local, pw);
        emit_sync_floor_to_global(&mut function, heap_floor_local, pw);
    }
    function.instruction(&Instruction::End);
    code.function(&function);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_tail_call_group_dispatch_chain(
    function: &mut Function,
    group_plan: &TailCallGroupPlan,
    member_by_name: &HashMap<&str, &AuxDecl>,
    named_local_bases: &HashMap<String, u32>,
    decl_func_indices: &HashMap<String, u32>,
    decl_returns_wasm_heap: &HashMap<String, bool>,
    decl_uses_heap: &HashMap<String, bool>,
    decl_table_slots: &HashMap<String, u32>,
    goby_dup_func_idx: u32,
    goby_drop_func_idx: u32,
    indirect_call_type_idx_1: Option<u32>,
    indirect_call_type_idx_2: Option<u32>,
    indirect_call_type_idx_3: Option<u32>,
    record_ctor_tags: &HashMap<String, u32>,
    static_strings: &StaticStringPool,
    static_heap: &StaticHeapPool,
    layout: &MemoryLayout,
    options: EmitOptions,
    total_named_i64_count: u32,
    helper_i64_scratch_count: u32,
    i32_base: u32,
    member_idx: usize,
) -> Result<(), CodegenError> {
    let member_name = &group_plan.member_names[member_idx];
    let decl = member_by_name
        .get(member_name.as_str())
        .copied()
        .ok_or_else(|| CodegenError {
            message: format!(
                "gen_lower/emit: missing aux decl while emitting tail-call dispatcher member '{member_name}'"
            ),
        })?;
    let tag = *group_plan
        .member_tags
        .get(member_name)
        .expect("tail-call group member tag");

    function.instruction(&Instruction::LocalGet(0));
    function.instruction(&Instruction::I64Const(tag as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        wasm_encoder::ValType::I64,
    )));

    let mut ctx = EmitContext::with_module_tables(
        decl_func_indices.clone(),
        decl_returns_wasm_heap.clone(),
        decl_uses_heap.clone(),
        decl_table_slots.clone(),
        goby_dup_func_idx,
        goby_drop_func_idx,
        indirect_call_type_idx_1,
        indirect_call_type_idx_2,
        indirect_call_type_idx_3,
        record_ctor_tags.clone(),
    );
    for (param_idx, param_name) in decl.param_names.iter().enumerate() {
        ctx.locals.insert(param_name.clone(), 1 + param_idx as u32);
    }
    ctx.next_local = *named_local_bases
        .get(member_name)
        .expect("dispatcher member named-local base");
    ctx.tail_call_mode = Some(TailCallMode::Group(TailCallGroupState {
        tag_local: 0,
        shared_arg_locals: (0..group_plan.max_arity)
            .map(|idx| 1 + idx as u32)
            .collect(),
        arg_scratch_base: 1 + group_plan.max_arity as u32 + total_named_i64_count,
        member_tags: group_plan.member_tags.clone(),
        member_arities: group_plan.member_arities.clone(),
    }));
    emit_instrs(
        function,
        &mut ctx,
        &decl.instrs,
        layout,
        total_named_i64_count,
        helper_i64_scratch_count,
        i32_base,
        static_strings,
        static_heap,
        options,
        true,
        member_idx as u32 + 1,
    )?;
    function.instruction(&Instruction::Else);
    if member_idx + 1 == group_plan.member_names.len() {
        function.instruction(&Instruction::Unreachable);
    } else {
        emit_tail_call_group_dispatch_chain(
            function,
            group_plan,
            member_by_name,
            named_local_bases,
            decl_func_indices,
            decl_returns_wasm_heap,
            decl_uses_heap,
            decl_table_slots,
            goby_dup_func_idx,
            goby_drop_func_idx,
            indirect_call_type_idx_1,
            indirect_call_type_idx_2,
            indirect_call_type_idx_3,
            record_ctor_tags,
            static_strings,
            static_heap,
            layout,
            options,
            total_named_i64_count,
            helper_i64_scratch_count,
            i32_base,
            member_idx + 1,
        )?;
    }
    function.instruction(&Instruction::End);
    Ok(())
}

/// Emit a direct Wasm `call`.
///
/// Direct calls always flush the caller's local allocation cursor first so any
/// caller-owned heap values that remain live across the call are visible to the
/// callee. Heap-returning callees additionally refresh the caller cursor from
/// the shared global slot after the call so later allocations do not overwrite
/// the returned value.
fn emit_heap_aware_direct_call(
    function: &mut Function,
    func_idx: u32,
    helper_state: Option<&HeapEmitState>,
    returns_wasm_heap: bool,
    callee_uses_heap: bool,
) {
    if let Some(hs) = helper_state {
        emit_sync_cursor_to_global(function, hs.alloc_cursor_local, hs.pw());
        emit_sync_floor_to_global(function, hs.heap_floor_local, hs.pw());
    }
    function.instruction(&Instruction::Call(func_idx));
    if let Some(hs) = helper_state {
        emit_return_if_runtime_error(function, hs);
    }
    if (returns_wasm_heap || callee_uses_heap)
        && let Some(hs) = helper_state
    {
        emit_sync_cursor_from_global(function, hs.alloc_cursor_local, hs.pw());
        emit_sync_floor_from_global(function, hs.heap_floor_local, hs.pw());
    }
}

fn emit_return_if_runtime_error(function: &mut Function, helper_state: &HeapEmitState) {
    let pw = helper_state.pw();
    // GLOBAL_RUNTIME_ERROR_OFFSET is always an i32 (4-byte) field.
    function.instruction(&ptr_const(pw, GLOBAL_RUNTIME_ERROR_OFFSET as u64));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Else);
    if helper_state.scratch.function_returns_i64 {
        function.instruction(&Instruction::I64Const(0));
    }
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);
}

/// Emit `ptr.const GLOBAL_HEAP_CURSOR_OFFSET; local.get alloc_cursor_local; ptr.store`
/// to flush the local alloc cursor to the global persistent slot before a `call_indirect`.
fn emit_sync_cursor_to_global(function: &mut Function, alloc_cursor_local: u32, pw: PtrWidth) {
    // Global slots are always i32 (4 bytes). Under W64, wrap the i64 local to i32 before storing.
    let mem_arg = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    function.instruction(&ptr_const(pw, GLOBAL_HEAP_CURSOR_OFFSET as u64));
    function.instruction(&Instruction::LocalGet(alloc_cursor_local));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I32WrapI64);
    }
    function.instruction(&Instruction::I32Store(mem_arg));
}

fn emit_sync_floor_to_global(function: &mut Function, heap_floor_local: u32, pw: PtrWidth) {
    let mem_arg = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    function.instruction(&ptr_const(pw, GLOBAL_HEAP_FLOOR_OFFSET as u64));
    function.instruction(&Instruction::LocalGet(heap_floor_local));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I32WrapI64);
    }
    function.instruction(&Instruction::I32Store(mem_arg));
}

/// Emit `ptr.const GLOBAL_HEAP_CURSOR_OFFSET; i32.load; [extend]; local.set alloc_cursor_local`
/// to reload the alloc cursor from the global slot after a `call_indirect`.
fn emit_sync_cursor_from_global(function: &mut Function, alloc_cursor_local: u32, pw: PtrWidth) {
    let mem_arg = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    function.instruction(&ptr_const(pw, GLOBAL_HEAP_CURSOR_OFFSET as u64));
    function.instruction(&Instruction::I32Load(mem_arg));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I64ExtendI32U);
    }
    function.instruction(&Instruction::LocalSet(alloc_cursor_local));
}

fn emit_sync_floor_from_global(function: &mut Function, heap_floor_local: u32, pw: PtrWidth) {
    let mem_arg = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    function.instruction(&ptr_const(pw, GLOBAL_HEAP_FLOOR_OFFSET as u64));
    function.instruction(&Instruction::I32Load(mem_arg));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I64ExtendI32U);
    }
    function.instruction(&Instruction::LocalSet(heap_floor_local));
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
    pw: PtrWidth,
    hs: Option<&HeapEmitState>,
) {
    // TAG_CLOSURE check: callee >> 60 == TAG_CLOSURE
    function.instruction(&Instruction::LocalGet(callee_local));
    function.instruction(&Instruction::I64Const(60));
    function.instruction(&Instruction::I64ShrU);
    function.instruction(&Instruction::I64Const(i64::from(TAG_CLOSURE)));
    function.instruction(&Instruction::I64Eq);
    if let Some(hs) = hs {
        emit_sync_cursor_to_global(function, hs.alloc_cursor_local, hs.pw());
        emit_sync_floor_to_global(function, hs.heap_floor_local, hs.pw());
    }
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        wasm_encoder::ValType::I64,
    )));
    // Closure branch: push callee (env ptr), then args, then func_ptr from closure[0].
    function.instruction(&Instruction::LocalGet(callee_local));
    for &arg in arg_locals {
        function.instruction(&Instruction::LocalGet(arg));
    }
    // Load func_handle from closure[0]: decode ptr then I64Load.
    function.instruction(&Instruction::LocalGet(callee_local));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    // CallIndirect table index is always i32.
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
    // CallIndirect table index is always i32.
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
        emit_sync_cursor_from_global(function, hs.alloc_cursor_local, hs.pw());
        emit_sync_floor_from_global(function, hs.heap_floor_local, hs.pw());
    }
}

fn emit_indirect_call_dispatch(
    function: &mut Function,
    ctx: &EmitContext,
    helper_i64_base: u32,
    arity: u8,
    hs: Option<&HeapEmitState>,
    pw: PtrWidth,
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
        pw,
        hs,
    );
    if let Some(hs) = hs {
        function.instruction(&Instruction::LocalSet(callee_local));
        emit_return_if_runtime_error(function, hs);
        function.instruction(&Instruction::LocalGet(callee_local));
    }
    Ok(())
}

fn emit_push_tagged_ptr(function: &mut Function, ptr_local: u32, tag: u8, pw: PtrWidth) {
    function.instruction(&Instruction::LocalGet(ptr_local));
    // W32: ptr_local is i32 — extend to i64 before tagging.
    // W64: ptr_local is already i64 — no extension needed.
    if pw == PtrWidth::W32 {
        function.instruction(&Instruction::I64ExtendI32U);
    }
    function.instruction(&Instruction::I64Const((tag as i64) << 60));
    function.instruction(&Instruction::I64Or);
}

fn emit_store_i64_payload_to_ptr_local(
    function: &mut Function,
    payload_local: u32,
    ptr_local: u32,
    pw: PtrWidth,
) {
    function.instruction(&Instruction::LocalGet(payload_local));
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(ptr_local));
}

fn emit_ref_count_drop_reuse(
    function: &mut Function,
    token_local: u32,
    scratch_i64_base: u32,
    ctx: &EmitContext,
    pw: PtrWidth,
) {
    let tagged_local = scratch_i64_base;
    let tag_local = scratch_i64_base + 1;
    let refcount_local = scratch_i64_base + 2;

    const PAYLOAD_MASK: i64 = 0x0FFF_FFFF_FFFF_FFFFu64 as i64;

    function.instruction(&Instruction::LocalSet(tagged_local));
    function.instruction(&Instruction::I64Const(0));
    function.instruction(&Instruction::LocalSet(token_local));

    function.instruction(&Instruction::LocalGet(tagged_local));
    function.instruction(&Instruction::I64Eqz);
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

    function.instruction(&Instruction::LocalGet(tagged_local));
    function.instruction(&Instruction::I64Const(60));
    function.instruction(&Instruction::I64ShrU);
    function.instruction(&Instruction::I64Const(0xF));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::LocalSet(tag_local));

    function.instruction(&Instruction::LocalGet(tag_local));
    function.instruction(&Instruction::I64Const(TAG_STRING as i64));
    function.instruction(&Instruction::I64Eq);
    for tag in [
        TAG_LIST,
        TAG_TUPLE,
        TAG_RECORD,
        TAG_CLOSURE,
        TAG_CELL,
        TAG_CHUNK,
    ] {
        function.instruction(&Instruction::LocalGet(tag_local));
        function.instruction(&Instruction::I64Const(tag as i64));
        function.instruction(&Instruction::I64Eq);
        function.instruction(&Instruction::I32Or);
    }
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

    function.instruction(&Instruction::LocalGet(tagged_local));
    function.instruction(&Instruction::I64Const(PAYLOAD_MASK));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::LocalTee(token_local));
    function.instruction(&Instruction::I64Const(-(REFCOUNT_WORD_BYTES as i64)));
    function.instruction(&Instruction::I64Add);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(refcount_local));

    function.instruction(&Instruction::LocalGet(refcount_local));
    function.instruction(&Instruction::I64Const(-1i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::I64Const(0));
    function.instruction(&Instruction::LocalSet(token_local));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(refcount_local));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(tagged_local));
    function.instruction(&Instruction::Call(ctx.goby_drop_func_idx));
    function.instruction(&Instruction::I64Const(0));
    function.instruction(&Instruction::LocalSet(token_local));
    function.instruction(&Instruction::Else);
    // refcount == 1: token is retained (reuse hit). Increment reuse_hits counter.
    function.instruction(&Instruction::I64Const(GLOBAL_REUSE_HITS_OFFSET as i64));
    function.instruction(&Instruction::I64Const(GLOBAL_REUSE_HITS_OFFSET as i64));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Add);
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
}

fn emit_alloc_reuse_payload(
    function: &mut Function,
    token_local: u32,
    result_ptr_local: u32,
    sc: SizeClass,
    hs: &HeapEmitState,
) -> Result<(), CodegenError> {
    let pw = hs.pw();
    let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE;
    let hit_flag = hs.scratch.i32_base + HS_ITER;
    let payload_bytes = sc.payload_bytes(pw).ok_or_else(|| CodegenError {
        message: format!("gen_lower/emit: AllocReuse requires fixed payload size, got {sc:?}"),
    })?;

    function.instruction(&Instruction::LocalGet(token_local));
    function.instruction(&Instruction::I64Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&ptr_const(pw, payload_bytes as u64));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_with_flag(sc, function, hs, s_alloc_size, result_ptr_local, hit_flag);
    function.instruction(&Instruction::Else);
    emit_store_i64_payload_to_ptr_local(function, token_local, result_ptr_local, pw);
    function.instruction(&Instruction::LocalGet(result_ptr_local));
    function.instruction(&ptr_const(pw, REFCOUNT_WORD_BYTES as u64));
    function.instruction(&ptr_sub(pw));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::End);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_alloc_reuse(
    function: &mut Function,
    ctx: &mut EmitContext,
    token_local: u32,
    sc: SizeClass,
    init: &BackendAllocInit,
    hs: &HeapEmitState,
    layout: &MemoryLayout,
    named_i64_count: u32,
    helper_i64_scratch_count: u32,
    i32_base: u32,
    static_strings: &StaticStringPool,
    static_heap: &StaticHeapPool,
    options: EmitOptions,
    function_returns_i64: bool,
    heap_base_depth: u32,
    self_tail_loop_depth: u32,
) -> Result<(), CodegenError> {
    let pw = hs.pw();
    let object_ptr = hs.scratch.i32_base + HELPER_SCRATCH_I32 + heap_base_depth;

    match init {
        BackendAllocInit::TupleLit { element_instrs } => {
            emit_alloc_reuse_payload(function, token_local, object_ptr, sc, hs)?;
            let n = element_instrs.len() as u64;
            function.instruction(&Instruction::LocalGet(object_ptr));
            function.instruction(&ptr_const(pw, n));
            function.instruction(&ptr_store(
                pw,
                MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                },
            ));
            for (i, elem_instrs) in element_instrs.iter().enumerate() {
                function.instruction(&Instruction::LocalGet(object_ptr));
                function.instruction(&ptr_const(pw, meta_slot_bytes(pw) as u64 + 8 * i as u64));
                function.instruction(&ptr_add(pw));
                emit_instrs_with_heap_depth(
                    function,
                    ctx,
                    elem_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    static_heap,
                    options,
                    function_returns_i64,
                    heap_base_depth + 1,
                    self_tail_loop_depth,
                )?;
                function.instruction(&Instruction::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }
            emit_push_tagged_ptr(function, object_ptr, TAG_TUPLE, pw);
        }
        BackendAllocInit::RecordLit {
            constructor,
            field_instrs,
        } => {
            emit_alloc_reuse_payload(function, token_local, object_ptr, sc, hs)?;
            let ctor_tag = ctx.record_ctor_tag(constructor)? as i64;
            function.instruction(&Instruction::LocalGet(object_ptr));
            function.instruction(&Instruction::I64Const(ctor_tag));
            function.instruction(&Instruction::I64Store(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
            for (i, field_instrs) in field_instrs.iter().enumerate() {
                function.instruction(&Instruction::LocalGet(object_ptr));
                function.instruction(&ptr_const(pw, 8 + 8 * i as u64));
                function.instruction(&ptr_add(pw));
                emit_instrs_with_heap_depth(
                    function,
                    ctx,
                    field_instrs,
                    layout,
                    named_i64_count,
                    helper_i64_scratch_count,
                    i32_base,
                    static_strings,
                    static_heap,
                    options,
                    function_returns_i64,
                    heap_base_depth + 1,
                    self_tail_loop_depth,
                )?;
                function.instruction(&Instruction::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }
            emit_push_tagged_ptr(function, object_ptr, TAG_RECORD, pw);
        }
        BackendAllocInit::ListLit { element_instrs } => {
            let n = element_instrs.len() as u32;
            let n_chunks = n.div_ceil(CHUNK_SIZE);
            if n_chunks > 1 {
                return Err(CodegenError {
                    message: format!(
                        "gen_lower/emit: AllocReuse list initializer supports at most one chunk, got {n_chunks}"
                    ),
                });
            }
            emit_alloc_reuse_payload(function, token_local, object_ptr, sc, hs)?;
            let s_chunk_ptr = hs.scratch.i32_base + HS_AUX_PTR;
            let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE;

            function.instruction(&Instruction::LocalGet(object_ptr));
            function.instruction(&ptr_const(pw, n as u64));
            function.instruction(&ptr_store(
                pw,
                MemArg {
                    offset: header_total_len_offset() as u64,
                    align: 2,
                    memory_index: 0,
                },
            ));
            function.instruction(&Instruction::LocalGet(object_ptr));
            function.instruction(&ptr_const(pw, n_chunks as u64));
            function.instruction(&ptr_store(
                pw,
                MemArg {
                    offset: header_n_chunks_offset_pw(pw) as u64,
                    align: 2,
                    memory_index: 0,
                },
            ));

            if n_chunks == 1 {
                function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
                function.instruction(&Instruction::LocalSet(s_alloc_size));
                emit_alloc_from_top(function, hs, s_alloc_size, s_chunk_ptr);
                function.instruction(&Instruction::LocalGet(object_ptr));
                function.instruction(&Instruction::LocalGet(s_chunk_ptr));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                function.instruction(&Instruction::LocalGet(s_chunk_ptr));
                function.instruction(&ptr_const(pw, n as u64));
                function.instruction(&ptr_store(
                    pw,
                    MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    },
                ));
                for (i, elem_instrs) in element_instrs.iter().enumerate() {
                    function.instruction(&Instruction::LocalGet(s_chunk_ptr));
                    function.instruction(&ptr_const(pw, chunk_item_offset_pw(pw, i as u32) as u64));
                    function.instruction(&ptr_add(pw));
                    emit_instrs_with_heap_depth(
                        function,
                        ctx,
                        elem_instrs,
                        layout,
                        named_i64_count,
                        helper_i64_scratch_count,
                        i32_base,
                        static_strings,
                        static_heap,
                        options,
                        function_returns_i64,
                        heap_base_depth + 1,
                        self_tail_loop_depth,
                    )?;
                    function.instruction(&Instruction::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
            }

            emit_push_tagged_ptr(function, object_ptr, TAG_LIST, pw);
        }
        BackendAllocInit::Interp(_) => {
            return Err(CodegenError {
                message:
                    "gen_lower/emit: AllocReuse interpolation initializer is not supported yet"
                        .to_string(),
            });
        }
        BackendAllocInit::Retain => {
            // Payload already contains valid data (e.g. list header updated in-place by
            // ListSetInPlace before this alloc_reuse call). Only reset refcount to 1.
            emit_alloc_reuse_payload(function, token_local, object_ptr, sc, hs)?;
            emit_push_tagged_ptr(function, object_ptr, TAG_LIST, pw);
        }
    }

    Ok(())
}

// Every heap object is preceded by an 8-byte refcount word.
// Goby is memory64-only, so the header is always 8 bytes regardless of PtrWidth.
const REFCOUNT_WORD_BYTES: u64 = 8;

fn emit_alloc_from_top(
    function: &mut Function,
    helper_state: &HeapEmitState,
    size_local: u32,
    result_local: u32,
) {
    let pw = helper_state.pw();
    // Prepend the 8-byte refcount header to the requested payload size before
    // alignment, so the cursor consumes the header too.
    function.instruction(&Instruction::LocalGet(size_local));
    function.instruction(&ptr_const(pw, REFCOUNT_WORD_BYTES));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(size_local));
    // Always allocate an 8-byte-aligned size so capacity checks and cursor movement
    // consume the same amount of memory.
    function.instruction(&Instruction::LocalGet(size_local));
    function.instruction(&ptr_const(pw, 7));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, !7u64));
    function.instruction(match pw {
        PtrWidth::W32 => &Instruction::I32And,
        PtrWidth::W64 => &Instruction::I64And,
    });
    function.instruction(&Instruction::LocalSet(size_local));

    // available = max(alloc_cursor - heap_floor, 0)
    //
    // Using a raw sub here can underflow when cursor < floor and produce a
    // huge unsigned value, which incorrectly skips growth and can drive the
    // cursor to invalid addresses. Guard first, then subtract only when safe.
    function.instruction(&Instruction::LocalGet(helper_state.alloc_cursor_local));
    function.instruction(&Instruction::LocalGet(helper_state.heap_floor_local));
    function.instruction(match pw {
        PtrWidth::W32 => &Instruction::I32GeU,
        PtrWidth::W64 => &Instruction::I64GeU,
    });
    let ptr_val_type = match pw {
        PtrWidth::W32 => ValType::I32,
        PtrWidth::W64 => ValType::I64,
    };
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ptr_val_type,
    )));
    function.instruction(&Instruction::LocalGet(helper_state.alloc_cursor_local));
    function.instruction(&Instruction::LocalGet(helper_state.heap_floor_local));
    function.instruction(&ptr_sub(pw));
    function.instruction(&Instruction::Else);
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalTee(result_local));
    function.instruction(&Instruction::LocalGet(size_local));
    function.instruction(&ptr_lt_u(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // `result_local` currently holds available bytes between the top-down cursor
    // and the heap floor. Grow by the extra bytes required for this allocation.
    function.instruction(&Instruction::LocalGet(size_local));
    function.instruction(&Instruction::LocalGet(result_local));
    function.instruction(&ptr_sub(pw));
    function.instruction(&ptr_const(pw, (WASM_PAGE_BYTES - 1) as u64));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, WASM_PAGE_BYTES as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&Instruction::MemoryGrow(0));
    function.instruction(&Instruction::LocalTee(result_local)); // old page count
    function.instruction(&ptr_neg_one(pw));
    function.instruction(&ptr_eq(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_memory_exhaustion_abort(function, helper_state);
    function.instruction(&Instruction::End);

    // Move the floor up to the previous top so older allocations are never
    // re-entered after a grow.
    function.instruction(&Instruction::LocalGet(result_local));
    function.instruction(&ptr_const(pw, WASM_PAGE_BYTES as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_const(pw, HOST_BUMP_RESERVED_BYTES as u64));
    function.instruction(&ptr_sub(pw));
    function.instruction(&Instruction::LocalSet(helper_state.heap_floor_local));
    // If host bump already advanced beyond this floor, keep floor at host cursor.
    // GLOBAL_HOST_BUMP_CURSOR_OFFSET is always an i32 (4-byte) field.
    function.instruction(&ptr_const(pw, GLOBAL_HOST_BUMP_CURSOR_OFFSET as u64));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I64ExtendI32U);
    }
    function.instruction(&Instruction::LocalSet(result_local));
    function.instruction(&Instruction::LocalGet(helper_state.heap_floor_local));
    function.instruction(&Instruction::LocalGet(result_local));
    function.instruction(&ptr_lt_u(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(result_local));
    function.instruction(&Instruction::LocalSet(helper_state.heap_floor_local));
    function.instruction(&Instruction::End);

    // Rebase cursor to the current dynamic top-of-heap slice after growth:
    //   alloc_cursor = memory.size * WASM_PAGE_BYTES - HOST_BUMP_RESERVED_BYTES
    //
    // `cursor += grown_bytes` (or rebasing without lifting the floor) re-enters
    // already-allocated regions. Pairing this with the floor update above keeps
    // each grown segment disjoint.
    function.instruction(&Instruction::MemorySize(0));
    function.instruction(&ptr_const(pw, WASM_PAGE_BYTES as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_const(pw, HOST_BUMP_RESERVED_BYTES as u64));
    function.instruction(&ptr_sub(pw));
    function.instruction(&Instruction::LocalSet(helper_state.alloc_cursor_local));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(helper_state.alloc_cursor_local));
    function.instruction(&Instruction::LocalGet(size_local));
    function.instruction(&ptr_sub(pw));
    function.instruction(&Instruction::LocalTee(result_local));
    function.instruction(&Instruction::LocalSet(helper_state.alloc_cursor_local));
    // result_local now points at the refcount header. Write refcount = 1.
    function.instruction(&Instruction::LocalGet(result_local));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    // Advance result_local past the header so callers receive the payload pointer.
    function.instruction(&Instruction::LocalGet(result_local));
    function.instruction(&ptr_const(pw, REFCOUNT_WORD_BYTES));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(result_local));
    // Increment the running allocation byte counter.
    // size_local holds the aligned total (payload + REFCOUNT_WORD_BYTES) at this point.
    // i64.store consumes [addr, value] — push store_addr first, then compute the new value.
    function.instruction(&ptr_const(pw, GLOBAL_ALLOC_BYTES_TOTAL_OFFSET as u64)); // store addr
    function.instruction(&ptr_const(pw, GLOBAL_ALLOC_BYTES_TOTAL_OFFSET as u64)); // load addr
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalGet(size_local));
    if pw == PtrWidth::W32 {
        function.instruction(&Instruction::I64ExtendI32U);
    }
    function.instruction(&Instruction::I64Add); // stack: [store_addr, new_total]
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
}

// ---------------------------------------------------------------------------
// Free-list helpers (Perceus M3)
// ---------------------------------------------------------------------------

/// Pop one object from the free list for `sc` and store the payload pointer in
/// `result_local`. Falls back to `emit_alloc_from_top` on a miss.
///
/// On a free-list hit the refcount word is re-initialised to 1 (the header
/// slot was previously overwritten with the intrusive next-pointer).
///
/// `size_local` must already hold the aligned payload size (EXCLUDING the
/// 8-byte refcount header) when this function is called.  The function
/// overwrites `size_local` as a scratch register (same contract as
/// `emit_alloc_from_top`).
///
/// Increments `GLOBAL_FREE_LIST_HITS_OFFSET` on every cache hit and updates
/// `GLOBAL_PEAK_BYTES_OFFSET` on every allocation path.
fn emit_alloc(
    sc: SizeClass,
    function: &mut Function,
    helper_state: &HeapEmitState,
    size_local: u32,
    result_local: u32,
) {
    let _pw = helper_state.pw();

    match sc.free_list_head_offset() {
        None => {
            // Large allocation — no free list, bump directly.
            emit_alloc_from_top(function, helper_state, size_local, result_local);
        }
        Some(head_offset) => {
            // Load free-list head.
            function.instruction(&Instruction::I64Const(head_offset as i64));
            function.instruction(&Instruction::I64Load(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
            function.instruction(&Instruction::LocalSet(result_local));

            // if result_local == 0 { bump } else { pop }
            function.instruction(&Instruction::LocalGet(result_local));
            function.instruction(&Instruction::I64Eqz);
            function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            // --- miss: bump allocate ---
            emit_alloc_from_top(function, helper_state, size_local, result_local);
            function.instruction(&Instruction::Else);
            // --- hit: pop from free list ---
            // Read next-pointer from payload[0] (== refcount slot, overwritten during push).
            function.instruction(&Instruction::LocalGet(result_local));
            function.instruction(&Instruction::I64Const(-(REFCOUNT_WORD_BYTES as i64)));
            function.instruction(&Instruction::I64Add);
            function.instruction(&Instruction::I64Load(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
            // Store it back as new head.
            function.instruction(&Instruction::I64Const(head_offset as i64));
            function.instruction(&Instruction::I64Load(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            })); // read next ptr
            // Re-do: store next into head slot.  (stack is dirty — redo cleanly)
            // We need: head_slot = payload[0], then refcount = 1.
            // payload[0] was loaded above and discarded; redo properly.
            function.instruction(&Instruction::End);
            // result_local now has the old head (hit path) or the new ptr (miss path).

            // --- Re-init refcount to 1 on the hit path ---
            // We need to distinguish hit/miss here.  Use a second If block with a flag.
            // Simpler: unconditionally rewrite refcount after the block since
            // emit_alloc_from_top already wrote it to 1.  But for the hit path the
            // header was trashed (intrusive next ptr).  We need the hit path to:
            //   (1) store new_head = mem[result_local - 8],
            //   (2) mem[result_local - 8] = 1 (restore refcount).
            // The restructured code below handles both paths with a boolean flag local.
            // -----------------------------------------------------------------------
            // NOTE: The block above is intentionally incomplete — we need a cleaner
            // structure.  Replace the whole block with the pattern below.
        }
    }
}

/// Allocate with free-list pop (clean implementation without the draft above).
///
/// This replaces the incomplete `emit_alloc` skeleton.  It uses a flag local
/// to distinguish hit vs miss so we can do the right thing after the branch.
///
/// Callers: use this function.  `emit_alloc` above will be removed once all
/// call-sites are migrated.  For now it is dead code; the real entry point is
/// `emit_alloc_with_flag`.
fn emit_alloc_with_flag(
    sc: SizeClass,
    function: &mut Function,
    helper_state: &HeapEmitState,
    size_local: u32,
    result_local: u32,
    hit_flag_local: u32, // i32 scratch: 1 = free-list hit, 0 = miss
) {
    let _pw = helper_state.pw();

    match sc.free_list_head_offset() {
        None => {
            // Large: always bump.
            function.instruction(&ptr_const(helper_state.pw(), 0));
            function.instruction(&Instruction::LocalSet(hit_flag_local));
            emit_alloc_from_top(function, helper_state, size_local, result_local);
        }
        Some(head_offset) => {
            // Load head pointer.
            function.instruction(&Instruction::I64Const(head_offset as i64));
            function.instruction(&Instruction::I64Load(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
            function.instruction(&Instruction::LocalTee(result_local));
            function.instruction(&Instruction::I64Eqz);
            function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            // miss
            function.instruction(&ptr_const(helper_state.pw(), 0));
            function.instruction(&Instruction::LocalSet(hit_flag_local));
            emit_alloc_from_top(function, helper_state, size_local, result_local);
            function.instruction(&Instruction::Else);
            // hit — result_local already has the payload ptr (old head).
            function.instruction(&ptr_const(helper_state.pw(), 1));
            function.instruction(&Instruction::LocalSet(hit_flag_local));
            // Read next-pointer from the header word (was overwritten by push)
            // into `size_local`, which is scratch on this path.
            function.instruction(&Instruction::LocalGet(result_local));
            function.instruction(&ptr_const(helper_state.pw(), REFCOUNT_WORD_BYTES));
            function.instruction(&ptr_sub(helper_state.pw()));
            function.instruction(&Instruction::I64Load(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
            if helper_state.pw() == PtrWidth::W32 {
                function.instruction(&Instruction::I32WrapI64);
            }
            function.instruction(&Instruction::LocalSet(size_local));

            // Write new head.
            function.instruction(&Instruction::I64Const(head_offset as i64));
            function.instruction(&Instruction::LocalGet(size_local));
            if helper_state.pw() == PtrWidth::W32 {
                function.instruction(&Instruction::I64ExtendI32U);
            }
            function.instruction(&Instruction::I64Store(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
            // Restore refcount = 1 in the header word.
            function.instruction(&Instruction::LocalGet(result_local));
            function.instruction(&ptr_const(helper_state.pw(), REFCOUNT_WORD_BYTES));
            function.instruction(&ptr_sub(helper_state.pw()));
            function.instruction(&Instruction::I64Const(1));
            function.instruction(&Instruction::I64Store(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
            function.instruction(&Instruction::End);

            // Update GLOBAL_FREE_LIST_HITS if hit.
            function.instruction(&Instruction::LocalGet(hit_flag_local));
            if helper_state.pw() == PtrWidth::W64 {
                function.instruction(&Instruction::I64Eqz);
                function.instruction(&Instruction::I32Eqz);
            }
            function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            function.instruction(&Instruction::I64Const(GLOBAL_FREE_LIST_HITS_OFFSET as i64));
            function.instruction(&Instruction::I64Const(GLOBAL_FREE_LIST_HITS_OFFSET as i64));
            function.instruction(&Instruction::I64Load(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
            function.instruction(&Instruction::I64Const(1));
            function.instruction(&Instruction::I64Add);
            function.instruction(&Instruction::I64Store(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
            function.instruction(&Instruction::End);
        }
    }

    // Update peak_bytes: peak = max(total_bytes - freed_bytes, peak).
    function.instruction(&Instruction::I64Const(GLOBAL_PEAK_BYTES_OFFSET as i64));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I64Const(
        GLOBAL_ALLOC_BYTES_TOTAL_OFFSET as i64,
    ));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I64Const(GLOBAL_FREED_BYTES_OFFSET as i64));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I64Sub);
    // stack: [peak, live]; update when live > peak.
    function.instruction(&Instruction::I64LtU);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::I64Const(GLOBAL_PEAK_BYTES_OFFSET as i64));
    function.instruction(&Instruction::I64Const(
        GLOBAL_ALLOC_BYTES_TOTAL_OFFSET as i64,
    ));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I64Const(GLOBAL_FREED_BYTES_OFFSET as i64));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::I64Sub);
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::End);
}

/// Push `payload_ptr` onto the free list for `sc`.
///
/// The object's header word (at `payload_ptr - 8`) is overwritten with the old
/// free-list head to form the intrusive linked list.
/// `size_bytes` is the aligned total size (payload + 8-byte header) that is
/// added to `GLOBAL_FREED_BYTES_OFFSET`.
fn emit_free_list_push(
    sc: SizeClass,
    function: &mut Function,
    pw: PtrWidth,
    payload_ptr_local: u32,
    size_bytes: u64,
) {
    if let Some(head_offset) = sc.free_list_head_offset() {
        // header_word = old head
        function.instruction(&Instruction::LocalGet(payload_ptr_local));
        function.instruction(&Instruction::I64Const(-(REFCOUNT_WORD_BYTES as i64)));
        function.instruction(&Instruction::I64Add);
        // stack: [header_addr]
        // load old head
        function.instruction(&Instruction::I64Const(head_offset as i64));
        function.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        // store old_head into header_word (intrusive next-ptr)
        function.instruction(&Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        // head = payload_ptr
        function.instruction(&Instruction::I64Const(head_offset as i64));
        function.instruction(&Instruction::LocalGet(payload_ptr_local));
        function.instruction(&Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        // freed_bytes += size_bytes
        function.instruction(&Instruction::I64Const(GLOBAL_FREED_BYTES_OFFSET as i64));
        function.instruction(&Instruction::I64Const(GLOBAL_FREED_BYTES_OFFSET as i64));
        function.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        function.instruction(&Instruction::I64Const(size_bytes as i64));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
    }
    // Large: not pushed to any list; memory is simply abandoned for now.
    let _ = pw;
}

/// Emit the `__goby_dup` Wasm helper into `code`.
///
/// Signature: `(tagged_value: i64) -> ()`
///
/// Non-heap values are ignored. Heap values branch on the static-literal sentinel and
/// otherwise increment the refcount at `payload_ptr - REFCOUNT_WORD_BYTES`.
fn emit_goby_dup_function(code: &mut CodeSection, pw: PtrWidth) {
    let param_tagged: u32 = 0;
    let l_tag: u32 = 1;
    let l_payload: u32 = 2;
    let l_refcount: u32 = 3;

    let mut function = Function::new(vec![(3, ValType::I64)]);
    const PAYLOAD_MASK: i64 = 0x0FFF_FFFF_FFFF_FFFFu64 as i64;

    let push_header_addr = |f: &mut Function| {
        f.instruction(&Instruction::LocalGet(l_payload));
        f.instruction(&Instruction::I64Const(-(REFCOUNT_WORD_BYTES as i64)));
        f.instruction(&Instruction::I64Add);
        if pw == PtrWidth::W32 {
            f.instruction(&Instruction::I32WrapI64);
        }
    };

    function.instruction(&Instruction::LocalGet(param_tagged));
    function.instruction(&Instruction::I64Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(param_tagged));
    function.instruction(&Instruction::I64Const(60));
    function.instruction(&Instruction::I64ShrU);
    function.instruction(&Instruction::I64Const(0xF));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::LocalSet(l_tag));

    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_STRING as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_LIST as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::I32Or);
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_TUPLE as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::I32Or);
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_RECORD as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::I32Or);
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_CLOSURE as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::I32Or);
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_CELL as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::I32Or);
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_CHUNK as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::I32Or);
    function.instruction(&Instruction::I32Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(param_tagged));
    function.instruction(&Instruction::I64Const(PAYLOAD_MASK));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::LocalSet(l_payload));

    push_header_addr(&mut function);
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(l_refcount));

    function.instruction(&Instruction::LocalGet(l_refcount));
    function.instruction(&Instruction::I64Const(-1i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    push_header_addr(&mut function);
    function.instruction(&Instruction::LocalGet(l_refcount));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Add);
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));

    function.instruction(&Instruction::End);
    code.function(&function);
}

/// Emit the `__goby_drop` Wasm helper function into `code`.
///
/// Signature: `(tagged_ptr: i64) -> ()`
///
/// Algorithm:
///  1. null check (tagged_ptr == 0 → return).
///  2. payload_ptr = tagged_ptr & PAYLOAD_MASK.
///  3. header_addr = payload_ptr - REFCOUNT_WORD_BYTES.
///  4. refcount = mem[header_addr]. Sentinel (u64::MAX) → return.
///  5. Decrement refcount; if still > 0 → return.
///  6. tag = (tagged_ptr >> 60) & 0xF; dispatch child-drop + free-list push.
///
/// Child-drop and free-list push per tag:
///  - TAG_CHUNK (0xA): internal chunk ptr — drop all items, push to Chunk free-list.
///  - TAG_LIST  (0x4): drop each chunk via recursive TAG_CHUNK call, no free-list (header Large).
///  - TAG_TUPLE (0x6): drop each element, no free-list (arity varies).
///  - TAG_CELL  (0x9): no children, push to Cell free-list.
///  - TAG_RECORD (0x7), TAG_CLOSURE (0x8): child-drop deferred (arity not in payload), no free-list.
///  - others: no child-drop, no free-list.
///
/// W32-only fallback: refcount decrement + Cell free-list push only; no recursive child-drop.
/// Child-drop for List/Tuple is not extended for W32.
fn emit_goby_drop_function_w32(code: &mut CodeSection) {
    let pw = PtrWidth::W32;
    let tagged_ptr: u32 = 0;
    let refcount: u32 = 1;
    let tag: u32 = 2;
    let header_addr: u32 = 3;

    let mut function = Function::new(vec![(3, ValType::I64)]);

    const PAYLOAD_MASK: i64 = 0x0FFF_FFFF_FFFF_FFFFu64 as i64;

    let push_header_addr = |f: &mut Function| {
        f.instruction(&Instruction::LocalGet(header_addr));
        f.instruction(&Instruction::I32WrapI64);
    };

    // null check
    function.instruction(&Instruction::LocalGet(tagged_ptr));
    function.instruction(&Instruction::I64Eqz);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    // header_addr = (tagged_ptr & MASK) - 8
    function.instruction(&Instruction::LocalGet(tagged_ptr));
    function.instruction(&Instruction::I64Const(PAYLOAD_MASK));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const(-(REFCOUNT_WORD_BYTES as i64)));
    function.instruction(&Instruction::I64Add);
    function.instruction(&Instruction::LocalSet(header_addr));

    // refcount = mem[header_addr]
    push_header_addr(&mut function);
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(refcount));

    // sentinel check
    function.instruction(&Instruction::LocalGet(refcount));
    function.instruction(&Instruction::I64Const(-1i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    // decrement
    push_header_addr(&mut function);
    function.instruction(&Instruction::LocalGet(refcount));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Sub);
    function.instruction(&Instruction::LocalTee(refcount));
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));

    // early return if still live
    function.instruction(&Instruction::LocalGet(refcount));
    function.instruction(&Instruction::I64Const(0));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    // tag
    function.instruction(&Instruction::LocalGet(tagged_ptr));
    function.instruction(&Instruction::I64Const(60));
    function.instruction(&Instruction::I64ShrU);
    function.instruction(&Instruction::I64Const(0xF));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::LocalSet(tag));

    // freed_bytes += REFCOUNT_WORD_BYTES
    {
        let addr_instr = ptr_const(pw, GLOBAL_FREED_BYTES_OFFSET as u64);
        function.instruction(&addr_instr.clone());
        function.instruction(&addr_instr);
        function.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        function.instruction(&Instruction::I64Const(REFCOUNT_WORD_BYTES as i64));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
    }

    // TAG_CELL: push to Cell free-list
    function.instruction(&Instruction::LocalGet(tag));
    function.instruction(&Instruction::I64Const(TAG_CELL as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    {
        use crate::layout::FREE_LIST_SLOT_CELL;
        let head_addr = ptr_const(pw, FREE_LIST_SLOT_CELL as u64);
        // store old_head at header_addr (intrusive next)
        push_header_addr(&mut function);
        function.instruction(&head_addr.clone());
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
        // payload_ptr = header_addr + 8
        function.instruction(&Instruction::LocalGet(header_addr));
        function.instruction(&Instruction::I64Const(REFCOUNT_WORD_BYTES as i64));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::LocalSet(header_addr));
        // head = payload_ptr
        function.instruction(&head_addr.clone());
        function.instruction(&Instruction::LocalGet(header_addr));
        function.instruction(&Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        // free_list_hits++
        let hits_addr = ptr_const(pw, GLOBAL_FREE_LIST_HITS_OFFSET as u64);
        function.instruction(&hits_addr.clone());
        function.instruction(&hits_addr);
        function.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        function.instruction(&Instruction::I64Const(1));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
    }
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::End);
    code.function(&function);
}

/// `self_func_idx` is the Wasm function index of `__goby_drop` itself for recursive calls.
fn emit_goby_drop_function(
    code: &mut CodeSection,
    _layout: &MemoryLayout,
    pw: PtrWidth,
    self_func_idx: u32,
) {
    // W32 uses the legacy implementation (refcount decrement only; child-drop not extended for W32
    // W32 remains only where prior code still produces it.
    if pw == PtrWidth::W32 {
        emit_goby_drop_function_w32(code);
        return;
    }
    // Locals layout:
    //  0: tagged_ptr (param, i64)
    //  1: refcount   (i64)
    //  2: tag        (i64)
    //  3: payload_ptr (i64)  — lower 60 bits of tagged_ptr
    //  4: n          (i64)   — n_chunks (list) or arity (tuple) or loop counter
    //  5: i          (i64)   — loop index
    //  6: item       (i64)   — loop item value (tagged or raw ptr)
    let param_tagged: u32 = 0;
    let l_refcount: u32 = 1;
    let l_tag: u32 = 2;
    let l_payload: u32 = 3;
    let l_n: u32 = 4;
    let l_i: u32 = 5;
    let l_item: u32 = 6;

    // 6 additional i64 locals (indices 1..=6).
    let mut function = Function::new(vec![(6, ValType::I64)]);

    const PAYLOAD_MASK: i64 = 0x0FFF_FFFF_FFFF_FFFFu64 as i64;

    // Helper closures for concise emission.
    let push_payload = |f: &mut Function| {
        f.instruction(&Instruction::LocalGet(l_payload));
        if pw == PtrWidth::W32 {
            f.instruction(&Instruction::I32WrapI64);
        }
    };

    // --- 1. Non-heap short-circuit (w64 path only; the w32 legacy path above
    //        keeps the original I64Eqz-against-tagged_ptr test).
    //
    // Tag lives in bits 63–60 of the tagged word. Scalar tags:
    //   Unit=0x0 (whole word is 0), Int=0x1, Bool=0x2. Their payloads are
    //   inline, so the drop is a no-op.
    // Func=0x5 is a funcref table index, not a heap pointer — also a no-op.
    // All remaining tags (TAG_STRING=0x3, TAG_LIST=0x4, TAG_TUPLE=0x6,
    // TAG_RECORD=0x7, TAG_CLOSURE=0x8, TAG_CELL=0x9, TAG_CHUNK=0xA) are
    // heap pointers and fall through to the refcount decrement below.
    //
    // Note: the `tag <= 2` check covers encode_unit() (tagged_ptr == 0)
    // because its tag bits are 0, but it is a *tag* comparison, not a
    // comparison against the raw word value, so an `Int(0)` (tag=0x1,
    // payload=0) is still correctly classified as non-heap.
    function.instruction(&Instruction::LocalGet(param_tagged));
    function.instruction(&Instruction::I64Const(60));
    function.instruction(&Instruction::I64ShrU);
    function.instruction(&Instruction::LocalSet(l_tag)); // l_tag holds just the 4-bit tag

    // if tag <= 2: non-heap scalar (Unit/Int/Bool) → return
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(2));
    function.instruction(&Instruction::I64LeU);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    // if tag == 5: Func (funcref index, not a heap ptr) → return
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_FUNC as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    // --- 2. payload_ptr = tagged_ptr & PAYLOAD_MASK ---
    function.instruction(&Instruction::LocalGet(param_tagged));
    function.instruction(&Instruction::I64Const(PAYLOAD_MASK));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::LocalSet(l_payload));

    // --- 3/4. header_addr = payload_ptr - 8; refcount = mem[header_addr] ---
    // (We compute header_addr inline from l_payload.)
    // Push header_addr as memory address.
    let push_header_addr = |f: &mut Function| {
        f.instruction(&Instruction::LocalGet(l_payload));
        f.instruction(&Instruction::I64Const(-(REFCOUNT_WORD_BYTES as i64)));
        f.instruction(&Instruction::I64Add);
        if pw == PtrWidth::W32 {
            f.instruction(&Instruction::I32WrapI64);
        }
    };

    push_header_addr(&mut function);
    function.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(l_refcount));

    // Sentinel check.
    function.instruction(&Instruction::LocalGet(l_refcount));
    function.instruction(&Instruction::I64Const(-1i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    // --- 5. Decrement refcount; return if still > 0 ---
    push_header_addr(&mut function);
    function.instruction(&Instruction::LocalGet(l_refcount));
    function.instruction(&Instruction::I64Const(1));
    function.instruction(&Instruction::I64Sub);
    function.instruction(&Instruction::LocalTee(l_refcount));
    function.instruction(&Instruction::I64Store(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalGet(l_refcount));
    function.instruction(&Instruction::I64Const(0));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Return);
    function.instruction(&Instruction::End);

    // --- 6. tag = (tagged_ptr >> 60) & 0xF ---
    function.instruction(&Instruction::LocalGet(param_tagged));
    function.instruction(&Instruction::I64Const(60));
    function.instruction(&Instruction::I64ShrU);
    function.instruction(&Instruction::I64Const(0xF));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::LocalSet(l_tag));

    // Helper: increment freed_bytes by delta (i64 literal).
    let add_freed_bytes = |f: &mut Function, delta: i64| {
        let addr_instr = ptr_const(pw, GLOBAL_FREED_BYTES_OFFSET as u64);
        f.instruction(&addr_instr.clone());
        f.instruction(&addr_instr);
        f.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        f.instruction(&Instruction::I64Const(delta));
        f.instruction(&Instruction::I64Add);
        f.instruction(&Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
    };

    // Helper: increment freed_bytes by the value in l_n.
    let add_freed_bytes_dyn = |f: &mut Function| {
        let addr_instr = ptr_const(pw, GLOBAL_FREED_BYTES_OFFSET as u64);
        f.instruction(&addr_instr.clone());
        f.instruction(&addr_instr);
        f.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        f.instruction(&Instruction::LocalGet(l_n));
        f.instruction(&Instruction::I64Add);
        f.instruction(&Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
    };

    // Helper: push payload_ptr to a free-list and increment free_list_hits.
    let push_to_free_list = |f: &mut Function, head_offset: u32| {
        // mem[payload_ptr - 8] = old_head  (clobbers refcount slot as intrusive next-ptr)
        push_header_addr(f);
        f.instruction(&Instruction::I64Const(head_offset as i64));
        f.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        f.instruction(&Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        // mem[head_offset] = payload_ptr
        f.instruction(&Instruction::I64Const(head_offset as i64));
        f.instruction(&Instruction::LocalGet(l_payload));
        f.instruction(&Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        // free_list_hits++
        let hits = ptr_const(pw, GLOBAL_FREE_LIST_HITS_OFFSET as u64);
        f.instruction(&hits.clone());
        f.instruction(&hits);
        f.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        f.instruction(&Instruction::I64Const(1));
        f.instruction(&Instruction::I64Add);
        f.instruction(&Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
    };

    // Helper: emit a counted loop that calls __goby_drop(mem[base + 8*i]) for i in 0..count.
    // base_offset_bytes: constant byte offset from l_payload to the first element.
    // count_local: i64 local holding the element count.
    let emit_child_drop_loop = |f: &mut Function,
                                base_offset_bytes: u64,
                                count_local: u32,
                                item_local: u32,
                                loop_i_local: u32| {
        // i = 0
        f.instruction(&Instruction::I64Const(0));
        f.instruction(&Instruction::LocalSet(loop_i_local));
        f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
        f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
        // if i >= count: break
        f.instruction(&Instruction::LocalGet(loop_i_local));
        f.instruction(&Instruction::LocalGet(count_local));
        f.instruction(&Instruction::I64GeU);
        f.instruction(&Instruction::BrIf(1)); // br to block (exit)
        // item = mem[payload + base_offset + i*8]
        f.instruction(&Instruction::LocalGet(l_payload));
        f.instruction(&Instruction::I64Const(base_offset_bytes as i64));
        f.instruction(&Instruction::I64Add);
        f.instruction(&Instruction::LocalGet(loop_i_local));
        f.instruction(&Instruction::I64Const(8));
        f.instruction(&Instruction::I64Mul);
        f.instruction(&Instruction::I64Add);
        if pw == PtrWidth::W32 {
            f.instruction(&Instruction::I32WrapI64);
        }
        f.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        f.instruction(&Instruction::LocalSet(item_local));
        // __goby_drop(item)
        f.instruction(&Instruction::LocalGet(item_local));
        f.instruction(&Instruction::Call(self_func_idx));
        // i++
        f.instruction(&Instruction::LocalGet(loop_i_local));
        f.instruction(&Instruction::I64Const(1));
        f.instruction(&Instruction::I64Add);
        f.instruction(&Instruction::LocalSet(loop_i_local));
        f.instruction(&Instruction::Br(0)); // br to loop
        f.instruction(&Instruction::End); // end loop
        f.instruction(&Instruction::End); // end block
    };

    // -----------------------------------------------------------------------
    // TAG_CHUNK (0xA) — internal list chunk drop.
    //   layout: [len: i64][item[0]: i64]...[item[CHUNK_SIZE-1]: i64]
    //   len is the number of valid items (≤ CHUNK_SIZE).
    // -----------------------------------------------------------------------
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_CHUNK as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    {
        // n = mem[payload_ptr + 0]  (chunk len)
        push_payload(&mut function);
        function.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        function.instruction(&Instruction::LocalSet(l_n));
        // Drop items[0..n].
        emit_child_drop_loop(&mut function, 8, l_n, l_item, l_i);
        // freed_bytes += REFCOUNT_WORD_BYTES + chunk_payload_size
        add_freed_bytes(
            &mut function,
            REFCOUNT_WORD_BYTES as i64 + chunk_alloc_size_pw(pw) as i64,
        );
        // push to Chunk free-list
        use crate::layout::FREE_LIST_SLOT_CHUNK;
        push_to_free_list(&mut function, FREE_LIST_SLOT_CHUNK);
    }
    function.instruction(&Instruction::End);

    // -----------------------------------------------------------------------
    // TAG_LIST (0x4) — list header drop.
    //   layout: [total_len: i64][n_chunks: i64][chunk_ptr[0]: i64]...
    // -----------------------------------------------------------------------
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_LIST as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    {
        // n_chunks = mem[payload_ptr + 8]
        push_payload(&mut function);
        function.instruction(&Instruction::I64Load(MemArg {
            offset: 8,
            align: 3,
            memory_index: 0,
        }));
        function.instruction(&Instruction::LocalSet(l_n));
        // For each chunk_ptr at [payload + 16 + i*8]:
        //   synthesise TAG_CHUNK-tagged ptr and call __goby_drop.
        {
            // i = 0
            function.instruction(&Instruction::I64Const(0));
            function.instruction(&Instruction::LocalSet(l_i));
            function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
            function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
            // if i >= n_chunks: break
            function.instruction(&Instruction::LocalGet(l_i));
            function.instruction(&Instruction::LocalGet(l_n));
            function.instruction(&Instruction::I64GeU);
            function.instruction(&Instruction::BrIf(1));
            // raw_chunk_ptr = mem[payload + 16 + i*8]
            function.instruction(&Instruction::LocalGet(l_payload));
            function.instruction(&Instruction::I64Const(16));
            function.instruction(&Instruction::I64Add);
            function.instruction(&Instruction::LocalGet(l_i));
            function.instruction(&Instruction::I64Const(8));
            function.instruction(&Instruction::I64Mul);
            function.instruction(&Instruction::I64Add);
            if pw == PtrWidth::W32 {
                function.instruction(&Instruction::I32WrapI64);
            }
            function.instruction(&Instruction::I64Load(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
            function.instruction(&Instruction::LocalSet(l_item));
            // tagged_chunk = (TAG_CHUNK << 60) | raw_chunk_ptr
            function.instruction(&Instruction::LocalGet(l_item));
            function.instruction(&Instruction::I64Const((TAG_CHUNK as i64) << 60));
            function.instruction(&Instruction::I64Or);
            function.instruction(&Instruction::Call(self_func_idx));
            // i++
            function.instruction(&Instruction::LocalGet(l_i));
            function.instruction(&Instruction::I64Const(1));
            function.instruction(&Instruction::I64Add);
            function.instruction(&Instruction::LocalSet(l_i));
            function.instruction(&Instruction::Br(0));
            function.instruction(&Instruction::End); // loop
            function.instruction(&Instruction::End); // block
        }
        // freed_bytes += REFCOUNT_WORD_BYTES + 16 + n_chunks * 8  (header payload)
        // = REFCOUNT_WORD_BYTES + 16 + l_n * 8
        // We compute: (16 + l_n * 8) into l_n, then add freed_bytes.
        function.instruction(&Instruction::LocalGet(l_n));
        function.instruction(&Instruction::I64Const(8));
        function.instruction(&Instruction::I64Mul);
        function.instruction(&Instruction::I64Const(
            (REFCOUNT_WORD_BYTES as i64) + 16, // refcount header + total_len + n_chunks words
        ));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::LocalSet(l_n));
        add_freed_bytes_dyn(&mut function);
        // No free-list for headers (size varies by n_chunks).
    }
    function.instruction(&Instruction::End);

    // -----------------------------------------------------------------------
    // TAG_TUPLE (0x6) — tuple drop.
    //   layout: [arity: i64][elem[0]: i64]...
    // -----------------------------------------------------------------------
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_TUPLE as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    {
        // arity = mem[payload_ptr + 0]
        push_payload(&mut function);
        function.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        function.instruction(&Instruction::LocalSet(l_n));
        // Drop elements[0..arity].
        emit_child_drop_loop(&mut function, 8, l_n, l_item, l_i);
        // freed_bytes += REFCOUNT_WORD_BYTES + 8 + arity * 8
        function.instruction(&Instruction::LocalGet(l_n));
        function.instruction(&Instruction::I64Const(8));
        function.instruction(&Instruction::I64Mul);
        function.instruction(&Instruction::I64Const((REFCOUNT_WORD_BYTES as i64) + 8));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::LocalSet(l_n));
        add_freed_bytes_dyn(&mut function);
        // No free-list for tuples (arity varies at runtime).
    }
    function.instruction(&Instruction::End);

    // -----------------------------------------------------------------------
    // TAG_CELL (0x9) — mutable cell drop.
    //   layout: [value: i64]; drop the contained value, then push to Cell free-list.
    // -----------------------------------------------------------------------
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_CELL as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    {
        // Drop contained value.
        push_payload(&mut function);
        function.instruction(&Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        function.instruction(&Instruction::Call(self_func_idx));
        // freed_bytes += REFCOUNT_WORD_BYTES + 8
        add_freed_bytes(&mut function, (REFCOUNT_WORD_BYTES + 8) as i64);
        // push to Cell free-list
        use crate::layout::FREE_LIST_SLOT_CELL;
        push_to_free_list(&mut function, FREE_LIST_SLOT_CELL);
    }
    function.instruction(&Instruction::End);

    // -----------------------------------------------------------------------
    // TAG_STRING (0x3) — string payload drop.
    //   layout: [len: i32][bytes...], allocated as payload+refcount aligned to 8.
    //   String free-list reuse is not wired into alloc yet, so reaching rc=0
    //   only removes the bytes from live accounting.
    // -----------------------------------------------------------------------
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_STRING as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    {
        push_payload(&mut function);
        function.instruction(&Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        function.instruction(&Instruction::I64ExtendI32U);
        function.instruction(&Instruction::I64Const(4));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::I64Const(7));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::I64Const(!7u64 as i64));
        function.instruction(&Instruction::I64And);
        function.instruction(&Instruction::I64Const(REFCOUNT_WORD_BYTES as i64));
        function.instruction(&Instruction::I64Add);
        function.instruction(&Instruction::LocalSet(l_n));
        add_freed_bytes_dyn(&mut function);
    }
    function.instruction(&Instruction::End);

    // -----------------------------------------------------------------------
    // TAG_RECORD (0x7): child-drop deferred (ctor_tag at offset 0; arity not in payload).
    // -----------------------------------------------------------------------
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_RECORD as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    add_freed_bytes(&mut function, REFCOUNT_WORD_BYTES as i64);
    function.instruction(&Instruction::End);

    // -----------------------------------------------------------------------
    // TAG_CLOSURE (0x8): child-drop deferred (func_handle at offset 0; slot count unknown).
    // -----------------------------------------------------------------------
    function.instruction(&Instruction::LocalGet(l_tag));
    function.instruction(&Instruction::I64Const(TAG_CLOSURE as i64));
    function.instruction(&Instruction::I64Eq);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    add_freed_bytes(&mut function, REFCOUNT_WORD_BYTES as i64);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::End);
    code.function(&function);
}

fn emit_chunked_list_item_store(
    function: &mut Function,
    header_ptr_local: u32,
    item_count_local: u32,
    item_ptr_local: u32,
    chunk_idx_local: u32,
    item_idx_local: u32,
    pw: PtrWidth,
) {
    // chunk/item index arithmetic — pw-aware
    function.instruction(&Instruction::LocalGet(item_count_local));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&Instruction::LocalSet(chunk_idx_local));
    function.instruction(&Instruction::LocalGet(item_count_local));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    match pw {
        PtrWidth::W32 => function.instruction(&Instruction::I32RemU),
        PtrWidth::W64 => function.instruction(&Instruction::I64RemU),
    };
    function.instruction(&Instruction::LocalSet(item_idx_local));

    // Load chunk pointer (address) from header — ptr_load
    function.instruction(&Instruction::LocalGet(header_ptr_local));
    function.instruction(&Instruction::LocalGet(chunk_idx_local));
    function.instruction(&ptr_const(pw, 4));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(item_idx_local));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    emit_push_tagged_ptr(function, item_ptr_local, TAG_STRING, pw);
    function.instruction(&Instruction::I64Store(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));

    // Update chunk item_count — ptr_load + ptr_store
    function.instruction(&Instruction::LocalGet(header_ptr_local));
    function.instruction(&Instruction::LocalGet(chunk_idx_local));
    function.instruction(&ptr_const(pw, 4));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(item_idx_local));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
}

fn emit_copy_string_slice_to_alloc(
    function: &mut Function,
    source_ptr_local: u32,
    start_local: u32,
    end_local: u32,
    dest_ptr_local: u32,
    copy_idx_local: u32,
    pw: PtrWidth,
) {
    function.instruction(&Instruction::LocalGet(end_local));
    function.instruction(&Instruction::LocalGet(start_local));
    function.instruction(&ptr_sub(pw));
    function.instruction(&Instruction::LocalSet(copy_idx_local));

    function.instruction(&Instruction::LocalGet(dest_ptr_local));
    function.instruction(&Instruction::LocalGet(copy_idx_local));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I32WrapI64);
    }
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));

    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(copy_idx_local));

    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(copy_idx_local));
    function.instruction(&Instruction::LocalGet(end_local));
    function.instruction(&Instruction::LocalGet(start_local));
    function.instruction(&ptr_sub(pw));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    // dest_ptr + 4 (len field) + copy_idx
    function.instruction(&Instruction::LocalGet(dest_ptr_local));
    function.instruction(&ptr_const(pw, 4));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(copy_idx_local));
    function.instruction(&ptr_add(pw));

    // source_ptr + 4 + start + copy_idx
    function.instruction(&Instruction::LocalGet(source_ptr_local));
    function.instruction(&ptr_const(pw, 4));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(start_local));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(copy_idx_local));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load_8u(
        pw,
        MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        },
    ));
    function.instruction(&ptr_store_8(
        pw,
        MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        },
    ));

    function.instruction(&Instruction::LocalGet(copy_idx_local));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(copy_idx_local));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
}

fn emit_string_split_helper(
    function: &mut Function,
    helper_state: &HeapEmitState,
) -> Result<(), CodegenError> {
    let pw = helper_state.pw();
    let graphemes_host_offset =
        host_import_offset_for_intrinsic(BackendIntrinsic::StringGraphemesList)?;
    let text_i64 = helper_state.scratch.i64_base;
    let sep_i64 = helper_state.scratch.i64_base + 1;
    let s_text_ptr = helper_state.scratch.i32_base + HS_TEXT_PTR;
    let s_text_len = helper_state.scratch.i32_base + HS_TEXT_LEN;
    let s_sep_ptr = helper_state.scratch.i32_base + HS_SEP_PTR;
    let s_sep_len = helper_state.scratch.i32_base + HS_SEP_LEN;
    let s_pos = helper_state.scratch.i32_base + HS_SCAN_POS;
    let s_start = helper_state.scratch.i32_base + HS_SEG_START;
    let s_item_count = helper_state.scratch.i32_base + HS_ITEM_COUNT;
    let s_aux_ptr = helper_state.scratch.i32_base + HS_AUX_PTR;
    let s_list_ptr = helper_state.scratch.i32_base + HS_LIST_PTR;
    let s_alloc_size = helper_state.scratch.i32_base + HS_ALLOC_SIZE;
    let s_iter = helper_state.scratch.i32_base + HS_ITER;

    function.instruction(&Instruction::LocalSet(sep_i64));
    function.instruction(&Instruction::LocalSet(text_i64));

    emit_decode_string_ptr(
        function,
        &helper_state.scratch,
        text_i64,
        s_text_ptr,
        s_text_len,
        helper_state.pw(),
    );
    emit_decode_string_ptr(
        function,
        &helper_state.scratch,
        sep_i64,
        s_sep_ptr,
        s_sep_len,
        helper_state.pw(),
    );

    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_sep_len));
    function.instruction(&ptr_eqz(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_sync_cursor_to_global(function, helper_state.alloc_cursor_local, helper_state.pw());
    emit_sync_floor_to_global(function, helper_state.heap_floor_local, helper_state.pw());
    function.instruction(&Instruction::LocalGet(text_i64));
    function.instruction(&Instruction::Call(
        HOST_IMPORT_BASE_IDX + graphemes_host_offset,
    ));
    function.instruction(&Instruction::LocalSet(sep_i64));
    emit_return_if_runtime_error(function, helper_state);
    emit_sync_cursor_from_global(function, helper_state.alloc_cursor_local, helper_state.pw());
    emit_sync_floor_from_global(function, helper_state.heap_floor_local, helper_state.pw());
    emit_clamp_heap_floor_to_host_bump_cursor(
        function,
        helper_state.heap_floor_local,
        helper_state.pw(),
    );
    emit_sync_floor_to_global(function, helper_state.heap_floor_local, helper_state.pw());
    function.instruction(&Instruction::Br(1));
    function.instruction(&Instruction::End);

    // Allocate a header large enough for the worst-case segment count:
    // each byte split separately plus one final segment => text_len + 1 items.
    function.instruction(&Instruction::LocalGet(s_text_len));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&Instruction::LocalSet(s_iter)); // max_chunks
    function.instruction(&ptr_const(pw, header_alloc_size_pw(pw, 0) as u64));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_list_ptr);

    // Pre-allocate all possible chunks and store their pointers in the header.
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_item_count));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_item_count));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_aux_ptr);

    function.instruction(&Instruction::LocalGet(s_aux_ptr));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));

    function.instruction(&Instruction::LocalGet(s_list_ptr));
    function.instruction(&Instruction::LocalGet(s_item_count));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_aux_ptr));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));

    function.instruction(&Instruction::LocalGet(s_item_count));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_item_count));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_item_count));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_start));

    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_text_len));
    function.instruction(&ptr_gt_u(pw));
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_iter));

    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_sep_len));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_text_len));
    function.instruction(&ptr_le_u(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::LocalGet(s_sep_len));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Br(2));
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_text_ptr));
    function.instruction(&ptr_const(pw, 4));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load_8u(
        pw,
        MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_sep_ptr));
    function.instruction(&ptr_const(pw, 4));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load_8u(
        pw,
        MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        },
    ));
    function.instruction(&ptr_ne(pw));
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_eq(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_start));
    function.instruction(&ptr_sub(pw));
    function.instruction(&ptr_const(pw, 4));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_aux_ptr);
    emit_copy_string_slice_to_alloc(
        function,
        s_text_ptr,
        s_start,
        s_pos,
        s_aux_ptr,
        s_iter,
        helper_state.pw(),
    );
    emit_chunked_list_item_store(
        function,
        s_list_ptr,
        s_item_count,
        s_aux_ptr,
        s_iter,
        s_alloc_size,
        helper_state.pw(),
    );
    function.instruction(&Instruction::LocalGet(s_item_count));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_item_count));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_sep_len));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalSet(s_start));
    function.instruction(&Instruction::Br(1));
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_text_len));
    function.instruction(&Instruction::LocalGet(s_start));
    function.instruction(&ptr_sub(pw));
    function.instruction(&ptr_const(pw, 4));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_aux_ptr);
    emit_copy_string_slice_to_alloc(
        function,
        s_text_ptr,
        s_start,
        s_text_len,
        s_aux_ptr,
        s_iter,
        helper_state.pw(),
    );
    emit_chunked_list_item_store(
        function,
        s_list_ptr,
        s_item_count,
        s_aux_ptr,
        s_iter,
        s_alloc_size,
        helper_state.pw(),
    );
    function.instruction(&Instruction::LocalGet(s_item_count));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_item_count));

    function.instruction(&Instruction::LocalGet(s_list_ptr));
    function.instruction(&Instruction::LocalGet(s_item_count));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_list_ptr));
    function.instruction(&Instruction::LocalGet(s_item_count));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64 - 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_n_chunks_offset_pw(pw) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    emit_push_tagged_ptr(function, s_list_ptr, TAG_LIST, pw);
    function.instruction(&Instruction::LocalSet(sep_i64));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalGet(sep_i64));
    Ok(())
}

fn emit_list_get_helper(
    function: &mut Function,
    helper_state: &EmitScratchState,
    pw: PtrWidth,
) -> Result<(), CodegenError> {
    // Chunked Sequence (Candidate B) index read.
    let list_i64 = helper_state.i64_base;
    let index_i64 = helper_state.i64_base + 1;
    // i32 scratch: reuse existing HS_* names where possible.
    let s_header_ptr = helper_state.i32_base + HS_TEXT_PTR; // u32 header ptr
    let s_total_len = helper_state.i32_base + HS_TEXT_LEN; // total_len (for bounds check)
    let s_index = helper_state.i32_base + HS_SEP_PTR; // decoded logical index
    let s_chunk_idx = helper_state.i32_base + HS_SEP_LEN; // index / CHUNK_SIZE
    let s_item_idx = helper_state.i32_base + HS_SCAN_POS; // index % CHUNK_SIZE

    function.instruction(&Instruction::LocalSet(index_i64));
    function.instruction(&Instruction::LocalSet(list_i64));

    // Validate TAG_LIST
    function.instruction(&Instruction::LocalGet(list_i64));
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // Validate TAG_INT for index
    function.instruction(&Instruction::LocalGet(index_i64));
    function.instruction(&Instruction::I64Const((TAG_INT as i64) << 60));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const((TAG_INT as i64) << 60));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // Decode signed index (sign-extend 60-bit payload) — keep as i64
    function.instruction(&Instruction::LocalGet(index_i64));
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64Shl);
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64ShrS);
    // In W32: store as i32 via I32WrapI64; in W64: store as i64 directly
    if pw == PtrWidth::W32 {
        function.instruction(&Instruction::I32WrapI64);
    }
    function.instruction(&Instruction::LocalSet(s_index));

    // Reject negative index (signed comparison with 0)
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&ptr_const(pw, 0));
    match pw {
        PtrWidth::W32 => function.instruction(&Instruction::I32LtS),
        PtrWidth::W64 => function.instruction(&Instruction::I64LtS),
    };
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // Decode header ptr from tagged i64 (lower 32 bits)
    function.instruction(&Instruction::LocalGet(list_i64));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_header_ptr));

    // Load total_len = header[0]
    function.instruction(&Instruction::LocalGet(s_header_ptr));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_total_len));

    // Bounds check: index >= total_len → abort
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&Instruction::LocalGet(s_total_len));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // chunk_idx = index / CHUNK_SIZE
    // CHUNK_SIZE is a locked power-of-two (32), so use shift instead of div.
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&ptr_const(pw, CHUNK_SIZE.trailing_zeros() as u64));
    match pw {
        PtrWidth::W32 => function.instruction(&Instruction::I32ShrU),
        PtrWidth::W64 => function.instruction(&Instruction::I64ShrU),
    };
    function.instruction(&Instruction::LocalSet(s_chunk_idx));

    // item_idx = index % CHUNK_SIZE
    // CHUNK_SIZE is a locked power-of-two (32), so use mask instead of rem.
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64 - 1));
    match pw {
        PtrWidth::W32 => function.instruction(&Instruction::I32And),
        PtrWidth::W64 => function.instruction(&Instruction::I64And),
    };
    function.instruction(&Instruction::LocalSet(s_item_idx));

    // chunk_ptr = header[2*meta_bytes + chunk_idx * ptr_slot_bytes]
    // (reuse s_total_len as s_chunk_ptr after bounds check is done)
    function.instruction(&Instruction::LocalGet(s_header_ptr));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_total_len)); // now holds chunk_ptr

    // item = chunk[meta_bytes + item_idx * 8]
    function.instruction(&Instruction::LocalGet(s_total_len)); // chunk_ptr
    function.instruction(&Instruction::LocalGet(s_item_idx));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));
    Ok(())
}

// ---------------------------------------------------------------------------
// Chunked Sequence Wasm-emitting helpers (Step 2 — no callers yet).
// These helpers emit Wasm instructions that perform chunk-aware list access
// using the Candidate B layout (see CHUNK_SIZE constants above).
// ---------------------------------------------------------------------------

/// Emit Wasm that loads element at logical index `index_local` from a chunked
/// list whose header starts at `header_ptr_local`.
///
/// Preconditions (caller must have verified):
///   - `header_ptr_local` holds a valid u32 pointer to a chunked list header.
///   - `index_local` holds a non-negative i32 logical index that is < total_len.
///
/// After execution, the loaded tagged i64 value is on the Wasm stack.
///
/// Scratch slots used: `s_chunk_idx`, `s_item_idx`, `s_chunk_ptr`.
/// The caller must supply three available i32 local indices for these.
fn emit_chunked_load(
    function: &mut Function,
    header_ptr_local: u32,
    index_local: u32,
    s_chunk_idx: u32,
    s_item_idx: u32,
    s_chunk_ptr: u32,
    pw: PtrWidth,
) {
    // chunk_idx = index / CHUNK_SIZE  (i32 value arithmetic — not address)
    // CHUNK_SIZE is a locked power-of-two (32), so use shift instead of div.
    function.instruction(&Instruction::LocalGet(index_local));
    function.instruction(&ptr_const(pw, CHUNK_SIZE.trailing_zeros() as u64));
    function.instruction(&ptr_shr_u(pw));
    function.instruction(&Instruction::LocalSet(s_chunk_idx));

    // item_idx = index % CHUNK_SIZE  (i32 value arithmetic — not address)
    // CHUNK_SIZE is a locked power-of-two (32), so use mask instead of rem.
    function.instruction(&Instruction::LocalGet(index_local));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64 - 1));
    function.instruction(&ptr_and(pw));
    function.instruction(&Instruction::LocalSet(s_item_idx));

    // chunk_ptr = header[8 + chunk_idx * 4]  (header_chunk_ptr_offset)
    // The chunk_idx*4 part is i32 value arithmetic; the I32Add/I32Mul here advance
    // into the header's chunk-pointer table, so result is an address → ptr_load.
    function.instruction(&Instruction::LocalGet(header_ptr_local));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64, // base offset 8; chunk_idx*4 added above
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_chunk_ptr));

    // item = chunk[4 + item_idx * 8]  (chunk_item_offset)  — tagged i64 value
    // item_idx*8 is value arithmetic; base load is a tagged i64 (not pointer), keep I64Load.
    function.instruction(&Instruction::LocalGet(s_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_item_idx));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64, // base offset 4; item_idx*8 added above
        align: 3,
        memory_index: 0,
    }));
}

/// Emit Wasm that loads one tagged i64 item from `header_ptr_local` at a
/// compile-time constant logical index.
#[inline]
fn emit_chunked_load_const(
    function: &mut Function,
    header_ptr_local: u32,
    logical_index: u32,
    pw: PtrWidth,
) {
    let chunk_idx = logical_index / CHUNK_SIZE;
    let item_idx = logical_index % CHUNK_SIZE;
    function.instruction(&Instruction::LocalGet(header_ptr_local));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, chunk_idx) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: chunk_item_offset_pw(pw, item_idx) as u64,
        align: 3,
        memory_index: 0,
    }));
}

#[inline]
fn emit_case_list_tag_check(function: &mut Function, scrutinee_idx: u32) {
    function.instruction(&Instruction::LocalGet(scrutinee_idx));
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64Eq);
}

#[inline]
fn emit_decode_list_header_ptr(
    function: &mut Function,
    scrutinee_idx: u32,
    s_header_ptr: u32,
    pw: PtrWidth,
) {
    function.instruction(&Instruction::LocalGet(scrutinee_idx));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_header_ptr));
}

#[inline]
fn emit_load_list_total_len(
    function: &mut Function,
    s_header_ptr: u32,
    s_total_len: u32,
    pw: PtrWidth,
) {
    function.instruction(&Instruction::LocalGet(s_header_ptr));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_total_len));
}

#[allow(clippy::too_many_arguments)]
fn emit_case_bind_tail_list(
    function: &mut Function,
    hs: &HeapEmitState,
    tail_local: u32,
    s_list_ptr: u32,
    s_list_len: u32,
    s_alloc_size: u32,
    s_iter: u32,
    s_tail_ptr: u32,
    s_chunk_idx: u32,
    s_item_idx: u32,
    s_chunk_ptr: u32,
    s_tail_chunk_ptr: u32,
    s_tail_chunk_len: u32,
    s_load_chunk_ptr: u32,
    n_items: i32,
) {
    let pw = hs.pw();
    // tail_len = s_list_len - n_items (runtime value)
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&ptr_const(pw, n_items as i64 as u64));
    function.instruction(&ptr_sub(pw));
    function.instruction(&Instruction::LocalSet(s_iter)); // s_iter = tail_len

    // Allocate tail header with enough slots for ceil(total_len / CHUNK_SIZE).
    // n_slots = (total_len + CHUNK_SIZE - 1) / CHUNK_SIZE
    // header_size = 2*meta_slot_bytes + n_slots * ptr_slot_bytes
    function.instruction(&Instruction::LocalGet(s_list_len));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64 - 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_const(pw, 2 * meta_slot_bytes(pw) as u64));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, hs, s_alloc_size, s_tail_ptr);

    // Allocate first tail chunk.
    function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, hs, s_alloc_size, s_tail_chunk_ptr);
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_tail_chunk_len));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_chunk_idx)); // n_tail_chunks

    // copy_i = 0
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_item_idx));

    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));

    // if copy_i >= tail_len: break
    function.instruction(&Instruction::LocalGet(s_item_idx));
    function.instruction(&Instruction::LocalGet(s_iter)); // tail_len
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    // src_logical = n_items + copy_i
    function.instruction(&Instruction::LocalGet(s_item_idx));
    function.instruction(&ptr_const(pw, n_items as u64));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_alloc_size)); // src_logical
    emit_chunked_load(
        function,
        s_list_ptr,
        s_alloc_size,
        s_list_len,
        s_chunk_ptr,
        s_load_chunk_ptr,
        pw,
    ); // val on stack (i64)

    let val_i64 = hs.scratch.i64_base;
    function.instruction(&Instruction::LocalSet(val_i64)); // save val

    // Rotate chunk if full.
    function.instruction(&Instruction::LocalGet(s_tail_chunk_len));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_eq(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_tail_ptr));
    function.instruction(&Instruction::LocalGet(s_chunk_idx)); // n_tail_chunks
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_tail_chunk_ptr));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_chunk_idx));
    function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, hs, s_alloc_size, s_tail_chunk_ptr);
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_tail_chunk_len));
    function.instruction(&Instruction::End);

    // tail_chunk[tail_chunk_len] = val
    function.instruction(&Instruction::LocalGet(s_tail_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_tail_chunk_len));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(val_i64));
    function.instruction(&Instruction::I64Store(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalGet(s_tail_chunk_len));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_tail_chunk_len));
    function.instruction(&Instruction::LocalGet(s_tail_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_tail_chunk_len));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));

    // copy_i++
    function.instruction(&Instruction::LocalGet(s_item_idx));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_item_idx));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Finalize last chunk iff tail_len > 0
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&ptr_eq(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(s_tail_ptr));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_tail_chunk_ptr));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_chunk_idx));
    function.instruction(&Instruction::End);

    // header.total_len = tail_len; header.n_chunks = n_tail_chunks
    function.instruction(&Instruction::LocalGet(s_tail_ptr));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_tail_ptr));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_n_chunks_offset_pw(pw) as u64,
            align: 2,
            memory_index: 0,
        },
    ));

    emit_push_tagged_ptr(function, s_tail_ptr, TAG_LIST, pw);
    function.instruction(&Instruction::LocalSet(tail_local));
}

/// Emit Wasm that stores a tagged i64 value (currently on the Wasm value stack)
/// to logical index `index_local` in the chunked list at `header_ptr_local`.
///
/// The value to store must be pushed on the Wasm stack by the caller AFTER
/// calling this function (this function emits the address calculation first;
/// the value and I64Store must be emitted by the caller pattern):
///
/// ```text
/// emit_chunked_store_address(...)   // pushes destination address
/// ... push value ...                // push i64 value
/// I64Store(align:3)                 // store
/// ```
///
/// This helper only emits the address calculation (resulting address on stack).
fn emit_chunked_store_address(
    function: &mut Function,
    header_ptr_local: u32,
    index_local: u32,
    s_chunk_idx: u32,
    s_item_idx: u32,
    s_chunk_ptr: u32,
    pw: PtrWidth,
) {
    // chunk_idx = index / CHUNK_SIZE  (i32 value arithmetic)
    // CHUNK_SIZE is a locked power-of-two (32), so use shift instead of div.
    function.instruction(&Instruction::LocalGet(index_local));
    function.instruction(&ptr_const(pw, CHUNK_SIZE.trailing_zeros() as u64));
    function.instruction(&ptr_shr_u(pw));
    function.instruction(&Instruction::LocalSet(s_chunk_idx));

    // item_idx = index % CHUNK_SIZE  (i32 value arithmetic)
    // CHUNK_SIZE is a locked power-of-two (32), so use mask instead of rem.
    function.instruction(&Instruction::LocalGet(index_local));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64 - 1));
    function.instruction(&ptr_and(pw));
    function.instruction(&Instruction::LocalSet(s_item_idx));

    // chunk_ptr = header[8 + chunk_idx * ptr_slot_bytes(pw)]  — ptr_load for address slot
    function.instruction(&Instruction::LocalGet(header_ptr_local));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_chunk_ptr));

    // address = chunk_ptr + 4 + item_idx * 8
    function.instruction(&Instruction::LocalGet(s_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_item_idx));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    // leaves (chunk_ptr + item_idx*8) on stack; caller adds I64Store with offset=4
}

fn emit_string_length_helper(
    function: &mut Function,
    helper_state: &EmitScratchState,
    pw: PtrWidth,
) -> Result<(), CodegenError> {
    let string_i64 = helper_state.i64_base;
    let s_ptr = helper_state.i32_base + HS_TEXT_PTR;
    let s_len = helper_state.i32_base + HS_TEXT_LEN;
    function.instruction(&Instruction::LocalSet(string_i64));
    emit_decode_string_ptr(function, helper_state, string_i64, s_ptr, s_len, pw);
    function.instruction(&Instruction::LocalGet(s_len));
    // W32: s_len is i32 → I64ExtendI32S; W64: s_len is already i64
    if let Some(ext) = ptr_extend_to_i64(pw) {
        function.instruction(&ext);
    }
    function.instruction(&Instruction::I64Const((TAG_INT as i64) << 60));
    function.instruction(&Instruction::I64Or);
    Ok(())
}

/// Read `total_len` from a chunked list header in O(1) and return it as a tagged Int.
///
/// Stack input: tagged list i64.
/// Stack output: tagged Int i64 (the element count).
fn emit_list_length_helper(
    function: &mut Function,
    helper_state: &EmitScratchState,
    pw: PtrWidth,
) -> Result<(), CodegenError> {
    let list_i64 = helper_state.i64_base;
    let s_header_ptr = helper_state.i32_base + HS_TEXT_PTR;
    let s_total_len = helper_state.i32_base + HS_TEXT_LEN;

    // Pop tagged list into local
    function.instruction(&Instruction::LocalSet(list_i64));

    // Decode header ptr and load total_len via shared helpers
    emit_decode_list_header_ptr(function, list_i64, s_header_ptr, pw);
    emit_load_list_total_len(function, s_header_ptr, s_total_len, pw);

    // Widen to i64 if needed (W32: i32 → I64ExtendI32U; W64: already i64) and tag as INT
    function.instruction(&Instruction::LocalGet(s_total_len));
    if let Some(ext) = ptr_extend_to_i64(pw) {
        function.instruction(&ext);
    }
    function.instruction(&Instruction::I64Const((TAG_INT as i64) << 60));
    function.instruction(&Instruction::I64Or);
    Ok(())
}

/// Left fold over a chunked list: chunk-walk loop + accumulator + callback dispatch.
///
/// Stack input: `[list_tagged, init_acc, func_tagged]` (func on top).
/// Stack output: final accumulator value (i64).
///
/// The callback `func(acc, elem)` is called via `emit_callable_dispatch` with 2 arguments.
fn emit_list_fold_helper(
    function: &mut Function,
    hs: &HeapEmitState,
    indirect_call_type_idx: u32,
    closure_call_type_idx: u32,
) -> Result<(), CodegenError> {
    let s_func = hs.scratch.i64_base; // i64: func handle
    let s_acc = hs.scratch.i64_base + 1; // i64: accumulator
    let s_elem = hs.scratch.i64_base + 2; // i64: current element (reuse slot)
    let s_header_ptr = hs.scratch.i32_base + HS_LIST_PTR;
    let s_n_chunks = hs.scratch.i32_base + HS_ITEM_COUNT;
    let s_chunk_idx = hs.scratch.i32_base + HS_ITER;
    let s_chunk_ptr = hs.scratch.i32_base + HS_AUX_PTR;
    let s_chunk_len = hs.scratch.i32_base + HS_ALLOC_SIZE;
    let s_item_iter = hs.scratch.i32_base + HS_TEXT_PTR;

    let pw = hs.pw();
    // Stack: [list_tagged, init_acc, func_tagged]
    // Pop in reverse order: func, acc, list
    function.instruction(&Instruction::LocalSet(s_func));
    function.instruction(&Instruction::LocalSet(s_acc));
    // list_tagged is on stack — decode header ptr
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_header_ptr));

    // n_chunks = header[meta_bytes]
    function.instruction(&Instruction::LocalGet(s_header_ptr));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_n_chunks_offset_pw(pw) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_n_chunks));

    // chunk_idx = 0
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_chunk_idx));

    // Outer loop: for chunk_idx in 0..n_chunks
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&Instruction::LocalGet(s_n_chunks));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    // chunk_ptr = header[2*meta_bytes + chunk_idx * ptr_slot_bytes]
    function.instruction(&Instruction::LocalGet(s_header_ptr));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_chunk_ptr));
    // chunk_len = chunk[0]
    function.instruction(&Instruction::LocalGet(s_chunk_ptr));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_chunk_len));
    // item_iter = 0
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_item_iter));

    // Inner loop: for item_iter in 0..chunk_len
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_item_iter));
    function.instruction(&Instruction::LocalGet(s_chunk_len));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    // elem = chunk[meta_bytes + item_iter * 8]
    function.instruction(&Instruction::LocalGet(s_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_item_iter));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(s_elem));

    // acc = func(acc, elem)
    emit_callable_dispatch(
        function,
        s_func,
        &[s_acc, s_elem],
        indirect_call_type_idx,
        closure_call_type_idx,
        pw,
        Some(hs),
    );
    function.instruction(&Instruction::LocalSet(s_acc));
    emit_return_if_runtime_error(function, hs);

    // item_iter += 1
    function.instruction(&Instruction::LocalGet(s_item_iter));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_item_iter));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End); // end inner loop
    function.instruction(&Instruction::End); // end inner block

    // chunk_idx += 1
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_chunk_idx));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End); // end outer loop
    function.instruction(&Instruction::End); // end outer block

    // Push final accumulator
    function.instruction(&Instruction::LocalGet(s_acc));
    Ok(())
}

fn emit_list_push_string_helper(
    function: &mut Function,
    helper_state: &HeapEmitState,
) -> Result<(), CodegenError> {
    let pw = helper_state.pw();
    // Chunked layout: append one string element to a chunked list.
    // Reads chunks from source, copies into a new chunked list with room for one more element.
    //
    // Scratch slots:
    //   i64_base+0  = list_i64
    //   i64_base+1  = string_i64
    //   HS_TEXT_PTR (0)  = s_src_header_ptr
    //   HS_TEXT_LEN (1)  = s_src_total_len
    //   HS_SEP_PTR  (2)  = s_string_ptr
    //   HS_SEP_LEN  (3)  = s_string_len (unused but declared for decode helper)
    //   HS_AUX_PTR  (7)  = s_dst_header_ptr
    //   HS_ALLOC_SIZE(9) = s_alloc_size
    //   HS_ITER     (10) = s_global_iter   (0 .. src_total_len+1, absolute element index)
    //   HS_LIST_PTR (8)  = s_chunk_ptr     (current src or dst chunk ptr)
    //   HS_ITEM_COUNT(6) = s_n_chunks      (src n_chunks / scratch)
    //   HS_SCAN_POS (4)  = s_dst_n_chunks
    //   HS_SEG_START(5)  = s_dst_chunk_ptr (current destination chunk ptr)
    let list_i64 = helper_state.scratch.i64_base;
    let string_i64 = helper_state.scratch.i64_base + 1;
    let s_src_header = helper_state.scratch.i32_base + HS_TEXT_PTR;
    let s_src_total_len = helper_state.scratch.i32_base + HS_TEXT_LEN;
    let s_string_ptr = helper_state.scratch.i32_base + HS_SEP_PTR;
    let s_string_len = helper_state.scratch.i32_base + HS_SEP_LEN;
    let s_dst_header = helper_state.scratch.i32_base + HS_AUX_PTR;
    let s_alloc_size = helper_state.scratch.i32_base + HS_ALLOC_SIZE;
    let s_global_iter = helper_state.scratch.i32_base + HS_ITER;
    let s_chunk_ptr = helper_state.scratch.i32_base + HS_LIST_PTR;
    let s_src_n_chunks = helper_state.scratch.i32_base + HS_ITEM_COUNT;
    let s_dst_n_chunks = helper_state.scratch.i32_base + HS_SCAN_POS;
    let s_dst_chunk_ptr = helper_state.scratch.i32_base + HS_SEG_START;

    function.instruction(&Instruction::LocalSet(string_i64));
    function.instruction(&Instruction::LocalSet(list_i64));

    // Validate list tag
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
        &helper_state.scratch,
        string_i64,
        s_string_ptr,
        s_string_len,
        helper_state.pw(),
    );

    // Decode source list header ptr
    function.instruction(&Instruction::LocalGet(list_i64));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_src_header));

    // src_total_len = header[0]
    function.instruction(&Instruction::LocalGet(s_src_header));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_src_total_len));

    // src_n_chunks = header[4]
    function.instruction(&Instruction::LocalGet(s_src_header));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_n_chunks_offset_pw(pw) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_src_n_chunks));

    // dst_n_chunks = ceil((src_total_len + 1) / CHUNK_SIZE) = (src_total_len + CHUNK_SIZE) / CHUNK_SIZE
    // Always >= 1 because we have at least 1 element (the new string).
    function.instruction(&Instruction::LocalGet(s_src_total_len));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&Instruction::LocalSet(s_dst_n_chunks));

    // Allocate destination header
    function.instruction(&ptr_const(pw, header_alloc_size_pw(pw, 0) as u64));
    function.instruction(&Instruction::LocalGet(s_dst_n_chunks));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_dst_header);

    // Allocate each dst chunk and copy source elements + new string element.
    // We iterate absolute index from 0 to src_total_len (inclusive).
    // dst_chunk_ptr tracks current destination chunk.
    // For absolute index i:
    //   dst_ci = i / CHUNK_SIZE,  dst_ii = i % CHUNK_SIZE
    //   src_ci = i / CHUNK_SIZE,  src_ii = i % CHUNK_SIZE  (same for elements i < src_total_len)
    //   element src_total_len = new string
    //
    // Strategy: allocate all dst chunks up front, then do one flat loop writing each element.

    // Allocate all dst chunks
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_global_iter));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&Instruction::LocalGet(s_dst_n_chunks));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    // alloc chunk
    function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_chunk_ptr);

    // dst_header[8 + global_iter*4] = chunk_ptr
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_chunk_ptr));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));

    // Set chunk len = 0 initially
    function.instruction(&Instruction::LocalGet(s_chunk_ptr));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));

    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_global_iter));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Copy source elements (flat loop over 0..src_total_len)
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_global_iter));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&Instruction::LocalGet(s_src_total_len));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    // src: ci = global_iter / CHUNK_SIZE, ii = global_iter % CHUNK_SIZE
    // src_chunk_ptr = src_header[8 + ci*4]
    function.instruction(&Instruction::LocalGet(s_src_header));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_chunk_ptr));

    // elem = src_chunk[4 + ii*8]
    function.instruction(&Instruction::LocalGet(s_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_rem_u(pw));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));
    // Save elem to list_i64 scratch (reused as temp; we're past the initial decode)
    function.instruction(&Instruction::LocalSet(list_i64));

    // dst: dst_ci = global_iter / CHUNK_SIZE, dst_ii = global_iter % CHUNK_SIZE
    // dst_chunk_ptr = dst_header[8 + dst_ci*4]
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_dst_chunk_ptr));

    // dst_chunk[4 + dst_ii*8] = elem  (address first, then value)
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_rem_u(pw));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(list_i64)); // elem value (i64) on top
    function.instruction(&Instruction::I64Store(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));

    // dst_chunk[0] (len) += 1 — increment chunk length
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));

    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_global_iter));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Append the new string element at absolute index src_total_len
    // dst_ci = src_total_len / CHUNK_SIZE, dst_ii = src_total_len % CHUNK_SIZE
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_src_total_len));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_dst_chunk_ptr));

    // dst_chunk[4 + dst_ii*8] = tagged_string
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_src_total_len));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_rem_u(pw));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    emit_push_tagged_ptr(function, s_string_ptr, TAG_STRING, pw);
    function.instruction(&Instruction::I64Store(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));

    // dst_chunk[0] += 1
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));

    // Write dst header fields: total_len = src_total_len + 1, n_chunks = dst_n_chunks
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_src_total_len));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_dst_n_chunks));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_n_chunks_offset_pw(pw) as u64,
            align: 2,
            memory_index: 0,
        },
    ));

    emit_push_tagged_ptr(function, s_dst_header, TAG_LIST, pw);
    Ok(())
}

fn emit_list_concat_helper(
    function: &mut Function,
    helper_state: &HeapEmitState,
    goby_drop_func_idx: u32,
) -> Result<(), CodegenError> {
    let pw = helper_state.pw();
    // Chunked layout: concatenate two chunked lists into a new chunked list.
    // Strategy: iterate global index 0..total_len, reading from prefix then tail.
    //
    // Scratch:
    //   i64_base+0 = tail_i64
    //   i64_base+1 = prefix_i64
    //   HS_TEXT_PTR (0)  = s_prefix_header
    //   HS_TEXT_LEN (1)  = s_prefix_len
    //   HS_SEP_PTR  (2)  = s_tail_header
    //   HS_SEP_LEN  (3)  = s_tail_len
    //   HS_AUX_PTR  (7)  = s_dst_header
    //   HS_ALLOC_SIZE(9) = s_alloc_size
    //   HS_ITER     (10) = s_global_iter
    //   HS_LIST_PTR (8)  = s_src_chunk_ptr / s_dst_chunk_ptr (reused)
    //   HS_ITEM_COUNT(6) = s_dst_n_chunks
    //   HS_SCAN_POS (4)  = s_total_len
    //   HS_SEG_START(5)  = s_dst_chunk_ptr
    let tail_i64 = helper_state.scratch.i64_base;
    let prefix_i64 = helper_state.scratch.i64_base + 1;
    let s_prefix_header = helper_state.scratch.i32_base + HS_TEXT_PTR;
    let s_prefix_len = helper_state.scratch.i32_base + HS_TEXT_LEN;
    let s_tail_header = helper_state.scratch.i32_base + HS_SEP_PTR;
    let s_tail_len = helper_state.scratch.i32_base + HS_SEP_LEN;
    let s_dst_header = helper_state.scratch.i32_base + HS_AUX_PTR;
    let s_alloc_size = helper_state.scratch.i32_base + HS_ALLOC_SIZE;
    let s_global_iter = helper_state.scratch.i32_base + HS_ITER;
    let s_src_chunk_ptr = helper_state.scratch.i32_base + HS_LIST_PTR;
    let s_dst_n_chunks = helper_state.scratch.i32_base + HS_ITEM_COUNT;
    let s_total_len = helper_state.scratch.i32_base + HS_SCAN_POS;
    let s_dst_chunk_ptr = helper_state.scratch.i32_base + HS_SEG_START;

    function.instruction(&Instruction::LocalSet(tail_i64));
    function.instruction(&Instruction::LocalSet(prefix_i64));

    // Validate tags
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

    // Decode header pointers
    function.instruction(&Instruction::LocalGet(prefix_i64));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_prefix_header));

    function.instruction(&Instruction::LocalGet(tail_i64));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_tail_header));

    // prefix_len = prefix_header[0]
    function.instruction(&Instruction::LocalGet(s_prefix_header));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_prefix_len));

    // tail_len = tail_header[0]
    function.instruction(&Instruction::LocalGet(s_tail_header));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_tail_len));

    // total_len = prefix_len + tail_len
    function.instruction(&Instruction::LocalGet(s_prefix_len));
    function.instruction(&Instruction::LocalGet(s_tail_len));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_total_len));

    // dst_n_chunks = max(1, ceil(total_len / CHUNK_SIZE))
    function.instruction(&Instruction::LocalGet(s_total_len));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64 - 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    // clamp to 1 if total_len was 0
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_gt_u(pw)); // if computed > 1
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ptr_val_type(pw),
    )));
    function.instruction(&Instruction::LocalGet(s_total_len));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64 - 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&Instruction::Else);
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalSet(s_dst_n_chunks));

    // Allocate dst header
    function.instruction(&ptr_const(pw, header_alloc_size_pw(pw, 0) as u64));
    function.instruction(&Instruction::LocalGet(s_dst_n_chunks));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_dst_header);

    // Allocate each dst chunk
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_global_iter));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&Instruction::LocalGet(s_dst_n_chunks));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_dst_chunk_ptr);

    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));

    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_global_iter));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Copy all elements: for global_iter in 0..total_len
    // src_header and src_idx:
    //   if global_iter < prefix_len: src_header = prefix_header, src_idx = global_iter
    //   else:                        src_header = tail_header,   src_idx = global_iter - prefix_len
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_global_iter));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&Instruction::LocalGet(s_total_len));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    // Determine source: pick prefix or tail
    // Use s_src_chunk_ptr as temp to hold chosen src_idx before computing chunk
    // if global_iter < prefix_len: src_header, idx=global_iter
    // else: tail_header, idx=global_iter - prefix_len
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&Instruction::LocalGet(s_prefix_len));
    function.instruction(&ptr_lt_u(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ValType::I64,
    )));
    // prefix branch: load from prefix_header at global_iter
    function.instruction(&Instruction::LocalGet(s_prefix_header));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_src_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_src_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_rem_u(pw));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::Else);
    // tail branch: load from tail_header at (global_iter - prefix_len)
    function.instruction(&Instruction::LocalGet(s_tail_header));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&Instruction::LocalGet(s_prefix_len));
    function.instruction(&ptr_sub(pw));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_src_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_src_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&Instruction::LocalGet(s_prefix_len));
    function.instruction(&ptr_sub(pw));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_rem_u(pw));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::End); // if/else result: elem (i64) on stack
    // Save elem to prefix_i64 scratch (prefix_i64 is free after decode above).
    // tail_i64 must remain unchanged so we can Drop the original tail list below.
    function.instruction(&Instruction::LocalSet(prefix_i64));

    // Write elem to dst: dst_chunk = dst_header[8 + (global_iter/CHUNK_SIZE)*4]
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_dst_chunk_ptr));

    // dst_chunk[4 + dst_ii*8] = elem
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_rem_u(pw));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(prefix_i64)); // elem value (i64) on top
    function.instruction(&Instruction::I64Store(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));

    // dst_chunk[0] += 1
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));

    function.instruction(&Instruction::LocalGet(s_global_iter));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_global_iter));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Write dst header: total_len, n_chunks
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_total_len));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_dst_n_chunks));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_n_chunks_offset_pw(pw) as u64,
            align: 2,
            memory_index: 0,
        },
    ));

    emit_push_tagged_ptr(function, s_dst_header, TAG_LIST, pw);

    // Drop the tail (spread source): ownership of the tail is consumed by the concat.
    function.instruction(&Instruction::LocalGet(tail_i64));
    function.instruction(&Instruction::Call(goby_drop_func_idx));

    Ok(())
}

/// Emit Wasm instructions for the `list.set(list, index, value)` intrinsic.
///
/// Stack on entry (bottom to top): list_i64, index_i64, value_i64.
fn emit_list_set_helper(
    function: &mut Function,
    helper_state: &HeapEmitState,
) -> Result<(), CodegenError> {
    let pw = helper_state.pw();
    // Chunk-local immutable update:
    //   1) copy header pointer table;
    //   2) copy only the touched chunk;
    //   3) patch one element in the copied chunk.
    //
    // Scratch:
    //   i64_base+0 = value_i64
    //   i64_base+1 = list_i64
    //   i64_base+2 = index_i64
    //   HS_TEXT_PTR (0)  = s_src_header
    //   HS_TEXT_LEN (1)  = s_total_len
    //   HS_SEP_PTR  (2)  = s_index
    //   HS_SEP_LEN  (3)  = s_item_idx
    //   HS_SCAN_POS (4)  = s_chunk_idx
    //   HS_SEG_START(5)  = s_dst_chunk_ptr
    //   HS_ITEM_COUNT(6) = s_n_chunks
    //   HS_AUX_PTR  (7)  = s_dst_header
    //   HS_LIST_PTR (8)  = s_src_chunk_ptr
    //   HS_ALLOC_SIZE(9) = s_alloc_size
    //   HS_ITER     (10) = s_word_off
    let value_i64 = helper_state.scratch.i64_base;
    let list_i64 = helper_state.scratch.i64_base + 1;
    let index_i64 = helper_state.scratch.i64_base + 2;
    let s_src_header = helper_state.scratch.i32_base + HS_TEXT_PTR;
    let s_total_len = helper_state.scratch.i32_base + HS_TEXT_LEN;
    let s_index = helper_state.scratch.i32_base + HS_SEP_PTR;
    let s_item_idx = helper_state.scratch.i32_base + HS_SEP_LEN;
    let s_chunk_idx = helper_state.scratch.i32_base + HS_SCAN_POS;
    let s_dst_chunk_ptr = helper_state.scratch.i32_base + HS_SEG_START;
    let s_n_chunks = helper_state.scratch.i32_base + HS_ITEM_COUNT;
    let s_dst_header = helper_state.scratch.i32_base + HS_AUX_PTR;
    let s_src_chunk_ptr = helper_state.scratch.i32_base + HS_LIST_PTR;
    let s_alloc_size = helper_state.scratch.i32_base + HS_ALLOC_SIZE;
    let s_word_off = helper_state.scratch.i32_base + HS_ITER;

    // Stack: list, index, value (value on top)
    function.instruction(&Instruction::LocalSet(value_i64));
    function.instruction(&Instruction::LocalSet(index_i64));
    function.instruction(&Instruction::LocalSet(list_i64));

    // Validate list tag
    function.instruction(&Instruction::LocalGet(list_i64));
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // Validate index tag
    function.instruction(&Instruction::LocalGet(index_i64));
    function.instruction(&Instruction::I64Const((TAG_INT as i64) << 60));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const((TAG_INT as i64) << 60));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // Decode index (sign-extend 60-bit payload)
    function.instruction(&Instruction::LocalGet(index_i64));
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64Shl);
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64ShrS);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_index));

    // Reject negative index
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&ptr_lt_s(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // Decode src header ptr
    function.instruction(&Instruction::LocalGet(list_i64));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_src_header));

    // total_len = src_header[0]
    function.instruction(&Instruction::LocalGet(s_src_header));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_total_len));

    // Bounds check: index >= total_len → abort
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&Instruction::LocalGet(s_total_len));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // n_chunks = src_header[4]
    function.instruction(&Instruction::LocalGet(s_src_header));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_n_chunks_offset_pw(pw) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_n_chunks));

    // Allocate dst header (same size as src header).
    function.instruction(&ptr_const(pw, header_alloc_size_pw(pw, 0) as u64));
    function.instruction(&Instruction::LocalGet(s_n_chunks));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_dst_header);

    // Copy src header -> dst header in pointer-width slots.
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_word_off));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_word_off));
    function.instruction(&ptr_const(
        pw,
        (header_alloc_size_pw(pw, 0) / ptr_slot_bytes(pw)) as u64,
    ));
    function.instruction(&Instruction::LocalGet(s_n_chunks));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_word_off));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_src_header));
    function.instruction(&Instruction::LocalGet(s_word_off));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));

    function.instruction(&Instruction::LocalGet(s_word_off));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_word_off));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // chunk_idx = index / CHUNK_SIZE ; item_idx = index % CHUNK_SIZE.
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&ptr_const(pw, CHUNK_SIZE.trailing_zeros() as u64));
    function.instruction(&ptr_shr_u(pw));
    function.instruction(&Instruction::LocalSet(s_chunk_idx));
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64 - 1));
    function.instruction(&ptr_and(pw));
    function.instruction(&Instruction::LocalSet(s_item_idx));

    // src_chunk_ptr = src_header.chunk_ptrs[chunk_idx].
    function.instruction(&Instruction::LocalGet(s_src_header));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_src_chunk_ptr));

    // Allocate replacement chunk.
    function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, helper_state, s_alloc_size, s_dst_chunk_ptr);

    // Copy source chunk -> replacement chunk in pointer-width slots.
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_word_off));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_word_off));
    function.instruction(&ptr_const(
        pw,
        (chunk_alloc_size_pw(pw) / ptr_slot_bytes(pw)) as u64,
    ));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_word_off));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_src_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_word_off));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));

    function.instruction(&Instruction::LocalGet(s_word_off));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_word_off));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Swap replacement chunk pointer into copied header.
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));

    // Store new value to replacement chunk[item_idx].
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_item_idx));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(value_i64));
    function.instruction(&Instruction::I64Store(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));

    // Header metadata is unchanged, but keep it explicit.
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_total_len));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_n_chunks));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_n_chunks_offset_pw(pw) as u64,
            align: 2,
            memory_index: 0,
        },
    ));

    emit_push_tagged_ptr(function, s_dst_header, TAG_LIST, pw);
    Ok(())
}

/// Emit Wasm instructions for `BackendIntrinsic::ListSetInPlace`.
///
/// **In-place element update** — does not allocate a new header or chunk.
/// Directly overwrites the target word in the existing chunk.
///
/// Stack contract (on entry): `list_i64`, `index_i64`, `value_i64` (value on top).
/// Stack contract (on exit): `list_i64` (same handle as input).
///
/// Validation prelude is identical to `emit_list_set_helper`:
/// tag checks, negative-index rejection, bounds check.
///
/// Aliasing contract: the caller (`lower_supported_inline_list_fold_mutating_each`)
/// has already verified that no other live reference to the list exists, so the
/// mutation is safe without copying.
///
/// Scratch slots used (same names as in `emit_list_set_helper`):
///   i64_base+0 = value_i64
///   i64_base+1 = list_i64
///   i64_base+2 = index_i64
///   HS_TEXT_PTR  (0) = s_src_header
///   HS_TEXT_LEN  (1) = s_total_len
///   HS_SEP_PTR   (2) = s_index
///   HS_SEP_LEN   (3) = s_item_idx
///   HS_SCAN_POS  (4) = s_chunk_idx
///   HS_SEG_START (5) = s_dst_chunk_ptr  (points into the *existing* chunk)
fn emit_list_set_in_place_helper(
    function: &mut Function,
    helper_state: &HeapEmitState,
) -> Result<(), CodegenError> {
    let pw = helper_state.pw();
    let value_i64 = helper_state.scratch.i64_base;
    let list_i64 = helper_state.scratch.i64_base + 1;
    let index_i64 = helper_state.scratch.i64_base + 2;
    let s_src_header = helper_state.scratch.i32_base + HS_TEXT_PTR;
    let s_total_len = helper_state.scratch.i32_base + HS_TEXT_LEN;
    let s_index = helper_state.scratch.i32_base + HS_SEP_PTR;
    let s_item_idx = helper_state.scratch.i32_base + HS_SEP_LEN;
    let s_chunk_idx = helper_state.scratch.i32_base + HS_SCAN_POS;
    let s_chunk_ptr = helper_state.scratch.i32_base + HS_SEG_START;

    // Stack: list, index, value (value on top)
    function.instruction(&Instruction::LocalSet(value_i64));
    function.instruction(&Instruction::LocalSet(index_i64));
    function.instruction(&Instruction::LocalSet(list_i64));

    // Validate list tag
    function.instruction(&Instruction::LocalGet(list_i64));
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // Validate index tag
    function.instruction(&Instruction::LocalGet(index_i64));
    function.instruction(&Instruction::I64Const((TAG_INT as i64) << 60));
    function.instruction(&Instruction::I64And);
    function.instruction(&Instruction::I64Const((TAG_INT as i64) << 60));
    function.instruction(&Instruction::I64Ne);
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // Decode index (sign-extend 60-bit payload)
    function.instruction(&Instruction::LocalGet(index_i64));
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64Shl);
    function.instruction(&Instruction::I64Const(4));
    function.instruction(&Instruction::I64ShrS);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_index));

    // Reject negative index
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&ptr_lt_s(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // Decode src header ptr
    function.instruction(&Instruction::LocalGet(list_i64));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_src_header));

    // total_len = src_header[0]
    function.instruction(&Instruction::LocalGet(s_src_header));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_total_len));

    // Bounds check: index >= total_len → abort
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&Instruction::LocalGet(s_total_len));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    // chunk_idx = index / CHUNK_SIZE ; item_idx = index % CHUNK_SIZE
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&ptr_const(pw, CHUNK_SIZE.trailing_zeros() as u64));
    function.instruction(&ptr_shr_u(pw));
    function.instruction(&Instruction::LocalSet(s_chunk_idx));
    function.instruction(&Instruction::LocalGet(s_index));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64 - 1));
    function.instruction(&ptr_and(pw));
    function.instruction(&Instruction::LocalSet(s_item_idx));

    // chunk_ptr = src_header.chunk_ptrs[chunk_idx]  (pointer into existing chunk)
    function.instruction(&Instruction::LocalGet(s_src_header));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_chunk_ptr));

    // Store value directly into existing chunk[item_idx]  — no allocation.
    function.instruction(&Instruction::LocalGet(s_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_item_idx));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(value_i64));
    function.instruction(&Instruction::I64Store(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));

    // Return the original list handle unchanged.
    function.instruction(&Instruction::LocalGet(list_i64));
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
    helper_state: Option<EmitScratchState>,
    pw: PtrWidth,
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
    // Like bool_from_i32! but the value on the stack is already i64 (0 or 1).
    // Used when the result comes from a ptr-width (i64 in W64) scratch local.
    macro_rules! bool_from_i64 {
        ($f:expr) => {
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
            // General tagged equality:
            // - identical tagged values are equal,
            // - otherwise String/String falls through to content comparison,
            // - all other unequal pairs are false.
            let (right_i64, left_i64) = require_scratch!(i64_scratch_base, "Eq");
            let hs = require_scratch_state(helper_state, "BinOp::Eq")?;
            let s_left_ptr = hs.i32_base + HS_TEXT_PTR;
            let s_left_len = hs.i32_base + HS_TEXT_LEN;
            let s_right_ptr = hs.i32_base + HS_SEP_PTR;
            let s_right_len = hs.i32_base + HS_SEP_LEN;
            let s_iter = hs.i32_base + HS_SCAN_POS;
            let s_result = hs.i32_base + HS_ITEM_COUNT;

            function.instruction(&Instruction::LocalSet(right_i64));
            function.instruction(&Instruction::LocalSet(left_i64));
            function.instruction(&ptr_const(pw, 0));
            function.instruction(&Instruction::LocalSet(s_result));

            function.instruction(&Instruction::LocalGet(left_i64));
            function.instruction(&Instruction::LocalGet(right_i64));
            function.instruction(&Instruction::I64Eq);
            function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
            function.instruction(&ptr_const(pw, 1));
            function.instruction(&Instruction::LocalSet(s_result));
            function.instruction(&Instruction::Else);

            function.instruction(&Instruction::LocalGet(left_i64));
            function.instruction(&Instruction::I64Const((TAG_STRING as i64) << 60));
            function.instruction(&Instruction::I64And);
            function.instruction(&Instruction::I64Const((TAG_STRING as i64) << 60));
            function.instruction(&Instruction::I64Eq);
            function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

            function.instruction(&Instruction::LocalGet(right_i64));
            function.instruction(&Instruction::I64Const((TAG_STRING as i64) << 60));
            function.instruction(&Instruction::I64And);
            function.instruction(&Instruction::I64Const((TAG_STRING as i64) << 60));
            function.instruction(&Instruction::I64Eq);
            function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

            emit_decode_string_ptr(function, &hs, left_i64, s_left_ptr, s_left_len, pw);
            emit_decode_string_ptr(function, &hs, right_i64, s_right_ptr, s_right_len, pw);
            emit_compare_decoded_strings(
                function,
                s_left_ptr,
                s_left_len,
                s_right_ptr,
                s_right_len,
                s_iter,
                s_result,
                pw,
            );
            function.instruction(&Instruction::End);
            function.instruction(&Instruction::End);
            function.instruction(&Instruction::End);

            function.instruction(&Instruction::LocalGet(s_result));
            if pw == PtrWidth::W64 {
                // s_result is an i64 scratch local (0 or 1); skip I64ExtendI32U
                bool_from_i64!(function);
            } else {
                bool_from_i32!(function);
            }
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
        IrBinOp::BitXor => {
            // Int: TAG_INT<<60 | payload. XOR of the upper 4 tag bits cancels to 0,
            // so retag_int! (PAYLOAD_MASK + TAG_INT_SHIFT) reinstalls the tag.
            function.instruction(&Instruction::I64Xor);
            retag_int!(function);
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
    helper_state: Option<&HeapEmitState>,
    strategy: EffectEmitStrategy,
    pw: PtrWidth,
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
                pw,
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
    helper_state: Option<&HeapEmitState>,
    pw: PtrWidth,
) -> Result<(), CodegenError> {
    match op {
        BackendEffectOp::Read(BackendReadOp::Read) => {
            // Store string data at buffer_ptr+4, length at buffer_ptr.
            // Bytes land at buffer_ptr+4; we read into buffer_ptr+4 to leave room for the len prefix.
            let data_ptr = buffer_ptr + 4;

            // iovec[0] = data_ptr
            function.instruction(&ptr_const(pw, iovec_offset as u64));
            function.instruction(&Instruction::I32Const(data_ptr));
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            // iovec[1] = buffer_len
            function.instruction(&ptr_const(pw, (iovec_offset + 4) as u64));
            function.instruction(&Instruction::I32Const(buffer_len));
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            // fd_read(0, iovec_offset, 1, nread_offset) — WASI Preview 1: always i32 args
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Const(iovec_offset));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Const(nread_offset));
            function.instruction(&Instruction::Call(FD_READ_IDX));
            function.instruction(&Instruction::Drop); // discard errno

            // Store len = memory[nread_offset] at buffer_ptr (length prefix).
            // Both nread and string length are 4-byte i32 fields regardless of pw.
            // Addresses must be ptr-width (i64 in W64).
            function.instruction(&ptr_const(pw, buffer_ptr as u64));
            function.instruction(&ptr_const(pw, nread_offset as u64));
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
                    helper_state.pw(),
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
            function.instruction(&ptr_const(pw, iovec_offset as u64));
            function.instruction(&Instruction::I32Const(data_ptr));
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            // iovec[1] = buffer_len
            function.instruction(&ptr_const(pw, (iovec_offset + 4) as u64));
            function.instruction(&Instruction::I32Const(buffer_len));
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            // fd_read(0, iovec_offset, 1, nread_offset) — WASI Preview 1: always i32 args
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Const(iovec_offset));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Const(nread_offset));
            function.instruction(&Instruction::Call(FD_READ_IDX));
            function.instruction(&Instruction::Drop);

            // WASI writes a 4-byte i32 nread value — use I32Load always.
            // Address must be ptr-width (i64 in W64).
            function.instruction(&ptr_const(pw, nread_offset as u64));
            function.instruction(&Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            if pw == PtrWidth::W64 {
                function.instruction(&Instruction::I64ExtendI32U);
            }
            function.instruction(&Instruction::LocalSet(total_len_local));
            function.instruction(&ptr_const(pw, 0));
            function.instruction(&Instruction::LocalSet(scan_idx_local));

            function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
            function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
            function.instruction(&Instruction::LocalGet(scan_idx_local));
            function.instruction(&Instruction::LocalGet(total_len_local));
            function.instruction(&ptr_ge_u(pw));
            function.instruction(&Instruction::BrIf(1));

            function.instruction(&ptr_const(pw, data_ptr as u64));
            function.instruction(&Instruction::LocalGet(scan_idx_local));
            function.instruction(&ptr_add(pw));
            function.instruction(&ptr_load_8u(
                pw,
                MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                },
            ));
            function.instruction(&ptr_const(pw, 10));
            function.instruction(&ptr_eq(pw));
            function.instruction(&Instruction::BrIf(1));

            function.instruction(&ptr_const(pw, data_ptr as u64));
            function.instruction(&Instruction::LocalGet(scan_idx_local));
            function.instruction(&ptr_add(pw));
            function.instruction(&ptr_load_8u(
                pw,
                MemArg {
                    offset: 0,
                    align: 0,
                    memory_index: 0,
                },
            ));
            function.instruction(&ptr_const(pw, 13));
            function.instruction(&ptr_eq(pw));
            function.instruction(&Instruction::BrIf(1));

            function.instruction(&Instruction::LocalGet(scan_idx_local));
            function.instruction(&ptr_const(pw, 1));
            function.instruction(&ptr_add(pw));
            function.instruction(&Instruction::LocalSet(scan_idx_local));
            function.instruction(&Instruction::Br(0));
            function.instruction(&Instruction::End);
            function.instruction(&Instruction::End);

            function.instruction(&ptr_const(pw, buffer_ptr as u64));
            function.instruction(&Instruction::LocalGet(scan_idx_local));
            if pw == PtrWidth::W64 {
                function.instruction(&Instruction::I32WrapI64);
            }
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
                    helper_state.pw(),
                );
            }

            function.instruction(&Instruction::I64Const(encode_string_ptr(buffer_ptr as u32)));
        }

        BackendEffectOp::Print(print_op) => {
            let append_newline = print_op.append_newline();
            let ptr_local = i32_base;

            // Stack top is a tagged-i64 string ptr. Extract raw pointer (low 32 bits).
            // Under W32: wrap to i32; under W64: keep as i64 (ptr_local is i64 local).
            function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
            function.instruction(&Instruction::I64And);
            if pw == PtrWidth::W32 {
                function.instruction(&Instruction::I32WrapI64);
            }
            function.instruction(&Instruction::LocalSet(ptr_local));

            // iovec[0] = ptr_local + 4  (data pointer, skipping len prefix)
            function.instruction(&ptr_const(pw, iovec_offset as u64));
            function.instruction(&Instruction::LocalGet(ptr_local));
            function.instruction(&ptr_const(pw, 4));
            function.instruction(&ptr_add(pw));
            if pw == PtrWidth::W64 {
                function.instruction(&Instruction::I32WrapI64);
            }
            function.instruction(&Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            // iovec[1] = memory[ptr_local]  (the len — always a 4-byte i32 field)
            function.instruction(&ptr_const(pw, (iovec_offset + 4) as u64));
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
            // fd_write(1, iovec_offset, 1, nread_offset) — WASI Preview 1: always i32 args
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Const(iovec_offset));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Const(nread_offset));
            function.instruction(&Instruction::Call(FD_WRITE_IDX));
            function.instruction(&Instruction::Drop); // discard errno

            if append_newline {
                function.instruction(&ptr_const(pw, newline_ptr as u64));
                function.instruction(&ptr_const(pw, 10)); // '\n'
                function.instruction(&ptr_store_8(
                    pw,
                    MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    },
                ));
                // iovec[0] = newline_ptr, iovec[1] = 1
                function.instruction(&ptr_const(pw, iovec_offset as u64));
                function.instruction(&Instruction::I32Const(newline_ptr));
                function.instruction(&Instruction::I32Store(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                function.instruction(&ptr_const(pw, (iovec_offset + 4) as u64));
                function.instruction(&Instruction::I32Const(1));
                function.instruction(&Instruction::I32Store(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                // fd_write(1, iovec_offset, 1, nread_offset) — WASI Preview 1: always i32 args
                function.instruction(&Instruction::I32Const(1));
                function.instruction(&Instruction::I32Const(iovec_offset));
                function.instruction(&Instruction::I32Const(1));
                function.instruction(&Instruction::I32Const(nread_offset));
                function.instruction(&Instruction::Call(FD_WRITE_IDX));
                function.instruction(&Instruction::Drop);
            }

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
    pw: PtrWidth,
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
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_str_ptr));

    // str_len = mem[str_ptr] (the len prefix — always i32 regardless of pw)
    function.instruction(&Instruction::LocalGet(s_str_ptr));
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I64ExtendI32U);
    }
    function.instruction(&Instruction::LocalSet(s_str_len));

    // pos = 0; line_start = 0
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_line_start));

    // if str_len == 0: skip everything
    function.instruction(&Instruction::LocalGet(s_str_len));
    function.instruction(&ptr_eqz(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Else);

    // Scan loop: for pos in 0..str_len
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty)); // block (break target)
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty)); // loop

    // if pos >= str_len: break
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_str_len));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1)); // break block

    // byte = mem[str_ptr + 4 + pos]  (data starts at str_ptr+4)
    function.instruction(&Instruction::LocalGet(s_str_ptr));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load_8u(
        pw,
        MemArg {
            offset: 4,
            align: 0,
            memory_index: 0,
        },
    ));

    // if byte == sep_byte: emit slice and advance line_start
    function.instruction(&ptr_const(pw, sep_byte as u64));
    function.instruction(&ptr_eq(pw));
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
        pw,
    );
    // line_start = pos + 1
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_line_start));
    function.instruction(&Instruction::End); // end if sep

    // pos += 1
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&Instruction::Br(0)); // continue loop

    function.instruction(&Instruction::End); // end loop
    function.instruction(&Instruction::End); // end block

    // After loop: emit final segment if line_start < str_len (last line w/o trailing sep)
    function.instruction(&Instruction::LocalGet(s_line_start));
    function.instruction(&Instruction::LocalGet(s_str_len));
    function.instruction(&ptr_lt_u(pw));
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
        pw,
    );
    function.instruction(&Instruction::End); // end if last segment

    function.instruction(&Instruction::End); // end else (str_len != 0)

    // Result: push unit value
    function.instruction(&Instruction::I64Const(encode_unit()));
}

/// Emit `list.map`: for each element in a chunked list, call a funcref and collect into a
/// new chunked list of the same structure.
///
/// Stack before: `[..., list_tagged: i64, func_tagged: i64]`
/// Stack after:  `[..., result_list_tagged: i64]`
fn emit_list_map(
    function: &mut Function,
    hs: &HeapEmitState,
    indirect_call_type_idx: u32,
    closure_call_type_idx: u32,
) -> Result<(), CodegenError> {
    let pw = hs.pw();
    let s_func = hs.scratch.i64_base;
    let s_elem = hs.scratch.i64_base + 1;
    let s_header_ptr = hs.scratch.i32_base + HS_LIST_PTR;
    let s_total_len = hs.scratch.i32_base + HS_ITEM_COUNT;
    let s_n_chunks = hs.scratch.i32_base + HS_TEXT_LEN;
    let s_chunk_idx = hs.scratch.i32_base + HS_ITER;
    let s_src_chunk = hs.scratch.i32_base + HS_TEXT_PTR;
    let s_dst_chunk = hs.scratch.i32_base + HS_AUX_PTR;
    let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE;
    let s_result_ptr = hs.scratch.i32_base + HS_SEP_PTR;
    let s_chunk_len = hs.scratch.i32_base + HS_SEP_LEN;
    let s_item_iter = hs.scratch.i32_base + HS_SCAN_POS;

    // Stack: [list_tagged, func_tagged]
    function.instruction(&Instruction::LocalSet(s_func));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_header_ptr));

    // Load total_len and n_chunks from source header
    function.instruction(&Instruction::LocalGet(s_header_ptr));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_total_len));
    function.instruction(&Instruction::LocalGet(s_header_ptr));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_n_chunks_offset_pw(pw) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_n_chunks));

    // Allocate result header (same n_chunks as source)
    // alloc_size = header_alloc_size_pw(pw, n_chunks) = 8 + n_chunks * 4
    function.instruction(&ptr_const(pw, header_alloc_size_pw(pw, 0) as u64));
    function.instruction(&Instruction::LocalGet(s_n_chunks));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, hs, s_alloc_size, s_result_ptr);

    // Write result header.total_len and header.n_chunks
    function.instruction(&Instruction::LocalGet(s_result_ptr));
    function.instruction(&Instruction::LocalGet(s_total_len));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_result_ptr));
    function.instruction(&Instruction::LocalGet(s_n_chunks));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_n_chunks_offset_pw(pw) as u64,
            align: 2,
            memory_index: 0,
        },
    ));

    // chunk_idx = 0
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_chunk_idx));

    // Outer loop: for each chunk
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&Instruction::LocalGet(s_n_chunks));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    // src_chunk = source header[8 + chunk_idx*4]
    function.instruction(&Instruction::LocalGet(s_header_ptr));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_src_chunk));
    // chunk_len = src_chunk[0]
    function.instruction(&Instruction::LocalGet(s_src_chunk));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_chunk_len));

    // Allocate dst chunk (same size as source chunk)
    function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, hs, s_alloc_size, s_dst_chunk);
    // Write dst_chunk.len = chunk_len
    function.instruction(&Instruction::LocalGet(s_dst_chunk));
    function.instruction(&Instruction::LocalGet(s_chunk_len));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    // Store dst_chunk ptr into result header at slot chunk_idx
    function.instruction(&Instruction::LocalGet(s_result_ptr));
    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_dst_chunk));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));

    // item_iter = 0; inner loop over items in chunk
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_item_iter));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_item_iter));
    function.instruction(&Instruction::LocalGet(s_chunk_len));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    // elem = src_chunk[4 + item_iter*8]
    function.instruction(&Instruction::LocalGet(s_src_chunk));
    function.instruction(&Instruction::LocalGet(s_item_iter));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
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
        hs.pw(),
        Some(hs),
    );
    function.instruction(&Instruction::LocalSet(s_elem));
    emit_return_if_runtime_error(function, hs);

    // dst_chunk[4 + item_iter*8] = mapped
    function.instruction(&Instruction::LocalGet(s_dst_chunk));
    function.instruction(&Instruction::LocalGet(s_item_iter));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_elem));
    function.instruction(&Instruction::I64Store(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));

    function.instruction(&Instruction::LocalGet(s_item_iter));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_item_iter));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End); // end inner loop
    function.instruction(&Instruction::End); // end inner block

    function.instruction(&Instruction::LocalGet(s_chunk_idx));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_chunk_idx));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End); // end outer loop
    function.instruction(&Instruction::End); // end outer block

    emit_push_tagged_ptr(function, s_result_ptr, TAG_LIST, pw);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn emit_list_reverse_fold_prepend(
    function: &mut Function,
    ctx: &mut EmitContext,
    hs: &HeapEmitState,
    item_local_idx: u32,
    prefix_element_instrs: &[Vec<WasmBackendInstr>],
    layout: &MemoryLayout,
    named_i64_count: u32,
    helper_i64_scratch_count: u32,
    i32_base: u32,
    static_strings: &StaticStringPool,
    static_heap: &StaticHeapPool,
    options: &EmitOptions,
    function_returns_i64: bool,
) -> Result<(), CodegenError> {
    let pw = hs.pw();
    // Chunked layout: source is chunked, destination is a new chunked list.
    // Iterates source in reverse (index list_len-1 down to 0), producing
    // prefix_element_instrs.len() output elements per source element.
    //
    // Scratch:
    //   HS_LIST_PTR  (8)  = s_src_header
    //   HS_ITEM_COUNT(6)  = s_src_total_len
    //   HS_AUX_PTR   (7)  = s_dst_header
    //   HS_ALLOC_SIZE(9)  = s_alloc_size
    //   HS_ITER      (10) = s_iter          (0..src_total_len, reverse walk)
    //   HS_TEXT_PTR  (0)  = s_output_len
    //   HS_SEG_START (5)  = s_dst_chunk_ptr
    //   HS_SEP_PTR   (2)  = s_source_index  (src absolute index = src_total_len - 1 - s_iter)
    //   HS_SEP_LEN   (3)  = s_dst_n_chunks
    //   HS_SCAN_POS  (4)  = s_output_index  (dst absolute index)
    //   HS_TEXT_LEN  (1)  = s_src_chunk_ptr (temp for source chunk lookup)
    let s_src_header = hs.scratch.i32_base + HS_LIST_PTR;
    let s_src_total_len = hs.scratch.i32_base + HS_ITEM_COUNT;
    let s_dst_header = hs.scratch.i32_base + HS_AUX_PTR;
    let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE;
    let s_iter = hs.scratch.i32_base + HS_ITER;
    let s_output_len = hs.scratch.i32_base + HS_TEXT_PTR;
    let s_dst_chunk_ptr = hs.scratch.i32_base + HS_SEG_START;
    let s_source_index = hs.scratch.i32_base + HS_SEP_PTR;
    let s_dst_n_chunks = hs.scratch.i32_base + HS_SEP_LEN;
    let s_output_index = hs.scratch.i32_base + HS_SCAN_POS;
    let s_src_chunk_ptr = hs.scratch.i32_base + HS_TEXT_LEN;

    let prefix_len = i32::try_from(prefix_element_instrs.len()).map_err(|_| CodegenError {
        message: "gen_lower/emit: fold prepend prefix length does not fit in i32".to_string(),
    })?;

    // Stack: [list_tagged]
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_src_header));

    // src_total_len = src_header[0]
    function.instruction(&Instruction::LocalGet(s_src_header));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_src_total_len));

    // output_len = src_total_len * prefix_len
    function.instruction(&Instruction::LocalGet(s_src_total_len));
    function.instruction(&ptr_const(pw, prefix_len as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&Instruction::LocalSet(s_output_len));

    // dst_n_chunks = max(1, ceil(output_len / CHUNK_SIZE))
    function.instruction(&Instruction::LocalGet(s_output_len));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64 - 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    // clamp: if output_len == 0, set to 1 anyway for header simplicity
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_gt_u(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        ptr_val_type(pw),
    )));
    function.instruction(&Instruction::LocalGet(s_output_len));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64 - 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&Instruction::Else);
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::LocalSet(s_dst_n_chunks));

    // Allocate dst header
    function.instruction(&ptr_const(pw, header_alloc_size_pw(pw, 0) as u64));
    function.instruction(&Instruction::LocalGet(s_dst_n_chunks));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, hs, s_alloc_size, s_dst_header);

    // Allocate all dst chunks
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::LocalGet(s_dst_n_chunks));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&ptr_const(pw, chunk_alloc_size_pw(pw) as u64));
    function.instruction(&Instruction::LocalSet(s_alloc_size));
    emit_alloc_from_top(function, hs, s_alloc_size, s_dst_chunk_ptr);

    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        },
    ));

    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Br(0));
    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Main loop: iter = 0 .. src_total_len  (source read in reverse)
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_output_index));

    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&Instruction::LocalGet(s_src_total_len));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    // source_index = src_total_len - 1 - iter  (reverse walk)
    function.instruction(&Instruction::LocalGet(s_src_total_len));
    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_sub(pw));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_sub(pw));
    function.instruction(&Instruction::LocalSet(s_source_index));

    // Load src element from chunked layout
    // src_ci = source_index / CHUNK_SIZE
    // src_ii = source_index % CHUNK_SIZE
    // src_chunk_ptr = src_header[8 + src_ci*4]
    function.instruction(&Instruction::LocalGet(s_src_header));
    function.instruction(&Instruction::LocalGet(s_source_index));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_div_u(pw));
    function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load(
        pw,
        MemArg {
            offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalSet(s_src_chunk_ptr));
    // elem = src_chunk[4 + src_ii*8]
    function.instruction(&Instruction::LocalGet(s_src_chunk_ptr));
    function.instruction(&Instruction::LocalGet(s_source_index));
    function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
    function.instruction(&ptr_rem_u(pw));
    function.instruction(&ptr_const(pw, 8));
    function.instruction(&ptr_mul(pw));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::I64Load(MemArg {
        offset: chunk_item_offset_pw(pw, 0) as u64,
        align: 3,
        memory_index: 0,
    }));
    function.instruction(&Instruction::LocalSet(item_local_idx));

    // For each prefix element instruction, emit and write to dst
    for elem_instrs in prefix_element_instrs {
        // dst_chunk_ptr = dst_header[8 + (output_index/CHUNK_SIZE)*4]
        function.instruction(&Instruction::LocalGet(s_dst_header));
        function.instruction(&Instruction::LocalGet(s_output_index));
        function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
        function.instruction(&ptr_div_u(pw));
        function.instruction(&ptr_const(pw, ptr_slot_bytes(pw) as u64));
        function.instruction(&ptr_mul(pw));
        function.instruction(&ptr_add(pw));
        function.instruction(&ptr_load(
            pw,
            MemArg {
                offset: header_chunk_ptr_offset_pw(pw, 0) as u64,
                align: 2,
                memory_index: 0,
            },
        ));
        function.instruction(&Instruction::LocalSet(s_dst_chunk_ptr));

        // dst_addr = dst_chunk + 4 + (output_index % CHUNK_SIZE)*8
        function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
        function.instruction(&Instruction::LocalGet(s_output_index));
        function.instruction(&ptr_const(pw, CHUNK_SIZE as u64));
        function.instruction(&ptr_rem_u(pw));
        function.instruction(&ptr_const(pw, 8));
        function.instruction(&ptr_mul(pw));
        function.instruction(&ptr_add(pw));

        emit_instrs(
            function,
            ctx,
            elem_instrs,
            layout,
            named_i64_count,
            helper_i64_scratch_count,
            i32_base,
            static_strings,
            static_heap,
            *options,
            function_returns_i64,
            0,
        )?;

        function.instruction(&Instruction::I64Store(MemArg {
            offset: chunk_item_offset_pw(pw, 0) as u64,
            align: 3,
            memory_index: 0,
        }));

        // dst_chunk[0] += 1
        function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
        function.instruction(&Instruction::LocalGet(s_dst_chunk_ptr));
        function.instruction(&ptr_load(
            pw,
            MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            },
        ));
        function.instruction(&ptr_const(pw, 1));
        function.instruction(&ptr_add(pw));
        function.instruction(&ptr_store(
            pw,
            MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            },
        ));

        function.instruction(&Instruction::LocalGet(s_output_index));
        function.instruction(&ptr_const(pw, 1));
        function.instruction(&ptr_add(pw));
        function.instruction(&Instruction::LocalSet(s_output_index));
    }

    function.instruction(&Instruction::LocalGet(s_iter));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_iter));
    function.instruction(&Instruction::Br(0));

    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    // Write dst header: total_len = output_len, n_chunks = dst_n_chunks
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_output_len));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_total_len_offset() as u64,
            align: 2,
            memory_index: 0,
        },
    ));
    function.instruction(&Instruction::LocalGet(s_dst_header));
    function.instruction(&Instruction::LocalGet(s_dst_n_chunks));
    function.instruction(&ptr_store(
        pw,
        MemArg {
            offset: header_n_chunks_offset_pw(pw) as u64,
            align: 2,
            memory_index: 0,
        },
    ));

    emit_push_tagged_ptr(function, s_dst_header, TAG_LIST, pw);
    Ok(())
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
    static_heap: &StaticHeapPool,
    scratch_state: &Option<EmitScratchState>,
    heap_state: &Option<HeapEmitState>,
    options: EmitOptions,
    function_returns_i64: bool,
    self_tail_loop_depth: u32,
) -> Result<(), CodegenError> {
    use crate::gen_lower::backend_ir::BackendCasePattern;
    let pw = PtrWidth::from_memory64(options.memory_config.memory64);

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
                    static_heap,
                    options,
                    function_returns_i64,
                    self_tail_loop_depth + 1,
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
                    static_heap,
                    options,
                    function_returns_i64,
                    self_tail_loop_depth + 2,
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
                    static_heap,
                    options,
                    function_returns_i64,
                    self_tail_loop_depth + 2,
                )?;
                function.instruction(&Instruction::Br(1));
                function.instruction(&Instruction::End);
            }

            BackendCasePattern::StrLit(pattern_text) => {
                let hs = require_scratch_state(*scratch_state, "StrLit case pattern")?;
                let s_pat_ptr = hs.i32_base + HS_TEXT_PTR;
                let s_pat_len = hs.i32_base + HS_TEXT_LEN;
                let s_scr_ptr = hs.i32_base + HS_SEP_PTR;
                let s_scr_len = hs.i32_base + HS_SEP_LEN;
                let s_loop_iter = hs.i32_base + HS_SCAN_POS;
                let s_match = hs.i32_base + HS_ITEM_COUNT;

                let pat_ptr = static_strings.ptr(pattern_text)?;
                function.instruction(&ptr_const(pw, pat_ptr as u64));
                function.instruction(&Instruction::LocalSet(s_pat_ptr));
                function.instruction(&ptr_const(pw, pat_ptr as u64));
                // String length is always a 4-byte i32 field regardless of pw.
                function.instruction(&Instruction::I32Load(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                if pw == PtrWidth::W64 {
                    function.instruction(&Instruction::I64ExtendI32U);
                }
                function.instruction(&Instruction::LocalSet(s_pat_len));
                function.instruction(&ptr_const(pw, 0));
                function.instruction(&Instruction::LocalSet(s_match));

                function.instruction(&Instruction::LocalGet(scrutinee_idx));
                function.instruction(&Instruction::I64Const((TAG_STRING as i64) << 60));
                function.instruction(&Instruction::I64And);
                function.instruction(&Instruction::I64Const((TAG_STRING as i64) << 60));
                function.instruction(&Instruction::I64Eq);
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                function.instruction(&Instruction::LocalGet(scrutinee_idx));
                function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
                function.instruction(&Instruction::I64And);
                if let Some(wrap) = ptr_wrap_from_i64(pw) {
                    function.instruction(&wrap);
                }
                function.instruction(&Instruction::LocalSet(s_scr_ptr));
                let pw = PtrWidth::from_memory64(options.memory_config.memory64);
                function.instruction(&Instruction::LocalGet(s_scr_ptr));
                // String length is always a 4-byte i32 field regardless of pw.
                function.instruction(&Instruction::I32Load(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                }));
                if pw == PtrWidth::W64 {
                    function.instruction(&Instruction::I64ExtendI32U);
                }
                function.instruction(&Instruction::LocalSet(s_scr_len));
                emit_compare_decoded_strings(
                    function,
                    s_pat_ptr,
                    s_pat_len,
                    s_scr_ptr,
                    s_scr_len,
                    s_loop_iter,
                    s_match,
                    pw,
                );
                function.instruction(&Instruction::End);
                function.instruction(&Instruction::LocalGet(s_match));
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
                    static_heap,
                    options,
                    function_returns_i64,
                    self_tail_loop_depth + 2,
                )?;
                function.instruction(&Instruction::Br(1));
                function.instruction(&Instruction::End);
            }

            BackendCasePattern::EmptyList => {
                // Emit true iff scrutinee is a List with len == 0.
                // Check: tag bits == TAG_LIST.
                emit_case_list_tag_check(function, scrutinee_idx);
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                // Extract list pointer (one i32 local from helper pool), load len at offset 0.
                // `needs_helper_state` returns true for any function with an EmptyList pattern,
                // so helper_state is always Some here.  The error path is unreachable in practice.
                let s_list_ptr = scratch_state
                    .map(|hs| hs.i32_base + HS_TEXT_PTR)
                    .ok_or_else(|| CodegenError {
                        message: "gen_lower/emit: EmptyList pattern requires helper state (internal error: needs_helper_state should have set this up)".to_string(),
                    })?;
                let s_list_len = scratch_state
                    .map(|hs| hs.i32_base + HS_TEXT_LEN)
                    .ok_or_else(|| CodegenError {
                        message: "gen_lower/emit: EmptyList pattern requires helper state (internal error: needs_helper_state should have set this up)".to_string(),
                    })?;
                emit_decode_list_header_ptr(function, scrutinee_idx, s_list_ptr, pw);
                emit_load_list_total_len(function, s_list_ptr, s_list_len, pw);
                function.instruction(&Instruction::LocalGet(s_list_len));
                function.instruction(&ptr_eqz(pw));
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
                    static_heap,
                    options,
                    function_returns_i64,
                    self_tail_loop_depth + 3,
                )?;
                function.instruction(&Instruction::Br(2)); // out of: if(len==0) + if(is list) + block
                function.instruction(&Instruction::End); // end if (len == 0)
                function.instruction(&Instruction::End); // end if (is list)
            }

            BackendCasePattern::ListPattern { items, tail } => {
                use crate::gen_lower::backend_ir::BackendListPatternItem;
                let hs = require_heap_state(*heap_state, "ListPattern")?;
                // Chunked Sequence layout scratch slots:
                let s_list_ptr = hs.scratch.i32_base + HS_TEXT_PTR; // source header ptr
                let s_list_len = hs.scratch.i32_base + HS_TEXT_LEN; // source total_len
                let s_alloc_size = hs.scratch.i32_base + HS_ALLOC_SIZE; // alloc size temp
                let s_iter = hs.scratch.i32_base + HS_ITER; // loop counter
                let s_tail_ptr = hs.scratch.i32_base + HS_AUX_PTR; // new tail header ptr
                // Extra slots for chunk-aware element loading:
                let s_chunk_idx = hs.scratch.i32_base + HS_SEP_PTR; // chunk_idx = i / CHUNK_SIZE
                let s_item_idx = hs.scratch.i32_base + HS_SEP_LEN; // item_idx  = i % CHUNK_SIZE
                let s_chunk_ptr = hs.scratch.i32_base + HS_SCAN_POS; // chunk_ptr (for load)
                // Extra slot for tail building: current tail chunk ptr
                let s_tail_chunk_ptr = hs.scratch.i32_base + HS_SEG_START;
                let s_tail_chunk_len = hs.scratch.i32_base + HS_ITEM_COUNT;
                // Secondary list scratch slot (free in this branch) for dynamic item loads.
                let s_load_chunk_ptr = hs.scratch.i32_base + HS_LIST_PTR;

                let n_items = items.len() as i32;

                // Check tag == TAG_LIST
                emit_case_list_tag_check(function, scrutinee_idx);
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

                // Decode header ptr, load total_len
                emit_decode_list_header_ptr(function, scrutinee_idx, s_list_ptr, pw);
                emit_load_list_total_len(function, s_list_ptr, s_list_len, pw);

                // Length check: exact (no tail) or >= n_items (with tail)
                function.instruction(&Instruction::LocalGet(s_list_len));
                function.instruction(&ptr_const(pw, n_items as u64));
                if tail.is_some() {
                    function.instruction(&ptr_ge_u(pw));
                } else {
                    function.instruction(&ptr_eq(pw));
                }
                function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

                // Item matching: for each item at logical index i, load via chunk-aware indexing
                for (i, item) in items.iter().enumerate() {
                    let logical_i = i as u32;

                    match item {
                        BackendListPatternItem::Bind(name) => {
                            let name_local = ctx.get(name)?;
                            let pw = PtrWidth::from_memory64(options.memory_config.memory64);
                            emit_chunked_load_const(function, s_list_ptr, logical_i, pw);
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
                            let pw = PtrWidth::from_memory64(options.memory_config.memory64);
                            emit_chunked_load_const(function, s_list_ptr, logical_i, pw);
                            function.instruction(&Instruction::I64Const(encoded));
                            function.instruction(&Instruction::I64Ne);
                            function.instruction(&Instruction::BrIf(0)); // exit if_len
                        }
                        BackendListPatternItem::StrLit(_) => {
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

                // Tail binding: build a new chunked list for elements [n_items..total_len].
                if let Some(tail_name) = tail {
                    let tail_local = ctx.get(tail_name)?;
                    emit_case_bind_tail_list(
                        function,
                        &hs,
                        tail_local,
                        s_list_ptr,
                        s_list_len,
                        s_alloc_size,
                        s_iter,
                        s_tail_ptr,
                        s_chunk_idx,
                        s_item_idx,
                        s_chunk_ptr,
                        s_tail_chunk_ptr,
                        s_tail_chunk_len,
                        s_load_chunk_ptr,
                        n_items,
                    );
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
                    static_heap,
                    options,
                    function_returns_i64,
                    self_tail_loop_depth + 3,
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

fn emit_memory_exhaustion_abort(function: &mut Function, helper_state: &HeapEmitState) {
    let pw = helper_state.pw();
    // GLOBAL_RUNTIME_ERROR_OFFSET is always an i32 (4-byte) field.
    function.instruction(&ptr_const(pw, GLOBAL_RUNTIME_ERROR_OFFSET as u64));
    function.instruction(&Instruction::I32Const(
        RUNTIME_ERROR_MEMORY_EXHAUSTION as i32,
    ));
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    if helper_state.scratch.function_returns_i64 {
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
    pw: PtrWidth,
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
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_target));

    function.instruction(&Instruction::LocalGet(text_local_idx));
    function.instruction(&Instruction::I64Const(0xFFFF_FFFFi64));
    function.instruction(&Instruction::I64And);
    if let Some(wrap) = ptr_wrap_from_i64(pw) {
        function.instruction(&wrap);
    }
    function.instruction(&Instruction::LocalSet(s_str_ptr));

    function.instruction(&Instruction::LocalGet(s_str_ptr));
    // String length is always a 4-byte i32 field regardless of pw.
    function.instruction(&Instruction::I32Load(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I64ExtendI32U);
    }
    function.instruction(&Instruction::LocalSet(s_str_len));

    function.instruction(&Instruction::LocalGet(s_str_len));
    function.instruction(&ptr_eqz(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    emit_abort(function);
    function.instruction(&Instruction::End);

    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&Instruction::LocalSet(s_line_start));

    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));

    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&Instruction::LocalGet(s_str_len));
    function.instruction(&ptr_ge_u(pw));
    function.instruction(&Instruction::BrIf(1));

    function.instruction(&Instruction::LocalGet(s_str_ptr));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&ptr_add(pw));
    function.instruction(&ptr_load_8u(
        pw,
        MemArg {
            offset: 4,
            align: 0,
            memory_index: 0,
        },
    ));
    function.instruction(&ptr_const(pw, sep_byte as u64));
    function.instruction(&ptr_eq(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

    function.instruction(&Instruction::LocalGet(s_target));
    function.instruction(&ptr_eqz(pw));
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
        pw,
    );
    function.instruction(&ptr_neg_one(pw));
    function.instruction(&Instruction::LocalSet(s_target));
    function.instruction(&Instruction::Br(3));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(s_target));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_sub(pw));
    function.instruction(&Instruction::LocalSet(s_target));
    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_line_start));
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_pos));
    function.instruction(&ptr_const(pw, 1));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalSet(s_pos));
    function.instruction(&Instruction::Br(0));

    function.instruction(&Instruction::End);
    function.instruction(&Instruction::End);

    function.instruction(&Instruction::LocalGet(s_target));
    function.instruction(&ptr_const(pw, 0));
    function.instruction(&ptr_lt_s(pw));
    function.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    function.instruction(&Instruction::Else);
    function.instruction(&Instruction::LocalGet(s_target));
    function.instruction(&ptr_eqz(pw));
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
        pw,
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
    pw: PtrWidth,
) {
    // iovec[0] = str_ptr + 4 + line_start  (data pointer for this slice)
    function.instruction(&ptr_const(pw, iovec_offset as u64));
    function.instruction(&Instruction::LocalGet(s_str_ptr));
    function.instruction(&ptr_const(pw, 4));
    function.instruction(&ptr_add(pw));
    function.instruction(&Instruction::LocalGet(s_line_start));
    function.instruction(&ptr_add(pw));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I32WrapI64);
    }
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));

    // iovec[1] = end - line_start  (length of this slice)
    function.instruction(&ptr_const(pw, (iovec_offset + 4) as u64));
    function.instruction(&Instruction::LocalGet(s_end));
    function.instruction(&Instruction::LocalGet(s_line_start));
    function.instruction(&ptr_sub(pw));
    if pw == PtrWidth::W64 {
        function.instruction(&Instruction::I32WrapI64);
    }
    function.instruction(&Instruction::I32Store(MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    }));

    // fd_write(1, iovec_offset, 1, nread_offset) — WASI args are always i32
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Const(iovec_offset));
    function.instruction(&Instruction::I32Const(1));
    function.instruction(&Instruction::I32Const(nread_offset));
    function.instruction(&Instruction::Call(FD_WRITE_IDX));
    function.instruction(&Instruction::Drop);

    if append_newline {
        // Write newline byte (pre-populated in data section at newline_ptr).
        function.instruction(&ptr_const(pw, iovec_offset as u64));
        function.instruction(&Instruction::I32Const(newline_ptr));
        function.instruction(&Instruction::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        function.instruction(&ptr_const(pw, (iovec_offset + 4) as u64));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Store(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        // fd_write(1, iovec_offset, 1, nread_offset) — WASI args are always i32
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Const(iovec_offset));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Const(nread_offset));
        function.instruction(&Instruction::Call(FD_WRITE_IDX));
        function.instruction(&Instruction::Drop);
    }
}

/// Emit `__test_alloc_list_1chunk`: allocates a 1-chunk list (header + chunk),
/// sets refcount to 1, and returns a TAG_LIST-tagged header pointer.
/// Only emitted when `expose_perceus_test_exports` is set; W64 only.
fn emit_test_alloc_list_1chunk(code: &mut CodeSection, layout: &MemoryLayout, pw: PtrWidth) {
    // locals: 0 = cursor (i64), 1 = chunk_ptr (i64), 2 = header_ptr (i64)
    let l_cursor: u32 = 0;
    let l_chunk: u32 = 1;
    let l_header: u32 = 2;
    let mut f = Function::new(vec![(3, ValType::I64)]);

    let mem8 = MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    };
    let mem4 = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };

    // Load alloc cursor from linear memory.
    f.instruction(&ptr_const(pw, GLOBAL_HEAP_CURSOR_OFFSET as u64));
    f.instruction(&Instruction::I32Load(mem4));
    if pw == PtrWidth::W64 {
        f.instruction(&Instruction::I64ExtendI32U);
    }
    f.instruction(&Instruction::LocalSet(l_cursor));

    // --- Allocate chunk: cursor -= REFCOUNT_WORD_BYTES + chunk_payload ---
    let chunk_total = REFCOUNT_WORD_BYTES as i64 + chunk_alloc_size_pw(pw) as i64;
    f.instruction(&Instruction::LocalGet(l_cursor));
    f.instruction(&Instruction::I64Const(chunk_total));
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::LocalTee(l_cursor));
    // chunk_ptr = cursor + REFCOUNT_WORD_BYTES  (payload ptr)
    f.instruction(&Instruction::I64Const(REFCOUNT_WORD_BYTES as i64));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalSet(l_chunk));
    // write refcount = 1 at chunk_ptr - REFCOUNT_WORD_BYTES
    f.instruction(&Instruction::LocalGet(l_chunk));
    f.instruction(&Instruction::I64Const(-(REFCOUNT_WORD_BYTES as i64)));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Store(mem8));
    // write chunk.len = 0 (empty chunk; no items to drop)
    f.instruction(&Instruction::LocalGet(l_chunk));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64Store(mem8));

    // --- Allocate header(1): cursor -= REFCOUNT_WORD_BYTES + header_payload ---
    let header_payload = header_alloc_size_pw(pw, 1) as i64;
    let header_total = REFCOUNT_WORD_BYTES as i64 + header_payload;
    f.instruction(&Instruction::LocalGet(l_cursor));
    f.instruction(&Instruction::I64Const(header_total));
    f.instruction(&Instruction::I64Sub);
    f.instruction(&Instruction::LocalTee(l_cursor));
    // header_ptr = cursor + REFCOUNT_WORD_BYTES
    f.instruction(&Instruction::I64Const(REFCOUNT_WORD_BYTES as i64));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalSet(l_header));
    // write refcount = 1 at header_ptr - REFCOUNT_WORD_BYTES
    f.instruction(&Instruction::LocalGet(l_header));
    f.instruction(&Instruction::I64Const(-(REFCOUNT_WORD_BYTES as i64)));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Store(mem8));
    // write header.total_len = 0 (offset 0)
    f.instruction(&Instruction::LocalGet(l_header));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64Store(mem8));
    // write header.n_chunks = 1 (offset 8)
    f.instruction(&Instruction::LocalGet(l_header));
    f.instruction(&Instruction::I64Const(8));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Store(mem8));
    // write header.chunk_ptr[0] = chunk_ptr (offset 16)
    f.instruction(&Instruction::LocalGet(l_header));
    f.instruction(&Instruction::I64Const(16));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalGet(l_chunk));
    f.instruction(&Instruction::I64Store(mem8));

    // --- Persist alloc cursor back to linear memory ---
    f.instruction(&ptr_const(pw, GLOBAL_HEAP_CURSOR_OFFSET as u64));
    f.instruction(&Instruction::LocalGet(l_cursor));
    if pw == PtrWidth::W64 {
        f.instruction(&Instruction::I32WrapI64);
    }
    f.instruction(&Instruction::I32Store(mem4));

    // --- Return tagged header pointer: TAG_LIST tag ---
    let _ = layout; // layout not needed; encode_list_ptr uses pure bit manipulation
    f.instruction(&Instruction::LocalGet(l_header));
    // encode_list_ptr = (TAG_LIST as i64) << 60 | header_ptr
    f.instruction(&Instruction::I64Const((TAG_LIST as i64) << 60));
    f.instruction(&Instruction::I64Or);

    f.instruction(&Instruction::End);
    code.function(&f);
}

/// Emit `__test_drop_reuse_ptr(tagged_ptr: i64) -> i64`: runs `drop_reuse` on the
/// given tagged pointer and returns the resulting reuse token (payload ptr or 0).
/// Only emitted when `expose_perceus_test_exports` is set.
fn emit_test_drop_reuse_ptr(code: &mut CodeSection, goby_drop_func_idx: u32, pw: PtrWidth) {
    // param 0: tagged_ptr (i64)
    // locals: 1=token (i64), 2=scratch0 (i64), 3=scratch1 (i64), 4=scratch2 (i64)
    let token_local: u32 = 1;
    let scratch_base: u32 = 2;
    let mut f = Function::new(vec![(4, ValType::I64)]);

    let ctx = EmitContext {
        locals: Default::default(),
        next_local: 0,
        decl_func_indices: Default::default(),
        decl_returns_wasm_heap: Default::default(),
        decl_uses_heap: Default::default(),
        decl_table_slots: Default::default(),
        goby_dup_func_idx: 0,
        goby_drop_func_idx,
        indirect_call_type_idx_1: None,
        indirect_call_type_idx_2: None,
        indirect_call_type_idx_3: None,
        record_ctor_tags: Default::default(),
        tail_call_mode: None,
    };

    // Push param 0 (tagged_ptr) onto the stack, then call emit_ref_count_drop_reuse.
    f.instruction(&Instruction::LocalGet(0));
    emit_ref_count_drop_reuse(&mut f, token_local, scratch_base, &ctx, pw);

    // Return the token.
    f.instruction(&Instruction::LocalGet(token_local));
    f.instruction(&Instruction::End);
    code.function(&f);
}

/// Emit `__test_drop_ptr`: calls `__goby_drop(tagged_ptr)` — thin wrapper for testing.
fn emit_test_drop_ptr(code: &mut CodeSection, goby_drop_func_idx: u32) {
    // param 0: tagged_ptr (i64)
    let mut f = Function::new(vec![]); // no extra locals
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(goby_drop_func_idx));
    f.instruction(&Instruction::End);
    code.function(&f);
}

/// Emit `__test_reuse_hits`: returns the current value of `GLOBAL_REUSE_HITS_OFFSET`.
/// Only emitted when `expose_perceus_test_exports` is set.
fn emit_test_reuse_hits(code: &mut CodeSection, pw: PtrWidth) {
    let mut f = Function::new(vec![]); // no params, no extra locals; returns i64
    f.instruction(&ptr_const(pw, GLOBAL_REUSE_HITS_OFFSET as u64));
    f.instruction(&Instruction::I64Load(MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }));
    f.instruction(&Instruction::End);
    code.function(&f);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gen_lower::backend_ir::{
        BackendAllocInit, BackendEffectOp, BackendPrintOp, BackendReadOp, SplitIndexOperand,
        WasmBackendInstr as I,
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
                memory_config: TEST_MEMORY_CONFIG,
                debug_alloc_stats: false,
                expose_perceus_test_exports: false,
            },
        )
        .expect("direct emit should succeed");
        let wasmfx = emit_general_module_with_aux_and_options(
            &instrs,
            &[],
            &default_layout(),
            EmitOptions {
                effect_emit_strategy: EffectEmitStrategy::Wb3BWasmFxExperimental,
                memory_config: TEST_MEMORY_CONFIG,
                debug_alloc_stats: false,
                expose_perceus_test_exports: false,
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
    fn emit_refcount_dup_and_drop_helpers_produce_valid_wasm() {
        let instrs = vec![
            I::I64Const(0),
            I::RefCountDup,
            I::I64Const(0),
            I::RefCountDrop,
            I::I64Const(encode_unit()),
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit refcount helper ops should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_refcount_reuse_ops_produce_valid_wasm() {
        let instrs = vec![
            I::DeclareLocal {
                name: "old".to_string(),
            },
            I::DeclareLocal {
                name: "token".to_string(),
            },
            I::TupleLit {
                element_instrs: vec![vec![I::I64Const(encode_int(9).unwrap())]],
            },
            I::StoreLocal {
                name: "old".to_string(),
            },
            I::LoadLocal {
                name: "old".to_string(),
            },
            I::RefCountDropReuse {
                token_local: "token".to_string(),
            },
            I::AllocReuse {
                token_local: "token".to_string(),
                size_class: SizeClass::Tuple(1),
                init: BackendAllocInit::TupleLit {
                    element_instrs: vec![vec![I::I64Const(encode_int(1).unwrap())]],
                },
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit refcount reuse ops should succeed");
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
                    initial_pages: TEST_MEMORY_CONFIG.initial_pages,
                    max_pages: TEST_MEMORY_CONFIG.initial_pages,
                    host_bump_reserved_bytes: TEST_MEMORY_CONFIG.host_bump_reserved_bytes,
                    max_wasm_stack_bytes: TEST_MEMORY_CONFIG.max_wasm_stack_bytes,
                    memory64: false,
                },
                debug_alloc_stats: false,
                expose_perceus_test_exports: false,
            },
        )
        .expect("low-max emit should still produce a module");
        let err = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
            .expect_err("heap growth beyond the configured maximum should fail");
        assert!(
            err.starts_with("runtime error: memory exhausted [E-MEMORY-EXHAUSTION]:"),
            "expected OOM error message, got: {err:?}"
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

    #[test]
    fn emit_list_set_produces_valid_wasm() {
        // Simulate: `list.set([1, 2, 3], 0, 99)` — update index 0 to 99, then print result
        let instrs = vec![
            I::ListLit {
                element_instrs: vec![
                    vec![I::I64Const(crate::gen_lower::value::encode_int(1).unwrap())],
                    vec![I::I64Const(crate::gen_lower::value::encode_int(2).unwrap())],
                    vec![I::I64Const(crate::gen_lower::value::encode_int(3).unwrap())],
                ],
            },
            I::I64Const(crate::gen_lower::value::encode_int(0).unwrap()),
            I::I64Const(crate::gen_lower::value::encode_int(99).unwrap()),
            I::Intrinsic {
                intrinsic: BackendIntrinsic::ListSet,
            },
            I::EffectOp {
                op: BackendEffectOp::Print(BackendPrintOp::Print),
            },
            I::Drop,
        ];
        let wasm = emit_general_module(&instrs, &default_layout())
            .expect("emit ListSet helper chain should succeed");
        assert_valid_wasm(&wasm);
    }

    #[test]
    fn emit_general_module_with_aux_includes_function_names() {
        let instrs = vec![I::I64Const(crate::gen_lower::value::encode_unit())];
        let aux_decls = vec![AuxDecl {
            decl_name: "__rr_named_helper".to_string(),
            param_names: vec!["_unit".to_string()],
            returns_wasm_heap: false,
            instrs: vec![I::I64Const(crate::gen_lower::value::encode_int(1).unwrap())],
            reuse_param_name: None,
        }];
        let wasm = emit_general_module_with_aux(&instrs, &aux_decls, &default_layout())
            .expect("module with aux decl should emit");
        assert!(
            wasm.windows("__rr_named_helper".len())
                .any(|w| w == b"__rr_named_helper"),
            "expected function name section to contain aux decl name"
        );
        assert!(
            wasm.windows("main".len()).any(|w| w == b"main"),
            "expected function name section to contain main"
        );
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
        std::fs::write("/tmp/debug_eq.wasm", &wasm).ok();
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
