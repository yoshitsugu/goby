// Suppress dead_code warnings for Track F stubs — these will be used in F3+.
#![allow(dead_code)]

//! General Wasm lowering — Track F.
//!
//! This module implements the general lowering pipeline described in
//! `doc/wasm_runtime_architecture.md §7`:
//!
//! ```text
//! Goby IR (CompExpr / ValueExpr)
//!   ↓  gen_lower/lower.rs (future)
//! Backend IR (WasmBackendInstr)
//!   ↓  backend.rs / gen_lower/emit.rs (future)
//! wasm_encoder calls
//! ```
//!
//! # Module ownership
//! - `value`: `RtValue` tagged-i64 representation and encode/decode helpers.
//! - `backend_ir`: `WasmBackendInstr` flat instruction set (variants locked in F2).
//!
//! # Import rules
//! This module and its submodules must NOT import from `runtime_io_plan.rs`.
//! They may import from `backend.rs`, `layout.rs`, `planning.rs`, and `goby-core/ir.rs`.

pub(crate) mod backend_ir;
pub(crate) mod value;

/// Entry point for the general Wasm lowering path.
///
/// Accepts a Goby IR declaration and lowers it to a Wasm binary via
/// `WasmBackendInstr` emission. Implementation is pending F3.
pub(crate) struct GeneralLowerer;
