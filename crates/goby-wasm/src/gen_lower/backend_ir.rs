//! Backend IR — variants locked in Track F F2 Step 2.
//!
//! `WasmBackendInstr` is a flat instruction set that sits between Goby IR and
//! `wasm_encoder`. Its purpose is to be independently testable.
//! See `doc/wasm_runtime_architecture.md §3` for the design rationale.

/// A backend instruction in the general Wasm lowering pipeline.
///
/// Variants are defined in F2 Step 2. This is a placeholder.
pub(crate) enum WasmBackendInstr {}
