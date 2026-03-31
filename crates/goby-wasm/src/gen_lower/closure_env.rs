//! Closure-environment helper layer for the general Wasm lowering path (CC2).
//!
//! `ClosureEnvHelper` bridges `CallableEnv` (the capture-analysis result from `goby-core`)
//! and the backend IR instructions introduced in CC2.  It produces `Vec<WasmBackendInstr>`
//! sequences for loading and storing closure slots, so the lowering pass (CC3) does not
//! need to open-code slot interpretation repeatedly.
//!
//! # Design
//! - A ByValue slot is loaded as a direct `LoadClosureSlot`.
//! - A SharedMutableCell slot is loaded as `LoadClosureSlot` followed by `LoadCellValue`,
//!   which pops the TAG_CELL pointer from the stack and loads the stored value.
//! - Storing into a SharedMutableCell slot uses `StoreCellValue` with the cell pointer
//!   coming from a `LoadClosureSlot` and the new value from caller-supplied `value_instrs`.

use goby_core::closure_capture::{CallableEnv, CallableEnvSlotKind};

use crate::gen_lower::backend_ir::WasmBackendInstr;

/// Helper for generating closure-environment load and store instruction sequences.
pub(crate) struct ClosureEnvHelper {
    pub env: CallableEnv,
}

impl ClosureEnvHelper {
    pub fn new(env: CallableEnv) -> Self {
        Self { env }
    }

    /// Emit instructions to load a captured slot value onto the stack.
    ///
    /// - ByValue slot: pushes the captured value directly.
    /// - SharedMutableCell slot: loads the cell pointer, then dereferences it to get the value.
    ///
    /// Returns `None` if `slot_name` is not a captured slot in this environment.
    pub fn slot_load_instrs(
        &self,
        closure_local: &str,
        slot_name: &str,
    ) -> Option<Vec<WasmBackendInstr>> {
        let slot_index = self.env.slot_index_of(slot_name)?;
        let slot = &self.env.slots[slot_index];
        let mut instrs = vec![WasmBackendInstr::LoadClosureSlot {
            closure_local: closure_local.to_string(),
            slot_index,
        }];
        if matches!(slot.slot_kind, CallableEnvSlotKind::SharedMutableCell { .. }) {
            instrs.push(WasmBackendInstr::LoadCellValue);
        }
        Some(instrs)
    }

    /// Emit instructions to store `value_instrs` into a SharedMutableCell slot.
    ///
    /// The cell pointer is loaded from the closure record via `LoadClosureSlot`.
    ///
    /// Returns `None` if `slot_name` is not a SharedMutableCell slot in this environment.
    pub fn cell_store_instrs(
        &self,
        closure_local: &str,
        slot_name: &str,
        value_instrs: Vec<WasmBackendInstr>,
    ) -> Option<Vec<WasmBackendInstr>> {
        let slot_index = self.env.slot_index_of(slot_name)?;
        let slot = &self.env.slots[slot_index];
        if !matches!(slot.slot_kind, CallableEnvSlotKind::SharedMutableCell { .. }) {
            return None;
        }
        Some(vec![WasmBackendInstr::StoreCellValue {
            cell_ptr_instrs: vec![WasmBackendInstr::LoadClosureSlot {
                closure_local: closure_local.to_string(),
                slot_index,
            }],
            value_instrs,
        }])
    }
}

#[cfg(test)]
mod tests {
    use goby_core::closure_capture::{
        CallableEnv, CallableEnvSlot, CallableEnvSlotKind, CaptureKind, MutableStorageId,
    };

    use super::*;

    fn by_value_env() -> CallableEnv {
        CallableEnv {
            slots: vec![CallableEnvSlot {
                name: "base".to_string(),
                capture_kind: CaptureKind::Immutable,
                slot_kind: CallableEnvSlotKind::ByValue,
            }],
        }
    }

    fn cell_env() -> CallableEnv {
        CallableEnv {
            slots: vec![CallableEnvSlot {
                name: "count".to_string(),
                capture_kind: CaptureKind::MutableWrite,
                slot_kind: CallableEnvSlotKind::SharedMutableCell {
                    storage_id: MutableStorageId::new(0),
                },
            }],
        }
    }

    #[test]
    fn slot_load_by_value_emits_only_load_closure_slot() {
        let helper = ClosureEnvHelper::new(by_value_env());
        let instrs = helper.slot_load_instrs("clo", "base").unwrap();
        assert_eq!(
            instrs,
            vec![WasmBackendInstr::LoadClosureSlot {
                closure_local: "clo".to_string(),
                slot_index: 0,
            }]
        );
    }

    #[test]
    fn slot_load_cell_emits_load_closure_slot_then_load_cell_value() {
        let helper = ClosureEnvHelper::new(cell_env());
        let instrs = helper.slot_load_instrs("clo", "count").unwrap();
        assert_eq!(
            instrs,
            vec![
                WasmBackendInstr::LoadClosureSlot {
                    closure_local: "clo".to_string(),
                    slot_index: 0,
                },
                WasmBackendInstr::LoadCellValue,
            ]
        );
    }

    #[test]
    fn slot_load_unknown_name_returns_none() {
        let helper = ClosureEnvHelper::new(by_value_env());
        assert!(helper.slot_load_instrs("clo", "missing").is_none());
    }

    #[test]
    fn cell_store_emits_store_cell_value_with_correct_ptr() {
        let helper = ClosureEnvHelper::new(cell_env());
        let value_instrs = vec![WasmBackendInstr::I64Const(42)];
        let instrs = helper
            .cell_store_instrs("clo", "count", value_instrs.clone())
            .unwrap();
        assert_eq!(
            instrs,
            vec![WasmBackendInstr::StoreCellValue {
                cell_ptr_instrs: vec![WasmBackendInstr::LoadClosureSlot {
                    closure_local: "clo".to_string(),
                    slot_index: 0,
                }],
                value_instrs,
            }]
        );
    }

    #[test]
    fn cell_store_on_by_value_slot_returns_none() {
        let helper = ClosureEnvHelper::new(by_value_env());
        assert!(
            helper
                .cell_store_instrs("clo", "base", vec![WasmBackendInstr::I64Const(1)])
                .is_none()
        );
    }

    #[test]
    fn slot_load_empty_env_returns_none() {
        let helper = ClosureEnvHelper::new(CallableEnv::default());
        assert!(helper.slot_load_instrs("clo", "x").is_none());
    }
}
