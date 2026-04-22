use std::cell::Cell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::rc::Rc;

use crate::ir::{
    CompExpr, IrCasePattern, IrHandlerClause, IrListPatternItem, IrListPatternTail, ValueExpr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MutableStorageId(u32);

impl MutableStorageId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn index(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureKind {
    Immutable,
    MutableRead,
    MutableWrite,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallableEnvSlotKind {
    ByValue,
    SharedMutableCell { storage_id: MutableStorageId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallableEnvSlot {
    pub name: String,
    pub capture_kind: CaptureKind,
    pub slot_kind: CallableEnvSlotKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CallableEnv {
    pub slots: Vec<CallableEnvSlot>,
}

impl CallableEnv {
    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    /// Return the zero-based slot index for `name`, or `None` if not captured.
    pub fn slot_index_of(&self, name: &str) -> Option<usize> {
        self.slots.iter().position(|s| s.name == name)
    }
}

#[derive(Debug, Clone)]
pub struct ClosureBindingEnv {
    bindings: HashMap<String, BindingInfo>,
    next_storage_id: Rc<Cell<u32>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BindingOrigin {
    Outer,
    Local,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BindingInfo {
    Immutable {
        origin: BindingOrigin,
    },
    Mutable {
        origin: BindingOrigin,
        storage_id: MutableStorageId,
    },
}

impl Default for ClosureBindingEnv {
    fn default() -> Self {
        Self {
            bindings: HashMap::new(),
            next_storage_id: Rc::new(Cell::new(0)),
        }
    }
}

impl ClosureBindingEnv {
    pub fn with_decl_params<'a>(params: impl IntoIterator<Item = &'a str>) -> Self {
        let mut env = Self::default();
        for param in params {
            env.bind_outer_immutable(param.to_string());
        }
        env
    }

    pub fn bind_outer_immutable(&mut self, name: String) {
        self.bindings.insert(
            name,
            BindingInfo::Immutable {
                origin: BindingOrigin::Outer,
            },
        );
    }

    pub fn bind_outer_mutable(&mut self, name: String) -> MutableStorageId {
        let storage_id = self.allocate_storage_id();
        self.bindings.insert(
            name,
            BindingInfo::Mutable {
                origin: BindingOrigin::Outer,
                storage_id,
            },
        );
        storage_id
    }

    fn bind_local_immutable(&mut self, name: String) {
        self.bindings.insert(
            name,
            BindingInfo::Immutable {
                origin: BindingOrigin::Local,
            },
        );
    }

    fn bind_local_mutable(&mut self, name: String) -> MutableStorageId {
        let storage_id = self.allocate_storage_id();
        self.bindings.insert(
            name,
            BindingInfo::Mutable {
                origin: BindingOrigin::Local,
                storage_id,
            },
        );
        storage_id
    }

    fn allocate_storage_id(&self) -> MutableStorageId {
        let next = self.next_storage_id.get();
        self.next_storage_id.set(next + 1);
        MutableStorageId(next)
    }

    fn get(&self, name: &str) -> Option<BindingInfo> {
        self.bindings.get(name).copied()
    }

    pub fn is_bound(&self, name: &str) -> bool {
        self.bindings.contains_key(name)
    }
}

pub fn analyze_lambda_callable_env(
    param: &str,
    body: &CompExpr,
    outer_bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
) -> CallableEnv {
    analyze_lambda_callable_env_for_params(&[param.to_string()], body, outer_bindings, known_decls)
}

pub fn analyze_lambda_callable_env_for_params(
    params: &[String],
    body: &CompExpr,
    outer_bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
) -> CallableEnv {
    let mut bindings = outer_bindings.clone();
    for param in params {
        bindings.bind_local_immutable(param.clone());
    }
    let mut captures: BTreeMap<String, CallableEnvSlot> = BTreeMap::new();
    analyze_comp(body, &bindings, known_decls, &mut captures);
    CallableEnv {
        slots: captures.into_values().collect(),
    }
}

pub fn collect_lambda_callable_envs(
    comp: &CompExpr,
    decl_params: &[String],
    known_decls: &HashSet<String>,
) -> Vec<CallableEnv> {
    let mut bindings = ClosureBindingEnv::with_decl_params(decl_params.iter().map(String::as_str));
    let mut envs = Vec::new();
    collect_comp_lambda_envs(comp, &mut bindings, known_decls, &mut envs);
    envs
}

/// The representation the lowering layer should use for a `LetMut` binding.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingRepr {
    /// Plain Wasm local; no heap promotion needed.
    Local,
    /// Heap cell promoted; any nested lambda that accesses this binding shares the cell.
    HeapCell,
}

/// Decide how a `LetMut` binding should be represented at the lowering layer.
///
/// Returns [`BindingRepr::HeapCell`] when any nested lambda captures `name` as a shared
/// mutable cell, and [`BindingRepr::Local`] otherwise.  This hides the write/read
/// distinction behind a single "how should this binding be represented?" answer so that
/// the lowering layer does not need to reason about capture shape directly.
///
/// The caller must supply the `outer_bindings` that are in scope at the `LetMut` site
/// (excluding `name` itself, which is added here as mutable).
pub fn binding_repr_for_let_mut(
    name: &str,
    body: &CompExpr,
    outer_bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
) -> BindingRepr {
    if has_mutable_write_capture_of(name, body, outer_bindings, known_decls) {
        BindingRepr::HeapCell
    } else {
        BindingRepr::Local
    }
}

/// Returns `true` if any lambda nested in `body` captures `name` as a mutable write.
///
/// Prefer [`binding_repr_for_let_mut`] in new code; this function is retained for
/// backward compatibility.
///
/// The caller must supply the `outer_bindings` that are in scope at the `LetMut` site
/// (excluding `name` itself, which is added here as mutable).
pub fn has_mutable_write_capture_of(
    name: &str,
    body: &CompExpr,
    outer_bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
) -> bool {
    let mut bindings = outer_bindings.clone();
    bindings.bind_outer_mutable(name.to_string());
    let mut envs = Vec::new();
    collect_comp_lambda_envs(body, &mut bindings, known_decls, &mut envs);
    envs.iter().any(|env| {
        env.slots.iter().any(|slot| {
            slot.name == name
                && matches!(
                    slot.slot_kind,
                    CallableEnvSlotKind::SharedMutableCell { .. }
                )
        })
    })
}

fn collect_comp_lambda_envs(
    comp: &CompExpr,
    bindings: &mut ClosureBindingEnv,
    known_decls: &HashSet<String>,
    envs: &mut Vec<CallableEnv>,
) {
    match comp {
        CompExpr::Value(value) => collect_value_lambda_envs(value, bindings, known_decls, envs),
        CompExpr::Let {
            name, value, body, ..
        } => {
            collect_comp_lambda_envs(value, bindings, known_decls, envs);
            let mut inner = bindings.clone();
            inner.bind_outer_immutable(name.clone());
            collect_comp_lambda_envs(body, &mut inner, known_decls, envs);
        }
        CompExpr::LetMut {
            name, value, body, ..
        } => {
            collect_comp_lambda_envs(value, bindings, known_decls, envs);
            let mut inner = bindings.clone();
            inner.bind_outer_mutable(name.clone());
            collect_comp_lambda_envs(body, &mut inner, known_decls, envs);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                collect_comp_lambda_envs(stmt, bindings, known_decls, envs);
            }
            collect_comp_lambda_envs(tail, bindings, known_decls, envs);
        }
        CompExpr::If { cond, then_, else_ } => {
            collect_value_lambda_envs(cond, bindings, known_decls, envs);
            collect_comp_lambda_envs(then_, bindings, known_decls, envs);
            collect_comp_lambda_envs(else_, bindings, known_decls, envs);
        }
        CompExpr::Call { callee, args } => {
            collect_value_lambda_envs(callee, bindings, known_decls, envs);
            for arg in args {
                collect_value_lambda_envs(arg, bindings, known_decls, envs);
            }
        }
        CompExpr::Assign { value, .. } => {
            collect_comp_lambda_envs(value, bindings, known_decls, envs);
        }
        CompExpr::AssignIndex { path, value, .. } => {
            for idx in path {
                collect_value_lambda_envs(idx, bindings, known_decls, envs);
            }
            collect_comp_lambda_envs(value, bindings, known_decls, envs);
        }
        CompExpr::Case { scrutinee, arms } => {
            collect_value_lambda_envs(scrutinee, bindings, known_decls, envs);
            for arm in arms {
                let mut arm_bindings = bindings.clone();
                bind_case_pattern(&arm.pattern, &mut arm_bindings);
                collect_comp_lambda_envs(&arm.body, &mut arm_bindings, known_decls, envs);
            }
        }
        CompExpr::Dup { value } | CompExpr::Drop { value } | CompExpr::DropReuse { value, .. } => {
            collect_value_lambda_envs(value, bindings, known_decls, envs);
        }
        CompExpr::AllocReuse { .. } => {}
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                collect_value_lambda_envs(arg, bindings, known_decls, envs);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                collect_handler_clause_lambda_envs(clause, bindings, known_decls, envs);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            collect_comp_lambda_envs(handler, bindings, known_decls, envs);
            collect_comp_lambda_envs(body, bindings, known_decls, envs);
        }
        CompExpr::Resume { value } => {
            collect_value_lambda_envs(value, bindings, known_decls, envs);
        }
    }
}

fn collect_handler_clause_lambda_envs(
    clause: &IrHandlerClause,
    bindings: &mut ClosureBindingEnv,
    known_decls: &HashSet<String>,
    envs: &mut Vec<CallableEnv>,
) {
    let mut clause_bindings = bindings.clone();
    for param in &clause.params {
        clause_bindings.bind_outer_immutable(param.clone());
    }
    collect_comp_lambda_envs(&clause.body, &mut clause_bindings, known_decls, envs);
}

fn collect_value_lambda_envs(
    value: &ValueExpr,
    bindings: &mut ClosureBindingEnv,
    known_decls: &HashSet<String>,
    envs: &mut Vec<CallableEnv>,
) {
    match value {
        ValueExpr::Lambda { param, body } => {
            envs.push(analyze_lambda_callable_env(
                param,
                body,
                bindings,
                known_decls,
            ));
            let mut nested_bindings = bindings.clone();
            nested_bindings.bind_outer_immutable(param.clone());
            collect_comp_lambda_envs(body, &mut nested_bindings, known_decls, envs);
        }
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                collect_value_lambda_envs(element, bindings, known_decls, envs);
            }
            if let Some(spread) = spread {
                collect_value_lambda_envs(spread, bindings, known_decls, envs);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                collect_value_lambda_envs(item, bindings, known_decls, envs);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                collect_value_lambda_envs(field, bindings, known_decls, envs);
            }
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let crate::ir::IrInterpPart::Expr(expr) = part {
                    collect_value_lambda_envs(expr, bindings, known_decls, envs);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            collect_value_lambda_envs(left, bindings, known_decls, envs);
            collect_value_lambda_envs(right, bindings, known_decls, envs);
        }
        ValueExpr::TupleProject { tuple, .. } => {
            collect_value_lambda_envs(tuple, bindings, known_decls, envs);
        }
        ValueExpr::ListGet { list, index } => {
            collect_value_lambda_envs(list, bindings, known_decls, envs);
            collect_value_lambda_envs(index, bindings, known_decls, envs);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

fn analyze_comp(
    comp: &CompExpr,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    captures: &mut BTreeMap<String, CallableEnvSlot>,
) {
    match comp {
        CompExpr::Value(value) => analyze_value(value, bindings, known_decls, captures),
        CompExpr::Let {
            name, value, body, ..
        } => {
            analyze_comp(value, bindings, known_decls, captures);
            let mut inner = bindings.clone();
            inner.bind_local_immutable(name.clone());
            analyze_comp(body, &inner, known_decls, captures);
        }
        CompExpr::LetMut {
            name, value, body, ..
        } => {
            analyze_comp(value, bindings, known_decls, captures);
            let mut inner = bindings.clone();
            inner.bind_local_mutable(name.clone());
            analyze_comp(body, &inner, known_decls, captures);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                analyze_comp(stmt, bindings, known_decls, captures);
            }
            analyze_comp(tail, bindings, known_decls, captures);
        }
        CompExpr::If { cond, then_, else_ } => {
            analyze_value(cond, bindings, known_decls, captures);
            analyze_comp(then_, bindings, known_decls, captures);
            analyze_comp(else_, bindings, known_decls, captures);
        }
        CompExpr::Call { callee, args } => {
            analyze_value(callee, bindings, known_decls, captures);
            for arg in args {
                analyze_value(arg, bindings, known_decls, captures);
            }
        }
        CompExpr::Assign { name, value } => {
            record_mutable_write_capture(name, bindings, known_decls, captures);
            analyze_comp(value, bindings, known_decls, captures);
        }
        CompExpr::AssignIndex { root, path, value } => {
            record_mutable_write_capture(root, bindings, known_decls, captures);
            for idx in path {
                analyze_value(idx, bindings, known_decls, captures);
            }
            analyze_comp(value, bindings, known_decls, captures);
        }
        CompExpr::Case { scrutinee, arms } => {
            analyze_value(scrutinee, bindings, known_decls, captures);
            for arm in arms {
                let mut arm_bindings = bindings.clone();
                bind_case_pattern(&arm.pattern, &mut arm_bindings);
                analyze_comp(&arm.body, &arm_bindings, known_decls, captures);
            }
        }
        CompExpr::Dup { value } | CompExpr::Drop { value } | CompExpr::DropReuse { value, .. } => {
            analyze_value(value, bindings, known_decls, captures);
        }
        CompExpr::AllocReuse { .. } => {}
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                analyze_value(arg, bindings, known_decls, captures);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                let mut clause_bindings = bindings.clone();
                for param in &clause.params {
                    clause_bindings.bind_local_immutable(param.clone());
                }
                analyze_comp(&clause.body, &clause_bindings, known_decls, captures);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            analyze_comp(handler, bindings, known_decls, captures);
            analyze_comp(body, bindings, known_decls, captures);
        }
        CompExpr::Resume { value } => {
            analyze_value(value, bindings, known_decls, captures);
        }
    }
}

fn analyze_value(
    value: &ValueExpr,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    captures: &mut BTreeMap<String, CallableEnvSlot>,
) {
    match value {
        ValueExpr::Var(name) => record_read_capture(name, bindings, known_decls, captures),
        ValueExpr::Lambda { .. } => {}
        ValueExpr::BinOp { left, right, .. } => {
            analyze_value(left, bindings, known_decls, captures);
            analyze_value(right, bindings, known_decls, captures);
        }
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                analyze_value(element, bindings, known_decls, captures);
            }
            if let Some(spread) = spread {
                analyze_value(spread, bindings, known_decls, captures);
            }
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                analyze_value(item, bindings, known_decls, captures);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, field) in fields {
                analyze_value(field, bindings, known_decls, captures);
            }
        }
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let crate::ir::IrInterpPart::Expr(expr) = part {
                    analyze_value(expr, bindings, known_decls, captures);
                }
            }
        }
        ValueExpr::TupleProject { tuple, .. } => {
            analyze_value(tuple, bindings, known_decls, captures);
        }
        ValueExpr::ListGet { list, index } => {
            analyze_value(list, bindings, known_decls, captures);
            analyze_value(index, bindings, known_decls, captures);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

fn record_read_capture(
    name: &str,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    captures: &mut BTreeMap<String, CallableEnvSlot>,
) {
    if known_decls.contains(name) {
        return;
    }
    match bindings.get(name) {
        Some(BindingInfo::Immutable {
            origin: BindingOrigin::Outer,
        }) => {
            upsert_capture(
                captures,
                name,
                CaptureKind::Immutable,
                CallableEnvSlotKind::ByValue,
            );
        }
        Some(BindingInfo::Mutable {
            origin: BindingOrigin::Outer,
            storage_id,
        }) => {
            upsert_capture(
                captures,
                name,
                CaptureKind::MutableRead,
                CallableEnvSlotKind::SharedMutableCell { storage_id },
            );
        }
        Some(BindingInfo::Immutable {
            origin: BindingOrigin::Local,
        })
        | Some(BindingInfo::Mutable {
            origin: BindingOrigin::Local,
            ..
        }) => {}
        None => {
            upsert_capture(
                captures,
                name,
                CaptureKind::Immutable,
                CallableEnvSlotKind::ByValue,
            );
        }
    }
}

fn record_mutable_write_capture(
    name: &str,
    bindings: &ClosureBindingEnv,
    known_decls: &HashSet<String>,
    captures: &mut BTreeMap<String, CallableEnvSlot>,
) {
    if known_decls.contains(name) {
        return;
    }
    match bindings.get(name) {
        Some(BindingInfo::Mutable {
            origin: BindingOrigin::Outer,
            storage_id,
        }) => {
            upsert_capture(
                captures,
                name,
                CaptureKind::MutableWrite,
                CallableEnvSlotKind::SharedMutableCell { storage_id },
            );
        }
        Some(BindingInfo::Immutable {
            origin: BindingOrigin::Outer,
        }) => {
            upsert_capture(
                captures,
                name,
                CaptureKind::Immutable,
                CallableEnvSlotKind::ByValue,
            );
        }
        Some(BindingInfo::Immutable {
            origin: BindingOrigin::Local,
        })
        | Some(BindingInfo::Mutable {
            origin: BindingOrigin::Local,
            ..
        }) => {}
        None => {
            upsert_capture(
                captures,
                name,
                CaptureKind::Immutable,
                CallableEnvSlotKind::ByValue,
            );
        }
    }
}

fn upsert_capture(
    captures: &mut BTreeMap<String, CallableEnvSlot>,
    name: &str,
    capture_kind: CaptureKind,
    slot_kind: CallableEnvSlotKind,
) {
    match captures.get_mut(name) {
        Some(existing) => {
            if capture_priority(capture_kind) > capture_priority(existing.capture_kind) {
                existing.capture_kind = capture_kind;
                // Also update slot_kind so it stays consistent with the capture_kind.
                // A name that is first read as Immutable/ByValue may later be upgraded
                // to MutableWrite/SharedMutableCell; the slot_kind must follow.
                existing.slot_kind = slot_kind;
            }
        }
        None => {
            captures.insert(
                name.to_string(),
                CallableEnvSlot {
                    name: name.to_string(),
                    capture_kind,
                    slot_kind,
                },
            );
        }
    }
}

fn capture_priority(kind: CaptureKind) -> u8 {
    match kind {
        CaptureKind::Immutable => 0,
        CaptureKind::MutableRead => 1,
        CaptureKind::MutableWrite => 2,
    }
}

fn bind_case_pattern(pattern: &IrCasePattern, bindings: &mut ClosureBindingEnv) {
    if let IrCasePattern::ListPattern { items, tail } = pattern {
        for item in items {
            if let IrListPatternItem::Bind(name) = item {
                bindings.bind_outer_immutable(name.clone());
            }
        }
        if let Some(IrListPatternTail::Bind(name)) = tail {
            bindings.bind_outer_immutable(name.clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir_lower::lower_declaration;
    use crate::{Declaration, Expr, Stmt};

    fn lambda_from_body(stmts: Vec<Stmt>) -> ValueExpr {
        let decl = Declaration {
            name: "main".to_string(),
            params: vec![],
            type_annotation: None,
            body: String::new(),
            parsed_body: Some(stmts),
            line: 1,
            col: 1,
        };
        let ir = lower_declaration(&decl).expect("declaration should lower");
        match ir.body {
            CompExpr::Let { body, .. } => match *body {
                CompExpr::Value(lambda @ ValueExpr::Lambda { .. }) => lambda,
                other => panic!("expected lambda body, got {other:?}"),
            },
            CompExpr::Value(lambda @ ValueExpr::Lambda { .. }) => lambda,
            other => panic!("expected lambda at top level, got {other:?}"),
        }
    }

    #[test]
    fn slot_index_of_empty_env_returns_none() {
        let env = CallableEnv::default();
        assert_eq!(env.slot_index_of("x"), None);
    }

    #[test]
    fn slot_index_of_by_value_slot() {
        let env = CallableEnv {
            slots: vec![CallableEnvSlot {
                name: "base".to_string(),
                capture_kind: CaptureKind::Immutable,
                slot_kind: CallableEnvSlotKind::ByValue,
            }],
        };
        assert_eq!(env.slot_index_of("base"), Some(0));
        assert_eq!(env.slot_index_of("other"), None);
    }

    #[test]
    fn slot_index_of_shared_mutable_cell_slot() {
        let env = CallableEnv {
            slots: vec![
                CallableEnvSlot {
                    name: "x".to_string(),
                    capture_kind: CaptureKind::Immutable,
                    slot_kind: CallableEnvSlotKind::ByValue,
                },
                CallableEnvSlot {
                    name: "count".to_string(),
                    capture_kind: CaptureKind::MutableWrite,
                    slot_kind: CallableEnvSlotKind::SharedMutableCell {
                        storage_id: MutableStorageId(0),
                    },
                },
            ],
        };
        assert_eq!(env.slot_index_of("x"), Some(0));
        assert_eq!(env.slot_index_of("count"), Some(1));
        assert_eq!(env.slot_index_of("missing"), None);
    }

    #[test]
    fn classifies_read_only_immutable_capture() {
        let lambda = lambda_from_body(vec![
            Stmt::Binding {
                name: "base".to_string(),
                value: Expr::IntLit(10),
                span: None,
            },
            Stmt::Expr(
                Expr::Lambda {
                    param: "x".to_string(),
                    body: Box::new(Expr::BinOp {
                        op: crate::BinOpKind::Add,
                        left: Box::new(Expr::var("base")),
                        right: Box::new(Expr::var("x")),
                    }),
                },
                None,
            ),
        ]);

        let ValueExpr::Lambda { param, body } = lambda else {
            panic!("expected lambda");
        };
        let mut outer = ClosureBindingEnv::default();
        outer.bind_outer_immutable("base".to_string());
        let env = analyze_lambda_callable_env(&param, &body, &outer, &HashSet::new());
        assert_eq!(
            env.slots,
            vec![CallableEnvSlot {
                name: "base".to_string(),
                capture_kind: CaptureKind::Immutable,
                slot_kind: CallableEnvSlotKind::ByValue,
            }]
        );
    }

    #[test]
    fn classifies_mutable_read_capture_and_assigns_shared_storage_id() {
        let comp = CompExpr::LetMut {
            name: "value".to_string(),
            ty: crate::ir::IrType::Unknown,
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(1))),
            body: Box::new(CompExpr::Value(ValueExpr::Lambda {
                param: "_".to_string(),
                body: Box::new(CompExpr::Value(ValueExpr::Var("value".to_string()))),
            })),
        };
        let envs = collect_lambda_callable_envs(&comp, &Vec::new(), &HashSet::new());
        assert_eq!(envs.len(), 1);
        assert_eq!(envs[0].slots.len(), 1);
        assert_eq!(envs[0].slots[0].name, "value");
        assert_eq!(envs[0].slots[0].capture_kind, CaptureKind::MutableRead);
        match envs[0].slots[0].slot_kind {
            CallableEnvSlotKind::SharedMutableCell { storage_id } => {
                assert_eq!(storage_id.index(), 0);
            }
            other => panic!("expected shared mutable cell, got {other:?}"),
        }
    }

    #[test]
    fn classifies_mutable_write_capture() {
        let comp = CompExpr::LetMut {
            name: "total".to_string(),
            ty: crate::ir::IrType::Unknown,
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(0))),
            body: Box::new(CompExpr::Value(ValueExpr::Lambda {
                param: "x".to_string(),
                body: Box::new(CompExpr::Assign {
                    name: "total".to_string(),
                    value: Box::new(CompExpr::Value(ValueExpr::BinOp {
                        op: crate::ir::IrBinOp::Add,
                        left: Box::new(ValueExpr::Var("total".to_string())),
                        right: Box::new(ValueExpr::Var("x".to_string())),
                    })),
                }),
            })),
        };
        let envs = collect_lambda_callable_envs(&comp, &Vec::new(), &HashSet::new());
        assert_eq!(envs.len(), 1);
        assert_eq!(
            envs[0].slots,
            vec![CallableEnvSlot {
                name: "total".to_string(),
                capture_kind: CaptureKind::MutableWrite,
                slot_kind: CallableEnvSlotKind::SharedMutableCell {
                    storage_id: MutableStorageId(0),
                },
            }]
        );
    }

    #[test]
    fn same_outer_mutable_binding_uses_one_storage_id_across_two_closures() {
        let comp = CompExpr::LetMut {
            name: "count".to_string(),
            ty: crate::ir::IrType::Unknown,
            value: Box::new(CompExpr::Value(ValueExpr::IntLit(0))),
            body: Box::new(CompExpr::Let {
                name: "inc".to_string(),
                ty: crate::ir::IrType::Unknown,
                value: Box::new(CompExpr::Value(ValueExpr::Lambda {
                    param: "_".to_string(),
                    body: Box::new(CompExpr::Assign {
                        name: "count".to_string(),
                        value: Box::new(CompExpr::Value(ValueExpr::BinOp {
                            op: crate::ir::IrBinOp::Add,
                            left: Box::new(ValueExpr::Var("count".to_string())),
                            right: Box::new(ValueExpr::IntLit(1)),
                        })),
                    }),
                })),
                body: Box::new(CompExpr::Value(ValueExpr::Lambda {
                    param: "_".to_string(),
                    body: Box::new(CompExpr::Value(ValueExpr::Var("count".to_string()))),
                })),
            }),
        };
        let envs = collect_lambda_callable_envs(&comp, &Vec::new(), &HashSet::new());
        assert_eq!(envs.len(), 2);
        let storage_ids: Vec<u32> = envs
            .iter()
            .map(|env| match env.slots[0].slot_kind {
                CallableEnvSlotKind::SharedMutableCell { storage_id } => storage_id.index(),
                other => panic!("expected shared cell, got {other:?}"),
            })
            .collect();
        assert_eq!(storage_ids, vec![0, 0]);
        assert_eq!(envs[0].slots[0].capture_kind, CaptureKind::MutableWrite);
        assert_eq!(envs[1].slots[0].capture_kind, CaptureKind::MutableRead);
    }

    #[test]
    fn section3_acceptance_shapes_are_classified() {
        let make_adder = CompExpr::Value(ValueExpr::Lambda {
            param: "x".to_string(),
            body: Box::new(CompExpr::Value(ValueExpr::BinOp {
                op: crate::ir::IrBinOp::Add,
                left: Box::new(ValueExpr::Var("base".to_string())),
                right: Box::new(ValueExpr::Var("x".to_string())),
            })),
        });
        let sum_with_bias = CompExpr::Call {
            callee: Box::new(ValueExpr::GlobalRef {
                module: "list".to_string(),
                name: "fold".to_string(),
            }),
            args: vec![
                ValueExpr::Var("xs".to_string()),
                ValueExpr::IntLit(0),
                ValueExpr::Lambda {
                    param: "acc".to_string(),
                    body: Box::new(CompExpr::Value(ValueExpr::Lambda {
                        param: "x".to_string(),
                        body: Box::new(CompExpr::Value(ValueExpr::BinOp {
                            op: crate::ir::IrBinOp::Add,
                            left: Box::new(ValueExpr::BinOp {
                                op: crate::ir::IrBinOp::Add,
                                left: Box::new(ValueExpr::Var("acc".to_string())),
                                right: Box::new(ValueExpr::Var("x".to_string())),
                            }),
                            right: Box::new(ValueExpr::Var("bias".to_string())),
                        })),
                    })),
                },
            ],
        };
        let envs: Vec<CallableEnv> = [
            collect_lambda_callable_envs(&make_adder, &["base".to_string()], &HashSet::new()),
            collect_lambda_callable_envs(
                &sum_with_bias,
                &["bias".to_string(), "xs".to_string()],
                &HashSet::new(),
            ),
        ]
        .into_iter()
        .flatten()
        .collect();
        assert_eq!(envs.len(), 3);
        assert_eq!(envs[0].slots[0].name, "base");
        assert_eq!(envs[0].slots[0].capture_kind, CaptureKind::Immutable);
        assert!(
            envs[1].is_empty(),
            "outer curried lambda should be zero-capture"
        );
        assert_eq!(envs[2].slots.len(), 2);
        assert_eq!(envs[2].slots[0].name, "acc");
        assert_eq!(envs[2].slots[0].capture_kind, CaptureKind::Immutable);
        assert_eq!(envs[2].slots[1].name, "bias");
        assert_eq!(envs[2].slots[1].capture_kind, CaptureKind::Immutable);
    }
}
