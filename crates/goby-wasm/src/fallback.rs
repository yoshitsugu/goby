//! Capability checker for the native Wasm lowering path.
//!
//! [`supports_native_codegen`] returns `true` when every construct reachable from `main`
//! is within the currently supported native subset. When it returns `false`,
//! [`native_unsupported_reason`] / [`native_unsupported_reason_kind`] report the first
//! unsupported construct found, using [`UnsupportedReason`] variants.
//!
//! ## Supported native subset (Phase A)
//!
//! - `main : Unit -> Unit` annotation required.
//! - `Stmt::Binding` / `Stmt::MutBinding` with value expressions in the native value subset,
//!   as long as no assignment is required at runtime.
//! - `Stmt::Expr(print <value>)` and `<value> |> print`.
//! - Value expressions: `IntLit`, `BoolLit`, `StringLit`, `Var`, `ListLit(IntLit)`,
//!   `BinOp(+|-|*|/|%|==|<|>|<=|>=)`, `If`, `Case`
//!   (patterns: `IntLit|StringLit|BoolLit|Wildcard`),
//!   `Call` to a direct named declaration whose body is also natively supported.
//!
//! ## Intentional fallback boundaries (Phase A)
//!
//! The following constructs force the entire module onto the fallback (interpreter) path:
//! - [`UnsupportedReason::CallTargetBodyNotNativeSupported`]: declarations reachable from `main`
//!   that contain forms outside the native subset (for example effect handler bodies).
//! - [`UnsupportedReason::MutableAssignmentNotNativeSupported`]: assignment requires mutable
//!   runtime state that the direct native evaluator does not implement yet.
//! - [`UnsupportedReason::UnsupportedValueExpr`]: expression forms not yet lowered
//!   (e.g. method calls, record operations).

use std::collections::HashSet;

use goby_core::{Expr, Module, Stmt, types::parse_function_type};

use crate::{
    call::{
        DirectCallTargetError, PrintCallError, direct_print_call, extract_direct_print_call_arg,
        flatten_named_call, resolve_direct_call_target,
    },
    planning::build_lowering_plan,
    support::{is_supported_binop_kind, is_supported_case_pattern, is_supported_list_item_expr},
    wasm_exec_plan::decl_exec_plan,
};

const BUILTIN_PRINT: &str = "print";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnsupportedReason {
    MissingMain,
    MainAnnotationNotUnitToUnit,
    MainParsedBodyUnavailable,
    MainBodyEmpty,
    CallCalleeNotDirectName,
    PrintArityNotOne,
    UnsupportedPipelineCallee,
    UnsupportedStatementExpr,
    MutableAssignmentNotNativeSupported,
    UnsupportedListItemExpr,
    UnsupportedBinop,
    CaseHasNoArms,
    UnsupportedCasePattern,
    PrintNotValueExpr,
    UnsupportedCallArgument,
    CallTargetNotDeclaration,
    CallTargetBodyNotNativeSupported,
    CallArityMismatch,
    UnsupportedValueExpr,
}

impl UnsupportedReason {
    pub(crate) const fn as_str(self) -> &'static str {
        match self {
            Self::MissingMain => "missing_main",
            Self::MainAnnotationNotUnitToUnit => "main_annotation_not_unit_to_unit",
            Self::MainParsedBodyUnavailable => "main_parsed_body_unavailable",
            Self::MainBodyEmpty => "main_body_empty",
            Self::CallCalleeNotDirectName => "call_callee_not_direct_name",
            Self::PrintArityNotOne => "print_arity_not_one",
            Self::UnsupportedPipelineCallee => "unsupported_pipeline_callee",
            Self::UnsupportedStatementExpr => "unsupported_statement_expr",
            Self::MutableAssignmentNotNativeSupported => "mutable_assignment_not_native_supported",
            Self::UnsupportedListItemExpr => "unsupported_list_item_expr",
            Self::UnsupportedBinop => "unsupported_binop",
            Self::CaseHasNoArms => "case_has_no_arms",
            Self::UnsupportedCasePattern => "unsupported_case_pattern",
            Self::PrintNotValueExpr => "print_not_value_expr",
            Self::UnsupportedCallArgument => "unsupported_call_argument",
            Self::CallTargetNotDeclaration => "call_target_not_declaration",
            Self::CallTargetBodyNotNativeSupported => "call_target_body_not_native_supported",
            Self::CallArityMismatch => "call_arity_mismatch",
            Self::UnsupportedValueExpr => "unsupported_value_expr",
        }
    }
}

#[cfg(test)]
pub(crate) fn supports_native_codegen(module: &Module) -> bool {
    native_unsupported_reason(module).is_none()
}

pub(crate) fn native_unsupported_reason(module: &Module) -> Option<&'static str> {
    native_unsupported_reason_kind(module).map(UnsupportedReason::as_str)
}

pub(crate) fn native_unsupported_reason_kind(module: &Module) -> Option<UnsupportedReason> {
    let Some(main) = module.declarations.iter().find(|decl| decl.name == "main") else {
        return Some(UnsupportedReason::MissingMain);
    };
    if !is_unit_to_unit(main.type_annotation.as_deref()) {
        return Some(UnsupportedReason::MainAnnotationNotUnitToUnit);
    }

    let Some(runtime_body) = decl_exec_plan(main).runtime else {
        return Some(UnsupportedReason::MainParsedBodyUnavailable);
    };
    let stmts = runtime_body.stmts.as_ref();
    if stmts.is_empty() {
        return Some(UnsupportedReason::MainBodyEmpty);
    }

    for stmt in stmts {
        let mut stack = HashSet::new();
        if let Some(reason) = unsupported_stmt_reason(stmt, module, &mut stack) {
            return Some(reason);
        }
    }

    // Keep capability check aligned with lower::try_emit_native_module direct-style gating.
    let lowering_plan = build_lowering_plan(module);
    if !lowering_plan.is_direct_style("main") {
        return Some(UnsupportedReason::CallTargetBodyNotNativeSupported);
    }

    None
}

fn is_unit_to_unit(type_annotation: Option<&str>) -> bool {
    let Some(annotation) = type_annotation else {
        return false;
    };
    let Some(fn_ty) = parse_function_type(annotation) else {
        return false;
    };
    fn_ty.arguments.as_slice() == ["Unit"] && fn_ty.result == "Unit"
}

fn unsupported_stmt_reason(
    stmt: &Stmt,
    module: &Module,
    stack: &mut HashSet<String>,
) -> Option<UnsupportedReason> {
    match stmt {
        Stmt::Binding { value, .. } => unsupported_value_expr_reason(value, module, stack),
        Stmt::MutBinding { value, .. } => unsupported_value_expr_reason(value, module, stack),
        Stmt::Assign { .. } => Some(UnsupportedReason::MutableAssignmentNotNativeSupported),
        Stmt::Expr(expr, _) => unsupported_stmt_expr_reason(expr, module, stack),
    }
}

fn is_phase2_supported_value_expr(
    expr: &Expr,
    module: &Module,
    stack: &mut HashSet<String>,
) -> bool {
    unsupported_value_expr_reason(expr, module, stack).is_none()
}

fn unsupported_stmt_expr_reason(
    expr: &Expr,
    module: &Module,
    stack: &mut HashSet<String>,
) -> Option<UnsupportedReason> {
    match expr {
        Expr::Call { .. } => match extract_direct_print_call_arg(expr) {
            Ok(arg) => unsupported_value_expr_reason(arg, module, stack),
            Err(PrintCallError::NotPrint) => unsupported_value_expr_reason(expr, module, stack),
            Err(PrintCallError::ArityNotOne) => Some(UnsupportedReason::PrintArityNotOne),
            Err(PrintCallError::NonDirectCallee) => {
                Some(UnsupportedReason::CallCalleeNotDirectName)
            }
        },
        Expr::Pipeline { value, callee } => {
            if callee != BUILTIN_PRINT {
                return Some(UnsupportedReason::UnsupportedPipelineCallee);
            }
            unsupported_value_expr_reason(value, module, stack)
        }
        _ => Some(UnsupportedReason::UnsupportedStatementExpr),
    }
}

fn unsupported_value_expr_reason(
    expr: &Expr,
    module: &Module,
    stack: &mut HashSet<String>,
) -> Option<UnsupportedReason> {
    match expr {
        Expr::StringLit(_) | Expr::IntLit(_) | Expr::BoolLit(_) | Expr::Var { name: _, .. } => None,
        Expr::ListLit { elements, spread } => {
            if spread.is_some() {
                return Some(UnsupportedReason::UnsupportedListItemExpr);
            }
            if elements.iter().all(is_supported_list_item_expr) {
                None
            } else {
                Some(UnsupportedReason::UnsupportedListItemExpr)
            }
        }
        Expr::BinOp { op, left, right } => {
            if !is_supported_binop_kind(op) {
                return Some(UnsupportedReason::UnsupportedBinop);
            }
            if let Some(reason) = unsupported_value_expr_reason(left, module, stack) {
                return Some(reason);
            }
            unsupported_value_expr_reason(right, module, stack)
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            if let Some(reason) = unsupported_value_expr_reason(condition, module, stack) {
                return Some(reason);
            }
            if let Some(reason) = unsupported_value_expr_reason(then_expr, module, stack) {
                return Some(reason);
            }
            unsupported_value_expr_reason(else_expr, module, stack)
        }
        Expr::Case { scrutinee, arms } => {
            if let Some(reason) = unsupported_value_expr_reason(scrutinee, module, stack) {
                return Some(reason);
            }
            if arms.is_empty() {
                return Some(UnsupportedReason::CaseHasNoArms);
            }
            for arm in arms {
                if !is_supported_case_pattern(&arm.pattern) {
                    return Some(UnsupportedReason::UnsupportedCasePattern);
                }
                if let Some(reason) = unsupported_value_expr_reason(&arm.body, module, stack) {
                    return Some(reason);
                }
            }
            None
        }
        Expr::Block(stmts) => {
            if !matches!(stmts.last(), Some(Stmt::Expr(_, _))) {
                return Some(UnsupportedReason::UnsupportedValueExpr);
            }
            for stmt in stmts {
                match stmt {
                    Stmt::Binding { value, .. } | Stmt::MutBinding { value, .. } => {
                        if let Some(reason) = unsupported_value_expr_reason(value, module, stack) {
                            return Some(reason);
                        }
                    }
                    Stmt::Assign { .. } => {
                        return Some(UnsupportedReason::MutableAssignmentNotNativeSupported);
                    }
                    Stmt::Expr(expr, _) => {
                        if let Some(reason) = unsupported_value_expr_reason(expr, module, stack) {
                            return Some(reason);
                        }
                    }
                }
            }
            None
        }
        Expr::Call { .. } => {
            if direct_print_call(expr).is_ok() {
                return Some(UnsupportedReason::PrintNotValueExpr);
            }
            let Some((name, args)) = flatten_named_call(expr) else {
                // Allow callable-value application (`f 1`, where `f` is a local/lambda)
                // when both callee and argument expressions are in the native value subset.
                if let Expr::Call { callee, arg, .. } = expr
                    && is_phase2_supported_value_expr(callee, module, stack)
                    && is_phase2_supported_value_expr(arg, module, stack)
                {
                    return None;
                }
                return Some(UnsupportedReason::CallCalleeNotDirectName);
            };
            if name == BUILTIN_PRINT {
                return Some(UnsupportedReason::PrintNotValueExpr);
            }
            if !args
                .iter()
                .all(|arg| is_phase2_supported_value_expr(arg, module, stack))
            {
                return Some(UnsupportedReason::UnsupportedCallArgument);
            }
            match resolve_direct_call_target(module, name, args.len()) {
                Ok(_) => {}
                Err(DirectCallTargetError::NotDeclaration) => {
                    return Some(UnsupportedReason::CallTargetNotDeclaration);
                }
                Err(DirectCallTargetError::ArityMismatch) => {
                    // Prefer reporting unsupported declaration bodies (for lambda/HOF)
                    // before generic call-shape mismatches when both are present.
                    if !declaration_body_supported_for_native(name, module, stack) {
                        return Some(UnsupportedReason::CallTargetBodyNotNativeSupported);
                    }
                    return Some(UnsupportedReason::CallArityMismatch);
                }
            }
            if !declaration_body_supported_for_native(name, module, stack) {
                return Some(UnsupportedReason::CallTargetBodyNotNativeSupported);
            }
            None
        }
        Expr::Lambda { body, .. } => unsupported_value_expr_reason(body, module, stack),
        _ => Some(UnsupportedReason::UnsupportedValueExpr),
    }
}

fn declaration_body_supported_for_native(
    decl_name: &str,
    module: &Module,
    stack: &mut HashSet<String>,
) -> bool {
    if !stack.insert(decl_name.to_string()) {
        // Cycle in declaration reachability graph: treat as capability-supported and
        // rely on evaluator depth guards to prevent infinite runtime recursion.
        return true;
    }

    let Some(decl) = module.declarations.iter().find(|d| d.name == decl_name) else {
        stack.remove(decl_name);
        return false;
    };
    let Some(runtime_body) = decl_exec_plan(decl).runtime else {
        stack.remove(decl_name);
        return false;
    };
    let stmts = runtime_body.stmts.as_ref();

    let supported = stmts
        .iter()
        .all(|stmt| is_decl_stmt_supported(stmt, module, stack));
    stack.remove(decl_name);
    supported
}

fn is_decl_stmt_supported(stmt: &Stmt, module: &Module, stack: &mut HashSet<String>) -> bool {
    match stmt {
        Stmt::Binding { value, .. } => is_decl_value_expr_supported(value, module, stack),
        Stmt::MutBinding { .. } | Stmt::Assign { .. } => false,
        Stmt::Expr(expr, _) => match expr {
            Expr::Call { callee, arg, .. } if matches!(callee.as_ref(), Expr::Var { name: n, .. } if n == BUILTIN_PRINT) => {
                is_decl_value_expr_supported(arg, module, stack)
            }
            _ => is_decl_value_expr_supported(expr, module, stack),
        },
    }
}

fn is_decl_value_expr_supported(expr: &Expr, module: &Module, stack: &mut HashSet<String>) -> bool {
    match expr {
        Expr::StringLit(_) | Expr::IntLit(_) | Expr::BoolLit(_) | Expr::Var { name: _, .. } => true,
        Expr::ListLit { elements, spread } => {
            spread.is_none() && elements.iter().all(is_supported_list_item_expr)
        }
        Expr::BinOp { op, left, right } => {
            is_supported_binop_kind(op)
                && is_decl_value_expr_supported(left, module, stack)
                && is_decl_value_expr_supported(right, module, stack)
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            is_decl_value_expr_supported(condition, module, stack)
                && is_decl_value_expr_supported(then_expr, module, stack)
                && is_decl_value_expr_supported(else_expr, module, stack)
        }
        Expr::Case { scrutinee, arms } => {
            is_decl_value_expr_supported(scrutinee, module, stack)
                && !arms.is_empty()
                && arms.iter().all(|arm| {
                    is_supported_case_pattern(&arm.pattern)
                        && is_decl_value_expr_supported(&arm.body, module, stack)
                })
        }
        Expr::Block(stmts) => {
            !stmts.is_empty()
                && matches!(stmts.last(), Some(Stmt::Expr(_, _)))
                && stmts.iter().all(|stmt| match stmt {
                    Stmt::Binding { value, .. } => {
                        is_decl_value_expr_supported(value, module, stack)
                    }
                    Stmt::MutBinding { .. } | Stmt::Assign { .. } => false,
                    Stmt::Expr(expr, _) => is_decl_value_expr_supported(expr, module, stack),
                })
        }
        Expr::Call { callee, arg, .. } => {
            if matches!(callee.as_ref(), Expr::Var { name, .. } if name == BUILTIN_PRINT) {
                return false;
            }
            if let Some((name, args)) = flatten_named_call(expr)
                && let Ok(decl) = resolve_direct_call_target(module, name, args.len())
            {
                return decl.name != "main"
                    && args
                        .iter()
                        .all(|arg| is_decl_value_expr_supported(arg, module, stack))
                    && declaration_body_supported_for_native(name, module, stack);
            }
            is_decl_value_expr_supported(callee, module, stack)
                && is_decl_value_expr_supported(arg, module, stack)
        }
        Expr::Lambda { body, .. } => is_decl_value_expr_supported(body, module, stack),
        _ => false,
    }
}
