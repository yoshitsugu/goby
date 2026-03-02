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
//! - `Stmt::Binding` with value expressions in the native value subset.
//! - `Stmt::Expr(print <value>)` and `<value> |> print`.
//! - Value expressions: `IntLit`, `BoolLit`, `StringLit`, `Var`, `ListLit(IntLit)`,
//!   `BinOp(+|*|==)`, `If`, `Case` (patterns: `IntLit|StringLit|BoolLit|Wildcard`),
//!   `Call` to a direct named declaration whose body is also natively supported.
//!
//! ## Intentional fallback boundaries (Phase A)
//!
//! The following constructs force the entire module onto the fallback (interpreter) path:
//! - [`UnsupportedReason::UsingNotSupported`]: `Stmt::Using` / effect handlers.
//! - [`UnsupportedReason::CallTargetBodyNotNativeSupported`]: lambda / HOF in any
//!   declaration reachable from `main`.
//! - [`UnsupportedReason::UnsupportedValueExpr`]: expression forms not yet lowered
//!   (e.g. `Expr::Lambda`, method calls, record operations).

use goby_core::{Expr, Module, Stmt, types::parse_function_type};

use crate::{
    call::{
        DirectCallTargetError, PrintCallError, extract_direct_print_call_arg, flatten_named_call,
        resolve_direct_call_target,
    },
    lower::declaration_body_supported_for_native,
    support::{is_supported_binop_kind, is_supported_case_pattern, is_supported_list_item_expr},
};

const BUILTIN_PRINT: &str = "print";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnsupportedReason {
    MissingMain,
    MainAnnotationNotUnitToUnit,
    MainParsedBodyUnavailable,
    MainBodyEmpty,
    UsingNotSupported,
    CallCalleeNotDirectName,
    PrintArityNotOne,
    UnsupportedPipelineCallee,
    UnsupportedStatementExpr,
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
            Self::UsingNotSupported => "using_not_supported",
            Self::CallCalleeNotDirectName => "call_callee_not_direct_name",
            Self::PrintArityNotOne => "print_arity_not_one",
            Self::UnsupportedPipelineCallee => "unsupported_pipeline_callee",
            Self::UnsupportedStatementExpr => "unsupported_statement_expr",
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

    let Some(stmts) = main.parsed_body.as_deref() else {
        return Some(UnsupportedReason::MainParsedBodyUnavailable);
    };
    if stmts.is_empty() {
        return Some(UnsupportedReason::MainBodyEmpty);
    }

    for stmt in stmts {
        if let Some(reason) = unsupported_stmt_reason(stmt, module) {
            return Some(reason);
        }
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

fn unsupported_stmt_reason(stmt: &Stmt, module: &Module) -> Option<UnsupportedReason> {
    match stmt {
        Stmt::Binding { value, .. } => unsupported_value_expr_reason(value, module),
        Stmt::Expr(expr) => unsupported_stmt_expr_reason(expr, module),
        Stmt::Using { .. } => Some(UnsupportedReason::UsingNotSupported),
    }
}

fn is_phase2_supported_value_expr(expr: &Expr, module: &Module) -> bool {
    unsupported_value_expr_reason(expr, module).is_none()
}

fn unsupported_stmt_expr_reason(expr: &Expr, module: &Module) -> Option<UnsupportedReason> {
    match expr {
        Expr::Call { .. } => match extract_direct_print_call_arg(expr) {
            Ok(arg) => unsupported_value_expr_reason(arg, module),
            Err(PrintCallError::NotPrint) => unsupported_value_expr_reason(expr, module),
            Err(PrintCallError::ArityNotOne) => Some(UnsupportedReason::PrintArityNotOne),
            Err(PrintCallError::NonDirectCallee) => {
                Some(UnsupportedReason::CallCalleeNotDirectName)
            }
        },
        Expr::Pipeline { value, callee } => {
            if callee != BUILTIN_PRINT {
                return Some(UnsupportedReason::UnsupportedPipelineCallee);
            }
            unsupported_value_expr_reason(value, module)
        }
        _ => Some(UnsupportedReason::UnsupportedStatementExpr),
    }
}

fn unsupported_value_expr_reason(expr: &Expr, module: &Module) -> Option<UnsupportedReason> {
    match expr {
        Expr::StringLit(_) | Expr::IntLit(_) | Expr::BoolLit(_) | Expr::Var(_) => None,
        Expr::ListLit(items) => {
            if items.iter().all(is_supported_list_item_expr) {
                None
            } else {
                Some(UnsupportedReason::UnsupportedListItemExpr)
            }
        }
        Expr::BinOp { op, left, right } => {
            if !is_supported_binop_kind(op) {
                return Some(UnsupportedReason::UnsupportedBinop);
            }
            if let Some(reason) = unsupported_value_expr_reason(left, module) {
                return Some(reason);
            }
            unsupported_value_expr_reason(right, module)
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            if let Some(reason) = unsupported_value_expr_reason(condition, module) {
                return Some(reason);
            }
            if let Some(reason) = unsupported_value_expr_reason(then_expr, module) {
                return Some(reason);
            }
            unsupported_value_expr_reason(else_expr, module)
        }
        Expr::Case { scrutinee, arms } => {
            if let Some(reason) = unsupported_value_expr_reason(scrutinee, module) {
                return Some(reason);
            }
            if arms.is_empty() {
                return Some(UnsupportedReason::CaseHasNoArms);
            }
            for arm in arms {
                if !is_supported_case_pattern(&arm.pattern) {
                    return Some(UnsupportedReason::UnsupportedCasePattern);
                }
                if let Some(reason) = unsupported_value_expr_reason(&arm.body, module) {
                    return Some(reason);
                }
            }
            None
        }
        Expr::Call { .. } => {
            let Some((name, args)) = flatten_named_call(expr) else {
                return Some(UnsupportedReason::CallCalleeNotDirectName);
            };
            if name == BUILTIN_PRINT {
                return Some(UnsupportedReason::PrintNotValueExpr);
            }
            if !args
                .iter()
                .all(|arg| is_phase2_supported_value_expr(arg, module))
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
                    if !declaration_body_supported_for_native(name, module) {
                        return Some(UnsupportedReason::CallTargetBodyNotNativeSupported);
                    }
                    return Some(UnsupportedReason::CallArityMismatch);
                }
            }
            if !declaration_body_supported_for_native(name, module) {
                return Some(UnsupportedReason::CallTargetBodyNotNativeSupported);
            }
            None
        }
        _ => Some(UnsupportedReason::UnsupportedValueExpr),
    }
}
