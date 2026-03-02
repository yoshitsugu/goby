use goby_core::{BinOpKind, CasePattern, Expr, Module, Stmt, types::parse_function_type};

use crate::{call::flatten_named_call, lower::declaration_body_supported_for_native};

const BUILTIN_PRINT: &str = "print";
const REASON_MISSING_MAIN: &str = "missing_main";
const REASON_MAIN_ANNOTATION_NOT_UNIT_TO_UNIT: &str = "main_annotation_not_unit_to_unit";
const REASON_MAIN_PARSED_BODY_UNAVAILABLE: &str = "main_parsed_body_unavailable";
const REASON_MAIN_BODY_EMPTY: &str = "main_body_empty";
const REASON_USING_NOT_SUPPORTED: &str = "using_not_supported";
const REASON_CALL_CALLEE_NOT_DIRECT_NAME: &str = "call_callee_not_direct_name";
const REASON_PRINT_ARITY_NOT_ONE: &str = "print_arity_not_one";
const REASON_UNSUPPORTED_PIPELINE_CALLEE: &str = "unsupported_pipeline_callee";
const REASON_UNSUPPORTED_STATEMENT_EXPR: &str = "unsupported_statement_expr";
const REASON_UNSUPPORTED_LIST_ITEM_EXPR: &str = "unsupported_list_item_expr";
const REASON_UNSUPPORTED_BINOP: &str = "unsupported_binop";
const REASON_CASE_HAS_NO_ARMS: &str = "case_has_no_arms";
const REASON_UNSUPPORTED_CASE_PATTERN: &str = "unsupported_case_pattern";
const REASON_PRINT_NOT_VALUE_EXPR: &str = "print_not_value_expr";
const REASON_UNSUPPORTED_CALL_ARGUMENT: &str = "unsupported_call_argument";
const REASON_CALL_TARGET_NOT_DECLARATION: &str = "call_target_not_declaration";
const REASON_CALL_TARGET_BODY_NOT_NATIVE_SUPPORTED: &str = "call_target_body_not_native_supported";
const REASON_CALL_ARITY_MISMATCH: &str = "call_arity_mismatch";
const REASON_UNSUPPORTED_VALUE_EXPR: &str = "unsupported_value_expr";

pub(crate) fn supports_native_codegen(module: &Module) -> bool {
    native_unsupported_reason(module).is_none()
}

pub(crate) fn native_unsupported_reason(module: &Module) -> Option<&'static str> {
    let Some(main) = module.declarations.iter().find(|decl| decl.name == "main") else {
        return Some(REASON_MISSING_MAIN);
    };
    if !is_unit_to_unit(main.type_annotation.as_deref()) {
        return Some(REASON_MAIN_ANNOTATION_NOT_UNIT_TO_UNIT);
    }

    let Some(stmts) = main.parsed_body.as_deref() else {
        return Some(REASON_MAIN_PARSED_BODY_UNAVAILABLE);
    };
    if stmts.is_empty() {
        return Some(REASON_MAIN_BODY_EMPTY);
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

fn unsupported_stmt_reason(stmt: &Stmt, module: &Module) -> Option<&'static str> {
    match stmt {
        Stmt::Binding { value, .. } => unsupported_value_expr_reason(value, module),
        Stmt::Expr(expr) => unsupported_stmt_expr_reason(expr, module),
        Stmt::Using { .. } => Some(REASON_USING_NOT_SUPPORTED),
    }
}

fn is_phase2_supported_value_expr(expr: &Expr, module: &Module) -> bool {
    unsupported_value_expr_reason(expr, module).is_none()
}

fn unsupported_stmt_expr_reason(expr: &Expr, module: &Module) -> Option<&'static str> {
    match expr {
        Expr::Call { .. } => {
            let Some((name, args)) = flatten_named_call(expr) else {
                return Some(REASON_CALL_CALLEE_NOT_DIRECT_NAME);
            };
            if name != BUILTIN_PRINT {
                return unsupported_value_expr_reason(expr, module);
            }
            if args.len() != 1 {
                return Some(REASON_PRINT_ARITY_NOT_ONE);
            }
            unsupported_value_expr_reason(args[0], module)
        }
        Expr::Pipeline { value, callee } => {
            if callee != BUILTIN_PRINT {
                return Some(REASON_UNSUPPORTED_PIPELINE_CALLEE);
            }
            unsupported_value_expr_reason(value, module)
        }
        _ => Some(REASON_UNSUPPORTED_STATEMENT_EXPR),
    }
}

fn unsupported_value_expr_reason(expr: &Expr, module: &Module) -> Option<&'static str> {
    match expr {
        Expr::StringLit(_) | Expr::IntLit(_) | Expr::BoolLit(_) | Expr::Var(_) => None,
        Expr::ListLit(items) => {
            if items
                .iter()
                .all(|item| matches!(item, Expr::IntLit(_) | Expr::Var(_)))
            {
                None
            } else {
                Some(REASON_UNSUPPORTED_LIST_ITEM_EXPR)
            }
        }
        Expr::BinOp { op, left, right } => {
            if !matches!(op, BinOpKind::Add | BinOpKind::Mul | BinOpKind::Eq) {
                return Some(REASON_UNSUPPORTED_BINOP);
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
                return Some(REASON_CASE_HAS_NO_ARMS);
            }
            for arm in arms {
                if !matches!(
                    arm.pattern,
                    CasePattern::IntLit(_)
                        | CasePattern::StringLit(_)
                        | CasePattern::BoolLit(_)
                        | CasePattern::Wildcard
                ) {
                    return Some(REASON_UNSUPPORTED_CASE_PATTERN);
                }
                if let Some(reason) = unsupported_value_expr_reason(&arm.body, module) {
                    return Some(reason);
                }
            }
            None
        }
        Expr::Call { .. } => {
            let Some((name, args)) = flatten_named_call(expr) else {
                return Some(REASON_CALL_CALLEE_NOT_DIRECT_NAME);
            };
            if name == BUILTIN_PRINT {
                return Some(REASON_PRINT_NOT_VALUE_EXPR);
            }
            if !args
                .iter()
                .all(|arg| is_phase2_supported_value_expr(arg, module))
            {
                return Some(REASON_UNSUPPORTED_CALL_ARGUMENT);
            }
            let Some(decl) = module.declarations.iter().find(|d| d.name == name) else {
                return Some(REASON_CALL_TARGET_NOT_DECLARATION);
            };
            // Prefer reporting unsupported declaration bodies (for lambda/HOF) before
            // generic call-shape mismatches when both are present.
            if !declaration_body_supported_for_native(name, module) {
                return Some(REASON_CALL_TARGET_BODY_NOT_NATIVE_SUPPORTED);
            }
            if decl.params.len() != args.len() {
                return Some(REASON_CALL_ARITY_MISMATCH);
            }
            None
        }
        _ => Some(REASON_UNSUPPORTED_VALUE_EXPR),
    }
}
