use goby_core::{BinOpKind, CasePattern, Expr, Module, Stmt, types::parse_function_type};

use crate::lower::declaration_body_supported_for_native;

const BUILTIN_PRINT: &str = "print";

pub(crate) fn supports_native_codegen(module: &Module) -> bool {
    native_unsupported_reason(module).is_none()
}

pub(crate) fn native_unsupported_reason(module: &Module) -> Option<&'static str> {
    let Some(main) = module.declarations.iter().find(|decl| decl.name == "main") else {
        return Some("missing_main");
    };
    if !is_unit_to_unit(main.type_annotation.as_deref()) {
        return Some("main_annotation_not_unit_to_unit");
    }

    let Some(stmts) = main.parsed_body.as_deref() else {
        return Some("main_parsed_body_unavailable");
    };
    if stmts.is_empty() {
        return Some("main_body_empty");
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

fn is_phase2_supported_expr(expr: &Expr, module: &Module) -> bool {
    match expr {
        Expr::Call { callee, arg } => {
            matches!(callee.as_ref(), Expr::Var(name) if name == BUILTIN_PRINT)
                && is_phase2_supported_value_expr(arg, module)
        }
        Expr::Pipeline { value, callee } => {
            callee == BUILTIN_PRINT && is_phase2_supported_value_expr(value, module)
        }
        _ => false,
    }
}

fn unsupported_stmt_reason(stmt: &Stmt, module: &Module) -> Option<&'static str> {
    match stmt {
        Stmt::Binding { value, .. } => {
            if is_phase2_supported_value_expr(value, module) {
                None
            } else {
                Some("unsupported_binding_expr")
            }
        }
        Stmt::Expr(expr) => {
            if is_phase2_supported_expr(expr, module) {
                None
            } else {
                Some("unsupported_statement_expr")
            }
        }
        Stmt::Using { .. } => Some("using_not_supported"),
    }
}

fn is_phase2_supported_value_expr(expr: &Expr, module: &Module) -> bool {
    match expr {
        Expr::StringLit(_) | Expr::IntLit(_) | Expr::BoolLit(_) | Expr::Var(_) => true,
        Expr::ListLit(items) => items
            .iter()
            .all(|item| matches!(item, Expr::IntLit(_) | Expr::Var(_))),
        Expr::BinOp { op, left, right } => {
            matches!(op, BinOpKind::Add | BinOpKind::Mul | BinOpKind::Eq)
                && is_phase2_supported_value_expr(left, module)
                && is_phase2_supported_value_expr(right, module)
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            is_phase2_supported_value_expr(condition, module)
                && is_phase2_supported_value_expr(then_expr, module)
                && is_phase2_supported_value_expr(else_expr, module)
        }
        Expr::Case { scrutinee, arms } => {
            is_phase2_supported_value_expr(scrutinee, module)
                && !arms.is_empty()
                && arms.iter().all(|arm| {
                    matches!(
                        arm.pattern,
                        CasePattern::IntLit(_)
                            | CasePattern::StringLit(_)
                            | CasePattern::BoolLit(_)
                            | CasePattern::Wildcard
                    ) && is_phase2_supported_value_expr(&arm.body, module)
                })
        }
        Expr::Call { callee, arg } => match callee.as_ref() {
            Expr::Var(name) => {
                if name == BUILTIN_PRINT {
                    return false;
                }
                if !is_phase2_supported_value_expr(arg, module) {
                    return false;
                }
                declaration_body_supported_for_native(name, module)
            }
            _ => false,
        },
        _ => false,
    }
}
