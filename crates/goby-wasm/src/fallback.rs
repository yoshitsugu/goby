use goby_core::{BinOpKind, Expr, Module, Stmt, types::parse_function_type};

use crate::lower::declaration_body_supported_for_native;

const BUILTIN_PRINT: &str = "print";

pub(crate) fn supports_native_codegen(module: &Module) -> bool {
    let Some(main) = module.declarations.iter().find(|decl| decl.name == "main") else {
        return false;
    };
    if !is_unit_to_unit(main.type_annotation.as_deref()) {
        return false;
    }

    let Some(stmts) = main.parsed_body.as_deref() else {
        return false;
    };
    if stmts.is_empty() {
        return false;
    }

    stmts
        .iter()
        .all(|stmt| is_phase2_supported_stmt(stmt, module))
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

fn is_phase2_supported_stmt(stmt: &Stmt, module: &Module) -> bool {
    match stmt {
        Stmt::Binding { value, .. } => is_phase2_supported_value_expr(value, module),
        Stmt::Expr(expr) => is_phase2_supported_expr(expr, module),
        Stmt::Using { .. } => false,
    }
}

fn is_phase2_supported_expr(expr: &Expr, module: &Module) -> bool {
    match expr {
        Expr::Call { callee, arg } => {
            matches!(callee.as_ref(), Expr::Var(name) if name == BUILTIN_PRINT)
                && is_phase2_supported_value_expr(arg, module)
        }
        _ => false,
    }
}

fn is_phase2_supported_value_expr(expr: &Expr, module: &Module) -> bool {
    match expr {
        Expr::StringLit(_) | Expr::IntLit(_) | Expr::BoolLit(_) | Expr::Var(_) => true,
        Expr::BinOp { op, left, right } => {
            matches!(op, BinOpKind::Add | BinOpKind::Mul | BinOpKind::Eq)
                && is_phase2_supported_value_expr(left, module)
                && is_phase2_supported_value_expr(right, module)
        }
        Expr::Call { callee, arg } => {
            let Expr::Var(name) = callee.as_ref() else {
                return false;
            };
            if name == BUILTIN_PRINT {
                return false;
            }
            if !is_phase2_supported_value_expr(arg, module) {
                return false;
            }
            declaration_body_supported_for_native(name, module)
        }
        _ => false,
    }
}
