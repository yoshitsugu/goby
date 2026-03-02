use goby_core::{Expr, Module, Stmt, types::parse_function_type};

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

    stmts.iter().all(is_phase0_supported_stmt)
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

fn is_phase0_supported_stmt(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Binding { value, .. } => matches!(value, Expr::StringLit(_)),
        Stmt::Expr(expr) => is_phase0_supported_expr(expr),
        Stmt::Using { .. } => false,
    }
}

fn is_phase0_supported_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Call { callee, arg } => {
            matches!(callee.as_ref(), Expr::Var(name) if name == BUILTIN_PRINT)
                && matches!(arg.as_ref(), Expr::StringLit(_) | Expr::Var(_))
        }
        _ => false,
    }
}
