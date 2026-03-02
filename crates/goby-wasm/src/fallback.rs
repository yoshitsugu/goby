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

fn unsupported_stmt_reason(stmt: &Stmt, module: &Module) -> Option<&'static str> {
    match stmt {
        Stmt::Binding { value, .. } => unsupported_value_expr_reason(value, module),
        Stmt::Expr(expr) => unsupported_stmt_expr_reason(expr, module),
        Stmt::Using { .. } => Some("using_not_supported"),
    }
}

fn is_phase2_supported_value_expr(expr: &Expr, module: &Module) -> bool {
    unsupported_value_expr_reason(expr, module).is_none()
}

fn unsupported_stmt_expr_reason(expr: &Expr, module: &Module) -> Option<&'static str> {
    match expr {
        Expr::Call { .. } => {
            let Some((name, args)) = flatten_named_call(expr) else {
                return Some("call_callee_not_direct_name");
            };
            if name != BUILTIN_PRINT {
                return unsupported_value_expr_reason(expr, module);
            }
            if args.len() != 1 {
                return Some("print_arity_not_one");
            }
            unsupported_value_expr_reason(args[0], module)
        }
        Expr::Pipeline { value, callee } => {
            if callee != BUILTIN_PRINT {
                return Some("unsupported_pipeline_callee");
            }
            unsupported_value_expr_reason(value, module)
        }
        _ => Some("unsupported_statement_expr"),
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
                Some("unsupported_list_item_expr")
            }
        }
        Expr::BinOp { op, left, right } => {
            if !matches!(op, BinOpKind::Add | BinOpKind::Mul | BinOpKind::Eq) {
                return Some("unsupported_binop");
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
                return Some("case_has_no_arms");
            }
            for arm in arms {
                if !matches!(
                    arm.pattern,
                    CasePattern::IntLit(_)
                        | CasePattern::StringLit(_)
                        | CasePattern::BoolLit(_)
                        | CasePattern::Wildcard
                ) {
                    return Some("unsupported_case_pattern");
                }
                if let Some(reason) = unsupported_value_expr_reason(&arm.body, module) {
                    return Some(reason);
                }
            }
            None
        }
        Expr::Call { .. } => {
            let Some((name, args)) = flatten_named_call(expr) else {
                return Some("call_callee_not_direct_name");
            };
            if name == BUILTIN_PRINT {
                return Some("print_not_value_expr");
            }
            if !args
                .iter()
                .all(|arg| is_phase2_supported_value_expr(arg, module))
            {
                return Some("unsupported_call_argument");
            }
            let Some(decl) = module.declarations.iter().find(|d| d.name == name) else {
                return Some("call_target_not_declaration");
            };
            if decl.params.len() != args.len() {
                return Some("call_arity_mismatch");
            }
            if declaration_body_supported_for_native(name, module) {
                None
            } else {
                Some("call_target_body_not_native_supported")
            }
        }
        _ => Some("unsupported_value_expr"),
    }
}

fn flatten_named_call(expr: &Expr) -> Option<(&str, Vec<&Expr>)> {
    let mut args = Vec::new();
    let mut cur = expr;
    loop {
        match cur {
            Expr::Call { callee, arg } => {
                args.push(arg.as_ref());
                cur = callee.as_ref();
            }
            Expr::Var(name) => {
                args.reverse();
                return Some((name.as_str(), args));
            }
            _ => return None,
        }
    }
}
