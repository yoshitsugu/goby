use std::collections::HashMap;

use goby_core::{Expr, ImportKind, Module};

use crate::runtime_eval::{is_identifier, is_string_literal};
use crate::runtime_flow::DirectCallHead;

pub(crate) fn flatten_direct_call(expr: &Expr) -> Option<(DirectCallHead, Vec<&Expr>)> {
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
                return Some((DirectCallHead::Bare(name.clone()), args));
            }
            Expr::Qualified { receiver, member } => {
                args.reverse();
                return Some((
                    DirectCallHead::Qualified {
                        receiver: receiver.clone(),
                        member: member.clone(),
                    },
                    args,
                ));
            }
            _ => return None,
        }
    }
}

pub(crate) fn module_has_selective_import_symbol(
    module: &Module,
    module_path: &str,
    symbol: &str,
) -> bool {
    module.imports.iter().any(|import| {
        if import.module_path != module_path {
            return false;
        }
        match &import.kind {
            ImportKind::Selective(selected) => selected.iter().any(|name| name == symbol),
            _ => false,
        }
    })
}

pub(crate) fn parse_pipeline(expr: &str) -> Option<(&str, &str)> {
    let (left, right) = expr.split_once("|>")?;
    let left = left.trim();
    let right = right.trim();
    if left.is_empty() || !is_identifier(right) {
        return None;
    }
    Some((left, right))
}

pub(crate) fn eval_string_expr(expr: &str, locals: &HashMap<String, String>) -> Option<String> {
    let expr = expr.trim();

    if is_string_literal(expr) {
        return Some(expr[1..expr.len() - 1].to_string());
    }

    if is_identifier(expr) {
        return locals.get(expr).cloned();
    }

    None
}
