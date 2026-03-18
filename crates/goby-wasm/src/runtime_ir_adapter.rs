use std::borrow::Cow;

use goby_core::ast::{BinOpKind, HandlerClause, InterpolatedPart};
use goby_core::ir::{CompExpr, IrBinOp, IrHandlerClause, ValueExpr};
use goby_core::{Declaration, Expr, Module, Stmt};

pub(crate) struct RuntimeBodyArtifacts<'a> {
    pub(crate) body: Option<Cow<'a, str>>,
    pub(crate) stmts: Cow<'a, [Stmt]>,
}

pub(crate) fn runtime_main_body_artifacts(module: &Module) -> Option<RuntimeBodyArtifacts<'_>> {
    let decl = module
        .declarations
        .iter()
        .find(|decl| decl.name == "main")?;
    runtime_body_artifacts_from_decl(decl)
}

pub(crate) fn runtime_body_artifacts_from_decl(
    decl: &Declaration,
) -> Option<RuntimeBodyArtifacts<'_>> {
    if let Ok(ir_decl) = goby_core::ir_lower::lower_declaration(decl) {
        let stmts = comp_to_stmts(&ir_decl.body)?;
        let body = stmts_to_body_source(&stmts).map(Cow::Owned);
        return Some(RuntimeBodyArtifacts {
            body,
            stmts: Cow::Owned(stmts),
        });
    }

    let stmts = decl.parsed_body.as_deref()?;
    Some(RuntimeBodyArtifacts {
        body: Some(Cow::Borrowed(decl.body.as_str())),
        stmts: Cow::Borrowed(stmts),
    })
}

fn comp_to_stmts(comp: &CompExpr) -> Option<Vec<Stmt>> {
    match comp {
        CompExpr::Let {
            name, value, body, ..
        } => {
            let mut stmts = vec![Stmt::Binding {
                name: name.clone(),
                value: comp_to_expr(value)?,
                span: None,
            }];
            stmts.extend(comp_to_stmts(body)?);
            Some(stmts)
        }
        CompExpr::Seq { stmts, tail } => {
            let mut out = Vec::new();
            for stmt in stmts {
                out.extend(comp_to_stmts(stmt)?);
            }
            out.extend(comp_to_stmts(tail)?);
            Some(out)
        }
        other => Some(vec![Stmt::Expr(comp_to_expr(other)?, None)]),
    }
}

fn comp_to_expr(comp: &CompExpr) -> Option<Expr> {
    match comp {
        CompExpr::Value(value) => value_to_expr(value),
        CompExpr::Let { .. } | CompExpr::Seq { .. } => Some(Expr::Block(comp_to_stmts(comp)?)),
        CompExpr::If { cond, then_, else_ } => Some(Expr::If {
            condition: Box::new(value_to_expr(cond)?),
            then_expr: Box::new(comp_to_expr(then_)?),
            else_expr: Box::new(comp_to_expr(else_)?),
        }),
        CompExpr::Call { callee, args } => {
            let mut expr = value_to_expr(callee)?;
            let args = if args.is_empty() {
                vec![Expr::unit_value()]
            } else {
                args.iter().map(value_to_expr).collect::<Option<Vec<_>>>()?
            };
            for arg in args {
                expr = Expr::call(expr, arg);
            }
            Some(expr)
        }
        CompExpr::PerformEffect { effect, op, args } => {
            let mut expr = Expr::qualified(effect.clone(), op.clone());
            let args = if args.is_empty() {
                vec![Expr::unit_value()]
            } else {
                args.iter().map(value_to_expr).collect::<Option<Vec<_>>>()?
            };
            for arg in args {
                expr = Expr::call(expr, arg);
            }
            Some(expr)
        }
        CompExpr::Handle { clauses } => Some(Expr::Handler {
            clauses: clauses
                .iter()
                .map(ir_handler_clause_to_ast)
                .collect::<Option<Vec<_>>>()?,
        }),
        CompExpr::WithHandler { handler, body } => Some(Expr::With {
            handler: Box::new(comp_to_expr(handler)?),
            body: comp_to_stmts(body)?,
        }),
        CompExpr::Resume { value } => Some(Expr::Resume {
            value: Box::new(value_to_expr(value)?),
        }),
    }
}

fn ir_handler_clause_to_ast(clause: &IrHandlerClause) -> Option<HandlerClause> {
    let parsed_body = comp_to_stmts(&clause.body)?;
    let body = stmts_to_body_source(&parsed_body).unwrap_or_else(|| "<ir>".to_string());
    Some(HandlerClause {
        name: clause.op_name.clone(),
        params: clause.params.clone(),
        body,
        parsed_body: Some(parsed_body),
        span: goby_core::Span::point(1, 1),
    })
}

fn value_to_expr(value: &ValueExpr) -> Option<Expr> {
    match value {
        ValueExpr::IntLit(n) => Some(Expr::IntLit(*n)),
        ValueExpr::BoolLit(b) => Some(Expr::BoolLit(*b)),
        ValueExpr::StrLit(text) => Some(Expr::StringLit(text.clone())),
        ValueExpr::Interp(parts) => Some(Expr::InterpolatedString(
            parts
                .iter()
                .map(ir_interp_part_to_ast)
                .collect::<Option<Vec<_>>>()?,
        )),
        ValueExpr::Var(name) => Some(Expr::var(name.clone())),
        ValueExpr::GlobalRef { module, name } => {
            Some(Expr::qualified(module.clone(), name.clone()))
        }
        ValueExpr::BinOp { op, left, right } => Some(Expr::BinOp {
            op: ir_binop_to_ast(op),
            left: Box::new(value_to_expr(left)?),
            right: Box::new(value_to_expr(right)?),
        }),
        ValueExpr::Unit => Some(Expr::unit_value()),
    }
}

fn ir_interp_part_to_ast(part: &goby_core::ir::IrInterpPart) -> Option<InterpolatedPart> {
    match part {
        goby_core::ir::IrInterpPart::Text(text) => Some(InterpolatedPart::Text(text.clone())),
        goby_core::ir::IrInterpPart::Expr(expr) => {
            Some(InterpolatedPart::Expr(Box::new(value_to_expr(expr)?)))
        }
    }
}

fn ir_binop_to_ast(op: &IrBinOp) -> BinOpKind {
    match op {
        IrBinOp::And => BinOpKind::And,
        IrBinOp::Add => BinOpKind::Add,
        IrBinOp::Mul => BinOpKind::Mul,
        IrBinOp::Eq => BinOpKind::Eq,
        IrBinOp::Lt => BinOpKind::Lt,
        IrBinOp::Gt => BinOpKind::Gt,
    }
}

fn stmts_to_body_source(stmts: &[Stmt]) -> Option<String> {
    let mut lines = Vec::with_capacity(stmts.len());
    for stmt in stmts {
        lines.push(stmt_to_source(stmt)?);
    }
    Some(lines.join("\n"))
}

fn stmt_to_source(stmt: &Stmt) -> Option<String> {
    match stmt {
        Stmt::Binding { name, value, .. } => Some(format!("{name} = {}", value.to_str_repr()?)),
        Stmt::MutBinding { name, value, .. } => {
            Some(format!("mut {name} = {}", value.to_str_repr()?))
        }
        Stmt::Assign { name, value, .. } => Some(format!("{name} = {}", value.to_str_repr()?)),
        Stmt::Expr(expr, _) => expr.to_str_repr(),
    }
}

#[cfg(test)]
mod tests {
    use goby_core::parse_module;

    use super::*;

    #[test]
    fn adapts_ir_main_with_effect_handler_to_runtime_stmts() {
        let module = parse_module(
            r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (next 0)
"#,
        )
        .expect("parse should work");
        let adapted = runtime_main_body_artifacts(&module).expect("main should adapt");
        assert!(matches!(adapted.stmts[0], Stmt::Expr(Expr::With { .. }, _)));
    }
}
