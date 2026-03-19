use std::borrow::Cow;

use goby_core::ast::{
    BinOpKind, CaseArm, CasePattern, HandlerClause, InterpolatedPart, ListPatternItem,
    ListPatternTail,
};
use goby_core::ir::{
    CompExpr, IrBinOp, IrCaseArm, IrCasePattern, IrDecl, IrHandlerClause, IrListPatternItem,
    IrListPatternTail, ValueExpr,
};
use goby_core::{Declaration, Expr, Module, Stmt};

pub(crate) struct WasmRuntimeArtifacts<'a> {
    pub(crate) body: Option<Cow<'a, str>>,
    pub(crate) stmts: Cow<'a, [Stmt]>,
}

pub(crate) struct WasmDeclExecPlan<'a> {
    pub(crate) ir_decl: Option<IrDecl>,
    pub(crate) runtime: Option<WasmRuntimeArtifacts<'a>>,
}

pub(crate) fn main_exec_plan(module: &Module) -> Option<WasmDeclExecPlan<'_>> {
    let decl = module
        .declarations
        .iter()
        .find(|decl| decl.name == "main")?;
    Some(decl_exec_plan(decl))
}

pub(crate) fn decl_exec_plan(decl: &Declaration) -> WasmDeclExecPlan<'_> {
    let ir_decl = goby_core::ir_lower::lower_declaration(decl).ok();
    let runtime = runtime_artifacts_from_decl_and_ir(decl, ir_decl.as_ref());
    WasmDeclExecPlan { ir_decl, runtime }
}

fn runtime_artifacts_from_decl_and_ir<'a>(
    decl: &'a Declaration,
    ir_decl: Option<&IrDecl>,
) -> Option<WasmRuntimeArtifacts<'a>> {
    if let Some(stmts) = decl.parsed_body.as_deref() {
        return Some(WasmRuntimeArtifacts {
            body: Some(Cow::Borrowed(decl.body.as_str())),
            stmts: Cow::Borrowed(stmts),
        });
    }

    if let Some(ir_decl) = ir_decl {
        let stmts = comp_to_stmts(&ir_decl.body)?;
        let body = stmts_to_body_source(&stmts).map(Cow::Owned);
        return Some(WasmRuntimeArtifacts {
            body,
            stmts: Cow::Owned(stmts),
        });
    }

    None
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
        CompExpr::LetMut {
            name, value, body, ..
        } => {
            let mut stmts = vec![Stmt::MutBinding {
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
        CompExpr::Assign { name, value } => Some(vec![Stmt::Assign {
            name: name.clone(),
            value: comp_to_expr(value)?,
            span: None,
        }]),
        other => Some(vec![Stmt::Expr(comp_to_expr(other)?, None)]),
    }
}

fn comp_to_expr(comp: &CompExpr) -> Option<Expr> {
    match comp {
        CompExpr::Value(value) => value_to_expr(value),
        CompExpr::Let { .. }
        | CompExpr::LetMut { .. }
        | CompExpr::Seq { .. }
        | CompExpr::Assign { .. } => Some(Expr::Block(comp_to_stmts(comp)?)),
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
        CompExpr::Case { scrutinee, arms } => Some(Expr::Case {
            scrutinee: Box::new(value_to_expr(scrutinee)?),
            arms: arms
                .iter()
                .map(ir_case_arm_to_ast)
                .collect::<Option<Vec<_>>>()?,
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
        ValueExpr::ListLit { elements, spread } => Some(Expr::ListLit {
            elements: elements
                .iter()
                .map(value_to_expr)
                .collect::<Option<Vec<_>>>()?,
            spread: match spread {
                Some(tail) => Some(Box::new(value_to_expr(tail)?)),
                None => None,
            },
        }),
        ValueExpr::TupleLit(items) => Some(Expr::TupleLit(
            items
                .iter()
                .map(value_to_expr)
                .collect::<Option<Vec<_>>>()?,
        )),
        ValueExpr::RecordLit {
            constructor,
            fields,
        } => Some(Expr::RecordConstruct {
            constructor: constructor.clone(),
            fields: fields
                .iter()
                .map(|(name, value)| Some((name.clone(), value_to_expr(value)?)))
                .collect::<Option<Vec<_>>>()?,
        }),
        ValueExpr::Lambda { param, body } => Some(Expr::Lambda {
            param: param.clone(),
            body: Box::new(comp_to_expr(body)?),
        }),
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

fn ir_case_arm_to_ast(arm: &IrCaseArm) -> Option<CaseArm> {
    Some(CaseArm {
        pattern: ir_case_pattern_to_ast(&arm.pattern),
        body: Box::new(comp_to_expr(&arm.body)?),
        span: goby_core::Span::point(1, 1),
    })
}

fn ir_case_pattern_to_ast(pattern: &IrCasePattern) -> CasePattern {
    match pattern {
        IrCasePattern::IntLit(n) => CasePattern::IntLit(*n),
        IrCasePattern::StringLit(text) => CasePattern::StringLit(text.clone()),
        IrCasePattern::BoolLit(flag) => CasePattern::BoolLit(*flag),
        IrCasePattern::EmptyList => CasePattern::EmptyList,
        IrCasePattern::ListPattern { items, tail } => CasePattern::ListPattern {
            items: items.iter().map(ir_list_pattern_item_to_ast).collect(),
            tail: tail.as_ref().map(ir_list_pattern_tail_to_ast),
        },
        IrCasePattern::Wildcard => CasePattern::Wildcard,
    }
}

fn ir_list_pattern_item_to_ast(item: &IrListPatternItem) -> ListPatternItem {
    match item {
        IrListPatternItem::IntLit(n) => ListPatternItem::IntLit(*n),
        IrListPatternItem::StringLit(text) => ListPatternItem::StringLit(text.clone()),
        IrListPatternItem::Bind(name) => ListPatternItem::Bind(name.clone()),
        IrListPatternItem::Wildcard => ListPatternItem::Wildcard,
    }
}

fn ir_list_pattern_tail_to_ast(tail: &IrListPatternTail) -> ListPatternTail {
    match tail {
        IrListPatternTail::Ignore => ListPatternTail::Ignore,
        IrListPatternTail::Bind(name) => ListPatternTail::Bind(name.clone()),
    }
}

fn ir_binop_to_ast(op: &IrBinOp) -> BinOpKind {
    match op {
        IrBinOp::And => BinOpKind::And,
        IrBinOp::Add => BinOpKind::Add,
        IrBinOp::Sub => BinOpKind::Sub,
        IrBinOp::Mul => BinOpKind::Mul,
        IrBinOp::Div => BinOpKind::Div,
        IrBinOp::Mod => BinOpKind::Mod,
        IrBinOp::Eq => BinOpKind::Eq,
        IrBinOp::Lt => BinOpKind::Lt,
        IrBinOp::Gt => BinOpKind::Gt,
        IrBinOp::Le => BinOpKind::Le,
        IrBinOp::Ge => BinOpKind::Ge,
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
    fn builds_main_exec_plan_for_handler_body() {
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
        let plan = main_exec_plan(&module).expect("main should exist");
        let runtime = plan.runtime.expect("runtime plan should exist");
        assert!(matches!(runtime.stmts[0], Stmt::Expr(Expr::With { .. }, _)));
    }

    #[test]
    fn ir_bridge_normalizes_collection_shape_for_runtime_artifacts() {
        let module = parse_module(
            r#"
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  println(lines[1])
"#,
        )
        .expect("parse should work");
        let decl = module
            .declarations
            .iter()
            .find(|decl| decl.name == "main")
            .expect("main should exist");
        let ir_decl =
            goby_core::ir_lower::lower_declaration(decl).expect("IR lowering should work");
        let stmts = comp_to_stmts(&ir_decl.body).expect("IR bridge should build stmts");
        let body = stmts_to_body_source(&stmts).expect("IR bridge should render source");
        assert_eq!(
            body,
            "text = Read.read ()\nlines = string.split text \"\n\"\n__goby_ir_effect_arg_0 = list.get lines 1\nPrint.println __goby_ir_effect_arg_0"
        );
        let stmt_src = match &stmts[3] {
            Stmt::Expr(expr, _) => expr.to_str_repr(),
            other => panic!("expected trailing expr stmt, got {:?}", other),
        };
        assert_eq!(
            stmt_src.as_deref(),
            Some("Print.println __goby_ir_effect_arg_0")
        );
    }
}
