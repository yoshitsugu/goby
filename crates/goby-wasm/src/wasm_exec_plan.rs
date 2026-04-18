use std::borrow::Cow;

use goby_core::ast::{
    BinOpKind, CaseArm, CasePattern, HandlerClause, InterpolatedPart, ListPatternItem,
    ListPatternTail,
};
use goby_core::ir::{
    CompExpr, IrBinOp, IrCaseArm, IrCasePattern, IrDecl, IrHandlerClause, IrListPatternItem,
    IrListPatternTail, ValueExpr,
};
use goby_core::{Declaration, Expr, Module, Stmt, parse_body_stmts};

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

pub(crate) fn runtime_artifacts_from_ir_decl(
    ir_decl: &IrDecl,
) -> Option<WasmRuntimeArtifacts<'static>> {
    let stmts = comp_to_stmts(&ir_decl.body)?;
    let body = stmts_to_body_source(&stmts).map(Cow::Owned);
    Some(WasmRuntimeArtifacts {
        body,
        stmts: Cow::Owned(stmts),
    })
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

    if let Some(stmts) = parse_body_stmts(&decl.body) {
        return Some(WasmRuntimeArtifacts {
            body: Some(Cow::Borrowed(decl.body.as_str())),
            stmts: Cow::Owned(stmts),
        });
    }

    ir_decl.and_then(runtime_artifacts_from_ir_decl)
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
            target: goby_core::ast::AssignTarget::Var(name.clone()),
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
        CompExpr::AssignIndex { .. } => None,
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
            span: None,
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
        // Tuple projection has no direct AST equivalent; cannot round-trip.
        ValueExpr::TupleProject { .. } => None,
        ValueExpr::ListGet { list, index } => Some(Expr::Call {
            callee: Box::new(Expr::Call {
                callee: Box::new(Expr::qualified("list", "get")),
                arg: Box::new(value_to_expr(list)?),
                span: None,
            }),
            arg: Box::new(value_to_expr(index)?),
            span: None,
        }),
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
        IrBinOp::Or => BinOpKind::Or,
        IrBinOp::And => BinOpKind::And,
        IrBinOp::BitXor => BinOpKind::BitXor,
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
        Stmt::Assign { target, value, .. } => {
            // List-index assignment is not representable in source-string form; opt out.
            let name = match target {
                goby_core::ast::AssignTarget::Var(n) => n,
                goby_core::ast::AssignTarget::ListIndex { .. } => return None,
            };
            Some(format!("{name} = {}", value.to_str_repr()?))
        }
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
            "text = Read.read ()\nlines = string.split text \"\n\"\nPrint.println (list.get lines 1)"
        );
        let stmt_src = match &stmts[2] {
            Stmt::Expr(expr, _) => expr.to_str_repr(),
            other => panic!("expected trailing expr stmt, got {:?}", other),
        };
        assert_eq!(
            stmt_src.as_deref(),
            Some("Print.println (list.get lines 1)")
        );
    }

    #[test]
    fn canonical_ir_bridge_split_each_shape_classifies_with_selective_imports() {
        let module = parse_module(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  each lines Print.println
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
        assert_eq!(
            crate::runtime_io_plan::classify_runtime_io(&module, Some(&stmts)),
            crate::runtime_io_plan::RuntimeIoClassification::DynamicWasiIo(
                crate::runtime_io_plan::RuntimeIoPlan::SplitLinesEach {
                    output_mode: crate::runtime_io_plan::OutputReadMode::Println,
                    suffix_prints: vec![],
                    transform: None,
                }
            )
        );
    }

    #[test]
    fn runtime_artifacts_from_ir_decl_prefers_canonical_bridge_shape() {
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
        let plan = main_exec_plan(&module).expect("main should exist");
        let ir_decl = plan.ir_decl.expect("IR should lower");
        let runtime =
            runtime_artifacts_from_ir_decl(&ir_decl).expect("runtime artifacts should exist");
        let body = runtime.body.expect("body should exist");
        assert_eq!(
            body,
            "text = Read.read ()\nlines = string.split text \"\n\"\nPrint.println (list.get lines 1)"
        );
    }

    #[test]
    fn runtime_artifacts_from_ir_decl_bridge_effectful_case_shape() {
        let module = parse_module(
            r#"
effect Pred
  flag: Int -> Bool
effect Iter
  next: Int -> Int

choose : Int -> Int
choose n =
  case flag n
    True  -> next 5
    False -> next 0
"#,
        )
        .expect("parse should work");
        let decl = module
            .declarations
            .iter()
            .find(|decl| decl.name == "choose")
            .expect("choose should exist");
        let ir_decl =
            goby_core::ir_lower::lower_declaration(decl).expect("IR lowering should work");
        let runtime =
            runtime_artifacts_from_ir_decl(&ir_decl).expect("runtime artifacts should exist");
        assert!(
            runtime.body.is_none(),
            "multi-line case bridge is expected to preserve stmts even when compact body rendering is unavailable"
        );
        assert!(matches!(
            runtime.stmts.as_ref(),
            [
                Stmt::Binding { name, value, .. },
                Stmt::Expr(Expr::Case { .. }, _)
            ] if name == "__goby_ir_case_scrutinee_0"
                && matches!(value, Expr::Call { .. })
        ));
    }

    #[test]
    fn runtime_artifacts_from_ir_decl_bridge_effectful_tuple_shape() {
        let module = parse_module(
            r#"
pair : Unit -> (String, Int) can Read
pair _ =
  (read (), 2)
"#,
        )
        .expect("parse should work");
        let decl = module
            .declarations
            .iter()
            .find(|decl| decl.name == "pair")
            .expect("pair should exist");
        let ir_decl =
            goby_core::ir_lower::lower_declaration(decl).expect("IR lowering should work");
        let runtime =
            runtime_artifacts_from_ir_decl(&ir_decl).expect("runtime artifacts should exist");
        let body = runtime.body.expect("body should render");
        assert_eq!(
            body,
            "__goby_ir_tuple_item_0 = Read.read ()\n(__goby_ir_tuple_item_0, 2)"
        );
    }

    #[test]
    fn runtime_artifacts_from_ir_decl_bridge_lambda_capture_shape() {
        let module = parse_module(
            r#"
capture : Unit -> (Int -> Int)
capture _ =
  base = 40
  fn n -> n + base
"#,
        )
        .expect("parse should work");
        let decl = module
            .declarations
            .iter()
            .find(|decl| decl.name == "capture")
            .expect("capture should exist");
        let ir_decl =
            goby_core::ir_lower::lower_declaration(decl).expect("IR lowering should work");
        let runtime =
            runtime_artifacts_from_ir_decl(&ir_decl).expect("runtime artifacts should exist");
        let body = runtime.body.expect("body should render");
        assert_eq!(body, "base = 40\nfn n -> n + base");
    }

    #[test]
    fn local_decl_runtime_plan_preserves_named_lambda_map_body() {
        let source = std::fs::read_to_string(
            std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("..")
                .join("..")
                .join("examples")
                .join("function.gb"),
        )
        .expect("function example should exist");
        let module = parse_module(&source).expect("function example should parse");
        let decl = module
            .declarations
            .iter()
            .find(|decl| decl.name == "mul_tens")
            .expect("mul_tens should exist");
        assert!(
            decl.parsed_body.is_some(),
            "mul_tens should keep parsed body"
        );
        let runtime = decl_exec_plan(decl)
            .runtime
            .expect("mul_tens runtime plan should exist");
        assert_eq!(
            runtime.body.as_deref().map(str::trim_end),
            Some("list.map ns (fn n -> n * 10)")
        );
    }
}
