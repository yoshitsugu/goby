use crate::Module;
use crate::ast::{
    CaseArm, CasePattern, Declaration, Expr, HandlerClause, InterpolatedPart, Span, Stmt,
};
use crate::diagnostic::{Diagnostic, Severity};

pub fn lint_module(module: &Module) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    for decl in &module.declarations {
        if let Some(body) = decl.parsed_body.as_deref() {
            lint_stmts(body, decl, &mut diagnostics);
        }
    }
    diagnostics
}

fn lint_stmts(stmts: &[Stmt], decl: &Declaration, diagnostics: &mut Vec<Diagnostic>) {
    for stmt in stmts {
        match stmt {
            Stmt::Binding { value, .. }
            | Stmt::MutBinding { value, .. }
            | Stmt::Assign { value, .. }
            | Stmt::Expr(value, _) => lint_expr(value, decl, diagnostics),
        }
    }
}

fn lint_expr(expr: &Expr, decl: &Declaration, diagnostics: &mut Vec<Diagnostic>) {
    match expr {
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) | Expr::Var { .. } => {}
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    lint_expr(expr, decl, diagnostics);
                }
            }
        }
        Expr::ListLit { elements, spread } => {
            for element in elements {
                lint_expr(element, decl, diagnostics);
            }
            if let Some(spread) = spread {
                lint_expr(spread, decl, diagnostics);
            }
        }
        Expr::TupleLit(items) => {
            for item in items {
                lint_expr(item, decl, diagnostics);
            }
        }
        Expr::Qualified { .. } => {}
        Expr::RecordConstruct { fields, .. } => {
            for (_, value) in fields {
                lint_expr(value, decl, diagnostics);
            }
        }
        Expr::UnaryOp { expr, .. } => lint_expr(expr, decl, diagnostics),
        Expr::BinOp { left, right, .. } => {
            lint_expr(left, decl, diagnostics);
            lint_expr(right, decl, diagnostics);
        }
        Expr::Call { callee, arg, .. } => {
            lint_expr(callee, decl, diagnostics);
            lint_expr(arg, decl, diagnostics);
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                lint_expr(arg, decl, diagnostics);
            }
        }
        Expr::Pipeline { value, .. } => lint_expr(value, decl, diagnostics),
        Expr::Lambda { body, .. } => lint_expr(body, decl, diagnostics),
        Expr::Handler { clauses } => lint_handler_clauses(clauses, decl, diagnostics),
        Expr::With { handler, body } => {
            lint_expr(handler, decl, diagnostics);
            lint_stmts(body, decl, diagnostics);
        }
        Expr::Resume { value } => lint_expr(value, decl, diagnostics),
        Expr::Block(stmts) => lint_stmts(stmts, decl, diagnostics),
        Expr::Case { scrutinee, arms } => {
            lint_expr(scrutinee, decl, diagnostics);
            lint_unreachable_case_arms(arms, decl, diagnostics);
            for arm in arms {
                lint_expr(&arm.body, decl, diagnostics);
            }
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            lint_expr(condition, decl, diagnostics);
            lint_expr(then_expr, decl, diagnostics);
            lint_expr(else_expr, decl, diagnostics);
        }
        Expr::ListIndex { list, index } => {
            lint_expr(list, decl, diagnostics);
            lint_expr(index, decl, diagnostics);
        }
    }
}

fn lint_handler_clauses(
    clauses: &[HandlerClause],
    decl: &Declaration,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for clause in clauses {
        if let Some(body) = clause.parsed_body.as_deref() {
            lint_stmts(body, decl, diagnostics);
        }
    }
}

fn lint_unreachable_case_arms(
    arms: &[CaseArm],
    decl: &Declaration,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut wildcard_seen = false;
    for arm in arms {
        if wildcard_seen {
            diagnostics.push(Diagnostic {
                severity: Severity::Warning,
                span: Some(body_span_to_source_span(decl, arm.span)),
                declaration: Some(decl.name.clone()),
                message: "unreachable case arm [unreachable-arm]: wildcard `_` arm must be last"
                    .to_string(),
            });
        }
        if matches!(arm.pattern, CasePattern::Wildcard) {
            wildcard_seen = true;
        }
    }
}

fn body_span_to_source_span(decl: &Declaration, span: Span) -> Span {
    let definition_line = decl.line + usize::from(decl.type_annotation.is_some());
    Span::new(
        definition_line + span.line,
        span.col,
        definition_line + span.end_line,
        span.end_col,
    )
}

#[cfg(test)]
mod tests {
    use super::lint_module;
    use crate::{Severity, Span, parse_module};

    #[test]
    fn lint_module_reports_unreachable_case_arm_after_wildcard() {
        let module = parse_module(
            r#"
f : Int -> Int
f n =
  case n
    _ -> 0
    1 -> 1
"#,
        )
        .expect("source should parse");
        let diagnostics = lint_module(&module);
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert_eq!(diagnostics[0].declaration.as_deref(), Some("f"));
        assert!(
            diagnostics[0]
                .message
                .contains("unreachable case arm [unreachable-arm]"),
            "unexpected lint message: {}",
            diagnostics[0].message
        );
        assert_eq!(diagnostics[0].span, Some(Span::new(7, 5, 7, 5)));
    }

    #[test]
    fn lint_module_accepts_wildcard_as_last_case_arm() {
        let module = parse_module(
            r#"
f : Int -> Int
f n =
  case n
    1 -> 1
    _ -> 0
"#,
        )
        .expect("source should parse");
        let diagnostics = lint_module(&module);
        assert!(
            diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            diagnostics
        );
    }
}
