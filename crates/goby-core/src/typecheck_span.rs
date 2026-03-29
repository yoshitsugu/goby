use crate::ast::{Expr, Span};

/// Return the span stored directly on this expression node, if any.
///
/// Current ER1 audit:
/// - `Expr::Var`, `Expr::Qualified`, and `Expr::Call` can carry direct spans.
/// - parser string-level expression parsing currently leaves those direct spans as `None`.
/// - body statement parsing currently attaches statement spans, not nested identifier spans.
/// - `Expr::Pipeline` stores its callee as a `String`, so its callee token span is not
///   recoverable without an AST/data-model change.
pub(crate) fn direct_expr_span(expr: &Expr) -> Option<Span> {
    match expr {
        Expr::Var { span, .. } | Expr::Qualified { span, .. } | Expr::Call { span, .. } => *span,
        _ => None,
    }
}

/// Return the narrowest currently-available name-like span for this expression.
///
/// This helper is intentionally conservative:
/// - variables and qualified names use their direct node spans,
/// - curried calls recurse into the callee and fall back to the outer call span,
/// - pipeline callees return `None` today because the AST does not store a callee span.
pub(crate) fn best_available_name_use_span(expr: &Expr) -> Option<Span> {
    match expr {
        Expr::Var { span, .. } | Expr::Qualified { span, .. } => *span,
        Expr::Call { callee, span, .. } => best_available_name_use_span(callee).or(*span),
        Expr::Pipeline { .. } => None,
        _ => direct_expr_span(expr),
    }
}

/// Return the best currently-available expression span without inventing locations.
///
/// For ER1 this remains a thin wrapper over the direct-node span plus call-head fallback.
/// Broader statement-level fallbacks should stay with the caller so future work can decide
/// explicitly when a whole-statement highlight is preferable to `span: None`.
pub(crate) fn best_available_expr_span(expr: &Expr) -> Option<Span> {
    direct_expr_span(expr).or_else(|| best_available_name_use_span(expr))
}

#[cfg(test)]
mod tests {
    use crate::{
        Expr, Span, Stmt, parse_body_stmts,
        typecheck_span::{
            best_available_expr_span, best_available_name_use_span, direct_expr_span,
        },
    };

    #[test]
    fn direct_expr_span_returns_direct_var_span() {
        let expr = Expr::Var {
            name: "map".to_string(),
            span: Some(Span::new(3, 7, 3, 10)),
        };

        assert_eq!(direct_expr_span(&expr), Some(Span::new(3, 7, 3, 10)));
        assert_eq!(
            best_available_name_use_span(&expr),
            Some(Span::new(3, 7, 3, 10))
        );
        assert_eq!(
            best_available_expr_span(&expr),
            Some(Span::new(3, 7, 3, 10))
        );
    }

    #[test]
    fn best_available_name_use_span_prefers_callee_span_in_curried_call() {
        let expr = Expr::Call {
            callee: Box::new(Expr::Var {
                name: "map".to_string(),
                span: Some(Span::new(4, 11, 4, 14)),
            }),
            arg: Box::new(Expr::Var {
                name: "xs".to_string(),
                span: Some(Span::new(4, 15, 4, 17)),
            }),
            span: Some(Span::new(4, 11, 4, 17)),
        };

        assert_eq!(
            best_available_name_use_span(&expr),
            Some(Span::new(4, 11, 4, 14))
        );
    }

    #[test]
    fn best_available_name_use_span_falls_back_to_outer_call_span() {
        let expr = Expr::Call {
            callee: Box::new(Expr::Var {
                name: "map".to_string(),
                span: None,
            }),
            arg: Box::new(Expr::Var {
                name: "xs".to_string(),
                span: None,
            }),
            span: Some(Span::new(5, 3, 5, 9)),
        };

        assert_eq!(
            best_available_name_use_span(&expr),
            Some(Span::new(5, 3, 5, 9))
        );
        assert_eq!(best_available_expr_span(&expr), Some(Span::new(5, 3, 5, 9)));
    }

    #[test]
    fn best_available_name_use_span_returns_none_for_pipeline_callee_today() {
        let expr = Expr::Pipeline {
            value: Box::new(Expr::Var {
                name: "xs".to_string(),
                span: Some(Span::new(6, 3, 6, 4)),
            }),
            callee: "map".to_string(),
            callee_span: None,
        };

        assert_eq!(best_available_name_use_span(&expr), None);
        assert_eq!(best_available_expr_span(&expr), None);
    }

    #[test]
    fn parse_body_stmts_populates_nested_var_span_for_expr_stmt_call_head() {
        let stmts = parse_body_stmts("map xs").expect("should parse");

        match &stmts[0] {
            Stmt::Expr(
                Expr::Call {
                    callee,
                    span: Some(call_span),
                    ..
                },
                Some(stmt_span),
            ) => {
                assert_eq!(*stmt_span, Span::point(1, 1));
                assert_eq!(*call_span, Span::new(1, 1, 1, 7));
                assert!(matches!(
                    callee.as_ref(),
                    Expr::Var {
                        name,
                        span: Some(span)
                    } if name == "map" && *span == Span::new(1, 1, 1, 4)
                ));
            }
            other => panic!(
                "expected expr stmt with call and stmt span, got {:?}",
                other
            ),
        }
    }

    #[test]
    fn parse_body_stmts_populates_nested_qualified_span_for_expr_stmt_call_head() {
        let stmts = parse_body_stmts("list.map xs").expect("should parse");

        match &stmts[0] {
            Stmt::Expr(
                Expr::Call {
                    callee,
                    span: Some(call_span),
                    ..
                },
                Some(stmt_span),
            ) => {
                assert_eq!(*stmt_span, Span::point(1, 1));
                assert_eq!(*call_span, Span::new(1, 1, 1, 12));
                assert!(matches!(
                    callee.as_ref(),
                    Expr::Qualified {
                        receiver,
                        member,
                        span: Some(span)
                    } if receiver == "list"
                        && member == "map"
                        && *span == Span::new(1, 1, 1, 9)
                ));
            }
            other => panic!(
                "expected expr stmt with qualified call and stmt span, got {:?}",
                other
            ),
        }
    }
}
