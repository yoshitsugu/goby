use goby_core::{BinOpKind, CasePattern, Expr};

pub(crate) fn peel_expr_spans(mut expr: &Expr) -> &Expr {
    while let Expr::Spanned { expr: inner, .. } = expr {
        expr = inner;
    }
    expr
}

pub(crate) fn is_supported_case_pattern(pattern: &CasePattern) -> bool {
    match pattern {
        CasePattern::IntLit(_)
        | CasePattern::StringLit(_)
        | CasePattern::BoolLit(_)
        | CasePattern::Wildcard => true,
        CasePattern::EmptyList | CasePattern::ListPattern { .. } => false,
    }
}

pub(crate) fn is_supported_list_item_expr(expr: &Expr) -> bool {
    matches!(
        peel_expr_spans(expr),
        Expr::IntLit(_) | Expr::Var { name: _, .. }
    )
}

pub(crate) fn is_supported_binop_kind(op: &BinOpKind) -> bool {
    matches!(
        op,
        BinOpKind::Or
            | BinOpKind::Add
            | BinOpKind::Sub
            | BinOpKind::Mul
            | BinOpKind::Div
            | BinOpKind::Mod
            | BinOpKind::Eq
            | BinOpKind::Lt
            | BinOpKind::Gt
            | BinOpKind::Le
            | BinOpKind::Ge
    )
}

#[cfg(test)]
mod tests {
    use goby_core::{BinOpKind, CasePattern, Expr};

    use super::*;

    #[test]
    fn supports_current_native_case_patterns() {
        assert!(is_supported_case_pattern(&CasePattern::IntLit(1)));
        assert!(is_supported_case_pattern(&CasePattern::StringLit(
            "x".to_string()
        )));
        assert!(is_supported_case_pattern(&CasePattern::BoolLit(true)));
        assert!(is_supported_case_pattern(&CasePattern::Wildcard));
    }

    #[test]
    fn supports_current_native_list_item_expressions() {
        assert!(is_supported_list_item_expr(&Expr::IntLit(1)));
        assert!(is_supported_list_item_expr(&Expr::Var {
            name: "x".to_string(),
            span: None
        }));
        assert!(!is_supported_list_item_expr(&Expr::BoolLit(true)));
    }

    #[test]
    fn supports_current_native_binop_kinds() {
        assert!(is_supported_binop_kind(&BinOpKind::Add));
        assert!(is_supported_binop_kind(&BinOpKind::Or));
        assert!(is_supported_binop_kind(&BinOpKind::Sub));
        assert!(is_supported_binop_kind(&BinOpKind::Mul));
        assert!(is_supported_binop_kind(&BinOpKind::Div));
        assert!(is_supported_binop_kind(&BinOpKind::Mod));
        assert!(is_supported_binop_kind(&BinOpKind::Eq));
        assert!(is_supported_binop_kind(&BinOpKind::Lt));
        assert!(is_supported_binop_kind(&BinOpKind::Gt));
        assert!(is_supported_binop_kind(&BinOpKind::Le));
        assert!(is_supported_binop_kind(&BinOpKind::Ge));
    }
}
