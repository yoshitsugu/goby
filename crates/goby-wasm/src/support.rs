use goby_core::{BinOpKind, CasePattern, Expr};

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
    matches!(expr, Expr::IntLit(_) | Expr::Var { name: _, .. })
}

pub(crate) fn is_supported_binop_kind(op: &BinOpKind) -> bool {
    matches!(op, BinOpKind::Add | BinOpKind::Mul | BinOpKind::Eq)
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
        assert!(is_supported_list_item_expr(&Expr::Var { name: "x".to_string(), span: None }));
        assert!(!is_supported_list_item_expr(&Expr::BoolLit(true)));
    }

    #[test]
    fn supports_current_native_binop_kinds() {
        assert!(is_supported_binop_kind(&BinOpKind::Add));
        assert!(is_supported_binop_kind(&BinOpKind::Mul));
        assert!(is_supported_binop_kind(&BinOpKind::Eq));
    }
}
