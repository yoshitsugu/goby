use goby_core::CasePattern;

pub(crate) fn is_supported_case_pattern(pattern: &CasePattern) -> bool {
    matches!(
        pattern,
        CasePattern::IntLit(_)
            | CasePattern::StringLit(_)
            | CasePattern::BoolLit(_)
            | CasePattern::Wildcard
    )
}

#[cfg(test)]
mod tests {
    use goby_core::CasePattern;

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
}
