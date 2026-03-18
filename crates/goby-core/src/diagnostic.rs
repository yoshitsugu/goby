use crate::ast::Span;
use crate::parser::ParseError;
use crate::typecheck::TypecheckError;

/// Severity level of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

/// A unified diagnostic that can represent both parse errors and typecheck errors.
///
/// This type is intended for use by tooling (CLI, LSP) that needs a common representation
/// of all compiler diagnostics.
///
/// # "No location" encoding
/// `span: None` always means "no location available" regardless of error source.
///
/// `From<ParseError>` normalises the col-1 sentinel (meaning "unknown column" in
/// `ParseError`) to `span: None`; callers can treat `span.is_some()` as a reliable
/// location indicator for parse-error diagnostics.
///
/// `From<TypecheckError>` passes `span` through unchanged. A `Some(Span)` with
/// `col = 1` may still appear for typecheck errors — it is an approximate location
/// (convention, not enforced by the type) that will be refined in D1c.
/// For typecheck-error diagnostics, `span.is_some()` means *a location is recorded*
/// but does not guarantee column precision.
///
/// Column values in non-None spans are 1-indexed byte offsets (ASCII assumption, MVP).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub severity: Severity,
    /// Source location. `None` when location is unavailable.
    pub span: Option<Span>,
    /// Name of the declaration containing this diagnostic, if known.
    pub declaration: Option<String>,
    pub message: String,
}

impl From<ParseError> for Diagnostic {
    fn from(err: ParseError) -> Self {
        // col=1 is the "unknown column" sentinel in ParseError; normalise to span: None.
        // Any other col value is a real 1-indexed byte offset.
        let span = if err.col == 1 {
            None
        } else {
            Some(Span::point(err.line, err.col))
        };
        Diagnostic {
            severity: Severity::Error,
            span,
            declaration: None,
            message: err.message,
        }
    }
}

impl From<TypecheckError> for Diagnostic {
    fn from(err: TypecheckError) -> Self {
        // TypecheckError already uses span: None for "unknown location".
        // Pass it through unchanged. By convention, col=1 inside a Some(Span) may
        // indicate an approximate/unknown column — this is not enforced by the type.
        // D1c will replace these with precise spans.
        Diagnostic {
            severity: Severity::Error,
            span: err.span,
            declaration: err.declaration,
            message: err.message,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_error_with_known_col_converts_to_point_span() {
        let err = ParseError {
            line: 3,
            col: 5,
            message: "unexpected token".to_string(),
        };
        let diag = Diagnostic::from(err);
        assert_eq!(diag.severity, Severity::Error);
        assert_eq!(diag.span, Some(Span::point(3, 5)));
        assert_eq!(diag.declaration, None);
        assert_eq!(diag.message, "unexpected token");
    }

    #[test]
    fn parse_error_col1_sentinel_normalises_to_none_span() {
        // col=1 is the "unknown column" sentinel in ParseError.
        let err = ParseError {
            line: 2,
            col: 1,
            message: "missing declaration body".to_string(),
        };
        let diag = Diagnostic::from(err);
        assert_eq!(diag.span, None);
        assert_eq!(diag.message, "missing declaration body");
    }

    #[test]
    fn parse_error_line1_col1_sentinel_also_normalises_to_none_span() {
        // The normalisation applies unconditionally regardless of line number.
        let err = ParseError {
            line: 1,
            col: 1,
            message: "invalid type annotation".to_string(),
        };
        let diag = Diagnostic::from(err);
        assert_eq!(diag.span, None);
    }

    #[test]
    fn typecheck_error_with_span_converts_to_diagnostic() {
        let err = TypecheckError {
            declaration: Some("add".to_string()),
            span: Some(Span::new(2, 4, 2, 9)),
            message: "type mismatch".to_string(),
        };
        let diag = Diagnostic::from(err);
        assert_eq!(diag.severity, Severity::Error);
        assert_eq!(diag.span, Some(Span::new(2, 4, 2, 9)));
        assert_eq!(diag.declaration, Some("add".to_string()));
        assert_eq!(diag.message, "type mismatch");
    }

    #[test]
    fn typecheck_error_without_span_converts_to_diagnostic_with_none_span() {
        let err = TypecheckError {
            declaration: None,
            span: None,
            message: "unknown effect".to_string(),
        };
        let diag = Diagnostic::from(err);
        assert_eq!(diag.severity, Severity::Error);
        assert_eq!(diag.span, None);
    }

    #[test]
    fn typecheck_error_with_col1_span_passes_through_as_is() {
        // TypecheckError with col=1 inside Some(Span) is passed through unchanged.
        // Callers should treat these as approximate locations until D1c.
        let err = TypecheckError {
            declaration: Some("f".to_string()),
            span: Some(Span::point(5, 1)),
            message: "effect not handled".to_string(),
        };
        let diag = Diagnostic::from(err);
        assert_eq!(diag.span, Some(Span::point(5, 1)));
    }
}
