use std::collections::HashSet;

use crate::{
    Module,
    ast::{Declaration, Span},
    parser_util::is_identifier,
    typecheck::TypecheckError,
    typecheck_env::{CanClause, Ty},
    typecheck_types::ty_from_annotation,
    types::{TypeExpr, parse_function_type, parse_type_expr},
};

pub(crate) fn validate_declaration_annotations(
    module: &Module,
    known_effects: &HashSet<String>,
    embedded_default_effects: &HashSet<String>,
) -> Result<(), TypecheckError> {
    let mut names = HashSet::new();
    for decl in &module.declarations {
        if !names.insert(decl.name.clone()) {
            return Err(TypecheckError {
                declaration: Some(decl.name.clone()),
                span: Some(Span::point(decl.line, decl.col)),
                message: "duplicate top-level declaration".to_string(),
            });
        }

        if let Some(annotation) = decl.type_annotation.as_deref() {
            validate_type_annotation(
                &decl.name,
                annotation,
                known_effects,
                embedded_default_effects,
                Some(Span::point(decl.line, decl.col)),
            )?;
        }
    }
    Ok(())
}

pub(crate) fn validate_main_annotation(module: &Module) -> Result<(), TypecheckError> {
    let Some(main) = module.declarations.iter().find(|d| d.name == "main") else {
        return Ok(());
    };
    let Some(annotation) = main.type_annotation.as_deref() else {
        return Ok(());
    };
    let base_annotation = strip_effect_clause(annotation);
    let ty = parse_function_type(base_annotation).ok_or_else(|| TypecheckError {
        declaration: Some("main".to_string()),
        span: Some(Span::point(main.line, 1)),
        message: "main type annotation must be a function type".to_string(),
    })?;

    if ty.arguments != vec!["Unit".to_string()] || ty.result != "Unit" {
        return Err(TypecheckError {
            declaration: Some("main".to_string()),
            span: Some(Span::point(main.line, 1)),
            message: "main type must be `Unit -> Unit` in MVP".to_string(),
        });
    }
    Ok(())
}

pub(crate) fn declaration_param_types(
    decl: &Declaration,
) -> Result<Vec<(String, Ty)>, TypecheckError> {
    let Some(function_type) = decl
        .type_annotation
        .as_deref()
        .and_then(|annotation| parse_function_type(strip_effect_clause(annotation)))
    else {
        return Ok(Vec::new());
    };

    let unit_param_omitted = decl.params.is_empty()
        && function_type.arguments.len() == 1
        && function_type.arguments[0] == "Unit";
    if !unit_param_omitted && decl.params.len() != function_type.arguments.len() {
        return Err(TypecheckError {
            declaration: Some(decl.name.clone()),
            span: Some(Span::point(decl.line, 1)),
            message: format!(
                "definition has {} parameter(s) but type annotation has {}",
                decl.params.len(),
                function_type.arguments.len()
            ),
        });
    }

    Ok(decl
        .params
        .iter()
        .zip(function_type.arguments.iter())
        .map(|(name, ann_ty)| (name.clone(), ty_from_annotation(ann_ty)))
        .collect())
}

pub(crate) fn annotation_return_ty(annotation: &str) -> Ty {
    let base = strip_effect_clause(annotation).trim();
    if let Some(ft) = parse_function_type(base) {
        ty_from_annotation(&ft.result)
    } else {
        ty_from_annotation(base)
    }
}

pub(crate) fn validate_type_annotation(
    decl_name: &str,
    annotation: &str,
    known_effects: &HashSet<String>,
    embedded_default_effects: &HashSet<String>,
    decl_span: Option<Span>,
) -> Result<(), TypecheckError> {
    if uses_legacy_void(annotation) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: decl_span,
            message: "legacy `void` is not supported; use `Unit`".to_string(),
        });
    }

    // EP-1c: unwrap any matching outer pairs of parentheses so a callback
    // segment such as `(Int -> Int can Ghost)` — and even a doubly-wrapped
    // `((Int -> Int can Ghost))` — is validated identically to a bare
    // top-level function annotation. Tuples (`(Int, Int)`) keep their
    // parentheses because the unwrap helper bails on a depth-0 comma that
    // does not belong to a `can` clause.
    let mut annotation = annotation.trim();
    loop {
        let unwrapped = unwrap_outer_parens(annotation).trim();
        if unwrapped == annotation {
            break;
        }
        annotation = unwrapped;
    }

    validate_effect_clause(
        decl_name,
        annotation,
        known_effects,
        embedded_default_effects,
        decl_span,
    )?;

    let base = strip_effect_clause(annotation).trim();
    if base.is_empty() {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: decl_span,
            message: "type annotation must not be empty".to_string(),
        });
    }

    if base.contains("->") {
        let Some(ft) = parse_function_type(base) else {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: decl_span,
                message: "invalid function type annotation".to_string(),
            });
        };
        let mut segments = ft.arguments;
        segments.push(ft.result);
        for segment in &segments {
            // EP-1c: each function-type segment may itself be a nested
            // function annotation that carries its own `can` clause (e.g.
            // a callback `(Int -> Int can Print)`). Recurse so we validate
            // every nested clause and every nested type expression, instead
            // of only inspecting the outer-arrow segments.
            validate_type_annotation(
                decl_name,
                segment,
                known_effects,
                embedded_default_effects,
                decl_span,
            )?;
        }
    } else {
        let Some(type_expr) = parse_type_expr(base) else {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: decl_span,
                message: "invalid type annotation".to_string(),
            });
        };
        validate_handler_type_expr(decl_name, &type_expr, known_effects, decl_span)?;
    }

    Ok(())
}

pub(crate) fn validate_handler_type_expr(
    decl_name: &str,
    expr: &TypeExpr,
    known_effects: &HashSet<String>,
    decl_span: Option<Span>,
) -> Result<(), TypecheckError> {
    match expr {
        TypeExpr::Name(_) => Ok(()),
        TypeExpr::Tuple(items) => {
            for item in items {
                validate_handler_type_expr(decl_name, item, known_effects, decl_span)?;
            }
            Ok(())
        }
        TypeExpr::Function { arguments, result } => {
            for arg in arguments {
                validate_handler_type_expr(decl_name, arg, known_effects, decl_span)?;
            }
            validate_handler_type_expr(decl_name, result, known_effects, decl_span)
        }
        TypeExpr::Apply { head, args } => {
            validate_handler_type_expr(decl_name, head, known_effects, decl_span)?;
            for arg in args {
                validate_handler_type_expr(decl_name, arg, known_effects, decl_span)?;
            }
            if let TypeExpr::Name(name) = head.as_ref()
                && name == "Handler"
            {
                if args.is_empty() {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: decl_span,
                        message: "Handler type must include at least one effect".to_string(),
                    });
                }
                for arg in args {
                    let TypeExpr::Name(effect_name) = arg else {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: decl_span,
                            message: "Handler type arguments must be effect names (identifiers)"
                                .to_string(),
                        });
                    };
                    if !known_effects.contains(effect_name) {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: decl_span,
                            message: format!(
                                "unknown effect `{}` in `Handler(...)` type annotation",
                                effect_name
                            ),
                        });
                    }
                }
            }
            Ok(())
        }
    }
}

fn uses_legacy_void(annotation: &str) -> bool {
    annotation
        .split(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
        .any(|token| token == "void")
}

pub(crate) fn validate_effect_clause(
    decl_name: &str,
    annotation: &str,
    known_effects: &HashSet<String>,
    embedded_default_effects: &HashSet<String>,
    decl_span: Option<Span>,
) -> Result<(), TypecheckError> {
    let clause = match parse_can_clause(annotation) {
        Ok(None) => return Ok(()),
        Ok(Some(clause)) => clause,
        Err(err) => {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: decl_span,
                message: parse_can_error_message(&err),
            });
        }
    };

    for effect_name in &clause.fixed {
        if !is_identifier(effect_name) {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: decl_span,
                message: format!("invalid effect name `{}` in type annotation", effect_name),
            });
        }
        let is_main_relaxed_embedded =
            decl_name == "main" && embedded_default_effects.contains(effect_name);
        if !known_effects.contains(effect_name) && !is_main_relaxed_embedded {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: decl_span,
                message: format!("unknown effect `{}` in `can` clause", effect_name),
            });
        }
    }

    Ok(())
}

fn parse_can_error_message(err: &ParseCanError) -> String {
    match err {
        ParseCanError::Empty => "effect list after `can` must not be empty".to_string(),
        ParseCanError::EmptySegment => {
            "effect list after `can` must not contain an empty entry".to_string()
        }
        ParseCanError::InvalidIdent(name) => {
            format!("invalid effect name `{}` in type annotation", name)
        }
        ParseCanError::InvalidRowVar(name) => format!(
            "row variable in `can` clause must be a lowercase identifier, got `{{{}}}`",
            name
        ),
        ParseCanError::MultipleRowVars => {
            "`can` clause may contain at most one row variable".to_string()
        }
        ParseCanError::EmptyBraceWithOthers => {
            "`{}` cannot appear more than once or be combined with other entries in `can` clause"
                .to_string()
        }
    }
}

pub(crate) fn strip_effect_clause(annotation: &str) -> &str {
    match find_top_level_can_keyword_index(annotation) {
        Some(idx) => &annotation[..idx],
        None => annotation,
    }
}

pub fn find_can_keyword_index(annotation: &str) -> Option<usize> {
    for (idx, _) in annotation.char_indices() {
        let rest = &annotation[idx..];
        if !rest.starts_with("can") {
            continue;
        }

        let has_left_whitespace = annotation[..idx]
            .chars()
            .last()
            .is_some_and(char::is_whitespace);
        if !has_left_whitespace {
            continue;
        }

        let has_right_whitespace = annotation[idx + 3..]
            .chars()
            .next()
            .is_none_or(char::is_whitespace);
        if !has_right_whitespace {
            continue;
        }

        return Some(idx);
    }

    None
}

/// Locate the rightmost top-level `can` keyword in a function-type annotation.
///
/// Top-level means at parenthesis depth 0; this excludes `can` clauses that
/// appear inside nested function-parameter annotations (e.g. the callback
/// signature in `(a -> b can {e}) -> List b can {e}`). EP-1c relies on this
/// distinction once `Ty::Fun` carries effect rows for parameters.
pub fn find_top_level_can_keyword_index(annotation: &str) -> Option<usize> {
    let bytes = annotation.as_bytes();
    let mut depth: usize = 0;
    let mut last_top_level: Option<usize> = None;
    let mut i = 0usize;

    while i < bytes.len() {
        let byte = bytes[i];
        match byte {
            b'(' => {
                depth += 1;
                i += 1;
                continue;
            }
            b')' => {
                depth = depth.saturating_sub(1);
                i += 1;
                continue;
            }
            _ => {}
        }

        if depth == 0 && byte == b'c' && annotation[i..].starts_with("can") && i + 3 <= bytes.len()
        {
            let left_ok = i == 0
                || annotation[..i]
                    .chars()
                    .last()
                    .is_some_and(char::is_whitespace);
            let right_ok = annotation[i + 3..]
                .chars()
                .next()
                .is_none_or(char::is_whitespace);
            if left_ok && right_ok {
                last_top_level = Some(i);
                i += 3;
                continue;
            }
        }

        i += 1;
    }

    last_top_level
}

/// Errors produced while parsing a `can` clause body. Callers translate these
/// into user-facing diagnostics; the variants exist so each consumer can
/// preserve its own wording.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ParseCanError {
    /// `can` keyword present but no body (e.g. `can` or `can   `).
    Empty,
    /// A comma-separated segment was empty (e.g. `can E,,F` or trailing comma).
    EmptySegment,
    /// Fixed-effect segment is not a valid identifier.
    InvalidIdent(String),
    /// Row variable braces present but the inner identifier is not a
    /// lowercase ASCII identifier (or is empty / has bad characters).
    InvalidRowVar(String),
    /// More than one row variable in a single clause (`can {e}, {f}`).
    MultipleRowVars,
    /// Explicit empty row `{}` combined with other entries (`can {}, Print`).
    EmptyBraceWithOthers,
}

/// Parse a `can` clause out of a full type annotation.
///
/// Returns `Ok(None)` when no top-level `can` keyword is present (the
/// closed-empty row, equivalent to no clause). Returns `Ok(Some(_))` for any
/// well-formed clause, including `can {}` (recorded as `explicit_empty=true`).
///
/// The clause is detected at parenthesis depth 0 only, so nested callback
/// annotations such as `(a -> b can {e}) -> List b can {e}` resolve to the
/// outer `can {e}`. Inner clauses become visible to the typechecker through
/// the parameter-type recursion in EP-1c, not through this helper.
pub(crate) fn parse_can_clause(annotation: &str) -> Result<Option<CanClause>, ParseCanError> {
    let Some(idx) = find_top_level_can_keyword_index(annotation) else {
        return Ok(None);
    };

    let body = annotation[idx + 3..].trim();
    if body.is_empty() {
        return Err(ParseCanError::Empty);
    }

    let mut fixed: Vec<String> = Vec::new();
    let mut row_var: Option<String> = None;
    let mut explicit_empty = false;

    for raw in body.split(',') {
        let segment = raw.trim();
        if segment.is_empty() {
            return Err(ParseCanError::EmptySegment);
        }

        if segment == "{}" {
            if explicit_empty {
                return Err(ParseCanError::EmptyBraceWithOthers);
            }
            explicit_empty = true;
            continue;
        }

        if let Some(inner) = segment
            .strip_prefix('{')
            .and_then(|rest| rest.strip_suffix('}'))
        {
            let inner_trim = inner.trim();
            if !is_row_variable_ident(inner_trim) {
                return Err(ParseCanError::InvalidRowVar(inner_trim.to_string()));
            }
            if row_var.is_some() {
                return Err(ParseCanError::MultipleRowVars);
            }
            row_var = Some(inner_trim.to_string());
            continue;
        }

        if !is_identifier(segment) {
            return Err(ParseCanError::InvalidIdent(segment.to_string()));
        }
        fixed.push(segment.to_string());
    }

    if explicit_empty && (!fixed.is_empty() || row_var.is_some()) {
        return Err(ParseCanError::EmptyBraceWithOthers);
    }

    Ok(Some(CanClause {
        fixed,
        row_var,
        explicit_empty,
    }))
}

/// Public, EP-1b/EP-1c-stable view of a parsed `can` clause restricted to its
/// fixed-effect names. Intended for consumers (e.g. the wasm planner) that
/// only need the closed portion of the row and must not depend on the internal
/// `CanClause` representation.
///
/// Returns an empty vector if the annotation has no `can` clause or if the
/// clause is malformed. This helper is intentionally tolerant — it can be
/// called from paths (e.g. the wasm planner, planning-layer unit tests) that
/// do not always run after the typecheck-validate phase, so a parse error
/// must not panic. Validation is performed separately by
/// `validate_effect_clause`.
pub fn fixed_effects_from_can_clause(annotation: &str) -> Vec<String> {
    match parse_can_clause(annotation) {
        Ok(Some(clause)) => clause.fixed,
        Ok(None) | Err(_) => Vec::new(),
    }
}

/// Strip a single matching outer pair of parentheses, when they wrap the
/// entire string and do not delimit a tuple. Used by the validation pass to
/// recurse into callback annotations such as `(Int -> Int can Print)`.
/// Mirrors `typecheck_types::unwrap_outer_parens`; the duplication is small
/// enough not to warrant a shared helper module today.
fn unwrap_outer_parens(s: &str) -> &str {
    if !s.starts_with('(') || !s.ends_with(')') {
        return s;
    }
    let inner = &s[1..s.len() - 1];
    // Validate that the leading `(` matches the trailing `)` (no early close)
    // and find the position of any depth-0 `,`. A depth-0 `,` denotes a tuple
    // unless it appears inside the outer `can` clause (e.g.
    // `Int -> Int can {e}, {f}`), in which case the comma is part of the
    // effect list and not a tuple separator.
    let mut depth: usize = 0;
    let mut first_top_comma: Option<usize> = None;
    for (idx, ch) in inner.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                if depth == 0 {
                    return s;
                }
                depth -= 1;
            }
            ',' if depth == 0 && first_top_comma.is_none() => {
                first_top_comma = Some(idx);
            }
            _ => {}
        }
    }
    if depth != 0 {
        return s;
    }
    if let Some(comma_idx) = first_top_comma {
        // Tuple iff the comma appears before any top-level `can` keyword.
        match find_top_level_can_keyword_index(inner) {
            Some(can_idx) if can_idx < comma_idx => {
                // `can ... ,` shape — comma belongs to the effect list, not a
                // tuple separator. Fall through to unwrap.
            }
            _ => return s,
        }
    }
    inner
}

fn is_row_variable_ident(s: &str) -> bool {
    let mut chars = s.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_lowercase() {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

#[cfg(test)]
mod parse_can_clause_tests {
    use super::*;

    fn ok_clause(annotation: &str) -> CanClause {
        match parse_can_clause(annotation) {
            Ok(Some(clause)) => clause,
            Ok(None) => panic!("expected Some(_), got None for {:?}", annotation),
            Err(err) => panic!("expected Ok, got Err({:?}) for {:?}", err, annotation),
        }
    }

    #[test]
    fn no_can_keyword_returns_none() {
        assert_eq!(parse_can_clause("Int -> Int"), Ok(None));
        assert_eq!(parse_can_clause(""), Ok(None));
    }

    #[test]
    fn single_fixed_effect() {
        let c = ok_clause("Unit -> Unit can Print");
        assert_eq!(c.fixed, vec!["Print".to_string()]);
        assert_eq!(c.row_var, None);
        assert!(!c.explicit_empty);
    }

    #[test]
    fn multiple_fixed_effects_preserve_order() {
        let c = ok_clause("Unit -> Unit can Print, Read");
        assert_eq!(c.fixed, vec!["Print".to_string(), "Read".to_string()]);
        assert_eq!(c.row_var, None);
    }

    #[test]
    fn duplicate_fixed_effects_are_accepted_idempotently() {
        let c = ok_clause("Unit -> Unit can Print, Print");
        assert_eq!(c.fixed, vec!["Print".to_string(), "Print".to_string()]);
    }

    #[test]
    fn fixed_plus_row_variable() {
        let c = ok_clause("Unit -> Unit can Print, {e}");
        assert_eq!(c.fixed, vec!["Print".to_string()]);
        assert_eq!(c.row_var, Some("e".to_string()));
    }

    #[test]
    fn row_variable_only() {
        let c = ok_clause("Unit -> Unit can {e}");
        assert!(c.fixed.is_empty());
        assert_eq!(c.row_var, Some("e".to_string()));
    }

    #[test]
    fn explicit_empty_row() {
        let c = ok_clause("Unit -> Unit can {}");
        assert!(c.fixed.is_empty());
        assert_eq!(c.row_var, None);
        assert!(c.explicit_empty);
    }

    #[test]
    fn multiple_row_variables_rejected() {
        assert_eq!(
            parse_can_clause("Unit -> Unit can {e}, {f}"),
            Err(ParseCanError::MultipleRowVars)
        );
    }

    #[test]
    fn uppercase_row_variable_rejected() {
        assert_eq!(
            parse_can_clause("Unit -> Unit can {Foo}"),
            Err(ParseCanError::InvalidRowVar("Foo".to_string()))
        );
    }

    #[test]
    fn underscore_row_variable_rejected() {
        assert_eq!(
            parse_can_clause("Unit -> Unit can {_}"),
            Err(ParseCanError::InvalidRowVar("_".to_string()))
        );
    }

    #[test]
    fn empty_brace_combined_with_other_rejected() {
        assert_eq!(
            parse_can_clause("Unit -> Unit can {}, Print"),
            Err(ParseCanError::EmptyBraceWithOthers)
        );
        assert_eq!(
            parse_can_clause("Unit -> Unit can Print, {}"),
            Err(ParseCanError::EmptyBraceWithOthers)
        );
    }

    #[test]
    fn duplicate_explicit_empty_brace_rejected() {
        // Duplicate `{}` collapses into the same EmptyBraceWithOthers
        // diagnostic; the message is broadened to cover both shapes.
        assert_eq!(
            parse_can_clause("Unit -> Unit can {}, {}"),
            Err(ParseCanError::EmptyBraceWithOthers)
        );
    }

    #[test]
    fn trailing_comma_rejected() {
        assert_eq!(
            parse_can_clause("Unit -> Unit can Print, "),
            Err(ParseCanError::EmptySegment)
        );
    }

    #[test]
    fn double_comma_rejected() {
        assert_eq!(
            parse_can_clause("Unit -> Unit can Print,,Read"),
            Err(ParseCanError::EmptySegment)
        );
    }

    #[test]
    fn empty_clause_body_rejected() {
        assert_eq!(
            parse_can_clause("Unit -> Unit can"),
            Err(ParseCanError::Empty)
        );
        assert_eq!(
            parse_can_clause("Unit -> Unit can   "),
            Err(ParseCanError::Empty)
        );
    }

    #[test]
    fn invalid_fixed_effect_ident_rejected() {
        assert_eq!(
            parse_can_clause("Unit -> Unit can 1bad"),
            Err(ParseCanError::InvalidIdent("1bad".to_string()))
        );
    }

    #[test]
    fn top_level_can_takes_outer_when_callback_has_can() {
        let c = ok_clause("(a -> b can Print) -> List b can Read");
        assert_eq!(c.fixed, vec!["Read".to_string()]);
        assert_eq!(c.row_var, None);
    }

    #[test]
    fn top_level_can_with_row_variable_in_callback() {
        let c = ok_clause("(a -> b can {e}) -> List b can {e}");
        assert!(c.fixed.is_empty());
        assert_eq!(c.row_var, Some("e".to_string()));
    }

    #[test]
    fn callback_only_can_returns_none_at_top_level() {
        // Callback has `can`, but the outer function has no `can` clause.
        assert_eq!(parse_can_clause("(a -> b can Print) -> List b"), Ok(None));
    }

    #[test]
    fn find_top_level_can_keyword_skips_nested() {
        // Index of the *outer* `can` (rightmost top-level) should be returned.
        let ann = "(a -> b can Print) -> List b can Read";
        let idx = find_top_level_can_keyword_index(ann).expect("outer can present");
        assert_eq!(&ann[idx..idx + 3], "can");
        // Confirm it is the second occurrence, not the inner one.
        assert!(idx > ann.find("(a -> b can").unwrap());
    }

    #[test]
    fn find_top_level_can_keyword_returns_none_when_only_nested() {
        let ann = "(a -> b can Print) -> List b";
        assert_eq!(find_top_level_can_keyword_index(ann), None);
    }

    #[test]
    fn fixed_effects_from_can_clause_basic() {
        assert_eq!(
            fixed_effects_from_can_clause("Unit -> Unit can Print, Read"),
            vec!["Print".to_string(), "Read".to_string()]
        );
        assert!(fixed_effects_from_can_clause("Unit -> Unit").is_empty());
    }

    #[test]
    fn fixed_effects_from_can_clause_drops_row_variable_and_empty_brace() {
        assert_eq!(
            fixed_effects_from_can_clause("Unit -> Unit can Print, {e}"),
            vec!["Print".to_string()]
        );
        assert!(fixed_effects_from_can_clause("Unit -> Unit can {}").is_empty());
        assert!(fixed_effects_from_can_clause("Unit -> Unit can {e}").is_empty());
    }
}
