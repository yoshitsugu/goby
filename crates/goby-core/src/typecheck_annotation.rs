use std::collections::HashSet;

use crate::{
    Module,
    ast::{Declaration, Span},
    typecheck::{TypecheckError, is_identifier},
    typecheck_env::Ty,
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
                span: Some(Span::point(decl.line, 1)),
                message: "duplicate top-level declaration".to_string(),
            });
        }

        if let Some(annotation) = decl.type_annotation.as_deref() {
            validate_type_annotation(
                &decl.name,
                annotation,
                known_effects,
                embedded_default_effects,
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
) -> Result<(), TypecheckError> {
    if uses_legacy_void(annotation) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: "legacy `void` is not supported; use `Unit`".to_string(),
        });
    }

    validate_effect_clause(
        decl_name,
        annotation,
        known_effects,
        embedded_default_effects,
    )?;

    let base = strip_effect_clause(annotation).trim();
    if base.is_empty() {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: "type annotation must not be empty".to_string(),
        });
    }

    if base.contains("->") {
        let Some(ft) = parse_function_type(base) else {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: "invalid function type annotation".to_string(),
            });
        };
        let mut segments = ft.arguments;
        segments.push(ft.result);
        for segment in &segments {
            let Some(type_expr) = parse_type_expr(segment) else {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: "invalid function type annotation".to_string(),
                });
            };
            validate_handler_type_expr(decl_name, &type_expr, known_effects)?;
        }
    } else {
        let Some(type_expr) = parse_type_expr(base) else {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: "invalid type annotation".to_string(),
            });
        };
        validate_handler_type_expr(decl_name, &type_expr, known_effects)?;
    }

    Ok(())
}

pub(crate) fn validate_handler_type_expr(
    decl_name: &str,
    expr: &TypeExpr,
    known_effects: &HashSet<String>,
) -> Result<(), TypecheckError> {
    match expr {
        TypeExpr::Name(_) => Ok(()),
        TypeExpr::Tuple(items) => {
            for item in items {
                validate_handler_type_expr(decl_name, item, known_effects)?;
            }
            Ok(())
        }
        TypeExpr::Function { arguments, result } => {
            for arg in arguments {
                validate_handler_type_expr(decl_name, arg, known_effects)?;
            }
            validate_handler_type_expr(decl_name, result, known_effects)
        }
        TypeExpr::Apply { head, args } => {
            validate_handler_type_expr(decl_name, head, known_effects)?;
            for arg in args {
                validate_handler_type_expr(decl_name, arg, known_effects)?;
            }
            if let TypeExpr::Name(name) = head.as_ref()
                && name == "Handler"
            {
                if args.is_empty() {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: "Handler type must include at least one effect".to_string(),
                    });
                }
                for arg in args {
                    let TypeExpr::Name(effect_name) = arg else {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: None,
                            message: "Handler type arguments must be effect names (identifiers)"
                                .to_string(),
                        });
                    };
                    if !known_effects.contains(effect_name) {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: None,
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
) -> Result<(), TypecheckError> {
    let Some(effect_idx) = find_can_keyword_index(annotation) else {
        return Ok(());
    };

    let effects_raw = annotation[effect_idx + 3..].trim();
    if effects_raw.is_empty() {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: "effect list after `can` must not be empty".to_string(),
        });
    }

    for effect_name in effects_raw.split(',').map(str::trim) {
        if !is_identifier(effect_name) {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!("invalid effect name `{}` in type annotation", effect_name),
            });
        }
        let is_main_relaxed_embedded =
            decl_name == "main" && embedded_default_effects.contains(effect_name);
        if !known_effects.contains(effect_name) && !is_main_relaxed_embedded {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!("unknown effect `{}` in `can` clause", effect_name),
            });
        }
    }

    Ok(())
}

pub(crate) fn strip_effect_clause(annotation: &str) -> &str {
    match find_can_keyword_index(annotation) {
        Some(idx) => &annotation[..idx],
        None => annotation,
    }
}

pub(crate) fn find_can_keyword_index(annotation: &str) -> Option<usize> {
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
