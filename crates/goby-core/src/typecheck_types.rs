use std::collections::HashSet;

use crate::{
    Module,
    ast::TypeDeclaration,
    typecheck::TypecheckError,
    typecheck_env::{EffectRow, Ty},
    types::{TypeExpr, parse_type_expr},
};

pub(crate) fn validate_type_declarations(
    module: &Module,
    imported_type_names: &HashSet<String>,
) -> Result<(), TypecheckError> {
    let mut known_type_names: HashSet<String> = builtin_type_names()
        .into_iter()
        .map(|name| name.to_string())
        .collect();
    known_type_names.extend(imported_type_names.iter().cloned());
    let mut declared_type_names = HashSet::new();

    for ty_decl in &module.type_declarations {
        let name = match ty_decl {
            TypeDeclaration::Alias { name, .. } => name,
            TypeDeclaration::Union { name, .. } => name,
            TypeDeclaration::Record { name, .. } => name,
        };
        if !declared_type_names.insert(name.clone()) {
            return Err(TypecheckError {
                declaration: Some(name.clone()),
                span: None, // expr span not yet available
                message: format!("duplicate type declaration `{}`", name),
            });
        }
        known_type_names.insert(name.clone());
    }

    for ty_decl in &module.type_declarations {
        match ty_decl {
            TypeDeclaration::Alias { name, target } => {
                let parsed = parse_type_expr(target).ok_or_else(|| TypecheckError {
                    declaration: Some(name.clone()),
                    span: None, // expr span not yet available
                    message: "invalid alias target type".to_string(),
                })?;
                validate_type_expr_names(&parsed, &known_type_names, name)?;
            }
            TypeDeclaration::Union { name, constructors } => {
                let mut seen = HashSet::new();
                for constructor in constructors {
                    if !seen.insert(constructor.clone()) {
                        return Err(TypecheckError {
                            declaration: Some(name.clone()),
                            span: None, // expr span not yet available
                            message: format!(
                                "duplicate constructor `{}` in type `{}`",
                                constructor, name
                            ),
                        });
                    }
                }
            }
            TypeDeclaration::Record { name, fields, .. } => {
                let mut seen = HashSet::new();
                for field in fields {
                    if !seen.insert(field.name.clone()) {
                        return Err(TypecheckError {
                            declaration: Some(name.clone()),
                            span: None, // expr span not yet available
                            message: format!("duplicate field `{}` in type `{}`", field.name, name),
                        });
                    }
                    let parsed =
                        parse_type_expr(&field.type_annotation).ok_or_else(|| TypecheckError {
                            declaration: Some(name.clone()),
                            span: None, // expr span not yet available
                            message: format!("invalid field type `{}`", field.type_annotation),
                        })?;
                    validate_type_expr_names(&parsed, &known_type_names, name)?;
                }
            }
        }
    }

    Ok(())
}

fn validate_type_expr_names(
    expr: &TypeExpr,
    known_type_names: &HashSet<String>,
    declaration: &str,
) -> Result<(), TypecheckError> {
    match expr {
        TypeExpr::Name(name) => {
            if builtin_type_names().contains(&name.as_str())
                || is_type_variable_name(name)
                || known_type_names.contains(name)
            {
                return Ok(());
            }
            Err(TypecheckError {
                declaration: Some(declaration.to_string()),
                span: None, // expr span not yet available
                message: format!("unknown type `{}` in type declaration", name),
            })
        }
        TypeExpr::Tuple(items) => {
            for item in items {
                validate_type_expr_names(item, known_type_names, declaration)?;
            }
            Ok(())
        }
        TypeExpr::Function { arguments, result } => {
            for arg in arguments {
                validate_type_expr_names(arg, known_type_names, declaration)?;
            }
            validate_type_expr_names(result, known_type_names, declaration)
        }
        TypeExpr::Apply { head, args } => {
            validate_type_expr_names(head, known_type_names, declaration)?;
            for arg in args {
                validate_type_expr_names(arg, known_type_names, declaration)?;
            }
            Ok(())
        }
    }
}

pub(crate) fn ty_from_annotation(s: &str) -> Ty {
    let mut type_hole_counter = 0usize;
    ty_from_annotation_with_holes(s, &mut type_hole_counter)
}

fn ty_from_annotation_with_holes(s: &str, type_hole_counter: &mut usize) -> Ty {
    // EP-1c: peel wrapping `(` ... `)` (one or more layers) first so callback
    // annotations such as `(Int -> Int can Print)` and even `((Int -> Int))`
    // are treated identically to a bare top-level function annotation for
    // `can`-clause detection. `parse_function_type` already strips
    // parentheses internally, but `parse_can_clause` works on raw strings,
    // so we must mirror the unwrap here.
    let mut current = s.trim();
    loop {
        let unwrapped = unwrap_outer_parens(current).trim();
        if unwrapped == current {
            break;
        }
        current = unwrapped;
    }
    let s = current;
    if s.is_empty() {
        return Ty::Unknown;
    }

    // Parse the surface `can` clause off the top-level annotation so function
    // types carry their residual effect row. Nested callback annotations are
    // recovered by recursing on raw segment strings (since
    // TypeExpr/parse_function_type strip `can` during shape parsing).
    let outer_clause = match crate::typecheck_annotation::parse_can_clause(s) {
        Ok(clause) => clause,
        // EP-1b's validate phase rejects malformed clauses before this point.
        // If a malformed clause still reaches us (e.g. through a planner-only
        // path), treat the row as closed-empty rather than panicking.
        Err(_) => None,
    };
    let outer_effects = outer_clause
        .as_ref()
        .map(EffectRow::from_can_clause)
        .unwrap_or_else(EffectRow::closed_empty);

    let base = crate::typecheck_annotation::strip_effect_clause(s).trim();

    if let Some(ft) = crate::types::parse_function_type(base) {
        let params: Vec<Ty> = ft
            .arguments
            .iter()
            .map(|arg| ty_from_annotation_with_holes(arg, type_hole_counter))
            .collect();
        let result = ty_from_annotation_with_holes(&ft.result, type_hole_counter);
        return Ty::Fun {
            params,
            result: Box::new(result),
            effects: outer_effects,
        };
    }

    let Some(type_expr) = parse_type_expr(base) else {
        return Ty::Unknown;
    };
    ty_from_type_expr_with_holes(&type_expr, type_hole_counter)
}

/// Strip a single matching pair of outer parentheses, when they wrap the
/// entire string and do not delimit a tuple. Returns the input unchanged if
/// the parens form a tuple (`(Int, Int)`) or do not pair with each other.
///
/// Special-cases the `can ...` clause: a depth-0 `,` that follows a
/// top-level `can` belongs to the effect list, not to a tuple separator
/// (`Int -> Int can {e}, {f}`), and the surrounding parens should still be
/// removed so the function form is recovered.
fn unwrap_outer_parens(s: &str) -> &str {
    if !s.starts_with('(') || !s.ends_with(')') {
        return s;
    }
    let inner = &s[1..s.len() - 1];
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
        match crate::typecheck_annotation::find_top_level_can_keyword_index(inner) {
            Some(can_idx) if can_idx < comma_idx => {
                // The depth-0 comma is part of the `can` clause, not a tuple.
            }
            _ => return s,
        }
    }
    inner
}

pub(crate) fn ty_from_type_expr(expr: &TypeExpr) -> Ty {
    let mut type_hole_counter = 0usize;
    ty_from_type_expr_with_holes(expr, &mut type_hole_counter)
}

fn ty_from_type_expr_with_holes(expr: &TypeExpr, type_hole_counter: &mut usize) -> Ty {
    match expr {
        TypeExpr::Name(name) => {
            if name == "_" {
                let current = *type_hole_counter;
                *type_hole_counter += 1;
                return Ty::Var(format!("__goby_type_hole_{}", current));
            }
            ty_from_name(name)
        }
        TypeExpr::Tuple(items) => Ty::Tuple(
            items
                .iter()
                .map(|item| ty_from_type_expr_with_holes(item, type_hole_counter))
                .collect(),
        ),
        TypeExpr::Function { arguments, result } => Ty::Fun {
            params: arguments
                .iter()
                .map(|arg| ty_from_type_expr_with_holes(arg, type_hole_counter))
                .collect(),
            result: Box::new(ty_from_type_expr_with_holes(result, type_hole_counter)),
            // EP-1c: TypeExpr does not preserve `can` clauses (they are
            // stripped by parse_type_expr / parse_function_type). The actual
            // effects row is plumbed via the string-based annotation path in
            // ty_from_annotation. Synthesizing closed-empty here is the
            // correct fallback for any caller still reaching Ty through
            // TypeExpr.
            effects: EffectRow::closed_empty(),
        },
        TypeExpr::Apply { head, args } => {
            let TypeExpr::Name(name) = head.as_ref() else {
                return Ty::Unknown;
            };
            let converted_args: Vec<Ty> = args
                .iter()
                .map(|arg| ty_from_type_expr_with_holes(arg, type_hole_counter))
                .collect();
            if name == "List" && converted_args.len() == 1 {
                Ty::List(Box::new(converted_args[0].clone()))
            } else {
                Ty::Con {
                    name: name.clone(),
                    args: converted_args,
                }
            }
        }
    }
}

fn ty_from_name(name: &str) -> Ty {
    match name {
        "Int" => Ty::Int,
        "Bool" => Ty::Bool,
        "String" => Ty::Str,
        "Unit" => Ty::Unit,
        _ if is_type_variable_name(name) => Ty::Var(name.to_string()),
        _ => Ty::Con {
            name: name.to_string(),
            args: Vec::new(),
        },
    }
}

pub(crate) fn is_type_variable_name(name: &str) -> bool {
    name.chars()
        .next()
        .is_some_and(|c| c.is_ascii_lowercase() || c == '_')
}

fn builtin_type_names() -> [&'static str; 5] {
    ["Int", "Bool", "String", "Unit", "List"]
}

#[cfg(test)]
mod ep1c_annotation_tests {
    use crate::typecheck_env::{EffectRow, RowVarId, Ty};

    use super::ty_from_annotation;

    fn fun_effects(ty: &Ty) -> &EffectRow {
        match ty {
            Ty::Fun { effects, .. } => effects,
            other => panic!("expected Ty::Fun, got {:?}", other),
        }
    }

    #[test]
    fn annotation_without_can_clause_yields_closed_empty_row() {
        let ty = ty_from_annotation("Int -> Int");
        assert_eq!(fun_effects(&ty), &EffectRow::closed_empty());
    }

    #[test]
    fn annotation_with_outer_can_clause_records_fixed_row() {
        let ty = ty_from_annotation("Int -> Int can Print");
        assert_eq!(
            fun_effects(&ty),
            &EffectRow::closed_from(["Print".to_string()])
        );
    }

    #[test]
    fn annotation_with_outer_can_clause_records_row_variable() {
        let ty = ty_from_annotation("Int -> Int can {e}");
        let row = fun_effects(&ty);
        assert!(row.fixed.is_empty());
        assert_eq!(row.tail, Some(RowVarId("e".to_string())));
    }

    #[test]
    fn annotation_with_callback_lifts_inner_can_into_callback_function_ty() {
        // EP-0 motivating shape: outer effect = Read, callback effect = Print.
        let ty = ty_from_annotation("(Int -> Int can Print) -> Int can Read");
        let outer = fun_effects(&ty);
        assert_eq!(outer, &EffectRow::closed_from(["Read".to_string()]));

        let Ty::Fun { params, .. } = &ty else {
            panic!("expected Ty::Fun");
        };
        let callback = &params[0];
        assert_eq!(
            fun_effects(callback),
            &EffectRow::closed_from(["Print".to_string()])
        );
    }

    #[test]
    fn annotation_with_callback_row_variable_propagates_through_recursion() {
        // EP-2 stdlib HOF shape: same row var on outer and callback.
        let ty = ty_from_annotation("List a -> (a -> b can {e}) -> List b can {e}");
        let outer = fun_effects(&ty);
        assert!(outer.fixed.is_empty());
        assert_eq!(outer.tail, Some(RowVarId("e".to_string())));

        let Ty::Fun { params, .. } = &ty else {
            panic!("expected Ty::Fun");
        };
        // The second parameter is the callback `(a -> b can {e})`.
        let callback = &params[1];
        let cb_row = fun_effects(callback);
        assert!(cb_row.fixed.is_empty());
        assert_eq!(cb_row.tail, Some(RowVarId("e".to_string())));
    }

    #[test]
    fn annotation_with_explicit_empty_row_collapses_to_closed_empty() {
        let ty = ty_from_annotation("Int -> Int can {}");
        assert_eq!(fun_effects(&ty), &EffectRow::closed_empty());
    }

    #[test]
    fn type_hole_counter_is_shared_across_function_segments() {
        // Codex Pass2 raised: when ty_from_annotation recursed into each
        // function segment with a fresh counter, both `_` placeholders
        // collapsed to the same `__goby_type_hole_0` and were treated as
        // the same type variable. The counter is now threaded through the
        // recursion so each `_` gets a distinct fresh name.
        let ty = ty_from_annotation("_ -> _ -> Unit");
        let Ty::Fun {
            params,
            result,
            effects,
        } = ty
        else {
            panic!("expected Ty::Fun");
        };
        assert_eq!(effects, EffectRow::closed_empty());
        // parse_function_type splits on every depth-0 `->`, so `_ -> _ -> Unit`
        // becomes a 2-arg function with `Unit` as result.
        assert_eq!(params.len(), 2);
        assert_eq!(result.as_ref(), &Ty::Unit);
        let first_hole = match &params[0] {
            Ty::Var(name) if name.starts_with("__goby_type_hole_") => name.clone(),
            other => panic!("expected first param to be a type hole, got {:?}", other),
        };
        let second_hole = match &params[1] {
            Ty::Var(name) if name.starts_with("__goby_type_hole_") => name.clone(),
            other => panic!("expected second param to be a type hole, got {:?}", other),
        };
        assert_ne!(
            first_hole, second_hole,
            "each `_` must mint a fresh hole id, got {first_hole} and {second_hole}"
        );
    }
}
