use crate::typecheck_env::{EffectRow, Ty};

pub(crate) fn ty_name(ty: &Ty) -> String {
    match ty {
        Ty::Int => "Int".to_string(),
        Ty::Bool => "Bool".to_string(),
        Ty::Str => "String".to_string(),
        Ty::Unit => "Unit".to_string(),
        Ty::List(inner) => format!("List {}", ty_name(inner)),
        Ty::Tuple(items) => {
            let inner: Vec<String> = items.iter().map(ty_name).collect();
            format!("({})", inner.join(", "))
        }
        Ty::Fun {
            params,
            result,
            effects,
        } => {
            let mut parts: Vec<String> = params.iter().map(format_fun_segment).collect();
            parts.push(ty_name(result));
            let mut rendered = parts.join(" -> ");
            // EP-1c: append `can ...` only when the row is not closed-empty.
            // Closed-empty rows render as the bare arrow type so existing
            // diagnostic snapshots remain unchanged.
            if !effects.is_empty_closed() {
                rendered.push_str(" can ");
                rendered.push_str(&format_effect_row(effects));
            }
            rendered
        }
        Ty::Var(name) => {
            if name.starts_with("__goby_type_hole_") {
                "_".to_string()
            } else {
                name.clone()
            }
        }
        Ty::Con { name, args } => {
            if args.is_empty() {
                name.clone()
            } else {
                let rendered_args: Vec<String> =
                    args.iter().map(format_type_application_arg).collect();
                format!("{} {}", name, rendered_args.join(" "))
            }
        }
        Ty::Handler { .. } => "Handler".to_string(),
        Ty::Unknown => "Unknown".to_string(),
    }
}

fn format_type_application_arg(ty: &Ty) -> String {
    match ty {
        Ty::Con { args, .. } if !args.is_empty() => format!("({})", ty_name(ty)),
        Ty::Fun { .. } => format!("({})", ty_name(ty)),
        _ => ty_name(ty),
    }
}

fn format_fun_segment(ty: &Ty) -> String {
    match ty {
        Ty::Fun { .. } => format!("({})", ty_name(ty)),
        _ => ty_name(ty),
    }
}

/// Render an effect row in surface form (`Print, Read` or `Print, {e}` or
/// `{e}` for an empty fixed set with a row variable). Caller decides whether
/// to render at all (closed-empty rows are typically suppressed).
fn format_effect_row(row: &EffectRow) -> String {
    let mut parts: Vec<String> = row.fixed.iter().cloned().collect();
    if let Some(tail) = &row.tail {
        parts.push(format!("{{{}}}", tail.0));
    }
    parts.join(", ")
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use crate::typecheck_env::{EffectRow, RowVarId, Ty};

    use super::ty_name;

    #[test]
    fn renders_list_items_with_element_type() {
        assert_eq!(ty_name(&Ty::List(Box::new(Ty::Int))), "List Int");
    }

    #[test]
    fn renders_function_with_closed_empty_effects_omits_can_clause() {
        let ty = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow::closed_empty(),
        };
        assert_eq!(ty_name(&ty), "Int -> Int");
    }

    #[test]
    fn renders_function_with_fixed_effects_appends_can_clause() {
        let ty = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow::closed_from(["Print".to_string()]),
        };
        assert_eq!(ty_name(&ty), "Int -> Int can Print");
    }

    #[test]
    fn renders_function_with_row_variable_only() {
        let ty = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow {
                fixed: BTreeSet::new(),
                tail: Some(RowVarId("e".to_string())),
            },
        };
        assert_eq!(ty_name(&ty), "Int -> Int can {e}");
    }

    #[test]
    fn renders_function_with_fixed_effects_and_row_variable() {
        let ty = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Int),
            effects: EffectRow {
                fixed: BTreeSet::from(["Print".to_string(), "Read".to_string()]),
                tail: Some(RowVarId("e".to_string())),
            },
        };
        assert_eq!(ty_name(&ty), "Int -> Int can Print, Read, {e}");
    }

    #[test]
    fn renders_nested_function_result_with_can_clause_documents_current_shape() {
        // Codex Pass1 raised: `Int -> Int -> Int can Print` is ambiguous
        // about which arrow the `can` attaches to. Today the renderer treats
        // the rightmost `Int can Print` as the result, which matches Goby
        // surface syntax (effects bind to the enclosing function). Pin this
        // observable shape so EP-2 / EP-3 diagnostics work re-discusses
        // explicitly rather than silently reshaping.
        let ty = Ty::Fun {
            params: vec![Ty::Int],
            result: Box::new(Ty::Fun {
                params: vec![Ty::Int],
                result: Box::new(Ty::Int),
                effects: EffectRow::closed_from(["Print".to_string()]),
            }),
            effects: EffectRow::closed_empty(),
        };
        // The result type is rendered via `ty_name`, not
        // `format_fun_segment`, so the inner function is *not* parenthesized
        // and the rendered form remains ambiguous about which arrow the
        // `can` attaches to. Pinning the current observable shape here so
        // any future change to the renderer is visible.
        assert_eq!(ty_name(&ty), "Int -> Int -> Int can Print");
    }

    #[test]
    fn renders_generic_applications_in_haskell_style() {
        let ty = Ty::Con {
            name: "TypeX".to_string(),
            args: vec![Ty::Var("a".to_string()), Ty::Var("b".to_string())],
        };
        assert_eq!(ty_name(&ty), "TypeX a b");
    }

    #[test]
    fn parenthesizes_nested_generic_application_args() {
        let ty = Ty::Con {
            name: "TypeX".to_string(),
            args: vec![
                Ty::Con {
                    name: "TypeY".to_string(),
                    args: vec![Ty::Var("a".to_string()), Ty::Var("b".to_string())],
                },
                Ty::Var("c".to_string()),
            ],
        };
        assert_eq!(ty_name(&ty), "TypeX (TypeY a b) c");
    }
}
