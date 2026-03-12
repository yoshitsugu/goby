use crate::typecheck_env::Ty;

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
        Ty::Fun { params, result } => {
            let mut parts: Vec<String> = params.iter().map(format_fun_segment).collect();
            parts.push(ty_name(result));
            parts.join(" -> ")
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

#[cfg(test)]
mod tests {
    use crate::typecheck_env::Ty;

    use super::ty_name;

    #[test]
    fn renders_list_items_with_element_type() {
        assert_eq!(ty_name(&Ty::List(Box::new(Ty::Int))), "List Int");
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
