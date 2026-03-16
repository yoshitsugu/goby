use std::collections::HashSet;

use crate::{
    Module,
    ast::TypeDeclaration,
    typecheck::TypecheckError,
    typecheck_env::Ty,
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
                span: None, // no span available: requires Expr/Stmt span (D1a-iii)
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
                    span: None, // no span available: requires Expr/Stmt span (D1a-iii)
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
                            span: None, // no span available: requires Expr/Stmt span (D1a-iii)
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
                            span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                            message: format!("duplicate field `{}` in type `{}`", field.name, name),
                        });
                    }
                    let parsed = parse_type_expr(&field.ty).ok_or_else(|| TypecheckError {
                        declaration: Some(name.clone()),
                        span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                        message: format!("invalid field type `{}`", field.ty),
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
                span: None, // no span available: requires Expr/Stmt span (D1a-iii)
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
    let s = s.trim();
    let Some(type_expr) = parse_type_expr(s) else {
        return Ty::Unknown;
    };
    ty_from_type_expr(&type_expr)
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
