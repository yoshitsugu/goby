use std::collections::HashMap;
use std::path::Path;

use crate::{
    Module,
    ast::TypeDeclaration,
    typecheck::{TypecheckError, ty_from_annotation, ty_from_type_expr},
    typecheck_annotation::strip_effect_clause,
    typecheck_env::{GlobalBinding, RecordTypeInfo, Ty, TypeEnv},
    typecheck_validate::inject_imported_symbols,
    types::parse_type_expr,
};

pub(crate) fn build_type_env(module: &Module, stdlib_root: &Path) -> TypeEnv {
    let mut globals = HashMap::new();
    let mut type_aliases = HashMap::new();
    let mut record_types = HashMap::new();
    for decl in &module.declarations {
        if let Some(annotation) = decl.type_annotation.as_deref() {
            let base = strip_effect_clause(annotation);
            if let Some(ft) = crate::types::parse_function_type(base) {
                let params: Vec<Ty> = ft.arguments.iter().map(|a| ty_from_annotation(a)).collect();
                let result = ty_from_annotation(&ft.result);
                insert_global_symbol(
                    &mut globals,
                    decl.name.clone(),
                    Ty::Fun {
                        params,
                        result: Box::new(result),
                    },
                    format!("declaration `{}`", decl.name),
                );
            } else {
                insert_global_symbol(
                    &mut globals,
                    decl.name.clone(),
                    ty_from_annotation(base.trim()),
                    format!("declaration `{}`", decl.name),
                );
            }
        }
    }
    insert_global_symbol(
        &mut globals,
        "print".to_string(),
        Ty::Fun {
            params: vec![Ty::Unknown],
            result: Box::new(Ty::Unit),
        },
        "builtin `print`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_string_length".to_string(),
        Ty::Fun {
            params: vec![Ty::Str],
            result: Box::new(Ty::Int),
        },
        "runtime intrinsic `__goby_string_length`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_env_fetch_env_var".to_string(),
        Ty::Fun {
            params: vec![Ty::Str],
            result: Box::new(Ty::Str),
        },
        "runtime intrinsic `__goby_env_fetch_env_var`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_string_each_grapheme".to_string(),
        Ty::Fun {
            params: vec![Ty::Str],
            result: Box::new(Ty::Int),
        },
        "runtime intrinsic `__goby_string_each_grapheme`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_list_push_string".to_string(),
        Ty::Fun {
            params: vec![Ty::List(Box::new(Ty::Str)), Ty::Str],
            result: Box::new(Ty::List(Box::new(Ty::Str))),
        },
        "runtime intrinsic `__goby_list_push_string`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_embeded_effect_stdout_handler".to_string(),
        Ty::Fun {
            params: vec![Ty::Str],
            result: Box::new(Ty::Unit),
        },
        "runtime intrinsic `__goby_embeded_effect_stdout_handler`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_embeded_effect_stdin_handler".to_string(),
        Ty::Unknown,
        "runtime intrinsic `__goby_embeded_effect_stdin_handler`".to_string(),
    );
    inject_imported_symbols(module, &mut globals, stdlib_root);
    inject_type_constructors(module, &mut globals, &mut type_aliases, &mut record_types);
    inject_effect_symbols(module, &mut globals);

    TypeEnv {
        globals,
        locals: HashMap::new(),
        type_aliases,
        record_types,
    }
}

pub(crate) fn ensure_no_ambiguous_globals(env: &TypeEnv) -> Result<(), TypecheckError> {
    let mut ambiguous: Vec<(&String, &Vec<String>)> = env
        .globals
        .iter()
        .filter_map(|(name, binding)| match binding {
            GlobalBinding::Ambiguous { sources } => {
                let has_import_source = sources.iter().any(|source| source.starts_with("import `"));
                if has_import_source {
                    Some((name, sources))
                } else {
                    None
                }
            }
            GlobalBinding::Resolved { .. } => None,
        })
        .collect();
    ambiguous.sort_by(|(left, _), (right, _)| left.cmp(right));
    let Some((name, sources)) = ambiguous.first() else {
        return Ok(());
    };
    let mut sorted_sources = (*sources).clone();
    sorted_sources.sort();
    Err(TypecheckError {
        declaration: None,
        span: None,
        message: format!(
            "name `{}` is ambiguous due to name resolution collision: {}",
            name,
            sorted_sources.join(", ")
        ),
    })
}

pub(crate) fn inject_effect_symbols(module: &Module, globals: &mut HashMap<String, GlobalBinding>) {
    for effect_decl in &module.effect_declarations {
        for member in &effect_decl.members {
            let qualified_key = format!("{}.{}", effect_decl.name, member.name);
            let ty = match parse_type_expr(&member.type_annotation)
                .map(|expr| ty_from_type_expr(&expr))
            {
                Some(fun_ty @ Ty::Fun { .. }) => fun_ty,
                _ => Ty::Unknown,
            };
            insert_global_symbol(
                globals,
                qualified_key,
                ty.clone(),
                format!("effect `{}` member", effect_decl.name),
            );
            insert_global_symbol(
                globals,
                member.name.clone(),
                ty,
                format!("effect `{}` member", effect_decl.name),
            );
        }
    }
}

pub(crate) fn inject_type_constructors(
    module: &Module,
    globals: &mut HashMap<String, GlobalBinding>,
    type_aliases: &mut HashMap<String, Ty>,
    record_types: &mut HashMap<String, RecordTypeInfo>,
) {
    for ty_decl in &module.type_declarations {
        match ty_decl {
            TypeDeclaration::Alias { name, target } => {
                type_aliases.insert(name.clone(), ty_from_annotation(target));
            }
            TypeDeclaration::Union { name, constructors } => {
                for constructor in constructors {
                    insert_global_symbol(
                        globals,
                        format!("{}.{}", name, constructor),
                        Ty::Con {
                            name: name.clone(),
                            args: Vec::new(),
                        },
                        format!("type `{}` constructor", name),
                    );
                }
            }
            TypeDeclaration::Record {
                name,
                constructor,
                fields,
            } => {
                let mut field_map = HashMap::new();
                let params: Vec<Ty> = fields.iter().map(|f| ty_from_annotation(&f.ty)).collect();
                for (field_name, field_ty) in
                    fields.iter().map(|f| (&f.name, ty_from_annotation(&f.ty)))
                {
                    field_map.insert(field_name.clone(), field_ty);
                }
                let result = Ty::Con {
                    name: name.clone(),
                    args: Vec::new(),
                };
                let ctor_ty = Ty::Fun {
                    params,
                    result: Box::new(result),
                };
                insert_global_symbol(
                    globals,
                    constructor.clone(),
                    ctor_ty.clone(),
                    format!("type `{}` constructor", name),
                );
                insert_global_symbol(
                    globals,
                    format!("{}.{}", name, constructor),
                    ctor_ty,
                    format!("type `{}` constructor", name),
                );
                record_types.insert(
                    constructor.clone(),
                    RecordTypeInfo {
                        type_name: name.clone(),
                        fields: field_map,
                    },
                );
            }
        }
    }
}

pub(crate) fn insert_global_symbol(
    globals: &mut HashMap<String, GlobalBinding>,
    symbol: String,
    ty: Ty,
    source: String,
) {
    let existing = globals.get(&symbol).cloned();
    match existing {
        None => {
            globals.insert(symbol, GlobalBinding::Resolved { ty, source });
        }
        Some(GlobalBinding::Resolved {
            source: existing_source,
            ..
        }) => {
            if existing_source == source {
                return;
            }
            globals.insert(
                symbol,
                GlobalBinding::Ambiguous {
                    sources: vec![existing_source, source],
                },
            );
        }
        Some(GlobalBinding::Ambiguous { mut sources }) => {
            if !sources.contains(&source) {
                sources.push(source);
            }
            globals.insert(symbol, GlobalBinding::Ambiguous { sources });
        }
    }
}
