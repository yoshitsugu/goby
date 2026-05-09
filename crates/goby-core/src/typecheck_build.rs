use std::collections::HashMap;
use std::path::Path;

use crate::{
    Module,
    ast::TypeDeclaration,
    stdlib::StdlibResolver,
    typecheck::TypecheckError,
    typecheck_annotation::strip_effect_clause,
    typecheck_diag::err_name_ambiguous,
    typecheck_env::{
        EffectRow, GlobalBinding, ImportedEffectDecl, RecordTypeInfo, Ty, TypeEnv, UnionTypeInfo,
        UnionVariantInfo,
    },
    typecheck_types::{ty_from_annotation, ty_from_type_expr},
    typecheck_validate::{
        collect_imported_effect_declarations, effective_imports, inject_imported_symbols,
    },
    types::parse_type_expr,
};

pub(crate) fn build_type_env(module: &Module, stdlib_root: &Path) -> TypeEnv {
    let mut globals = HashMap::new();
    let mut type_aliases = HashMap::new();
    let mut record_types = HashMap::new();
    let mut union_types = HashMap::new();
    for decl in &module.declarations {
        if let Some(annotation) = decl.type_annotation.as_deref() {
            let base = strip_effect_clause(annotation);
            if crate::types::parse_function_type(base).is_some() {
                // EP-1c: route through ty_from_annotation so the full
                // function type — including its top-level `can` clause —
                // is lifted into Ty::Fun.effects. Without this, callers
                // looking up `decl` would observe a closed-empty row even
                // when the source declares `can Print` etc.
                insert_global_symbol(
                    &mut globals,
                    decl.name.clone(),
                    ty_from_annotation(annotation),
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
        "__goby_string_length".to_string(),
        Ty::Fun {
            params: vec![Ty::Str],
            result: Box::new(Ty::Int),
            effects: EffectRow::closed_empty(),
        },
        "runtime intrinsic `__goby_string_length`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_env_fetch_env_var".to_string(),
        Ty::Fun {
            params: vec![Ty::Str],
            result: Box::new(Ty::Str),
            effects: EffectRow::closed_empty(),
        },
        "runtime intrinsic `__goby_env_fetch_env_var`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_string_each_grapheme".to_string(),
        Ty::Fun {
            params: vec![Ty::Str],
            result: Box::new(Ty::Int),
            effects: EffectRow::closed_empty(),
        },
        "runtime intrinsic `__goby_string_each_grapheme`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_list_push_string".to_string(),
        Ty::Fun {
            params: vec![Ty::List(Box::new(Ty::Str)), Ty::Str],
            result: Box::new(Ty::List(Box::new(Ty::Str))),
            effects: EffectRow::closed_empty(),
        },
        "runtime intrinsic `__goby_list_push_string`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_list_concat".to_string(),
        Ty::Fun {
            params: vec![
                Ty::List(Box::new(Ty::Var("a".to_string()))),
                Ty::List(Box::new(Ty::Var("a".to_string()))),
            ],
            result: Box::new(Ty::List(Box::new(Ty::Var("a".to_string())))),
            effects: EffectRow::closed_empty(),
        },
        "runtime intrinsic `__goby_list_concat`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_list_join_string".to_string(),
        Ty::Fun {
            params: vec![Ty::List(Box::new(Ty::Str)), Ty::Str],
            result: Box::new(Ty::Str),
            effects: EffectRow::closed_empty(),
        },
        "runtime intrinsic `__goby_list_join_string`".to_string(),
    );
    insert_global_symbol(
        &mut globals,
        "__goby_embeded_effect_stdout_handler".to_string(),
        Ty::Fun {
            params: vec![Ty::Str],
            result: Box::new(Ty::Unit),
            effects: EffectRow::closed_empty(),
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
    inject_imported_effect_symbols(
        &collect_imported_effect_declarations(module, stdlib_root),
        &mut globals,
    );
    inject_imported_type_constructors(module, &mut globals, &mut record_types, stdlib_root);
    inject_type_constructors(
        module,
        &mut globals,
        &mut type_aliases,
        &mut record_types,
        &mut union_types,
    );
    inject_effect_symbols(module, &mut globals);

    TypeEnv {
        globals,
        locals: HashMap::new(),
        type_aliases,
        record_types,
        union_types,
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
    Err(err_name_ambiguous(
        None,
        name,
        &sorted_sources,
        None, // no span available: global symbol has no declaration span
    ))
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

fn inject_imported_effect_symbols(
    imported_effects: &[ImportedEffectDecl],
    globals: &mut HashMap<String, GlobalBinding>,
) {
    for imported in imported_effects {
        if !imported.imported_via_prelude {
            continue;
        }
        for member in &imported.decl.members {
            let qualified_key = format!("{}.{}", imported.decl.name, member.name);
            let ty = match parse_type_expr(&member.type_annotation)
                .map(|expr| ty_from_type_expr(&expr))
            {
                Some(fun_ty @ Ty::Fun { .. }) => fun_ty,
                _ => Ty::Unknown,
            };
            let source = format!(
                "effect `{}` member from import `{}`",
                imported.decl.name, imported.source_module
            );
            insert_global_symbol(globals, qualified_key, ty.clone(), source.clone());
            insert_global_symbol(globals, member.name.clone(), ty, source);
        }
    }
}

fn inject_imported_type_constructors(
    module: &Module,
    globals: &mut HashMap<String, GlobalBinding>,
    record_types: &mut HashMap<String, RecordTypeInfo>,
    stdlib_root: &Path,
) {
    let resolver = StdlibResolver::new(stdlib_root.to_path_buf());
    for import in effective_imports(module, &resolver) {
        let Ok(resolved) = resolver.resolve_module(&import.module_path) else {
            continue;
        };
        for ty_decl in &resolved.module.type_declarations {
            match (&import.kind, ty_decl) {
                (
                    crate::ImportKind::Selective(names),
                    TypeDeclaration::Record {
                        name,
                        type_params,
                        constructor,
                        fields,
                    },
                ) if names.iter().any(|selected| selected == name) => {
                    let is_generic = !type_params.is_empty();
                    let mut field_map = HashMap::new();
                    let params: Vec<Ty> = fields
                        .iter()
                        .map(|f| ty_from_annotation(&f.type_annotation))
                        .collect();
                    for (field_name, field_ty) in fields
                        .iter()
                        .map(|f| (&f.name, ty_from_annotation(&f.type_annotation)))
                    {
                        field_map.insert(field_name.clone(), field_ty);
                    }
                    let result = union_or_record_result_template(name, type_params);
                    // Same template-leakage hazard as the local
                    // generic-record path; defer the real signature to
                    // GU-S3 by registering `Ty::Unknown` for now.
                    let ctor_ty = if is_generic {
                        Ty::Unknown
                    } else {
                        Ty::Fun {
                            params,
                            result: Box::new(result),
                            effects: EffectRow::closed_empty(),
                        }
                    };
                    insert_global_symbol(
                        globals,
                        constructor.clone(),
                        ctor_ty,
                        format!(
                            "import type `{}` ({}) from `{}`",
                            name, constructor, import.module_path
                        ),
                    );
                    record_types.insert(
                        constructor.clone(),
                        RecordTypeInfo {
                            type_name: name.clone(),
                            type_params: type_params.clone(),
                            constructor: constructor.clone(),
                            fields: field_map,
                        },
                    );
                }
                _ => {}
            }
        }
    }
}

pub(crate) fn inject_type_constructors(
    module: &Module,
    globals: &mut HashMap<String, GlobalBinding>,
    type_aliases: &mut HashMap<String, Ty>,
    record_types: &mut HashMap<String, RecordTypeInfo>,
    union_types: &mut HashMap<String, UnionTypeInfo>,
) {
    for ty_decl in &module.type_declarations {
        match ty_decl {
            TypeDeclaration::Alias { name, target } => {
                type_aliases.insert(name.clone(), ty_from_annotation(target));
            }
            TypeDeclaration::Union {
                name,
                type_params,
                variants,
            } => {
                let is_generic = !type_params.is_empty();
                let result_template = union_or_record_result_template(name, type_params);
                let mut variant_infos = Vec::with_capacity(variants.len());
                for (idx, variant) in variants.iter().enumerate() {
                    let arg_types: Vec<Ty> = variant
                        .args
                        .iter()
                        .map(|annotation| ty_from_annotation(annotation))
                        .collect();
                    // Generic union constructors carry `Ty::Var(...)`
                    // templates that must be freshened per use site.
                    // The freshening helper only lands in GU-S3, so for
                    // now register generic constructors as `Ty::Unknown`
                    // to avoid the unfreshened template leaking into
                    // unification (which would reject e.g. `Just 42`
                    // against an expected `Maybe Int`). Non-generic
                    // unions are unaffected and keep the previous
                    // direct-`Ty::Con` / `Ty::Fun` shapes.
                    let ctor_ty = if is_generic {
                        Ty::Unknown
                    } else if arg_types.is_empty() {
                        result_template.clone()
                    } else {
                        Ty::Fun {
                            params: arg_types.clone(),
                            result: Box::new(result_template.clone()),
                            effects: EffectRow::closed_empty(),
                        }
                    };
                    insert_global_symbol(
                        globals,
                        variant.ctor.clone(),
                        ctor_ty.clone(),
                        format!("type `{}` constructor", name),
                    );
                    insert_global_symbol(
                        globals,
                        format!("{}.{}", name, variant.ctor),
                        ctor_ty,
                        format!("type `{}` constructor", name),
                    );
                    variant_infos.push(UnionVariantInfo {
                        ctor: variant.ctor.clone(),
                        variant_index: idx as u32,
                        arg_types,
                    });
                }
                union_types.insert(
                    name.clone(),
                    UnionTypeInfo {
                        type_name: name.clone(),
                        type_params: type_params.clone(),
                        variants: variant_infos,
                    },
                );
            }
            TypeDeclaration::Record {
                name,
                type_params,
                constructor,
                fields,
            } => {
                let is_generic = !type_params.is_empty();
                let mut field_map = HashMap::new();
                let params: Vec<Ty> = fields
                    .iter()
                    .map(|f| ty_from_annotation(&f.type_annotation))
                    .collect();
                for (field_name, field_ty) in fields
                    .iter()
                    .map(|f| (&f.name, ty_from_annotation(&f.type_annotation)))
                {
                    field_map.insert(field_name.clone(), field_ty);
                }
                let result = union_or_record_result_template(name, type_params);
                // Generic record constructors face the same template-leakage
                // hazard as generic unions; defer real signatures to GU-S3.
                let ctor_ty = if is_generic {
                    Ty::Unknown
                } else {
                    Ty::Fun {
                        params,
                        result: Box::new(result),
                        effects: EffectRow::closed_empty(),
                    }
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
                        type_params: type_params.clone(),
                        constructor: constructor.clone(),
                        fields: field_map,
                    },
                );
            }
        }
    }
}

/// Build the `Ty::Con { name, args }` template that a union/record
/// constructor returns. Type parameters appear as `Ty::Var(name)`
/// templates; `freshen_type_scheme` later substitutes fresh inference
/// variables per use site.
fn union_or_record_result_template(name: &str, type_params: &[String]) -> Ty {
    let args = type_params
        .iter()
        .map(|p| Ty::Var(p.clone()))
        .collect();
    Ty::Con {
        name: name.to_string(),
        args,
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
