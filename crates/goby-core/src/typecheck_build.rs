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
        CTOR_SOURCE_PREFIX, CTOR_SOURCE_SUFFIX, CtorOrigin, EffectRow, GlobalBinding,
        ImportedEffectDecl, RecordTypeInfo, Ty, TypeEnv, UnionTypeInfo, UnionVariantInfo,
    },
    typecheck_types::{ty_from_annotation, ty_from_type_expr},
    typecheck_validate::{
        collect_imported_effect_declarations, effective_imports, inject_imported_symbols,
    },
    types::parse_type_expr,
};

/// GU-S3 CA-3b: render the `source` field of a ctor `GlobalBinding`. The
/// detector `TypeEnv::is_ctor_binding` matches on the same
/// `CTOR_SOURCE_PREFIX` / `CTOR_SOURCE_SUFFIX` constants, so updates to the
/// rendering must go through this helper to keep both sides in sync.
fn ctor_source(type_name: &str) -> String {
    format!("{}{}{}", CTOR_SOURCE_PREFIX, type_name, CTOR_SOURCE_SUFFIX)
}

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
    // GU-S3 IU-2: register **local** type constructors first, then
    // imported. The imported pass uses the now-populated `union_types` /
    // `globals` to skip any name that would collide with a local
    // declaration; this preserves the legacy "local wins" semantics that
    // the previous import-first ordering broke (`insert_global_symbol`
    // collapses different sources into `Ambiguous`, which leaks `Ty::Unknown`
    // through `lookup`).
    inject_type_constructors(
        module,
        &mut globals,
        &mut type_aliases,
        &mut record_types,
        &mut union_types,
    );
    inject_imported_type_constructors(
        module,
        &mut globals,
        &mut record_types,
        &mut union_types,
        stdlib_root,
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

/// GU-S3 IU-2/IU-3: render the `source` field of an **imported** ctor
/// `GlobalBinding`. Final shape:
/// `` "type `<TYPE>` from `<MOD>` constructor" ``.
///
/// Both `CTOR_SOURCE_PREFIX` (`` "type `" ``) and `CTOR_SOURCE_SUFFIX`
/// (`` "` constructor" ``) already include one backtick at the
/// type-name boundary, so the imported-form infix is built from the same
/// const but with the type-name section closed before ` from ``<MOD>`` and
/// reopened by `CTOR_SOURCE_SUFFIX`. This keeps `is_ctor_binding`
/// (which only looks at the prefix/suffix) happy for imported sources.
fn ctor_source_imported(type_name: &str, source_module: &str) -> String {
    // Codex pass-1 follow-up: encode the prefix/suffix backtick contract
    // so a future drift in `CTOR_SOURCE_PREFIX` / `CTOR_SOURCE_SUFFIX`
    // (e.g. dropping the wrapping `` ` ``) trips immediately in debug
    // builds rather than silently producing a malformed source string
    // that still satisfies the loose `is_ctor_binding` predicate.
    debug_assert!(
        CTOR_SOURCE_PREFIX.ends_with('`') && CTOR_SOURCE_SUFFIX.starts_with('`'),
        "ctor source prefix/suffix must wrap the type name in backticks"
    );
    format!(
        "{}{}` from `{}{}",
        CTOR_SOURCE_PREFIX, type_name, source_module, CTOR_SOURCE_SUFFIX
    )
}

fn inject_imported_type_constructors(
    module: &Module,
    globals: &mut HashMap<String, GlobalBinding>,
    record_types: &mut HashMap<String, RecordTypeInfo>,
    union_types: &mut HashMap<String, UnionTypeInfo>,
    stdlib_root: &Path,
) {
    // GU-S3 IU-2 shadow guard: collect local type names and ctor names
    // from the **AST** (Codex pass-1 follow-up). Walking
    // `module.type_declarations` directly is more robust than reading
    // back `union_types` / `record_types` because:
    //   - `record_types` is keyed on **constructor**, not type_name, so
    //     deriving local type names from it loses ones whose constructor
    //     was overwritten elsewhere;
    //   - `type_aliases` (e.g. `type Maybe = Int`) lives in a separate
    //     map; missing it would let an imported `Maybe` union slip past
    //     the guard and shadow the local alias semantically.
    // The imported pass uses these sets to skip any name a local
    // declaration already owns, preserving "local wins" without a
    // special `local-shadows-imported` axis in `resolve_ctor`.
    let mut local_type_names: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut local_ctor_names: std::collections::HashSet<String> = std::collections::HashSet::new();
    for ty_decl in &module.type_declarations {
        match ty_decl {
            TypeDeclaration::Alias { name, .. } => {
                local_type_names.insert(name.clone());
            }
            TypeDeclaration::Union { name, variants, .. } => {
                local_type_names.insert(name.clone());
                for v in variants {
                    local_ctor_names.insert(v.ctor.clone());
                }
            }
            TypeDeclaration::Record {
                name, constructor, ..
            } => {
                local_type_names.insert(name.clone());
                local_ctor_names.insert(constructor.clone());
            }
        }
    }

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
                    // Shadow guard: a local declaration with the same
                    // type name OR a local ctor with the same constructor
                    // name wins; skip the imported registration entirely.
                    if local_type_names.contains(name) || local_ctor_names.contains(constructor) {
                        continue;
                    }
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
                    // GU-S3 IU-2: drop the legacy `is_generic -> Ty::Unknown`
                    // placeholder. CA-3b/GR-1 already make
                    // `infer_expr_ty` Var/Qualified/RecordConstruct freshen
                    // ctor lookups via `freshen_type_scheme`, so registering
                    // the real `Ty::Fun { params, result }` template is now
                    // safe and lets imported generic records (e.g.
                    // `import box (Box); Box(value: 42) : Box Int`) infer
                    // a concrete type instead of leaking `Ty::Unknown`.
                    let ctor_ty = Ty::Fun {
                        params,
                        result: Box::new(result),
                        effects: EffectRow::closed_empty(),
                    };
                    let source = ctor_source_imported(name, &import.module_path);
                    insert_global_symbol(globals, constructor.clone(), ctor_ty, source);
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
                (
                    crate::ImportKind::Selective(names),
                    TypeDeclaration::Union {
                        name,
                        type_params,
                        variants,
                    },
                ) if names.iter().any(|selected| selected == name) => {
                    // Shadow guard: a local declaration with the same
                    // type name wins; skip the entire imported union
                    // registration so neither `union_types` nor `globals`
                    // see it.
                    if local_type_names.contains(name) {
                        continue;
                    }
                    // Per-variant ctor shadow check. If **any** variant's
                    // ctor name collides with a local ctor, skip the
                    // entire imported union registration (Codex pass-1
                    // follow-up): otherwise `union_types` would still
                    // carry the imported union and `resolve_ctor_without_pin`
                    // would walk it, turning a local-only `Just` lookup
                    // into `Ambiguous { Local Maybe, Imported Maybe }`.
                    if variants.iter().any(|v| local_ctor_names.contains(&v.ctor)) {
                        continue;
                    }
                    let result_template = union_or_record_result_template(name, type_params);
                    let mut variant_infos = Vec::with_capacity(variants.len());
                    let source = ctor_source_imported(name, &import.module_path);
                    for (idx, variant) in variants.iter().enumerate() {
                        let arg_types: Vec<Ty> = variant
                            .args
                            .iter()
                            .map(|annotation| ty_from_annotation(annotation))
                            .collect();
                        let ctor_ty = if arg_types.is_empty() {
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
                            format!("{}.{}", name, variant.ctor),
                            ctor_ty.clone(),
                            source.clone(),
                        );
                        insert_global_symbol(
                            globals,
                            variant.ctor.clone(),
                            ctor_ty,
                            source.clone(),
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
                            origin: CtorOrigin::Imported {
                                source_module: import.module_path.clone(),
                            },
                        },
                    );
                }
                _ => {}
            }
        }
    }
}

/// GU-S3 IU-3 test helper: returns true iff `source` is the **local**
/// ctor source rendered by `ctor_source(name)` — i.e. has no
/// ` from `<MOD>`` infix. The shadow-guard collection moved to walking
/// the AST directly (Codex pass-1 follow-up), so this helper is now
/// only consumed by `ctor_source_tests` to pin the source-string shape
/// contract.
#[cfg(test)]
fn is_local_ctor_source(source: &str) -> bool {
    source.starts_with(CTOR_SOURCE_PREFIX)
        && source.ends_with(CTOR_SOURCE_SUFFIX)
        && !source.contains("` from `")
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
                let result_template = union_or_record_result_template(name, type_params);
                let mut variant_infos = Vec::with_capacity(variants.len());
                for (idx, variant) in variants.iter().enumerate() {
                    let arg_types: Vec<Ty> = variant
                        .args
                        .iter()
                        .map(|annotation| ty_from_annotation(annotation))
                        .collect();
                    // GU-S3 CA-3b: previously, generic union ctors were
                    // registered as `Ty::Unknown` to avoid the unfreshened
                    // `Ty::Var` template leaking into unification. Now that
                    // `infer_expr_ty` Var/Qualified arms freshen ctor lookups
                    // through `freshen_type_scheme`, register the real
                    // template directly. Non-generic ctors are unaffected
                    // (`Ty::Con` for nullary, `Ty::Fun { params, result }`
                    // for arg-bearing).
                    let ctor_ty = if arg_types.is_empty() {
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
                        ctor_source(name),
                    );
                    insert_global_symbol(
                        globals,
                        format!("{}.{}", name, variant.ctor),
                        ctor_ty,
                        ctor_source(name),
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
                        origin: CtorOrigin::Local,
                    },
                );
            }
            TypeDeclaration::Record {
                name,
                type_params,
                constructor,
                fields,
            } => {
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
                // GU-S3 GR-1: generic record ctors used to be registered as
                // `Ty::Unknown` to avoid leaking unfreshened `Ty::Var`
                // templates into unification. Now that
                // `infer_expr_ty`'s Var/Qualified arm freshens ctor
                // lookups (CA-3b) and `Expr::RecordConstruct` unifies
                // field values against freshened field templates (GR-2),
                // we register the real `Ty::Fun { params, result }`
                // template just like the non-generic case (which is the
                // same shape with empty `type_params`).
                let ctor_ty = Ty::Fun {
                    params,
                    result: Box::new(result),
                    effects: EffectRow::closed_empty(),
                };
                insert_global_symbol(
                    globals,
                    constructor.clone(),
                    ctor_ty.clone(),
                    ctor_source(name),
                );
                insert_global_symbol(
                    globals,
                    format!("{}.{}", name, constructor),
                    ctor_ty,
                    ctor_source(name),
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
    let args = type_params.iter().map(|p| Ty::Var(p.clone())).collect();
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

#[cfg(test)]
mod ctor_source_tests {
    use super::*;
    use crate::typecheck_env::TypeEnv;

    #[test]
    fn local_ctor_source_format_matches_is_ctor_binding() {
        // GU-S3 IU-3: pin the local ctor source string against the
        // `is_ctor_binding` detector. Drift in either side breaks the
        // ctor lookup path used by `infer_expr_ty` Var/Qualified arms.
        let source = ctor_source("Maybe");
        assert_eq!(source, "type `Maybe` constructor");
        assert!(is_local_ctor_source(&source));
        let mut env = TypeEnv::empty();
        env.globals.insert(
            "Just".to_string(),
            GlobalBinding::Resolved {
                ty: Ty::Unknown,
                source,
            },
        );
        assert!(env.is_ctor_binding("Just"));
    }

    #[test]
    fn imported_ctor_source_format_matches_is_ctor_binding() {
        // GU-S3 IU-2/IU-3: imported ctor source must satisfy
        // `is_ctor_binding` (so resolver freshen path triggers) AND
        // must NOT be classified as local (so the IU-2 shadow guard
        // can distinguish local from imported sources).
        let source = ctor_source_imported("Maybe", "goby/maybe");
        assert_eq!(source, "type `Maybe` from `goby/maybe` constructor");
        assert!(!is_local_ctor_source(&source));
        let mut env = TypeEnv::empty();
        env.globals.insert(
            "Just".to_string(),
            GlobalBinding::Resolved {
                ty: Ty::Unknown,
                source,
            },
        );
        assert!(env.is_ctor_binding("Just"));
    }

    #[test]
    fn local_and_imported_ctor_sources_are_distinct() {
        // Different `source` strings ensure `insert_global_symbol`
        // would produce `Ambiguous` if both fired for the same symbol
        // — the IU-2 shadow guard prevents that, but the underlying
        // distinguishability is what makes the guard non-vacuous.
        let local = ctor_source("Maybe");
        let imported = ctor_source_imported("Maybe", "goby/maybe");
        assert_ne!(local, imported);
    }
}

#[cfg(test)]
mod shadow_guard_tests {
    //! GU-S3 IU-2 (Codex pass-1): integration-shape pins for the imported
    //! type-constructor pass. They build a small in-memory `Module` plus
    //! an on-disk stdlib module and check that the local-shadow guard
    //! suppresses the imported registration entirely (no ctor in
    //! `globals`, no entry in `union_types`, the local origin survives).
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    use crate::ast::{Module, TypeDeclaration, UnionVariant};
    use crate::typecheck_env::CtorOrigin;
    use crate::{ImportDecl, ImportKind};

    use super::*;

    struct TempDirGuard {
        path: PathBuf,
    }

    impl TempDirGuard {
        fn new(label: &str) -> Self {
            let nanos = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("clock should be monotonic enough for tests")
                .as_nanos();
            let path = std::env::temp_dir().join(format!(
                "goby_shadow_guard_{}_{}_{}",
                label,
                std::process::id(),
                nanos
            ));
            fs::create_dir_all(&path).expect("temp directory should be creatable");
            Self { path }
        }
    }

    impl Drop for TempDirGuard {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn empty_module_with_imports_and_types(
        imports: Vec<ImportDecl>,
        types: Vec<TypeDeclaration>,
    ) -> Module {
        Module {
            imports,
            type_declarations: types,
            effect_declarations: vec![],
            embed_declarations: vec![],
            declarations: vec![],
        }
    }

    fn write_module(stdlib_root: &std::path::Path, module_path: &str, contents: &str) {
        let path = stdlib_root.join(format!("{}.gb", module_path));
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("dir should be creatable");
        }
        fs::write(path, contents).expect("module should be writable");
    }

    fn import_selective(module_path: &str, names: &[&str]) -> ImportDecl {
        ImportDecl {
            module_path: module_path.to_string(),
            module_path_span: None,
            kind: ImportKind::Selective(names.iter().map(|s| s.to_string()).collect()),
            kind_span: None,
        }
    }

    #[test]
    fn local_alias_with_same_type_name_skips_imported_union() {
        // Pass-1 (1): `type Maybe = Int` must shadow `import (Maybe)` so
        // the imported union is not registered.
        let sandbox = TempDirGuard::new("alias_skips_imported");
        let stdlib_root = sandbox.path.join("stdlib");
        fs::create_dir_all(stdlib_root.join("goby")).expect("dir");
        write_module(
            &stdlib_root,
            "goby/maybe",
            "type Maybe a = Just(a) | Nothing\n",
        );
        let module = empty_module_with_imports_and_types(
            vec![import_selective("goby/maybe", &["Maybe"])],
            vec![TypeDeclaration::Alias {
                name: "Maybe".to_string(),
                target: "Int".to_string(),
            }],
        );
        let env = build_type_env(&module, &stdlib_root);
        assert!(
            !env.union_types.contains_key("Maybe"),
            "imported union must be skipped when a local alias shadows the type name"
        );
        assert!(!env.globals.contains_key("Just"));
        assert!(!env.globals.contains_key("Maybe.Just"));
    }

    #[test]
    fn local_union_skips_imported_union_keeping_local_origin() {
        // Pass-1 (3): the local `Maybe` keeps `origin: Local`, the
        // imported `Maybe` is dropped entirely, and `globals["Just"]`
        // is the local-source ctor (not `Ambiguous`).
        let sandbox = TempDirGuard::new("local_skips_imported");
        let stdlib_root = sandbox.path.join("stdlib");
        fs::create_dir_all(stdlib_root.join("goby")).expect("dir");
        write_module(
            &stdlib_root,
            "goby/maybe",
            "type Maybe a = Just(a) | Nothing\n",
        );
        let module = empty_module_with_imports_and_types(
            vec![import_selective("goby/maybe", &["Maybe"])],
            vec![TypeDeclaration::Union {
                name: "Maybe".to_string(),
                type_params: vec!["a".to_string()],
                variants: vec![
                    UnionVariant {
                        ctor: "Just".to_string(),
                        args: vec!["a".to_string()],
                    },
                    UnionVariant {
                        ctor: "Nothing".to_string(),
                        args: vec![],
                    },
                ],
            }],
        );
        let env = build_type_env(&module, &stdlib_root);
        let info = env
            .union_types
            .get("Maybe")
            .expect("local `Maybe` must remain");
        assert_eq!(info.origin, CtorOrigin::Local);
        let just = env.globals.get("Just").expect("local `Just` must remain");
        match just {
            GlobalBinding::Resolved { source, .. } => {
                assert_eq!(source, &ctor_source("Maybe"), "local source expected");
            }
            other => panic!("expected Resolved local Just, got {:?}", other),
        }
    }

    #[test]
    fn local_ctor_only_skips_entire_imported_union() {
        // Pass-1 (2): when only the ctor name (`Just`) collides — the
        // local type name is different — the *entire* imported union
        // must still be skipped. Otherwise `union_types` carries the
        // imported union and `resolve_ctor_without_pin` walks it,
        // turning a local-only `Just` lookup into ambiguous.
        let sandbox = TempDirGuard::new("ctor_only_collide");
        let stdlib_root = sandbox.path.join("stdlib");
        fs::create_dir_all(stdlib_root.join("goby")).expect("dir");
        write_module(
            &stdlib_root,
            "goby/maybe",
            "type Maybe a = Just(a) | Nothing\n",
        );
        let module = empty_module_with_imports_and_types(
            vec![import_selective("goby/maybe", &["Maybe"])],
            vec![TypeDeclaration::Union {
                name: "MyOption".to_string(),
                type_params: vec!["a".to_string()],
                variants: vec![UnionVariant {
                    ctor: "Just".to_string(),
                    args: vec!["a".to_string()],
                }],
            }],
        );
        let env = build_type_env(&module, &stdlib_root);
        assert!(
            !env.union_types.contains_key("Maybe"),
            "imported `Maybe` must be skipped when any of its variants collides with a local ctor"
        );
        assert_eq!(env.union_types.len(), 1);
        assert!(env.union_types.contains_key("MyOption"));
    }

    #[test]
    #[ignore = "GU-S3 IU follow-up: effect member names share the value namespace with ctors. \
                A local `effect E\\n  Just : ...` would currently mix into `globals[\"Just\"]` \
                with the imported ctor as Ambiguous, and `ensure_no_ambiguous_globals` does not \
                catch it because the predicate only fires on `import \\`` sources. Tracked as \
                a follow-up — needs either a same-namespace collision diagnostic or a \
                lowercase-only restriction on effect members."]
    fn local_effect_member_with_ctor_name_should_collide_with_imported_union() {
        // Codex pass-2 (1): pin the known hole. Today this is silently
        // accepted; once the follow-up lands, the test should fail to
        // typecheck (or return an `Ambiguous` diagnostic) and the
        // `#[ignore]` is removed.
        let sandbox = TempDirGuard::new("effect_ctor_collide");
        let stdlib_root = sandbox.path.join("stdlib");
        fs::create_dir_all(stdlib_root.join("goby")).expect("dir");
        write_module(
            &stdlib_root,
            "goby/maybe",
            "type Maybe a = Just(a) | Nothing\n",
        );
        // Synthesised module with a CamelCase effect member `Just`.
        // This must not silently coexist with imported `Just`.
        let module = Module {
            imports: vec![import_selective("goby/maybe", &["Maybe"])],
            type_declarations: vec![],
            effect_declarations: vec![crate::ast::EffectDecl {
                name: "E".to_string(),
                type_params: vec![],
                members: vec![crate::ast::EffectMember {
                    name: "Just".to_string(),
                    type_annotation: "Int -> Unit".to_string(),
                    span: crate::Span::point(1, 1),
                }],
                span: crate::Span::point(1, 1),
            }],
            embed_declarations: vec![],
            declarations: vec![],
        };
        let env = build_type_env(&module, &stdlib_root);
        // Today: `Just` ends up as `Ambiguous`. The follow-up should
        // make this either be rejected at build_type_env time, or
        // disambiguate with a clear diagnostic.
        match env.globals.get("Just") {
            Some(GlobalBinding::Ambiguous { .. }) => {
                panic!("expected non-ambiguous `Just` after the follow-up lands")
            }
            _ => {}
        }
    }

    #[test]
    #[ignore = "GU-S3 IU follow-up: imported-vs-imported same-name union (e.g. two `Maybe` \
                modules selectively imported) currently last-writer-wins on `union_types` \
                because the IU shadow guard only protects against local-vs-imported. Tracked \
                as a follow-up — needs ambiguity-on-collision detection at the imported pass."]
    fn imported_vs_imported_same_name_union_should_be_ambiguous() {
        // Codex pass-2 (2): pin the known scope-out behaviour. The
        // follow-up should turn this into a deterministic diagnostic
        // rather than the current silent last-writer-wins.
        let sandbox = TempDirGuard::new("imported_vs_imported");
        let stdlib_root = sandbox.path.join("stdlib");
        fs::create_dir_all(stdlib_root.join("goby")).expect("dir");
        write_module(
            &stdlib_root,
            "goby/maybe_a",
            "type Maybe a = Just(a) | Nothing\n",
        );
        write_module(
            &stdlib_root,
            "goby/maybe_b",
            "type Maybe a = Just(a) | Nothing\n",
        );
        let module = empty_module_with_imports_and_types(
            vec![
                import_selective("goby/maybe_a", &["Maybe"]),
                import_selective("goby/maybe_b", &["Maybe"]),
            ],
            vec![],
        );
        let env = build_type_env(&module, &stdlib_root);
        // After the follow-up: the two imports should produce a
        // diagnostic or an Ambiguous `union_types` axis. Today only one
        // wins.
        let info = env
            .union_types
            .get("Maybe")
            .expect("at least one union should land");
        match &info.origin {
            CtorOrigin::Imported { source_module } => {
                assert!(
                    source_module == "goby/maybe_a",
                    "the follow-up should not allow last-writer-wins; once it lands, expect a diagnostic instead"
                );
            }
            other => panic!("expected Imported origin, got {:?}", other),
        }
    }

    #[test]
    fn no_local_collision_registers_imported_union() {
        // Negative control: with no local `Maybe` / `Just`, the
        // imported union lands in env with `origin: Imported`.
        let sandbox = TempDirGuard::new("no_collision_registers");
        let stdlib_root = sandbox.path.join("stdlib");
        fs::create_dir_all(stdlib_root.join("goby")).expect("dir");
        write_module(
            &stdlib_root,
            "goby/maybe",
            "type Maybe a = Just(a) | Nothing\n",
        );
        let module = empty_module_with_imports_and_types(
            vec![import_selective("goby/maybe", &["Maybe"])],
            vec![],
        );
        let env = build_type_env(&module, &stdlib_root);
        let info = env
            .union_types
            .get("Maybe")
            .expect("imported `Maybe` must register");
        assert_eq!(
            info.origin,
            CtorOrigin::Imported {
                source_module: "goby/maybe".to_string(),
            }
        );
        let just = env.globals.get("Just").expect("imported `Just` global");
        match just {
            GlobalBinding::Resolved { source, .. } => {
                assert_eq!(source, &ctor_source_imported("Maybe", "goby/maybe"));
            }
            other => panic!("expected Resolved imported Just, got {:?}", other),
        }
    }
}
