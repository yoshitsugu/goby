use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::{
    Module,
    typecheck::TypecheckError,
    typecheck_annotation::{
        annotation_return_ty, declaration_param_types, validate_declaration_annotations,
        validate_main_annotation,
    },
    typecheck_build::{build_type_env, ensure_no_ambiguous_globals},
    typecheck_effect::{
        build_effect_map, build_required_effects_map,
        builtin_effect_names, ops_from_can_clause, validate_effect_declarations,
        validate_effect_member_effect_clauses,
    },
    typecheck_env::{EffectMap, ImportedEffectDecl, Ty, TypeEnv},
    typecheck_resume::check_resume_in_stmts,
    typecheck_stmt::check_body_stmts,
    typecheck_types::validate_type_declarations,
    typecheck_validate::{
        collect_imported_effect_declarations, collect_imported_effect_names,
        collect_imported_embedded_defaults, collect_imported_type_names,
        collect_local_embedded_defaults, default_stdlib_root, validate_embed_declarations,
        validate_imports, validate_intrinsic_namespace_policy, validate_no_ambiguous_effect_names,
    },
};

pub(crate) struct ValidationPhase {
    pub(crate) imported_effect_declarations: Vec<ImportedEffectDecl>,
    pub(crate) embedded_default_effects: HashSet<String>,
}

pub(crate) struct CheckingPhase {
    pub(crate) env: TypeEnv,
    pub(crate) effect_map: EffectMap,
    pub(crate) required_effects_map: HashMap<String, Vec<String>>,
    pub(crate) embedded_default_effects: HashSet<String>,
}

pub(crate) fn default_typecheck_stdlib_root(stdlib_root: Option<&Path>) -> std::path::PathBuf {
    stdlib_root
        .map(Path::to_path_buf)
        .unwrap_or_else(default_stdlib_root)
}

pub(crate) fn validate_module_phase(
    module: &Module,
    source_path: Option<&Path>,
    stdlib_root: Option<&Path>,
    stdlib_root_path: &Path,
) -> Result<ValidationPhase, TypecheckError> {
    validate_intrinsic_namespace_policy(module, source_path, stdlib_root_path)?;
    validate_imports(module, stdlib_root_path)?;
    validate_embed_declarations(module, source_path, stdlib_root)?;
    let imported_types = collect_imported_type_names(module, stdlib_root_path);
    validate_type_declarations(module, &imported_types)?;
    validate_effect_declarations(module)?;

    let imported_embedded_defaults = collect_imported_embedded_defaults(module, stdlib_root_path)?;
    let imported_effect_declarations =
        collect_imported_effect_declarations(module, stdlib_root_path);
    validate_no_ambiguous_effect_names(&imported_effect_declarations, &module.effect_declarations)?;
    let known_effects = known_effects(module, stdlib_root_path);
    let embedded_default_effects = embedded_default_effects(module, &imported_embedded_defaults);
    validate_effect_member_effect_clauses(module)?;
    validate_declaration_annotations(module, &known_effects, &embedded_default_effects)?;
    validate_main_annotation(module)?;
    Ok(ValidationPhase {
        imported_effect_declarations,
        embedded_default_effects,
    })
}

pub(crate) fn build_checking_phase(
    module: &Module,
    stdlib_root_path: &Path,
    validation: &ValidationPhase,
) -> Result<CheckingPhase, TypecheckError> {
    let env = build_type_env(module, stdlib_root_path);
    ensure_no_ambiguous_globals(&env)?;
    let effect_map = build_effect_map(module, &validation.imported_effect_declarations);
    let required_effects_map = build_required_effects_map(module);
    Ok(CheckingPhase {
        env,
        effect_map,
        required_effects_map,
        embedded_default_effects: validation.embedded_default_effects.clone(),
    })
}

pub(crate) fn check_declaration_bodies(
    module: &Module,
    checking: &CheckingPhase,
) -> Result<(), TypecheckError> {
    for decl in &module.declarations {
        let Some(stmts) = &decl.parsed_body else {
            continue;
        };
        let declared_return_ty = decl.type_annotation.as_deref().map(annotation_return_ty);
        let param_tys = declaration_param_types(decl)?;
        let param_ty_refs: Vec<(&str, Ty)> = param_tys
            .iter()
            .map(|(name, ty)| (name.as_str(), ty.clone()))
            .collect();
        let mut decl_covered_ops =
            ops_from_can_clause(decl.type_annotation.as_deref(), &checking.effect_map);
        if decl.name == "main" {
            for effect_name in &checking.embedded_default_effects {
                if let Some(ops) = checking.effect_map.effect_to_ops.get(effect_name) {
                    decl_covered_ops.extend(ops.iter().cloned());
                }
            }
        }
        check_resume_in_stmts(stmts, &checking.env, &decl.name, &param_ty_refs, None)?;
        check_body_stmts(
            stmts,
            &checking.env,
            &checking.effect_map,
            &checking.required_effects_map,
            &decl.name,
            declared_return_ty,
            &param_ty_refs,
            &decl_covered_ops,
        )?;
    }
    Ok(())
}

fn known_effects(module: &Module, stdlib_root_path: &Path) -> HashSet<String> {
    let imported_effects = collect_imported_effect_names(module, stdlib_root_path);
    builtin_effect_names()
        .iter()
        .copied()
        .map(str::to_string)
        .chain(imported_effects)
        .chain(module.effect_declarations.iter().map(|e| e.name.clone()))
        .collect()
}

fn embedded_default_effects(
    module: &Module,
    imported_embedded_defaults: &HashMap<String, String>,
) -> HashSet<String> {
    let local_embedded_defaults = collect_local_embedded_defaults(module);
    imported_embedded_defaults
        .keys()
        .cloned()
        .chain(local_embedded_defaults.keys().cloned())
        .collect()
}
