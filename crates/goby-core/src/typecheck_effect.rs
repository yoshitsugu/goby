use std::collections::{HashMap, HashSet};

use crate::{
    Module,
    typecheck::{TypecheckError, is_identifier},
    typecheck_annotation::find_can_keyword_index,
    typecheck_env::{EffectDependencyInfo, EffectMap, ImportedEffectDecl},
    typecheck_types::is_type_variable_name,
    types::{TypeExpr, parse_type_expr},
};

pub(crate) fn builtin_effect_names() -> &'static [&'static str] {
    &[]
}

pub(crate) fn build_required_effects_map(module: &Module) -> HashMap<String, Vec<String>> {
    let mut map = HashMap::new();
    for decl in &module.declarations {
        let Some(ann) = decl.type_annotation.as_deref() else {
            continue;
        };
        let Some(idx) = find_can_keyword_index(ann) else {
            continue;
        };
        let effects_raw = ann[idx + 3..].trim();
        let effects: Vec<String> = effects_raw
            .split(',')
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .map(str::to_string)
            .collect();
        if !effects.is_empty() {
            map.insert(decl.name.clone(), effects);
        }
    }
    map
}

pub(crate) fn build_effect_map(
    module: &Module,
    imported_effects: &[ImportedEffectDecl],
) -> EffectMap {
    let mut effect_to_ops: HashMap<String, HashSet<String>> = HashMap::new();
    for effect_decl in imported_effects
        .iter()
        .map(|imported| &imported.decl)
        .chain(module.effect_declarations.iter())
    {
        let ops = effect_to_ops.entry(effect_decl.name.clone()).or_default();
        for member in &effect_decl.members {
            let _ = ops.insert(format!("{}.{}", effect_decl.name, member.name));
            let _ = ops.insert(member.name.clone());
        }
    }

    EffectMap {
        effect_to_ops,
        op_to_effects: build_op_to_effects(module, imported_effects),
    }
}

pub(crate) fn build_effect_dependency_info(
    module: &Module,
    imported_effects: &[ImportedEffectDecl],
) -> EffectDependencyInfo {
    let mut op_required_effects: HashMap<String, Vec<String>> = HashMap::new();
    for effect_decl in imported_effects
        .iter()
        .map(|imported| &imported.decl)
        .chain(module.effect_declarations.iter())
    {
        for member in &effect_decl.members {
            op_required_effects.insert(
                format!("{}.{}", effect_decl.name, member.name),
                parse_can_clause_effects(&member.type_annotation),
            );
        }
    }
    EffectDependencyInfo {
        op_required_effects,
    }
}

pub(crate) fn ops_from_can_clause(
    annotation: Option<&str>,
    effect_map: &EffectMap,
) -> HashSet<String> {
    let Some(ann) = annotation else {
        return HashSet::new();
    };
    let Some(idx) = find_can_keyword_index(ann) else {
        return HashSet::new();
    };
    let effects_raw = ann[idx + 3..].trim();
    effects_raw
        .split(',')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .flat_map(|effect_name| {
            effect_map
                .effect_to_ops
                .get(effect_name)
                .into_iter()
                .flat_map(|ops| ops.iter().cloned())
        })
        .collect()
}

pub(crate) fn parse_can_clause_effects(annotation: &str) -> Vec<String> {
    let Some(idx) = find_can_keyword_index(annotation) else {
        return Vec::new();
    };
    let effects_raw = annotation[idx + 3..].trim();
    effects_raw
        .split(',')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .map(str::to_string)
        .collect()
}

pub(crate) fn validate_effect_declarations(module: &Module) -> Result<(), TypecheckError> {
    let mut seen = HashSet::new();
    for effect_decl in &module.effect_declarations {
        if !seen.insert(effect_decl.name.clone()) {
            return Err(TypecheckError {
                declaration: Some(effect_decl.name.clone()),
                span: None,
                message: format!("duplicate effect declaration `{}`", effect_decl.name),
            });
        }
    }
    Ok(())
}

pub(crate) fn validate_effect_member_effect_clauses(
    module: &Module,
    known_effects: &HashSet<String>,
) -> Result<(), TypecheckError> {
    for effect_decl in &module.effect_declarations {
        let declared_type_params: HashSet<String> =
            effect_decl.type_params.iter().cloned().collect();
        for member in &effect_decl.members {
            let parsed =
                parse_type_expr(&member.type_annotation).ok_or_else(|| TypecheckError {
                    declaration: Some(effect_decl.name.clone()),
                    span: None,
                    message: format!(
                        "invalid effect member type annotation in `{}.{}`",
                        effect_decl.name, member.name
                    ),
                })?;
            let mut used_type_vars = HashSet::new();
            collect_type_variable_names_in_type_expr(&parsed, &mut used_type_vars);
            for type_var in used_type_vars {
                if type_var == "_" {
                    continue;
                }
                if !declared_type_params.contains(&type_var) {
                    return Err(TypecheckError {
                        declaration: Some(effect_decl.name.clone()),
                        span: None,
                        message: format!(
                            "unknown effect type parameter `{}` in `{}.{}`",
                            type_var, effect_decl.name, member.name
                        ),
                    });
                }
            }
            if let Some(idx) = find_can_keyword_index(&member.type_annotation)
                && member.type_annotation[idx + 3..].trim().is_empty()
            {
                return Err(TypecheckError {
                    declaration: Some(effect_decl.name.clone()),
                    span: None,
                    message: format!(
                        "effect list after `can` must not be empty in `{}.{}`",
                        effect_decl.name, member.name
                    ),
                });
            }
            let deps = parse_can_clause_effects(&member.type_annotation);
            if deps.is_empty() {
                continue;
            }
            for dep in deps {
                if !is_identifier(&dep) {
                    return Err(TypecheckError {
                        declaration: Some(effect_decl.name.clone()),
                        span: None,
                        message: format!(
                            "invalid effect name `{}` in `can` clause of `{}.{}`",
                            dep, effect_decl.name, member.name
                        ),
                    });
                }
                if !known_effects.contains(&dep) {
                    return Err(TypecheckError {
                        declaration: Some(effect_decl.name.clone()),
                        span: None,
                        message: format!(
                            "unknown effect `{}` in `can` clause of `{}.{}`",
                            dep, effect_decl.name, member.name
                        ),
                    });
                }
            }
        }
    }
    Ok(())
}

pub(crate) fn validate_effect_dependency_cycles(module: &Module) -> Result<(), TypecheckError> {
    let mut graph: HashMap<String, Vec<String>> = HashMap::new();
    let local_effects: HashSet<String> = module
        .effect_declarations
        .iter()
        .map(|decl| decl.name.clone())
        .collect();

    for effect_decl in &module.effect_declarations {
        let deps = graph.entry(effect_decl.name.clone()).or_default();
        for member in &effect_decl.members {
            for dep in parse_can_clause_effects(&member.type_annotation) {
                if local_effects.contains(&dep) && !deps.contains(&dep) {
                    deps.push(dep);
                }
            }
        }
    }

    #[derive(Clone, Copy, PartialEq, Eq)]
    enum VisitState {
        Visiting,
        Visited,
    }

    fn dfs_cycle(
        effect: &str,
        graph: &HashMap<String, Vec<String>>,
        state: &mut HashMap<String, VisitState>,
        stack: &mut Vec<String>,
    ) -> Option<Vec<String>> {
        match state.get(effect) {
            Some(VisitState::Visiting) => {
                let cycle_start = stack
                    .iter()
                    .position(|name| name == effect)
                    .unwrap_or_default();
                let mut cycle = stack[cycle_start..].to_vec();
                cycle.push(effect.to_string());
                return Some(cycle);
            }
            Some(VisitState::Visited) => return None,
            None => {}
        }

        state.insert(effect.to_string(), VisitState::Visiting);
        stack.push(effect.to_string());
        if let Some(deps) = graph.get(effect) {
            for dep in deps {
                if let Some(cycle) = dfs_cycle(dep, graph, state, stack) {
                    return Some(cycle);
                }
            }
        }
        stack.pop();
        state.insert(effect.to_string(), VisitState::Visited);
        None
    }

    let mut state: HashMap<String, VisitState> = HashMap::new();
    let mut stack = Vec::new();
    for effect_decl in &module.effect_declarations {
        if let Some(cycle) = dfs_cycle(&effect_decl.name, &graph, &mut state, &mut stack) {
            let cycle_display = cycle.join(" -> ");
            return Err(TypecheckError {
                declaration: Some(effect_decl.name.clone()),
                span: None,
                message: format!(
                    "effect dependency cycle detected in member `can` clauses: {}",
                    cycle_display
                ),
            });
        }
    }

    Ok(())
}

fn build_op_to_effects(
    module: &Module,
    imported_effects: &[ImportedEffectDecl],
) -> HashMap<String, HashSet<String>> {
    let mut map: HashMap<String, HashSet<String>> = HashMap::new();
    for effect_decl in imported_effects
        .iter()
        .map(|imported| &imported.decl)
        .chain(module.effect_declarations.iter())
    {
        for member in &effect_decl.members {
            map.entry(member.name.clone())
                .or_default()
                .insert(effect_decl.name.clone());
        }
    }
    map
}

fn collect_type_variable_names_in_type_expr(expr: &TypeExpr, out: &mut HashSet<String>) {
    match expr {
        TypeExpr::Name(name) => {
            if is_type_variable_name(name) {
                out.insert(name.clone());
            }
        }
        TypeExpr::Tuple(items) => {
            for item in items {
                collect_type_variable_names_in_type_expr(item, out);
            }
        }
        TypeExpr::Function { arguments, result } => {
            for arg in arguments {
                collect_type_variable_names_in_type_expr(arg, out);
            }
            collect_type_variable_names_in_type_expr(result, out);
        }
        TypeExpr::Apply { head, args } => {
            collect_type_variable_names_in_type_expr(head, out);
            for arg in args {
                collect_type_variable_names_in_type_expr(arg, out);
            }
        }
    }
}
