use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::{
    Expr, ImportKind, Module, Span, Stmt,
    ast::InterpolatedPart,
    stdlib::{StdlibResolveError, StdlibResolver},
    typecheck::{PRELUDE_MODULE_PATH, TypecheckError},
    typecheck_build::insert_global_symbol,
    typecheck_env::{GlobalBinding, ImportedEffectDecl, Ty},
    typecheck_types::ty_from_annotation,
    types::parse_function_type,
};

pub(crate) fn validate_imports(module: &Module, stdlib_root: &Path) -> Result<(), TypecheckError> {
    let resolver = StdlibResolver::new(stdlib_root.to_path_buf());
    for import in effective_imports(module, &resolver) {
        let resolved = resolver
            .resolve_module(&import.module_path)
            .map_err(|err| {
                let message = match err {
                    StdlibResolveError::ModuleNotFound { attempted_path, .. } => format!(
                        "unknown module `{}` (attempted stdlib path: {})",
                        import.module_path,
                        attempted_path.display()
                    ),
                    _ => format!(
                        "failed to resolve stdlib module `{}`: {}",
                        import.module_path,
                        stdlib_error_message(&err)
                    ),
                };
                TypecheckError {
                    declaration: None,
                    span: None, // no span available: ImportDecl has no span field
                    message,
                }
            })?;
        if let ImportKind::Selective(names) = &import.kind {
            for name in names {
                let exists = resolved.exports.contains_key(name)
                    || resolved.types.iter().any(|ty| ty == name)
                    || resolved.effects.iter().any(|effect| effect == name);
                if !exists {
                    return Err(TypecheckError {
                        declaration: None,
                        span: None, // no span available: ImportDecl has no span field
                        message: format!(
                            "unknown symbol `{}` in import from `{}`",
                            name, import.module_path
                        ),
                    });
                }
            }
        }
    }
    Ok(())
}

pub(crate) fn validate_embed_declarations(
    module: &Module,
    source_path: Option<&Path>,
    stdlib_root: Option<&Path>,
) -> Result<(), TypecheckError> {
    if module.embed_declarations.is_empty() {
        return Ok(());
    }

    if let Some(source_path) = source_path {
        let stdlib_root = stdlib_root
            .map(Path::to_path_buf)
            .unwrap_or_else(default_stdlib_root);
        if !is_path_within_root(source_path, &stdlib_root) {
            return Err(TypecheckError {
                declaration: None,
                span: None, // no span available: source path check, no AST node span
                message: format!(
                    "@embed declarations are only allowed under stdlib root `{}`",
                    stdlib_root.display()
                ),
            });
        }
    }

    let mut seen = HashSet::new();
    let declared_effects: HashSet<&str> = module
        .effect_declarations
        .iter()
        .map(|effect| effect.name.as_str())
        .collect();
    for embed in &module.embed_declarations {
        if !seen.insert(embed.effect_name.clone()) {
            return Err(TypecheckError {
                declaration: None,
                span: Some(Span::point(embed.line, 1)),
                message: format!("duplicate embedded effect `{}`", embed.effect_name),
            });
        }
        if !embed.handler_name.starts_with("__goby_embeded_effect_") {
            return Err(TypecheckError {
                declaration: None,
                span: Some(Span::point(embed.line, 1)),
                message: format!(
                    "embedded handler `{}` must start with `__goby_embeded_effect_`",
                    embed.handler_name
                ),
            });
        }
        if !is_known_runtime_intrinsic_name(&embed.handler_name) {
            return Err(TypecheckError {
                declaration: None,
                span: Some(Span::point(embed.line, 1)),
                message: format!(
                    "unknown embedded handler intrinsic `{}`",
                    embed.handler_name
                ),
            });
        }
        if !declared_effects.contains(embed.effect_name.as_str()) {
            return Err(TypecheckError {
                declaration: None,
                span: Some(Span::point(embed.line, 1)),
                message: format!(
                    "embedded effect `{}` must be declared in the same module",
                    embed.effect_name
                ),
            });
        }
    }

    Ok(())
}

pub(crate) fn validate_intrinsic_namespace_policy(
    module: &Module,
    source_path: Option<&Path>,
    stdlib_root: &Path,
) -> Result<(), TypecheckError> {
    let Some(source_path) = source_path else {
        return Ok(());
    };
    let is_stdlib_source = is_path_within_root(source_path, stdlib_root);

    for decl in &module.declarations {
        if is_reserved_intrinsic_name(&decl.name) {
            return Err(TypecheckError {
                declaration: Some(decl.name.clone()),
                span: Some(Span::point(decl.line, 1)),
                message: format!(
                    "reserved intrinsic name `{}` is stdlib-only (`__goby_*`)",
                    decl.name
                ),
            });
        }
        if let Some(stmts) = &decl.parsed_body
            && let Some((name, kind)) = first_disallowed_intrinsic_in_stmts(stmts, is_stdlib_source)
        {
            return Err(TypecheckError {
                declaration: Some(decl.name.clone()),
                span: Some(Span::point(decl.line, 1)),
                message: intrinsic_error_message(&name, kind),
            });
        }
    }

    Ok(())
}

pub(crate) fn default_stdlib_root() -> PathBuf {
    crate::path_util::workspace_root().join("stdlib")
}

pub(crate) fn collect_imported_embedded_defaults(
    module: &Module,
    stdlib_root: &Path,
) -> Result<HashMap<String, String>, TypecheckError> {
    let resolver = StdlibResolver::new(stdlib_root.to_path_buf());
    let mut defaults = HashMap::new();
    for import in effective_imports(module, &resolver) {
        let Ok(resolved) = resolver.resolve_module(&import.module_path) else {
            continue;
        };
        for embed in resolved.embedded_defaults {
            if let Some(existing) = defaults.get(&embed.effect_name)
                && existing != &embed.handler_name
            {
                return Err(TypecheckError {
                    declaration: None,
                    span: None, // no span available: ImportDecl has no span field
                    message: format!(
                        "conflicting embedded default handler for effect `{}` across stdlib imports (`{}` vs `{}`)",
                        embed.effect_name, existing, embed.handler_name
                    ),
                });
            }
            defaults.insert(embed.effect_name, embed.handler_name);
        }
    }
    Ok(defaults)
}

pub(crate) fn collect_imported_effect_declarations(
    module: &Module,
    stdlib_root: &Path,
) -> Vec<ImportedEffectDecl> {
    let resolver = StdlibResolver::new(stdlib_root.to_path_buf());
    let mut effects = Vec::new();
    for import in effective_imports(module, &resolver) {
        let Ok(source_path) = resolver.module_file_path(&import.module_path) else {
            continue;
        };
        let Ok(source) = std::fs::read_to_string(source_path) else {
            continue;
        };
        let Ok(parsed) = crate::parse_module(&source) else {
            continue;
        };
        effects.extend(parsed.effect_declarations.into_iter().filter_map(|decl| {
            import_selects_name(&import.kind, &decl.name).then_some(ImportedEffectDecl {
                source_module: import.module_path.clone(),
                decl,
            })
        }));
    }
    effects
}

pub(crate) fn validate_no_ambiguous_effect_names(
    imported_effects: &[ImportedEffectDecl],
    local_effects: &[crate::ast::EffectDecl],
) -> Result<(), TypecheckError> {
    let mut signatures_by_effect: HashMap<String, HashSet<String>> = HashMap::new();
    let mut sources_by_effect: HashMap<String, HashSet<String>> = HashMap::new();

    for imported in imported_effects {
        signatures_by_effect
            .entry(imported.decl.name.clone())
            .or_default()
            .insert(effect_decl_signature(&imported.decl));
        sources_by_effect
            .entry(imported.decl.name.clone())
            .or_default()
            .insert(format!("import `{}`", imported.source_module));
    }
    for local in local_effects {
        signatures_by_effect
            .entry(local.name.clone())
            .or_default()
            .insert(effect_decl_signature(local));
        sources_by_effect
            .entry(local.name.clone())
            .or_default()
            .insert("local effect declaration".to_string());
    }

    let mut conflicting_effects: Vec<(String, Vec<String>)> = signatures_by_effect
        .into_iter()
        .filter_map(|(name, signatures)| {
            (signatures.len() > 1).then(|| {
                let mut sorted_sources = sources_by_effect
                    .remove(&name)
                    .unwrap_or_default()
                    .into_iter()
                    .collect::<Vec<_>>();
                sorted_sources.sort();
                (name, sorted_sources)
            })
        })
        .collect();
    conflicting_effects.sort_by(|(left, _), (right, _)| left.cmp(right));

    let Some((effect_name, sources)) = conflicting_effects.first() else {
        return Ok(());
    };
    Err(TypecheckError {
        declaration: None,
        span: None, // no span available: ImportDecl has no span field
        message: format!(
            "effect `{}` has conflicting declarations across imports/declarations: {}",
            effect_name,
            sources.join(", ")
        ),
    })
}

pub(crate) fn collect_imported_type_names(module: &Module, stdlib_root: &Path) -> HashSet<String> {
    let resolver = StdlibResolver::new(stdlib_root.to_path_buf());
    let mut types = HashSet::new();
    for import in effective_imports(module, &resolver) {
        let Ok(resolved) = resolver.resolve_module(&import.module_path) else {
            continue;
        };
        types.extend(
            resolved
                .types
                .into_iter()
                .filter(|name| import_selects_name(&import.kind, name)),
        );
    }
    types
}

pub(crate) fn collect_imported_effect_names(
    module: &Module,
    stdlib_root: &Path,
) -> HashSet<String> {
    let resolver = StdlibResolver::new(stdlib_root.to_path_buf());
    let mut effects = HashSet::new();
    for import in &module.imports {
        let Ok(resolved) = resolver.resolve_module(&import.module_path) else {
            continue;
        };
        effects.extend(
            resolved
                .effects
                .into_iter()
                .filter(|name| import_selects_name(&import.kind, name)),
        );
    }
    effects
}

pub(crate) fn collect_local_embedded_defaults(module: &Module) -> HashMap<String, String> {
    module
        .embed_declarations
        .iter()
        .map(|embed| (embed.effect_name.clone(), embed.handler_name.clone()))
        .collect()
}

pub(crate) fn inject_imported_symbols(
    module: &Module,
    globals: &mut HashMap<String, GlobalBinding>,
    stdlib_root: &Path,
) {
    let resolver = StdlibResolver::new(stdlib_root.to_path_buf());
    for import in effective_imports(module, &resolver) {
        let Ok(exports) = module_exports_for_import_with_resolver(&import.module_path, &resolver)
        else {
            continue;
        };
        match &import.kind {
            ImportKind::Plain => {
                if import.module_path == PRELUDE_MODULE_PATH {
                    for (name, ty) in &exports {
                        insert_global_symbol(
                            globals,
                            name.clone(),
                            ty.clone(),
                            format!("implicit prelude `{}`", import.module_path),
                        );
                    }
                }
                let qualifier = import
                    .module_path
                    .rsplit('/')
                    .next()
                    .expect("module path should have at least one segment");
                for (name, ty) in exports {
                    insert_global_symbol(
                        globals,
                        format!("{}.{}", qualifier, name),
                        ty.clone(),
                        format!("import `{}`", import.module_path),
                    );
                }
            }
            ImportKind::Alias(alias) => {
                for (name, ty) in exports {
                    insert_global_symbol(
                        globals,
                        format!("{}.{}", alias, name),
                        ty.clone(),
                        format!("import `{}` as `{}`", import.module_path, alias),
                    );
                }
            }
            ImportKind::Selective(names) => {
                for name in names {
                    if let Some(ty) = exports.get(name) {
                        insert_global_symbol(
                            globals,
                            name.clone(),
                            ty.clone(),
                            format!("import `{}` ({})", import.module_path, name),
                        );
                    }
                }
            }
        }
    }
}

pub(crate) fn import_selects_name(kind: &ImportKind, name: &str) -> bool {
    match kind {
        ImportKind::Selective(names) => names.iter().any(|selected| selected == name),
        ImportKind::Plain | ImportKind::Alias(_) => true,
    }
}

fn is_path_within_root(path: &Path, root: &Path) -> bool {
    let path = canonical_or_absolute(path);
    let root = canonical_or_absolute(root);
    path.starts_with(root)
}

fn canonical_or_absolute(path: &Path) -> PathBuf {
    match std::fs::canonicalize(path) {
        Ok(canonical) => canonical,
        Err(_) => {
            if path.is_absolute() {
                path.to_path_buf()
            } else {
                std::env::current_dir()
                    .unwrap_or_else(|_| PathBuf::from("."))
                    .join(path)
            }
        }
    }
}

pub(crate) fn module_exports_for_import_with_resolver(
    module_path: &str,
    resolver: &StdlibResolver,
) -> Result<HashMap<String, Ty>, TypecheckError> {
    match resolver.resolve_module(module_path) {
        Ok(resolved) => Ok(resolved
            .exports
            .into_iter()
            .map(|(name, annotation)| (name, ty_from_import_annotation(&annotation)))
            .collect()),
        Err(StdlibResolveError::ModuleNotFound { attempted_path, .. }) => Err(TypecheckError {
            declaration: None,
            span: None, // no span available: ImportDecl has no span field
            message: format!(
                "unknown module `{}` (attempted stdlib path: {})",
                module_path,
                attempted_path.display()
            ),
        }),
        Err(err) => Err(TypecheckError {
            declaration: None,
            span: None, // no span available: ImportDecl has no span field
            message: format!(
                "failed to resolve stdlib module `{}`: {}",
                module_path,
                stdlib_error_message(&err)
            ),
        }),
    }
}

fn effective_imports(module: &Module, resolver: &StdlibResolver) -> Vec<crate::ast::ImportDecl> {
    let mut imports = module.imports.clone();
    let has_prelude = imports
        .iter()
        .any(|import| import.module_path == PRELUDE_MODULE_PATH);
    let prelude_available = resolver
        .module_file_path(PRELUDE_MODULE_PATH)
        .ok()
        .is_some_and(|path| path.exists());
    if !has_prelude && prelude_available {
        imports.push(crate::ast::ImportDecl {
            module_path: PRELUDE_MODULE_PATH.to_string(),
            kind: crate::ast::ImportKind::Plain,
        });
    }
    imports
}

fn stdlib_error_message(err: &StdlibResolveError) -> String {
    match err {
        StdlibResolveError::InvalidModulePath(path) => {
            format!("invalid module path `{path}`")
        }
        StdlibResolveError::ModuleNotFound {
            module_path,
            attempted_path,
        } => format!(
            "module `{}` not found at {}",
            module_path,
            attempted_path.display()
        ),
        StdlibResolveError::ReadFailed { path, message } => {
            format!("failed to read {}: {}", path.display(), message)
        }
        StdlibResolveError::ParseFailed {
            module_path,
            message,
        } => format!("failed to parse `{module_path}`: {message}"),
        StdlibResolveError::DuplicateExport {
            module_path,
            symbol,
        } => format!("duplicate export `{symbol}` in `{module_path}`"),
        StdlibResolveError::DuplicateEmbeddedEffect {
            module_path,
            effect_name,
        } => format!(
            "duplicate embedded effect `{}` in `{}`",
            effect_name, module_path
        ),
        StdlibResolveError::ExportTypeMissing {
            module_path,
            symbol,
        } => format!("missing type annotation for export `{symbol}` in `{module_path}`"),
    }
}

fn is_reserved_intrinsic_name(name: &str) -> bool {
    name.starts_with("__goby_")
}

fn is_known_runtime_intrinsic_name(name: &str) -> bool {
    matches!(
        name,
        "__goby_string_length"
            | "__goby_env_fetch_env_var"
            | "__goby_string_each_grapheme"
            | "__goby_list_push_string"
            | "__goby_embeded_effect_stdout_handler"
            | "__goby_embeded_effect_stdin_handler"
    )
}

#[derive(Clone, Copy)]
enum IntrinsicViolationKind {
    NonStdlibUse,
    UnknownIntrinsic,
}

fn intrinsic_error_message(name: &str, kind: IntrinsicViolationKind) -> String {
    match kind {
        IntrinsicViolationKind::NonStdlibUse => {
            format!("reserved intrinsic call `{name}` is stdlib-only (`__goby_*`)")
        }
        IntrinsicViolationKind::UnknownIntrinsic => {
            format!("unknown runtime intrinsic `{name}`")
        }
    }
}

fn first_disallowed_intrinsic_in_stmts(
    stmts: &[Stmt],
    is_stdlib_source: bool,
) -> Option<(String, IntrinsicViolationKind)> {
    for stmt in stmts {
        match stmt {
            Stmt::Binding { value, .. }
            | Stmt::MutBinding { value, .. }
            | Stmt::Assign { value, .. }
            | Stmt::Expr(value, _) => {
                if let Some(hit) = first_disallowed_intrinsic_in_expr(value, is_stdlib_source) {
                    return Some(hit);
                }
            }
        }
    }
    None
}

fn first_disallowed_intrinsic_in_expr(
    expr: &Expr,
    is_stdlib_source: bool,
) -> Option<(String, IntrinsicViolationKind)> {
    let classify = |name: &str| -> Option<(String, IntrinsicViolationKind)> {
        if !is_reserved_intrinsic_name(name) {
            return None;
        }
        if !is_stdlib_source {
            return Some((name.to_string(), IntrinsicViolationKind::NonStdlibUse));
        }
        if !is_known_runtime_intrinsic_name(name) {
            return Some((name.to_string(), IntrinsicViolationKind::UnknownIntrinsic));
        }
        None
    };

    match expr {
        Expr::Var { name, .. } => classify(name),
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) | Expr::Qualified { .. } => None,
        Expr::InterpolatedString(parts) => parts.iter().find_map(|part| match part {
            InterpolatedPart::Text(_) => None,
            InterpolatedPart::Expr(expr) => {
                first_disallowed_intrinsic_in_expr(expr, is_stdlib_source)
            }
        }),
        Expr::ListLit { elements, spread } => elements
            .iter()
            .find_map(|item| first_disallowed_intrinsic_in_expr(item, is_stdlib_source))
            .or_else(|| {
                spread
                    .as_ref()
                    .and_then(|s| first_disallowed_intrinsic_in_expr(s, is_stdlib_source))
            }),
        Expr::TupleLit(items) => items
            .iter()
            .find_map(|item| first_disallowed_intrinsic_in_expr(item, is_stdlib_source)),
        Expr::RecordConstruct { fields, .. } => fields
            .iter()
            .find_map(|(_, value)| first_disallowed_intrinsic_in_expr(value, is_stdlib_source)),
        Expr::BinOp { left, right, .. } => {
            first_disallowed_intrinsic_in_expr(left, is_stdlib_source)
                .or_else(|| first_disallowed_intrinsic_in_expr(right, is_stdlib_source))
        }
        Expr::Call { callee, arg, .. } => {
            first_disallowed_intrinsic_in_expr(callee, is_stdlib_source)
                .or_else(|| first_disallowed_intrinsic_in_expr(arg, is_stdlib_source))
        }
        Expr::MethodCall { args, .. } => args
            .iter()
            .find_map(|arg| first_disallowed_intrinsic_in_expr(arg, is_stdlib_source)),
        Expr::Pipeline { value, callee } => {
            first_disallowed_intrinsic_in_expr(value, is_stdlib_source).or_else(|| classify(callee))
        }
        Expr::Lambda { body, .. } => first_disallowed_intrinsic_in_expr(body, is_stdlib_source),
        Expr::Handler { clauses } => clauses.iter().find_map(|clause| {
            clause
                .parsed_body
                .as_ref()
                .and_then(|stmts| first_disallowed_intrinsic_in_stmts(stmts, is_stdlib_source))
        }),
        Expr::With { handler, body } => {
            first_disallowed_intrinsic_in_expr(handler, is_stdlib_source)
                .or_else(|| first_disallowed_intrinsic_in_stmts(body, is_stdlib_source))
        }
        Expr::Resume { value } => first_disallowed_intrinsic_in_expr(value, is_stdlib_source),
        Expr::Block(stmts) => first_disallowed_intrinsic_in_stmts(stmts, is_stdlib_source),
        Expr::Case { scrutinee, arms } => {
            first_disallowed_intrinsic_in_expr(scrutinee, is_stdlib_source).or_else(|| {
                arms.iter()
                    .find_map(|arm| first_disallowed_intrinsic_in_expr(&arm.body, is_stdlib_source))
            })
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => first_disallowed_intrinsic_in_expr(condition, is_stdlib_source)
            .or_else(|| first_disallowed_intrinsic_in_expr(then_expr, is_stdlib_source))
            .or_else(|| first_disallowed_intrinsic_in_expr(else_expr, is_stdlib_source)),
        Expr::ListIndex { list, index } => {
            first_disallowed_intrinsic_in_expr(list, is_stdlib_source)
                .or_else(|| first_disallowed_intrinsic_in_expr(index, is_stdlib_source))
        }
    }
}

fn effect_decl_signature(effect_decl: &crate::ast::EffectDecl) -> String {
    let params = if effect_decl.type_params.is_empty() {
        String::new()
    } else {
        format!("<{}>", effect_decl.type_params.join(","))
    };
    let mut members: Vec<(String, String)> = effect_decl
        .members
        .iter()
        .map(|member| {
            (
                member.name.clone(),
                member.type_annotation.trim().to_string(),
            )
        })
        .collect();
    members.sort();
    format!(
        "{}{}::{}",
        effect_decl.name,
        params,
        members
            .into_iter()
            .map(|(name, annotation)| format!("{name}:{annotation}"))
            .collect::<Vec<_>>()
            .join("|")
    )
}

fn ty_from_import_annotation(annotation: &str) -> Ty {
    let base = strip_effect_clause(annotation).trim();
    if let Some(ft) = parse_function_type(base) {
        let params: Vec<Ty> = ft.arguments.iter().map(|a| ty_from_annotation(a)).collect();
        let result = ty_from_annotation(&ft.result);
        Ty::Fun {
            params,
            result: Box::new(result),
        }
    } else {
        ty_from_annotation(base)
    }
}

fn strip_effect_clause(annotation: &str) -> &str {
    if let Some(idx) = annotation.find(" can ") {
        annotation[..idx].trim_end()
    } else {
        annotation
    }
}
