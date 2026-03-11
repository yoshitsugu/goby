use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::{
    Module,
    ast::{
        BinOpKind, CasePattern, Expr, InterpolatedPart, ListPatternItem, ListPatternTail, Span,
        Stmt, TypeDeclaration,
    },
    typecheck_effect::{
        build_effect_dependency_info, build_effect_map, build_required_effects_map,
        builtin_effect_names, ops_from_can_clause, validate_effect_declarations,
        validate_effect_dependency_cycles, validate_effect_member_effect_clauses,
    },
    typecheck_env::{
        EffectDependencyInfo, EffectMap, GlobalBinding, RecordTypeInfo, ResumeContext, Ty, TypeEnv,
        TypeSubst,
    },
    typecheck_validate::{
        collect_imported_effect_declarations, collect_imported_effect_names,
        collect_imported_embedded_defaults, collect_imported_type_names,
        collect_local_embedded_defaults, default_stdlib_root, inject_imported_symbols,
        validate_embed_declarations, validate_imports, validate_intrinsic_namespace_policy,
        validate_no_ambiguous_effect_names,
    },
    types::{TypeExpr, parse_function_type, parse_type_expr},
};

pub(crate) const PRELUDE_MODULE_PATH: &str = "goby/prelude";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypecheckError {
    pub declaration: Option<String>,
    /// Source location of the error, if known.
    pub span: Option<Span>,
    pub message: String,
}

impl std::fmt::Display for TypecheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let target = self.declaration.as_deref().unwrap_or("<module>");
        match &self.span {
            Some(s) => write!(
                f,
                "typecheck error in {} at line {}:{}: {}",
                target, s.line, s.col, self.message
            ),
            None => write!(f, "typecheck error in {}: {}", target, self.message),
        }
    }
}

impl std::error::Error for TypecheckError {}

pub fn typecheck_module(module: &Module) -> Result<(), TypecheckError> {
    typecheck_module_with_context(module, None, None)
}

pub fn typecheck_module_with_context(
    module: &Module,
    source_path: Option<&Path>,
    stdlib_root: Option<&Path>,
) -> Result<(), TypecheckError> {
    let stdlib_root_path = stdlib_root
        .map(Path::to_path_buf)
        .unwrap_or_else(default_stdlib_root);
    validate_intrinsic_namespace_policy(module, source_path, &stdlib_root_path)?;
    validate_imports(module, &stdlib_root_path)?;
    validate_embed_declarations(module, source_path, stdlib_root)?;
    let imported_types = collect_imported_type_names(module, &stdlib_root_path);
    validate_type_declarations(module, &imported_types)?;
    validate_effect_declarations(module)?;

    let imported_embedded_defaults = collect_imported_embedded_defaults(module, &stdlib_root_path)?;
    let imported_effect_declarations =
        collect_imported_effect_declarations(module, &stdlib_root_path);
    validate_no_ambiguous_effect_names(&imported_effect_declarations, &module.effect_declarations)?;
    let imported_effects = collect_imported_effect_names(module, &stdlib_root_path);
    let local_embedded_defaults = collect_local_embedded_defaults(module);
    let embedded_default_effects: HashSet<String> = imported_embedded_defaults
        .keys()
        .cloned()
        .chain(local_embedded_defaults.keys().cloned())
        .collect();
    let known_effects: HashSet<String> = builtin_effect_names()
        .iter()
        .copied()
        .map(str::to_string)
        .chain(imported_effects)
        .chain(module.effect_declarations.iter().map(|e| e.name.clone()))
        .collect();
    validate_effect_member_effect_clauses(module, &known_effects)?;
    validate_effect_dependency_cycles(module)?;

    let mut names = HashSet::new();

    for decl in &module.declarations {
        if !names.insert(decl.name.clone()) {
            return Err(TypecheckError {
                declaration: Some(decl.name.clone()),
                span: Some(Span {
                    line: decl.line,
                    col: 1,
                }),
                message: "duplicate top-level declaration".to_string(),
            });
        }

        if let Some(annotation) = decl.type_annotation.as_deref() {
            validate_type_annotation(
                &decl.name,
                annotation,
                &known_effects,
                &embedded_default_effects,
            )?;
        }
    }

    if let Some(main) = module.declarations.iter().find(|d| d.name == "main") {
        // A `main` declaration without a type annotation is allowed during `check`
        // (e.g. `effect.gb`).  The Wasm backend will enforce the annotation at
        // compile time when `run` is invoked.
        if let Some(annotation) = main.type_annotation.as_deref() {
            let base_annotation = strip_effect_clause(annotation);
            let ty = parse_function_type(base_annotation).ok_or_else(|| TypecheckError {
                declaration: Some("main".to_string()),
                span: Some(Span {
                    line: main.line,
                    col: 1,
                }),
                message: "main type annotation must be a function type".to_string(),
            })?;

            if ty.arguments != vec!["Unit".to_string()] || ty.result != "Unit" {
                return Err(TypecheckError {
                    declaration: Some("main".to_string()),
                    span: Some(Span {
                        line: main.line,
                        col: 1,
                    }),
                    message: "main type must be `Unit -> Unit` in MVP".to_string(),
                });
            }
        }
    }

    // Expression-level type checking (when parsed_body is available).
    let env = build_type_env(module, &stdlib_root_path);
    ensure_no_ambiguous_globals(&env)?;
    let effect_map = build_effect_map(module, &imported_effect_declarations);
    let effect_dependency_info =
        build_effect_dependency_info(module, &imported_effect_declarations);
    let required_effects_map = build_required_effects_map(module);

    for decl in &module.declarations {
        if let Some(stmts) = &decl.parsed_body {
            let declared_return_ty = decl.type_annotation.as_deref().map(annotation_return_ty);

            // Derive per-parameter types from the function type annotation.
            // Also validate that the number of declared params matches the annotation.
            let param_tys: Vec<(String, Ty)> = {
                let ft_opt = decl.type_annotation.as_deref().and_then(|ann| {
                    let base = strip_effect_clause(ann);
                    parse_function_type(base)
                });

                if let Some(ft) = ft_opt {
                    // A single `Unit` parameter may be omitted from the definition
                    // (e.g. `main : Unit -> Unit; main = ...` is idiomatic in MVP).
                    let unit_param_omitted = decl.params.is_empty()
                        && ft.arguments.len() == 1
                        && ft.arguments[0] == "Unit";

                    if !unit_param_omitted && decl.params.len() != ft.arguments.len() {
                        return Err(TypecheckError {
                            declaration: Some(decl.name.clone()),
                            span: Some(Span {
                                line: decl.line,
                                col: 1,
                            }),
                            message: format!(
                                "definition has {} parameter(s) but type annotation has {}",
                                decl.params.len(),
                                ft.arguments.len()
                            ),
                        });
                    }
                    decl.params
                        .iter()
                        .zip(ft.arguments.iter())
                        .map(|(name, ann_ty)| (name.clone(), ty_from_annotation(ann_ty)))
                        .collect()
                } else {
                    Vec::new()
                }
            };

            let param_ty_refs: Vec<(&str, Ty)> = param_tys
                .iter()
                .map(|(name, ty)| (name.as_str(), ty.clone()))
                .collect();

            // Ops declared in the function's own `can` clause are implicitly
            // available inside that function's body.
            let decl_covered_ops =
                ops_from_can_clause(decl.type_annotation.as_deref(), &effect_map);

            // Step 2 (`resume`) checks in regular declaration bodies.
            check_resume_in_stmts(stmts, &env, &decl.name, &param_ty_refs, None)?;

            check_body_stmts(
                stmts,
                &env,
                &effect_map,
                &effect_dependency_info,
                &required_effects_map,
                &decl.name,
                declared_return_ty,
                &param_ty_refs,
                &decl_covered_ops,
            )?;
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Internal type representation
// ---------------------------------------------------------------------------

/// Returns the expected return type of a declaration from its type annotation.
/// For function types (`A -> B`), returns the result type `B`.
/// For non-function types (`Int`, `String`, …), returns the annotation type itself.
fn annotation_return_ty(annotation: &str) -> Ty {
    let base = strip_effect_clause(annotation).trim();
    if let Some(ft) = parse_function_type(base) {
        ty_from_annotation(&ft.result)
    } else {
        ty_from_annotation(base)
    }
}

fn build_type_env(module: &Module, stdlib_root: &Path) -> TypeEnv {
    let mut globals = HashMap::new();
    let mut type_aliases = HashMap::new();
    let mut record_types = HashMap::new();
    for decl in &module.declarations {
        if let Some(annotation) = decl.type_annotation.as_deref() {
            let base = strip_effect_clause(annotation);
            if let Some(ft) = parse_function_type(base) {
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
                // Non-function annotation (e.g. tuple, plain type)
                insert_global_symbol(
                    &mut globals,
                    decl.name.clone(),
                    ty_from_annotation(base.trim()),
                    format!("declaration `{}`", decl.name),
                );
            }
        }
    }
    // Register built-in functions so that call-site type inference can use them.
    // `print` accepts any value and returns Unit.
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

fn ensure_no_ambiguous_globals(env: &TypeEnv) -> Result<(), TypecheckError> {
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

fn inject_effect_symbols(module: &Module, globals: &mut HashMap<String, GlobalBinding>) {
    for effect_decl in &module.effect_declarations {
        for member in &effect_decl.members {
            // Register as qualified key `EffectName.member`.
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
            // Also register unqualified bare name for unqualified calls like `log result`.
            insert_global_symbol(
                globals,
                member.name.clone(),
                ty,
                format!("effect `{}` member", effect_decl.name),
            );
        }
    }
}

fn inject_type_constructors(
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
                // Record constructors are available by bare constructor name.
                insert_global_symbol(
                    globals,
                    constructor.clone(),
                    ctor_ty.clone(),
                    format!("type `{}` constructor", name),
                );
                // Also expose qualified form for consistency with union constructors.
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

fn validate_type_declarations(
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
                span: None,
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
                    span: None,
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
                            span: None,
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
                            span: None,
                            message: format!("duplicate field `{}` in type `{}`", field.name, name),
                        });
                    }
                    let parsed = parse_type_expr(&field.ty).ok_or_else(|| TypecheckError {
                        declaration: Some(name.clone()),
                        span: None,
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
                span: None,
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

pub(crate) fn ty_from_annotation(s: &str) -> Ty {
    let s = s.trim();
    let Some(type_expr) = parse_type_expr(s) else {
        return Ty::Unknown;
    };
    ty_from_type_expr(&type_expr)
}

fn ty_from_type_expr(expr: &TypeExpr) -> Ty {
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

/// Effects that are provided by the runtime and do not require an `effect` declaration.
// ---------------------------------------------------------------------------
// Expression type inference
// ---------------------------------------------------------------------------

fn check_expr(expr: &Expr, env: &TypeEnv) -> Ty {
    match expr {
        Expr::IntLit(_) => Ty::Int,
        Expr::BoolLit(_) => Ty::Bool,
        Expr::StringLit(_) => Ty::Str,
        Expr::InterpolatedString(_) => Ty::Str,
        Expr::ListLit { elements, spread } => {
            if elements.is_empty() {
                return Ty::List(Box::new(Ty::Unknown));
            }
            let mut item_ty = check_expr(&elements[0], env);
            for item in &elements[1..] {
                item_ty = merge_branch_type(env, item_ty, check_expr(item, env));
            }
            if let Some(tail) = spread {
                let tail_ty = check_expr(tail, env);
                if let Ty::List(tail_inner) = env.resolve_alias(&tail_ty, 0) {
                    item_ty = merge_branch_type(env, item_ty, *tail_inner);
                }
            }
            Ty::List(Box::new(item_ty))
        }
        Expr::TupleLit(items) => {
            if items.is_empty() {
                return Ty::Unit;
            }
            let tys: Vec<Ty> = items.iter().map(|i| check_expr(i, env)).collect();
            Ty::Tuple(tys)
        }
        Expr::Var(name) => env.lookup(name),
        Expr::Qualified { receiver, member } => {
            let receiver_ty = env.lookup(receiver);
            let resolved_receiver_ty = env.resolve_alias(&receiver_ty, 0);
            if let Ty::Tuple(items) = &resolved_receiver_ty
                && let Some(index) = parse_tuple_member_index(member)
            {
                return items.get(index).cloned().unwrap_or(Ty::Unknown);
            }
            if let Some(receiver_ty) = env.locals.get(receiver) {
                let resolved_receiver_ty = env.resolve_alias(receiver_ty, 0);
                if let Ty::Con { name, .. } = &resolved_receiver_ty
                    && let Some(field_ty) = env.record_field_ty(name, member)
                {
                    return env.resolve_alias(&field_ty, 0);
                }
            }
            env.lookup(&format!("{}.{}", receiver, member))
        }
        Expr::RecordConstruct {
            constructor,
            fields,
        } => {
            let Some(record) = env.lookup_record_by_constructor(constructor) else {
                return Ty::Unknown;
            };
            if fields.len() != record.fields.len() {
                return Ty::Unknown;
            }
            for (name, value) in fields {
                let Some(expected_ty) = record.fields.get(name) else {
                    return Ty::Unknown;
                };
                let actual_ty = check_expr(value, env);
                if actual_ty != Ty::Unknown && !env.are_compatible(expected_ty, &actual_ty) {
                    return Ty::Unknown;
                }
            }
            Ty::Con {
                name: record.type_name.clone(),
                args: Vec::new(),
            }
        }
        Expr::BinOp { op, left, right } => {
            let lt = check_expr(left, env);
            let rt = check_expr(right, env);
            match (op, &lt, &rt) {
                (BinOpKind::And, Ty::Bool, Ty::Bool) => Ty::Bool,
                (BinOpKind::Add, Ty::Int, Ty::Int) => Ty::Int,
                (BinOpKind::Mul, Ty::Int, Ty::Int) => Ty::Int,
                (BinOpKind::Eq, Ty::Int, Ty::Int) => Ty::Bool,
                (BinOpKind::Eq, Ty::Str, Ty::Str) => Ty::Bool,
                (BinOpKind::Lt, Ty::Int, Ty::Int) => Ty::Bool,
                (BinOpKind::Gt, Ty::Int, Ty::Int) => Ty::Bool,
                // Unknown operands are tolerated (forward-compatibility)
                (_, Ty::Unknown, _) | (_, _, Ty::Unknown) => Ty::Unknown,
                _ => Ty::Unknown,
            }
        }
        Expr::Call { callee, arg } => {
            // Positional single-field record constructor sugar: `Ctor(value)` → `Ctor(field: value)`
            // Apply when callee is a bare name that is a known single-field record constructor.
            if let Expr::Var(name) = callee.as_ref()
                && let Some(record) = env.lookup_record_by_constructor(name)
                && record.fields.len() == 1
            {
                let field_name = record.fields.keys().next().unwrap().clone();
                let rewritten = Expr::RecordConstruct {
                    constructor: name.clone(),
                    fields: vec![(field_name, *arg.clone())],
                };
                return check_expr(&rewritten, env);
            }
            // Infer result type from callee's function type
            let callee_ty = check_expr(callee, env);
            match callee_ty {
                Ty::Fun { result, .. } => *result,
                _ => Ty::Unknown,
            }
        }
        Expr::MethodCall {
            receiver, method, ..
        } => {
            let qualified = format!("{}.{}", receiver, method);
            match env.lookup(&qualified) {
                Ty::Fun { result, .. } => *result,
                _ => Ty::Unknown,
            }
        }
        Expr::Pipeline { value: _, callee } => {
            let callee_ty = env.lookup(callee);
            match callee_ty {
                Ty::Fun { result, .. } => *result,
                _ => Ty::Unknown,
            }
        }
        Expr::Lambda { param, body } => {
            // Infer body type with param as Unknown for now
            let child_env = env.with_local(param, Ty::Unknown);
            let result = check_expr(body, &child_env);
            Ty::Fun {
                params: vec![Ty::Unknown],
                result: Box::new(result),
            }
        }
        Expr::Handler { clauses } => Ty::Handler {
            covered_ops: infer_handler_covered_ops_lenient(clauses, env),
        },
        Expr::With { .. } => Ty::Unknown,
        Expr::Resume { value } => {
            let _ = check_expr(value, env);
            Ty::Unknown
        }
        Expr::Block(stmts) => infer_block_expr_ty(stmts, env),
        Expr::Case { scrutinee, arms } => {
            let scrutinee_ty = check_expr(scrutinee, env);
            let mut merged: Option<Ty> = None;
            for arm in arms {
                let arm_env = env_with_case_pattern_bindings(env, &arm.pattern, &scrutinee_ty);
                let arm_ty = check_expr(&arm.body, &arm_env);
                merged = Some(match merged {
                    Some(prev) => merge_branch_type(env, prev, arm_ty),
                    None => arm_ty,
                });
            }
            merged.unwrap_or(Ty::Unknown)
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            let _ = check_expr(condition, env);
            let then_ty = check_expr(then_expr, env);
            let else_ty = check_expr(else_expr, env);
            merge_branch_type(env, then_ty, else_ty)
        }
    }
}

fn check_list_spread_constraints(
    elements: &[Expr],
    spread: Option<&Expr>,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    let Some(tail_expr) = spread else {
        return Ok(());
    };

    let mut merged_prefix_ty: Option<Ty> = None;
    for (idx, element) in elements.iter().enumerate() {
        let element_ty = env.resolve_alias(&check_expr(element, env), 0);
        if element_ty == Ty::Unknown || ty_contains_type_var(&element_ty) {
            continue;
        }
        if let Some(expected) = &merged_prefix_ty
            && !env.are_compatible(expected, &element_ty)
            && !env.are_compatible(&element_ty, expected)
        {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!(
                    "list spread prefix element type mismatch: element #{} is `{}` but earlier prefix elements require `{}`",
                    idx + 1,
                    ty_name(&element_ty),
                    ty_name(expected)
                ),
            });
        }
        merged_prefix_ty = Some(match merged_prefix_ty {
            Some(prev) => merge_branch_type(env, prev, element_ty),
            None => element_ty,
        });
    }

    let tail_ty = env.resolve_alias(&check_expr(tail_expr, env), 0);
    match tail_ty {
        Ty::List(tail_item_ty) => {
            let tail_item_ty = *tail_item_ty;
            if tail_item_ty == Ty::Unknown || ty_contains_type_var(&tail_item_ty) {
                return Ok(());
            }
            if let Some(expected_prefix_ty) = merged_prefix_ty
                && expected_prefix_ty != Ty::Unknown
                && !ty_contains_type_var(&expected_prefix_ty)
                && !env.are_compatible(&expected_prefix_ty, &tail_item_ty)
                && !env.are_compatible(&tail_item_ty, &expected_prefix_ty)
            {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "list spread tail element type mismatch: expected `List {}` but got `List {}`",
                        ty_name(&expected_prefix_ty),
                        ty_name(&tail_item_ty)
                    ),
                });
            }
            Ok(())
        }
        Ty::Unknown | Ty::Var(_) => Ok(()),
        other => {
            let expected = merged_prefix_ty.unwrap_or(Ty::Unknown);
            Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!(
                    "list spread tail must be `List {}` but got `{}`",
                    ty_name(&expected),
                    ty_name(&other)
                ),
            })
        }
    }
}

fn merge_branch_type(env: &TypeEnv, left: Ty, right: Ty) -> Ty {
    let left = env.resolve_alias(&left, 0);
    let right = env.resolve_alias(&right, 0);
    match (left, right) {
        (Ty::Unknown, ty) | (ty, Ty::Unknown) => ty,
        (Ty::List(l), Ty::List(r)) => Ty::List(Box::new(merge_branch_type(env, *l, *r))),
        (Ty::Tuple(ls), Ty::Tuple(rs)) if ls.len() == rs.len() => {
            let merged = ls
                .into_iter()
                .zip(rs)
                .map(|(l, r)| merge_branch_type(env, l, r))
                .collect();
            Ty::Tuple(merged)
        }
        (
            Ty::Fun {
                params: lps,
                result: lr,
            },
            Ty::Fun {
                params: rps,
                result: rr,
            },
        ) if lps.len() == rps.len() => {
            let merged_params = lps
                .into_iter()
                .zip(rps)
                .map(|(l, r)| merge_branch_type(env, l, r))
                .collect();
            Ty::Fun {
                params: merged_params,
                result: Box::new(merge_branch_type(env, *lr, *rr)),
            }
        }
        (Ty::Con { name: ln, args: la }, Ty::Con { name: rn, args: ra })
            if ln == rn && la.len() == ra.len() =>
        {
            let merged_args = la
                .into_iter()
                .zip(ra)
                .map(|(l, r)| merge_branch_type(env, l, r))
                .collect();
            Ty::Con {
                name: ln,
                args: merged_args,
            }
        }
        (l, r) if env.are_compatible(&l, &r) => l,
        (l, r) if env.are_compatible(&r, &l) => r,
        _ => Ty::Unknown,
    }
}

fn branch_types_compatible(env: &TypeEnv, left: &Ty, right: &Ty) -> bool {
    let left = env.resolve_alias(left, 0);
    let right = env.resolve_alias(right, 0);
    match (left, right) {
        (Ty::Unknown, _) | (_, Ty::Unknown) => true,
        (Ty::List(l), Ty::List(r)) => branch_types_compatible(env, &l, &r),
        (Ty::Tuple(ls), Ty::Tuple(rs)) if ls.len() == rs.len() => ls
            .iter()
            .zip(rs.iter())
            .all(|(l, r)| branch_types_compatible(env, l, r)),
        (
            Ty::Fun {
                params: lps,
                result: lr,
            },
            Ty::Fun {
                params: rps,
                result: rr,
            },
        ) if lps.len() == rps.len() => {
            lps.iter()
                .zip(rps.iter())
                .all(|(l, r)| branch_types_compatible(env, l, r))
                && branch_types_compatible(env, &lr, &rr)
        }
        (Ty::Con { name: ln, args: la }, Ty::Con { name: rn, args: ra })
            if ln == rn && la.len() == ra.len() =>
        {
            la.iter()
                .zip(ra.iter())
                .all(|(l, r)| branch_types_compatible(env, l, r))
        }
        (l, r) => env.are_compatible(&l, &r) || env.are_compatible(&r, &l),
    }
}

fn infer_block_expr_ty(stmts: &[Stmt], env: &TypeEnv) -> Ty {
    let mut local_env = env.clone();
    let mut last_expr_ty = Ty::Unknown;
    let mut has_tail_expr = false;
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                let ty = check_expr(value, &local_env);
                local_env.locals.insert(name.clone(), ty);
                has_tail_expr = false;
            }
            Stmt::Assign { value, .. } => {
                let _ = check_expr(value, &local_env);
                has_tail_expr = false;
            }
            Stmt::Expr(expr) => {
                last_expr_ty = check_expr(expr, &local_env);
                has_tail_expr = true;
            }
        }
    }
    if has_tail_expr {
        last_expr_ty
    } else {
        Ty::Unknown
    }
}

fn is_list_case_pattern(pattern: &CasePattern) -> bool {
    matches!(
        pattern,
        CasePattern::EmptyList | CasePattern::ListPattern { .. }
    )
}

fn list_item_ty_for_case_scrutinee(env: &TypeEnv, scrutinee_ty: &Ty) -> Ty {
    match env.resolve_alias(scrutinee_ty, 0) {
        Ty::List(inner) => *inner,
        Ty::Unknown => Ty::Unknown,
        _ => Ty::Unknown,
    }
}

fn env_with_case_pattern_bindings(
    env: &TypeEnv,
    pattern: &CasePattern,
    scrutinee_ty: &Ty,
) -> TypeEnv {
    let mut child = env.clone();
    if let CasePattern::ListPattern { items, tail } = pattern {
        let item_ty = list_item_ty_for_case_scrutinee(env, scrutinee_ty);
        for item in items {
            if let ListPatternItem::Bind(name) = item
                && name != "_"
            {
                child.locals.insert(name.clone(), item_ty.clone());
            }
        }
        if let Some(ListPatternTail::Bind(name)) = tail
            && name != "_"
        {
            child
                .locals
                .insert(name.clone(), Ty::List(Box::new(item_ty)));
        }
    }
    child
}

fn infer_handler_covered_ops_lenient(
    clauses: &[crate::ast::HandlerClause],
    env: &TypeEnv,
) -> HashSet<String> {
    let mut covered = HashSet::new();
    for clause in clauses {
        let effects = effect_candidates_for_operation(env, &clause.name);
        if effects.len() != 1 {
            continue;
        }
        let effect = &effects[0];
        covered.insert(clause.name.clone());
        covered.insert(format!("{}.{}", effect, clause.name));
    }
    covered
}

fn infer_handler_covered_ops_strict(
    handler_expr: &Expr,
    env: &TypeEnv,
    effect_map: &EffectMap,
    decl_name: &str,
) -> Result<HashSet<String>, TypecheckError> {
    match handler_expr {
        Expr::Handler { clauses } => {
            let mut covered = HashSet::new();
            let mut seen_ops = HashSet::new();
            for clause in clauses {
                if !seen_ops.insert(clause.name.clone()) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "duplicate handler clause for operation `{}`",
                            clause.name
                        ),
                    });
                }

                let Some(effects) = effect_map.op_to_effects.get(&clause.name) else {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "unknown effect operation `{}` in handler expression",
                            clause.name
                        ),
                    });
                };

                if effects.len() > 1 {
                    let mut names: Vec<String> = effects.iter().cloned().collect();
                    names.sort();
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "operation '{}' is ambiguous across effects in Handler(...): {}",
                            clause.name,
                            names.join(", ")
                        ),
                    });
                }

                let effect = effects.iter().next().expect("non-empty set");
                covered.insert(clause.name.clone());
                covered.insert(format!("{}.{}", effect, clause.name));
            }
            Ok(covered)
        }
        Expr::Var(name) => match env.lookup(name) {
            Ty::Handler { covered_ops } => Ok(covered_ops),
            _ => Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!(
                    "`with` expects a handler value, but `{}` is not a Handler",
                    name
                ),
            }),
        },
        _ => Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: "`with` expects a handler value".to_string(),
        }),
    }
}

fn effect_candidates_for_operation(env: &TypeEnv, op_name: &str) -> Vec<String> {
    let mut out = Vec::new();
    let Some(binding) = env.globals.get(op_name) else {
        return out;
    };
    match binding {
        GlobalBinding::Resolved { source, .. } => {
            if let Some(effect) = extract_effect_name_from_source(source) {
                out.push(effect);
            }
        }
        GlobalBinding::Ambiguous { sources } => {
            for source in sources {
                if let Some(effect) = extract_effect_name_from_source(source) {
                    out.push(effect);
                }
            }
            out.sort();
            out.dedup();
        }
    }
    out
}

fn extract_effect_name_from_source(source: &str) -> Option<String> {
    let prefix = "effect `";
    let suffix = "` member";
    if !source.starts_with(prefix) || !source.ends_with(suffix) {
        return None;
    }
    let start = prefix.len();
    let end = source.len() - suffix.len();
    Some(source[start..end].to_string())
}

fn check_resume_in_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    decl_name: &str,
    param_tys: &[(&str, Ty)],
    resume_ctx: Option<&ResumeContext>,
) -> Result<(), TypecheckError> {
    let mut local_env = TypeEnv {
        globals: env.globals.clone(),
        locals: param_tys
            .iter()
            .map(|(name, ty)| (name.to_string(), ty.clone()))
            .collect(),
        type_aliases: env.type_aliases.clone(),
        record_types: env.record_types.clone(),
    };
    check_resume_in_stmts_with_local_env(stmts, &mut local_env, decl_name, resume_ctx)
}

fn check_resume_in_stmts_with_local_env(
    stmts: &[Stmt],
    local_env: &mut TypeEnv,
    decl_name: &str,
    resume_ctx: Option<&ResumeContext>,
) -> Result<(), TypecheckError> {
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                check_resume_in_expr(value, local_env, decl_name, resume_ctx)?;
                let ty = infer_binding_ty_with_resume_context(value, local_env, resume_ctx);
                local_env.locals.insert(name.clone(), ty);
            }
            Stmt::Assign { value, .. } => {
                check_resume_in_expr(value, local_env, decl_name, resume_ctx)?;
            }
            Stmt::Expr(expr) => {
                check_resume_in_expr(expr, local_env, decl_name, resume_ctx)?;
            }
        }
    }
    Ok(())
}

fn infer_binding_ty_with_resume_context(
    value: &Expr,
    env: &TypeEnv,
    resume_ctx: Option<&ResumeContext>,
) -> Ty {
    if let Expr::Resume { .. } = value
        && let Some(ctx) = resume_ctx
        && let Some(expected) = ctx.expected_arg_ty.as_ref()
        && !ty_contains_type_var(expected)
    {
        // Conservative local inference: for non-generic operation result types,
        // use the known handler operation result type for resume-bound locals.
        return expected.clone();
    }
    check_expr(value, env)
}

fn apply_type_substitution(ty: &Ty, subst: &TypeSubst, env: &TypeEnv) -> Ty {
    let resolved = env.resolve_alias(ty, 0);
    match resolved {
        Ty::Var(name) => match subst.get(&name) {
            Some(bound) => apply_type_substitution(bound, subst, env),
            None => Ty::Var(name),
        },
        Ty::List(inner) => Ty::List(Box::new(apply_type_substitution(&inner, subst, env))),
        Ty::Tuple(items) => Ty::Tuple(
            items
                .iter()
                .map(|item| apply_type_substitution(item, subst, env))
                .collect(),
        ),
        Ty::Fun { params, result } => Ty::Fun {
            params: params
                .iter()
                .map(|param| apply_type_substitution(param, subst, env))
                .collect(),
            result: Box::new(apply_type_substitution(&result, subst, env)),
        },
        Ty::Con { name, args } => Ty::Con {
            name,
            args: args
                .iter()
                .map(|arg| apply_type_substitution(arg, subst, env))
                .collect(),
        },
        Ty::Handler { covered_ops } => Ty::Handler { covered_ops },
        Ty::Int | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => resolved,
    }
}

fn bind_type_variable(name: &str, ty: &Ty, subst: &mut TypeSubst, env: &TypeEnv) -> bool {
    if let Some(bound) = subst.get(name).cloned() {
        return unify_types_with_subst(&bound, ty, subst, env);
    }
    if matches!(ty, Ty::Var(other) if other == name) {
        return true;
    }
    subst.insert(name.to_string(), apply_type_substitution(ty, subst, env));
    true
}

fn unify_types_with_subst(
    expected: &Ty,
    actual: &Ty,
    subst: &mut TypeSubst,
    env: &TypeEnv,
) -> bool {
    let expected = apply_type_substitution(expected, subst, env);
    let actual = apply_type_substitution(actual, subst, env);
    match (expected, actual) {
        (Ty::Unknown, _) | (_, Ty::Unknown) => true,
        (Ty::Var(name), ty) => bind_type_variable(&name, &ty, subst, env),
        (ty, Ty::Var(name)) => bind_type_variable(&name, &ty, subst, env),
        (Ty::Int, Ty::Int) | (Ty::Bool, Ty::Bool) | (Ty::Str, Ty::Str) | (Ty::Unit, Ty::Unit) => {
            true
        }
        (Ty::List(left), Ty::List(right)) => unify_types_with_subst(&left, &right, subst, env),
        (Ty::Tuple(left), Ty::Tuple(right)) if left.len() == right.len() => left
            .iter()
            .zip(right.iter())
            .all(|(l, r)| unify_types_with_subst(l, r, subst, env)),
        (
            Ty::Fun {
                params: left_params,
                result: left_result,
            },
            Ty::Fun {
                params: right_params,
                result: right_result,
            },
        ) if left_params.len() == right_params.len() => {
            left_params
                .iter()
                .zip(right_params.iter())
                .all(|(l, r)| unify_types_with_subst(l, r, subst, env))
                && unify_types_with_subst(&left_result, &right_result, subst, env)
        }
        (
            Ty::Con {
                name: left_name,
                args: left_args,
            },
            Ty::Con {
                name: right_name,
                args: right_args,
            },
        ) if left_name == right_name && left_args.len() == right_args.len() => left_args
            .iter()
            .zip(right_args.iter())
            .all(|(l, r)| unify_types_with_subst(l, r, subst, env)),
        (
            Ty::Handler {
                covered_ops: left_ops,
            },
            Ty::Handler {
                covered_ops: right_ops,
            },
        ) => left_ops == right_ops,
        _ => false,
    }
}

fn instantiate_ty_with_fresh_type_vars(
    ty: &Ty,
    mapping: &mut HashMap<String, String>,
    next_id: &mut usize,
) -> Ty {
    match ty {
        Ty::Var(name) => {
            let fresh = mapping.entry(name.clone()).or_insert_with(|| {
                let id = *next_id;
                *next_id += 1;
                format!("__goby_fresh_ty_{}", id)
            });
            Ty::Var(fresh.clone())
        }
        Ty::List(inner) => Ty::List(Box::new(instantiate_ty_with_fresh_type_vars(
            inner, mapping, next_id,
        ))),
        Ty::Tuple(items) => Ty::Tuple(
            items
                .iter()
                .map(|item| instantiate_ty_with_fresh_type_vars(item, mapping, next_id))
                .collect(),
        ),
        Ty::Fun { params, result } => Ty::Fun {
            params: params
                .iter()
                .map(|param| instantiate_ty_with_fresh_type_vars(param, mapping, next_id))
                .collect(),
            result: Box::new(instantiate_ty_with_fresh_type_vars(
                result, mapping, next_id,
            )),
        },
        Ty::Con { name, args } => Ty::Con {
            name: name.clone(),
            args: args
                .iter()
                .map(|arg| instantiate_ty_with_fresh_type_vars(arg, mapping, next_id))
                .collect(),
        },
        Ty::Handler { covered_ops } => Ty::Handler {
            covered_ops: covered_ops.clone(),
        },
        Ty::Int | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => ty.clone(),
    }
}

fn instantiate_handler_clause_signature(
    env: &TypeEnv,
    clause_name: &str,
    next_id: &mut usize,
) -> Option<(Vec<Ty>, Ty)> {
    let op_ty = {
        let effects = effect_candidates_for_operation(env, clause_name);
        if effects.len() == 1 {
            env.lookup(&format!("{}.{}", effects[0], clause_name))
        } else {
            env.lookup(clause_name)
        }
    };
    let Ty::Fun { params, result } = op_ty else {
        return None;
    };
    let mut mapping = HashMap::new();
    let params = params
        .iter()
        .map(|param| instantiate_ty_with_fresh_type_vars(param, &mut mapping, next_id))
        .collect();
    let result = instantiate_ty_with_fresh_type_vars(&result, &mut mapping, next_id);
    Some((params, result))
}

fn ty_contains_type_var(ty: &Ty) -> bool {
    match ty {
        Ty::Var(_) => true,
        Ty::List(inner) => ty_contains_type_var(inner),
        Ty::Tuple(items) => items.iter().any(ty_contains_type_var),
        Ty::Fun { params, result } => {
            params.iter().any(ty_contains_type_var) || ty_contains_type_var(result)
        }
        Ty::Con { args, .. } => args.iter().any(ty_contains_type_var),
        Ty::Handler { .. } => false,
        Ty::Int | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => false,
    }
}

fn parse_tuple_member_index(member: &str) -> Option<usize> {
    if member.is_empty() || !member.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    member.parse::<usize>().ok()
}

fn ty_contains_anonymous_type_hole(ty: &Ty) -> bool {
    match ty {
        Ty::Var(name) => name.starts_with("__goby_type_hole_"),
        Ty::List(inner) => ty_contains_anonymous_type_hole(inner),
        Ty::Tuple(items) => items.iter().any(ty_contains_anonymous_type_hole),
        Ty::Fun { params, result } => {
            params.iter().any(ty_contains_anonymous_type_hole)
                || ty_contains_anonymous_type_hole(result)
        }
        Ty::Con { args, .. } => args.iter().any(ty_contains_anonymous_type_hole),
        Ty::Handler { .. } | Ty::Int | Ty::Bool | Ty::Str | Ty::Unit | Ty::Unknown => false,
    }
}

fn type_hole_conflict_note(expected: &Ty) -> &'static str {
    if ty_contains_anonymous_type_hole(expected) {
        " (anonymous type-hole `_` constraints conflict)"
    } else {
        ""
    }
}

fn check_resume_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
    resume_ctx: Option<&ResumeContext>,
) -> Result<(), TypecheckError> {
    if let Expr::Var(name) = expr
        && name == "Unit"
    {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: "legacy_unit_value_syntax: `Unit` is no longer a value expression; use `()`"
                .to_string(),
        });
    }

    macro_rules! recurse {
        ($e:expr) => {
            check_resume_in_expr($e, env, decl_name, resume_ctx)
        };
        ($e:expr, $child_env:expr) => {
            check_resume_in_expr($e, $child_env, decl_name, resume_ctx)
        };
    }

    match expr {
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) | Expr::Var(_) => Ok(()),
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    recurse!(expr)?;
                }
            }
            Ok(())
        }
        Expr::ListLit { elements, spread } => {
            for item in elements {
                recurse!(item)?;
            }
            if let Some(s) = spread {
                recurse!(s)?;
            }
            check_list_spread_constraints(elements, spread.as_deref(), env, decl_name)?;
            Ok(())
        }
        Expr::TupleLit(items) => {
            for item in items {
                recurse!(item)?;
            }
            Ok(())
        }
        Expr::Qualified { .. } => Ok(()),
        Expr::RecordConstruct { fields, .. } => {
            for (_, value) in fields {
                recurse!(value)?;
            }
            Ok(())
        }
        Expr::BinOp { left, right, .. } => {
            recurse!(left)?;
            recurse!(right)
        }
        Expr::Call { callee, arg } => {
            recurse!(callee)?;
            recurse!(arg)
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                recurse!(arg)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, .. } => recurse!(value),
        Expr::Lambda { param, body } => {
            let child_env = env.with_local(param, Ty::Unknown);
            recurse!(body, &child_env)
        }
        Expr::Handler { clauses } => {
            let mut fresh_type_counter = 0usize;
            for clause in clauses {
                let instantiated = instantiate_handler_clause_signature(
                    env,
                    &clause.name,
                    &mut fresh_type_counter,
                );
                let mut child_env = TypeEnv {
                    globals: env.globals.clone(),
                    locals: env.locals.clone(),
                    type_aliases: env.type_aliases.clone(),
                    record_types: env.record_types.clone(),
                };
                let expected_arg_ty = instantiated.as_ref().map(|(_, result)| result.clone());
                if let Some((param_tys, _)) = instantiated.as_ref() {
                    for (idx, param_name) in clause.params.iter().enumerate() {
                        let ty = param_tys.get(idx).cloned().unwrap_or(Ty::Unknown);
                        child_env.locals.insert(param_name.clone(), ty);
                    }
                } else if let Some(first) = clause.params.first() {
                    child_env.locals.insert(first.clone(), Ty::Unknown);
                }
                if expected_arg_ty.is_none() {
                    // Unknown clause op is diagnosed in effect coverage checks.
                    continue;
                }
                if let Some(stmts) = &clause.parsed_body {
                    let ctx = ResumeContext { expected_arg_ty };
                    check_resume_in_stmts_with_local_env(
                        stmts,
                        &mut child_env,
                        decl_name,
                        Some(&ctx),
                    )?;
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            recurse!(handler)?;
            let mut child_env = TypeEnv {
                globals: env.globals.clone(),
                locals: env.locals.clone(),
                type_aliases: env.type_aliases.clone(),
                record_types: env.record_types.clone(),
            };
            check_resume_in_stmts_with_local_env(body, &mut child_env, decl_name, resume_ctx)
        }
        Expr::Resume { value } => {
            recurse!(value)?;
            let Some(ctx) = resume_ctx else {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: "resume_outside_handler: `resume` can only be used inside handler method bodies".to_string(),
                });
            };
            let Some(expected) = ctx.expected_arg_ty.as_ref() else {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: "resume_in_unknown_operation_context: cannot resolve handler operation signature for this `resume`".to_string(),
                });
            };
            let actual = check_expr(value, env);
            let mut subst = TypeSubst::new();
            let expected_after_subst = apply_type_substitution(expected, &subst, env);
            if actual == Ty::Unknown && ty_contains_type_var(&expected_after_subst) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "resume_unresolved_generic_constraints: cannot resolve generic constraints for `resume` argument (expected `{}` but got unresolved argument type)",
                        ty_name(&expected_after_subst)
                    ),
                });
            }
            if actual != Ty::Unknown && !unify_types_with_subst(expected, &actual, &mut subst, env)
            {
                let expected_rendered = apply_type_substitution(expected, &subst, env);
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "resume_arg_type_mismatch: `resume` expects argument of type `{}` but got `{}`{}",
                        ty_name(&expected_rendered),
                        ty_name(&actual),
                        type_hole_conflict_note(&expected_rendered)
                    ),
                });
            }
            Ok(())
        }
        Expr::Block(stmts) => {
            let mut child_env = TypeEnv {
                globals: env.globals.clone(),
                locals: env.locals.clone(),
                type_aliases: env.type_aliases.clone(),
                record_types: env.record_types.clone(),
            };
            check_resume_in_stmts_with_local_env(stmts, &mut child_env, decl_name, resume_ctx)
        }
        Expr::Case { scrutinee, arms } => {
            recurse!(scrutinee)?;
            let scrutinee_ty = check_expr(scrutinee, env);
            let resolved_scrutinee_ty = env.resolve_alias(&scrutinee_ty, 0);
            for arm in arms {
                if is_list_case_pattern(&arm.pattern)
                    && !matches!(resolved_scrutinee_ty, Ty::List(_) | Ty::Unknown)
                {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "list case pattern requires `List` scrutinee, but got `{}`",
                            ty_name(&resolved_scrutinee_ty)
                        ),
                    });
                }
                let arm_env = env_with_case_pattern_bindings(env, &arm.pattern, &scrutinee_ty);
                recurse!(&arm.body, &arm_env)?;
            }
            Ok(())
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            recurse!(condition)?;
            recurse!(then_expr)?;
            recurse!(else_expr)
        }
    }
}

// ---------------------------------------------------------------------------
// Statement-level checking
// ---------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
fn check_body_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    effect_map: &EffectMap,
    effect_dependency_info: &EffectDependencyInfo,
    required_effects_map: &HashMap<String, Vec<String>>,
    decl_name: &str,
    declared_return_ty: Option<Ty>,
    param_tys: &[(&str, Ty)],
    // Op names (qualified and bare) that are covered by enclosing handler scope.
    covered_ops: &HashSet<String>,
) -> Result<(), TypecheckError> {
    let mut local_env = TypeEnv {
        globals: env.globals.clone(),
        locals: param_tys
            .iter()
            .map(|(name, ty)| (name.to_string(), ty.clone()))
            .collect(),
        type_aliases: env.type_aliases.clone(),
        record_types: env.record_types.clone(),
    };
    let mut local_mutability: HashMap<String, bool> = param_tys
        .iter()
        .map(|(name, _)| (name.to_string(), false))
        .collect();

    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } => {
                if local_mutability.contains_key(name) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "duplicate declaration `{}` in the same scope; use `:=` for mutation",
                            name
                        ),
                    });
                }
                ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                check_unhandled_effects_in_expr(
                    value,
                    &local_env,
                    effect_dependency_info,
                    required_effects_map,
                    effect_map,
                    covered_ops,
                    decl_name,
                )?;
                check_branch_type_consistency_in_expr(value, &local_env, decl_name)?;
                let ty = check_expr(value, &local_env);
                local_env.locals.insert(name.clone(), ty);
                local_mutability.insert(name.clone(), false);
            }
            Stmt::MutBinding { name, value } => {
                if local_mutability.contains_key(name) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "duplicate declaration `{}` in the same scope; use `:=` for mutation",
                            name
                        ),
                    });
                }
                ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                check_unhandled_effects_in_expr(
                    value,
                    &local_env,
                    effect_dependency_info,
                    required_effects_map,
                    effect_map,
                    covered_ops,
                    decl_name,
                )?;
                check_branch_type_consistency_in_expr(value, &local_env, decl_name)?;
                let ty = check_expr(value, &local_env);
                local_env.locals.insert(name.clone(), ty);
                local_mutability.insert(name.clone(), true);
            }
            Stmt::Assign { name, value } => {
                if !local_mutability.contains_key(name) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!("cannot assign to undeclared variable `{}`", name),
                    });
                }
                if !local_mutability.get(name).copied().unwrap_or(false) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "cannot assign to immutable variable `{}`; declare it with `mut` first",
                            name
                        ),
                    });
                }
                ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                check_unhandled_effects_in_expr(
                    value,
                    &local_env,
                    effect_dependency_info,
                    required_effects_map,
                    effect_map,
                    covered_ops,
                    decl_name,
                )?;
                check_branch_type_consistency_in_expr(value, &local_env, decl_name)?;
                let current_ty = local_env.locals.get(name).cloned().unwrap_or(Ty::Unknown);
                let assigned_ty = check_expr(value, &local_env);
                if current_ty != Ty::Unknown
                    && assigned_ty != Ty::Unknown
                    && !env.are_compatible(&current_ty, &assigned_ty)
                {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "assignment type `{}` does not match variable `{}` type `{}`",
                            ty_name(&assigned_ty),
                            name,
                            ty_name(&current_ty)
                        ),
                    });
                }
                if current_ty == Ty::Unknown {
                    local_env.locals.insert(name.clone(), assigned_ty);
                }
            }
            Stmt::Expr(expr) => {
                ensure_no_ambiguous_refs_in_expr(expr, &local_env, decl_name)?;
                check_unhandled_effects_in_expr(
                    expr,
                    &local_env,
                    effect_dependency_info,
                    required_effects_map,
                    effect_map,
                    covered_ops,
                    decl_name,
                )?;
                check_branch_type_consistency_in_expr(expr, &local_env, decl_name)?;
            }
        }
    }

    // Validate the inferred return type of the body against the declared return
    // type, when both are known.  `Ty::Unknown` means we lack enough type
    // information to make a judgement, so we skip the check in that case.
    if let Some(declared) = declared_return_ty.filter(|d| *d != Ty::Unknown) {
        // The "return value" of a body is the last expression statement.
        // If the body ends with a binding there is no return value to check.
        let inferred = stmts
            .iter()
            .rev()
            .find_map(|s| {
                if let Stmt::Expr(expr) = s {
                    Some(check_expr(expr, &local_env))
                } else {
                    None
                }
            })
            .unwrap_or(Ty::Unit);

        if inferred != Ty::Unknown && !env.are_compatible(&declared, &inferred) {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!(
                    "body type `{}` does not match declared return type `{}`",
                    ty_name(&inferred),
                    ty_name(&declared),
                ),
            });
        }
    }

    Ok(())
}

/// Recursively walks `expr` and checks that every effect operation call is covered
/// by the enclosing handler scope (`with`, expressed as `covered_ops`).
/// Only direct calls to effect operations are checked here; calls to user-declared
/// functions that themselves require effects are handled in Step 3.
/// Check that calling `callee_name` does not require effects not yet covered.
/// Reports the first uncovered required effect found.
fn check_callee_required_effects(
    callee_name: &str,
    required_effects_map: &HashMap<String, Vec<String>>,
    effect_map: &EffectMap,
    covered_ops: &HashSet<String>,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    let Some(required) = required_effects_map.get(callee_name) else {
        return Ok(());
    };
    for effect_name in required {
        // Check if any op of this effect is missing from covered_ops.
        // If the effect has no ops registered (e.g. a builtin effect), skip.
        let Some(ops) = effect_map.effect_to_ops.get(effect_name) else {
            continue;
        };
        let all_covered = ops.iter().all(|op| covered_ops.contains(op));
        if !all_covered {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!(
                    "function `{}` requires effect `{}` which is not handled by any enclosing `with` scope",
                    callee_name, effect_name
                ),
            });
        }
    }
    Ok(())
}

fn check_unhandled_effects_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    effect_dependency_info: &EffectDependencyInfo,
    required_effects_map: &HashMap<String, Vec<String>>,
    effect_map: &EffectMap,
    covered_ops: &HashSet<String>,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    fn check_effect_op_call_arg_types_in_handler_scope(
        op_name: &str,
        args: &[&Expr],
        env: &TypeEnv,
        covered_ops: &HashSet<String>,
        decl_name: &str,
    ) -> Result<(), TypecheckError> {
        if env.is_effect_op(op_name)
            && covered_ops.contains(op_name)
            && let Ty::Fun { params, .. } = env.lookup(op_name)
        {
            if args.len() > params.len() {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` expects {} argument(s) but got at least {}",
                        op_name,
                        params.len(),
                        args.len()
                    ),
                });
            }
            let mut subst = TypeSubst::new();
            for (idx, arg) in args.iter().enumerate() {
                let expected = &params[idx];
                if *expected == Ty::Unknown {
                    continue;
                }
                let actual = check_expr(arg, env);
                let expected_after_subst = apply_type_substitution(expected, &subst, env);
                if actual == Ty::Unknown && ty_contains_type_var(&expected_after_subst) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "effect_op_unresolved_generic_constraints: effect operation `{}` argument #{} cannot resolve generic constraints (expected `{}` but argument type is unresolved)",
                            op_name,
                            idx + 1,
                            ty_name(&expected_after_subst)
                        ),
                    });
                }
                if actual != Ty::Unknown
                    && !unify_types_with_subst(expected, &actual, &mut subst, env)
                {
                    let expected_rendered = apply_type_substitution(expected, &subst, env);
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "effect operation `{}` expects argument of type `{}` but got `{}`{}",
                            op_name,
                            ty_name(&expected_rendered),
                            ty_name(&actual),
                            type_hole_conflict_note(&expected_rendered)
                        ),
                    });
                }
            }
        }
        Ok(())
    }

    fn effect_op_call_target_and_args(expr: &Expr) -> Option<(String, Vec<&Expr>)> {
        match expr {
            Expr::Var(name) => Some((name.clone(), Vec::new())),
            Expr::Qualified { receiver, member } => {
                Some((format!("{}.{}", receiver, member), Vec::new()))
            }
            Expr::Call { callee, arg } => {
                let (target, mut args) = effect_op_call_target_and_args(callee)?;
                args.push(arg.as_ref());
                Some((target, args))
            }
            _ => None,
        }
    }

    // Shorthand for recursive calls.
    macro_rules! recurse {
        ($e:expr) => {
            check_unhandled_effects_in_expr(
                $e,
                env,
                effect_dependency_info,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )
        };
        ($e:expr, $child_env:expr) => {
            check_unhandled_effects_in_expr(
                $e,
                $child_env,
                effect_dependency_info,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )
        };
    }

    match expr {
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) => Ok(()),
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    recurse!(expr)?;
                }
            }
            Ok(())
        }
        Expr::ListLit { elements, spread } => {
            for item in elements {
                recurse!(item)?;
            }
            if let Some(s) = spread {
                recurse!(s)?;
            }
            Ok(())
        }
        Expr::TupleLit(items) => {
            for item in items {
                recurse!(item)?;
            }
            Ok(())
        }
        Expr::Var(name) => {
            if env.is_effect_op(name) && !covered_ops.contains(name.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `with` scope",
                        name
                    ),
                });
            }
            // Also check if this is a user function that requires effects (bare reference).
            check_callee_required_effects(
                name,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )
        }
        Expr::Qualified { receiver, member } => {
            let qualified = format!("{}.{}", receiver, member);
            if env.is_effect_op(&qualified) && !covered_ops.contains(qualified.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `with` scope",
                        qualified
                    ),
                });
            }
            Ok(())
        }
        Expr::RecordConstruct { fields, .. } => {
            for (_, value) in fields {
                recurse!(value)?;
            }
            Ok(())
        }
        Expr::BinOp { left, right, .. } => {
            recurse!(left)?;
            recurse!(right)
        }
        Expr::Call { callee, arg } => {
            // Check that callee's required effects are covered before recursing.
            if let Expr::Var(name) = callee.as_ref() {
                check_callee_required_effects(
                    name,
                    required_effects_map,
                    effect_map,
                    covered_ops,
                    decl_name,
                )?;
            }
            if let Some((op_name, args)) = effect_op_call_target_and_args(expr) {
                check_effect_op_call_arg_types_in_handler_scope(
                    &op_name,
                    &args,
                    env,
                    covered_ops,
                    decl_name,
                )?;
            }
            recurse!(callee)?;
            recurse!(arg)
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
        } => {
            let qualified = format!("{}.{}", receiver, method);
            if env.is_effect_op(&qualified) && !covered_ops.contains(qualified.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `with` scope",
                        qualified
                    ),
                });
            }
            let provided: Vec<&Expr> = args.iter().collect();
            check_effect_op_call_arg_types_in_handler_scope(
                &qualified,
                &provided,
                env,
                covered_ops,
                decl_name,
            )?;
            for arg in args {
                recurse!(arg)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, callee } => {
            if env.is_effect_op(callee) && !covered_ops.contains(callee.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `with` scope",
                        callee
                    ),
                });
            }
            check_effect_op_call_arg_types_in_handler_scope(
                callee,
                &[value.as_ref()],
                env,
                covered_ops,
                decl_name,
            )?;
            recurse!(value)?;
            // Also check callee as a user-declared function that requires effects.
            check_callee_required_effects(
                callee,
                required_effects_map,
                effect_map,
                covered_ops,
                decl_name,
            )
        }
        Expr::Lambda { param, body } => {
            let child_env = env.with_local(param, Ty::Unknown);
            recurse!(body, &child_env)
        }
        Expr::Handler { clauses } => {
            let mut fresh_type_counter = 0usize;
            for clause in clauses {
                let Some(effects) = effect_map.op_to_effects.get(&clause.name) else {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "unknown effect operation `{}` in handler expression",
                            clause.name
                        ),
                    });
                };
                if effects.len() > 1 {
                    let mut names: Vec<String> = effects.iter().cloned().collect();
                    names.sort();
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "operation '{}' is ambiguous across effects in Handler(...): {}",
                            clause.name,
                            names.join(", ")
                        ),
                    });
                }
                let effect_name = effects
                    .iter()
                    .next()
                    .expect("single-effect handler clause resolution");
                let qualified_op = format!("{}.{}", effect_name, clause.name);
                let mut clause_covered_ops = covered_ops.clone();
                if let Some(required_effects) = effect_dependency_info
                    .op_required_effects
                    .get(&qualified_op)
                {
                    for required_effect in required_effects {
                        if let Some(required_ops) = effect_map.effect_to_ops.get(required_effect) {
                            clause_covered_ops.extend(required_ops.iter().cloned());
                        }
                    }
                }
                if let Some(stmts) = &clause.parsed_body {
                    let instantiated = instantiate_handler_clause_signature(
                        env,
                        &clause.name,
                        &mut fresh_type_counter,
                    );
                    let params: Vec<(String, Ty)> = if let Some((param_tys, _)) = instantiated {
                        clause
                            .params
                            .iter()
                            .enumerate()
                            .map(|(idx, name)| {
                                (
                                    name.clone(),
                                    param_tys.get(idx).cloned().unwrap_or(Ty::Unknown),
                                )
                            })
                            .collect()
                    } else {
                        clause
                            .params
                            .iter()
                            .map(|name| (name.clone(), Ty::Unknown))
                            .collect()
                    };
                    let param_refs: Vec<(&str, Ty)> = params
                        .iter()
                        .map(|(name, ty)| (name.as_str(), ty.clone()))
                        .collect();
                    check_body_stmts(
                        stmts,
                        env,
                        effect_map,
                        effect_dependency_info,
                        required_effects_map,
                        decl_name,
                        None,
                        &param_refs,
                        &clause_covered_ops,
                    )?;
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            // Validate handler clause bodies under current scope + per-operation dependency rules.
            recurse!(handler)?;
            let handler_covered =
                infer_handler_covered_ops_strict(handler, env, effect_map, decl_name)?;
            let mut merged = covered_ops.clone();
            merged.extend(handler_covered);
            check_body_stmts(
                body,
                env,
                effect_map,
                effect_dependency_info,
                required_effects_map,
                decl_name,
                None,
                &[],
                &merged,
            )
        }
        Expr::Resume { value } => recurse!(value),
        Expr::Block(stmts) => {
            if !matches!(stmts.last(), Some(Stmt::Expr(_))) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: "block expression must end with an expression".to_string(),
                });
            }
            let mut local_env = env.clone();
            for stmt in stmts {
                match stmt {
                    Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                        check_unhandled_effects_in_expr(
                            value,
                            &local_env,
                            effect_dependency_info,
                            required_effects_map,
                            effect_map,
                            covered_ops,
                            decl_name,
                        )?;
                        let ty = check_expr(value, &local_env);
                        local_env.locals.insert(name.clone(), ty);
                    }
                    Stmt::Assign { value, .. } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                        check_unhandled_effects_in_expr(
                            value,
                            &local_env,
                            effect_dependency_info,
                            required_effects_map,
                            effect_map,
                            covered_ops,
                            decl_name,
                        )?;
                    }
                    Stmt::Expr(expr) => {
                        ensure_no_ambiguous_refs_in_expr(expr, &local_env, decl_name)?;
                        check_unhandled_effects_in_expr(
                            expr,
                            &local_env,
                            effect_dependency_info,
                            required_effects_map,
                            effect_map,
                            covered_ops,
                            decl_name,
                        )?;
                    }
                }
            }
            Ok(())
        }
        Expr::Case { scrutinee, arms } => {
            recurse!(scrutinee)?;
            let scrutinee_ty = check_expr(scrutinee, env);
            for arm in arms {
                let arm_env = env_with_case_pattern_bindings(env, &arm.pattern, &scrutinee_ty);
                recurse!(&arm.body, &arm_env)?;
            }
            Ok(())
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            recurse!(condition)?;
            recurse!(then_expr)?;
            recurse!(else_expr)
        }
    }
}

fn ensure_no_ambiguous_refs_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    match expr {
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) => Ok(()),
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    ensure_no_ambiguous_refs_in_expr(expr, env, decl_name)?;
                }
            }
            Ok(())
        }
        Expr::ListLit { elements, spread } => {
            for item in elements {
                ensure_no_ambiguous_refs_in_expr(item, env, decl_name)?;
            }
            if let Some(s) = spread {
                ensure_no_ambiguous_refs_in_expr(s, env, decl_name)?;
            }
            Ok(())
        }
        Expr::TupleLit(items) => {
            for item in items {
                ensure_no_ambiguous_refs_in_expr(item, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Var(name) => ensure_name_not_ambiguous(name, env, decl_name),
        Expr::Qualified { receiver, member } => {
            if let Some(index) = parse_tuple_member_index(member) {
                let receiver_ty = env.lookup(receiver);
                let resolved_receiver_ty = env.resolve_alias(&receiver_ty, 0);
                return match resolved_receiver_ty {
                    Ty::Tuple(items) => {
                        if index < items.len() {
                            Ok(())
                        } else {
                            Err(TypecheckError {
                                declaration: Some(decl_name.to_string()),
                                span: None,
                                message: format!(
                                    "tuple member access index `{}` is out of range for receiver `{}` of type `{}`",
                                    index,
                                    receiver,
                                    ty_name(&Ty::Tuple(items))
                                ),
                            })
                        }
                    }
                    Ty::Unknown => {
                        if env.locals.contains_key(receiver) {
                            Ok(())
                        } else {
                            Err(TypecheckError {
                                declaration: Some(decl_name.to_string()),
                                span: None,
                                message: format!(
                                    "tuple member access `{}` requires tuple receiver, but `{}` type is unresolved",
                                    member, receiver
                                ),
                            })
                        }
                    }
                    other => Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "tuple member access `{}` requires tuple receiver, but `{}` has type `{}`",
                            member,
                            receiver,
                            ty_name(&other)
                        ),
                    }),
                };
            }
            if env.locals.contains_key(receiver) {
                return Ok(());
            }
            ensure_name_not_ambiguous(&format!("{}.{}", receiver, member), env, decl_name)
        }
        Expr::RecordConstruct {
            constructor,
            fields,
        } => {
            ensure_name_not_ambiguous(constructor, env, decl_name)?;
            let Some(record) = env.lookup_record_by_constructor(constructor) else {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!("unknown record constructor `{}`", constructor),
                });
            };
            let mut seen = HashSet::new();
            for (_, value) in fields {
                ensure_no_ambiguous_refs_in_expr(value, env, decl_name)?;
            }
            for (name, value) in fields {
                if !seen.insert(name.clone()) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "duplicate field `{}` in constructor call `{}`",
                            name, constructor
                        ),
                    });
                }
                let Some(expected_ty) = record.fields.get(name) else {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "unknown field `{}` in constructor call `{}`",
                            name, constructor
                        ),
                    });
                };
                let actual_ty = check_expr(value, env);
                if actual_ty != Ty::Unknown && !env.are_compatible(expected_ty, &actual_ty) {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "field `{}` in constructor `{}` has type `{}` but expected `{}`",
                            name,
                            constructor,
                            ty_name(&actual_ty),
                            ty_name(expected_ty),
                        ),
                    });
                }
            }
            if seen.len() != record.fields.len() {
                let mut missing: Vec<String> = record
                    .fields
                    .keys()
                    .filter(|field| !seen.contains(*field))
                    .cloned()
                    .collect();
                missing.sort();
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "missing field(s) in constructor call `{}`: {}",
                        constructor,
                        missing.join(", ")
                    ),
                });
            }
            Ok(())
        }
        Expr::BinOp { left, right, .. } => {
            ensure_no_ambiguous_refs_in_expr(left, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(right, env, decl_name)
        }
        Expr::Call { callee, arg } => {
            ensure_no_ambiguous_refs_in_expr(callee, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(arg, env, decl_name)
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
        } => {
            let qualified = format!("{}.{}", receiver, method);
            ensure_name_not_ambiguous(&qualified, env, decl_name)?;
            for arg in args {
                ensure_no_ambiguous_refs_in_expr(arg, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, callee } => {
            ensure_no_ambiguous_refs_in_expr(value, env, decl_name)?;
            ensure_name_not_ambiguous(callee, env, decl_name)
        }
        Expr::Lambda { param, body } => {
            let child_env = env.with_local(param, Ty::Unknown);
            ensure_no_ambiguous_refs_in_expr(body, &child_env, decl_name)
        }
        Expr::Handler { clauses } => {
            for clause in clauses {
                if let Some(stmts) = &clause.parsed_body {
                    ensure_no_ambiguous_refs_in_stmts(stmts, env, decl_name)?;
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            ensure_no_ambiguous_refs_in_expr(handler, env, decl_name)?;
            ensure_no_ambiguous_refs_in_stmts(body, env, decl_name)
        }
        Expr::Resume { value } => ensure_no_ambiguous_refs_in_expr(value, env, decl_name),
        Expr::Block(stmts) => {
            if !matches!(stmts.last(), Some(Stmt::Expr(_))) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: "block expression must end with an expression".to_string(),
                });
            }
            let mut local_env = env.clone();
            for stmt in stmts {
                match stmt {
                    Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                        let ty = check_expr(value, &local_env);
                        local_env.locals.insert(name.clone(), ty);
                    }
                    Stmt::Assign { value, .. } => {
                        ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                    }
                    Stmt::Expr(expr) => {
                        ensure_no_ambiguous_refs_in_expr(expr, &local_env, decl_name)?;
                    }
                }
            }
            Ok(())
        }
        Expr::Case { scrutinee, arms } => {
            ensure_no_ambiguous_refs_in_expr(scrutinee, env, decl_name)?;
            let scrutinee_ty = check_expr(scrutinee, env);
            for arm in arms {
                let arm_env = env_with_case_pattern_bindings(env, &arm.pattern, &scrutinee_ty);
                ensure_no_ambiguous_refs_in_expr(&arm.body, &arm_env, decl_name)?;
            }
            Ok(())
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            ensure_no_ambiguous_refs_in_expr(condition, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(then_expr, env, decl_name)?;
            ensure_no_ambiguous_refs_in_expr(else_expr, env, decl_name)
        }
    }
}

fn ensure_no_ambiguous_refs_in_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    for stmt in stmts {
        match stmt {
            Stmt::Binding { value, .. }
            | Stmt::MutBinding { value, .. }
            | Stmt::Assign { value, .. } => {
                ensure_no_ambiguous_refs_in_expr(value, env, decl_name)?
            }
            Stmt::Expr(expr) => ensure_no_ambiguous_refs_in_expr(expr, env, decl_name)?,
        }
    }
    Ok(())
}

fn check_branch_type_consistency_in_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    let mut local_env = env.clone();
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                check_branch_type_consistency_in_expr(value, &local_env, decl_name)?;
                let ty = check_expr(value, &local_env);
                local_env.locals.insert(name.clone(), ty);
            }
            Stmt::Assign { value, .. } => {
                check_branch_type_consistency_in_expr(value, &local_env, decl_name)?;
            }
            Stmt::Expr(expr) => {
                check_branch_type_consistency_in_expr(expr, &local_env, decl_name)?;
            }
        }
    }
    Ok(())
}

fn check_branch_type_consistency_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    macro_rules! recurse {
        ($e:expr) => {
            check_branch_type_consistency_in_expr($e, env, decl_name)
        };
        ($e:expr, $child_env:expr) => {
            check_branch_type_consistency_in_expr($e, $child_env, decl_name)
        };
    }

    match expr {
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) | Expr::Var(_) => Ok(()),
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let InterpolatedPart::Expr(expr) = part {
                    recurse!(expr)?;
                }
            }
            Ok(())
        }
        Expr::ListLit { elements, spread } => {
            for item in elements {
                recurse!(item)?;
            }
            if let Some(s) = spread {
                recurse!(s)?;
            }
            Ok(())
        }
        Expr::TupleLit(items) => {
            for item in items {
                recurse!(item)?;
            }
            Ok(())
        }
        Expr::Qualified { .. } => Ok(()),
        Expr::RecordConstruct { fields, .. } => {
            for (_, value) in fields {
                recurse!(value)?;
            }
            Ok(())
        }
        Expr::BinOp { left, right, .. } => {
            recurse!(left)?;
            recurse!(right)
        }
        Expr::Call { callee, arg } => {
            recurse!(callee)?;
            recurse!(arg)
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                recurse!(arg)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, .. } => recurse!(value),
        Expr::Lambda { param, body } => {
            let child_env = env.with_local(param, Ty::Unknown);
            recurse!(body, &child_env)
        }
        Expr::Handler { clauses } => {
            for clause in clauses {
                if let Some(stmts) = &clause.parsed_body {
                    let mut child_env = TypeEnv {
                        globals: env.globals.clone(),
                        locals: env.locals.clone(),
                        type_aliases: env.type_aliases.clone(),
                        record_types: env.record_types.clone(),
                    };
                    if let Some(first) = clause.params.first() {
                        child_env.locals.insert(first.clone(), Ty::Unknown);
                    }
                    check_branch_type_consistency_in_stmts(stmts, &child_env, decl_name)?;
                }
            }
            Ok(())
        }
        Expr::With { handler, body } => {
            recurse!(handler)?;
            let child_env = TypeEnv {
                globals: env.globals.clone(),
                locals: env.locals.clone(),
                type_aliases: env.type_aliases.clone(),
                record_types: env.record_types.clone(),
            };
            check_branch_type_consistency_in_stmts(body, &child_env, decl_name)
        }
        Expr::Resume { value } => recurse!(value),
        Expr::Block(stmts) => check_branch_type_consistency_in_stmts(stmts, env, decl_name),
        Expr::Case { scrutinee, arms } => {
            recurse!(scrutinee)?;
            let scrutinee_ty = check_expr(scrutinee, env);
            let mut merged: Option<Ty> = None;
            for arm in arms {
                let arm_env = env_with_case_pattern_bindings(env, &arm.pattern, &scrutinee_ty);
                recurse!(&arm.body, &arm_env)?;
                let arm_ty = check_expr(&arm.body, &arm_env);
                if let Some(prev) = &merged
                    && *prev != Ty::Unknown
                    && arm_ty != Ty::Unknown
                    && !branch_types_compatible(env, prev, &arm_ty)
                {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: format!(
                            "case branch type mismatch: `{}` vs `{}`",
                            ty_name(prev),
                            ty_name(&arm_ty)
                        ),
                    });
                }
                merged = Some(match merged {
                    Some(prev) => merge_branch_type(env, prev, arm_ty),
                    None => arm_ty,
                });
            }
            Ok(())
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            recurse!(condition)?;
            recurse!(then_expr)?;
            recurse!(else_expr)?;
            let then_ty = check_expr(then_expr, env);
            let else_ty = check_expr(else_expr, env);
            if then_ty != Ty::Unknown
                && else_ty != Ty::Unknown
                && !branch_types_compatible(env, &then_ty, &else_ty)
            {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "if branch type mismatch: then is `{}`, else is `{}`",
                        ty_name(&then_ty),
                        ty_name(&else_ty)
                    ),
                });
            }
            Ok(())
        }
    }
}

fn ensure_name_not_ambiguous(
    name: &str,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    if env.locals.contains_key(name) {
        return Ok(());
    }
    if let Some(sources) = env.ambiguous_sources(name) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: format!(
                "name `{}` is ambiguous due to name resolution collision: {}",
                name,
                sources.join(", ")
            ),
        });
    }
    Ok(())
}

fn ty_name(ty: &Ty) -> String {
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

fn validate_type_annotation(
    decl_name: &str,
    annotation: &str,
    known_effects: &HashSet<String>,
    embedded_default_effects: &HashSet<String>,
) -> Result<(), TypecheckError> {
    if uses_legacy_void(annotation) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: "legacy `void` is not supported; use `Unit`".to_string(),
        });
    }

    validate_effect_clause(
        decl_name,
        annotation,
        known_effects,
        embedded_default_effects,
    )?;

    let base = strip_effect_clause(annotation).trim();
    if base.is_empty() {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: "type annotation must not be empty".to_string(),
        });
    }

    if base.contains("->") {
        let Some(ft) = parse_function_type(base) else {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: "invalid function type annotation".to_string(),
            });
        };
        let mut segments = ft.arguments;
        segments.push(ft.result);
        for segment in &segments {
            let Some(type_expr) = parse_type_expr(segment) else {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: "invalid function type annotation".to_string(),
                });
            };
            validate_handler_type_expr(decl_name, &type_expr, known_effects)?;
        }
    } else {
        let Some(type_expr) = parse_type_expr(base) else {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: "invalid type annotation".to_string(),
            });
        };
        validate_handler_type_expr(decl_name, &type_expr, known_effects)?;
    }

    Ok(())
}

fn validate_handler_type_expr(
    decl_name: &str,
    expr: &TypeExpr,
    known_effects: &HashSet<String>,
) -> Result<(), TypecheckError> {
    match expr {
        TypeExpr::Name(_) => Ok(()),
        TypeExpr::Tuple(items) => {
            for item in items {
                validate_handler_type_expr(decl_name, item, known_effects)?;
            }
            Ok(())
        }
        TypeExpr::Function { arguments, result } => {
            for arg in arguments {
                validate_handler_type_expr(decl_name, arg, known_effects)?;
            }
            validate_handler_type_expr(decl_name, result, known_effects)
        }
        TypeExpr::Apply { head, args } => {
            validate_handler_type_expr(decl_name, head, known_effects)?;
            for arg in args {
                validate_handler_type_expr(decl_name, arg, known_effects)?;
            }
            if let TypeExpr::Name(name) = head.as_ref()
                && name == "Handler"
            {
                if args.is_empty() {
                    return Err(TypecheckError {
                        declaration: Some(decl_name.to_string()),
                        span: None,
                        message: "Handler type must include at least one effect".to_string(),
                    });
                }
                for arg in args {
                    let TypeExpr::Name(effect_name) = arg else {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: None,
                            message: "Handler type arguments must be effect names (identifiers)"
                                .to_string(),
                        });
                    };
                    if !known_effects.contains(effect_name) {
                        return Err(TypecheckError {
                            declaration: Some(decl_name.to_string()),
                            span: None,
                            message: format!(
                                "unknown effect `{}` in `Handler(...)` type annotation",
                                effect_name
                            ),
                        });
                    }
                }
            }
            Ok(())
        }
    }
}

fn uses_legacy_void(annotation: &str) -> bool {
    annotation
        .split(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
        .any(|token| token == "void")
}

fn validate_effect_clause(
    decl_name: &str,
    annotation: &str,
    known_effects: &HashSet<String>,
    embedded_default_effects: &HashSet<String>,
) -> Result<(), TypecheckError> {
    let Some(effect_idx) = find_can_keyword_index(annotation) else {
        return Ok(());
    };

    let effects_raw = annotation[effect_idx + 3..].trim();
    if effects_raw.is_empty() {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: "effect list after `can` must not be empty".to_string(),
        });
    }

    for effect_name in effects_raw.split(',').map(str::trim) {
        if !is_identifier(effect_name) {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!("invalid effect name `{}` in type annotation", effect_name),
            });
        }
        let is_main_relaxed_embedded =
            decl_name == "main" && embedded_default_effects.contains(effect_name);
        if !known_effects.contains(effect_name) && !is_main_relaxed_embedded {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: format!("unknown effect `{}` in `can` clause", effect_name),
            });
        }
    }

    Ok(())
}

fn strip_effect_clause(annotation: &str) -> &str {
    match find_can_keyword_index(annotation) {
        Some(idx) => &annotation[..idx],
        None => annotation,
    }
}

pub(crate) fn find_can_keyword_index(annotation: &str) -> Option<usize> {
    for (idx, _) in annotation.char_indices() {
        let rest = &annotation[idx..];
        if !rest.starts_with("can") {
            continue;
        }

        let has_left_whitespace = annotation[..idx]
            .chars()
            .last()
            .is_some_and(char::is_whitespace);
        if !has_left_whitespace {
            continue;
        }

        let has_right_whitespace = annotation[idx + 3..]
            .chars()
            .next()
            .is_none_or(char::is_whitespace);
        if !has_right_whitespace {
            continue;
        }

        return Some(idx);
    }

    None
}

pub(crate) fn is_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    use crate::parse_module;
    use crate::stdlib::StdlibResolver;

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
                "goby_typecheck_{}_{}_{}",
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

    #[test]
    fn typechecks_examples() {
        let hello = std::fs::read_to_string(format!(
            "{}/../../examples/hello.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("hello example should exist");
        let basic = std::fs::read_to_string(format!(
            "{}/../../examples/basic_types.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("basic_types example should exist");
        let generic_types = std::fs::read_to_string(format!(
            "{}/../../examples/generic_types.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("generic_types example should exist");
        let import_example = std::fs::read_to_string(format!(
            "{}/../../examples/import.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("import example should exist");
        let control_flow = std::fs::read_to_string(format!(
            "{}/../../examples/control_flow.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("control_flow example should exist");
        let type_example = std::fs::read_to_string(format!(
            "{}/../../examples/type.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("type example should exist");
        let effect_example = std::fs::read_to_string(format!(
            "{}/../../examples/effect.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("effect example should exist");
        let iterator_example = std::fs::read_to_string(format!(
            "{}/../../examples/iterator.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("iterator example should exist");
        let iterator_unified_example = std::fs::read_to_string(format!(
            "{}/../../examples/iterator_unified.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("iterator_unified example should exist");
        let list_case_example = std::fs::read_to_string(format!(
            "{}/../../examples/list_case.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("list_case example should exist");

        let hello_module = parse_module(&hello).expect("hello should parse");
        let basic_module = parse_module(&basic).expect("basic_types should parse");
        let generic_types_module =
            parse_module(&generic_types).expect("generic_types should parse");
        let import_module = parse_module(&import_example).expect("import example should parse");
        let control_flow_module = parse_module(&control_flow).expect("control_flow should parse");
        let type_module = parse_module(&type_example).expect("type should parse");
        let effect_module = parse_module(&effect_example).expect("effect.gb should parse");
        let iterator_module = parse_module(&iterator_example).expect("iterator.gb should parse");
        let iterator_unified_module =
            parse_module(&iterator_unified_example).expect("iterator_unified.gb should parse");
        let list_case_module = parse_module(&list_case_example).expect("list_case should parse");

        typecheck_module(&hello_module).expect("hello should typecheck");
        typecheck_module(&basic_module).expect("basic_types should typecheck");
        typecheck_module(&generic_types_module).expect("generic_types should typecheck");
        typecheck_module(&import_module).expect("import example should typecheck");
        typecheck_module(&control_flow_module).expect("control_flow should typecheck");
        typecheck_module(&type_module).expect("type example should typecheck");
        typecheck_module(&effect_module).expect("effect.gb should typecheck");
        typecheck_module(&iterator_module).expect("iterator.gb should typecheck");
        typecheck_module(&iterator_unified_module).expect("iterator_unified.gb should typecheck");
        typecheck_module(&list_case_module).expect("list_case should typecheck");
    }

    #[test]
    fn rejects_void_main_type() {
        let module =
            parse_module("main : void -> void\nmain = print \"legacy\"\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("void main type should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(err.message.contains("use `Unit`"));
    }

    #[test]
    fn rejects_void_in_non_main_annotation() {
        let module = parse_module("legacy : void\nlegacy = 0\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("void annotation should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("legacy"));
        assert!(err.message.contains("use `Unit`"));
    }

    #[test]
    fn rejects_empty_effect_list() {
        let module = parse_module("x : Int can \nx = 1\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("empty effect list should fail");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(err.message.contains("effect list"));
    }

    #[test]
    fn accepts_tab_separated_effect_clause() {
        let source = "effect Log\n  log: String -> Unit\nx : Int can\tLog\nx = 1\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("tab-separated `can` clause should be accepted");
    }

    #[test]
    fn accepts_well_formed_type_declarations() {
        let source = "\
type UserID = String
type UserStatus = Activated | Deactivated
type User = User(id: UserID, name: String, status: UserStatus)
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("well-formed type declarations should pass");
    }

    #[test]
    fn typechecks_record_constructor_and_field_access() {
        let source = "\
type UserID = String
type UserStatus = Activated | Deactivated
type User = User(id: UserID, name: String, status: UserStatus)
get_name : Unit -> String
get_name =
  user = User(id: \"1234\", name: \"John\", status: UserStatus.Activated)
  user.name
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("record constructor and field access should typecheck");
    }

    #[test]
    fn rejects_duplicate_field_in_record_constructor_call() {
        let source = "\
type UserID = String
type UserStatus = Activated | Deactivated
type User = User(id: UserID, name: String, status: UserStatus)
mk : Unit -> User
mk =
  User(id: \"1234\", id: \"5678\", status: UserStatus.Activated)
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate constructor field should fail");
        assert_eq!(err.declaration.as_deref(), Some("mk"));
        assert!(err.message.contains("duplicate field"));
    }

    #[test]
    fn resolves_record_field_access_for_alias_receiver_type() {
        let mut type_aliases = HashMap::new();
        type_aliases.insert(
            "UserAlias".to_string(),
            Ty::Con {
                name: "User".to_string(),
                args: Vec::new(),
            },
        );
        let mut fields = HashMap::new();
        fields.insert("name".to_string(), Ty::Str);
        let mut record_types = HashMap::new();
        record_types.insert(
            "User".to_string(),
            RecordTypeInfo {
                type_name: "User".to_string(),
                fields,
            },
        );
        let mut locals = HashMap::new();
        locals.insert(
            "user".to_string(),
            Ty::Con {
                name: "UserAlias".to_string(),
                args: Vec::new(),
            },
        );
        let env = TypeEnv {
            globals: HashMap::new(),
            locals,
            type_aliases,
            record_types,
        };
        let expr = crate::ast::Expr::Qualified {
            receiver: "user".to_string(),
            member: "name".to_string(),
        };
        assert_eq!(check_expr(&expr, &env), Ty::Str);
    }

    #[test]
    fn resolves_tuple_member_access_by_index() {
        let mut locals = HashMap::new();
        locals.insert("pair".to_string(), Ty::Tuple(vec![Ty::Bool, Ty::Int]));
        let env = TypeEnv {
            globals: HashMap::new(),
            locals,
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
        };
        let expr = crate::ast::Expr::Qualified {
            receiver: "pair".to_string(),
            member: "1".to_string(),
        };
        assert_eq!(check_expr(&expr, &env), Ty::Int);
    }

    #[test]
    fn tuple_member_access_out_of_range_is_unknown() {
        let mut locals = HashMap::new();
        locals.insert("pair".to_string(), Ty::Tuple(vec![Ty::Bool]));
        let env = TypeEnv {
            globals: HashMap::new(),
            locals,
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
        };
        let expr = crate::ast::Expr::Qualified {
            receiver: "pair".to_string(),
            member: "1".to_string(),
        };
        assert_eq!(check_expr(&expr, &env), Ty::Unknown);
    }

    #[test]
    fn rejects_numeric_qualified_access_on_non_tuple_receiver() {
        let source = "\
type Status = Ready | Busy
main : Unit -> Unit
main =
  print Status.0
";
        let module = parse_module(source).expect("should parse");
        let err =
            typecheck_module(&module).expect_err("numeric member access on non-tuple should fail");
        assert!(
            err.message.contains("tuple member access"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn accepts_numeric_qualified_access_on_global_tuple_receiver() {
        let source = "\
pair : (Bool, Int)
pair = (True, 42)
main : Unit -> Unit
main =
  if pair.0
    print pair.1
  else
    print 0
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("numeric member access on tuple-typed global should typecheck");
    }

    #[test]
    fn resolves_nested_aliases_when_checking_type_compatibility() {
        let mut type_aliases = HashMap::new();
        type_aliases.insert("UserID".to_string(), Ty::Str);
        let env = TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases,
            record_types: HashMap::new(),
        };
        let expected = Ty::List(Box::new(Ty::Con {
            name: "UserID".to_string(),
            args: Vec::new(),
        }));
        let actual = Ty::List(Box::new(Ty::Str));
        assert!(env.are_compatible(&expected, &actual));
    }

    #[test]
    fn rejects_duplicate_type_declarations() {
        let source = "\
type User = String
type User = Int
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate type declarations should fail");
        assert_eq!(err.declaration.as_deref(), Some("User"));
        assert!(err.message.contains("duplicate type declaration"));
    }

    #[test]
    fn rejects_duplicate_effect_declarations() {
        let source = "\
effect Log
  log: String -> Unit
effect Log
  log: String -> Unit
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate effect declarations should fail");
        assert_eq!(err.declaration.as_deref(), Some("Log"));
        assert!(err.message.contains("duplicate effect declaration"));
    }

    // -----------------------------------------------------------------------
    // with / unhandled-effect tests
    // -----------------------------------------------------------------------

    #[test]
    fn rejects_direct_effect_op_call_outside_with() {
        // `log x` is called directly in `main` without any `with`.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        let err =
            typecheck_module(&module).expect_err("unhandled effect op call should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(
            err.message.contains("not handled"),
            "unexpected message: {}",
            err.message
        );
        assert!(err.message.contains("log"));
    }

    #[test]
    fn accepts_effect_op_call_inside_with_scope() {
        // `log x` is called inside a `with` block.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("effect op call inside with should be accepted");
    }

    #[test]
    fn accepts_effect_op_call_inside_with() {
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    log msg ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("effect op call inside with should be accepted");
    }

    #[test]
    fn accepts_effect_op_call_inside_with_variable() {
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  h = handler
    log msg ->
      resume ()
  with h
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("effect op call inside with <handler-var> should be accepted");
    }

    #[test]
    fn rejects_unknown_operation_in_handler_expression() {
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    unknown_op msg ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("unknown operation in handler expression should fail");
        assert!(err.message.contains("unknown effect operation"));
        assert!(err.message.contains("unknown_op"));
    }

    #[test]
    fn rejects_ambiguous_operation_in_handler_expression() {
        let source = "\
effect Log
  log: String -> Unit
effect Logger
  log: String -> Unit
main : Unit -> Unit
main =
  with
    log msg ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("ambiguous operation in handler expression should fail");
        assert!(err.message.contains("ambiguous"));
        assert!(err.message.contains("log"));
    }

    #[test]
    fn rejects_with_non_handler_expression() {
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with 1
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("with non-handler expression should fail");
        assert!(err.message.contains("with"));
        assert!(err.message.contains("handler value"));
    }

    #[test]
    fn accepts_handler_return_annotation_with_matching_handler_value() {
        let source = "\
effect Log
  log: String -> Unit
mk : Unit -> Handler(Log)
mk =
  h = handler
    log msg ->
      resume ()
  h
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("matching Handler(Log) annotation should pass");
    }

    #[test]
    fn accepts_handler_return_annotation_with_order_insensitive_effect_list() {
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> Unit
mk : Unit -> Handler(Env, Log)
mk =
  h = handler
    log msg ->
      resume ()
    from_env key ->
      resume ()
  h
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("Handler effect list order should be ignored");
    }

    #[test]
    fn rejects_handler_return_annotation_when_effect_set_mismatches() {
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> Unit
mk : Unit -> Handler(Log)
mk =
  h = handler
    from_env key ->
      resume ()
  h
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("mismatched Handler annotation should fail");
        assert!(err.message.contains("body type"));
        assert!(err.message.contains("Handler"));
    }

    #[test]
    fn rejects_unknown_effect_in_handler_type_annotation() {
        let source = "\
effect Log
  log: String -> Unit
mk : Unit -> Handler(Log, MissingEffect)
mk =
  handler
    log msg ->
      resume ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("unknown effect in Handler annotation should fail");
        assert!(err.message.contains("unknown effect"));
        assert!(err.message.contains("Handler"));
    }

    #[test]
    fn accepts_qualified_effect_op_inside_with() {
        // `Log.log x` (qualified form) inside a `with` block.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    Log.log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("qualified effect op call inside with should be accepted");
    }

    #[test]
    fn rejects_effect_op_when_wrong_handler_used() {
        // `with` only covers `Log` ops; calling `Env.from_env` is unhandled.
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> String
main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    from_env \"PATH\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("wrong handler should not cover unrelated effect op");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(err.message.contains("not handled"));
        assert!(err.message.contains("from_env"));
    }

    #[test]
    fn accepts_can_clause_ops_inside_function_body() {
        // A function with `can Log` may call `log` in its own body without handlers at call site.
        let source = "\
effect Log
  log: String -> Unit
f : String -> Unit can Log
f msg =
  log msg
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("can-declared effect op should be allowed in function body");
    }

    #[test]
    fn rejects_effect_op_in_binding_value_outside_with() {
        // Effect op used in binding RHS, no enclosing `with`.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  x = log \"hi\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("effect op in binding outside with should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(err.message.contains("not handled"));
    }

    #[test]
    fn rejects_effect_op_as_pipeline_callee_outside_with() {
        // `"hello" |> log` — effect op used as pipeline callee without `with`.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  \"hello\" |> log
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("effect op as pipeline callee outside with should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(err.message.contains("not handled"));
        assert!(err.message.contains("log"));
    }

    #[test]
    fn accepts_effect_op_as_pipeline_callee_inside_with_scope() {
        // `"hello" |> log` inside `with` should be accepted.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    \"hello\" |> log
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("effect op as pipeline callee inside with should be accepted");
    }

    #[test]
    fn accepts_lambda_param_shadowing_effect_op_name() {
        // `|log| -> log "hi"` — `log` inside the lambda refers to the parameter,
        // not the effect op; should not be flagged as unhandled.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  f = |log| -> log \"hi\"
  f \"ignored\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("lambda param shadowing effect op name should not be flagged");
    }

    #[test]
    fn accepts_nested_with_with_merged_covered_ops() {
        // Outer `with(log)` + inner `with(from_env)`; inner body calls both ops.
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> String
main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    with
      from_env str ->
        resume str
    in
      log \"hi\"
      from_env \"PATH\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("nested with scopes with merged covered ops should be accepted");
    }

    #[test]
    fn accepts_multi_op_effect_via_can_clause() {
        // `can Log` where `Log` has two ops (`log` and `warn`); both usable in the body.
        let source = "\
effect Log
  log: String -> Unit
  warn: String -> Unit
f : String -> Unit can Log
f msg =
  log msg
  warn msg
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("multi-op effect via can clause should allow all ops in body");
    }

    // ── Step 3: calling effectful functions requires an appropriate handler scope ──

    #[test]
    fn rejects_call_to_effectful_function_outside_with() {
        // `plus_ten_with_log` requires the `Log` effect; calling it from `main` without
        // `with` should be rejected.
        let source = "\
effect Log
  log: String -> Unit
plus_ten_with_log : Int -> Int can Log
plus_ten_with_log n =
  log \"calling\"
  n
main : Unit -> Unit
main =
  plus_ten_with_log 3
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("calling effectful function without with should fail");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(
            err.message.contains("unhandled effect") || err.message.contains("Log"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_call_to_effectful_function_inside_with_scope() {
        // Same call, but wrapped in `with` — should succeed.
        let source = "\
effect Log
  log: String -> Unit
plus_ten_with_log : Int -> Int can Log
plus_ten_with_log n =
  log \"calling\"
  n
main : Unit -> Unit
main =
  with
    log msg ->
      resume ()
  in
    plus_ten_with_log 3
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("calling effectful function inside appropriate with should succeed");
    }

    #[test]
    fn rejects_call_when_partial_handlers_present() {
        // `show_env_var` requires both `Log` and `Env`; only log handler is in scope.
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> String
show_env_var : String -> Unit can Log, Env
show_env_var name =
  v = from_env name
  log v
main : Unit -> Unit
main =
  with
    log msg ->
      resume ()
  in
    show_env_var \"PATH\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("missing Env handler should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(
            err.message.contains("unhandled effect") || err.message.contains("Env"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_effectful_pipeline_callee_inside_with_scope() {
        // `3 |> plus_ten_with_log` inside `with` — pipeline form should also pass.
        let source = "\
effect Log
  log: String -> Unit
plus_ten_with_log : Int -> Int can Log
plus_ten_with_log n =
  log \"calling\"
  n
main : Unit -> Unit
main =
  with
    log msg ->
      resume ()
  in
    3 |> plus_ten_with_log
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("effectful pipeline callee inside with should succeed");
    }

    #[test]
    fn rejects_unknown_type_in_alias_target() {
        let source = "type UserID = UnknownType\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("unknown alias target should fail");
        assert_eq!(err.declaration.as_deref(), Some("UserID"));
        assert!(err.message.contains("unknown type"));
    }

    #[test]
    fn rejects_duplicate_union_constructor_names() {
        let source = "type Flag = On | Off | On\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate union constructor should fail");
        assert_eq!(err.declaration.as_deref(), Some("Flag"));
        assert!(err.message.contains("duplicate constructor"));
    }

    #[test]
    fn rejects_duplicate_record_field_names() {
        let source = "type User = User(id: String, id: String)\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate record field should fail");
        assert_eq!(err.declaration.as_deref(), Some("User"));
        assert!(err.message.contains("duplicate field"));
    }

    #[test]
    fn accepts_record_field_with_list_string_type() {
        let source = "type S = S(xs: List String)\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("record field List String type should be accepted");
    }

    #[test]
    fn rejects_malformed_generic_record_field_type() {
        let source = "type S = S(xs: List (String)\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("malformed generic field type should fail");
        assert_eq!(err.declaration.as_deref(), Some("S"));
        assert!(err.message.contains("invalid field type"));
    }

    #[test]
    fn rejects_invalid_effect_name() {
        // `Log` is declared so the identifier check can reach `1Bad`.
        let source = "effect Log\n  log: String -> Unit\nx : Int can Log, 1Bad\nx = 1\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("invalid effect name should fail");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(err.message.contains("invalid effect name"));
    }

    #[test]
    fn rejects_unknown_effect_in_effect_member_can_clause() {
        let source = "\
effect Trace
  trace : String -> Unit can Ghost
main : Unit -> Unit
main = ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("unknown effect in effect member can-clause should fail");
        assert_eq!(err.declaration.as_deref(), Some("Trace"));
        assert!(err.message.contains("unknown effect `Ghost`"));
    }

    #[test]
    fn rejects_unknown_effect_type_parameter_in_effect_member_type_annotation() {
        let source = "
effect Iter a
  op : b -> Unit
main : Unit -> Unit
main = ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("unknown effect type parameter in member annotation should fail");
        assert!(err.message.contains("unknown effect type parameter `b`"));
    }

    #[test]
    fn rejects_effect_dependency_cycle_in_effect_member_can_clause() {
        let source = "\
effect A
  a : Unit -> Unit can B
effect B
  b : Unit -> Unit can A
main : Unit -> Unit
main = ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("effect dependency cycle should fail");
        assert!(err.message.contains("effect dependency cycle detected"));
        assert!(err.message.contains("A -> B -> A"));
    }

    #[test]
    fn rejects_self_effect_dependency_cycle_in_effect_member_can_clause() {
        let source = "\
effect A
  a : Unit -> Unit can A
main : Unit -> Unit
main = ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("self effect dependency cycle should fail");
        assert!(err.message.contains("effect dependency cycle detected"));
        assert!(err.message.contains("A -> A"));
    }

    #[test]
    fn rejects_effect_use_in_handler_clause_when_member_dependency_is_not_declared() {
        let source = "\
effect Log
  log : String -> Unit
effect Trace
  trace : String -> Unit
main : Unit -> Unit can Trace
main =
  with
    trace msg ->
      log msg
  in
    trace \"x\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("handler clause should reject undeclared effect dependency");
        assert!(
            err.message
                .contains("effect operation `log` is not handled")
        );
    }

    #[test]
    fn accepts_effect_use_in_handler_clause_when_member_dependency_is_declared() {
        let source = "\
effect Log
  log : String -> Unit
effect Trace
  trace : String -> Unit can Log
main : Unit -> Unit can Trace
main =
  with
    trace msg ->
      log msg
  in
    trace \"x\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("handler clause should allow effect usage declared via member can-clause");
    }

    #[test]
    fn accepts_can_clause_with_implicit_prelude_print_effect() {
        // `can Print` is accepted via implicit `goby/prelude` embed defaults.
        let source = "main : Unit -> Unit can Print\nmain = print \"hi\"\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("implicit prelude Print effect should be accepted in `can` clause");
    }

    #[test]
    fn accepts_println_call_with_implicit_prelude_print_effect() {
        let source = "main : Unit -> Unit can Print\nmain = println \"hi\"\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("implicit prelude Print effect should resolve `println` operation");
    }

    #[test]
    fn accepts_can_clause_with_implicit_prelude_read_effect() {
        // `can Read` is accepted via implicit `goby/prelude` embed defaults.
        let source = "main : Unit -> Unit can Read\nmain = ()\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("implicit prelude Read effect should be accepted in `can` clause");
    }

    #[test]
    fn accepts_spaced_unit_argument_call_for_read_line() {
        let source = "\
main : Unit -> Unit can Read
main =
  line = read_line ()
  print line
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`read_line ()` should typecheck as Unit-arg call");
    }

    #[test]
    fn accepts_parenthesized_unit_argument_call_for_read_line() {
        let source = "\
main : Unit -> Unit can Read
main =
  line = read_line()
  print line
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`read_line()` should typecheck as Unit-arg call");
    }

    #[test]
    fn accepts_can_clause_with_explicit_context_prelude() {
        let sandbox = TempDirGuard::new("implicit_prelude_context");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = sandbox.path.join("user/main.gb");
        fs::create_dir_all(stdlib_root.join("goby")).expect("stdlib/goby should be creatable");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("user path should be creatable");
        fs::write(
            stdlib_root.join("goby/prelude.gb"),
            "effect Print\n  print : String -> Unit\n  println : String -> Unit\n@embed Print __goby_embeded_effect_stdout_handler\n",
        )
        .expect("prelude file should be writable");
        let source = "main : Unit -> Unit can Print\nmain = print \"hi\"\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect("Print should resolve via implicit prelude import");
    }

    #[test]
    fn rejects_print_can_clause_when_prelude_is_missing_in_context_root() {
        let sandbox = TempDirGuard::new("implicit_prelude_missing");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = sandbox.path.join("user/main.gb");
        fs::create_dir_all(&stdlib_root).expect("stdlib root should be creatable");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("user path should be creatable");
        let source = "main : Unit -> Unit can Print\nmain = print \"hi\"\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("missing prelude should reject Print in can-clause");
        assert!(err.message.contains("unknown effect"));
    }

    #[test]
    fn rejects_non_main_can_clause_with_implicit_prelude_effect() {
        let source = "\
f : Unit -> Unit can Print
f = ()
main : Unit -> Unit
main = ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("non-main can-clause should not use implicit-prelude embedded effect");
        assert!(err.message.contains("unknown effect"));
    }

    #[test]
    fn rejects_unknown_effect_in_can_clause() {
        // `can UndeclaredEffect` — no matching `effect UndeclaredEffect` in the module.
        let source = "x : Int -> Int can UndeclaredEffect\nx n = n\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("unknown effect should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(
            err.message.contains("unknown effect"),
            "unexpected message: {}",
            err.message
        );
        assert!(err.message.contains("UndeclaredEffect"));
    }

    #[test]
    fn accepts_can_clause_with_declared_effect() {
        // `can Log` where `effect Log` is declared in the same module.
        let source = "\
effect Log
  log: String -> Unit
x : Int -> Int can Log
x n = n
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("declared effect in `can` clause should be accepted");
    }

    #[test]
    fn accepts_can_clause_with_multiple_declared_effects() {
        // `can Log, Env` where both are declared.
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> String
f : Int -> Int can Log, Env
f n = n
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("multiple declared effects in `can` clause should be accepted");
    }

    #[test]
    fn rejects_second_of_two_effects_when_undeclared() {
        // First effect is declared but second is not.
        let source = "\
effect Log
  log: String -> Unit
f : Int -> Int can Log, Ghost
f n = n
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("undeclared second effect should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(err.message.contains("unknown effect"));
        assert!(err.message.contains("Ghost"));
    }

    #[test]
    fn allows_non_function_type_annotation() {
        let module =
            parse_module("pair : (String, Int)\npair = (\"a\", 1)\n").expect("should parse");
        typecheck_module(&module).expect("tuple type annotation should be accepted");
    }

    #[test]
    fn rejects_tuple_annotation_body_mismatch() {
        // pair : (String, Int) but body returns plain Int — type mismatch.
        let module = parse_module("pair : (String, Int)\npair = 42\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("type mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("pair"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_matching_tuple_annotation_body() {
        // pair : (String, Int) and body returns a (String, Int) tuple.
        let module =
            parse_module("pair : (String, Int)\npair = (\"hello\", 42)\n").expect("should parse");
        typecheck_module(&module).expect("matching tuple annotation should be accepted");
    }

    #[test]
    fn grouped_type_annotation_is_unwrapped() {
        // `n : (Int)` is a grouped type, equivalent to `n : Int`.
        // The body `42` is Int, so this should pass.
        let module = parse_module("n : (Int)\nn = 42\n").expect("should parse");
        typecheck_module(&module).expect("grouped type annotation should be accepted");
    }

    #[test]
    fn grouped_type_annotation_mismatch_is_rejected() {
        // `n : (Int)` but body is String — should be rejected.
        let module = parse_module("n : (Int)\nn = \"oops\"\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("grouped type mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("n"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_malformed_function_type_annotation() {
        let module = parse_module("f : Int -> -> Int\nf = 1\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("malformed function type should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(err.message.contains("invalid function type annotation"));
    }

    // -----------------------------------------------------------------------
    // Expression-level type inference tests
    // -----------------------------------------------------------------------

    #[test]
    fn accepts_list_int_annotation_matching_list_literal_body() {
        let module = parse_module("xs : List Int\nxs = [1, 2]\n").expect("should parse");
        typecheck_module(&module).expect("list literal body should typecheck");
    }

    #[test]
    fn accepts_list_spread_annotation_matching_list_literal_body() {
        let module = parse_module("xs : List Int\nxs = [1, ..[2, 3]]\n").expect("should parse");
        typecheck_module(&module).expect("list spread body should typecheck");
    }

    #[test]
    fn rejects_list_spread_tail_when_tail_is_not_list() {
        let module = parse_module("xs : List Int\nxs = [1, ..2]\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("non-list spread tail should fail");
        assert_eq!(err.declaration.as_deref(), Some("xs"));
        assert!(
            err.message.contains("list spread tail must be"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_list_spread_prefix_element_type_mismatch() {
        let module = parse_module("xs : List Int\nxs = [1, \"x\", ..[2]]\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("spread prefix mismatch should fail");
        assert_eq!(err.declaration.as_deref(), Some("xs"));
        assert!(
            err.message
                .contains("list spread prefix element type mismatch"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_list_spread_tail_element_type_mismatch() {
        let module = parse_module("xs : List Int\nxs = [1, ..[\"x\"]]\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("spread tail element mismatch should fail");
        assert_eq!(err.declaration.as_deref(), Some("xs"));
        assert!(
            err.message
                .contains("list spread tail element type mismatch"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_case_list_pattern_bindings_in_arm_body() {
        let source = r#"
id : Int -> Int
id n = n

head_or_zero : List Int -> Int
head_or_zero xs =
  id
    case xs
      [] -> 0
      [x, ..xxs] -> x
"#;
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("list case bindings should typecheck");
    }

    #[test]
    fn accepts_case_list_pattern_with_wildcard_head() {
        let source = r#"
id : List Int -> List Int
id xs = xs

tail_or_empty : List Int -> List Int
tail_or_empty xs =
  id
    case xs
      [] -> []
      [_, ..tail] -> tail
"#;
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("wildcard head list pattern should typecheck");
    }

    #[test]
    fn accepts_case_fixed_length_and_literal_head_list_patterns() {
        let source = r#"
id : Int -> Int
id n = n

f : List Int -> Int
f xs =
  id
    case xs
      [1] -> 10
      [4, ..] -> 20
      [_, _] -> 30
      [a, ..b] -> a
      _ -> 0
"#;
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("list pattern variants should typecheck");
    }

    #[test]
    fn accepts_case_arm_block_body() {
        let source = r#"
id : Int -> Int
id n = n

f : Int -> Int
f x =
  id
    case x
      0 ->
        y = 1
        y + 10
      _ -> 0
"#;
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("case arm block body should typecheck");
    }

    #[test]
    fn rejects_case_arm_block_without_tail_expression() {
        let source = r#"
id : Int -> Int
id n = n

f : Int -> Int
f x =
  id
    case x
      0 ->
        y = 1
      _ -> 0
"#;
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("case arm block without tail expression should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message
                .contains("block expression must end with an expression"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_list_case_pattern_on_non_list_scrutinee() {
        let source = r#"
id : Int -> Int
id n = n

f : Int -> Int
f x =
  id
    case x
      [head, ..tail] -> head
      _ -> 0
"#;
        let module = parse_module(source).expect("should parse");
        let err =
            typecheck_module(&module).expect_err("list pattern on non-list scrutinee should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message
                .contains("list case pattern requires `List` scrutinee"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_if_branch_type_mismatch() {
        let source = r#"
f : Unit -> Int
f _ =
  if True
    1
  else
    "oops"
"#;
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("if branch type mismatch should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message.contains("if branch type mismatch"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_case_branch_type_mismatch() {
        let source = r#"
f : Int -> Int
f x =
  case x
    0 -> 1
    _ -> "oops"
"#;
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("case branch type mismatch should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message.contains("case branch type mismatch"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn infers_string_literal_type() {
        let module = parse_module("s : String\ns = \"hello\"\n").expect("should parse");
        typecheck_module(&module).expect("string literal body should typecheck");
    }

    #[test]
    fn infers_interpolated_string_type() {
        let module = parse_module("s : String\ns = \"n=${1}\"\n").expect("should parse");
        typecheck_module(&module).expect("interpolated string body should typecheck as String");
    }

    #[test]
    fn infers_equality_as_bool() {
        let module = parse_module("flag : Bool\nflag = 1 == 1\n").expect("should parse");
        typecheck_module(&module).expect("equality result should typecheck as Bool");
    }

    #[test]
    fn infers_string_equality_as_bool() {
        let module = parse_module("flag : Bool\nflag = \"a\" == \"a\"\n").expect("should parse");
        typecheck_module(&module).expect("string equality result should typecheck as Bool");
    }

    #[test]
    fn accepts_bool_literal_annotation_match() {
        let module = parse_module("flag : Bool\nflag = True\n").expect("should parse");
        typecheck_module(&module).expect("Bool literal should typecheck as Bool");
    }

    #[test]
    fn typechecks_function_example() {
        let source = std::fs::read_to_string(format!(
            "{}/../../examples/function.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("function example should exist");
        let module = parse_module(&source).expect("function.gb should parse");
        typecheck_module(&module).expect("function.gb should typecheck");
    }

    #[test]
    fn typechecks_import_example() {
        let source = std::fs::read_to_string(format!(
            "{}/../../examples/import.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("import example should exist");
        let module = parse_module(&source).expect("import.gb should parse");
        typecheck_module(&module).expect("import.gb should typecheck");
    }

    #[test]
    fn baseline_plain_import_works_with_qualified_access() {
        let source = "\
import goby/string
f : Unit -> List String
f = string.split(\"a,b\", \",\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("plain import should expose qualified symbols");
    }

    #[test]
    fn baseline_alias_import_works_with_qualified_access() {
        let source = "\
import goby/list as l
f : Unit -> String
f = l.join([\"a\", \"b\"], \",\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("alias import should expose alias-qualified symbols");
    }

    #[test]
    fn baseline_selective_import_exposes_bare_symbol() {
        let source = "\
import goby/env ( fetch_env_var )
f : Unit -> String
f = fetch_env_var(\"HOME\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("selective import should expose bare symbol");
    }

    #[test]
    fn baseline_bare_print_builtin_is_available_without_import() {
        let source = "main : Unit -> Unit\nmain = print \"hi\"\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("bare print should remain available as builtin");
    }

    #[test]
    fn resolver_first_prefers_file_based_stdlib_exports() {
        let sandbox = TempDirGuard::new("resolver_first");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/env.gb"),
            "fetch_env_var : String -> Int\nfetch_env_var name = 1\n",
        )
        .expect("stdlib file should be writable");
        let resolver = StdlibResolver::new(root);

        let exports = crate::typecheck_validate::module_exports_for_import_with_resolver(
            "goby/env", &resolver,
        )
        .expect("resolver export lookup should succeed");
        let ty = exports
            .get("fetch_env_var")
            .expect("fetch_env_var should be exported");
        assert_eq!(
            ty,
            &Ty::Fun {
                params: vec![Ty::Str],
                result: Box::new(Ty::Int),
            }
        );
    }

    #[test]
    fn main_can_clause_accepts_imported_embedded_default_effect() {
        let sandbox = TempDirGuard::new("embedded_effect_visible");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/stdio.gb"),
            "effect Console\n  log : String -> Unit\n@embed Console __goby_embeded_effect_stdout_handler\nlog : String -> Unit can Console\nlog msg = msg |> print\n",
        )
        .expect("stdlib file should be writable");
        let source_path = sandbox.path.join("main.gb");
        let source = "\
import goby/stdio ( log )
main : Unit -> Unit can Console
main = ()
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&root))
            .expect("main can-clause should accept imported embedded default effect");
    }

    #[test]
    fn rejects_conflicting_embedded_default_handlers_across_imports() {
        let sandbox = TempDirGuard::new("embedded_effect_conflict");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/a.gb"),
            "effect Console\n  log : String -> Unit\n@embed Console __goby_embeded_effect_stdout_handler\nlog : String -> Unit can Console\nlog msg = msg |> print\n",
        )
        .expect("stdlib file should be writable");
        fs::write(
            root.join("goby/b.gb"),
            "effect Console\n  log : String -> Unit\n@embed Console __goby_embeded_effect_other_handler\nlog : String -> Unit can Console\nlog msg = msg |> print\n",
        )
        .expect("stdlib file should be writable");
        let source_path = sandbox.path.join("main.gb");
        let source = "\
import goby/a ( log )
import goby/b
main : Unit -> Unit can Console
main = ()
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&root))
            .expect_err("conflicting embedded defaults across imports should be rejected");
        assert!(
            err.message
                .contains("conflicting embedded default handler for effect"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn non_main_unhandled_embedded_default_effect_is_rejected() {
        let sandbox = TempDirGuard::new("embedded_effect_local_visible");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/console.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "\
effect Console
  log : String -> Unit
@embed Console __goby_embeded_effect_stdout_handler
log_value : String -> Unit can Console
log_value msg = Console.log msg
f : Unit -> Unit
f = log_value \"x\"
main : Unit -> Unit
main = ()
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("non-main unhandled embedded default effect should be rejected");
        assert!(
            err.message.contains("not handled"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn resolver_falls_back_to_builtin_exports_when_file_missing() {
        let sandbox = TempDirGuard::new("resolver_fallback");
        let resolver = StdlibResolver::new(sandbox.path.join("stdlib"));
        let exports = crate::typecheck_validate::module_exports_for_import_with_resolver(
            "goby/env", &resolver,
        )
        .expect("builtin fallback should resolve");
        let ty = exports
            .get("fetch_env_var")
            .expect("fetch_env_var should be available from builtin fallback");
        assert_eq!(
            ty,
            &Ty::Fun {
                params: vec![Ty::Str],
                result: Box::new(Ty::Str),
            }
        );
    }

    #[test]
    fn resolver_parse_failure_is_reported_during_import_validation() {
        let sandbox = TempDirGuard::new("resolver_parse_failure");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(root.join("goby/env.gb"), "fetch_env_var : String ->\n")
            .expect("stdlib file should be writable");
        let resolver = StdlibResolver::new(root);

        let err = crate::typecheck_validate::module_exports_for_import_with_resolver(
            "goby/env", &resolver,
        )
        .expect_err("parse failure should return a typecheck error");
        assert!(
            err.message
                .contains("failed to resolve stdlib module `goby/env`"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn unknown_module_diagnostic_includes_attempted_stdlib_path() {
        let sandbox = TempDirGuard::new("unknown_module_path_diag");
        let resolver = StdlibResolver::new(sandbox.path.join("stdlib"));
        let err = crate::typecheck_validate::module_exports_for_import_with_resolver(
            "goby/unknown_mod",
            &resolver,
        )
        .expect_err("unknown module should fail");
        assert!(err.message.contains("unknown module `goby/unknown_mod`"));
        assert!(err.message.contains("attempted stdlib path"));
        assert!(err.message.contains("goby/unknown_mod.gb"));
    }

    #[test]
    fn resolver_first_preserves_function_shape_for_split() {
        let sandbox = TempDirGuard::new("resolver_split_shape");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/string.gb"),
            "split : String -> String -> List String\nsplit a b = []\n",
        )
        .expect("stdlib file should be writable");
        let resolver = StdlibResolver::new(root);

        let exports = crate::typecheck_validate::module_exports_for_import_with_resolver(
            "goby/string",
            &resolver,
        )
        .expect("resolver export lookup should succeed");
        let ty = exports.get("split").expect("split should be exported");
        assert_eq!(
            ty,
            &Ty::Fun {
                params: vec![Ty::Str, Ty::Str],
                result: Box::new(Ty::List(Box::new(Ty::Str))),
            }
        );
    }

    #[test]
    fn typechecks_file_based_stdlib_symbol_not_in_builtin_table() {
        let source = "\
import goby/string ( length )
f : Unit -> Int
f = length(\"abc\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("file-based stdlib symbol should typecheck");
    }

    #[test]
    fn typechecks_import_from_goby_stdio_module() {
        let source = "\
import goby/stdio
f : Unit -> Unit can Print
f = print \"hi\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("builtin print should remain callable with stdio import");
    }

    #[test]
    fn typechecks_import_from_goby_int_module() {
        let source = "\
import goby/int as i
effect StringParseError
  invalid_integer : String -> Int
f : Unit -> Int can StringParseError
f =
  with
    invalid_integer _ ->
      resume -1
  in
    i.parse(\"42\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("int.parse should typecheck when StringParseError is handled");
    }

    #[test]
    fn typechecks_list_each_with_plain_import() {
        let source = "\
import goby/list
main : Unit -> Unit can Print
main =
  list.each [1, 2] (|n| -> print \"${n}\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("list.each should typecheck via plain import");
    }

    #[test]
    fn typechecks_list_each_with_alias_import() {
        let source = "\
import goby/list as l
main : Unit -> Unit can Print
main =
  l.each [1, 2] (|n| -> print \"${n}\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("list.each should typecheck via alias import");
    }

    #[test]
    fn typechecks_list_each_with_selective_import() {
        let source = "\
import goby/list ( each )
main : Unit -> Unit can Print
main =
  each [1, 2] (|n| -> print \"${n}\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("list.each should typecheck via selective import");
    }

    #[test]
    fn typechecks_with_operation_from_imported_effect_without_redeclaration() {
        let source = "\
import goby/int as i
f : Unit -> Int can StringParseError
f =
  with
    invalid_integer _ ->
      resume -1
  in
    i.parse(\"42\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect(
            "handler op from imported effect should resolve without local effect redeclaration",
        );
    }

    #[test]
    fn typechecks_selective_import_of_type_and_effect_names() {
        let sandbox = TempDirGuard::new("selective_type_effect_import");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/custom.gb"),
            "type Token = Token(value: String)\neffect CustomEffect\n  fail : String -> Int\nto_int : String -> Int can CustomEffect\nto_int s = 0\n",
        )
        .expect("stdlib file should be writable");
        let source_path = sandbox.path.join("main.gb");
        let source = "\
import goby/custom ( Token, CustomEffect )
type Boxed = Boxed(value: Token)
f : Unit -> Int can CustomEffect
f =
  with
    fail _ ->
      resume 0
  in
    1
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&root))
            .expect("selective type/effect import should be accepted");
    }

    #[test]
    fn rejects_same_effect_name_imported_from_multiple_modules() {
        let sandbox = TempDirGuard::new("ambiguous_effect_name_across_imports");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/a.gb"),
            "effect ParseError\n  fail_a : String -> Int\n",
        )
        .expect("stdlib file should be writable");
        fs::write(
            root.join("goby/b.gb"),
            "effect ParseError\n  fail_b : String -> Int\n",
        )
        .expect("stdlib file should be writable");
        let source_path = sandbox.path.join("main.gb");
        let source = "\
import goby/a ( ParseError )
import goby/b ( ParseError )
main : Unit -> Unit
main = ()
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&root))
            .expect_err("same effect name from different imports should be rejected");
        assert!(
            err.message
                .contains("effect `ParseError` has conflicting declarations")
        );
    }

    #[test]
    fn rejects_same_effect_name_from_local_and_imported_declaration() {
        let sandbox = TempDirGuard::new("ambiguous_effect_name_local_and_import");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/a.gb"),
            "effect ParseError\n  fail_a : String -> Int\n",
        )
        .expect("stdlib file should be writable");
        let source_path = sandbox.path.join("main.gb");
        let source = "\
import goby/a ( ParseError )
effect ParseError
  fail_local : String -> Int
main : Unit -> Unit
main = ()
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&root))
            .expect_err("same effect name from local/import should be rejected");
        assert!(
            err.message
                .contains("effect `ParseError` has conflicting declarations")
        );
    }

    #[test]
    fn rejects_selective_import_of_stdio_print_symbol() {
        let source = "\
import goby/stdio ( print )
f : Unit -> Unit can Print
f = print \"hi\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("stdio module should not export print symbol for selective import");
        assert!(
            err.message
                .contains("unknown symbol `print` in import from `goby/stdio`"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_embed_declaration_inside_stdlib_root_with_context() {
        let sandbox = TempDirGuard::new("embed_in_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/stdio.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "effect Print\n  print : String -> Unit\n@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect("@embed under stdlib root should be accepted");
    }

    #[test]
    fn rejects_embed_declaration_outside_stdlib_root_with_context() {
        let sandbox = TempDirGuard::new("embed_outside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = sandbox.path.join("user/main.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("user path should be creatable");
        let source = "effect Print\n  print : String -> Unit\n@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("@embed outside stdlib root should be rejected");
        assert!(
            err.message
                .contains("@embed declarations are only allowed under stdlib root"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_reserved_intrinsic_call_outside_stdlib_root() {
        let sandbox = TempDirGuard::new("intrinsic_call_outside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = sandbox.path.join("user/main.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("user path should be creatable");
        let source = "f : String -> Int\nf s = __goby_string_length s\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("reserved intrinsic calls should be rejected outside stdlib");
        assert!(
            err.message.contains("reserved intrinsic call"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_reserved_intrinsic_declaration_name_outside_stdlib_root() {
        let sandbox = TempDirGuard::new("intrinsic_decl_outside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = sandbox.path.join("user/main.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("user path should be creatable");
        let source = "__goby_string_length : String -> Int\n__goby_string_length s = 0\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("reserved intrinsic declaration names should be rejected outside stdlib");
        assert!(
            err.message.contains("reserved intrinsic name"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_reserved_intrinsic_calls_inside_stdlib_root() {
        let sandbox = TempDirGuard::new("intrinsic_call_inside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/string.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "length : String -> Int\nlength s = __goby_string_length s\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect("reserved intrinsic calls should be accepted under stdlib root");
    }

    #[test]
    fn accepts_each_grapheme_intrinsic_inside_stdlib_root() {
        let sandbox = TempDirGuard::new("intrinsic_each_grapheme_inside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/string.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "\
effect Iterator a b
  yield : a -> b -> (Bool, b)
@embed Iterator __goby_embeded_effect_stdout_handler
count_graphemes : String -> Int can Iterator
count_graphemes s =
  with
    yield _ _ ->
      resume (True, ())
  in
    __goby_string_each_grapheme s
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect("`__goby_string_each_grapheme` should be accepted under stdlib root");
    }

    #[test]
    fn accepts_each_grapheme_intrinsic_unified_iterator_mode_inside_stdlib_root() {
        let sandbox = TempDirGuard::new("intrinsic_each_grapheme_unified_inside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/string.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "\
type GraphemeState = GraphemeState(grapheme: String, current: String)
effect Iterator a b
  yield : a -> b -> (Bool, b)
@embed Iterator __goby_embeded_effect_stdout_handler
f : String -> GraphemeState can Iterator
f s =
  state = GraphemeState(grapheme: \"\", current: \"\")
  out = state
  with
    yield grapheme step ->
      resume (True, step)
  in
    out = __goby_string_each_grapheme s state
  out
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect("unified iterator mode should be accepted under stdlib root");
    }

    #[test]
    fn accepts_string_eq_operator_and_list_push_intrinsic_inside_stdlib_root() {
        let sandbox = TempDirGuard::new("string_eq_operator_and_push_inside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/string.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "\
f : Unit -> List String
f =
  items = __goby_list_push_string [] \"a\"
  ok = \"a\" == \"a\"
  if ok
    items
  else
    __goby_list_push_string [] \"b\"
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root)).expect(
            "`String == String` and `__goby_list_push_string` should be accepted under stdlib root",
        );
    }

    #[test]
    fn rejects_unknown_intrinsic_calls_inside_stdlib_root() {
        let sandbox = TempDirGuard::new("unknown_intrinsic_inside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/string.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "length : String -> Int\nlength s = __goby_string_len s\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("unknown intrinsic calls should be rejected under stdlib root");
        assert!(
            err.message.contains("unknown runtime intrinsic"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn allows_embed_declaration_without_source_context_for_legacy_api_compat() {
        let source = "effect Print\n  print : String -> Unit\n  println : String -> Unit\n@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("legacy typecheck API should remain compatible without source context");
    }

    #[test]
    fn rejects_embed_missing_effect_without_source_context() {
        let source = "@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("missing in-module effect should fail even without source context");
        assert!(
            err.message.contains("must be declared in the same module"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_embed_when_effect_is_not_declared_in_same_module() {
        let sandbox = TempDirGuard::new("embed_missing_effect");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/stdio.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("embedded effect should require in-module effect declaration");
        assert!(
            err.message.contains("must be declared in the same module"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_duplicate_embed_declaration_names_in_stdlib() {
        let sandbox = TempDirGuard::new("embed_duplicate");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/stdio.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "effect Print\n  print : String -> Unit\n  println : String -> Unit\n@embed Print __goby_embeded_effect_stdout_handler\n@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("duplicate embedded effects should be rejected");
        assert!(err.message.contains("duplicate embedded effect"));
    }

    #[test]
    fn rejects_embed_with_invalid_handler_namespace() {
        let sandbox = TempDirGuard::new("embed_invalid_handler_namespace");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/stdio.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "effect Print\n  print : String -> Unit\n  println : String -> Unit\n@embed Print stdout_handler\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("invalid embed handler namespace should be rejected");
        assert!(
            err.message
                .contains("must start with `__goby_embeded_effect_`")
        );
    }

    #[test]
    fn rejects_embed_with_unknown_handler_intrinsic() {
        let sandbox = TempDirGuard::new("embed_unknown_handler_intrinsic");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/stdio.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "effect Print\n  print : String -> Unit\n@embed Print __goby_embeded_effect_missing\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("unknown embedded handler intrinsic should be rejected");
        assert!(err.message.contains("unknown embedded handler intrinsic"));
    }

    #[test]
    fn rejects_unknown_import_module() {
        let module = parse_module("import goby/unknown\nmain : Unit -> Unit\nmain = 1\n")
            .expect("should parse");
        let err = typecheck_module(&module).expect_err("unknown module should fail");
        assert!(err.message.contains("unknown module"));
    }

    #[test]
    fn rejects_unknown_symbol_in_selective_import() {
        let source = "import goby/env ( missing )\nmain : Unit -> Unit\nmain = 1\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("unknown imported symbol should fail");
        assert!(err.message.contains("unknown symbol"));
    }

    #[test]
    fn rejects_used_name_when_import_collides_with_declaration() {
        let source = "\
import goby/env ( fetch_env_var )
fetch_env_var : String -> String
fetch_env_var name = name
main : Unit -> Unit
main =
  fetch_env_var \"GOBY_PATH\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("used ambiguous name should fail");
        assert!(err.message.contains("ambiguous"));
        assert!(err.message.contains("fetch_env_var"));
    }

    #[test]
    fn rejects_unused_name_when_import_collides_with_declaration() {
        let source = "\
import goby/env ( fetch_env_var )
fetch_env_var : String -> String
fetch_env_var name = name
main : Unit -> Unit can Print
main =
  print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("import collisions should fail at name resolution time");
        assert!(err.message.contains("ambiguous"));
        assert!(err.message.contains("fetch_env_var"));
    }

    #[test]
    fn rejects_list_int_annotation_body_mismatch_shows_element_type() {
        // ty_name(Ty::List(Ty::Int)) must appear as "List Int" in the error message,
        // not just "List".
        let module = parse_module("xs : List Int\nxs = \"oops\"\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("type mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("xs"));
        assert!(
            err.message.contains("List Int"),
            "expected 'List Int' in error message, got: {}",
            err.message
        );
    }

    #[test]
    fn rejects_generic_application_mismatch_shows_haskell_style_name() {
        let module = parse_module("x : TypeX a b\nx = 1\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("type mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(
            err.message.contains("TypeX a b"),
            "expected `TypeX a b` in error message, got: {}",
            err.message
        );
    }

    #[test]
    fn rejects_nested_generic_application_mismatch_with_parenthesized_arg() {
        let module = parse_module("x : TypeX (TypeY a b) c\nx = 1\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("type mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(
            err.message.contains("TypeX (TypeY a b) c"),
            "expected nested haskell-style type in error message, got: {}",
            err.message
        );
    }

    #[test]
    fn rejects_constant_annotation_type_mismatch() {
        // `x : Int; x = "hello"` — non-function annotation; body is String not Int.
        let module = parse_module("x : Int\nx = \"hello\"\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("type mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_print_as_last_expr_in_int_returning_function() {
        // `f : Int -> Int` but body ends with `print`, which returns Unit.
        let source = "f : Int -> Int\nf x =\n  x + 1\n  print \"side\"\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("Unit body in Int->Int should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_print_as_last_expr_in_unit_returning_function() {
        // `main : Unit -> Unit` body ending with `print` should be accepted.
        let source = "main : Unit -> Unit\nmain =\n  print \"hi\"\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("Unit-returning function with print body should pass");
    }

    #[test]
    fn accepts_unit_literal_value_for_unit_returning_function() {
        let source = "main : Unit -> Unit\nmain = ()\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`()` should be accepted as Unit value");
    }

    #[test]
    fn rejects_legacy_unit_value_expression() {
        let source = "main : Unit -> Unit\nmain = Unit\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("legacy Unit value expression should fail");
        assert!(err.message.contains("legacy_unit_value_syntax"));
        assert!(err.message.contains("use `()`"));
    }

    #[test]
    fn accepts_constant_annotation_matching_body() {
        // `n : Int; n = 42` — non-function annotation matching body type.
        let module = parse_module("n : Int\nn = 42\n").expect("should parse");
        typecheck_module(&module).expect("matching constant annotation should be accepted");
    }

    #[test]
    fn rejects_body_type_mismatch_int_vs_string() {
        // `f : Int -> Int; f x = "oops"` — body returns String but declared Int.
        let module = parse_module("f : Int -> Int\nf x = \"oops\"\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("type mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_body_type_matching_declared_return() {
        // `double : Int -> Int; double x = x + x` — body type is Int, declared Int.
        let module = parse_module("double : Int -> Int\ndouble x = x + x\n").expect("should parse");
        typecheck_module(&module).expect("matching body type should be accepted");
    }

    #[test]
    fn rejects_function_body_type_mismatch_via_param() {
        // `greet : String -> Int; greet name = name` — param is String, declared return is Int.
        // After A1 fix, `name` resolves to String, which conflicts with declared return Int.
        let module =
            parse_module("greet : String -> Int\ngreet name = name\n").expect("should parse");
        let err =
            typecheck_module(&module).expect_err("type mismatch via param should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("greet"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_function_body_with_param_matching_return_type() {
        // `id : Int -> Int; id x = x` — param x is Int, return is Int — should pass.
        let module = parse_module("id : Int -> Int\nid x = x\n").expect("should parse");
        typecheck_module(&module).expect("identity function should typecheck");
    }

    #[test]
    fn rejects_param_count_mismatch_fewer_params() {
        // Annotation has 2 params but definition only has 1 — should be rejected.
        let module = parse_module("add : Int -> Int -> Int\nadd a = a\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("param count mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("add"));
        assert!(
            err.message.contains("parameter"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_unit_param_omitted_in_definition() {
        // `main : Unit -> Unit; main = ...` — Unit param may be omitted in MVP.
        let module =
            parse_module("main : Unit -> Unit\nmain = print \"hi\"\n").expect("should parse");
        typecheck_module(&module).expect("Unit param omission should be accepted");
    }

    #[test]
    fn rejects_rebinding_in_same_scope() {
        let source = "f : Unit -> Int\nf =\n  a = 1\n  a = a + 1\n  a\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("re-binding in same scope should fail");
        assert!(err.message.contains("duplicate declaration `a`"));
        assert!(err.message.contains("use `:=` for mutation"));
    }

    #[test]
    fn accepts_mut_declaration_and_assignment() {
        let source = "f : Unit -> Int\nf =\n  mut a = 1\n  a := 2\n  a\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("mut declaration and assignment should typecheck");
    }

    #[test]
    fn rejects_assignment_to_immutable_variable() {
        let source = "f : Unit -> Int\nf =\n  a = 1\n  a := 2\n  a\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("assignment to immutable should fail");
        assert!(
            err.message
                .contains("cannot assign to immutable variable `a`")
        );
    }

    #[test]
    fn rejects_assignment_to_undeclared_variable() {
        let source = "f : Unit -> Int\nf =\n  x := 1\n  0\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("assignment to undeclared should fail");
        assert!(
            err.message
                .contains("cannot assign to undeclared variable `x`")
        );
    }

    // --- TypecheckError span regression tests ---

    #[test]
    fn typecheck_error_duplicate_declaration_has_span_with_line() {
        // Two declarations named "foo" — duplicate error; span must point to the second
        // declaration's line (line 3 = annotation line of the second foo).
        let source = "foo : Int\nfoo = 1\nfoo : Int\nfoo = 2\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate decl should fail");
        let span = err.span.expect("duplicate decl error must have a span");
        // Second annotation is on line 3; decl_line = 3.
        assert_eq!(span.line, 3, "span.line should point to second declaration");
        assert_eq!(span.col, 1);
    }

    #[test]
    fn typecheck_error_main_wrong_type_has_span_with_line() {
        // main declared with a non-function type annotation; triggers "must be a function type"
        // error (parse_function_type returns None). span must include a line number.
        let source = "main : Int\nmain = 1\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("wrong main type should fail");
        let span = err.span.expect("main type error must have a span");
        assert_eq!(
            span.line, 1,
            "span.line should point to main declaration line"
        );
        assert_eq!(span.col, 1);
    }

    #[test]
    fn typecheck_error_main_wrong_function_type_has_span() {
        // main declared with a valid function type but wrong signature (not Unit -> Unit).
        // Triggers the second error branch: "main type must be `Unit -> Unit` in MVP".
        let source = "main : Int -> String\nmain x = \"ab\"\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("wrong main function type should fail");
        let span = err.span.expect("main type error must have a span");
        assert_eq!(
            span.line, 1,
            "span.line should point to main declaration line"
        );
        assert_eq!(span.col, 1);
        assert!(err.message.contains("Unit -> Unit"));
    }

    // --- effect op argument type checking (§4.1.1) ---

    #[test]
    fn rejects_effect_op_call_with_wrong_arg_type() {
        // `catch "NoCoffeeError"` when `catch : Error -> Unit` — String is not Error.
        // This should be a typecheck error, not a silent runtime failure.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    catch \"NoCoffeeError\"
";
        let module = parse_module(source).expect("should parse");
        let result = typecheck_module(&module);
        assert!(
            result.is_err(),
            "passing String to an effect op expecting Error should be a typecheck error"
        );
        let err = result.unwrap_err();
        assert!(
            err.message.contains("catch"),
            "error message should mention the op name; got: {}",
            err.message
        );
        assert!(
            err.message.contains("Error"),
            "error message should mention expected type; got: {}",
            err.message
        );
        assert!(
            err.message.contains("String") || err.message.contains("Str"),
            "error message should mention actual type; got: {}",
            err.message
        );
    }

    #[test]
    fn rejects_qualified_effect_op_call_with_wrong_arg_type() {
        // `ErrorEffect.catch "NoCoffeeError"` when `catch : Error -> Unit`.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    ErrorEffect.catch \"NoCoffeeError\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("qualified effect op call type mismatch should fail");
        assert!(err.message.contains("ErrorEffect.catch") || err.message.contains("catch"));
        assert!(err.message.contains("Error"));
        assert!(err.message.contains("String") || err.message.contains("Str"));
    }

    #[test]
    fn rejects_method_style_effect_op_call_with_wrong_arg_type() {
        // `ErrorEffect.catch("NoCoffeeError")` when `catch : Error -> Unit`.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    ErrorEffect.catch(\"NoCoffeeError\")
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("method-style effect op call type mismatch should fail");
        assert!(err.message.contains("ErrorEffect.catch") || err.message.contains("catch"));
        assert!(err.message.contains("Error"));
        assert!(err.message.contains("String") || err.message.contains("Str"));
    }

    #[test]
    fn rejects_multi_arg_effect_op_call_when_later_arg_type_mismatches() {
        let source = "
effect E
  op: String -> Int -> Unit

main : Unit -> Unit
main =
  with
    op s n ->
      resume ()
  in
    E.op \"ok\" \"bad\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("second argument type mismatch should fail");
        assert!(err.message.contains("E.op") || err.message.contains("op"));
        assert!(err.message.contains("Int"));
        assert!(err.message.contains("String") || err.message.contains("Str"));
    }

    #[test]
    fn rejects_multi_arg_method_style_effect_op_call_when_later_arg_type_mismatches() {
        let source = "
effect E
  op: String -> Int -> Unit

main : Unit -> Unit
main =
  with
    op s n ->
      resume ()
  in
    E.op(\"ok\", \"bad\")
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("method-style second argument type mismatch should fail");
        assert!(err.message.contains("E.op") || err.message.contains("op"));
        assert!(err.message.contains("Int"));
        assert!(err.message.contains("String") || err.message.contains("Str"));
    }

    #[test]
    fn rejects_effect_op_call_with_too_many_arguments() {
        let source = "
effect E
  op: String -> Unit

main : Unit -> Unit
main =
  with
    op s ->
      resume ()
  in
    E.op \"ok\" 1
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("too many args should fail");
        assert!(err.message.contains("expects 1 argument(s)"));
    }

    #[test]
    fn rejects_effect_op_pipeline_with_wrong_arg_type() {
        // `"NoCoffeeError" |> catch` when `catch : Error -> Unit` — same mismatch via pipeline.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    \"NoCoffeeError\" |> catch
";
        let module = parse_module(source).expect("should parse");
        let result = typecheck_module(&module);
        assert!(
            result.is_err(),
            "piping String to an effect op expecting Error should be a typecheck error"
        );
        let err = result.unwrap_err();
        assert!(
            err.message.contains("catch"),
            "error message should mention the op name; got: {}",
            err.message
        );
        assert!(
            err.message.contains("Error"),
            "error message should mention expected type; got: {}",
            err.message
        );
        assert!(
            err.message.contains("String") || err.message.contains("Str"),
            "error message should mention actual type; got: {}",
            err.message
        );
    }

    #[test]
    fn accepts_effect_op_pipeline_with_correct_arg_type() {
        // `Error(message: "oops") |> catch` when `catch : Error -> Unit` — should pass.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    Error(message: \"oops\") |> catch
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("correct arg type via pipeline should be accepted");
    }

    #[test]
    fn accepts_effect_op_call_with_correct_arg_type() {
        // catch receives Error(message:...) which matches the declared Error param — should pass.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    catch Error(message: \"oops\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("correct arg type should be accepted");
    }

    #[test]
    fn accepts_effect_op_call_with_string_when_op_expects_string() {
        // log receives String, which is the declared param type — no error.
        // Also covers the Unknown-guard path via the correct type.
        let source = "
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("String arg to String op should be accepted");
    }

    #[test]
    fn accepts_generic_effect_op_call_with_inferred_type_variable() {
        let source = "
effect Iter a
  op: a -> Unit

main : Unit -> Unit
main =
  with
    op x ->
      resume ()
  in
    op 1
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("generic effect op call should infer `a = Int`");
    }

    #[test]
    fn accepts_multiple_calls_to_same_generic_effect_op_with_different_types() {
        let source = "
effect Iter a
  op: a -> Unit

main : Unit -> Unit
main =
  with
    op x ->
      resume ()
  in
    op 1
    op \"two\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("separate calls to same generic effect op should instantiate independently");
    }

    #[test]
    fn rejects_generic_effect_op_call_when_type_variable_constraints_conflict() {
        let source = "
effect Iter a
  pair: a -> a -> Unit

main : Unit -> Unit
main =
  with
    pair x y ->
      resume ()
  in
    pair 1 \"oops\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("conflicting generic args should fail");
        assert!(err.message.contains("pair"));
        assert!(err.message.contains("Int"));
        assert!(err.message.contains("String") || err.message.contains("Str"));
    }

    #[test]
    fn rejects_effect_op_call_when_generic_constraints_are_unresolved() {
        let source = "
effect Iter a
  op: a -> Unit

main : Unit -> Unit
main =
  with
    op _ ->
      resume ()
  in
    op missing
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("unresolved generic constraint in effect-op call should fail");
        assert!(
            err.message
                .contains("effect_op_unresolved_generic_constraints"),
            "unexpected error: {}",
            err.message
        );
        assert!(
            err.message.contains("op"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn rejects_effect_op_call_with_type_hole_conflict_note() {
        let source = "
effect Iter
  op: List _ -> Unit

main : Unit -> Unit
main =
  with
    op _ ->
      resume ()
  in
    op \"oops\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("type-hole conflict should fail");
        assert!(
            err.message
                .contains("anonymous type-hole `_` constraints conflict"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn accepts_effect_op_call_with_independent_anonymous_type_holes() {
        let source = "
effect Iter
  op: _ -> _ -> Unit

main : Unit -> Unit
main =
  with
    op x y ->
      resume ()
  in
    op 1 \"s\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("anonymous type holes should be independent per occurrence");
    }

    #[test]
    fn accepts_positional_single_field_constructor_in_effect_op_call() {
        // `raise Error("msg")` — positional sugar for `raise Error(message: "msg")`.
        // check should accept this because Error has exactly one field.
        let source = "
type Error = Error(message: String)

effect RaiseError
  raise: Error -> Unit

main : Unit -> Unit
main =
  with
    raise e ->
      resume ()
  in
    raise Error(\"oops\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("positional single-field constructor should be accepted");
    }

    #[test]
    fn no_sugar_for_multi_field_constructor() {
        // `raise Pair("a")` when Pair has two fields should NOT be treated as RecordConstruct.
        // It falls through to Expr::Call, type is Unknown → arg type check skipped → Ok.
        // (No false positive: we do not fabricate an error for multi-field positional.)
        let source = "
type Pair = Pair(first: String, second: String)

effect E
  op: Pair -> Unit

main : Unit -> Unit
main =
  with
    op p ->
      resume ()
  in
    op Pair(\"a\")
";
        let module = parse_module(source).expect("should parse");
        // Multi-field positional is not sugar — type is Unknown, no error expected.
        typecheck_module(&module).expect("multi-field positional should not raise false error");
    }

    #[test]
    fn rejects_resume_outside_handler() {
        let source = "
main : Unit -> Unit
main =
  resume 1
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("resume outside handler should fail");
        assert!(
            err.message.contains("resume_outside_handler"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn rejects_resume_arg_type_mismatch_in_handler_method() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume \"oops\"
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("resume arg mismatch should fail");
        assert!(
            err.message.contains("resume_arg_type_mismatch"),
            "unexpected error: {}",
            err.message
        );
        assert!(
            err.message.contains("Int"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn rejects_resume_in_unknown_operation_context() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    unknown x ->
      resume 1
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("unknown op context should fail");
        assert!(
            err.message.contains("resume_in_unknown_operation_context")
                || err.message.contains("unknown effect operation"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn accepts_resume_when_arg_matches_operation_return_type() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("resume with matching return type should pass");
    }

    #[test]
    fn accepts_resume_with_generic_operation_result_type() {
        let source = "
effect Iterator a b
  yield: a -> b -> (Bool, b)

main : Unit -> Unit
main =
  with
    yield x state ->
      resume (True, state)
  in
    yield \"a\" 0
    ()
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("resume should accept generic `(Bool, b)` result");
    }

    #[test]
    fn rejects_resume_when_generic_constraints_are_unresolved() {
        let source = "
effect Iterator a b
  yield: a -> b -> (Bool, b)

main : Unit -> Unit
main =
  with
    yield _ _ ->
      resume missing
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("resume unresolved generic constraints should fail");
        assert!(
            err.message
                .contains("resume_unresolved_generic_constraints"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn rejects_resume_when_generic_operation_result_shape_mismatches() {
        let source = "
effect Iterator a b
  yield: a -> b -> (Bool, b)

main : Unit -> Unit
main =
  with
    yield x state ->
      resume (1, state)
  in
    yield \"a\" 0
    ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("resume should reject incompatible generic result shape");
        assert!(err.message.contains("resume_arg_type_mismatch"));
        assert!(err.message.contains("Bool"));
    }

    #[test]
    fn accepts_multiple_resume_expressions_in_same_handler_method() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1
      resume 2
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("multiple resume expressions should no longer be conservatively rejected");
    }

    #[test]
    fn accepts_multiple_resume_expressions_in_single_expression_for_runtime_validation() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1 + resume 2
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("multi-resume expression should defer invalid progression handling to runtime");
    }

    #[test]
    fn rejects_multiple_resume_expressions_when_one_branch_has_type_mismatch() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    next x ->
      if True
        resume 1
      else
        resume \"oops\"
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("resume type mismatch should still be rejected under multiple branches");
        assert!(err.message.contains("resume_arg_type_mismatch"));
    }

    #[test]
    fn infers_resume_binding_type_from_non_generic_resume_context() {
        let env = TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
        };
        let value = Expr::Resume {
            value: Box::new(Expr::IntLit(1)),
        };
        let ctx = ResumeContext {
            expected_arg_ty: Some(Ty::Int),
        };
        let inferred = infer_binding_ty_with_resume_context(&value, &env, Some(&ctx));
        assert_eq!(inferred, Ty::Int);
    }

    #[test]
    fn keeps_resume_binding_unknown_when_resume_context_is_generic() {
        let env = TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
        };
        let value = Expr::Resume {
            value: Box::new(Expr::Var("x".to_string())),
        };
        let ctx = ResumeContext {
            expected_arg_ty: Some(Ty::Var("T".to_string())),
        };
        let inferred = infer_binding_ty_with_resume_context(&value, &env, Some(&ctx));
        assert_eq!(inferred, Ty::Unknown);
    }

    #[test]
    fn check_expr_infers_addition() {
        let env = TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
        };
        let expr = crate::ast::Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(crate::ast::Expr::IntLit(1)),
            right: Box::new(crate::ast::Expr::IntLit(2)),
        };
        assert_eq!(check_expr(&expr, &env), Ty::Int);
    }
}
