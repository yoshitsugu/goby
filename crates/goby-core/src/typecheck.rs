use std::collections::{HashMap, HashSet};

use crate::{
    Module,
    ast::{BinOpKind, Expr, ImportKind, Stmt},
    types::{TypeExpr, parse_function_type, parse_type_expr},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypecheckError {
    pub declaration: Option<String>,
    pub message: String,
}

pub fn typecheck_module(module: &Module) -> Result<(), TypecheckError> {
    validate_imports(module)?;

    let mut names = HashSet::new();

    for decl in &module.declarations {
        if !names.insert(decl.name.clone()) {
            return Err(TypecheckError {
                declaration: Some(decl.name.clone()),
                message: "duplicate top-level declaration".to_string(),
            });
        }

        if let Some(annotation) = decl.type_annotation.as_deref() {
            validate_type_annotation(&decl.name, annotation)?;
        }
    }

    if let Some(main) = module.declarations.iter().find(|d| d.name == "main") {
        let annotation = main
            .type_annotation
            .as_deref()
            .ok_or_else(|| TypecheckError {
                declaration: Some("main".to_string()),
                message: "main must have an explicit type annotation".to_string(),
            })?;

        let base_annotation = strip_effect_clause(annotation);
        let ty = parse_function_type(base_annotation).ok_or_else(|| TypecheckError {
            declaration: Some("main".to_string()),
            message: "main type annotation must be a function type".to_string(),
        })?;

        if ty.arguments != vec!["Unit".to_string()] || ty.result != "Unit" {
            return Err(TypecheckError {
                declaration: Some("main".to_string()),
                message: "main type must be `Unit -> Unit` in MVP".to_string(),
            });
        }
    }

    // Expression-level type checking (when parsed_body is available).
    let env = build_type_env(module);
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

            check_body_stmts(stmts, &env, &decl.name, declared_return_ty, &param_ty_refs)?;
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Internal type representation
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    Int,
    Bool,
    Str,
    Unit,
    List(Box<Ty>),
    Tuple(Vec<Ty>),
    Fun { params: Vec<Ty>, result: Box<Ty> },
    Var(String),
    Con { name: String, args: Vec<Ty> },
    Unknown,
}

struct TypeEnv {
    globals: HashMap<String, GlobalBinding>,
    locals: HashMap<String, Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum GlobalBinding {
    Resolved { ty: Ty, source: String },
    Ambiguous { sources: Vec<String> },
}

impl TypeEnv {
    fn lookup(&self, name: &str) -> Ty {
        if let Some(ty) = self.locals.get(name) {
            return ty.clone();
        }
        if let Some(binding) = self.globals.get(name) {
            return match binding {
                GlobalBinding::Resolved { ty, .. } => ty.clone(),
                GlobalBinding::Ambiguous { .. } => Ty::Unknown,
            };
        }
        Ty::Unknown
    }

    fn ambiguous_sources(&self, name: &str) -> Option<&[String]> {
        let binding = self.globals.get(name)?;
        if let GlobalBinding::Ambiguous { sources } = binding {
            Some(sources)
        } else {
            None
        }
    }

    fn with_local(&self, name: &str, ty: Ty) -> TypeEnv {
        let mut locals = self.locals.clone();
        locals.insert(name.to_string(), ty);
        TypeEnv {
            globals: self.globals.clone(),
            locals,
        }
    }
}

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

fn build_type_env(module: &Module) -> TypeEnv {
    let mut globals = HashMap::new();
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
    inject_imported_symbols(module, &mut globals);

    TypeEnv {
        globals,
        locals: HashMap::new(),
    }
}

fn validate_imports(module: &Module) -> Result<(), TypecheckError> {
    for import in &module.imports {
        let exports = builtin_module_exports(&import.module_path).ok_or_else(|| TypecheckError {
            declaration: None,
            message: format!("unknown module `{}`", import.module_path),
        })?;

        if let ImportKind::Selective(names) = &import.kind {
            for name in names {
                if !exports.contains_key(name.as_str()) {
                    return Err(TypecheckError {
                        declaration: None,
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

fn inject_imported_symbols(module: &Module, globals: &mut HashMap<String, GlobalBinding>) {
    for import in &module.imports {
        let Some(exports) = builtin_module_exports(&import.module_path) else {
            continue;
        };
        match &import.kind {
            ImportKind::Plain => {
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
                    if let Some(ty) = exports.get(name.as_str()) {
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

fn insert_global_symbol(
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

fn builtin_module_exports(module_path: &str) -> Option<HashMap<&'static str, Ty>> {
    let mut exports = HashMap::new();
    match module_path {
        "goby/string" => {
            exports.insert(
                "concat",
                Ty::Fun {
                    params: vec![Ty::Str, Ty::Str],
                    result: Box::new(Ty::Str),
                },
            );
            exports.insert(
                "split",
                Ty::Fun {
                    params: vec![Ty::Str, Ty::Str],
                    result: Box::new(Ty::List(Box::new(Ty::Str))),
                },
            );
        }
        "goby/list" => {
            exports.insert(
                "join",
                Ty::Fun {
                    params: vec![Ty::List(Box::new(Ty::Str)), Ty::Str],
                    result: Box::new(Ty::Str),
                },
            );
        }
        "goby/env" => {
            exports.insert(
                "fetch_env_var",
                Ty::Fun {
                    params: vec![Ty::Str],
                    result: Box::new(Ty::Str),
                },
            );
        }
        _ => return None,
    }
    Some(exports)
}

fn ty_from_annotation(s: &str) -> Ty {
    let s = s.trim();
    let Some(type_expr) = parse_type_expr(s) else {
        return Ty::Unknown;
    };
    ty_from_type_expr(&type_expr)
}

fn ty_from_type_expr(expr: &TypeExpr) -> Ty {
    match expr {
        TypeExpr::Name(name) => ty_from_name(name),
        TypeExpr::Tuple(items) => Ty::Tuple(items.iter().map(ty_from_type_expr).collect()),
        TypeExpr::Function { arguments, result } => Ty::Fun {
            params: arguments.iter().map(ty_from_type_expr).collect(),
            result: Box::new(ty_from_type_expr(result)),
        },
        TypeExpr::Apply { head, args } => {
            let TypeExpr::Name(name) = head.as_ref() else {
                return Ty::Unknown;
            };
            let converted_args: Vec<Ty> = args.iter().map(ty_from_type_expr).collect();
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

fn is_type_variable_name(name: &str) -> bool {
    name.chars()
        .next()
        .is_some_and(|c| c.is_ascii_lowercase() || c == '_')
}


// ---------------------------------------------------------------------------
// Expression type inference
// ---------------------------------------------------------------------------

fn check_expr(expr: &Expr, env: &TypeEnv) -> Ty {
    match expr {
        Expr::IntLit(_) => Ty::Int,
        Expr::BoolLit(_) => Ty::Bool,
        Expr::StringLit(_) => Ty::Str,
        Expr::ListLit(items) => {
            if items.is_empty() {
                return Ty::List(Box::new(Ty::Unknown));
            }
            let item_ty = check_expr(&items[0], env);
            Ty::List(Box::new(item_ty))
        }
        Expr::TupleLit(items) => {
            let tys: Vec<Ty> = items.iter().map(|i| check_expr(i, env)).collect();
            Ty::Tuple(tys)
        }
        Expr::Var(name) => env.lookup(name),
        Expr::BinOp { op, left, right } => {
            let lt = check_expr(left, env);
            let rt = check_expr(right, env);
            match (op, &lt, &rt) {
                (BinOpKind::Add, Ty::Int, Ty::Int) => Ty::Int,
                (BinOpKind::Mul, Ty::Int, Ty::Int) => Ty::Int,
                (BinOpKind::Eq, Ty::Int, Ty::Int) => Ty::Bool,
                // Unknown operands are tolerated (forward-compatibility)
                (_, Ty::Unknown, _) | (_, _, Ty::Unknown) => Ty::Unknown,
                _ => Ty::Unknown,
            }
        }
        Expr::Call { callee, arg: _ } => {
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
    }
}

// ---------------------------------------------------------------------------
// Statement-level checking
// ---------------------------------------------------------------------------

fn check_body_stmts(
    stmts: &[Stmt],
    env: &TypeEnv,
    decl_name: &str,
    declared_return_ty: Option<Ty>,
    param_tys: &[(&str, Ty)],
) -> Result<(), TypecheckError> {
    let mut local_env = TypeEnv {
        globals: env.globals.clone(),
        locals: param_tys
            .iter()
            .map(|(name, ty)| (name.to_string(), ty.clone()))
            .collect(),
    };

    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } => {
                ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                let ty = check_expr(value, &local_env);
                local_env.locals.insert(name.clone(), ty);
            }
            Stmt::Expr(expr) => {
                ensure_no_ambiguous_refs_in_expr(expr, &local_env, decl_name)?;
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

        if inferred != Ty::Unknown && inferred != declared {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
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

fn ensure_no_ambiguous_refs_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    match expr {
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) => Ok(()),
        Expr::ListLit(items) | Expr::TupleLit(items) => {
            for item in items {
                ensure_no_ambiguous_refs_in_expr(item, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Var(name) => ensure_name_not_ambiguous(name, env, decl_name),
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
    }
}

fn ensure_name_not_ambiguous(name: &str, env: &TypeEnv, decl_name: &str) -> Result<(), TypecheckError> {
    if env.locals.contains_key(name) {
        return Ok(());
    }
    if let Some(sources) = env.ambiguous_sources(name) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            message: format!(
                "name `{}` is ambiguous due to import collision: {}",
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
        Ty::Var(name) => name.clone(),
        Ty::Con { name, args } => {
            if args.is_empty() {
                name.clone()
            } else {
                let rendered_args: Vec<String> = args.iter().map(format_type_application_arg).collect();
                format!("{} {}", name, rendered_args.join(" "))
            }
        }
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

fn validate_type_annotation(decl_name: &str, annotation: &str) -> Result<(), TypecheckError> {
    if uses_legacy_void(annotation) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            message: "legacy `void` is not supported; use `Unit`".to_string(),
        });
    }

    validate_effect_clause(decl_name, annotation)?;

    let base = strip_effect_clause(annotation).trim();
    if base.is_empty() {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            message: "type annotation must not be empty".to_string(),
        });
    }

    if base.contains("->") {
        let Some(ft) = parse_function_type(base) else {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                message: "invalid function type annotation".to_string(),
            });
        };
        let mut segments = ft.arguments;
        segments.push(ft.result);
        if segments.iter().any(|segment| parse_type_expr(segment).is_none()) {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                message: "invalid function type annotation".to_string(),
            });
        }
    } else if parse_type_expr(base).is_none() {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            message: "invalid type annotation".to_string(),
        });
    }

    Ok(())
}

fn uses_legacy_void(annotation: &str) -> bool {
    annotation
        .split(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
        .any(|token| token == "void")
}

fn validate_effect_clause(decl_name: &str, annotation: &str) -> Result<(), TypecheckError> {
    let Some(effect_idx) = find_can_keyword_index(annotation) else {
        return Ok(());
    };

    let effects_raw = annotation[effect_idx + 3..].trim();
    if effects_raw.is_empty() {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            message: "effect list after `can` must not be empty".to_string(),
        });
    }

    for effect_name in effects_raw.split(',').map(str::trim) {
        if !is_identifier(effect_name) {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                message: format!("invalid effect name `{}` in type annotation", effect_name),
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

fn find_can_keyword_index(annotation: &str) -> Option<usize> {
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

fn is_identifier(s: &str) -> bool {
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
    use crate::parse_module;

    use super::*;

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

        let hello_module = parse_module(&hello).expect("hello should parse");
        let basic_module = parse_module(&basic).expect("basic_types should parse");
        let generic_types_module =
            parse_module(&generic_types).expect("generic_types should parse");
        let import_module = parse_module(&import_example).expect("import example should parse");
        let control_flow_module = parse_module(&control_flow).expect("control_flow should parse");

        typecheck_module(&hello_module).expect("hello should typecheck");
        typecheck_module(&basic_module).expect("basic_types should typecheck");
        typecheck_module(&generic_types_module).expect("generic_types should typecheck");
        typecheck_module(&import_module).expect("import example should typecheck");
        typecheck_module(&control_flow_module).expect("control_flow should typecheck");
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
        let module = parse_module("x : Int can\tLog\nx = 1\n").expect("should parse");
        typecheck_module(&module).expect("tab-separated `can` clause should be accepted");
    }

    #[test]
    fn rejects_invalid_effect_name() {
        let module = parse_module("x : Int can Log, 1Bad\nx = 1\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("invalid effect name should fail");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(err.message.contains("invalid effect name"));
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
        let err =
            typecheck_module(&module).expect_err("grouped type mismatch should be rejected");
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
    fn infers_string_literal_type() {
        let module = parse_module("s : String\ns = \"hello\"\n").expect("should parse");
        typecheck_module(&module).expect("string literal body should typecheck");
    }

    #[test]
    fn infers_equality_as_bool() {
        let module = parse_module("flag : Bool\nflag = 1 == 1\n").expect("should parse");
        typecheck_module(&module).expect("equality result should typecheck as Bool");
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
    fn allows_unused_name_when_import_collides_with_declaration() {
        let source = "\
import goby/env ( fetch_env_var )
fetch_env_var : String -> String
fetch_env_var name = name
main : Unit -> Unit can Print
main =
  print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("unused ambiguous name should be tolerated");
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
        let err = typecheck_module(&module).expect_err("type mismatch via param should be rejected");
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
        let module =
            parse_module("add : Int -> Int -> Int\nadd a = a\n").expect("should parse");
        let err =
            typecheck_module(&module).expect_err("param count mismatch should be rejected");
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
        let module = parse_module("main : Unit -> Unit\nmain = print \"hi\"\n").expect("should parse");
        typecheck_module(&module).expect("Unit param omission should be accepted");
    }

    #[test]
    fn accepts_rebinding_shadowing_in_same_body() {
        let source = "f : Unit -> Int\nf =\n  a = 1\n  a = a + 1\n  a\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("re-binding in same body should be accepted");
    }

    #[test]
    fn check_expr_infers_addition() {
        let env = TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
        };
        let expr = crate::ast::Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(crate::ast::Expr::IntLit(1)),
            right: Box::new(crate::ast::Expr::IntLit(2)),
        };
        assert_eq!(check_expr(&expr, &env), Ty::Int);
    }
}
