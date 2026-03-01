use std::collections::{HashMap, HashSet};

use crate::{
    Module,
    ast::{BinOpKind, Expr, ImportKind, Span, Stmt, TypeDeclaration},
    types::{TypeExpr, parse_function_type, parse_type_expr},
};

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
            Some(s) => write!(f, "typecheck error in {} at line {}:{}: {}", target, s.line, s.col, self.message),
            None => write!(f, "typecheck error in {}: {}", target, self.message),
        }
    }
}

impl std::error::Error for TypecheckError {}

pub fn typecheck_module(module: &Module) -> Result<(), TypecheckError> {
    validate_imports(module)?;
    validate_type_declarations(module)?;
    validate_effect_declarations(module)?;

    let known_effects: HashSet<String> = builtin_effect_names()
        .iter()
        .copied()
        .map(str::to_string)
        .chain(module.effect_declarations.iter().map(|e| e.name.clone()))
        .collect();

    let mut names = HashSet::new();

    for decl in &module.declarations {
        if !names.insert(decl.name.clone()) {
            return Err(TypecheckError {
                declaration: Some(decl.name.clone()),
                span: Some(Span { line: decl.line, col: 1 }),
                message: "duplicate top-level declaration".to_string(),
            });
        }

        if let Some(annotation) = decl.type_annotation.as_deref() {
            validate_type_annotation(&decl.name, annotation, &known_effects)?;
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
                span: Some(Span { line: main.line, col: 1 }),
                message: "main type annotation must be a function type".to_string(),
            })?;

            if ty.arguments != vec!["Unit".to_string()] || ty.result != "Unit" {
                return Err(TypecheckError {
                    declaration: Some("main".to_string()),
                    span: Some(Span { line: main.line, col: 1 }),
                    message: "main type must be `Unit -> Unit` in MVP".to_string(),
                });
            }
        }
    }

    // Expression-level type checking (when parsed_body is available).
    let env = build_type_env(module);
    let effect_map = build_effect_map(module);
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
                            span: Some(Span { line: decl.line, col: 1 }),
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
            // available inside that function's body (they do not need an enclosing
            // `using` block at the call site within the same function).
            let decl_covered_ops =
                ops_from_can_clause(decl.type_annotation.as_deref(), &effect_map);

            check_body_stmts(
                stmts,
                &env,
                &effect_map,
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
    type_aliases: HashMap<String, Ty>,
    record_types: HashMap<String, RecordTypeInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum GlobalBinding {
    Resolved { ty: Ty, source: String },
    Ambiguous { sources: Vec<String> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RecordTypeInfo {
    type_name: String,
    fields: HashMap<String, Ty>,
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
            type_aliases: self.type_aliases.clone(),
            record_types: self.record_types.clone(),
        }
    }

    fn lookup_record_by_constructor(&self, constructor: &str) -> Option<&RecordTypeInfo> {
        self.record_types.get(constructor)
    }

    fn record_field_ty(&self, type_name: &str, field: &str) -> Option<Ty> {
        self.record_types
            .values()
            .find(|info| info.type_name == type_name)
            .and_then(|info| info.fields.get(field).cloned())
    }

    /// Returns true if `name` is a known effect operation (registered via `inject_effect_symbols`).
    /// A bare name that is `Ambiguous` due to two effects both declaring it is also considered an
    /// effect op; the ambiguity will have been caught by `ensure_no_ambiguous_refs_in_expr` first.
    fn is_effect_op(&self, name: &str) -> bool {
        if self.locals.contains_key(name) {
            return false;
        }
        match self.globals.get(name) {
            Some(GlobalBinding::Resolved { source, .. }) => source.starts_with("effect `"),
            Some(GlobalBinding::Ambiguous { sources }) => {
                sources.iter().all(|s| s.starts_with("effect `"))
            }
            None => false,
        }
    }

    fn are_compatible(&self, expected: &Ty, actual: &Ty) -> bool {
        let expected = self.resolve_alias(expected, 0);
        let actual = self.resolve_alias(actual, 0);
        expected == actual
    }

    fn resolve_alias(&self, ty: &Ty, depth: usize) -> Ty {
        if depth > 32 {
            return ty.clone();
        }
        match ty {
            Ty::List(inner) => Ty::List(Box::new(self.resolve_alias(inner, depth + 1))),
            Ty::Tuple(items) => Ty::Tuple(
                items
                    .iter()
                    .map(|item| self.resolve_alias(item, depth + 1))
                    .collect(),
            ),
            Ty::Fun { params, result } => Ty::Fun {
                params: params
                    .iter()
                    .map(|param| self.resolve_alias(param, depth + 1))
                    .collect(),
                result: Box::new(self.resolve_alias(result, depth + 1)),
            },
            Ty::Con { name, args } => {
                if args.is_empty()
                    && let Some(target) = self.type_aliases.get(name)
                {
                    return self.resolve_alias(target, depth + 1);
                }
                Ty::Con {
                    name: name.clone(),
                    args: args
                        .iter()
                        .map(|arg| self.resolve_alias(arg, depth + 1))
                        .collect(),
                }
            }
            _ => ty.clone(),
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

/// Maps handler names to the set of effect operation names they cover.
struct EffectMap {
    /// handler_name -> effect_name
    handler_to_effect: HashMap<String, String>,
    /// effect_name -> set of op identifiers: both qualified ("E.op") and bare ("op")
    effect_to_ops: HashMap<String, HashSet<String>>,
}

impl EffectMap {
    /// Returns the set of all op names (qualified + bare) covered by the given handler names.
    fn covered_ops(&self, handlers: &[String]) -> HashSet<String> {
        let mut ops = HashSet::new();
        for handler in handlers {
            if let Some(effect_name) = self.handler_to_effect.get(handler)
                && let Some(op_set) = self.effect_to_ops.get(effect_name)
            {
                ops.extend(op_set.iter().cloned());
            }
        }
        ops
    }
}

/// Maps top-level declaration names to the list of effect names they require (from `can` clause).
/// Used to check that callers provide appropriate `using` handlers.
fn build_required_effects_map(module: &Module) -> HashMap<String, Vec<String>> {
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

fn build_effect_map(module: &Module) -> EffectMap {
    let mut handler_to_effect = HashMap::new();
    for handler_decl in &module.handler_declarations {
        handler_to_effect.insert(handler_decl.name.clone(), handler_decl.effect.clone());
    }

    let mut effect_to_ops: HashMap<String, HashSet<String>> = HashMap::new();
    for effect_decl in &module.effect_declarations {
        let mut ops = HashSet::new();
        for member in &effect_decl.members {
            ops.insert(format!("{}.{}", effect_decl.name, member.name));
            ops.insert(member.name.clone());
        }
        effect_to_ops.insert(effect_decl.name.clone(), ops);
    }

    EffectMap { handler_to_effect, effect_to_ops }
}

/// Returns the set of op names (qualified + bare) for all effects listed in
/// a function's `can` clause.  Used to seed `covered_ops` for that function's body.
fn ops_from_can_clause(annotation: Option<&str>, effect_map: &EffectMap) -> HashSet<String> {
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

fn build_type_env(module: &Module) -> TypeEnv {
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
    inject_imported_symbols(module, &mut globals);
    inject_type_constructors(module, &mut globals, &mut type_aliases, &mut record_types);
    inject_effect_symbols(module, &mut globals);

    TypeEnv {
        globals,
        locals: HashMap::new(),
        type_aliases,
        record_types,
    }
}

fn inject_effect_symbols(module: &Module, globals: &mut HashMap<String, GlobalBinding>) {
    for effect_decl in &module.effect_declarations {
        for member in &effect_decl.members {
            // Register as qualified key `EffectName.member`.
            let qualified_key = format!("{}.{}", effect_decl.name, member.name);
            let ty = if let Some(ft) = parse_function_type(&member.type_annotation) {
                let params: Vec<Ty> = ft.arguments.iter().map(|a| ty_from_annotation(a)).collect();
                let result = ty_from_annotation(&ft.result);
                Ty::Fun {
                    params,
                    result: Box::new(result),
                }
            } else {
                Ty::Unknown
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
    for handler_decl in &module.handler_declarations {
        // Register handler name as Unknown so that `using HandlerName` doesn't produce
        // an "ambiguous name" error.
        insert_global_symbol(
            globals,
            handler_decl.name.clone(),
            Ty::Unknown,
            format!("handler `{}`", handler_decl.name),
        );
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
                for (field_name, field_ty) in fields.iter().map(|f| (&f.name, ty_from_annotation(&f.ty))) {
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

// TODO(post-MVP): EffectDecl and TypeDeclaration do not carry a source line field yet,
// so errors from these validators always have span: None. Add a line field to those
// AST nodes (similar to Declaration.line) to enable span reporting here.
fn validate_effect_declarations(module: &Module) -> Result<(), TypecheckError> {
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

fn validate_imports(module: &Module) -> Result<(), TypecheckError> {
    for import in &module.imports {
        let exports = builtin_module_exports(&import.module_path).ok_or_else(|| TypecheckError {
            declaration: None,
            span: None,
            message: format!("unknown module `{}`", import.module_path),
        })?;

        if let ImportKind::Selective(names) = &import.kind {
            for name in names {
                if !exports.contains_key(name.as_str()) {
                    return Err(TypecheckError {
                        declaration: None,
                        span: None,
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

fn validate_type_declarations(module: &Module) -> Result<(), TypecheckError> {
    let mut known_type_names: HashSet<String> = builtin_type_names()
        .into_iter()
        .map(|name| name.to_string())
        .collect();
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

fn builtin_type_names() -> [&'static str; 4] {
    ["Int", "Bool", "String", "Unit"]
}

/// Effects that are provided by the runtime and do not require an `effect` declaration.
fn builtin_effect_names() -> &'static [&'static str] {
    &["Print"]
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
        Expr::Qualified { receiver, member } => {
            if let Some(receiver_ty) = env.locals.get(receiver)
                && let Ty::Con { name, .. } = env.resolve_alias(receiver_ty, 0)
                && let Some(field_ty) = env.record_field_ty(&name, member)
            {
                return env.resolve_alias(&field_ty, 0);
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
        Expr::Case { scrutinee, arms } => {
            let _ = check_expr(scrutinee, env);
            arms.first()
                .map(|arm| check_expr(&arm.body, env))
                .unwrap_or(Ty::Unknown)
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            let _ = check_expr(condition, env);
            let then_ty = check_expr(then_expr, env);
            let _ = check_expr(else_expr, env);
            then_ty
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
    required_effects_map: &HashMap<String, Vec<String>>,
    decl_name: &str,
    declared_return_ty: Option<Ty>,
    param_tys: &[(&str, Ty)],
    // Op names (qualified and bare) that are covered by enclosing `using` handlers.
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

    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } => {
                ensure_no_ambiguous_refs_in_expr(value, &local_env, decl_name)?;
                check_unhandled_effects_in_expr(
                    value,
                    &local_env,
                    required_effects_map,
                    effect_map,
                    covered_ops,
                    decl_name,
                )?;
                let ty = check_expr(value, &local_env);
                local_env.locals.insert(name.clone(), ty);
            }
            Stmt::Expr(expr) => {
                ensure_no_ambiguous_refs_in_expr(expr, &local_env, decl_name)?;
                check_unhandled_effects_in_expr(
                    expr,
                    &local_env,
                    required_effects_map,
                    effect_map,
                    covered_ops,
                    decl_name,
                )?;
            }
            Stmt::Using { handlers, body } => {
                // Compute the ops covered by these handlers, merged with the enclosing set.
                let mut merged = covered_ops.clone();
                merged.extend(effect_map.covered_ops(handlers));
                check_body_stmts(
                    body,
                    &local_env,
                    effect_map,
                    required_effects_map,
                    decl_name,
                    None,
                    &[],
                    &merged,
                )?;
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
/// by the enclosing `using` handlers (expressed as `covered_ops`).
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
                    "function `{}` requires effect `{}` which is not handled by any enclosing `using` block",
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
    required_effects_map: &HashMap<String, Vec<String>>,
    effect_map: &EffectMap,
    covered_ops: &HashSet<String>,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    // Shorthand for recursive calls.
    macro_rules! recurse {
        ($e:expr) => {
            check_unhandled_effects_in_expr($e, env, required_effects_map, effect_map, covered_ops, decl_name)
        };
        ($e:expr, $child_env:expr) => {
            check_unhandled_effects_in_expr($e, $child_env, required_effects_map, effect_map, covered_ops, decl_name)
        };
    }

    match expr {
        Expr::IntLit(_) | Expr::BoolLit(_) | Expr::StringLit(_) => Ok(()),
        Expr::ListLit(items) | Expr::TupleLit(items) => {
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
                        "effect operation `{}` is not handled by any enclosing `using` block",
                        name
                    ),
                });
            }
            // Also check if this is a user function that requires effects (bare reference).
            check_callee_required_effects(name, required_effects_map, effect_map, covered_ops, decl_name)
        }
        Expr::Qualified { receiver, member } => {
            let qualified = format!("{}.{}", receiver, member);
            if env.is_effect_op(&qualified) && !covered_ops.contains(qualified.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `using` block",
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
            recurse!(callee)?;
            recurse!(arg)
        }
        Expr::MethodCall { receiver, method, args } => {
            let qualified = format!("{}.{}", receiver, method);
            if env.is_effect_op(&qualified) && !covered_ops.contains(qualified.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `using` block",
                        qualified
                    ),
                });
            }
            for arg in args {
                recurse!(arg)?;
            }
            Ok(())
        }
        Expr::Pipeline { value, callee } => {
            recurse!(value)?;
            if env.is_effect_op(callee) && !covered_ops.contains(callee.as_str()) {
                return Err(TypecheckError {
                    declaration: Some(decl_name.to_string()),
                    span: None,
                    message: format!(
                        "effect operation `{}` is not handled by any enclosing `using` block",
                        callee
                    ),
                });
            }
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
        Expr::Case { scrutinee, arms } => {
            recurse!(scrutinee)?;
            for arm in arms {
                recurse!(&arm.body)?;
            }
            Ok(())
        }
        Expr::If { condition, then_expr, else_expr } => {
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
        Expr::ListLit(items) | Expr::TupleLit(items) => {
            for item in items {
                ensure_no_ambiguous_refs_in_expr(item, env, decl_name)?;
            }
            Ok(())
        }
        Expr::Var(name) => ensure_name_not_ambiguous(name, env, decl_name),
        Expr::Qualified { receiver, member } => {
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
        Expr::Case { scrutinee, arms } => {
            ensure_no_ambiguous_refs_in_expr(scrutinee, env, decl_name)?;
            for arm in arms {
                ensure_no_ambiguous_refs_in_expr(&arm.body, env, decl_name)?;
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

fn ensure_name_not_ambiguous(name: &str, env: &TypeEnv, decl_name: &str) -> Result<(), TypecheckError> {
    if env.locals.contains_key(name) {
        return Ok(());
    }
    if let Some(sources) = env.ambiguous_sources(name) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
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

fn validate_type_annotation(
    decl_name: &str,
    annotation: &str,
    known_effects: &HashSet<String>,
) -> Result<(), TypecheckError> {
    if uses_legacy_void(annotation) {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
            message: "legacy `void` is not supported; use `Unit`".to_string(),
        });
    }

    validate_effect_clause(decl_name, annotation, known_effects)?;

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
        if segments.iter().any(|segment| parse_type_expr(segment).is_none()) {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None,
                message: "invalid function type annotation".to_string(),
            });
        }
    } else if parse_type_expr(base).is_none() {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            span: None,
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

fn validate_effect_clause(
    decl_name: &str,
    annotation: &str,
    known_effects: &HashSet<String>,
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
        if !known_effects.contains(effect_name) {
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

        let hello_module = parse_module(&hello).expect("hello should parse");
        let basic_module = parse_module(&basic).expect("basic_types should parse");
        let generic_types_module =
            parse_module(&generic_types).expect("generic_types should parse");
        let import_module = parse_module(&import_example).expect("import example should parse");
        let control_flow_module = parse_module(&control_flow).expect("control_flow should parse");
        let type_module = parse_module(&type_example).expect("type should parse");
        let effect_module = parse_module(&effect_example).expect("effect.gb should parse");

        typecheck_module(&hello_module).expect("hello should typecheck");
        typecheck_module(&basic_module).expect("basic_types should typecheck");
        typecheck_module(&generic_types_module).expect("generic_types should typecheck");
        typecheck_module(&import_module).expect("import example should typecheck");
        typecheck_module(&control_flow_module).expect("control_flow should typecheck");
        typecheck_module(&type_module).expect("type example should typecheck");
        typecheck_module(&effect_module).expect("effect.gb should typecheck");
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
        let err =
            typecheck_module(&module).expect_err("duplicate effect declarations should fail");
        assert_eq!(err.declaration.as_deref(), Some("Log"));
        assert!(err.message.contains("duplicate effect declaration"));
    }

    // -----------------------------------------------------------------------
    // using / unhandled-effect tests
    // -----------------------------------------------------------------------

    #[test]
    fn rejects_direct_effect_op_call_outside_using() {
        // `log x` is called directly in `main` without any `using LogHandler`.
        let source = "\
effect Log
  log: String -> Unit
handler LogHandler for Log
  log str = print str
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
    fn accepts_effect_op_call_inside_using() {
        // `log x` is called inside a `using LogHandler` block.
        let source = "\
effect Log
  log: String -> Unit
handler LogHandler for Log
  log str = print str
main : Unit -> Unit
main =
  using LogHandler
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("effect op call inside using should be accepted");
    }

    #[test]
    fn accepts_qualified_effect_op_inside_using() {
        // `Log.log x` (qualified form) inside a `using LogHandler` block.
        let source = "\
effect Log
  log: String -> Unit
handler LogHandler for Log
  log str = print str
main : Unit -> Unit
main =
  using LogHandler
    Log.log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("qualified effect op call inside using should be accepted");
    }

    #[test]
    fn rejects_effect_op_when_wrong_handler_used() {
        // `using LogHandler` only covers `Log` ops; calling `Env.from_env` is unhandled.
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> String
handler LogHandler for Log
  log str = print str
handler EnvHandler for Env
  from_env str = str
main : Unit -> Unit
main =
  using LogHandler
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
        // A function with `can Log` may call `log` in its own body without `using`.
        let source = "\
effect Log
  log: String -> Unit
handler LogHandler for Log
  log str = print str
f : String -> Unit can Log
f msg =
  log msg
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("can-declared effect op should be allowed in function body");
    }

    #[test]
    fn rejects_effect_op_in_binding_value_outside_using() {
        // Effect op used in binding RHS, no enclosing `using`.
        let source = "\
effect Log
  log: String -> Unit
handler LogHandler for Log
  log str = print str
main : Unit -> Unit
main =
  x = log \"hi\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("effect op in binding outside using should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(err.message.contains("not handled"));
    }

    #[test]
    fn rejects_effect_op_as_pipeline_callee_outside_using() {
        // `"hello" |> log` — effect op used as pipeline callee without `using`.
        let source = "\
effect Log
  log: String -> Unit
handler LogHandler for Log
  log str = print str
main : Unit -> Unit
main =
  \"hello\" |> log
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("effect op as pipeline callee outside using should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(err.message.contains("not handled"));
        assert!(err.message.contains("log"));
    }

    #[test]
    fn accepts_effect_op_as_pipeline_callee_inside_using() {
        // `"hello" |> log` inside `using LogHandler` should be accepted.
        let source = "\
effect Log
  log: String -> Unit
handler LogHandler for Log
  log str = print str
main : Unit -> Unit
main =
  using LogHandler
    \"hello\" |> log
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("effect op as pipeline callee inside using should be accepted");
    }

    #[test]
    fn accepts_lambda_param_shadowing_effect_op_name() {
        // `|log| -> log "hi"` — `log` inside the lambda refers to the parameter,
        // not the effect op; should not be flagged as unhandled.
        let source = "\
effect Log
  log: String -> Unit
handler LogHandler for Log
  log str = print str
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
    fn accepts_nested_using_with_merged_covered_ops() {
        // Outer `using LogHandler` + inner `using EnvHandler`; inner body calls both `log` and `from_env`.
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> String
handler LogHandler for Log
  log str = print str
handler EnvHandler for Env
  from_env str = str
main : Unit -> Unit
main =
  using LogHandler
    using EnvHandler
      log \"hi\"
      from_env \"PATH\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("nested using with merged covered ops should be accepted");
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

    // ── Step 3: calling effectful functions requires an appropriate `using` handler ──

    #[test]
    fn rejects_call_to_effectful_function_outside_using() {
        // `plus_ten_with_log` requires the `Log` effect; calling it from `main` without
        // `using LogHandler` should be rejected.
        let source = "\
effect Log
  log: String -> Unit
handler LogHandler for Log
  log msg = print msg
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
            .expect_err("calling effectful function without using should fail");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(
            err.message.contains("unhandled effect") || err.message.contains("Log"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_call_to_effectful_function_inside_using() {
        // Same call, but wrapped in `using LogHandler` — should succeed.
        let source = "\
effect Log
  log: String -> Unit
handler LogHandler for Log
  log msg = print msg
plus_ten_with_log : Int -> Int can Log
plus_ten_with_log n =
  log \"calling\"
  n
main : Unit -> Unit
main =
  using LogHandler
    plus_ten_with_log 3
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("calling effectful function inside appropriate using should succeed");
    }

    #[test]
    fn rejects_call_when_partial_handlers_present() {
        // `show_env_var` requires both `Log` and `Env`; only `LogHandler` is in scope.
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> String
handler LogHandler for Log
  log msg = print msg
handler EnvHandler for Env
  from_env key = key
show_env_var : String -> Unit can Log, Env
show_env_var name =
  v = from_env name
  log v
main : Unit -> Unit
main =
  using LogHandler
    show_env_var \"PATH\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("missing Env handler should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(
            err.message.contains("unhandled effect") || err.message.contains("Env"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_effectful_pipeline_callee_inside_using() {
        // `3 |> plus_ten_with_log` inside `using LogHandler` — pipeline form should also pass.
        let source = "\
effect Log
  log: String -> Unit
handler LogHandler for Log
  log msg = print msg
plus_ten_with_log : Int -> Int can Log
plus_ten_with_log n =
  log \"calling\"
  n
main : Unit -> Unit
main =
  using LogHandler
    3 |> plus_ten_with_log
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("effectful pipeline callee inside using should succeed");
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
    fn rejects_invalid_effect_name() {
        // `Log` is declared so the identifier check can reach `1Bad`.
        let source = "effect Log\n  log: String -> Unit\nx : Int can Log, 1Bad\nx = 1\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("invalid effect name should fail");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(err.message.contains("invalid effect name"));
    }

    #[test]
    fn accepts_can_clause_with_builtin_print_effect() {
        // `can Print` uses the built-in Print effect — no `effect` declaration needed.
        let source = "main : Unit -> Unit can Print\nmain = print \"hi\"\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("builtin Print effect should be accepted in `can` clause");
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
        typecheck_module(&module).expect("multiple declared effects in `can` clause should be accepted");
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
        assert_eq!(span.line, 1, "span.line should point to main declaration line");
        assert_eq!(span.col, 1);
    }

    #[test]
    fn typecheck_error_main_wrong_function_type_has_span() {
        // main declared with a valid function type but wrong signature (not Unit -> Unit).
        // Triggers the second error branch: "main type must be `Unit -> Unit` in MVP".
        let source = "main : Int -> String\nmain x = string.concat(\"a\", \"b\")\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("wrong main function type should fail");
        let span = err.span.expect("main type error must have a span");
        assert_eq!(span.line, 1, "span.line should point to main declaration line");
        assert_eq!(span.col, 1);
        assert!(err.message.contains("Unit -> Unit"));
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
