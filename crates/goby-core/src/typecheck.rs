use std::collections::{HashMap, HashSet};

use crate::{
    Module,
    ast::{BinOpKind, Expr, Stmt},
    str_util::split_top_level_commas,
    types::parse_function_type,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypecheckError {
    pub declaration: Option<String>,
    pub message: String,
}

pub fn typecheck_module(module: &Module) -> Result<(), TypecheckError> {
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
    Str,
    Unit,
    List(Box<Ty>),
    Tuple(Vec<Ty>),
    Fun { params: Vec<Ty>, result: Box<Ty> },
    Unknown,
}

struct TypeEnv {
    globals: HashMap<String, Ty>,
    locals: HashMap<String, Ty>,
}

impl TypeEnv {
    fn lookup(&self, name: &str) -> Ty {
        if let Some(ty) = self.locals.get(name) {
            return ty.clone();
        }
        if let Some(ty) = self.globals.get(name) {
            return ty.clone();
        }
        Ty::Unknown
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
                globals.insert(
                    decl.name.clone(),
                    Ty::Fun {
                        params,
                        result: Box::new(result),
                    },
                );
            } else {
                // Non-function annotation (e.g. tuple, plain type)
                globals.insert(decl.name.clone(), ty_from_annotation(base.trim()));
            }
        }
    }
    // Register built-in functions so that call-site type inference can use them.
    // `print` accepts any value and returns Unit.
    globals.insert(
        "print".to_string(),
        Ty::Fun {
            params: vec![Ty::Unknown],
            result: Box::new(Ty::Unit),
        },
    );

    TypeEnv {
        globals,
        locals: HashMap::new(),
    }
}

fn ty_from_annotation(s: &str) -> Ty {
    let s = s.trim();
    match s {
        "Int" => Ty::Int,
        "String" => Ty::Str,
        "Unit" => Ty::Unit,
        _ if s.starts_with("List ") => {
            let inner = ty_from_annotation(&s["List ".len()..]);
            Ty::List(Box::new(inner))
        }
        _ if s.starts_with('(') && s.ends_with(')') => {
            let inner = &s[1..s.len() - 1];
            let parts = split_top_level_commas(inner);
            if parts.len() >= 2 {
                Ty::Tuple(parts.iter().map(|p| ty_from_annotation(p)).collect())
            } else if parts.len() == 1 {
                // Grouped type like `(Int -> Int)` — unwrap the parens.
                ty_from_annotation(parts[0])
            } else {
                Ty::Unknown
            }
        }
        _ => Ty::Unknown,
    }
}


// ---------------------------------------------------------------------------
// Expression type inference
// ---------------------------------------------------------------------------

fn check_expr(expr: &Expr, env: &TypeEnv) -> Ty {
    match expr {
        Expr::IntLit(_) => Ty::Int,
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
            // string.concat -> String
            if receiver == "string" && method == "concat" {
                Ty::Str
            } else {
                Ty::Unknown
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
                let ty = check_expr(value, &local_env);
                local_env.locals.insert(name.clone(), ty);
            }
            Stmt::Expr(_expr) => {
                // Expressions as statements are allowed; no checks enforced yet.
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

fn ty_name(ty: &Ty) -> String {
    match ty {
        Ty::Int => "Int".to_string(),
        Ty::Str => "String".to_string(),
        Ty::Unit => "Unit".to_string(),
        Ty::List(inner) => format!("List {}", ty_name(inner)),
        Ty::Tuple(items) => {
            let inner: Vec<String> = items.iter().map(ty_name).collect();
            format!("({})", inner.join(", "))
        }
        Ty::Fun { .. } => "Fun".to_string(),
        Ty::Unknown => "Unknown".to_string(),
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

    if base.contains("->") && parse_function_type(base).is_none() {
        return Err(TypecheckError {
            declaration: Some(decl_name.to_string()),
            message: "invalid function type annotation".to_string(),
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

        let hello_module = parse_module(&hello).expect("hello should parse");
        let basic_module = parse_module(&basic).expect("basic_types should parse");

        typecheck_module(&hello_module).expect("hello should typecheck");
        typecheck_module(&basic_module).expect("basic_types should typecheck");
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
    fn infers_int_literal_type() {
        let module = parse_module("x : Int\nx = 42\n").expect("should parse");
        typecheck_module(&module).expect("int literal body should typecheck");
    }

    #[test]
    fn infers_string_literal_type() {
        let module = parse_module("s : String\ns = \"hello\"\n").expect("should parse");
        typecheck_module(&module).expect("string literal body should typecheck");
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
