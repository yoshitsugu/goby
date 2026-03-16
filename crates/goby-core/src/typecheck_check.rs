use std::collections::HashSet;

use crate::ast::{BinOpKind, CasePattern, Expr, ListPatternItem, ListPatternTail, Stmt};
use crate::typecheck::TypecheckError;
use crate::typecheck_env::{GlobalBinding, Ty, TypeEnv};
use crate::typecheck_render::ty_name;
use crate::typecheck_unify::ty_contains_type_var;

pub(crate) fn check_expr(expr: &Expr, env: &TypeEnv) -> Ty {
    infer_expr_ty(expr, env)
}

fn infer_expr_ty(expr: &Expr, env: &TypeEnv) -> Ty {
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
                (_, Ty::Unknown, _) | (_, _, Ty::Unknown) => Ty::Unknown,
                _ => Ty::Unknown,
            }
        }
        Expr::Call { callee, arg } => {
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
        Expr::ListIndex { list, index } => {
            let list_ty = env.resolve_alias(&check_expr(list, env), 0);
            let _index_ty = check_expr(index, env);
            match list_ty {
                Ty::List(elem_ty) => *elem_ty,
                Ty::Unknown => Ty::Unknown,
                _ => Ty::Unknown,
            }
        }
    }
}

pub(crate) fn check_list_spread_constraints(
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
                span: None, // no span available: requires Expr/Stmt span (D1a-iii)
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
                    span: None, // no span available: requires Expr/Stmt span (D1a-iii)
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
                span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                message: format!(
                    "list spread tail must be `List {}` but got `{}`",
                    ty_name(&expected),
                    ty_name(&other)
                ),
            })
        }
    }
}

pub(crate) fn check_list_index_constraints(
    list: &Expr,
    index: &Expr,
    env: &TypeEnv,
    decl_name: &str,
) -> Result<(), TypecheckError> {
    let list_ty = env.resolve_alias(&check_expr(list, env), 0);
    match &list_ty {
        Ty::List(_) | Ty::Unknown | Ty::Var(_) => {}
        other => {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                message: format!(
                    "list index requires a `List` receiver, but got `{}`",
                    ty_name(other)
                ),
            });
        }
    }

    let index_ty = env.resolve_alias(&check_expr(index, env), 0);
    match &index_ty {
        Ty::Int | Ty::Unknown | Ty::Var(_) => {}
        other => {
            return Err(TypecheckError {
                declaration: Some(decl_name.to_string()),
                span: None, // no span available: requires Expr/Stmt span (D1a-iii)
                message: format!(
                    "list index must be `Int`, but got `{}`",
                    ty_name(other)
                ),
            });
        }
    }

    Ok(())
}

pub(crate) fn merge_branch_type(env: &TypeEnv, left: Ty, right: Ty) -> Ty {
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

pub(crate) fn branch_types_compatible(env: &TypeEnv, left: &Ty, right: &Ty) -> bool {
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

pub(crate) fn is_list_case_pattern(pattern: &CasePattern) -> bool {
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

pub(crate) fn env_with_case_pattern_bindings(
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
        // For qualified names like "Log.log", extract the bare op name.
        let bare_name = if let Some((_e, op)) = clause.name.split_once('.') {
            op
        } else {
            &clause.name
        };
        covered.insert(bare_name.to_string());
        covered.insert(format!("{}.{}", effect, bare_name));
    }
    covered
}

pub(crate) fn effect_candidates_for_operation(env: &TypeEnv, op_name: &str) -> Vec<String> {
    // If this is a qualified name like "Effect.op", resolve directly without env lookup.
    if let Some((effect, _op)) = op_name.split_once('.') {
        return vec![effect.to_string()];
    }

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

pub(crate) fn parse_tuple_member_index(member: &str) -> Option<usize> {
    if member.is_empty() || !member.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    member.parse::<usize>().ok()
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::ast::Expr;
    use crate::parse_module;
    use crate::typecheck::typecheck_module;
    use crate::typecheck_env::{RecordTypeInfo, Ty, TypeEnv};

    use super::check_expr;

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
        let expr = Expr::Qualified {
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
        let expr = Expr::Qualified {
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
        let expr = Expr::Qualified {
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
    fn list_index_infers_element_type() {
        let module = parse_module("xs : List Int\nxs = [1, 2, 3]\nv : Int\nv = xs[0]\n")
            .expect("should parse");
        typecheck_module(&module).expect("list index result should typecheck as Int");
    }

    #[test]
    fn list_index_rejects_non_int_index() {
        let module =
            parse_module("xs : List Int\nxs = [1, 2]\nv : Int\nv = xs[True]\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("bool index should fail");
        assert!(
            err.message.contains("list index must be `Int`"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn list_index_rejects_non_list_receiver() {
        let module =
            parse_module("n : Int\nn = 42\nv : Int\nv = n[0]\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("int receiver index should fail");
        assert!(
            err.message.contains("list index requires a `List` receiver"),
            "unexpected message: {}",
            err.message
        );
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
    fn check_expr_infers_addition() {
        let env = TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
        };
        let expr = Expr::BinOp {
            op: crate::BinOpKind::Add,
            left: Box::new(Expr::IntLit(1)),
            right: Box::new(Expr::IntLit(2)),
        };
        assert_eq!(check_expr(&expr, &env), Ty::Int);
    }
}
