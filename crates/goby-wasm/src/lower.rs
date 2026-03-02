use std::collections::{HashMap, HashSet};

use goby_core::{BinOpKind, CasePattern, Expr, Module, Stmt};

use crate::{
    CodegenError,
    backend::WasmProgramBuilder,
    call::{extract_direct_print_call_arg, flatten_named_call, resolve_direct_call_target},
    layout::MemoryLayout,
    support::{is_supported_binop_kind, is_supported_case_pattern, is_supported_list_item_expr},
};

pub(crate) fn try_emit_native_module(module: &Module) -> Result<Option<Vec<u8>>, CodegenError> {
    if module.declarations.is_empty() {
        return Err(CodegenError {
            message: "module has no declarations".to_string(),
        });
    }

    let builder = WasmProgramBuilder::new(MemoryLayout::default());
    let layout = builder.layout();
    let _ = (
        layout.iovec_offset,
        layout.nwritten_offset,
        layout.heap_base,
    );

    let Some(main_decl) = module.declarations.iter().find(|decl| decl.name == "main") else {
        return Ok(None);
    };
    let Some(stmts) = main_decl.parsed_body.as_deref() else {
        return Ok(None);
    };

    let Some(output_text) = collect_phase2_output_text(module, stmts) else {
        return Ok(None);
    };
    if output_text.is_empty() {
        return Ok(None);
    }

    let wasm = builder.emit_static_print_module(&output_text)?;
    Ok(Some(wasm))
}

#[derive(Clone)]
enum NativeValue {
    String(String),
    Int(i64),
    Bool(bool),
    ListInt(Vec<i64>),
}

impl NativeValue {
    fn as_output_text(&self) -> String {
        match self {
            Self::String(s) => s.clone(),
            Self::Int(n) => n.to_string(),
            Self::Bool(true) => "True".to_string(),
            Self::Bool(false) => "False".to_string(),
            Self::ListInt(values) => {
                let inner = values
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", inner)
            }
        }
    }
}

fn collect_phase2_output_text(module: &Module, stmts: &[Stmt]) -> Option<String> {
    let env = EvalEnv::from_module(module);
    let mut bindings: HashMap<&str, NativeValue> = HashMap::new();
    let mut outputs: Vec<String> = Vec::new();
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } => {
                let val = eval_expr(value, &bindings, &env, 0)?;
                bindings.insert(name.as_str(), val);
            }
            Stmt::Expr(expr) => {
                let val = as_print_expr(expr, &bindings, &env)?;
                outputs.push(val.as_output_text());
            }
            Stmt::Using { .. } => return None,
        }
    }
    Some(outputs.join("\n"))
}

fn as_print_expr(
    expr: &Expr,
    bindings: &HashMap<&str, NativeValue>,
    env: &EvalEnv<'_>,
) -> Option<NativeValue> {
    match expr {
        Expr::Call { .. } => eval_expr(extract_direct_print_call_arg(expr).ok()?, bindings, env, 0),
        Expr::Pipeline { value, callee } if callee == "print" => eval_expr(value, bindings, env, 0),
        _ => None,
    }
}

fn eval_expr(
    expr: &Expr,
    bindings: &HashMap<&str, NativeValue>,
    env: &EvalEnv<'_>,
    depth: usize,
) -> Option<NativeValue> {
    const MAX_DEPTH: usize = 32;
    if depth >= MAX_DEPTH {
        return None;
    }

    match expr {
        Expr::StringLit(s) => Some(NativeValue::String(s.clone())),
        Expr::IntLit(n) => Some(NativeValue::Int(*n)),
        Expr::BoolLit(b) => Some(NativeValue::Bool(*b)),
        Expr::Var(name) => bindings.get(name.as_str()).cloned(),
        Expr::BinOp { op, left, right } => {
            let lhs = eval_expr(left, bindings, env, depth + 1)?;
            let rhs = eval_expr(right, bindings, env, depth + 1)?;
            match (op, lhs, rhs) {
                (BinOpKind::Add, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Int(a.checked_add(b)?))
                }
                (BinOpKind::Mul, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Int(a.checked_mul(b)?))
                }
                (BinOpKind::Eq, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Bool(a == b))
                }
                (BinOpKind::Eq, NativeValue::Bool(a), NativeValue::Bool(b)) => {
                    Some(NativeValue::Bool(a == b))
                }
                _ => None,
            }
        }
        Expr::Call { .. } => {
            let (fn_name, args) = flatten_named_call(expr)?;
            if fn_name == "print" {
                return None;
            }
            let arg_values = args
                .iter()
                .map(|arg| eval_expr(arg, bindings, env, depth + 1))
                .collect::<Option<Vec<_>>>()?;
            eval_named_function(fn_name, arg_values, env, depth + 1)
        }
        Expr::ListLit(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                let value = eval_expr(item, bindings, env, depth + 1)?;
                let NativeValue::Int(n) = value else {
                    return None;
                };
                out.push(n);
            }
            Some(NativeValue::ListInt(out))
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            let cond = eval_expr(condition, bindings, env, depth + 1)?;
            let NativeValue::Bool(flag) = cond else {
                return None;
            };
            if flag {
                eval_expr(then_expr, bindings, env, depth + 1)
            } else {
                eval_expr(else_expr, bindings, env, depth + 1)
            }
        }
        Expr::Case { scrutinee, arms } => {
            let scrutinee_val = eval_expr(scrutinee, bindings, env, depth + 1)?;
            for arm in arms {
                let matched = match (&arm.pattern, &scrutinee_val) {
                    (CasePattern::Wildcard, _) => true,
                    (CasePattern::IntLit(p), NativeValue::Int(v)) => p == v,
                    (CasePattern::StringLit(p), NativeValue::String(v)) => p == v,
                    (CasePattern::BoolLit(p), NativeValue::Bool(v)) => p == v,
                    _ => false,
                };
                if matched {
                    return eval_expr(&arm.body, bindings, env, depth + 1);
                }
            }
            None
        }
        _ => None,
    }
}

struct EvalEnv<'a> {
    declarations: HashMap<&'a str, &'a goby_core::Declaration>,
}

impl<'a> EvalEnv<'a> {
    fn from_module(module: &'a Module) -> Self {
        let mut declarations = HashMap::new();
        for decl in &module.declarations {
            declarations.insert(decl.name.as_str(), decl);
        }
        Self { declarations }
    }
}

fn eval_named_function(
    fn_name: &str,
    args: Vec<NativeValue>,
    env: &EvalEnv<'_>,
    depth: usize,
) -> Option<NativeValue> {
    let decl = env.declarations.get(fn_name)?;
    if decl.name == "main" {
        return None;
    }
    if decl.params.len() != args.len() {
        return None;
    }
    let stmts = decl.parsed_body.as_deref()?;

    let mut locals: HashMap<&str, NativeValue> = HashMap::new();
    for (param, arg_val) in decl.params.iter().zip(args.into_iter()) {
        locals.insert(param.as_str(), arg_val);
    }

    let mut last_expr: Option<NativeValue> = None;
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } => {
                let val = eval_expr(value, &locals, env, depth + 1)?;
                locals.insert(name.as_str(), val);
            }
            Stmt::Expr(expr) => {
                let value = eval_expr(expr, &locals, env, depth + 1)?;
                last_expr = Some(value);
            }
            Stmt::Using { .. } => return None,
        }
    }
    last_expr
}

pub(crate) fn declaration_body_supported_for_native(decl_name: &str, module: &Module) -> bool {
    let env = EvalEnv::from_module(module);
    let mut stack = HashSet::new();
    is_decl_supported(decl_name, module, &env, &mut stack)
}

fn is_decl_supported(
    decl_name: &str,
    module: &Module,
    env: &EvalEnv<'_>,
    stack: &mut HashSet<String>,
) -> bool {
    if !stack.insert(decl_name.to_string()) {
        return false;
    }
    let Some(decl) = env.declarations.get(decl_name) else {
        stack.remove(decl_name);
        return false;
    };
    let Some(stmts) = decl.parsed_body.as_deref() else {
        stack.remove(decl_name);
        return false;
    };
    let ok = stmts
        .iter()
        .all(|s| is_stmt_supported(s, module, env, stack));
    stack.remove(decl_name);
    ok
}

fn is_stmt_supported(
    stmt: &Stmt,
    module: &Module,
    env: &EvalEnv<'_>,
    stack: &mut HashSet<String>,
) -> bool {
    match stmt {
        Stmt::Binding { value, .. } => is_value_expr_supported(value, module, env, stack),
        Stmt::Expr(expr) => match expr {
            Expr::Call { callee, arg } if matches!(callee.as_ref(), Expr::Var(n) if n == "print") => {
                is_value_expr_supported(arg, module, env, stack)
            }
            _ => is_value_expr_supported(expr, module, env, stack),
        },
        Stmt::Using { .. } => false,
    }
}

fn is_value_expr_supported(
    expr: &Expr,
    module: &Module,
    env: &EvalEnv<'_>,
    stack: &mut HashSet<String>,
) -> bool {
    match expr {
        Expr::StringLit(_) | Expr::IntLit(_) | Expr::BoolLit(_) | Expr::Var(_) => true,
        Expr::ListLit(items) => items.iter().all(is_supported_list_item_expr),
        Expr::BinOp { op, left, right } => {
            is_supported_binop_kind(op)
                && is_value_expr_supported(left, module, env, stack)
                && is_value_expr_supported(right, module, env, stack)
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            is_value_expr_supported(condition, module, env, stack)
                && is_value_expr_supported(then_expr, module, env, stack)
                && is_value_expr_supported(else_expr, module, env, stack)
        }
        Expr::Case { scrutinee, arms } => {
            is_value_expr_supported(scrutinee, module, env, stack)
                && !arms.is_empty()
                && arms.iter().all(|arm| {
                    is_supported_case_pattern(&arm.pattern)
                        && is_value_expr_supported(&arm.body, module, env, stack)
                })
        }
        Expr::Call { .. } => {
            let Some((name, args)) = flatten_named_call(expr) else {
                return false;
            };
            if name == "print" {
                return false;
            }
            if !args
                .iter()
                .all(|arg| is_value_expr_supported(arg, module, env, stack))
            {
                return false;
            }
            if resolve_direct_call_target(module, name, args.len()).is_err() {
                return false;
            }
            is_decl_supported(name, module, env, stack)
        }
        _ => false,
    }
}
