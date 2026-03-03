use std::collections::HashMap;

use goby_core::{BinOpKind, CasePattern, Expr, Module, Stmt};

use crate::{
    CodegenError,
    backend::WasmProgramBuilder,
    call::extract_direct_print_call_arg,
    layout::MemoryLayout,
    planning::{DeclarationLoweringMode, LoweringPlan, LoweringStyle, build_lowering_plan},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EffectBoundaryHandoff {
    pub(crate) main_style: LoweringStyle,
    pub(crate) handler_resume_present: bool,
    pub(crate) evidence_operation_table_len: usize,
    pub(crate) evidence_requirements_len: usize,
    pub(crate) evidence_checksum: usize,
    pub(crate) declaration_modes: Vec<DeclarationLoweringMode>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum NativeLoweringResult {
    Emitted(Vec<u8>),
    EffectBoundaryHandoff(EffectBoundaryHandoff),
    NotLowered,
}

pub(crate) fn try_emit_native_module_with_handoff(
    module: &Module,
) -> Result<NativeLoweringResult, CodegenError> {
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
        return Ok(NativeLoweringResult::NotLowered);
    };
    let lowering_plan = build_lowering_plan(module);
    let evidence_shape = lowering_plan.evidence_shape();
    let _ = (
        lowering_plan.handler_resume_present(),
        evidence_shape.operation_table_len(),
        evidence_shape.requirements_len(),
        evidence_shape.checksum(),
        lowering_plan.evidence_requirement_for("main").map(|req| {
            (
                req.style(),
                req.passes_evidence(),
                req.required_effect_count(),
                req.referenced_operation_count(),
            )
        }),
    );
    let main_style = lowering_plan
        .style_for("main")
        .unwrap_or(LoweringStyle::EffectBoundary);
    if main_style != LoweringStyle::DirectStyle {
        return Ok(NativeLoweringResult::EffectBoundaryHandoff(
            EffectBoundaryHandoff {
                main_style,
                handler_resume_present: lowering_plan.handler_resume_present(),
                evidence_operation_table_len: evidence_shape.operation_table_len(),
                evidence_requirements_len: evidence_shape.requirements_len(),
                evidence_checksum: evidence_shape.checksum(),
                declaration_modes: lowering_plan.declaration_lowering_modes(),
            },
        ));
    }
    let Some(stmts) = main_decl.parsed_body.as_deref() else {
        return Ok(NativeLoweringResult::NotLowered);
    };

    let Some(output_text) = collect_phase2_output_text(module, stmts, &lowering_plan) else {
        return Ok(NativeLoweringResult::NotLowered);
    };
    if output_text.is_empty() {
        return Ok(NativeLoweringResult::NotLowered);
    }

    let wasm = builder.emit_static_print_module(&output_text)?;
    Ok(NativeLoweringResult::Emitted(wasm))
}

#[derive(Clone)]
enum NativeValue {
    String(String),
    Int(i64),
    Bool(bool),
    ListInt(Vec<i64>),
    Callable(NativeCallable),
}

#[derive(Clone)]
enum NativeCallable {
    Named {
        name: String,
        applied_args: Vec<NativeValue>,
    },
    Lambda(NativeLambda),
}

#[derive(Clone)]
struct NativeLambda {
    param: String,
    body: Expr,
    captured: HashMap<String, NativeValue>,
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
            Self::Callable(_) => "<function>".to_string(),
        }
    }
}

fn collect_phase2_output_text(
    module: &Module,
    stmts: &[Stmt],
    lowering_plan: &LoweringPlan,
) -> Option<String> {
    let env = EvalEnv::from_module(module, lowering_plan);
    let mut bindings: HashMap<String, NativeValue> = HashMap::new();
    let mut outputs: Vec<String> = Vec::new();
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } => {
                let val = eval_expr(value, &bindings, &env, 0)?;
                bindings.insert(name.to_string(), val);
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
    bindings: &HashMap<String, NativeValue>,
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
    bindings: &HashMap<String, NativeValue>,
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
        Expr::Var(name) => bindings.get(name).cloned().or_else(|| {
            if (env.declarations.contains_key(name.as_str())
                && env.lowering_plan.is_direct_style(name.as_str()))
                || name == "map"
            {
                Some(NativeValue::Callable(NativeCallable::Named {
                    name: name.clone(),
                    applied_args: Vec::new(),
                }))
            } else {
                None
            }
        }),
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
        Expr::Call { callee, arg } => {
            let callee_value = eval_expr(callee, bindings, env, depth + 1)?;
            let arg_value = eval_expr(arg, bindings, env, depth + 1)?;
            apply_callable(callee_value, arg_value, env, depth + 1)
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
        Expr::Lambda { param, body } => Some(NativeValue::Callable(NativeCallable::Lambda(
            NativeLambda {
                param: param.clone(),
                body: body.as_ref().clone(),
                captured: bindings.clone(),
            },
        ))),
        _ => None,
    }
}

struct EvalEnv<'a> {
    declarations: HashMap<&'a str, &'a goby_core::Declaration>,
    lowering_plan: &'a LoweringPlan,
}

impl<'a> EvalEnv<'a> {
    fn from_module(module: &'a Module, lowering_plan: &'a LoweringPlan) -> Self {
        let mut declarations = HashMap::new();
        for decl in &module.declarations {
            declarations.insert(decl.name.as_str(), decl);
        }
        Self {
            declarations,
            lowering_plan,
        }
    }
}

fn eval_named_function(
    fn_name: &str,
    args: Vec<NativeValue>,
    env: &EvalEnv<'_>,
    depth: usize,
) -> Option<NativeValue> {
    if !env.lowering_plan.is_direct_style(fn_name) {
        return None;
    }
    let decl = env.declarations.get(fn_name)?;
    if decl.params.len() != args.len() {
        return None;
    }
    let stmts = decl.parsed_body.as_deref()?;

    let mut locals: HashMap<String, NativeValue> = HashMap::new();
    for (param, arg_val) in decl.params.iter().zip(args.into_iter()) {
        locals.insert(param.clone(), arg_val);
    }

    let mut last_expr: Option<NativeValue> = None;
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } => {
                let val = eval_expr(value, &locals, env, depth + 1)?;
                locals.insert(name.clone(), val);
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

fn apply_callable(
    callee: NativeValue,
    arg: NativeValue,
    env: &EvalEnv<'_>,
    depth: usize,
) -> Option<NativeValue> {
    let NativeValue::Callable(callable) = callee else {
        return None;
    };
    match callable {
        NativeCallable::Named {
            name,
            mut applied_args,
        } => {
            if name == "print" {
                return None;
            }
            applied_args.push(arg);
            apply_named_callable(name, applied_args, env, depth + 1)
        }
        NativeCallable::Lambda(lambda) => {
            let mut locals = lambda.captured.clone();
            locals.insert(lambda.param, arg);
            eval_expr(&lambda.body, &locals, env, depth + 1)
        }
    }
}

fn apply_named_callable(
    name: String,
    args: Vec<NativeValue>,
    env: &EvalEnv<'_>,
    depth: usize,
) -> Option<NativeValue> {
    if name == "map" {
        return apply_map_builtin(args, env, depth + 1);
    }

    let decl = env.declarations.get(name.as_str())?;
    if !env.lowering_plan.is_direct_style(name.as_str()) {
        return None;
    }
    if decl.name == "main" {
        return None;
    }
    if args.len() < decl.params.len() {
        return Some(NativeValue::Callable(NativeCallable::Named {
            name,
            applied_args: args,
        }));
    }
    if args.len() > decl.params.len() {
        return None;
    }
    eval_named_function(name.as_str(), args, env, depth + 1)
}

fn apply_map_builtin(
    args: Vec<NativeValue>,
    env: &EvalEnv<'_>,
    depth: usize,
) -> Option<NativeValue> {
    if args.len() < 2 {
        return Some(NativeValue::Callable(NativeCallable::Named {
            name: "map".to_string(),
            applied_args: args,
        }));
    }
    if args.len() > 2 {
        return None;
    }
    let mut iter = args.into_iter();
    // `map ns f` is parsed as `Call(Call(map, ns), f)`, so args are `[list, mapper]`.
    let list = iter.next()?;
    let mapper = iter.next()?;
    let NativeValue::ListInt(values) = list else {
        return None;
    };

    let mut mapped = Vec::with_capacity(values.len());
    for value in values {
        let out = apply_callable(mapper.clone(), NativeValue::Int(value), env, depth + 1)?;
        let NativeValue::Int(n) = out else {
            return None;
        };
        mapped.push(n);
    }
    Some(NativeValue::ListInt(mapped))
}

#[cfg(test)]
mod tests {
    use goby_core::parse_module;

    use super::{NativeLoweringResult, try_emit_native_module_with_handoff};

    #[test]
    fn native_lowerer_rejects_main_when_call_graph_contains_effect_boundary() {
        let source = r#"
tick : Int -> Int can Tick
tick n = n

main : Unit -> Unit
main =
  print (tick 1)
"#;
        let module = parse_module(source).expect("source should parse");
        let lowered =
            try_emit_native_module_with_handoff(&module).expect("native lowering should not error");
        let NativeLoweringResult::EffectBoundaryHandoff(handoff) = lowered else {
            panic!("effect-boundary declarations should produce explicit handoff metadata");
        };
        assert!(
            handoff
                .declaration_modes
                .iter()
                .any(|entry| entry.declaration_name == "main"),
            "handoff should expose per-declaration lowering mode snapshot"
        );
    }

    #[test]
    fn native_lowerer_accepts_direct_style_declarations() {
        let source = r#"
double : Int -> Int
double n = n * 2

main : Unit -> Unit
main =
  print (double 21)
"#;
        let module = parse_module(source).expect("source should parse");
        let lowered =
            try_emit_native_module_with_handoff(&module).expect("native lowering should not error");
        assert!(matches!(lowered, NativeLoweringResult::Emitted(_)));
    }
}
