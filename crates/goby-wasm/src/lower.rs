use std::collections::HashMap;

use goby_core::{BinOpKind, CasePattern, Expr, Module, Stmt, ast::InterpolatedPart};

use crate::{
    CodegenError,
    backend::WasmProgramBuilder,
    call::extract_direct_print_call_arg,
    layout::MemoryLayout,
    planning::{
        DeclarationLoweringMode, EffectId, EffectOperationRef, LoweringPlan, LoweringStyle,
        build_lowering_plan,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EffectExecutionMode {
    PortableFallback,
    TypedContinuationOptimized,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RuntimeProfile {
    Unknown,
    Wasmtime,
    Wasmer,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EffectModeFallbackReason {
    ForcedPortableOverride,
    RuntimeProfileNotSupported,
    MainNotEffectBoundary,
    UnsupportedEffectConstruct,
    OptimizationGateDisabled,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct EffectExecutionSelection {
    mode: EffectExecutionMode,
    fallback_reason: Option<EffectModeFallbackReason>,
    runtime_profile: RuntimeProfile,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EffectBoundaryHandoff {
    pub(crate) main_style: LoweringStyle,
    pub(crate) selected_mode: EffectExecutionMode,
    pub(crate) selected_mode_fallback_reason: Option<EffectModeFallbackReason>,
    pub(crate) runtime_profile: RuntimeProfile,
    pub(crate) typed_continuation_ir: Option<TypedContinuationIr>,
    pub(crate) main_requirement: Option<MainEvidenceRequirementSummary>,
    pub(crate) handler_resume_present: bool,
    pub(crate) evidence_operation_table_len: usize,
    pub(crate) evidence_requirements_len: usize,
    pub(crate) evidence_fingerprint_hint: usize,
    pub(crate) declaration_modes: Vec<DeclarationLoweringMode>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct MainEvidenceRequirementSummary {
    pub(crate) style: LoweringStyle,
    pub(crate) passes_evidence: bool,
    pub(crate) required_effect_count: usize,
    pub(crate) referenced_operation_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TypedContinuationIr {
    pub(crate) operation_table: Vec<EffectOperationRef>,
    pub(crate) main_required_effects: Vec<EffectId>,
    pub(crate) main_referenced_operations: Vec<EffectOperationRef>,
    pub(crate) one_shot_resume: bool,
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

    let Some(main_decl) = module.declarations.iter().find(|decl| decl.name == "main") else {
        return Ok(NativeLoweringResult::NotLowered);
    };
    let lowering_plan = build_lowering_plan(module);
    let evidence_shape = lowering_plan.evidence_shape();
    let main_style = lowering_plan
        .style_for("main")
        .unwrap_or(LoweringStyle::EffectBoundary);
    if main_style != LoweringStyle::DirectStyle {
        let selection = select_effect_execution_mode(main_style, module);
        let main_requirement = lowering_plan.evidence_requirement_for("main").map(|req| {
            MainEvidenceRequirementSummary {
                style: req.style(),
                passes_evidence: req.passes_evidence(),
                required_effect_count: req.required_effect_count(),
                referenced_operation_count: req.referenced_operation_count(),
            }
        });
        let typed_continuation_ir =
            if selection.mode == EffectExecutionMode::TypedContinuationOptimized {
                build_typed_continuation_ir(&lowering_plan)
            } else {
                None
            };
        return Ok(NativeLoweringResult::EffectBoundaryHandoff(
            EffectBoundaryHandoff {
                main_style,
                selected_mode: selection.mode,
                selected_mode_fallback_reason: selection.fallback_reason,
                runtime_profile: selection.runtime_profile,
                typed_continuation_ir,
                main_requirement,
                handler_resume_present: lowering_plan.handler_resume_present(),
                evidence_operation_table_len: evidence_shape.operation_table_len(),
                evidence_requirements_len: evidence_shape.requirements_len(),
                evidence_fingerprint_hint: evidence_shape.fingerprint_hint(),
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

fn build_typed_continuation_ir(lowering_plan: &LoweringPlan) -> Option<TypedContinuationIr> {
    let evidence_shape = lowering_plan.evidence_shape();
    let main_requirement = lowering_plan.evidence_requirement_for("main")?;
    Some(TypedContinuationIr {
        operation_table: evidence_shape.operation_table().to_vec(),
        main_required_effects: main_requirement.required_effects().to_vec(),
        main_referenced_operations: main_requirement.referenced_operations().to_vec(),
        one_shot_resume: true,
    })
}

fn select_effect_execution_mode(
    main_style: LoweringStyle,
    module: &Module,
) -> EffectExecutionSelection {
    select_effect_execution_mode_with_inputs(
        main_style,
        module,
        runtime_force_portable_fallback_override_enabled(),
        compile_time_runtime_profile(),
        typed_continuation_optimization_gate_enabled(),
    )
}

fn select_effect_execution_mode_with_inputs(
    main_style: LoweringStyle,
    module: &Module,
    force_portable_fallback_override: bool,
    runtime_profile: RuntimeProfile,
    optimization_gate_enabled: bool,
) -> EffectExecutionSelection {
    if force_portable_fallback_override {
        return EffectExecutionSelection {
            mode: EffectExecutionMode::PortableFallback,
            fallback_reason: Some(EffectModeFallbackReason::ForcedPortableOverride),
            runtime_profile,
        };
    }
    if !matches!(
        runtime_profile,
        RuntimeProfile::Wasmtime | RuntimeProfile::Wasmer
    ) {
        return EffectExecutionSelection {
            mode: EffectExecutionMode::PortableFallback,
            fallback_reason: Some(EffectModeFallbackReason::RuntimeProfileNotSupported),
            runtime_profile,
        };
    }
    if main_style != LoweringStyle::EffectBoundary {
        return EffectExecutionSelection {
            mode: EffectExecutionMode::PortableFallback,
            fallback_reason: Some(EffectModeFallbackReason::MainNotEffectBoundary),
            runtime_profile,
        };
    }
    if !optimization_gate_enabled {
        return EffectExecutionSelection {
            mode: EffectExecutionMode::PortableFallback,
            fallback_reason: Some(EffectModeFallbackReason::OptimizationGateDisabled),
            runtime_profile,
        };
    }
    if contains_unsupported_effect_construct_for_optimized_path(module) {
        return EffectExecutionSelection {
            mode: EffectExecutionMode::PortableFallback,
            fallback_reason: Some(EffectModeFallbackReason::UnsupportedEffectConstruct),
            runtime_profile,
        };
    }
    EffectExecutionSelection {
        mode: EffectExecutionMode::TypedContinuationOptimized,
        fallback_reason: None,
        runtime_profile,
    }
}

fn contains_unsupported_effect_construct_for_optimized_path(module: &Module) -> bool {
    module
        .declarations
        .iter()
        .filter_map(|decl| decl.parsed_body.as_deref())
        .any(stmts_contain_resume)
}

fn compile_time_runtime_profile() -> RuntimeProfile {
    match option_env!("GOBY_WASM_RUNTIME_PROFILE") {
        Some("wasmtime") => RuntimeProfile::Wasmtime,
        Some("wasmer") => RuntimeProfile::Wasmer,
        _ => RuntimeProfile::Unknown,
    }
}

fn typed_continuation_optimization_gate_enabled() -> bool {
    cfg!(feature = "typed-continuation-optimized")
}

fn runtime_force_portable_fallback_override_enabled() -> bool {
    matches!(
        std::env::var("GOBY_WASM_FORCE_PORTABLE_FALLBACK")
            .ok()
            .as_deref(),
        Some("1" | "true" | "TRUE" | "yes" | "on")
    )
}

fn stmts_contain_resume(stmts: &[Stmt]) -> bool {
    stmts.iter().any(stmt_contains_resume)
}

fn stmt_contains_resume(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Binding { value, .. }
        | Stmt::MutBinding { value, .. }
        | Stmt::Assign { value, .. } => expr_contains_resume(value),
        Stmt::Expr(expr) => expr_contains_resume(expr),
    }
}

fn expr_contains_resume(expr: &Expr) -> bool {
    match expr {
        Expr::Resume { .. } => true,
        Expr::Call { callee, arg } => expr_contains_resume(callee) || expr_contains_resume(arg),
        Expr::Pipeline { value, .. } => expr_contains_resume(value),
        Expr::BinOp { left, right, .. } => {
            expr_contains_resume(left) || expr_contains_resume(right)
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            expr_contains_resume(condition)
                || expr_contains_resume(then_expr)
                || expr_contains_resume(else_expr)
        }
        Expr::Case { scrutinee, arms } => {
            expr_contains_resume(scrutinee)
                || arms.iter().any(|arm| expr_contains_resume(&arm.body))
        }
        Expr::ListLit(items) => items.iter().any(expr_contains_resume),
        Expr::TupleLit(items) => items.iter().any(expr_contains_resume),
        Expr::Lambda { body, .. } => expr_contains_resume(body),
        Expr::Handler { clauses } => clauses.iter().any(|clause| {
            clause
                .parsed_body
                .as_ref()
                .is_some_and(|stmts| stmts_contain_resume(stmts))
        }),
        Expr::With { handler, body } => expr_contains_resume(handler) || stmts_contain_resume(body),
        Expr::Block(stmts) => stmts_contain_resume(stmts),
        Expr::RecordConstruct { fields, .. } => {
            fields.iter().any(|(_, value)| expr_contains_resume(value))
        }
        Expr::MethodCall { args, .. } => args.iter().any(expr_contains_resume),
        Expr::InterpolatedString(parts) => parts.iter().any(|part| match part {
            InterpolatedPart::Text(_) => false,
            InterpolatedPart::Expr(expr) => expr_contains_resume(expr),
        }),
        Expr::Qualified { .. }
        | Expr::Var(_)
        | Expr::StringLit(_)
        | Expr::IntLit(_)
        | Expr::BoolLit(_) => false,
    }
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
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                let val = eval_expr(value, &bindings, &env, 0)?;
                bindings.insert(name.to_string(), val);
            }
            Stmt::Assign { name, value } => {
                let val = eval_expr(value, &bindings, &env, 0)?;
                if !bindings.contains_key(name) {
                    return None;
                }
                bindings.insert(name.to_string(), val);
            }
            Stmt::Expr(expr) => {
                let val = as_print_expr(expr, &bindings, &env)?;
                outputs.push(val.as_output_text());
            }
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
                || is_runtime_intrinsic_name(name)
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
                    // List patterns are intentionally unsupported in native lowering.
                    // Return `None` to force fallback runtime evaluation.
                    (CasePattern::EmptyList | CasePattern::ListPattern { .. }, _) => return None,
                    _ => false,
                };
                if matched {
                    return eval_expr(&arm.body, bindings, env, depth + 1);
                }
            }
            None
        }
        Expr::Block(stmts) => {
            let mut locals = bindings.clone();
            let mut last_expr: Option<NativeValue> = None;
            for stmt in stmts {
                match stmt {
                    Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                        let val = eval_expr(value, &locals, env, depth + 1)?;
                        locals.insert(name.clone(), val);
                        last_expr = None;
                    }
                    Stmt::Assign { name, value } => {
                        if !locals.contains_key(name) {
                            return None;
                        }
                        let val = eval_expr(value, &locals, env, depth + 1)?;
                        locals.insert(name.clone(), val);
                        last_expr = None;
                    }
                    Stmt::Expr(expr) => {
                        last_expr = Some(eval_expr(expr, &locals, env, depth + 1)?);
                    }
                }
            }
            last_expr
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
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                let val = eval_expr(value, &locals, env, depth + 1)?;
                locals.insert(name.clone(), val);
            }
            Stmt::Assign { name, value } => {
                if !locals.contains_key(name) {
                    return None;
                }
                let val = eval_expr(value, &locals, env, depth + 1)?;
                locals.insert(name.clone(), val);
            }
            Stmt::Expr(expr) => {
                let value = eval_expr(expr, &locals, env, depth + 1)?;
                last_expr = Some(value);
            }
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
    if let Some(intrinsic_result) = apply_runtime_intrinsic(name.as_str(), args.as_slice()) {
        return Some(intrinsic_result);
    }
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

fn is_runtime_intrinsic_name(name: &str) -> bool {
    matches!(name, "__goby_string_length" | "__goby_env_fetch_env_var")
}

fn apply_runtime_intrinsic(name: &str, args: &[NativeValue]) -> Option<NativeValue> {
    match name {
        "__goby_string_length" => {
            if args.len() != 1 {
                return None;
            }
            let NativeValue::String(value) = &args[0] else {
                return None;
            };
            let len = i64::try_from(value.chars().count()).ok()?;
            Some(NativeValue::Int(len))
        }
        "__goby_env_fetch_env_var" => {
            if args.len() != 1 {
                return None;
            }
            let NativeValue::String(var_name) = &args[0] else {
                return None;
            };
            Some(NativeValue::String(
                std::env::var(var_name).unwrap_or_default(),
            ))
        }
        _ => None,
    }
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

    use super::{
        EffectExecutionMode, EffectModeFallbackReason, NativeLoweringResult, RuntimeProfile,
        build_lowering_plan, build_typed_continuation_ir, select_effect_execution_mode_with_inputs,
        try_emit_native_module_with_handoff,
    };

    fn expected_compile_time_runtime_profile() -> RuntimeProfile {
        match option_env!("GOBY_WASM_RUNTIME_PROFILE") {
            Some("wasmtime") => RuntimeProfile::Wasmtime,
            Some("wasmer") => RuntimeProfile::Wasmer,
            _ => RuntimeProfile::Unknown,
        }
    }

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
        assert_eq!(handoff.selected_mode, EffectExecutionMode::PortableFallback);
        let expected_reason = match expected_compile_time_runtime_profile() {
            RuntimeProfile::Unknown => EffectModeFallbackReason::RuntimeProfileNotSupported,
            RuntimeProfile::Wasmtime | RuntimeProfile::Wasmer => {
                EffectModeFallbackReason::OptimizationGateDisabled
            }
        };
        assert_eq!(handoff.selected_mode_fallback_reason, Some(expected_reason));
        assert_eq!(
            handoff.runtime_profile,
            expected_compile_time_runtime_profile()
        );
        assert_eq!(handoff.typed_continuation_ir, None);
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

    #[test]
    fn effect_mode_selection_prefers_gate_reason_before_construct_reason() {
        let source = r#"
effect Tick
  tick: Int -> Int

main : Unit -> Unit can Tick
main =
  with_handler
    tick n ->
      resume n
  in
    tick 1
"#;
        let module = parse_module(source).expect("source should parse");
        let selection = select_effect_execution_mode_with_inputs(
            super::LoweringStyle::EffectBoundary,
            &module,
            false,
            RuntimeProfile::Wasmtime,
            false,
        );
        assert_eq!(selection.mode, EffectExecutionMode::PortableFallback);
        assert_eq!(
            selection.fallback_reason,
            Some(EffectModeFallbackReason::OptimizationGateDisabled)
        );
    }

    #[test]
    fn effect_mode_selection_reports_unsupported_construct_when_gate_enabled() {
        let source = r#"
effect Tick
  tick: Int -> Int

main : Unit -> Unit can Tick
main =
  with_handler
    tick n ->
      resume n
  in
    tick 1
"#;
        let module = parse_module(source).expect("source should parse");
        let selection = select_effect_execution_mode_with_inputs(
            super::LoweringStyle::EffectBoundary,
            &module,
            false,
            RuntimeProfile::Wasmtime,
            true,
        );
        assert_eq!(selection.mode, EffectExecutionMode::PortableFallback);
        assert_eq!(
            selection.fallback_reason,
            Some(EffectModeFallbackReason::UnsupportedEffectConstruct)
        );
    }

    #[test]
    fn effect_mode_selection_allows_optimized_mode_for_can_only_effect_boundary() {
        let source = r#"
effect Tick
  tick: Int -> Int

main : Unit -> Unit can Tick
main =
  print 1
"#;
        let module = parse_module(source).expect("source should parse");
        let selection = select_effect_execution_mode_with_inputs(
            super::LoweringStyle::EffectBoundary,
            &module,
            false,
            RuntimeProfile::Wasmer,
            true,
        );
        assert_eq!(
            selection.mode,
            EffectExecutionMode::TypedContinuationOptimized
        );
        assert_eq!(selection.fallback_reason, None);

        let plan = build_lowering_plan(&module);
        let ir = build_typed_continuation_ir(&plan).expect("typed continuation IR should be built");
        assert!(ir.one_shot_resume);
    }

    #[test]
    fn effect_mode_selection_force_portable_override_has_highest_priority() {
        let source = r#"
effect Tick
  tick: Int -> Int

main : Unit -> Unit can Tick
main =
  print 1
"#;
        let module = parse_module(source).expect("source should parse");
        let selection = select_effect_execution_mode_with_inputs(
            super::LoweringStyle::EffectBoundary,
            &module,
            true,
            RuntimeProfile::Wasmer,
            true,
        );
        assert_eq!(selection.mode, EffectExecutionMode::PortableFallback);
        assert_eq!(
            selection.fallback_reason,
            Some(EffectModeFallbackReason::ForcedPortableOverride)
        );
    }
}
