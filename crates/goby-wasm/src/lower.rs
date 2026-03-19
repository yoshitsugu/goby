use std::collections::HashMap;

use goby_core::Module;
use goby_core::ir::{CompExpr, IrBinOp, ValueExpr};

use crate::{
    CodegenError,
    backend::WasmProgramBuilder,
    layout::MemoryLayout,
    planning::{
        DeclarationLoweringMode, EffectId, EffectOperationRef, LoweringPlan, LoweringStyle,
        build_lowering_plan,
    },
    wasm_exec_plan::{decl_exec_plan, main_exec_plan},
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

    if !module.declarations.iter().any(|decl| decl.name == "main") {
        return Ok(NativeLoweringResult::NotLowered);
    }
    let lowering_plan = build_lowering_plan(module);
    let evidence_shape = lowering_plan.evidence_shape();
    let main_style = lowering_plan
        .style_for("main")
        .unwrap_or(LoweringStyle::EffectBoundary);
    if main_style != LoweringStyle::DirectStyle {
        let selection = select_effect_execution_mode(main_style, &lowering_plan);
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
    let Some(output_text) = collect_phase2_output_text_from_ir(module, &lowering_plan) else {
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
    lowering_plan: &LoweringPlan,
) -> EffectExecutionSelection {
    select_effect_execution_mode_with_inputs(
        main_style,
        lowering_plan.handler_resume_present(),
        runtime_force_portable_fallback_override_enabled(),
        compile_time_runtime_profile(),
        typed_continuation_optimization_gate_enabled(),
    )
}

fn select_effect_execution_mode_with_inputs(
    main_style: LoweringStyle,
    handler_resume_present: bool,
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
    if contains_unsupported_effect_construct_for_optimized_path(handler_resume_present) {
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

fn contains_unsupported_effect_construct_for_optimized_path(handler_resume_present: bool) -> bool {
    handler_resume_present
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

#[derive(Clone)]
enum NativeValue {
    String(String),
    Int(i64),
    Bool(bool),
    Unit,
    Callable(NativeCallable),
}

#[derive(Clone)]
enum NativeCallable {
    Named {
        name: String,
        applied_args: Vec<NativeValue>,
    },
}

impl NativeValue {
    fn as_output_text(&self) -> String {
        match self {
            Self::String(s) => s.clone(),
            Self::Int(n) => n.to_string(),
            Self::Bool(true) => "True".to_string(),
            Self::Bool(false) => "False".to_string(),
            Self::Unit => String::new(),
            Self::Callable(_) => "<function>".to_string(),
        }
    }
}

fn collect_phase2_output_text_from_ir(
    module: &Module,
    lowering_plan: &LoweringPlan,
) -> Option<String> {
    let env = EvalEnv::from_module(module, lowering_plan);
    let main_ir = main_exec_plan(module)?.ir_decl?;
    let mut outputs = Vec::new();
    eval_comp(&main_ir.body, &HashMap::new(), &env, 0, &mut outputs)?;
    Some(outputs.join(""))
}

fn eval_comp(
    comp: &CompExpr,
    bindings: &HashMap<String, NativeValue>,
    env: &EvalEnv<'_>,
    depth: usize,
    outputs: &mut Vec<String>,
) -> Option<NativeValue> {
    const MAX_DEPTH: usize = 32;
    if depth >= MAX_DEPTH {
        return None;
    }

    match comp {
        CompExpr::Value(value) => eval_value(value, bindings, env, depth + 1),
        CompExpr::Let {
            name, value, body, ..
        } => {
            let bound = eval_comp(value, bindings, env, depth + 1, outputs)?;
            let mut locals = bindings.clone();
            locals.insert(name.clone(), bound);
            eval_comp(body, &locals, env, depth + 1, outputs)
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                let _ = eval_comp(stmt, bindings, env, depth + 1, outputs)?;
            }
            eval_comp(tail, bindings, env, depth + 1, outputs)
        }
        CompExpr::If { cond, then_, else_ } => {
            let cond = eval_value(cond, bindings, env, depth + 1)?;
            let NativeValue::Bool(flag) = cond else {
                return None;
            };
            if flag {
                eval_comp(then_, bindings, env, depth + 1, outputs)
            } else {
                eval_comp(else_, bindings, env, depth + 1, outputs)
            }
        }
        CompExpr::Call { callee, args } => {
            if matches!(callee.as_ref(), ValueExpr::Var(name) if name == "print") && args.len() == 1
            {
                let value = eval_value(&args[0], bindings, env, depth + 1)?;
                outputs.push(value.as_output_text());
                return Some(NativeValue::Unit);
            }
            let callee_value = eval_value(callee, bindings, env, depth + 1)?;
            let mut arg_values = Vec::with_capacity(args.len());
            for arg in args {
                arg_values.push(eval_value(arg, bindings, env, depth + 1)?);
            }
            apply_callable(callee_value, arg_values, env, depth + 1)
        }
        CompExpr::PerformEffect { effect, op, args } => {
            if effect == "Print" && args.len() == 1 {
                let value = eval_value(&args[0], bindings, env, depth + 1)?;
                match op.as_str() {
                    "print" => outputs.push(value.as_output_text()),
                    "println" => {
                        let mut line = value.as_output_text();
                        line.push('\n');
                        outputs.push(line);
                    }
                    _ => return None,
                }
                Some(NativeValue::Unit)
            } else {
                None
            }
        }
        CompExpr::Handle { .. } | CompExpr::WithHandler { .. } | CompExpr::Resume { .. } => None,
    }
}

fn eval_value(
    value: &ValueExpr,
    bindings: &HashMap<String, NativeValue>,
    env: &EvalEnv<'_>,
    depth: usize,
) -> Option<NativeValue> {
    match value {
        ValueExpr::StrLit(s) => Some(NativeValue::String(s.clone())),
        ValueExpr::IntLit(n) => Some(NativeValue::Int(*n)),
        ValueExpr::BoolLit(b) => Some(NativeValue::Bool(*b)),
        ValueExpr::Unit => Some(NativeValue::Unit),
        ValueExpr::Var(name) => bindings.get(name).cloned().or_else(|| {
            if (env.declarations.contains_key(name.as_str())
                && env.lowering_plan.is_direct_style(name.as_str()))
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
        ValueExpr::GlobalRef { name, .. } => {
            if env.declarations.contains_key(name.as_str())
                && env.lowering_plan.is_direct_style(name)
            {
                Some(NativeValue::Callable(NativeCallable::Named {
                    name: name.clone(),
                    applied_args: Vec::new(),
                }))
            } else {
                None
            }
        }
        ValueExpr::BinOp { op, left, right } => {
            let lhs = eval_value(left, bindings, env, depth + 1)?;
            let rhs = eval_value(right, bindings, env, depth + 1)?;
            match (op, lhs, rhs) {
                (IrBinOp::Add, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Int(a.checked_add(b)?))
                }
                (IrBinOp::Sub, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Int(a.checked_sub(b)?))
                }
                (IrBinOp::Mul, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Int(a.checked_mul(b)?))
                }
                (IrBinOp::Div, NativeValue::Int(a), NativeValue::Int(b)) if b != 0 => {
                    Some(NativeValue::Int(a / b))
                }
                (IrBinOp::Mod, NativeValue::Int(a), NativeValue::Int(b)) if b != 0 => {
                    Some(NativeValue::Int(a % b))
                }
                (IrBinOp::Eq, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Bool(a == b))
                }
                (IrBinOp::Eq, NativeValue::Bool(a), NativeValue::Bool(b)) => {
                    Some(NativeValue::Bool(a == b))
                }
                (IrBinOp::Eq, NativeValue::String(a), NativeValue::String(b)) => {
                    Some(NativeValue::Bool(a == b))
                }
                (IrBinOp::Eq, NativeValue::Unit, NativeValue::Unit) => {
                    Some(NativeValue::Bool(true))
                }
                (IrBinOp::Lt, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Bool(a < b))
                }
                (IrBinOp::Gt, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Bool(a > b))
                }
                (IrBinOp::Le, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Bool(a <= b))
                }
                (IrBinOp::Ge, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Bool(a >= b))
                }
                _ => None,
            }
        }
        ValueExpr::Interp(parts) => {
            let mut out = String::new();
            for part in parts {
                match part {
                    goby_core::ir::IrInterpPart::Text(text) => out.push_str(text),
                    goby_core::ir::IrInterpPart::Expr(expr) => {
                        out.push_str(&eval_value(expr, bindings, env, depth + 1)?.as_output_text());
                    }
                }
            }
            Some(NativeValue::String(out))
        }
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
    let ir_decl = decl_exec_plan(decl).ir_decl?;
    if ir_decl.params.len() != args.len() {
        return None;
    }
    let mut locals: HashMap<String, NativeValue> = HashMap::new();
    for ((param, _), arg_val) in ir_decl.params.iter().zip(args.into_iter()) {
        locals.insert(param.clone(), arg_val);
    }
    let mut outputs = Vec::new();
    eval_comp(&ir_decl.body, &locals, env, depth + 1, &mut outputs)
}

fn apply_callable(
    callee: NativeValue,
    args: Vec<NativeValue>,
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
            applied_args.extend(args);
            apply_named_callable(name, applied_args, env, depth + 1)
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

/// Apply a compile-time-safe runtime intrinsic during direct-style native lowering.
///
/// # Host-environment access policy
///
/// This function is called from `collect_phase2_output_text` which runs **during
/// compilation** (inside `try_emit_native_module_with_handoff`).  Only intrinsics
/// that are safe to evaluate at compile time should be listed here.
///
/// - `__goby_string_length`: COMPILE-TIME-SAFE — pure function over a string literal.
/// - `__goby_env_fetch_env_var`: reads the **compiler-process** environment at compile
///   time.  This is intentional for direct-style programs (the output string is baked
///   into the emitted Wasm), but means that `fetch_env_var` in a direct-style `main`
///   is evaluated against the build-time host env, not the runtime env.
///   TODO(F-sweep): decide whether compile-time env reads should be documented as a
///   user-visible guarantee or restricted to prevent surprising behavior when env vars
///   differ between build time and execution time.
///
/// **Do not add** intrinsics that observe stdin, network, files, clocks, or other
/// non-deterministic host state here.  Those must be lowered to WASI imports instead.
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
  value = double 21
  print value
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
  with
    tick n ->
      resume n
  in
    tick 1
"#;
        let _module = parse_module(source).expect("source should parse");
        let selection = select_effect_execution_mode_with_inputs(
            super::LoweringStyle::EffectBoundary,
            true,
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
  with
    tick n ->
      resume n
  in
    tick 1
"#;
        let _module = parse_module(source).expect("source should parse");
        let selection = select_effect_execution_mode_with_inputs(
            super::LoweringStyle::EffectBoundary,
            true,
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
            false,
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
        let _module = parse_module(source).expect("source should parse");
        let selection = select_effect_execution_mode_with_inputs(
            super::LoweringStyle::EffectBoundary,
            false,
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
