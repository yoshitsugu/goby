use goby_core::Module;

use crate::CodegenError;
use crate::fallback;
use crate::gen_lower;
use crate::gen_lower::GeneralLowerUnsupportedReason;
use crate::lower;
use crate::print_codegen;
use crate::runtime_entry::{
    resolve_module_runtime_output_for_compile, resolve_module_runtime_output_with_mode_and_stdin,
};
use crate::runtime_io_plan::{
    RuntimeIoClassification, RuntimeIoExecutionKind, classify_runtime_io_with_ir_fallback,
    runtime_io_execution_kind,
};
use crate::wasm_exec;

#[derive(Debug, Clone)]
struct FallbackExecutionContext {
    runtime_mode: lower::EffectExecutionMode,
    effect_boundary_handoff: Option<lower::EffectBoundaryHandoff>,
    io_classification: RuntimeIoClassification,
}

impl FallbackExecutionContext {
    fn for_module(module: &Module) -> Result<Self, CodegenError> {
        let (runtime_mode, effect_boundary_handoff) = runtime_mode_and_handoff(module)?;
        Ok(Self {
            runtime_mode,
            effect_boundary_handoff,
            io_classification: classify_runtime_io_with_ir_fallback(module),
        })
    }

    fn compile_runtime_artifact(&self) -> Result<Option<Vec<u8>>, CodegenError> {
        self.io_classification
            .clone()
            .compile_module_wasm_or_error()
    }

    fn require_interpreter_bridge(&self) -> Result<(), CodegenError> {
        self.io_classification
            .clone()
            .require_interpreter_bridge_stdin()
    }

    fn unresolved_output_error(&self, module: &Module) -> CodegenError {
        unresolved_runtime_output_error(module, self.effect_boundary_handoff.clone())
    }
}

enum RuntimeStdinExecutionPlan {
    GeneralLowered,
    InterpreterBridge(Box<FallbackExecutionContext>),
    /// Program was classified as `Unsupported` (not GeneralLowered, not InterpreterBridge) and
    /// `supports_general_lower_module` returned a specific rejection reason.
    /// Callers should surface this as a `CodegenError` with the reason's message.
    UnsupportedWithReason(GeneralLowerUnsupportedReason),
    Fallthrough,
}

pub(crate) fn compile_module_entrypoint(module: &Module) -> Result<Vec<u8>, CodegenError> {
    if let lower::NativeLoweringResult::Emitted(wasm) =
        lower::try_emit_native_module_with_handoff(module)?
    {
        return Ok(wasm);
    }
    if let Some(wasm) = gen_lower::try_general_lower_module(module)? {
        return Ok(wasm);
    }

    let context = FallbackExecutionContext::for_module(module)?;
    if let Some(reason) = unsupported_runtime_io_reason(module, &context.io_classification)? {
        return Err(CodegenError {
            message: format!("general lowering unsupported: {reason}"),
        });
    }
    if let Some(wasm) = context.compile_runtime_artifact()? {
        return Ok(wasm);
    }
    if let Some(text) = resolve_module_runtime_output_for_compile(module, context.runtime_mode)
        .map_err(|message| CodegenError { message })?
    {
        return print_codegen::compile_print_module(&text);
    }

    Err(context.unresolved_output_error(module))
}

pub(crate) fn execute_module_with_stdin_entrypoint(
    module: &Module,
    stdin_seed: Option<String>,
) -> Result<Option<String>, CodegenError> {
    let context = FallbackExecutionContext::for_module(module)?;
    execute_interpreter_bridge_with_context(module, &context, stdin_seed)
}

fn execute_interpreter_bridge_with_context(
    module: &Module,
    context: &FallbackExecutionContext,
    stdin_seed: Option<String>,
) -> Result<Option<String>, CodegenError> {
    context.require_interpreter_bridge()?;
    if let Some(text) =
        resolve_module_runtime_output_with_mode_and_stdin(module, context.runtime_mode, stdin_seed)
    {
        return Ok(Some(text));
    }

    Err(context.unresolved_output_error(module))
}

pub(crate) fn execute_runtime_module_with_stdin_entrypoint(
    module: &Module,
    stdin_seed: Option<String>,
) -> Result<Option<String>, CodegenError> {
    match runtime_stdin_execution_plan(module)? {
        RuntimeStdinExecutionPlan::GeneralLowered => {
            let wasm = compile_module_entrypoint(module)?;
            let output = wasm_exec::run_wasm_bytes_with_stdin(&wasm, stdin_seed.as_deref())
                .map_err(|message| CodegenError { message })?;
            Ok(Some(output))
        }
        RuntimeStdinExecutionPlan::InterpreterBridge(context) => {
            execute_interpreter_bridge_with_context(module, context.as_ref(), stdin_seed)
        }
        RuntimeStdinExecutionPlan::UnsupportedWithReason(reason) => Err(CodegenError {
            message: format!("general lowering unsupported: {reason}"),
        }),
        RuntimeStdinExecutionPlan::Fallthrough => Ok(None),
    }
}

fn unresolved_runtime_output_error(
    module: &Module,
    handoff: Option<lower::EffectBoundaryHandoff>,
) -> CodegenError {
    if let Some(handoff) = handoff {
        if let Some((record, issue)) = handoff.handler_legality.first_issue() {
            return CodegenError {
                message: format!(
                    "backend limitation [E-BACKEND-LIMITATION]: effect handler in '{}' is outside the one-shot tail-resumptive subset (ops={:?}, issue={})",
                    record.decl_name,
                    record.clause_ops,
                    issue.as_str(),
                ),
            };
        }
        if handoff.handler_legality.all_one_shot_tail_resumptive() {
            return CodegenError {
                message: "main body uses one-shot tail-resumptive effect handlers, but WB-3A direct-call lowering is not implemented yet".to_string(),
            };
        }
        return CodegenError {
            message: format!(
                "main lowered as effect boundary (style={:?}, selected_mode={:?}, selected_mode_fallback_reason={:?}, runtime_profile={:?}, typed_continuation_ir_present={}, handlers_resume={}, handler_with_count={}, handler_with_unsupported={}, evidence_ops={}, evidence_requirements={}, evidence_fingerprint_hint={}); fallback runtime output could not be resolved",
                handoff.main_style,
                handoff.selected_mode,
                handoff.selected_mode_fallback_reason,
                handoff.runtime_profile,
                handoff.typed_continuation_ir.is_some(),
                handoff.handler_resume_present,
                handoff.handler_legality.records().len(),
                handoff.handler_legality.has_unsupported(),
                handoff.evidence_operation_table_len,
                handoff.evidence_requirements_len,
                handoff.evidence_fingerprint_hint,
            ),
        };
    }

    if let Some(reason) = fallback::native_unsupported_reason(module) {
        return CodegenError {
            message: format!(
                "main body requires backend features not yet supported by native lowering or static-output resolution (native_backend_limitation={})",
                reason
            ),
        };
    }

    CodegenError {
        message:
            "main body requires backend features not yet supported by native lowering or static-output resolution"
                .to_string(),
    }
}

#[allow(clippy::type_complexity)]
fn runtime_mode_and_handoff(
    module: &Module,
) -> Result<
    (
        lower::EffectExecutionMode,
        Option<lower::EffectBoundaryHandoff>,
    ),
    CodegenError,
> {
    module
        .declarations
        .iter()
        .find(|d| d.name == "main")
        .ok_or_else(|| CodegenError {
            message: "Wasm codegen requires a `main` declaration".to_string(),
        })?;

    let native_attempt = lower::try_emit_native_module_with_handoff(module)?;
    let mut effect_boundary_handoff: Option<lower::EffectBoundaryHandoff> = None;
    match native_attempt {
        lower::NativeLoweringResult::Emitted(_) => {}
        lower::NativeLoweringResult::EffectBoundaryHandoff(handoff) => {
            if handoff.main_style == crate::planning::LoweringStyle::DirectStyle {
                return Err(CodegenError {
                    message:
                        "internal lowering invariant violation: direct-style main produced boundary handoff"
                            .to_string(),
                });
            }
            if let Some(main_req) = handoff.main_requirement
                && main_req.style == crate::planning::LoweringStyle::DirectStyle
                && main_req.passes_evidence
            {
                return Err(CodegenError {
                    message: "internal lowering invariant violation: direct-style main requirement marked as evidence-passing".to_string(),
                });
            }
            effect_boundary_handoff = Some(handoff);
        }
        lower::NativeLoweringResult::NotLowered => {}
    }

    let runtime_mode = effect_boundary_handoff
        .as_ref()
        .map(|handoff| handoff.selected_mode)
        .unwrap_or(lower::EffectExecutionMode::PortableFallback);

    Ok((runtime_mode, effect_boundary_handoff))
}

fn runtime_stdin_execution_plan(
    module: &Module,
) -> Result<RuntimeStdinExecutionPlan, CodegenError> {
    match runtime_io_execution_kind(module)? {
        RuntimeIoExecutionKind::GeneralLowered => Ok(RuntimeStdinExecutionPlan::GeneralLowered),
        RuntimeIoExecutionKind::InterpreterBridge => {
            Ok(RuntimeStdinExecutionPlan::InterpreterBridge(Box::new(
                FallbackExecutionContext::for_module(module)?,
            )))
        }
        RuntimeIoExecutionKind::Unsupported => {
            unsupported_runtime_io_reason(module, &RuntimeIoClassification::Unsupported)?
                .map(RuntimeStdinExecutionPlan::UnsupportedWithReason)
                .ok_or_else(|| CodegenError {
                    message: "internal error: Unsupported IO kind but \
                              supports_general_lower_module returned None on second call"
                        .to_string(),
                })
        }
        RuntimeIoExecutionKind::DynamicWasiIo
        | RuntimeIoExecutionKind::StaticOutput
        | RuntimeIoExecutionKind::NotRuntimeIo => Ok(RuntimeStdinExecutionPlan::Fallthrough),
    }
}

fn unsupported_runtime_io_reason(
    module: &Module,
    classification: &RuntimeIoClassification,
) -> Result<Option<GeneralLowerUnsupportedReason>, CodegenError> {
    if !matches!(classification, RuntimeIoClassification::Unsupported) {
        return Ok(None);
    }

    // The program needs runtime capabilities but general lowering rejected it.
    // Retrieve the specific reason so callers can surface a precise diagnostic.
    // Note: this calls supports_general_lower_module again (also called inside
    // runtime_io_execution_kind). Known double-call trade-off; caching is out of scope.
    gen_lower::supports_general_lower_module(module)
}
