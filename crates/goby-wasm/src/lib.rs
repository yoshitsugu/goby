mod backend;
mod call;
#[cfg(test)]
mod compile_tests;
mod fallback;
mod layout;
mod lower;
mod planning;
mod print_codegen;
mod runtime_apply;
#[cfg(test)]
mod runtime_behavior_tests;
mod runtime_decl;
mod runtime_dispatch;
mod runtime_entry;
mod runtime_env;
mod runtime_eval;
mod runtime_exec;
mod runtime_expr;
mod runtime_flow;
#[cfg(test)]
mod runtime_output_tests;
#[cfg(test)]
mod runtime_parity;
mod runtime_replay;
mod runtime_resolver;
#[cfg(test)]
mod runtime_resume_tests;
mod runtime_support;
mod runtime_unit;
mod runtime_value;
mod support;

use std::collections::{HashMap, HashSet};
use unicode_segmentation::UnicodeSegmentation;

use crate::call::flatten_named_call;
use crate::runtime_env::{
    EmbeddedEffectRuntime, RuntimeImportContext, effective_runtime_imports,
    load_runtime_import_context, runtime_import_selects_name,
};
use crate::runtime_eval::{
    AstLambdaCallable, IntCallable, Statement, is_identifier, parse_call, parse_int_callable,
    statements,
};
use crate::runtime_flow::{
    ApplyStep, Cont, Continuation, DirectCallHead, Escape, FinishKind, HandlerCompletion,
    HandlerContinuationState, InlineHandlerMethod, InlineHandlerValue, OptimizedResumeToken, Out,
    ResolvedEffectHandler, ResolvedHandlerMethod, ResumeToken, RuntimeDeclInfo, RuntimeError,
    RuntimeEvaluators, RuntimeHandlerMethod, StoreOp, WithId,
};
use crate::runtime_support::{eval_string_expr, flatten_direct_call, parse_pipeline};
use crate::runtime_value::{RuntimeLocals, RuntimeValue, runtime_value_option_eq};
use goby_core::{
    CasePattern, Expr, HandlerClause, ListPatternItem, ListPatternTail, Module, Stmt,
    ast::InterpolatedPart, types::parse_function_type,
};
const ERR_MISSING_MAIN: &str = "Wasm codegen requires a `main` declaration";
pub(crate) const BUILTIN_PRINT: &str = "print";
const PRELUDE_MODULE_PATH: &str = "goby/prelude";
pub(crate) const MAX_EVAL_DEPTH: usize = 32;
const ERR_RESUME_MISSING: &str = "resume used without an active continuation [E-RESUME-MISSING]: `resume` can only be called while executing a handler operation body";
const ERR_RESUME_CONSUMED: &str = "resume continuation already consumed [E-RESUME-CONSUMED]: continuations are one-shot; call `resume` at most once per handled operation";
const ERR_RESUME_STACK_MISMATCH: &str = "internal resume token stack mismatch [E-RESUME-STACK-MISMATCH]: continuation token stack became unbalanced";
const INTERNAL_ABORT_MARKER: &str = "__goby_runtime_abort__";
const ERR_CALLABLE_DISPATCH_DECL_PARAM: &str = "unsupported callable dispatch [E-CALLABLE-DISPATCH]: callable parameter requires a lambda or function name argument";

#[cfg(test)]
pub(crate) use crate::runtime_entry::resolve_main_runtime_output;
use crate::runtime_entry::resolve_main_runtime_output_for_compile;
#[cfg(test)]
pub(crate) use crate::runtime_entry::resolve_main_runtime_output_with_mode;
use crate::runtime_entry::resolve_main_runtime_output_with_mode_and_stdin;
#[cfg(test)]
pub(crate) use crate::runtime_parity::{
    assert_mode_parity, assert_perf_within_threshold, measure_runtime_mode_micros,
};

#[derive(Debug, Clone, PartialEq, Eq)]
/// Error returned by [`compile_module`] when Wasm emission fails.
pub struct CodegenError {
    pub message: String,
}

fn unresolved_runtime_output_error(
    module: &Module,
    handoff: Option<lower::EffectBoundaryHandoff>,
) -> CodegenError {
    if let Some(handoff) = handoff {
        return CodegenError {
            message: format!(
                "main lowered as effect boundary (style={:?}, selected_mode={:?}, selected_mode_fallback_reason={:?}, runtime_profile={:?}, typed_continuation_ir_present={}, handlers_resume={}, evidence_ops={}, evidence_requirements={}, evidence_fingerprint_hint={}); fallback runtime output could not be resolved",
                handoff.main_style,
                handoff.selected_mode,
                handoff.selected_mode_fallback_reason,
                handoff.runtime_profile,
                handoff.typed_continuation_ir.is_some(),
                handoff.handler_resume_present,
                handoff.evidence_operation_table_len,
                handoff.evidence_requirements_len,
                handoff.evidence_fingerprint_hint,
            ),
        };
    }

    if let Some(reason) = fallback::native_unsupported_reason(module) {
        return CodegenError {
            message: format!(
                "main body contains unsupported constructs that cannot be lowered natively or resolved as static output (native_unsupported_reason={})",
                reason
            ),
        };
    }

    CodegenError {
        message:
            "main body contains unsupported constructs that cannot be lowered natively or resolved as static output"
                .to_string(),
    }
}

fn runtime_mode_and_handoff(
    module: &Module,
) -> Result<
    (
        lower::EffectExecutionMode,
        Option<lower::EffectBoundaryHandoff>,
        Option<(String, Option<&[Stmt]>)>,
    ),
    CodegenError,
> {
    let main = module
        .declarations
        .iter()
        .find(|d| d.name == "main")
        .ok_or_else(|| CodegenError {
            message: ERR_MISSING_MAIN.to_string(),
        })?;

    let native_attempt = lower::try_emit_native_module_with_handoff(module)?;
    let mut effect_boundary_handoff: Option<lower::EffectBoundaryHandoff> = None;
    match native_attempt {
        lower::NativeLoweringResult::Emitted(_) => {}
        lower::NativeLoweringResult::EffectBoundaryHandoff(handoff) => {
            if handoff.main_style == planning::LoweringStyle::DirectStyle {
                return Err(CodegenError {
                    message:
                        "internal lowering invariant violation: direct-style main produced boundary handoff"
                            .to_string(),
                });
            }
            if let Some(main_req) = handoff.main_requirement
                && main_req.style == planning::LoweringStyle::DirectStyle
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

    Ok((
        runtime_mode,
        effect_boundary_handoff,
        Some((main.body.clone(), main.parsed_body.as_deref())),
    ))
}

/// Compile a parsed Goby [`Module`] into a WASI Preview 1 Wasm binary.
///
/// # Errors
///
/// Returns [`CodegenError`] when:
/// - `main` declaration is missing.
/// - `main` body contains constructs that are neither natively lowerable nor
///   resolvable as static print output.
/// - Internal Wasm encoding fails (e.g. string literal too large).
pub fn compile_module(module: &Module) -> Result<Vec<u8>, CodegenError> {
    if let lower::NativeLoweringResult::Emitted(wasm) =
        lower::try_emit_native_module_with_handoff(module)?
    {
        return Ok(wasm);
    }
    let (runtime_mode, effect_boundary_handoff, main) = runtime_mode_and_handoff(module)?;
    let Some((main_body, parsed_body)) = main else {
        return Err(CodegenError {
            message: ERR_MISSING_MAIN.to_string(),
        });
    };
    if let Some(text) =
        resolve_main_runtime_output_for_compile(module, &main_body, parsed_body, runtime_mode)
    {
        if text.contains("compile-time fallback cannot consume stdin") {
            return Err(CodegenError {
                message: text.trim_start_matches("runtime error: ").to_string(),
            });
        }
        return print_codegen::compile_print_module(&text);
    }

    Err(unresolved_runtime_output_error(
        module,
        effect_boundary_handoff,
    ))
}

pub fn execute_module_with_stdin(
    module: &Module,
    stdin_seed: Option<String>,
) -> Result<Option<String>, CodegenError> {
    let (runtime_mode, effect_boundary_handoff, main) = runtime_mode_and_handoff(module)?;
    let Some((main_body, parsed_body)) = main else {
        return Err(CodegenError {
            message: ERR_MISSING_MAIN.to_string(),
        });
    };

    if let Some(text) = resolve_main_runtime_output_with_mode_and_stdin(
        module,
        &main_body,
        parsed_body,
        runtime_mode,
        stdin_seed,
    ) {
        return Ok(Some(text));
    }

    Err(unresolved_runtime_output_error(
        module,
        effect_boundary_handoff,
    ))
}

pub(crate) struct RuntimeOutputResolver<'m> {
    pub(crate) locals: RuntimeLocals,
    pub(crate) module: &'m Module,
    pub(crate) runtime_imports: RuntimeImportContext,
    pub(crate) embedded_effect_runtime: EmbeddedEffectRuntime,
    pub(crate) current_module_stack: Vec<String>,
    pub(crate) current_decl_stack: Vec<String>,
    /// Active inline handlers installed via `with` / `with`.
    pub(crate) active_inline_handler_stack: Vec<InlineHandlerValue>,
    pub(crate) resume_tokens: Vec<ResumeToken>,
    pub(crate) optimized_resume_tokens: Vec<OptimizedResumeToken>,
    /// Phase 4: stack of caller's remaining continuations, one entry per active
    /// execute_ast_stmt_sequence invocation. Top entry is consumed by
    /// begin_handler_continuation_bridge during handler dispatch.
    pub(crate) pending_caller_cont_stack: Vec<Option<Cont>>,
    pub(crate) runtime_error: Option<String>,
    pub(crate) next_with_id: u64,
    pub(crate) execution_mode: lower::EffectExecutionMode,
}

impl<'m> RuntimeOutputResolver<'m> {}

#[cfg(test)]
mod tests {
    use goby_core::parse_module;

    use super::*;

    fn assert_valid_wasm_module(wasm: &[u8]) {
        assert!(wasm.len() >= 8, "module too short: {} bytes", wasm.len());
        assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d], "bad wasm magic");
        assert_eq!(&wasm[4..8], &[0x01, 0x00, 0x00, 0x00], "bad wasm version");
    }

    #[test]
    fn emits_valid_wasm_for_long_print_literal() {
        let long_text = "x".repeat(128);
        let source = format!("main : Unit -> Unit\nmain = print \"{}\"\n", long_text);
        let module = parse_module(&source).expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn emits_valid_wasm_for_print_via_local_binding() {
        let source = r#"
main : Unit -> Unit
main =
  greeting = "Hello from local"
  print greeting
"#;
        let module = parse_module(source).expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn emits_valid_wasm_for_print_int_binding() {
        let source = r#"
main : Unit -> Unit
main =
  n = 10
  print n
"#;
        let module = parse_module(source).expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }
}
