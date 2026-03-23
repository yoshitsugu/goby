mod backend;
mod call;
#[cfg(test)]
mod compile_tests;
mod fallback;
mod gen_lower;
mod grapheme_semantics;
mod host_runtime;
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
mod runtime_io_plan;
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
mod wasm_exec;
mod wasm_exec_plan;

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
use crate::runtime_io_plan::classify_runtime_io_with_ir_fallback;
pub use crate::runtime_io_plan::{RuntimeIoExecutionKind, runtime_io_execution_kind};
use crate::runtime_support::{eval_string_expr, parse_pipeline};
use crate::runtime_value::{RuntimeLocals, RuntimeValue, runtime_value_option_eq};
use goby_core::{
    CasePattern, Expr, HandlerClause, ListPatternItem, ListPatternTail, Module, Stmt,
    ast::InterpolatedPart, types::parse_function_type,
};
use std::collections::{HashMap, HashSet};
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
pub(crate) use crate::runtime_entry::resolve_module_runtime_output;
use crate::runtime_entry::resolve_module_runtime_output_for_compile;
#[cfg(test)]
pub(crate) use crate::runtime_entry::resolve_module_runtime_output_with_mode;
#[cfg(not(test))]
use crate::runtime_entry::resolve_module_runtime_output_with_mode_and_stdin;
#[cfg(test)]
pub(crate) use crate::runtime_entry::resolve_module_runtime_output_with_mode_and_stdin;
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

    Ok((runtime_mode, effect_boundary_handoff))
}

/// Compile a parsed Goby [`Module`] into a WASI Preview 1 Wasm binary.
///
/// # Errors
///
/// Returns [`CodegenError`] when:
/// - `main` declaration is missing.
/// - `main` body requires backend/runtime features that are neither supported by
///   native lowering nor resolvable as static print output.
/// - Internal Wasm encoding fails (e.g. string literal too large).
pub fn compile_module(module: &Module) -> Result<Vec<u8>, CodegenError> {
    if let lower::NativeLoweringResult::Emitted(wasm) =
        lower::try_emit_native_module_with_handoff(module)?
    {
        return Ok(wasm);
    }
    // Attempt general lowering before shape-specific classification.
    if let Some(wasm) = gen_lower::try_general_lower_module(module)? {
        return Ok(wasm);
    }
    let (runtime_mode, effect_boundary_handoff) = runtime_mode_and_handoff(module)?;
    // G6: IR-based classification with AST fallback.
    let io_classification = classify_runtime_io_with_ir_fallback(module);
    if let Some(wasm) = io_classification.compile_module_wasm_or_error()? {
        return Ok(wasm);
    }
    if let Some(text) = resolve_module_runtime_output_for_compile(module, runtime_mode)
        .map_err(|message| CodegenError { message })?
    {
        return print_codegen::compile_print_module(&text);
    }

    Err(unresolved_runtime_output_error(
        module,
        effect_boundary_handoff,
    ))
}

/// Execute an [`InterpreterBridge`][`crate::RuntimeIoExecutionKind::InterpreterBridge`]
/// program using the interpreter runtime, seeding stdin with `stdin_seed`.
///
/// # Temporary bridge — marked for shrinkage
///
/// This function is a **temporary migration aid** intended only for programs that
/// the planner classifies as [`RuntimeIoExecutionKind::InterpreterBridge`].  It must
/// not be used as a general-purpose execution path:
///
/// - It rejects programs classified as `DynamicWasiIo`, `StaticOutput`, `Unsupported`,
///   or `NotRuntimeIo` via an internal guard.
/// - New work should prefer shrinking the set of programs that reach this path by
///   extending `DynamicWasiIo` plan support,
///   not by widening this function's accepted surface.
/// - This function is intended to be called only from the CLI `run` command.
///   It is `pub` only because the CLI lives in a separate crate.
///
/// **Current status**: `InterpreterBridge` is currently reserved for the narrow
/// Track E grapheme-backed stdlib subset in runtime-`Read` programs.  This is
/// intentionally not a generic fallback for arbitrary unsupported runtime shapes.
///
/// # Errors
///
/// Returns [`CodegenError`] when:
/// - `main` declaration is missing.
/// - The program is not classified as `InterpreterBridge`.
/// - The interpreter cannot resolve the program's runtime output.
pub fn execute_module_with_stdin(
    module: &Module,
    stdin_seed: Option<String>,
) -> Result<Option<String>, CodegenError> {
    let (runtime_mode, effect_boundary_handoff) = runtime_mode_and_handoff(module)?;
    // G6: IR-based classification with AST fallback.
    let io_classification = classify_runtime_io_with_ir_fallback(module);
    io_classification.require_interpreter_bridge_stdin()?;

    if let Some(text) =
        resolve_module_runtime_output_with_mode_and_stdin(module, runtime_mode, stdin_seed)
    {
        return Ok(Some(text));
    }

    Err(unresolved_runtime_output_error(
        module,
        effect_boundary_handoff,
    ))
}

/// Execute a runtime-stdin Goby module through the `goby-wasm` owned execution boundary.
///
/// This API owns Track E runtime branching for `goby-cli`: callers should not
/// inspect `RuntimeIoExecutionKind` and special-case `InterpreterBridge`
/// themselves.
///
/// Current behavior:
/// - `GeneralLowered`: compiles the module and executes it via the Goby-owned
///   Wasm runtime, which wires `goby:runtime/track-e` host intrinsics. This
///   path replaces the raw `wasmtime run` process launch for Track E modules.
/// - `InterpreterBridge`: delegates to the narrow seeded-stdin runtime path.
/// - All other execution kinds return `Ok(None)` so the caller can continue with
///   its normal Wasm file execution flow.
pub fn execute_runtime_module_with_stdin(
    module: &Module,
    stdin_seed: Option<String>,
) -> Result<Option<String>, CodegenError> {
    match runtime_io_execution_kind(module)? {
        RuntimeIoExecutionKind::GeneralLowered => {
            let wasm = compile_module(module)?;
            let output = wasm_exec::run_wasm_bytes_with_stdin(&wasm, stdin_seed.as_deref())
                .map_err(|message| CodegenError { message })?;
            Ok(Some(output))
        }
        RuntimeIoExecutionKind::InterpreterBridge => execute_module_with_stdin(module, stdin_seed),
        RuntimeIoExecutionKind::DynamicWasiIo
        | RuntimeIoExecutionKind::StaticOutput
        | RuntimeIoExecutionKind::Unsupported
        | RuntimeIoExecutionKind::NotRuntimeIo => Ok(None),
    }
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
    use wasmparser::Validator;

    use super::*;

    fn assert_valid_wasm_module(wasm: &[u8]) {
        assert!(wasm.len() >= 8, "module too short: {} bytes", wasm.len());
        assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d], "bad wasm magic");
        assert_eq!(&wasm[4..8], &[0x01, 0x00, 0x00, 0x00], "bad wasm version");
        Validator::new()
            .validate_all(wasm)
            .expect("module should pass wasm validation");
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

    #[test]
    fn execute_runtime_module_with_stdin_owns_interpreter_bridge_branch() {
        let module = parse_module(
            r#"
import goby/string ( graphemes )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  parts = graphemes text
  println(parts[1])
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(&module, Some("a👨‍👩‍👧‍👦b".to_string()))
            .expect("Track E bridge execution should be owned by goby-wasm");
        assert_eq!(output.as_deref(), Some("👨‍👩‍👧‍👦\n"));
    }

    #[test]
    fn e4_backend_path_grapheme_count_compiles_with_host_import() {
        // E4 parity test: proves that `__goby_string_each_grapheme` (count form, 1-arg)
        // compiles to Wasm and emits the host import name
        // `__goby_string_each_grapheme_count` in the Wasm binary.
        // This is an unconditional parity gate for E4 — must NOT skip on compile failure.
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  text = read ()
  n = __goby_string_each_grapheme text
  print n
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "grapheme count (1-arg) program with read must classify as GeneralLowered"
        );

        let wasm = compile_module(&module)
            .expect("E4: grapheme count program must compile to Wasm without error");

        let import_name = b"__goby_string_each_grapheme_count";
        assert!(
            wasm.windows(import_name.len()).any(|w| w == import_name),
            "E4: compiled Wasm must contain the track-e host import '__goby_string_each_grapheme_count'"
        );
    }

    #[test]
    fn execute_runtime_module_with_stdin_routes_general_lowered_through_goby_owned_runtime() {
        // `print(read())` is a GeneralLowered program (no Track E host intrinsics needed).
        // This test verifies that the GeneralLowered arm in execute_runtime_module_with_stdin
        // compiles the module and executes it via the Goby-owned Wasm runtime, returning
        // Ok(Some(output)) rather than Ok(None) (which would fall through to external wasmtime).
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  print (read ())
"#,
        )
        .expect("parse should work");

        let output =
            execute_runtime_module_with_stdin(&module, Some("hello from wasm".to_string()))
                .expect("GeneralLowered execution should succeed via Goby-owned runtime");
        assert_eq!(
            output.as_deref(),
            Some("hello from wasm"),
            "GeneralLowered path should execute via run_wasm_bytes_with_stdin and return stdout"
        );
    }

    #[test]
    fn e5_split_list_get_print_executes_via_goby_owned_wasm_runtime() {
        // E5 condition 1 execution gate: `__goby_list_push_string` accumulation path
        // (split -> list.get -> print) executes end-to-end through the Goby-owned Wasm runtime
        // without introducing a second list/string runtime representation.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  println(lines[1])
"#,
        )
        .expect("parse should work");

        let output =
            execute_runtime_module_with_stdin(&module, Some("alpha\nbeta\ngamma".to_string()))
                .expect(
                    "E5: split -> list.get -> println must execute via Goby-owned Wasm runtime",
                );
        assert_eq!(
            output.as_deref(),
            Some("beta\n"),
            "E5: list index 1 of split('alpha\\nbeta\\ngamma', '\\n') must be 'beta'"
        );
    }

    #[test]
    fn e6_graphemes_index_executes_via_goby_owned_wasm_runtime() {
        // E6 end-to-end execution gate: graphemes(text)[1] executes through the
        // fused graphemes-index lowering path and returns the correct grapheme cluster.
        // This test proves that __goby_string_each_grapheme_state executes correctly
        // for an emoji-family input via the Goby-owned Wasm runtime (GeneralLowered path).
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/string ( graphemes )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  parts = graphemes text
  println(parts[1])
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "E6: graphemes + index program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("a👨‍👩‍👧‍👦b".to_string()))
            .expect("E6: graphemes + index must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("👨\u{200d}👩\u{200d}👧\u{200d}👦\n"),
            "E6: graphemes('a👨‍👩‍👧‍👦b')[1] must be the emoji family cluster"
        );
    }

    #[test]
    fn wb1_binop_eq_executes_via_general_lowered() {
        // WB-1: BinOp Eq lowers to tagged i64 comparison and executes correctly.
        // Tests that `2 + 3 == 5` evaluates to True and routes through If correctly.
        // (Printing ints directly requires int-to-string conversion not yet in WB-1.)
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  input = read ()
  println
    if 2 + 3 == 5
      "correct"
    else
      "wrong"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "WB-1: BinOp Eq + If program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("WB-1: BinOp Eq + If must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("correct\n"),
            "WB-1: 2 + 3 == 5 must be True"
        );
    }

    #[test]
    fn wb1_if_true_branch_executes_via_general_lowered() {
        // WB-1: If true branch executes correctly.
        // Goby `if` uses multiline block syntax.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  input = read ()
  println
    if True
      "yes"
    else
      "no"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "WB-1: If program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("WB-1: If true must execute via Goby-owned Wasm runtime");
        assert_eq!(output.as_deref(), Some("yes\n"), "WB-1: if True → yes");
    }

    #[test]
    fn wb1_if_false_branch_executes_via_general_lowered() {
        // WB-1: If false branch executes correctly.
        // Goby `if` uses multiline block syntax with `True`/`False` booleans.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  input = read ()
  println
    if False
      "yes"
    else
      "no"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "WB-1: If false program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("WB-1: If false must execute via Goby-owned Wasm runtime");
        assert_eq!(output.as_deref(), Some("no\n"), "WB-1: if false → no");
    }

    #[test]
    fn wb1_let_mut_and_assign_executes_via_general_lowered() {
        // WB-1: LetMut + Assign lowers correctly and the final value reflects the assignment.
        // Uses string value (not int) since printing ints requires int-to-string not in WB-1.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  input = read ()
  mut x = "first"
  x := "second"
  println x
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "WB-1: LetMut+Assign program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("WB-1: LetMut+Assign must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("second\n"),
            "WB-1: x := second then println x → second"
        );
    }

    #[test]
    fn wb1_string_interp_executes_via_general_lowered() {
        // WB-1: String interpolation lowers via StringConcat and executes correctly.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  name = read()
  println "hello ${name}"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "WB-1: Interp program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("world".to_string()))
            .expect("WB-1: Interp must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("hello world\n"),
            "WB-1: hello ${{name}} with name=world → hello world"
        );
    }

    #[test]
    fn wb2a_helper_decl_call_executes_via_general_lowered() {
        // WB-2A: A top-level helper function called from main is compiled via DeclCall.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
greet : String -> Unit can Print
greet name =
  println "hello ${name}"

main : Unit -> Unit can Print, Read
main =
  input = read()
  greet input
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "WB-2A: helper decl call program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("world".to_string()))
            .expect("WB-2A: helper decl call must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("hello world\n"),
            "WB-2A: greet(world) → hello world"
        );
    }

    #[test]
    fn wb2a_recursive_decl_call_executes_via_general_lowered() {
        // WB-2A: A recursive helper function is compiled and executes correctly.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
repeat : Int -> String -> Unit can Print
repeat n s =
  if n == 0
    ()
  else
    println s
    repeat (n - 1) s

main : Unit -> Unit can Print, Read
main =
  input = read()
  repeat 3 input
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "WB-2A: recursive decl call program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("hi".to_string()))
            .expect("WB-2A: recursive decl call must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("hi\nhi\nhi\n"),
            "WB-2A: repeat 3 hi → hi x3"
        );
    }
}
