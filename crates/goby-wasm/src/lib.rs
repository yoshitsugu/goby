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
pub(crate) use crate::runtime_entry::resolve_main_runtime_output_with_mode;
#[cfg(test)]
pub(crate) use crate::runtime_parity::{
    assert_mode_parity, assert_perf_within_threshold, measure_runtime_mode_micros,
};

#[derive(Debug, Clone, PartialEq, Eq)]
/// Error returned by [`compile_module`] when Wasm emission fails.
pub struct CodegenError {
    pub message: String,
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
    let Some(main) = module.declarations.iter().find(|d| d.name == "main") else {
        return Err(CodegenError {
            message: ERR_MISSING_MAIN.to_string(),
        });
    };

    let native_attempt = lower::try_emit_native_module_with_handoff(module)?;
    let mut effect_boundary_handoff: Option<lower::EffectBoundaryHandoff> = None;
    match native_attempt {
        lower::NativeLoweringResult::Emitted(wasm) => return Ok(wasm),
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
        _ => {}
    }

    let runtime_mode = effect_boundary_handoff
        .as_ref()
        .map(|handoff| handoff.selected_mode)
        .unwrap_or(lower::EffectExecutionMode::PortableFallback);
    if let Some(text) = resolve_main_runtime_output_with_mode(
        module,
        &main.body,
        main.parsed_body.as_deref(),
        runtime_mode,
    ) {
        return print_codegen::compile_print_module(&text);
    }

    if let Some(handoff) = effect_boundary_handoff {
        return Err(CodegenError {
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
        });
    }

    if let Some(reason) = fallback::native_unsupported_reason(module) {
        return Err(CodegenError {
            message: format!(
                "main body contains unsupported constructs that cannot be lowered natively or resolved as static output (native_unsupported_reason={})",
                reason
            ),
        });
    }

    Err(CodegenError {
        message: "main body contains unsupported constructs that cannot be lowered natively or resolved as static output".to_string(),
    })
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
    use std::sync::Mutex;

    use goby_core::parse_module;

    use super::*;

    /// Serializes tests that read or write process-wide environment variables.
    static ENV_MUTEX: Mutex<()> = Mutex::new(());

    fn assert_valid_wasm_module(wasm: &[u8]) {
        assert!(wasm.len() >= 8, "module too short: {} bytes", wasm.len());
        assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d], "bad wasm magic");
        assert_eq!(&wasm[4..8], &[0x01, 0x00, 0x00, 0x00], "bad wasm version");
    }

    fn main_body(module: &Module) -> &str {
        module
            .declarations
            .iter()
            .find(|decl| decl.name == "main")
            .map(|decl| decl.body.as_str())
            .expect("main should exist")
    }

    fn main_parsed_body(module: &Module) -> Option<&[Stmt]> {
        module
            .declarations
            .iter()
            .find(|decl| decl.name == "main")
            .and_then(|decl| decl.parsed_body.as_deref())
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
    fn resolves_runtime_output_for_list_spread_int_values() {
        let source = r#"
main : Unit -> Unit
main =
  rest = [2, 3]
  print [1, ..rest]
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            !fallback::supports_native_codegen(&module),
            "list spread currently routes through fallback runtime path"
        );
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[1, 2, 3]");
    }

    #[test]
    fn resolves_runtime_output_for_list_spread_string_values() {
        let source = r#"
main : Unit -> Unit
main =
  rest = ["b", "c"]
  print ["a", ..rest]
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            !fallback::supports_native_codegen(&module),
            "list spread currently routes through fallback runtime path"
        );
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[\"a\", \"b\", \"c\"]");
    }

    #[test]
    fn list_literal_replays_handled_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    xs = [next 0, 2]
    print xs
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[1, 2]");
    }

    #[test]
    fn typed_mode_matches_fallback_for_list_literal_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    xs = [next 0, 2]
    print xs
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "list literal replay");
        assert_eq!(typed.stdout.as_deref(), Some("[2, 2]"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn record_constructor_replays_handled_field_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
type Box = Box(value: Int)

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    box = Box(value: next 0)
    print box.value
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "1");
    }

    #[test]
    fn typed_mode_matches_fallback_for_record_constructor_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
type Box = Box(value: Int)

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    box = Box(value: next 0)
    print box.value
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "record constructor replay");
        assert_eq!(typed.stdout.as_deref(), Some("2"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn positional_single_field_constructor_replays_handled_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
type Error = Error(message: String)

effect Msg
  next: Unit -> String

effect RaiseError
  raise: Error -> Unit

main : Unit -> Unit
main =
  with
    next _ ->
      resume "oops"
  in
    with
      raise e ->
        print e.message
        resume ()
    in
      raise Error(next ())
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "oops");
    }

    #[test]
    fn typed_mode_matches_fallback_for_positional_single_field_constructor_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
type Error = Error(message: String)

effect Msg
  next: Unit -> String

effect RaiseError
  raise: Error -> Unit

main : Unit -> Unit
main =
  with
    next _ ->
      resume "later"
  in
    with
      raise e ->
        print e.message
        resume ()
    in
      raise Error(next ())
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "positional single-field constructor replay");
        assert_eq!(typed.stdout.as_deref(), Some("later"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn handler_body_sequential_value_binding_with_inner_effect_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape A: handler body has a sequential value-position binding where
        // the RHS calls a different effect whose handler also lives in the same
        // `with` block.
        //
        //   outer `next _ ->` handler body:
        //     x = get ()     <- calls the `get` handler; resolves synchronously
        //     resume (x + 10)
        //
        // Because `get _ -> resume 5` resolves inline, dispatch_handler_method_core
        // returns HandlerCompletion::Resumed(5), which is converted to
        // AstEvalOutcome::Complete(5) at the call site. The Suspended branch inside
        // the Stmt::Binding arm is NOT reached here. This test locks the happy-path
        // where both effects coexist in one `with` block and the inner one completes
        // synchronously.
        //
        // Two different effects (and therefore two distinct handler methods) are used
        // so that the outer `next` handler body does not invoke itself.
        let source = r#"
effect Source
  next: Unit -> Int

effect Store
  get: Unit -> Int

main : Unit -> Unit
main =
  with
    next _ ->
      x = get ()
      resume (x + 10)
    get _ ->
      resume 5
  in
    print (next ())
"#;
        let module = parse_module(source).expect("parse should work");
        // get() resolves to 5, then next resumes with 5+10=15
        let typed = assert_mode_parity(
            &module,
            "handler body sequential value binding with inner effect call",
        );
        assert_eq!(typed.stdout.as_deref(), Some("15"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn in_block_calls_declaration_that_invokes_effect() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape B (in-block variant): the `in` block calls a declaration
        // `get_plus` whose body calls an effect and adds to the result.
        // (See `handler_body_sequential_value_binding_with_inner_effect_call`
        // for Shape A.)
        //
        // `get_plus n = next n + 5`, handler resumes with `n + 5`.
        // `get_plus 0`: next(0) fires handler → resumes 5 →
        //   declaration continues 5 + 5 = 10. Output: "10".
        //
        // This exercises the path where a declaration called from the `in`
        // block suspends at an inner effect call, then resumes and continues
        // with the remaining computation.
        //
        // Note: calling get_plus from the handler body itself (not the `in`
        // block) currently does not work because the handler body statement
        // executor drops the continuation when an inner eval suspends (see
        // TODO comments in dispatch_handler_method_core). This test covers
        // the supported in-block variant.
        let source = r#"
effect Eff
  next: Int -> Int

get_plus : Int -> Int
get_plus n = next n + 5

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 5)
  in
    print (get_plus 0)
"#;
        let module = parse_module(source).expect("parse should work");
        // get_plus 0 -> next 0 (handler resumes 5) -> 5 + 5 = 10
        let typed = assert_mode_parity(&module, "in-block call to declaration that invokes effect");
        assert_eq!(typed.stdout.as_deref(), Some("10"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn handler_body_binding_resumes_via_outer_with_block_synchronous_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Handler body sequential value binding where the inner effect is
        // handled by an OUTER `with` block (nested `with` blocks).
        //
        // Inner `next` handler body: `x = get (); resume (x + 10)`.
        // Outer handler for `get` resumes with 5.
        //
        // When the handler body calls `get ()`, dispatch_handler_method_core
        // for `get` runs synchronously within the call stack. The resume token
        // created for the `get` handler dispatch has frame=None (no pending
        // stmt/value continuation at that dispatch depth), so the resume path
        // takes the synchronous branch:
        //   resume_through_active_continuation_out returns Out::Done(5).
        // Thus eval_expr_to_option(get ()) inside the handler body returns
        // Some(5), x=5 is stored, then resume(5+10)=15.
        //
        // The Suspended branch of Stmt::Binding in dispatch_handler_method_core
        // is NOT reached here (synchronous dispatch only; the async suspension
        // path where Suspended is returned is a known TODO at that site).
        //
        // Expected output: "15".
        let source = r#"
effect Source
  next: Unit -> Int

effect Store
  get: Unit -> Int

main : Unit -> Unit
main =
  with
    get _ ->
      resume 5
  in
    with
      next _ ->
        x = get ()
        resume (x + 10)
    in
      print (next ())
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(
            &module,
            "handler body binding resumes via outer with block (synchronous dispatch)",
        );
        assert_eq!(typed.stdout.as_deref(), Some("15"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn declaration_body_two_binding_progression_value_combination() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape C: declaration body with two sequential value-position bindings
        // both suspending at effect calls, whose results are combined in the
        // final expression.
        //
        // `sum_two`: a = next(0) → 1, b = next(a) → 2, returns a + b = 3.
        //
        // This exercises two sequential Stmt::Binding suspensions in a
        // declaration body. Each binding goes through the stmt continuation
        // mechanism:
        //   1st: push BindValue{a}, eval next(n) suspends, stmt cont captures
        //        remaining [b=next(a), a+b]. Resume → a=1 stored, remaining runs.
        //   2nd: push BindValue{b}, eval next(a) suspends, stmt cont captures
        //        remaining [a+b]. Resume → b=2 stored, a+b=3 returned.
        let source = r#"
effect Iter
  next: Int -> Int

sum_two : Int -> Int
sum_two n =
  a = next n
  b = next a
  a + b

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (sum_two 0)
"#;
        let module = parse_module(source).expect("parse should work");
        // sum_two 0: a=next(0)=1, b=next(1)=2, a+b=3
        let typed = assert_mode_parity(
            &module,
            "declaration body two binding progression value combination",
        );
        assert_eq!(typed.stdout.as_deref(), Some("3"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn three_step_in_block_binding_progression() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape E: 3 sequential in-block binding suspensions driven by the same handler.
        // (Shape D = handler_body_binding_resumes_via_outer_with_block_synchronous_dispatch.)
        // Extends `typed_mode_matches_fallback_for_binding_value_replay` (2-step) to confirm
        // the stmt continuation mechanism handles an arbitrary chain of suspensions.
        //
        // x=next(0)=1, y=next(1)=2, z=next(2)=3. print z. Output: "3".
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    x = next 0
    y = next x
    z = next y
    print z
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "three-step in-block binding progression");
        assert_eq!(typed.stdout.as_deref(), Some("3"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn handler_body_with_inner_with_block_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape F: nested `with` as the binding-RHS inside a handler body.
        // The outer handler for `run` binds `result` to the value produced by an inner
        // `with` block that handles `step`. The inner handler drives two sequential
        // bindings; `resume result` delivers the combined value to the outer continuation.
        //
        // a=step(1)=2, b=step(2)=4. result=a+b=6. resume 6. Output: "6".
        let source = r#"
effect Outer
  run: Unit -> Int

effect Inner
  step: Int -> Int

main : Unit -> Unit
main =
  with
    run _ ->
      result =
        with
          step n ->
            resume (n * 2)
        in
          a = step 1
          b = step a
          a + b
      resume result
  in
    print (run ())
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "handler body with inner with block value");
        assert_eq!(typed.stdout.as_deref(), Some("6"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn assignment_rhs_next_line_with_block_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape G: `name :=\n  with ...` (assignment from next-line with block).
        // Exercises the Assign branch in parse_stmts_from_lines for next-line `with` RHS,
        // plus runtime evaluation of Stmt::Assign where the value comes from a with block.
        //
        // mut result = 0
        // result :=
        //   with step n -> resume (n * 3)
        //   in a = step 1; a + 5
        // a = step(1) = 3; result = 3 + 5 = 8. Output: "8".
        let source = r#"
effect Iter
  step: Int -> Int

main : Unit -> Unit
main =
  with
    step n ->
      resume (n * 3)
  in
    mut result = 0
    result :=
      with
        step n ->
          resume (n * 3)
      in
        a = step 1
        a + 5
    print result
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "assignment rhs next-line with block value");
        assert_eq!(typed.stdout.as_deref(), Some("8"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn case_arm_body_calls_effect_operation() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape H: effect call inside a `case` arm body (not the scrutinee).
        // Covers the arm-body effect-call path in the AST evaluator, which is distinct
        // from the scrutinee-replay path tested by resume_replays_case_scrutinee_continuation.
        //
        // get_next_from_case 0 → case arm `0 -> next 0` → next(0)=10. Output: "10".
        // get_next_from_case 5 → case arm `_ -> next n` → next(5)=15. Output: "15".
        let source = r#"
effect Iter
  next: Int -> Int

get_next_from_case : Int -> Int
get_next_from_case n =
  case n
    0 -> next 0
    _ -> next n

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 10)
  in
    print (get_next_from_case 0)
    print (get_next_from_case 5)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "case arm body calls effect operation");
        assert_eq!(typed.stdout.as_deref(), Some("1015"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn declaration_block_body_with_binding_and_effect_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape I: declaration body (Expr::Block) has a Binding + effect call, called via
        // `print (get_advanced 5)`. Exercises eval_expr_to_option in the print arg path
        // (migrated from eval_ast_value so declaration-body effect calls are supported).
        //
        // get_advanced n: a = next(n) = n+1, returns a+10. With n=5: a=6, a+10=16.
        let source = r#"
effect Iter
  next: Int -> Int

get_advanced : Int -> Int
get_advanced n =
  a = next n
  a + 10

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (get_advanced 5)
"#;
        let module = parse_module(source).expect("parse should work");
        // a=next(5)=6, a+10=16. Output: "16".
        let typed = assert_mode_parity(
            &module,
            "declaration block body with binding and effect call",
        );
        assert_eq!(typed.stdout.as_deref(), Some("16"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn interpolated_string_with_declaration_body_effect_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape J: string interpolation embeds a declaration call that invokes an effect.
        // `get_val n` returns next(n). `print "result=${get_val 3}"` → "result=4".
        // Exercises eval_expr_to_option for InterpolatedString segment evaluation
        // when the segment is a call whose declaration body calls an effect operation.
        let source = r#"
effect Iter
  next: Int -> Int

get_val : Int -> Int
get_val n =
  next n

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print "result=${get_val 3}"
"#;
        let module = parse_module(source).expect("parse should work");
        // get_val(3) = next(3) = 4. Output: "result=4".
        let typed = assert_mode_parity(
            &module,
            "interpolated string with declaration body effect call",
        );
        assert_eq!(typed.stdout.as_deref(), Some("result=4"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn pipeline_value_with_declaration_body_effect_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape K: pipeline left-hand side is a declaration call whose body invokes an effect.
        // Exercises eval_expr_to_option for the Pipeline value arm in eval_ast_side_effect
        // (migrated from eval_ast_value).
        //
        // `(get_val 3) |> log` where get_val n = next n. next(3)=4. log("4") prints "4".
        let source = r#"
effect Iter
  next: Int -> Int

get_val : Int -> Int
get_val n =
  next n

main : Unit -> Unit
main =
  with
    next n -> resume (n + 1)
  in
    print (get_val 3 |> get_val)
"#;
        let module = parse_module(source).expect("parse should work");
        // get_val(3) = next(3) = 4, get_val(4) = next(4) = 5. Output: "5".
        let typed = assert_mode_parity(&module, "pipeline value with declaration body effect call");
        assert_eq!(typed.stdout.as_deref(), Some("5"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_list_each_with_effect_callback() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for list.each with effectful lambda callback.
        // Covers the typed-mode path for execute_decl_with_callable_as_side_effect
        // when the callable invokes an effect operation inside the lambda body.
        let source = r#"
import goby/list

effect Log
  log : Int -> Unit

main : Unit -> Unit
main =
  with
    log n ->
      print "${n}"
      resume ()
  in
    list.each [2, 4, 6] (|n| -> log n)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "list each with effect callback");
        assert_eq!(typed.stdout.as_deref(), Some("246"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_with_handler_variable() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for `with <handler-var> in ...` where the handler is stored
        // in a local variable before being installed. Covers the handler-value path
        // in both PortableFallback and TypedContinuationOptimized modes.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  h = handler
    log msg ->
      print msg
      resume ()
  with h
  in
    log "parity"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "with handler variable");
        assert_eq!(typed.stdout.as_deref(), Some("parity"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_with_captures_lexical_local() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for handler clause that captures a lexical local (`prefix`)
        // defined before the `with` block. Covers closure-capture in both modes.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  prefix = "pre:"
  with
    log msg ->
      print "${prefix}${msg}"
      resume ()
  in
    log "hello"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "with captures lexical local");
        assert_eq!(typed.stdout.as_deref(), Some("pre:hello"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_nested_with_nearest_handler_wins() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for nested `with` blocks on the same effect: the inner handler
        // should win over the outer one in both modes.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "outer"
      resume ()
  in
    with
      log msg ->
        print "inner"
        resume ()
    in
      log "x"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "nested with nearest handler wins");
        assert_eq!(typed.stdout.as_deref(), Some("inner"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_qualified_effect_call_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for `Effect.op arg` (qualified call) dispatching to an active
        // inline handler in both PortableFallback and TypedContinuationOptimized modes.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    Log.log "qualified"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "qualified effect call dispatch");
        assert_eq!(typed.stdout.as_deref(), Some("qualified"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_pipeline_effect_call_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for `value |> effect_op` (pipeline into an effect operation)
        // dispatching to the active handler in both modes.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    "piped" |> log
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "pipeline effect call dispatch");
        assert_eq!(typed.stdout.as_deref(), Some("piped"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_basic_with_inline_handler_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for the most basic `with` form: inline handler, single operation call.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    log "hello"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "basic with inline handler dispatch");
        assert_eq!(typed.stdout.as_deref(), Some("hello"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_resume_outside_handler_error() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for `resume` used outside any handler context.
        // Both modes should surface a deterministic runtime error (E-RESUME-MISSING).
        let source = r#"
main : Unit -> Unit
main =
  print (resume 1)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "resume outside handler error");
        assert_eq!(typed.runtime_error_kind, Some("continuation_missing"));
    }

    #[test]
    fn typed_mode_matches_fallback_for_multi_arg_effect_op_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for a multi-argument effect operation: `yield _ step -> resume (True, step+1)`.
        // Exercises the named-call path with two-arg dispatch in both modes.
        let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

main : Unit -> Unit
main =
  with
    yield _ step ->
      resume (True, step + 1)
  in
    print (yield "x" 41)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "multi-arg effect op call");
        assert_eq!(typed.stdout.as_deref(), Some("(True, 42)"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_qualified_call_without_active_handler() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test: `Log.log "x"` with no active handler for `Log` should produce no
        // output and no runtime error in both modes (silent fall-through).
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  Log.log "unreachable"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "qualified call without active handler");
        assert_eq!(typed.stdout, None);
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_lambda_closure_capture() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for inline lambda that captures a lexical local from the enclosing scope.
        // `each_two (|n| -> print "${n + base}")` where `base = 40`.
        let source = r#"
each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

main : Unit -> Unit
main =
  base = 40
  each_two (|n| -> print "${n + base}")
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "lambda closure capture");
        assert_eq!(typed.stdout.as_deref(), Some("4142"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_list_each_style_callback_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for list-each-style iteration using a custom `each` that installs a
        // handler variable internally. Covers recursive `iter` + handler-variable `with` path.
        let source = r#"
effect ListYield
  yield : Int -> Bool

iter : List Int -> Unit can ListYield
iter xs =
  case xs
    [] -> ()
    [x, ..xxs] ->
      if yield x
        iter xxs
      else
        ()

each : List Int -> (Int -> Unit) -> Unit
each xs f =
  emit_handler = handler
    yield x ->
      f x
      resume True
  with emit_handler
  in
    iter xs

main : Unit -> Unit
main =
  each [3, 5] (|n| -> print "${n}")
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "list each style callback dispatch");
        assert_eq!(typed.stdout.as_deref(), Some("35"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn case_scrutinee_suspends_and_arm_body_calls_effect() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Double-suspension pattern: the case scrutinee is an effect call (suspends), then
        // the selected arm body is also an effect call (suspends again).
        // Exercises the Out-path `ApplyStep::CaseSelect` continuation (eval_expr path),
        // which is the active path for declarations supported by eval_decl_as_value_with_args_out.
        //
        // flag 0 → resume True; case arm `True -> next 5`; next 5 → resume 6. Output: "6".
        let source = r#"
effect Pred
  flag: Int -> Bool
effect Iter
  next: Int -> Int

choose : Int -> Int
choose n =
  case flag n
    True  -> next 5
    False -> next 0

main : Unit -> Unit
main =
  with
    flag _ -> resume True
    next n -> resume (n + 1)
  in
    print (choose 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed =
            assert_mode_parity(&module, "case scrutinee suspends and arm body calls effect");
        assert_eq!(typed.stdout.as_deref(), Some("6"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn if_condition_suspends_and_branch_body_calls_effect() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Double-suspension pattern: the if condition is an effect call (suspends), then
        // the selected branch body is also an effect call (suspends again).
        // Exercises the Out-path `ApplyStep::IfBranch` continuation (eval_expr path),
        // which is the active path for declarations supported by eval_decl_as_value_with_args_out.
        //
        // flag 0 → resume True; then-branch `next 5`; next 5 → resume 6. Output: "6".
        let source = r#"
effect Pred
  flag: Int -> Bool
effect Iter
  next: Int -> Int

choose : Int -> Int
choose n =
  if flag n
    next 5
  else
    next 0

main : Unit -> Unit
main =
  with
    flag _ -> resume True
    next n -> resume (n + 1)
  in
    print (choose 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(
            &module,
            "if condition suspends and branch body calls effect",
        );
        assert_eq!(typed.stdout.as_deref(), Some("6"));
        assert_eq!(typed.runtime_error_kind, None);
    }
}
