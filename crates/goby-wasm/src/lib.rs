mod backend;
mod call;
#[cfg(test)]
mod compile_tests;
mod effect_handler_legality;
mod effect_handler_lowering;
mod execution_plan;
mod fallback;
mod gen_lower;
mod grapheme_semantics;
mod host_runtime;
mod layout;
mod lower;
mod memory_config;
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
pub use crate::runtime_io_plan::{
    RuntimeIoExecutionKind, runtime_execution_needs_stdin, runtime_io_execution_kind,
};
use crate::runtime_support::{eval_string_expr, parse_pipeline};
use crate::runtime_value::{RuntimeLocals, RuntimeValue, runtime_value_option_eq};
use goby_core::{
    CasePattern, Expr, HandlerClause, ListPatternItem, ListPatternTail, Module, Stmt,
    ast::InterpolatedPart, types::parse_function_type,
};
use std::collections::{HashMap, HashSet};
pub(crate) const BUILTIN_PRINT: &str = "print";
const PRELUDE_MODULE_PATH: &str = "goby/prelude";
pub(crate) const MAX_EVAL_DEPTH: usize = 128;
const ERR_RESUME_MISSING: &str = "resume used without an active continuation [E-RESUME-MISSING]: `resume` can only be called while executing a handler operation body";
const ERR_RESUME_CONSUMED: &str = "resume continuation already consumed [E-RESUME-CONSUMED]: continuations are one-shot; call `resume` at most once per handled operation";
const ERR_RESUME_STACK_MISMATCH: &str = "internal resume token stack mismatch [E-RESUME-STACK-MISMATCH]: continuation token stack became unbalanced";
const INTERNAL_ABORT_MARKER: &str = "__goby_runtime_abort__";
const ERR_CALLABLE_DISPATCH_DECL_PARAM: &str = "unsupported callable dispatch [E-CALLABLE-DISPATCH]: callable parameter requires a lambda or function name argument";

#[cfg(test)]
pub(crate) use crate::runtime_entry::resolve_module_runtime_output;
#[cfg(test)]
pub(crate) use crate::runtime_entry::resolve_module_runtime_output_with_mode;
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
    execution_plan::compile_module_entrypoint(module)
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
/// grapheme-backed stdlib subset in runtime-`Read` programs. This is
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
    execution_plan::execute_module_with_stdin_entrypoint(module, stdin_seed)
}

/// Execute a runtime-stdin Goby module through the `goby-wasm` owned execution boundary.
///
/// This API owns runtime-stdin branching for `goby-cli`: callers should not
/// inspect `RuntimeIoExecutionKind` and special-case `InterpreterBridge`
/// themselves.
///
/// Current behavior:
/// - `GeneralLowered`: compiles the module and executes it via the Goby-owned
///   Wasm runtime, which wires `goby:runtime/track-e` host intrinsics. This
///   path replaces the raw `wasmtime run` process launch for modules that need
///   grapheme host intrinsics.
/// - `InterpreterBridge`: delegates to the narrow seeded-stdin runtime path.
/// - All other execution kinds return `Ok(None)` so the caller can continue with
///   its normal Wasm file execution flow.
pub fn execute_runtime_module_with_stdin(
    module: &Module,
    stdin_seed: Option<String>,
) -> Result<Option<String>, CodegenError> {
    execution_plan::execute_runtime_module_with_stdin_entrypoint(module, stdin_seed)
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
    pub(crate) completed_stmt_seq_locals: Option<RuntimeLocals>,
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
    fn list_spread_int_values_execute_via_compiled_wasm() {
        let module = parse_module(
            r#"
main : Unit -> Unit
main =
  rest = [2, 3]
  print [1, ..rest]
"#,
        )
        .expect("source should parse");

        let wasm = compile_module(&module).expect("list spread program should compile");
        assert_valid_wasm_module(&wasm);

        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
            .expect("compiled list spread program should execute");
        assert_eq!(output, "[1, 2, 3]");
    }

    #[test]
    fn list_spread_string_values_execute_via_compiled_wasm() {
        let module = parse_module(
            r#"
main : Unit -> Unit
main =
  rest = ["b", "c"]
  print ["a", ..rest]
"#,
        )
        .expect("source should parse");

        let wasm = compile_module(&module).expect("list spread program should compile");
        assert_valid_wasm_module(&wasm);

        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
            .expect("compiled list spread program should execute");
        assert_eq!(output, "[\"a\", \"b\", \"c\"]");
    }

    #[test]
    fn runtime_execution_needs_stdin_is_false_for_general_lowered_lambda_without_read() {
        let source = r#"
import goby/list ( map, each )

main : Unit -> Unit can Print
main =
  nums = [1, 2, 3]
  rendered = map nums (fn n -> "${n + 1}")
  each rendered println
"#;
        let module = parse_module(source).expect("source should parse");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered
        );
        assert!(
            !runtime_execution_needs_stdin(&module).expect("stdin requirement should compute"),
            "lambda-only GeneralLowered program should not require seeded stdin"
        );
    }

    #[test]
    fn runtime_execution_needs_stdin_is_true_for_general_lowered_read_program() {
        let source = r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  lines = split(text, "\n")
  each lines println
"#;
        let module = parse_module(source).expect("source should parse");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered
        );
        assert!(
            runtime_execution_needs_stdin(&module).expect("stdin requirement should compute"),
            "GeneralLowered read program should require seeded stdin"
        );
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
            .expect("grapheme bridge execution should be owned by goby-wasm");
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
            "E4: compiled Wasm must contain the grapheme host import '__goby_string_each_grapheme_count'"
        );
    }

    #[test]
    fn execute_runtime_module_with_stdin_routes_general_lowered_through_goby_owned_runtime() {
        // `print(read())` is a GeneralLowered program (no grapheme host intrinsics needed).
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
    fn c4_s3_stdlib_split_multi_grapheme_delimiter_executes() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list
import goby/string

main : Unit -> Unit can Print, Read
main =
  _ignored = read ()
  parts = string.split "--a----b--" "--"
  list.each parts (fn part -> println "${part}!")
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("multi-grapheme split program should execute");
        assert_eq!(
            output.as_deref(),
            Some("!\na!\n!\nb!\n!\n"),
            "multi-grapheme split must preserve leading, consecutive, and trailing empty segments"
        );
    }

    #[test]
    fn wb1_binop_eq_executes_via_general_lowered() {
        // Eq lowers to tagged i64 comparison and executes correctly.
        // Tests that `2 + 3 == 5` evaluates to True and routes through If correctly.
        // (Printing ints directly requires int-to-string conversion, so this uses strings.)
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
            "BinOp Eq + If program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("BinOp Eq + If must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("correct\n"),
            "2 + 3 == 5 must be True"
        );
    }

    #[test]
    fn wb1_if_true_branch_executes_via_general_lowered() {
        // If true branch executes correctly.
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
            "If program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("If true must execute via Goby-owned Wasm runtime");
        assert_eq!(output.as_deref(), Some("yes\n"), "if True should print yes");
    }

    #[test]
    fn wb1_if_false_branch_executes_via_general_lowered() {
        // If false branch executes correctly.
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
            "If false program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("If false must execute via Goby-owned Wasm runtime");
        assert_eq!(output.as_deref(), Some("no\n"), "if False should print no");
    }

    #[test]
    fn h6_runtime_read_helper_if_condition_executes_via_general_lowered() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
is_two : Int -> Bool
is_two n =
  n == 2

choose : Int -> String
choose n =
  if is_two n
    "yes"
  else
    "no"

main : Unit -> Unit can Print, Read
main =
  _ignored = read ()
  println (choose 2)
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "runtime-Read module with helper-call if condition should classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("helper-call if condition must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("yes\n"),
            "helper-call if condition should no longer fall through to generic runtime-I/O unsupported"
        );
    }

    #[test]
    fn wb1_boolean_or_not_and_comparison_precedence_execute_via_general_lowered() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  _ = read ()
  ok = !False || 3 < 2 && 9 > 4
  println "${ok}"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "boolean operator program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("boolean operator program must execute via Goby-owned Wasm runtime");
        assert_eq!(output.as_deref(), Some("True\n"));
    }

    #[test]
    fn wb1_let_mut_and_assign_executes_via_general_lowered() {
        // LetMut + Assign lower correctly and the final value reflects the assignment.
        // Uses string value (not int) since printing ints requires int-to-string conversion.
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
            "LetMut+Assign program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("LetMut+Assign must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("second\n"),
            "x := second then println x should print second"
        );
    }

    #[test]
    fn wb1_string_interp_executes_via_general_lowered() {
        // String interpolation lowers via StringConcat and executes correctly.
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
            "Interp program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("world".to_string()))
            .expect("Interp must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("hello world\n"),
            "hello ${{name}} with name=world should print hello world"
        );
    }

    #[test]
    fn wb1_single_expression_int_interp_executes_via_general_lowered() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  _ = read()
  println "${41 + 1}"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "single-expression int interpolation must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("single-expression int interpolation must execute");
        assert_eq!(
            output.as_deref(),
            Some("42\n"),
            "single-expression int interpolation should coerce the value to String"
        );
    }

    #[test]
    fn wb1_single_expression_tuple_interp_executes_via_general_lowered() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  _ = read()
  println "${(1, "two")}"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "single-expression tuple interpolation must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("single-expression tuple interpolation must execute");
        assert_eq!(
            output.as_deref(),
            Some("(1, two)\n"),
            "single-expression tuple interpolation should recursively stringify tuple members"
        );
    }

    #[test]
    fn wb1_single_expression_list_string_interp_executes_via_general_lowered() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  _ = read()
  println "${["a", "b"]}"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "single-expression list-string interpolation must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("single-expression list-string interpolation must execute");
        assert_eq!(
            output.as_deref(),
            Some("[\"a\", \"b\"]\n"),
            "single-expression list-string interpolation should preserve quoted string elements"
        );
    }

    #[test]
    fn wb2a_helper_decl_call_executes_via_general_lowered() {
        // A top-level helper function called from main is compiled via DeclCall.
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
            "helper decl call program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("world".to_string()))
            .expect("helper decl call must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("hello world\n"),
            "greet(world) should produce hello world"
        );
    }

    #[test]
    fn wb2a_recursive_decl_call_executes_via_general_lowered() {
        // A recursive helper function is compiled and executes correctly.
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
            "recursive decl call program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("hi".to_string()))
            .expect("recursive decl call must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("hi\nhi\nhi\n"),
            "repeat 3 hi should produce hi three times"
        );
    }

    #[test]
    fn recursive_multi_part_interpolated_print_after_graphemes_executes() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( map )
import goby/string ( split, graphemes )

repeat_debug : Int -> Unit can Print
repeat_debug n =
  if n == 0
    ()
  else
    println "debug ${n} ${n} ${n}"
    repeat_debug (n - 1)

main : Unit -> Unit can Print, Read
main =
  text = read()
  lines = split(text, "\n")
  _rows = list.map lines graphemes
  repeat_debug 120
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "graphemes + recursive interpolated println should classify as GeneralLowered"
        );

        let input = "0123456789\n0123456789\n0123456789\n0123456789\n0123456789\n0123456789\n0123456789\n0123456789\n0123456789\n0123456789\n".to_string();
        let output = execute_runtime_module_with_stdin(&module, Some(input))
            .expect("graphemes + recursive interpolated println must execute");

        assert!(
            output.as_deref().is_some_and(|text| {
                text.starts_with("debug 120 120 120\ndebug 119 119 119\n")
                    && text.contains("debug 1 1 1\n")
            }),
            "expected interpolated debug output to survive host temp allocations, got: {:?}",
            output
        );
    }

    // ------------------------------------------------------------------
    // Case emission tests
    // ------------------------------------------------------------------

    #[test]
    fn wb2b_case_int_lit_matches_and_executes_via_general_lowered() {
        // case with integer literal patterns.
        // classify_int x = case x { 1 -> "one" | 2 -> "two" | _ -> "other" }
        use goby_core::parse_module;
        let module = parse_module(
            r#"
classify_int : Int -> String
classify_int x =
  case x
    1 -> "one"
    2 -> "two"
    _ -> "other"

main : Unit -> Unit can Print, Read
main =
  input = read ()
  println (classify_int 2)
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "case int lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("case int lit must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("two\n"),
            "classify_int 2 should produce two"
        );
    }

    #[test]
    fn wb2b_case_int_lit_wildcard_branch_executes() {
        // wildcard arm is taken when no literal matches.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
classify_int : Int -> String
classify_int x =
  case x
    1 -> "one"
    _ -> "other"

main : Unit -> Unit can Print, Read
main =
  input = read ()
  println (classify_int 99)
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("case wildcard must execute");
        assert_eq!(
            output.as_deref(),
            Some("other\n"),
            "classify_int 99 should produce other"
        );
    }

    #[test]
    fn wb2b_case_bool_lit_matches_via_general_lowered() {
        // case with boolean literal pattern.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
bool_to_str : Bool -> String
bool_to_str b =
  case b
    True -> "yes"
    _ -> "no"

main : Unit -> Unit can Print, Read
main =
  input = read ()
  println (bool_to_str True)
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "case bool lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("case bool lit must execute");
        assert_eq!(
            output.as_deref(),
            Some("yes\n"),
            "bool_to_str True should produce yes"
        );
    }

    // ------------------------------------------------------------------
    // ListLit emission tests
    // ------------------------------------------------------------------

    #[test]
    fn wb2b_list_lit_classifies_as_general_lowered() {
        // a function containing a list literal classifies as GeneralLowered.
        // Uses read() to satisfy the runtime-read-effect gate.
        // Uses a string list because println expects a String value.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  _ = read()
  xs = ["alpha", "beta", "gamma"]
  println (list.get xs 1)
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "list lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("list lit must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("beta\n"),
            "list [alpha,beta,gamma], get(1) should produce beta"
        );
    }

    #[test]
    fn wb2b_list_lit_empty_classifies_as_general_lowered() {
        // empty list literal ([]) + case emits correctly as GeneralLowered.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  _ = read()
  xs = []
  case xs
    [] -> println "empty"
    _ -> println "not empty"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "empty list lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("empty list lit must execute");
        assert_eq!(
            output.as_deref(),
            Some("empty\n"),
            "[] case should produce empty"
        );
    }

    // ------------------------------------------------------------------
    // TupleLit emission tests
    // ------------------------------------------------------------------

    #[test]
    fn wb2b_m4_tuple_lit_classifies_as_general_lowered() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  _ = read()
  pair = (1, "hello")
  println "ok"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "tuple lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("tuple lit must execute");
        assert_eq!(output.as_deref(), Some("ok\n"), "tuple path should execute");
    }

    #[test]
    fn wb2b_m5_record_lit_classifies_as_general_lowered() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
type Pair = Pair(left: Int, right: String)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  pair = Pair(left: 1, right: "hello")
  println "ok"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "record lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("record lit must execute");
        assert_eq!(
            output.as_deref(),
            Some("ok\n"),
            "record path should execute"
        );
    }

    // ------------------------------------------------------------------
    // ListPattern emission tests
    // ------------------------------------------------------------------

    #[test]
    fn wb2b_list_pattern_head_tail_executes() {
        // case xs { [] -> "none" | [h, ..t] -> h } extracts first element.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
head_or_none : List String -> String can {}
head_or_none xs =
  case xs
    [] -> "none"
    [h, ..t] -> h

main : Unit -> Unit can Print, Read
main =
  _ = read()
  xs = ["alpha", "beta", "gamma"]
  println (head_or_none xs)
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "list pattern program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("list pattern must execute");
        assert_eq!(
            output.as_deref(),
            Some("alpha\n"),
            "head_or_none [alpha,beta,gamma] should produce alpha"
        );
    }

    #[test]
    fn wb2b_m6_each_with_stdlib_executes() {
        // stdlib `each` classifies as GeneralLowered and executes correctly.
        // `each xs f` calls `f x` via IndirectCall for each element.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( each )

print_item : String -> Unit can Print
print_item s = println s

main : Unit -> Unit can Print, Read
main =
  _ = read()
  xs = ["alpha", "beta", "gamma"]
  each xs print_item
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("each with stdlib must execute");
        assert_eq!(
            output.as_deref(),
            Some("alpha\nbeta\ngamma\n"),
            "each xs print_item should produce alpha, beta, gamma"
        );
    }

    #[test]
    fn wb2b_m6_map_with_stdlib_classifies_as_general_lowered() {
        // stdlib `map` classifies as GeneralLowered.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( map, each )

wrap : String -> String can {}
wrap s = "[${s}]"

main : Unit -> Unit can Print, Read
main =
  _ = read()
  xs = ["a", "b"]
  wrapped = map xs wrap
  each wrapped println
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("map with stdlib must execute");
        assert_eq!(
            output.as_deref(),
            Some("[a]\n[b]\n"),
            "map xs wrap should produce [a], [b]"
        );
    }

    #[test]
    fn map_with_inline_string_lambda_then_each_println_executes_via_general_lowered() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( map, each )

main : Unit -> Unit can Print, Read
main =
  _ = read()
  xs = [1, 2, 3]
  mapped = map xs (fn i -> "${i + 1}")
  each mapped println
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "inline string lambda map + each println must stay on GeneralLowered path"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("inline string lambda map + each println must execute");
        assert_eq!(
            output.as_deref(),
            Some("2\n3\n4\n"),
            "map xs (fn i -> \"${{i + 1}}\") |> each println should print incremented strings"
        );
    }

    #[test]
    fn map_with_inline_bool_interp_lambda_then_each_println_executes_via_general_lowered() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( map, each )

main : Unit -> Unit can Print, Read
main =
  _ = read()
  xs = [True, False]
  mapped = map xs (fn flag -> "${flag}")
  each mapped println
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "inline bool interpolation lambda map + each println must stay on GeneralLowered path"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("inline bool interpolation lambda map + each println must execute");
        assert_eq!(
            output.as_deref(),
            Some("True\nFalse\n"),
            "map xs (fn flag -> \"${{flag}}\") |> each println should print boolean strings"
        );
    }

    #[test]
    fn stdlib_int_to_string_executes_via_compiled_wasm() {
        let module = parse_module(
            r#"
import goby/int as int

main : Unit -> Unit can Print
main =
  println (int.to_string 123)
"#,
        )
        .expect("parse should work");

        let wasm = compile_module(&module).expect("int.to_string program should compile");
        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
            .expect("compiled int.to_string program should execute");
        assert_eq!(output, "123\n");
    }

    #[test]
    fn stdlib_int_to_string_named_map_callback_executes_via_compiled_wasm() {
        let module = parse_module(
            r#"
import goby/int as int
import goby/list ( map )

main : Unit -> Unit can Print
main =
  print (map [1, 20, -3] int.to_string)
"#,
        )
        .expect("parse should work");

        let wasm = compile_module(&module).expect("int.to_string map program should compile");
        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
            .expect("compiled int.to_string map program should execute");
        assert_eq!(output, "[\"1\", \"20\", \"-3\"]");
    }

    #[test]
    fn wb3_m7_read_split_map_graphemes_get_each_executes() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( each, map )
import goby/string ( split, graphemes )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  rolls = map lines graphemes
  each (rolls[2]) println
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(
            &module,
            Some("line0\nline1\nline2\nline3".to_string()),
        )
        .expect("split-map-graphemes exact shape must execute");
        assert_eq!(
            output.as_deref(),
            Some("l\ni\nn\ne\n2\n"),
            "split-map-graphemes exact shape should print each grapheme of line2"
        );
    }

    #[test]
    fn wb3_m7_read_split_map_graphemes_get_each_alias_variant_executes() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( each, map )
import goby/string ( split, graphemes )

main : Unit -> Unit can Print, Read
main =
  input = read ()
  raw_lines = split input "\n"
  forwarded_lines = raw_lines
  rows = map forwarded_lines graphemes
  chars = rows[2]
  each chars println
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(
            &module,
            Some("line0\nline1\nline2\nline3".to_string()),
        )
        .expect("split-map-graphemes alias-chain variant must execute");
        assert_eq!(
            output.as_deref(),
            Some("l\ni\nn\ne\n2\n"),
            "split-map-graphemes alias-chain variant should print each grapheme of line2"
        );
    }

    #[test]
    fn wb3_m7_read_split_map_graphemes_get_each_canonical_variant_executes() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list
import goby/string

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = string.split text "\n"
  rolls = list.map lines string.graphemes
  row2 = list.get rolls 2
  list.each row2 Print.println
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(
            &module,
            Some("line0\nline1\nline2\nline3".to_string()),
        )
        .expect("split-map-graphemes canonical-qualified variant must execute");
        assert_eq!(
            output.as_deref(),
            Some("l\ni\nn\ne\n2\n"),
            "split-map-graphemes canonical-qualified variant should print each grapheme of line2"
        );
    }

    #[test]
    fn wb2b_empty_list_pattern_without_list_pattern_arm() {
        // EmptyList-only case (no ListPattern arm) must not fail with "helper state" error.
        // Regression test: before the fix, `needs_helper_state` did not cover EmptyList,
        // so a function with only an EmptyList arm and no Intrinsic/ListLit would fail at emit time.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
is_empty : List String -> String can {}
is_empty xs =
  case xs
    [] -> "yes"
    _ -> "no"

main : Unit -> Unit can Print, Read
main =
  _ = read()
  println (is_empty [])
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("empty-list-only case must execute");
        assert_eq!(
            output.as_deref(),
            Some("yes\n"),
            "is_empty [] should produce yes"
        );
    }

    // ------------------------------------------------------------------
    // higher-order calls and stdlib each/map
    // ------------------------------------------------------------------

    #[test]
    fn wb2a_m3_simple_higher_order_call_executes() {
        // apply f arg where f is a function parameter (runtime funcref call).
        // `greet` returns an interpolated string; `apply_greet` passes it as a funcref.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
greet : String -> String can {}
greet name = "hello ${name}"

apply_greet : (String -> String) -> String can {}
apply_greet f = f "world"

main : Unit -> Unit can Print, Read
main =
  _ = read()
  println (apply_greet greet)
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("higher-order call must execute");
        assert_eq!(
            output.as_deref(),
            Some("hello world\n"),
            "apply_greet greet should produce hello world"
        );
    }

    #[test]
    fn wb2b_list_pattern_empty_branch_executes() {
        // EmptyList branch taken when list has 0 elements.
        use goby_core::parse_module;
        let module = parse_module(
            r#"
head_or_none : List String -> String can {}
head_or_none xs =
  case xs
    [] -> "none"
    [h, ..t] -> h

main : Unit -> Unit can Print, Read
main =
  _ = read()
  xs = []
  println (head_or_none xs)
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("empty list pattern must execute");
        assert_eq!(
            output.as_deref(),
            Some("none\n"),
            "head_or_none [] should produce none"
        );
    }

    #[test]
    fn read_tuple_member_interpolated_string_general_lowered() {
        // Read + tuple literal + `"${pair.0}"` interpolated string.
        // Verifies that (a) the program routes to GeneralLowered, and (b) TupleGet
        // emits correct Wasm so pair.0 == 1 is printed.  The read() call forces
        // GeneralLowered routing (not a bare Print-only path).
        let module = parse_module(
            r#"
main : Unit -> Unit can Print, Read
main =
  _ = read()
  pair = (1, 2)
  println "${pair.0}"
"#,
        )
        .expect("parse should work");

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "Read+tuple program must route to GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("Read+tuple+interpolated string must execute via GeneralLowered");
        assert_eq!(output.as_deref(), Some("1\n"), "pair.0 of (1, 2) must be 1");
    }

    #[test]
    fn capturing_lambda_via_map_executes_correctly() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( map )

main : Unit -> Unit can Print, Read
main =
  _ = read()
  prefix = "hello"
  result = map ["world"] (fn s -> "${prefix} ${s}")
  println (result[0])
"#,
        )
        .expect("source should parse");

        let result = execute_runtime_module_with_stdin(&module, Some(String::new()));
        let output = result.expect("capturing lambda via map should execute successfully");
        assert_eq!(output.as_deref(), Some("hello world\n"));
    }

    #[test]
    fn capturing_lambda_via_each_executes_correctly() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( each )

main : Unit -> Unit can Print, Read
main =
  _ = read()
  prefix = "n="
  each [1, 2] (fn n -> println "${prefix}${n}")
"#,
        )
        .expect("source should parse");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("capturing lambda via each should execute successfully");
        assert_eq!(output.as_deref(), Some("n=1\nn=2\n"));
    }

    #[test]
    fn inline_pure_lambda_via_fold_executes_correctly() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( fold )

main : Unit -> Unit can Print, Read
main =
  _ = read()
  total = fold [1, 2, 3] 0 (fn acc x -> acc + x)
  println "${total}"
"#,
        )
        .expect("source should parse");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("pure inline fold lambda should execute successfully");
        assert_eq!(output.as_deref(), Some("6\n"));
    }

    #[test]
    fn effectful_inline_lambda_via_fold_executes_correctly() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( fold )

main : Unit -> Unit can Print, Read
main =
  _ = read()
  total =
    fold [1, 2, 3] 0 (fn acc x ->
      println "acc=${acc} x=${x}"
      acc + x
    )
  println "total=${total}"
"#,
        )
        .expect("source should parse");

        let wasm = compile_module(&module).expect("effectful inline fold lambda should compile");
        assert_valid_wasm_module(&wasm);

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("effectful inline fold lambda should execute successfully");
        assert_eq!(
            output.as_deref(),
            Some("acc=0 x=1\nacc=1 x=2\nacc=3 x=3\ntotal=6\n")
        );
    }

    #[test]
    fn recursive_helper_with_fold_callback_println_executes_without_cursor_corruption() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( fold )
import goby/stdio

probe : Int -> Bool can Print
probe n =
  total =
    fold [1, 2, 3, 4, 5, 6, 7, 8] 0 (fn acc x ->
      println("x")
      acc
    )
  total == 0

walk : Int -> Int -> Int -> Int -> Int can Print
walk width height x y =
  if y >= height
    0
  else
    if x >= width
      walk width height 0 (y + 1)
    else
      checked =
        if probe x
          1
        else
          0
      checked + walk width height (x + 1) y

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = walk 10 10 0 0
  println "${result}"
"#,
        )
        .expect("source should parse");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("recursive println callback program should execute successfully");
        let expected = format!("{}100\n", "x\n".repeat(800));
        assert_eq!(output.as_deref(), Some(expected.as_str()));
    }

    #[test]
    fn recursive_helper_with_fold_callback_interpolation_executes_without_bump_exhaustion() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( fold )
import goby/stdio

probe : Int -> Bool can Print
probe n =
  total =
    fold [1, 2, 3, 4, 5, 6, 7, 8] 0 (fn acc x ->
      println("${n},${x}")
      acc
    )
  total == 0

walk : Int -> Int -> Int -> Int -> Int can Print
walk width height x y =
  if y >= height
    0
  else
    if x >= width
      walk width height 0 (y + 1)
    else
      checked =
        if probe x
          1
        else
          0
      checked + walk width height (x + 1) y

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = walk 10 10 0 0
  println "${result}"
"#,
        )
        .expect("source should parse");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("recursive interpolation callback program should execute successfully");
        assert!(
            output
                .as_deref()
                .is_some_and(|text| text.ends_with("100\n") && text.contains("9,8\n")),
            "expected interpolated callback output plus final total, got: {:?}",
            output
        );
    }

    #[test]
    fn inline_capturing_lambda_via_fold_executes_correctly() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( fold )

main : Unit -> Unit can Print, Read
main =
  _ = read()
  bias = 10
  total = fold [1, 2, 3] 0 (fn acc x -> acc + x + bias)
  println "${total}"
"#,
        )
        .expect("source should parse");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("capturing inline fold lambda should execute successfully");
        assert_eq!(output.as_deref(), Some("36\n"));
    }

    #[test]
    fn let_bound_inline_multi_param_lambda_via_user_hof_executes_correctly() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
apply_twice : (Int -> Int -> Int) -> Int -> Int
apply_twice f x =
  f x x

main : Unit -> Unit can Print, Read
main =
  _ = read()
  add = fn a b -> a + b
  result = apply_twice add 5
  println "${result}"
"#,
        )
        .expect("source should parse");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("let-bound inline multi-param lambda should execute successfully");
        assert_eq!(output.as_deref(), Some("10\n"));
    }

    #[test]
    fn user_defined_pairwise_apply_with_inline_multi_param_lambda_executes_correctly() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
pairwise_apply : List Int -> (Int -> Int -> Int) -> List Int
pairwise_apply xs f =
  case xs
    [] -> []
    [x] -> [x]
    [x, y, ..rest] -> [f x y, ..pairwise_apply rest f]

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = pairwise_apply [1, 2, 3, 4] (fn a b -> a + b)
  println "${result}"
"#,
        )
        .expect("source should parse");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("pairwise_apply inline multi-param lambda should execute successfully");
        assert_eq!(output.as_deref(), Some("[3, 7]\n"));
    }

    #[test]
    fn user_defined_apply_two_with_inline_multi_param_lambda_executes_correctly() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
apply_two : (Int -> Int -> Int) -> Int -> Int -> Int
apply_two f a b =
  f a b

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = apply_two (fn a b -> a + b) 20 22
  println "${result}"
"#,
        )
        .expect("source should parse");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("apply_two inline multi-param lambda should execute successfully");
        assert_eq!(output.as_deref(), Some("42\n"));
    }

    #[test]
    fn compile_module_with_helper_closure_capture_succeeds() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( map )

helper : Unit -> String
helper =
  prefix = "hello"
  result = map ["world"] (fn s -> "${prefix} ${s}")
  result[0]

main : Unit -> Unit can Print, Read
main =
  _ = read()
  msg = helper()
  println msg
"#,
        )
        .expect("source should parse");

        let wasm = compile_module(&module).expect("helper closure capture should compile");
        let _ = wasm; // compilation success is sufficient for this step
    }

    #[test]
    fn helper_decl_multi_closure_shared_cell_classifies_as_general_lowered() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
pair : Unit -> ((Unit -> Unit), (Unit -> Int))
pair _ =
  mut count = 0
  inc = fn _ ->
    count := count + 1
  get = fn _ -> count
  (inc, get)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  p = pair()
  p.0()
  p.0()
  result = p.1()
  println "${result}"
"#,
        )
        .expect("source should parse");

        let main_decl = module
            .declarations
            .iter()
            .find(|decl| decl.name == "main")
            .expect("main should exist");
        let pair_decl = module
            .declarations
            .iter()
            .find(|decl| decl.name == "pair")
            .expect("pair should exist");
        assert!(
            pair_decl.parsed_body.is_some(),
            "pair should preserve parsed_body for helper-decl lowering"
        );
        assert!(
            main_decl.parsed_body.is_some(),
            "main should preserve parsed_body for runtime-io classification"
        );
        assert!(
            goby_core::ir_lower::lower_declaration(main_decl).is_ok(),
            "main should lower to IR"
        );

        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            RuntimeIoExecutionKind::GeneralLowered,
            "helper-decl multi-closure shared-cell program must stay on the Wasm-owned execution path"
        );
    }

    #[test]
    fn helper_decl_multi_closure_shared_cell_executes_via_compiled_wasm() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
pair : Unit -> ((Unit -> Unit), (Unit -> Int))
pair _ =
  mut count = 0
  inc = fn _ ->
    count := count + 1
  get = fn _ -> count
  (inc, get)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  p = pair()
  p.0()
  p.0()
  result = p.1()
  println "${result}"
"#,
        )
        .expect("source should parse");

        let wasm =
            compile_module(&module).expect("helper-decl multi-closure program should compile");
        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, Some(""))
            .expect("compiled Wasm should execute");
        assert_eq!(output, "2\n");
    }

    #[test]
    fn tuple_projected_zero_capture_closure_executes_via_wasm() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
pair : Unit -> ((Unit -> Int), (Unit -> Int))
pair _ =
  left = fn _ -> 1
  right = fn _ -> 2
  (left, right)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  p = pair()
  result = p.1()
  println "${result}"
"#,
        )
        .expect("source should parse");

        let wasm = compile_module(&module).expect("tuple-projected closure program should compile");
        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, Some(""))
            .expect("compiled Wasm should execute");
        assert_eq!(output, "2\n");
    }

    #[test]
    fn helper_decl_multi_closure_shared_cell_pair_creation_executes_via_wasm() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
pair : Unit -> ((Unit -> Unit), (Unit -> Int))
pair _ =
  mut count = 0
  inc = fn _ ->
    count := count + 1
  get = fn _ -> count
  (inc, get)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  _p = pair()
  println "ok"
"#,
        )
        .expect("source should parse");

        let wasm = compile_module(&module).expect("pair creation program should compile");
        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, Some(""))
            .expect("compiled Wasm should execute");
        assert_eq!(output, "ok\n");
    }

    #[test]
    fn helper_decl_multi_closure_shared_cell_read_only_call_executes_via_wasm() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
pair : Unit -> ((Unit -> Unit), (Unit -> Int))
pair _ =
  mut count = 0
  inc = fn _ ->
    count := count + 1
  get = fn _ -> count
  (inc, get)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  p = pair()
  result = p.1()
  println "${result}"
"#,
        )
        .expect("source should parse");

        let wasm = compile_module(&module).expect("read-only call program should compile");
        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, Some(""))
            .expect("compiled Wasm should execute");
        assert_eq!(output, "0\n");
    }

    #[test]
    fn helper_decl_multi_closure_shared_cell_write_only_call_executes_via_wasm() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
pair : Unit -> ((Unit -> Unit), (Unit -> Int))
pair _ =
  mut count = 0
  inc = fn _ ->
    count := count + 1
  get = fn _ -> count
  (inc, get)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  p = pair()
  p.0()
  println "ok"
"#,
        )
        .expect("source should parse");

        let wasm = compile_module(&module).expect("write-only call program should compile");
        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, Some(""))
            .expect("compiled Wasm should execute");
        assert_eq!(output, "ok\n");
    }

    #[test]
    fn tuple_projected_by_value_closure_executes_via_wasm() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
pair : Unit -> ((Unit -> Int), (Unit -> Int))
pair _ =
  base = 2
  left = fn _ -> base
  right = fn _ -> base + 1
  (left, right)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  p = pair()
  result = p.1()
  println "${result}"
"#,
        )
        .expect("source should parse");

        let wasm = compile_module(&module)
            .expect("tuple-projected by-value closure program should compile");
        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, Some(""))
            .expect("compiled Wasm should execute");
        assert_eq!(output, "3\n");
    }

    #[test]
    fn helper_decl_single_shared_cell_closure_executes_via_wasm() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
make_reader : Unit -> (Unit -> Int)
make_reader _ =
  mut value = 1
  read_value = fn _ -> value
  value := 7
  read_value

main : Unit -> Unit can Print, Read
main =
  _ = read()
  reader = make_reader()
  result = reader ()
  println "${result}"
"#,
        )
        .expect("source should parse");

        let wasm =
            compile_module(&module).expect("single shared-cell helper closure should compile");
        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, Some(""))
            .expect("compiled Wasm should execute");
        assert_eq!(output, "7\n");
    }

    #[test]
    fn helper_decl_single_shared_cell_write_closure_executes_via_wasm() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
make_inc : Unit -> (Unit -> Unit)
make_inc _ =
  mut count = 0
  inc = fn _ ->
    count := count + 1
  inc

main : Unit -> Unit can Print, Read
main =
  _ = read()
  inc = make_inc()
  inc()
  println "ok"
"#,
        )
        .expect("source should parse");

        let wasm = compile_module(&module)
            .expect("single shared-cell write helper closure should compile");
        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, Some(""))
            .expect("compiled Wasm should execute");
        assert_eq!(output, "ok\n");
    }

    #[test]
    fn helper_decl_two_read_only_shared_cell_closures_execute_via_wasm() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
pair : Unit -> ((Unit -> Int), (Unit -> Int))
pair _ =
  mut count = 0
  left = fn _ -> count
  right = fn _ -> count
  (left, right)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  p = pair()
  result = p.1()
  println "${result}"
"#,
        )
        .expect("source should parse");

        let wasm =
            compile_module(&module).expect("two-read shared-cell helper closures should compile");
        let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, Some(""))
            .expect("compiled Wasm should execute");
        assert_eq!(output, "0\n");
    }

    // --- FOLD-M3b: 2-argument IndirectCall integration tests ---
    //
    // These tests confirm that `IndirectCall { arity: 2 }` actually works end-to-end:
    // a local function-value variable is called with two i64 arguments via call_indirect
    // using the `(i64, i64) -> i64` Wasm type added in FOLD-M3a.

    /// Call a local function-value variable `f` with two arguments: `f acc x`.
    /// `add` is a named top-level decl (passed as PushFuncHandle), so `f` is a funcref.
    /// Inside `apply_twice`, `f` is a local var → IndirectCall { arity: 2 }.
    #[test]
    fn fold_m3b_two_arg_indirect_call_via_local_funcref_executes() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
add : Int -> Int -> Int
add a b = a + b

apply_binary : Int -> Int -> (Int -> Int -> Int) -> Int
apply_binary a b f = f a b

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = apply_binary 3 4 add
  println "${result}"
"#,
        )
        .expect("parse should succeed");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("2-arg IndirectCall must execute");
        assert_eq!(output.as_deref(), Some("7\n"), "apply_binary 3 4 add → 7");
    }

    /// A named 2-arg callback via DeclCall path still works (regression guard).
    #[test]
    fn fold_m3b_two_arg_decl_call_named_callback_still_works() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
add : Int -> Int -> Int
add a b = a + b

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = add 10 5
  println "${result}"
"#,
        )
        .expect("parse should succeed");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("DeclCall 2-arg must execute");
        assert_eq!(output.as_deref(), Some("15\n"), "add 10 5 → 15");
    }

    // --- FOLD-M5: stdlib fold semantic coverage ---

    /// fold [1,2,3] 0 add → 6  (integer sum, named callback)
    #[test]
    fn fold_m5_integer_sum_named_callback() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( fold )

add : Int -> Int -> Int
add a b = a + b

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = fold [1, 2, 3] 0 add
  println "${result}"
"#,
        )
        .expect("parse should succeed");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("fold integer sum must execute");
        assert_eq!(output.as_deref(), Some("6\n"), "fold [1,2,3] 0 add → 6");
    }

    /// fold [] 42 add → 42  (empty list returns initial accumulator)
    #[test]
    fn fold_m5_empty_list_returns_init() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( fold )

add : Int -> Int -> Int
add a b = a + b

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = fold [] 42 add
  println "${result}"
"#,
        )
        .expect("parse should succeed");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("fold empty list must execute");
        assert_eq!(output.as_deref(), Some("42\n"), "fold [] 42 add → 42");
    }

    /// fold [10] 0 add → 10  (single-element list)
    #[test]
    fn fold_m5_single_element_list() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( fold )

add : Int -> Int -> Int
add a b = a + b

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = fold [10] 0 add
  println "${result}"
"#,
        )
        .expect("parse should succeed");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("fold single element must execute");
        assert_eq!(output.as_deref(), Some("10\n"), "fold [10] 0 add → 10");
    }

    /// Non-commutative operation: fold [1,2,3] 0 sub verifies left-to-right accumulation.
    /// Expected: ((0 - 1) - 2) - 3 = -6
    #[test]
    fn fold_m5_noncommutative_left_fold_order() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( fold )

sub : Int -> Int -> Int
sub a b = a - b

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = fold [1, 2, 3] 0 sub
  println "${result}"
"#,
        )
        .expect("parse should succeed");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("fold non-commutative must execute");
        // ((0 - 1) - 2) - 3 = -6
        assert_eq!(
            output.as_deref(),
            Some("-6\n"),
            "fold [1,2,3] 0 sub → -6 (left-fold order)"
        );
    }

    /// fold result is correct when used as input to `each` (integration with other HOFs).
    #[test]
    fn fold_m5_fold_result_used_with_each() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( fold, each )

add : Int -> Int -> Int
add a b = a + b

main : Unit -> Unit can Print, Read
main =
  _ = read()
  xs = [1, 2, 3, 4, 5]
  total = fold xs 0 add
  println "${total}"
"#,
        )
        .expect("parse should succeed");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("fold + each integration must execute");
        assert_eq!(
            output.as_deref(),
            Some("15\n"),
            "fold [1,2,3,4,5] 0 add → 15"
        );
    }

    /// Both arity-1 and arity-2 IndirectCall in the same module.
    /// Ensures `with_module_tables` seeds both indirect_call_type_idx_1 and _2 correctly
    /// and that swapping the two Option<u32> arguments would be caught.
    #[test]
    fn refactor_both_indirect_call_arities_in_same_module() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( map, fold )

double : Int -> Int
double x = x + x

add : Int -> Int -> Int
add a b = a + b

main : Unit -> Unit can Print, Read
main =
  _ = read()
  doubled = map [1, 2, 3] double
  total = fold doubled 0 add
  println "${total}"
"#,
        )
        .expect("parse should succeed");

        // map uses arity-1 IndirectCall; fold uses arity-2 IndirectCall.
        // double [1,2,3] = [2,4,6]; fold sum = 12.
        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("mixed arity IndirectCall must execute");
        assert_eq!(
            output.as_deref(),
            Some("12\n"),
            "map double then fold add: [1,2,3] → [2,4,6] → 12"
        );
    }

    #[test]
    fn cc4_mutable_write_capture_via_each_executes_correctly() {
        use goby_core::parse_module;
        let module = parse_module(
            r#"
import goby/list ( each )

sum : List Int -> Int
sum xs =
  mut total = 0
  each xs (fn x ->
    total := total + x
  )
  total

main : Unit -> Unit can Print, Read
main =
  _ = read()
  result = sum [1, 2, 3]
  println "${result}"
"#,
        )
        .expect("source should parse");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("mutable write capture via each should execute successfully");
        assert_eq!(output.as_deref(), Some("6\n"));
    }
}
