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
pub use crate::runtime_io_plan::{RuntimeIoExecutionKind, runtime_io_execution_kind};
use crate::runtime_support::{eval_string_expr, parse_pipeline};
use crate::runtime_value::{RuntimeLocals, RuntimeValue, runtime_value_option_eq};
use goby_core::{
    CasePattern, Expr, HandlerClause, ListPatternItem, ListPatternTail, Module, Stmt,
    ast::InterpolatedPart, types::parse_function_type,
};
use std::collections::{HashMap, HashSet};
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
    execution_plan::execute_module_with_stdin_entrypoint(module, stdin_seed)
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
  list.each parts (|part| -> println "${part}!")
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("C4-S3 multi-grapheme split program should execute");
        assert_eq!(
            output.as_deref(),
            Some("!\na!\n!\nb!\n!\n"),
            "C4-S3 multi-grapheme split must preserve leading, consecutive, and trailing empty segments"
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

    // ------------------------------------------------------------------
    // WB-2B: Case emission tests
    // ------------------------------------------------------------------

    #[test]
    fn wb2b_case_int_lit_matches_and_executes_via_general_lowered() {
        // WB-2B-M1: case with integer literal patterns.
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
            "WB-2B: case int lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("WB-2B: case int lit must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("two\n"),
            "WB-2B: classify_int 2 → two"
        );
    }

    #[test]
    fn wb2b_case_int_lit_wildcard_branch_executes() {
        // WB-2B-M1: wildcard arm is taken when no literal matches.
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
            .expect("WB-2B: case wildcard must execute");
        assert_eq!(
            output.as_deref(),
            Some("other\n"),
            "WB-2B: classify_int 99 → other"
        );
    }

    #[test]
    fn wb2b_case_bool_lit_matches_via_general_lowered() {
        // WB-2B-M1: case with boolean literal pattern.
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
            "WB-2B: case bool lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some("ignored".to_string()))
            .expect("WB-2B: case bool lit must execute");
        assert_eq!(
            output.as_deref(),
            Some("yes\n"),
            "WB-2B: bool_to_str True → yes"
        );
    }

    // ------------------------------------------------------------------
    // WB-2B-M3: ListLit emission tests
    // ------------------------------------------------------------------

    #[test]
    fn wb2b_list_lit_classifies_as_general_lowered() {
        // WB-2B-M3: a function containing a list literal classifies as GeneralLowered.
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
            "WB-2B-M3: list lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("WB-2B-M3: list lit must execute via Goby-owned Wasm runtime");
        assert_eq!(
            output.as_deref(),
            Some("beta\n"),
            "WB-2B-M3: list [alpha,beta,gamma], get(1) → beta"
        );
    }

    #[test]
    fn wb2b_list_lit_empty_classifies_as_general_lowered() {
        // WB-2B-M3: empty list literal ([]) + case emits correctly as GeneralLowered.
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
            "WB-2B-M3: empty list lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("WB-2B-M3: empty list lit must execute");
        assert_eq!(
            output.as_deref(),
            Some("empty\n"),
            "WB-2B-M3: [] case → empty"
        );
    }

    // ------------------------------------------------------------------
    // WB-2B-M4: TupleLit emission tests
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
            "WB-2B-M4: tuple lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("WB-2B-M4: tuple lit must execute");
        assert_eq!(
            output.as_deref(),
            Some("ok\n"),
            "WB-2B-M4: tuple path should execute"
        );
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
            "WB-2B-M5: record lit program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("WB-2B-M5: record lit must execute");
        assert_eq!(
            output.as_deref(),
            Some("ok\n"),
            "WB-2B-M5: record path should execute"
        );
    }

    // ------------------------------------------------------------------
    // WB-2B-M2: ListPattern emission tests
    // ------------------------------------------------------------------

    #[test]
    fn wb2b_list_pattern_head_tail_executes() {
        // WB-2B-M2: case xs { [] -> "none" | [h, ..t] -> h } extracts first element.
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
            "WB-2B-M2: list pattern program must classify as GeneralLowered"
        );

        let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
            .expect("WB-2B-M2: list pattern must execute");
        assert_eq!(
            output.as_deref(),
            Some("alpha\n"),
            "WB-2B-M2: head_or_none [alpha,beta,gamma] → alpha"
        );
    }

    #[test]
    fn wb2b_m6_each_with_stdlib_executes() {
        // WB-2B-M6: stdlib `each` classifies as GeneralLowered and executes correctly.
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
            .expect("WB-2B-M6: each with stdlib must execute");
        assert_eq!(
            output.as_deref(),
            Some("alpha\nbeta\ngamma\n"),
            "WB-2B-M6: each xs print_item → alpha, beta, gamma"
        );
    }

    #[test]
    fn wb2b_m6_map_with_stdlib_classifies_as_general_lowered() {
        // WB-2B-M6: stdlib `map` classifies as GeneralLowered.
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
            .expect("WB-2B-M6: map with stdlib must execute");
        assert_eq!(
            output.as_deref(),
            Some("[a]\n[b]\n"),
            "WB-2B-M6: map xs wrap → [a], [b]"
        );
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
  row2 = rolls[2]
  each row2 println
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(
            &module,
            Some("line0\nline1\nline2\nline3".to_string()),
        )
        .expect("WB-3-M7 exact shape must execute");
        assert_eq!(
            output.as_deref(),
            Some("l\ni\nn\ne\n2\n"),
            "WB-3-M7 exact shape should print each grapheme of line2"
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
  selected = rows[2]
  chars = selected
  each chars println
"#,
        )
        .expect("parse should work");

        let output = execute_runtime_module_with_stdin(
            &module,
            Some("line0\nline1\nline2\nline3".to_string()),
        )
        .expect("WB-3-M7 alias-chain variant must execute");
        assert_eq!(
            output.as_deref(),
            Some("l\ni\nn\ne\n2\n"),
            "WB-3-M7 alias-chain variant should print each grapheme of line2"
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
        .expect("WB-3-M7 canonical-qualified variant must execute");
        assert_eq!(
            output.as_deref(),
            Some("l\ni\nn\ne\n2\n"),
            "WB-3-M7 canonical-qualified variant should print each grapheme of line2"
        );
    }

    #[test]
    fn wb2b_empty_list_pattern_without_list_pattern_arm() {
        // WB-2B: EmptyList-only case (no ListPattern arm) must not fail with "helper state" error.
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
            .expect("WB-2B: empty-list-only case must execute");
        assert_eq!(output.as_deref(), Some("yes\n"), "WB-2B: is_empty [] → yes");
    }

    // ------------------------------------------------------------------
    // WB-2A-M3 / WB-2B-M6: higher-order calls and stdlib each/map
    // ------------------------------------------------------------------

    #[test]
    fn wb2a_m3_simple_higher_order_call_executes() {
        // WB-2A-M3: apply f arg where f is a function parameter (runtime funcref call).
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
            .expect("WB-2A-M3: higher-order call must execute");
        assert_eq!(
            output.as_deref(),
            Some("hello world\n"),
            "WB-2A-M3: apply_greet greet → hello world"
        );
    }

    #[test]
    fn wb2b_list_pattern_empty_branch_executes() {
        // WB-2B-M2: EmptyList branch taken when list has 0 elements.
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
            .expect("WB-2B-M2: empty list pattern must execute");
        assert_eq!(
            output.as_deref(),
            Some("none\n"),
            "WB-2B-M2: head_or_none [] → none"
        );
    }
}
