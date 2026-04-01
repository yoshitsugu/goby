use std::path::PathBuf;
use std::sync::Mutex;

use goby_core::parse_module;
use goby_wasm::{
    RuntimeIoExecutionKind, compile_module, execute_module_with_stdin,
    execute_runtime_module_with_stdin, runtime_io_execution_kind,
};
use wasmparser::Validator;

/// Serializes tests that read or write process-wide environment variables.
static ENV_MUTEX: Mutex<()> = Mutex::new(());

fn assert_valid_wasm_module(wasm: &[u8]) {
    assert!(wasm.len() >= 8, "module too short: {} bytes", wasm.len());
    assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d], "bad wasm magic");
    assert_eq!(&wasm[4..8], &[0x01, 0x00, 0x00, 0x00], "bad wasm version");
    Validator::new()
        .validate_all(wasm)
        .expect("module should pass wasm validation");
}

fn read_example(name: &str) -> String {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("..");
    path.push("..");
    path.push("examples");
    path.push(name);
    std::fs::read_to_string(path).expect("example file should exist")
}

/// Return the payload bytes of the first Wasm section with the given id, or `None`.
/// Parses LEB128-encoded section sizes; returns `None` on malformed input.
fn find_wasm_section(wasm: &[u8], target_id: u8) -> Option<&[u8]> {
    let mut pos = 8usize; // skip magic (4) + version (4)
    while pos < wasm.len() {
        let sec_id = *wasm.get(pos)?;
        pos += 1;
        let mut size: u32 = 0;
        let mut shift = 0u32;
        loop {
            let b = *wasm.get(pos)?;
            pos += 1;
            size |= ((b & 0x7f) as u32) << shift;
            shift += 7;
            if b & 0x80 == 0 {
                break;
            }
            if shift >= 35 {
                return None;
            }
        }
        let end = pos.checked_add(size as usize)?;
        if end > wasm.len() {
            return None;
        }
        if sec_id == target_id {
            return Some(&wasm[pos..end]);
        }
        pos = end;
    }
    None
}

fn assert_has_fd_read_and_fd_write_imports(wasm: &[u8]) {
    let import_section = find_wasm_section(wasm, 0x02).expect("import section must exist");
    assert!(
        import_section
            .windows(b"\x07fd_read".len())
            .any(|w| w == b"\x07fd_read"),
        "expected fd_read import in import section"
    );
    assert!(
        import_section
            .windows(b"\x08fd_write".len())
            .any(|w| w == b"\x08fd_write"),
        "expected fd_write import in import section"
    );
}

#[test]
fn exports_start_entrypoint() {
    let src = "main : Unit -> Unit\nmain = print \"Hello\"\n";
    let module = parse_module(src).expect("parse");
    let wasm = compile_module(&module).expect("compile");
    assert_valid_wasm_module(&wasm);
    let export_section = find_wasm_section(&wasm, 0x07).expect("export section must exist");
    let needle_start: &[u8] = b"\x06_start";
    let needle_main: &[u8] = b"\x04main";
    assert!(
        export_section
            .windows(needle_start.len())
            .any(|w| w == needle_start),
        "expected `_start` export in export section"
    );
    assert!(
        !export_section
            .windows(needle_main.len())
            .any(|w| w == needle_main),
        "found stale `main` export in export section"
    );
}

#[test]
fn unsupported_main_body_returns_codegen_error() {
    let module = parse_module("main : Unit -> Unit\nmain = 0\n").expect("parse should work");
    assert!(
        compile_module(&module).is_err(),
        "main body with unsupported constructs should return CodegenError"
    );
}

#[test]
fn effect_boundary_with_unresolvable_fallback_reports_boundary_context() {
    let module =
        parse_module("main : Unit -> Unit can Print\nmain = 0\n").expect("parse should work");
    let err = compile_module(&module).expect_err("compile should report boundary fallback failure");
    assert!(
        err.message.contains("effect boundary"),
        "error should surface effect-boundary context, got: {}",
        err.message
    );
}

#[test]
fn read_split_each_println_program_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  lines = split(text, "\n")
  each lines (fn line -> println(line))
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("split each println shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn simple_read_echo_program_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  print (read())
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("simple read echo should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn println_read_program_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  println (read())
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("println read should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn binding_read_then_print_program_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  print text
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("binding read print should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn forwarded_read_binding_then_print_program_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  copied = text
  print copied
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("forwarded read print should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn bound_newline_split_each_println_program_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  each lines (fn line -> println(line))
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("bound newline split shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn split_lines_each_with_delimiter_alias_chain_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  newline = "\n"
  delim = newline
  lines = split(text, delim)
  each lines println
"#,
    )
    .expect("parse should work");
    let wasm =
        compile_module(&module).expect("delimiter alias-chain split shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn forwarded_split_lines_each_println_program_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  copied = lines
  each copied (fn line -> println(line))
"#,
    )
    .expect("parse should work");
    let wasm =
        compile_module(&module).expect("forwarded split each println shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn split_lines_each_named_println_callback_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  each lines println
"#,
    )
    .expect("parse should work");
    let wasm =
        compile_module(&module).expect("named println callback split shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn split_lines_each_named_print_callback_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  each lines print
"#,
    )
    .expect("parse should work");
    let wasm =
        compile_module(&module).expect("named print callback split shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn split_lines_each_interpolated_passthrough_println_callback_emits_wasm_with_fd_read_and_fd_write_imports()
 {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  each lines (fn line -> println "${line}")
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module)
        .expect("interpolated passthrough println split shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn split_lines_each_local_println_callback_alias_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  printer = println
  each lines printer
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module)
        .expect("local println callback alias split shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn split_lines_each_forwarded_local_print_callback_alias_emits_wasm_with_fd_read_and_fd_write_imports()
 {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  printer = print
  writer = printer
  each lines writer
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module)
        .expect("forwarded local print callback alias split shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn split_lines_each_lambda_calling_local_println_alias_emits_wasm_with_fd_read_and_fd_write_imports()
 {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  printer = println
  each lines (fn line -> printer line)
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module)
        .expect("lambda-calling local println alias split shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn split_lines_each_lambda_calling_forwarded_local_print_alias_emits_wasm_with_fd_read_and_fd_write_imports()
 {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  printer = print
  writer = printer
  each lines (fn line -> writer line)
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module)
        .expect("lambda-calling forwarded local print alias split shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn split_lines_each_with_callback_alias_before_split_emits_wasm_with_fd_read_and_fd_write_imports()
{
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  printer = println
  delim = "\n"
  lines = split(text, delim)
  each lines printer
"#,
    )
    .expect("parse should work");
    let wasm =
        compile_module(&module).expect("callback-alias-before-split shape should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn split_lines_each_with_static_print_suffixes_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  lines = split(text, "\n")
  each lines (fn line -> println(line))
  println "test"
  print "done"
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module)
        .expect("split each with static print suffixes should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn simple_read_line_program_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  print (read_line())
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("simple read_line should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn binding_read_line_then_println_program_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  line = read_line()
  println line
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("binding read_line println should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn forwarded_read_line_binding_then_println_program_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  line = read_line()
  copied = line
  println copied
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("forwarded read_line println should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn read_line_echo_with_static_suffixes_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  line = read_line()
  println line
  print "done"
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module)
        .expect("read_line echo with static suffixes should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn read_echo_with_output_alias_emits_wasm_with_fd_read_and_fd_write_imports() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  printer = print
  copied = text
  printer copied
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("read echo with output alias should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn compile_module_produces_wasm_for_transformed_split_callback() {
    // This shape used to miss the general-lowered path.
    // It now classifies as GeneralLowered because lambda lowering handles `fn line -> println "${line}!"`.
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  copied = lines
  forwarded = copied
  each forwarded (fn line -> println "${line}!")
"#,
    )
    .expect("parse should work");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should work"),
        RuntimeIoExecutionKind::GeneralLowered,
        "transformed split callback should now classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("should compile to Wasm");
    assert_valid_wasm_module(&wasm);
    assert_has_fd_read_and_fd_write_imports(&wasm);
}

#[test]
fn runtime_io_execution_kind_reports_general_lowered_for_transformed_split_callback() {
    let module = parse_module(
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  copied = lines
  forwarded = copied
  each forwarded (fn line -> println "${line}!")
"#,
    )
    .expect("parse should work");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should work"),
        RuntimeIoExecutionKind::GeneralLowered
    );
}

#[test]
fn runtime_io_execution_kind_reports_general_lowered_for_qualified_read_print() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  text = Read.read ()
  Print.print text
"#,
    )
    .expect("parse should work");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should work"),
        RuntimeIoExecutionKind::GeneralLowered
    );
}

#[test]
fn runtime_io_execution_kind_reports_general_lowered_for_interp_read_transform() {
    // Interp lowering is now supported via StringConcat.
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  decorated = "${text}!"
  print decorated
"#,
    )
    .expect("parse should work");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should work"),
        RuntimeIoExecutionKind::GeneralLowered
    );
}

#[test]
fn runtime_io_execution_kind_reports_general_lowered_for_read_graphemes_program() {
    // E6: graphemes + index pattern now lowers through fused graphemes-index path
    // to __goby_string_each_grapheme_state, so classification is GeneralLowered.
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
        runtime_io_execution_kind(&module).expect("classification should work"),
        RuntimeIoExecutionKind::GeneralLowered
    );
}

#[test]
fn execute_runtime_module_with_stdin_runs_general_lowered_graphemes_program() {
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
        .expect("general lowered path should execute graphemes program");
    assert_eq!(
        output.as_deref(),
        Some("👨\u{200d}👩\u{200d}👧\u{200d}👦\n")
    );
}

#[test]
fn execute_module_with_stdin_rejects_dynamic_wasi_io_program() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  print (read())
"#,
    )
    .expect("parse should work");
    let err = execute_module_with_stdin(&module, Some("hello".to_string()))
        .expect_err("dynamic wasi io program should not use interpreter bridge path");
    assert!(
        err.message.contains("compile this program to Wasm instead"),
        "unexpected error message: {}",
        err.message
    );
}

#[test]
fn execute_module_with_stdin_rejects_unsupported_runtime_io_program() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  decorated = "${text}!"
  print decorated
"#,
    )
    .expect("parse should work");
    let err = execute_module_with_stdin(&module, Some("hello".to_string()))
        .expect_err("unsupported runtime I/O program should not use interpreter bridge path");
    assert!(
        err.message.contains("unsupported"),
        "unexpected error message: {}",
        err.message
    );
}

#[test]
fn runtime_override_force_portable_is_reflected_in_boundary_diagnostics() {
    let _guard = ENV_MUTEX.lock().expect("env mutex lock");
    // SAFETY: guarded by ENV_MUTEX in this test module.
    unsafe { std::env::set_var("GOBY_WASM_FORCE_PORTABLE_FALLBACK", "1") };
    let module =
        parse_module("main : Unit -> Unit can Print\nmain = 0\n").expect("parse should work");
    let err = compile_module(&module).expect_err("compile should report boundary fallback failure");
    assert!(
        err.message
            .contains("selected_mode_fallback_reason=Some(ForcedPortableOverride)"),
        "error should expose forced fallback override reason, got: {}",
        err.message
    );
    // SAFETY: guarded by ENV_MUTEX in this test module.
    unsafe { std::env::remove_var("GOBY_WASM_FORCE_PORTABLE_FALLBACK") };
}

#[test]
fn compile_module_reports_backend_limitation_for_non_tail_resume_handler() {
    let module = parse_module(
        r#"
effect Tick
  tick: Unit -> Unit

main : Unit -> Unit can Tick
main =
  with
    tick _ ->
      resume ()
      ()
  in
    tick ()
"#,
    )
    .expect("parse should work");
    let err = compile_module(&module).expect_err("compile should reject unsupported handler");
    assert!(
        err.message.contains("E-BACKEND-LIMITATION"),
        "unexpected error message: {}",
        err.message
    );
    assert!(
        err.message.contains("tail"),
        "unexpected error message: {}",
        err.message
    );
}

#[test]
fn compile_module_accepts_safe_tail_resume_handler() {
    let module = parse_module(
        r#"
effect Tick
  tick: Unit -> Unit

main : Unit -> Unit can Tick
main =
  with
    tick _ ->
      resume ()
  in
    tick ()
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("compile should succeed for safe tail handler");
    assert_valid_wasm_module(&wasm);
}

#[test]
#[ignore = "qualified iterator handler clauses are not yet covered by general-lowered smoke examples"]
fn iterator_example_classifies_as_general_lowered() {
    let source = read_example("iterator.gb");
    let module = parse_module(&source).expect("iterator.gb should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should work"),
        RuntimeIoExecutionKind::GeneralLowered
    );
}

#[test]
#[ignore = "qualified iterator handler clauses are not yet covered by general-lowered smoke examples"]
fn iterator_example_executes_via_general_lowered_wasm_path() {
    let source = read_example("iterator.gb");
    let module = parse_module(&source).expect("iterator.gb should parse");
    let output = goby_wasm::execute_runtime_module_with_stdin(&module, None)
        .expect("iterator should execute")
        .expect("iterator should run via Wasm-owned path");
    assert_eq!(output, "tick:atick:btick:c");
}

#[test]
fn emits_valid_wasm_for_print_literal() {
    let module =
        parse_module("main : Unit -> Unit\nmain = print \"Hello Goby!\"\n").expect("parse");
    let wasm = compile_module(&module).expect("codegen");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn emits_valid_wasm_for_function_example() {
    let source = read_example("function.gb");
    let module = parse_module(&source).expect("parse");
    let wasm = compile_module(&module).expect("codegen");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn execute_module_with_stdin_rejects_static_output_program() {
    // StaticOutput programs compile directly to Wasm and do not go through the
    // interpreter bridge; execute_module_with_stdin must reject them.
    let module = parse_module(
        r#"
main : Unit -> Unit can Print
main =
  println "hello"
"#,
    )
    .expect("parse should work");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        goby_wasm::RuntimeIoExecutionKind::StaticOutput,
        "program should be classified as StaticOutput"
    );
    let err = execute_module_with_stdin(&module, None)
        .expect_err("static output program should not use interpreter bridge path");
    assert!(
        err.message.contains("static output"),
        "unexpected error message: {}",
        err.message
    );
}

// ─── Lambda Lowering Tests ────────────────────────────────────────────────────

/// `map [1,2,3] (fn x -> x + 1)` classifies as GeneralLowered and compiles.
#[test]
fn wb3_m3_map_with_lambda_classifies_as_general_lowered() {
    let source = r#"
import goby/list ( map )

main : Unit -> Unit can Print, Read
main =
  text = read()
  nums = [1, 2, 3]
  doubled = map nums (fn x -> x + 1)
  println text
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "map with lambda should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("map with lambda should compile to Wasm");
    assert_valid_wasm_module(&wasm);
}

/// Two lambdas in the same program both get unique AuxDecl entries.
#[test]
fn wb3_m3_two_lambdas_in_same_program_compile() {
    let source = r#"
import goby/list ( map, each )

main : Unit -> Unit can Print, Read
main =
  text = read()
  nums = [1, 2, 3]
  doubled = map nums (fn x -> x + 1)
  tripled = map nums (fn x -> x + 2)
  println text
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "program with two lambdas should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("two-lambda program should compile to Wasm");
    assert_valid_wasm_module(&wasm);
}

/// Lambda with Print effect in body (`each lines (fn line -> println line)`).
#[test]
fn wb3_m3_each_with_lambda_body_having_effect_executes_correctly() {
    let source = r#"
import goby/list ( each )

main : Unit -> Unit can Print, Read
main =
  text = read()
  lines = [text]
  each lines (fn line -> println line)
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "each with lambda effect body should classify as GeneralLowered"
    );
    let output = execute_runtime_module_with_stdin(&module, Some("hello".to_string()))
        .expect("execution should not error")
        .expect("execution should produce output");
    assert_eq!(output, "hello\n");
}

/// A lambda body referencing a free variable now stays on the general-lowered path.
///
/// `base` is defined in the enclosing scope but is not the lambda param (`x`),
/// so this program exercises ByValue capture classification. Runtime dispatch for
/// capturing list callbacks is still incomplete, but classification now succeeds.
#[test]
fn wb3_m3_lambda_with_by_value_capture_classifies_as_general_lowered() {
    let source = r#"
import goby/list ( map )

main : Unit -> Unit can Print, Read
main =
  text = read()
  base = 10
  nums = [1, 2, 3]
  result = map nums (fn x -> x + base)
  println text
"#;
    let module = parse_module(source).expect("source should parse");
    let kind = runtime_io_execution_kind(&module).expect("classification should succeed");
    assert_eq!(
        kind,
        RuntimeIoExecutionKind::GeneralLowered,
        "ByValue-capturing lambda should classify as GeneralLowered"
    );
}

// ---------------------------------------------------------------------------
// Graphemes GeneralLowered End-to-End
// ---------------------------------------------------------------------------

#[test]
fn wb3_m4_graphemes_each_classifies_as_general_lowered() {
    // Uses `parts = graphemes text; each parts println` to get a List String and iterate it.
    // Avoids the fused graphemes-get-print (index-access) pattern.
    // Requires StringGraphemesList intrinsic to lower the graphemes call.
    let source = r#"
import goby/string ( graphemes )
import goby/list ( each )

main : Unit -> Unit can Print, Read
main =
  text = read()
  parts = graphemes text
  each parts println
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "graphemes + each should classify as GeneralLowered end-to-end"
    );
}

#[test]
fn wb3_m4_graphemes_each_executes_correctly() {
    // Execution test: emoji-family input "a👨‍👩‍👧‍👦b" should print each grapheme cluster on its own line.
    let source = r#"
import goby/string ( graphemes )
import goby/list ( each )

main : Unit -> Unit can Print, Read
main =
  text = read()
  parts = graphemes text
  each parts println
"#;
    let module = parse_module(source).expect("source should parse");
    // Confirm classification first.
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "wb3_m4 execution test: program must be GeneralLowered"
    );
    let output = execute_runtime_module_with_stdin(&module, Some("a👨‍👩‍👧‍👦b".to_string()))
        .expect("execution should succeed")
        .expect("output should be Some");
    // "a👨‍👩‍👧‍👦b" has 3 extended grapheme clusters: "a", "👨‍👩‍👧‍👦", "b"
    assert_eq!(output, "a\n👨\u{200d}👩\u{200d}👧\u{200d}👦\nb\n");
}

/// Full split-map-graphemes integration test.
/// `map lines graphemes` requires passing `graphemes` as a function value to `map`.
/// The list index in `each (rolls[2]) println` is now normalized through shared-IR ANF.
#[test]
fn wb3_m7_split_map_graphemes_executes_correctly() {
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
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "wb3_m7 program must be GeneralLowered"
    );
    // stdin: "line0\nline1\nline2\nline3"
    // lines = ["line0","line1","line2","line3"]
    // rolls[2] = graphemes "line2" = ["l","i","n","e","2"]
    let output =
        execute_runtime_module_with_stdin(&module, Some("line0\nline1\nline2\nline3".to_string()))
            .expect("execution should succeed")
            .expect("output should be Some");
    assert_eq!(output, "l\ni\nn\ne\n2\n");
}

// stdlib split behavior hardening tests.
// Each test asserts GeneralLowered classification first, then verifies runtime output.
// Output format: `each parts println` prints one element per line with trailing `\n`.

// ── Diagnostic: verify split_with_single_delimiter works directly ────────────

#[test]
fn c4_s4_split_single_delimiter_comma_executes() {
    // split "a,b,c" "," → ["a","b","c"] via single-grapheme delimiter path
    let source = r#"
import goby/string ( split )
import goby/list ( each )

main : Unit -> Unit can Print, Read
main =
  text = read()
  parts = split text ","
  each parts println
"#;
    let module = parse_module(source).expect("parse should succeed");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "split with single-char delimiter must classify as GeneralLowered"
    );
    let output = execute_runtime_module_with_stdin(&module, Some("a,b,c".to_string()))
        .expect("single-delimiter execution should succeed")
        .expect("output should be Some");
    assert_eq!(output, "a\nb\nc\n");
}

#[test]
fn c4_s4_split_single_delimiter_leading_trailing_preserves_empty_segments() {
    // leading/trailing delimiters should preserve empty segments
    // split ",a,b," "," → ["","a","b",""]  → println each → "\na\nb\n\n"
    let source = r#"
import goby/string ( split )
import goby/list ( each )

main : Unit -> Unit can Print, Read
main =
  text = read()
  parts = split text ","
  each parts println
"#;
    let module = parse_module(source).expect("parse should succeed");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "split with leading/trailing empty segments must classify as GeneralLowered"
    );
    let output = execute_runtime_module_with_stdin(&module, Some(",a,b,".to_string()))
        .expect("leading/trailing empty segments execution should succeed")
        .expect("output should be Some");
    assert_eq!(
        output, "\na\nb\n\n",
        "split ',a,b,' ',' must preserve leading/trailing empty segments"
    );
}

#[test]
fn c4_s4_graphemes_directly_executes() {
    // graphemes text works as expected (baseline for split "" equivalence)
    let source = r#"
import goby/string ( graphemes )
import goby/list ( each )

main : Unit -> Unit can Print, Read
main =
  text = read()
  parts = graphemes text
  each parts println
"#;
    let module = parse_module(source).expect("parse should succeed");
    let output = execute_runtime_module_with_stdin(&module, Some("abc".to_string()))
        .expect("graphemes execution should succeed")
        .expect("output should be Some");
    assert_eq!(output, "a\nb\nc\n");
}

// ── Step 1: empty-delimiter ──────────────────────────────────────────────────

#[test]
fn c4_s4_split_empty_delimiter_ascii_executes_correctly() {
    // split "abc" "" → grapheme-wise split → ["a","b","c"]
    let source = r#"
import goby/string ( split )
import goby/list ( each )

main : Unit -> Unit can Print, Read
main =
  text = read()
  parts = split text ""
  each parts println
"#;
    let module = parse_module(source).expect("parse should succeed");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "c4_s4 empty-delimiter program must be GeneralLowered"
    );
    let output = execute_runtime_module_with_stdin(&module, Some("abc".to_string()))
        .expect("execution should succeed")
        .expect("output should be Some");
    assert_eq!(
        output, "a\nb\nc\n",
        "split 'abc' '' should yield each grapheme on its own line"
    );
}

#[test]
fn c4_s4_split_empty_delimiter_emoji_executes_correctly() {
    // split with empty delimiter on input containing a multi-codepoint emoji ZWJ sequence
    // must yield grapheme clusters, not individual bytes/codepoints
    let source = r#"
import goby/string ( split )
import goby/list ( each )

main : Unit -> Unit can Print, Read
main =
  text = read()
  parts = split text ""
  each parts println
"#;
    let module = parse_module(source).expect("parse should succeed");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "c4_s4 emoji empty-delimiter program must be GeneralLowered"
    );
    // "a👨‍👩‍👧‍👦b" contains one emoji family ZWJ sequence (a single grapheme cluster)
    let output = execute_runtime_module_with_stdin(&module, Some("a👨‍👩‍👧‍👦b".to_string()))
        .expect("execution should succeed")
        .expect("output should be Some");
    assert_eq!(
        output, "a\n👨‍👩‍👧‍👦\nb\n",
        "split 'a👨‍👩‍👧‍👦b' '' should yield 3 grapheme clusters (not byte-split)"
    );
}

#[test]
fn c4_s4_split_empty_input_and_empty_delimiter_yields_empty_output() {
    let source = r#"
import goby/string ( split )
import goby/list ( each )

main : Unit -> Unit can Print, Read
main =
  text = read()
  parts = split text ""
  each parts println
"#;
    let module = parse_module(source).expect("parse should succeed");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "split empty input with empty delimiter must classify as GeneralLowered"
    );
    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("execution should succeed")
        .expect("output should be Some");
    assert_eq!(output, "", "split '' '' should yield [] and print nothing");
}

#[test]
fn c4_s4_split_multi_grapheme_unicode_delimiter_executes_correctly() {
    let source = r#"
import goby/string ( split )
import goby/list ( each )

main : Unit -> Unit can Print, Read
main =
  text = read()
  parts = split text "👨‍👩‍👧‍👦"
  each parts println
"#;
    let module = parse_module(source).expect("parse should succeed");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        RuntimeIoExecutionKind::GeneralLowered,
        "multi-grapheme Unicode delimiter split must classify as GeneralLowered"
    );
    let output = execute_runtime_module_with_stdin(&module, Some("a👨‍👩‍👧‍👦b👨‍👩‍👧‍👦👨‍👩‍👧‍👦c".to_string()))
        .expect("execution should succeed")
        .expect("output should be Some");
    assert_eq!(
        output, "a\nb\n\nc\n",
        "split must treat the emoji family delimiter as one Unicode grapheme and preserve empty parts"
    );
}

#[test]
fn execute_module_with_stdin_rejects_not_runtime_io_program() {
    // NotRuntimeIo programs (complex static evaluation via variable bindings etc.)
    // are also not valid interpreter-bridge inputs.
    let module = parse_module(
        r#"
main : Unit -> Unit can Print
main =
  msg = "hello"
  println msg
"#,
    )
    .expect("parse should work");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        goby_wasm::RuntimeIoExecutionKind::NotRuntimeIo,
        "program should be classified as NotRuntimeIo"
    );
    let err = execute_module_with_stdin(&module, None)
        .expect_err("not-runtime-io program should not use interpreter bridge path");
    assert!(
        err.message.contains("interpreter-bridge"),
        "unexpected error message: {}",
        err.message
    );
}

// ---------------------------------------------------------------------------
// Closure capture acceptance programs (failing until higher-order closure support lands)
//
// These tests correspond to the five acceptance programs in
// doc/PLAN_CLOSURE_CAPTURE.md §3.  Each one exercises a distinct closure
// capture shape that the current Wasm backend does not support.  The tests
// assert that compilation fails with a recognisable "closure capture" error
// rather than silently producing wrong code.
//
// When higher-order closure support is implemented, these tests should be updated to assert that
// compilation *succeeds* and execution produces the expected output.
// ---------------------------------------------------------------------------

/// Closure that reads an immutable outer binding (`base`) must not fully execute
/// on the current Wasm path. The lambda itself lowers; indirect closure call
/// dispatch is still needed for correct execution.
/// The program fails at IR lowering level (no IR decl), at lowering, or at call dispatch.
#[test]
fn read_only_immutable_capture_does_not_compile_on_wasm_path() {
    let source = r#"
make_adder : Int -> (Int -> Int)
make_adder base =
  fn x -> base + x

main : Unit -> Unit can Print, Read
main =
  _ = read()
  add10 = make_adder 10
  println "${add10 5}"
"#;
    let module = parse_module(source).expect("source should parse");
    // The program currently fails at IR lowering (no IR decl) or Wasm lowering.
    // Either error is acceptable until indirect closure call dispatch is complete.
    let err = compile_module(&module).expect_err(
        "capturing lambda program should not compile correctly on the current Wasm path",
    );
    assert!(
        err.message.contains("unsupported IR form")
            || err.message.contains("Lambda")
            || err.message.contains("no IR decl"),
        "error should indicate unsupported form or missing IR, got: {}",
        err.message
    );
}

/// Closure that writes to an outer mutable binding (`total`) executes correctly
/// and produces the expected sum.
#[test]
fn mutable_write_capture_via_each_executes_correctly_on_wasm_path() {
    let source = r#"
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
"#;
    let module = parse_module(source).expect("source should parse");
    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("mutable write capture via each should execute successfully");
    assert_eq!(
        output.as_deref(),
        Some("6\n"),
        "sum [1,2,3] with mutable capture should produce 6"
    );
}

/// Outer mutation after closure creation: closure sees updated value via shared cell.
#[test]
fn outer_mutation_after_closure_creation_executes_correctly_on_wasm_path() {
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  _ = read()
  mut value = 1
  read_value = fn _ -> value
  value := 7
  result = read_value ()
  println "${result}"
"#;
    let module = parse_module(source).expect("source should parse");
    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("outer mutation after closure creation should execute");
    assert_eq!(
        output.as_deref(),
        Some("7\n"),
        "closure should see the mutated value via shared cell"
    );
}

/// Inline capturing lambda passed to `fold` should compile on the Wasm path.
#[test]
fn inline_capturing_lambda_to_fold_compiles_on_wasm_path() {
    let source = r#"
import goby/list ( fold )

main : Unit -> Unit can Print, Read
main =
  _ = read()
  bias = 10
  total = fold [1, 2, 3] 0 (fn acc x -> acc + x + bias)
  println "${total}"
"#;
    let module = parse_module(source).expect("source should parse");
    let wasm =
        compile_module(&module).expect("capturing lambda to fold should compile on the Wasm path");
    assert!(!wasm.is_empty(), "compiled Wasm bytes should not be empty");
}

/// Two closures sharing one mutable cell: both see the same counter value.
/// CC4 complete: enabled; previously rejected on the Wasm path.
#[test]
#[ignore = "CC4-pending: multi-closure shared mutable cell not yet lowered on Wasm path"]
fn two_closures_sharing_mutable_cell_execute_correctly_on_wasm_path() {
    let source = r#"
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
  println "${p.1()}"
"#;
    let module = parse_module(source).expect("source should parse");
    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("two closures sharing mutable cell should execute");
    assert_eq!(
        output.as_deref(),
        Some("2\n"),
        "two inc() calls then get() should return 2"
    );
}

// ---------------------------------------------------------------------------
// ByValue closure capture + direct call dispatch
// ---------------------------------------------------------------------------

/// Inline capturing lambda with ByValue capture executes correctly.
/// `base = 10; add = fn x -> base + x; result = add 5; println "${result}"`
/// should print "15\n".
#[test]
fn cc3_inline_by_value_capture_executes_correctly() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  _ = read()
  base = 10
  add = fn x -> base + x
  result = add 5
  println "${result}"
"#,
    )
    .expect("source should parse");

    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("inline ByValue capture should execute without error");
    assert_eq!(
        output.as_deref(),
        Some("15\n"),
        "add 5 with base=10 should produce 15"
    );
}

/// Capturing lambda with string interpolation executes correctly.
/// The lambda captures `prefix` (a string) ByValue.
#[test]
fn cc3_string_capture_executes_correctly() {
    let module = parse_module(
        r#"
main : Unit -> Unit can Print, Read
main =
  _ = read()
  prefix = "Hello"
  greet = fn name -> "${prefix}, ${name}!"
  msg = greet "world"
  println msg
"#,
    )
    .expect("source should parse");

    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("string-capturing lambda should execute without error");
    assert_eq!(
        output.as_deref(),
        Some("Hello, world!\n"),
        "greet 'world' with prefix='Hello' should produce 'Hello, world!'"
    );
}
