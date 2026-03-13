use std::path::PathBuf;
use std::sync::Mutex;

use goby_core::parse_module;
use goby_wasm::{
    RuntimeIoExecutionKind, compile_module, execute_module_with_stdin, runtime_io_execution_kind,
};

/// Serializes tests that read or write process-wide environment variables.
static ENV_MUTEX: Mutex<()> = Mutex::new(());

fn assert_valid_wasm_module(wasm: &[u8]) {
    assert!(wasm.len() >= 8, "module too short: {} bytes", wasm.len());
    assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d], "bad wasm magic");
    assert_eq!(&wasm[4..8], &[0x01, 0x00, 0x00, 0x00], "bad wasm version");
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
  each lines (|line| -> println(line))
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
  each lines (|line| -> println(line))
"#,
    )
    .expect("parse should work");
    let wasm = compile_module(&module).expect("bound newline split shape should compile to Wasm");
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
fn execute_module_with_stdin_runs_read_program_at_runtime() {
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
  each copied (|line| -> println(line))
"#,
    )
    .expect("parse should work");
    let output = execute_module_with_stdin(&module, Some("hogehoge\nfugafuga".to_string()))
        .expect("runtime execution should succeed");
    assert_eq!(output.as_deref(), Some("hogehoge\nfugafuga\n"));
}

#[test]
fn runtime_io_execution_kind_reports_interpreter_bridge_for_complex_read_program() {
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
  each copied (|line| -> println(line))
"#,
    )
    .expect("parse should work");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should work"),
        RuntimeIoExecutionKind::InterpreterBridge
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
