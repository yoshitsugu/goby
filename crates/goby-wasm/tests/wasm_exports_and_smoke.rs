use std::path::PathBuf;

use goby_core::parse_module;
use goby_wasm::compile_module;

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
