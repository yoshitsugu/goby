use goby_core::parse_module;

use crate::memory_config::DEFAULT_WASM_MEMORY_CONFIG;
use crate::wasm_exec::run_wasm_bytes_with_stdin_for_tests;
use crate::{
    RuntimeIoExecutionKind, compile_module, execute_runtime_module_with_stdin,
    runtime_io_execution_kind,
};

const SELF_TAIL_RECURSION_SOURCE: &str = r#"
import goby/stdio

count_down : Int -> Unit can Print
count_down n =
  if n == 0
    ()
  else
    count_down (n - 1)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  count_down 1000000
  println "done"
"#;

const NON_TAIL_SCAN_SOURCE: &str = r#"
import goby/stdio

probe : Int -> Bool can Print
probe n =
  n >= 0

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
"#;

const RECURSIVE_LIST_SPREAD_SOURCE: &str = r#"
import goby/stdio

build : Int -> List Int can Print
build n =
  if n == 0
    []
  else
    rest = build (n - 1)
    [n, ..rest]

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  xs = build 5000
  println "${xs[0]}"
"#;

const CALLBACK_ASSISTED_SCAN_SOURCE: &str = r#"
import goby/list ( fold )
import goby/stdio

probe : Int -> Bool can Print
probe n =
  total =
    fold [1, 2, 3, 4, 5, 6, 7, 8] 0 (fn acc x ->
      acc + x + n
    )
  total >= 0

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
"#;

fn parse_general_lowered_module(source: &str) -> goby_core::Module {
    let module = parse_module(source).expect("representative RR source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module)
            .expect("representative RR source should classify for runtime execution"),
        RuntimeIoExecutionKind::GeneralLowered,
        "RR representative should stay on the Goby-owned Wasm runtime boundary"
    );
    module
}

#[test]
fn rr2_self_tail_recursion_repro_surfaces_stack_pressure_under_tight_stack_limit() {
    let module = parse_general_lowered_module(SELF_TAIL_RECURSION_SOURCE);
    let wasm = compile_module(&module).expect("self-tail recursion repro should compile");
    let low_stack = crate::memory_config::WasmMemoryConfig {
        max_wasm_stack_bytes: 64 * 1024,
        ..DEFAULT_WASM_MEMORY_CONFIG
    };

    let err = run_wasm_bytes_with_stdin_for_tests(&wasm, Some("x\n"), low_stack)
        .expect_err("tight stack limit should preserve the self-tail recursion bucket");
    assert!(
        err.contains("likely stack pressure [E-STACK-PRESSURE]"),
        "expected stack-pressure classification, got: {err}"
    );
}

#[test]
fn rr2_non_tail_recursive_scan_repro_executes_as_general_lowered() {
    let module = parse_general_lowered_module(NON_TAIL_SCAN_SOURCE);
    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("non-tail recursive scan representative should execute successfully");
    assert_eq!(output.as_deref(), Some("100\n"));
}

#[test]
fn rr2_recursive_list_spread_repro_reports_memory_exhaustion() {
    let module = parse_general_lowered_module(RECURSIVE_LIST_SPREAD_SOURCE);
    let err = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect_err("recursive list-spread representative should preserve memory exhaustion");
    assert!(
        err.message
            .contains("memory exhausted [E-MEMORY-EXHAUSTION]"),
        "expected memory exhaustion classification, got: {err:?}"
    );
}

#[test]
fn rr2_callback_assisted_scan_repro_executes_as_general_lowered() {
    let module = parse_general_lowered_module(CALLBACK_ASSISTED_SCAN_SOURCE);
    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("callback-assisted recursive scan representative should execute successfully");
    assert_eq!(output.as_deref(), Some("100\n"));
}
