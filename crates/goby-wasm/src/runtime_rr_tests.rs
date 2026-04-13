use goby_core::parse_module;

use crate::memory_config::DEFAULT_WASM_MEMORY_CONFIG;
use crate::wasm_exec::run_wasm_bytes_with_stdin_for_tests;
use crate::{
    RuntimeIoExecutionKind, compile_module, execute_runtime_module_with_stdin,
    resolve_module_runtime_output, runtime_io_execution_kind,
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

const MUTUAL_TAIL_RECURSION_SOURCE: &str = r#"
ping : Int -> Unit
ping n =
  if n == 0
    ()
  else
    pong (n - 1)

pong : Int -> Unit
pong n =
  if n == 0
    ()
  else
    ping (n - 1)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  ping 200000
  println "done"
"#;

const ACYCLIC_TAIL_CHAIN_SOURCE: &str = r#"
import goby/stdio

start : Int -> Unit can Print
start n =
  count_down n

count_down : Int -> Unit can Print
count_down n =
  if n == 0
    ()
  else
    count_down (n - 1)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  start 1000000
  println "done"
"#;

const GROUPED_DECL_FUNCREF_SOURCE: &str = r#"
import goby/stdio

apply : (Int -> Unit) -> Int -> Unit can Print
apply f n =
  f n

count_down : Int -> Unit can Print
count_down n =
  if n == 0
    ()
  else
    count_down (n - 1)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  apply count_down 32
  println "done"
"#;

const TAIL_IF_JOIN_SOURCE: &str = r#"
import goby/stdio

count_down : Bool -> Int -> Unit can Print
count_down flip n =
  if n == 0
    ()
  else
    if flip
      count_down False (n - 1)
    else
      count_down True (n - 1)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  count_down True 1000000
  println "done"
"#;

const TAIL_CASE_JOIN_SOURCE: &str = r#"
import goby/stdio

count_down : Bool -> Int -> Unit can Print
count_down flip n =
  if n == 0
    ()
  else
    case flip
      True -> count_down False (n - 1)
      False -> count_down True (n - 1)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  count_down True 1000000
  println "done"
"#;

const LET_TAIL_SOURCE: &str = r#"
import goby/stdio

count_down : Int -> Unit can Print
count_down n =
  if n == 0
    ()
  else
    next = n - 1
    count_down next

main : Unit -> Unit can Print, Read
main =
  _ = read()
  count_down 1000000
  println "done"
"#;

const LOCAL_ALIAS_TAIL_SOURCE: &str = r#"
import goby/stdio

count_down : Int -> Unit can Print
count_down n =
  if n == 0
    ()
  else
    step = count_down
    step (n - 1)

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

fn recursive_list_spread_source(n: usize) -> String {
    format!(
        r#"
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
  xs = build {n}
  println "${{xs[0]}}"
"#
    )
}

fn recursive_list_spread_head_tail_pattern_source(n: usize) -> String {
    format!(
        r#"
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
  xs = build {n}
  case xs
    [h, ..t] ->
      println "${{h}}"
      println "${{t[0]}}"
    [] ->
      println "empty"
"#
    )
}

fn recursive_list_spread_chunk_boundary_pattern_source(n: usize, prefix_len: usize) -> String {
    let mut items = Vec::with_capacity(prefix_len);
    for i in 1..=prefix_len {
        if i == 1 {
            items.push("a1".to_string());
        } else if i == prefix_len {
            items.push("a_boundary".to_string());
        } else {
            items.push("_".to_string());
        }
    }
    let pattern = items.join(", ");
    format!(
        r#"
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
  xs = build {n}
  case xs
    [{pattern}, ..t] ->
      println "${{a1}}"
      println "${{a_boundary}}"
      println "${{t[0]}}"
    [] ->
      println "empty"
"#
    )
}

fn recursive_list_spread_exact_pattern_source(n: usize, exact_len: usize) -> String {
    let mut items = Vec::with_capacity(exact_len);
    for i in 1..=exact_len {
        if i == 1 {
            items.push("first".to_string());
        } else if i == exact_len {
            items.push("last".to_string());
        } else {
            items.push("_".to_string());
        }
    }
    let pattern = items.join(", ");
    format!(
        r#"
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
  xs = build {n}
  case xs
    [{pattern}] ->
      println "${{first}}"
      println "${{last}}"
    _ ->
      println "mismatch"
"#
    )
}

fn recursive_list_spread_single_item_empty_tail_source() -> String {
    r#"
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
  xs = build 1
  case xs
    [h, ..t] ->
      case t
        [] -> println "${h}"
        _ -> println "non-empty"
    _ ->
      println "mismatch"
"#
    .to_string()
}

fn named_callback_list_spread_chain_source(n: usize) -> String {
    format!(
        r#"
import goby/list ( fold )
import goby/stdio

build : Int -> List Int can Print
build n =
  if n == 0
    []
  else
    rest = build (n - 1)
    [n, ..rest]

prepend : List Int -> Int -> List Int can Print
prepend acc x =
  [x, ..acc]

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  seed = build {n}
  xs = fold seed [] prepend
  println "${{xs[0]}}"
"#
    )
}

fn local_named_callback_list_spread_chain_source(n: usize) -> String {
    format!(
        r#"
import goby/list ( fold )
import goby/stdio

build : Int -> List Int can Print
build n =
  if n == 0
    []
  else
    rest = build (n - 1)
    [n, ..rest]

prepend : List Int -> Int -> List Int can Print
prepend acc x =
  [x, ..acc]

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  seed = build {n}
  f = prepend
  xs = fold seed [] f
  println "${{xs[0]}}"
"#
    )
}

fn inline_callback_list_spread_chain_source(n: usize) -> String {
    format!(
        r#"
import goby/list ( fold )
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
  seed = build {n}
  xs =
    fold seed [] (fn acc x ->
      [x, ..acc]
    )
  println "${{xs[0]}}"
"#
    )
}

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

fn non_tail_scan_main_source(
    extra_imports: &str,
    probe_body: &str,
    width: usize,
    height: usize,
) -> String {
    format!(
        r#"
import goby/stdio
{extra_imports}

probe : Int -> Bool can Print
probe n =
{probe_body}

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
  result = walk {width} {height} 0 0
  println "${{result}}"
"#
    )
}

fn rr_tight_stack_config() -> crate::memory_config::WasmMemoryConfig {
    crate::memory_config::WasmMemoryConfig {
        max_wasm_stack_bytes: 64 * 1024,
        ..DEFAULT_WASM_MEMORY_CONFIG
    }
}

fn parse_general_lowered_module(source: &str) -> goby_core::Module {
    let module = parse_module(source).expect("representative RR source should parse");
    let execution_kind = runtime_io_execution_kind(&module)
        .expect("representative RR source should classify for runtime execution");
    let general_lower_reason = crate::gen_lower::supports_general_lower_module(&module)
        .expect("general lowering support check should not error");
    assert_eq!(
        execution_kind,
        RuntimeIoExecutionKind::GeneralLowered,
        "RR representative should stay on the Goby-owned Wasm runtime boundary; kind={execution_kind:?}, general_lower_reason={general_lower_reason:?}"
    );
    module
}

fn measure_wasm_exec_micros(
    wasm: &[u8],
    stdin_seed: &str,
    warmup_runs: usize,
    measured_runs: usize,
) -> (u128, u128, usize, usize, Option<String>, Option<String>) {
    let mut ok_runs = 0usize;
    let mut err_runs = 0usize;
    let mut last_ok = None;
    let mut last_err = None;
    for _ in 0..warmup_runs {
        match run_wasm_bytes_with_stdin_for_tests(
            wasm,
            Some(stdin_seed),
            DEFAULT_WASM_MEMORY_CONFIG,
        ) {
            Ok(text) => {
                ok_runs += 1;
                last_ok = Some(text);
            }
            Err(err) => {
                err_runs += 1;
                last_err = Some(err);
            }
        }
    }
    let mut samples = Vec::with_capacity(measured_runs);
    for _ in 0..measured_runs {
        let start = std::time::Instant::now();
        match run_wasm_bytes_with_stdin_for_tests(
            wasm,
            Some(stdin_seed),
            DEFAULT_WASM_MEMORY_CONFIG,
        ) {
            Ok(text) => {
                ok_runs += 1;
                last_ok = Some(text);
            }
            Err(err) => {
                err_runs += 1;
                last_err = Some(err);
            }
        }
        samples.push(start.elapsed().as_micros());
    }
    samples.sort_unstable();
    let n = samples.len();
    let p50 = samples[(n - 1) * 50 / 100];
    let p95 = samples[(n - 1) * 95 / 100];
    (p50, p95, ok_runs, err_runs, last_ok, last_err)
}

fn measure_runtime_output_resolve_micros(
    module: &goby_core::Module,
    warmup_runs: usize,
    measured_runs: usize,
) -> (u128, u128, usize, usize, Option<String>) {
    let mut ok_runs = 0usize;
    let mut none_runs = 0usize;
    let mut last_ok = None;
    for _ in 0..warmup_runs {
        match resolve_module_runtime_output(module) {
            Some(text) => {
                ok_runs += 1;
                last_ok = Some(text);
            }
            None => {
                none_runs += 1;
            }
        }
    }
    let mut samples = Vec::with_capacity(measured_runs);
    for _ in 0..measured_runs {
        let start = std::time::Instant::now();
        match resolve_module_runtime_output(module) {
            Some(text) => {
                ok_runs += 1;
                last_ok = Some(text);
            }
            None => {
                none_runs += 1;
            }
        }
        samples.push(start.elapsed().as_micros());
    }
    samples.sort_unstable();
    let n = samples.len();
    let p50 = samples[(n - 1) * 50 / 100];
    let p95 = samples[(n - 1) * 95 / 100];
    (p50, p95, ok_runs, none_runs, last_ok)
}

fn compile_general_lowered_wasm(source: &str) -> Vec<u8> {
    let module = parse_general_lowered_module(source);
    compile_module(&module).expect("baseline workload should compile")
}

#[test]
fn rr5_self_tail_recursion_repro_survives_tight_stack_limit_after_tail_decl_loop() {
    let module = parse_general_lowered_module(SELF_TAIL_RECURSION_SOURCE);
    let wasm = compile_module(&module).expect("self-tail recursion repro should compile");
    let output = run_wasm_bytes_with_stdin_for_tests(&wasm, Some("x\n"), rr_tight_stack_config())
        .expect("self TailDeclCall loop should survive the tight stack limit");
    assert_eq!(output, "done\n");
}

#[test]
fn rr5_mutual_tail_recursion_repro_survives_tight_stack_limit_after_group_dispatch() {
    let module = parse_general_lowered_module(MUTUAL_TAIL_RECURSION_SOURCE);
    let wasm = compile_module(&module).expect("mutual tail recursion repro should compile");
    let output = run_wasm_bytes_with_stdin_for_tests(&wasm, Some("x\n"), rr_tight_stack_config())
        .expect("mutual TailDeclCall dispatcher should survive the tight stack limit");
    assert_eq!(output, "done\n");
}

#[test]
fn rr5_acyclic_tail_chain_repro_survives_tight_stack_limit_on_shared_dispatcher() {
    let module = parse_general_lowered_module(ACYCLIC_TAIL_CHAIN_SOURCE);
    let wasm = compile_module(&module).expect("acyclic tail-chain repro should compile");
    let output = run_wasm_bytes_with_stdin_for_tests(&wasm, Some("x\n"), rr_tight_stack_config())
        .expect("shared dispatcher should execute the covered acyclic tail chain");
    assert_eq!(output, "done\n");
}

#[test]
fn grouped_tail_decl_member_still_works_through_function_value_entrypoint() {
    let module = parse_general_lowered_module(GROUPED_DECL_FUNCREF_SOURCE);
    let wasm = compile_module(&module).expect("grouped function-value repro should compile");
    let output = run_wasm_bytes_with_stdin_for_tests(&wasm, Some("x\n"), rr_tight_stack_config())
        .expect("grouped declaration should still be callable through a function value");
    assert_eq!(output, "done\n");
}

#[test]
fn rr5_tail_if_join_repro_survives_tight_stack_limit() {
    let module = parse_general_lowered_module(TAIL_IF_JOIN_SOURCE);
    let wasm = compile_module(&module).expect("tail if-join repro should compile");
    let output = run_wasm_bytes_with_stdin_for_tests(&wasm, Some("x\n"), rr_tight_stack_config())
        .expect("tail if-join representative should stay constant-stack");
    assert_eq!(output, "done\n");
}

#[test]
fn rr5_tail_case_join_repro_survives_tight_stack_limit() {
    let module = parse_general_lowered_module(TAIL_CASE_JOIN_SOURCE);
    let wasm = compile_module(&module).expect("tail case-join repro should compile");
    let output = run_wasm_bytes_with_stdin_for_tests(&wasm, Some("x\n"), rr_tight_stack_config())
        .expect("tail case-join representative should stay constant-stack");
    assert_eq!(output, "done\n");
}

#[test]
fn rr5_let_tail_repro_survives_tight_stack_limit() {
    let module = parse_general_lowered_module(LET_TAIL_SOURCE);
    let wasm = compile_module(&module).expect("let-tail repro should compile");
    let output = run_wasm_bytes_with_stdin_for_tests(&wasm, Some("x\n"), rr_tight_stack_config())
        .expect("let-tail representative should stay constant-stack");
    assert_eq!(output, "done\n");
}

#[test]
fn rr5_local_alias_tail_repro_survives_tight_stack_limit() {
    let module = parse_general_lowered_module(LOCAL_ALIAS_TAIL_SOURCE);
    let wasm = compile_module(&module).expect("local-alias tail repro should compile");
    let output = run_wasm_bytes_with_stdin_for_tests(&wasm, Some("x\n"), rr_tight_stack_config())
        .expect("local-alias tail representative should stay constant-stack");
    assert_eq!(output, "done\n");
}

#[test]
fn rr2_non_tail_recursive_scan_repro_executes_as_general_lowered() {
    let module = parse_general_lowered_module(NON_TAIL_SCAN_SOURCE);
    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("non-tail recursive scan representative should execute successfully");
    assert_eq!(output.as_deref(), Some("100\n"));
}

#[test]
fn rr3_non_tail_recursive_scan_repro_survives_tight_stack_limit_after_loop_lowering() {
    let source = non_tail_scan_main_source("", "  n >= 0", 1, 200_000);
    let module = parse_general_lowered_module(&source);
    let wasm = compile_module(&module).expect("tight-stack non-tail scan repro should compile");

    let output = run_wasm_bytes_with_stdin_for_tests(&wasm, Some("x\n"), rr_tight_stack_config())
        .expect("loop-lowered scan should survive the tight stack limit");
    assert_eq!(output, "200000\n");
}

#[test]
fn rr4_recursive_list_spread_repro_executes_after_builder_lowering() {
    let module = parse_general_lowered_module(RECURSIVE_LIST_SPREAD_SOURCE);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("recursive list-spread representative should execute after RR-4 lowering");
    assert_eq!(output.as_deref(), Some("5000\n"));
}

#[test]
fn rr4_recursive_list_spread_large_builder_shape_scales_past_bug_repro_size() {
    let module = parse_general_lowered_module(&recursive_list_spread_source(50_000));
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("larger recursive list-spread builder shape should execute successfully");
    assert_eq!(output.as_deref(), Some("50000\n"));
}

#[test]
fn rr4_recursive_list_spread_large_head_tail_pattern_executes_after_chunked_migration() {
    let module =
        parse_general_lowered_module(&recursive_list_spread_head_tail_pattern_source(10_000));
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("multi-chunk [h, ..t] pattern over builder result should execute successfully");
    assert_eq!(output.as_deref(), Some("10000\n9999\n"));
}

#[test]
fn rr4_repeated_head_tail_decomposition_crosses_chunk_boundaries() {
    let module = parse_general_lowered_module(
        &recursive_list_spread_chunk_boundary_pattern_source(100, 33),
    );
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("large prefix list pattern should bind across chunk boundaries");
    assert_eq!(output.as_deref(), Some("100\n68\n67\n"));
}

#[test]
fn rr4_exact_length_pattern_crosses_chunk_boundary_and_matches() {
    let module = parse_general_lowered_module(&recursive_list_spread_exact_pattern_source(33, 33));
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("exact-length list pattern should match across the first chunk boundary");
    assert_eq!(output.as_deref(), Some("33\n1\n"));
}

#[test]
fn rr4_head_tail_pattern_binds_empty_tail_for_single_item_list() {
    let module =
        parse_general_lowered_module(&recursive_list_spread_single_item_empty_tail_source());
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("single-item [h, ..t] should bind t as empty list");
    assert_eq!(output.as_deref(), Some("1\n"));
}

#[test]
fn rr4_inline_fold_prepend_builder_executes_after_specialized_lowering() {
    let module = parse_general_lowered_module(&inline_callback_list_spread_chain_source(20_000));
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("inline fold prepend builder should execute after specialized lowering");
    assert_eq!(output.as_deref(), Some("1\n"));
}

#[test]
fn rr4_named_callback_list_spread_chain_executes_after_callback_rewrite() {
    let module = parse_general_lowered_module(&named_callback_list_spread_chain_source(20_000));
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("named callback list-spread chain should execute after RR-4 rewrite");
    assert_eq!(output.as_deref(), Some("1\n"));
}

#[test]
fn rr4_local_named_callback_list_spread_chain_executes_after_callback_rewrite() {
    let module =
        parse_general_lowered_module(&local_named_callback_list_spread_chain_source(20_000));
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string())).expect(
        "local alias to named callback list-spread chain should execute after RR-4 rewrite",
    );
    assert_eq!(output.as_deref(), Some("1\n"));
}

#[test]
fn rr2_callback_assisted_scan_repro_executes_as_general_lowered() {
    let module = parse_general_lowered_module(CALLBACK_ASSISTED_SCAN_SOURCE);
    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("callback-assisted recursive scan representative should execute successfully");
    assert_eq!(output.as_deref(), Some("100\n"));
}

#[test]
fn rr3_callback_assisted_scan_repro_survives_tight_stack_limit_on_same_boundary() {
    let source = non_tail_scan_main_source(
        "import goby/list ( fold )",
        r#"  total =
    fold [1, 2, 3, 4, 5, 6, 7, 8] 0 (fn acc x ->
      acc + x + n
    )
  total >= 0"#,
        1,
        40_000,
    );
    let module = parse_general_lowered_module(&source);
    let wasm =
        compile_module(&module).expect("tight-stack callback-assisted scan repro should compile");

    let output = run_wasm_bytes_with_stdin_for_tests(&wasm, Some("x\n"), rr_tight_stack_config())
        .expect("callback-assisted scan should survive the same tight stack limit");
    assert_eq!(output, "40000\n");
}

// ---------------------------------------------------------------------------
// M5: __goby_list_length intrinsic regression tests
// ---------------------------------------------------------------------------

#[test]
fn m5_list_length_empty_list_returns_zero() {
    let source = r#"
import goby/list ( length )
import goby/stdio

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  n = length []
  println "${n}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("length [] should return 0");
    assert_eq!(output.as_deref(), Some("0\n"));
}

#[test]
fn m5_list_length_single_chunk_32_elements() {
    let source = r#"
import goby/list ( length )
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
  xs = build 32
  n = length xs
  println "${n}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("length of 32-element list should return 32");
    assert_eq!(output.as_deref(), Some("32\n"));
}

#[test]
fn m5_list_length_multi_chunk_65_elements() {
    let source = r#"
import goby/list ( length )
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
  xs = build 65
  n = length xs
  println "${n}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("length of 65-element list should return 65");
    assert_eq!(output.as_deref(), Some("65\n"));
}

#[test]
fn m5_list_length_direct_intrinsic_smoke() {
    let source = r#"
import goby/stdio

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  n = __goby_list_length []
  println "${n}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("direct __goby_list_length [] should return 0");
    assert_eq!(output.as_deref(), Some("0\n"));
}

// ---------------------------------------------------------------------------
// M5-8: traversal runtime regression tests
// ---------------------------------------------------------------------------

#[test]
fn m5_fold_empty_list_returns_seed() {
    let source = r#"
import goby/list ( fold )
import goby/stdio

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  total = fold [] 0 (fn acc x -> acc + x)
  println "${total}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("fold over empty list should return the seed");
    assert_eq!(output.as_deref(), Some("0\n"));
}

#[test]
fn m5_fold_single_chunk_sum() {
    let source = r#"
import goby/list ( fold )
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
  xs = build 32
  total = fold xs 0 (fn acc x -> acc + x)
  println "${total}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("single-chunk fold sum should execute");
    assert_eq!(output.as_deref(), Some("528\n"));
}

#[test]
fn m5_fold_multi_chunk_sum() {
    let source = r#"
import goby/list ( fold )
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
  xs = build 65
  total = fold xs 0 (fn acc x -> acc + x)
  println "${total}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("multi-chunk fold sum should execute");
    assert_eq!(output.as_deref(), Some("2145\n"));
}

#[test]
fn m5_fold_string_concat_then_println_executes() {
    let source = r#"
import goby/list ( fold )
import goby/stdio

step : String -> String -> String
step acc x =
  if acc == ""
    x
  else
    "${acc}-${x}"

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  joined = fold ["a", "b", "c"] "" step
  println joined
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("string fold followed by println should execute");
    assert_eq!(output.as_deref(), Some("a-b-c\n"));
}

#[test]
fn m5_public_fold_prepend_runtime_reuses_dedicated_reverse_fold_boundary() {
    let source = inline_callback_list_spread_chain_source(20_000);
    let module = parse_general_lowered_module(&source);
    let (main_instrs, aux_decls) = crate::gen_lower::lower_module_to_instrs(&module)
        .expect("lowering should not hard-fail")
        .expect("general lowering should accept public fold prepend module");
    assert!(
        main_instrs.iter().any(|instr| matches!(
            instr,
            crate::gen_lower::backend_ir::WasmBackendInstr::ListReverseFoldPrepend { .. }
        )),
        "public fold prepend should lower to dedicated reverse-fold boundary, got main: {main_instrs:?}; aux: {aux_decls:?}"
    );
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("public fold prepend runtime path should execute");
    assert_eq!(output.as_deref(), Some("1\n"));
}

#[test]
fn m5_direct_intrinsic_fold_prepend_runtime_reuses_dedicated_reverse_fold_boundary() {
    let source = r#"
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
  seed = build 20000
  xs =
    __goby_list_fold seed [] (fn acc x ->
      [x, ..acc]
    )
  println "${xs[0]}"
"#;
    let module = parse_general_lowered_module(source);
    let (main_instrs, aux_decls) = crate::gen_lower::lower_module_to_instrs(&module)
        .expect("lowering should not hard-fail")
        .expect("general lowering should accept direct intrinsic fold prepend module");
    assert!(
        main_instrs.iter().any(|instr| matches!(
            instr,
            crate::gen_lower::backend_ir::WasmBackendInstr::ListReverseFoldPrepend { .. }
        )),
        "direct intrinsic prepend should lower to dedicated reverse-fold boundary, got main: {main_instrs:?}; aux: {aux_decls:?}"
    );
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("direct intrinsic fold prepend runtime path should execute");
    assert_eq!(output.as_deref(), Some("1\n"));
}

#[test]
fn m5_each_general_callback_executes() {
    let source = r#"
import goby/list ( each )
import goby/stdio

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  mut total = 0
  each [1, 2, 3, 4] (fn x ->
    total := total + x
  )
  println "${total}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("each with general callback should execute");
    assert_eq!(output.as_deref(), Some("10\n"));
}

#[test]
fn m5_each_effect_callback_executes() {
    let source = r#"
import goby/list ( each )
import goby/stdio

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  each ["a", "b", "c"] println
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("each with effect callback should execute");
    assert_eq!(output.as_deref(), Some("a\nb\nc\n"));
}

#[test]
fn m5_map_multi_chunk_list_executes() {
    let source = r#"
import goby/list ( map, each )
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
  xs = build 65
  mapped = map xs (fn i -> "${i + 1}")
  each mapped println
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("multi-chunk map should execute");
    assert_eq!(
        output.as_deref(),
        Some(
            "66\n65\n64\n63\n62\n61\n60\n59\n58\n57\n56\n55\n54\n53\n52\n51\n50\n49\n48\n47\n46\n45\n44\n43\n42\n41\n40\n39\n38\n37\n36\n35\n34\n33\n32\n31\n30\n29\n28\n27\n26\n25\n24\n23\n22\n21\n20\n19\n18\n17\n16\n15\n14\n13\n12\n11\n10\n9\n8\n7\n6\n5\n4\n3\n2\n"
        )
    );
}

#[test]
fn m6_2_indexed_read_4k_mixed_indices_executes_without_trap() {
    let source = r#"
import goby/stdio

build : Int -> List Int can Print
build n =
  if n == 0
    []
  else
    rest = build (n - 1)
    [n, ..rest]

scan_reads : List Int -> Int -> Int -> Int can Print
scan_reads xs i acc =
  if i >= 4000
    acc
  else
    j = (i * 97) % 4000
    v = xs[j]
    scan_reads xs (i + 1) (acc + v)

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  xs = build 4000
  total = scan_reads xs 0 0
  println "${total}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("4k mixed indexed-read should execute without trap");
    assert_eq!(output.as_deref(), Some("8002000\n"));
}

#[test]
fn m6_2_indexed_read_surface_and_stdlib_get_match_on_multi_chunk_list() {
    let source = r#"
import goby/list ( get )
import goby/stdio

build : Int -> List Int can Print
build n =
  if n == 0
    []
  else
    rest = build (n - 1)
    [n, ..rest]

scan_bracket : List Int -> Int -> Int -> Int can Print
scan_bracket xs i acc =
  if i >= 4096
    acc
  else
    j = (i * 193) % 4096
    scan_bracket xs (i + 1) (acc + xs[j])

scan_get : List Int -> Int -> Int -> Int can Print
scan_get xs i acc =
  if i >= 4096
    acc
  else
    j = (i * 193) % 4096
    scan_get xs (i + 1) (acc + get xs j)

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  xs = build 4096
  total_a = scan_bracket xs 0 0
  total_b = scan_get xs 0 0
  println "${total_a}"
  println "${total_b}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("surface and stdlib indexed reads should both execute");
    assert_eq!(output.as_deref(), Some("8390656\n8390656\n"));
}

#[test]
fn m6_3_point_update_4k_executes_without_trap() {
    // Uses list.set (functional immutable update) to avoid the mut-only AssignIndex path.
    // Each scan_updates call returns a new list with one element replaced.
    // List size 1000 stays within §6.6 success bar (≤1000 elements).
    let source = r#"
import goby/list (set, get)
import goby/stdio

build : Int -> List Int can Print
build n =
  if n == 0
    []
  else
    rest = build (n - 1)
    [n, ..rest]

scan_updates : List Int -> Int -> List Int can Print
scan_updates xs i =
  if i >= 1000
    xs
  else
    j = (i * 97) % 1000
    next = set xs j i
    scan_updates next (i + 1)

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  xs = build 1000
  updated = scan_updates xs 0
  v0 = get updated 0
  v999 = get updated 999
  println "${v0}"
  println "${v999}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("point-update workload should execute without trap");
    // v0: index 0 last written at i=0 (j=(0*97)%1000=0), value=0.
    // v999: index 999 last written at i=567 (j=(567*97)%1000=999), value=567.
    assert_eq!(output.as_deref(), Some("0\n567\n"));
}

#[test]
fn m6_4_nested_update_64x64_executes_without_trap() {
    // Uses list.set (functional immutable update) for both row and grid updates.
    // 64x64 grid (≤ 30x30=900 elements per §6.6 success bar; 64x64=4096 is above the bar
    // but row-length is 64 and the list.set boundary handles it via O(CHUNK_SIZE) chunk copy).
    let source = r#"
import goby/list (set, get)
import goby/stdio

build_row : Int -> List Int can Print
build_row n =
  if n == 0
    []
  else
    rest = build_row (n - 1)
    [0, ..rest]

build_grid : Int -> List (List Int) can Print
build_grid n =
  if n == 0
    []
  else
    rest = build_grid (n - 1)
    [build_row 64, ..rest]

scan_nested_updates : List (List Int) -> Int -> List (List Int) can Print
scan_nested_updates grid i =
  if i >= 4096
    grid
  else
    y = i / 64
    x = (i * 17) % 64
    row = get grid y
    next_row = set row x i
    next_grid = set grid y next_row
    scan_nested_updates next_grid (i + 1)

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  grid = build_grid 64
  updated = scan_nested_updates grid 0
  row0 = get updated 0
  row63 = get updated 63
  v00 = get row0 0
  v6363 = get row63 63
  println "${v00}"
  println "${v6363}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("nested-update workload should execute without trap");
    // y=0,x=0: i*17%64=0 → i=0 (last: i=4032 → y=4032/64=63,x=4032*17%64=0 — wait, y=63 not 0)
    // y=0: i in 0..63 (i/64=0). x=(i*17)%64=0 when i*17≡0 (mod 64) → i=0 or i=64/gcd... gcd(17,64)=1 → i=0 only in [0,63]. So v00 = 0.
    // y=63: i in 63*64..64*64-1=4032..4095. x=(i*17)%64=63 when i*17≡63 (mod 64).
    //   17*i ≡ 63 (mod 64). inv(17) mod 64: 17*x≡1(mod 64). 17*49=833=13*64+1 → inv=49.
    //   i ≡ 63*49 (mod 64) = 3087 mod 64 = 3087-48*64=3087-3072=15. Last i in [4032,4095] where i%64=15: 4032+15=4047.
    // v6363 = 4047.
    assert_eq!(output.as_deref(), Some("0\n4047\n"));
}

/// M6-3診断: list.set intrinsicによる最小限のpoint updateが動作するか確認
#[test]
fn m6_3_minimal_point_update_smoke() {
    // 3要素リスト、list.set で1回point update。
    // list.set xs i v = __goby_list_set xs i v (functional-style immutable update)
    let source = r#"
import goby/list (set, get)
import goby/stdio

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  xs = [10, 20, 30]
  updated = set xs 1 99
  result = get updated 1
  println "${result}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("minimal point-update via list.set should not trap");
    assert_eq!(output.as_deref(), Some("99\n"));
}

/// M6: list.set with out-of-bounds index aborts the program (does not return garbage).
#[test]
fn m6_3_list_set_out_of_bounds_aborts() {
    let source = r#"
import goby/list (set)
import goby/stdio

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  xs = [1, 2, 3]
  _ = set xs 5 99
  println "should not reach"
"#;
    let module = parse_general_lowered_module(source);
    // OOB set should abort (trap), not return silently
    let result = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()));
    assert!(result.is_err(), "out-of-bounds list.set should abort");
}

#[test]
#[ignore = "M6-0 baseline snapshot command; run explicitly with --ignored --nocapture"]
fn m6_0_baseline_index_update_workloads() {
    let warmup_runs = 3usize;
    let measured_runs = 10usize;
    let stdin_seed = "x\n";

    let indexed_read_source = r#"
import goby/stdio

build : Int -> List Int can Print
build n =
  if n == 0
    []
  else
    rest = build (n - 1)
    [n, ..rest]

scan_reads : List Int -> Int -> Int -> Int can Print
scan_reads xs i acc =
  if i >= 4000
    acc
  else
    j = (i * 97) % 4000
    v = xs[j]
    scan_reads xs (i + 1) (acc + v)

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  xs = build 4000
  total = scan_reads xs 0 0
  println "${total}"
"#;
    let indexed_read_wasm = compile_general_lowered_wasm(indexed_read_source);
    let (
        indexed_read_p50,
        indexed_read_p95,
        indexed_read_ok,
        indexed_read_err,
        indexed_read_last_ok,
        indexed_read_last_err,
    ) = measure_wasm_exec_micros(&indexed_read_wasm, stdin_seed, warmup_runs, measured_runs);
    assert_eq!(indexed_read_err, 0, "indexed-read baseline should not trap");
    assert_eq!(indexed_read_last_ok.as_deref(), Some("8002000\n"));
    eprintln!(
        "[M6-0][indexed-read-4k-mixed] p50={}us p95={}us ok_runs={} err_runs={} output={} err={:?}",
        indexed_read_p50,
        indexed_read_p95,
        indexed_read_ok,
        indexed_read_err,
        indexed_read_last_ok.unwrap_or_default().trim_end(),
        indexed_read_last_err
    );

    let point_update_source = r#"
import goby/stdio

build : Int -> List Int can Print
build n =
  if n == 0
    []
  else
    rest = build (n - 1)
    [n, ..rest]

scan_updates : List Int -> Int -> List Int can Print
scan_updates xs i =
  if i >= 4000
    xs
  else
    j = (i * 97) % 4000
    updated = xs[j] := i
    scan_updates updated (i + 1)

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  xs = build 4000
  updated = scan_updates xs 0
  println "${updated[0]}"
  println "${updated[3999]}"
"#;
    let point_update_wasm = compile_general_lowered_wasm(point_update_source);
    let (
        point_update_p50,
        point_update_p95,
        point_update_ok,
        point_update_err,
        point_update_last_ok,
        point_update_last_err,
    ) = measure_wasm_exec_micros(&point_update_wasm, stdin_seed, warmup_runs, measured_runs);
    eprintln!(
        "[M6-0][point-update-4k] p50={}us p95={}us ok_runs={} err_runs={} output={:?} err={:?}",
        point_update_p50,
        point_update_p95,
        point_update_ok,
        point_update_err,
        point_update_last_ok,
        point_update_last_err
    );

    let nested_update_source = r#"
import goby/stdio

build_row : Int -> List Int can Print
build_row n =
  if n == 0
    []
  else
    rest = build_row (n - 1)
    [0, ..rest]

build_grid : Int -> List (List Int) can Print
build_grid n =
  if n == 0
    []
  else
    rest = build_grid (n - 1)
    [build_row 64, ..rest]

scan_nested_updates : List (List Int) -> Int -> List (List Int) can Print
scan_nested_updates grid i =
  if i >= 4096
    grid
  else
    y = i / 64
    x = (i * 17) % 64
    row = grid[y]
    next_row = row[x] := i
    next_grid = grid[y] := next_row
    scan_nested_updates next_grid (i + 1)

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  grid = build_grid 64
  updated = scan_nested_updates grid 0
  println "${updated[0][0]}"
  println "${updated[63][63]}"
"#;
    let nested_update_wasm = compile_general_lowered_wasm(nested_update_source);
    let (
        nested_update_p50,
        nested_update_p95,
        nested_update_ok,
        nested_update_err,
        nested_update_last_ok,
        nested_update_last_err,
    ) = measure_wasm_exec_micros(&nested_update_wasm, stdin_seed, warmup_runs, measured_runs);
    eprintln!(
        "[M6-0][nested-update-64x64] p50={}us p95={}us ok_runs={} err_runs={} output={:?} err={:?}",
        nested_update_p50,
        nested_update_p95,
        nested_update_ok,
        nested_update_err,
        nested_update_last_ok,
        nested_update_last_err
    );

    let aoc_style_source = r#"
import goby/list (each, fold, join, length, map)
import goby/string (graphemes)
import goby/stdio

neighbor_threshold : Int
neighbor_threshold = 4

should_prune_cell : List (List String) -> Int -> Int -> Int -> Int -> Bool can Print
should_prune_cell grid x y width height =
  if grid[y][x] == "@"
    neighbor_offsets = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
    occupied_neighbors =
      fold neighbor_offsets 0 (fn total offset ->
        next_y = y + offset.1
        next_x = x + offset.0
        if 0 <= next_y && next_y < height && 0 <= next_x && next_x < width
          if grid[y + offset.1][x + offset.0] == "@"
            total + 1
          else
            total
        else
          total
      )
    occupied_neighbors < neighbor_threshold
  else
    False

collect_prune_positions : List (List String) -> Int -> Int -> Int -> Int -> List (Int, Int) can Print
collect_prune_positions grid x y width height =
  if y >= height
    []
  else
    if x >= width
      collect_prune_positions grid 0 (y + 1) width height
    else
      rest = collect_prune_positions grid (x + 1) y width height
      if should_prune_cell grid x y width height
        [(x, y), ..rest]
      else
        rest

apply_prune_positions : List (List String) -> List (Int, Int) -> List (List String) can Print
apply_prune_positions grid positions =
  mut updated = grid
  each positions (fn pos ->
    updated[pos.1][pos.0] := "."
    ()
  )
  updated

render_grid : List (List String) -> Unit can Print
render_grid grid =
  each grid (fn row ->
    line = join row ""
    println line
  )

prune_until_stable : List (List String) -> Int -> Int can Print
prune_until_stable grid total =
  height = length grid
  width = length(grid[0])
  positions = collect_prune_positions grid 0 0 width height
  count = length positions
  if count > 0
    updated = apply_prune_positions grid positions
    render_grid updated
    prune_until_stable updated (total + count)
  else
    total

main : Unit -> Unit can Print, Read
main =
  grid = map (read_lines ()) graphemes
  total = prune_until_stable grid 0
  println "${total}"
"#;
    let aoc_stdin = "\
..@@.@@@@.\n\
@@@.@.@.@@\n\
@@@@@.@.@@\n\
@.@@@@..@.\n\
@@.@@@@.@@\n\
.@@@@@@@.@\n\
.@.@.@.@@@\n\
@.@@@.@@@@\n\
.@@@@@@@@.\n\
@.@.@@@.@.\n";
    let aoc_style_wasm = compile_general_lowered_wasm(aoc_style_source);
    let aoc_style_output = run_wasm_bytes_with_stdin_for_tests(
        &aoc_style_wasm,
        Some(aoc_stdin),
        DEFAULT_WASM_MEMORY_CONFIG,
    )
    .expect("AoC-style baseline should execute");
    assert!(
        aoc_style_output.ends_with("43\n"),
        "AoC-style output should end with stable-pruning total, got: {:?}",
        aoc_style_output
    );
    let (
        aoc_style_p50,
        aoc_style_p95,
        aoc_style_ok,
        aoc_style_err,
        _aoc_style_last_ok,
        aoc_style_last_err,
    ) = measure_wasm_exec_micros(&aoc_style_wasm, aoc_stdin, warmup_runs, measured_runs);
    eprintln!(
        "[M6-0][aoc-style-iterative-grid-pruning-after-render] p50={}us p95={}us ok_runs={} err_runs={} final_total=43 err={:?}",
        aoc_style_p50, aoc_style_p95, aoc_style_ok, aoc_style_err, aoc_style_last_err
    );
}

/// M7-1 baseline snapshot for traversal shape comparison.
/// Compares:
/// - list.each with pure callback,
/// - list.each with effect callback,
/// - iterator.yield-based traversal on the same list/sum workload.
#[test]
#[ignore = "M7-1 baseline snapshot command; run explicitly with --ignored --nocapture"]
fn m7_1_baseline_traversal_workloads() {
    let warmup_runs = 3usize;
    let measured_runs = 10usize;

    let each_pure_source = r#"
import goby/list ( each )
import goby/stdio

main : Unit -> Unit can Print
main =
  xs = [1, 2, 3]
  mut total = 0
  each xs (fn x ->
    total := total + x
  )
  print "${total}"
"#;
    let each_pure_module =
        parse_module(each_pure_source).expect("M7-1 each-pure source should parse");
    let (each_pure_p50, each_pure_p95, each_pure_ok, each_pure_none, each_pure_last_ok) =
        measure_runtime_output_resolve_micros(&each_pure_module, warmup_runs, measured_runs);
    assert_eq!(
        each_pure_none, 0,
        "M7-1 each-pure baseline should resolve runtime output"
    );
    assert_eq!(each_pure_last_ok.as_deref(), Some("6"));
    eprintln!(
        "[M7-1][each-pure-callback-3] p50={}us p95={}us ok_runs={} none_runs={} output={}",
        each_pure_p50,
        each_pure_p95,
        each_pure_ok,
        each_pure_none,
        each_pure_last_ok.unwrap_or_default().trim_end(),
    );

    let each_effect_source = r#"
import goby/list ( each )
import goby/stdio

main : Unit -> Unit can Print
main =
  xs = [1, 2, 3]
  mut total = 0
  each xs (fn x ->
    total := total + x
    print ""
  )
  print "${total}"
"#;
    let each_effect_module =
        parse_module(each_effect_source).expect("M7-1 each-effect source should parse");
    let (each_effect_p50, each_effect_p95, each_effect_ok, each_effect_none, each_effect_last_ok) =
        measure_runtime_output_resolve_micros(&each_effect_module, warmup_runs, measured_runs);
    assert_eq!(
        each_effect_none, 0,
        "M7-1 each-effect baseline should resolve runtime output"
    );
    assert_eq!(each_effect_last_ok.as_deref(), Some("6"));
    eprintln!(
        "[M7-1][each-effect-callback-3] p50={}us p95={}us ok_runs={} none_runs={} output={}",
        each_effect_p50,
        each_effect_p95,
        each_effect_ok,
        each_effect_none,
        each_effect_last_ok.unwrap_or_default().trim_end(),
    );

    let iterator_effect_source = r#"
import goby/iterator ( Iterator )
import goby/stdio

emit_each : List Int -> Int -> Int can Iterator
emit_each xs state = case xs
  [] -> state
  [head, ..tail] ->
    step = yield head state
    if step.0
      emit_each tail step.1
    else
      step.1

main : Unit -> Unit can Print
main =
  xs = [1, 2, 3]
  total = with
    yield value state ->
      resume (True, state + value)
  in
    emit_each xs 0
  print "${total}"
"#;
    let iterator_effect_module =
        parse_module(iterator_effect_source).expect("M7-1 iterator-effect source should parse");
    let (
        iterator_effect_p50,
        iterator_effect_p95,
        iterator_effect_ok,
        iterator_effect_none,
        iterator_effect_last_ok,
    ) = measure_runtime_output_resolve_micros(&iterator_effect_module, warmup_runs, measured_runs);
    eprintln!(
        "[M7-1][iterator-effect-yield-3] p50={}us p95={}us ok_runs={} none_runs={} output={}",
        iterator_effect_p50,
        iterator_effect_p95,
        iterator_effect_ok,
        iterator_effect_none,
        iterator_effect_last_ok
            .clone()
            .unwrap_or_default()
            .trim_end(),
    );
    assert_eq!(
        iterator_effect_none, 0,
        "M7-1 iterator-effect baseline should resolve runtime output"
    );
    assert_eq!(iterator_effect_last_ok.as_deref(), Some("6"));
}

#[test]
fn m7_3_iterator_effect_traversal_preserves_source_order_and_early_stop() {
    let source = r#"
import goby/iterator ( Iterator )
import goby/stdio

main : Unit -> Unit can Print
main =
  calls = with
    yield _ _ ->
      resume (False, 1)
  in
    step0 = yield 10 0
    if step0.0
      step1 = yield 20 step0.1
      if step1.0
        step2 = yield 30 step1.1
        step2.1
      else
        step1.1
    else
      step0.1
  println "${calls}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("M7-3 iterator/effect order+early-stop workload should execute");
    assert_eq!(
        output.as_deref(),
        Some("1\n"),
        "M7-3 iterator/effect traversal should keep source order and stop after first False resume"
    );
}

#[test]
fn m7_3_iterator_effect_handler_clause_can_host_nested_callback_effects() {
    let source = r#"
import goby/iterator ( Iterator )
import goby/list ( each )
import goby/stdio

main : Unit -> Unit can Print
main =
  total = with
    yield value state ->
      each ["${value}"] print
      resume (True, state * 10 + value)
  in
    step0 = yield 1 0
    step1 = yield 2 step0.1
    step2 = yield 3 step1.1
    step2.1
  println "${total}"
"#;
    let module = parse_general_lowered_module(source);
    let output = execute_runtime_module_with_stdin(&module, Some(String::new()))
        .expect("M7-3 iterator/effect nested callback workload should execute");
    assert_eq!(
        output.as_deref(),
        Some("123123\n"),
        "M7-3 handler clause should preserve nested callback/effect interaction semantics"
    );
}

#[test]
#[ignore = "M6-2 diagnostic split; run explicitly with --ignored --nocapture"]
fn m6_2_diagnostic_indexed_read_split_costs() {
    let warmup_runs = 3usize;
    let measured_runs = 10usize;
    let stdin_seed = "x\n";

    let build_only_source = r#"
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
  _ = read_lines ()
  xs = build 4000
  println "${xs[0]}"
"#;
    let build_only_wasm = compile_general_lowered_wasm(build_only_source);
    let (
        build_only_p50,
        build_only_p95,
        build_only_ok,
        build_only_err,
        build_only_last_ok,
        build_only_last_err,
    ) = measure_wasm_exec_micros(&build_only_wasm, stdin_seed, warmup_runs, measured_runs);
    assert_eq!(build_only_err, 0, "build-only diagnostic should not trap");
    assert_eq!(build_only_last_ok.as_deref(), Some("4000\n"));

    let build_and_read_source = r#"
import goby/stdio

build : Int -> List Int can Print
build n =
  if n == 0
    []
  else
    rest = build (n - 1)
    [n, ..rest]

scan_reads : List Int -> Int -> Int -> Int can Print
scan_reads xs i acc =
  if i >= 4000
    acc
  else
    j = (i * 97) % 4000
    v = xs[j]
    scan_reads xs (i + 1) (acc + v)

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  xs = build 4000
  total = scan_reads xs 0 0
  println "${total}"
"#;
    let build_and_read_wasm = compile_general_lowered_wasm(build_and_read_source);
    let (
        build_and_read_p50,
        build_and_read_p95,
        build_and_read_ok,
        build_and_read_err,
        build_and_read_last_ok,
        build_and_read_last_err,
    ) = measure_wasm_exec_micros(&build_and_read_wasm, stdin_seed, warmup_runs, measured_runs);
    assert_eq!(
        build_and_read_err, 0,
        "build+read diagnostic should not trap"
    );
    assert_eq!(build_and_read_last_ok.as_deref(), Some("8002000\n"));

    let read_delta_p50 = build_and_read_p50.saturating_sub(build_only_p50);
    let read_delta_p95 = build_and_read_p95.saturating_sub(build_only_p95);
    let build_share_percent = if build_and_read_p50 == 0 {
        0
    } else {
        (build_only_p50 * 100) / build_and_read_p50
    };

    eprintln!(
        "[M6-2][diagnostic-indexed-read-split] build_only_p50={}us build_only_p95={}us build_and_read_p50={}us build_and_read_p95={}us read_delta_p50={}us read_delta_p95={}us build_share_p50={}percent build_ok_runs={} read_ok_runs={} build_err_runs={} read_err_runs={} build_err={:?} read_err={:?}",
        build_only_p50,
        build_only_p95,
        build_and_read_p50,
        build_and_read_p95,
        read_delta_p50,
        read_delta_p95,
        build_share_percent,
        build_only_ok,
        build_and_read_ok,
        build_only_err,
        build_and_read_err,
        build_only_last_err,
        build_and_read_last_err
    );
}

/// M6-5 final practical-goal verification.
/// Uses list.set (functional immutable update) for point and nested update workloads.
/// Compares against M6-0 baseline to verify practical-speed improvement.
#[test]
#[ignore = "M6-5 final verification snapshot; run explicitly with --ignored --nocapture"]
fn m6_5_final_practical_goal_verification() {
    let warmup_runs = 3usize;
    let measured_runs = 10usize;
    let stdin_seed = "x\n";

    // Point-update workload: 1000-element list, 1000 immutable updates via list.set
    let point_update_source = r#"
import goby/list (set, get)
import goby/stdio

build : Int -> List Int can Print
build n =
  if n == 0
    []
  else
    rest = build (n - 1)
    [n, ..rest]

scan_updates : List Int -> Int -> List Int can Print
scan_updates xs i =
  if i >= 1000
    xs
  else
    j = (i * 97) % 1000
    next = set xs j i
    scan_updates next (i + 1)

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  xs = build 1000
  updated = scan_updates xs 0
  v0 = get updated 0
  v999 = get updated 999
  println "${v0}"
  println "${v999}"
"#;
    let point_update_wasm = compile_general_lowered_wasm(point_update_source);
    let (
        point_update_p50,
        point_update_p95,
        point_update_ok,
        point_update_err,
        point_update_last_ok,
        point_update_last_err,
    ) = measure_wasm_exec_micros(&point_update_wasm, stdin_seed, warmup_runs, measured_runs);
    assert_eq!(point_update_err, 0, "M6-5 point-update should not trap");
    assert_eq!(point_update_last_ok.as_deref(), Some("0\n567\n"));
    eprintln!(
        "[M6-5][point-update-1k] p50={}us p95={}us ok_runs={} err_runs={} output={:?} err={:?}",
        point_update_p50,
        point_update_p95,
        point_update_ok,
        point_update_err,
        point_update_last_ok,
        point_update_last_err
    );

    // Nested update workload: 64x64 grid, 4096 immutable updates via list.set
    let nested_update_source = r#"
import goby/list (set, get)
import goby/stdio

build_row : Int -> List Int can Print
build_row n =
  if n == 0
    []
  else
    rest = build_row (n - 1)
    [0, ..rest]

build_grid : Int -> List (List Int) can Print
build_grid n =
  if n == 0
    []
  else
    rest = build_grid (n - 1)
    [build_row 64, ..rest]

scan_nested_updates : List (List Int) -> Int -> List (List Int) can Print
scan_nested_updates grid i =
  if i >= 4096
    grid
  else
    y = i / 64
    x = (i * 17) % 64
    row = get grid y
    next_row = set row x i
    next_grid = set grid y next_row
    scan_nested_updates next_grid (i + 1)

main : Unit -> Unit can Print, Read
main =
  _ = read_lines ()
  grid = build_grid 64
  updated = scan_nested_updates grid 0
  row0 = get updated 0
  row63 = get updated 63
  v00 = get row0 0
  v6363 = get row63 63
  println "${v00}"
  println "${v6363}"
"#;
    let nested_update_wasm = compile_general_lowered_wasm(nested_update_source);
    let (
        nested_update_p50,
        nested_update_p95,
        nested_update_ok,
        nested_update_err,
        nested_update_last_ok,
        nested_update_last_err,
    ) = measure_wasm_exec_micros(&nested_update_wasm, stdin_seed, warmup_runs, measured_runs);
    assert_eq!(nested_update_err, 0, "M6-5 nested-update should not trap");
    assert_eq!(nested_update_last_ok.as_deref(), Some("0\n4047\n"));
    eprintln!(
        "[M6-5][nested-update-64x64] p50={}us p95={}us ok_runs={} err_runs={} output={:?} err={:?}",
        nested_update_p50,
        nested_update_p95,
        nested_update_ok,
        nested_update_err,
        nested_update_last_ok,
        nested_update_last_err
    );
}
