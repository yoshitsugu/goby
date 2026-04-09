use std::path::PathBuf;

use goby_core::parse_module;
use wasmparser::{Operator, Parser, Payload, Validator};

use super::*;

fn assert_valid_wasm_module(wasm: &[u8]) {
    assert!(wasm.len() >= 8, "module too short: {} bytes", wasm.len());
    assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d], "bad wasm magic");
    assert_eq!(&wasm[4..8], &[0x01, 0x00, 0x00, 0x00], "bad wasm version");
    Validator::new()
        .validate_all(wasm)
        .expect("module should pass wasm validation");
}

fn import_function_count(wasm: &[u8]) -> u32 {
    let mut count = 0u32;
    for payload in Parser::new(0).parse_all(wasm) {
        let Ok(payload) = payload else { continue };
        if let Payload::ImportSection(reader) = payload {
            for import in reader {
                let Ok(import) = import else { continue };
                match import {
                    wasmparser::Imports::Single(_, import) => {
                        if matches!(
                            import.ty,
                            wasmparser::TypeRef::Func(_) | wasmparser::TypeRef::FuncExact(_)
                        ) {
                            count += 1;
                        }
                    }
                    wasmparser::Imports::Compact1 { items, .. } => {
                        for item in items {
                            let Ok(item) = item else { continue };
                            if matches!(
                                item.ty,
                                wasmparser::TypeRef::Func(_) | wasmparser::TypeRef::FuncExact(_)
                            ) {
                                count += 1;
                            }
                        }
                    }
                    wasmparser::Imports::Compact2 { ty, names, .. } => {
                        if matches!(
                            ty,
                            wasmparser::TypeRef::Func(_) | wasmparser::TypeRef::FuncExact(_)
                        ) {
                            count += names.count();
                        }
                    }
                }
            }
        }
    }
    count
}

fn all_code_body_ops(wasm: &[u8]) -> Vec<Vec<Operator<'_>>> {
    let mut bodies = Vec::new();
    for payload in Parser::new(0).parse_all(wasm) {
        let Ok(payload) = payload else { continue };
        if let Payload::CodeSectionEntry(body) = payload {
            let mut ops = Vec::new();
            let reader = body
                .get_operators_reader()
                .expect("operators reader should be available");
            for op in reader {
                ops.push(op.expect("operator should decode"));
            }
            bodies.push(ops);
        }
    }
    bodies
}

fn read_example(name: &str) -> String {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("..");
    path.push("..");
    path.push("examples");
    path.push(name);
    std::fs::read_to_string(path).expect("example file should exist")
}

fn read_runtime_io_general_lowering_fixture(name: &str) -> String {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push("fixtures");
    path.push("runtime-io-general-lowering");
    path.push(name);
    std::fs::read_to_string(&path).unwrap_or_else(|e| {
        panic!(
            "runtime-io general-lowering fixture '{}' should exist: {}",
            name, e
        )
    })
}

#[test]
fn compile_module_self_tail_tail_decl_call_emits_looped_helper_without_recursive_call() {
    let source = r#"
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
  count_down 1000
  println "done"
"#;
    let module = parse_module(source).expect("source should parse");
    let wasm = compile_module(&module).expect("module should compile");

    let import_count = import_function_count(&wasm);
    let helper_func_idx = import_count + 1;
    let bodies = all_code_body_ops(&wasm);
    let helper_ops = &bodies[1];
    assert_valid_wasm_module(&wasm);
    assert!(
        helper_ops
            .iter()
            .any(|op| matches!(op, Operator::Loop { .. })),
        "self-tail helper should contain a looped execution body, got: {helper_ops:?}"
    );
    assert!(
        !helper_ops.iter().any(|op| matches!(
            op,
            Operator::Call { function_index } if *function_index == helper_func_idx
        )),
        "self-tail helper should not call itself recursively after RR-5 loop emission, got: {helper_ops:?}"
    );
}

#[test]
fn native_codegen_capability_checker_rejects_hello_effect_boundary_subset() {
    let source = read_example("hello.gb");
    let module = parse_module(&source).expect("hello.gb should parse");
    assert!(
        !fallback::supports_native_codegen(&module),
        "hello.gb should be rejected by native capability checker when main is EffectBoundary"
    );
    assert_eq!(
        fallback::native_unsupported_reason_kind(&module),
        Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported)
    );
    assert_eq!(
        fallback::native_unsupported_reason(&module),
        Some("call_target_body_not_native_supported")
    );
}

#[test]
fn native_codegen_capability_checker_rejects_effect_example() {
    let source = read_example("effect.gb");
    let module = parse_module(&source).expect("effect.gb should parse");
    assert!(
        !fallback::supports_native_codegen(&module),
        "effect.gb should remain on fallback path in Phase 0"
    );
    assert_eq!(
        fallback::native_unsupported_reason_kind(&module),
        Some(fallback::UnsupportedReason::MainAnnotationNotUnitToUnit),
        "effect example should expose typed fallback reason"
    );
    assert_eq!(
        fallback::native_unsupported_reason(&module),
        Some("main_annotation_not_unit_to_unit"),
        "effect example should expose explicit fallback reason"
    );
}

#[test]
fn compile_module_emits_valid_wasm_for_phase1_subset_via_fallback() {
    let source = read_example("hello.gb");
    let module = parse_module(&source).expect("hello.gb should parse");
    assert!(
        !fallback::supports_native_codegen(&module),
        "phase-1 hello subset should take fallback path because main is EffectBoundary"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn native_codegen_capability_checker_accepts_phase2_int_bool_subset() {
    let source = r#"
main : Unit -> Unit
main =
  x = 6 * 7
  ok = x == 42
  print x
  print ok
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        fallback::supports_native_codegen(&module),
        "phase-2 int/bool subset should be accepted by native capability checker"
    );
}

#[test]
fn compile_module_uses_native_emitter_for_phase2_int_bool_subset() {
    let source = r#"
main : Unit -> Unit
main =
  x = 6 * 7
  ok = x == 42
  print x
  print ok
"#;
    let module = parse_module(source).expect("source should parse");
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "42True");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_supports_extended_int_operator_family() {
    let source = r#"
main : Unit -> Unit
main =
  println (1 + 1)
  println (5 - 1)
  println (5 / 2)
  println (3 * 3)
  println (5 % 2)
  println (3 > 1 + 1)
  println (2 < 1 - 1)
  println (8 >= 1 + 3)
  println (8 <= 5 - 3)
  println (2 == 1 + 1)
  println (2 == 1 - 1)
  println (True || False)
  println (!False)
"#;
    let module = parse_module(source).expect("source should parse");
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(
        expected_text,
        "2\n4\n2\n9\n1\nTrue\nFalse\nTrue\nFalse\nTrue\nFalse\nTrue\nTrue\n"
    );
    assert_valid_wasm_module(&wasm);
}

#[test]
fn native_codegen_capability_checker_accepts_mut_binding_without_assignment() {
    let source = r#"
main : Unit -> Unit
main =
  mut x = 1
  print x
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        fallback::supports_native_codegen(&module),
        "mut binding without assignment should use the native direct path"
    );
    assert_eq!(fallback::native_unsupported_reason_kind(&module), None);
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "1");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn native_codegen_capability_checker_reports_assignment_as_backend_limitation() {
    let source = r#"
main : Unit -> Unit
main =
  mut x = 1
  x := 2
  print x
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        fallback::native_unsupported_reason_kind(&module),
        Some(fallback::UnsupportedReason::MutableAssignmentNotNativeSupported),
        "assignment should be reported as a native backend limitation"
    );
    assert_eq!(
        fallback::native_unsupported_reason(&module),
        Some("mutable_assignment_not_native_supported")
    );
}

#[test]
fn native_codegen_capability_checker_accepts_direct_function_call_subset() {
    let source = r#"
double : Int -> Int
double n = n * 2

main : Unit -> Unit
main =
  print (double 21)
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        fallback::supports_native_codegen(&module),
        "direct single-arg function call subset should be accepted"
    );
}

#[test]
fn compile_module_uses_native_emitter_for_direct_function_call_subset() {
    let source = r#"
double : Int -> Int
double n = n * 2

main : Unit -> Unit
main =
  print (double 21)
"#;
    let module = parse_module(source).expect("source should parse");
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "42");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_scan_loop_lowering_eliminates_walk_self_call_in_wasm() {
    let source = r#"
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
    let module = parse_module(source).expect("source should parse");
    let (main_instrs, aux_decls) = crate::gen_lower::lower_module_to_instrs(&module)
        .expect("lowering should not hard-fail")
        .expect("general lowering should accept recursive scan module");
    let walk_aux = aux_decls
        .iter()
        .find(|decl| decl.decl_name == "walk")
        .expect("walk aux decl should exist");
    assert!(
        walk_aux.instrs.iter().any(|instr| matches!(
            instr,
            crate::gen_lower::backend_ir::WasmBackendInstr::Loop { .. }
        )),
        "walk should lower to backend loop, got aux decls: {aux_decls:?}; main: {main_instrs:?}"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);

    let imported = import_function_count(&wasm);
    let bodies = all_code_body_ops(&wasm);
    let loop_body = bodies
        .iter()
        .enumerate()
        .find(|(_, ops)| ops.iter().any(|op| matches!(op, Operator::Loop { .. })))
        .expect("one defined function body should contain a loop after RR-3 lowering");
    let loop_func_index = imported + loop_body.0 as u32;
    let loop_ops = loop_body.1;

    assert!(
        !loop_ops.iter().any(
            |op| matches!(op, Operator::Call { function_index } if *function_index == loop_func_index)
        ),
        "loop-lowered body should not directly call itself after RR-3 lowering, ops: {loop_ops:?}"
    );
}

#[test]
fn compile_module_list_spread_builder_lowering_validates_and_eliminates_build_self_call() {
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
  _ = read()
  xs = build 32
  println "${xs[0]}"
"#;
    let module = parse_module(source).expect("source should parse");
    let (main_instrs, aux_decls) = crate::gen_lower::lower_module_to_instrs(&module)
        .expect("lowering should not hard-fail")
        .expect("general lowering should accept recursive list-spread module");
    let build_aux = aux_decls
        .iter()
        .find(|decl| decl.decl_name == "build")
        .expect("build aux decl should exist");
    assert!(
        build_aux.instrs.iter().any(|instr| matches!(
            instr,
            crate::gen_lower::backend_ir::WasmBackendInstr::ListBuilderNew { .. }
        )),
        "build should lower to builder-backed loop, got aux decls: {aux_decls:?}; main: {main_instrs:?}"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);

    let imported = import_function_count(&wasm);
    let bodies = all_code_body_ops(&wasm);
    let loop_body = bodies
        .iter()
        .enumerate()
        .find(|(_, ops)| ops.iter().any(|op| matches!(op, Operator::Loop { .. })))
        .expect("one defined function body should contain a loop after RR-4 lowering");
    let loop_func_index = imported + loop_body.0 as u32;
    let loop_ops = loop_body.1;

    assert!(
        !loop_ops.iter().any(
            |op| matches!(op, Operator::Call { function_index } if *function_index == loop_func_index)
        ),
        "loop-lowered builder body should not directly call itself after RR-4 lowering, ops: {loop_ops:?}"
    );
}

#[test]
fn compile_module_inline_fold_prepend_lowering_rewrites_concat_chain_in_main() {
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
  seed = build 32
  xs =
    fold seed [] (fn acc x ->
      [x, ..acc]
    )
  println "${xs[0]}"
"#;
    let module = parse_module(source).expect("source should parse");
    let (main_instrs, aux_decls) = crate::gen_lower::lower_module_to_instrs(&module)
        .expect("lowering should not hard-fail")
        .expect("general lowering should accept inline fold prepend module");
    assert!(
        main_instrs.iter().any(|instr| matches!(
            instr,
            crate::gen_lower::backend_ir::WasmBackendInstr::ListReverseFoldPrepend { .. }
        )),
        "main should lower inline fold prepend builder to dedicated reverse-fold instruction, got main: {main_instrs:?}; aux: {aux_decls:?}"
    );
    let rendered = format!("{main_instrs:?}");
    assert!(
        !rendered.contains("DeclCall { decl_name: \"fold\" }"),
        "specialized inline fold prepend lowering should eliminate direct stdlib fold call, got: {rendered}"
    );
    assert!(
        !rendered.contains("ListConcat"),
        "specialized inline fold prepend lowering should eliminate callback ListConcat chain, got: {rendered}"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_named_fold_prepend_lowering_rewrites_decl_callback_chain_in_main() {
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

prepend : List Int -> Int -> List Int can Print
prepend acc x =
  [x, ..acc]

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  seed = build 32
  xs = fold seed [] prepend
  println "${xs[0]}"
"#;
    let module = parse_module(source).expect("source should parse");
    let (main_instrs, aux_decls) = crate::gen_lower::lower_module_to_instrs(&module)
        .expect("lowering should not hard-fail")
        .expect("general lowering should accept named fold prepend module");
    assert!(
        main_instrs.iter().any(|instr| matches!(
            instr,
            crate::gen_lower::backend_ir::WasmBackendInstr::ListReverseFoldPrepend { .. }
        )),
        "main should lower named fold prepend builder to dedicated reverse-fold instruction, got main: {main_instrs:?}; aux: {aux_decls:?}"
    );
    let rendered = format!("{main_instrs:?}");
    assert!(
        !rendered.contains("DeclCall { decl_name: \"fold\" }"),
        "named callback rewrite should eliminate direct stdlib fold call, got: {rendered}"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_local_alias_fold_prepend_lowering_rewrites_decl_callback_chain_in_main() {
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

prepend : List Int -> Int -> List Int can Print
prepend acc x =
  [x, ..acc]

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  seed = build 32
  f = prepend
  xs = fold seed [] f
  println "${xs[0]}"
"#;
    let module = parse_module(source).expect("source should parse");
    let (main_instrs, aux_decls) = crate::gen_lower::lower_module_to_instrs(&module)
        .expect("lowering should not hard-fail")
        .expect("general lowering should accept local alias fold prepend module");
    assert!(
        main_instrs.iter().any(|instr| matches!(
            instr,
            crate::gen_lower::backend_ir::WasmBackendInstr::ListReverseFoldPrepend { .. }
        )),
        "main should lower local alias fold prepend builder to dedicated reverse-fold instruction, got main: {main_instrs:?}; aux: {aux_decls:?}"
    );
    let rendered = format!("{main_instrs:?}");
    assert!(
        !rendered.contains("DeclCall { decl_name: \"fold\" }"),
        "local alias callback rewrite should eliminate direct stdlib fold call, got: {rendered}"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_uses_native_emitter_for_multi_arg_direct_function_call_subset() {
    let source = r#"
add4 : Int -> Int -> Int -> Int -> Int
add4 a b c d = a + b + c + d

main : Unit -> Unit
main =
  print (add4 1 2 3 4)
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        fallback::supports_native_codegen(&module),
        "multi-arg direct function call subset should be accepted"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_uses_native_emitter_for_function_example_first_order_subset() {
    let source = r#"
add_ten : Int -> Int
add_ten x = x + 10

add_ten_mul_three : Int -> Int
add_ten_mul_three a =
  b = a + 10
  b * 3

main : Unit -> Unit
main =
  b = add_ten 10
  c = add_ten_mul_three b
  print c
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        fallback::supports_native_codegen(&module),
        "first-order subset derived from function.gb should be accepted"
    );
    assert_eq!(fallback::native_unsupported_reason_kind(&module), None);
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "90");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn native_codegen_ignores_unused_hof_declaration() {
    let source = r#"
import goby/list ( map )

mul_tens : List Int -> List Int
mul_tens ns = map ns (fn n -> n * 10)

main : Unit -> Unit
main =
  print 42
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        fallback::supports_native_codegen(&module),
        "unused HOF declaration should not block native path"
    );
    assert_eq!(fallback::native_unsupported_reason_kind(&module), None);
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "42");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn native_codegen_accepts_transitively_required_hof_declaration() {
    let source = r#"
import goby/list ( map )

mul_tens : List Int -> List Int
mul_tens ns = map ns (fn n -> n * 10)

wrapped_mul_tens : List Int -> List Int
wrapped_mul_tens ns = mul_tens ns

main : Unit -> Unit
main =
  print (wrapped_mul_tens [1, 2])
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        fallback::supports_native_codegen(&module),
        "transitively required HOF declaration should be accepted by native lowering"
    );
    assert_eq!(fallback::native_unsupported_reason_kind(&module), None);
    assert_eq!(fallback::native_unsupported_reason(&module), None);
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "[10, 20]");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_uses_native_emitter_for_list_int_print_subset() {
    let source = r#"
main : Unit -> Unit
main =
  xs = [1, 2, 3]
  print xs
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        fallback::supports_native_codegen(&module),
        "list-int print subset should be accepted by native capability checker"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "[1, 2, 3]");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_uses_native_emitter_for_list_int_pipeline_print_subset() {
    let source = r#"
main : Unit -> Unit
main =
  [4, 5, 6] |> print
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        fallback::supports_native_codegen(&module),
        "list-int pipeline print subset should be accepted by native capability checker"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "[4, 5, 6]");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_uses_native_emitter_for_if_print_subset() {
    let source = r#"
main : Unit -> Unit
main =
  a = 10
  b = 20
  print
    if a + b == 30
      "30"
    else
      "other"
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        fallback::supports_native_codegen(&module),
        "if-print subset should be accepted by native capability checker"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "30");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_uses_native_emitter_for_case_print_subset() {
    let source = r#"
main : Unit -> Unit
main =
  x = 5
  print
    case x
      5 -> "Five!"
      3 -> "Three!"
      _ -> "Other"
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        fallback::supports_native_codegen(&module),
        "case-print subset should be accepted by native capability checker"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "Five!");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_emits_valid_wasm_for_control_flow_example_via_fallback() {
    let source = read_example("control_flow.gb");
    let module = parse_module(&source).expect("control_flow.gb should parse");
    assert!(
        !fallback::supports_native_codegen(&module),
        "control_flow.gb should take fallback path because main is EffectBoundary"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "Five!5030");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn native_codegen_capability_checker_accepts_function_example_with_hof_lambda() {
    let source = read_example("function.gb");
    let module = parse_module(&source).expect("function.gb should parse");
    assert!(
        !fallback::supports_native_codegen(&module),
        "function.gb now carries `main : Unit -> Unit can Print`, so it should take the fallback path"
    );
    assert_eq!(
        fallback::native_unsupported_reason_kind(&module),
        Some(fallback::UnsupportedReason::CallCalleeNotDirectName),
        "function.gb should report the higher-order call fallback reason"
    );
    assert_eq!(
        fallback::native_unsupported_reason(&module),
        Some("call_callee_not_direct_name"),
        "function.gb should report the higher-order call fallback reason"
    );
}

#[test]
fn native_codegen_capability_checker_reports_expected_call_reasons() {
    let cases = [
        (
            "non_direct_callee",
            r#"
main : Unit -> Unit
main =
  print (Foo.bar 1)
"#,
            Some(fallback::UnsupportedReason::CallCalleeNotDirectName),
        ),
        (
            "arity_mismatch",
            r#"
id : Int -> Int
id x = x

main : Unit -> Unit
main =
  print (id 1 2)
"#,
            Some(fallback::UnsupportedReason::CallArityMismatch),
        ),
        (
            "target_missing",
            r#"
main : Unit -> Unit
main =
  print (unknown 1)
"#,
            Some(fallback::UnsupportedReason::CallTargetNotDeclaration),
        ),
        (
            "target_body_not_supported",
            r#"
uses_with : Int -> Int
uses_with x =
  with
    log v ->
      resume ()
  in
    x

main : Unit -> Unit
main =
  print (uses_with 1)
"#,
            Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
        ),
        (
            "target_body_not_supported_with_lambda",
            r#"
uses_with_callback : (Int -> Int) -> Int
uses_with_callback f =
  with
    log v ->
      resume ()
  in
    f 1

main : Unit -> Unit
main =
  print (uses_with_callback (fn x -> x + 1))
"#,
            Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
        ),
        (
            "target_body_not_supported_due_to_effect_boundary",
            r#"
tick : Int -> Int can Tick
tick n = n

main : Unit -> Unit
main =
  print (tick 1)
"#,
            Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
        ),
    ];

    for (name, source, expected_kind) in cases {
        let module = parse_module(source).expect("source should parse");
        let expected_str = expected_kind.map(fallback::UnsupportedReason::as_str);
        assert_eq!(
            fallback::native_unsupported_reason_kind(&module),
            expected_kind,
            "unexpected typed fallback reason for case: {}",
            name
        );
        assert_eq!(
            fallback::native_unsupported_reason(&module),
            expected_str,
            "unexpected fallback reason for case: {}",
            name
        );
    }
}

#[test]
fn native_codegen_capability_checker_prioritizes_body_reason_over_arity_mismatch() {
    let source = r#"
uses_with : Int -> Int
uses_with x =
  with
    log v ->
      resume ()
  in
    x

main : Unit -> Unit
main =
  print (uses_with 1 2)
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        fallback::native_unsupported_reason_kind(&module),
        Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
        "typed reason should prefer unsupported declaration body over arity mismatch"
    );
    assert_eq!(
        fallback::native_unsupported_reason(&module),
        Some("call_target_body_not_native_supported"),
        "unsupported declaration body reason should win when call-shape mismatch coexists"
    );
}

#[test]
fn native_fallback_path_matrix_for_examples() {
    let cases = [
        (
            "hello.gb",
            false,
            Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
            Some("call_target_body_not_native_supported"),
        ),
        (
            "control_flow.gb",
            false,
            Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
            Some("call_target_body_not_native_supported"),
        ),
        (
            "effect.gb",
            false,
            Some(fallback::UnsupportedReason::MainAnnotationNotUnitToUnit),
            Some("main_annotation_not_unit_to_unit"),
        ),
        (
            "function.gb",
            false,
            Some(fallback::UnsupportedReason::CallCalleeNotDirectName),
            Some("call_callee_not_direct_name"),
        ),
    ];

    for (name, expect_native, expected_reason_kind, expected_reason) in cases {
        let source = read_example(name);
        let module = parse_module(&source).expect("example should parse");
        let reason_kind = fallback::native_unsupported_reason_kind(&module);
        let reason = fallback::native_unsupported_reason(&module);
        let supports_native = fallback::supports_native_codegen(&module);
        assert_eq!(
            reason_kind, expected_reason_kind,
            "unexpected typed fallback reason for {}",
            name
        );
        assert_eq!(
            reason, expected_reason,
            "unexpected fallback reason for {}",
            name
        );
        assert_eq!(
            supports_native, expect_native,
            "unexpected native capability result for {}",
            name
        );

        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }
}

// Round-trip tests: assert that classify_runtime_io returns DynamicWasiIo for known
// dynamic-Wasm shapes AND that compile_module produces valid Wasm for each.
// These tests mirror the StaticOutput round-trip tests below and confirm that the
// planning boundary (all shape decisions in runtime_io_plan.rs) is working end-to-end.

#[test]
fn compile_module_routes_echo_read_through_dynamic_wasi_io_classification() {
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  print (read())
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "print(read()) should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_accepts_mutable_nested_list_update_via_general_lowering() {
    let source = r#"
main : Unit -> Unit can Print
main =
  mut xs = [[1, 2], [3, 4]]
  xs[0][1] := 99
  println "${xs[0][1]}"
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "mutable nested-list program should classify by rooted-update capability"
    );
    let wasm = compile_module(&module).expect("mutable nested-list program should compile");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_preserves_mutable_nested_list_output_parity_with_fallback_runtime() {
    let source = r#"
main : Unit -> Unit can Print
main =
  mut xs = [[1, 2], [3, 4]]
  before = xs[1][1]
  xs[1][1] := 30
  println("${before}")
  println("${xs[1][0]},${xs[1][1]}")
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "mutable nested-list parity sample should classify as GeneralLowered"
    );
    let expected = resolve_module_runtime_output(&module)
        .expect("fallback runtime should resolve mutable nested-list output");
    let wasm = compile_module(&module).expect("mutable nested-list parity sample should compile");
    assert_valid_wasm_module(&wasm);
    let actual = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
        .expect("compiled Wasm should execute mutable nested-list parity sample");
    assert_eq!(actual, expected);
}

#[test]
fn compile_module_routes_echo_read_line_println_through_dynamic_wasi_io_classification() {
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  println (read_line())
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "println(read_line()) should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_routes_echo_read_all_println_through_dynamic_wasi_io_classification() {
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  println (read())
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "println(read()) should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_routes_echo_read_line_print_through_dynamic_wasi_io_classification() {
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  print (read_line())
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "print(read_line()) should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_routes_echo_with_static_suffixes_through_dynamic_wasi_io_classification() {
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  line = read_line()
  println line
  print "done"
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "read_line echo with static suffix should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_routes_echo_with_output_alias_through_dynamic_wasi_io_classification() {
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  printer = print
  copied = text
  printer copied
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "read echo with local output alias should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_general_lowers_qualified_read_with_static_suffixes() {
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  text = Read.read ()
  Print.println text
  Print.print "done"
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "qualified read + static suffixes should route through the general lowering path"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_interpolated_read_transform_is_general_lowered() {
    // Interp lowering is now supported. "${text}!" with a dynamic Var
    // lowers via StringConcat and classifies as GeneralLowered.
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  decorated = "${text}!"
  print decorated
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "interpolated read transform should now classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_general_lowers_import_example() {
    let source = read_example("import.gb");
    let module = parse_module(&source).expect("import.gb should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::NotRuntimeIo,
        "import.gb is a compile-path parity target, not a runtime-stdin program"
    );
    let wasm = compile_module(&module).expect("import.gb should compile to Wasm");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_compiles_transformed_split_callback_as_general_lowered() {
    // This shape used to miss the general-lowered path.
    // It now classifies as GeneralLowered because lambda lowering is supported.
    let source = r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  each lines (fn line -> println "${line}!")
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "transformed split callback should now classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("transformed split callback should compile to Wasm");
    assert_valid_wasm_module(&wasm);
}

/// Milestone test: tracks which shapes are DynamicWasiIo, Unsupported, etc.
/// InterpreterBridge is intentionally absent from the cases below: the
/// transformed split-callback family (formerly the only bridge-only shape) has
/// been promoted to DynamicWasiIo.  The bridge surface is now effectively empty.
#[test]
fn runtime_io_milestone_bridge_surface_is_narrow_and_explicit() {
    let cases = [
        (
            "dynamic_echo",
            r#"
main : Unit -> Unit can Print, Read
main =
  print (read())
"#,
            crate::RuntimeIoExecutionKind::GeneralLowered,
        ),
        (
            "dynamic_echo_alias",
            r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  printer = print
  printer text
"#,
            crate::RuntimeIoExecutionKind::GeneralLowered,
        ),
        (
            // Lambda lowering now handles `fn line -> println "${line}"`,
            // so this program is promoted from DynamicWasiIo to GeneralLowered.
            "dynamic_split_passthrough",
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  lines = split(text, "\n")
  each lines (fn line -> println "${line}")
"#,
            crate::RuntimeIoExecutionKind::GeneralLowered,
        ),
        (
            // Same as above with a suffix — now GeneralLowered.
            "dynamic_transformed_split_callback",
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  lines = split(text, "\n")
  each lines (fn line -> println "${line}!")
"#,
            crate::RuntimeIoExecutionKind::GeneralLowered,
        ),
        (
            "general_lowered_interp_read_transform",
            r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  decorated = "${text}!"
  print decorated
"#,
            // Interp lowering is now supported via StringConcat.
            crate::RuntimeIoExecutionKind::GeneralLowered,
        ),
        (
            "unsupported_mixed_read_line_then_read",
            r#"
main : Unit -> Unit can Print, Read
main =
  first = read_line()
  rest = read()
  print "${first}|${rest}"
"#,
            crate::RuntimeIoExecutionKind::Unsupported,
        ),
    ];

    for (name, source, expected) in cases {
        let module = parse_module(source).expect("source should parse");
        assert_eq!(
            runtime_io_execution_kind(&module).expect("classification should succeed"),
            expected,
            "unexpected runtime I/O execution kind for {}",
            name
        );
    }
}

#[test]
fn compile_module_routes_split_lines_each_through_dynamic_wasi_io_classification() {
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
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "read + split + each println should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_general_lowers_bare_split_index_println_program() {
    let source = r#"
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  println(lines[1])
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "bare read + bare split + list index + bare println should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_routes_non_runtime_io_program_through_fallback_resolution() {
    let source = r#"
main : Unit -> Unit can Print
main =
  msg = "hello"
  println msg
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::NotRuntimeIo,
        "bound println should classify as NotRuntimeIo rather than StaticOutput"
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    let expected_text =
        resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(expected_text, "hello\n");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn runtime_io_execution_kind_general_lowers_supported_non_fused_helpers() {
    let source = r#"
import goby/string

main : Unit -> Unit can Print, Read
main =
  text = read ()
  _lines = string.split text "\n"
  print "ok"
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "supported helper calls should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("supported helper emission should compile");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_routes_print_literal_through_static_output_classification() {
    let source = r#"
main : Unit -> Unit can Print
main =
  println "hello"
"#;
    let module = parse_module(source).expect("source should parse");
    // verify it is classified as StaticOutput
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::StaticOutput,
    );
    // verify it still produces valid Wasm
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_routes_multiple_print_literals_through_static_output() {
    let source = r#"
main : Unit -> Unit can Print
main =
  print "a"
  println "b"
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::StaticOutput,
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_routes_qualified_print_literals_through_static_output() {
    let source = r#"
main : Unit -> Unit can Print
main =
  Print.print "a"
  Print.println "b"
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::StaticOutput,
    );
    let wasm = compile_module(&module).expect("codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

// ---------------------------------------------------------------------------
// Runtime-I/O general-lowering fixtures
// These tests lock the convergence state of the representative read/split/index
// programs. They should classify as GeneralLowered rather than old handwritten
// RuntimeIoPlan categories.
// ---------------------------------------------------------------------------

/// Plain read+print fixture current status: GeneralLowered.
#[test]
fn runtime_io_general_lowering_print_read_all_current_classification() {
    let source = read_runtime_io_general_lowering_fixture("print_read_all.gb");
    let module = parse_module(&source).expect("print_read_all.gb should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "plain read+print fixture should report GeneralLowered once the general path is the semantic source of truth"
    );
}

/// Plain read+print fixture: `print (read())`.
#[test]
fn runtime_io_general_lowering_print_read_all_is_general_lowered() {
    let source = read_runtime_io_general_lowering_fixture("print_read_all.gb");
    let module = parse_module(&source).expect("print_read_all.gb should parse");
    let wasm = compile_module(&module).expect("plain read+print codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

/// Split+each fixture current status: GeneralLowered.
#[test]
fn runtime_io_general_lowering_split_lines_each_is_currently_unsupported() {
    let source = read_runtime_io_general_lowering_fixture("split_lines_each.gb");
    let module = parse_module(&source).expect("split_lines_each.gb should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "split+each fixture should report GeneralLowered once the fused general path owns this shape"
    );
}

/// Split+each fixture.
#[test]
fn runtime_io_general_lowering_split_lines_each_is_general_lowered() {
    let source = read_runtime_io_general_lowering_fixture("split_lines_each.gb");
    let module = parse_module(&source).expect("split_lines_each.gb should parse");
    let wasm = compile_module(&module).expect("split+each codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

// ---------------------------------------------------------------------------
// Split+each parity: programs produce valid Wasm with fd_read and fd_write.
// ---------------------------------------------------------------------------

/// Parity test: f4 fixture compiles via general path and has fd_read + fd_write imports.
#[test]
fn runtime_io_general_lowering_split_lines_each_has_fd_read_and_fd_write() {
    let source = read_runtime_io_general_lowering_fixture("split_lines_each.gb");
    let module = parse_module(&source).expect("split_lines_each.gb should parse");
    let wasm = compile_module(&module).expect("split+each codegen should succeed");
    assert_valid_wasm_module(&wasm);
    let fd_read = b"fd_read";
    let fd_write = b"fd_write";
    assert!(
        wasm.windows(fd_read.len()).any(|w| w == fd_read),
        "split+each general path should import fd_read"
    );
    assert!(
        wasm.windows(fd_write.len()).any(|w| w == fd_write),
        "split+each general path should import fd_write"
    );
}

/// Parity test: inline split+each form also compiles via general path.
#[test]
fn runtime_io_general_lowering_inline_split_lines_each_compiles() {
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  text = Read.read ()
  lines = string.split text "\n"
  each lines Print.println
"#;
    let module = parse_module(source).expect("inline f4 source should parse");
    let wasm = compile_module(&module).expect("inline split+each codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

/// Split+index fixture current status: GeneralLowered.
#[test]
fn runtime_io_general_lowering_split_lines_index_is_currently_unsupported() {
    let source = read_runtime_io_general_lowering_fixture("split_lines_index.gb");
    let module = parse_module(&source).expect("split_lines_index.gb should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "split+index fixture should report GeneralLowered once the fused general path owns this shape"
    );
}

/// Split+index fixture.
#[test]
fn runtime_io_general_lowering_split_lines_index_is_general_lowered() {
    let source = read_runtime_io_general_lowering_fixture("split_lines_index.gb");
    let module = parse_module(&source).expect("split_lines_index.gb should parse");
    let wasm = compile_module(&module).expect("split+index codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn runtime_io_general_lowering_split_lines_index_has_fd_read_and_fd_write() {
    let source = read_runtime_io_general_lowering_fixture("split_lines_index.gb");
    let module = parse_module(&source).expect("split_lines_index.gb should parse");
    let wasm = compile_module(&module).expect("split+index codegen should succeed");
    assert_valid_wasm_module(&wasm);
    let fd_read = b"fd_read";
    let fd_write = b"fd_write";
    assert!(
        wasm.windows(fd_read.len()).any(|w| w == fd_read),
        "split+index general path should import fd_read"
    );
    assert!(
        wasm.windows(fd_write.len()).any(|w| w == fd_write),
        "split+index general path should import fd_write"
    );
}

#[test]
fn runtime_io_general_lowering_inline_split_lines_index_compiles() {
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  text = Read.read ()
  lines = string.split text "\n"
  line = list.get lines 1
  Print.println line
"#;
    let module = parse_module(source).expect("inline f5 source should parse");
    let wasm = compile_module(&module).expect("inline split+index codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_parity_list_index_sugar_matches_canonical_helper_spelling() {
    let sugar_source = r#"
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  println(lines[1])
"#;
    let helper_source = r#"
import goby/list ( get )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  line = get lines 1
  println line
"#;
    let sugar_module = parse_module(sugar_source).expect("sugar source should parse");
    let helper_module = parse_module(helper_source).expect("helper source should parse");
    assert_eq!(
        runtime_io_execution_kind(&sugar_module).expect("classification should succeed"),
        runtime_io_execution_kind(&helper_module).expect("classification should succeed"),
        "list index sugar and canonical helper spelling should converge at backend planning time"
    );
    let sugar_wasm = compile_module(&sugar_module).expect("sugar shape should compile");
    let helper_wasm = compile_module(&helper_module).expect("helper shape should compile");
    assert_valid_wasm_module(&sugar_wasm);
    assert_valid_wasm_module(&helper_wasm);
}

#[test]
fn compile_module_general_lowers_non_fused_split_and_list_get_helpers() {
    let source = r#"
import goby/string
import goby/list

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = string.split text "\n"
  line = list.get lines 1
  echoed = line
  println echoed
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
    );
    let wasm = compile_module(&module).expect("non-fused helper chain should compile");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_general_lowers_list_push_string_helper_chain() {
    let source = r#"
import goby/string
import goby/list

main : Unit -> Unit can Print, Read
main =
  text = read ()
  base = string.split text "\n"
  pushed = __goby_list_push_string base "tail"
  line = list.get pushed 2
  print line
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
    );
    let wasm = compile_module(&module).expect("list push helper chain should compile");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn runtime_io_execution_kind_routes_read_graphemes_program_to_general_lowered() {
    // E6: `graphemes(text)[N]` now lowers through the fused graphemes-index pattern
    // to `__goby_string_each_grapheme_state`, so classification is GeneralLowered
    // rather than InterpreterBridge.
    let source = r#"
import goby/string ( graphemes )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  parts = graphemes text
  println(parts[1])
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "graphemes + index pattern should now classify as GeneralLowered via fused lowering"
    );
    let wasm = compile_module(&module).expect("E6: graphemes + index program must compile to Wasm");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn runtime_io_execution_kind_general_lowers_read_split_map_graphemes_get_each_program() {
    let source = r#"
import goby/list ( each, map )
import goby/string ( split, graphemes )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  rolls = map lines graphemes
  each (rolls[2]) println
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "read -> split -> map(graphemes) -> list.get -> each(println) should classify as GeneralLowered"
    );
    let wasm =
        compile_module(&module).expect("composed read/split/map/graphemes/each should compile");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn runtime_io_execution_kind_general_lowers_read_split_map_graphemes_get_each_alias_variant() {
    let source = r#"
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
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "alias-chain variant should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("alias-chain variant should compile");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn runtime_io_execution_kind_general_lowers_read_split_map_graphemes_get_each_canonical_variant() {
    let source = r#"
import goby/list
import goby/string

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = string.split text "\n"
  rolls = list.map lines string.graphemes
  row2 = list.get rolls 2
  list.each row2 Print.println
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "canonical-qualified variant should classify as GeneralLowered"
    );
    let wasm = compile_module(&module).expect("canonical-qualified variant should compile");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn runtime_io_execution_kind_general_lowers_read_lines_map_graphemes_program() {
    let source = r#"
import goby/list ( each, map )
import goby/string ( graphemes )

main : Unit -> Unit can Print, Read
main =
  lines = read_lines ()
  rolls = map lines graphemes
  each (rolls[1]) println
"#;
    let module = parse_module(source).expect("source should parse");
    assert_eq!(
        runtime_io_execution_kind(&module).expect("classification should succeed"),
        crate::RuntimeIoExecutionKind::GeneralLowered,
        "read_lines -> map(graphemes) -> list.get -> each(println) should classify as GeneralLowered"
    );
    let wasm =
        compile_module(&module).expect("read_lines -> map(graphemes) program should compile");
    assert_valid_wasm_module(&wasm);
}

// ---------------------------------------------------------------------------
// Plain read+print parity: programs previously handled by handwritten runtime-I/O plans must
// produce valid Wasm through the general lowering path.
// ---------------------------------------------------------------------------

/// Parity test: `print (read())` compiles via general path and produces valid Wasm
/// with both fd_read and fd_write imports (same WASI interface as the old Echo path).
#[test]
fn runtime_io_general_lowering_print_read_all_has_fd_read_and_fd_write() {
    let source = read_runtime_io_general_lowering_fixture("print_read_all.gb");
    let module = parse_module(&source).expect("print_read_all.gb should parse");
    let wasm = compile_module(&module).expect("plain read+print codegen should succeed");
    assert_valid_wasm_module(&wasm);
    // Verify Wasm binary contains both "fd_read" and "fd_write" import name strings.
    let fd_read = b"fd_read";
    let fd_write = b"fd_write";
    assert!(
        wasm.windows(fd_read.len()).any(|w| w == fd_read),
        "plain read+print general path should import fd_read from WASI"
    );
    assert!(
        wasm.windows(fd_write.len()).any(|w| w == fd_write),
        "plain read+print general path should import fd_write from WASI"
    );
}

/// Parity test: inline `print (read())` program also compiles via general path.
#[test]
fn runtime_io_general_lowering_inline_print_read_all_compiles() {
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  text = Read.read ()
  Print.print text
"#;
    let module = parse_module(source).expect("inline f3 source should parse");
    let wasm = compile_module(&module).expect("inline plain read+print codegen should succeed");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn compile_module_parity_selective_prelude_read_print_compiles() {
    let source = r#"
import goby/prelude ( read, print )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  print text
"#;
    let module = parse_module(source).expect("prelude selective source should parse");
    let wasm = compile_module(&module)
        .expect("selective-import prelude read+print should compile via the general path");
    assert_valid_wasm_module(&wasm);
}

#[test]
fn runtime_io_general_lowering_f6_runtime_io_wasm_size_guardrails_hold() {
    let general_sources = [
        read_runtime_io_general_lowering_fixture("print_read_all.gb"),
        read_runtime_io_general_lowering_fixture("split_lines_each.gb"),
        read_runtime_io_general_lowering_fixture("split_lines_index.gb"),
    ];
    for source in general_sources {
        let module =
            parse_module(&source).expect("runtime-io general-lowering source should parse");
        let wasm =
            compile_module(&module).expect("runtime-io general-lowering source should compile");
        assert!(
            wasm.len() < 65_536,
            "general-lowered runtime I/O wasm unexpectedly large: {} bytes",
            wasm.len()
        );
    }

    let optimized_source = r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  each lines (fn line -> println "${line}!")
"#;
    let optimized_module = parse_module(optimized_source).expect("optimized source should parse");
    let optimized_wasm =
        compile_module(&optimized_module).expect("optimized source should compile");
    assert!(
        optimized_wasm.len() < 65_536,
        "optimized runtime I/O wasm unexpectedly large: {} bytes",
        optimized_wasm.len()
    );
}
