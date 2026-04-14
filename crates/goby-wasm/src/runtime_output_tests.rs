use std::path::PathBuf;
use std::sync::Mutex;

use goby_core::{Module, parse_module, typecheck_module_collect};

use crate::{
    assert_mode_parity, execute_runtime_module_with_stdin, lower, resolve_module_runtime_output,
    resolve_module_runtime_output_with_mode_and_stdin,
};

static ENV_MUTEX: Mutex<()> = Mutex::new(());

fn read_example(name: &str) -> String {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("..");
    path.push("..");
    path.push("examples");
    path.push(name);
    std::fs::read_to_string(path).expect("example file should exist")
}

fn read_example_io(name: &str) -> String {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("..");
    path.push("..");
    path.push("examples");
    path.push(name);
    std::fs::read_to_string(path).expect("example fixture should exist")
}

fn resolve_with_seeded_stdin(module: &Module, stdin: &str) -> Option<String> {
    resolve_module_runtime_output_with_mode_and_stdin(
        module,
        lower::EffectExecutionMode::PortableFallback,
        Some(stdin.to_string()),
    )
}

#[test]
fn locks_runtime_output_for_function_example() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = read_example("function.gb");
    let module = parse_module(&source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "90[30, 40, 50][60, 70]something15");
}

#[test]
fn executes_hof_fold_print_example_with_locked_stdin_and_stdout() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = read_example("hof_fold_print.gb");
    let stdin = read_example_io("hof_fold_print.in");
    let expected = read_example_io("hof_fold_print.out");
    let module = parse_module(&source).expect("hof_fold_print example should parse");
    let output = execute_runtime_module_with_stdin(&module, Some(stdin))
        .expect("hof_fold_print example should execute");
    assert_eq!(
        output.as_deref(),
        Some(expected.as_str()),
        "hof_fold_print example must keep the locked acceptance stdout"
    );
}

#[test]
fn resolves_runtime_output_for_function_argument_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
callback_after_print : (Int -> Int) -> Unit
callback_after_print f =
  print "something"
  i = f 10
  print i

main : Unit -> Unit
main =
  callback_after_print (fn n -> n + 5)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "something15");
}

#[test]
fn resolves_runtime_output_for_unit_callback_argument_inline_lambda() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

main : Unit -> Unit
main =
  each_two (fn n -> print "${n}")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "12");
}

#[test]
fn resolves_runtime_output_for_unit_callback_argument_named_function() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
log_num : Int -> Unit
log_num n =
  print "${n}"

each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

main : Unit -> Unit
main =
  each_two log_num
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "12");
}

#[test]
fn resolves_runtime_output_for_unit_callback_argument_forwarded_alias() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
log_num : Int -> Unit
log_num n =
  print "${n}"

each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

wrapper : (Int -> Unit) -> Unit
wrapper g =
  each_two g

main : Unit -> Unit
main =
  wrapper log_num
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "12");
}

#[test]
fn resolves_runtime_output_for_unit_callback_argument_inline_lambda_with_capture() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

main : Unit -> Unit
main =
  base = 40
  each_two (fn n -> print "${n + base}")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "4142");
}

#[test]
fn resolves_runtime_output_for_list_each_style_callback_dispatch() {
    let _guard = ENV_MUTEX.lock().unwrap();
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
  each [3, 5] (fn n -> print "${n}")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "35");
}

#[test]
fn resolves_runtime_output_for_list_each_with_plain_import() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list

main : Unit -> Unit
main =
  list.each [2, 4] (fn n -> print "${n}")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "24");
}

#[test]
fn resolves_runtime_output_for_list_each_with_alias_import() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list as l

main : Unit -> Unit
main =
  l.each [6, 8] (fn n -> print "${n}")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "68");
}

#[test]
fn resolves_runtime_output_for_list_each_with_selective_import() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( each )

main : Unit -> Unit
main =
  each [10, 12] (fn n -> print "${n}")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1012");
}

#[test]
fn resolves_runtime_output_for_list_each_with_selective_import_and_canonical_qualifier() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( each )

main : Unit -> Unit
main =
  list.each [14, 16] (fn n -> print "${n}")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1416");
}

#[test]
fn resolves_runtime_output_for_runtime_io_general_lowering_split_lines_index_with_seeded_stdin() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = Read.read ()
  lines = split(text, "\n")
  Print.println (lines[1])
"#;
    let module = parse_module(source).expect("source should parse");
    let output = resolve_with_seeded_stdin(&module, "first\nsecond\nthird");
    assert_eq!(output.as_deref(), Some("second\n"));
}

#[test]
fn runtime_io_general_lowering_split_lines_index_out_of_range_aborts_with_seeded_stdin() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = Read.read ()
  lines = split(text, "\n")
  Print.println (lines[9])
"#;
    let module = parse_module(source).expect("source should parse");
    let output = resolve_with_seeded_stdin(&module, "first\nsecond");
    assert_eq!(output, None);
}

#[test]
fn resolves_runtime_output_for_effectful_callback_with_list_each_plain_import() {
    let _guard = ENV_MUTEX.lock().unwrap();
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
    list.each [1, 3] (fn n -> log n)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "13");
}

#[test]
fn resolves_runtime_output_for_effectful_callback_with_list_each_alias_import() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list as l

effect Log
  log : Int -> Unit

main : Unit -> Unit
main =
  with
    log n ->
      print "${n}"
      resume ()
  in
    l.each [5, 7] (fn n -> log n)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "57");
}

#[test]
fn resolves_runtime_output_for_effectful_callback_with_list_each_selective_import() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( each )

effect Log
  log : Int -> Unit

main : Unit -> Unit
main =
  with
    log n ->
      print "${n}"
      resume ()
  in
    each [9, 11] (fn n -> log n)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "911");
}

#[test]
fn resolves_runtime_output_for_list_each_callback_with_bare_println() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list

main : Unit -> Unit can Print
main =
  ns = [1, 2, 3]
  list.each ns (fn i -> println(i * 10))
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "10\n20\n30\n");
}

#[test]
fn resolves_runtime_output_for_list_each_with_string_callback() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list

main : Unit -> Unit can Print
main =
  list.each ["go", "by"] (fn s -> println s)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "go\nby\n");
}

#[test]
fn resolves_runtime_output_for_list_each_with_string_callback_and_selective_import() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( each )

main : Unit -> Unit can Print
main =
  each ["go", "by"] (fn s -> println(s))
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "go\nby\n");
}

#[test]
fn resolves_runtime_output_for_imported_string_split_with_selective_import() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print
main =
  lines = split("hogehoge\nfugafuga", "\n")
  each lines (fn line -> println(line))
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "hogehoge\nfugafuga\n");
}

#[test]
fn resolves_runtime_output_for_imported_string_graphemes_with_selective_import() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/string ( graphemes )

main : Unit -> Unit
main =
  print (graphemes "a👨‍👩‍👧‍👦b")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(
        output,
        "[\"a\", \"👨\u{200d}👩\u{200d}👧\u{200d}👦\", \"b\"]"
    );
}

#[test]
fn resolves_runtime_output_for_imported_string_split_with_empty_delimiter_helper() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let stdlib_source = std::fs::read_to_string(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("..")
            .join("stdlib")
            .join("goby/string.gb"),
    )
    .expect("stdlib string should exist");
    let stdlib = parse_module(&stdlib_source).expect("stdlib string should parse");
    let helper_decl = stdlib
        .declarations
        .iter()
        .find(|decl| decl.name == "split_with_empty_delimiter")
        .expect("helper decl should exist");
    let helper_plan = crate::wasm_exec_plan::decl_exec_plan(helper_decl);
    assert_ne!(
        helper_plan
            .runtime
            .as_ref()
            .and_then(|runtime| runtime.body.as_deref()),
        Some("()"),
        "runtime plan should not collapse helper body to Unit"
    );
    let source = r#"
import goby/string ( split_with_empty_delimiter )

main : Unit -> Unit
main =
  print (split_with_empty_delimiter "a👨‍👩‍👧‍👦b")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(
        output,
        "[\"a\", \"👨\u{200d}👩\u{200d}👧\u{200d}👦\", \"b\"]"
    );
}

#[test]
fn resolves_runtime_output_for_local_decl_with_following_statement_after_with() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

after_with : String -> Int can Print
after_with value =
  with
    yield _ _ ->
      resume (True, ())
  in
    __goby_string_each_grapheme value
  42

main : Unit -> Unit
main =
  print (after_with "abc")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "42");
}

#[test]
fn resolves_runtime_output_for_local_decl_with_with_body_assignment_to_outer_local() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

count_graphemes : String -> Int can Print
count_graphemes value =
  mut n = 0
  with
    yield _ _ ->
      resume (True, ())
  in
    n := __goby_string_each_grapheme value
  n

main : Unit -> Unit
main =
  print (count_graphemes "a👨‍👩‍👧‍👦b")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "3");
}

#[test]
fn resolves_runtime_output_for_read_line_then_read_remaining() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  first = read_line()
  rest = read()
  print "${first}|${rest}"
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_with_seeded_stdin(&module, "alpha\nbeta\ngamma")
        .expect("runtime output should resolve");
    assert_eq!(output, "alpha|beta\ngamma");
}

#[test]
fn resolves_runtime_output_for_repeated_read_after_exhaustion() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  first = read()
  second = read()
  print "${first}|${second}"
"#;
    let module = parse_module(source).expect("parse should work");
    let output =
        resolve_with_seeded_stdin(&module, "payload").expect("runtime output should resolve");
    assert_eq!(output, "payload|");
}

#[test]
fn resolves_runtime_output_for_repeated_read_line_at_eof() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
main : Unit -> Unit can Print, Read
main =
  first = read_line()
  second = read_line()
  third = read_line()
  print "${first}|${second}|${third}"
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_with_seeded_stdin(&module, "a\nb").expect("runtime output should resolve");
    assert_eq!(output, "a|b|");
}

#[test]
fn resolves_runtime_output_for_imported_list_map_with_ints() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( map )

main : Unit -> Unit
main =
  print (map [1, 2, 3] (fn n -> n * 10))
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[10, 20, 30]");
}

#[test]
fn resolves_runtime_output_for_imported_list_map_with_strings() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list

main : Unit -> Unit
main =
  print (list.map ["go", "by"] (fn s -> "${s}!"))
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[\"go!\", \"by!\"]");
}

#[test]
fn resolves_runtime_output_for_imported_int_to_string_direct_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/int as int

main : Unit -> Unit
main =
  print (int.to_string -7)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "-7");
}

#[test]
fn resolves_runtime_output_for_imported_int_to_string_named_map_callback() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/int as int
import goby/list ( map )

main : Unit -> Unit
main =
  print (map [1, 20, -3] int.to_string)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[\"1\", \"20\", \"-3\"]");
}

#[test]
fn resolves_runtime_output_for_local_string_map_then_each_println_chain() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( map, each )

plus_one : Int -> String
plus_one i =
  j = i + 1
  "${j}"

main : Unit -> Unit can Print
main =
  a = [1, 2, 3]
  b = map a plus_one
  each b println
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "2\n3\n4\n");
}

#[test]
fn resolves_runtime_output_for_local_decl_wrapping_imported_list_map_named_lambda() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( map )

mul_tens : List Int -> List Int
mul_tens ns = map ns (fn n -> n * 10)

main : Unit -> Unit
main =
  print (mul_tens [3, 4, 5])
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[30, 40, 50]");
}

#[test]
fn reports_callable_dispatch_error_for_list_each_non_callable_callback() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list

main : Unit -> Unit
main =
  list.each [1, 2] 1
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some(
            "runtime error: unsupported callable dispatch [E-CALLABLE-DISPATCH]: callable parameter requires a lambda or function name argument"
        )
    );
}

#[test]
fn reports_callable_dispatch_error_for_decl_callable_param_non_callable_arg() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

main : Unit -> Unit
main =
  each_two 1
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some(
            "runtime error: unsupported callable dispatch [E-CALLABLE-DISPATCH]: callable parameter requires a lambda or function name argument"
        )
    );
}

#[test]
fn resolves_runtime_output_for_intrinsic_string_length_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
main : Unit -> Unit
main =
  print (__goby_string_length "hello")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "5");
}

#[test]
fn resolves_runtime_output_for_intrinsic_env_fetch_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    unsafe { std::env::set_var("GOBY_INTRINSIC_TEST_PATH", "intrinsic-ok") };
    let source = r#"
main : Unit -> Unit
main =
  print (__goby_env_fetch_env_var "GOBY_INTRINSIC_TEST_PATH")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    unsafe { std::env::remove_var("GOBY_INTRINSIC_TEST_PATH") };
    assert_eq!(output, "intrinsic-ok");
}

#[test]
fn resolves_runtime_output_for_import_example_shape() {
    let _guard = ENV_MUTEX.lock().unwrap();
    unsafe { std::env::set_var("GOBY_PATH", "alpha,beta") };
    let source = read_example("import.gb");
    let module = parse_module(&source).expect("import example should parse");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    unsafe { std::env::remove_var("GOBY_PATH") };
    assert_eq!(output, "alpha\nbeta\n");
}

#[test]
fn resolves_runtime_output_for_import_example_shape_with_missing_env() {
    let _guard = ENV_MUTEX.lock().unwrap();
    unsafe { std::env::remove_var("GOBY_PATH") };
    let source = read_example("import.gb");
    let module = parse_module(&source).expect("import example should parse");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "\n");
}

#[test]
fn runtime_resolves_goby_env_fetch_via_imported_decl() {
    let _guard = ENV_MUTEX.lock().unwrap();
    unsafe { std::env::set_var("GOBY_ENV_BRIDGE_TEST_PATH", "env-bridge-ok") };
    let source = r#"
import goby/env ( fetch_env_var )
main : Unit -> Unit
main =
  print (fetch_env_var "GOBY_ENV_BRIDGE_TEST_PATH")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    unsafe { std::env::remove_var("GOBY_ENV_BRIDGE_TEST_PATH") };
    assert_eq!(output, "env-bridge-ok");
}

#[test]
fn runtime_resolves_goby_string_length_via_qualified_imported_decl() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/string
main : Unit -> Unit
main =
  print (string.length "hello")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "5");
}

#[test]
fn runtime_resolves_goby_string_length_via_selective_imported_decl() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/string ( length )
main : Unit -> Unit
main =
  print (length "hello")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "5");
}

#[test]
fn runtime_resolves_goby_string_length_via_selective_import_and_canonical_qualifier() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/string ( length )
main : Unit -> Unit
main =
  print (string.length "hello")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "5");
}

#[test]
fn resolves_runtime_output_for_imported_string_split_with_selective_import_and_canonical_qualifier()
{
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/string ( split )
import goby/list ( each )

main : Unit -> Unit
main =
  lines = string.split("go\nby", "\n")
  each lines (fn line -> Print.println line)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "go\nby\n");
}

#[test]
fn resolves_runtime_output_for_imported_string_split_with_plain_import_and_canonical_qualifier() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/string

main : Unit -> Unit
main =
  print (string.split("go\nby", "\n"))
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[\"go\", \"by\"]");
}

#[test]
fn resolves_runtime_output_for_imported_list_join_with_alias_import_on_empty_list() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list as l

main : Unit -> Unit
main =
  println l.join([], "\n")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "\n");
}

#[test]
fn runtime_resolves_goby_int_parse_via_selective_imported_declaration_execution() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/int ( parse )

effect StringParseError
  invalid_integer : String -> Int

main : Unit -> Unit can Print, StringParseError
main =
  with
    invalid_integer _ ->
      resume -1
  in
    print parse("42")
    print parse("x")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(output.as_deref(), Some("42-1"));
}

#[test]
fn resolves_runtime_output_for_intrinsic_each_grapheme_count_mode_unified_contract() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/iterator

effect Iterator a b
  yield : a -> b -> (Bool, b)

main : Unit -> Unit can Print
main =
  with
    yield _ _ ->
      resume (True, ())
  in
    n = __goby_string_each_grapheme "a👨‍👩‍👧‍👦b"
    print n
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "3");
}

#[test]
fn resolves_runtime_output_for_intrinsic_each_grapheme_unified_iterator_contract() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
type GraphemeState = GraphemeState(grapheme: String, current: String)

effect Iterator a b
  yield : a -> b -> (Bool, b)

main : Unit -> Unit can Print
main =
  state = GraphemeState(grapheme: "", current: "")
  out = state
  with
    yield grapheme step ->
      next = "${step.current}${grapheme}"
      resume (True, GraphemeState(grapheme: grapheme, current: next))
  in
    out = __goby_string_each_grapheme "a👨‍👩‍👧‍👦b" state
  print out.current
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "a👨\u{200d}👩\u{200d}👧\u{200d}👦b");
}

#[test]
fn resolves_runtime_output_for_intrinsic_each_grapheme_unified_early_stop() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

main : Unit -> Unit can Print
main =
  with
    yield _ _ ->
      resume (False, ())
  in
    n = __goby_string_each_grapheme "abc"
    print n
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1");
}

#[test]
fn resolves_runtime_output_for_multi_arg_effect_op_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

main : Unit -> Unit can Print
main =
  with
    yield _ step ->
      resume (True, step + 1)
  in
    print (yield "x" 41)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "(True, 42)");
}

#[test]
fn resolves_runtime_output_for_string_equality_operator() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
main : Unit -> Unit
main =
  print ("alpha" == "alpha")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "True");
}

#[test]
fn resolves_runtime_output_for_unit_literal_value() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
main : Unit -> Unit
main = ()
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert!(
        output.is_none(),
        "unit literal main without print should produce no runtime output"
    );
}

#[test]
fn resolves_runtime_output_for_tuple_member_access_by_index() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
main : Unit -> Unit
main =
  pair = (True, 42)
  if pair.0
    print pair.1
  else
    print 0
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "42");
}

#[test]
fn tuple_literal_replays_handled_value() {
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
    pair = (next 0, 2)
    print pair.0
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1");
}

#[test]
fn typed_mode_matches_fallback_for_tuple_literal_replay() {
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
    pair = (next 0, 2)
    print pair.0
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "tuple literal replay");
    assert_eq!(typed.stdout.as_deref(), Some("2"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn rejects_runtime_output_for_numeric_member_on_non_tuple_receiver() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
main : Unit -> Unit
main =
  print Status.0
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert!(
        output.is_none(),
        "numeric member access on non-tuple receiver should not produce runtime output"
    );
}

#[test]
fn resolves_runtime_output_for_intrinsic_list_push_string_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
main : Unit -> Unit
main =
  xs = __goby_list_push_string [] "a"
  ys = __goby_list_push_string xs "b"
  print ys
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[\"a\", \"b\"]");
}

#[test]
fn resolves_runtime_output_for_record_field_list_push_string_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
type GraphemeState = GraphemeState(grapheme: String, parts: List String, current: String, delimiter: String, seen: Bool)

main : Unit -> Unit
main =
  state = GraphemeState(grapheme: "", parts: [], current: "", delimiter: "", seen: False)
  print (__goby_list_push_string state.parts "a")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[\"a\"]");
}

#[test]
fn locks_runtime_output_for_effect_gb() {
    let _guard = ENV_MUTEX.lock().unwrap();
    unsafe { std::env::set_var("GOBY_PATH", "hello") };
    let source = read_example("effect.gb");
    let module = parse_module(&source).expect("effect.gb should parse");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    unsafe { std::env::remove_var("GOBY_PATH") };
    assert_eq!(output, "13donedevelopment");
}

#[test]
#[ignore = "qualified iterator handler clauses are not yet covered by fallback runtime-output locking"]
fn locks_runtime_output_for_iterator_gb() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = read_example("iterator.gb");
    let module = parse_module(&source).expect("iterator.gb should parse");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "tick:atick:btick:c");
}

// ── Fn-form callback parity tests ────────────────────────────────────────────
// These tests confirm that `fn a -> expr` and `fn a b -> expr` callbacks lower
// through the same callable model as `fn x -> expr` (single-param) lambdas.
// The desugaring happens at parse time; no new runtime branch is added.

#[test]
fn resolves_runtime_output_for_map_with_fn_lambda_callback() {
    let _guard = ENV_MUTEX.lock().unwrap();
    // `fn x -> x * 2` ≡ `fn x -> x * 2` after desugaring; map parity check.
    let source = r#"
import goby/list ( map )

main : Unit -> Unit
main =
  print (map [1, 2, 3] (fn x -> x * 2))
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[2, 4, 6]");
}

#[test]
fn resolves_runtime_output_for_each_with_fn_lambda_callback() {
    let _guard = ENV_MUTEX.lock().unwrap();
    // `fn n -> print "${n}"` ≡ `fn n -> print "${n}"` after desugaring; each parity check.
    let source = r#"
import goby/list ( each )

main : Unit -> Unit
main =
  each [10, 20] (fn n -> print "${n}")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1020");
}

#[test]
fn fold_with_inline_fn_lambda_callback_typechecks_and_parses() {
    // Keep a focused parse/typecheck lock for the inline callback shape itself.
    let source = r#"
import goby/list ( fold )

main : Unit -> Unit
main =
  total = fold [1, 2, 3] 0 (fn acc x -> acc + x)
  ()
"#;
    let module = parse_module(source).expect("parse should succeed");
    let errors = typecheck_module_collect(&module);
    assert!(
        errors.is_empty(),
        "fold + fn lambda callback should typecheck: {:?}",
        errors
    );
}

#[test]
fn resolves_runtime_output_for_fold_with_inline_multi_param_lambda_callback() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( fold )

main : Unit -> Unit
main =
  total = fold [1, 2, 3] 0 (fn acc x -> acc + x)
  print "${total}"
"#;
    let module = parse_module(source).expect("parse should succeed");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "6");
}

#[test]
fn resolves_runtime_output_for_effectful_fold_with_inline_multi_param_lambda_callback() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( fold )

main : Unit -> Unit
main =
  total =
    fold [1, 2, 3] 0 (fn acc x ->
      println "acc=${acc} x=${x}"
      acc + x
    )
  println "total=${total}"
"#;
    let module = parse_module(source).expect("parse should succeed");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "acc=0 x=1\nacc=1 x=2\nacc=3 x=3\ntotal=6\n");
}

#[test]
fn resolves_runtime_output_for_let_bound_inline_multi_param_lambda_via_user_hof() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
apply_twice : (Int -> Int -> Int) -> Int -> Int
apply_twice f x =
  f x x

main : Unit -> Unit
main =
  add = fn a b -> a + b
  result = apply_twice add 5
  print "${result}"
"#;
    let module = parse_module(source).expect("parse should succeed");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "10");
}

#[test]
fn resolves_runtime_output_for_capturing_inline_multi_param_lambda_via_fold() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( fold )

main : Unit -> Unit
main =
  bias = 10
  total = fold [1, 2, 3] 0 (fn acc x -> acc + x + bias)
  print "${total}"
"#;
    let module = parse_module(source).expect("parse should succeed");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "36");
}

#[test]
fn resolves_runtime_output_for_pairwise_apply_with_inline_multi_param_lambda() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
pairwise_apply : List Int -> (Int -> Int -> Int) -> List Int
pairwise_apply xs f =
  case xs
    [] -> []
    [x] -> [x]
    [x, y, ..rest] -> [f x y, ..pairwise_apply rest f]

main : Unit -> Unit
main =
  result = pairwise_apply [1, 2, 3, 4] (fn a b -> a + b)
  print "${result}"
"#;
    let module = parse_module(source).expect("parse should succeed");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[3, 7]");
}

#[test]
fn resolves_runtime_output_for_apply_two_with_inline_multi_param_lambda() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
apply_two : (Int -> Int -> Int) -> Int -> Int -> Int
apply_two f a b =
  f a b

main : Unit -> Unit
main =
  result = apply_two (fn a b -> a + b) 20 22
  print "${result}"
"#;
    let module = parse_module(source).expect("parse should succeed");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "42");
}

#[test]
fn list_join_empty_list_returns_empty_string() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( join )

main : Unit -> Unit can Print
main =
  s = join [] "-"
  println s
"#;
    let module = parse_module(source).expect("parse should work");
    let wasm = crate::compile_module(&module).expect("join empty-list should compile");
    let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
        .expect("join empty-list wasm should execute");
    assert_eq!(output, "\n");
}

#[test]
fn list_join_single_element_returns_element_without_sep() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( join )

main : Unit -> Unit can Print
main =
  s = join ["only"] ","
  println s
"#;
    let module = parse_module(source).expect("parse should work");
    let wasm = crate::compile_module(&module).expect("join single-element should compile");
    let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
        .expect("join single-element wasm should execute");
    assert_eq!(output, "only\n");
}

#[test]
fn list_join_multi_element_no_trailing_sep() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( join )

main : Unit -> Unit can Print
main =
  s = join ["a", "b", "c"] "-"
  println s
"#;
    let module = parse_module(source).expect("parse should work");
    let wasm = crate::compile_module(&module).expect("join multi-element should compile");
    let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
        .expect("join multi-element wasm should execute");
    assert_eq!(output, "a-b-c\n");
}

#[test]
fn list_join_is_non_destructive_when_called_twice_on_same_list() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( join )

main : Unit -> Unit can Print
main =
  xs = ["a", "b", "c"]
  println (join xs "-")
  println (join xs "-")
"#;
    let module = parse_module(source).expect("parse should work");
    let wasm = crate::compile_module(&module).expect("double join should compile");
    let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
        .expect("double join wasm should execute");
    assert_eq!(output, "a-b-c\na-b-c\n");
}

#[test]
fn list_each_nested_list_set_preserves_row_shape() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list (each, join, length)

apply : List (List String) -> List (Int, Int) -> List (List String)
apply grid positions =
  mut updated = grid
  each positions (fn pos ->
    updated[pos.1][pos.0] := "."
    ()
  )
  updated

main : Unit -> Unit can Print
main =
  grid = [["a", "b", "c"], ["d", "e", "f"]]
  positions = [(0, 0), (1, 0)]
  updated = apply grid positions
  println (join(updated[0], ""))
  println (join(updated[0], ""))
  println "${length(updated[0])}"
"#;
    let module = parse_module(source).expect("parse should work");
    let wasm = crate::compile_module(&module).expect("nested list set should compile");
    let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
        .expect("nested list set wasm should execute");
    assert_eq!(output, "..c\n..c\n3\n");
}

#[test]
fn list_fold_callback_accepts_tuple_second_param_with_negative_ints() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list (fold)
import goby/stdio

main : Unit -> Unit can Print
main =
  neighbor_offsets = [(-1, -1), (0, -1), (1, -1)]
  s = fold neighbor_offsets 0 (fn total offset ->
    total + offset.0 + offset.1
  )
  println "${s}"
"#;
    let module = parse_module(source).expect("parse should work");
    let wasm = crate::compile_module(&module).expect("fold tuple test should compile");
    let output = crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, None)
        .expect("fold tuple test should execute");
    assert_eq!(output, "-3\n");
}

#[test]
fn rendering_after_first_prune_preserves_grid_for_second_collect() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let lines = [
        "..@@.@@@@.",
        "@@@.@.@.@@",
        "@@@@@.@.@@",
        "@.@@@@..@.",
        "@@.@@@@.@@",
        ".@@@@@@@.@",
        ".@.@.@.@@@",
        "@.@@@.@@@@",
        ".@@@@@@@@.",
        "@.@.@@@.@.",
    ];
    let grid_literal = lines
        .iter()
        .map(|line| {
            let elems = line
                .chars()
                .map(|c| format!("\"{}\"", c))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{elems}]")
        })
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!(
        r#"
import goby/list (each, fold, join, length)
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

main : Unit -> Unit can Print, Read
main =
  _ = read()
  grid = [{grid_literal}]
  height = length grid
  width = length(grid[0])
  p1 = collect_prune_positions grid 0 0 width height
  g1 = apply_prune_positions grid p1
  count1 = length p1
  println "${{count1}}"
  render_grid g1
  p2 = collect_prune_positions g1 0 0 width height
  count2 = length p2
  println "${{count2}}"
"#
    );
    let module = parse_module(&source).expect("parse should work");
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("program should execute")
        .expect("program should emit output");
    assert!(
        output.starts_with("13\n") && output.ends_with("12\n"),
        "expected first prune count 13 and second prune count 12, got {output:?}"
    );
}

#[test]
fn second_collect_without_render_stays_stable() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let lines = [
        "..@@.@@@@.",
        "@@@.@.@.@@",
        "@@@@@.@.@@",
        "@.@@@@..@.",
        "@@.@@@@.@@",
        ".@@@@@@@.@",
        ".@.@.@.@@@",
        "@.@@@.@@@@",
        ".@@@@@@@@.",
        "@.@.@@@.@.",
    ];
    let grid_literal = lines
        .iter()
        .map(|line| {
            let elems = line
                .chars()
                .map(|c| format!("\"{}\"", c))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{elems}]")
        })
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!(
        r#"
import goby/list (each, fold, length)
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

main : Unit -> Unit can Print, Read
main =
  _ = read()
  grid = [{grid_literal}]
  height = length grid
  width = length(grid[0])
  p1 = collect_prune_positions grid 0 0 width height
  g1 = apply_prune_positions grid p1
  p2 = collect_prune_positions g1 0 0 width height
  count1 = length p1
  count2 = length p2
  println "${{count1}}"
  println "${{count2}}"
"#
    );
    let module = parse_module(&source).expect("parse should work");
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("program should execute")
        .expect("program should emit output");
    assert_eq!(output, "13\n12\n");
}

#[test]
fn iterative_grid_pruning_without_render_reaches_total_43() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list (each, fold, length, map)
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

prune_until_stable : List (List String) -> Int -> Int can Print
prune_until_stable grid total =
  height = length grid
  width = length(grid[0])
  positions = collect_prune_positions grid 0 0 width height
  count = length positions
  if count > 0
    updated = apply_prune_positions grid positions
    prune_until_stable updated (total + count)
  else
    total

main : Unit -> Unit can Print, Read
main =
  grid = map (read_lines ()) graphemes
  total = prune_until_stable grid 0
  println "${total}"
"#;
    let input = "\
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
    let module = parse_module(source).expect("source should parse");
    let output = execute_runtime_module_with_stdin(&module, Some(input.to_string()))
        .expect("iterative grid pruning without render should execute");
    assert_eq!(output.as_deref(), Some("43\n"));
}

#[test]
fn collect_prune_positions_on_second_iteration_grid_returns_twelve() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let lines = [
        ".......@..",
        ".@@.@.@.@@",
        "@@@@@...@@",
        "@.@@@@..@.",
        ".@.@@@@.@.",
        ".@@@@@@@.@",
        ".@.@.@.@@@",
        "..@@@.@@@@",
        ".@@@@@@@@.",
        "....@@@...",
    ];
    let grid_literal = lines
        .iter()
        .map(|line| {
            let elems = line
                .chars()
                .map(|c| format!("\"{}\"", c))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{elems}]")
        })
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!(
        r#"
import goby/list (fold, length)
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

main : Unit -> Unit can Print, Read
main =
  _ = read()
  grid = [{grid_literal}]
  height = length grid
  width = length(grid[0])
  positions = collect_prune_positions grid 0 0 width height
  count = length positions
  println "${{count}}"
"#
    );
    let module = parse_module(&source).expect("parse should work");
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("program should execute")
        .expect("program should emit output");
    assert_eq!(output, "12\n");
}

#[test]
fn index_scan_after_first_prune_counts_fifty_eight_live_cells() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let lines = [
        "..@@.@@@@.",
        "@@@.@.@.@@",
        "@@@@@.@.@@",
        "@.@@@@..@.",
        "@@.@@@@.@@",
        ".@@@@@@@.@",
        ".@.@.@.@@@",
        "@.@@@.@@@@",
        ".@@@@@@@@.",
        "@.@.@@@.@.",
    ];
    let grid_literal = lines
        .iter()
        .map(|line| {
            let elems = line
                .chars()
                .map(|c| format!("\"{}\"", c))
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{elems}]")
        })
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!(
        r#"
import goby/list (each, fold, length)
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

count_live : List (List String) -> Int -> Int -> Int -> Int -> Int can Print
count_live grid x y width height =
  if y >= height
    0
  else
    if x >= width
      count_live grid 0 (y + 1) width height
    else
      here =
        if grid[y][x] == "@"
          1
        else
          0
      here + count_live grid (x + 1) y width height

main : Unit -> Unit can Print, Read
main =
  _ = read()
  grid = [{grid_literal}]
  height = length grid
  width = length(grid[0])
  p1 = collect_prune_positions grid 0 0 width height
  g1 = apply_prune_positions grid p1
  total = count_live g1 0 0 width height
  println "${{total}}"
"#
    );
    let module = parse_module(&source).expect("parse should work");
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("program should execute")
        .expect("program should emit output");
    assert_eq!(output, "58\n");
}

#[test]
fn general_lowered_recursive_grid_scan_without_spread_terminates() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/stdio

scan : Int -> Int -> Int -> Int -> Int can Print
scan x y width height =
  if y >= height
    0
  else
    if x >= width
      scan 0 (y + 1) width height
    else
      1 + scan (x + 1) y width height

main : Unit -> Unit can Print, Read
main =
  _ = read()
  total = scan 0 0 10 10
  println "${total}"
"#;
    let module = parse_module(source).expect("parse should work");
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("scan program should execute");
    assert_eq!(output.as_deref(), Some("100\n"));
}

#[test]
fn general_lowered_recursive_grid_scan_with_spread_terminates() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list (length)
import goby/stdio

scan : Int -> Int -> Int -> Int -> List (Int, Int) can Print
scan x y width height =
  if y >= height
    []
  else
    if x >= width
      scan 0 (y + 1) width height
    else
      rest = scan (x + 1) y width height
      [(x, y), ..rest]

main : Unit -> Unit can Print, Read
main =
  _ = read()
  total = length (scan 0 0 10 10)
  println "${total}"
"#;
    let module = parse_module(source).expect("parse should work");
    let output = execute_runtime_module_with_stdin(&module, Some("x\n".to_string()))
        .expect("spread-scan program should execute");
    assert_eq!(output.as_deref(), Some("100\n"));
}

#[test]
fn resolves_runtime_output_for_list_join_single_element() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list as l

main : Unit -> Unit
main =
  println l.join(["only"], ",")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "only\n");
}

#[test]
fn resolves_runtime_output_for_list_join_multi_element_no_trailing_sep() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list as l

main : Unit -> Unit
main =
  println l.join(["a", "b", "c"], "-")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "a-b-c\n");
}

// Regression test: stdlib function called inside a lambda (each callback) must be resolved
// as an aux_decl.  Previously, lambda_decls were not scanned during the stdlib fixpoint loop,
// so a stdlib DeclCall reachable only through a lambda was never added to aux_decls and the
// emit step raised "unknown declaration '...' in DeclCall".
#[test]
fn resolves_runtime_output_for_stdlib_decl_call_inside_each_lambda() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list (each, join)
import goby/stdio

main : Unit -> Unit can Print
main =
  rows = [["x", "y"], ["a", "b"]]
  each rows (fn row ->
    line = join row "-"
    println line
  )
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "x-y\na-b\n");
}

#[test]
#[ignore = "qualified iterator handler clauses are not yet covered by fallback runtime-output locking"]
fn locks_runtime_output_for_iterator_unified_gb() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = read_example("iterator_unified.gb");
    let module = parse_module(&source).expect("iterator_unified.gb should parse");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "tick:atick:btick:c31");
}

/// G0 regression: the BUGS.md minimal repro (50×50 grid, 5000 iterations of in-place
/// diagonal update via `each` + `AssignIndex`) must complete without E-MEMORY-EXHAUSTION
/// and print `0`.
///
/// Before the ListSetInPlace optimisation this OOM'd under the default 64 MiB ceiling.
///
/// The test runs on a thread with an enlarged stack (8 MiB) because `build_grid 50`
/// uses a cons-recursive helper that depth-walks 50 levels.
#[test]
fn each_assign_index_in_place_bugs_md_minimal_repro_completes() {
    let _guard = ENV_MUTEX.lock().unwrap();
    // Source is the exact BUGS.md minimal repro.
    let source = r#"
import goby/list (each)
import goby/stdio

build_row : Int -> List String can Print
build_row n =
  if n == 0
    []
  else
    rest = build_row (n - 1)
    ["@", ..rest]

build_grid : Int -> List (List String) can Print
build_grid n =
  if n == 0
    []
  else
    rest = build_grid (n - 1)
    [build_row 50, ..rest]

build_indices : Int -> List Int can Print
build_indices n =
  if n == 0
    []
  else
    rest = build_indices (n - 1)
    [n - 1, ..rest]

clear_all : List (List String) -> List Int -> List (List String) can Print
clear_all rolls indices =
  mut new_rolls = rolls
  each indices (fn i ->
    new_rolls[i][i] := "."
    ()
  )
  new_rolls

loop : List (List String) -> List Int -> Int -> Int can Print
loop rolls indices n =
  if n == 0
    0
  else
    next = clear_all rolls indices
    loop next indices (n - 1)

main : Unit -> Unit can Print, Read
main =
  _lines = read_lines ()
  rolls = build_grid 50
  indices = build_indices 50
  r = loop rolls indices 5000
  println "${r}"
"#;
    // Run on a larger stack to accommodate cons-recursive build_grid/build_row (depth ≤ 50).
    let result = std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(move || {
            let module = parse_module(source).expect("minimal repro should parse");
            let wasm = crate::compile_module(&module).expect("minimal repro should compile");
            crate::wasm_exec::run_wasm_bytes_with_stdin(&wasm, Some("x\n"))
                .expect("minimal repro should execute without E-MEMORY-EXHAUSTION")
        })
        .expect("thread spawn should succeed")
        .join()
        .expect("thread should not panic");
    assert_eq!(result, "0\n", "expected output 0 after 5000 diagonal-zeroing iterations");
}
