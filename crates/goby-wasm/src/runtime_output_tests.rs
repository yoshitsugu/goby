use std::path::PathBuf;
use std::sync::Mutex;

use goby_core::{Module, parse_module};

use crate::{
    assert_mode_parity, lower, resolve_module_runtime_output,
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
  callback_after_print (|n| -> n + 5)
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
  each_two (|n| -> print "${n}")
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
  each_two (|n| -> print "${n + base}")
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
  each [3, 5] (|n| -> print "${n}")
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
  list.each [2, 4] (|n| -> print "${n}")
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
  l.each [6, 8] (|n| -> print "${n}")
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
  each [10, 12] (|n| -> print "${n}")
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
  list.each [14, 16] (|n| -> print "${n}")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1416");
}

#[test]
fn resolves_runtime_output_for_track_f_f5_split_get_with_seeded_stdin() {
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
fn track_f_f5_split_get_out_of_range_aborts_with_seeded_stdin() {
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
    list.each [1, 3] (|n| -> log n)
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
    l.each [5, 7] (|n| -> log n)
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
    each [9, 11] (|n| -> log n)
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
  list.each ns (|i| -> println(i * 10))
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
  list.each ["go", "by"] (|s| -> println s)
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
  each ["go", "by"] (|s| -> println(s))
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
  each lines (|line| -> println(line))
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
  print (map [1, 2, 3] (|n| -> n * 10))
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
  print (list.map ["go", "by"] (|s| -> "${s}!"))
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
fn resolves_runtime_output_for_local_decl_wrapping_imported_list_map_named_lambda() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
import goby/list ( map )

mul_tens : List Int -> List Int
mul_tens ns = map ns (|n| -> n * 10)

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
  each lines (|line| -> Print.println line)
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

#[test]
#[ignore = "qualified iterator handler clauses are not yet covered by fallback runtime-output locking"]
fn locks_runtime_output_for_iterator_unified_gb() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = read_example("iterator_unified.gb");
    let module = parse_module(&source).expect("iterator_unified.gb should parse");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "tick:atick:btick:c31");
}
