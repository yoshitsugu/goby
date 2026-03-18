use std::sync::Mutex;

use goby_core::parse_module;

use crate::{assert_mode_parity, fallback, resolve_module_runtime_output};

static ENV_MUTEX: Mutex<()> = Mutex::new(());

#[test]
fn resolves_runtime_output_for_list_spread_int_values() {
    let source = r#"
main : Unit -> Unit
main =
  rest = [2, 3]
  print [1, ..rest]
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        !fallback::supports_native_codegen(&module),
        "list spread currently routes through fallback runtime path"
    );
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[1, 2, 3]");
}

#[test]
fn resolves_runtime_output_for_list_spread_string_values() {
    let source = r#"
main : Unit -> Unit
main =
  rest = ["b", "c"]
  print ["a", ..rest]
"#;
    let module = parse_module(source).expect("source should parse");
    assert!(
        !fallback::supports_native_codegen(&module),
        "list spread currently routes through fallback runtime path"
    );
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[\"a\", \"b\", \"c\"]");
}

#[test]
fn list_literal_replays_handled_value() {
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
    xs = [next 0, 2]
    print xs
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "[1, 2]");
}

#[test]
fn typed_mode_matches_fallback_for_list_literal_replay() {
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
    xs = [next 0, 2]
    print xs
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "list literal replay");
    assert_eq!(typed.stdout.as_deref(), Some("[2, 2]"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn record_constructor_replays_handled_field_value() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
type Box = Box(value: Int)

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    box = Box(value: next 0)
    print box.value
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1");
}

#[test]
fn typed_mode_matches_fallback_for_record_constructor_replay() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
type Box = Box(value: Int)

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    box = Box(value: next 0)
    print box.value
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "record constructor replay");
    assert_eq!(typed.stdout.as_deref(), Some("2"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn positional_single_field_constructor_replays_handled_value() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
type Error = Error(message: String)

effect Msg
  next: Unit -> String

effect RaiseError
  raise: Error -> Unit

main : Unit -> Unit
main =
  with
    next _ ->
      resume "oops"
  in
    with
      raise e ->
        print e.message
        resume ()
    in
      raise Error(next ())
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "oops");
}

#[test]
fn typed_mode_matches_fallback_for_positional_single_field_constructor_replay() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
type Error = Error(message: String)

effect Msg
  next: Unit -> String

effect RaiseError
  raise: Error -> Unit

main : Unit -> Unit
main =
  with
    next _ ->
      resume "later"
  in
    with
      raise e ->
        print e.message
        resume ()
    in
      raise Error(next ())
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "positional single-field constructor replay");
    assert_eq!(typed.stdout.as_deref(), Some("later"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn handler_body_sequential_value_binding_with_inner_effect_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Source
  next: Unit -> Int

effect Store
  get: Unit -> Int

main : Unit -> Unit
main =
  with
    next _ ->
      x = get ()
      resume (x + 10)
    get _ ->
      resume 5
  in
    print (next ())
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(
        &module,
        "handler body sequential value binding with inner effect call",
    );
    assert_eq!(typed.stdout.as_deref(), Some("15"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn in_block_calls_declaration_that_invokes_effect() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Eff
  next: Int -> Int

get_plus : Int -> Int
get_plus n = next n + 5

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 5)
  in
    print (get_plus 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "in-block call to declaration that invokes effect");
    assert_eq!(typed.stdout.as_deref(), Some("10"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn handler_body_binding_resumes_via_outer_with_block_synchronous_dispatch() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Source
  next: Unit -> Int

effect Store
  get: Unit -> Int

main : Unit -> Unit
main =
  with
    get _ ->
      resume 5
  in
    with
      next _ ->
        x = get ()
        resume (x + 10)
    in
      print (next ())
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(
        &module,
        "handler body binding resumes via outer with block (synchronous dispatch)",
    );
    assert_eq!(typed.stdout.as_deref(), Some("15"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn declaration_body_two_binding_progression_value_combination() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

sum_two : Int -> Int
sum_two n =
  a = next n
  b = next a
  a + b

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (sum_two 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(
        &module,
        "declaration body two binding progression value combination",
    );
    assert_eq!(typed.stdout.as_deref(), Some("3"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn three_step_in_block_binding_progression() {
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
    x = next 0
    y = next x
    z = next y
    print z
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "three-step in-block binding progression");
    assert_eq!(typed.stdout.as_deref(), Some("3"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn handler_body_with_inner_with_block_value() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Outer
  run: Unit -> Int

effect Inner
  step: Int -> Int

main : Unit -> Unit
main =
  with
    run _ ->
      result =
        with
          step n ->
            resume (n * 2)
        in
          a = step 1
          b = step a
          a + b
      resume result
  in
    print (run ())
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "handler body with inner with block value");
    assert_eq!(typed.stdout.as_deref(), Some("6"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn assignment_rhs_next_line_with_block_value() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  step: Int -> Int

main : Unit -> Unit
main =
  with
    step n ->
      resume (n * 3)
  in
    mut result = 0
    result :=
      with
        step n ->
          resume (n * 3)
      in
        a = step 1
        a + 5
    print result
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "assignment rhs next-line with block value");
    assert_eq!(typed.stdout.as_deref(), Some("8"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn case_arm_body_calls_effect_operation() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

get_next_from_case : Int -> Int
get_next_from_case n =
  case n
    0 -> next 0
    _ -> next n

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 10)
  in
    print (get_next_from_case 0)
    print (get_next_from_case 5)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "case arm body calls effect operation");
    assert_eq!(typed.stdout.as_deref(), Some("1015"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn declaration_block_body_with_binding_and_effect_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

get_advanced : Int -> Int
get_advanced n =
  a = next n
  a + 10

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (get_advanced 5)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(
        &module,
        "declaration block body with binding and effect call",
    );
    assert_eq!(typed.stdout.as_deref(), Some("16"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn interpolated_string_with_declaration_body_effect_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

get_val : Int -> Int
get_val n =
  next n

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print "result=${get_val 3}"
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(
        &module,
        "interpolated string with declaration body effect call",
    );
    assert_eq!(typed.stdout.as_deref(), Some("result=4"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn pipeline_value_with_declaration_body_effect_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

get_val : Int -> Int
get_val n =
  next n

main : Unit -> Unit
main =
  with
    next n -> resume (n + 1)
  in
    print (get_val 3 |> get_val)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "pipeline value with declaration body effect call");
    assert_eq!(typed.stdout.as_deref(), Some("5"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_list_each_with_effect_callback() {
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
    list.each [2, 4, 6] (|n| -> log n)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "list each with effect callback");
    assert_eq!(typed.stdout.as_deref(), Some("246"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_with_handler_variable() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  h = handler
    log msg ->
      print msg
      resume ()
  with h
  in
    log "parity"
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "with handler variable");
    assert_eq!(typed.stdout.as_deref(), Some("parity"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_with_captures_lexical_local() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  prefix = "pre:"
  with
    log msg ->
      print "${prefix}${msg}"
      resume ()
  in
    log "hello"
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "with captures lexical local");
    assert_eq!(typed.stdout.as_deref(), Some("pre:hello"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_nested_with_nearest_handler_wins() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "outer"
      resume ()
  in
    with
      log msg ->
        print "inner"
        resume ()
    in
      log "x"
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "nested with nearest handler wins");
    assert_eq!(typed.stdout.as_deref(), Some("inner"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_qualified_effect_call_dispatch() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    Log.log "qualified"
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "qualified effect call dispatch");
    assert_eq!(typed.stdout.as_deref(), Some("qualified"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_pipeline_effect_call_dispatch() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    "piped" |> log
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "pipeline effect call dispatch");
    assert_eq!(typed.stdout.as_deref(), Some("piped"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_basic_with_inline_handler_dispatch() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    log "hello"
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "basic with inline handler dispatch");
    assert_eq!(typed.stdout.as_deref(), Some("hello"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_resume_outside_handler_error() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
main : Unit -> Unit
main =
  print (resume 1)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "resume outside handler error");
    assert_eq!(typed.runtime_error_kind, Some("continuation_missing"));
}

#[test]
fn typed_mode_matches_fallback_for_multi_arg_effect_op_call() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

main : Unit -> Unit
main =
  with
    yield _ step ->
      resume (True, step + 1)
  in
    print (yield "x" 41)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "multi-arg effect op call");
    assert_eq!(typed.stdout.as_deref(), Some("(True, 42)"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_qualified_call_without_active_handler() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  Log.log "unreachable"
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "qualified call without active handler");
    assert_eq!(typed.stdout, None);
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_lambda_closure_capture() {
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
    let typed = assert_mode_parity(&module, "lambda closure capture");
    assert_eq!(typed.stdout.as_deref(), Some("4142"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_list_each_style_callback_dispatch() {
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
    let typed = assert_mode_parity(&module, "list each style callback dispatch");
    assert_eq!(typed.stdout.as_deref(), Some("35"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn case_scrutinee_suspends_and_arm_body_calls_effect() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool
effect Iter
  next: Int -> Int

choose : Int -> Int
choose n =
  case flag n
    True  -> next 5
    False -> next 0

main : Unit -> Unit
main =
  with
    flag _ -> resume True
    next n -> resume (n + 1)
  in
    print (choose 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "case scrutinee suspends and arm body calls effect");
    assert_eq!(typed.stdout.as_deref(), Some("6"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn if_condition_suspends_and_branch_body_calls_effect() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool
effect Iter
  next: Int -> Int

choose : Int -> Int
choose n =
  if flag n
    next 5
  else
    next 0

main : Unit -> Unit
main =
  with
    flag _ -> resume True
    next n -> resume (n + 1)
  in
    print (choose 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(
        &module,
        "if condition suspends and branch body calls effect",
    );
    assert_eq!(typed.stdout.as_deref(), Some("6"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn resolves_list_index_int_access() {
    let _guard = ENV_MUTEX.lock().unwrap();
    // Note: `print xs[1]` would parse as `(print xs)[1]` (call-receiver precedence).
    // Use `print (xs[1])` to explicitly index then print.
    let source = r#"
xs : List Int
xs = [10, 20, 30]

main : Unit -> Unit
main =
  print (xs[1])
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "list index access on int list");
    assert_eq!(typed.stdout.as_deref(), Some("20"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn resolves_list_index_string_access() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
words : List String
words = ["hello", "world"]

main : Unit -> Unit
main =
  print (words[0])
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "list index access on string list");
    assert_eq!(typed.stdout.as_deref(), Some("hello"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn list_index_out_of_bounds_aborts() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
xs : List Int
xs = [1, 2]

v : Int
v = xs[5]

main : Unit -> Unit
main =
  print v
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    // OOB triggers abort: no output produced
    assert_eq!(output, None);
}

#[test]
fn list_index_negative_index_aborts() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
xs : List Int
xs = [1, 2]

v : Int
v = xs[-1]

main : Unit -> Unit
main =
  print v
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    // Negative index aborts: no output produced
    assert_eq!(output, None);
}

#[test]
fn list_index_string_list_out_of_bounds_aborts() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
words : List String
words = ["a", "b"]

v : String
v = words[9]

main : Unit -> Unit
main =
  print v
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    // OOB on string list triggers abort: no output produced
    assert_eq!(output, None);
}
