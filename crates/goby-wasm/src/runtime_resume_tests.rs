use std::path::PathBuf;
use std::sync::Mutex;

use goby_core::{Module, Stmt, parse_module};

use crate::{
    assert_mode_parity, assert_perf_within_threshold, lower, measure_runtime_mode_micros,
    resolve_module_runtime_output,
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

fn main_parsed_body(module: &Module) -> Option<&[Stmt]> {
    module
        .declarations
        .iter()
        .find(|decl| decl.name == "main")
        .and_then(|decl| decl.parsed_body.as_deref())
}

#[test]
fn positional_single_field_constructor_dispatches_handler() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
type Error = Error(message: String)

effect RaiseError
  raise: Error -> Unit

main : Unit -> Unit
main =
  with
    raise e ->
      print e.message
      resume ()
  in
    raise Error("oops")
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("oops"),
        "positional single-field constructor should dispatch handler with correct record value"
    );
}

#[test]
fn resume_in_handler_returns_value_to_effect_call_site() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume 7
  in
    print (next 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("7"),
        "resume should return value to the operation call site"
    );
}

#[test]
fn one_shot_resume_guard_rejects_second_resume_on_same_token() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (resume 1)
  in
    print (next 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref().map(|s| s.contains("[E-RESUME-CONSUMED]")),
        Some(true),
        "second resume on one-shot token should surface a deterministic runtime error"
    );
}

#[test]
fn no_resume_in_value_position_exits_current_with_scope() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  result =
    with
      next n ->
        print "handled"
        42
    in
      next 0 + 1
  print result
  print "after"
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("handled42after"),
        "no-resume in value position should exit only the current with body and yield the clause result"
    );
}

#[test]
fn no_resume_in_unit_position_skips_remaining_with_body_only() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "handled:${msg}"
  in
    log "hello"
    print "inner-after"
  print "outer-after"
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("handled:helloouter-after"),
        "no-resume in unit position should skip only the remaining statements in the current with body"
    );
}

#[test]
fn nested_scoped_exit_only_leaves_inner_with_scope() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Outer
  op: String -> Unit

effect Inner
  boom: String -> Int

main : Unit -> Unit
main =
  with
    op msg ->
      inner =
        with
          boom inner ->
            print "inner:${inner}"
            7
        in
          boom msg + 1
      print "outer:${inner}"
      resume ()
  in
    op "x"
    print "main-after"
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("inner:xouter:7main-after"),
        "nested no-resume should exit only the targeted inner with scope and let outer execution continue"
    );
}

#[test]
fn resume_outside_handler_surfaces_runtime_error() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
main : Unit -> Unit
main =
  print (resume 1)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref().map(|s| s.contains("[E-RESUME-MISSING]")),
        Some(true),
        "resume outside handler should report runtime error in fallback runtime"
    );
}

#[test]
fn with_dispatches_effect_operation_in_runtime() {
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
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("hello"),
        "with should install inline handler and dispatch operation"
    );
}

#[test]
fn with_variable_dispatches_effect_operation_in_runtime() {
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
    log "world"
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("world"),
        "with <handler-var> should install stored handler value and dispatch operation"
    );
}

#[test]
fn with_captures_lexical_local_in_runtime() {
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
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("pre:hello"),
        "handler value should capture lexical locals used inside clause body"
    );
}

#[test]
fn handler_mutation_persists_across_repeated_calls_in_same_with() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Counter
  next: Unit -> Int

main : Unit -> Unit
main =
  mut counter = 0
  with
    next _ ->
      counter := counter + 1
      resume counter
  in
    print (next ())
    print (next ())
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("12"),
        "handler mutation should persist across repeated calls in the same with scope"
    );
}

#[test]
fn nested_with_prefers_nearest_inline_handler() {
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
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("inner"),
        "nearest inline handler should win under nested with blocks"
    );
}

#[test]
fn inner_with_overrides_outer_with() {
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
        print "inline"
        resume ()
    in
      log "x"
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("inline"),
        "inner with should take precedence over outer with"
    );
}

#[test]
fn with_dispatches_qualified_effect_call_in_runtime() {
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
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("qualified"),
        "qualified effect call should dispatch to active inline handler for that effect"
    );
}

#[test]
fn typed_mode_matches_fallback_for_resume_success_path() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume 7
  in
    print (next 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "resume success path");
    assert_eq!(typed.stdout.as_deref(), Some("7"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_no_resume_value_scope_exit() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  result =
    with
      next n ->
        print "handled"
        42
    in
      next 0 + 1
  print result
  print "after"
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "no-resume value scope exit");
    assert_eq!(typed.stdout.as_deref(), Some("handled42after"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_no_resume_unit_scope_exit() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "handled:${msg}"
  in
    log "hello"
    print "inner-after"
  print "outer-after"
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "no-resume unit scope exit");
    assert_eq!(typed.stdout.as_deref(), Some("handled:helloouter-after"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_nested_scoped_exit() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Outer
  op: String -> Unit

effect Inner
  boom: String -> Int

main : Unit -> Unit
main =
  with
    op msg ->
      inner =
        with
          boom inner ->
            print "inner:${inner}"
            7
        in
          boom msg + 1
      print "outer:${inner}"
      resume ()
  in
    op "x"
    print "main-after"
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "nested scoped-exit handler path");
    assert_eq!(typed.stdout.as_deref(), Some("inner:xouter:7main-after"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_double_resume_error() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (resume 1)
  in
    print (next 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "double-resume deterministic error path");
    assert_eq!(typed.stdout, None);
    assert_eq!(typed.runtime_error_kind, Some("continuation_consumed"));
}

#[test]
fn resume_replays_remaining_unit_statements_before_second_resume_error() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "handled:${msg}"
      resume ()
      print "after-resume"
      resume ()
  in
    log "hello"
    print "continued"
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert!(
        output.starts_with("handled:hellocontinuedafter-resume"),
        "first resume should replay remaining unit statements before handler continues"
    );
    assert!(
        output.contains("[E-RESUME-CONSUMED]"),
        "second resume after replayed continuation completion should report exhaustion"
    );
}

#[test]
fn typed_mode_matches_fallback_for_resume_replay_then_exhaustion() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "handled:${msg}"
      resume ()
      print "after-resume"
      resume ()
  in
    log "hello"
    print "continued"
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "resume replay then exhaustion");
    assert_eq!(
        typed.stdout.as_deref(),
        Some("handled:hellocontinuedafter-resume")
    );
    assert_eq!(typed.runtime_error_kind, Some("continuation_consumed"));
}

#[test]
fn resume_replays_binding_value_continuation_into_following_statements() {
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
    print y
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(
        output, "2",
        "resume should bind the resumed value and continue through later statements"
    );
}

#[test]
fn typed_mode_matches_fallback_for_binding_value_replay() {
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
    print y
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "binding value replay");
    assert_eq!(typed.stdout.as_deref(), Some("2"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn declaration_value_call_replays_nested_binding_progression() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

yield_int_state : String -> Int -> (Bool, Int) can Iterator
yield_int_state value state = yield value state

count_values : Unit -> Int can Iterator
count_values =
  s1 = yield_int_state "a" 0
  s2 = if s1.0
    yield_int_state "b" s1.1
  else
    s1
  s3 = if s2.0
    yield_int_state "c" s2.1
  else
    s2
  s3.1

main : Unit -> Unit
main =
  with
    yield _ step ->
      resume (True, step + 1)
  in
    print (count_values ())
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "3");
}

#[test]
fn typed_mode_matches_fallback_for_declaration_value_call_progression() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

yield_int_state : String -> Int -> (Bool, Int) can Iterator
yield_int_state value state = yield value state

count_values : Unit -> Int can Iterator
count_values =
  s1 = yield_int_state "a" 0
  s2 = if s1.0
    yield_int_state "b" s1.1
  else
    s1
  s3 = if s2.0
    yield_int_state "c" s2.1
  else
    s2
  s3.1

main : Unit -> Unit
main =
  with
    yield _ step ->
      resume (True, step + 1)
  in
    print (count_values ())
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "declaration value call progression");
    assert_eq!(typed.stdout.as_deref(), Some("3"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
#[ignore = "qualified iterator handler clauses are not yet covered by fallback/typed parity locking"]
fn typed_mode_matches_fallback_for_iterator_unified_example_shape() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = read_example("iterator_unified.gb");
    let module = parse_module(&source).expect("iterator_unified.gb should parse");
    let typed = assert_mode_parity(&module, "iterator unified progression shape");
    assert_eq!(typed.stdout.as_deref(), Some("tick:atick:btick:c31"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn resume_replays_single_arg_call_continuation_in_value_position() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

id : Int -> Int
id x = x

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (id (next 0))
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1");
}

#[test]
fn typed_mode_matches_fallback_for_single_arg_call_value_replay() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

id : Int -> Int
id x = x

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (id (next 0))
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "single-arg call value replay");
    assert_eq!(typed.stdout.as_deref(), Some("1"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn resume_replays_binop_left_operand_continuation() {
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
    print (next 0 + 4)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "5");
}

#[test]
fn resume_replays_binop_right_operand_continuation() {
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
    print (4 + next 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "5");
}

#[test]
fn typed_mode_matches_fallback_for_binop_operand_replay() {
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
    print (4 + next 0)
    print (next 0 + 4)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "binop operand replay");
    assert_eq!(typed.stdout.as_deref(), Some("55"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_binop_both_operands_suspend() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      print n
      resume (n + 3)
  in
    print (next 0 + next 10)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "binop both operands suspend");
    assert_eq!(typed.stdout.as_deref(), Some("01016"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn resume_replays_if_condition_continuation() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

choose : Int -> Int
choose n =
  if flag n
    10
  else
    20

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
  in
    print (choose 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "10");
}

#[test]
fn typed_mode_matches_fallback_for_if_condition_replay() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

choose : Int -> Int
choose n =
  if flag n
    10
  else
    20

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
  in
    print (choose 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "if condition replay");
    assert_eq!(typed.stdout.as_deref(), Some("20"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn unit_position_if_condition_replay_uses_suspended_frame_path() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
  in
    if flag 0
      print 10
    else
      print 20
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "10");
}

#[test]
fn typed_mode_matches_fallback_for_unit_position_if_condition_replay() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
  in
    if flag 0
      print 10
    else
      print 20
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "unit-position if condition replay");
    assert_eq!(typed.stdout.as_deref(), Some("20"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn unit_position_if_selected_branch_replays_value_path() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
    next n ->
      resume (n + 1)
  in
    if flag 0
      print (next 0)
    else
      print 99
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1");
}

#[test]
fn typed_mode_matches_fallback_for_unit_position_if_branch_value_replay() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
    next n ->
      resume (n + 1)
  in
    if flag 0
      print 99
    else
      print (next 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "unit-position if branch value replay");
    assert_eq!(typed.stdout.as_deref(), Some("1"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn binding_rhs_if_replays_through_outcome_path() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
    next n ->
      resume (n + 1)
  in
    value = if flag 0
      next 0
    else
      99
    print value
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1");
}

#[test]
fn typed_mode_matches_fallback_for_binding_rhs_if_outcome_path() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
    next n ->
      resume (n + 2)
  in
    value = if flag 0
      99
    else
      next 0
    print value
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "binding rhs if outcome path");
    assert_eq!(typed.stdout.as_deref(), Some("2"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn assignment_rhs_if_replays_through_outcome_path() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
    next n ->
      resume (n + 1)
  in
    value = 0
    value = if flag 0
      next 0
    else
      99
    print value
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1");
}

#[test]
fn typed_mode_matches_fallback_for_assignment_rhs_if_outcome_path() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
    next n ->
      resume (n + 3)
  in
    value = 0
    value = if flag 0
      99
    else
      next 0
    print value
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "assignment rhs if outcome path");
    assert_eq!(typed.stdout.as_deref(), Some("3"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn resume_replays_case_scrutinee_continuation() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

choose : Int -> Int
choose n =
  case flag n
    True -> 10
    False -> 20

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
  in
    print (choose 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "10");
}

#[test]
fn typed_mode_matches_fallback_for_case_scrutinee_replay() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Pred
  flag: Int -> Bool

choose : Int -> Int
choose n =
  case flag n
    True -> 10
    False -> 20

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
  in
    print (choose 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "case scrutinee replay");
    assert_eq!(typed.stdout.as_deref(), Some("20"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn parenthesized_multiline_case_call_uses_parsed_body() {
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
    print (
      case 0
        0 -> next 0
        _ -> 99
    )
"#;
    let module = parse_module(source).expect("parse should work");
    assert!(
        main_parsed_body(&module).is_some(),
        "main parsed_body should exist for parenthesized multiline case call"
    );
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1");
}

#[test]
fn typed_mode_matches_fallback_for_parenthesized_multiline_case_call() {
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
    print (
      case 0
        0 -> next 0
        _ -> 99
    )
"#;
    let module = parse_module(source).expect("parse should work");
    assert!(
        main_parsed_body(&module).is_some(),
        "main parsed_body should exist for parenthesized multiline case call"
    );
    let typed = assert_mode_parity(&module, "parenthesized multiline case call");
    assert_eq!(typed.stdout.as_deref(), Some("2"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn parenthesized_multiline_case_block_body_replays_value_path() {
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
    print (
      case 0
        0 ->
          x = next 0
          x + 10
        _ -> 99
    )
"#;
    let module = parse_module(source).expect("parse should work");
    assert!(
        main_parsed_body(&module).is_some(),
        "main parsed_body should exist for parenthesized multiline case block call"
    );
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "11");
}

#[test]
fn typed_mode_matches_fallback_for_parenthesized_multiline_case_block_call() {
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
    print (
      case 0
        0 ->
          x = next 0
          x + 10
        _ -> 99
    )
"#;
    let module = parse_module(source).expect("parse should work");
    assert!(
        main_parsed_body(&module).is_some(),
        "main parsed_body should exist for parenthesized multiline case block call"
    );
    let typed = assert_mode_parity(&module, "parenthesized multiline case block call");
    assert_eq!(typed.stdout.as_deref(), Some("12"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn resume_replays_multi_arg_named_call_arguments() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

sum3 : Int -> Int -> Int -> Int
sum3 a b c = a + b + c

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (sum3 (next 0) 2 3)
    print (sum3 1 (next 0) 3)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "65");
}

#[test]
fn typed_mode_matches_fallback_for_multi_arg_named_call_replay() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

sum3 : Int -> Int -> Int -> Int
sum3 a b c = a + b + c

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    print (sum3 (next 0) 2 3)
    print (sum3 1 (next 0) 3)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "multi-arg named call replay");
    assert_eq!(typed.stdout.as_deref(), Some("76"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn resume_replays_receiver_method_call_argument() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

effect Log
  log: Int -> Int

pick : Int -> Int
pick n =
  Log.log (next n)

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
    log n ->
      resume (n + 10)
  in
    print (pick 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "11");
}

#[test]
fn typed_mode_matches_fallback_for_receiver_method_call_argument_replay() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

effect Log
  log: Int -> Int

pick : Int -> Int
pick n =
  Log.log (next n)

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
    log n ->
      resume (n + 20)
  in
    print (pick 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "receiver method call argument replay");
    assert_eq!(typed.stdout.as_deref(), Some("22"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn resume_replays_pipeline_value_continuation() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

id : Int -> Int
id x = x

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (next 0 |> id)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module).expect("runtime output should resolve");
    assert_eq!(output, "1");
}

#[test]
fn resume_replays_bare_var_call_arg_in_side_effect_position() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

effect Log
  log: Int -> Unit

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 5)
    log v ->
      print v
      resume ()
  in
    log (next 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "bare var call arg replay in side-effect position");
    assert_eq!(typed.stdout.as_deref(), Some("5"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn resume_replays_bare_var_call_arg_in_execute_unit_expr_ast_path() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

effect Log
  log: Int -> Unit

run_log : Unit -> Unit
run_log _ =
  log (next 0)

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 7)
    log v ->
      print v
      resume ()
  in
    run_log ()
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(
        &module,
        "bare var call arg replay in execute_unit_expr_ast path",
    );
    assert_eq!(typed.stdout.as_deref(), Some("7"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_pipeline_value_replay() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Iter
  next: Int -> Int

id : Int -> Int
id x = x

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    print (next 0 |> id)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "pipeline value replay");
    assert_eq!(typed.stdout.as_deref(), Some("2"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_nearest_handler_dispatch() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect A
  next: Int -> Int

effect B
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1
  in
    with
      next y ->
        resume 2
    in
    print (B.next 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "qualified nearest-handler dispatch path");
    assert_eq!(typed.stdout.as_deref(), Some("2"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
fn typed_mode_matches_fallback_for_nested_same_effect_nearest_handler_dispatch() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect Log
  log: String -> String

main : Unit -> Unit
main =
  with
    log msg ->
      resume "outer"
  in
    with
      log msg ->
        resume "inner"
    in
      print (log "x")
"#;
    let module = parse_module(source).expect("parse should work");
    let typed = assert_mode_parity(&module, "nested same-effect nearest-handler dispatch path");
    assert_eq!(typed.stdout.as_deref(), Some("inner"));
    assert_eq!(typed.runtime_error_kind, None);
}

#[test]
#[ignore = "performance acceptance protocol (Step 8.6); run explicitly with --ignored"]
fn step8_perf_acceptance_resume_heavy_samples() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let warmup_runs = 5usize;
    let measured_runs = 30usize;
    let max_slowdown_ratio = 1.03f64;
    let perf_samples = [
        (
            "resume_success_path",
            r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume 7
  in
    print (next 0)
"#,
        ),
        (
            "double_resume_error_path",
            r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (resume 1)
  in
    print (next 0)
"#,
        ),
        (
            "nested_handler_dispatch_path",
            r#"
effect Log
  log: String -> String

main : Unit -> Unit
main =
  with
    log msg ->
      resume "outer"
  in
    with
      log msg ->
        resume "inner"
    in
      print (log "x")
"#,
        ),
    ];

    for (name, source) in perf_samples {
        let module = parse_module(source).expect("performance sample should parse");
        let fallback = measure_runtime_mode_micros(
            &module,
            lower::EffectExecutionMode::PortableFallback,
            warmup_runs,
            measured_runs,
        );
        let typed = measure_runtime_mode_micros(
            &module,
            lower::EffectExecutionMode::TypedContinuationOptimized,
            warmup_runs,
            measured_runs,
        );
        assert_perf_within_threshold(name, fallback, typed, max_slowdown_ratio);
    }
}

#[test]
fn qualified_resume_with_overlapping_method_names_uses_target_handler() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect A
  next: Int -> Int

effect B
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1
  in
    with
      next y ->
        resume 2
    in
    print (B.next 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("2"),
        "qualified call should dispatch to the matching effect handler even with overlapping method names"
    );
}

#[test]
fn resume_does_not_leak_handler_context_between_calls() {
    let _guard = ENV_MUTEX.lock().unwrap();
    let source = r#"
effect A
  next: Int -> Int

effect B
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1
  in
    with
      next y ->
        resume 2
    in
    print (B.next 0)
    print (B.next 0)
"#;
    let module = parse_module(source).expect("parse should work");
    let output = resolve_module_runtime_output(&module);
    assert_eq!(
        output.as_deref(),
        Some("22"),
        "handler context should remain stable after resume across multiple qualified calls"
    );
}
