use std::path::Path;

use crate::{
    Module,
    ast::Span,
    typecheck_phase::{
        build_checking_phase, check_declaration_bodies, default_typecheck_stdlib_root,
        validate_module_phase,
    },
};

#[cfg(test)]
use crate::{
    ast::Expr,
    typecheck_env::{ResumeContext, Ty, TypeEnv},
    typecheck_resume::infer_binding_ty_with_resume_context,
};
#[cfg(test)]
use std::collections::HashMap;

pub(crate) const PRELUDE_MODULE_PATH: &str = "goby/prelude";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypecheckError {
    pub declaration: Option<String>,
    /// Source location of the error, if known.
    pub span: Option<Span>,
    pub message: String,
}

impl std::fmt::Display for TypecheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let target = self.declaration.as_deref().unwrap_or("<module>");
        match &self.span {
            Some(s) => write!(
                f,
                "typecheck error in {} at line {}:{}: {}",
                target, s.line, s.col, self.message
            ),
            None => write!(f, "typecheck error in {}: {}", target, self.message),
        }
    }
}

impl std::error::Error for TypecheckError {}

pub fn typecheck_module(module: &Module) -> Result<(), TypecheckError> {
    typecheck_module_with_context(module, None, None)
}

pub fn typecheck_module_with_context(
    module: &Module,
    source_path: Option<&Path>,
    stdlib_root: Option<&Path>,
) -> Result<(), TypecheckError> {
    let stdlib_root_path = default_typecheck_stdlib_root(stdlib_root);
    let validation = validate_module_phase(module, source_path, stdlib_root, &stdlib_root_path)?;
    let checking = build_checking_phase(module, &stdlib_root_path, &validation)?;
    check_declaration_bodies(module, &checking)
}

pub(crate) fn is_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    use crate::parse_module;
    use crate::parser_test_support::read_example;
    use crate::stdlib::StdlibResolver;

    use super::*;

    struct TempDirGuard {
        path: PathBuf,
    }

    impl TempDirGuard {
        fn new(label: &str) -> Self {
            let nanos = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("clock should be monotonic enough for tests")
                .as_nanos();
            let path = std::env::temp_dir().join(format!(
                "goby_typecheck_{}_{}_{}",
                label,
                std::process::id(),
                nanos
            ));
            fs::create_dir_all(&path).expect("temp directory should be creatable");
            Self { path }
        }
    }

    impl Drop for TempDirGuard {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    #[test]
    fn typechecks_examples() {
        let hello = read_example("hello.gb");
        let basic = read_example("basic_types.gb");
        let generic_types = read_example("generic_types.gb");
        let import_example = read_example("import.gb");
        let control_flow = read_example("control_flow.gb");
        let type_example = read_example("type.gb");
        let effect_example = read_example("effect.gb");
        let iterator_example = read_example("iterator.gb");
        let iterator_unified_example = read_example("iterator_unified.gb");
        let list_case_example = read_example("list_case.gb");

        let hello_module = parse_module(&hello).expect("hello should parse");
        let basic_module = parse_module(&basic).expect("basic_types should parse");
        let generic_types_module =
            parse_module(&generic_types).expect("generic_types should parse");
        let import_module = parse_module(&import_example).expect("import example should parse");
        let control_flow_module = parse_module(&control_flow).expect("control_flow should parse");
        let type_module = parse_module(&type_example).expect("type should parse");
        let effect_module = parse_module(&effect_example).expect("effect.gb should parse");
        let iterator_module = parse_module(&iterator_example).expect("iterator.gb should parse");
        let iterator_unified_module =
            parse_module(&iterator_unified_example).expect("iterator_unified.gb should parse");
        let list_case_module = parse_module(&list_case_example).expect("list_case should parse");

        typecheck_module(&hello_module).expect("hello should typecheck");
        typecheck_module(&basic_module).expect("basic_types should typecheck");
        typecheck_module(&generic_types_module).expect("generic_types should typecheck");
        typecheck_module(&import_module).expect("import example should typecheck");
        typecheck_module(&control_flow_module).expect("control_flow should typecheck");
        typecheck_module(&type_module).expect("type example should typecheck");
        typecheck_module(&effect_module).expect("effect.gb should typecheck");
        typecheck_module(&iterator_module).expect("iterator.gb should typecheck");
        typecheck_module(&iterator_unified_module).expect("iterator_unified.gb should typecheck");
        typecheck_module(&list_case_module).expect("list_case should typecheck");
    }

    #[test]
    fn rejects_void_main_type() {
        let module =
            parse_module("main : void -> void\nmain = print \"legacy\"\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("void main type should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(err.message.contains("use `Unit`"));
    }

    #[test]
    fn rejects_void_in_non_main_annotation() {
        let module = parse_module("legacy : void\nlegacy = 0\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("void annotation should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("legacy"));
        assert!(err.message.contains("use `Unit`"));
    }

    #[test]
    fn rejects_empty_effect_list() {
        let module = parse_module("x : Int can \nx = 1\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("empty effect list should fail");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(err.message.contains("effect list"));
    }

    #[test]
    fn accepts_tab_separated_effect_clause() {
        let source = "effect Log\n  log: String -> Unit\nx : Int can\tLog\nx = 1\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("tab-separated `can` clause should be accepted");
    }

    #[test]
    fn accepts_well_formed_type_declarations() {
        let source = "\
type UserID = String
type UserStatus = Activated | Deactivated
type User = User(id: UserID, name: String, status: UserStatus)
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("well-formed type declarations should pass");
    }

    #[test]
    fn typechecks_record_constructor_and_field_access() {
        let source = "\
type UserID = String
type UserStatus = Activated | Deactivated
type User = User(id: UserID, name: String, status: UserStatus)
get_name : Unit -> String
get_name =
  user = User(id: \"1234\", name: \"John\", status: UserStatus.Activated)
  user.name
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("record constructor and field access should typecheck");
    }

    #[test]
    fn rejects_duplicate_field_in_record_constructor_call() {
        let source = "\
type UserID = String
type UserStatus = Activated | Deactivated
type User = User(id: UserID, name: String, status: UserStatus)
mk : Unit -> User
mk =
  User(id: \"1234\", id: \"5678\", status: UserStatus.Activated)
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate constructor field should fail");
        assert_eq!(err.declaration.as_deref(), Some("mk"));
        assert!(err.message.contains("duplicate field"));
    }

    #[test]
    fn resolves_nested_aliases_when_checking_type_compatibility() {
        let mut type_aliases = HashMap::new();
        type_aliases.insert("UserID".to_string(), Ty::Str);
        let env = TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases,
            record_types: HashMap::new(),
        };
        let expected = Ty::List(Box::new(Ty::Con {
            name: "UserID".to_string(),
            args: Vec::new(),
        }));
        let actual = Ty::List(Box::new(Ty::Str));
        assert!(env.are_compatible(&expected, &actual));
    }

    #[test]
    fn rejects_duplicate_type_declarations() {
        let source = "\
type User = String
type User = Int
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate type declarations should fail");
        assert_eq!(err.declaration.as_deref(), Some("User"));
        assert!(err.message.contains("duplicate type declaration"));
    }

    #[test]
    fn rejects_duplicate_effect_declarations() {
        let source = "\
effect Log
  log: String -> Unit
effect Log
  log: String -> Unit
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate effect declarations should fail");
        assert_eq!(err.declaration.as_deref(), Some("Log"));
        assert!(err.message.contains("duplicate effect declaration"));
    }

    // -----------------------------------------------------------------------
    // with / unhandled-effect tests
    // -----------------------------------------------------------------------

    #[test]
    fn rejects_direct_effect_op_call_outside_with() {
        // `log x` is called directly in `main` without any `with`.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        let err =
            typecheck_module(&module).expect_err("unhandled effect op call should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(
            err.message.contains("not handled"),
            "unexpected message: {}",
            err.message
        );
        assert!(err.message.contains("log"));
    }

    #[test]
    fn accepts_effect_op_call_inside_with_scope() {
        // `log x` is called inside a `with` block.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("effect op call inside with should be accepted");
    }

    #[test]
    fn accepts_effect_op_call_inside_with() {
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    log msg ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("effect op call inside with should be accepted");
    }

    #[test]
    fn accepts_effect_op_call_inside_with_variable() {
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  h = handler
    log msg ->
      resume ()
  with h
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("effect op call inside with <handler-var> should be accepted");
    }

    #[test]
    fn rejects_unknown_operation_in_handler_expression() {
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    unknown_op msg ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("unknown operation in handler expression should fail");
        assert!(err.message.contains("unknown effect operation"));
        assert!(err.message.contains("unknown_op"));
    }

    #[test]
    fn rejects_ambiguous_operation_in_handler_expression() {
        let source = "\
effect Log
  log: String -> Unit
effect Logger
  log: String -> Unit
main : Unit -> Unit
main =
  with
    log msg ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("ambiguous operation in handler expression should fail");
        assert!(err.message.contains("ambiguous"));
        assert!(err.message.contains("log"));
    }

    #[test]
    fn resolves_ambiguous_operation_via_qualified_handler_clause_name() {
        // When two effects share the same op name, using qualified form in the handler
        // clause head should work (qualified call in body avoids name-resolution ambiguity).
        let source = "\
effect Log
  log: String -> Unit
effect Logger
  log: String -> Unit
main : Unit -> Unit
main =
  with
    Log.log msg ->
      resume ()
  in
    Log.log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("qualified handler clause name should resolve ambiguous operation");
    }

    #[test]
    fn rejects_unknown_qualified_operation_in_handler_expression() {
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    Log.nonexistent msg ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("unknown qualified operation should fail");
        assert!(err.message.contains("unknown effect operation"));
        assert!(err.message.contains("Log.nonexistent"));
    }

    #[test]
    fn rejects_duplicate_bare_and_qualified_handler_clauses() {
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    log msg ->
      resume ()
    Log.log msg ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("duplicate bare and qualified handler clauses should fail");
        assert!(err.message.contains("duplicate handler clause"));
        assert!(err.message.contains("log"));
    }

    #[test]
    fn rejects_with_non_handler_expression() {
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with 1
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("with non-handler expression should fail");
        assert!(err.message.contains("with"));
        assert!(err.message.contains("handler value"));
    }

    #[test]
    fn accepts_handler_return_annotation_with_matching_handler_value() {
        let source = "\
effect Log
  log: String -> Unit
mk : Unit -> Handler(Log)
mk =
  h = handler
    log msg ->
      resume ()
  h
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("matching Handler(Log) annotation should pass");
    }

    #[test]
    fn accepts_handler_return_annotation_with_order_insensitive_effect_list() {
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> Unit
mk : Unit -> Handler(Env, Log)
mk =
  h = handler
    log msg ->
      resume ()
    from_env key ->
      resume ()
  h
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("Handler effect list order should be ignored");
    }

    #[test]
    fn rejects_handler_return_annotation_when_effect_set_mismatches() {
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> Unit
mk : Unit -> Handler(Log)
mk =
  h = handler
    from_env key ->
      resume ()
  h
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("mismatched Handler annotation should fail");
        assert!(err.message.contains("body type"));
        assert!(err.message.contains("Handler"));
    }

    #[test]
    fn rejects_unknown_effect_in_handler_type_annotation() {
        let source = "\
effect Log
  log: String -> Unit
mk : Unit -> Handler(Log, MissingEffect)
mk =
  handler
    log msg ->
      resume ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("unknown effect in Handler annotation should fail");
        assert!(err.message.contains("unknown effect"));
        assert!(err.message.contains("Handler"));
    }

    #[test]
    fn accepts_qualified_effect_op_inside_with() {
        // `Log.log x` (qualified form) inside a `with` block.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    Log.log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("qualified effect op call inside with should be accepted");
    }

    #[test]
    fn rejects_effect_op_when_wrong_handler_used() {
        // `with` only covers `Log` ops; calling `Env.from_env` is unhandled.
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> String
main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    from_env \"PATH\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("wrong handler should not cover unrelated effect op");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(err.message.contains("not handled"));
        assert!(err.message.contains("from_env"));
    }

    #[test]
    fn accepts_can_clause_ops_inside_function_body() {
        // A function with `can Log` may call `log` in its own body without handlers at call site.
        let source = "\
effect Log
  log: String -> Unit
f : String -> Unit can Log
f msg =
  log msg
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("can-declared effect op should be allowed in function body");
    }

    #[test]
    fn rejects_effect_op_in_binding_value_outside_with() {
        // Effect op used in binding RHS, no enclosing `with`.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  x = log \"hi\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("effect op in binding outside with should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(err.message.contains("not handled"));
    }

    #[test]
    fn rejects_effect_op_as_pipeline_callee_outside_with() {
        // `"hello" |> log` — effect op used as pipeline callee without `with`.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  \"hello\" |> log
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("effect op as pipeline callee outside with should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(err.message.contains("not handled"));
        assert!(err.message.contains("log"));
    }

    #[test]
    fn accepts_effect_op_as_pipeline_callee_inside_with_scope() {
        // `"hello" |> log` inside `with` should be accepted.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    \"hello\" |> log
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("effect op as pipeline callee inside with should be accepted");
    }

    #[test]
    fn accepts_lambda_param_shadowing_effect_op_name() {
        // `|log| -> log "hi"` — `log` inside the lambda refers to the parameter,
        // not the effect op; should not be flagged as unhandled.
        let source = "\
effect Log
  log: String -> Unit
main : Unit -> Unit
main =
  f = |log| -> log \"hi\"
  f \"ignored\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("lambda param shadowing effect op name should not be flagged");
    }

    #[test]
    fn accepts_nested_with_with_merged_covered_ops() {
        // Outer `with(log)` + inner `with(from_env)`; inner body calls both ops.
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> String
main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    with
      from_env str ->
        resume str
    in
      log \"hi\"
      from_env \"PATH\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("nested with scopes with merged covered ops should be accepted");
    }

    #[test]
    fn accepts_multi_op_effect_via_can_clause() {
        // `can Log` where `Log` has two ops (`log` and `warn`); both usable in the body.
        let source = "\
effect Log
  log: String -> Unit
  warn: String -> Unit
f : String -> Unit can Log
f msg =
  log msg
  warn msg
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("multi-op effect via can clause should allow all ops in body");
    }

    // ── Step 3: calling effectful functions requires an appropriate handler scope ──

    #[test]
    fn rejects_call_to_effectful_function_outside_with() {
        // `plus_ten_with_log` requires the `Log` effect; calling it from `main` without
        // `with` should be rejected.
        let source = "\
effect Log
  log: String -> Unit
plus_ten_with_log : Int -> Int can Log
plus_ten_with_log n =
  log \"calling\"
  n
main : Unit -> Unit
main =
  plus_ten_with_log 3
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("calling effectful function without with should fail");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(
            err.message.contains("unhandled effect") || err.message.contains("Log"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_call_to_effectful_function_inside_with_scope() {
        // Same call, but wrapped in `with` — should succeed.
        let source = "\
effect Log
  log: String -> Unit
plus_ten_with_log : Int -> Int can Log
plus_ten_with_log n =
  log \"calling\"
  n
main : Unit -> Unit
main =
  with
    log msg ->
      resume ()
  in
    plus_ten_with_log 3
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("calling effectful function inside appropriate with should succeed");
    }

    #[test]
    fn rejects_call_when_partial_handlers_present() {
        // `show_env_var` requires both `Log` and `Env`; only log handler is in scope.
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> String
show_env_var : String -> Unit can Log, Env
show_env_var name =
  v = from_env name
  log v
main : Unit -> Unit
main =
  with
    log msg ->
      resume ()
  in
    show_env_var \"PATH\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("missing Env handler should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("main"));
        assert!(
            err.message.contains("unhandled effect") || err.message.contains("Env"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_effectful_pipeline_callee_inside_with_scope() {
        // `3 |> plus_ten_with_log` inside `with` — pipeline form should also pass.
        let source = "\
effect Log
  log: String -> Unit
plus_ten_with_log : Int -> Int can Log
plus_ten_with_log n =
  log \"calling\"
  n
main : Unit -> Unit
main =
  with
    log msg ->
      resume ()
  in
    3 |> plus_ten_with_log
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("effectful pipeline callee inside with should succeed");
    }

    #[test]
    fn rejects_unknown_type_in_alias_target() {
        let source = "type UserID = UnknownType\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("unknown alias target should fail");
        assert_eq!(err.declaration.as_deref(), Some("UserID"));
        assert!(err.message.contains("unknown type"));
    }

    #[test]
    fn rejects_duplicate_union_constructor_names() {
        let source = "type Flag = On | Off | On\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate union constructor should fail");
        assert_eq!(err.declaration.as_deref(), Some("Flag"));
        assert!(err.message.contains("duplicate constructor"));
    }

    #[test]
    fn rejects_duplicate_record_field_names() {
        let source = "type User = User(id: String, id: String)\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate record field should fail");
        assert_eq!(err.declaration.as_deref(), Some("User"));
        assert!(err.message.contains("duplicate field"));
    }

    #[test]
    fn accepts_record_field_with_list_string_type() {
        let source = "type S = S(xs: List String)\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("record field List String type should be accepted");
    }

    #[test]
    fn rejects_malformed_generic_record_field_type() {
        let source = "type S = S(xs: List (String)\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("malformed generic field type should fail");
        assert_eq!(err.declaration.as_deref(), Some("S"));
        assert!(err.message.contains("invalid field type"));
    }

    #[test]
    fn rejects_invalid_effect_name() {
        // `Log` is declared so the identifier check can reach `1Bad`.
        let source = "effect Log\n  log: String -> Unit\nx : Int can Log, 1Bad\nx = 1\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("invalid effect name should fail");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(err.message.contains("invalid effect name"));
    }

    #[test]
    fn rejects_can_clause_on_effect_member() {
        let source = "\
effect Trace
  trace : String -> Unit can Ghost
main : Unit -> Unit
main = ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("can clause on effect member should fail");
        assert_eq!(err.declaration.as_deref(), Some("Trace"));
        assert!(err.message.contains("can clauses on effect members are not supported"));
    }

    #[test]
    fn rejects_unknown_effect_type_parameter_in_effect_member_type_annotation() {
        let source = "
effect Iter a
  op : b -> Unit
main : Unit -> Unit
main = ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("unknown effect type parameter in member annotation should fail");
        assert!(err.message.contains("unknown effect type parameter `b`"));
    }

    #[test]
    fn rejects_uncovered_effect_op_in_handler_clause_body() {
        let source = "\
effect Log
  log : String -> Unit
effect Trace
  trace : String -> Unit
main : Unit -> Unit can Trace
main =
  with
    trace msg ->
      log msg
  in
    trace \"x\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("handler clause should reject undeclared effect dependency");
        assert!(
            err.message
                .contains("effect operation `log` is not handled")
        );
    }

    #[test]
    fn accepts_effect_in_handler_clause_when_covered_by_outer_with() {
        // handler clause body can use an effect op that is covered by an outer `with`
        let source = "\
effect Log
  log : String -> Unit
effect Trace
  trace : String -> Unit
main : Unit -> Unit
main =
  with
    log str -> resume ()
  in
    with
      trace msg ->
        log msg
    in
      trace \"x\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("handler clause body can use effect op covered by outer with");
    }

    #[test]
    fn accepts_effect_in_handler_clause_when_covered_by_can_clause() {
        // handler clause body can use an effect op that is covered by the function's can clause
        let source = "\
effect Log
  log : String -> Unit
effect Trace
  trace : String -> Unit
f : Unit -> Unit can Trace, Log
f =
  with
    trace msg ->
      log msg
  in
    trace \"x\"
main : Unit -> Unit
main = ()
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("handler clause body can use effect op covered by caller can clause");
    }

    #[test]
    fn rejects_sibling_op_in_handler_clause_body_without_outer_with() {
        // within a handler clause body, sibling ops from the same effect are not in covered_ops.
        // to use them, an outer `with` or the function's `can` clause must cover them.
        let source = "\
effect Foo
  a : String -> Unit
  b : String -> Unit
main : Unit -> Unit
main =
  with
    a msg -> b msg
    b msg -> resume ()
  in
    a \"x\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("sibling op in handler clause body without outer coverage should fail");
        assert!(err.message.contains("effect operation `b` is not handled"));
    }

    #[test]
    fn accepts_can_clause_with_implicit_prelude_print_effect() {
        // `can Print` is accepted via implicit `goby/prelude` embed defaults.
        let source = "main : Unit -> Unit can Print\nmain = print \"hi\"\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("implicit prelude Print effect should be accepted in `can` clause");
    }

    #[test]
    fn accepts_println_call_with_implicit_prelude_print_effect() {
        let source = "main : Unit -> Unit can Print\nmain = println \"hi\"\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("implicit prelude Print effect should resolve `println` operation");
    }

    #[test]
    fn accepts_can_clause_with_implicit_prelude_read_effect() {
        // `can Read` is accepted via implicit `goby/prelude` embed defaults.
        let source = "main : Unit -> Unit can Read\nmain = ()\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("implicit prelude Read effect should be accepted in `can` clause");
    }

    #[test]
    fn accepts_spaced_unit_argument_call_for_read_line() {
        let source = "\
main : Unit -> Unit can Read
main =
  line = read_line ()
  print line
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`read_line ()` should typecheck as Unit-arg call");
    }

    #[test]
    fn accepts_parenthesized_unit_argument_call_for_read_line() {
        let source = "\
main : Unit -> Unit can Read
main =
  line = read_line()
  print line
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`read_line()` should typecheck as Unit-arg call");
    }

    #[test]
    fn accepts_can_clause_with_explicit_context_prelude() {
        let sandbox = TempDirGuard::new("implicit_prelude_context");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = sandbox.path.join("user/main.gb");
        fs::create_dir_all(stdlib_root.join("goby")).expect("stdlib/goby should be creatable");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("user path should be creatable");
        fs::write(
            stdlib_root.join("goby/prelude.gb"),
            "effect Print\n  print : String -> Unit\n  println : String -> Unit\n@embed Print __goby_embeded_effect_stdout_handler\n",
        )
        .expect("prelude file should be writable");
        let source = "main : Unit -> Unit can Print\nmain = print \"hi\"\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect("Print should resolve via implicit prelude import");
    }

    #[test]
    fn rejects_print_can_clause_when_prelude_is_missing_in_context_root() {
        let sandbox = TempDirGuard::new("implicit_prelude_missing");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = sandbox.path.join("user/main.gb");
        fs::create_dir_all(&stdlib_root).expect("stdlib root should be creatable");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("user path should be creatable");
        let source = "main : Unit -> Unit can Print\nmain = print \"hi\"\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("missing prelude should reject Print in can-clause");
        assert!(err.message.contains("unknown effect"));
    }

    #[test]
    fn rejects_bare_print_call_when_prelude_is_missing_in_context_root() {
        let sandbox = TempDirGuard::new("implicit_prelude_print_missing");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = sandbox.path.join("user/main.gb");
        fs::create_dir_all(&stdlib_root).expect("stdlib root should be creatable");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("user path should be creatable");
        let source = "main : Unit -> Unit\nmain = print \"hi\"\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("missing prelude should reject bare print call");
        assert!(
            err.message
                .contains("unknown function or constructor `print`"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_non_main_can_clause_with_implicit_prelude_effect() {
        let source = "\
f : Unit -> Unit can Print
f = ()
main : Unit -> Unit
main = ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("non-main can-clause should not use implicit-prelude embedded effect");
        assert!(err.message.contains("unknown effect"));
    }

    #[test]
    fn rejects_unknown_effect_in_can_clause() {
        // `can UndeclaredEffect` — no matching `effect UndeclaredEffect` in the module.
        let source = "x : Int -> Int can UndeclaredEffect\nx n = n\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("unknown effect should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(
            err.message.contains("unknown effect"),
            "unexpected message: {}",
            err.message
        );
        assert!(err.message.contains("UndeclaredEffect"));
    }

    #[test]
    fn accepts_can_clause_with_declared_effect() {
        // `can Log` where `effect Log` is declared in the same module.
        let source = "\
effect Log
  log: String -> Unit
x : Int -> Int can Log
x n = n
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("declared effect in `can` clause should be accepted");
    }

    #[test]
    fn accepts_can_clause_with_multiple_declared_effects() {
        // `can Log, Env` where both are declared.
        let source = "\
effect Log
  log: String -> Unit
effect Env
  from_env: String -> String
f : Int -> Int can Log, Env
f n = n
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("multiple declared effects in `can` clause should be accepted");
    }

    #[test]
    fn rejects_second_of_two_effects_when_undeclared() {
        // First effect is declared but second is not.
        let source = "\
effect Log
  log: String -> Unit
f : Int -> Int can Log, Ghost
f n = n
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("undeclared second effect should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(err.message.contains("unknown effect"));
        assert!(err.message.contains("Ghost"));
    }

    #[test]
    fn allows_non_function_type_annotation() {
        let module =
            parse_module("pair : (String, Int)\npair = (\"a\", 1)\n").expect("should parse");
        typecheck_module(&module).expect("tuple type annotation should be accepted");
    }

    #[test]
    fn rejects_tuple_annotation_body_mismatch() {
        // pair : (String, Int) but body returns plain Int — type mismatch.
        let module = parse_module("pair : (String, Int)\npair = 42\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("type mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("pair"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_matching_tuple_annotation_body() {
        // pair : (String, Int) and body returns a (String, Int) tuple.
        let module =
            parse_module("pair : (String, Int)\npair = (\"hello\", 42)\n").expect("should parse");
        typecheck_module(&module).expect("matching tuple annotation should be accepted");
    }

    #[test]
    fn grouped_type_annotation_is_unwrapped() {
        // `n : (Int)` is a grouped type, equivalent to `n : Int`.
        // The body `42` is Int, so this should pass.
        let module = parse_module("n : (Int)\nn = 42\n").expect("should parse");
        typecheck_module(&module).expect("grouped type annotation should be accepted");
    }

    #[test]
    fn grouped_type_annotation_mismatch_is_rejected() {
        // `n : (Int)` but body is String — should be rejected.
        let module = parse_module("n : (Int)\nn = \"oops\"\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("grouped type mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("n"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_malformed_function_type_annotation() {
        let module = parse_module("f : Int -> -> Int\nf = 1\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("malformed function type should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(err.message.contains("invalid function type annotation"));
    }

    // -----------------------------------------------------------------------
    // Expression-level type inference tests
    // -----------------------------------------------------------------------

    #[test]
    fn accepts_list_int_annotation_matching_list_literal_body() {
        let module = parse_module("xs : List Int\nxs = [1, 2]\n").expect("should parse");
        typecheck_module(&module).expect("list literal body should typecheck");
    }

    #[test]
    fn accepts_case_list_pattern_bindings_in_arm_body() {
        let source = r#"
id : Int -> Int
id n = n

head_or_zero : List Int -> Int
head_or_zero xs =
  id
    case xs
      [] -> 0
      [x, ..xxs] -> x
"#;
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("list case bindings should typecheck");
    }

    #[test]
    fn accepts_case_list_pattern_with_wildcard_head() {
        let source = r#"
id : List Int -> List Int
id xs = xs

tail_or_empty : List Int -> List Int
tail_or_empty xs =
  id
    case xs
      [] -> []
      [_, ..tail] -> tail
"#;
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("wildcard head list pattern should typecheck");
    }

    #[test]
    fn accepts_case_fixed_length_and_literal_head_list_patterns() {
        let source = r#"
id : Int -> Int
id n = n

f : List Int -> Int
f xs =
  id
    case xs
      [1] -> 10
      [4, ..] -> 20
      [_, _] -> 30
      [a, ..b] -> a
      _ -> 0
"#;
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("list pattern variants should typecheck");
    }

    #[test]
    fn accepts_case_arm_block_body() {
        let source = r#"
id : Int -> Int
id n = n

f : Int -> Int
f x =
  id
    case x
      0 ->
        y = 1
        y + 10
      _ -> 0
"#;
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("case arm block body should typecheck");
    }

    #[test]
    fn rejects_case_arm_block_without_tail_expression() {
        let source = r#"
id : Int -> Int
id n = n

f : Int -> Int
f x =
  id
    case x
      0 ->
        y = 1
      _ -> 0
"#;
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("case arm block without tail expression should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message
                .contains("block expression must end with an expression"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_list_case_pattern_on_non_list_scrutinee() {
        let source = r#"
id : Int -> Int
id n = n

f : Int -> Int
f x =
  id
    case x
      [head, ..tail] -> head
      _ -> 0
"#;
        let module = parse_module(source).expect("should parse");
        let err =
            typecheck_module(&module).expect_err("list pattern on non-list scrutinee should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message
                .contains("list case pattern requires `List` scrutinee"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_if_branch_type_mismatch() {
        let source = r#"
f : Unit -> Int
f _ =
  if True
    1
  else
    "oops"
"#;
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("if branch type mismatch should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message.contains("if branch type mismatch"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_case_branch_type_mismatch() {
        let source = r#"
f : Int -> Int
f x =
  case x
    0 -> 1
    _ -> "oops"
"#;
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("case branch type mismatch should fail");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message.contains("case branch type mismatch"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_bool_literal_annotation_match() {
        let module = parse_module("flag : Bool\nflag = True\n").expect("should parse");
        typecheck_module(&module).expect("Bool literal should typecheck as Bool");
    }

    #[test]
    fn typechecks_function_example() {
        let source = read_example("function.gb");
        let module = parse_module(&source).expect("function.gb should parse");
        typecheck_module(&module).expect("function.gb should typecheck");
    }

    #[test]
    fn typechecks_named_function_reference_in_higher_order_position() {
        let source = r#"
import goby/list ( map )

plus_ten : Int -> Int
plus_ten x = x + 10

apply_all : List Int -> List Int
apply_all xs = map xs plus_ten
"#;
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("named function reference should typecheck in higher-order position");
    }

    #[test]
    fn typechecks_import_example() {
        let source = read_example("import.gb");
        let module = parse_module(&source).expect("import.gb should parse");
        typecheck_module(&module).expect("import.gb should typecheck");
    }

    #[test]
    fn baseline_plain_import_works_with_qualified_access() {
        let source = "\
import goby/string
f : Unit -> List String
f = string.split(\"a,b\", \",\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("plain import should expose qualified symbols");
    }

    #[test]
    fn baseline_selective_import_exposes_string_graphemes() {
        let source = "\
import goby/string ( graphemes )
f : Unit -> List String
f = graphemes(\"a👨‍👩‍👧‍👦b\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("selective import should expose stdlib string.graphemes");
    }

    #[test]
    fn baseline_alias_import_works_with_qualified_access() {
        let source = "\
import goby/list as l
f : Unit -> String
f = l.join([\"a\", \"b\"], \",\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("alias import should expose alias-qualified symbols");
    }

    #[test]
    fn baseline_selective_import_exposes_bare_symbol() {
        let source = "\
import goby/env ( fetch_env_var )
f : Unit -> String
f = fetch_env_var(\"HOME\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("selective import should expose bare symbol");
    }

    #[test]
    fn baseline_bare_print_is_available_via_implicit_prelude() {
        let source = "main : Unit -> Unit\nmain = print \"hi\"\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("bare print should resolve via implicit prelude");
    }

    #[test]
    fn resolver_first_prefers_file_based_stdlib_exports() {
        let sandbox = TempDirGuard::new("resolver_first");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/env.gb"),
            "fetch_env_var : String -> Int\nfetch_env_var name = 1\n",
        )
        .expect("stdlib file should be writable");
        let resolver = StdlibResolver::new(root);

        let exports = crate::typecheck_validate::module_exports_for_import_with_resolver(
            "goby/env", &resolver,
        )
        .expect("resolver export lookup should succeed");
        let ty = exports
            .get("fetch_env_var")
            .expect("fetch_env_var should be exported");
        assert_eq!(
            ty,
            &Ty::Fun {
                params: vec![Ty::Str],
                result: Box::new(Ty::Int),
            }
        );
    }

    #[test]
    fn main_can_clause_accepts_imported_embedded_default_effect() {
        let sandbox = TempDirGuard::new("embedded_effect_visible");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/stdio.gb"),
            "effect Console\n  log : String -> Unit\n@embed Console __goby_embeded_effect_stdout_handler\nlog : String -> Unit can Console\nlog msg = msg |> print\n",
        )
        .expect("stdlib file should be writable");
        let source_path = sandbox.path.join("main.gb");
        let source = "\
import goby/stdio ( log )
main : Unit -> Unit can Console
main = ()
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&root))
            .expect("main can-clause should accept imported embedded default effect");
    }

    #[test]
    fn rejects_conflicting_embedded_default_handlers_across_imports() {
        let sandbox = TempDirGuard::new("embedded_effect_conflict");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/a.gb"),
            "effect Console\n  log : String -> Unit\n@embed Console __goby_embeded_effect_stdout_handler\nlog : String -> Unit can Console\nlog msg = msg |> print\n",
        )
        .expect("stdlib file should be writable");
        fs::write(
            root.join("goby/b.gb"),
            "effect Console\n  log : String -> Unit\n@embed Console __goby_embeded_effect_other_handler\nlog : String -> Unit can Console\nlog msg = msg |> print\n",
        )
        .expect("stdlib file should be writable");
        let source_path = sandbox.path.join("main.gb");
        let source = "\
import goby/a ( log )
import goby/b
main : Unit -> Unit can Console
main = ()
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&root))
            .expect_err("conflicting embedded defaults across imports should be rejected");
        assert!(
            err.message
                .contains("conflicting embedded default handler for effect"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn non_main_unhandled_embedded_default_effect_is_rejected() {
        let sandbox = TempDirGuard::new("embedded_effect_local_visible");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/console.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "\
effect Console
  log : String -> Unit
@embed Console __goby_embeded_effect_stdout_handler
log_value : String -> Unit can Console
log_value msg = Console.log msg
f : Unit -> Unit
f = log_value \"x\"
main : Unit -> Unit
main = ()
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("non-main unhandled embedded default effect should be rejected");
        assert!(
            err.message.contains("not handled"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn resolver_reports_missing_stdlib_module_when_file_is_missing() {
        let sandbox = TempDirGuard::new("resolver_fallback");
        let resolver = StdlibResolver::new(sandbox.path.join("stdlib"));
        let err = crate::typecheck_validate::module_exports_for_import_with_resolver(
            "goby/env", &resolver,
        )
        .expect_err("missing stdlib file should fail");
        assert!(err.message.contains("unknown module `goby/env`"));
        assert!(err.message.contains("attempted stdlib path"));
        assert!(err.message.contains("goby/env.gb"));
    }

    #[test]
    fn resolver_parse_failure_is_reported_during_import_validation() {
        let sandbox = TempDirGuard::new("resolver_parse_failure");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(root.join("goby/env.gb"), "fetch_env_var : String ->\n")
            .expect("stdlib file should be writable");
        let resolver = StdlibResolver::new(root);

        let err = crate::typecheck_validate::module_exports_for_import_with_resolver(
            "goby/env", &resolver,
        )
        .expect_err("parse failure should return a typecheck error");
        assert!(
            err.message
                .contains("failed to resolve stdlib module `goby/env`"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn unknown_module_diagnostic_includes_attempted_stdlib_path() {
        let sandbox = TempDirGuard::new("unknown_module_path_diag");
        let resolver = StdlibResolver::new(sandbox.path.join("stdlib"));
        let err = crate::typecheck_validate::module_exports_for_import_with_resolver(
            "goby/unknown_mod",
            &resolver,
        )
        .expect_err("unknown module should fail");
        assert!(err.message.contains("unknown module `goby/unknown_mod`"));
        assert!(err.message.contains("attempted stdlib path"));
        assert!(err.message.contains("goby/unknown_mod.gb"));
    }

    #[test]
    fn resolver_first_preserves_function_shape_for_split() {
        let sandbox = TempDirGuard::new("resolver_split_shape");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/string.gb"),
            "split : String -> String -> List String\nsplit a b = []\n",
        )
        .expect("stdlib file should be writable");
        let resolver = StdlibResolver::new(root);

        let exports = crate::typecheck_validate::module_exports_for_import_with_resolver(
            "goby/string",
            &resolver,
        )
        .expect("resolver export lookup should succeed");
        let ty = exports.get("split").expect("split should be exported");
        assert_eq!(
            ty,
            &Ty::Fun {
                params: vec![Ty::Str, Ty::Str],
                result: Box::new(Ty::List(Box::new(Ty::Str))),
            }
        );
    }

    #[test]
    fn typechecks_file_based_stdlib_symbol_not_in_builtin_table() {
        let source = "\
import goby/string ( length )
f : Unit -> Int
f = length(\"abc\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("file-based stdlib symbol should typecheck");
    }

    #[test]
    fn typechecks_import_from_goby_stdio_module() {
        let source = "\
import goby/stdio
f : Unit -> Unit can Print
f = print \"hi\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("builtin print should remain callable with stdio import");
    }

    #[test]
    fn typechecks_import_from_goby_int_module() {
        let source = "\
import goby/int as i
effect StringParseError
  invalid_integer : String -> Int
f : Unit -> Int can StringParseError
f =
  with
    invalid_integer _ ->
      resume -1
  in
    i.parse(\"42\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("int.parse should typecheck when StringParseError is handled");
    }

    #[test]
    fn typechecks_list_each_with_plain_import() {
        let source = "\
import goby/list
main : Unit -> Unit can Print
main =
  list.each [1, 2] (|n| -> print \"${n}\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("list.each should typecheck via plain import");
    }

    #[test]
    fn typechecks_list_each_with_alias_import() {
        let source = "\
import goby/list as l
main : Unit -> Unit can Print
main =
  l.each [1, 2] (|n| -> print \"${n}\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("list.each should typecheck via alias import");
    }

    #[test]
    fn typechecks_list_each_with_selective_import() {
        let source = "\
import goby/list ( each )
main : Unit -> Unit can Print
main =
  each [1, 2] (|n| -> print \"${n}\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("list.each should typecheck via selective import");
    }

    #[test]
    fn typechecks_with_operation_from_imported_effect_without_redeclaration() {
        let source = "\
import goby/int as i
f : Unit -> Int can StringParseError
f =
  with
    invalid_integer _ ->
      resume -1
  in
    i.parse(\"42\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect(
            "handler op from imported effect should resolve without local effect redeclaration",
        );
    }

    #[test]
    fn typechecks_selective_import_of_type_and_effect_names() {
        let sandbox = TempDirGuard::new("selective_type_effect_import");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/custom.gb"),
            "type Token = Token(value: String)\neffect CustomEffect\n  fail : String -> Int\nto_int : String -> Int can CustomEffect\nto_int s = 0\n",
        )
        .expect("stdlib file should be writable");
        let source_path = sandbox.path.join("main.gb");
        let source = "\
import goby/custom ( Token, CustomEffect )
type Boxed = Boxed(value: Token)
f : Unit -> Int can CustomEffect
f =
  with
    fail _ ->
      resume 0
  in
    1
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&root))
            .expect("selective type/effect import should be accepted");
    }

    #[test]
    fn rejects_same_effect_name_imported_from_multiple_modules() {
        let sandbox = TempDirGuard::new("ambiguous_effect_name_across_imports");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/a.gb"),
            "effect ParseError\n  fail_a : String -> Int\n",
        )
        .expect("stdlib file should be writable");
        fs::write(
            root.join("goby/b.gb"),
            "effect ParseError\n  fail_b : String -> Int\n",
        )
        .expect("stdlib file should be writable");
        let source_path = sandbox.path.join("main.gb");
        let source = "\
import goby/a ( ParseError )
import goby/b ( ParseError )
main : Unit -> Unit
main = ()
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&root))
            .expect_err("same effect name from different imports should be rejected");
        assert!(
            err.message
                .contains("effect `ParseError` has conflicting declarations")
        );
    }

    #[test]
    fn rejects_same_effect_name_from_local_and_imported_declaration() {
        let sandbox = TempDirGuard::new("ambiguous_effect_name_local_and_import");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/a.gb"),
            "effect ParseError\n  fail_a : String -> Int\n",
        )
        .expect("stdlib file should be writable");
        let source_path = sandbox.path.join("main.gb");
        let source = "\
import goby/a ( ParseError )
effect ParseError
  fail_local : String -> Int
main : Unit -> Unit
main = ()
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&root))
            .expect_err("same effect name from local/import should be rejected");
        assert!(
            err.message
                .contains("effect `ParseError` has conflicting declarations")
        );
    }

    #[test]
    fn rejects_selective_import_of_stdio_print_symbol() {
        let source = "\
import goby/stdio ( print )
f : Unit -> Unit can Print
f = print \"hi\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("stdio module should not export print symbol for selective import");
        assert!(
            err.message
                .contains("unknown symbol `print` in import from `goby/stdio`"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_embed_declaration_inside_stdlib_root_with_context() {
        let sandbox = TempDirGuard::new("embed_in_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/stdio.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "effect Print\n  print : String -> Unit\n@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect("@embed under stdlib root should be accepted");
    }

    #[test]
    fn rejects_embed_declaration_outside_stdlib_root_with_context() {
        let sandbox = TempDirGuard::new("embed_outside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = sandbox.path.join("user/main.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("user path should be creatable");
        let source = "effect Print\n  print : String -> Unit\n@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("@embed outside stdlib root should be rejected");
        assert!(
            err.message
                .contains("@embed declarations are only allowed under stdlib root"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_reserved_intrinsic_call_outside_stdlib_root() {
        let sandbox = TempDirGuard::new("intrinsic_call_outside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = sandbox.path.join("user/main.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("user path should be creatable");
        let source = "f : String -> Int\nf s = __goby_string_length s\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("reserved intrinsic calls should be rejected outside stdlib");
        assert!(
            err.message.contains("reserved intrinsic call"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_reserved_intrinsic_declaration_name_outside_stdlib_root() {
        let sandbox = TempDirGuard::new("intrinsic_decl_outside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = sandbox.path.join("user/main.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("user path should be creatable");
        let source = "__goby_string_length : String -> Int\n__goby_string_length s = 0\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("reserved intrinsic declaration names should be rejected outside stdlib");
        assert!(
            err.message.contains("reserved intrinsic name"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_reserved_intrinsic_calls_inside_stdlib_root() {
        let sandbox = TempDirGuard::new("intrinsic_call_inside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/string.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "length : String -> Int\nlength s = __goby_string_length s\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect("reserved intrinsic calls should be accepted under stdlib root");
    }

    #[test]
    fn accepts_each_grapheme_intrinsic_inside_stdlib_root() {
        let sandbox = TempDirGuard::new("intrinsic_each_grapheme_inside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/string.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "\
effect Iterator a b
  yield : a -> b -> (Bool, b)
@embed Iterator __goby_embeded_effect_stdout_handler
count_graphemes : String -> Int can Iterator
count_graphemes s =
  with
    yield _ _ ->
      resume (True, ())
  in
    __goby_string_each_grapheme s
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect("`__goby_string_each_grapheme` should be accepted under stdlib root");
    }

    #[test]
    fn accepts_each_grapheme_intrinsic_unified_iterator_mode_inside_stdlib_root() {
        let sandbox = TempDirGuard::new("intrinsic_each_grapheme_unified_inside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/string.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "\
type GraphemeState = GraphemeState(grapheme: String, current: String)
effect Iterator a b
  yield : a -> b -> (Bool, b)
@embed Iterator __goby_embeded_effect_stdout_handler
f : String -> GraphemeState can Iterator
f s =
  state = GraphemeState(grapheme: \"\", current: \"\")
  out = state
  with
    yield grapheme step ->
      resume (True, step)
  in
    out = __goby_string_each_grapheme s state
  out
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect("unified iterator mode should be accepted under stdlib root");
    }

    #[test]
    fn accepts_string_eq_operator_and_list_push_intrinsic_inside_stdlib_root() {
        let sandbox = TempDirGuard::new("string_eq_operator_and_push_inside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/string.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "\
f : Unit -> List String
f =
  items = __goby_list_push_string [] \"a\"
  ok = \"a\" == \"a\"
  if ok
    items
  else
    __goby_list_push_string [] \"b\"
";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root)).expect(
            "`String == String` and `__goby_list_push_string` should be accepted under stdlib root",
        );
    }

    #[test]
    fn rejects_unknown_intrinsic_calls_inside_stdlib_root() {
        let sandbox = TempDirGuard::new("unknown_intrinsic_inside_stdlib");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/string.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "length : String -> Int\nlength s = __goby_string_len s\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("unknown intrinsic calls should be rejected under stdlib root");
        assert!(
            err.message.contains("unknown runtime intrinsic"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn allows_embed_declaration_without_source_context_for_legacy_api_compat() {
        let source = "effect Print\n  print : String -> Unit\n  println : String -> Unit\n@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("legacy typecheck API should remain compatible without source context");
    }

    #[test]
    fn rejects_embed_missing_effect_without_source_context() {
        let source = "@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("missing in-module effect should fail even without source context");
        assert!(
            err.message.contains("must be declared in the same module"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_embed_when_effect_is_not_declared_in_same_module() {
        let sandbox = TempDirGuard::new("embed_missing_effect");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/stdio.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("embedded effect should require in-module effect declaration");
        assert!(
            err.message.contains("must be declared in the same module"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_duplicate_embed_declaration_names_in_stdlib() {
        let sandbox = TempDirGuard::new("embed_duplicate");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/stdio.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "effect Print\n  print : String -> Unit\n  println : String -> Unit\n@embed Print __goby_embeded_effect_stdout_handler\n@embed Print __goby_embeded_effect_stdout_handler\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("duplicate embedded effects should be rejected");
        assert!(err.message.contains("duplicate embedded effect"));
    }

    #[test]
    fn rejects_embed_with_invalid_handler_namespace() {
        let sandbox = TempDirGuard::new("embed_invalid_handler_namespace");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/stdio.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "effect Print\n  print : String -> Unit\n  println : String -> Unit\n@embed Print stdout_handler\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("invalid embed handler namespace should be rejected");
        assert!(
            err.message
                .contains("must start with `__goby_embeded_effect_`")
        );
    }

    #[test]
    fn rejects_embed_with_unknown_handler_intrinsic() {
        let sandbox = TempDirGuard::new("embed_unknown_handler_intrinsic");
        let stdlib_root = sandbox.path.join("stdlib");
        let source_path = stdlib_root.join("goby/stdio.gb");
        fs::create_dir_all(source_path.parent().expect("parent should exist"))
            .expect("stdlib path should be creatable");
        let source = "effect Print\n  print : String -> Unit\n@embed Print __goby_embeded_effect_missing\nf : Unit -> Int\nf = 1\n";
        fs::write(&source_path, source).expect("fixture file should be writable");
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module_with_context(&module, Some(&source_path), Some(&stdlib_root))
            .expect_err("unknown embedded handler intrinsic should be rejected");
        assert!(err.message.contains("unknown embedded handler intrinsic"));
    }

    #[test]
    fn rejects_unknown_import_module() {
        let module = parse_module("import goby/unknown\nmain : Unit -> Unit\nmain = 1\n")
            .expect("should parse");
        let err = typecheck_module(&module).expect_err("unknown module should fail");
        assert!(err.message.contains("unknown module"));
    }

    #[test]
    fn rejects_unknown_symbol_in_selective_import() {
        let source = "import goby/env ( missing )\nmain : Unit -> Unit\nmain = 1\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("unknown imported symbol should fail");
        assert!(err.message.contains("unknown symbol"));
    }

    #[test]
    fn rejects_used_name_when_import_collides_with_declaration() {
        let source = "\
import goby/env ( fetch_env_var )
fetch_env_var : String -> String
fetch_env_var name = name
main : Unit -> Unit
main =
  fetch_env_var \"GOBY_PATH\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("used ambiguous name should fail");
        assert!(err.message.contains("ambiguous"));
        assert!(err.message.contains("fetch_env_var"));
    }

    #[test]
    fn rejects_unused_name_when_import_collides_with_declaration() {
        let source = "\
import goby/env ( fetch_env_var )
fetch_env_var : String -> String
fetch_env_var name = name
main : Unit -> Unit can Print
main =
  print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("import collisions should fail at name resolution time");
        assert!(err.message.contains("ambiguous"));
        assert!(err.message.contains("fetch_env_var"));
    }

    #[test]
    fn rejects_constant_annotation_type_mismatch() {
        // `x : Int; x = "hello"` — non-function annotation; body is String not Int.
        let module = parse_module("x : Int\nx = \"hello\"\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("type mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("x"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_print_as_last_expr_in_int_returning_function() {
        // `f : Int -> Int` but body ends with `print`, which returns Unit.
        let source = "f : Int -> Int\nf x =\n  x + 1\n  print \"side\"\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("Unit body in Int->Int should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_print_as_last_expr_in_unit_returning_function() {
        // `main : Unit -> Unit` body ending with `print` should be accepted.
        let source = "main : Unit -> Unit\nmain =\n  print \"hi\"\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("Unit-returning function with print body should pass");
    }

    #[test]
    fn accepts_unit_literal_value_for_unit_returning_function() {
        let source = "main : Unit -> Unit\nmain = ()\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("`()` should be accepted as Unit value");
    }

    #[test]
    fn rejects_legacy_unit_value_expression() {
        let source = "main : Unit -> Unit\nmain = Unit\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("legacy Unit value expression should fail");
        assert!(err.message.contains("legacy_unit_value_syntax"));
        assert!(err.message.contains("use `()`"));
    }

    #[test]
    fn accepts_constant_annotation_matching_body() {
        // `n : Int; n = 42` — non-function annotation matching body type.
        let module = parse_module("n : Int\nn = 42\n").expect("should parse");
        typecheck_module(&module).expect("matching constant annotation should be accepted");
    }

    #[test]
    fn rejects_body_type_mismatch_int_vs_string() {
        // `f : Int -> Int; f x = "oops"` — body returns String but declared Int.
        let module = parse_module("f : Int -> Int\nf x = \"oops\"\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("type mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("f"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_body_type_matching_declared_return() {
        // `double : Int -> Int; double x = x + x` — body type is Int, declared Int.
        let module = parse_module("double : Int -> Int\ndouble x = x + x\n").expect("should parse");
        typecheck_module(&module).expect("matching body type should be accepted");
    }

    #[test]
    fn rejects_function_body_type_mismatch_via_param() {
        // `greet : String -> Int; greet name = name` — param is String, declared return is Int.
        // After A1 fix, `name` resolves to String, which conflicts with declared return Int.
        let module =
            parse_module("greet : String -> Int\ngreet name = name\n").expect("should parse");
        let err =
            typecheck_module(&module).expect_err("type mismatch via param should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("greet"));
        assert!(
            err.message.contains("does not match"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_function_body_with_param_matching_return_type() {
        // `id : Int -> Int; id x = x` — param x is Int, return is Int — should pass.
        let module = parse_module("id : Int -> Int\nid x = x\n").expect("should parse");
        typecheck_module(&module).expect("identity function should typecheck");
    }

    #[test]
    fn rejects_param_count_mismatch_fewer_params() {
        // Annotation has 2 params but definition only has 1 — should be rejected.
        let module = parse_module("add : Int -> Int -> Int\nadd a = a\n").expect("should parse");
        let err = typecheck_module(&module).expect_err("param count mismatch should be rejected");
        assert_eq!(err.declaration.as_deref(), Some("add"));
        assert!(
            err.message.contains("parameter"),
            "unexpected message: {}",
            err.message
        );
    }

    #[test]
    fn accepts_unit_param_omitted_in_definition() {
        // `main : Unit -> Unit; main = ...` — Unit param may be omitted in MVP.
        let module =
            parse_module("main : Unit -> Unit\nmain = print \"hi\"\n").expect("should parse");
        typecheck_module(&module).expect("Unit param omission should be accepted");
    }

    #[test]
    fn rejects_rebinding_in_same_scope() {
        let source = "f : Unit -> Int\nf =\n  a = 1\n  a = a + 1\n  a\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("re-binding in same scope should fail");
        assert!(err.message.contains("duplicate declaration `a`"));
        assert!(err.message.contains("use `:=` for mutation"));
    }

    #[test]
    fn accepts_mut_declaration_and_assignment() {
        let source = "f : Unit -> Int\nf =\n  mut a = 1\n  a := 2\n  a\n";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("mut declaration and assignment should typecheck");
    }

    #[test]
    fn rejects_assignment_to_immutable_variable() {
        let source = "f : Unit -> Int\nf =\n  a = 1\n  a := 2\n  a\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("assignment to immutable should fail");
        assert!(
            err.message
                .contains("cannot assign to immutable variable `a`")
        );
    }

    #[test]
    fn rejects_assignment_to_undeclared_variable() {
        let source = "f : Unit -> Int\nf =\n  x := 1\n  0\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("assignment to undeclared should fail");
        assert!(
            err.message
                .contains("cannot assign to undeclared variable `x`")
        );
    }

    // --- TypecheckError span regression tests ---

    #[test]
    fn typecheck_error_duplicate_declaration_has_span_with_line() {
        // Two declarations named "foo" — duplicate error; span must point to the second
        // declaration's line (line 3 = annotation line of the second foo).
        let source = "foo : Int\nfoo = 1\nfoo : Int\nfoo = 2\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("duplicate decl should fail");
        let span = err.span.expect("duplicate decl error must have a span");
        // Second annotation is on line 3; decl_line = 3.
        assert_eq!(span.line, 3, "span.line should point to second declaration");
        assert_eq!(span.col, 1);
    }

    #[test]
    fn typecheck_error_main_wrong_type_has_span_with_line() {
        // main declared with a non-function type annotation; triggers "must be a function type"
        // error (parse_function_type returns None). span must include a line number.
        let source = "main : Int\nmain = 1\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("wrong main type should fail");
        let span = err.span.expect("main type error must have a span");
        assert_eq!(
            span.line, 1,
            "span.line should point to main declaration line"
        );
        assert_eq!(span.col, 1);
    }

    #[test]
    fn typecheck_error_main_wrong_function_type_has_span() {
        // main declared with a valid function type but wrong signature (not Unit -> Unit).
        // Triggers the second error branch: "main type must be `Unit -> Unit` in MVP".
        let source = "main : Int -> String\nmain x = \"ab\"\n";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("wrong main function type should fail");
        let span = err.span.expect("main type error must have a span");
        assert_eq!(
            span.line, 1,
            "span.line should point to main declaration line"
        );
        assert_eq!(span.col, 1);
        assert!(err.message.contains("Unit -> Unit"));
    }

    // --- effect op argument type checking (§4.1.1) ---

    #[test]
    fn rejects_effect_op_call_with_wrong_arg_type() {
        // `catch "NoCoffeeError"` when `catch : Error -> Unit` — String is not Error.
        // This should be a typecheck error, not a silent runtime failure.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    catch \"NoCoffeeError\"
";
        let module = parse_module(source).expect("should parse");
        let result = typecheck_module(&module);
        assert!(
            result.is_err(),
            "passing String to an effect op expecting Error should be a typecheck error"
        );
        let err = result.unwrap_err();
        assert!(
            err.message.contains("catch"),
            "error message should mention the op name; got: {}",
            err.message
        );
        assert!(
            err.message.contains("Error"),
            "error message should mention expected type; got: {}",
            err.message
        );
        assert!(
            err.message.contains("String") || err.message.contains("Str"),
            "error message should mention actual type; got: {}",
            err.message
        );
    }

    #[test]
    fn rejects_qualified_effect_op_call_with_wrong_arg_type() {
        // `ErrorEffect.catch "NoCoffeeError"` when `catch : Error -> Unit`.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    ErrorEffect.catch \"NoCoffeeError\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("qualified effect op call type mismatch should fail");
        assert!(err.message.contains("ErrorEffect.catch") || err.message.contains("catch"));
        assert!(err.message.contains("Error"));
        assert!(err.message.contains("String") || err.message.contains("Str"));
    }

    #[test]
    fn rejects_method_style_effect_op_call_with_wrong_arg_type() {
        // `ErrorEffect.catch("NoCoffeeError")` when `catch : Error -> Unit`.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    ErrorEffect.catch(\"NoCoffeeError\")
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("method-style effect op call type mismatch should fail");
        assert!(err.message.contains("ErrorEffect.catch") || err.message.contains("catch"));
        assert!(err.message.contains("Error"));
        assert!(err.message.contains("String") || err.message.contains("Str"));
    }

    #[test]
    fn rejects_multi_arg_effect_op_call_when_later_arg_type_mismatches() {
        let source = "
effect E
  op: String -> Int -> Unit

main : Unit -> Unit
main =
  with
    op s n ->
      resume ()
  in
    E.op \"ok\" \"bad\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("second argument type mismatch should fail");
        assert!(err.message.contains("E.op") || err.message.contains("op"));
        assert!(err.message.contains("Int"));
        assert!(err.message.contains("String") || err.message.contains("Str"));
    }

    #[test]
    fn rejects_multi_arg_method_style_effect_op_call_when_later_arg_type_mismatches() {
        let source = "
effect E
  op: String -> Int -> Unit

main : Unit -> Unit
main =
  with
    op s n ->
      resume ()
  in
    E.op(\"ok\", \"bad\")
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("method-style second argument type mismatch should fail");
        assert!(err.message.contains("E.op") || err.message.contains("op"));
        assert!(err.message.contains("Int"));
        assert!(err.message.contains("String") || err.message.contains("Str"));
    }

    #[test]
    fn rejects_effect_op_call_with_too_many_arguments() {
        let source = "
effect E
  op: String -> Unit

main : Unit -> Unit
main =
  with
    op s ->
      resume ()
  in
    E.op \"ok\" 1
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("too many args should fail");
        assert!(err.message.contains("expects 1 argument(s)"));
    }

    #[test]
    fn rejects_effect_op_pipeline_with_wrong_arg_type() {
        // `"NoCoffeeError" |> catch` when `catch : Error -> Unit` — same mismatch via pipeline.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    \"NoCoffeeError\" |> catch
";
        let module = parse_module(source).expect("should parse");
        let result = typecheck_module(&module);
        assert!(
            result.is_err(),
            "piping String to an effect op expecting Error should be a typecheck error"
        );
        let err = result.unwrap_err();
        assert!(
            err.message.contains("catch"),
            "error message should mention the op name; got: {}",
            err.message
        );
        assert!(
            err.message.contains("Error"),
            "error message should mention expected type; got: {}",
            err.message
        );
        assert!(
            err.message.contains("String") || err.message.contains("Str"),
            "error message should mention actual type; got: {}",
            err.message
        );
    }

    #[test]
    fn accepts_effect_op_pipeline_with_correct_arg_type() {
        // `Error(message: "oops") |> catch` when `catch : Error -> Unit` — should pass.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    Error(message: \"oops\") |> catch
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("correct arg type via pipeline should be accepted");
    }

    #[test]
    fn accepts_effect_op_call_with_correct_arg_type() {
        // catch receives Error(message:...) which matches the declared Error param — should pass.
        let source = "
type Error = Error(message: String)

effect ErrorEffect
  catch: Error -> Unit

main : Unit -> Unit
main =
  with
    catch e ->
      resume ()
  in
    catch Error(message: \"oops\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("correct arg type should be accepted");
    }

    #[test]
    fn accepts_effect_op_call_with_string_when_op_expects_string() {
        // log receives String, which is the declared param type — no error.
        // Also covers the Unknown-guard path via the correct type.
        let source = "
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log str ->
      resume ()
  in
    log \"hello\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("String arg to String op should be accepted");
    }

    #[test]
    fn accepts_generic_effect_op_call_with_inferred_type_variable() {
        let source = "
effect Iter a
  op: a -> Unit

main : Unit -> Unit
main =
  with
    op x ->
      resume ()
  in
    op 1
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("generic effect op call should infer `a = Int`");
    }

    #[test]
    fn accepts_multiple_calls_to_same_generic_effect_op_with_different_types() {
        let source = "
effect Iter a
  op: a -> Unit

main : Unit -> Unit
main =
  with
    op x ->
      resume ()
  in
    op 1
    op \"two\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("separate calls to same generic effect op should instantiate independently");
    }

    #[test]
    fn rejects_generic_effect_op_call_when_type_variable_constraints_conflict() {
        let source = "
effect Iter a
  pair: a -> a -> Unit

main : Unit -> Unit
main =
  with
    pair x y ->
      resume ()
  in
    pair 1 \"oops\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("conflicting generic args should fail");
        assert!(err.message.contains("pair"));
        assert!(err.message.contains("Int"));
        assert!(err.message.contains("String") || err.message.contains("Str"));
    }

    #[test]
    fn rejects_effect_op_call_when_generic_constraints_are_unresolved() {
        let source = "
effect Iter a
  op: a -> Unit

main : Unit -> Unit
main =
  with
    op _ ->
      resume ()
  in
    op missing
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("unresolved generic constraint in effect-op call should fail");
        assert!(
            err.message.contains("unresolved type"),
            "unexpected error: {}",
            err.message
        );
        assert!(
            err.message.contains("op"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn rejects_effect_op_call_with_type_hole_conflict_note() {
        let source = "
effect Iter
  op: List _ -> Unit

main : Unit -> Unit
main =
  with
    op _ ->
      resume ()
  in
    op \"oops\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("type-hole conflict should fail");
        assert!(
            err.message
                .contains("anonymous type-hole `_` constraints conflict"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn accepts_effect_op_call_with_independent_anonymous_type_holes() {
        let source = "
effect Iter
  op: _ -> _ -> Unit

main : Unit -> Unit
main =
  with
    op x y ->
      resume ()
  in
    op 1 \"s\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("anonymous type holes should be independent per occurrence");
    }

    #[test]
    fn accepts_positional_single_field_constructor_in_effect_op_call() {
        // `raise Error("msg")` — positional sugar for `raise Error(message: "msg")`.
        // check should accept this because Error has exactly one field.
        let source = "
type Error = Error(message: String)

effect RaiseError
  raise: Error -> Unit

main : Unit -> Unit
main =
  with
    raise e ->
      resume ()
  in
    raise Error(\"oops\")
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("positional single-field constructor should be accepted");
    }

    #[test]
    fn no_sugar_for_multi_field_constructor() {
        // `raise Pair("a")` when Pair has two fields should NOT be treated as RecordConstruct.
        // It falls through to Expr::Call, type is Unknown → arg type check skipped → Ok.
        // (No false positive: we do not fabricate an error for multi-field positional.)
        let source = "
type Pair = Pair(first: String, second: String)

effect E
  op: Pair -> Unit

main : Unit -> Unit
main =
  with
    op p ->
      resume ()
  in
    op Pair(\"a\")
";
        let module = parse_module(source).expect("should parse");
        // Multi-field positional is not sugar — type is Unknown, no error expected.
        typecheck_module(&module).expect("multi-field positional should not raise false error");
    }

    #[test]
    fn rejects_resume_outside_handler() {
        let source = "
main : Unit -> Unit
main =
  resume 1
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("resume outside handler should fail");
        assert!(
            err.message.contains("resume_outside_handler"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn rejects_resume_arg_type_mismatch_in_handler_method() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume \"oops\"
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("resume arg mismatch should fail");
        assert!(
            err.message.contains("resume_arg_type_mismatch"),
            "unexpected error: {}",
            err.message
        );
        assert!(
            err.message.contains("Int"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn rejects_resume_in_unknown_operation_context() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    unknown x ->
      resume 1
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module).expect_err("unknown op context should fail");
        assert!(
            err.message.contains("resume_in_unknown_operation_context")
                || err.message.contains("unknown effect operation"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn accepts_resume_when_arg_matches_operation_return_type() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("resume with matching return type should pass");
    }

    #[test]
    fn accepts_resume_with_generic_operation_result_type() {
        let source = "
effect Iterator a b
  yield: a -> b -> (Bool, b)

main : Unit -> Unit
main =
  with
    yield x state ->
      resume (True, state)
  in
    yield \"a\" 0
    ()
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module).expect("resume should accept generic `(Bool, b)` result");
    }

    #[test]
    fn rejects_resume_when_generic_constraints_are_unresolved() {
        let source = "
effect Iterator a b
  yield: a -> b -> (Bool, b)

main : Unit -> Unit
main =
  with
    yield _ _ ->
      resume missing
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("resume unresolved generic constraints should fail");
        assert!(
            err.message
                .contains("resume_unresolved_generic_constraints"),
            "unexpected error: {}",
            err.message
        );
    }

    #[test]
    fn rejects_resume_when_generic_operation_result_shape_mismatches() {
        let source = "
effect Iterator a b
  yield: a -> b -> (Bool, b)

main : Unit -> Unit
main =
  with
    yield x state ->
      resume (1, state)
  in
    yield \"a\" 0
    ()
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("resume should reject incompatible generic result shape");
        assert!(err.message.contains("resume_arg_type_mismatch"));
        assert!(err.message.contains("Bool"));
    }

    #[test]
    fn accepts_multiple_resume_expressions_in_same_handler_method() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1
      resume 2
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("multiple resume expressions should no longer be conservatively rejected");
    }

    #[test]
    fn accepts_multiple_resume_expressions_in_single_expression_for_runtime_validation() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1 + resume 2
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        typecheck_module(&module)
            .expect("multi-resume expression should defer invalid progression handling to runtime");
    }

    #[test]
    fn rejects_multiple_resume_expressions_when_one_branch_has_type_mismatch() {
        let source = "
effect Iter
  next: Unit -> Int

main : Unit -> Unit
main =
  with
    next x ->
      if True
        resume 1
      else
        resume \"oops\"
  in
    print \"ok\"
";
        let module = parse_module(source).expect("should parse");
        let err = typecheck_module(&module)
            .expect_err("resume type mismatch should still be rejected under multiple branches");
        assert!(err.message.contains("resume_arg_type_mismatch"));
    }

    #[test]
    fn infers_resume_binding_type_from_non_generic_resume_context() {
        let env = TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
        };
        let value = Expr::Resume {
            value: Box::new(Expr::IntLit(1)),
        };
        let ctx = ResumeContext {
            expected_arg_ty: Some(Ty::Int),
        };
        let inferred = infer_binding_ty_with_resume_context(&value, &env, Some(&ctx));
        assert_eq!(inferred, Ty::Int);
    }

    #[test]
    fn keeps_resume_binding_unknown_when_resume_context_is_generic() {
        let env = TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
        };
        let value = Expr::Resume {
            value: Box::new(Expr::Var("x".to_string())),
        };
        let ctx = ResumeContext {
            expected_arg_ty: Some(Ty::Var("T".to_string())),
        };
        let inferred = infer_binding_ty_with_resume_context(&value, &env, Some(&ctx));
        assert_eq!(inferred, Ty::Unknown);
    }
}
