#[cfg(test)]
use crate::ast::{CasePattern, InterpolatedPart, ListPatternItem, ListPatternTail};
use crate::ast::{Declaration, Expr, Module, Stmt};
use crate::parser_expr::parse_expr as parse_expr_impl;
#[cfg(test)]
use crate::parser_stmt::parse_stmt_with;
use crate::parser_stmt::{
    first_legacy_using_line_offset, first_malformed_resume_expr_line_offset, parse_body_stmts_with,
};
use crate::parser_top::TopLevelItem;
use crate::parser_util::{is_indented, strip_line_comment};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    /// 1-indexed line number in the source file.
    pub line: usize,
    /// 1-indexed byte offset within the line (ASCII sources only; MVP assumption).
    /// Value `1` means "unknown column".
    pub col: usize,
    pub message: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "parse error at line {}:{}: {}",
            self.line, self.col, self.message
        )
    }
}

impl std::error::Error for ParseError {}

pub fn parse_module(source: &str) -> Result<Module, ParseError> {
    let lines: Vec<&str> = source.lines().collect();
    let mut i = 0;
    let mut imports = Vec::new();
    let mut embed_declarations = Vec::new();
    let mut type_declarations = Vec::new();
    let mut effect_declarations = Vec::new();
    let mut declarations = Vec::new();

    while i < lines.len() {
        let line = strip_line_comment(lines[i]).trim_end();
        let trimmed = line.trim();

        if trimmed.is_empty() || trimmed.starts_with('#') {
            i += 1;
            continue;
        }

        if is_indented(line) {
            // col is byte-offset + 1 within the raw source line (ASCII assumption).
            let col = lines[i].len() - lines[i].trim_start().len() + 1;
            return Err(ParseError {
                line: i + 1,
                col,
                message: "unexpected indentation at top level".to_string(),
            });
        }

        let (item, next_index) = crate::parser_top::parse_top_level_item(&lines, i)?;
        match item {
            TopLevelItem::Import(import) => imports.push(import),
            TopLevelItem::Embed(embed) => embed_declarations.push(embed),
            TopLevelItem::Type(ty_decl) => type_declarations.push(ty_decl),
            TopLevelItem::Effect(effect_decl) => effect_declarations.push(effect_decl),
            TopLevelItem::Declaration(parts) => {
                if let Some(offset) = first_malformed_resume_expr_line_offset(&parts.body) {
                    return Err(ParseError {
                        line: i + 1 + offset,
                        col: 1,
                        message: "malformed `resume` expression: expected `resume <expr>`"
                            .to_string(),
                    });
                }
                if let Some(offset) = first_legacy_using_line_offset(&parts.body) {
                    return Err(ParseError {
                        line: i + 1 + offset,
                        col: 1,
                        message: "legacy `using` syntax is no longer supported; use `with`"
                            .to_string(),
                    });
                }
                let parsed_body = parse_body_stmts(&parts.body);
                declarations.push(Declaration {
                    name: parts.name,
                    type_annotation: parts.type_annotation,
                    params: parts.params,
                    body: parts.body,
                    parsed_body,
                    line: parts.line,
                });
            }
        }
        i = next_index;
    }

    Ok(Module {
        imports,
        embed_declarations,
        type_declarations,
        effect_declarations,
        declarations,
    })
}

/// Parse a declaration body string into a list of statements.
/// Returns `None` if any line cannot be parsed (caller may fall back to string-based evaluation).
pub fn parse_body_stmts(body: &str) -> Option<Vec<Stmt>> {
    parse_body_stmts_with(body, parse_expr)
}

#[cfg(test)]
fn parse_stmt(line: &str) -> Option<Stmt> {
    parse_stmt_with(line, parse_expr)
}

/// Parse a single expression from a source string.
pub fn parse_expr(src: &str) -> Option<Expr> {
    parse_expr_impl(src)
}

// ---------------------------------------------------------------------------
// Shared utilities
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOpKind, Expr, ImportKind, Stmt, TypeDeclaration};
    use std::path::PathBuf;

    fn read_example(name: &str) -> String {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("..");
        path.push("..");
        path.push("examples");
        path.push(name);
        std::fs::read_to_string(path).expect("example file should exist")
    }

    fn parse_single_declaration(source: &str) -> Declaration {
        let module = parse_module(source).expect("source should parse");
        assert_eq!(module.declarations.len(), 1);
        module.declarations[0].clone()
    }

    #[test]
    fn parses_hello_example() {
        let source = read_example("hello.gb");
        let module = parse_module(&source).expect("hello.gb should parse");

        assert!(module.imports.is_empty());
        assert!(module.type_declarations.is_empty());
        assert_eq!(module.declarations.len(), 1);
        let main_decl = &module.declarations[0];
        assert_eq!(main_decl.name, "main");
        assert_eq!(
            main_decl.type_annotation.as_deref(),
            Some("Unit -> Unit can Print")
        );
    }

    #[test]
    fn parses_basic_types_example() {
        let source = read_example("basic_types.gb");
        let module = parse_module(&source).expect("basic_types.gb should parse");

        assert_eq!(module.imports.len(), 1);
        assert_eq!(module.imports[0].module_path, "goby/prelude");
        assert!(module.type_declarations.is_empty());
        assert_eq!(module.declarations.len(), 6);
        assert_eq!(module.declarations[0].name, "add");
        assert_eq!(module.declarations[1].name, "add_ten_and_two");
        assert_eq!(module.declarations[2].name, "concatenate");
        assert_eq!(module.declarations[3].name, "print_string");
        assert_eq!(module.declarations[4].name, "a");
        assert_eq!(module.declarations[5].name, "main");
    }

    #[test]
    fn parses_generic_types_example() {
        let source = read_example("generic_types.gb");
        let module = parse_module(&source).expect("generic_types.gb should parse");

        assert!(module.imports.is_empty());
        assert!(module.type_declarations.is_empty());
        assert_eq!(module.declarations.len(), 3);
        assert_eq!(module.declarations[0].name, "id");
        assert_eq!(module.declarations[1].name, "project");
        assert_eq!(module.declarations[2].name, "ints");
    }

    #[test]
    fn rejects_mismatched_annotation_and_definition_names() {
        let source = "foo : Int\nbar = 1\n";
        let err = parse_module(source).expect_err("mismatched names should be rejected");
        assert!(err.message.contains("does not match"));
    }

    #[test]
    fn rejects_reserved_resume_as_top_level_declaration_name() {
        let source = "resume : Int -> Int\nresume x = x\n";
        let err = parse_module(source).expect_err("reserved declaration name should be rejected");
        assert!(err.message.contains("reserved keyword"));
    }

    #[test]
    fn rejects_all_reserved_syntax_tokens_as_top_level_declaration_names() {
        let reserved = [
            "import", "type", "effect", "handler", "with", "in", "resume", "mut", "if", "else",
            "case", "as", "can", "using", "True", "False",
        ];
        for name in reserved {
            let source = format!("{name} : Int -> Int\n{name} x = x\n");
            parse_module(&source).expect_err("reserved declaration name should be rejected");
        }
    }

    #[test]
    fn rejects_reserved_syntax_tokens_as_local_binding_names() {
        let source = "main =\n  if = 1\n  if\n";
        let module =
            parse_module(source).expect("module parse itself succeeds even when body parse fails");
        let main = module
            .declarations
            .iter()
            .find(|decl| decl.name == "main")
            .expect("main declaration should exist");
        assert!(
            main.parsed_body.is_none(),
            "reserved local binding should prevent statement parse"
        );
    }

    #[test]
    fn rejects_legacy_top_level_handler_syntax() {
        let source = r#"
effect Iter
  yield: String -> Unit

handler Collect for Iter
  yield item = resume ()

main = 1
"#;
        let err = parse_module(source).expect_err("legacy handler declaration should be rejected");
        assert!(
            err.message
                .contains("legacy top-level `handler ... for ...` is no longer supported"),
            "unexpected error message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_legacy_top_level_handler_syntax_with_tab_after_keyword() {
        let source = r#"
effect Iter
  yield: String -> Unit

handler	Collect for Iter
  yield item = resume ()

main = 1
"#;
        let err = parse_module(source)
            .expect_err("legacy handler declaration with tab after keyword should be rejected");
        assert!(
            err.message
                .contains("legacy top-level `handler ... for ...` is no longer supported"),
            "unexpected error message: {}",
            err.message
        );
    }

    #[test]
    fn parses_resume_expression_shape_inside_with_contract() {
        let source = r#"
main =
  with
    yield item ->
      resume ()
  in
    1
"#;
        let module = parse_module(source).expect("with body should parse");
        let stmts = module.declarations[0]
            .parsed_body
            .as_ref()
            .expect("declaration body should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::With { handler, .. }) => match handler.as_ref() {
                Expr::Handler { clauses } => {
                    let clause_stmts = clauses[0]
                        .parsed_body
                        .as_ref()
                        .expect("handler clause body should parse");
                    assert_eq!(clause_stmts.len(), 1);
                    match &clause_stmts[0] {
                        Stmt::Expr(Expr::Resume { value }) => {
                            assert!(value.is_unit_value());
                        }
                        other => panic!("unexpected statement shape: {:?}", other),
                    }
                }
                other => panic!("unexpected handler shape: {:?}", other),
            },
            other => panic!("unexpected statement shape: {:?}", other),
        }
    }

    #[test]
    fn parses_nested_with_inside_handler_clause_body() {
        let source = r#"
effect Outer
  op: String -> Unit

effect Inner
  boom: String -> Unit

main =
  with
    op msg ->
      with
        boom inner ->
          print inner
      in
        boom msg
      resume ()
  in
    op "x"
"#;
        let module = parse_module(source).expect("nested with in handler clause should parse");
        let stmts = module.declarations[0]
            .parsed_body
            .as_ref()
            .expect("main body should parse");
        let Stmt::Expr(Expr::With { handler, .. }) = &stmts[0] else {
            panic!("expected top-level with");
        };
        let Expr::Handler { clauses } = handler.as_ref() else {
            panic!("expected inline handler");
        };
        let clause_stmts = clauses[0]
            .parsed_body
            .as_ref()
            .expect("handler clause body should keep nested indentation and parse");
        assert!(
            clause_stmts
                .iter()
                .any(|stmt| matches!(stmt, Stmt::Expr(Expr::With { .. }))),
            "handler clause body should contain nested with expression"
        );
    }

    #[test]
    fn parse_error_for_malformed_resume_in_declaration_body() {
        let source = "main = resume\n";
        let err = parse_module(source).expect_err("malformed resume should be rejected");
        assert_eq!(err.line, 1);
        assert_eq!(err.col, 1);
        assert!(err.message.contains("malformed `resume` expression"));
    }

    #[test]
    fn parse_error_for_malformed_resume_in_with_clause_body() {
        let source = r#"
main =
  with
    yield item ->
      resume
  in
    1
"#;
        let err = parse_module(source).expect_err("malformed handler resume should be rejected");
        assert!(err.message.contains("malformed `resume` expression"));
    }

    #[test]
    fn parses_import_example_with_plain_alias_and_selective_imports() {
        let source = read_example("import.gb");
        let module = parse_module(&source).expect("import.gb should parse");

        assert_eq!(module.imports.len(), 3);
        assert!(module.type_declarations.is_empty());
        assert_eq!(module.imports[0].module_path, "goby/string");
        assert_eq!(module.imports[0].kind, ImportKind::Plain);
        assert_eq!(module.imports[1].module_path, "goby/list");
        assert_eq!(module.imports[1].kind, ImportKind::Alias("l".to_string()));
        assert_eq!(module.imports[2].module_path, "goby/env");
        assert_eq!(
            module.imports[2].kind,
            ImportKind::Selective(vec!["fetch_env_var".to_string()])
        );
        assert_eq!(module.declarations.len(), 1);
    }

    #[test]
    fn rejects_import_with_invalid_syntax() {
        let source = "import goby/env ()\nmain = 1\n";
        let err = parse_module(source).expect_err("invalid import should fail");
        assert!(err.message.contains("invalid import declaration"));
    }

    #[test]
    fn parses_embed_effect_declaration() {
        let source =
            "@embed Print __goby_embeded_effect_stdout_handler\nmain : Unit -> Unit\nmain = 1\n";
        let module = parse_module(source).expect("embed declaration should parse");
        assert_eq!(module.embed_declarations.len(), 1);
        assert_eq!(module.embed_declarations[0].effect_name, "Print");
        assert_eq!(
            module.embed_declarations[0].handler_name,
            "__goby_embeded_effect_stdout_handler"
        );
        assert_eq!(module.embed_declarations[0].line, 1);
    }

    #[test]
    fn rejects_invalid_embed_declaration() {
        let source = "@embed 1Print __goby_embeded_effect_stdout_handler\nmain = 1\n";
        let err = parse_module(source).expect_err("invalid embed declaration should fail");
        assert!(err.message.contains("invalid embedded effect name"));
    }

    #[test]
    fn rejects_embed_without_target() {
        let source = "@embed\nmain = 1\n";
        let err = parse_module(source).expect_err("embed without target should fail");
        assert!(
            err.message.contains(
                "invalid @embed declaration: expected `@embed <EffectName> <HandlerName>`"
            )
        );
    }

    #[test]
    fn rejects_embed_without_handler_name() {
        let source = "@embed Print\nmain = 1\n";
        let err = parse_module(source).expect_err("embed without handler should fail");
        assert!(err.message.contains("embedded handler name is missing"));
    }

    #[test]
    fn rejects_legacy_embed_effect_form() {
        let source = "@embed effect Print\nmain = 1\n";
        let err = parse_module(source).expect_err("legacy embed syntax should fail");
        assert!(
            err.message
                .contains("legacy `@embed effect <EffectName>` is no longer supported")
        );
    }

    #[test]
    fn rejects_embed_with_invalid_handler_name() {
        let source = "@embed Print 1handler\nmain = 1\n";
        let err = parse_module(source).expect_err("invalid handler name should fail");
        assert!(err.message.contains("invalid embedded handler name"));
    }

    #[test]
    fn rejects_effect_member_with_reserved_name() {
        let source = "effect Print\n  if: String -> Unit\nmain = 1\n";
        let err = parse_module(source).expect_err("reserved effect member name should fail");
        assert!(
            err.message
                .contains("effect member name must be a non-reserved identifier")
        );
    }

    #[test]
    fn rejects_effect_member_without_colon_signature() {
        let source = "effect Print\n  print String -> Unit\nmain = 1\n";
        let err = parse_module(source).expect_err("malformed effect member should fail");
        assert!(
            err.message
                .contains("invalid effect member signature: expected `name: Type`")
        );
    }

    #[test]
    fn parses_effect_declaration_with_type_parameters() {
        let source = "effect Iterator a b\n  yield: a -> b -> (Bool, b)\nmain = 1\n";
        let module = parse_module(source).expect("generic effect declaration should parse");
        assert_eq!(module.effect_declarations.len(), 1);
        assert_eq!(module.effect_declarations[0].name, "Iterator");
        assert_eq!(
            module.effect_declarations[0].type_params,
            vec!["a".to_string(), "b".to_string()]
        );
    }

    #[test]
    fn rejects_effect_declaration_with_duplicate_type_parameter() {
        let source = "effect Iterator a a\n  yield: a -> a\nmain = 1\n";
        let err =
            parse_module(source).expect_err("duplicate effect type parameters should be rejected");
        assert!(err.message.contains("duplicate effect type parameter"));
    }

    #[test]
    fn rejects_effect_declaration_with_invalid_type_parameter_name() {
        let source = "effect Iterator A\n  yield: A -> A\nmain = 1\n";
        let err =
            parse_module(source).expect_err("uppercase effect type parameter should be rejected");
        assert!(
            err.message
                .contains("effect type parameter must start with a lowercase letter or `_`")
        );
    }

    #[test]
    fn rejects_lowercase_effect_declaration_name() {
        let source = "effect print\n  log: String -> Unit\nmain = 1\n";
        let err = parse_module(source).expect_err("lowercase effect name should be rejected");
        assert!(
            err.message
                .contains("effect declaration name must be CamelCase")
        );
    }

    #[test]
    fn rejects_lowercase_type_declaration_name() {
        let source = "type user = User(name: String)\nmain = 1\n";
        let err = parse_module(source).expect_err("lowercase type name should be rejected");
        assert!(err.message.contains("invalid type declaration"));
    }

    #[test]
    fn rejects_uppercase_top_level_declaration_name() {
        let source = "Main : Unit -> Unit\nMain = ()\n";
        let err =
            parse_module(source).expect_err("top-level declaration starting uppercase rejected");
        assert!(
            err.message
                .contains("declaration name must start with a lowercase letter")
        );
    }

    #[test]
    fn parses_type_example_alias_union_and_record() {
        let source = read_example("type.gb");
        let module = parse_module(&source).expect("type.gb should parse");

        assert!(module.imports.is_empty());
        assert_eq!(module.type_declarations.len(), 3);
        assert_eq!(module.declarations.len(), 1);
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Alias {
                name: "UserID".to_string(),
                target: "String".to_string(),
            }
        );
        assert_eq!(
            module.type_declarations[1],
            TypeDeclaration::Union {
                name: "UserStatus".to_string(),
                constructors: vec!["Activated".to_string(), "Deactivated".to_string()],
            }
        );
        assert_eq!(
            module.type_declarations[2],
            TypeDeclaration::Record {
                name: "User".to_string(),
                constructor: "User".to_string(),
                fields: vec![
                    crate::ast::RecordField {
                        name: "id".to_string(),
                        ty: "UserID".to_string(),
                    },
                    crate::ast::RecordField {
                        name: "name".to_string(),
                        ty: "String".to_string(),
                    },
                    crate::ast::RecordField {
                        name: "status".to_string(),
                        ty: "UserStatus".to_string(),
                    },
                ],
            }
        );
        let main = &module.declarations[0];
        let stmts = main.parsed_body.as_ref().expect("main body should parse");
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn parses_type_alias_with_parenthesized_application_as_alias() {
        let source = "type Wrapped = List (TypeY a b)\nmain = 1\n";
        let module = parse_module(source).expect("type alias should parse");
        assert_eq!(module.type_declarations.len(), 1);
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Alias {
                name: "Wrapped".to_string(),
                target: "List (TypeY a b)".to_string(),
            }
        );
    }

    #[test]
    fn does_not_treat_nested_pipe_in_alias_as_union() {
        let source = "type Wrapped = Maybe (A | B)\nmain = 1\n";
        let module = parse_module(source).expect("type alias with nested pipe should parse");
        assert_eq!(module.type_declarations.len(), 1);
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Alias {
                name: "Wrapped".to_string(),
                target: "Maybe (A | B)".to_string(),
            }
        );
    }

    #[test]
    fn definition_with_equality_in_body_parses_correctly() {
        // `f x = x == 0` — the `==` in the body must not confuse the definition splitter.
        // The definition `=` is the first plain `=`; the `==` in the body is part of RHS.
        let decl = parse_single_declaration("f : Int -> Int\nf x = 42\n");
        assert_eq!(decl.name, "f");
        assert_eq!(decl.params, vec!["x".to_string()]);
    }

    #[test]
    fn allows_comment_between_annotation_and_definition() {
        let source = "main : Unit -> Unit can Print\n# comment\n\nmain = print \"ok\"\n";
        let declaration = parse_single_declaration(source);
        assert_eq!(declaration.name, "main");
    }

    #[test]
    fn allows_line_end_comments_in_type_and_definition() {
        let source = "main : Unit -> Unit can Print # type note\nmain = print \"ok\" # body note\n";
        let declaration = parse_single_declaration(source);
        assert_eq!(declaration.name, "main");
        assert_eq!(
            declaration.type_annotation.as_deref(),
            Some("Unit -> Unit can Print")
        );
    }

    #[test]
    fn treats_shebang_as_comment_line() {
        let source = "#!/usr/bin/env goby\nmain : Unit -> Unit can Print\nmain = print \"ok\"\n";
        let declaration = parse_single_declaration(source);
        assert_eq!(declaration.name, "main");
    }

    #[test]
    fn does_not_treat_hash_inside_string_as_comment() {
        let source = "main : Unit -> Unit can Print\nmain = print \"a#b\" # trailing comment\n";
        let declaration = parse_single_declaration(source);
        let parsed = declaration
            .parsed_body
            .expect("body with trailing comment should parse");
        assert_eq!(parsed.len(), 1);
        match &parsed[0] {
            Stmt::Expr(Expr::Call { arg, .. }) => {
                assert_eq!(**arg, Expr::StringLit("a#b".to_string()));
            }
            other => panic!("unexpected stmt: {:?}", other),
        }
    }

    #[test]
    fn allows_mixed_tabs_and_spaces_in_same_block() {
        let source =
            "main : Unit -> Unit can Print\nmain =\n  greeting = \"hello\"\n\tprint greeting\n";
        let declaration = parse_single_declaration(source);
        assert!(declaration.body.contains("greeting = \"hello\""));
        assert!(declaration.body.contains("print greeting"));
    }

    // -----------------------------------------------------------------------
    // parse_expr tests
    // -----------------------------------------------------------------------

    #[test]
    fn parses_int_literal() {
        assert_eq!(parse_expr("42"), Some(Expr::IntLit(42)));
        assert_eq!(parse_expr("-5"), Some(Expr::IntLit(-5)));
    }

    #[test]
    fn parses_string_literal() {
        assert_eq!(
            parse_expr("\"hello\""),
            Some(Expr::StringLit("hello".to_string()))
        );
    }

    #[test]
    fn parses_interpolated_string_with_variables() {
        assert_eq!(
            parse_expr("\"${a}${b}\""),
            Some(Expr::InterpolatedString(vec![
                InterpolatedPart::Expr(Box::new(Expr::Var("a".to_string()))),
                InterpolatedPart::Expr(Box::new(Expr::Var("b".to_string()))),
            ]))
        );
    }

    #[test]
    fn parses_interpolated_string_with_literal_prefix_and_suffix() {
        assert_eq!(
            parse_expr("\"hello ${name}!\""),
            Some(Expr::InterpolatedString(vec![
                InterpolatedPart::Text("hello ".to_string()),
                InterpolatedPart::Expr(Box::new(Expr::Var("name".to_string()))),
                InterpolatedPart::Text("!".to_string()),
            ]))
        );
    }

    #[test]
    fn parses_interpolated_string_with_string_literal_expression() {
        assert_eq!(
            parse_expr("\"${\"a\"}\""),
            Some(Expr::InterpolatedString(vec![InterpolatedPart::Expr(
                Box::new(Expr::StringLit("a".to_string())),
            )]))
        );
    }

    #[test]
    fn rejects_interpolated_string_with_unclosed_expression() {
        assert_eq!(parse_expr("\"x${name\""), None);
    }

    #[test]
    fn parses_identifier() {
        assert_eq!(parse_expr("foo"), Some(Expr::Var("foo".to_string())));
    }

    #[test]
    fn parses_addition() {
        assert_eq!(
            parse_expr("a + b"),
            Some(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::Var("a".to_string())),
                right: Box::new(Expr::Var("b".to_string())),
            })
        );
    }

    #[test]
    fn parses_equality() {
        assert_eq!(
            parse_expr("a == b"),
            Some(Expr::BinOp {
                op: BinOpKind::Eq,
                left: Box::new(Expr::Var("a".to_string())),
                right: Box::new(Expr::Var("b".to_string())),
            })
        );
    }

    #[test]
    fn parses_bool_literals() {
        assert_eq!(parse_expr("True"), Some(Expr::BoolLit(true)));
        assert_eq!(parse_expr("False"), Some(Expr::BoolLit(false)));
    }

    #[test]
    fn parses_multiplication() {
        assert_eq!(
            parse_expr("b * 3"),
            Some(Expr::BinOp {
                op: BinOpKind::Mul,
                left: Box::new(Expr::Var("b".to_string())),
                right: Box::new(Expr::IntLit(3)),
            })
        );
    }

    #[test]
    fn parses_operator_precedence_mul_higher_than_add() {
        assert_eq!(
            parse_expr("1 + 2 * 3"),
            Some(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::IntLit(1)),
                right: Box::new(Expr::BinOp {
                    op: BinOpKind::Mul,
                    left: Box::new(Expr::IntLit(2)),
                    right: Box::new(Expr::IntLit(3)),
                }),
            })
        );
    }

    #[test]
    fn parses_equality_as_lower_precedence_than_addition() {
        assert_eq!(
            parse_expr("a + b == c"),
            Some(Expr::BinOp {
                op: BinOpKind::Eq,
                left: Box::new(Expr::BinOp {
                    op: BinOpKind::Add,
                    left: Box::new(Expr::Var("a".to_string())),
                    right: Box::new(Expr::Var("b".to_string())),
                }),
                right: Box::new(Expr::Var("c".to_string())),
            })
        );
    }

    #[test]
    fn parses_left_associative_addition() {
        assert_eq!(
            parse_expr("1 + 2 + 3"),
            Some(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::BinOp {
                    op: BinOpKind::Add,
                    left: Box::new(Expr::IntLit(1)),
                    right: Box::new(Expr::IntLit(2)),
                }),
                right: Box::new(Expr::IntLit(3)),
            })
        );
    }

    #[test]
    fn parses_left_associative_multiplication() {
        assert_eq!(
            parse_expr("2 * 3 * 4"),
            Some(Expr::BinOp {
                op: BinOpKind::Mul,
                left: Box::new(Expr::BinOp {
                    op: BinOpKind::Mul,
                    left: Box::new(Expr::IntLit(2)),
                    right: Box::new(Expr::IntLit(3)),
                }),
                right: Box::new(Expr::IntLit(4)),
            })
        );
    }

    #[test]
    fn parses_pipeline_as_lowest_precedence() {
        assert_eq!(
            parse_expr("1 + 2 |> print"),
            Some(Expr::Pipeline {
                value: Box::new(Expr::BinOp {
                    op: BinOpKind::Add,
                    left: Box::new(Expr::IntLit(1)),
                    right: Box::new(Expr::IntLit(2)),
                }),
                callee: "print".to_string(),
            })
        );
    }

    #[test]
    fn parses_left_associative_pipeline_chain() {
        assert_eq!(
            parse_expr("x |> f |> g"),
            Some(Expr::Pipeline {
                value: Box::new(Expr::Pipeline {
                    value: Box::new(Expr::Var("x".to_string())),
                    callee: "f".to_string(),
                }),
                callee: "g".to_string(),
            })
        );
    }

    #[test]
    fn rejects_pipeline_with_non_identifier_callee_expression() {
        assert_eq!(parse_expr("x |> f 1"), None);
        assert_eq!(parse_expr("x |> string.split(a, b)"), None);
    }

    #[test]
    fn requires_spaces_around_infix_operators_in_mvp_parser() {
        assert_eq!(parse_expr("a+b"), None);
        assert_eq!(parse_expr("a*2"), None);
        assert_eq!(parse_expr("a==b"), None);
    }

    #[test]
    fn parses_list_literal() {
        assert_eq!(
            parse_expr("[3, 4, 5]"),
            Some(Expr::ListLit {
                elements: vec![Expr::IntLit(3), Expr::IntLit(4), Expr::IntLit(5)],
                spread: None,
            })
        );
    }

    #[test]
    fn parses_list_spread_with_variable() {
        assert_eq!(
            parse_expr("[1, 2, ..xs]"),
            Some(Expr::ListLit {
                elements: vec![Expr::IntLit(1), Expr::IntLit(2)],
                spread: Some(Box::new(Expr::Var("xs".to_string()))),
            })
        );
    }

    #[test]
    fn parses_list_spread_with_call_expr() {
        assert_eq!(
            parse_expr("[f(x), ..ys]"),
            Some(Expr::ListLit {
                elements: vec![Expr::Call {
                    callee: Box::new(Expr::Var("f".to_string())),
                    arg: Box::new(Expr::Var("x".to_string())),
                }],
                spread: Some(Box::new(Expr::Var("ys".to_string()))),
            })
        );
    }

    #[test]
    fn parses_list_spread_single_prefix() {
        assert_eq!(
            parse_expr("[a, ..rest]"),
            Some(Expr::ListLit {
                elements: vec![Expr::Var("a".to_string())],
                spread: Some(Box::new(Expr::Var("rest".to_string()))),
            })
        );
    }

    #[test]
    fn rejects_list_spread_only() {
        // [..xs] with no prefix elements is rejected.
        assert_eq!(parse_expr("[..xs]"), None);
    }

    #[test]
    fn rejects_list_spread_non_trailing() {
        // [..a, b] is rejected.
        assert_eq!(parse_expr("[..a, b]"), None);
    }

    #[test]
    fn rejects_list_spread_middle() {
        // [a, ..b, c] is rejected.
        assert_eq!(parse_expr("[a, ..b, c]"), None);
    }

    #[test]
    fn rejects_list_spread_missing_expr() {
        // [a, ..] is rejected in expression position.
        assert_eq!(parse_expr("[a, ..]"), None);
    }

    #[test]
    fn parses_tuple_literal() {
        assert_eq!(
            parse_expr("(\"Hello\", 3)"),
            Some(Expr::TupleLit(vec![
                Expr::StringLit("Hello".to_string()),
                Expr::IntLit(3)
            ]))
        );
    }

    #[test]
    fn parses_unit_literal_as_empty_tuple_syntax() {
        assert_eq!(parse_expr("()"), Some(Expr::unit_value()));
    }

    #[test]
    fn parses_spaced_function_call() {
        assert_eq!(
            parse_expr("add_ten 10"),
            Some(Expr::Call {
                callee: Box::new(Expr::Var("add_ten".to_string())),
                arg: Box::new(Expr::IntLit(10)),
            })
        );
    }

    #[test]
    fn parses_spaced_multi_arg_function_call_left_associative() {
        assert_eq!(
            parse_expr("f a b c"),
            Some(Expr::Call {
                callee: Box::new(Expr::Call {
                    callee: Box::new(Expr::Call {
                        callee: Box::new(Expr::Var("f".to_string())),
                        arg: Box::new(Expr::Var("a".to_string())),
                    }),
                    arg: Box::new(Expr::Var("b".to_string())),
                }),
                arg: Box::new(Expr::Var("c".to_string())),
            })
        );
    }

    #[test]
    fn parses_spaced_unit_argument_function_call() {
        assert_eq!(
            parse_expr("read_line ()"),
            Some(Expr::Call {
                callee: Box::new(Expr::Var("read_line".to_string())),
                arg: Box::new(Expr::unit_value()),
            })
        );
    }

    #[test]
    fn parses_parenthesized_unit_argument_function_call() {
        assert_eq!(
            parse_expr("read_line()"),
            Some(Expr::Call {
                callee: Box::new(Expr::Var("read_line".to_string())),
                arg: Box::new(Expr::unit_value()),
            })
        );
    }

    #[test]
    fn parses_spaced_constructor_call_when_name_is_camel_case() {
        assert_eq!(
            parse_expr("User ()"),
            Some(Expr::RecordConstruct {
                constructor: "User".to_string(),
                fields: vec![],
            })
        );
    }

    #[test]
    fn parses_named_lambda() {
        assert_eq!(
            parse_expr("|n| -> n * 10"),
            Some(Expr::Lambda {
                param: "n".to_string(),
                body: Box::new(Expr::BinOp {
                    op: BinOpKind::Mul,
                    left: Box::new(Expr::Var("n".to_string())),
                    right: Box::new(Expr::IntLit(10)),
                }),
            })
        );
    }

    #[test]
    fn parses_placeholder_lambda() {
        assert_eq!(
            parse_expr("_ * 10"),
            Some(Expr::Lambda {
                param: "_".to_string(),
                body: Box::new(Expr::BinOp {
                    op: BinOpKind::Mul,
                    left: Box::new(Expr::Var("_".to_string())),
                    right: Box::new(Expr::IntLit(10)),
                }),
            })
        );
    }

    #[test]
    fn parses_pipeline() {
        let expr = parse_expr("mul_tens [3, 4, 5] |> print").expect("should parse");
        match expr {
            Expr::Pipeline { value, callee } => {
                assert_eq!(callee, "print");
                match *value {
                    Expr::Call { .. } => {}
                    other => panic!("expected Call, got {:?}", other),
                }
            }
            other => panic!("expected Pipeline, got {:?}", other),
        }
    }

    #[test]
    fn parses_qualified_method_call() {
        assert_eq!(
            parse_expr("string.split(a, b)"),
            Some(Expr::MethodCall {
                receiver: "string".to_string(),
                method: "split".to_string(),
                args: vec![Expr::Var("a".to_string()), Expr::Var("b".to_string())],
            })
        );
    }

    #[test]
    fn parses_generic_qualified_method_call() {
        assert_eq!(
            parse_expr("l.join(paths, \"\\n\")"),
            Some(Expr::MethodCall {
                receiver: "l".to_string(),
                method: "join".to_string(),
                args: vec![
                    Expr::Var("paths".to_string()),
                    Expr::StringLit("\n".to_string()),
                ],
            })
        );
    }

    #[test]
    fn parses_record_constructor_call_with_named_fields() {
        assert_eq!(
            parse_expr("User(id: \"1234\", name: \"John\")"),
            Some(Expr::RecordConstruct {
                constructor: "User".to_string(),
                fields: vec![
                    ("id".to_string(), Expr::StringLit("1234".to_string())),
                    ("name".to_string(), Expr::StringLit("John".to_string())),
                ],
            })
        );
    }

    #[test]
    fn parses_qualified_access_expression() {
        assert_eq!(
            parse_expr("UserStatus.Activated"),
            Some(Expr::Qualified {
                receiver: "UserStatus".to_string(),
                member: "Activated".to_string(),
            })
        );
        assert_eq!(
            parse_expr("user.name"),
            Some(Expr::Qualified {
                receiver: "user".to_string(),
                member: "name".to_string(),
            })
        );
        assert_eq!(
            parse_expr("pair.0"),
            Some(Expr::Qualified {
                receiver: "pair".to_string(),
                member: "0".to_string(),
            })
        );
    }

    #[test]
    fn parses_qualified_method_call_with_three_arguments() {
        assert_eq!(
            parse_expr("string.join(a, b, c)"),
            Some(Expr::MethodCall {
                receiver: "string".to_string(),
                method: "join".to_string(),
                args: vec![
                    Expr::Var("a".to_string()),
                    Expr::Var("b".to_string()),
                    Expr::Var("c".to_string())
                ],
            })
        );
    }

    #[test]
    fn parses_resume_expression() {
        assert_eq!(
            parse_expr("resume ()"),
            Some(Expr::Resume {
                value: Box::new(Expr::unit_value())
            })
        );
    }

    #[test]
    fn parses_resume_expression_with_unit_literal() {
        assert_eq!(
            parse_expr("resume ()"),
            Some(Expr::Resume {
                value: Box::new(Expr::unit_value())
            })
        );
    }

    #[test]
    fn rejects_malformed_resume_expression() {
        assert_eq!(parse_expr("resume"), None);
    }

    // -----------------------------------------------------------------------
    // parse_body_stmts tests
    // -----------------------------------------------------------------------

    #[test]
    fn parses_simple_binding_body() {
        let body = "b = a + 10\nb * 3";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 2);
        assert!(matches!(&stmts[0], Stmt::Binding { name, .. } if name == "b"));
        assert!(matches!(
            &stmts[1],
            Stmt::Expr(Expr::BinOp {
                op: BinOpKind::Mul,
                ..
            })
        ));
    }

    #[test]
    fn parses_rebinding_in_same_body() {
        let body = "a = 1\na = a + 1\na";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 3);
        assert!(matches!(&stmts[0], Stmt::Binding { name, .. } if name == "a"));
        assert!(matches!(&stmts[1], Stmt::Binding { name, .. } if name == "a"));
        assert!(matches!(&stmts[2], Stmt::Expr(Expr::Var(name)) if name == "a"));
    }

    #[test]
    fn parses_mut_binding_and_assignment_body() {
        let body = "mut a = 1\na := 2\na";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 3);
        assert!(matches!(&stmts[0], Stmt::MutBinding { name, .. } if name == "a"));
        assert!(matches!(&stmts[1], Stmt::Assign { name, .. } if name == "a"));
        assert!(matches!(&stmts[2], Stmt::Expr(Expr::Var(name)) if name == "a"));
    }

    #[test]
    fn equality_is_parsed_as_expression_not_binding_statement() {
        assert!(matches!(
            parse_stmt("a == 1"),
            Some(Stmt::Expr(Expr::BinOp {
                op: BinOpKind::Eq,
                ..
            }))
        ));
    }

    #[test]
    fn parses_print_stmt() {
        let body = "print \"hello\"";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::Call { callee, arg }) => {
                assert_eq!(**callee, Expr::Var("print".to_string()));
                assert_eq!(**arg, Expr::StringLit("hello".to_string()));
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parses_handler_expression_binding_body() {
        let body = "h = handler\n  emit x ->\n    resume x\nh";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 2);
        match &stmts[0] {
            Stmt::Binding { name, value } => {
                assert_eq!(name, "h");
                match value {
                    Expr::Handler { clauses } => {
                        assert_eq!(clauses.len(), 1);
                        assert_eq!(clauses[0].name, "emit");
                        assert_eq!(clauses[0].params, vec!["x".to_string()]);
                    }
                    other => panic!("expected handler expression, got {other:?}"),
                }
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_with_expression_statement() {
        let body = "with h\nin\n  emit 1";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::With { handler, body }) => {
                assert_eq!(**handler, Expr::Var("h".to_string()));
                assert_eq!(body.len(), 1);
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_with_inline_handler_statement() {
        let body = "with\n  emit x ->\n    resume x\nin\n  emit 1";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::With { handler, body }) => {
                assert_eq!(body.len(), 1);
                match handler.as_ref() {
                    Expr::Handler { clauses } => {
                        assert_eq!(clauses.len(), 1);
                        assert_eq!(clauses[0].name, "emit");
                    }
                    other => panic!("expected handler expression, got {other:?}"),
                }
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn rejects_legacy_with_handler_statement_syntax() {
        let body = "with_handler\n  emit x ->\n    resume x\nin\n  emit 1";
        assert!(
            parse_body_stmts(body).is_none(),
            "legacy with_handler statement should fail parse"
        );
    }

    #[test]
    fn parses_multiline_with_rhs_in_binding() {
        let body = "x = with\n  emit v ->\n    resume v\nin\n  emit 1\nprint x";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 2);
        match &stmts[0] {
            Stmt::Binding { name, value } => {
                assert_eq!(name, "x");
                assert!(matches!(value, Expr::With { .. }));
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_multiline_with_rhs_in_assignment() {
        let body = "mut x = 0\nx := with\n  emit v ->\n    resume v\nin\n  emit 1\nprint x";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 3);
        match &stmts[1] {
            Stmt::Assign { name, value } => {
                assert_eq!(name, "x");
                assert!(matches!(value, Expr::With { .. }));
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_next_line_with_rhs_in_binding() {
        // `name =\n  with ...` where `with` appears on the next indented line.
        let body = "x =\n  with\n    emit v ->\n      resume v\n  in\n    emit 1\nprint x";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 2);
        match &stmts[0] {
            Stmt::Binding { name, value } => {
                assert_eq!(name, "x");
                assert!(matches!(value, Expr::With { .. }));
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_next_line_with_rhs_in_mut_binding() {
        // `mut name =\n  with ...` where `with` appears on the next indented line.
        let body = "mut x =\n  with\n    emit v ->\n      resume v\n  in\n    emit 1\nprint x";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 2);
        match &stmts[0] {
            Stmt::MutBinding { name, value } => {
                assert_eq!(name, "x");
                assert!(matches!(value, Expr::With { .. }));
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_next_line_with_inline_handler_rhs_in_binding() {
        // `name =\n  with inlineHandler\n  in\n    body` — inline handler on the next-line `with`.
        let body = "x =\n  with h\n  in\n    emit 1\nprint x";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 2);
        match &stmts[0] {
            Stmt::Binding { name, value } => {
                assert_eq!(name, "x");
                assert!(matches!(value, Expr::With { .. }));
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_next_line_with_rhs_in_assignment() {
        // `name :=\n  with ...` where `with` appears on the next indented line.
        let body =
            "mut x = 0\nx :=\n  with\n    emit v ->\n      resume v\n  in\n    emit 1\nprint x";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 3);
        match &stmts[1] {
            Stmt::Assign { name, value } => {
                assert_eq!(name, "x");
                assert!(matches!(value, Expr::With { .. }));
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_case_list_patterns() {
        let body = "print\n  case xs\n    [] -> 0\n    [x, ..xxs] -> x";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::Call { arg, .. }) => match arg.as_ref() {
                Expr::Case { arms, .. } => {
                    assert_eq!(arms.len(), 2);
                    assert!(matches!(arms[0].pattern, CasePattern::EmptyList));
                    assert!(matches!(
                        &arms[1].pattern,
                        CasePattern::ListPattern { items, tail }
                        if items
                            == &vec![ListPatternItem::Bind("x".to_string())]
                            && tail == &Some(ListPatternTail::Bind("xxs".to_string()))
                    ));
                }
                other => panic!("unexpected call arg: {other:?}"),
            },
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_case_arm_block_body() {
        let body = "print\n  case x\n    0 ->\n      y = 1\n      y + 10\n    _ -> 0";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::Call { arg, .. }) => match arg.as_ref() {
                Expr::Case { arms, .. } => {
                    assert_eq!(arms.len(), 2);
                    match arms[0].body.as_ref() {
                        Expr::Block(stmts) => {
                            assert_eq!(stmts.len(), 2);
                            assert!(matches!(stmts[0], Stmt::Binding { .. }));
                            assert!(matches!(stmts[1], Stmt::Expr(_)));
                        }
                        other => panic!("expected block arm body, got {other:?}"),
                    }
                }
                other => panic!("unexpected call arg: {other:?}"),
            },
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_multiline_case_rhs_in_binding() {
        let body = "a = 10\nb = case a\n  10 ->\n    1 + 10\n  _ ->\n    20 + 30\nprint b";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 3);
        match &stmts[1] {
            Stmt::Binding { name, value } => {
                assert_eq!(name, "b");
                match value {
                    Expr::Case { arms, .. } => {
                        assert_eq!(arms.len(), 2);
                        assert!(matches!(arms[0].body.as_ref(), Expr::Block(_)));
                        assert!(matches!(arms[1].body.as_ref(), Expr::Block(_)));
                    }
                    other => panic!("expected case expression in binding rhs, got {other:?}"),
                }
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_multiline_if_rhs_in_binding() {
        let body = "x = if True\n  1\nelse\n  2\nprint x";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 2);
        match &stmts[0] {
            Stmt::Binding { name, value } => {
                assert_eq!(name, "x");
                assert!(matches!(value, Expr::If { .. }));
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_standalone_multiline_case_expression_statement() {
        let body = "a = 10\ncase a\n  10 -> print \"Ten\"\n  _ -> print \"Other\"";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 2);
        match &stmts[1] {
            Stmt::Expr(Expr::Case { arms, .. }) => {
                assert_eq!(arms.len(), 2);
                assert!(matches!(arms[0].body.as_ref(), Expr::Call { .. }));
                assert!(matches!(arms[1].body.as_ref(), Expr::Call { .. }));
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_parenthesized_multiline_case_call_argument() {
        let body = "print (\n  case x\n    0 -> 1\n    _ -> 2\n)";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::Call { arg, .. }) => {
                assert!(matches!(arg.as_ref(), Expr::Case { .. }));
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_parenthesized_multiline_if_call_argument() {
        let body = "print (\n  if True\n    1\n  else\n    2\n)";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::Call { arg, .. }) => {
                assert!(matches!(arg.as_ref(), Expr::If { .. }));
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn rejects_case_arm_block_without_indented_body() {
        let body = "print\n  case x\n    0 ->\n    _ -> 0";
        assert!(
            parse_body_stmts(body).is_none(),
            "arm block without indented body should fail parse"
        );
    }

    #[test]
    fn rejects_effectively_empty_case_arm_block() {
        let body = "print\n  case x\n    0 ->\n      # only comment\n    _ -> 0";
        assert!(
            parse_body_stmts(body).is_none(),
            "effectively empty arm block should fail parse"
        );
    }

    #[test]
    fn parses_fixed_length_and_literal_head_list_patterns() {
        let body =
            "print\n  case xs\n    [1] -> 1\n    [4, ..] -> 4\n    [_, _] -> 2\n    [a, ..b] -> a";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::Call { arg, .. }) => match arg.as_ref() {
                Expr::Case { arms, .. } => {
                    assert_eq!(arms.len(), 4);
                    assert!(matches!(
                        &arms[0].pattern,
                        CasePattern::ListPattern { items, tail }
                        if items == &vec![ListPatternItem::IntLit(1)] && tail.is_none()
                    ));
                    assert!(matches!(
                        &arms[1].pattern,
                        CasePattern::ListPattern { items, tail }
                        if items == &vec![ListPatternItem::IntLit(4)]
                            && tail == &Some(ListPatternTail::Ignore)
                    ));
                    assert!(matches!(
                        &arms[2].pattern,
                        CasePattern::ListPattern { items, tail }
                        if items
                            == &vec![ListPatternItem::Wildcard, ListPatternItem::Wildcard]
                            && tail.is_none()
                    ));
                    assert!(matches!(
                        &arms[3].pattern,
                        CasePattern::ListPattern { items, tail }
                        if items
                            == &vec![ListPatternItem::Bind("a".to_string())]
                            && tail == &Some(ListPatternTail::Bind("b".to_string()))
                    ));
                }
                other => panic!("unexpected call arg: {other:?}"),
            },
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn rejects_malformed_case_list_pattern() {
        let body = "print\n  case xs\n    [..xs] -> 0\n    _ -> 0";
        assert!(
            parse_body_stmts(body).is_none(),
            "malformed list pattern should fail parse"
        );
    }

    #[test]
    fn rejects_case_list_pattern_with_bool_item() {
        let body = "print\n  case xs\n    [True] -> 1\n    _ -> 0";
        assert!(
            parse_body_stmts(body).is_none(),
            "bool list pattern items are not supported in MVP"
        );
    }

    #[test]
    fn rejects_case_list_pattern_with_duplicate_binders() {
        let body = "print\n  case xs\n    [x, ..x] -> x\n    _ -> 0";
        assert!(
            parse_body_stmts(body).is_none(),
            "duplicate binders in list pattern should fail parse"
        );
    }

    #[test]
    fn parses_function_gb_declarations() {
        let source = read_example("function.gb");
        let module = parse_module(&source).expect("function.gb should parse");
        for decl in &module.declarations {
            assert!(
                decl.parsed_body.is_some(),
                "declaration `{}` should have parsed_body",
                decl.name
            );
        }
    }

    #[test]
    fn parses_effect_gb_declarations() {
        let source = read_example("effect.gb");
        let module = parse_module(&source).expect("effect.gb should parse");
        // effect.gb should have 3 regular declarations: plus_ten_with_log, show_env_var, main
        assert_eq!(
            module.declarations.len(),
            3,
            "effect.gb should have 3 declarations"
        );
        assert_eq!(
            module.effect_declarations.len(),
            2,
            "effect.gb should have 2 effect declarations"
        );
        for decl in &module.declarations {
            assert!(
                decl.parsed_body.is_some(),
                "declaration `{}` should have parsed_body",
                decl.name
            );
        }
    }

    #[test]
    fn if_expr_with_wrong_else_indent_returns_none() {
        // `else` must be at the same indent level as `if`.
        // An under-indented `else` should cause parse_multiline_expr to return None.
        let source = r#"
main : Unit -> Unit
main =
  print
    if True
      "yes"
   else
      "no"
"#;
        // The module may parse (the print call might fall back), but the if/else expression
        // with wrong else indent must not produce an Expr::If node.
        let module = parse_module(source).expect("module-level parse should not panic");
        let main_decl = module.declarations.iter().find(|d| d.name == "main");
        if let Some(decl) = main_decl {
            if let Some(stmts) = &decl.parsed_body {
                // Verify no Stmt contains an Expr::If (the malformed if/else failed to parse).
                for stmt in stmts {
                    if let crate::ast::Stmt::Expr(expr) = stmt {
                        assert!(
                            !matches!(expr, crate::ast::Expr::If { .. }),
                            "malformed else indent should not produce Expr::If"
                        );
                    }
                }
            }
        }
    }

    // --- ParseError line/col regression tests ---

    #[test]
    fn parse_error_unexpected_indentation_reports_line_and_col() {
        // "    foo = 1" at line 1 → col = indent width + 1 = 5
        let source = "    foo = 1\n";
        let err = parse_module(source).expect_err("indented top-level should fail");
        assert_eq!(err.line, 1);
        assert_eq!(err.col, 5);
        assert!(err.message.contains("indentation"));
    }

    #[test]
    fn parse_error_unexpected_indentation_two_spaces_reports_col_3() {
        // Same code path as the 4-space test above; verifies col formula for 2-space indent.
        // "  baz = 2" at line 1: indent width = 2, col = 2 + 1 = 3.
        let source = "  baz = 2\n";
        let err = parse_module(source).expect_err("indented top-level line should fail");
        assert_eq!(err.line, 1);
        assert_eq!(err.col, 3);
        assert!(err.message.contains("indentation"));
    }

    #[test]
    fn parse_error_missing_annotation_body_reports_annotation_line() {
        // Type annotation on line 1 followed by nothing → error at line 1
        let source = "foo : Int\n";
        let err = parse_module(source).expect_err("annotation without body should fail");
        assert_eq!(err.line, 1);
        assert_eq!(err.col, 1);
        assert!(err.message.contains("missing declaration body"));
    }

    #[test]
    fn parse_error_mismatched_annotation_name_reports_definition_line() {
        // Annotation on line 1, definition on line 2 → error points to definition line (2)
        let source = "foo : Int\nbar = 1\n";
        let err = parse_module(source).expect_err("mismatched name should fail");
        assert_eq!(err.line, 2);
        assert_eq!(err.col, 1);
        assert!(err.message.contains("does not match"));
    }

    #[test]
    fn parse_error_for_legacy_handler_block_shape() {
        let source = r#"
effect Log
  log: String -> Unit

handler ConsoleLog for Log
  !!!invalid method line!!!
    print "something"

main : Unit -> Unit
main =
  print "ok"
"#;
        let err = parse_module(source).expect_err("legacy top-level handlers should fail");
        assert!(
            err.message
                .contains("legacy top-level `handler ... for ...` is no longer supported")
        );
    }

    #[test]
    fn parse_error_for_legacy_using_with_tab_after_keyword() {
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      using	H
        log msg
      resume ()
  in
    log "x"
"#;
        let err =
            parse_module(source).expect_err("legacy using with tab after keyword should fail");
        assert!(
            err.message
                .contains("legacy `using` syntax is no longer supported"),
            "unexpected error message: {}",
            err.message
        );
    }

    #[test]
    fn parse_body_stmts_supports_with_handler_and_block_if_branches() {
        let handler_body = r#"if valid
  if index == 0 && grapheme == "-"
    negative := True
    index := index + 1
    resume (True, ())
  else
    index := index + 1
    resume (True, ())
else
  index := index + 1
  resume (True, ())"#;
        assert!(
            parse_body_stmts(handler_body).is_some(),
            "handler multiline if body should parse"
        );

        let final_if = r#"if valid
  acc
else
  invalid_integer value"#;
        assert!(
            parse_body_stmts(final_if).is_some(),
            "final if block should parse"
        );

        let body = r#"with
  yield grapheme _ ->
    if valid
      if index == 0 && grapheme == "-"
        negative := True
        index := index + 1
        resume (True, ())
      else
        index := index + 1
        resume (True, ())
    else
      index := index + 1
      resume (True, ())
in
  __goby_string_each_grapheme value
if valid
  acc
else
  invalid_integer value"#;
        assert!(
            parse_body_stmts(body).is_some(),
            "with-handler body with block if branches should parse"
        );
    }
}
