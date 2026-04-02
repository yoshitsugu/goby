use crate::ast::{Declaration, Expr, Module, Stmt};
use crate::parser_expr::parse_expr as parse_expr_impl;
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
                    col: parts.col,
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
    use crate::ast::{Expr, Stmt};
    use crate::parser_test_support::read_example;
    use std::path::PathBuf;

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

        assert_eq!(module.imports.len(), 2);
        assert_eq!(module.imports[0].module_path, "goby/prelude");
        assert_eq!(module.imports[1].module_path, "goby/int");
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
            Stmt::Expr(Expr::With { handler, .. }, _) => match handler.as_ref() {
                Expr::Handler { clauses } => {
                    let clause_stmts = clauses[0]
                        .parsed_body
                        .as_ref()
                        .expect("handler clause body should parse");
                    assert_eq!(clause_stmts.len(), 1);
                    match &clause_stmts[0] {
                        Stmt::Expr(Expr::Resume { value }, _) => {
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
        let Stmt::Expr(Expr::With { handler, .. }, _) = &stmts[0] else {
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
                .any(|stmt| matches!(stmt, Stmt::Expr(Expr::With { .. }, _))),
            "handler clause body should contain nested with expression"
        );
    }

    #[test]
    fn parses_stdlib_split_helper_body_into_statements() {
        let source = std::fs::read_to_string(
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("..")
                .join("..")
                .join("stdlib")
                .join("goby")
                .join("string.gb"),
        )
        .expect("stdlib string should exist");
        let module = parse_module(&source).expect("stdlib string should parse");
        let helper = module
            .declarations
            .iter()
            .find(|decl| decl.name == "split_with_empty_delimiter")
            .expect("helper decl should exist");
        assert!(
            helper.parsed_body.is_some(),
            "split_with_empty_delimiter body should parse into statements"
        );
    }

    #[test]
    fn preserves_parsed_body_for_tuple_member_call_statements() {
        let source = r#"
pair : Unit -> ((Unit -> Unit), (Unit -> Int))
pair _ =
  mut count = 0
  inc = fn _ ->
    count := count + 1
  get = fn _ -> count
  (inc, get)

main : Unit -> Unit can Print, Read
main =
  _ = read()
  p = pair()
  p.0()
  p.0()
  result = p.1()
  println "${result}"
"#;
        let module = parse_module(source).expect("source should parse");
        let pair = module
            .declarations
            .iter()
            .find(|decl| decl.name == "pair")
            .expect("pair should exist");
        let main = module
            .declarations
            .iter()
            .find(|decl| decl.name == "main")
            .expect("main should exist");
        assert!(
            pair.parsed_body.is_some(),
            "multiline lambda helper body should preserve parsed_body"
        );
        assert!(
            main.parsed_body.is_some(),
            "tuple-member call statements should preserve parsed_body"
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
    fn does_not_treat_hash_inside_string_as_comment() {
        let source = "main : Unit -> Unit can Print\nmain = print \"a#b\" # trailing comment\n";
        let module = parse_module(source).expect("source should parse");
        let declaration = module.declarations[0].clone();
        let parsed = declaration
            .parsed_body
            .expect("body with trailing comment should parse");
        assert_eq!(parsed.len(), 1);
        match &parsed[0] {
            Stmt::Expr(Expr::Call { arg, .. }, _) => {
                assert_eq!(**arg, Expr::StringLit("a#b".to_string()));
            }
            other => panic!("unexpected stmt: {:?}", other),
        }
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
                    if let crate::ast::Stmt::Expr(expr, _) = stmt {
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
}
