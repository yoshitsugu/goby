use std::collections::HashSet;

use crate::ast::{
    EffectDecl, EffectMember, EmbedDecl, ImportDecl, ImportKind, ImportKindSpan, Span,
    TypeDeclaration, UnionVariant,
};
use crate::parser::ParseError;
use crate::parser_util::{
    collect_indented_body, contains_top_level_colon, has_balanced_delimiters,
    is_camel_case_identifier, is_identifier, is_indented, is_lowercase_start_identifier,
    is_module_path, is_non_reserved_identifier, is_reserved_keyword,
    is_type_parameter_identifier, parse_record_field, skip_blank_and_comment_lines,
    split_record_constructor_shape, split_top_level_definition, split_top_level_pipes,
    split_top_level_type, split_type_header, starts_with_keyword_token, strip_line_comment,
};
use crate::str_util::split_top_level_commas;

pub(crate) enum TopLevelItem {
    Import(ImportDecl),
    Embed(EmbedDecl),
    Type(TypeDeclaration),
    Effect(EffectDecl),
    Declaration(DeclarationParts),
}

pub(crate) struct DeclarationParts {
    pub(crate) name: String,
    pub(crate) type_annotation: Option<String>,
    pub(crate) params: Vec<String>,
    pub(crate) body: String,
    pub(crate) line: usize,
    pub(crate) definition_line: usize,
    /// 1-indexed byte offset of the declaration name on the definition line.
    pub(crate) col: usize,
}

pub(crate) fn parse_top_level_item(
    lines: &[&str],
    index: usize,
) -> Result<(TopLevelItem, usize), ParseError> {
    let line = strip_line_comment(lines[index]).trim_end();
    let trimmed = line.trim();
    let line_no = index + 1;

    if trimmed.starts_with("import ") {
        let import = parse_import_line(trimmed, line_no).ok_or_else(|| ParseError {
            line: line_no,
            col: 1,
            message: "invalid import declaration".to_string(),
        })?;
        return Ok((TopLevelItem::Import(import), index + 1));
    }

    if trimmed.starts_with("@embed") {
        let (effect_name, handler_name) =
            parse_embed_line(trimmed).map_err(|message| ParseError {
                line: line_no,
                col: 1,
                message,
            })?;
        return Ok((
            TopLevelItem::Embed(EmbedDecl {
                effect_name,
                handler_name,
                line: line_no,
            }),
            index + 1,
        ));
    }

    if trimmed.starts_with("type ") {
        let ty_decl = parse_type_declaration_line(trimmed).ok_or_else(|| ParseError {
            line: line_no,
            col: 1,
            message: "invalid type declaration".to_string(),
        })?;
        return Ok((TopLevelItem::Type(ty_decl), index + 1));
    }

    if trimmed.starts_with("effect ") && !trimmed.contains('=') {
        let (effect_decl, next_index) = parse_effect_declaration(lines, index)?;
        return Ok((TopLevelItem::Effect(effect_decl), next_index));
    }

    if starts_with_keyword_token(trimmed, "handler") {
        return Err(ParseError {
            line: line_no,
            col: 1,
            message:
                "legacy top-level `handler ... for ...` is no longer supported; use `handler` expressions with `with`"
                    .to_string(),
        });
    }

    let (declaration, next_index) = parse_declaration_header(lines, index)?;
    Ok((TopLevelItem::Declaration(declaration), next_index))
}

fn parse_effect_declaration(
    lines: &[&str],
    start: usize,
) -> Result<(EffectDecl, usize), ParseError> {
    let trimmed = strip_line_comment(lines[start]).trim_end().trim();
    let line_no = start + 1;
    let effect_header = trimmed["effect ".len()..].trim();
    if effect_header.is_empty() {
        return Err(ParseError {
            line: line_no,
            col: 1,
            message: "effect declaration requires a name".to_string(),
        });
    }

    let mut header_parts = effect_header.split_whitespace();
    let effect_name = header_parts
        .next()
        .expect("effect header should have first token")
        .to_string();
    if !is_camel_case_identifier(&effect_name) {
        return Err(ParseError {
            line: line_no,
            col: 1,
            message: "effect declaration name must be CamelCase".to_string(),
        });
    }

    let mut type_params = Vec::new();
    let mut seen_type_params = HashSet::new();
    for param in header_parts {
        if !is_type_parameter_identifier(param) {
            return Err(ParseError {
                line: line_no,
                col: 1,
                message: "effect type parameter must start with a lowercase letter or `_`"
                    .to_string(),
            });
        }
        if !seen_type_params.insert(param) {
            return Err(ParseError {
                line: line_no,
                col: 1,
                message: format!("duplicate effect type parameter `{}`", param),
            });
        }
        type_params.push(param.to_string());
    }

    let mut members = Vec::new();
    let mut index = start + 1;
    while index < lines.len() {
        let member_line = lines[index];
        let member_trimmed = strip_line_comment(member_line).trim_end();
        let member_trimmed_str = member_trimmed.trim();
        if member_trimmed_str.is_empty() || member_trimmed_str.starts_with('#') {
            index += 1;
            continue;
        }
        if !is_indented(member_line) {
            break;
        }
        let Some((name_raw, ty_raw)) = member_trimmed_str.split_once(':') else {
            return Err(ParseError {
                line: index + 1,
                col: 1,
                message: "invalid effect member signature: expected `name: Type`".to_string(),
            });
        };
        let name = name_raw.trim();
        let ty = ty_raw.trim();
        if !is_non_reserved_identifier(name) {
            return Err(ParseError {
                line: index + 1,
                col: 1,
                message: "effect member name must be a non-reserved identifier".to_string(),
            });
        }
        if ty.is_empty() {
            return Err(ParseError {
                line: index + 1,
                col: 1,
                message: "effect member type must not be empty".to_string(),
            });
        }
        let member_indent = member_line.len() - member_line.trim_start().len();
        let member_col = member_indent + 1;
        members.push(EffectMember {
            name: name.to_string(),
            type_annotation: ty.to_string(),
            span: Span::point(index + 1, member_col),
        });
        index += 1;
    }

    Ok((
        EffectDecl {
            name: effect_name,
            type_params,
            members,
            span: Span::point(line_no, 1),
        },
        index,
    ))
}

fn parse_declaration_header(
    lines: &[&str],
    start: usize,
) -> Result<(DeclarationParts, usize), ParseError> {
    let line = strip_line_comment(lines[start]).trim_end();
    let line_no = start + 1;
    let mut annotated_name: Option<&str> = None;
    let mut type_annotation = None;
    let decl_line = line_no;
    let mut index = start;

    if let Some((name, ty)) = split_top_level_type(line) {
        if name.is_empty() || ty.is_empty() {
            return Err(ParseError {
                line: line_no,
                col: 1,
                message: "invalid type annotation".to_string(),
            });
        }
        annotated_name = Some(name);
        type_annotation = Some(ty.to_string());
        index += 1;
        index = skip_blank_and_comment_lines(lines, index);
        if index >= lines.len() {
            return Err(ParseError {
                line: decl_line,
                col: 1,
                message: "missing declaration body after type annotation".to_string(),
            });
        }
    }

    let body_line = strip_line_comment(lines[index]).trim_end();
    let (name, params, mut body) =
        split_top_level_definition(body_line).ok_or_else(|| ParseError {
            line: index + 1,
            col: 1,
            message: "expected top-level definition (`name ... = ...`)".to_string(),
        })?;
    if !is_lowercase_start_identifier(name) {
        return Err(ParseError {
            line: index + 1,
            col: 1,
            message: "declaration name must start with a lowercase letter".to_string(),
        });
    }
    if is_reserved_keyword(name) {
        return Err(ParseError {
            line: index + 1,
            col: 1,
            message: format!("`{name}` is a reserved keyword"),
        });
    }
    if let Some(annotated_name) = annotated_name
        && annotated_name != name
    {
        return Err(ParseError {
            line: index + 1,
            col: 1,
            message: format!(
                "type annotation name `{}` does not match definition name `{}`",
                annotated_name, name
            ),
        });
    }

    // col: byte offset of the declaration name on the definition line (1-indexed).
    let def_raw_line = lines[index];
    let def_indent = def_raw_line.len() - def_raw_line.trim_start().len();
    let def_col = def_indent + 1;

    let next_index = collect_indented_body(lines, index + 1, &mut body);
    Ok((
        DeclarationParts {
            name: name.to_string(),
            type_annotation,
            params,
            body,
            line: decl_line,
            definition_line: index + 1,
            col: def_col,
        },
        next_index,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ImportKind, Stmt, TypeDeclaration};
    use crate::parser::parse_module;
    use crate::parser_test_support::read_example;

    fn read_parser_fixture(name: &str) -> String {
        let mut path = crate::path_util::workspace_root();
        path.push("crates");
        path.push("goby-core");
        path.push("tests");
        path.push("fixtures");
        path.push("parser");
        path.push(name);
        std::fs::read_to_string(path).expect("parser fixture should exist")
    }

    fn parse_single_declaration(source: &str) -> DeclarationParts {
        let module = parse_module(source).expect("source should parse");
        assert_eq!(module.declarations.len(), 1);
        let decl = &module.declarations[0];
        DeclarationParts {
            name: decl.name.clone(),
            type_annotation: decl.type_annotation.clone(),
            params: decl.params.clone(),
            body: decl.body.clone(),
            line: decl.line,
            definition_line: decl.line,
            col: decl.col,
        }
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
            "case", "as", "can", "using", "fn", "True", "False",
        ];
        for name in reserved {
            let source = format!("{name} : Int -> Int\n{name} x = x\n");
            parse_module(&source).expect_err("reserved declaration name should be rejected");
        }
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
                type_params: Vec::new(),
                variants: vec![
                    crate::ast::UnionVariant {
                        ctor: "Activated".to_string(),
                        args: Vec::new(),
                    },
                    crate::ast::UnionVariant {
                        ctor: "Deactivated".to_string(),
                        args: Vec::new(),
                    },
                ],
            }
        );
        assert_eq!(
            module.type_declarations[2],
            TypeDeclaration::Record {
                name: "User".to_string(),
                type_params: Vec::new(),
                constructor: "User".to_string(),
                fields: vec![
                    crate::ast::RecordField {
                        name: "id".to_string(),
                        type_annotation: "UserID".to_string(),
                    },
                    crate::ast::RecordField {
                        name: "name".to_string(),
                        type_annotation: "String".to_string(),
                    },
                    crate::ast::RecordField {
                        name: "status".to_string(),
                        type_annotation: "UserStatus".to_string(),
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
        let decl = parse_single_declaration("f : Int -> Int\nf x = 42\n");
        assert_eq!(decl.name, "f");
        assert_eq!(decl.params, vec!["x".to_string()]);
    }

    #[test]
    fn allows_comment_between_annotation_and_definition() {
        let declaration = parse_single_declaration(
            "main : Unit -> Unit can Print\n# comment\n\nmain = print \"ok\"\n",
        );
        assert_eq!(declaration.name, "main");
    }

    #[test]
    fn allows_line_end_comments_in_type_and_definition() {
        let declaration = parse_single_declaration(
            "main : Unit -> Unit can Print # type note\nmain = print \"ok\" # body note\n",
        );
        assert_eq!(declaration.name, "main");
        assert_eq!(
            declaration.type_annotation.as_deref(),
            Some("Unit -> Unit can Print")
        );
    }

    #[test]
    fn treats_shebang_as_comment_line() {
        let declaration = parse_single_declaration(
            "#!/usr/bin/env goby\nmain : Unit -> Unit can Print\nmain = print \"ok\"\n",
        );
        assert_eq!(declaration.name, "main");
    }

    #[test]
    fn allows_mixed_tabs_and_spaces_in_same_block() {
        let source = read_parser_fixture("mixed_indent.gb");
        let declaration = parse_single_declaration(&source);
        assert!(declaration.body.contains("greeting = \"Hello\""));
        assert!(declaration.body.contains("println greeting"));
    }

    #[test]
    fn preserves_parsed_body_for_type_example_main() {
        let source = read_example("type.gb");
        let module = parse_module(&source).expect("type.gb should parse");
        let stmts = module.declarations[0]
            .parsed_body
            .as_ref()
            .expect("main body should parse");
        assert_eq!(stmts.len(), 2);
        assert!(matches!(stmts[0], Stmt::Binding { .. }));
    }

    // --- Span population tests ---

    #[test]
    fn declaration_col_is_1_for_top_level_definition() {
        // Simple top-level definition starts at column 1.
        let source = "main = 1\n";
        let module = parse_module(source).expect("source should parse");
        assert_eq!(module.declarations[0].col, 1);
    }

    #[test]
    fn declaration_col_is_1_for_annotated_definition() {
        // With type annotation, col should point to definition line which starts at col 1.
        let source = "main : Unit -> Unit\nmain = 1\n";
        let module = parse_module(source).expect("source should parse");
        let decl = &module.declarations[0];
        assert_eq!(decl.line, 1); // annotation line
        assert_eq!(decl.col, 1); // definition is also at col 1
    }

    #[test]
    fn effect_decl_span_points_to_header_line() {
        // `effect Print` on line 1 → span.line == 1, span.col == 1.
        let source = "effect Print\n  log: String -> Unit\nmain = 1\n";
        let module = parse_module(source).expect("source should parse");
        let effect = &module.effect_declarations[0];
        assert_eq!(effect.span.line, 1);
        assert_eq!(effect.span.col, 1);
    }

    #[test]
    fn effect_member_span_points_to_member_line() {
        // Member `log` is on line 2, indented 2 spaces → col == 3.
        let source = "effect Print\n  log: String -> Unit\nmain = 1\n";
        let module = parse_module(source).expect("source should parse");
        let member = &module.effect_declarations[0].members[0];
        assert_eq!(member.span.line, 2);
        assert_eq!(member.span.col, 3); // 2 spaces indent + 1
    }

    #[test]
    fn effect_decl_span_for_second_effect_on_line_three() {
        // Effect starting on line 3 → span.line == 3.
        let source = "main = 1\n\neffect Log\n  log: String -> Unit\n";
        let module = parse_module(source).expect("source should parse");
        let effect = &module.effect_declarations[0];
        assert_eq!(effect.span.line, 3);
    }

    // --- Import span population tests ---

    #[test]
    fn plain_import_module_path_span_covers_path_token() {
        // "import goby/string" — "goby/string" starts at col 8, length 11 → end col 18
        let source = "import goby/string\nmain = 1\n";
        let module = parse_module(source).expect("should parse");
        let import = &module.imports[0];
        assert_eq!(import.module_path_span, Some(Span::new(1, 8, 1, 18)));
        assert_eq!(import.kind_span, Some(ImportKindSpan::Plain));
    }

    #[test]
    fn selective_import_symbol_spans_point_to_each_symbol() {
        // "import goby/list ( each, map )"
        //  123456789012345678901234567890
        //         8       18   23   28
        // "goby/list" → col 8..16
        // "each" → col 20..23, "map" → col 26..28
        let source = "import goby/list ( each, map )\nmain = 1\n";
        let module = parse_module(source).expect("should parse");
        let import = &module.imports[0];
        assert_eq!(import.module_path_span, Some(Span::new(1, 8, 1, 16)));
        let Some(ImportKindSpan::Selective(spans)) = &import.kind_span else {
            panic!("expected Selective kind span");
        };
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0], Span::new(1, 20, 1, 23)); // "each"
        assert_eq!(spans[1], Span::new(1, 26, 1, 28)); // "map"
    }

    #[test]
    fn alias_import_kind_span_covers_alias_token() {
        // "import goby/list as l"
        //  12345678901234567890123
        // "goby/list" → col 8..16, "l" → col 21..21
        let source = "import goby/list as l\nmain = 1\n";
        let module = parse_module(source).expect("should parse");
        let import = &module.imports[0];
        assert_eq!(import.module_path_span, Some(Span::new(1, 8, 1, 16)));
        assert_eq!(
            import.kind_span,
            Some(ImportKindSpan::Alias(Span::new(1, 21, 1, 21)))
        );
    }

    #[test]
    fn import_span_line_number_reflects_source_line() {
        // Import on line 3
        let source = "main = 1\n\nimport goby/list ( get )\n";
        let module = parse_module(source).expect("should parse");
        let import = &module.imports[0];
        assert_eq!(import.module_path_span.unwrap().line, 3);
        let Some(ImportKindSpan::Selective(spans)) = &import.kind_span else {
            panic!("expected Selective kind span");
        };
        assert_eq!(spans[0].line, 3);
    }

    #[test]
    fn parses_generic_union_with_args() {
        let source = "type Maybe a = Just(a) | Nothing\nmain = 1\n";
        let module = parse_module(source).expect("generic union should parse");
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Union {
                name: "Maybe".to_string(),
                type_params: vec!["a".to_string()],
                variants: vec![
                    crate::ast::UnionVariant {
                        ctor: "Just".to_string(),
                        args: vec!["a".to_string()],
                    },
                    crate::ast::UnionVariant {
                        ctor: "Nothing".to_string(),
                        args: Vec::new(),
                    },
                ],
            }
        );
    }

    #[test]
    fn parses_generic_record() {
        let source = "type Box a = Box(value: a)\nmain = 1\n";
        let module = parse_module(source).expect("generic record should parse");
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Record {
                name: "Box".to_string(),
                type_params: vec!["a".to_string()],
                constructor: "Box".to_string(),
                fields: vec![crate::ast::RecordField {
                    name: "value".to_string(),
                    type_annotation: "a".to_string(),
                }],
            }
        );
    }

    #[test]
    fn parses_multi_arg_variant_with_nested_type() {
        let source = "type Tree a = Node(a, Tree a, Tree a) | Leaf\nmain = 1\n";
        let module = parse_module(source).expect("tree union should parse");
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Union {
                name: "Tree".to_string(),
                type_params: vec!["a".to_string()],
                variants: vec![
                    crate::ast::UnionVariant {
                        ctor: "Node".to_string(),
                        args: vec![
                            "a".to_string(),
                            "Tree a".to_string(),
                            "Tree a".to_string(),
                        ],
                    },
                    crate::ast::UnionVariant {
                        ctor: "Leaf".to_string(),
                        args: Vec::new(),
                    },
                ],
            }
        );
    }

    #[test]
    fn parses_single_variant_generic_union_without_pipe() {
        let source = "type Box a = Box(a)\nmain = 1\n";
        let module = parse_module(source).expect("single-variant union should parse");
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Union {
                name: "Box".to_string(),
                type_params: vec!["a".to_string()],
                variants: vec![crate::ast::UnionVariant {
                    ctor: "Box".to_string(),
                    args: vec!["a".to_string()],
                }],
            }
        );
    }

    #[test]
    fn parses_alias_with_nested_paren_args() {
        let source = "type Wrap = Maybe (A | B)\nmain = 1\n";
        let module = parse_module(source).expect("alias with nested paren args should parse");
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Alias {
                name: "Wrap".to_string(),
                target: "Maybe (A | B)".to_string(),
            }
        );
    }

    #[test]
    fn rejects_uppercase_type_parameter() {
        let source = "type Foo A = Bar(A)\nmain = 1\n";
        assert!(
            parse_module(source).is_err(),
            "uppercase type-parameter must be rejected"
        );
    }

    #[test]
    fn rejects_empty_arg_variant_in_union_position() {
        // `Bar()` is not a legal union variant; the pipe forces union
        // parsing, which then fails. The whole declaration is rejected.
        let source = "type Foo = Bar() | Baz\nmain = 1\n";
        assert!(
            parse_module(source).is_err(),
            "empty-arg union variant must be rejected"
        );
    }

    #[test]
    fn empty_record_constructor_remains_record() {
        // No pipe, single `Bar()` candidate: the union path rejects the
        // empty positional list, but the record fallback accepts an
        // empty field list, matching the existing zero-field record
        // grammar.
        let source = "type Foo = Bar()\nmain = 1\n";
        let module = parse_module(source).expect("zero-field record should parse");
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Record {
                name: "Foo".to_string(),
                type_params: Vec::new(),
                constructor: "Bar".to_string(),
                fields: Vec::new(),
            }
        );
    }

    #[test]
    fn rejects_top_level_colon_in_union_variant_args() {
        // `Bad(a: Int)` looks like record-style fields inside a union
        // variant; reject as a union variant candidate.
        let source = "type Foo = Bad(a: Int) | Other\nmain = 1\n";
        assert!(
            parse_module(source).is_err(),
            "union variants must not carry record-style `field: type` args"
        );
    }

    #[test]
    fn rejects_unmatched_open_paren_in_variant() {
        // Unbalanced parens in a variant candidate must not silently
        // succeed; the declaration is rejected.
        let source = "type Foo = Just((a) | Other\nmain = 1\n";
        assert!(
            parse_module(source).is_err(),
            "unmatched `(` in variant args must be rejected"
        );
    }

    #[test]
    fn rejects_duplicate_type_parameter_on_union() {
        let source = "type Foo a a = Bar(a)\nmain = 1\n";
        assert!(
            parse_module(source).is_err(),
            "duplicate type parameters must be rejected"
        );
    }

    #[test]
    fn rejects_duplicate_type_parameter_on_record() {
        let source = "type Pair a a = Pair(fst: a, snd: a)\nmain = 1\n";
        assert!(
            parse_module(source).is_err(),
            "duplicate type parameters must be rejected on records"
        );
    }
}

fn parse_import_line(line: &str, line_no: usize) -> Option<ImportDecl> {
    let rest = line.strip_prefix("import ")?.trim();
    if rest.is_empty() {
        return None;
    }

    // "import " prefix is 7 bytes; top-level lines start at col 1 → module path starts at col 8.
    const PREFIX_LEN: usize = "import ".len(); // = 7
    let mp_col_start = PREFIX_LEN + 1; // = 8

    if let Some(open_idx) = rest.find('(') {
        if !rest.ends_with(')') {
            return None;
        }
        let module_path = rest[..open_idx].trim();
        if !is_module_path(module_path) {
            return None;
        }
        let module_path_span = Some(Span::new(
            line_no,
            mp_col_start,
            line_no,
            mp_col_start + module_path.len() - 1,
        ));
        let inner_raw = &rest[open_idx + 1..rest.len() - 1];
        let inner = inner_raw.trim();
        if inner.is_empty() {
            return None;
        }
        // Byte offset of `inner` start within `rest`: open_idx + 1 + leading whitespace.
        let inner_offset_in_rest = open_idx + 1 + (inner_raw.len() - inner_raw.trim_start().len());
        let names = split_top_level_commas(inner);
        let mut symbols = Vec::new();
        let mut symbol_spans = Vec::new();
        // Walk inner to find byte positions of each symbol.
        let mut search_offset = 0usize;
        for name in &names {
            let name = name.trim();
            if !is_non_reserved_identifier(name) {
                return None;
            }
            // Find name's start within inner starting from search_offset.
            let rel = inner[search_offset..].find(name)?;
            let sym_offset_in_inner = search_offset + rel;
            // col is 1-indexed; rest starts at col PREFIX_LEN+1, inner at col PREFIX_LEN+1+inner_offset_in_rest
            let sym_col = PREFIX_LEN + 1 + inner_offset_in_rest + sym_offset_in_inner;
            symbol_spans.push(Span::new(
                line_no,
                sym_col,
                line_no,
                sym_col + name.len() - 1,
            ));
            search_offset = sym_offset_in_inner + name.len();
            symbols.push(name.to_string());
        }
        return Some(ImportDecl {
            module_path: module_path.to_string(),
            kind: ImportKind::Selective(symbols),
            module_path_span,
            kind_span: Some(ImportKindSpan::Selective(symbol_spans)),
        });
    }

    if let Some((module_path_raw, alias_raw)) = rest.split_once(" as ") {
        let module_path = module_path_raw.trim();
        let alias = alias_raw.trim();
        if !is_module_path(module_path) || !is_non_reserved_identifier(alias) {
            return None;
        }
        let module_path_span = Some(Span::new(
            line_no,
            mp_col_start,
            line_no,
            mp_col_start + module_path.len() - 1,
        ));
        // alias_raw is a subslice of rest (from split_once), so pointer arithmetic is safe.
        let alias_raw_offset = alias_raw.as_ptr() as usize - rest.as_ptr() as usize;
        let alias_leading_ws = alias_raw.len() - alias_raw.trim_start().len();
        let alias_col = PREFIX_LEN + 1 + alias_raw_offset + alias_leading_ws;
        let kind_span = Some(ImportKindSpan::Alias(Span::new(
            line_no,
            alias_col,
            line_no,
            alias_col + alias.len() - 1,
        )));
        return Some(ImportDecl {
            module_path: module_path.to_string(),
            kind: ImportKind::Alias(alias.to_string()),
            module_path_span,
            kind_span,
        });
    }

    if !is_module_path(rest) {
        return None;
    }
    let module_path_span = Some(Span::new(
        line_no,
        mp_col_start,
        line_no,
        mp_col_start + rest.len() - 1,
    ));
    Some(ImportDecl {
        module_path: rest.to_string(),
        kind: ImportKind::Plain,
        module_path_span,
        kind_span: Some(ImportKindSpan::Plain),
    })
}

fn parse_embed_line(line: &str) -> Result<(String, String), String> {
    let rest = line
        .strip_prefix("@embed")
        .map(str::trim_start)
        .ok_or_else(|| "invalid @embed declaration".to_string())?;
    if rest.is_empty() {
        return Err(
            "invalid @embed declaration: expected `@embed <EffectName> <HandlerName>`".to_string(),
        );
    }

    if rest.starts_with("effect ") {
        return Err(
            "legacy `@embed effect <EffectName>` is no longer supported; use `@embed <EffectName> <HandlerName>`".to_string(),
        );
    }

    let mut tokens = rest.split_whitespace();
    let Some(effect_name) = tokens.next() else {
        return Err(
            "invalid @embed declaration: expected `@embed <EffectName> <HandlerName>`".to_string(),
        );
    };
    let Some(handler_name) = tokens.next() else {
        return Err("embedded handler name is missing".to_string());
    };
    if tokens.next().is_some() {
        return Err(
            "invalid @embed declaration: expected `@embed <EffectName> <HandlerName>`".to_string(),
        );
    }

    if !is_identifier(effect_name) {
        return Err("invalid embedded effect name".to_string());
    }
    if !is_identifier(handler_name) {
        return Err("invalid embedded handler name".to_string());
    }
    Ok((effect_name.to_string(), handler_name.to_string()))
}

fn parse_type_declaration_line(line: &str) -> Option<TypeDeclaration> {
    let rest = line.strip_prefix("type ")?.trim();
    let (lhs, rhs) = rest.split_once('=')?;
    let lhs = lhs.trim();
    let rhs = rhs.trim();
    if rhs.is_empty() {
        return None;
    }
    if !has_balanced_delimiters(rhs) {
        return None;
    }
    let (name, type_params) = split_type_header(lhs)?;

    // RHS is parsed as a list of pipe-separated candidates. When two or
    // more candidates exist, every one must parse as a union variant
    // (`Ctor` or `Ctor(t, ...)` with no top-level `:`). When there is
    // no pipe, a *single* candidate is only treated as a union when it
    // carries an explicit `(...)` payload — otherwise the bare
    // `type Name = OtherType` form keeps its long-standing alias
    // meaning and never becomes a one-variant nullary union.
    let candidates = split_top_level_pipes(rhs).unwrap_or_else(|| vec![rhs]);
    let pipe_present = candidates.len() > 1;

    if pipe_present {
        let parsed_variants: Option<Vec<UnionVariant>> = candidates
            .iter()
            .map(|c| parse_union_variant_candidate(c.trim()))
            .collect();
        if let Some(variants) = parsed_variants {
            if !variants.is_empty() {
                return Some(TypeDeclaration::Union {
                    name,
                    type_params,
                    variants,
                });
            }
        }
        // Pipe present but at least one candidate is not a valid union
        // variant. Do not silently fall through to alias — the user
        // intended a union and the diagnostic should reflect that.
        return None;
    }

    // Single-candidate union: only accepted when the constructor body
    // is explicit (`Ctor(...)`), so `type Wrapped = Maybe Int` and
    // `type UserID = String` keep their alias meaning.
    if rhs.contains('(') {
        if let Some(variant) = parse_union_variant_candidate(rhs) {
            if !variant.args.is_empty() {
                return Some(TypeDeclaration::Union {
                    name,
                    type_params,
                    variants: vec![variant],
                });
            }
        }
    }

    if let Some((constructor, inner)) = split_record_constructor_shape(rhs) {
        if !is_camel_case_identifier(constructor) {
            return None;
        }
        let fields = if inner.is_empty() {
            Vec::new()
        } else {
            let parts = split_top_level_commas(inner);
            let parsed_fields: Option<Vec<_>> = parts
                .iter()
                .map(|part| parse_record_field(part.trim()))
                .collect();
            parsed_fields?
        };
        return Some(TypeDeclaration::Record {
            name,
            type_params,
            constructor: constructor.to_string(),
            fields,
        });
    }

    if !type_params.is_empty() {
        // Type aliases do not (yet) support type parameters; reject
        // `type StrMap a = ...` rather than silently producing an
        // alias whose `type_params` cannot be carried.
        return None;
    }
    Some(TypeDeclaration::Alias {
        name,
        target: rhs.to_string(),
    })
}

/// Parse a single union-variant candidate: bare `Ctor` (nullary) or
/// `Ctor(t1, t2, ...)` with one or more positional argument type
/// annotations. A constructor body containing a top-level `:` is
/// rejected (it is the record-style `field: type` form).
fn parse_union_variant_candidate(src: &str) -> Option<UnionVariant> {
    let src = src.trim();
    if src.is_empty() {
        return None;
    }
    if let Some(open_idx) = src.find('(') {
        if !src.ends_with(')') || !has_balanced_delimiters(src) {
            return None;
        }
        let ctor = &src[..open_idx];
        // Constructor application requires the `(` immediately after the
        // ctor name with no whitespace, so `Maybe (A | B)` is not a
        // single-arg variant `Maybe("A | B")` — it stays a type alias.
        if open_idx != ctor.len() || !is_camel_case_identifier(ctor) {
            return None;
        }
        let inner = src[open_idx + 1..src.len() - 1].trim();
        if inner.is_empty() {
            // `Ctor()` is not a valid union variant; an empty positional
            // arg list has no use case under the spec.
            return None;
        }
        if contains_top_level_colon(inner) {
            return None;
        }
        let parts = split_top_level_commas(inner);
        let mut args = Vec::with_capacity(parts.len());
        for part in parts {
            let trimmed = part.trim();
            if trimmed.is_empty() {
                return None;
            }
            args.push(trimmed.to_string());
        }
        Some(UnionVariant {
            ctor: ctor.to_string(),
            args,
        })
    } else if is_camel_case_identifier(src) {
        Some(UnionVariant {
            ctor: src.to_string(),
            args: Vec::new(),
        })
    } else {
        None
    }
}
