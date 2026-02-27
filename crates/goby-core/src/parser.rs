use crate::ast::{Declaration, Module};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub line: usize,
    pub message: String,
}

pub fn parse_module(source: &str) -> Result<Module, ParseError> {
    let lines: Vec<&str> = source.lines().collect();
    let mut i = 0;
    let mut declarations = Vec::new();

    while i < lines.len() {
        let line = lines[i].trim_end();
        let trimmed = line.trim();

        if trimmed.is_empty() || trimmed.starts_with('#') {
            i += 1;
            continue;
        }

        if is_indented(line) {
            return Err(ParseError {
                line: i + 1,
                message: "unexpected indentation at top level".to_string(),
            });
        }

        let mut annotated_name: Option<&str> = None;
        let mut type_annotation = None;

        if let Some((name, ty)) = split_top_level_type(line) {
            if name.is_empty() || ty.is_empty() {
                return Err(ParseError {
                    line: i + 1,
                    message: "invalid type annotation".to_string(),
                });
            }
            annotated_name = Some(name);
            type_annotation = Some(ty.to_string());
            i += 1;
            i = skip_blank_and_comment_lines(&lines, i);
            if i >= lines.len() {
                return Err(ParseError {
                    line: i,
                    message: "missing declaration body after type annotation".to_string(),
                });
            }
        }

        let body_line = lines[i].trim_end();
        let (name, mut body) = split_top_level_definition(body_line).ok_or_else(|| ParseError {
            line: i + 1,
            message: "expected top-level definition (`name ... = ...`)".to_string(),
        })?;

        if let Some(annotated_name) = annotated_name
            && annotated_name != name
        {
            return Err(ParseError {
                line: i + 1,
                message: format!(
                    "type annotation name `{}` does not match definition name `{}`",
                    annotated_name, name
                ),
            });
        }

        let j = collect_indented_body(lines.as_slice(), i + 1, &mut body);

        declarations.push(Declaration {
            name: name.to_string(),
            type_annotation,
            body,
        });

        i = j;
    }

    Ok(Module { declarations })
}

fn is_indented(line: &str) -> bool {
    line.starts_with(' ') || line.starts_with('\t')
}

fn collect_indented_body(lines: &[&str], mut index: usize, body: &mut String) -> usize {
    while index < lines.len() {
        let next = lines[index];
        let next_trimmed = next.trim();
        if next_trimmed.is_empty() {
            body.push('\n');
            index += 1;
            continue;
        }
        if is_indented(next) {
            body.push('\n');
            body.push_str(next.trim_end());
            index += 1;
            continue;
        }
        break;
    }
    index
}

fn skip_blank_and_comment_lines(lines: &[&str], mut index: usize) -> usize {
    while index < lines.len() {
        let trimmed = lines[index].trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            index += 1;
        } else {
            break;
        }
    }
    index
}

fn split_top_level_type(line: &str) -> Option<(&str, &str)> {
    let idx = line.find(':')?;
    if line[..idx].contains('=') {
        return None;
    }
    Some((line[..idx].trim(), line[idx + 1..].trim()))
}

fn split_top_level_definition(line: &str) -> Option<(&str, String)> {
    let idx = line.find('=')?;
    let lhs = line[..idx].trim();
    let rhs = line[idx + 1..].trim_start();
    let name = lhs.split_whitespace().next()?;
    Some((name, rhs.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;
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

        assert_eq!(module.declarations.len(), 5);
        assert_eq!(module.declarations[0].name, "add");
        assert_eq!(module.declarations[1].name, "add_ten_and_two");
        assert_eq!(module.declarations[2].name, "concatenate");
        assert_eq!(module.declarations[3].name, "print_string");
        assert_eq!(module.declarations[4].name, "a");
    }

    #[test]
    fn rejects_mismatched_annotation_and_definition_names() {
        let source = "foo : Int\nbar = 1\n";
        let err = parse_module(source).expect_err("mismatched names should be rejected");
        assert!(err.message.contains("does not match"));
    }

    #[test]
    fn allows_comment_between_annotation_and_definition() {
        let source = "main : Unit -> Unit can Print\n# comment\n\nmain = print \"ok\"\n";
        let declaration = parse_single_declaration(source);
        assert_eq!(declaration.name, "main");
    }

    #[test]
    fn allows_mixed_tabs_and_spaces_in_same_block() {
        let source =
            "main : Unit -> Unit can Print\nmain =\n  greeting = \"hello\"\n\tprint greeting\n";
        let declaration = parse_single_declaration(source);
        assert!(declaration.body.contains("greeting = \"hello\""));
        assert!(declaration.body.contains("print greeting"));
    }
}
