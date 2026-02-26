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

        if line.starts_with(' ') || line.starts_with('\t') {
            return Err(ParseError {
                line: i + 1,
                message: "unexpected indentation at top level".to_string(),
            });
        }

        let mut type_annotation = None;

        if let Some((name, ty)) = split_top_level_type(line) {
            if name.is_empty() || ty.is_empty() {
                return Err(ParseError {
                    line: i + 1,
                    message: "invalid type annotation".to_string(),
                });
            }
            type_annotation = Some(ty.to_string());
            i += 1;
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

        let mut j = i + 1;
        while j < lines.len() {
            let next = lines[j];
            let next_trimmed = next.trim();
            if next_trimmed.is_empty() {
                body.push('\n');
                j += 1;
                continue;
            }
            if next.starts_with(' ') || next.starts_with('\t') {
                body.push('\n');
                body.push_str(next.trim_end());
                j += 1;
                continue;
            }
            break;
        }

        declarations.push(Declaration {
            name: name.to_string(),
            type_annotation,
            body,
        });

        i = j;
    }

    Ok(Module { declarations })
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

    #[test]
    fn parses_hello_example() {
        let source = read_example("hello.gb");
        let module = parse_module(&source).expect("hello.gb should parse");

        assert_eq!(module.declarations.len(), 1);
        let main_decl = &module.declarations[0];
        assert_eq!(main_decl.name, "main");
        assert_eq!(main_decl.type_annotation.as_deref(), Some("void -> void can Print"));
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
}
