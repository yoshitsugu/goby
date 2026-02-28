use crate::str_util::split_top_level_commas;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub arguments: Vec<String>,
    pub result: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeExpr {
    Name(String),
    Tuple(Vec<TypeExpr>),
    Apply {
        head: Box<TypeExpr>,
        args: Vec<TypeExpr>,
    },
    Function {
        arguments: Vec<TypeExpr>,
        result: Box<TypeExpr>,
    },
}

pub fn parse_function_type(annotation: &str) -> Option<FunctionType> {
    let base = strip_effect(annotation);
    let parts = split_top_level_function_arrows(base)?;

    let result = parts.last()?.clone();
    let arguments = parts[..parts.len() - 1].to_vec();
    Some(FunctionType { arguments, result })
}

fn split_top_level_function_arrows(annotation: &str) -> Option<Vec<String>> {
    let mut parts = Vec::new();
    let mut segment_start = 0usize;
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escaped = false;
    let bytes = annotation.as_bytes();
    let mut i = 0usize;

    while i < bytes.len() {
        let byte = bytes[i];

        if in_string {
            if escaped {
                escaped = false;
                i += 1;
                continue;
            }
            if byte == b'\\' {
                escaped = true;
                i += 1;
                continue;
            }
            if byte == b'"' {
                in_string = false;
            }
            i += 1;
            continue;
        }

        match byte {
            b'"' => in_string = true,
            b'(' => depth += 1,
            b')' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
            }
            b'-' if depth == 0 && i + 1 < bytes.len() && bytes[i + 1] == b'>' => {
                let part = annotation[segment_start..i].trim();
                if part.is_empty() {
                    return None;
                }
                parts.push(part.to_string());
                i += 1;
                segment_start = i + 1;
            }
            _ => {}
        }

        i += 1;
    }

    if in_string || depth != 0 {
        return None;
    }

    let tail = annotation[segment_start..].trim();
    if tail.is_empty() {
        return None;
    }
    parts.push(tail.to_string());

    if parts.len() < 2 {
        return None;
    }

    Some(parts)
}

pub fn strip_effect(annotation: &str) -> &str {
    match find_can_keyword_index(annotation) {
        Some(idx) => annotation[..idx].trim_end(),
        None => annotation.trim(),
    }
}

fn find_can_keyword_index(annotation: &str) -> Option<usize> {
    for (idx, _) in annotation.char_indices() {
        let rest = &annotation[idx..];
        if !rest.starts_with("can") {
            continue;
        }

        let has_left_whitespace = annotation[..idx]
            .chars()
            .last()
            .is_some_and(char::is_whitespace);
        if !has_left_whitespace {
            continue;
        }

        let has_right_whitespace = annotation[idx + 3..]
            .chars()
            .next()
            .is_none_or(char::is_whitespace);
        if !has_right_whitespace {
            continue;
        }

        return Some(idx);
    }

    None
}

pub fn parse_type_expr(annotation: &str) -> Option<TypeExpr> {
    let annotation = annotation.trim();
    if annotation.is_empty() {
        return None;
    }

    if let Some(function) = parse_type_function_expr(annotation) {
        return Some(function);
    }

    parse_non_function_type_expr(annotation)
}

fn parse_type_function_expr(annotation: &str) -> Option<TypeExpr> {
    let ft = parse_function_type(annotation)?;
    let arguments: Option<Vec<TypeExpr>> = ft
        .arguments
        .iter()
        .map(|arg| parse_type_expr(arg))
        .collect();
    let result = parse_type_expr(&ft.result)?;
    Some(TypeExpr::Function {
        arguments: arguments?,
        result: Box::new(result),
    })
}

fn parse_non_function_type_expr(annotation: &str) -> Option<TypeExpr> {
    if let Some(inner) = trim_outer_parens(annotation) {
        let tuple_parts = split_top_level_commas(inner);
        if tuple_parts.len() > 1 {
            let items: Option<Vec<TypeExpr>> =
                tuple_parts.iter().map(|part| parse_type_expr(part)).collect();
            return Some(TypeExpr::Tuple(items?));
        }
        if tuple_parts.len() == 1 {
            return parse_type_expr(tuple_parts[0]);
        }
        return None;
    }

    let parts = split_top_level_spaces(annotation);
    if parts.is_empty() {
        return None;
    }
    if parts.len() == 1 {
        return is_identifier(parts[0]).then(|| TypeExpr::Name(parts[0].to_string()));
    }

    let head = parse_non_function_type_expr(parts[0])?;
    let args: Option<Vec<TypeExpr>> = parts[1..]
        .iter()
        .map(|part| parse_non_function_type_expr(part))
        .collect();
    Some(TypeExpr::Apply {
        head: Box::new(head),
        args: args?,
    })
}

fn split_top_level_spaces(s: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escaped = false;
    let mut start: Option<usize> = None;

    for (idx, ch) in s.char_indices() {
        if in_string {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == '"' {
                in_string = false;
            }
            continue;
        }

        match ch {
            '"' => in_string = true,
            '(' => {
                if start.is_none() {
                    start = Some(idx);
                }
                depth += 1;
            }
            ')' => {
                if start.is_none() {
                    start = Some(idx);
                }
                depth = depth.saturating_sub(1);
            }
            c if c.is_whitespace() && depth == 0 => {
                if let Some(st) = start.take() {
                    parts.push(&s[st..idx]);
                }
            }
            _ => {
                if start.is_none() {
                    start = Some(idx);
                }
            }
        }
    }

    if let Some(st) = start {
        parts.push(&s[st..]);
    }
    parts
}

fn trim_outer_parens(s: &str) -> Option<&str> {
    let s = s.trim();
    if !(s.starts_with('(') && s.ends_with(')')) {
        return None;
    }

    let mut depth = 0usize;
    let mut in_string = false;
    let mut escaped = false;
    let bytes = s.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        let byte = bytes[i];
        if in_string {
            if escaped {
                escaped = false;
                i += 1;
                continue;
            }
            if byte == b'\\' {
                escaped = true;
                i += 1;
                continue;
            }
            if byte == b'"' {
                in_string = false;
            }
            i += 1;
            continue;
        }

        match byte {
            b'"' => in_string = true,
            b'(' => depth += 1,
            b')' => {
                if depth == 0 {
                    return None;
                }
                depth -= 1;
                if depth == 0 && i != bytes.len() - 1 {
                    return None;
                }
            }
            _ => {}
        }
        i += 1;
    }

    if depth != 0 || in_string {
        return None;
    }
    Some(&s[1..s.len() - 1])
}

fn is_identifier(s: &str) -> bool {
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
    use super::*;

    #[test]
    fn parses_function_type_with_effect_annotation() {
        let ty = parse_function_type("Unit -> Unit can Print").expect("should parse");
        assert_eq!(ty.arguments, vec!["Unit"]);
        assert_eq!(ty.result, "Unit");
    }

    #[test]
    fn parses_function_type_with_tab_separated_effect_annotation() {
        let ty = parse_function_type("Unit -> Unit can\tPrint").expect("should parse");
        assert_eq!(ty.arguments, vec!["Unit"]);
        assert_eq!(ty.result, "Unit");
    }

    #[test]
    fn rejects_malformed_function_type_with_empty_segment() {
        assert_eq!(parse_function_type("Int -> -> Int"), None);
        assert_eq!(parse_function_type("-> Int"), None);
        assert_eq!(parse_function_type("Int ->"), None);
    }

    #[test]
    fn parses_function_type_with_parenthesized_function_argument() {
        let ty = parse_function_type("(Int -> Int) -> Unit").expect("should parse");
        assert_eq!(ty.arguments, vec!["(Int -> Int)"]);
        assert_eq!(ty.result, "Unit");
    }

    #[test]
    fn rejects_unbalanced_parentheses_in_function_type() {
        assert_eq!(parse_function_type("(Int -> Int -> Unit"), None);
        assert_eq!(parse_function_type("Int -> Int) -> Unit"), None);
    }

    #[test]
    fn parses_haskell_style_list_application() {
        assert_eq!(
            parse_type_expr("List Int"),
            Some(TypeExpr::Apply {
                head: Box::new(TypeExpr::Name("List".to_string())),
                args: vec![TypeExpr::Name("Int".to_string())],
            })
        );
    }

    #[test]
    fn parses_multi_parameter_application() {
        assert_eq!(
            parse_type_expr("TypeX a b"),
            Some(TypeExpr::Apply {
                head: Box::new(TypeExpr::Name("TypeX".to_string())),
                args: vec![
                    TypeExpr::Name("a".to_string()),
                    TypeExpr::Name("b".to_string()),
                ],
            })
        );
    }

    #[test]
    fn parses_nested_application_with_parentheses() {
        assert_eq!(
            parse_type_expr("TypeX (TypeY a b) c"),
            Some(TypeExpr::Apply {
                head: Box::new(TypeExpr::Name("TypeX".to_string())),
                args: vec![
                    TypeExpr::Apply {
                        head: Box::new(TypeExpr::Name("TypeY".to_string())),
                        args: vec![
                            TypeExpr::Name("a".to_string()),
                            TypeExpr::Name("b".to_string()),
                        ],
                    },
                    TypeExpr::Name("c".to_string()),
                ],
            })
        );
    }
}
