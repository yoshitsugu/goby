#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub arguments: Vec<String>,
    pub result: String,
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
}
