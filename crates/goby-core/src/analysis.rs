use std::collections::HashMap;

pub fn resolve_print_text(body: &str) -> Result<Option<String>, String> {
    let mut locals: HashMap<String, String> = HashMap::new();
    let mut resolved_print: Option<String> = None;

    for raw_line in body.lines() {
        let line = raw_line.trim();
        if line.is_empty() {
            continue;
        }

        if let Some((name, expr)) = split_binding(line) {
            if let Some(text) = parse_string_literal(expr) {
                locals.insert(name.to_string(), text.to_string());
            } else {
                locals.remove(name);
            }
            continue;
        }

        if let Some(expr) = parse_print_call(line) {
            let text = if let Some(text) = parse_string_literal(expr) {
                text.to_string()
            } else if is_identifier(expr) {
                if let Some(text) = locals.get(expr) {
                    text.clone()
                } else {
                    return Err("print argument must resolve to a String binding".to_string());
                }
            } else {
                return Err("print argument must be a String literal or identifier".to_string());
            };

            if resolved_print.is_some() {
                return Err("multiple print calls are not yet supported in MVP".to_string());
            }
            resolved_print = Some(text);
        }
    }

    Ok(resolved_print)
}

fn split_binding(line: &str) -> Option<(&str, &str)> {
    let idx = line.find('=')?;
    if !is_assignment_operator(line, idx) {
        return None;
    }
    let name = line[..idx].trim();
    let expr = line[idx + 1..].trim();
    if is_identifier(name) {
        Some((name, expr))
    } else {
        None
    }
}

fn parse_print_call(line: &str) -> Option<&str> {
    line.strip_prefix("print ").map(str::trim)
}

fn parse_string_literal(expr: &str) -> Option<&str> {
    if !expr.starts_with('"') || !expr.ends_with('"') || expr.len() < 2 {
        return None;
    }
    let inner = &expr[1..expr.len() - 1];
    if inner.contains('"') {
        return None;
    }
    Some(inner)
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

fn is_assignment_operator(line: &str, eq_index: usize) -> bool {
    let bytes = line.as_bytes();

    if eq_index > 0 && bytes[eq_index - 1] == b'=' {
        return false;
    }
    if eq_index + 1 < bytes.len() && bytes[eq_index + 1] == b'=' {
        return false;
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolves_direct_print_literal() {
        let result = resolve_print_text(r#"print "hello""#).expect("analysis should work");
        assert_eq!(result.as_deref(), Some("hello"));
    }

    #[test]
    fn resolves_print_via_local_binding() {
        let source = r#"
greeting = "hello"
print greeting
"#;
        let result = resolve_print_text(source).expect("analysis should work");
        assert_eq!(result.as_deref(), Some("hello"));
    }

    #[test]
    fn rejects_non_string_print_argument() {
        let err = resolve_print_text("print 1").expect_err("analysis should fail");
        assert!(err.contains("String"));
    }

    #[test]
    fn rejects_multiple_print_calls_in_mvp() {
        let source = r#"
print "a"
print "b"
"#;
        let err = resolve_print_text(source).expect_err("analysis should fail");
        assert!(err.contains("multiple print"));
    }

    #[test]
    fn does_not_treat_equality_as_binding() {
        let source = r#"
a == "hello"
print "ok"
"#;
        let result = resolve_print_text(source).expect("analysis should work");
        assert_eq!(result.as_deref(), Some("ok"));
    }
}
