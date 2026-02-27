use std::collections::HashMap;

pub fn resolve_print_text(body: &str) -> Result<Option<String>, String> {
    let mut locals: HashMap<String, String> = HashMap::new();

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
            if let Some(text) = parse_string_literal(expr) {
                return Ok(Some(text.to_string()));
            }
            if is_identifier(expr) {
                if let Some(text) = locals.get(expr) {
                    return Ok(Some(text.clone()));
                }
                return Err("print argument must resolve to a String binding".to_string());
            }
            return Err("print argument must be a String literal or identifier".to_string());
        }
    }

    Ok(None)
}

fn split_binding(line: &str) -> Option<(&str, &str)> {
    let idx = line.find('=')?;
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
}
