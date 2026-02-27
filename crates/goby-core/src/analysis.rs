use std::collections::HashMap;

const ERR_PRINT_ARG_TYPE: &str =
    "print argument must resolve to String (literal, identifier, or string.concat)";
const ERR_PRINT_ARG_UNBOUND: &str = "print argument must resolve to a String binding";
const ERR_MULTIPLE_PRINTS: &str = "multiple print calls are not yet supported in MVP";

pub fn resolve_print_text(body: &str) -> Result<Option<String>, String> {
    let mut locals: HashMap<String, String> = HashMap::new();
    let mut resolved_print: Option<String> = None;

    for raw_line in body.lines() {
        let line = raw_line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        if let Some((name, expr)) = split_binding(line) {
            update_local_binding(&mut locals, name, expr);
            continue;
        }

        if let Some(expr) = parse_print_call(line) {
            let text = resolve_print_argument(expr, &locals)?;

            if resolved_print.is_some() {
                return Err(ERR_MULTIPLE_PRINTS.to_string());
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
    let rest = line.strip_prefix("print")?;
    let first = rest.chars().next()?;
    if !first.is_whitespace() {
        return None;
    }
    Some(rest.trim())
}

fn update_local_binding(locals: &mut HashMap<String, String>, name: &str, expr: &str) {
    if let Some(text) = eval_string_expr(expr, locals) {
        locals.insert(name.to_string(), text);
    } else {
        locals.remove(name);
    }
}

fn resolve_print_argument(expr: &str, locals: &HashMap<String, String>) -> Result<String, String> {
    let expr = expr.trim();

    if let Some(text) = parse_string_literal(expr) {
        return Ok(text.to_string());
    }

    if is_identifier(expr) {
        return locals
            .get(expr)
            .cloned()
            .ok_or_else(|| ERR_PRINT_ARG_UNBOUND.to_string());
    }

    if let Some(text) = eval_string_expr(expr, locals) {
        return Ok(text);
    }

    Err(ERR_PRINT_ARG_TYPE.to_string())
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

fn eval_string_expr(expr: &str, locals: &HashMap<String, String>) -> Option<String> {
    let expr = expr.trim();

    if let Some(text) = parse_string_literal(expr) {
        return Some(text.to_string());
    }

    if is_identifier(expr) {
        return locals.get(expr).cloned();
    }

    if let Some((left, right)) = parse_string_concat_call(expr) {
        let left_text = eval_string_expr(left, locals)?;
        let right_text = eval_string_expr(right, locals)?;
        return Some(format!("{}{}", left_text, right_text));
    }

    None
}

fn parse_string_concat_call(expr: &str) -> Option<(&str, &str)> {
    let prefix = "string.concat(";
    if !expr.starts_with(prefix) || !expr.ends_with(')') {
        return None;
    }

    let inner = &expr[prefix.len()..expr.len() - 1];
    let (left, right) = split_top_level_comma(inner)?;
    Some((left.trim(), right.trim()))
}

fn split_top_level_comma(s: &str) -> Option<(&str, &str)> {
    let mut depth = 0usize;
    let mut in_string = false;
    let mut prev_escape = false;

    for (idx, ch) in s.char_indices() {
        if in_string {
            if prev_escape {
                prev_escape = false;
                continue;
            }
            if ch == '\\' {
                prev_escape = true;
                continue;
            }
            if ch == '"' {
                in_string = false;
            }
            continue;
        }

        match ch {
            '"' => in_string = true,
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                return Some((&s[..idx], &s[idx + 1..]));
            }
            _ => {}
        }
    }

    None
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
        assert_eq!(err, ERR_PRINT_ARG_TYPE);
    }

    #[test]
    fn rejects_multiple_print_calls_in_mvp() {
        let source = r#"
print "a"
print "b"
"#;
        let err = resolve_print_text(source).expect_err("analysis should fail");
        assert_eq!(err, ERR_MULTIPLE_PRINTS);
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

    #[test]
    fn resolves_print_with_string_concat_and_bindings() {
        let source = r#"
a = "Hello, "
b = "Goby!"
print string.concat(a, b)
"#;
        let result = resolve_print_text(source).expect("analysis should work");
        assert_eq!(result.as_deref(), Some("Hello, Goby!"));
    }

    #[test]
    fn resolves_nested_string_concat() {
        let source = r#"
print string.concat("A", string.concat("B", "C"))
"#;
        let result = resolve_print_text(source).expect("analysis should work");
        assert_eq!(result.as_deref(), Some("ABC"));
    }

    #[test]
    fn resolves_print_with_tab_after_keyword() {
        let source = "print\t\"tab ok\"";
        let result = resolve_print_text(source).expect("analysis should work");
        assert_eq!(result.as_deref(), Some("tab ok"));
    }

    #[test]
    fn ignores_comment_lines_inside_body() {
        let source = r#"
# setup
a = "hello"
# output
print a
"#;
        let result = resolve_print_text(source).expect("analysis should work");
        assert_eq!(result.as_deref(), Some("hello"));
    }
}
