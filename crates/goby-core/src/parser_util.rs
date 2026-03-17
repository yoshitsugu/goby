use crate::ast::RecordField;

pub(crate) fn starts_with_keyword_token(src: &str, keyword: &str) -> bool {
    let Some(rest) = src.strip_prefix(keyword) else {
        return false;
    };
    rest.chars().next().is_some_and(char::is_whitespace)
}

pub(crate) fn is_reserved_keyword(s: &str) -> bool {
    matches!(
        s,
        "@embed"
            | "import"
            | "type"
            | "effect"
            | "handler"
            | "with"
            | "in"
            | "resume"
            | "mut"
            | "if"
            | "else"
            | "case"
            | "as"
            | "can"
            | "using"
            | "True"
            | "False"
    )
}

pub fn is_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

pub(crate) fn is_qualified_name(s: &str) -> bool {
    if let Some((receiver, member)) = s.split_once('.') {
        is_identifier(receiver) && is_identifier(member)
    } else {
        false
    }
}

pub(crate) fn is_non_reserved_identifier(s: &str) -> bool {
    is_identifier(s) && !is_reserved_keyword(s)
}

pub(crate) fn is_camel_case_identifier(s: &str) -> bool {
    if !is_non_reserved_identifier(s) {
        return false;
    }
    s.chars().next().is_some_and(|c| c.is_ascii_uppercase())
}

pub(crate) fn is_lowercase_start_identifier(s: &str) -> bool {
    if !is_identifier(s) {
        return false;
    }
    s.chars()
        .next()
        .is_some_and(|c| c.is_ascii_lowercase() || c == '_')
}

pub(crate) fn is_type_parameter_identifier(s: &str) -> bool {
    is_non_reserved_identifier(s)
        && s.chars()
            .next()
            .is_some_and(|c| c.is_ascii_lowercase() || c == '_')
}

pub(crate) fn is_module_path(s: &str) -> bool {
    if s.is_empty() || s.starts_with('/') || s.ends_with('/') || s.contains("//") {
        return false;
    }
    s.split('/').all(is_identifier)
}

pub(crate) fn strip_line_comment(line: &str) -> &str {
    let bytes = line.as_bytes();
    let mut in_string = false;
    let mut escaped = false;
    let mut i = 0;
    while i < bytes.len() {
        if in_string {
            if escaped {
                escaped = false;
                i += 1;
                continue;
            }
            if bytes[i] == b'\\' {
                escaped = true;
                i += 1;
                continue;
            }
            if bytes[i] == b'"' {
                in_string = false;
            }
            i += 1;
            continue;
        }

        if bytes[i] == b'"' {
            in_string = true;
            i += 1;
            continue;
        }
        if bytes[i] == b'#' {
            return &line[..i];
        }
        i += 1;
    }
    line
}

pub(crate) fn is_indented(line: &str) -> bool {
    line.starts_with(' ') || line.starts_with('\t')
}

pub(crate) fn collect_indented_body(lines: &[&str], mut index: usize, body: &mut String) -> usize {
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

pub(crate) fn skip_blank_and_comment_lines(lines: &[&str], mut index: usize) -> usize {
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

pub(crate) fn split_top_level_type(line: &str) -> Option<(&str, &str)> {
    let idx = line.find(':')?;
    if line[..idx].contains('=') {
        return None;
    }
    Some((line[..idx].trim(), line[idx + 1..].trim()))
}

pub(crate) fn split_top_level_definition(line: &str) -> Option<(&str, Vec<String>, String)> {
    let idx = line
        .char_indices()
        .find_map(|(i, ch)| (ch == '=' && is_assignment_eq(line, i)).then_some(i))?;
    let lhs = line[..idx].trim();
    let rhs = line[idx + 1..].trim_start();
    let mut tokens = lhs.split_whitespace();
    let name = tokens.next()?;
    let mut params = Vec::new();
    for token in tokens {
        if !is_non_reserved_identifier(token) {
            return None;
        }
        params.push(token.to_string());
    }
    Some((name, params, rhs.to_string()))
}

pub(crate) fn split_top_level_pipes(src: &str) -> Option<Vec<&str>> {
    let bytes = src.as_bytes();
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escaped = false;
    let mut start = 0usize;
    let mut parts = Vec::new();
    let mut saw_pipe = false;
    let mut i = 0usize;
    while i < bytes.len() {
        if in_string {
            if escaped {
                escaped = false;
                i += 1;
                continue;
            }
            if bytes[i] == b'\\' {
                escaped = true;
                i += 1;
                continue;
            }
            if bytes[i] == b'"' {
                in_string = false;
            }
            i += 1;
            continue;
        }
        match bytes[i] {
            b'"' => in_string = true,
            b'(' | b'[' => depth += 1,
            b')' | b']' => depth = depth.saturating_sub(1),
            b'|' if depth == 0 => {
                saw_pipe = true;
                parts.push(src[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
        i += 1;
    }
    if !saw_pipe {
        return None;
    }
    parts.push(src[start..].trim());
    if parts.iter().any(|p| p.is_empty()) {
        return None;
    }
    Some(parts)
}

pub(crate) fn split_record_constructor_shape(rhs: &str) -> Option<(&str, &str)> {
    if !rhs.ends_with(')') {
        return None;
    }
    let open_idx = rhs.find('(')?;
    let constructor = rhs[..open_idx].trim();
    if constructor.is_empty() || !is_camel_case_identifier(constructor) {
        return None;
    }
    if open_idx != constructor.len() {
        return None;
    }
    let inner = rhs[open_idx + 1..rhs.len() - 1].trim();
    Some((constructor, inner))
}

pub(crate) fn parse_record_field(field_src: &str) -> Option<RecordField> {
    let (name, ty) = field_src.split_once(':')?;
    let name = name.trim();
    let ty = ty.trim();
    if !is_non_reserved_identifier(name) || ty.is_empty() {
        return None;
    }
    Some(RecordField {
        name: name.to_string(),
        type_annotation: ty.to_string(),
    })
}

fn is_assignment_eq(line: &str, eq_index: usize) -> bool {
    let bytes = line.as_bytes();
    if eq_index > 0 && bytes[eq_index - 1] == b'=' {
        return false;
    }
    if eq_index + 1 < bytes.len() && bytes[eq_index + 1] == b'=' {
        return false;
    }
    true
}
