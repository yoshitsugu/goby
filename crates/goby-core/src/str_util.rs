/// Split `s` at the first top-level comma (i.e. not inside parentheses,
/// brackets, or string literals).  Returns `(left, right)` where `right`
/// includes everything after the comma.  Returns `None` if no top-level
/// comma exists.
pub(crate) fn split_top_level_comma(s: &str) -> Option<(&str, &str)> {
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
            '(' | '[' => depth += 1,
            ')' | ']' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                return Some((&s[..idx], &s[idx + 1..]));
            }
            _ => {}
        }
    }

    None
}

/// Split `s` at every top-level comma (not inside parentheses or string
/// literals) and return the trimmed parts.  Always returns at least one element.
pub(crate) fn split_top_level_commas(s: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut rest = s;
    loop {
        match split_top_level_comma(rest) {
            Some((left, right)) => {
                parts.push(left.trim());
                rest = right;
            }
            None => {
                parts.push(rest.trim());
                break;
            }
        }
    }
    parts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comma_inside_string_is_not_split() {
        assert_eq!(
            split_top_level_comma("\"a,b\", \"c\""),
            Some(("\"a,b\"", " \"c\""))
        );
    }
}
