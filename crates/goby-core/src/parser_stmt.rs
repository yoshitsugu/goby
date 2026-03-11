use crate::ast::{CaseArm, Expr, HandlerClause, Stmt};
use crate::parser_pattern::parse_case_pattern;
use crate::parser_util::{
    is_identifier, is_non_reserved_identifier, starts_with_keyword_token, strip_line_comment,
};

pub(crate) fn parse_body_stmts_with<F>(body: &str, parse_expr: F) -> Option<Vec<Stmt>>
where
    F: Copy + Fn(&str) -> Option<Expr>,
{
    parse_stmts_from_lines(&body.lines().collect::<Vec<_>>(), 0, parse_expr).map(|(stmts, _)| stmts)
}

#[cfg(test)]
pub(crate) fn parse_stmt_with<F>(line: &str, parse_expr: F) -> Option<Stmt>
where
    F: Copy + Fn(&str) -> Option<Expr>,
{
    parse_stmt(line, parse_expr)
}

pub(crate) fn first_malformed_resume_expr_line_offset(body: &str) -> Option<usize> {
    body.lines().position(is_malformed_resume_expr_line)
}

pub(crate) fn first_legacy_using_line_offset(body: &str) -> Option<usize> {
    body.lines().position(|line| {
        let trimmed = strip_line_comment(line).trim();
        !trimmed.is_empty() && starts_with_keyword_token(trimmed, "using")
    })
}

fn parse_stmts_from_lines<F>(
    lines: &[&str],
    start: usize,
    parse_expr: F,
) -> Option<(Vec<Stmt>, usize)>
where
    F: Copy + Fn(&str) -> Option<Expr>,
{
    let mut stmts = Vec::new();
    let mut i = start;

    let baseline = {
        let mut b = None;
        for line in lines[start..].iter() {
            let stripped = strip_line_comment(line).trim_end();
            let trimmed = stripped.trim();
            if trimmed.is_empty() || trimmed.starts_with('#') {
                continue;
            }
            b = Some(indent_len(stripped));
            break;
        }
        b?
    };

    while i < lines.len() {
        let raw = lines[i];
        let stripped = strip_line_comment(raw).trim_end();
        let trimmed = stripped.trim();

        if trimmed.is_empty() || trimmed.starts_with('#') {
            i += 1;
            continue;
        }

        let this_indent = indent_len(stripped);
        if this_indent < baseline {
            break;
        }
        if this_indent > baseline {
            return None;
        }

        if let Some((name, rhs)) = try_split_binding(trimmed)
            && rhs.is_empty()
            && let Some(next_i) = find_next_nonblank(lines, i + 1)
        {
            let next_stripped = strip_line_comment(lines[next_i]).trim_end();
            let next_trimmed = next_stripped.trim();
            let next_indent = indent_len(next_stripped);
            if next_indent > this_indent
                && (next_trimmed == "with" || next_trimmed.starts_with("with "))
                && let Some((value, after)) =
                    parse_multiline_rhs_expr(lines, next_i, next_indent, next_trimmed, parse_expr)
            {
                stmts.push(Stmt::Binding {
                    name: name.to_string(),
                    value,
                });
                i = after;
                continue;
            }
        }

        if let Some((name, rhs)) = try_split_binding(trimmed)
            && rhs == "handler"
        {
            let (handler, next_i) =
                parse_handler_expr_from_lines(lines, i + 1, this_indent, parse_expr)?;
            stmts.push(Stmt::Binding {
                name: name.to_string(),
                value: handler,
            });
            i = next_i;
            continue;
        }

        if trimmed == "with" {
            let (handler, next_i) =
                parse_handler_expr_from_lines(lines, i + 1, this_indent, parse_expr)?;
            let (body, after_with) = parse_with_in_body(lines, next_i, this_indent, parse_expr)?;
            stmts.push(Stmt::Expr(Expr::With {
                handler: Box::new(handler),
                body,
            }));
            i = after_with;
            continue;
        }

        if let Some(handler_src) = trimmed.strip_prefix("with ") {
            let handler = parse_expr(handler_src.trim())?;
            let (body, after_with) = parse_with_in_body(lines, i + 1, this_indent, parse_expr)?;
            stmts.push(Stmt::Expr(Expr::With {
                handler: Box::new(handler),
                body,
            }));
            i = after_with;
            continue;
        }

        if (is_identifier(trimmed) || is_qualified_name(trimmed))
            && let Some(next_i) = find_next_nonblank(lines, i + 1)
        {
            let next_raw = lines[next_i];
            let next_stripped = strip_line_comment(next_raw).trim_end();
            let next_trimmed = next_stripped.trim();
            let next_indent = indent_len(next_stripped);
            if next_indent > this_indent
                && (next_trimmed.starts_with("case ") || next_trimmed.starts_with("if "))
                && let Some((multi_expr, consumed)) =
                    parse_multiline_expr(lines, next_i, parse_expr)
            {
                let callee = parse_expr(trimmed)?;
                stmts.push(Stmt::Expr(Expr::Call {
                    callee: Box::new(callee),
                    arg: Box::new(multi_expr),
                }));
                i = next_i + consumed;
                continue;
            }
        }

        if let Some(callee_src) = trimmed.strip_suffix('(')
            && !callee_src.trim().is_empty()
            && (is_identifier(callee_src.trim()) || is_qualified_name(callee_src.trim()))
            && let Some((call_expr, next_i)) = parse_parenthesized_multiline_call(
                lines,
                i,
                this_indent,
                callee_src.trim(),
                parse_expr,
            )
        {
            stmts.push(call_expr);
            i = next_i;
            continue;
        }

        if (trimmed.starts_with("case ") || trimmed.starts_with("if "))
            && let Some((multi_expr, consumed)) = parse_multiline_expr(lines, i, parse_expr)
        {
            stmts.push(Stmt::Expr(multi_expr));
            i += consumed;
            continue;
        }

        if let Some(rest) = trimmed.strip_prefix("mut ")
            && let Some((name, rhs)) = try_split_binding(rest)
            && rhs.is_empty()
            && let Some(next_i) = find_next_nonblank(lines, i + 1)
        {
            let next_stripped = strip_line_comment(lines[next_i]).trim_end();
            let next_trimmed = next_stripped.trim();
            let next_indent = indent_len(next_stripped);
            if next_indent > this_indent
                && (next_trimmed == "with" || next_trimmed.starts_with("with "))
                && let Some((value, after)) =
                    parse_multiline_rhs_expr(lines, next_i, next_indent, next_trimmed, parse_expr)
            {
                stmts.push(Stmt::MutBinding {
                    name: name.to_string(),
                    value,
                });
                i = after;
                continue;
            }
        }

        if let Some((name, rhs)) = try_split_binding(trimmed)
            && !rhs.is_empty()
            && let Some((value, next_i)) =
                parse_multiline_rhs_expr(lines, i, this_indent, rhs, parse_expr)
        {
            stmts.push(Stmt::Binding {
                name: name.to_string(),
                value,
            });
            i = next_i;
            continue;
        }

        if let Some(rest) = trimmed.strip_prefix("mut ")
            && let Some((name, rhs)) = try_split_binding(rest)
            && !rhs.is_empty()
            && let Some((value, next_i)) =
                parse_multiline_rhs_expr(lines, i, this_indent, rhs, parse_expr)
        {
            stmts.push(Stmt::MutBinding {
                name: name.to_string(),
                value,
            });
            i = next_i;
            continue;
        }

        if let Some((lhs, rhs)) = trimmed.split_once(":=")
            && rhs.trim().is_empty()
            && is_non_reserved_identifier(lhs.trim())
            && let Some(next_i) = find_next_nonblank(lines, i + 1)
        {
            let next_stripped = strip_line_comment(lines[next_i]).trim_end();
            let next_trimmed = next_stripped.trim();
            let next_indent = indent_len(next_stripped);
            if next_indent > this_indent
                && (next_trimmed == "with" || next_trimmed.starts_with("with "))
                && let Some((value, after)) =
                    parse_multiline_rhs_expr(lines, next_i, next_indent, next_trimmed, parse_expr)
            {
                stmts.push(Stmt::Assign {
                    name: lhs.trim().to_string(),
                    value,
                });
                i = after;
                continue;
            }
        }

        if let Some((name, rhs)) = try_split_assignment(trimmed)
            && let Some((value, next_i)) =
                parse_multiline_rhs_expr(lines, i, this_indent, rhs, parse_expr)
        {
            stmts.push(Stmt::Assign {
                name: name.to_string(),
                value,
            });
            i = next_i;
            continue;
        }

        stmts.push(parse_stmt(trimmed, parse_expr)?);
        i += 1;
    }

    Some((stmts, i - start))
}

fn parse_parenthesized_multiline_call<F>(
    lines: &[&str],
    line_idx: usize,
    line_indent: usize,
    callee_src: &str,
    parse_expr: F,
) -> Option<(Stmt, usize)>
where
    F: Copy + Fn(&str) -> Option<Expr>,
{
    let mut close_idx = line_idx + 1;
    while close_idx < lines.len() {
        let stripped = strip_line_comment(lines[close_idx]).trim_end();
        let trimmed = stripped.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            close_idx += 1;
            continue;
        }
        let indent = indent_len(stripped);
        if indent == line_indent && trimmed == ")" {
            break;
        }
        if indent <= line_indent {
            return None;
        }
        close_idx += 1;
    }
    if close_idx >= lines.len() {
        return None;
    }

    let inner_lines = &lines[line_idx + 1..close_idx];
    let next_nonblank = find_next_nonblank(inner_lines, 0)?;
    let inner_trimmed = strip_line_comment(inner_lines[next_nonblank])
        .trim_end()
        .trim();
    if !(inner_trimmed.starts_with("case ") || inner_trimmed.starts_with("if ")) {
        return None;
    }
    let (arg, consumed) = parse_multiline_expr(inner_lines, next_nonblank, parse_expr)?;
    if close_idx != line_idx + 1 + consumed {
        return None;
    }

    Some((
        Stmt::Expr(Expr::Call {
            callee: Box::new(parse_expr(callee_src)?),
            arg: Box::new(arg),
        }),
        close_idx + 1,
    ))
}

fn parse_multiline_rhs_expr<F>(
    lines: &[&str],
    line_idx: usize,
    line_indent: usize,
    rhs: &str,
    parse_expr: F,
) -> Option<(Expr, usize)>
where
    F: Copy + Fn(&str) -> Option<Expr>,
{
    let rhs_trimmed = rhs.trim();
    if rhs_trimmed == "with" {
        let (handler, next_i) =
            parse_handler_expr_from_lines(lines, line_idx + 1, line_indent, parse_expr)?;
        let (body, after_with) = parse_with_in_body(lines, next_i, line_indent, parse_expr)?;
        return Some((
            Expr::With {
                handler: Box::new(handler),
                body,
            },
            after_with,
        ));
    }

    if let Some(handler_src) = rhs_trimmed.strip_prefix("with ") {
        let handler = parse_expr(handler_src.trim())?;
        let (body, after_with) = parse_with_in_body(lines, line_idx + 1, line_indent, parse_expr)?;
        return Some((
            Expr::With {
                handler: Box::new(handler),
                body,
            },
            after_with,
        ));
    }

    if !(rhs_trimmed.starts_with("case ") || rhs_trimmed.starts_with("if ")) {
        return None;
    }

    let mut virtual_lines: Vec<String> = Vec::new();
    virtual_lines.push(format!("{}{}", " ".repeat(line_indent), rhs_trimmed));
    for raw in &lines[line_idx + 1..] {
        virtual_lines.push((*raw).to_string());
    }
    let refs: Vec<&str> = virtual_lines.iter().map(String::as_str).collect();
    let (expr, consumed) = parse_multiline_expr(&refs, 0, parse_expr)?;
    Some((expr, line_idx + consumed))
}

fn parse_with_in_body<F>(
    lines: &[&str],
    from: usize,
    expected_indent: usize,
    parse_expr: F,
) -> Option<(Vec<Stmt>, usize)>
where
    F: Copy + Fn(&str) -> Option<Expr>,
{
    let mut in_line = from;
    while in_line < lines.len() {
        let stripped = strip_line_comment(lines[in_line]).trim_end();
        let trimmed = stripped.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            in_line += 1;
            continue;
        }
        let indent = indent_len(stripped);
        if indent != expected_indent || trimmed != "in" {
            return None;
        }
        let (body, consumed) = parse_stmts_from_lines(lines, in_line + 1, parse_expr)?;
        if body.is_empty() {
            return None;
        }
        return Some((body, in_line + 1 + consumed));
    }
    None
}

fn parse_handler_expr_from_lines<F>(
    lines: &[&str],
    start: usize,
    parent_indent: usize,
    parse_expr: F,
) -> Option<(Expr, usize)>
where
    F: Copy + Fn(&str) -> Option<Expr>,
{
    let mut clauses = Vec::new();
    let mut i = start;

    while i < lines.len() {
        let clause_raw = lines[i];
        let clause_stripped = strip_line_comment(clause_raw).trim_end();
        let clause_trimmed = clause_stripped.trim();
        if clause_trimmed.is_empty() || clause_trimmed.starts_with('#') {
            i += 1;
            continue;
        }

        let clause_indent = indent_len(clause_stripped);
        if clause_indent <= parent_indent {
            break;
        }

        let (name, params, inline_body) = parse_handler_clause_header(clause_trimmed)?;
        let mut body = inline_body.to_string();
        i += 1;

        while i < lines.len() {
            let sub_raw = lines[i];
            let sub_stripped = strip_line_comment(sub_raw).trim_end();
            let sub_trimmed = sub_stripped.trim();
            if sub_trimmed.is_empty() || sub_trimmed.starts_with('#') {
                i += 1;
                continue;
            }

            let sub_indent = indent_len(sub_stripped);
            if sub_indent <= clause_indent {
                break;
            }
            if !body.is_empty() {
                body.push('\n');
            }
            body.push_str(sub_stripped.get(clause_indent..).unwrap_or(sub_trimmed));
            i += 1;
        }

        if body.is_empty() {
            return None;
        }

        let parsed_body = parse_body_stmts_with(&body, parse_expr);
        clauses.push(HandlerClause {
            name,
            params,
            body,
            parsed_body,
        });
    }

    if clauses.is_empty() {
        return None;
    }
    Some((Expr::Handler { clauses }, i))
}

fn parse_multiline_expr<F>(lines: &[&str], start: usize, parse_expr: F) -> Option<(Expr, usize)>
where
    F: Copy + Fn(&str) -> Option<Expr>,
{
    let raw = lines[start];
    let stripped = strip_line_comment(raw).trim_end();
    let trimmed = stripped.trim();
    let case_indent = indent_len(stripped);

    if let Some(scrutinee_src) = trimmed.strip_prefix("case ") {
        let scrutinee = parse_expr(scrutinee_src.trim())?;
        let mut i = start + 1;
        let mut arms: Vec<CaseArm> = Vec::new();
        while i < lines.len() {
            let arm_raw = lines[i];
            let arm_stripped = strip_line_comment(arm_raw).trim_end();
            let arm_trimmed = arm_stripped.trim();
            if arm_trimmed.is_empty() || arm_trimmed.starts_with('#') {
                i += 1;
                continue;
            }
            let arm_indent = indent_len(arm_stripped);
            if arm_indent <= case_indent {
                break;
            }
            let (pat_src, body_src) = split_case_arm(arm_trimmed)?;
            let pattern = parse_case_pattern(pat_src)?;
            let (body, next_i) = if body_src.is_empty() {
                let (stmts, next_i) =
                    parse_required_indented_stmt_block(lines, i + 1, arm_indent, parse_expr)?;
                (Expr::Block(stmts), next_i)
            } else {
                (parse_expr(body_src)?, i + 1)
            };
            arms.push(CaseArm {
                pattern,
                body: Box::new(body),
            });
            i = next_i;
        }
        if arms.is_empty() {
            return None;
        }
        Some((
            Expr::Case {
                scrutinee: Box::new(scrutinee),
                arms,
            },
            i - start,
        ))
    } else if let Some(cond_src) = trimmed.strip_prefix("if ") {
        let condition = parse_expr(cond_src.trim())?;
        let mut i = start + 1;

        while i < lines.len() {
            let then_raw = lines[i];
            let then_stripped = strip_line_comment(then_raw).trim_end();
            let then_trimmed = then_stripped.trim();
            if then_trimmed.is_empty() || then_trimmed.starts_with('#') {
                i += 1;
                continue;
            }
            let then_indent = indent_len(then_stripped);
            if then_indent <= case_indent {
                return None;
            }
            break;
        }
        if i >= lines.len() {
            return None;
        }
        let then_start = i;
        let (then_stmts, consumed) = parse_stmts_from_lines(lines, then_start, parse_expr)?;
        let then_expr = expr_from_branch_stmts(then_stmts);
        i = then_start + consumed;

        let mut found_else = false;
        while i < lines.len() && !found_else {
            let kw_raw = lines[i];
            let kw_stripped = strip_line_comment(kw_raw).trim_end();
            let kw_trimmed = kw_stripped.trim();
            if kw_trimmed.is_empty() || kw_trimmed.starts_with('#') {
                i += 1;
                continue;
            }
            let kw_indent = indent_len(kw_stripped);
            if kw_indent != case_indent || kw_trimmed != "else" {
                return None;
            }
            found_else = true;
            i += 1;
        }
        if !found_else {
            return None;
        }

        while i < lines.len() {
            let else_raw = lines[i];
            let else_stripped = strip_line_comment(else_raw).trim_end();
            let else_trimmed = else_stripped.trim();
            if else_trimmed.is_empty() || else_trimmed.starts_with('#') {
                i += 1;
                continue;
            }
            let else_indent = indent_len(else_stripped);
            if else_indent <= case_indent {
                return None;
            }
            break;
        }
        if i >= lines.len() {
            return None;
        }
        let else_start = i;
        let (else_stmts, consumed) = parse_stmts_from_lines(lines, else_start, parse_expr)?;
        let else_expr = expr_from_branch_stmts(else_stmts);
        i = else_start + consumed;

        Some((
            Expr::If {
                condition: Box::new(condition),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            },
            i - start,
        ))
    } else {
        None
    }
}

fn parse_stmt<F>(line: &str, parse_expr: F) -> Option<Stmt>
where
    F: Copy + Fn(&str) -> Option<Expr>,
{
    if let Some(rest) = line.strip_prefix("mut ") {
        let (name, rhs) = try_split_binding(rest)?;
        let value = parse_expr(rhs)?;
        return Some(Stmt::MutBinding {
            name: name.to_string(),
            value,
        });
    }
    if let Some((name, rhs)) = try_split_assignment(line) {
        let value = parse_expr(rhs)?;
        return Some(Stmt::Assign {
            name: name.to_string(),
            value,
        });
    }
    if let Some((name, rhs)) = try_split_binding(line) {
        let value = parse_expr(rhs)?;
        return Some(Stmt::Binding {
            name: name.to_string(),
            value,
        });
    }
    let expr = parse_expr(line)?;
    Some(Stmt::Expr(expr))
}

fn expr_from_branch_stmts(stmts: Vec<Stmt>) -> Expr {
    match stmts.as_slice() {
        [Stmt::Expr(expr)] => expr.clone(),
        _ => Expr::Block(stmts),
    }
}

fn parse_required_indented_stmt_block<F>(
    lines: &[&str],
    from: usize,
    parent_indent: usize,
    parse_expr: F,
) -> Option<(Vec<Stmt>, usize)>
where
    F: Copy + Fn(&str) -> Option<Expr>,
{
    let first_idx = find_next_nonblank(lines, from)?;
    let first_stripped = strip_line_comment(lines[first_idx]).trim_end();
    if indent_len(first_stripped) <= parent_indent {
        return None;
    }
    let (stmts, consumed) = parse_stmts_from_lines(lines, first_idx, parse_expr)?;
    if stmts.is_empty() {
        return None;
    }
    Some((stmts, first_idx + consumed))
}

fn parse_handler_clause_header(src: &str) -> Option<(String, Vec<String>, &str)> {
    let (lhs, rhs) = src.split_once("->")?;
    let lhs = lhs.trim();
    let rhs = rhs.trim();
    let mut parts = lhs.split_whitespace();
    let name = parts.next()?;
    if !is_non_reserved_identifier(name) {
        return None;
    }

    let mut params = Vec::new();
    for param in parts {
        if !is_non_reserved_identifier(param) {
            return None;
        }
        params.push(param.to_string());
    }

    Some((name.to_string(), params, rhs))
}

fn find_next_nonblank(lines: &[&str], from: usize) -> Option<usize> {
    for (offset, line) in lines[from..].iter().enumerate() {
        let stripped = strip_line_comment(line).trim_end();
        let trimmed = stripped.trim();
        if !trimmed.is_empty() && !trimmed.starts_with('#') {
            return Some(from + offset);
        }
    }
    None
}

fn split_case_arm(src: &str) -> Option<(&str, &str)> {
    let src = src.trim();
    let sep_idx = find_top_level_case_arm_separator(src)?;
    let pat_src = src[..sep_idx].trim();
    let body_src = src[sep_idx + 3..].trim();
    if pat_src.is_empty() {
        return None;
    }
    Some((pat_src, body_src))
}

fn find_top_level_case_arm_separator(src: &str) -> Option<usize> {
    let bytes = src.as_bytes();
    let mut in_string = false;
    let mut escaped = false;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut brace_depth = 0usize;

    let mut i = 0usize;
    while i + 2 < bytes.len() {
        let b = bytes[i];
        if in_string {
            if escaped {
                escaped = false;
            } else if b == b'\\' {
                escaped = true;
            } else if b == b'"' {
                in_string = false;
            }
            i += 1;
            continue;
        }
        match b {
            b'"' => in_string = true,
            b'(' => paren_depth += 1,
            b')' => paren_depth = paren_depth.saturating_sub(1),
            b'[' => bracket_depth += 1,
            b']' => bracket_depth = bracket_depth.saturating_sub(1),
            b'{' => brace_depth += 1,
            b'}' => brace_depth = brace_depth.saturating_sub(1),
            b' ' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                if bytes[i + 1] == b'-'
                    && bytes[i + 2] == b'>'
                    && (i + 3 == bytes.len() || bytes[i + 3].is_ascii_whitespace())
                {
                    return Some(i);
                }
            }
            _ => {}
        }
        i += 1;
    }
    None
}

fn indent_len(line: &str) -> usize {
    line.len() - line.trim_start().len()
}

fn is_qualified_name(s: &str) -> bool {
    if let Some((receiver, member)) = s.split_once('.') {
        is_identifier(receiver) && is_identifier(member)
    } else {
        false
    }
}

fn try_split_binding(line: &str) -> Option<(&str, &str)> {
    let idx = line.find('=')?;
    if !is_assignment_eq(line, idx) {
        return None;
    }
    let lhs = line[..idx].trim();
    let rhs = line[idx + 1..].trim();
    if is_non_reserved_identifier(lhs) {
        Some((lhs, rhs))
    } else {
        None
    }
}

fn try_split_assignment(line: &str) -> Option<(&str, &str)> {
    let (lhs, rhs) = line.split_once(":=")?;
    let lhs = lhs.trim();
    let rhs = rhs.trim();
    if lhs.is_empty() || rhs.is_empty() || !is_non_reserved_identifier(lhs) {
        return None;
    }
    Some((lhs, rhs))
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

fn is_malformed_resume_expr_line(src: &str) -> bool {
    let trimmed = strip_line_comment(src).trim();
    trimmed == "resume"
}
