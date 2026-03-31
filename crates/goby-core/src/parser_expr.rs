use crate::ast::{BinOpKind, Expr, InterpolatedPart, Span, UnaryOpKind};
use crate::parser_util::{
    is_camel_case_identifier, is_identifier, is_non_reserved_identifier, is_qualified_name,
};
use crate::str_util::split_top_level_commas;

/// Parse a single expression from a source string.
pub(crate) fn parse_expr(src: &str) -> Option<Expr> {
    let src = src.trim();

    if let Some((left, right)) = split_top_level_pipeline(src) {
        let right = right.trim();
        if is_identifier(right) {
            let value = parse_expr(left)?;
            return Some(Expr::Pipeline {
                value: Box::new(value),
                callee: right.to_string(),
                callee_span: None,
            });
        }
    }

    if let Some(expr) = parse_lambda(src) {
        return Some(expr);
    }

    if let Some(expr) = parse_resume_expr(src) {
        return Some(expr);
    }

    if let Some((left, right)) = split_top_level_double_token(src, "||") {
        return Some(Expr::BinOp {
            op: BinOpKind::Or,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some((left, right)) = split_top_level_double_token(src, "&&") {
        return Some(Expr::BinOp {
            op: BinOpKind::And,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some((left, right)) = split_top_level_eq(src) {
        return Some(Expr::BinOp {
            op: BinOpKind::Eq,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some((left, right)) = split_top_level_le(src) {
        return Some(Expr::BinOp {
            op: BinOpKind::Le,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some((left, right)) = split_top_level_ge(src) {
        return Some(Expr::BinOp {
            op: BinOpKind::Ge,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some((left, right)) = split_top_level_binop(src, '<') {
        return Some(Expr::BinOp {
            op: BinOpKind::Lt,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some((left, right)) = split_top_level_binop(src, '>') {
        return Some(Expr::BinOp {
            op: BinOpKind::Gt,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some((left, right)) = split_top_level_binop(src, '+') {
        return Some(Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some((left, right)) = split_top_level_binop(src, '-') {
        return Some(Expr::BinOp {
            op: BinOpKind::Sub,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some((left, right)) = split_top_level_binop(src, '*') {
        return Some(Expr::BinOp {
            op: BinOpKind::Mul,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some((left, right)) = split_top_level_binop(src, '/') {
        return Some(Expr::BinOp {
            op: BinOpKind::Div,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some((left, right)) = split_top_level_binop(src, '%') {
        return Some(Expr::BinOp {
            op: BinOpKind::Mod,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    if let Some(expr) = parse_not_expr(src) {
        return Some(expr);
    }

    // List index access: `expr[index]` — placed after binary-op splits so that
    // `xs[0] + 1` parses as `BinOp(+, ListIndex(xs,0), 1)` (the `+` split fires
    // first and recurses, hitting `parse_list_index_suffix` on `xs[0]`).
    // Note: `f xs[0]` ends with `]` so `parse_list_index_suffix` fires on the
    // whole string here, producing `ListIndex(Call(f, xs), 0)`.  Users who need
    // `Call(f, ListIndex(xs, 0))` should write `f (xs[0])` explicitly.
    // Placed before the `starts_with('[')` list-literal gate so that `[1,2,3][0]`
    // is not consumed as a plain list literal.
    if let Some(expr) = parse_list_index_suffix(src) {
        return Some(expr);
    }

    if src.starts_with('[') && src.ends_with(']') {
        return parse_list_expr(src);
    }

    if src.starts_with('(') && src.ends_with(')') {
        return parse_tuple_or_grouped_expr(src);
    }

    if let Some(expr) = parse_record_constructor_call(src) {
        return Some(expr);
    }

    if let Some(expr) = parse_method_call(src) {
        return Some(expr);
    }

    if let Some(expr) = parse_qualified_access(src) {
        return Some(expr);
    }

    if let Some(expr) = parse_call_expr(src) {
        return Some(expr);
    }

    if src.starts_with('"') && src.ends_with('"') && src.len() >= 2 {
        let inner = &src[1..src.len() - 1];
        return parse_interpolated_string_body(inner);
    }

    if src == "True" {
        return Some(Expr::BoolLit(true));
    }
    if src == "False" {
        return Some(Expr::BoolLit(false));
    }

    if let Ok(n) = src.parse::<i64>() {
        return Some(Expr::IntLit(n));
    }

    if is_non_reserved_identifier(src) {
        return Some(Expr::var(src));
    }

    None
}

pub(crate) fn enrich_expr_spans(expr: &mut Expr, src: &str, line: usize, col: usize) {
    let Some(spanned) = parse_expr_with_spans(src, line, col) else {
        return;
    };
    copy_expr_spans(expr, &spanned);
}

fn parse_expr_with_spans(src: &str, line: usize, col: usize) -> Option<Expr> {
    let trimmed = src.trim();
    let leading_ws = src.len().checked_sub(src.trim_start().len())?;
    let col = col + leading_ws;

    // MethodCall: receiver.method(...) — must come before parse_call_expr_with_spans
    if let Some(Expr::MethodCall {
        receiver,
        method,
        args,
        ..
    }) = parse_method_call(trimmed)
    {
        let len = trimmed.len();
        return Some(Expr::MethodCall {
            receiver,
            method,
            args,
            span: Some(Span::new(line, col, line, col + len)),
        });
    }

    // RecordConstruct: CamelCase(...) — must come before parse_call_expr_with_spans
    if let Some(Expr::RecordConstruct {
        constructor,
        fields,
        ..
    }) = parse_record_constructor_call(trimmed)
    {
        let len = trimmed.len();
        return Some(Expr::RecordConstruct {
            constructor,
            fields,
            span: Some(Span::new(line, col, line, col + len)),
        });
    }

    // Pipeline: value |> callee — callee_span uses subslice_offset (safe: callee is a subslice of trimmed)
    if let Some((left, right)) = split_top_level_pipeline(trimmed) {
        let callee = right.trim();
        if is_identifier(callee) {
            let callee_col = col + subslice_offset(trimmed, callee);
            let value = parse_expr(left)?;
            return Some(Expr::Pipeline {
                value: Box::new(value),
                callee: callee.to_string(),
                callee_span: Some(Span::new(line, callee_col, line, callee_col + callee.len())),
            });
        }
    }

    if let Some(expr) = parse_call_expr_with_spans(trimmed, line, col) {
        return Some(expr);
    }

    if let Some((receiver, member)) = trimmed.split_once('.')
        && is_identifier(receiver.trim())
        && (is_identifier(member.trim()) || is_tuple_member_index(member.trim()))
    {
        let len = trimmed.len();
        return Some(Expr::Qualified {
            receiver: receiver.trim().to_string(),
            member: member.trim().to_string(),
            span: Some(Span::new(line, col, line, col + len)),
        });
    }

    if is_non_reserved_identifier(trimmed) {
        let len = trimmed.len();
        return Some(Expr::Var {
            name: trimmed.to_string(),
            span: Some(Span::new(line, col, line, col + len)),
        });
    }

    parse_expr(trimmed)
}

fn parse_call_expr_with_spans(src: &str, line: usize, col: usize) -> Option<Expr> {
    if let Some(open) = src.find('(').filter(|_| src.ends_with(')')) {
        let callee_src = src[..open].trim();
        let inner = src[open + 1..src.len() - 1].trim();
        if is_identifier(callee_src) || is_qualified_name(callee_src) {
            let callee_col = col + subslice_offset(src, callee_src);
            let mut expr = parse_expr_with_spans(callee_src, line, callee_col)?;
            if inner.is_empty() {
                return Some(Expr::Call {
                    callee: Box::new(expr),
                    arg: Box::new(Expr::unit_value()),
                    span: Some(Span::new(line, col, line, col + src.len())),
                });
            }
            let parts = split_top_level_commas(inner);
            if parts.is_empty() || parts.iter().any(|part| part.trim().is_empty()) {
                return None;
            }
            for part in parts {
                let arg_src = part.trim();
                let arg_col = col + open + 1 + subslice_offset(inner, arg_src);
                expr = Expr::Call {
                    callee: Box::new(expr),
                    arg: Box::new(parse_expr_with_spans(arg_src, line, arg_col)?),
                    span: Some(Span::new(line, col, line, col + src.len())),
                };
            }
            return Some(expr);
        }
    }

    if src.starts_with('|') || src.starts_with('"') || src.starts_with('[') {
        return None;
    }

    let parts = split_top_level_whitespace_terms(src);
    if parts.len() < 2 {
        return None;
    }
    let callee_src = parts[0];
    if !(is_identifier(callee_src) || is_qualified_name(callee_src)) {
        return None;
    }
    let callee_col = col + subslice_offset(src, callee_src);
    let mut expr = parse_expr_with_spans(callee_src, line, callee_col)?;
    for part in parts.iter().skip(1) {
        let arg_src = part.trim();
        let arg_col = col + subslice_offset(src, arg_src);
        expr = Expr::Call {
            callee: Box::new(expr),
            arg: Box::new(parse_expr_with_spans(arg_src, line, arg_col)?),
            span: Some(Span::new(line, col, line, col + src.len())),
        };
    }
    Some(expr)
}

fn copy_expr_spans(dst: &mut Expr, src: &Expr) {
    match (dst, src) {
        (Expr::Var { span: dst_span, .. }, Expr::Var { span: src_span, .. }) => {
            *dst_span = *src_span
        }
        (Expr::Qualified { span: dst_span, .. }, Expr::Qualified { span: src_span, .. }) => {
            *dst_span = *src_span
        }
        (
            Expr::Call {
                callee: dst_callee,
                arg: dst_arg,
                span: dst_span,
            },
            Expr::Call {
                callee: src_callee,
                arg: src_arg,
                span: src_span,
            },
        ) => {
            *dst_span = *src_span;
            copy_expr_spans(dst_callee, src_callee);
            copy_expr_spans(dst_arg, src_arg);
        }
        (Expr::MethodCall { span: dst_span, .. }, Expr::MethodCall { span: src_span, .. }) => {
            *dst_span = *src_span;
        }
        (
            Expr::RecordConstruct { span: dst_span, .. },
            Expr::RecordConstruct { span: src_span, .. },
        ) => {
            *dst_span = *src_span;
        }
        (
            Expr::Pipeline {
                callee_span: dst_cs,
                ..
            },
            Expr::Pipeline {
                callee_span: src_cs,
                ..
            },
        ) => {
            *dst_cs = *src_cs;
        }
        _ => {}
    }
}

fn subslice_offset(parent: &str, child: &str) -> usize {
    child.as_ptr() as usize - parent.as_ptr() as usize
}

fn unescape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    result
}

fn parse_interpolated_string_body(body: &str) -> Option<Expr> {
    let bytes = body.as_bytes();
    let mut i = 0usize;
    let mut literal_start = 0usize;
    let mut parts: Vec<InterpolatedPart> = Vec::new();
    let mut saw_interpolation = false;

    while i < bytes.len() {
        if bytes[i] == b'\\' {
            i += 1;
            if i < bytes.len() {
                i += 1;
            }
            continue;
        }

        if bytes[i] == b'$' && i + 1 < bytes.len() && bytes[i + 1] == b'{' {
            saw_interpolation = true;
            let literal_raw = &body[literal_start..i];
            if !literal_raw.is_empty() {
                parts.push(InterpolatedPart::Text(unescape_string(literal_raw)));
            }

            let mut j = i + 2;
            let mut depth = 1usize;
            let mut in_string = false;
            let mut escaped = false;
            while j < bytes.len() {
                if in_string {
                    if escaped {
                        escaped = false;
                        j += 1;
                        continue;
                    }
                    if bytes[j] == b'\\' {
                        escaped = true;
                        j += 1;
                        continue;
                    }
                    if bytes[j] == b'"' {
                        in_string = false;
                    }
                    j += 1;
                    continue;
                }

                match bytes[j] {
                    b'"' => in_string = true,
                    b'{' => depth += 1,
                    b'}' => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
                j += 1;
            }

            if j >= bytes.len() || bytes[j] != b'}' {
                return None;
            }

            let expr_src = body[i + 2..j].trim();
            if expr_src.is_empty() {
                return None;
            }
            parts.push(InterpolatedPart::Expr(Box::new(parse_expr(expr_src)?)));

            i = j + 1;
            literal_start = i;
            continue;
        }

        if bytes[i] == b'"' {
            return None;
        }

        i += 1;
    }

    if !saw_interpolation {
        return Some(Expr::StringLit(unescape_string(body)));
    }

    let trailing = &body[literal_start..];
    if !trailing.is_empty() {
        parts.push(InterpolatedPart::Text(unescape_string(trailing)));
    }

    if parts.is_empty() {
        return Some(Expr::StringLit(String::new()));
    }
    Some(Expr::InterpolatedString(parts))
}

fn parse_resume_expr(src: &str) -> Option<Expr> {
    let rest = src.strip_prefix("resume")?;
    if rest.is_empty() {
        return None;
    }
    if !rest.chars().next().is_some_and(char::is_whitespace) {
        return None;
    }
    let value_src = rest.trim();
    if value_src.is_empty() {
        return None;
    }
    let value = parse_expr(value_src)?;
    Some(Expr::Resume {
        value: Box::new(value),
    })
}

fn split_top_level_pipeline(src: &str) -> Option<(&str, &str)> {
    let bytes = src.as_bytes();
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escaped = false;
    let mut last_split: Option<usize> = None;
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
        match bytes[i] {
            b'"' => in_string = true,
            b'(' | b'[' => depth += 1,
            b')' | b']' => depth = depth.saturating_sub(1),
            b'|' if depth == 0 && i + 1 < bytes.len() && bytes[i + 1] == b'>' => {
                last_split = Some(i);
            }
            _ => {}
        }
        i += 1;
    }

    if let Some(pos) = last_split {
        let left = src[..pos].trim();
        let right = src[pos + 2..].trim();
        if !left.is_empty() && !right.is_empty() {
            return Some((left, right));
        }
    }

    None
}

fn split_top_level_binop(src: &str, op: char) -> Option<(&str, &str)> {
    let op_byte = op as u8;
    let bytes = src.as_bytes();
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escaped = false;
    let mut last_split: Option<usize> = None;
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
        match bytes[i] {
            b'"' => in_string = true,
            b'(' | b'[' => depth += 1,
            b')' | b']' => depth = depth.saturating_sub(1),
            b if b == op_byte && depth == 0 => {
                let left_space = i > 0 && bytes[i - 1] == b' ';
                let right_space = i + 1 < bytes.len() && bytes[i + 1] == b' ';
                if left_space && right_space {
                    last_split = Some(i);
                }
            }
            _ => {}
        }
        i += 1;
    }

    if let Some(pos) = last_split {
        let left = src[..pos - 1].trim();
        let right = src[pos + 2..].trim();
        if !left.is_empty() && !right.is_empty() {
            return Some((left, right));
        }
    }
    None
}

fn split_top_level_eq(src: &str) -> Option<(&str, &str)> {
    split_top_level_double_token(src, "==")
}

fn split_top_level_le(src: &str) -> Option<(&str, &str)> {
    split_top_level_double_token(src, "<=")
}

fn split_top_level_ge(src: &str) -> Option<(&str, &str)> {
    split_top_level_double_token(src, ">=")
}

fn split_top_level_double_token<'a>(src: &'a str, token: &str) -> Option<(&'a str, &'a str)> {
    let bytes = src.as_bytes();
    let token_bytes = token.as_bytes();
    debug_assert_eq!(token_bytes.len(), 2);
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escaped = false;
    let mut last_split: Option<usize> = None;
    let mut i = 0usize;
    while i + 1 < bytes.len() {
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
            b if depth == 0 && b == token_bytes[0] && bytes[i + 1] == token_bytes[1] => {
                let left_space = i > 0 && bytes[i - 1] == b' ';
                let right_space = i + 2 < bytes.len() && bytes[i + 2] == b' ';
                if left_space && right_space {
                    last_split = Some(i);
                }
                i += 1;
            }
            _ => {}
        }
        i += 1;
    }

    if let Some(pos) = last_split {
        let left = src[..pos - 1].trim();
        let right = src[pos + 3..].trim();
        if !left.is_empty() && !right.is_empty() {
            return Some((left, right));
        }
    }
    None
}

fn parse_not_expr(src: &str) -> Option<Expr> {
    let inner = src.strip_prefix('!')?.trim();
    if inner.is_empty() {
        return None;
    }
    Some(Expr::UnaryOp {
        op: UnaryOpKind::Not,
        expr: Box::new(parse_expr(inner)?),
    })
}

fn parse_lambda(src: &str) -> Option<Expr> {
    let src = src.trim();

    // `fn a b -> expr` — multi-parameter anonymous function syntax.
    // Desugars to nested single-param lambdas at parse time:
    //   `fn a b -> expr`  ≡  Lambda { "a", Lambda { "b", expr } }
    // `fn a -> expr` (single-param) is also accepted.
    if let Some(stripped) = src.strip_prefix("fn ") {
        let rest = stripped.trim();
        // Collect parameter identifiers up to `->`
        let arrow = rest
            .find("->")
            .filter(|&i| rest.get(i.saturating_sub(1)..i) != Some("-"))?;
        let params_src = rest[..arrow].trim();
        let body_src = rest[arrow + 2..].trim();
        if body_src.is_empty() {
            return None;
        }
        let params: Vec<&str> = params_src.split_whitespace().collect();
        if params.is_empty() || !params.iter().all(|p| is_non_reserved_identifier(p)) {
            return None;
        }
        let body = parse_expr(body_src)?;
        // Desugar right-to-left: fn a b -> e  ≡  Lambda { "a", Lambda { "b", e } }
        let lambda = params.iter().rev().fold(body, |acc, &param| Expr::Lambda {
            param: param.to_string(),
            body: Box::new(acc),
        });
        return Some(lambda);
    }

    if src.starts_with("_ ")
        && let Some(body) = parse_placeholder_body(src)
    {
        return Some(Expr::Lambda {
            param: "_".to_string(),
            body: Box::new(body),
        });
    }

    None
}

fn parse_placeholder_body(src: &str) -> Option<Expr> {
    if let Some(rhs) = src.strip_prefix("_ + ") {
        let right = parse_non_lambda_expr(rhs.trim())?;
        return Some(Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(Expr::var("_")),
            right: Box::new(right),
        });
    }
    if let Some(rhs) = src.strip_prefix("_ * ") {
        let right = parse_non_lambda_expr(rhs.trim())?;
        return Some(Expr::BinOp {
            op: BinOpKind::Mul,
            left: Box::new(Expr::var("_")),
            right: Box::new(right),
        });
    }
    None
}

fn parse_non_lambda_expr(src: &str) -> Option<Expr> {
    let src = src.trim();
    if let Ok(n) = src.parse::<i64>() {
        return Some(Expr::IntLit(n));
    }
    if is_non_reserved_identifier(src) {
        return Some(Expr::var(src));
    }
    if let Some((left, right)) = split_top_level_binop(src, '+') {
        return Some(Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(parse_non_lambda_expr(left)?),
            right: Box::new(parse_non_lambda_expr(right)?),
        });
    }
    if let Some((left, right)) = split_top_level_binop(src, '*') {
        return Some(Expr::BinOp {
            op: BinOpKind::Mul,
            left: Box::new(parse_non_lambda_expr(left)?),
            right: Box::new(parse_non_lambda_expr(right)?),
        });
    }
    None
}

fn parse_list_expr(src: &str) -> Option<Expr> {
    let inner = src[1..src.len() - 1].trim();
    if inner.is_empty() {
        return Some(Expr::ListLit {
            elements: Vec::new(),
            spread: None,
        });
    }
    let parts = split_top_level_commas(inner);

    for part in &parts[..parts.len() - 1] {
        if part.trim().starts_with("..") {
            return None;
        }
    }

    let last = parts.last().unwrap().trim();
    if let Some(tail_src) = last.strip_prefix("..") {
        let tail_src = tail_src.trim();
        if parts.len() == 1 || tail_src.is_empty() {
            return None;
        }
        let spread_expr = parse_expr(tail_src)?;
        let elements: Option<Vec<Expr>> = parts[..parts.len() - 1]
            .iter()
            .map(|p| parse_expr(p.trim()))
            .collect();
        return Some(Expr::ListLit {
            elements: elements?,
            spread: Some(Box::new(spread_expr)),
        });
    }

    let items: Option<Vec<Expr>> = parts.iter().map(|p| parse_expr(p.trim())).collect();
    Some(Expr::ListLit {
        elements: items?,
        spread: None,
    })
}

fn parse_tuple_or_grouped_expr(src: &str) -> Option<Expr> {
    let inner = src[1..src.len() - 1].trim();
    if inner.is_empty() {
        return Some(Expr::unit_value());
    }
    let parts = split_top_level_commas(inner);
    if parts.len() == 1 {
        return parse_expr(parts[0].trim());
    }
    let items: Option<Vec<Expr>> = parts.iter().map(|p| parse_expr(p.trim())).collect();
    Some(Expr::TupleLit(items?))
}

fn parse_method_call(src: &str) -> Option<Expr> {
    if !src.ends_with(')') {
        return None;
    }
    let open_idx = src.find('(')?;
    let callee = src[..open_idx].trim();
    let (receiver, method) = callee.split_once('.')?;
    if !is_identifier(receiver) || !is_identifier(method) {
        return None;
    }
    let inner = &src[open_idx + 1..src.len() - 1];
    let parts = split_top_level_commas(inner);
    if parts.is_empty() || parts.iter().any(|p| p.trim().is_empty()) {
        return None;
    }
    let args: Option<Vec<Expr>> = parts.iter().map(|p| parse_expr(p.trim())).collect();
    Some(Expr::MethodCall {
        receiver: receiver.to_string(),
        method: method.to_string(),
        args: args?,
        span: None,
    })
}

fn parse_record_constructor_call(src: &str) -> Option<Expr> {
    if !src.ends_with(')') {
        return None;
    }
    let open_idx = src.find('(')?;
    let constructor = src[..open_idx].trim();
    if !is_camel_case_identifier(constructor) {
        return None;
    }
    let inner = src[open_idx + 1..src.len() - 1].trim();
    if inner.is_empty() {
        return Some(Expr::RecordConstruct {
            constructor: constructor.to_string(),
            fields: Vec::new(),
            span: None,
        });
    }

    let parts = split_top_level_commas(inner);
    let parsed_fields: Option<Vec<(String, Expr)>> = parts
        .iter()
        .map(|part| parse_record_constructor_field(part.trim()))
        .collect();
    Some(Expr::RecordConstruct {
        constructor: constructor.to_string(),
        fields: parsed_fields?,
        span: None,
    })
}

fn parse_record_constructor_field(part: &str) -> Option<(String, Expr)> {
    let (name, value) = part.split_once(':')?;
    let name = name.trim();
    let value = value.trim();
    if !is_identifier(name) || value.is_empty() {
        return None;
    }
    Some((name.to_string(), parse_expr(value)?))
}

fn parse_qualified_access(src: &str) -> Option<Expr> {
    if src.contains('(') || src.contains(')') {
        return None;
    }
    let (receiver, member) = src.split_once('.')?;
    let receiver = receiver.trim();
    let member = member.trim();
    if !is_identifier(receiver) || !(is_identifier(member) || is_tuple_member_index(member)) {
        return None;
    }
    Some(Expr::Qualified {
        receiver: receiver.to_string(),
        member: member.to_string(),
        span: None,
    })
}

fn is_tuple_member_index(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_ascii_digit())
}

fn parse_call_expr(src: &str) -> Option<Expr> {
    if let Some(open) = src.find('(').filter(|_| src.ends_with(')')) {
        let callee = src[..open].trim();
        let inner = src[open + 1..src.len() - 1].trim();
        if is_identifier(callee) || is_qualified_name(callee) {
            let mut expr = parse_expr(callee)?;
            if inner.is_empty() {
                return Some(Expr::Call {
                    callee: Box::new(expr),
                    arg: Box::new(Expr::unit_value()),
                    span: None,
                });
            }
            let parts = split_top_level_commas(inner);
            if parts.is_empty() || parts.iter().any(|part| part.trim().is_empty()) {
                return None;
            }
            for part in parts {
                expr = Expr::Call {
                    callee: Box::new(expr),
                    arg: Box::new(parse_expr(part.trim())?),
                    span: None,
                };
            }
            return Some(expr);
        }
    }

    if src.starts_with('|') || src.starts_with('"') || src.starts_with('[') {
        return None;
    }

    let parts = split_top_level_whitespace_terms(src);
    if parts.len() < 2 {
        return None;
    }
    let callee = parts[0];
    if !(is_identifier(callee) || is_qualified_name(callee)) {
        return None;
    }
    let mut expr = parse_expr(callee)?;
    for part in parts.iter().skip(1) {
        let arg = parse_expr(part)?;
        expr = Expr::Call {
            callee: Box::new(expr),
            arg: Box::new(arg),
            span: None,
        };
    }
    Some(expr)
}

fn split_top_level_whitespace_terms(src: &str) -> Vec<&str> {
    let mut terms = Vec::new();
    let mut start: Option<usize> = None;
    let mut depth = 0usize;
    let mut in_string = false;
    let mut escaped = false;

    for (idx, ch) in src.char_indices() {
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
            '"' => {
                in_string = true;
                if start.is_none() {
                    start = Some(idx);
                }
            }
            '(' | '[' => {
                depth += 1;
                if start.is_none() {
                    start = Some(idx);
                }
            }
            ')' | ']' => {
                depth = depth.saturating_sub(1);
            }
            c if c.is_whitespace() && depth == 0 => {
                if let Some(s) = start.take() {
                    let piece = src[s..idx].trim();
                    if !piece.is_empty() {
                        terms.push(piece);
                    }
                }
            }
            _ => {
                if start.is_none() {
                    start = Some(idx);
                }
            }
        }
    }

    if let Some(s) = start {
        let piece = src[s..].trim();
        if !piece.is_empty() {
            terms.push(piece);
        }
    }
    terms
}

/// Try to parse `src` as a list-index expression `expr[index]`.
///
/// Scans from the right end of `src` looking for a top-level `[…]` suffix.
/// If found and the prefix (the `list` expression) is non-empty, recurses to
/// parse both sub-expressions.
///
/// Placed after binary-op splits so that binary operators bind more tightly than
/// index access (e.g. `xs[0] + 1` → `(xs[0]) + 1` since `+` split fires first).
/// Placed before the `starts_with('[')` list-literal gate so that `[1,2,3][0]`
/// is not consumed as a plain list literal.
///
/// Chaining (`xs[0][1]`) works naturally because the recursive call to
/// `parse_expr` on the prefix will re-enter this function.
///
/// Notes:
/// - `(paren_expr)[idx]` is supported: the backward scan finds the `[` before the
///   `)`, so `(xs)[0]` and `(f x)[0]` both work via `parse_tuple_or_grouped_expr`.
/// - String literals in the index containing `]` are handled correctly because
///   the scan tracks string literal boundaries right-to-left.
/// - `f xs[0]` parses as `ListIndex(Call(f, xs), 0)` because the whole string ends
///   with `]` and this function fires before `parse_call_expr`.
fn parse_list_index_suffix(src: &str) -> Option<Expr> {
    // Quick rejection: must end with `]` to possibly be an index access.
    if !src.ends_with(']') {
        return None;
    }

    // Walk backwards to find the `[` matching the trailing `]`.
    // Track `[`/`]`, `(`/`)` nesting depth and string literal boundaries.
    // All of `[`, `]`, `(`, `)`, `"`, `\` are ASCII single-byte — scanning
    // byte-by-byte is correct even for multi-byte UTF-8 content.
    let bytes = src.as_bytes();
    let len = bytes.len();

    let mut depth = 0usize;
    let mut bracket_start: Option<usize> = None;
    let mut i = len; // start just past the last character
    while i > 0 {
        i -= 1;
        match bytes[i] {
            b'"' => {
                // Right-to-left string literal skip: scan left until we find the
                // opening `"`, accounting for `\`-escape sequences.
                // We walk left past the closing `"` we just found, looking for the
                // matching opening `"`.
                if i == 0 {
                    return None; // malformed — opening quote would be before start
                }
                i -= 1;
                loop {
                    if bytes[i] == b'"' {
                        // Check if this `"` is escaped by counting preceding backslashes.
                        let mut backslashes = 0usize;
                        let mut j = i;
                        while j > 0 && bytes[j - 1] == b'\\' {
                            backslashes += 1;
                            j -= 1;
                        }
                        if backslashes.is_multiple_of(2) {
                            // Unescaped `"` — this is the opening quote; done.
                            break;
                        }
                    }
                    if i == 0 {
                        return None; // unterminated string
                    }
                    i -= 1;
                }
                // `i` now points at the opening `"`. Continue the outer loop.
            }
            b']' | b')' => depth += 1,
            b'[' | b'(' => {
                if depth == 0 {
                    // A `[` or `(` that has no matching closer to our right —
                    // the source is malformed (e.g. an unbalanced `xs[0` or `(xs`).
                    // Note: `(expr)[idx]` does NOT reach here; the scanner finds the
                    // index `[` at depth==1 and breaks before ever reaching the `(`.
                    return None;
                }
                depth -= 1;
                if depth == 0 && bytes[i] == b'[' {
                    bracket_start = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }

    let bracket_start = bracket_start?;

    // The prefix (list expression) is src[..bracket_start].
    let list_src = src[..bracket_start].trim();
    if list_src.is_empty() {
        // `[…]` with no prefix — plain list literal, not an index access.
        return None;
    }

    // The index expression is src[bracket_start+1..len-1].
    let index_src = src[bracket_start + 1..len - 1].trim();
    if index_src.is_empty() {
        // Empty bracket `xs[]` — reject.
        return None;
    }

    let list = parse_expr(list_src)?;
    let index = parse_expr(index_src)?;
    Some(Expr::ListIndex {
        list: Box::new(list),
        index: Box::new(index),
    })
}

#[cfg(test)]
mod tests {
    use super::parse_expr;
    use crate::ast::{BinOpKind, Expr, InterpolatedPart, UnaryOpKind};

    #[test]
    fn parses_int_literal() {
        assert_eq!(parse_expr("42"), Some(Expr::IntLit(42)));
        assert_eq!(parse_expr("-5"), Some(Expr::IntLit(-5)));
    }

    #[test]
    fn parses_string_literal() {
        assert_eq!(
            parse_expr("\"hello\""),
            Some(Expr::StringLit("hello".to_string()))
        );
    }

    #[test]
    fn parses_interpolated_string_with_variables() {
        assert_eq!(
            parse_expr("\"${a}${b}\""),
            Some(Expr::InterpolatedString(vec![
                InterpolatedPart::Expr(Box::new(Expr::var("a"))),
                InterpolatedPart::Expr(Box::new(Expr::var("b"))),
            ]))
        );
    }

    #[test]
    fn parses_interpolated_string_with_literal_prefix_and_suffix() {
        assert_eq!(
            parse_expr("\"hello ${name}!\""),
            Some(Expr::InterpolatedString(vec![
                InterpolatedPart::Text("hello ".to_string()),
                InterpolatedPart::Expr(Box::new(Expr::var("name"))),
                InterpolatedPart::Text("!".to_string()),
            ]))
        );
    }

    #[test]
    fn parses_interpolated_string_with_string_literal_expression() {
        assert_eq!(
            parse_expr("\"${\"a\"}\""),
            Some(Expr::InterpolatedString(vec![InterpolatedPart::Expr(
                Box::new(Expr::StringLit("a".to_string())),
            )]))
        );
    }

    #[test]
    fn rejects_interpolated_string_with_unclosed_expression() {
        assert_eq!(parse_expr("\"x${name\""), None);
    }

    #[test]
    fn parses_identifier() {
        assert_eq!(parse_expr("foo"), Some(Expr::var("foo")));
    }

    #[test]
    fn parses_addition() {
        assert_eq!(
            parse_expr("a + b"),
            Some(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::var("a")),
                right: Box::new(Expr::var("b")),
            })
        );
    }

    #[test]
    fn parses_subtraction() {
        assert_eq!(
            parse_expr("a - b"),
            Some(Expr::BinOp {
                op: BinOpKind::Sub,
                left: Box::new(Expr::var("a")),
                right: Box::new(Expr::var("b")),
            })
        );
    }

    #[test]
    fn parses_equality() {
        assert_eq!(
            parse_expr("a == b"),
            Some(Expr::BinOp {
                op: BinOpKind::Eq,
                left: Box::new(Expr::var("a")),
                right: Box::new(Expr::var("b")),
            })
        );
    }

    #[test]
    fn parses_bool_literals() {
        assert_eq!(parse_expr("True"), Some(Expr::BoolLit(true)));
        assert_eq!(parse_expr("False"), Some(Expr::BoolLit(false)));
    }

    #[test]
    fn parses_multiplication() {
        assert_eq!(
            parse_expr("b * 3"),
            Some(Expr::BinOp {
                op: BinOpKind::Mul,
                left: Box::new(Expr::var("b")),
                right: Box::new(Expr::IntLit(3)),
            })
        );
    }

    #[test]
    fn parses_division_and_modulo() {
        assert_eq!(
            parse_expr("b / 3"),
            Some(Expr::BinOp {
                op: BinOpKind::Div,
                left: Box::new(Expr::var("b")),
                right: Box::new(Expr::IntLit(3)),
            })
        );
        assert_eq!(
            parse_expr("b % 3"),
            Some(Expr::BinOp {
                op: BinOpKind::Mod,
                left: Box::new(Expr::var("b")),
                right: Box::new(Expr::IntLit(3)),
            })
        );
    }

    #[test]
    fn parses_operator_precedence_mul_higher_than_add() {
        assert_eq!(
            parse_expr("1 + 2 * 3"),
            Some(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::IntLit(1)),
                right: Box::new(Expr::BinOp {
                    op: BinOpKind::Mul,
                    left: Box::new(Expr::IntLit(2)),
                    right: Box::new(Expr::IntLit(3)),
                }),
            })
        );
    }

    #[test]
    fn parses_comparison_family() {
        assert_eq!(
            parse_expr("a <= b"),
            Some(Expr::BinOp {
                op: BinOpKind::Le,
                left: Box::new(Expr::var("a")),
                right: Box::new(Expr::var("b")),
            })
        );
        assert_eq!(
            parse_expr("a >= b"),
            Some(Expr::BinOp {
                op: BinOpKind::Ge,
                left: Box::new(Expr::var("a")),
                right: Box::new(Expr::var("b")),
            })
        );
    }

    #[test]
    fn parses_boolean_operator_family() {
        assert_eq!(
            parse_expr("a || b"),
            Some(Expr::BinOp {
                op: BinOpKind::Or,
                left: Box::new(Expr::var("a")),
                right: Box::new(Expr::var("b")),
            })
        );
        assert_eq!(
            parse_expr("!flag"),
            Some(Expr::UnaryOp {
                op: UnaryOpKind::Not,
                expr: Box::new(Expr::var("flag")),
            })
        );
    }

    #[test]
    fn parses_comparison_as_higher_precedence_than_logical_ops() {
        assert_eq!(
            parse_expr("a < 1 && b > 3"),
            Some(Expr::BinOp {
                op: BinOpKind::And,
                left: Box::new(Expr::BinOp {
                    op: BinOpKind::Lt,
                    left: Box::new(Expr::var("a")),
                    right: Box::new(Expr::IntLit(1)),
                }),
                right: Box::new(Expr::BinOp {
                    op: BinOpKind::Gt,
                    left: Box::new(Expr::var("b")),
                    right: Box::new(Expr::IntLit(3)),
                }),
            })
        );
    }

    #[test]
    fn parses_equality_as_lower_precedence_than_addition() {
        assert_eq!(
            parse_expr("a + b == c"),
            Some(Expr::BinOp {
                op: BinOpKind::Eq,
                left: Box::new(Expr::BinOp {
                    op: BinOpKind::Add,
                    left: Box::new(Expr::var("a")),
                    right: Box::new(Expr::var("b")),
                }),
                right: Box::new(Expr::var("c")),
            })
        );
    }

    #[test]
    fn parses_left_associative_addition() {
        assert_eq!(
            parse_expr("1 + 2 + 3"),
            Some(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::BinOp {
                    op: BinOpKind::Add,
                    left: Box::new(Expr::IntLit(1)),
                    right: Box::new(Expr::IntLit(2)),
                }),
                right: Box::new(Expr::IntLit(3)),
            })
        );
    }

    #[test]
    fn parses_left_associative_multiplication() {
        assert_eq!(
            parse_expr("2 * 3 * 4"),
            Some(Expr::BinOp {
                op: BinOpKind::Mul,
                left: Box::new(Expr::BinOp {
                    op: BinOpKind::Mul,
                    left: Box::new(Expr::IntLit(2)),
                    right: Box::new(Expr::IntLit(3)),
                }),
                right: Box::new(Expr::IntLit(4)),
            })
        );
    }

    #[test]
    fn parses_pipeline_as_lowest_precedence() {
        assert_eq!(
            parse_expr("1 + 2 |> print"),
            Some(Expr::Pipeline {
                value: Box::new(Expr::BinOp {
                    op: BinOpKind::Add,
                    left: Box::new(Expr::IntLit(1)),
                    right: Box::new(Expr::IntLit(2)),
                }),
                callee: "print".to_string(),
                callee_span: None,
            })
        );
    }

    #[test]
    fn parses_left_associative_pipeline_chain() {
        assert_eq!(
            parse_expr("x |> f |> g"),
            Some(Expr::Pipeline {
                value: Box::new(Expr::Pipeline {
                    value: Box::new(Expr::var("x")),
                    callee: "f".to_string(),
                    callee_span: None,
                }),
                callee: "g".to_string(),
                callee_span: None,
            })
        );
    }

    #[test]
    fn rejects_pipeline_with_non_identifier_callee_expression() {
        assert_eq!(parse_expr("x |> f 1"), None);
        assert_eq!(parse_expr("x |> string.split(a, b)"), None);
    }

    #[test]
    fn requires_spaces_around_infix_operators_in_mvp_parser() {
        assert_eq!(parse_expr("a+b"), None);
        assert_eq!(parse_expr("a*2"), None);
        assert_eq!(parse_expr("a==b"), None);
    }

    #[test]
    fn parses_list_literal() {
        assert_eq!(
            parse_expr("[3, 4, 5]"),
            Some(Expr::ListLit {
                elements: vec![Expr::IntLit(3), Expr::IntLit(4), Expr::IntLit(5)],
                spread: None,
            })
        );
    }

    #[test]
    fn parses_list_spread_with_variable() {
        assert_eq!(
            parse_expr("[1, 2, ..xs]"),
            Some(Expr::ListLit {
                elements: vec![Expr::IntLit(1), Expr::IntLit(2)],
                spread: Some(Box::new(Expr::var("xs"))),
            })
        );
    }

    #[test]
    fn parses_list_spread_with_call_expr() {
        assert_eq!(
            parse_expr("[f(x), ..ys]"),
            Some(Expr::ListLit {
                elements: vec![Expr::call(Expr::var("f"), Expr::var("x"))],
                spread: Some(Box::new(Expr::var("ys"))),
            })
        );
    }

    #[test]
    fn parses_list_spread_single_prefix() {
        assert_eq!(
            parse_expr("[a, ..rest]"),
            Some(Expr::ListLit {
                elements: vec![Expr::var("a")],
                spread: Some(Box::new(Expr::var("rest"))),
            })
        );
    }

    #[test]
    fn rejects_list_spread_only() {
        assert_eq!(parse_expr("[..xs]"), None);
    }

    #[test]
    fn rejects_list_spread_non_trailing() {
        assert_eq!(parse_expr("[..a, b]"), None);
    }

    #[test]
    fn rejects_list_spread_middle() {
        assert_eq!(parse_expr("[a, ..b, c]"), None);
    }

    #[test]
    fn rejects_list_spread_missing_expr() {
        assert_eq!(parse_expr("[a, ..]"), None);
    }

    #[test]
    fn parses_tuple_literal() {
        assert_eq!(
            parse_expr("(\"Hello\", 3)"),
            Some(Expr::TupleLit(vec![
                Expr::StringLit("Hello".to_string()),
                Expr::IntLit(3)
            ]))
        );
    }

    #[test]
    fn parses_unit_literal_as_empty_tuple_syntax() {
        assert_eq!(parse_expr("()"), Some(Expr::unit_value()));
    }

    #[test]
    fn parses_spaced_function_call() {
        assert_eq!(
            parse_expr("add_ten 10"),
            Some(Expr::call(Expr::var("add_ten"), Expr::IntLit(10)))
        );
    }

    #[test]
    fn parses_spaced_multi_arg_function_call_left_associative() {
        assert_eq!(
            parse_expr("f a b c"),
            Some(Expr::call(
                Expr::call(Expr::call(Expr::var("f"), Expr::var("a")), Expr::var("b")),
                Expr::var("c"),
            ))
        );
    }

    #[test]
    fn parses_spaced_unit_argument_function_call() {
        assert_eq!(
            parse_expr("read_line ()"),
            Some(Expr::call(Expr::var("read_line"), Expr::unit_value()))
        );
    }

    #[test]
    fn parses_parenthesized_unit_argument_function_call() {
        assert_eq!(
            parse_expr("read_line()"),
            Some(Expr::call(Expr::var("read_line"), Expr::unit_value()))
        );
    }

    #[test]
    fn parses_parenthesized_multi_arg_function_call_left_associative() {
        assert_eq!(
            parse_expr("split(text, \"\\n\")"),
            Some(Expr::call(
                Expr::call(Expr::var("split"), Expr::var("text")),
                Expr::StringLit("\n".to_string()),
            ))
        );
    }

    #[test]
    fn parses_spaced_constructor_call_when_name_is_camel_case() {
        assert_eq!(
            parse_expr("User ()"),
            Some(Expr::RecordConstruct {
                constructor: "User".to_string(),
                fields: vec![],
                span: None,
            })
        );
    }

    #[test]
    fn parses_named_lambda() {
        assert_eq!(
            parse_expr("fn n -> n * 10"),
            Some(Expr::Lambda {
                param: "n".to_string(),
                body: Box::new(Expr::BinOp {
                    op: BinOpKind::Mul,
                    left: Box::new(Expr::var("n")),
                    right: Box::new(Expr::IntLit(10)),
                }),
            })
        );
    }

    #[test]
    fn parses_placeholder_lambda() {
        assert_eq!(
            parse_expr("_ * 10"),
            Some(Expr::Lambda {
                param: "_".to_string(),
                body: Box::new(Expr::BinOp {
                    op: BinOpKind::Mul,
                    left: Box::new(Expr::var("_")),
                    right: Box::new(Expr::IntLit(10)),
                }),
            })
        );
    }

    #[test]
    fn parses_pipeline() {
        let expr = parse_expr("mul_tens [3, 4, 5] |> print").expect("should parse");
        assert!(matches!(expr, Expr::Pipeline { .. }));
    }

    #[test]
    fn parses_qualified_method_call() {
        assert_eq!(
            parse_expr("string.split(a, b)"),
            Some(Expr::MethodCall {
                receiver: "string".to_string(),
                method: "split".to_string(),
                args: vec![Expr::var("a"), Expr::var("b")],
                span: None,
            })
        );
    }

    #[test]
    fn parses_generic_qualified_method_call() {
        assert_eq!(
            parse_expr("l.join(paths, \"\\n\")"),
            Some(Expr::MethodCall {
                receiver: "l".to_string(),
                method: "join".to_string(),
                args: vec![Expr::var("paths"), Expr::StringLit("\n".to_string()),],
                span: None,
            })
        );
    }

    #[test]
    fn parses_record_constructor_call_with_named_fields() {
        assert_eq!(
            parse_expr("User(id: \"1234\", name: \"John\")"),
            Some(Expr::RecordConstruct {
                constructor: "User".to_string(),
                fields: vec![
                    ("id".to_string(), Expr::StringLit("1234".to_string())),
                    ("name".to_string(), Expr::StringLit("John".to_string())),
                ],
                span: None,
            })
        );
    }

    #[test]
    fn parses_qualified_access_expression() {
        assert_eq!(
            parse_expr("user.name"),
            Some(Expr::qualified("user", "name"))
        );
    }

    #[test]
    fn parses_qualified_method_call_with_three_arguments() {
        assert_eq!(
            parse_expr("string.join(a, b, c)"),
            Some(Expr::MethodCall {
                receiver: "string".to_string(),
                method: "join".to_string(),
                args: vec![Expr::var("a"), Expr::var("b"), Expr::var("c")],
                span: None,
            })
        );
    }

    #[test]
    fn parses_resume_expression() {
        assert_eq!(
            parse_expr("resume ()"),
            Some(Expr::Resume {
                value: Box::new(Expr::unit_value()),
            })
        );
    }

    #[test]
    fn parses_resume_expression_with_unit_literal() {
        assert_eq!(
            parse_expr("resume ()"),
            Some(Expr::Resume {
                value: Box::new(Expr::unit_value()),
            })
        );
    }

    #[test]
    fn rejects_malformed_resume_expression() {
        assert_eq!(parse_expr("resume"), None);
    }

    // --- Phase F1b: list index access parser tests ---

    fn list_index(list: Expr, index: Expr) -> Expr {
        Expr::ListIndex {
            list: Box::new(list),
            index: Box::new(index),
        }
    }

    #[test]
    fn parses_list_index_simple() {
        assert_eq!(
            parse_expr("xs[0]"),
            Some(list_index(Expr::var("xs"), Expr::IntLit(0)))
        );
    }

    #[test]
    fn parses_list_index_var_index() {
        assert_eq!(
            parse_expr("xs[i]"),
            Some(list_index(Expr::var("xs"), Expr::var("i")))
        );
    }

    #[test]
    fn parses_list_index_with_spaces_around_bracket() {
        // xs [ 0 ] — spaces inside brackets are trimmed from the index expression
        assert_eq!(
            parse_expr("xs [ 0 ]"),
            Some(list_index(Expr::var("xs"), Expr::IntLit(0)))
        );
    }

    #[test]
    fn list_index_in_call_arg_position() {
        // `f xs[0]` — ListIndex suffix fires only after binary-op splits but
        // BEFORE parse_call_expr; however parse_call_expr splits on whitespace
        // terms and each term is recursively parsed, so the result depends on
        // whether `parse_list_index_suffix` fires on the whole string first.
        // Current behaviour: "f xs[0]" ends with `]`, so `parse_list_index_suffix`
        // fires first and produces ListIndex(Call(f, xs), 0).
        // This matches the right-to-left scan finding `[` at position 4 with
        // list_src = "f xs" parsed as Call(f, xs).
        // Document the actual parse so regressions are caught.
        assert_eq!(
            parse_expr("f xs[0]"),
            Some(list_index(
                Expr::call(Expr::var("f"), Expr::var("xs")),
                Expr::IntLit(0)
            ))
        );
    }

    #[test]
    fn parses_list_index_expr_index() {
        // xs[i + 1]
        assert_eq!(
            parse_expr("xs[i + 1]"),
            Some(list_index(
                Expr::var("xs"),
                Expr::BinOp {
                    op: BinOpKind::Add,
                    left: Box::new(Expr::var("i")),
                    right: Box::new(Expr::IntLit(1)),
                }
            ))
        );
    }

    #[test]
    fn parses_list_index_call_receiver() {
        // f()[0]
        assert_eq!(
            parse_expr("f()[0]"),
            Some(list_index(
                Expr::call(Expr::var("f"), Expr::unit_value()),
                Expr::IntLit(0)
            ))
        );
    }

    #[test]
    fn parses_list_index_chained() {
        // xs[0][1]
        assert_eq!(
            parse_expr("xs[0][1]"),
            Some(list_index(
                list_index(Expr::var("xs"), Expr::IntLit(0)),
                Expr::IntLit(1)
            ))
        );
    }

    #[test]
    fn parses_list_literal_index_access() {
        // [1, 2, 3][0]
        assert_eq!(
            parse_expr("[1, 2, 3][0]"),
            Some(list_index(
                Expr::ListLit {
                    elements: vec![Expr::IntLit(1), Expr::IntLit(2), Expr::IntLit(3)],
                    spread: None,
                },
                Expr::IntLit(0)
            ))
        );
    }

    #[test]
    fn parses_list_index_paren_grouped_receiver() {
        // (xs)[0] — the backward scan finds `[` at position 4 (before `(xs)`),
        // list_src = "(xs)" which parse_tuple_or_grouped_expr turns into Var("xs").
        assert_eq!(
            parse_expr("(xs)[0]"),
            Some(list_index(Expr::var("xs"), Expr::IntLit(0)))
        );
    }

    #[test]
    fn rejects_empty_index_bracket() {
        assert_eq!(parse_expr("xs[]"), None);
    }

    #[test]
    fn list_literal_without_index_still_parses() {
        // Plain list literal must not be consumed as a ListIndex.
        assert_eq!(
            parse_expr("[1, 2, 3]"),
            Some(Expr::ListLit {
                elements: vec![Expr::IntLit(1), Expr::IntLit(2), Expr::IntLit(3)],
                spread: None,
            })
        );
    }

    // ── fn lambda syntax ──────────────────────────────────────────────────────

    #[test]
    fn fn_lambda_two_params_desugars_to_nested_lambda() {
        // `fn acc x -> acc + x` must parse to Lambda { "acc", Lambda { "x", acc + x } }
        let ast = parse_expr("fn acc x -> acc + x").expect("should parse");
        match &ast {
            Expr::Lambda { param, body } => {
                assert_eq!(param, "acc");
                match body.as_ref() {
                    Expr::Lambda { param: p2, .. } => assert_eq!(p2, "x"),
                    other => panic!("expected inner lambda, got {:?}", other),
                }
            }
            other => panic!("expected outer lambda, got {:?}", other),
        }
    }

    #[test]
    fn fn_lambda_one_param_desugars_to_single_lambda() {
        // `fn n -> n + 1` parses to Lambda { "n", n + 1 }
        let ast = parse_expr("fn n -> n + 1").expect("should parse");
        match &ast {
            Expr::Lambda { param, body } => {
                assert_eq!(param, "n");
                assert!(matches!(
                    body.as_ref(),
                    Expr::BinOp {
                        op: BinOpKind::Add,
                        ..
                    }
                ));
            }
            other => panic!("expected lambda, got {:?}", other),
        }
    }

    #[test]
    fn fn_lambda_three_params_desugars_to_nested_lambda() {
        // `fn a b c -> a + b + c` → Lambda { "a", Lambda { "b", Lambda { "c", a+b+c } } }
        let ast = parse_expr("fn a b c -> a + b + c").expect("should parse");
        match &ast {
            Expr::Lambda { param, body } => {
                assert_eq!(param, "a");
                match body.as_ref() {
                    Expr::Lambda {
                        param: p2,
                        body: b2,
                    } => {
                        assert_eq!(p2, "b");
                        assert!(matches!(b2.as_ref(), Expr::Lambda { param: p3, .. } if p3 == "c"));
                    }
                    other => panic!("expected second lambda, got {:?}", other),
                }
            }
            other => panic!("expected outer lambda, got {:?}", other),
        }
    }

    #[test]
    fn fn_lambda_zero_params_fails_to_parse() {
        // `fn -> expr` has no parameters — must not parse
        assert_eq!(parse_expr("fn -> x"), None);
    }

    #[test]
    fn pipe_lambda_syntax_rejected() {
        // `|x| -> expr` is the removed pipe-lambda syntax; must not parse
        assert_eq!(parse_expr("|n| -> n + 1"), None);
        assert_eq!(parse_expr("|a| -> |b| -> a + b"), None);
    }

    #[test]
    fn fn_lambda_reserved_keyword_param_rejected() {
        // reserved keywords must not be accepted as fn lambda parameters
        assert_eq!(parse_expr("fn if -> 1"), None);
        assert_eq!(parse_expr("fn fn -> 1"), None);
        assert_eq!(parse_expr("fn case x -> x"), None);
    }
}
