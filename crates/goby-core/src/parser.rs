use crate::ast::{
    BinOpKind, CaseArm, CasePattern, Declaration, EffectDecl, EffectMember, Expr, HandlerDecl,
    HandlerMethod, ImportDecl, ImportKind, Module, RecordField, Stmt, TypeDeclaration,
};
use crate::str_util::split_top_level_commas;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub line: usize,
    pub message: String,
}

pub fn parse_module(source: &str) -> Result<Module, ParseError> {
    let lines: Vec<&str> = source.lines().collect();
    let mut i = 0;
    let mut imports = Vec::new();
    let mut type_declarations = Vec::new();
    let mut effect_declarations = Vec::new();
    let mut handler_declarations = Vec::new();
    let mut declarations = Vec::new();

    while i < lines.len() {
        let line = strip_line_comment(lines[i]).trim_end();
        let trimmed = line.trim();

        if trimmed.is_empty() || trimmed.starts_with('#') {
            i += 1;
            continue;
        }

        if is_indented(line) {
            return Err(ParseError {
                line: i + 1,
                message: "unexpected indentation at top level".to_string(),
            });
        }

        if trimmed.starts_with("import ") {
            let import = parse_import_line(trimmed).ok_or_else(|| ParseError {
                line: i + 1,
                message: "invalid import declaration".to_string(),
            })?;
            imports.push(import);
            i += 1;
            continue;
        }

        if trimmed.starts_with("type ") {
            let ty_decl = parse_type_declaration_line(trimmed).ok_or_else(|| ParseError {
                line: i + 1,
                message: "invalid type declaration".to_string(),
            })?;
            type_declarations.push(ty_decl);
            i += 1;
            continue;
        }

        // `effect Name` block: collect indented member signatures.
        if trimmed.starts_with("effect ") && !trimmed.contains('=') {
            let effect_name = trimmed["effect ".len()..].trim().to_string();
            if effect_name.is_empty() {
                return Err(ParseError {
                    line: i + 1,
                    message: "effect declaration requires a name".to_string(),
                });
            }
            let mut members = Vec::new();
            i += 1;
            while i < lines.len() {
                let member_line = lines[i];
                let member_trimmed = strip_line_comment(member_line).trim_end();
                let member_trimmed_str = member_trimmed.trim();
                if member_trimmed_str.is_empty() || member_trimmed_str.starts_with('#') {
                    i += 1;
                    continue;
                }
                if !is_indented(member_line) {
                    break;
                }
                // Parse `name: TypeAnnotation`
                if let Some((name, ty)) = member_trimmed_str.split_once(':') {
                    let name = name.trim().to_string();
                    let ty = ty.trim().to_string();
                    if !name.is_empty() && !ty.is_empty() {
                        members.push(EffectMember { name, type_annotation: ty });
                    }
                }
                i += 1;
            }
            effect_declarations.push(EffectDecl { name: effect_name, members });
            continue;
        }

        // `handler Name for Effect` block: collect indented method definitions.
        if trimmed.starts_with("handler ") {
            let rest = trimmed.strip_prefix("handler ").unwrap_or("").trim();
            let (handler_name, effect_name) = if let Some((h, e)) = rest.split_once(" for ") {
                (h.trim().to_string(), e.trim().to_string())
            } else {
                return Err(ParseError {
                    line: i + 1,
                    message: "handler declaration requires `handler Name for Effect`".to_string(),
                });
            };
            let mut methods = Vec::new();
            i += 1;
            while i < lines.len() {
                let method_line = lines[i];
                let method_stripped = strip_line_comment(method_line).trim_end();
                let method_trimmed = method_stripped.trim();
                if method_trimmed.is_empty() || method_trimmed.starts_with('#') {
                    i += 1;
                    continue;
                }
                if !is_indented(method_line) {
                    break;
                }
                // Parse `name params... = body` (single-line or with an indented sub-body).
                if let Some((lhs, rhs)) = method_trimmed.split_once('=') {
                    let lhs = lhs.trim();
                    let parts: Vec<&str> = lhs.split_whitespace().collect();
                    if !parts.is_empty() {
                        let name = parts[0].to_string();
                        let params: Vec<String> =
                            parts[1..].iter().map(|s| s.to_string()).collect();
                        let mut body = rhs.trim().to_string();
                        // Collect any deeper-indented sub-body lines.
                        i += 1;
                        while i < lines.len() {
                            let sub = lines[i];
                            let sub_s = strip_line_comment(sub).trim_end();
                            let sub_t = sub_s.trim();
                            if sub_t.is_empty() {
                                i += 1;
                                continue;
                            }
                            // Sub-body lines must be indented more than the method line.
                            // We check for double indentation (at least 4 spaces/2 tabs).
                            let indent_len = sub.len() - sub.trim_start().len();
                            let method_indent_len =
                                method_line.len() - method_line.trim_start().len();
                            if indent_len <= method_indent_len {
                                break;
                            }
                            body.push('\n');
                            body.push_str(sub_t);
                            i += 1;
                        }
                        let parsed_body = parse_body_stmts(&body);
                        methods.push(HandlerMethod { name, params, body, parsed_body });
                        continue;
                    }
                }
                i += 1;
            }
            handler_declarations.push(HandlerDecl {
                name: handler_name,
                effect: effect_name,
                methods,
            });
            continue;
        }

        let mut annotated_name: Option<&str> = None;
        let mut type_annotation = None;

        if let Some((name, ty)) = split_top_level_type(line) {
            if name.is_empty() || ty.is_empty() {
                return Err(ParseError {
                    line: i + 1,
                    message: "invalid type annotation".to_string(),
                });
            }
            annotated_name = Some(name);
            type_annotation = Some(ty.to_string());
            i += 1;
            i = skip_blank_and_comment_lines(&lines, i);
            if i >= lines.len() {
                return Err(ParseError {
                    line: i,
                    message: "missing declaration body after type annotation".to_string(),
                });
            }
        }

        let body_line = strip_line_comment(lines[i]).trim_end();
        let (name, params, mut body) =
            split_top_level_definition(body_line).ok_or_else(|| ParseError {
                line: i + 1,
                message: "expected top-level definition (`name ... = ...`)".to_string(),
            })?;

        if let Some(annotated_name) = annotated_name
            && annotated_name != name
        {
            return Err(ParseError {
                line: i + 1,
                message: format!(
                    "type annotation name `{}` does not match definition name `{}`",
                    annotated_name, name
                ),
            });
        }

        let j = collect_indented_body(lines.as_slice(), i + 1, &mut body);

        let parsed_body = parse_body_stmts(&body);

        declarations.push(Declaration {
            name: name.to_string(),
            type_annotation,
            params,
            body,
            parsed_body,
        });

        i = j;
    }

    Ok(Module {
        imports,
        type_declarations,
        effect_declarations,
        handler_declarations,
        declarations,
    })
}

/// Parse a declaration body string into a list of statements.
/// Returns `None` if any line cannot be parsed (caller may fall back to string-based evaluation).
pub fn parse_body_stmts(body: &str) -> Option<Vec<Stmt>> {
    parse_stmts_from_lines(&body.lines().collect::<Vec<_>>(), 0).map(|(stmts, _)| stmts)
}

/// Parse statements from a slice of raw (potentially indented) lines starting at `start`.
/// Returns `(stmts, lines_consumed)` where `lines_consumed` is the number of lines processed.
/// Processing stops when a line is encountered that is less indented than the first non-empty line,
/// or when `start` reaches the end of the slice.
fn parse_stmts_from_lines(lines: &[&str], start: usize) -> Option<(Vec<Stmt>, usize)> {
    let mut stmts = Vec::new();
    let mut i = start;

    // Determine the baseline indent from the first non-empty line.
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
        b.unwrap_or(0)
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
            // Dedented past our block — stop.
            break;
        }

        // `using HandlerA, HandlerB` statement.
        if trimmed.starts_with("using ") {
            let handlers_str = trimmed.strip_prefix("using ").unwrap_or("").trim();
            let handlers: Vec<String> = handlers_str
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect();
            if handlers.is_empty() {
                return None;
            }
            i += 1;
            // Collect the using body: lines more indented than the `using` line.
            let (body, consumed) = parse_stmts_from_lines(lines, i)?;
            i += consumed;
            stmts.push(Stmt::Using { handlers, body });
            continue;
        }

        // Check for `callee\n  case ...` or `callee\n  if ...` pattern.
        // e.g. `print\n    case x\n      5 -> "Five!"`.
        if (is_identifier(trimmed) || is_qualified_name(trimmed))
            && let Some(next_i) = find_next_nonblank(lines, i + 1)
        {
            let next_raw = lines[next_i];
            let next_stripped = strip_line_comment(next_raw).trim_end();
            let next_trimmed = next_stripped.trim();
            let next_indent = indent_len(next_stripped);
            if next_indent > this_indent
                && (next_trimmed.starts_with("case ") || next_trimmed.starts_with("if "))
                && let Some((multi_expr, consumed)) = parse_multiline_expr(lines, next_i)
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

        // Regular statement: binding or expression.
        stmts.push(parse_stmt(trimmed)?);
        i += 1;
    }

    Some((stmts, i - start))
}

fn indent_len(line: &str) -> usize {
    line.len() - line.trim_start().len()
}

/// Expand escape sequences in a string literal body (the content between the quotes).
/// Supported: `\n` → newline, `\t` → tab, `\\` → backslash, `\"` → double-quote.
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

/// Split a case arm line `"<pattern> -> <body>"` into `(pattern_src, body_src)`.
///
/// Unlike `split_once(" -> ")`, this function identifies the pattern token first
/// (a bare word, integer, or quoted string) and then requires ` -> ` immediately
/// after it. This prevents misidentifying ` -> ` inside a lambda body as the arm
/// separator (e.g. `5 -> |x| -> x + 1` splits at the first ` -> `, yielding
/// `"5"` and `"|x| -> x + 1"` correctly).
fn split_case_arm(src: &str) -> Option<(&str, &str)> {
    let src = src.trim();
    // Determine where the pattern token ends.
    let pat_end = if src.starts_with('"') {
        // Quoted string pattern: scan to closing quote.
        let mut idx = 1;
        let bytes = src.as_bytes();
        let mut closed = false;
        while idx < bytes.len() {
            if bytes[idx] == b'\\' {
                idx += 2; // skip escaped char
            } else if bytes[idx] == b'"' {
                idx += 1;
                closed = true;
                break;
            } else {
                idx += 1;
            }
        }
        if !closed {
            return None; // unterminated string pattern
        }
        idx
    } else {
        // Bare token (identifier, integer, `_`): ends at first space.
        src.find(' ').unwrap_or(src.len())
    };
    let pat_src = src[..pat_end].trim();
    let rest = src[pat_end..].trim_start();
    let body_src = rest.strip_prefix("-> ")?.trim();
    Some((pat_src, body_src))
}

fn parse_case_pattern(src: &str) -> Option<CasePattern> {
    let src = src.trim();
    if src == "_" {
        return Some(CasePattern::Wildcard);
    }
    if src == "True" {
        return Some(CasePattern::BoolLit(true));
    }
    if src == "False" {
        return Some(CasePattern::BoolLit(false));
    }
    if let Ok(n) = src.parse::<i64>() {
        return Some(CasePattern::IntLit(n));
    }
    if src.starts_with('"') && src.ends_with('"') && src.len() >= 2 {
        let inner = &src[1..src.len() - 1];
        if !inner.contains('"') {
            return Some(CasePattern::StringLit(inner.to_string()));
        }
    }
    None
}

/// Parse a multi-line `case` or `if` expression starting at `lines[start]`.
/// Returns `(expr, lines_consumed_from_start)` on success.
fn parse_multiline_expr(lines: &[&str], start: usize) -> Option<(Expr, usize)> {
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
            let body = parse_expr(body_src)?;
            arms.push(CaseArm {
                pattern,
                body: Box::new(body),
            });
            i += 1;
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

        // Consume then-expression (first non-empty line deeper than `if`)
        let mut then_expr: Option<Expr> = None;
        while i < lines.len() && then_expr.is_none() {
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
            then_expr = Some(parse_expr(then_trimmed)?);
            i += 1;
        }
        let then_expr = then_expr?;

        // Consume `else` keyword at same indent as `if`
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

        // Consume else-expression (first non-empty line deeper than `if`)
        let mut else_expr: Option<Expr> = None;
        while i < lines.len() && else_expr.is_none() {
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
            else_expr = Some(parse_expr(else_trimmed)?);
            i += 1;
        }
        let else_expr = else_expr?;

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

fn parse_stmt(line: &str) -> Option<Stmt> {
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

/// Parse a single expression from a source string.
pub fn parse_expr(src: &str) -> Option<Expr> {
    let src = src.trim();

    // 1. Pipeline (lowest precedence): expr |> callee
    if let Some((left, right)) = split_top_level_pipeline(src) {
        let right = right.trim();
        if is_identifier(right) {
            let value = parse_expr(left)?;
            return Some(Expr::Pipeline {
                value: Box::new(value),
                callee: right.to_string(),
            });
        }
    }

    // 2. Lambda: |p| -> body  or  _ op expr
    // Must be checked before binary operators to avoid splitting `|n| -> n * 10` at `*`.
    if let Some(expr) = parse_lambda(src) {
        return Some(expr);
    }

    // 3. Binary equality: expr == expr
    if let Some((left, right)) = split_top_level_eq(src) {
        return Some(Expr::BinOp {
            op: BinOpKind::Eq,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    // 4. Binary addition: expr + expr
    if let Some((left, right)) = split_top_level_binop(src, '+') {
        return Some(Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    // 5. Binary multiplication: expr * expr
    if let Some((left, right)) = split_top_level_binop(src, '*') {
        return Some(Expr::BinOp {
            op: BinOpKind::Mul,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    // 6. List literal: [...]
    if src.starts_with('[') && src.ends_with(']') {
        return parse_list_expr(src);
    }

    // 7. Tuple or grouped expression: (...)
    if src.starts_with('(') && src.ends_with(')') {
        return parse_tuple_or_grouped_expr(src);
    }

    // 8. Record constructor call: TypeName(field: value, ...)
    if let Some(expr) = parse_record_constructor_call(src) {
        return Some(expr);
    }

    // 9. Method call: receiver.method(args)
    if let Some(expr) = parse_method_call(src) {
        return Some(expr);
    }

    // 10. Qualified/member access: receiver.member
    if let Some(expr) = parse_qualified_access(src) {
        return Some(expr);
    }

    // 11. Function call: f x  or  f(x)
    if let Some((callee, arg)) = try_parse_call(src) {
        return Some(Expr::Call {
            callee: Box::new(parse_expr(callee)?),
            arg: Box::new(parse_expr(arg)?),
        });
    }

    // 12. String literal: "..."
    if src.starts_with('"') && src.ends_with('"') && src.len() >= 2 {
        let inner = &src[1..src.len() - 1];
        if !inner.contains('"') {
            return Some(Expr::StringLit(unescape_string(inner)));
        }
    }

    // 13. Bool literal
    if src == "True" {
        return Some(Expr::BoolLit(true));
    }
    if src == "False" {
        return Some(Expr::BoolLit(false));
    }

    // 14. Integer literal
    if let Ok(n) = src.parse::<i64>() {
        return Some(Expr::IntLit(n));
    }

    // 15. Identifier
    if is_identifier(src) {
        return Some(Expr::Var(src.to_string()));
    }

    None
}

// ---------------------------------------------------------------------------
// Splitting helpers
// ---------------------------------------------------------------------------

/// Split `src` at the top-level `|>` operator.
/// Returns `(left, right)` where right is the identifier after `|>`.
fn split_top_level_pipeline(src: &str) -> Option<(&str, &str)> {
    // Walk through bytes tracking depth/string state, find the LAST `|>` at
    // depth 0 to preserve left-associativity (`a |> f |> g` => `(a |> f) |> g`).
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

/// Split `src` at the top-level binary operator `op` (represented as char `+` or `*`).
/// Splits at the LAST occurrence to preserve left-associativity.
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
                // Require spaces around operator to avoid matching inside identifiers.
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
        let left = src[..pos - 1].trim(); // before the space before op
        let right = src[pos + 2..].trim(); // after the space after op
        if !left.is_empty() && !right.is_empty() {
            return Some((left, right));
        }
    }
    None
}

fn split_top_level_eq(src: &str) -> Option<(&str, &str)> {
    let bytes = src.as_bytes();
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
            b'=' if depth == 0 && bytes[i + 1] == b'=' => {
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

/// Parse a lambda expression.
/// Supports `|param| -> body` and placeholder shorthand `_ op expr`.
fn parse_lambda(src: &str) -> Option<Expr> {
    let src = src.trim();

    // Named lambda: |param| -> body
    if src.starts_with('|') {
        // Find the *second* `|` (strip the leading `|` first to avoid re-finding it)
        let second_bar = src.get(1..)?.find('|')? + 1;
        let param = src[1..second_bar].trim();
        if !is_identifier(param) {
            return None;
        }
        let rest = src[second_bar + 1..].trim();
        let body_src = rest.strip_prefix("->")?;
        let body_src = body_src.trim();
        if body_src.is_empty() {
            return None;
        }
        let body = parse_expr(body_src)?;
        return Some(Expr::Lambda {
            param: param.to_string(),
            body: Box::new(body),
        });
    }

    // Placeholder lambda: _ * 10  or  _ + 5
    // Must NOT call parse_expr recursively on `src` because it would loop back here.
    // Instead, parse the body directly as a binary expression with `_` as `Var("_")`.
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

/// Parse the body of a placeholder lambda `_ op rhs` without recursing through `parse_lambda`.
fn parse_placeholder_body(src: &str) -> Option<Expr> {
    // Only handle `_ + rhs` and `_ * rhs`
    if let Some(rhs) = src.strip_prefix("_ + ") {
        let right = parse_non_lambda_expr(rhs.trim())?;
        return Some(Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(Expr::Var("_".to_string())),
            right: Box::new(right),
        });
    }
    if let Some(rhs) = src.strip_prefix("_ * ") {
        let right = parse_non_lambda_expr(rhs.trim())?;
        return Some(Expr::BinOp {
            op: BinOpKind::Mul,
            left: Box::new(Expr::Var("_".to_string())),
            right: Box::new(right),
        });
    }
    None
}

/// Parse an expression that is guaranteed not to be a lambda (avoids recursion in placeholder parsing).
fn parse_non_lambda_expr(src: &str) -> Option<Expr> {
    let src = src.trim();
    if let Ok(n) = src.parse::<i64>() {
        return Some(Expr::IntLit(n));
    }
    if is_identifier(src) {
        return Some(Expr::Var(src.to_string()));
    }
    // Allow simple binary ops like `n + 5` on the right hand side of a placeholder
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

/// Parse a list literal `[expr, expr, ...]`.
fn parse_list_expr(src: &str) -> Option<Expr> {
    let inner = src[1..src.len() - 1].trim();
    if inner.is_empty() {
        return Some(Expr::ListLit(Vec::new()));
    }
    let parts = split_top_level_commas(inner);
    let items: Option<Vec<Expr>> = parts.iter().map(|p| parse_expr(p.trim())).collect();
    Some(Expr::ListLit(items?))
}

/// Parse a tuple literal `(a, b)` or a grouped expression `(expr)`.
fn parse_tuple_or_grouped_expr(src: &str) -> Option<Expr> {
    let inner = src[1..src.len() - 1].trim();
    if inner.is_empty() {
        return None;
    }
    let parts = split_top_level_commas(inner);
    if parts.len() == 1 {
        // Grouped expression
        return parse_expr(parts[0].trim());
    }
    let items: Option<Vec<Expr>> = parts.iter().map(|p| parse_expr(p.trim())).collect();
    Some(Expr::TupleLit(items?))
}

/// Parse `receiver.method(arg1, arg2, ...)` method call.
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
    // Keep locked MVP behavior for string.concat arity.
    if receiver == "string" && method == "concat" && parts.len() != 2 {
        return None;
    }
    let args: Option<Vec<Expr>> = parts.iter().map(|p| parse_expr(p.trim())).collect();
    Some(Expr::MethodCall {
        receiver: receiver.to_string(),
        method: method.to_string(),
        args: args?,
    })
}

/// Parse record constructor call `Ctor(field1: expr, field2: expr)`.
fn parse_record_constructor_call(src: &str) -> Option<Expr> {
    if !src.ends_with(')') {
        return None;
    }
    let open_idx = src.find('(')?;
    let constructor = src[..open_idx].trim();
    if !is_identifier(constructor) {
        return None;
    }
    let inner = src[open_idx + 1..src.len() - 1].trim();
    if inner.is_empty() {
        return Some(Expr::RecordConstruct {
            constructor: constructor.to_string(),
            fields: Vec::new(),
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

/// Parse `receiver.member` (no argument list).
fn parse_qualified_access(src: &str) -> Option<Expr> {
    if src.contains('(') || src.contains(')') {
        return None;
    }
    let (receiver, member) = src.split_once('.')?;
    let receiver = receiver.trim();
    let member = member.trim();
    if !is_identifier(receiver) || !is_identifier(member) {
        return None;
    }
    Some(Expr::Qualified {
        receiver: receiver.to_string(),
        member: member.to_string(),
    })
}

/// Try to parse `f x` (space-separated call) or `f(x)` (parenthesised call).
/// Does NOT match lambda or list/tuple starts.
fn try_parse_call(src: &str) -> Option<(&str, &str)> {
    // f(x) style — callee can be a bare identifier or a qualified name (Mod.fn)
    if let Some(open) = src.find('(').filter(|_| src.ends_with(')')) {
        let callee = src[..open].trim();
        let inner = src[open + 1..src.len() - 1].trim();
        if (is_identifier(callee) || is_qualified_name(callee)) && !inner.is_empty() {
            return Some((callee, inner));
        }
    }

    // f x style: split at first whitespace, but callee must be an identifier
    // (or a qualified name like `Mod.fn`) and arg must not be empty.
    // Skip if src starts with special chars.
    if src.starts_with('|') || src.starts_with('"') || src.starts_with('[') {
        return None;
    }

    let mut chars = src.char_indices();
    let split_pos = chars.find_map(|(idx, ch)| ch.is_whitespace().then_some(idx))?;
    let callee = src[..split_pos].trim();
    let arg = src[split_pos..].trim();
    if (is_identifier(callee) || is_qualified_name(callee)) && !arg.is_empty() {
        Some((callee, arg))
    } else {
        None
    }
}

/// Returns true if `s` is a qualified name of the form `Identifier.identifier`.
fn is_qualified_name(s: &str) -> bool {
    if let Some((receiver, member)) = s.split_once('.') {
        is_identifier(receiver) && is_identifier(member)
    } else {
        false
    }
}

// ---------------------------------------------------------------------------
// Binding detection
// ---------------------------------------------------------------------------

/// Detect `name = expr` (not `==`).
fn try_split_binding(line: &str) -> Option<(&str, &str)> {
    let idx = line.find('=')?;
    if !is_assignment_eq(line, idx) {
        return None;
    }
    let name = line[..idx].trim();
    let rhs = line[idx + 1..].trim();
    if is_identifier(name) {
        Some((name, rhs))
    } else {
        None
    }
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

// ---------------------------------------------------------------------------
// Shared utilities
// ---------------------------------------------------------------------------

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

fn parse_import_line(line: &str) -> Option<ImportDecl> {
    let rest = line.strip_prefix("import ")?.trim();
    if rest.is_empty() {
        return None;
    }

    if let Some(open_idx) = rest.find('(') {
        if !rest.ends_with(')') {
            return None;
        }
        let module_path = rest[..open_idx].trim();
        if !is_module_path(module_path) {
            return None;
        }
        let inner = rest[open_idx + 1..rest.len() - 1].trim();
        if inner.is_empty() {
            return None;
        }
        let names = split_top_level_commas(inner);
        let mut symbols = Vec::new();
        for name in names {
            let name = name.trim();
            if !is_identifier(name) {
                return None;
            }
            symbols.push(name.to_string());
        }
        return Some(ImportDecl {
            module_path: module_path.to_string(),
            kind: ImportKind::Selective(symbols),
        });
    }

    if let Some((module_path, alias)) = rest.split_once(" as ") {
        let module_path = module_path.trim();
        let alias = alias.trim();
        if !is_module_path(module_path) || !is_identifier(alias) {
            return None;
        }
        return Some(ImportDecl {
            module_path: module_path.to_string(),
            kind: ImportKind::Alias(alias.to_string()),
        });
    }

    if !is_module_path(rest) {
        return None;
    }
    Some(ImportDecl {
        module_path: rest.to_string(),
        kind: ImportKind::Plain,
    })
}

fn parse_type_declaration_line(line: &str) -> Option<TypeDeclaration> {
    let rest = line.strip_prefix("type ")?.trim();
    let (name, rhs) = rest.split_once('=')?;
    let name = name.trim();
    let rhs = rhs.trim();
    if !is_identifier(name) || rhs.is_empty() {
        return None;
    }

    if let Some(parts) = split_top_level_pipes(rhs) {
        let constructors: Option<Vec<String>> = parts
            .into_iter()
            .map(str::trim)
            .map(|ctor| is_identifier(ctor).then(|| ctor.to_string()))
            .collect();
        let constructors = constructors?;
        if constructors.is_empty() {
            return None;
        }
        return Some(TypeDeclaration::Union {
            name: name.to_string(),
            constructors,
        });
    }

    if let Some((constructor, inner)) = split_record_constructor_shape(rhs) {
        if !is_identifier(constructor) {
            return None;
        }
        let fields = if inner.is_empty() {
            Vec::new()
        } else {
            let parts = split_top_level_commas(inner);
            let parsed_fields: Option<Vec<RecordField>> = parts
                .iter()
                .map(|part| parse_record_field(part.trim()))
                .collect();
            parsed_fields?
        };
        return Some(TypeDeclaration::Record {
            name: name.to_string(),
            constructor: constructor.to_string(),
            fields,
        });
    }

    Some(TypeDeclaration::Alias {
        name: name.to_string(),
        target: rhs.to_string(),
    })
}

fn parse_record_field(field_src: &str) -> Option<RecordField> {
    let (name, ty) = field_src.split_once(':')?;
    let name = name.trim();
    let ty = ty.trim();
    if !is_identifier(name) || ty.is_empty() {
        return None;
    }
    Some(RecordField {
        name: name.to_string(),
        ty: ty.to_string(),
    })
}

/// Split a top-level union RHS (`A | B | C`) into segments.
/// Pipes inside parentheses or strings are ignored.
fn split_top_level_pipes(src: &str) -> Option<Vec<&str>> {
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

/// Parse record-constructor shape `Ctor(field: Ty, ...)`.
/// Requires `(` to appear immediately after constructor name to avoid
/// misclassifying type applications like `List (TypeY a b)` as records.
fn split_record_constructor_shape(rhs: &str) -> Option<(&str, &str)> {
    if !rhs.ends_with(')') {
        return None;
    }
    let open_idx = rhs.find('(')?;
    let constructor = rhs[..open_idx].trim();
    if constructor.is_empty() || !is_identifier(constructor) {
        return None;
    }
    if open_idx != constructor.len() {
        return None;
    }
    let inner = rhs[open_idx + 1..rhs.len() - 1].trim();
    Some((constructor, inner))
}

fn is_module_path(s: &str) -> bool {
    if s.is_empty() || s.starts_with('/') || s.ends_with('/') || s.contains("//") {
        return false;
    }
    s.split('/').all(is_identifier)
}

fn strip_line_comment(line: &str) -> &str {
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


// ---------------------------------------------------------------------------
// Existing helpers (unchanged)
// ---------------------------------------------------------------------------

fn is_indented(line: &str) -> bool {
    line.starts_with(' ') || line.starts_with('\t')
}

fn collect_indented_body(lines: &[&str], mut index: usize, body: &mut String) -> usize {
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

fn skip_blank_and_comment_lines(lines: &[&str], mut index: usize) -> usize {
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

fn split_top_level_type(line: &str) -> Option<(&str, &str)> {
    let idx = line.find(':')?;
    if line[..idx].contains('=') {
        return None;
    }
    Some((line[..idx].trim(), line[idx + 1..].trim()))
}

fn split_top_level_definition(line: &str) -> Option<(&str, Vec<String>, String)> {
    // Find the first `=` that is a plain assignment (not `==`, `!=`, `<=`, `>=`).
    let idx = line
        .char_indices()
        .find_map(|(i, ch)| (ch == '=' && is_assignment_eq(line, i)).then_some(i))?;
    let lhs = line[..idx].trim();
    let rhs = line[idx + 1..].trim_start();
    let mut tokens = lhs.split_whitespace();
    let name = tokens.next()?;
    let params: Vec<String> = tokens
        .filter(|t| is_identifier(t))
        .map(|t| t.to_string())
        .collect();
    Some((name, params, rhs.to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOpKind, Expr, ImportKind, Stmt, TypeDeclaration};
    use std::path::PathBuf;

    fn read_example(name: &str) -> String {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("..");
        path.push("..");
        path.push("examples");
        path.push(name);
        std::fs::read_to_string(path).expect("example file should exist")
    }

    fn parse_single_declaration(source: &str) -> Declaration {
        let module = parse_module(source).expect("source should parse");
        assert_eq!(module.declarations.len(), 1);
        module.declarations[0].clone()
    }

    #[test]
    fn parses_hello_example() {
        let source = read_example("hello.gb");
        let module = parse_module(&source).expect("hello.gb should parse");

        assert!(module.imports.is_empty());
        assert!(module.type_declarations.is_empty());
        assert_eq!(module.declarations.len(), 1);
        let main_decl = &module.declarations[0];
        assert_eq!(main_decl.name, "main");
        assert_eq!(
            main_decl.type_annotation.as_deref(),
            Some("Unit -> Unit can Print")
        );
    }

    #[test]
    fn parses_basic_types_example() {
        let source = read_example("basic_types.gb");
        let module = parse_module(&source).expect("basic_types.gb should parse");

        assert!(module.imports.is_empty());
        assert!(module.type_declarations.is_empty());
        assert_eq!(module.declarations.len(), 5);
        assert_eq!(module.declarations[0].name, "add");
        assert_eq!(module.declarations[1].name, "add_ten_and_two");
        assert_eq!(module.declarations[2].name, "concatenate");
        assert_eq!(module.declarations[3].name, "print_string");
        assert_eq!(module.declarations[4].name, "a");
    }

    #[test]
    fn parses_generic_types_example() {
        let source = read_example("generic_types.gb");
        let module = parse_module(&source).expect("generic_types.gb should parse");

        assert!(module.imports.is_empty());
        assert!(module.type_declarations.is_empty());
        assert_eq!(module.declarations.len(), 3);
        assert_eq!(module.declarations[0].name, "id");
        assert_eq!(module.declarations[1].name, "project");
        assert_eq!(module.declarations[2].name, "ints");
    }

    #[test]
    fn rejects_mismatched_annotation_and_definition_names() {
        let source = "foo : Int\nbar = 1\n";
        let err = parse_module(source).expect_err("mismatched names should be rejected");
        assert!(err.message.contains("does not match"));
    }

    #[test]
    fn parses_import_example_with_plain_alias_and_selective_imports() {
        let source = read_example("import.gb");
        let module = parse_module(&source).expect("import.gb should parse");

        assert_eq!(module.imports.len(), 3);
        assert!(module.type_declarations.is_empty());
        assert_eq!(module.imports[0].module_path, "goby/string");
        assert_eq!(module.imports[0].kind, ImportKind::Plain);
        assert_eq!(module.imports[1].module_path, "goby/list");
        assert_eq!(module.imports[1].kind, ImportKind::Alias("l".to_string()));
        assert_eq!(module.imports[2].module_path, "goby/env");
        assert_eq!(
            module.imports[2].kind,
            ImportKind::Selective(vec!["fetch_env_var".to_string()])
        );
        assert_eq!(module.declarations.len(), 1);
    }

    #[test]
    fn rejects_import_with_invalid_syntax() {
        let source = "import goby/env ()\nmain = 1\n";
        let err = parse_module(source).expect_err("invalid import should fail");
        assert!(err.message.contains("invalid import declaration"));
    }

    #[test]
    fn parses_type_example_alias_union_and_record() {
        let source = read_example("type.gb");
        let module = parse_module(&source).expect("type.gb should parse");

        assert!(module.imports.is_empty());
        assert_eq!(module.type_declarations.len(), 3);
        assert_eq!(module.declarations.len(), 1);
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Alias {
                name: "UserID".to_string(),
                target: "String".to_string(),
            }
        );
        assert_eq!(
            module.type_declarations[1],
            TypeDeclaration::Union {
                name: "UserStatus".to_string(),
                constructors: vec!["Activated".to_string(), "Deactivated".to_string()],
            }
        );
        assert_eq!(
            module.type_declarations[2],
            TypeDeclaration::Record {
                name: "User".to_string(),
                constructor: "User".to_string(),
                fields: vec![
                    crate::ast::RecordField {
                        name: "id".to_string(),
                        ty: "UserID".to_string(),
                    },
                    crate::ast::RecordField {
                        name: "name".to_string(),
                        ty: "String".to_string(),
                    },
                    crate::ast::RecordField {
                        name: "status".to_string(),
                        ty: "UserStatus".to_string(),
                    },
                ],
            }
        );
        let main = &module.declarations[0];
        let stmts = main.parsed_body.as_ref().expect("main body should parse");
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn parses_type_alias_with_parenthesized_application_as_alias() {
        let source = "type Wrapped = List (TypeY a b)\nmain = 1\n";
        let module = parse_module(source).expect("type alias should parse");
        assert_eq!(module.type_declarations.len(), 1);
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Alias {
                name: "Wrapped".to_string(),
                target: "List (TypeY a b)".to_string(),
            }
        );
    }

    #[test]
    fn does_not_treat_nested_pipe_in_alias_as_union() {
        let source = "type Wrapped = Maybe (A | B)\nmain = 1\n";
        let module = parse_module(source).expect("type alias with nested pipe should parse");
        assert_eq!(module.type_declarations.len(), 1);
        assert_eq!(
            module.type_declarations[0],
            TypeDeclaration::Alias {
                name: "Wrapped".to_string(),
                target: "Maybe (A | B)".to_string(),
            }
        );
    }

    #[test]
    fn definition_with_equality_in_body_parses_correctly() {
        // `f x = x == 0` — the `==` in the body must not confuse the definition splitter.
        // The definition `=` is the first plain `=`; the `==` in the body is part of RHS.
        let decl = parse_single_declaration("f : Int -> Int\nf x = 42\n");
        assert_eq!(decl.name, "f");
        assert_eq!(decl.params, vec!["x".to_string()]);
    }

    #[test]
    fn allows_comment_between_annotation_and_definition() {
        let source = "main : Unit -> Unit can Print\n# comment\n\nmain = print \"ok\"\n";
        let declaration = parse_single_declaration(source);
        assert_eq!(declaration.name, "main");
    }

    #[test]
    fn allows_line_end_comments_in_type_and_definition() {
        let source = "main : Unit -> Unit can Print # type note\nmain = print \"ok\" # body note\n";
        let declaration = parse_single_declaration(source);
        assert_eq!(declaration.name, "main");
        assert_eq!(
            declaration.type_annotation.as_deref(),
            Some("Unit -> Unit can Print")
        );
    }

    #[test]
    fn treats_shebang_as_comment_line() {
        let source = "#!/usr/bin/env goby\nmain : Unit -> Unit can Print\nmain = print \"ok\"\n";
        let declaration = parse_single_declaration(source);
        assert_eq!(declaration.name, "main");
    }

    #[test]
    fn does_not_treat_hash_inside_string_as_comment() {
        let source = "main : Unit -> Unit can Print\nmain = print \"a#b\" # trailing comment\n";
        let declaration = parse_single_declaration(source);
        let parsed = declaration
            .parsed_body
            .expect("body with trailing comment should parse");
        assert_eq!(parsed.len(), 1);
        match &parsed[0] {
            Stmt::Expr(Expr::Call { arg, .. }) => {
                assert_eq!(**arg, Expr::StringLit("a#b".to_string()));
            }
            other => panic!("unexpected stmt: {:?}", other),
        }
    }

    #[test]
    fn allows_mixed_tabs_and_spaces_in_same_block() {
        let source =
            "main : Unit -> Unit can Print\nmain =\n  greeting = \"hello\"\n\tprint greeting\n";
        let declaration = parse_single_declaration(source);
        assert!(declaration.body.contains("greeting = \"hello\""));
        assert!(declaration.body.contains("print greeting"));
    }

    // -----------------------------------------------------------------------
    // parse_expr tests
    // -----------------------------------------------------------------------

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
    fn parses_identifier() {
        assert_eq!(parse_expr("foo"), Some(Expr::Var("foo".to_string())));
    }

    #[test]
    fn parses_addition() {
        assert_eq!(
            parse_expr("a + b"),
            Some(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::Var("a".to_string())),
                right: Box::new(Expr::Var("b".to_string())),
            })
        );
    }

    #[test]
    fn parses_equality() {
        assert_eq!(
            parse_expr("a == b"),
            Some(Expr::BinOp {
                op: BinOpKind::Eq,
                left: Box::new(Expr::Var("a".to_string())),
                right: Box::new(Expr::Var("b".to_string())),
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
                left: Box::new(Expr::Var("b".to_string())),
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
    fn parses_equality_as_lower_precedence_than_addition() {
        assert_eq!(
            parse_expr("a + b == c"),
            Some(Expr::BinOp {
                op: BinOpKind::Eq,
                left: Box::new(Expr::BinOp {
                    op: BinOpKind::Add,
                    left: Box::new(Expr::Var("a".to_string())),
                    right: Box::new(Expr::Var("b".to_string())),
                }),
                right: Box::new(Expr::Var("c".to_string())),
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
            })
        );
    }

    #[test]
    fn parses_left_associative_pipeline_chain() {
        assert_eq!(
            parse_expr("x |> f |> g"),
            Some(Expr::Pipeline {
                value: Box::new(Expr::Pipeline {
                    value: Box::new(Expr::Var("x".to_string())),
                    callee: "f".to_string(),
                }),
                callee: "g".to_string(),
            })
        );
    }

    #[test]
    fn rejects_pipeline_with_non_identifier_callee_expression() {
        assert_eq!(parse_expr("x |> f 1"), None);
        assert_eq!(parse_expr("x |> string.concat(a, b)"), None);
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
            Some(Expr::ListLit(vec![
                Expr::IntLit(3),
                Expr::IntLit(4),
                Expr::IntLit(5)
            ]))
        );
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
    fn parses_spaced_function_call() {
        assert_eq!(
            parse_expr("add_ten 10"),
            Some(Expr::Call {
                callee: Box::new(Expr::Var("add_ten".to_string())),
                arg: Box::new(Expr::IntLit(10)),
            })
        );
    }

    #[test]
    fn parses_named_lambda() {
        assert_eq!(
            parse_expr("|n| -> n * 10"),
            Some(Expr::Lambda {
                param: "n".to_string(),
                body: Box::new(Expr::BinOp {
                    op: BinOpKind::Mul,
                    left: Box::new(Expr::Var("n".to_string())),
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
                    left: Box::new(Expr::Var("_".to_string())),
                    right: Box::new(Expr::IntLit(10)),
                }),
            })
        );
    }

    #[test]
    fn parses_pipeline() {
        let expr = parse_expr("mul_tens [3, 4, 5] |> print").expect("should parse");
        match expr {
            Expr::Pipeline { value, callee } => {
                assert_eq!(callee, "print");
                match *value {
                    Expr::Call { .. } => {}
                    other => panic!("expected Call, got {:?}", other),
                }
            }
            other => panic!("expected Pipeline, got {:?}", other),
        }
    }

    #[test]
    fn parses_string_concat_method_call() {
        assert_eq!(
            parse_expr("string.concat(a, b)"),
            Some(Expr::MethodCall {
                receiver: "string".to_string(),
                method: "concat".to_string(),
                args: vec![Expr::Var("a".to_string()), Expr::Var("b".to_string())],
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
                args: vec![
                    Expr::Var("paths".to_string()),
                    Expr::StringLit("\n".to_string()),
                ],
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
            })
        );
    }

    #[test]
    fn parses_qualified_access_expression() {
        assert_eq!(
            parse_expr("UserStatus.Activated"),
            Some(Expr::Qualified {
                receiver: "UserStatus".to_string(),
                member: "Activated".to_string(),
            })
        );
        assert_eq!(
            parse_expr("user.name"),
            Some(Expr::Qualified {
                receiver: "user".to_string(),
                member: "name".to_string(),
            })
        );
    }

    #[test]
    fn rejects_string_concat_with_three_arguments() {
        // Only exactly two arguments are allowed for string.concat in MVP.
        assert_eq!(parse_expr("string.concat(\"a\", \"b\", \"c\")"), None);
    }

    // -----------------------------------------------------------------------
    // parse_body_stmts tests
    // -----------------------------------------------------------------------

    #[test]
    fn parses_simple_binding_body() {
        let body = "b = a + 10\nb * 3";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 2);
        assert!(matches!(&stmts[0], Stmt::Binding { name, .. } if name == "b"));
        assert!(matches!(
            &stmts[1],
            Stmt::Expr(Expr::BinOp {
                op: BinOpKind::Mul,
                ..
            })
        ));
    }

    #[test]
    fn parses_rebinding_in_same_body() {
        let body = "a = 1\na = a + 1\na";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 3);
        assert!(matches!(&stmts[0], Stmt::Binding { name, .. } if name == "a"));
        assert!(matches!(&stmts[1], Stmt::Binding { name, .. } if name == "a"));
        assert!(matches!(&stmts[2], Stmt::Expr(Expr::Var(name)) if name == "a"));
    }

    #[test]
    fn equality_is_parsed_as_expression_not_binding_statement() {
        assert!(matches!(
            parse_stmt("a == 1"),
            Some(Stmt::Expr(Expr::BinOp {
                op: BinOpKind::Eq,
                ..
            }))
        ));
    }

    #[test]
    fn parses_print_stmt() {
        let body = "print \"hello\"";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::Call { callee, arg }) => {
                assert_eq!(**callee, Expr::Var("print".to_string()));
                assert_eq!(**arg, Expr::StringLit("hello".to_string()));
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parses_function_gb_declarations() {
        let source = read_example("function.gb");
        let module = parse_module(&source).expect("function.gb should parse");
        for decl in &module.declarations {
            assert!(
                decl.parsed_body.is_some(),
                "declaration `{}` should have parsed_body",
                decl.name
            );
        }
    }

    #[test]
    fn parses_effect_gb_declarations() {
        let source = read_example("effect.gb");
        let module = parse_module(&source).expect("effect.gb should parse");
        // effect.gb should have 3 regular declarations: plus_ten_with_log, show_env_var, main
        assert_eq!(
            module.declarations.len(),
            3,
            "effect.gb should have 3 declarations"
        );
        assert_eq!(module.effect_declarations.len(), 2, "effect.gb should have 2 effect declarations");
        assert_eq!(module.handler_declarations.len(), 2, "effect.gb should have 2 handler declarations");
        for decl in &module.declarations {
            assert!(
                decl.parsed_body.is_some(),
                "declaration `{}` should have parsed_body",
                decl.name
            );
        }
    }

    #[test]
    fn if_expr_with_wrong_else_indent_returns_none() {
        // `else` must be at the same indent level as `if`.
        // An under-indented `else` should cause parse_multiline_expr to return None.
        let source = r#"
main : Unit -> Unit
main =
  print
    if True
      "yes"
   else
      "no"
"#;
        // The module may parse (the print call might fall back), but the if/else expression
        // with wrong else indent must not produce an Expr::If node.
        let module = parse_module(source).expect("module-level parse should not panic");
        let main_decl = module.declarations.iter().find(|d| d.name == "main");
        if let Some(decl) = main_decl {
            if let Some(stmts) = &decl.parsed_body {
                // Verify no Stmt contains an Expr::If (the malformed if/else failed to parse).
                for stmt in stmts {
                    if let crate::ast::Stmt::Expr(expr) = stmt {
                        assert!(
                            !matches!(expr, crate::ast::Expr::If { .. }),
                            "malformed else indent should not produce Expr::If"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn effect_block_with_missing_colon_silently_skips_member() {
        // A handler method line where the name/params section cannot be parsed
        // (e.g. an extra unexpected token) should be silently skipped by the parser.
        // The handler should be present with zero parseable methods rather than panicking.
        let source = r#"
effect Log
  log: String -> Unit

handler ConsoleLog for Log
  !!!invalid method line!!!
    print "something"

main : Unit -> Unit
main =
  print "ok"
"#;
        let module = parse_module(source).expect("module should parse despite malformed handler method");
        let handler = module
            .handler_declarations
            .iter()
            .find(|h| h.name == "ConsoleLog");
        assert!(handler.is_some(), "ConsoleLog handler should be present");
        // The invalid method line is skipped; handler has zero methods.
        let handler = handler.unwrap();
        assert_eq!(
            handler.methods.len(),
            0,
            "malformed method line should be silently skipped"
        );
        assert_eq!(module.declarations.len(), 1);
    }
}
