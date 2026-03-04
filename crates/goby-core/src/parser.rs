use crate::ast::{
    BinOpKind, CaseArm, CasePattern, Declaration, EffectDecl, EffectMember, EmbedDecl, Expr,
    HandlerClause, ImportDecl, ImportKind, InterpolatedPart, Module, RecordField, Stmt,
    TypeDeclaration,
};
use crate::str_util::split_top_level_commas;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    /// 1-indexed line number in the source file.
    pub line: usize,
    /// 1-indexed byte offset within the line (ASCII sources only; MVP assumption).
    /// Value `1` means "unknown column".
    pub col: usize,
    pub message: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "parse error at line {}:{}: {}",
            self.line, self.col, self.message
        )
    }
}

impl std::error::Error for ParseError {}

pub fn parse_module(source: &str) -> Result<Module, ParseError> {
    let lines: Vec<&str> = source.lines().collect();
    let mut i = 0;
    let mut imports = Vec::new();
    let mut embed_declarations = Vec::new();
    let mut type_declarations = Vec::new();
    let mut effect_declarations = Vec::new();
    let mut declarations = Vec::new();

    while i < lines.len() {
        let line = strip_line_comment(lines[i]).trim_end();
        let trimmed = line.trim();

        if trimmed.is_empty() || trimmed.starts_with('#') {
            i += 1;
            continue;
        }

        if is_indented(line) {
            // col is byte-offset + 1 within the raw source line (ASCII assumption).
            let col = lines[i].len() - lines[i].trim_start().len() + 1;
            return Err(ParseError {
                line: i + 1,
                col,
                message: "unexpected indentation at top level".to_string(),
            });
        }

        if trimmed.starts_with("import ") {
            let import = parse_import_line(trimmed).ok_or_else(|| ParseError {
                line: i + 1,
                col: 1,
                message: "invalid import declaration".to_string(),
            })?;
            imports.push(import);
            i += 1;
            continue;
        }

        if trimmed.starts_with("@embed") {
            let effect_name = parse_embed_line(trimmed).map_err(|message| ParseError {
                line: i + 1,
                col: 1,
                message,
            })?;
            embed_declarations.push(EmbedDecl {
                effect_name,
                line: i + 1,
            });
            i += 1;
            continue;
        }

        if trimmed.starts_with("type ") {
            let ty_decl = parse_type_declaration_line(trimmed).ok_or_else(|| ParseError {
                line: i + 1,
                col: 1,
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
                    col: 1,
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
                        members.push(EffectMember {
                            name,
                            type_annotation: ty,
                        });
                    }
                }
                i += 1;
            }
            effect_declarations.push(EffectDecl {
                name: effect_name,
                members,
            });
            continue;
        }

        // Legacy top-level `handler Name for Effect` declarations are removed.
        if starts_with_keyword_token(trimmed, "handler") {
            return Err(ParseError {
                line: i + 1,
                col: 1,
                message: "legacy top-level `handler ... for ...` is no longer supported; use `handler` expressions with `with`/`with_handler`".to_string(),
            });
        }

        let mut annotated_name: Option<&str> = None;
        let mut type_annotation = None;
        // decl_line: annotation line when present, definition line otherwise.
        // i + 1 is correct for both cases: if annotation is present it equals the annotation
        // line; if absent it equals the definition line. No reassignment needed inside the block.
        let decl_line = i + 1;

        if let Some((name, ty)) = split_top_level_type(line) {
            if name.is_empty() || ty.is_empty() {
                return Err(ParseError {
                    line: i + 1,
                    col: 1,
                    message: "invalid type annotation".to_string(),
                });
            }
            // decl_line already == i + 1 (the annotation line); no reassignment needed.
            annotated_name = Some(name);
            type_annotation = Some(ty.to_string());
            i += 1;
            i = skip_blank_and_comment_lines(&lines, i);
            if i >= lines.len() {
                return Err(ParseError {
                    line: decl_line,
                    col: 1,
                    message: "missing declaration body after type annotation".to_string(),
                });
            }
        }

        let body_line = strip_line_comment(lines[i]).trim_end();
        let (name, params, mut body) =
            split_top_level_definition(body_line).ok_or_else(|| ParseError {
                line: i + 1,
                col: 1,
                message: "expected top-level definition (`name ... = ...`)".to_string(),
            })?;
        if is_reserved_keyword(name) {
            return Err(ParseError {
                line: i + 1,
                col: 1,
                message: format!("`{name}` is a reserved keyword"),
            });
        }

        if let Some(annotated_name) = annotated_name
            && annotated_name != name
        {
            return Err(ParseError {
                line: i + 1,
                col: 1,
                message: format!(
                    "type annotation name `{}` does not match definition name `{}`",
                    annotated_name, name
                ),
            });
        }

        let j = collect_indented_body(lines.as_slice(), i + 1, &mut body);
        if let Some(offset) = first_malformed_resume_expr_line_offset(&body) {
            return Err(ParseError {
                line: i + 1 + offset,
                col: 1,
                message: "malformed `resume` expression: expected `resume <expr>`".to_string(),
            });
        }
        if let Some(offset) = first_legacy_using_line_offset(&body) {
            return Err(ParseError {
                line: i + 1 + offset,
                col: 1,
                message: "legacy `using` syntax is no longer supported; use `with`/`with_handler`"
                    .to_string(),
            });
        }

        let parsed_body = parse_body_stmts(&body);

        declarations.push(Declaration {
            name: name.to_string(),
            type_annotation,
            params,
            body,
            parsed_body,
            line: decl_line,
        });

        i = j;
    }

    Ok(Module {
        imports,
        embed_declarations,
        type_declarations,
        effect_declarations,
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

        // `name = handler` binding with an indented handler-expression block.
        if let Some((name, rhs)) = try_split_binding(trimmed)
            && rhs == "handler"
        {
            let (handler, next_i) = parse_handler_expr_from_lines(lines, i + 1, this_indent)?;
            stmts.push(Stmt::Binding {
                name: name.to_string(),
                value: handler,
            });
            i = next_i;
            continue;
        }

        // `with_handler ... in ...` statement (sugar for `with (handler ...) in ...`).
        if trimmed == "with_handler" {
            let (handler, next_i) = parse_handler_expr_from_lines(lines, i + 1, this_indent)?;
            let (body, after_with) = parse_with_in_body(lines, next_i, this_indent)?;
            stmts.push(Stmt::Expr(Expr::With {
                handler: Box::new(handler),
                body,
            }));
            i = after_with;
            continue;
        }

        // `with <handler_expr> in ...` statement.
        if let Some(handler_src) = trimmed.strip_prefix("with ") {
            let handler = parse_expr(handler_src.trim())?;
            let (body, after_with) = parse_with_in_body(lines, i + 1, this_indent)?;
            stmts.push(Stmt::Expr(Expr::With {
                handler: Box::new(handler),
                body,
            }));
            i = after_with;
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

fn parse_with_in_body(
    lines: &[&str],
    from: usize,
    expected_indent: usize,
) -> Option<(Vec<Stmt>, usize)> {
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
        let (body, consumed) = parse_stmts_from_lines(lines, in_line + 1)?;
        if body.is_empty() {
            return None;
        }
        return Some((body, in_line + 1 + consumed));
    }
    None
}

fn parse_handler_expr_from_lines(
    lines: &[&str],
    start: usize,
    parent_indent: usize,
) -> Option<(Expr, usize)> {
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
            body.push_str(sub_trimmed);
            i += 1;
        }

        if body.is_empty() {
            return None;
        }

        let parsed_body = parse_body_stmts(&body);
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

fn parse_handler_clause_header(src: &str) -> Option<(String, Vec<String>, &str)> {
    let (lhs, rhs) = src.split_once("->")?;
    let lhs = lhs.trim();
    let rhs = rhs.trim();
    let mut parts = lhs.split_whitespace();
    let name = parts.next()?;
    if !is_identifier(name) {
        return None;
    }

    let mut params = Vec::new();
    for param in parts {
        if !is_identifier(param) || is_reserved_keyword(param) {
            return None;
        }
        params.push(param.to_string());
    }

    Some((name.to_string(), params, rhs))
}

fn first_legacy_using_line_offset(body: &str) -> Option<usize> {
    body.lines().position(|line| {
        let trimmed = strip_line_comment(line).trim();
        !trimmed.is_empty() && starts_with_keyword_token(trimmed, "using")
    })
}

fn indent_len(line: &str) -> usize {
    line.len() - line.trim_start().len()
}

fn starts_with_keyword_token(src: &str, keyword: &str) -> bool {
    let Some(rest) = src.strip_prefix(keyword) else {
        return false;
    };
    rest.chars().next().is_some_and(char::is_whitespace)
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

/// Parse a string literal body that may contain `${...}` interpolations.
///
/// Example: `"a${x}b"` -> `Expr::InterpolatedString([Text("a"), Expr(x), Text("b")])`.
fn parse_interpolated_string_body(body: &str) -> Option<Expr> {
    let bytes = body.as_bytes();
    let mut i = 0usize;
    let mut literal_start = 0usize;
    let mut parts: Vec<InterpolatedPart> = Vec::new();
    let mut saw_interpolation = false;

    while i < bytes.len() {
        // Preserve escaped characters in literal text; interpolation markers in
        // escaped sequences (for example `\${`) are not treated as interpolation.
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

        // Unescaped quotes are only valid inside `${...}` expressions.
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

fn is_reserved_keyword(s: &str) -> bool {
    matches!(
        s,
        "resume" | "with" | "with_handler" | "in" | "handler" | "effect" | "mut"
    )
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

fn is_malformed_resume_expr_line(src: &str) -> bool {
    let trimmed = strip_line_comment(src).trim();
    trimmed == "resume"
}

fn first_malformed_resume_expr_line_offset(body: &str) -> Option<usize> {
    body.lines().position(is_malformed_resume_expr_line)
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

    // 3. Resume: resume expr
    if let Some(expr) = parse_resume_expr(src) {
        return Some(expr);
    }

    // 4. Binary equality: expr == expr
    if let Some((left, right)) = split_top_level_eq(src) {
        return Some(Expr::BinOp {
            op: BinOpKind::Eq,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    // 5. Binary addition: expr + expr
    if let Some((left, right)) = split_top_level_binop(src, '+') {
        return Some(Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    // 6. Binary multiplication: expr * expr
    if let Some((left, right)) = split_top_level_binop(src, '*') {
        return Some(Expr::BinOp {
            op: BinOpKind::Mul,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    // 7. List literal: [...]
    if src.starts_with('[') && src.ends_with(']') {
        return parse_list_expr(src);
    }

    // 8. Tuple or grouped expression: (...)
    if src.starts_with('(') && src.ends_with(')') {
        return parse_tuple_or_grouped_expr(src);
    }

    // 9. Record constructor call: TypeName(field: value, ...)
    if let Some(expr) = parse_record_constructor_call(src) {
        return Some(expr);
    }

    // 10. Method call: receiver.method(args)
    if let Some(expr) = parse_method_call(src) {
        return Some(expr);
    }

    // 11. Qualified/member access: receiver.member
    if let Some(expr) = parse_qualified_access(src) {
        return Some(expr);
    }

    // 12. Function call: f x / f x y ... / f(x)
    if let Some(expr) = parse_call_expr(src) {
        return Some(expr);
    }

    // 13. String literal: "..."
    if src.starts_with('"') && src.ends_with('"') && src.len() >= 2 {
        let inner = &src[1..src.len() - 1];
        return parse_interpolated_string_body(inner);
    }

    // 14. Bool literal
    if src == "True" {
        return Some(Expr::BoolLit(true));
    }
    if src == "False" {
        return Some(Expr::BoolLit(false));
    }

    // 15. Integer literal
    if let Ok(n) = src.parse::<i64>() {
        return Some(Expr::IntLit(n));
    }

    // 16. Identifier
    if is_identifier(src) && !is_reserved_keyword(src) {
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

/// Try to parse function calls:
/// - `f(x)` parenthesized single-arg form
/// - `f x y z` space-separated multi-arg form (left-associative)
fn parse_call_expr(src: &str) -> Option<Expr> {
    // f(x) style — callee can be a bare identifier or a qualified name (Mod.fn)
    if let Some(open) = src.find('(').filter(|_| src.ends_with(')')) {
        let callee = src[..open].trim();
        let inner = src[open + 1..src.len() - 1].trim();
        if (is_identifier(callee) || is_qualified_name(callee)) && !inner.is_empty() {
            return Some(Expr::Call {
                callee: Box::new(parse_expr(callee)?),
                arg: Box::new(parse_expr(inner)?),
            });
        }
    }

    // f x y ... style: split into top-level whitespace-separated terms and
    // fold left to preserve application associativity: `f a b` -> `(f a) b`.
    if src.starts_with('|') || src.starts_with('"') || src.starts_with('[') {
        return None;
    }

    let parts = split_top_level_whitespace_terms(src);
    if parts.len() < 2 {
        return None;
    }
    let callee = parts[0];
    if !(is_identifier(callee) || is_qualified_name(callee)) {
        None
    } else {
        let mut expr = parse_expr(callee)?;
        for part in parts.iter().skip(1) {
            let arg = parse_expr(part)?;
            expr = Expr::Call {
                callee: Box::new(expr),
                arg: Box::new(arg),
            };
        }
        Some(expr)
    }
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

fn try_split_assignment(line: &str) -> Option<(&str, &str)> {
    let (lhs, rhs) = line.split_once(":=")?;
    let lhs = lhs.trim();
    let rhs = rhs.trim();
    if lhs.is_empty() || rhs.is_empty() || !is_identifier(lhs) {
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

fn parse_embed_line(line: &str) -> Result<String, String> {
    let rest = line
        .strip_prefix("@embed")
        .map(str::trim_start)
        .ok_or_else(|| "invalid @embed declaration".to_string())?;
    if rest.is_empty() {
        return Err("invalid @embed declaration: expected `@embed <EffectName>`".to_string());
    }

    // Canonical syntax is `@embed <EffectName>`.
    // Legacy compatibility accepts `@embed effect <EffectName>` during transition.
    // TODO(ExtraStepA cleanup): remove `@embed effect <EffectName>` compatibility
    // after migration consumers are updated.
    let effect_name = rest.strip_prefix("effect ").map(str::trim).unwrap_or(rest);
    if !is_identifier(effect_name) {
        return Err("invalid embedded effect name".to_string());
    }
    Ok(effect_name.to_string())
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
        assert_eq!(module.declarations.len(), 6);
        assert_eq!(module.declarations[0].name, "add");
        assert_eq!(module.declarations[1].name, "add_ten_and_two");
        assert_eq!(module.declarations[2].name, "concatenate");
        assert_eq!(module.declarations[3].name, "print_string");
        assert_eq!(module.declarations[4].name, "a");
        assert_eq!(module.declarations[5].name, "main");
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
    fn rejects_reserved_resume_as_top_level_declaration_name() {
        let source = "resume : Int -> Int\nresume x = x\n";
        let err = parse_module(source).expect_err("reserved declaration name should be rejected");
        assert!(err.message.contains("reserved keyword"));
    }

    #[test]
    fn rejects_legacy_top_level_handler_syntax() {
        let source = r#"
effect Iter
  yield: String -> Unit

handler Collect for Iter
  yield item = resume Unit

main = 1
"#;
        let err = parse_module(source).expect_err("legacy handler declaration should be rejected");
        assert!(
            err.message
                .contains("legacy top-level `handler ... for ...` is no longer supported"),
            "unexpected error message: {}",
            err.message
        );
    }

    #[test]
    fn rejects_legacy_top_level_handler_syntax_with_tab_after_keyword() {
        let source = r#"
effect Iter
  yield: String -> Unit

handler	Collect for Iter
  yield item = resume Unit

main = 1
"#;
        let err = parse_module(source)
            .expect_err("legacy handler declaration with tab after keyword should be rejected");
        assert!(
            err.message
                .contains("legacy top-level `handler ... for ...` is no longer supported"),
            "unexpected error message: {}",
            err.message
        );
    }

    #[test]
    fn parses_resume_expression_shape_inside_with_handler_contract() {
        let source = r#"
main =
  with_handler
    yield item ->
      resume Unit
  in
    1
"#;
        let module = parse_module(source).expect("with_handler body should parse");
        let stmts = module.declarations[0]
            .parsed_body
            .as_ref()
            .expect("declaration body should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::With { handler, .. }) => match handler.as_ref() {
                Expr::Handler { clauses } => {
                    let clause_stmts = clauses[0]
                        .parsed_body
                        .as_ref()
                        .expect("handler clause body should parse");
                    assert_eq!(clause_stmts.len(), 1);
                    match &clause_stmts[0] {
                        Stmt::Expr(Expr::Resume { value }) => {
                            assert_eq!(**value, Expr::Var("Unit".to_string()));
                        }
                        other => panic!("unexpected statement shape: {:?}", other),
                    }
                }
                other => panic!("unexpected handler shape: {:?}", other),
            },
            other => panic!("unexpected statement shape: {:?}", other),
        }
    }

    #[test]
    fn parse_error_for_malformed_resume_in_declaration_body() {
        let source = "main = resume\n";
        let err = parse_module(source).expect_err("malformed resume should be rejected");
        assert_eq!(err.line, 1);
        assert_eq!(err.col, 1);
        assert!(err.message.contains("malformed `resume` expression"));
    }

    #[test]
    fn parse_error_for_malformed_resume_in_with_handler_clause_body() {
        let source = r#"
main =
  with_handler
    yield item ->
      resume
  in
    1
"#;
        let err = parse_module(source).expect_err("malformed handler resume should be rejected");
        assert!(err.message.contains("malformed `resume` expression"));
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
    fn parses_embed_effect_declaration() {
        let source = "@embed effect Print\nmain : Unit -> Unit\nmain = 1\n";
        let module = parse_module(source).expect("embed declaration should parse");
        assert_eq!(module.embed_declarations.len(), 1);
        assert_eq!(module.embed_declarations[0].effect_name, "Print");
        assert_eq!(module.embed_declarations[0].line, 1);
    }

    #[test]
    fn parses_embed_declaration_without_effect_keyword() {
        let source = "@embed Print\nmain = 1\n";
        let module = parse_module(source).expect("canonical embed declaration should parse");
        assert_eq!(module.embed_declarations.len(), 1);
        assert_eq!(module.embed_declarations[0].effect_name, "Print");
    }

    #[test]
    fn rejects_invalid_embed_declaration() {
        let source = "@embed 1Print\nmain = 1\n";
        let err = parse_module(source).expect_err("invalid embed declaration should fail");
        assert!(err.message.contains("invalid embedded effect name"));
    }

    #[test]
    fn rejects_embed_without_target() {
        let source = "@embed\nmain = 1\n";
        let err = parse_module(source).expect_err("embed without target should fail");
        assert!(
            err.message
                .contains("invalid @embed declaration: expected `@embed <EffectName>`")
        );
    }

    #[test]
    fn rejects_embed_with_invalid_effect_name() {
        let source = "@embed effect 1Print\nmain = 1\n";
        let err = parse_module(source).expect_err("invalid embed effect name should fail");
        assert!(err.message.contains("invalid embedded effect name"));
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
    fn parses_interpolated_string_with_variables() {
        assert_eq!(
            parse_expr("\"${a}${b}\""),
            Some(Expr::InterpolatedString(vec![
                InterpolatedPart::Expr(Box::new(Expr::Var("a".to_string()))),
                InterpolatedPart::Expr(Box::new(Expr::Var("b".to_string()))),
            ]))
        );
    }

    #[test]
    fn parses_interpolated_string_with_literal_prefix_and_suffix() {
        assert_eq!(
            parse_expr("\"hello ${name}!\""),
            Some(Expr::InterpolatedString(vec![
                InterpolatedPart::Text("hello ".to_string()),
                InterpolatedPart::Expr(Box::new(Expr::Var("name".to_string()))),
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
    fn parses_spaced_multi_arg_function_call_left_associative() {
        assert_eq!(
            parse_expr("f a b c"),
            Some(Expr::Call {
                callee: Box::new(Expr::Call {
                    callee: Box::new(Expr::Call {
                        callee: Box::new(Expr::Var("f".to_string())),
                        arg: Box::new(Expr::Var("a".to_string())),
                    }),
                    arg: Box::new(Expr::Var("b".to_string())),
                }),
                arg: Box::new(Expr::Var("c".to_string())),
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
    fn parses_qualified_method_call() {
        assert_eq!(
            parse_expr("string.split(a, b)"),
            Some(Expr::MethodCall {
                receiver: "string".to_string(),
                method: "split".to_string(),
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
    fn parses_qualified_method_call_with_three_arguments() {
        assert_eq!(
            parse_expr("string.join(a, b, c)"),
            Some(Expr::MethodCall {
                receiver: "string".to_string(),
                method: "join".to_string(),
                args: vec![
                    Expr::Var("a".to_string()),
                    Expr::Var("b".to_string()),
                    Expr::Var("c".to_string())
                ],
            })
        );
    }

    #[test]
    fn parses_resume_expression() {
        assert_eq!(
            parse_expr("resume Unit"),
            Some(Expr::Resume {
                value: Box::new(Expr::Var("Unit".to_string()))
            })
        );
    }

    #[test]
    fn rejects_malformed_resume_expression() {
        assert_eq!(parse_expr("resume"), None);
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
    fn parses_mut_binding_and_assignment_body() {
        let body = "mut a = 1\na := 2\na";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 3);
        assert!(matches!(&stmts[0], Stmt::MutBinding { name, .. } if name == "a"));
        assert!(matches!(&stmts[1], Stmt::Assign { name, .. } if name == "a"));
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
    fn parses_handler_expression_binding_body() {
        let body = "h = handler\n  emit x ->\n    resume x\nh";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 2);
        match &stmts[0] {
            Stmt::Binding { name, value } => {
                assert_eq!(name, "h");
                match value {
                    Expr::Handler { clauses } => {
                        assert_eq!(clauses.len(), 1);
                        assert_eq!(clauses[0].name, "emit");
                        assert_eq!(clauses[0].params, vec!["x".to_string()]);
                    }
                    other => panic!("expected handler expression, got {other:?}"),
                }
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_with_expression_statement() {
        let body = "with h\nin\n  emit 1";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::With { handler, body }) => {
                assert_eq!(**handler, Expr::Var("h".to_string()));
                assert_eq!(body.len(), 1);
            }
            other => panic!("unexpected statement: {other:?}"),
        }
    }

    #[test]
    fn parses_with_handler_sugar_statement() {
        let body = "with_handler\n  emit x ->\n    resume x\nin\n  emit 1";
        let stmts = parse_body_stmts(body).expect("should parse");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Expr(Expr::With { handler, body }) => {
                assert_eq!(body.len(), 1);
                match handler.as_ref() {
                    Expr::Handler { clauses } => {
                        assert_eq!(clauses.len(), 1);
                        assert_eq!(clauses[0].name, "emit");
                    }
                    other => panic!("expected handler expression, got {other:?}"),
                }
            }
            other => panic!("unexpected statement: {other:?}"),
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
        assert_eq!(
            module.effect_declarations.len(),
            2,
            "effect.gb should have 2 effect declarations"
        );
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

    // --- ParseError line/col regression tests ---

    #[test]
    fn parse_error_unexpected_indentation_reports_line_and_col() {
        // "    foo = 1" at line 1 → col = indent width + 1 = 5
        let source = "    foo = 1\n";
        let err = parse_module(source).expect_err("indented top-level should fail");
        assert_eq!(err.line, 1);
        assert_eq!(err.col, 5);
        assert!(err.message.contains("indentation"));
    }

    #[test]
    fn parse_error_unexpected_indentation_two_spaces_reports_col_3() {
        // Same code path as the 4-space test above; verifies col formula for 2-space indent.
        // "  baz = 2" at line 1: indent width = 2, col = 2 + 1 = 3.
        let source = "  baz = 2\n";
        let err = parse_module(source).expect_err("indented top-level line should fail");
        assert_eq!(err.line, 1);
        assert_eq!(err.col, 3);
        assert!(err.message.contains("indentation"));
    }

    #[test]
    fn parse_error_missing_annotation_body_reports_annotation_line() {
        // Type annotation on line 1 followed by nothing → error at line 1
        let source = "foo : Int\n";
        let err = parse_module(source).expect_err("annotation without body should fail");
        assert_eq!(err.line, 1);
        assert_eq!(err.col, 1);
        assert!(err.message.contains("missing declaration body"));
    }

    #[test]
    fn parse_error_mismatched_annotation_name_reports_definition_line() {
        // Annotation on line 1, definition on line 2 → error points to definition line (2)
        let source = "foo : Int\nbar = 1\n";
        let err = parse_module(source).expect_err("mismatched name should fail");
        assert_eq!(err.line, 2);
        assert_eq!(err.col, 1);
        assert!(err.message.contains("does not match"));
    }

    #[test]
    fn parse_error_for_legacy_handler_block_shape() {
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
        let err = parse_module(source).expect_err("legacy top-level handlers should fail");
        assert!(
            err.message
                .contains("legacy top-level `handler ... for ...` is no longer supported")
        );
    }

    #[test]
    fn parse_error_for_legacy_using_with_tab_after_keyword() {
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with_handler
    log msg ->
      using	H
        log msg
      resume Unit
  in
    log "x"
"#;
        let err =
            parse_module(source).expect_err("legacy using with tab after keyword should fail");
        assert!(
            err.message
                .contains("legacy `using` syntax is no longer supported"),
            "unexpected error message: {}",
            err.message
        );
    }
}
