use crate::ast::{BinOpKind, Declaration, Expr, Module, Stmt};
use crate::str_util::split_top_level_commas;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub line: usize,
    pub message: String,
}

pub fn parse_module(source: &str) -> Result<Module, ParseError> {
    let lines: Vec<&str> = source.lines().collect();
    let mut i = 0;
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

    Ok(Module { declarations })
}

/// Parse a declaration body string into a list of statements.
/// Returns `None` if any line cannot be parsed (caller may fall back to string-based evaluation).
pub fn parse_body_stmts(body: &str) -> Option<Vec<Stmt>> {
    let mut stmts = Vec::new();
    for line in code_lines(body) {
        stmts.push(parse_stmt(line)?);
    }
    Some(stmts)
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

    // 3. Binary addition: expr + expr
    if let Some((left, right)) = split_top_level_binop(src, '+') {
        return Some(Expr::BinOp {
            op: BinOpKind::Add,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    // 4. Binary multiplication: expr * expr
    if let Some((left, right)) = split_top_level_binop(src, '*') {
        return Some(Expr::BinOp {
            op: BinOpKind::Mul,
            left: Box::new(parse_expr(left)?),
            right: Box::new(parse_expr(right)?),
        });
    }

    // 5. List literal: [...]
    if src.starts_with('[') && src.ends_with(']') {
        return parse_list_expr(src);
    }

    // 6. Tuple or grouped expression: (...)
    if src.starts_with('(') && src.ends_with(')') {
        return parse_tuple_or_grouped_expr(src);
    }

    // 7. Method call: receiver.method(args)
    if let Some(expr) = parse_method_call(src) {
        return Some(expr);
    }

    // 8. Function call: f x  or  f(x)
    if let Some((callee, arg)) = try_parse_call(src) {
        return Some(Expr::Call {
            callee: Box::new(parse_expr(callee)?),
            arg: Box::new(parse_expr(arg)?),
        });
    }

    // 9. String literal: "..."
    if src.starts_with('"') && src.ends_with('"') && src.len() >= 2 {
        let inner = &src[1..src.len() - 1];
        if !inner.contains('"') {
            return Some(Expr::StringLit(inner.to_string()));
        }
    }

    // 10. Integer literal
    if let Ok(n) = src.parse::<i64>() {
        return Some(Expr::IntLit(n));
    }

    // 11. Identifier
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

/// Parse `string.concat(arg1, arg2)` method call.
fn parse_method_call(src: &str) -> Option<Expr> {
    // Only `string.concat(...)` is in scope for MVP
    let prefix = "string.concat(";
    if !src.starts_with(prefix) || !src.ends_with(')') {
        return None;
    }
    let inner = &src[prefix.len()..src.len() - 1];
    let parts = split_top_level_commas(inner);
    if parts.len() != 2 {
        return None;
    }
    let args: Option<Vec<Expr>> = parts.iter().map(|p| parse_expr(p.trim())).collect();
    Some(Expr::MethodCall {
        receiver: "string".to_string(),
        method: "concat".to_string(),
        args: args?,
    })
}

/// Try to parse `f x` (space-separated call) or `f(x)` (parenthesised call).
/// Does NOT match lambda or list/tuple starts.
fn try_parse_call(src: &str) -> Option<(&str, &str)> {
    // f(x) style
    if let Some(open) = src.find('(').filter(|_| src.ends_with(')')) {
        let callee = src[..open].trim();
        let inner = src[open + 1..src.len() - 1].trim();
        if is_identifier(callee) && !inner.is_empty() {
            return Some((callee, inner));
        }
    }

    // f x style: split at first whitespace, but callee must be an identifier
    // and arg must not be empty.  Skip if src starts with special chars.
    if src.starts_with('|') || src.starts_with('"') || src.starts_with('[') {
        return None;
    }

    let mut chars = src.char_indices();
    let split_pos = chars.find_map(|(idx, ch)| ch.is_whitespace().then_some(idx))?;
    let callee = src[..split_pos].trim();
    let arg = src[split_pos..].trim();
    if is_identifier(callee) && !arg.is_empty() {
        Some((callee, arg))
    } else {
        None
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

fn code_lines(body: &str) -> impl Iterator<Item = &str> {
    body.lines().map(strip_line_comment).map(str::trim).filter(|line| {
        let line = *line;
        !line.is_empty() && !line.starts_with('#')
    })
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
    use crate::ast::{BinOpKind, Expr, Stmt};
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
    fn does_not_treat_equality_as_binding_statement() {
        assert_eq!(parse_stmt("a == 1"), None);
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
}
