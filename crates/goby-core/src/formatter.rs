//! AST pretty-printer for Goby source files.
//!
//! # Comment policy (Option A)
//! Comments are stripped during parsing and do not appear in formatter output.
//! This is intentional: use `goby fmt` to normalise whitespace and layout,
//! not to preserve inline documentation.  If comment preservation is required
//! in the future it will be implemented as a separate Track (CST-level).
//!
//! # Style rules
//! - 2-space indentation per nesting level.
//! - One blank line between top-level declarations.
//! - Trailing newline on the last line.
//! - No trailing whitespace on any line.
//!
//! # Indentation convention
//! All internal `format_*` functions return strings **without** a leading indent
//! prefix.  The `indent` parameter controls the *internal* indentation of nested
//! sub-structures (e.g. case arms, handler clause bodies), but the caller is
//! responsible for prepending the leading pad to the entire returned string via
//! `apply_indent`.
//!
//! The top-level entry point `format_module` assembles sections with their
//! correct column offsets.

use crate::ast::{
    BinOpKind, CaseArm, CasePattern, Declaration, EffectDecl, EmbedDecl, Expr, HandlerClause,
    ImportDecl, ImportKind, InterpolatedPart, ListPatternItem, ListPatternTail, Module,
    RecordField, Stmt, TypeDeclaration, UnaryOpKind,
};

const INDENT: &str = "  ";

fn indent_str(level: usize) -> String {
    INDENT.repeat(level)
}

/// Re-escape a string value that was unescaped by the parser.
/// Reverses `unescape_string`: `\n` → `\\n`, `\t` → `\\t`, `\\` → `\\\\`, `"` → `\\"`.
fn escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            c => out.push(c),
        }
    }
    out
}

/// Indent every line of `s` by `level` levels, returning the indented string.
/// Empty lines are left empty (no trailing whitespace).
fn apply_indent(s: &str, level: usize) -> String {
    if level == 0 {
        return s.to_string();
    }
    let pad = indent_str(level);
    s.lines()
        .map(|line| {
            if line.is_empty() {
                String::new()
            } else {
                format!("{}{}", pad, line)
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

// ---------------------------------------------------------------------------
// Expr formatting
// ---------------------------------------------------------------------------

/// Format an expression.
///
/// Returns a string with **no leading indent** on the first line.
/// Internal nesting (e.g. case arms, handler clauses) is indented relative to
/// the natural column where the expression begins, using `indent` as the base
/// level for internal sub-structures.
pub(crate) fn format_expr(expr: &Expr, indent: usize) -> String {
    match expr {
        Expr::IntLit(n) => n.to_string(),
        Expr::BoolLit(v) => if *v { "True" } else { "False" }.to_string(),
        Expr::StringLit(s) => format!("\"{}\"", escape_string(s)),
        Expr::InterpolatedString(parts) => {
            let mut out = String::from("\"");
            for part in parts {
                match part {
                    InterpolatedPart::Text(t) => out.push_str(&escape_string(t)),
                    InterpolatedPart::Expr(e) => {
                        out.push_str("${");
                        out.push_str(&format_expr(e, indent));
                        out.push('}');
                    }
                }
            }
            out.push('"');
            out
        }
        Expr::ListLit { elements, spread } => {
            let mut parts: Vec<String> = elements.iter().map(|e| format_expr(e, indent)).collect();
            if let Some(tail) = spread {
                parts.push(format!("..{}", format_expr(tail, indent)));
            }
            format!("[{}]", parts.join(", "))
        }
        Expr::TupleLit(items) => {
            let parts: Vec<String> = items.iter().map(|e| format_expr(e, indent)).collect();
            format!("({})", parts.join(", "))
        }
        Expr::Var { name, .. } => name.clone(),
        Expr::Qualified {
            receiver, member, ..
        } => format!("{}.{}", receiver, member),
        Expr::RecordConstruct {
            constructor,
            fields,
            ..
        } => {
            let field_strs: Vec<String> = fields
                .iter()
                .map(|(name, val)| format!("{}: {}", name, format_expr(val, indent)))
                .collect();
            format!("{}({})", constructor, field_strs.join(", "))
        }
        Expr::UnaryOp { op, expr } => {
            let op_str = match op {
                UnaryOpKind::Not => "!",
            };
            let inner_raw = format_expr(expr, indent);
            let inner = if expr.needs_parens_as_subexpr() {
                format!("({})", inner_raw)
            } else {
                inner_raw
            };
            format!("{}{}", op_str, inner)
        }
        Expr::BinOp { op, left, right } => {
            let op_str = match op {
                BinOpKind::Or => "||",
                BinOpKind::And => "&&",
                BinOpKind::Add => "+",
                BinOpKind::Sub => "-",
                BinOpKind::Mul => "*",
                BinOpKind::Div => "/",
                BinOpKind::Mod => "%",
                BinOpKind::Eq => "==",
                BinOpKind::Lt => "<",
                BinOpKind::Gt => ">",
                BinOpKind::Le => "<=",
                BinOpKind::Ge => ">=",
            };
            let l_raw = format_expr(left, indent);
            let r_raw = format_expr(right, indent);
            let l = if left.needs_parens_as_subexpr() {
                format!("({})", l_raw)
            } else {
                l_raw
            };
            let r = if right.needs_parens_as_subexpr() {
                format!("({})", r_raw)
            } else {
                r_raw
            };
            format!("{} {} {}", l, op_str, r)
        }
        Expr::Call { callee, arg, .. } => {
            let c = format_expr(callee, indent);
            let a_raw = format_expr(arg, indent);
            let a = if arg.needs_parens_as_subexpr() {
                format!("({})", a_raw)
            } else {
                a_raw
            };
            format!("{} {}", c, a)
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            let arg_strs: Vec<String> = args.iter().map(|a| format_expr(a, indent)).collect();
            format!("{}.{}({})", receiver, method, arg_strs.join(", "))
        }
        Expr::Pipeline { value, callee, .. } => {
            let v = format_expr(value, indent);
            format!("{} |> {}", v, callee)
        }
        Expr::Lambda { param, body } => {
            if param == "_" {
                // Placeholder form: `_ * 10` — the body already encodes the op + rhs.
                format_expr(body, indent)
            } else if matches!(body.as_ref(), Expr::Lambda { param: p, .. } if p != "_") {
                // Multi-parameter lambda: collect all params and emit `fn a b -> expr`.
                let mut params = vec![param.as_str()];
                let mut cur: &Expr = body;
                while let Expr::Lambda { param: p, body: b } = cur {
                    if p == "_" {
                        break;
                    }
                    params.push(p.as_str());
                    cur = b;
                }
                let final_body = format_expr(cur, indent);
                format!("fn {} -> {}", params.join(" "), final_body)
            } else {
                let b = format_expr(body, indent);
                format!("|{}| -> {}", param, b)
            }
        }
        Expr::Handler { clauses } => format_handler_expr(clauses, indent),
        Expr::With { handler, body } => format_with_expr(handler, body, indent),
        Expr::Resume { value } => {
            let v_raw = format_expr(value, indent);
            let v = if value.needs_parens_as_subexpr() {
                format!("({})", v_raw)
            } else {
                v_raw
            };
            format!("resume {}", v)
        }
        Expr::Block(stmts) => format_stmts(stmts, indent),
        Expr::Case { scrutinee, arms } => format_case_expr(scrutinee, arms, indent),
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => format_if_expr(condition, then_expr, else_expr, indent),
        Expr::ListIndex { list, index } => {
            let l_raw = format_expr(list, indent);
            let i = format_expr(index, indent);
            // Chained ListIndex does not need extra parens; other complex sub-exprs do.
            let l = if list.needs_parens_as_subexpr() && !matches!(**list, Expr::ListIndex { .. }) {
                format!("({})", l_raw)
            } else {
                l_raw
            };
            format!("{}[{}]", l, i)
        }
    }
}

/// Format a `case` expression.
///
/// Output (no leading indent on first line):
/// ```text
/// case <scrutinee>
///   <pat1> -> <body1>
///   <pat2> ->
///     <multi-line body2 line 1>
///     <multi-line body2 line 2>
/// ```
fn format_case_expr(scrutinee: &Expr, arms: &[CaseArm], indent: usize) -> String {
    let scrutinee_str = format_expr(scrutinee, indent);
    let arm_pad = INDENT; // one level relative to the "case" keyword
    let body_base = indent + 2; // absolute indent for arm bodies

    let mut out = format!("case {}", scrutinee_str);
    for arm in arms {
        let pat = format_pattern(&arm.pattern);
        let body_str = format_expr(&arm.body, body_base);
        if body_str.contains('\n') {
            // Multi-line arm body: put it on the next line, indented one more level.
            let indented_body = apply_indent(&body_str, indent + 2);
            out.push_str(&format!("\n{}{} ->\n{}", arm_pad, pat, indented_body));
        } else {
            out.push_str(&format!("\n{}{} -> {}", arm_pad, pat, body_str));
        }
    }
    out
}

/// Format an `if` expression.
///
/// Output (no leading indent on first line):
/// ```text
/// if <cond>
///   <then>
/// else
///   <else>
/// ```
fn format_if_expr(condition: &Expr, then_expr: &Expr, else_expr: &Expr, indent: usize) -> String {
    let cond = format_expr(condition, indent);
    // Format sub-expressions at indent+1, then indent the whole result.
    let then_str = format_expr(then_expr, indent + 1);
    let else_str = format_expr(else_expr, indent + 1);
    // Indent multi-line sub-expressions by one more level.
    let then_indented = apply_indent(&then_str, 1);
    let else_indented = apply_indent(&else_str, 1);
    format!("if {}\n{}\nelse\n{}", cond, then_indented, else_indented)
}

/// Format a `with … in` expression.
///
/// Two output forms depending on the handler expression:
///
/// (a) Inline handler (non-`Handler` expression, e.g. a variable `h`):
/// ```text
/// with <handler_expr>
/// in
///   <body stmt>
/// ```
///
/// (b) Anonymous handler block (`Expr::Handler { clauses }`):
/// ```text
/// with
///   <clause1_name> <params> ->
///     <body1>
/// in
///   <body stmt>
/// ```
///
/// Form (b) matches the `trimmed == "with"` parse path so the output is
/// round-trip safe.  Form (a) uses `with <expr>` which the parser handles via
/// `strip_prefix("with ")`.
fn format_with_expr(handler: &Expr, body: &[Stmt], indent: usize) -> String {
    let body_str = format_stmts(body, indent + 1);
    let body_indented = apply_indent(&body_str, 1);

    match handler {
        Expr::Handler { clauses } => {
            // Emit `with` alone on its line; clause block is one level deeper.
            let clauses_str = format_handler_clauses_block(clauses, indent + 1);
            format!("with\n{}\nin\n{}", clauses_str, body_indented)
        }
        _ => {
            let handler_str = format_expr(handler, indent + 1);
            format!("with {}\nin\n{}", handler_str, body_indented)
        }
    }
}

/// Format a `with … in` expression in **RHS** (binding value) position.
///
/// The `with` keyword is placed inline (no leading `\n`).  The result can be
/// used directly after `name = ` in `format_binding_stmt`.
///
/// - `With { handler: Handler { clauses }, body }` →
///   ```text
///   with
///     <clause1> ->
///       <body1>
///   in
///     <body_stmt>
///   ```
///   Parser picks this up via `parse_multiline_rhs_expr` with `rhs == "with"`.
///
/// - `With { handler: var/expr, body }` →
///   ```text
///   with <handler_expr>
///   in
///     <body_stmt>
///   ```
///   Parser picks this up via `parse_multiline_rhs_expr` with `rhs.starts_with("with ")`.
fn format_with_expr_rhs(handler: &Expr, body: &[Stmt], indent: usize) -> String {
    let pad = indent_str(indent);
    let body_str = format_stmts(body, indent + 1);
    let body_indented = apply_indent(&body_str, indent + 1);
    match handler {
        Expr::Handler { clauses } => {
            let clauses_str = format_handler_clauses_block(clauses, indent + 1);
            format!("with\n{}\n{}in\n{}", clauses_str, pad, body_indented)
        }
        _ => {
            let handler_str = format_expr(handler, indent + 1);
            format!("with {}\n{}in\n{}", handler_str, pad, body_indented)
        }
    }
}

/// Format handler clauses as an indented block (used by `format_with_expr` form b).
///
/// Each clause is output at `indent` level:
/// ```text
/// <indent><name> <params> ->
/// <indent+1><body>
/// ```
fn format_handler_clauses_block(clauses: &[HandlerClause], indent: usize) -> String {
    let mut lines: Vec<String> = Vec::new();
    let pad = indent_str(indent);
    for clause in clauses {
        let params_str = if clause.params.is_empty() {
            String::new()
        } else {
            format!(" {}", clause.params.join(" "))
        };
        lines.push(format!("{}{}{} ->", pad, clause.name, params_str));
        if let Some(stmts) = &clause.parsed_body {
            let body_str = format_stmts(stmts, indent + 1);
            lines.push(body_str);
        } else {
            let body_trimmed = clause.body.trim();
            if !body_trimmed.is_empty() {
                let min_indent = body_trimmed
                    .lines()
                    .filter(|l| !l.trim().is_empty())
                    .map(|l| l.len() - l.trim_start().len())
                    .min()
                    .unwrap_or(0);
                for line in body_trimmed.lines() {
                    if line.trim().is_empty() {
                        lines.push(String::new());
                    } else {
                        let stripped = &line[min_indent.min(line.len())..];
                        lines.push(format!("{}{}", indent_str(indent + 1), stripped.trim_end()));
                    }
                }
            }
        }
    }
    lines.join("\n")
}

/// Format a `handler` expression.
///
/// Output (no leading indent on first line):
/// ```text
/// handler
///   <clause1_name> <params> ->
///     <body1>
///   <clause2_name> <params> ->
///     <body2>
/// ```
fn format_handler_expr(clauses: &[HandlerClause], indent: usize) -> String {
    let mut out = String::from("handler");
    for clause in clauses {
        let params_str = if clause.params.is_empty() {
            String::new()
        } else {
            format!(" {}", clause.params.join(" "))
        };
        out.push_str(&format!("\n  {}{} ->", clause.name, params_str));
        if let Some(stmts) = &clause.parsed_body {
            let body_str = format_stmts(stmts, indent + 2);
            let body_indented = apply_indent(&body_str, 2);
            out.push('\n');
            out.push_str(&body_indented);
        } else {
            // Fallback to raw body string: trim trailing whitespace per line,
            // preserve relative indentation by using trim_end (not trim).
            let body_trimmed = clause.body.trim();
            if !body_trimmed.is_empty() {
                // Determine minimum indentation to normalize.
                let min_indent = body_trimmed
                    .lines()
                    .filter(|l| !l.trim().is_empty())
                    .map(|l| l.len() - l.trim_start().len())
                    .min()
                    .unwrap_or(0);
                for line in body_trimmed.lines() {
                    out.push('\n');
                    if line.trim().is_empty() {
                        // empty line — no whitespace
                    } else {
                        let stripped = &line[min_indent.min(line.len())..];
                        out.push_str(&format!("    {}", stripped.trim_end()));
                    }
                }
            }
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Pattern formatting
// ---------------------------------------------------------------------------

fn format_pattern(pat: &CasePattern) -> String {
    match pat {
        CasePattern::IntLit(n) => n.to_string(),
        CasePattern::StringLit(s) => format!("\"{}\"", escape_string(s)),
        CasePattern::BoolLit(v) => if *v { "True" } else { "False" }.to_string(),
        CasePattern::EmptyList => "[]".to_string(),
        CasePattern::ListPattern { items, tail } => {
            let mut parts: Vec<String> = items
                .iter()
                .map(|item| match item {
                    ListPatternItem::IntLit(n) => n.to_string(),
                    ListPatternItem::StringLit(s) => format!("\"{}\"", escape_string(s)),
                    ListPatternItem::Bind(name) => name.clone(),
                    ListPatternItem::Wildcard => "_".to_string(),
                })
                .collect();
            if let Some(t) = tail {
                match t {
                    ListPatternTail::Ignore => parts.push("..".to_string()),
                    ListPatternTail::Bind(name) => parts.push(format!("..{}", name)),
                }
            }
            format!("[{}]", parts.join(", "))
        }
        CasePattern::Wildcard => "_".to_string(),
    }
}

// ---------------------------------------------------------------------------
// Stmt formatting
// ---------------------------------------------------------------------------

/// Returns true if `expr` is a simple callee that the parser recognises in
/// the `identifier\n  case/if` multi-line call pattern (Pattern A).
fn is_simple_callee(expr: &Expr) -> bool {
    matches!(expr, Expr::Var { .. } | Expr::Qualified { .. })
}

/// Format an expression as a top-level statement line (with leading indent).
///
/// For `Call { callee: identifier, arg: Case/If }` we emit the parser-compatible
/// "Pattern A" form:
/// ```text
/// <indent>callee
/// <indent>  case scrutinee
/// <indent>    arm -> body
/// ```
/// All other expressions fall through to `apply_indent(format_expr(...), indent)`.
fn format_expr_stmt(expr: &Expr, indent: usize) -> String {
    if let Expr::Call { callee, arg, .. } = expr
        && is_simple_callee(callee)
        && matches!(**arg, Expr::Case { .. } | Expr::If { .. })
    {
        let callee_str = format_expr(callee, indent);
        let arg_str = format_expr(arg, indent + 1);
        let arg_indented = apply_indent(&arg_str, indent + 1);
        let pad = indent_str(indent);
        return format!("{}{}\n{}", pad, callee_str, arg_indented);
    }
    let expr_str = format_expr(expr, indent);
    apply_indent(&expr_str, indent)
}

pub(crate) fn format_stmt(stmt: &Stmt, indent: usize) -> String {
    let pad = indent_str(indent);
    match stmt {
        Stmt::Binding { name, value, .. } => {
            let val_str = format_expr_rhs(value, indent);
            format_binding_stmt(&pad, name, "=", &val_str)
        }
        Stmt::MutBinding { name, value, .. } => {
            let val_str = format_expr_rhs(value, indent);
            format_binding_stmt(&pad, &format!("mut {}", name), "=", &val_str)
        }
        Stmt::Assign { name, value, .. } => {
            let val_str = format_expr_rhs(value, indent);
            format_binding_stmt(&pad, name, ":=", &val_str)
        }
        Stmt::Expr(expr, _) => format_expr_stmt(expr, indent),
    }
}

/// Format `<pad><name> <op> <val_str>` without trailing whitespace before `val_str`.
///
/// When `val_str` starts with `\n` (multi-line RHS), emits `<pad><name> <op>\n...`
/// (no space before the newline).  Otherwise emits `<pad><name> <op> <val>`.
fn format_binding_stmt(pad: &str, name: &str, op: &str, val_str: &str) -> String {
    if val_str.starts_with('\n') {
        format!("{}{} {}{}", pad, name, op, val_str)
    } else {
        format!("{}{} {} {}", pad, name, op, val_str)
    }
}

/// Format an expression in RHS position (after `=`, `:=`).
///
/// Returns a string suitable for placing after `name = ` (no leading indent).
/// Multi-line output has the first token on the same line and subsequent lines
/// relative to `indent` so that the resulting stmt is round-trip parseable.
///
/// Layout rules:
/// - `Block` → newline + indented body
/// - `Case`  → `"case scrutinee\n  arm"` (inline, parser sees `name = case …`)
/// - `If`    → `"if cond\n  then\nelse\n  else_"` (inline, `else` at `indent`)
/// - `Handler` → newline + `handler\n  clauses` (rhs == "handler" parse path)
/// - `With`  → newline + `with\n  clauses\nin\n  body`
/// - other   → inline single-line expression
fn format_expr_rhs(expr: &Expr, indent: usize) -> String {
    match expr {
        Expr::Block(stmts) => {
            let body = format_stmts(stmts, indent + 1);
            format!("\n{}", body)
        }
        Expr::Case { scrutinee, arms } => {
            // Emit inline: `case scrutinee\n  arm -> body`.
            // Arms are indented one extra level relative to `indent` so that
            // `parse_multiline_rhs_expr` (which prefixes line_indent spaces) sees
            // the correct indentation.
            format_case_expr_rhs(scrutinee, arms, indent)
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            // Emit inline: `if cond\n  then\n<indent_pad>else\n  else_`.
            // The `else` keyword is at `indent` so that `parse_multiline_rhs_expr`
            // (which prefixes the binding line's indent) places it correctly.
            format_if_expr_rhs(condition, then_expr, else_expr, indent)
        }
        Expr::Handler { clauses } => {
            // `name = handler` on same line; parser special-cases rhs == "handler".
            // Clause block is one level deeper than the binding.
            let clauses_str = format_handler_clauses_block(clauses, indent + 1);
            if clauses_str.is_empty() {
                "handler".to_string()
            } else {
                format!("handler\n{}", clauses_str)
            }
        }
        Expr::With { handler, body } => {
            // `name = with` on same line; parser handles rhs == "with".
            // format_with_expr_rhs keeps `with` inline (no leading \n).
            format_with_expr_rhs(handler, body, indent)
        }
        _ => format_expr(expr, indent),
    }
}

/// Format a `case` expression as an inline RHS value.
///
/// Returns `"case scrutinee\n  arm -> body"` with arm bodies indented relative
/// to `indent + 1`.  No leading indent on the first line.
fn format_case_expr_rhs(scrutinee: &Expr, arms: &[CaseArm], indent: usize) -> String {
    let scrutinee_str = format_expr(scrutinee, indent);
    let body_base = indent + 2;
    let mut out = format!("case {}", scrutinee_str);
    for arm in arms {
        let pat = format_pattern(&arm.pattern);
        let body_str = format_expr(&arm.body, body_base);
        if body_str.contains('\n') {
            let indented_body = apply_indent(&body_str, indent + 2);
            out.push_str(&format!("\n  {} ->\n{}", pat, indented_body));
        } else {
            out.push_str(&format!("\n  {} -> {}", pat, body_str));
        }
    }
    out
}

/// Format an `if` expression as an inline RHS value.
///
/// Returns `"if cond\n  then\n<pad>else\n  else_"` where `<pad>` = `indent_str(indent)`.
/// This ensures the `else` keyword is at column `indent` so that
/// `parse_multiline_rhs_expr` can find it.  No leading indent on the first line.
fn format_if_expr_rhs(
    condition: &Expr,
    then_expr: &Expr,
    else_expr: &Expr,
    indent: usize,
) -> String {
    let cond = format_expr(condition, indent);
    let then_str = format_expr(then_expr, indent + 1);
    let else_str = format_expr(else_expr, indent + 1);
    let then_indented = apply_indent(&then_str, indent + 1);
    let else_indented = apply_indent(&else_str, indent + 1);
    let else_pad = indent_str(indent);
    format!(
        "if {}\n{}\n{}else\n{}",
        cond, then_indented, else_pad, else_indented
    )
}

pub(crate) fn format_stmts(stmts: &[Stmt], indent: usize) -> String {
    stmts
        .iter()
        .map(|s| format_stmt(s, indent))
        .collect::<Vec<_>>()
        .join("\n")
}

// ---------------------------------------------------------------------------
// Top-level declaration formatting
// ---------------------------------------------------------------------------

pub(crate) fn format_import_decl(imp: &ImportDecl) -> String {
    match &imp.kind {
        ImportKind::Plain => format!("import {}", imp.module_path),
        ImportKind::Alias(alias) => format!("import {} as {}", imp.module_path, alias),
        ImportKind::Selective(names) => {
            format!("import {} ( {} )", imp.module_path, names.join(", "))
        }
    }
}

pub(crate) fn format_embed_decl(ed: &EmbedDecl) -> String {
    format!("@embed {} {}", ed.effect_name, ed.handler_name)
}

pub(crate) fn format_type_declaration(td: &TypeDeclaration) -> String {
    match td {
        TypeDeclaration::Alias { name, target } => {
            format!("type {} = {}", name, target)
        }
        TypeDeclaration::Union { name, constructors } => {
            format!("type {} = {}", name, constructors.join(" | "))
        }
        TypeDeclaration::Record {
            name,
            constructor,
            fields,
        } => {
            let field_strs: Vec<String> = fields
                .iter()
                .map(
                    |RecordField {
                         name: fname,
                         type_annotation,
                     }| { format!("{}: {}", fname, type_annotation) },
                )
                .collect();
            format!("type {} = {}({})", name, constructor, field_strs.join(", "))
        }
    }
}

pub(crate) fn format_effect_decl(ed: &EffectDecl) -> String {
    let params_str = if ed.type_params.is_empty() {
        String::new()
    } else {
        format!(" {}", ed.type_params.join(" "))
    };
    let mut out = format!("effect {}{}", ed.name, params_str);
    for member in &ed.members {
        out.push('\n');
        out.push_str(&format!("  {}: {}", member.name, member.type_annotation));
    }
    out
}

pub(crate) fn format_declaration(decl: &Declaration) -> String {
    let mut out = String::new();

    // Type annotation line (optional).
    if let Some(ann) = &decl.type_annotation {
        out.push_str(&format!("{} : {}", decl.name, ann));
        out.push('\n');
    }

    // Definition line: `name params... =`
    let def_line = if decl.params.is_empty() {
        format!("{} =", decl.name)
    } else {
        format!("{} {} =", decl.name, decl.params.join(" "))
    };

    if let Some(stmts) = &decl.parsed_body {
        if stmts.is_empty() {
            // `f = ()` — unit body
            out.push_str(&format!("{} ()", def_line));
        } else {
            // Format the body at indent=1 (under the declaration).
            let body = format_stmts(stmts, 1);
            if !body.contains('\n') && stmts.len() == 1 {
                // Single-line body: try to place on the same line as the definition.
                let trimmed = body.trim_start();
                out.push_str(&format!("{} {}", def_line, trimmed));
            } else {
                out.push_str(&def_line);
                out.push('\n');
                out.push_str(&body);
            }
        }
    } else {
        // parsed_body is None: fall back to raw body string.
        // Normalize indentation: strip the minimum leading whitespace and
        // re-indent with 2 spaces per level.
        let body_raw = decl.body.trim_end();
        if body_raw.is_empty() {
            out.push_str(&format!("{} ()", def_line));
        } else if !body_raw.contains('\n') {
            out.push_str(&format!("{} {}", def_line, body_raw.trim()));
        } else {
            // Multi-line: strip common leading whitespace, then re-indent with 2 spaces.
            let min_leading: usize = body_raw
                .lines()
                .filter(|l| !l.trim().is_empty())
                .map(|l| l.len() - l.trim_start().len())
                .min()
                .unwrap_or(0);
            out.push_str(&def_line);
            out.push('\n');
            for line in body_raw.lines() {
                let trimmed = line.trim_end();
                if trimmed.is_empty() {
                    out.push('\n');
                } else {
                    let stripped = &line[min_leading.min(line.len())..];
                    out.push_str(&format!("  {}", stripped.trim_end()));
                    out.push('\n');
                }
            }
            // Remove trailing newline.
            if out.ends_with('\n') {
                out.pop();
            }
        }
    }

    out
}

// ---------------------------------------------------------------------------
// Module formatting
// ---------------------------------------------------------------------------

/// Format a `Module` back to Goby source.
///
/// Section order: imports → embed declarations → type declarations →
/// effect declarations → value declarations.
///
/// Each top-level item is separated by a blank line.  The output ends with a
/// single trailing newline.
pub fn format_module(module: &Module) -> String {
    let mut sections: Vec<String> = Vec::new();

    // imports (all on consecutive lines, no blank line between them)
    if !module.imports.is_empty() {
        let block: Vec<String> = module.imports.iter().map(format_import_decl).collect();
        sections.push(block.join("\n"));
    }

    // embed declarations
    if !module.embed_declarations.is_empty() {
        let block: Vec<String> = module
            .embed_declarations
            .iter()
            .map(format_embed_decl)
            .collect();
        sections.push(block.join("\n"));
    }

    // type declarations (each as a separate section for blank-line separation)
    for td in &module.type_declarations {
        sections.push(format_type_declaration(td));
    }

    // effect declarations
    for ed in &module.effect_declarations {
        sections.push(format_effect_decl(ed));
    }

    // value declarations
    for decl in &module.declarations {
        sections.push(format_declaration(decl));
    }

    let mut out = sections.join("\n\n");
    out.push('\n');
    out
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOpKind, CaseArm, CasePattern, Expr, Span, Stmt};
    use crate::parser::parse_module;

    fn dummy_span() -> Span {
        Span::point(1, 1)
    }

    // --- escape_string ---

    #[test]
    fn escape_string_plain() {
        assert_eq!(escape_string("hello"), "hello");
    }

    #[test]
    fn escape_string_newline() {
        assert_eq!(escape_string("a\nb"), "a\\nb");
    }

    #[test]
    fn escape_string_quote() {
        assert_eq!(escape_string("say \"hi\""), "say \\\"hi\\\"");
    }

    #[test]
    fn escape_string_backslash() {
        assert_eq!(escape_string("a\\b"), "a\\\\b");
    }

    // --- format_expr unit tests ---

    #[test]
    fn format_string_lit_with_escape() {
        // StringLit containing a real newline must be re-escaped in the output.
        let expr = Expr::StringLit("hello\nworld".to_string());
        assert_eq!(format_expr(&expr, 0), "\"hello\\nworld\"");
    }

    #[test]
    fn format_binop_parens_left_operand() {
        // `(a + b) * c`
        let expr = Expr::BinOp {
            op: BinOpKind::Mul,
            left: Box::new(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::var("a")),
                right: Box::new(Expr::var("b")),
            }),
            right: Box::new(Expr::var("c")),
        };
        assert_eq!(format_expr(&expr, 0), "(a + b) * c");
    }

    #[test]
    fn format_call_wraps_complex_arg_in_parens() {
        // `double (1 + 2)`
        let expr = Expr::call(
            Expr::var("double"),
            Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::IntLit(1)),
                right: Box::new(Expr::IntLit(2)),
            },
        );
        assert_eq!(format_expr(&expr, 0), "double (1 + 2)");
    }

    #[test]
    fn format_lambda_normal() {
        let expr = Expr::Lambda {
            param: "n".to_string(),
            body: Box::new(Expr::BinOp {
                op: BinOpKind::Mul,
                left: Box::new(Expr::var("n")),
                right: Box::new(Expr::IntLit(10)),
            }),
        };
        assert_eq!(format_expr(&expr, 0), "|n| -> n * 10");
    }

    #[test]
    fn format_lambda_placeholder() {
        // `_ * 10` — param == "_" so body is emitted directly
        let expr = Expr::Lambda {
            param: "_".to_string(),
            body: Box::new(Expr::BinOp {
                op: BinOpKind::Mul,
                left: Box::new(Expr::var("_")),
                right: Box::new(Expr::IntLit(10)),
            }),
        };
        assert_eq!(format_expr(&expr, 0), "_ * 10");
    }

    #[test]
    fn format_fn_lambda_two_params() {
        // nested lambda desugared from `fn acc x -> acc + x` should format as `fn acc x -> acc + x`
        use crate::parser_expr::parse_expr;
        let expr = parse_expr("fn acc x -> acc + x").expect("should parse");
        assert_eq!(format_expr(&expr, 0), "fn acc x -> acc + x");
    }

    #[test]
    fn format_fn_lambda_round_trip() {
        // parse → format → parse should be stable
        use crate::parser_expr::parse_expr;
        let src = "fn acc x -> acc + x";
        let ast1 = parse_expr(src).expect("should parse");
        let formatted = format_expr(&ast1, 0);
        let ast2 = parse_expr(&formatted).expect("formatted form should re-parse");
        assert_eq!(ast1, ast2, "round-trip should be stable");
    }

    #[test]
    fn format_interpolated_string() {
        let expr = Expr::InterpolatedString(vec![
            InterpolatedPart::Text("hello ".to_string()),
            InterpolatedPart::Expr(Box::new(Expr::var("name"))),
            InterpolatedPart::Text("!".to_string()),
        ]);
        assert_eq!(format_expr(&expr, 0), "\"hello ${name}!\"");
    }

    #[test]
    fn format_list_with_spread() {
        let expr = Expr::ListLit {
            elements: vec![Expr::IntLit(1), Expr::IntLit(2)],
            spread: Some(Box::new(Expr::var("xs"))),
        };
        assert_eq!(format_expr(&expr, 0), "[1, 2, ..xs]");
    }

    #[test]
    fn format_list_index_chained() {
        let inner = Expr::ListIndex {
            list: Box::new(Expr::var("xs")),
            index: Box::new(Expr::IntLit(0)),
        };
        let outer = Expr::ListIndex {
            list: Box::new(inner),
            index: Box::new(Expr::IntLit(1)),
        };
        assert_eq!(format_expr(&outer, 0), "xs[0][1]");
    }

    #[test]
    fn format_case_simple() {
        let expr = Expr::Case {
            scrutinee: Box::new(Expr::var("x")),
            arms: vec![
                CaseArm {
                    pattern: CasePattern::IntLit(0),
                    body: Box::new(Expr::StringLit("zero".to_string())),
                    span: dummy_span(),
                },
                CaseArm {
                    pattern: CasePattern::Wildcard,
                    body: Box::new(Expr::StringLit("other".to_string())),
                    span: dummy_span(),
                },
            ],
        };
        let out = format_expr(&expr, 0);
        assert!(out.starts_with("case x\n"), "got: {:?}", out);
        assert!(out.contains("  0 -> \"zero\""), "got: {:?}", out);
        assert!(out.contains("  _ -> \"other\""), "got: {:?}", out);
    }

    #[test]
    fn format_resume_wraps_complex_value() {
        let expr = Expr::Resume {
            value: Box::new(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::IntLit(1)),
                right: Box::new(Expr::IntLit(2)),
            }),
        };
        assert_eq!(format_expr(&expr, 0), "resume (1 + 2)");
    }

    #[test]
    fn format_resume_simple_value() {
        let expr = Expr::Resume {
            value: Box::new(Expr::TupleLit(vec![])),
        };
        assert_eq!(format_expr(&expr, 0), "resume ()");
    }

    // --- Stmt unit tests ---

    #[test]
    fn format_stmt_binding() {
        let stmt = Stmt::Binding {
            name: "x".to_string(),
            value: Expr::IntLit(42),
            span: None,
        };
        assert_eq!(format_stmt(&stmt, 1), "  x = 42");
    }

    #[test]
    fn format_stmt_mut_binding() {
        let stmt = Stmt::MutBinding {
            name: "x".to_string(),
            value: Expr::IntLit(1),
            span: None,
        };
        assert_eq!(format_stmt(&stmt, 0), "mut x = 1");
    }

    #[test]
    fn format_stmt_assign() {
        let stmt = Stmt::Assign {
            name: "counter".to_string(),
            value: Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::var("counter")),
                right: Box::new(Expr::IntLit(1)),
            },
            span: None,
        };
        assert_eq!(format_stmt(&stmt, 0), "counter := counter + 1");
    }

    // --- Top-level formatters ---

    #[test]
    fn format_import_plain() {
        let imp = ImportDecl {
            module_path: "goby/string".to_string(),
            kind: ImportKind::Plain,
            module_path_span: None,
            kind_span: None,
        };
        assert_eq!(format_import_decl(&imp), "import goby/string");
    }

    #[test]
    fn format_import_alias() {
        let imp = ImportDecl {
            module_path: "goby/list".to_string(),
            kind: ImportKind::Alias("l".to_string()),
            module_path_span: None,
            kind_span: None,
        };
        assert_eq!(format_import_decl(&imp), "import goby/list as l");
    }

    #[test]
    fn format_import_selective() {
        let imp = ImportDecl {
            module_path: "goby/env".to_string(),
            kind: ImportKind::Selective(vec!["fetch_env_var".to_string()]),
            module_path_span: None,
            kind_span: None,
        };
        assert_eq!(
            format_import_decl(&imp),
            "import goby/env ( fetch_env_var )"
        );
    }

    #[test]
    fn format_embed() {
        let ed = EmbedDecl {
            effect_name: "Print".to_string(),
            handler_name: "__goby_embeded_effect_stdout_handler".to_string(),
            line: 1,
        };
        assert_eq!(
            format_embed_decl(&ed),
            "@embed Print __goby_embeded_effect_stdout_handler"
        );
    }

    #[test]
    fn format_type_alias() {
        let td = TypeDeclaration::Alias {
            name: "UserID".to_string(),
            target: "String".to_string(),
        };
        assert_eq!(format_type_declaration(&td), "type UserID = String");
    }

    #[test]
    fn format_type_union() {
        let td = TypeDeclaration::Union {
            name: "UserStatus".to_string(),
            constructors: vec!["Activated".to_string(), "Deactivated".to_string()],
        };
        assert_eq!(
            format_type_declaration(&td),
            "type UserStatus = Activated | Deactivated"
        );
    }

    #[test]
    fn format_type_record() {
        use crate::ast::RecordField;
        let td = TypeDeclaration::Record {
            name: "User".to_string(),
            constructor: "User".to_string(),
            fields: vec![
                RecordField {
                    name: "id".to_string(),
                    type_annotation: "UserID".to_string(),
                },
                RecordField {
                    name: "name".to_string(),
                    type_annotation: "String".to_string(),
                },
            ],
        };
        assert_eq!(
            format_type_declaration(&td),
            "type User = User(id: UserID, name: String)"
        );
    }

    #[test]
    fn format_effect_with_type_params() {
        use crate::ast::{EffectMember, Span};
        let ed = EffectDecl {
            name: "Stream".to_string(),
            type_params: vec!["a".to_string(), "b".to_string()],
            members: vec![EffectMember {
                name: "emit".to_string(),
                type_annotation: "a -> b -> (Bool, b)".to_string(),
                span: Span::point(1, 1),
            }],
            span: Span::point(1, 1),
        };
        assert_eq!(
            format_effect_decl(&ed),
            "effect Stream a b\n  emit: a -> b -> (Bool, b)"
        );
    }

    // --- format_if indentation test ---

    #[test]
    fn format_if_indents_both_branches() {
        let expr = Expr::If {
            condition: Box::new(Expr::BoolLit(true)),
            then_expr: Box::new(Expr::IntLit(1)),
            else_expr: Box::new(Expr::IntLit(2)),
        };
        let out = format_expr(&expr, 0);
        assert_eq!(out, "if True\n  1\nelse\n  2", "got: {:?}", out);
    }

    // --- format_with structure test ---

    #[test]
    fn format_with_produces_in_keyword() {
        // Handler expression: uses `with` alone (form b).
        let handler = Expr::Handler { clauses: vec![] };
        let body = vec![Stmt::Expr(Expr::IntLit(42), None)];
        let out = format_with_expr(&handler, &body, 0);
        assert!(out.starts_with("with\n"), "got: {:?}", out);
        assert!(
            out.contains("\nin\n"),
            "should have `in` keyword: {:?}",
            out
        );
        assert!(out.contains("  42"), "should have indented body: {:?}", out);
    }

    #[test]
    fn format_with_named_handler_uses_inline_form() {
        // Variable handler: uses `with h` (form a).
        let handler = Expr::Var {
            name: "h".to_string(),
            span: None,
        };
        let body = vec![Stmt::Expr(Expr::IntLit(1), None)];
        let out = format_with_expr(&handler, &body, 0);
        assert!(out.starts_with("with h\n"), "got: {:?}", out);
        assert!(
            out.contains("\nin\n"),
            "should have `in` keyword: {:?}",
            out
        );
    }

    // --- Idempotency tests over examples/ ---
    //
    // For each file: fmt(fmt(src)) == fmt(src)
    // Files with no parsed_body (raw fallback bodies) may not be idempotent through
    // the parser; those are tested separately where feasible.

    fn assert_idempotent(label: &str, src: &str) {
        let module1 = parse_module(src).expect(&format!("{}: first parse failed", label));
        let fmt1 = format_module(&module1);
        let module2 =
            parse_module(&fmt1).expect(&format!("{}: second parse failed:\n{}", label, fmt1));
        let fmt2 = format_module(&module2);
        assert_eq!(fmt1, fmt2, "{}: fmt(fmt(src)) != fmt(src)", label);
    }

    #[test]
    fn idempotent_hello() {
        assert_idempotent("hello", include_str!("../../../examples/hello.gb"));
    }

    #[test]
    fn idempotent_function() {
        assert_idempotent("function", include_str!("../../../examples/function.gb"));
    }

    #[test]
    fn idempotent_control_flow() {
        assert_idempotent(
            "control_flow",
            include_str!("../../../examples/control_flow.gb"),
        );
    }

    #[test]
    fn idempotent_case_arm_block() {
        assert_idempotent(
            "case_arm_block",
            include_str!("../../../examples/case_arm_block.gb"),
        );
    }

    #[test]
    fn idempotent_basic_types() {
        assert_idempotent(
            "basic_types",
            include_str!("../../../examples/basic_types.gb"),
        );
    }

    #[test]
    fn idempotent_list_index() {
        assert_idempotent(
            "list_index",
            include_str!("../../../examples/list_index.gb"),
        );
    }

    #[test]
    fn idempotent_list_spread() {
        assert_idempotent(
            "list_spread",
            include_str!("../../../examples/list_spread.gb"),
        );
    }

    #[test]
    fn idempotent_list_case() {
        assert_idempotent("list_case", include_str!("../../../examples/list_case.gb"));
    }

    #[test]
    fn idempotent_type() {
        assert_idempotent("type", include_str!("../../../examples/type.gb"));
    }

    #[test]
    fn idempotent_generic_types() {
        assert_idempotent(
            "generic_types",
            include_str!("../../../examples/generic_types.gb"),
        );
    }

    #[test]
    fn idempotent_effect() {
        assert_idempotent("effect", include_str!("../../../examples/effect.gb"));
    }

    #[test]
    fn idempotent_effect_generic() {
        assert_idempotent(
            "effect_generic",
            include_str!("../../../examples/effect_generic.gb"),
        );
    }

    #[test]
    fn idempotent_import() {
        assert_idempotent("import", include_str!("../../../examples/import.gb"));
    }

    #[test]
    #[ignore = "qualified iterator handler clauses currently break formatter idempotence; tracked separately"]
    fn idempotent_iterator() {
        assert_idempotent("iterator", include_str!("../../../examples/iterator.gb"));
    }

    #[test]
    #[ignore = "qualified iterator handler clauses currently break formatter idempotence; tracked separately"]
    fn idempotent_iterator_unified() {
        assert_idempotent(
            "iterator_unified",
            include_str!("../../../examples/iterator_unified.gb"),
        );
    }

    #[test]
    fn idempotent_function_reference() {
        assert_idempotent(
            "function_reference",
            include_str!("../../../examples/function_reference.gb"),
        );
    }

    #[test]
    fn idempotent_mut() {
        assert_idempotent("mut", include_str!("../../../examples/mut.gb"));
    }
}
