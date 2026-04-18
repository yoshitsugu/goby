//! Shared Typed IR for Goby.
//!
//! This module defines the canonical intermediate representation shared between
//! the front-end (parser, typechecker) and all backends. Backends must consume
//! IR rather than raw AST shapes.
//!
//! # Ownership
//! - Definitions here: `goby-core` (this module)
//! - Backend analysis and lowering: `goby-wasm` (consumes IR)
//!
//! # Effect nodes
//! `PerformEffect`, `Handle`, `WithHandler`, and `Resume` represent the effect
//! boundary. `With`/`Handler`/`Resume` from the AST lower into these
//! nodes. Full `PerformEffect` lowering from qualified calls is still deferred.

/// A type annotation carried by IR nodes.
///
/// `IrType` does not mirror the private typechecker `Ty` enum. It carries
/// whatever type evidence is available at lowering time. `Unknown` is used
/// when no type information is available.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrType {
    Int,
    Bool,
    Str,
    Unit,
    /// Type is not known at IR construction time.
    Unknown,
    /// Opaque annotation string (e.g. from a parsed type annotation).
    Opaque(String),
}

/// Binary operation kinds, mirroring `ast::BinOpKind` exactly.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrBinOp {
    Or,
    And,
    BitXor,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
}

/// A pure value expression that does not perform effects.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueExpr {
    IntLit(i64),
    BoolLit(bool),
    StrLit(String),
    /// List literal with optional spread tail.
    ListLit {
        elements: Vec<ValueExpr>,
        spread: Option<Box<ValueExpr>>,
    },
    /// Tuple literal. The empty tuple is the unit value.
    TupleLit(Vec<ValueExpr>),
    /// Record construction using a constructor and named field values.
    RecordLit {
        constructor: String,
        fields: Vec<(String, ValueExpr)>,
    },
    /// Lambda value. Creation is pure even when the body computation is effectful.
    Lambda {
        param: String,
        body: Box<CompExpr>,
    },
    /// Interpolated string with pure parts only.
    Interp(Vec<IrInterpPart>),
    /// Local variable reference.
    Var(String),
    /// Qualified global reference (e.g. `Module.name`).
    GlobalRef {
        module: String,
        name: String,
    },
    BinOp {
        op: IrBinOp,
        left: Box<ValueExpr>,
        right: Box<ValueExpr>,
    },
    /// Unit value `()`.
    Unit,
    /// Tuple member projection: load the `index`-th field from a tuple value.
    TupleProject {
        tuple: Box<ValueExpr>,
        index: usize,
    },
    /// Pure list indexing (`list.get list index`) lowered as a value expression.
    ListGet {
        list: Box<ValueExpr>,
        index: Box<ValueExpr>,
    },
}

/// A part of a pure interpolated string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrInterpPart {
    Text(String),
    Expr(ValueExpr),
}

/// A computation expression that may sequence effects or control flow.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompExpr {
    /// Lift a pure value into a computation.
    Value(ValueExpr),
    /// Local let-binding: `let name: ty = value; body`.
    Let {
        name: String,
        ty: IrType,
        value: Box<CompExpr>,
        body: Box<CompExpr>,
    },
    /// Mutable local let-binding: `let mut name: ty = value; body`.
    LetMut {
        name: String,
        ty: IrType,
        value: Box<CompExpr>,
        body: Box<CompExpr>,
    },
    /// Sequential evaluation: evaluate each statement, then the tail.
    Seq {
        stmts: Vec<CompExpr>,
        tail: Box<CompExpr>,
    },
    /// Conditional branch.
    If {
        cond: Box<ValueExpr>,
        then_: Box<CompExpr>,
        else_: Box<CompExpr>,
    },
    /// Function call (flat multi-arg; curried AST chains are flattened during lowering).
    Call {
        callee: Box<ValueExpr>,
        args: Vec<ValueExpr>,
    },
    /// Assignment to a mutable local. Produces `Unit`.
    Assign { name: String, value: Box<CompExpr> },
    /// List-index assignment: update element(s) of a mutable list local using
    /// path-copying semantics.  `root` names the mutable local; `path` is a
    /// non-empty sequence of pure index expressions (outermost first); `value`
    /// is the new element value.  Produces `Unit`.
    AssignIndex {
        root: String,
        path: Vec<ValueExpr>,
        value: Box<CompExpr>,
    },
    /// Pattern-matching case expression.
    Case {
        scrutinee: Box<ValueExpr>,
        arms: Vec<IrCaseArm>,
    },
    /// Effect operation invocation.
    ///
    /// This node is constructable and printable. Lowering from
    /// qualified AST calls (e.g. `Read.read()`) into `PerformEffect` requires
    /// an effect-table lookup is still deferred.
    PerformEffect {
        effect: String,
        op: String,
        args: Vec<ValueExpr>,
    },
    /// Handler expression: a set of operation clauses for one or more effects.
    ///
    /// Lowered from `Expr::Handler { clauses }`. Corresponds to the handler
    /// value that can be passed to a `with` expression.
    Handle { clauses: Vec<IrHandlerClause> },
    /// Install a handler over a body computation.
    ///
    /// Lowered from `Expr::With { handler, body }`.
    WithHandler {
        handler: Box<CompExpr>,
        body: Box<CompExpr>,
    },
    /// Resume from an effect handler continuation.
    ///
    /// Lowered from `Expr::Resume { value }`.
    Resume { value: Box<ValueExpr> },
}

/// A single operation clause in a handler expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrHandlerClause {
    /// The operation name handled (e.g. `"read"`, `"print"`).
    pub op_name: String,
    /// Parameter names for this operation clause.
    pub params: Vec<String>,
    /// Body of the clause.
    pub body: CompExpr,
}

/// A single arm in a `case` expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrCaseArm {
    pub pattern: IrCasePattern,
    pub body: CompExpr,
}

/// A list-pattern element in a `case` arm.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrListPatternItem {
    IntLit(i64),
    StringLit(String),
    Bind(String),
    Wildcard,
}

/// A list-pattern tail in a `case` arm.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrListPatternTail {
    Ignore,
    Bind(String),
}

/// A `case` pattern in shared IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrCasePattern {
    IntLit(i64),
    StringLit(String),
    BoolLit(bool),
    EmptyList,
    ListPattern {
        items: Vec<IrListPatternItem>,
        tail: Option<IrListPatternTail>,
    },
    Wildcard,
}

/// A top-level declaration in the IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrDecl {
    pub name: String,
    pub params: Vec<(String, IrType)>,
    pub result_ty: IrType,
    /// Residual effects declared in the type annotation (e.g. `can Read, Print`).
    /// Empty if no `can` clause is present or no annotation is available.
    pub residual_effects: Vec<String>,
    pub body: CompExpr,
}

/// A compiled module in the IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrModule {
    pub decls: Vec<IrDecl>,
}

// ---------------------------------------------------------------------------
// IR text rendering
// ---------------------------------------------------------------------------

/// Render an `IrModule` to a human-readable indented string.
pub fn fmt_ir(module: &IrModule) -> String {
    let mut out = String::new();
    for decl in &module.decls {
        fmt_decl(&mut out, decl, 0);
        out.push('\n');
    }
    out
}

fn indent(out: &mut String, depth: usize) {
    for _ in 0..depth {
        out.push_str("  ");
    }
}

fn fmt_decl(out: &mut String, decl: &IrDecl, depth: usize) {
    indent(out, depth);
    out.push_str("decl ");
    out.push_str(&decl.name);
    if !decl.params.is_empty() {
        out.push('(');
        for (i, (pname, pty)) in decl.params.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            out.push_str(pname);
            out.push_str(": ");
            fmt_type(out, pty);
        }
        out.push(')');
    }
    out.push_str(": ");
    fmt_type(out, &decl.result_ty);
    if !decl.residual_effects.is_empty() {
        out.push_str(" can ");
        out.push_str(&decl.residual_effects.join(", "));
    }
    out.push_str(" =\n");
    fmt_comp(out, &decl.body, depth + 1);
}

/// Escape a string for display in IR text output.
/// Handles `\`, `"`, `\n`, `\r`, and `\t`.
fn escape_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            other => out.push(other),
        }
    }
    out
}

fn fmt_type(out: &mut String, ty: &IrType) {
    let s = match ty {
        IrType::Int => "Int",
        IrType::Bool => "Bool",
        IrType::Str => "Str",
        IrType::Unit => "Unit",
        IrType::Unknown => "?",
        IrType::Opaque(s) => {
            out.push_str(s);
            return;
        }
    };
    out.push_str(s);
}

fn fmt_value(out: &mut String, v: &ValueExpr) {
    match v {
        ValueExpr::IntLit(n) => out.push_str(&n.to_string()),
        ValueExpr::BoolLit(b) => out.push_str(if *b { "true" } else { "false" }),
        ValueExpr::StrLit(s) => {
            out.push('"');
            out.push_str(&escape_str(s));
            out.push('"');
        }
        ValueExpr::ListLit { elements, spread } => {
            out.push('[');
            for (i, element) in elements.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                fmt_value(out, element);
            }
            if let Some(tail) = spread {
                if !elements.is_empty() {
                    out.push_str(", ");
                }
                out.push_str("..");
                fmt_value(out, tail);
            }
            out.push(']');
        }
        ValueExpr::TupleLit(items) => {
            out.push('(');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                fmt_value(out, item);
            }
            out.push(')');
        }
        ValueExpr::RecordLit {
            constructor,
            fields,
        } => {
            out.push_str(constructor);
            out.push('(');
            for (i, (name, value)) in fields.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(name);
                out.push_str(": ");
                fmt_value(out, value);
            }
            out.push(')');
        }
        ValueExpr::Lambda { param, body } => {
            out.push('\\');
            out.push_str(param);
            out.push_str(" ->\n");
            fmt_comp(out, body, 1);
        }
        ValueExpr::Interp(parts) => {
            out.push_str("interp(");
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                match part {
                    IrInterpPart::Text(t) => {
                        out.push('"');
                        out.push_str(&escape_str(t));
                        out.push('"');
                    }
                    IrInterpPart::Expr(e) => {
                        out.push_str("expr(");
                        fmt_value(out, e);
                        out.push(')');
                    }
                }
            }
            out.push(')');
        }
        ValueExpr::Var(name) => out.push_str(name),
        ValueExpr::GlobalRef { module, name } => {
            out.push_str(module);
            out.push('.');
            out.push_str(name);
        }
        ValueExpr::BinOp { op, left, right } => {
            out.push('(');
            fmt_value(out, left);
            out.push(' ');
            out.push_str(match op {
                IrBinOp::Or => "||",
                IrBinOp::And => "&&",
                IrBinOp::BitXor => "^",
                IrBinOp::Add => "+",
                IrBinOp::Sub => "-",
                IrBinOp::Mul => "*",
                IrBinOp::Div => "/",
                IrBinOp::Mod => "%",
                IrBinOp::Eq => "==",
                IrBinOp::Lt => "<",
                IrBinOp::Gt => ">",
                IrBinOp::Le => "<=",
                IrBinOp::Ge => ">=",
            });
            out.push(' ');
            fmt_value(out, right);
            out.push(')');
        }
        ValueExpr::Unit => out.push_str("()"),
        ValueExpr::TupleProject { tuple, index } => {
            out.push('(');
            fmt_value(out, tuple);
            out.push(')');
            out.push('.');
            out.push_str(&index.to_string());
        }
        ValueExpr::ListGet { list, index } => {
            out.push_str("list.get ");
            fmt_value(out, list);
            out.push(' ');
            fmt_value(out, index);
        }
    }
}

fn fmt_comp(out: &mut String, c: &CompExpr, depth: usize) {
    match c {
        CompExpr::Value(v) => {
            indent(out, depth);
            fmt_value(out, v);
            out.push('\n');
        }
        CompExpr::Let {
            name,
            ty,
            value,
            body,
        } => {
            indent(out, depth);
            out.push_str("let ");
            out.push_str(name);
            out.push_str(": ");
            fmt_type(out, ty);
            out.push_str(" =\n");
            fmt_comp(out, value, depth + 1);
            indent(out, depth);
            out.push_str("in\n");
            fmt_comp(out, body, depth + 1);
        }
        CompExpr::LetMut {
            name,
            ty,
            value,
            body,
        } => {
            indent(out, depth);
            out.push_str("let mut ");
            out.push_str(name);
            out.push_str(": ");
            fmt_type(out, ty);
            out.push_str(" =\n");
            fmt_comp(out, value, depth + 1);
            indent(out, depth);
            out.push_str("in\n");
            fmt_comp(out, body, depth + 1);
        }
        CompExpr::Seq { stmts, tail } => {
            indent(out, depth);
            out.push_str("seq\n");
            for s in stmts {
                fmt_comp(out, s, depth + 1);
            }
            indent(out, depth);
            out.push_str("=>\n");
            fmt_comp(out, tail, depth + 1);
        }
        CompExpr::If { cond, then_, else_ } => {
            indent(out, depth);
            out.push_str("if ");
            fmt_value(out, cond);
            out.push_str(" then\n");
            fmt_comp(out, then_, depth + 1);
            indent(out, depth);
            out.push_str("else\n");
            fmt_comp(out, else_, depth + 1);
        }
        CompExpr::Call { callee, args } => {
            indent(out, depth);
            out.push_str("call ");
            fmt_value(out, callee);
            out.push('(');
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                fmt_value(out, a);
            }
            out.push_str(")\n");
        }
        CompExpr::Assign { name, value } => {
            indent(out, depth);
            out.push_str("assign ");
            out.push_str(name);
            out.push_str(" =\n");
            fmt_comp(out, value, depth + 1);
        }
        CompExpr::AssignIndex { root, path, value } => {
            indent(out, depth);
            out.push_str("assign_index ");
            out.push_str(root);
            for idx in path {
                out.push('[');
                fmt_value(out, idx);
                out.push(']');
            }
            out.push_str(" =\n");
            fmt_comp(out, value, depth + 1);
        }
        CompExpr::Case { scrutinee, arms } => {
            indent(out, depth);
            out.push_str("case ");
            fmt_value(out, scrutinee);
            out.push_str(" of\n");
            for arm in arms {
                indent(out, depth + 1);
                fmt_case_pattern(out, &arm.pattern);
                out.push_str(" ->\n");
                fmt_comp(out, &arm.body, depth + 2);
            }
        }
        CompExpr::PerformEffect { effect, op, args } => {
            indent(out, depth);
            out.push_str("perform ");
            out.push_str(effect);
            out.push('.');
            out.push_str(op);
            out.push('(');
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                fmt_value(out, a);
            }
            out.push_str(")\n");
        }
        CompExpr::Handle { clauses } => {
            indent(out, depth);
            out.push_str("handler\n");
            for clause in clauses {
                indent(out, depth + 1);
                out.push_str(&clause.op_name);
                if !clause.params.is_empty() {
                    out.push('(');
                    out.push_str(&clause.params.join(", "));
                    out.push(')');
                }
                out.push_str(" ->\n");
                fmt_comp(out, &clause.body, depth + 2);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            indent(out, depth);
            out.push_str("with\n");
            fmt_comp(out, handler, depth + 1);
            indent(out, depth);
            out.push_str("do\n");
            fmt_comp(out, body, depth + 1);
        }
        CompExpr::Resume { value } => {
            indent(out, depth);
            out.push_str("resume ");
            fmt_value(out, value);
            out.push('\n');
        }
    }
}

fn fmt_case_pattern(out: &mut String, pattern: &IrCasePattern) {
    match pattern {
        IrCasePattern::IntLit(n) => out.push_str(&n.to_string()),
        IrCasePattern::StringLit(s) => {
            out.push('"');
            out.push_str(&escape_str(s));
            out.push('"');
        }
        IrCasePattern::BoolLit(b) => out.push_str(if *b { "true" } else { "false" }),
        IrCasePattern::EmptyList => out.push_str("[]"),
        IrCasePattern::ListPattern { items, tail } => {
            out.push('[');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                match item {
                    IrListPatternItem::IntLit(n) => out.push_str(&n.to_string()),
                    IrListPatternItem::StringLit(s) => {
                        out.push('"');
                        out.push_str(&escape_str(s));
                        out.push('"');
                    }
                    IrListPatternItem::Bind(name) => out.push_str(name),
                    IrListPatternItem::Wildcard => out.push('_'),
                }
            }
            if let Some(tail) = tail {
                if !items.is_empty() {
                    out.push_str(", ");
                }
                out.push_str("..");
                match tail {
                    IrListPatternTail::Ignore => out.push('_'),
                    IrListPatternTail::Bind(name) => out.push_str(name),
                }
            }
            out.push(']');
        }
        IrCasePattern::Wildcard => out.push('_'),
    }
}

// ---------------------------------------------------------------------------
// IR validation
// ---------------------------------------------------------------------------

/// An error from `validate_ir`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrValidateError {
    pub message: String,
}

impl std::fmt::Display for IrValidateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "IR validation error: {}", self.message)
    }
}

impl std::error::Error for IrValidateError {}

/// Validate structural well-formedness of an `IrModule`.
///
/// Structural checks include:
/// - `Call` nodes must have at least one argument.
/// - `Handle` nodes must have at least one clause.
/// - Each `IrHandlerClause` body is recursively validated.
/// - `WithHandler` handler and body are recursively validated.
///
/// Undefined-variable-reference checking is deferred.
pub fn validate_ir(module: &IrModule) -> Result<(), IrValidateError> {
    for decl in &module.decls {
        validate_comp(&decl.body, &decl.name)?;
    }
    Ok(())
}

fn validate_comp(c: &CompExpr, decl_name: &str) -> Result<(), IrValidateError> {
    match c {
        CompExpr::Value(value) => validate_value(value, decl_name),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            validate_comp(value, decl_name)?;
            validate_comp(body, decl_name)
        }
        CompExpr::Seq { stmts, tail } => {
            for s in stmts {
                validate_comp(s, decl_name)?;
            }
            validate_comp(tail, decl_name)
        }
        CompExpr::If { cond, then_, else_ } => {
            validate_comp(then_, decl_name)?;
            validate_value(cond, decl_name)?;
            validate_comp(else_, decl_name)
        }
        CompExpr::Call { callee, args } => {
            if args.is_empty() {
                Err(IrValidateError {
                    message: format!(
                        "in decl `{}`: Call node must have at least one argument",
                        decl_name
                    ),
                })
            } else {
                validate_value(callee, decl_name)?;
                for arg in args {
                    validate_value(arg, decl_name)?;
                }
                Ok(())
            }
        }
        CompExpr::Assign { value, .. } => validate_comp(value, decl_name),
        CompExpr::AssignIndex { path, value, .. } => {
            for idx in path {
                validate_value(idx, decl_name)?;
            }
            validate_comp(value, decl_name)
        }
        CompExpr::Case { scrutinee, arms } => {
            if arms.is_empty() {
                return Err(IrValidateError {
                    message: format!(
                        "in decl `{}`: Case node must have at least one arm",
                        decl_name
                    ),
                });
            }
            validate_value(scrutinee, decl_name)?;
            for arm in arms {
                validate_case_pattern(&arm.pattern, decl_name)?;
                validate_comp(&arm.body, decl_name)?;
            }
            Ok(())
        }
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                validate_value(arg, decl_name)?;
            }
            Ok(())
        }
        CompExpr::Handle { clauses } => {
            if clauses.is_empty() {
                return Err(IrValidateError {
                    message: format!(
                        "in decl `{}`: Handle node must have at least one clause",
                        decl_name
                    ),
                });
            }
            for clause in clauses {
                validate_comp(&clause.body, decl_name)?;
            }
            Ok(())
        }
        CompExpr::WithHandler { handler, body } => {
            validate_comp(handler, decl_name)?;
            validate_comp(body, decl_name)
        }
        CompExpr::Resume { value } => validate_value(value, decl_name),
    }
}

fn validate_value(value: &ValueExpr, decl_name: &str) -> Result<(), IrValidateError> {
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for element in elements {
                validate_value(element, decl_name)?;
            }
            if let Some(tail) = spread {
                validate_value(tail, decl_name)?;
            }
            Ok(())
        }
        ValueExpr::TupleLit(items) => {
            for item in items {
                validate_value(item, decl_name)?;
            }
            Ok(())
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, value) in fields {
                validate_value(value, decl_name)?;
            }
            Ok(())
        }
        ValueExpr::Lambda { body, .. } => validate_comp(body, decl_name),
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(expr) = part {
                    validate_value(expr, decl_name)?;
                }
            }
            Ok(())
        }
        ValueExpr::BinOp { left, right, .. } => {
            validate_value(left, decl_name)?;
            validate_value(right, decl_name)
        }
        ValueExpr::TupleProject { tuple, .. } => validate_value(tuple, decl_name),
        ValueExpr::ListGet { list, index } => {
            validate_value(list, decl_name)?;
            validate_value(index, decl_name)
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {
            let _ = decl_name;
            Ok(())
        }
    }
}

fn validate_case_pattern(pattern: &IrCasePattern, decl_name: &str) -> Result<(), IrValidateError> {
    match pattern {
        IrCasePattern::ListPattern { items, tail } => {
            let _ = tail;
            if items.is_empty() && tail.is_none() {
                return Err(IrValidateError {
                    message: format!(
                        "in decl `{}`: ListPattern must bind at least one item or tail",
                        decl_name
                    ),
                });
            }
            Ok(())
        }
        IrCasePattern::IntLit(_)
        | IrCasePattern::StringLit(_)
        | IrCasePattern::BoolLit(_)
        | IrCasePattern::EmptyList
        | IrCasePattern::Wildcard => Ok(()),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn simple_module(decls: Vec<IrDecl>) -> IrModule {
        IrModule { decls }
    }

    fn val(v: ValueExpr) -> CompExpr {
        CompExpr::Value(v)
    }

    /// Build a simple `IrDecl` with no params, no residual effects.
    fn simple_decl(name: &str, result_ty: IrType, body: CompExpr) -> IrDecl {
        IrDecl {
            name: name.into(),
            params: vec![],
            result_ty,
            residual_effects: vec![],
            body,
        }
    }

    /// Build a simple `IrDecl` with params but no residual effects.
    fn decl_with_params_ir(
        name: &str,
        params: Vec<(&str, IrType)>,
        result_ty: IrType,
        body: CompExpr,
    ) -> IrDecl {
        IrDecl {
            name: name.into(),
            params: params.into_iter().map(|(n, t)| (n.into(), t)).collect(),
            result_ty,
            residual_effects: vec![],
            body,
        }
    }

    // --- printer tests ---

    #[test]
    fn print_int_lit() {
        let m = simple_module(vec![simple_decl(
            "answer",
            IrType::Int,
            val(ValueExpr::IntLit(42)),
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_bool_lit() {
        let m = simple_module(vec![simple_decl(
            "flag",
            IrType::Bool,
            val(ValueExpr::BoolLit(true)),
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_str_lit() {
        let m = simple_module(vec![simple_decl(
            "greeting",
            IrType::Str,
            val(ValueExpr::StrLit("hello".into())),
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_list_lit_with_spread() {
        let m = simple_module(vec![simple_decl(
            "items",
            IrType::Unknown,
            val(ValueExpr::ListLit {
                elements: vec![ValueExpr::IntLit(1), ValueExpr::IntLit(2)],
                spread: Some(Box::new(ValueExpr::Var("tail".into()))),
            }),
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_tuple_lit() {
        let m = simple_module(vec![simple_decl(
            "pair",
            IrType::Unknown,
            val(ValueExpr::TupleLit(vec![
                ValueExpr::IntLit(1),
                ValueExpr::BoolLit(true),
            ])),
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_record_lit() {
        let m = simple_module(vec![simple_decl(
            "pair",
            IrType::Unknown,
            val(ValueExpr::RecordLit {
                constructor: "Pair".into(),
                fields: vec![
                    ("left".into(), ValueExpr::IntLit(1)),
                    ("right".into(), ValueExpr::IntLit(2)),
                ],
            }),
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_lambda_value() {
        let m = simple_module(vec![simple_decl(
            "make_increment",
            IrType::Unknown,
            val(ValueExpr::Lambda {
                param: "x".into(),
                body: Box::new(CompExpr::Value(ValueExpr::BinOp {
                    op: IrBinOp::Add,
                    left: Box::new(ValueExpr::Var("x".into())),
                    right: Box::new(ValueExpr::IntLit(1)),
                })),
            }),
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_var() {
        let m = simple_module(vec![decl_with_params_ir(
            "f",
            vec![("x", IrType::Int)],
            IrType::Int,
            val(ValueExpr::Var("x".into())),
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_global_ref() {
        let m = simple_module(vec![simple_decl(
            "use_io",
            IrType::Unknown,
            CompExpr::Call {
                callee: Box::new(ValueExpr::GlobalRef {
                    module: "Print".into(),
                    name: "print".into(),
                }),
                args: vec![ValueExpr::StrLit("hi".into())],
            },
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_binop_all_ops() {
        let ops = [
            IrBinOp::And,
            IrBinOp::Add,
            IrBinOp::Mul,
            IrBinOp::Eq,
            IrBinOp::Lt,
            IrBinOp::Gt,
        ];
        for op in ops {
            let m = simple_module(vec![simple_decl(
                "op_test",
                IrType::Unknown,
                val(ValueExpr::BinOp {
                    op,
                    left: Box::new(ValueExpr::IntLit(1)),
                    right: Box::new(ValueExpr::IntLit(2)),
                }),
            )]);
            let _ = fmt_ir(&m);
        }
    }

    #[test]
    fn print_binop_add_snapshot() {
        let m = simple_module(vec![decl_with_params_ir(
            "add",
            vec![("a", IrType::Int), ("b", IrType::Int)],
            IrType::Int,
            val(ValueExpr::BinOp {
                op: IrBinOp::Add,
                left: Box::new(ValueExpr::Var("a".into())),
                right: Box::new(ValueExpr::Var("b".into())),
            }),
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_let() {
        let m = simple_module(vec![simple_decl(
            "with_let",
            IrType::Int,
            CompExpr::Let {
                name: "x".into(),
                ty: IrType::Int,
                value: Box::new(val(ValueExpr::IntLit(10))),
                body: Box::new(val(ValueExpr::Var("x".into()))),
            },
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_if() {
        let m = simple_module(vec![decl_with_params_ir(
            "check",
            vec![("b", IrType::Bool)],
            IrType::Int,
            CompExpr::If {
                cond: Box::new(ValueExpr::Var("b".into())),
                then_: Box::new(val(ValueExpr::IntLit(1))),
                else_: Box::new(val(ValueExpr::IntLit(0))),
            },
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_seq() {
        let m = simple_module(vec![simple_decl(
            "seq_example",
            IrType::Unit,
            CompExpr::Seq {
                stmts: vec![CompExpr::Call {
                    callee: Box::new(ValueExpr::GlobalRef {
                        module: "Print".into(),
                        name: "print".into(),
                    }),
                    args: vec![ValueExpr::StrLit("a".into())],
                }],
                tail: Box::new(val(ValueExpr::Unit)),
            },
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_call() {
        let m = simple_module(vec![simple_decl(
            "invoke",
            IrType::Unknown,
            CompExpr::Call {
                callee: Box::new(ValueExpr::Var("f".into())),
                args: vec![ValueExpr::IntLit(1), ValueExpr::IntLit(2)],
            },
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_case() {
        let m = simple_module(vec![simple_decl(
            "inspect",
            IrType::Unknown,
            CompExpr::Case {
                scrutinee: Box::new(ValueExpr::Var("items".into())),
                arms: vec![
                    IrCaseArm {
                        pattern: IrCasePattern::EmptyList,
                        body: val(ValueExpr::IntLit(0)),
                    },
                    IrCaseArm {
                        pattern: IrCasePattern::ListPattern {
                            items: vec![IrListPatternItem::Bind("head".into())],
                            tail: Some(IrListPatternTail::Bind("tail".into())),
                        },
                        body: val(ValueExpr::Var("head".into())),
                    },
                ],
            },
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_let_mut_and_assign() {
        let m = simple_module(vec![simple_decl(
            "mutate",
            IrType::Unknown,
            CompExpr::LetMut {
                name: "counter".into(),
                ty: IrType::Int,
                value: Box::new(val(ValueExpr::IntLit(0))),
                body: Box::new(CompExpr::Seq {
                    stmts: vec![CompExpr::Assign {
                        name: "counter".into(),
                        value: Box::new(val(ValueExpr::IntLit(1))),
                    }],
                    tail: Box::new(val(ValueExpr::Var("counter".into()))),
                }),
            },
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_handler_with_clause() {
        let m = simple_module(vec![simple_decl(
            "my_handler",
            IrType::Unknown,
            CompExpr::Handle {
                clauses: vec![IrHandlerClause {
                    op_name: "read".into(),
                    params: vec!["resume".into()],
                    body: CompExpr::Resume {
                        value: Box::new(ValueExpr::StrLit("hello".into())),
                    },
                }],
            },
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_with_handler() {
        let m = simple_module(vec![simple_decl(
            "main_with",
            IrType::Unit,
            CompExpr::WithHandler {
                handler: Box::new(CompExpr::Handle {
                    clauses: vec![IrHandlerClause {
                        op_name: "print".into(),
                        params: vec!["msg".into(), "resume".into()],
                        body: CompExpr::Resume {
                            value: Box::new(ValueExpr::Unit),
                        },
                    }],
                }),
                body: Box::new(CompExpr::Call {
                    callee: Box::new(ValueExpr::GlobalRef {
                        module: "Print".into(),
                        name: "print".into(),
                    }),
                    args: vec![ValueExpr::StrLit("hello".into())],
                }),
            },
        )]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_decl_with_residual_effects() {
        let m = simple_module(vec![IrDecl {
            name: "effectful_fn".into(),
            params: vec![],
            result_ty: IrType::Unit,
            residual_effects: vec!["Read".into(), "Print".into()],
            body: val(ValueExpr::Unit),
        }]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    // --- validation tests ---

    #[test]
    fn validate_ok_simple() {
        let m = simple_module(vec![simple_decl(
            "ok",
            IrType::Int,
            val(ValueExpr::IntLit(1)),
        )]);
        assert!(validate_ir(&m).is_ok());
    }

    #[test]
    fn validate_ok_list_lit_with_nested_values() {
        let m = simple_module(vec![simple_decl(
            "items",
            IrType::Unknown,
            val(ValueExpr::ListLit {
                elements: vec![
                    ValueExpr::IntLit(1),
                    ValueExpr::BinOp {
                        op: IrBinOp::Add,
                        left: Box::new(ValueExpr::IntLit(2)),
                        right: Box::new(ValueExpr::IntLit(3)),
                    },
                ],
                spread: Some(Box::new(ValueExpr::Var("rest".into()))),
            }),
        )]);
        assert!(validate_ir(&m).is_ok());
    }

    #[test]
    fn validate_ok_tuple_record_lambda_case_and_mutation() {
        let m = simple_module(vec![simple_decl(
            "ok_ir2",
            IrType::Unknown,
            CompExpr::LetMut {
                name: "state".into(),
                ty: IrType::Unknown,
                value: Box::new(val(ValueExpr::RecordLit {
                    constructor: "Pair".into(),
                    fields: vec![(
                        "items".into(),
                        ValueExpr::TupleLit(vec![
                            ValueExpr::IntLit(1),
                            ValueExpr::Lambda {
                                param: "x".into(),
                                body: Box::new(val(ValueExpr::Var("x".into()))),
                            },
                        ]),
                    )],
                })),
                body: Box::new(CompExpr::Case {
                    scrutinee: Box::new(ValueExpr::Var("xs".into())),
                    arms: vec![IrCaseArm {
                        pattern: IrCasePattern::Wildcard,
                        body: CompExpr::Assign {
                            name: "state".into(),
                            value: Box::new(val(ValueExpr::Var("state".into()))),
                        },
                    }],
                }),
            },
        )]);
        assert!(validate_ir(&m).is_ok());
    }

    #[test]
    fn validate_err_empty_handle_clauses() {
        let m = simple_module(vec![simple_decl(
            "bad_handler",
            IrType::Unknown,
            CompExpr::Handle { clauses: vec![] },
        )]);
        let err = validate_ir(&m).unwrap_err();
        assert!(
            err.message.contains("at least one clause"),
            "{}",
            err.message
        );
    }

    #[test]
    fn validate_ok_with_handler() {
        let m = simple_module(vec![simple_decl(
            "ok_with",
            IrType::Unit,
            CompExpr::WithHandler {
                handler: Box::new(CompExpr::Handle {
                    clauses: vec![IrHandlerClause {
                        op_name: "read".into(),
                        params: vec![],
                        body: val(ValueExpr::Unit),
                    }],
                }),
                body: Box::new(val(ValueExpr::Unit)),
            },
        )]);
        assert!(validate_ir(&m).is_ok());
    }

    #[test]
    fn validate_err_empty_args_call() {
        let m = simple_module(vec![simple_decl(
            "bad_call",
            IrType::Unknown,
            CompExpr::Call {
                callee: Box::new(ValueExpr::Var("f".into())),
                args: vec![],
            },
        )]);
        let err = validate_ir(&m).unwrap_err();
        assert!(
            err.message.contains("at least one argument"),
            "{}",
            err.message
        );
    }

    #[test]
    fn validate_err_empty_case_arms() {
        let m = simple_module(vec![simple_decl(
            "bad_case",
            IrType::Unknown,
            CompExpr::Case {
                scrutinee: Box::new(ValueExpr::Var("xs".into())),
                arms: vec![],
            },
        )]);
        let err = validate_ir(&m).unwrap_err();
        assert!(err.message.contains("at least one arm"), "{}", err.message);
    }

    #[test]
    fn validate_err_empty_list_pattern() {
        let m = simple_module(vec![simple_decl(
            "bad_pattern",
            IrType::Unknown,
            CompExpr::Case {
                scrutinee: Box::new(ValueExpr::Var("xs".into())),
                arms: vec![IrCaseArm {
                    pattern: IrCasePattern::ListPattern {
                        items: vec![],
                        tail: None,
                    },
                    body: val(ValueExpr::Unit),
                }],
            },
        )]);
        let err = validate_ir(&m).unwrap_err();
        assert!(
            err.message
                .contains("ListPattern must bind at least one item or tail"),
            "{}",
            err.message
        );
    }

    #[test]
    fn validate_ok_let_if() {
        let m = simple_module(vec![decl_with_params_ir(
            "f",
            vec![("x", IrType::Bool)],
            IrType::Int,
            CompExpr::Let {
                name: "v".into(),
                ty: IrType::Int,
                value: Box::new(CompExpr::If {
                    cond: Box::new(ValueExpr::Var("x".into())),
                    then_: Box::new(val(ValueExpr::IntLit(1))),
                    else_: Box::new(val(ValueExpr::IntLit(0))),
                }),
                body: Box::new(val(ValueExpr::Var("v".into()))),
            },
        )]);
        assert!(validate_ir(&m).is_ok());
    }
}
