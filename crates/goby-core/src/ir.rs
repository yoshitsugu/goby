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
//! `PerformEffect`, `Handle`, and `Resume` are reserved as structural nodes.
//! Full lowering of effect handlers is deferred to Track G4+.

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
    And,
    Add,
    Mul,
    Eq,
    Lt,
    Gt,
}

/// A pure value expression that does not perform effects.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueExpr {
    IntLit(i64),
    BoolLit(bool),
    StrLit(String),
    /// Interpolated string with pure parts only.
    Interp(Vec<IrInterpPart>),
    /// Local variable reference.
    Var(String),
    /// Qualified global reference (e.g. `Module.name`).
    GlobalRef { module: String, name: String },
    BinOp {
        op: IrBinOp,
        left: Box<ValueExpr>,
        right: Box<ValueExpr>,
    },
    /// Unit value `()`.
    Unit,
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
    /// Effect operation invocation (reserved; full lowering in G4+).
    PerformEffect {
        effect: String,
        op: String,
        args: Vec<ValueExpr>,
    },
    /// Handler installation (reserved structural stub; full lowering in G4+).
    Handle {
        body: Box<CompExpr>,
    },
    /// Resume from an effect handler (reserved structural stub; full lowering in G4+).
    Resume {
        value: Box<ValueExpr>,
    },
}

/// A top-level declaration in the IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrDecl {
    pub name: String,
    pub params: Vec<(String, IrType)>,
    pub result_ty: IrType,
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
                IrBinOp::And => "&&",
                IrBinOp::Add => "+",
                IrBinOp::Mul => "*",
                IrBinOp::Eq => "==",
                IrBinOp::Lt => "<",
                IrBinOp::Gt => ">",
            });
            out.push(' ');
            fmt_value(out, right);
            out.push(')');
        }
        ValueExpr::Unit => out.push_str("()"),
    }
}

fn fmt_comp(out: &mut String, c: &CompExpr, depth: usize) {
    match c {
        CompExpr::Value(v) => {
            indent(out, depth);
            fmt_value(out, v);
            out.push('\n');
        }
        CompExpr::Let { name, ty, value, body } => {
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
        CompExpr::Handle { body } => {
            indent(out, depth);
            out.push_str("handle\n");
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
/// Scope (Track G2): structural checks only.
/// Undefined-variable-reference checking is deferred to G4+.
///
/// Checks:
/// - Reserved stub nodes (`Handle`, `Resume`) must not have non-trivial bodies
///   (Handle body must not itself be Handle/Resume stubs at the top level — this
///   is a placeholder constraint until G4 fleshes out the invariant).
pub fn validate_ir(module: &IrModule) -> Result<(), IrValidateError> {
    for decl in &module.decls {
        validate_comp(&decl.body, &decl.name)?;
    }
    Ok(())
}

fn validate_comp(c: &CompExpr, decl_name: &str) -> Result<(), IrValidateError> {
    match c {
        CompExpr::Value(_) => Ok(()),
        CompExpr::Let { value, body, .. } => {
            validate_comp(value, decl_name)?;
            validate_comp(body, decl_name)
        }
        CompExpr::Seq { stmts, tail } => {
            for s in stmts {
                validate_comp(s, decl_name)?;
            }
            validate_comp(tail, decl_name)
        }
        CompExpr::If { then_, else_, .. } => {
            validate_comp(then_, decl_name)?;
            validate_comp(else_, decl_name)
        }
        CompExpr::Call { args, .. } => {
            if args.is_empty() {
                Err(IrValidateError {
                    message: format!(
                        "in decl `{}`: Call node must have at least one argument",
                        decl_name
                    ),
                })
            } else {
                Ok(())
            }
        }
        CompExpr::PerformEffect { .. } => Ok(()),
        CompExpr::Handle { body } => {
            // Reserved stub: body must not itself be a bare Handle or Resume at top level
            // (those would indicate malformed nesting in the stub phase).
            match body.as_ref() {
                CompExpr::Handle { .. } => Err(IrValidateError {
                    message: format!(
                        "in decl `{}`: nested Handle stubs are not valid",
                        decl_name
                    ),
                }),
                CompExpr::Resume { .. } => Err(IrValidateError {
                    message: format!(
                        "in decl `{}`: Resume directly inside Handle stub is not valid",
                        decl_name
                    ),
                }),
                other => validate_comp(other, decl_name),
            }
        }
        CompExpr::Resume { .. } => Ok(()),
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

    // --- printer tests ---

    #[test]
    fn print_int_lit() {
        let m = simple_module(vec![IrDecl {
            name: "answer".into(),
            params: vec![],
            result_ty: IrType::Int,
            body: val(ValueExpr::IntLit(42)),
        }]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_bool_lit() {
        let m = simple_module(vec![IrDecl {
            name: "flag".into(),
            params: vec![],
            result_ty: IrType::Bool,
            body: val(ValueExpr::BoolLit(true)),
        }]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_str_lit() {
        let m = simple_module(vec![IrDecl {
            name: "greeting".into(),
            params: vec![],
            result_ty: IrType::Str,
            body: val(ValueExpr::StrLit("hello".into())),
        }]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_var() {
        let m = simple_module(vec![IrDecl {
            name: "f".into(),
            params: vec![("x".into(), IrType::Int)],
            result_ty: IrType::Int,
            body: val(ValueExpr::Var("x".into())),
        }]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_global_ref() {
        let m = simple_module(vec![IrDecl {
            name: "use_io".into(),
            params: vec![],
            result_ty: IrType::Unknown,
            body: CompExpr::Call {
                callee: Box::new(ValueExpr::GlobalRef {
                    module: "Print".into(),
                    name: "print".into(),
                }),
                args: vec![ValueExpr::StrLit("hi".into())],
            },
        }]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_binop_all_ops() {
        let ops = [
            (IrBinOp::And, "&&"),
            (IrBinOp::Add, "+"),
            (IrBinOp::Mul, "*"),
            (IrBinOp::Eq, "=="),
            (IrBinOp::Lt, "<"),
            (IrBinOp::Gt, ">"),
        ];
        for (op, _sym) in ops {
            let m = simple_module(vec![IrDecl {
                name: "op_test".into(),
                params: vec![],
                result_ty: IrType::Unknown,
                body: val(ValueExpr::BinOp {
                    op,
                    left: Box::new(ValueExpr::IntLit(1)),
                    right: Box::new(ValueExpr::IntLit(2)),
                }),
            }]);
            // Just ensure no panic; snapshot covers the Add case below.
            let _ = fmt_ir(&m);
        }
    }

    #[test]
    fn print_binop_add_snapshot() {
        let m = simple_module(vec![IrDecl {
            name: "add".into(),
            params: vec![("a".into(), IrType::Int), ("b".into(), IrType::Int)],
            result_ty: IrType::Int,
            body: val(ValueExpr::BinOp {
                op: IrBinOp::Add,
                left: Box::new(ValueExpr::Var("a".into())),
                right: Box::new(ValueExpr::Var("b".into())),
            }),
        }]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_let() {
        let m = simple_module(vec![IrDecl {
            name: "with_let".into(),
            params: vec![],
            result_ty: IrType::Int,
            body: CompExpr::Let {
                name: "x".into(),
                ty: IrType::Int,
                value: Box::new(val(ValueExpr::IntLit(10))),
                body: Box::new(val(ValueExpr::Var("x".into()))),
            },
        }]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_if() {
        let m = simple_module(vec![IrDecl {
            name: "check".into(),
            params: vec![("b".into(), IrType::Bool)],
            result_ty: IrType::Int,
            body: CompExpr::If {
                cond: Box::new(ValueExpr::Var("b".into())),
                then_: Box::new(val(ValueExpr::IntLit(1))),
                else_: Box::new(val(ValueExpr::IntLit(0))),
            },
        }]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_seq() {
        let m = simple_module(vec![IrDecl {
            name: "seq_example".into(),
            params: vec![],
            result_ty: IrType::Unit,
            body: CompExpr::Seq {
                stmts: vec![
                    CompExpr::Call {
                        callee: Box::new(ValueExpr::GlobalRef {
                            module: "Print".into(),
                            name: "print".into(),
                        }),
                        args: vec![ValueExpr::StrLit("a".into())],
                    },
                ],
                tail: Box::new(val(ValueExpr::Unit)),
            },
        }]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn print_call() {
        let m = simple_module(vec![IrDecl {
            name: "invoke".into(),
            params: vec![],
            result_ty: IrType::Unknown,
            body: CompExpr::Call {
                callee: Box::new(ValueExpr::Var("f".into())),
                args: vec![ValueExpr::IntLit(1), ValueExpr::IntLit(2)],
            },
        }]);
        insta::assert_snapshot!(fmt_ir(&m));
    }

    // --- validation tests ---

    #[test]
    fn validate_ok_simple() {
        let m = simple_module(vec![IrDecl {
            name: "ok".into(),
            params: vec![],
            result_ty: IrType::Int,
            body: val(ValueExpr::IntLit(1)),
        }]);
        assert!(validate_ir(&m).is_ok());
    }

    #[test]
    fn validate_err_nested_handle_stubs() {
        let m = simple_module(vec![IrDecl {
            name: "bad".into(),
            params: vec![],
            result_ty: IrType::Unknown,
            body: CompExpr::Handle {
                body: Box::new(CompExpr::Handle {
                    body: Box::new(val(ValueExpr::Unit)),
                }),
            },
        }]);
        let err = validate_ir(&m).unwrap_err();
        assert!(err.message.contains("nested Handle"), "{}", err.message);
    }

    #[test]
    fn validate_err_resume_inside_handle_stub() {
        let m = simple_module(vec![IrDecl {
            name: "bad".into(),
            params: vec![],
            result_ty: IrType::Unknown,
            body: CompExpr::Handle {
                body: Box::new(CompExpr::Resume {
                    value: Box::new(ValueExpr::Unit),
                }),
            },
        }]);
        let err = validate_ir(&m).unwrap_err();
        assert!(err.message.contains("Resume"), "{}", err.message);
    }

    #[test]
    fn validate_err_empty_args_call() {
        let m = simple_module(vec![IrDecl {
            name: "bad_call".into(),
            params: vec![],
            result_ty: IrType::Unknown,
            body: CompExpr::Call {
                callee: Box::new(ValueExpr::Var("f".into())),
                args: vec![],
            },
        }]);
        let err = validate_ir(&m).unwrap_err();
        assert!(err.message.contains("at least one argument"), "{}", err.message);
    }

    #[test]
    fn validate_ok_let_if() {
        let m = simple_module(vec![IrDecl {
            name: "f".into(),
            params: vec![("x".into(), IrType::Bool)],
            result_ty: IrType::Int,
            body: CompExpr::Let {
                name: "v".into(),
                ty: IrType::Int,
                value: Box::new(CompExpr::If {
                    cond: Box::new(ValueExpr::Var("x".into())),
                    then_: Box::new(val(ValueExpr::IntLit(1))),
                    else_: Box::new(val(ValueExpr::IntLit(0))),
                }),
                body: Box::new(val(ValueExpr::Var("v".into()))),
            },
        }]);
        assert!(validate_ir(&m).is_ok());
    }
}
