//! AST-to-IR lowering for the pure typed subset (Track G3).
//!
//! This module lowers a typed subset of the Goby AST into the shared IR
//! defined in `ir.rs`. Only pure expressions are accepted; effectful forms
//! (`With`, `Handler`, `Resume`, `Lambda`, `MethodCall`, `Pipeline`) return a
//! `LowerError`.
//!
//! # Curried calls
//! The AST uses curried single-argument `Expr::Call { callee, arg }` chains.
//! This lowerer collects the full chain into a flat multi-argument IR `Call` node.
//!
//! # Type information
//! The typechecker does not currently expose a typed AST. All IR nodes are
//! annotated with `IrType::Unknown` unless the lowerer can infer the type
//! from the AST literal form (Int/Bool/Str).

use crate::ast::{BinOpKind, Declaration, Expr, InterpolatedPart, Module, Stmt};
use crate::ir::{
    CompExpr, IrBinOp, IrDecl, IrHandlerClause, IrInterpPart, IrModule, IrType, ValueExpr,
};

/// An error produced when an AST node cannot be lowered to IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LowerError {
    pub message: String,
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "IR lowering error: {}", self.message)
    }
}

impl std::error::Error for LowerError {}

fn err(msg: impl Into<String>) -> LowerError {
    LowerError { message: msg.into() }
}

// ---------------------------------------------------------------------------
// Public entry points
// ---------------------------------------------------------------------------

/// Lower a parsed `Module` to an `IrModule`.
///
/// Fail-fast: if any declaration body fails to lower, the whole module returns
/// an error. This is intentional for the pure-subset stage (G3). Future tracks
/// (G4+) may accumulate per-declaration errors instead.
pub fn lower_module(module: &Module) -> Result<IrModule, LowerError> {
    let mut decls = Vec::new();
    for decl in &module.declarations {
        decls.push(lower_declaration(decl)?);
    }
    Ok(IrModule { decls })
}

/// Lower a single `Declaration` to an `IrDecl`.
pub fn lower_declaration(decl: &Declaration) -> Result<IrDecl, LowerError> {
    let stmts = decl.parsed_body.as_deref().ok_or_else(|| {
        err(format!("declaration `{}` has no parsed body", decl.name))
    })?;

    let body = lower_stmts(stmts)?;

    // Parameters: types are unknown at this stage.
    let params: Vec<(String, IrType)> =
        decl.params.iter().map(|p| (p.clone(), IrType::Unknown)).collect();

    // Extract residual effects from the `can` clause in the type annotation.
    let residual_effects = extract_residual_effects(decl.type_annotation.as_deref());

    Ok(IrDecl {
        name: decl.name.clone(),
        params,
        result_ty: IrType::Unknown,
        residual_effects,
        body,
    })
}

/// Extract effect names from a `can EffectA, EffectB` clause in a type annotation.
/// Returns an empty Vec if no `can` clause is present.
fn extract_residual_effects(annotation: Option<&str>) -> Vec<String> {
    let Some(ann) = annotation else { return vec![] };
    let Some(idx) = crate::find_can_keyword_index(ann) else { return vec![] };
    let effects_raw = ann[idx + 3..].trim();
    effects_raw
        .split(',')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect()
}

// ---------------------------------------------------------------------------
// Statement / expression lowering
// ---------------------------------------------------------------------------

/// Lower a slice of statements into a `CompExpr`.
///
/// An empty statement list is lowered to `Value(Unit)`.
/// A single `Stmt::Expr` is lowered as its tail value.
/// Multiple statements are lowered into a `Let`/`Seq` chain.
fn lower_stmts(stmts: &[Stmt]) -> Result<CompExpr, LowerError> {
    if stmts.is_empty() {
        return Ok(CompExpr::Value(ValueExpr::Unit));
    }

    // Build from right to left so we can construct Let chains.
    lower_stmts_slice(stmts)
}

fn lower_stmts_slice(stmts: &[Stmt]) -> Result<CompExpr, LowerError> {
    match stmts {
        [] => Ok(CompExpr::Value(ValueExpr::Unit)),
        [Stmt::Expr(expr, _)] => lower_expr_as_comp(expr),
        [Stmt::Binding { name, value, .. }] => {
            // A lone binding as the final statement is lowered to `let name = value in name`.
            // The binding's own value is exposed as the block result.
            // This is an explicit semantics choice: the block yields the bound value.
            // Backends that expect Unit-returning blocks should handle this at
            // their own IR analysis layer.
            let val_comp = lower_expr_as_comp(value)?;
            Ok(CompExpr::Let {
                name: name.clone(),
                ty: infer_type_from_expr(value),
                value: Box::new(val_comp),
                body: Box::new(CompExpr::Value(ValueExpr::Var(name.clone()))),
            })
        }
        [Stmt::MutBinding { name, .. }] => Err(err(format!(
            "mutable binding `{}` is not supported in the pure IR subset",
            name
        ))),
        [Stmt::Assign { name, .. }] => Err(err(format!(
            "assignment to `{}` is not supported in the pure IR subset",
            name
        ))),
        [head, rest @ ..] => match head {
            Stmt::Binding { name, value, .. } => {
                let val_comp = lower_expr_as_comp(value)?;
                let body = lower_stmts_slice(rest)?;
                Ok(CompExpr::Let {
                    name: name.clone(),
                    ty: infer_type_from_expr(value),
                    value: Box::new(val_comp),
                    body: Box::new(body),
                })
            }
            Stmt::Expr(expr, _) => {
                let head_comp = lower_expr_as_comp(expr)?;
                let tail = lower_stmts_slice(rest)?;
                // Use Seq to sequence a discarded expression with the rest.
                // Avoid Vec::insert(0, ...) to prevent O(n²) growth for long Seq chains.
                match tail {
                    CompExpr::Seq { stmts: mut seq_stmts, tail } => {
                        // Prepend by building a new vec: head + existing stmts.
                        let mut merged = Vec::with_capacity(1 + seq_stmts.len());
                        merged.push(head_comp);
                        merged.append(&mut seq_stmts);
                        Ok(CompExpr::Seq { stmts: merged, tail })
                    }
                    other => Ok(CompExpr::Seq {
                        stmts: vec![head_comp],
                        tail: Box::new(other),
                    }),
                }
            }
            Stmt::MutBinding { name, .. } => Err(err(format!(
                "mutable binding `{}` is not supported in the pure IR subset",
                name
            ))),
            Stmt::Assign { name, .. } => Err(err(format!(
                "assignment to `{}` is not supported in the pure IR subset",
                name
            ))),
        },
    }
}

/// Lower an `Expr` as a `CompExpr`.
fn lower_expr_as_comp(expr: &Expr) -> Result<CompExpr, LowerError> {
    // First try as a pure value.
    match try_lower_value(expr)? {
        Some(v) => Ok(CompExpr::Value(v)),
        None => lower_expr_as_comp_non_value(expr),
    }
}

/// Lower compound expressions that are not pure values.
fn lower_expr_as_comp_non_value(expr: &Expr) -> Result<CompExpr, LowerError> {
    match expr {
        Expr::If { condition, then_expr, else_expr } => {
            let cond = lower_value_required(condition, "if condition")?;
            let then_ = lower_expr_as_comp(then_expr)?;
            let else_ = lower_expr_as_comp(else_expr)?;
            Ok(CompExpr::If {
                cond: Box::new(cond),
                then_: Box::new(then_),
                else_: Box::new(else_),
            })
        }
        Expr::Block(stmts) => lower_stmts(stmts),
        Expr::Call { .. } => {
            // Collect curried call chain into flat IR Call.
            let (callee_expr, args_exprs) = collect_call_chain(expr);
            let callee = lower_value_required(callee_expr, "call callee")?;
            let mut args = Vec::new();
            for a in args_exprs {
                args.push(lower_value_required(a, "call argument")?);
            }
            Ok(CompExpr::Call {
                callee: Box::new(callee),
                args,
            })
        }
        Expr::Lambda { param, .. } => Err(err(format!(
            "lambda `\\{}` is not supported in the pure IR subset (G3); deferred to a later track",
            param
        ))),
        Expr::Handler { clauses } => lower_handler_expr(clauses),
        Expr::With { handler, body } => {
            let ir_handler = lower_expr_as_comp(handler)?;
            let ir_body = lower_stmts(body)?;
            Ok(CompExpr::WithHandler {
                handler: Box::new(ir_handler),
                body: Box::new(ir_body),
            })
        }
        Expr::Resume { value } => {
            // Resume requires a pure value to resume with.
            let ir_value = lower_value_required(value, "resume value")?;
            Ok(CompExpr::Resume { value: Box::new(ir_value) })
        }
        Expr::MethodCall { receiver, method, .. } => Err(err(format!(
            "method call `{}.{}` is not supported in the pure IR subset",
            receiver, method
        ))),
        Expr::Pipeline { callee, .. } => Err(err(format!(
            "pipeline `|> {}` is not supported in the pure IR subset",
            callee
        ))),
        Expr::Case { .. } => Err(err(
            "case expressions are not supported in the pure IR subset (G3); deferred to a later track",
        )),
        Expr::ListLit { .. } => Err(err(
            "list literals are not supported in the pure IR subset (G3)",
        )),
        Expr::ListIndex { .. } => Err(err(
            "list indexing is not supported in the pure IR subset (G3)",
        )),
        Expr::TupleLit(items) if !items.is_empty() => Err(err(
            "non-unit tuple literals are not supported in the pure IR subset (G3)",
        )),
        Expr::RecordConstruct { constructor, .. } => Err(err(format!(
            "record construction `{}` is not supported in the pure IR subset (G3)",
            constructor
        ))),
        other => {
            // Fallback: attempt as value one more time (handles unit tuple, etc.)
            match try_lower_value(other)? {
                Some(v) => Ok(CompExpr::Value(v)),
                None => Err(err(format!("unsupported expression form: {:?}", other))),
            }
        }
    }
}

/// Attempt to lower an `Expr` as a pure `ValueExpr`.
///
/// Returns `Ok(None)` if the expression is not a pure value form (e.g., it is
/// an `If` or `Block`). Returns `Err` if the form is explicitly unsupported.
fn try_lower_value(expr: &Expr) -> Result<Option<ValueExpr>, LowerError> {
    match expr {
        Expr::IntLit(n) => Ok(Some(ValueExpr::IntLit(*n))),
        Expr::BoolLit(b) => Ok(Some(ValueExpr::BoolLit(*b))),
        Expr::StringLit(s) => Ok(Some(ValueExpr::StrLit(s.clone()))),
        Expr::TupleLit(items) if items.is_empty() => Ok(Some(ValueExpr::Unit)),
        Expr::Var { name, .. } => Ok(Some(ValueExpr::Var(name.clone()))),
        Expr::Qualified { receiver, member, .. } => Ok(Some(ValueExpr::GlobalRef {
            module: receiver.clone(),
            name: member.clone(),
        })),
        Expr::BinOp { op, left, right } => {
            let ir_op = lower_binop(op);
            let l = lower_value_required(left, "binary operator left operand")?;
            let r = lower_value_required(right, "binary operator right operand")?;
            Ok(Some(ValueExpr::BinOp {
                op: ir_op,
                left: Box::new(l),
                right: Box::new(r),
            }))
        }
        Expr::InterpolatedString(parts) => {
            let mut ir_parts = Vec::new();
            for part in parts {
                match part {
                    InterpolatedPart::Text(t) => {
                        ir_parts.push(IrInterpPart::Text(t.clone()));
                    }
                    InterpolatedPart::Expr(inner) => {
                        match try_lower_value(inner)? {
                            Some(v) => ir_parts.push(IrInterpPart::Expr(v)),
                            None => {
                                return Err(err(
                                    "interpolated string contains a non-pure expression; \
                                     only pure values are supported in the pure IR subset (G3)",
                                ));
                            }
                        }
                    }
                }
            }
            Ok(Some(ValueExpr::Interp(ir_parts)))
        }
        // These are compound/effectful — not pure values.
        Expr::If { .. }
        | Expr::Block(_)
        | Expr::Call { .. }
        | Expr::Lambda { .. }
        | Expr::Handler { .. }
        | Expr::With { .. }
        | Expr::Resume { .. }
        | Expr::MethodCall { .. }
        | Expr::Pipeline { .. }
        | Expr::Case { .. }
        | Expr::ListLit { .. }
        | Expr::ListIndex { .. }
        | Expr::RecordConstruct { .. } => Ok(None),
        Expr::TupleLit(_) => Ok(None), // non-empty tuple
    }
}

/// Lower an `Expr` as a `ValueExpr`, returning an error if not possible.
fn lower_value_required(expr: &Expr, context: &str) -> Result<ValueExpr, LowerError> {
    match try_lower_value(expr)? {
        Some(v) => Ok(v),
        None => Err(err(format!(
            "{} must be a pure value expression in the pure IR subset",
            context
        ))),
    }
}

/// Collect a left-associated curried `Call` chain into `(callee, [args])`.
///
/// `f(a)(b)(c)` in the AST is `Call { callee: Call { callee: Call { callee: f, arg: a }, arg: b }, arg: c }`.
/// This returns `(f, [a, b, c])`.
fn collect_call_chain(expr: &Expr) -> (&Expr, Vec<&Expr>) {
    let mut args = Vec::new();
    let mut cur = expr;
    while let Expr::Call { callee, arg, .. } = cur {
        args.push(arg.as_ref());
        cur = callee.as_ref();
    }
    args.reverse();
    (cur, args)
}

/// Lower a `Handler { clauses }` expression to an IR `Handle` node.
fn lower_handler_expr(clauses: &[crate::ast::HandlerClause]) -> Result<CompExpr, LowerError> {
    if clauses.is_empty() {
        return Err(err("handler expression must have at least one clause"));
    }
    let mut ir_clauses = Vec::with_capacity(clauses.len());
    for clause in clauses {
        let stmts = clause.parsed_body.as_deref().ok_or_else(|| {
            err(format!(
                "handler clause `{}` has no parsed body",
                clause.name
            ))
        })?;
        let body = lower_stmts(stmts)?;
        ir_clauses.push(IrHandlerClause {
            op_name: clause.name.clone(),
            params: clause.params.clone(),
            body,
        });
    }
    Ok(CompExpr::Handle { clauses: ir_clauses })
}

fn lower_binop(op: &BinOpKind) -> IrBinOp {
    match op {
        BinOpKind::And => IrBinOp::And,
        BinOpKind::Add => IrBinOp::Add,
        BinOpKind::Mul => IrBinOp::Mul,
        BinOpKind::Eq => IrBinOp::Eq,
        BinOpKind::Lt => IrBinOp::Lt,
        BinOpKind::Gt => IrBinOp::Gt,
    }
}

/// Infer a simple `IrType` from an `Expr`'s literal form.
fn infer_type_from_expr(expr: &Expr) -> IrType {
    match expr {
        Expr::IntLit(_) => IrType::Int,
        Expr::BoolLit(_) => IrType::Bool,
        Expr::StringLit(_) | Expr::InterpolatedString(_) => IrType::Str,
        Expr::TupleLit(items) if items.is_empty() => IrType::Unit,
        _ => IrType::Unknown,
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOpKind, Declaration, Expr, InterpolatedPart, Stmt};
    use crate::ir::{CompExpr, IrModule, ValueExpr, fmt_ir, validate_ir};

    /// Build a minimal Declaration with a pre-parsed body.
    fn decl_with_body(name: &str, stmts: Vec<Stmt>) -> Declaration {
        Declaration {
            name: name.to_string(),
            type_annotation: None,
            params: vec![],
            body: String::new(),
            parsed_body: Some(stmts),
            line: 1,
            col: 1,
        }
    }

    fn decl_with_params(name: &str, params: Vec<&str>, stmts: Vec<Stmt>) -> Declaration {
        Declaration {
            name: name.to_string(),
            type_annotation: None,
            params: params.iter().map(|s| s.to_string()).collect(),
            body: String::new(),
            parsed_body: Some(stmts),
            line: 1,
            col: 1,
        }
    }

    fn expr_stmt(e: Expr) -> Stmt {
        Stmt::Expr(e, None)
    }

    fn binding(name: &str, value: Expr) -> Stmt {
        Stmt::Binding { name: name.to_string(), value, span: None }
    }

    // --- acceptance tests (snapshot) ---

    #[test]
    fn lower_int_lit() {
        let decl = decl_with_body("answer", vec![expr_stmt(Expr::IntLit(42))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_bool_lit() {
        let decl = decl_with_body("flag", vec![expr_stmt(Expr::BoolLit(false))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_str_lit() {
        let decl = decl_with_body("msg", vec![expr_stmt(Expr::StringLit("hello".into()))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_var() {
        let decl = decl_with_params("id", vec!["x"], vec![expr_stmt(Expr::var("x"))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_qualified() {
        let decl = decl_with_body(
            "ref_io",
            vec![expr_stmt(Expr::qualified("Print", "print"))],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_binop_add() {
        let decl = decl_with_params(
            "add",
            vec!["a", "b"],
            vec![expr_stmt(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::var("a")),
                right: Box::new(Expr::var("b")),
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_binop_all_kinds() {
        // Verify no panic / compile for all six BinOpKind variants.
        let ops = [
            BinOpKind::And,
            BinOpKind::Add,
            BinOpKind::Mul,
            BinOpKind::Eq,
            BinOpKind::Lt,
            BinOpKind::Gt,
        ];
        for op in ops {
            let decl = decl_with_body(
                "op",
                vec![expr_stmt(Expr::BinOp {
                    op,
                    left: Box::new(Expr::IntLit(1)),
                    right: Box::new(Expr::IntLit(2)),
                })],
            );
            let result = lower_declaration(&decl);
            assert!(result.is_ok(), "op lowering failed: {:?}", result);
        }
    }

    #[test]
    fn lower_if_expr() {
        let decl = decl_with_params(
            "check",
            vec!["b"],
            vec![expr_stmt(Expr::If {
                condition: Box::new(Expr::var("b")),
                then_expr: Box::new(Expr::IntLit(1)),
                else_expr: Box::new(Expr::IntLit(0)),
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_let_binding() {
        let decl = decl_with_body(
            "with_let",
            vec![
                binding("x", Expr::IntLit(10)),
                expr_stmt(Expr::var("x")),
            ],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_multi_let_bindings() {
        let decl = decl_with_body(
            "multi_let",
            vec![
                binding("a", Expr::IntLit(1)),
                binding("b", Expr::IntLit(2)),
                expr_stmt(Expr::BinOp {
                    op: BinOpKind::Add,
                    left: Box::new(Expr::var("a")),
                    right: Box::new(Expr::var("b")),
                }),
            ],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_block_expr() {
        let decl = decl_with_body(
            "block_fn",
            vec![expr_stmt(Expr::Block(vec![
                binding("x", Expr::IntLit(5)),
                expr_stmt(Expr::var("x")),
            ]))],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_call_curried() {
        // f(a)(b) in AST: Call { callee: Call { callee: f, arg: a }, arg: b }
        // Expected IR: call f(a, b)
        let inner_call = Expr::Call {
            callee: Box::new(Expr::var("f")),
            arg: Box::new(Expr::var("a")),
            span: None,
        };
        let outer_call = Expr::Call {
            callee: Box::new(inner_call),
            arg: Box::new(Expr::var("b")),
            span: None,
        };
        let decl = decl_with_params("call_test", vec!["f", "a", "b"], vec![expr_stmt(outer_call)]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_interpolated_string_pure() {
        let decl = decl_with_params(
            "greet",
            vec!["name"],
            vec![expr_stmt(Expr::InterpolatedString(vec![
                InterpolatedPart::Text("Hello, ".into()),
                InterpolatedPart::Expr(Box::new(Expr::var("name"))),
                InterpolatedPart::Text("!".into()),
            ]))],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    // --- rejection tests ---

    #[test]
    fn reject_lambda() {
        let decl = decl_with_body(
            "with_lambda",
            vec![expr_stmt(Expr::Lambda {
                param: "x".into(),
                body: Box::new(Expr::var("x")),
            })],
        );
        let err = lower_declaration(&decl).unwrap_err();
        assert!(err.message.contains("lambda"), "{}", err.message);
    }

    #[test]
    fn lower_with_expr() {
        // with h do {}  -> WithHandler { handler: Value(Var("h")), body: Value(Unit) }
        let decl = decl_with_params(
            "use_handler",
            vec!["h"],
            vec![expr_stmt(Expr::With {
                handler: Box::new(Expr::var("h")),
                body: vec![],
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_resume() {
        // resume x  -> Resume { value: Var("x") }
        let decl = decl_with_params(
            "do_resume",
            vec!["x"],
            vec![expr_stmt(Expr::Resume { value: Box::new(Expr::var("x")) })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn reject_mut_binding() {
        let decl = decl_with_body(
            "mut_bind",
            vec![Stmt::MutBinding { name: "x".into(), value: Expr::IntLit(1), span: None }],
        );
        let err = lower_declaration(&decl).unwrap_err();
        assert!(err.message.contains("mutable binding"), "{}", err.message);
    }

    #[test]
    fn reject_interpolated_string_with_non_pure_expr() {
        // InterpolatedString containing a With expression (non-pure).
        let decl = decl_with_body(
            "bad_interp",
            vec![expr_stmt(Expr::InterpolatedString(vec![
                InterpolatedPart::Text("Result: ".into()),
                InterpolatedPart::Expr(Box::new(Expr::With {
                    handler: Box::new(Expr::var("h")),
                    body: vec![],
                })),
            ]))],
        );
        let err = lower_declaration(&decl).unwrap_err();
        assert!(err.message.contains("pure"), "{}", err.message);
    }

    #[test]
    fn lower_handler_empty_clauses_is_error() {
        // An empty handler expression should return LowerError.
        let decl = decl_with_body(
            "bad_handler",
            vec![expr_stmt(Expr::Handler { clauses: vec![] })],
        );
        let err = lower_declaration(&decl).unwrap_err();
        assert!(err.message.contains("clause"), "{}", err.message);
    }

    #[test]
    fn lower_handler_with_clause() {
        use crate::ast::{HandlerClause, Span};
        // handler { read resume -> resume "hi" }
        let decl = decl_with_body(
            "read_handler",
            vec![expr_stmt(Expr::Handler {
                clauses: vec![HandlerClause {
                    name: "read".into(),
                    params: vec!["resume".into()],
                    body: String::new(),
                    parsed_body: Some(vec![
                        expr_stmt(Expr::Resume { value: Box::new(Expr::StringLit("hi".into())) }),
                    ]),
                    span: Span::point(1, 1),
                }],
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn reject_assign() {
        let decl = decl_with_body(
            "assign_test",
            vec![Stmt::Assign { name: "x".into(), value: Expr::IntLit(1), span: None }],
        );
        let err = lower_declaration(&decl).unwrap_err();
        assert!(err.message.contains("assignment"), "{}", err.message);
    }

    // --- lone binding semantics test ---

    #[test]
    fn lower_lone_binding_yields_bound_value() {
        // A body consisting only of a single binding `x = 5` should lower to
        // `let x: Int = 5 in x`, yielding the bound value as the block result.
        let decl = decl_with_body("lone_bind", vec![binding("x", Expr::IntLit(5))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        // Verify structure: should be Let { name: "x", body: Value(Var("x")) }
        match &m.decls[0].body {
            CompExpr::Let { name, body, .. } => {
                assert_eq!(name, "x");
                assert!(
                    matches!(body.as_ref(), CompExpr::Value(ValueExpr::Var(v)) if v == "x"),
                    "body of lone binding should yield the bound variable"
                );
            }
            other => panic!("expected Let, got {:?}", other),
        }
        insta::assert_snapshot!(fmt_ir(&m));
    }

    // --- lower_module tests ---

    #[test]
    fn lower_module_two_decls() {
        use crate::ast::Module;
        let m = Module {
            imports: vec![],
            embed_declarations: vec![],
            type_declarations: vec![],
            effect_declarations: vec![],
            declarations: vec![
                decl_with_body("a", vec![expr_stmt(Expr::IntLit(1))]),
                decl_with_body("b", vec![expr_stmt(Expr::IntLit(2))]),
            ],
        };
        let ir = lower_module(&m).unwrap();
        assert_eq!(ir.decls.len(), 2);
        assert!(validate_ir(&ir).is_ok());
        insta::assert_snapshot!(fmt_ir(&ir));
    }

    #[test]
    fn lower_module_fails_on_effectful_decl() {
        use crate::ast::Module;
        let m = Module {
            imports: vec![],
            embed_declarations: vec![],
            type_declarations: vec![],
            effect_declarations: vec![],
            declarations: vec![
                decl_with_body("ok", vec![expr_stmt(Expr::IntLit(1))]),
                decl_with_body(
                    "bad",
                    vec![expr_stmt(Expr::Lambda {
                        param: "x".into(),
                        body: Box::new(Expr::var("x")),
                    })],
                ),
            ],
        };
        let err = lower_module(&m).unwrap_err();
        assert!(err.message.contains("lambda"), "{}", err.message);
    }

    // --- residual effects tests ---

    #[test]
    fn lower_declaration_extracts_residual_effects() {
        let mut decl = decl_with_body("effectful", vec![expr_stmt(Expr::IntLit(1))]);
        decl.type_annotation = Some("Unit -> Unit can Read, Print".into());
        let ir_decl = lower_declaration(&decl).unwrap();
        assert_eq!(ir_decl.residual_effects, vec!["Read", "Print"]);
    }

    #[test]
    fn lower_declaration_no_can_clause() {
        let decl = decl_with_body("pure_fn", vec![expr_stmt(Expr::IntLit(1))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        assert!(ir_decl.residual_effects.is_empty());
    }

    #[test]
    fn lower_declaration_residual_effects_in_fmt() {
        let mut decl = decl_with_body("io_fn", vec![expr_stmt(Expr::IntLit(42))]);
        decl.type_annotation = Some("Unit -> Unit can IO".into());
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    // --- parse-to-IR snapshot test for with/handler ---

    #[test]
    fn lower_with_handler_end_to_end() {
        use crate::ast::{HandlerClause, Span};
        // Simulates: handler { read resume -> resume "input" }
        // then: with my_handler do { read () }
        let handler_expr = Expr::Handler {
            clauses: vec![HandlerClause {
                name: "read".into(),
                params: vec!["resume".into()],
                body: String::new(),
                parsed_body: Some(vec![expr_stmt(Expr::Resume {
                    value: Box::new(Expr::StringLit("input".into())),
                })]),
                span: Span::point(1, 1),
            }],
        };
        let with_expr = Expr::With {
            handler: Box::new(handler_expr),
            body: vec![binding("result", Expr::var("result")), expr_stmt(Expr::var("result"))],
        };
        let decl = decl_with_body("main_effect", vec![expr_stmt(with_expr)]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule { decls: vec![ir_decl] };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }
}
