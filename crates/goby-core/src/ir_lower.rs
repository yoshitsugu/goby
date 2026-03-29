//! AST-to-IR lowering.
//!
//! The public wrappers accept parsed AST, but the actual lowering boundary is
//! the resolved front-end form in `resolved.rs`. This keeps helper/effect
//! identity normalization out of backend-specific logic.

use crate::ast::{BinOpKind, Declaration, Module};
use crate::ir::{
    CompExpr, IrBinOp, IrCaseArm, IrCasePattern, IrDecl, IrHandlerClause, IrInterpPart,
    IrListPatternItem, IrListPatternTail, IrModule, IrType, ValueExpr,
};
use crate::resolved::{
    ResolvedDeclaration, ResolvedExpr, ResolvedHandlerClause, ResolvedInterpolatedPart,
    ResolvedModule, ResolvedRef, ResolvedStmt,
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
    LowerError {
        message: msg.into(),
    }
}

#[derive(Default)]
struct LowerCtx {
    next_tmp: usize,
}

impl LowerCtx {
    fn fresh_tmp(&mut self, stem: &str) -> String {
        let name = format!("__goby_ir_{stem}_{}", self.next_tmp);
        self.next_tmp += 1;
        name
    }
}

/// Lower a parsed `Module` to an `IrModule`.
pub fn lower_module(module: &Module) -> Result<IrModule, LowerError> {
    let resolved = crate::resolved::resolve_module(module);
    lower_resolved_module(&resolved)
}

/// Lower a single parsed `Declaration` to an `IrDecl`.
pub fn lower_declaration(decl: &Declaration) -> Result<IrDecl, LowerError> {
    let resolved = crate::resolved::resolve_declaration(decl);
    lower_resolved_declaration(&resolved)
}

/// Lower a resolved module to shared IR.
pub fn lower_resolved_module(module: &ResolvedModule) -> Result<IrModule, LowerError> {
    let mut decls = Vec::new();
    for decl in &module.declarations {
        decls.push(lower_resolved_declaration(decl)?);
    }
    Ok(IrModule { decls })
}

/// Lower a single resolved declaration to shared IR.
pub fn lower_resolved_declaration(decl: &ResolvedDeclaration) -> Result<IrDecl, LowerError> {
    let mut ctx = LowerCtx::default();
    let body = lower_stmts(&mut ctx, &decl.body)?;

    let params: Vec<(String, IrType)> = decl
        .params
        .iter()
        .map(|p| (p.clone(), IrType::Unknown))
        .collect();

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
    let Some(idx) = crate::find_can_keyword_index(ann) else {
        return vec![];
    };
    let effects_raw = ann[idx + 3..].trim();
    effects_raw
        .split(',')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect()
}

fn lower_stmts(ctx: &mut LowerCtx, stmts: &[ResolvedStmt]) -> Result<CompExpr, LowerError> {
    if stmts.is_empty() {
        return Ok(CompExpr::Value(ValueExpr::Unit));
    }
    lower_stmts_slice(ctx, stmts)
}

fn lower_stmts_slice(ctx: &mut LowerCtx, stmts: &[ResolvedStmt]) -> Result<CompExpr, LowerError> {
    match stmts {
        [] => Ok(CompExpr::Value(ValueExpr::Unit)),
        [ResolvedStmt::Expr(expr, _)] => lower_expr_as_comp(ctx, expr),
        [ResolvedStmt::Binding { name, value, .. }] => {
            let val_comp = lower_expr_as_comp(ctx, value)?;
            Ok(CompExpr::Let {
                name: name.clone(),
                ty: infer_type_from_expr(value),
                value: Box::new(val_comp),
                body: Box::new(CompExpr::Value(ValueExpr::Var(name.clone()))),
            })
        }
        [ResolvedStmt::MutBinding { name, value, .. }] => {
            let val_comp = lower_expr_as_comp(ctx, value)?;
            Ok(CompExpr::LetMut {
                name: name.clone(),
                ty: infer_type_from_expr(value),
                value: Box::new(val_comp),
                body: Box::new(CompExpr::Value(ValueExpr::Var(name.clone()))),
            })
        }
        [ResolvedStmt::Assign { target, value, .. }] => {
            let target_name = mutable_target_name(target)?;
            Ok(CompExpr::Assign {
                name: target_name.to_string(),
                value: Box::new(lower_expr_as_comp(ctx, value)?),
            })
        }
        [head, rest @ ..] => match head {
            ResolvedStmt::Binding { name, value, .. } => {
                let val_comp = lower_expr_as_comp(ctx, value)?;
                let body = lower_stmts_slice(ctx, rest)?;
                Ok(CompExpr::Let {
                    name: name.clone(),
                    ty: infer_type_from_expr(value),
                    value: Box::new(val_comp),
                    body: Box::new(body),
                })
            }
            ResolvedStmt::Expr(expr, _) => {
                let head_comp = lower_expr_as_comp(ctx, expr)?;
                let tail = lower_stmts_slice(ctx, rest)?;
                match tail {
                    CompExpr::Seq {
                        stmts: mut seq_stmts,
                        tail,
                    } => {
                        let mut merged = Vec::with_capacity(1 + seq_stmts.len());
                        merged.push(head_comp);
                        merged.append(&mut seq_stmts);
                        Ok(CompExpr::Seq {
                            stmts: merged,
                            tail,
                        })
                    }
                    other => Ok(CompExpr::Seq {
                        stmts: vec![head_comp],
                        tail: Box::new(other),
                    }),
                }
            }
            ResolvedStmt::MutBinding { name, value, .. } => {
                let val_comp = lower_expr_as_comp(ctx, value)?;
                let body = lower_stmts_slice(ctx, rest)?;
                Ok(CompExpr::LetMut {
                    name: name.clone(),
                    ty: infer_type_from_expr(value),
                    value: Box::new(val_comp),
                    body: Box::new(body),
                })
            }
            ResolvedStmt::Assign { target, value, .. } => {
                let head_comp = CompExpr::Assign {
                    name: mutable_target_name(target)?.to_string(),
                    value: Box::new(lower_expr_as_comp(ctx, value)?),
                };
                let tail = lower_stmts_slice(ctx, rest)?;
                match tail {
                    CompExpr::Seq {
                        stmts: mut seq_stmts,
                        tail,
                    } => {
                        let mut merged = Vec::with_capacity(1 + seq_stmts.len());
                        merged.push(head_comp);
                        merged.append(&mut seq_stmts);
                        Ok(CompExpr::Seq {
                            stmts: merged,
                            tail,
                        })
                    }
                    other => Ok(CompExpr::Seq {
                        stmts: vec![head_comp],
                        tail: Box::new(other),
                    }),
                }
            }
        },
    }
}

fn lower_expr_as_comp(ctx: &mut LowerCtx, expr: &ResolvedExpr) -> Result<CompExpr, LowerError> {
    match try_lower_value(ctx, expr)? {
        Some(v) => Ok(CompExpr::Value(v)),
        None => lower_expr_as_comp_non_value(ctx, expr),
    }
}

fn lower_expr_as_comp_non_value(
    ctx: &mut LowerCtx,
    expr: &ResolvedExpr,
) -> Result<CompExpr, LowerError> {
    match expr {
        ResolvedExpr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            let then_ = lower_expr_as_comp(ctx, then_expr)?;
            let else_ = lower_expr_as_comp(ctx, else_expr)?;
            lower_expr_to_value_anf(ctx, condition, "if_condition", move |cond| {
                Ok(CompExpr::If {
                    cond: Box::new(cond),
                    then_: Box::new(then_),
                    else_: Box::new(else_),
                })
            })
        }
        ResolvedExpr::Block(stmts) => lower_stmts(ctx, stmts),
        ResolvedExpr::Call { .. } => {
            let (callee_expr, args_exprs) = collect_call_chain(expr);

            if let ResolvedExpr::Ref(ResolvedRef::EffectOp { effect, op }) = callee_expr {
                return lower_effect_call(ctx, effect, op, &args_exprs);
            }

            lower_ordinary_call(ctx, callee_expr, &args_exprs)
        }
        ResolvedExpr::Lambda { param, body } => Ok(CompExpr::Value(ValueExpr::Lambda {
            param: param.clone(),
            body: Box::new(lower_expr_as_comp(ctx, body)?),
        })),
        ResolvedExpr::Handler { clauses } => lower_handler_expr(ctx, clauses),
        ResolvedExpr::With { handler, body } => {
            let ir_handler = lower_expr_as_comp(ctx, handler)?;
            let ir_body = lower_stmts(ctx, body)?;
            Ok(CompExpr::WithHandler {
                handler: Box::new(ir_handler),
                body: Box::new(ir_body),
            })
        }
        ResolvedExpr::Resume { value } => {
            let ir_value = lower_value_required(ctx, value, "resume value")?;
            Ok(CompExpr::Resume {
                value: Box::new(ir_value),
            })
        }
        ResolvedExpr::BinOp { op, left, right } => {
            lower_binop_anf(ctx, lower_binop(op), left, right)
        }
        ResolvedExpr::MethodCall {
            receiver, method, ..
        } => Err(err(format!(
            "method call lowering is not implemented yet for `{}.{}`; shared IR uses canonical call forms",
            receiver, method
        ))),
        ResolvedExpr::Pipeline { callee, .. } => Err(err(format!(
            "pipeline lowering is not implemented yet for `|> {}`; shared IR uses canonical call forms",
            callee
        ))),
        ResolvedExpr::Case { scrutinee, arms } => lower_case_expr(ctx, scrutinee, arms),
        ResolvedExpr::ListLit { elements, spread } => {
            lower_list_literal(ctx, elements, spread.as_deref())
        }
        ResolvedExpr::TupleLit(items) if !items.is_empty() => lower_tuple_literal(ctx, items),
        ResolvedExpr::RecordConstruct {
            constructor,
            fields,
        } => lower_record_construct(ctx, constructor, fields),
        other => match try_lower_value(ctx, other)? {
            Some(v) => Ok(CompExpr::Value(v)),
            None => Err(err(format!("unsupported expression form: {:?}", other))),
        },
    }
}

fn try_lower_value(
    ctx: &mut LowerCtx,
    expr: &ResolvedExpr,
) -> Result<Option<ValueExpr>, LowerError> {
    match expr {
        ResolvedExpr::IntLit(n) => Ok(Some(ValueExpr::IntLit(*n))),
        ResolvedExpr::BoolLit(b) => Ok(Some(ValueExpr::BoolLit(*b))),
        ResolvedExpr::StringLit(s) => Ok(Some(ValueExpr::StrLit(s.clone()))),
        ResolvedExpr::TupleLit(items) if items.is_empty() => Ok(Some(ValueExpr::Unit)),
        ResolvedExpr::Lambda { param, body } => Ok(Some(ValueExpr::Lambda {
            param: param.clone(),
            body: Box::new(lower_expr_as_comp(ctx, body)?),
        })),
        ResolvedExpr::ListLit { elements, spread } => {
            let mut ir_elements = Vec::with_capacity(elements.len());
            for element in elements {
                let Some(ir_element) = try_lower_value(ctx, element)? else {
                    return Ok(None);
                };
                ir_elements.push(ir_element);
            }
            let ir_spread = match spread {
                Some(tail) => Some(Box::new(match try_lower_value(ctx, tail)? {
                    Some(value) => value,
                    None => return Ok(None),
                })),
                None => None,
            };
            Ok(Some(ValueExpr::ListLit {
                elements: ir_elements,
                spread: ir_spread,
            }))
        }
        ResolvedExpr::TupleLit(items) => {
            let mut ir_items = Vec::with_capacity(items.len());
            for item in items {
                let Some(ir_item) = try_lower_value(ctx, item)? else {
                    return Ok(None);
                };
                ir_items.push(ir_item);
            }
            Ok(Some(ValueExpr::TupleLit(ir_items)))
        }
        ResolvedExpr::RecordConstruct {
            constructor,
            fields,
        } => {
            let mut ir_fields = Vec::with_capacity(fields.len());
            for (name, value) in fields {
                let Some(ir_value) = try_lower_value(ctx, value)? else {
                    return Ok(None);
                };
                ir_fields.push((name.clone(), ir_value));
            }
            Ok(Some(ValueExpr::RecordLit {
                constructor: constructor.clone(),
                fields: ir_fields,
            }))
        }
        ResolvedExpr::Ref(reference) => Ok(Some(lower_ref_value(reference))),
        ResolvedExpr::UnaryOp { op, expr } => match op {
            crate::ast::UnaryOpKind::Not => {
                let value = lower_value_required(ctx, expr, "unary `!` operand")?;
                Ok(Some(ValueExpr::BinOp {
                    op: IrBinOp::Eq,
                    left: Box::new(value),
                    right: Box::new(ValueExpr::BoolLit(false)),
                }))
            }
        },
        ResolvedExpr::BinOp { op, left, right } => {
            // Only produce a pure ValueExpr when both operands are already values.
            // If either operand is non-value, return None so lower_expr_as_comp_non_value
            // can ANF-hoist them into Let bindings via lower_binop_anf.
            let Some(l) = try_lower_value(ctx, left)? else {
                return Ok(None);
            };
            let Some(r) = try_lower_value(ctx, right)? else {
                return Ok(None);
            };
            Ok(Some(ValueExpr::BinOp {
                op: lower_binop(op),
                left: Box::new(l),
                right: Box::new(r),
            }))
        }
        ResolvedExpr::InterpolatedString(parts) => {
            let mut ir_parts = Vec::new();
            for part in parts {
                match part {
                    ResolvedInterpolatedPart::Text(t) => {
                        ir_parts.push(IrInterpPart::Text(t.clone()));
                    }
                    ResolvedInterpolatedPart::Expr(inner) => match try_lower_value(ctx, inner)? {
                        Some(v) => ir_parts.push(IrInterpPart::Expr(v)),
                        None => {
                            return Err(err(
                                "interpolated string contains a non-pure expression; \
                                     only pure values are supported in shared IR today",
                            ));
                        }
                    },
                }
            }
            Ok(Some(ValueExpr::Interp(ir_parts)))
        }
        ResolvedExpr::TupleProject { receiver, index } => Ok(Some(ValueExpr::TupleProject {
            tuple: Box::new(ValueExpr::Var(receiver.clone())),
            index: *index,
        })),
        ResolvedExpr::If { .. }
        | ResolvedExpr::Block(_)
        | ResolvedExpr::Call { .. }
        | ResolvedExpr::Handler { .. }
        | ResolvedExpr::With { .. }
        | ResolvedExpr::Resume { .. }
        | ResolvedExpr::MethodCall { .. }
        | ResolvedExpr::Pipeline { .. }
        | ResolvedExpr::Case { .. } => Ok(None),
    }
}

fn lower_list_literal(
    ctx: &mut LowerCtx,
    elements: &[ResolvedExpr],
    spread: Option<&ResolvedExpr>,
) -> Result<CompExpr, LowerError> {
    let mut pending = Vec::new();
    let mut ir_elements = Vec::with_capacity(elements.len());

    for element in elements {
        if let Some(value) = try_lower_value(ctx, element)? {
            ir_elements.push(value);
        } else {
            let tmp = ctx.fresh_tmp("list_element");
            pending.push((tmp.clone(), lower_expr_as_comp(ctx, element)?));
            ir_elements.push(ValueExpr::Var(tmp));
        }
    }

    let ir_spread = match spread {
        Some(tail) => {
            if let Some(value) = try_lower_value(ctx, tail)? {
                Some(Box::new(value))
            } else {
                let tmp = ctx.fresh_tmp("list_spread");
                pending.push((tmp.clone(), lower_expr_as_comp(ctx, tail)?));
                Some(Box::new(ValueExpr::Var(tmp)))
            }
        }
        None => None,
    };

    let list_value = CompExpr::Value(ValueExpr::ListLit {
        elements: ir_elements,
        spread: ir_spread,
    });

    Ok(pending
        .into_iter()
        .rev()
        .fold(list_value, |body, (name, value)| CompExpr::Let {
            name,
            ty: IrType::Unknown,
            value: Box::new(value),
            body: Box::new(body),
        }))
}

fn lower_tuple_literal(ctx: &mut LowerCtx, items: &[ResolvedExpr]) -> Result<CompExpr, LowerError> {
    let mut pending = Vec::new();
    let mut ir_items = Vec::with_capacity(items.len());

    for item in items {
        if let Some(value) = try_lower_value(ctx, item)? {
            ir_items.push(value);
        } else {
            let tmp = ctx.fresh_tmp("tuple_item");
            pending.push((tmp.clone(), lower_expr_as_comp(ctx, item)?));
            ir_items.push(ValueExpr::Var(tmp));
        }
    }

    let tuple_value = CompExpr::Value(ValueExpr::TupleLit(ir_items));
    Ok(pending
        .into_iter()
        .rev()
        .fold(tuple_value, |body, (name, value)| CompExpr::Let {
            name,
            ty: IrType::Unknown,
            value: Box::new(value),
            body: Box::new(body),
        }))
}

fn lower_record_construct(
    ctx: &mut LowerCtx,
    constructor: &str,
    fields: &[(String, ResolvedExpr)],
) -> Result<CompExpr, LowerError> {
    let mut pending = Vec::new();
    let mut ir_fields = Vec::with_capacity(fields.len());

    for (name, value) in fields {
        if let Some(ir_value) = try_lower_value(ctx, value)? {
            ir_fields.push((name.clone(), ir_value));
        } else {
            let tmp = ctx.fresh_tmp("record_field");
            pending.push((tmp.clone(), lower_expr_as_comp(ctx, value)?));
            ir_fields.push((name.clone(), ValueExpr::Var(tmp)));
        }
    }

    let record_value = CompExpr::Value(ValueExpr::RecordLit {
        constructor: constructor.to_string(),
        fields: ir_fields,
    });
    Ok(pending
        .into_iter()
        .rev()
        .fold(record_value, |body, (name, value)| CompExpr::Let {
            name,
            ty: IrType::Unknown,
            value: Box::new(value),
            body: Box::new(body),
        }))
}

fn lower_case_expr(
    ctx: &mut LowerCtx,
    scrutinee: &ResolvedExpr,
    arms: &[crate::resolved::ResolvedCaseArm],
) -> Result<CompExpr, LowerError> {
    if arms.is_empty() {
        return Err(err("case expression must have at least one arm"));
    }

    let mut ir_arms = Vec::with_capacity(arms.len());
    for arm in arms {
        ir_arms.push(IrCaseArm {
            pattern: lower_case_pattern(&arm.pattern),
            body: lower_expr_as_comp(ctx, arm.body.as_ref())?,
        });
    }

    lower_expr_to_value_anf(ctx, scrutinee, "case_scrutinee", move |ir_scrutinee| {
        Ok(CompExpr::Case {
            scrutinee: Box::new(ir_scrutinee),
            arms: ir_arms,
        })
    })
}

fn lower_case_pattern(pattern: &crate::ast::CasePattern) -> IrCasePattern {
    match pattern {
        crate::ast::CasePattern::IntLit(n) => IrCasePattern::IntLit(*n),
        crate::ast::CasePattern::StringLit(text) => IrCasePattern::StringLit(text.clone()),
        crate::ast::CasePattern::BoolLit(flag) => IrCasePattern::BoolLit(*flag),
        crate::ast::CasePattern::EmptyList => IrCasePattern::EmptyList,
        crate::ast::CasePattern::ListPattern { items, tail } => IrCasePattern::ListPattern {
            items: items.iter().map(lower_list_pattern_item).collect(),
            tail: tail.as_ref().map(lower_list_pattern_tail),
        },
        crate::ast::CasePattern::Wildcard => IrCasePattern::Wildcard,
    }
}

fn lower_list_pattern_item(item: &crate::ast::ListPatternItem) -> IrListPatternItem {
    match item {
        crate::ast::ListPatternItem::IntLit(n) => IrListPatternItem::IntLit(*n),
        crate::ast::ListPatternItem::StringLit(text) => IrListPatternItem::StringLit(text.clone()),
        crate::ast::ListPatternItem::Bind(name) => IrListPatternItem::Bind(name.clone()),
        crate::ast::ListPatternItem::Wildcard => IrListPatternItem::Wildcard,
    }
}

fn lower_list_pattern_tail(tail: &crate::ast::ListPatternTail) -> IrListPatternTail {
    match tail {
        crate::ast::ListPatternTail::Ignore => IrListPatternTail::Ignore,
        crate::ast::ListPatternTail::Bind(name) => IrListPatternTail::Bind(name.clone()),
    }
}

fn lower_expr_to_value_anf<F>(
    ctx: &mut LowerCtx,
    expr: &ResolvedExpr,
    stem: &str,
    build: F,
) -> Result<CompExpr, LowerError>
where
    F: FnOnce(ValueExpr) -> Result<CompExpr, LowerError>,
{
    if let Some(value) = try_lower_value(ctx, expr)? {
        return build(value);
    }

    let tmp = ctx.fresh_tmp(stem);
    let value_comp = lower_expr_as_comp(ctx, expr)?;
    let body = build(ValueExpr::Var(tmp.clone()))?;
    Ok(CompExpr::Let {
        name: tmp,
        ty: IrType::Unknown,
        value: Box::new(value_comp),
        body: Box::new(body),
    })
}

/// ANF-hoist both operands of a binary operator.
/// If an operand is already a pure value it is used directly; otherwise a Let
/// binding is inserted for the temporary so the resulting expression remains
/// a valid ValueExpr::BinOp.
fn lower_binop_anf(
    ctx: &mut LowerCtx,
    op: IrBinOp,
    left: &ResolvedExpr,
    right: &ResolvedExpr,
) -> Result<CompExpr, LowerError> {
    // Resolve left operand (ANF if non-value).
    let (l_val, l_let): (ValueExpr, Option<(String, CompExpr)>) =
        if let Some(v) = try_lower_value(ctx, left)? {
            (v, None)
        } else {
            let tmp = ctx.fresh_tmp("binop_left");
            let comp = lower_expr_as_comp(ctx, left)?;
            (ValueExpr::Var(tmp.clone()), Some((tmp, comp)))
        };

    // Resolve right operand (ANF if non-value).
    let (r_val, r_let): (ValueExpr, Option<(String, CompExpr)>) =
        if let Some(v) = try_lower_value(ctx, right)? {
            (v, None)
        } else {
            let tmp = ctx.fresh_tmp("binop_right");
            let comp = lower_expr_as_comp(ctx, right)?;
            (ValueExpr::Var(tmp.clone()), Some((tmp, comp)))
        };

    let binop = CompExpr::Value(ValueExpr::BinOp {
        op,
        left: Box::new(l_val),
        right: Box::new(r_val),
    });

    // Wrap in Let bindings from innermost outward (right first, then left).
    let with_r = match r_let {
        None => binop,
        Some((name, value)) => CompExpr::Let {
            name,
            ty: IrType::Unknown,
            value: Box::new(value),
            body: Box::new(binop),
        },
    };
    let with_l = match l_let {
        None => with_r,
        Some((name, value)) => CompExpr::Let {
            name,
            ty: IrType::Unknown,
            value: Box::new(value),
            body: Box::new(with_r),
        },
    };
    Ok(with_l)
}

fn lower_value_required(
    ctx: &mut LowerCtx,
    expr: &ResolvedExpr,
    context: &str,
) -> Result<ValueExpr, LowerError> {
    match try_lower_value(ctx, expr)? {
        Some(v) => Ok(v),
        None => Err(err(format!(
            "{} must be a pure value expression in shared IR today",
            context
        ))),
    }
}

fn collect_call_chain(expr: &ResolvedExpr) -> (&ResolvedExpr, Vec<&ResolvedExpr>) {
    let mut args = Vec::new();
    let mut cur = expr;
    while let ResolvedExpr::Call { callee, arg, .. } = cur {
        args.push(arg.as_ref());
        cur = callee.as_ref();
    }
    args.reverse();
    (cur, args)
}

fn lower_handler_expr(
    ctx: &mut LowerCtx,
    clauses: &[ResolvedHandlerClause],
) -> Result<CompExpr, LowerError> {
    if clauses.is_empty() {
        return Err(err("handler expression must have at least one clause"));
    }
    let mut ir_clauses = Vec::with_capacity(clauses.len());
    for clause in clauses {
        let body = lower_stmts(ctx, &clause.body)?;
        ir_clauses.push(IrHandlerClause {
            op_name: clause.name.clone(),
            params: clause.params.clone(),
            body,
        });
    }
    Ok(CompExpr::Handle {
        clauses: ir_clauses,
    })
}

fn lower_effect_call(
    ctx: &mut LowerCtx,
    effect: &str,
    op: &str,
    args_exprs: &[&ResolvedExpr],
) -> Result<CompExpr, LowerError> {
    let filtered_args: Vec<&ResolvedExpr> = args_exprs
        .iter()
        .copied()
        .filter(|arg| !matches!(arg, ResolvedExpr::TupleLit(items) if items.is_empty()))
        .collect();

    match filtered_args.as_slice() {
        [] => Ok(CompExpr::PerformEffect {
            effect: effect.to_string(),
            op: op.to_string(),
            args: vec![],
        }),
        [arg] => match try_lower_value(ctx, arg)? {
            Some(value) => Ok(CompExpr::PerformEffect {
                effect: effect.to_string(),
                op: op.to_string(),
                args: vec![value],
            }),
            None => {
                let tmp = ctx.fresh_tmp("effect_arg");
                let body = CompExpr::PerformEffect {
                    effect: effect.to_string(),
                    op: op.to_string(),
                    args: vec![ValueExpr::Var(tmp.clone())],
                };
                Ok(CompExpr::Let {
                    name: tmp,
                    ty: IrType::Unknown,
                    value: Box::new(lower_expr_as_comp(ctx, arg)?),
                    body: Box::new(body),
                })
            }
        },
        _ => {
            let mut args = Vec::new();
            for arg in filtered_args {
                args.push(lower_value_required(ctx, arg, "effect operation argument")?);
            }
            Ok(CompExpr::PerformEffect {
                effect: effect.to_string(),
                op: op.to_string(),
                args,
            })
        }
    }
}

fn lower_ordinary_call(
    ctx: &mut LowerCtx,
    callee_expr: &ResolvedExpr,
    args_exprs: &[&ResolvedExpr],
) -> Result<CompExpr, LowerError> {
    let callee = lower_call_operand_to_value(ctx, callee_expr, "call_callee")?;
    let mut args = Vec::with_capacity(args_exprs.len());
    let mut pending = Vec::new();

    let callee = match callee {
        CallOperandLowering::Value(value) => value,
        CallOperandLowering::Pending { name, value } => {
            pending.push((name.clone(), value));
            ValueExpr::Var(name)
        }
    };

    for arg in args_exprs {
        let ir_arg = match lower_call_operand_to_value(ctx, arg, "call_arg")? {
            CallOperandLowering::Value(value) => value,
            CallOperandLowering::Pending { name, value } => {
                pending.push((name.clone(), value));
                ValueExpr::Var(name)
            }
        };
        args.push(ir_arg);
    }

    let mut body = CompExpr::Call {
        callee: Box::new(callee),
        args,
    };
    for (name, value) in pending.into_iter().rev() {
        body = CompExpr::Let {
            name,
            ty: IrType::Unknown,
            value: Box::new(value),
            body: Box::new(body),
        };
    }
    Ok(body)
}

enum CallOperandLowering {
    Value(ValueExpr),
    Pending { name: String, value: CompExpr },
}

fn lower_call_operand_to_value(
    ctx: &mut LowerCtx,
    expr: &ResolvedExpr,
    stem: &str,
) -> Result<CallOperandLowering, LowerError> {
    match try_lower_value(ctx, expr)? {
        Some(value) => Ok(CallOperandLowering::Value(value)),
        None => {
            let name = ctx.fresh_tmp(stem);
            let value = lower_expr_as_comp(ctx, expr)?;
            Ok(CallOperandLowering::Pending { name, value })
        }
    }
}

fn lower_binop(op: &BinOpKind) -> IrBinOp {
    match op {
        BinOpKind::Or => IrBinOp::Or,
        BinOpKind::And => IrBinOp::And,
        BinOpKind::Add => IrBinOp::Add,
        BinOpKind::Sub => IrBinOp::Sub,
        BinOpKind::Mul => IrBinOp::Mul,
        BinOpKind::Div => IrBinOp::Div,
        BinOpKind::Mod => IrBinOp::Mod,
        BinOpKind::Eq => IrBinOp::Eq,
        BinOpKind::Lt => IrBinOp::Lt,
        BinOpKind::Gt => IrBinOp::Gt,
        BinOpKind::Le => IrBinOp::Le,
        BinOpKind::Ge => IrBinOp::Ge,
    }
}

fn infer_type_from_expr(expr: &ResolvedExpr) -> IrType {
    match expr {
        ResolvedExpr::IntLit(_) => IrType::Int,
        ResolvedExpr::BoolLit(_) => IrType::Bool,
        ResolvedExpr::StringLit(_) | ResolvedExpr::InterpolatedString(_) => IrType::Str,
        ResolvedExpr::TupleLit(items) if items.is_empty() => IrType::Unit,
        _ => IrType::Unknown,
    }
}

fn lower_ref_value(reference: &ResolvedRef) -> ValueExpr {
    match reference {
        ResolvedRef::Local(name) | ResolvedRef::Decl(name) | ResolvedRef::ValueName(name) => {
            ValueExpr::Var(name.clone())
        }
        ResolvedRef::Helper { module, name } | ResolvedRef::Global { module, name } => {
            ValueExpr::GlobalRef {
                module: module.clone(),
                name: name.clone(),
            }
        }
        ResolvedRef::EffectOp { effect, op } => ValueExpr::GlobalRef {
            module: effect.clone(),
            name: op.clone(),
        },
    }
}

fn ref_name(reference: &ResolvedRef) -> &str {
    match reference {
        ResolvedRef::Local(name) | ResolvedRef::Decl(name) | ResolvedRef::ValueName(name) => name,
        ResolvedRef::Helper { name, .. }
        | ResolvedRef::EffectOp { op: name, .. }
        | ResolvedRef::Global { name, .. } => name,
    }
}

fn mutable_target_name(reference: &ResolvedRef) -> Result<&str, LowerError> {
    match reference {
        ResolvedRef::Local(name) | ResolvedRef::ValueName(name) => Ok(name),
        other => Err(err(format!(
            "assignment target must be a mutable local, got `{}`",
            ref_name(other)
        ))),
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        BinOpKind, CaseArm, CasePattern, Declaration, Expr, InterpolatedPart, ListPatternItem,
        ListPatternTail, Span, Stmt,
    };
    use crate::ir::{CompExpr, IrCasePattern, IrModule, ValueExpr, fmt_ir, validate_ir};

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
        Stmt::Binding {
            name: name.to_string(),
            value,
            span: None,
        }
    }

    // --- acceptance tests (snapshot) ---

    #[test]
    fn lower_int_lit() {
        let decl = decl_with_body("answer", vec![expr_stmt(Expr::IntLit(42))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_bool_lit() {
        let decl = decl_with_body("flag", vec![expr_stmt(Expr::BoolLit(false))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_str_lit() {
        let decl = decl_with_body("msg", vec![expr_stmt(Expr::StringLit("hello".into()))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_var() {
        let decl = decl_with_params("id", vec!["x"], vec![expr_stmt(Expr::var("x"))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_qualified() {
        let decl = decl_with_body("ref_io", vec![expr_stmt(Expr::qualified("Print", "print"))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
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
        let m = IrModule {
            decls: vec![ir_decl],
        };
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
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_if_with_helper_call_condition_introduces_temp_binding() {
        let module = crate::parse_module(
            r#"
is_two n =
  n == 2

choose n =
  if is_two n
    1
  else
    0
"#,
        )
        .expect("module should parse");
        let ir_module =
            lower_module(&module).expect("helper-call if condition should lower through ANF");
        let choose_decl = ir_module
            .decls
            .iter()
            .find(|decl| decl.name == "choose")
            .expect("choose decl should be present");
        match &choose_decl.body {
            CompExpr::Let {
                name, value, body, ..
            } => {
                assert!(name.starts_with("__goby_ir_if_condition_"));
                assert!(matches!(value.as_ref(), CompExpr::Call { .. }));
                assert!(matches!(
                    body.as_ref(),
                    CompExpr::If { cond, .. }
                        if matches!(cond.as_ref(), ValueExpr::Var(var) if var == name)
                ));
            }
            other => panic!(
                "expected helper-call if condition to ANF-normalize through let, got {other:?}"
            ),
        }
    }

    #[test]
    fn lower_nested_if_with_non_value_condition_introduces_temp_binding() {
        let decl = decl_with_params(
            "choose",
            vec!["flag"],
            vec![expr_stmt(Expr::If {
                condition: Box::new(Expr::If {
                    condition: Box::new(Expr::var("flag")),
                    then_expr: Box::new(Expr::BoolLit(true)),
                    else_expr: Box::new(Expr::BoolLit(false)),
                }),
                then_expr: Box::new(Expr::IntLit(1)),
                else_expr: Box::new(Expr::IntLit(0)),
            })],
        );
        let ir_decl = lower_declaration(&decl)
            .expect("nested non-value if condition should lower through ANF");
        match &ir_decl.body {
            CompExpr::Let {
                name, value, body, ..
            } => {
                assert!(name.starts_with("__goby_ir_if_condition_"));
                assert!(matches!(value.as_ref(), CompExpr::If { .. }));
                assert!(matches!(
                    body.as_ref(),
                    CompExpr::If { cond, .. }
                        if matches!(cond.as_ref(), ValueExpr::Var(var) if var == name)
                ));
            }
            other => {
                panic!("expected nested if condition to ANF-normalize through let, got {other:?}")
            }
        }
    }

    #[test]
    fn lower_case_expr() {
        let decl = decl_with_params(
            "select",
            vec!["n"],
            vec![expr_stmt(Expr::Case {
                scrutinee: Box::new(Expr::var("n")),
                arms: vec![
                    CaseArm {
                        pattern: CasePattern::IntLit(0),
                        body: Box::new(Expr::IntLit(10)),
                        span: Span::point(1, 1),
                    },
                    CaseArm {
                        pattern: CasePattern::ListPattern {
                            items: vec![
                                ListPatternItem::Bind("x".to_string()),
                                ListPatternItem::Wildcard,
                            ],
                            tail: Some(ListPatternTail::Bind("xs".to_string())),
                        },
                        body: Box::new(Expr::IntLit(20)),
                        span: Span::point(2, 1),
                    },
                    CaseArm {
                        pattern: CasePattern::Wildcard,
                        body: Box::new(Expr::IntLit(30)),
                        span: Span::point(3, 1),
                    },
                ],
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        assert_eq!(
            fmt_ir(&m),
            "decl select(n: ?): ? =\n  case n of\n    0 ->\n      10\n    [x, _, ..xs] ->\n      20\n    _ ->\n      30\n\n"
        );
    }

    #[test]
    fn lower_case_with_effectful_scrutinee_normalizes_via_let() {
        let decl = decl_with_body(
            "main",
            vec![expr_stmt(Expr::Case {
                scrutinee: Box::new(Expr::call(
                    Expr::qualified("Read", "read"),
                    Expr::unit_value(),
                )),
                arms: vec![
                    CaseArm {
                        pattern: CasePattern::StringLit("ok".to_string()),
                        body: Box::new(Expr::StringLit("yes".to_string())),
                        span: Span::point(1, 1),
                    },
                    CaseArm {
                        pattern: CasePattern::Wildcard,
                        body: Box::new(Expr::StringLit("no".to_string())),
                        span: Span::point(2, 1),
                    },
                ],
            })],
        );
        let ir_decl = lower_declaration(&decl).expect("case lowering should succeed");
        match &ir_decl.body {
            CompExpr::Let {
                name, value, body, ..
            } => {
                assert!(name.starts_with("__goby_ir_case_scrutinee_"));
                assert!(matches!(
                    value.as_ref(),
                    CompExpr::PerformEffect { effect, op, args }
                        if effect == "Read" && op == "read" && args.is_empty()
                ));
                assert!(matches!(
                    body.as_ref(),
                    CompExpr::Case { scrutinee, .. }
                        if matches!(scrutinee.as_ref(), ValueExpr::Var(var) if var == name)
                ));
            }
            other => {
                panic!("expected effectful scrutinee to ANF-normalize through let, got {other:?}")
            }
        }
    }

    #[test]
    fn lower_case_arm_body_effect_call() {
        let decl = decl_with_body(
            "main",
            vec![expr_stmt(Expr::Case {
                scrutinee: Box::new(Expr::IntLit(0)),
                arms: vec![
                    CaseArm {
                        pattern: CasePattern::IntLit(0),
                        body: Box::new(Expr::call(
                            Expr::qualified("Print", "println"),
                            Expr::StringLit("zero".to_string()),
                        )),
                        span: Span::point(1, 1),
                    },
                    CaseArm {
                        pattern: CasePattern::Wildcard,
                        body: Box::new(Expr::call(
                            Expr::qualified("Print", "println"),
                            Expr::StringLit("other".to_string()),
                        )),
                        span: Span::point(2, 1),
                    },
                ],
            })],
        );
        let ir_decl = lower_declaration(&decl).expect("case lowering should succeed");
        match &ir_decl.body {
            CompExpr::Case { arms, .. } => {
                assert_eq!(arms.len(), 2);
                assert!(matches!(arms[0].pattern, IrCasePattern::IntLit(0)));
                assert!(matches!(
                    arms[0].body,
                    CompExpr::PerformEffect { ref effect, ref op, ref args }
                        if effect == "Print"
                            && op == "println"
                            && matches!(args.as_slice(), [ValueExpr::StrLit(text)] if text == "zero")
                ));
            }
            other => panic!("expected case body, got {other:?}"),
        }
    }

    #[test]
    fn lower_let_binding() {
        let decl = decl_with_body(
            "with_let",
            vec![binding("x", Expr::IntLit(10)), expr_stmt(Expr::var("x"))],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
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
        let m = IrModule {
            decls: vec![ir_decl],
        };
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
        let m = IrModule {
            decls: vec![ir_decl],
        };
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
        let decl = decl_with_params(
            "call_test",
            vec!["f", "a", "b"],
            vec![expr_stmt(outer_call)],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
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
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    // --- rejection tests ---

    #[test]
    fn lower_lambda_value() {
        let decl = decl_with_body(
            "with_lambda",
            vec![expr_stmt(Expr::Lambda {
                param: "x".into(),
                body: Box::new(Expr::var("x")),
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        assert_eq!(fmt_ir(&m), "decl with_lambda: ? =\n  \\x ->\n  x\n\n\n");
    }

    #[test]
    fn lower_lambda_with_capture_and_effectful_body() {
        let decl = decl_with_body(
            "captured",
            vec![
                binding("base", Expr::IntLit(40)),
                expr_stmt(Expr::Lambda {
                    param: "x".into(),
                    body: Box::new(Expr::call(
                        Expr::qualified("Print", "println"),
                        Expr::InterpolatedString(vec![InterpolatedPart::Expr(Box::new(
                            Expr::BinOp {
                                op: BinOpKind::Add,
                                left: Box::new(Expr::var("x")),
                                right: Box::new(Expr::var("base")),
                            },
                        ))]),
                    )),
                }),
            ],
        );
        let ir_decl = lower_declaration(&decl).expect("lambda lowering should succeed");
        match &ir_decl.body {
            CompExpr::Let { body, .. } => {
                assert!(matches!(
                    body.as_ref(),
                    CompExpr::Value(ValueExpr::Lambda { param, body })
                        if param == "x"
                            && matches!(
                                body.as_ref(),
                                CompExpr::PerformEffect { effect, op, .. }
                                    if effect == "Print" && op == "println"
                            )
                ));
            }
            other => panic!("expected let-bound capture feeding lambda, got {other:?}"),
        }
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
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_resume() {
        // resume x  -> Resume { value: Var("x") }
        let decl = decl_with_params(
            "do_resume",
            vec!["x"],
            vec![expr_stmt(Expr::Resume {
                value: Box::new(Expr::var("x")),
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_pure_list_literal() {
        let decl = decl_with_body(
            "items",
            vec![expr_stmt(Expr::ListLit {
                elements: vec![Expr::IntLit(1), Expr::IntLit(2)],
                spread: None,
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_list_literal_with_effectful_element_normalizes_via_let() {
        let decl = decl_with_body(
            "items",
            vec![expr_stmt(Expr::ListLit {
                elements: vec![
                    Expr::Call {
                        callee: Box::new(Expr::var("read")),
                        arg: Box::new(Expr::TupleLit(vec![])),
                        span: None,
                    },
                    Expr::IntLit(2),
                ],
                spread: None,
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        assert!(
            matches!(ir_decl.body, CompExpr::Let { .. }),
            "effectful list element should ANF-normalize through let, got {:?}",
            ir_decl.body
        );
    }

    #[test]
    fn lower_list_literal_with_effectful_spread_normalizes_via_let() {
        let decl = decl_with_body(
            "items",
            vec![expr_stmt(Expr::ListLit {
                elements: vec![Expr::IntLit(1)],
                spread: Some(Box::new(Expr::Call {
                    callee: Box::new(Expr::var("read")),
                    arg: Box::new(Expr::TupleLit(vec![])),
                    span: None,
                })),
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        assert!(
            matches!(ir_decl.body, CompExpr::Let { .. }),
            "effectful list spread should ANF-normalize through let, got {:?}",
            ir_decl.body
        );
    }

    #[test]
    fn lower_pure_tuple_literal() {
        let decl = decl_with_body(
            "pair",
            vec![expr_stmt(Expr::TupleLit(vec![
                Expr::IntLit(1),
                Expr::StringLit("x".to_string()),
            ]))],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        assert_eq!(fmt_ir(&m), "decl pair: ? =\n  (1, \"x\")\n\n");
    }

    #[test]
    fn lower_tuple_literal_with_effectful_item_normalizes_via_let() {
        let decl = decl_with_body(
            "pair",
            vec![expr_stmt(Expr::TupleLit(vec![
                Expr::Call {
                    callee: Box::new(Expr::var("read")),
                    arg: Box::new(Expr::TupleLit(vec![])),
                    span: None,
                },
                Expr::IntLit(2),
            ]))],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        assert!(
            matches!(ir_decl.body, CompExpr::Let { .. }),
            "effectful tuple item should ANF-normalize through let, got {:?}",
            ir_decl.body
        );
    }

    #[test]
    fn lower_pure_record_construct() {
        let decl = decl_with_body(
            "pair",
            vec![expr_stmt(Expr::RecordConstruct {
                constructor: "Pair".to_string(),
                fields: vec![
                    ("left".to_string(), Expr::IntLit(1)),
                    ("right".to_string(), Expr::StringLit("x".to_string())),
                ],
                span: None,
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        assert_eq!(
            fmt_ir(&m),
            "decl pair: ? =\n  Pair(left: 1, right: \"x\")\n\n"
        );
    }

    #[test]
    fn lower_record_construct_with_effectful_field_normalizes_via_let() {
        let decl = decl_with_body(
            "pair",
            vec![expr_stmt(Expr::RecordConstruct {
                constructor: "Pair".to_string(),
                fields: vec![
                    (
                        "left".to_string(),
                        Expr::Call {
                            callee: Box::new(Expr::var("read")),
                            arg: Box::new(Expr::TupleLit(vec![])),
                            span: None,
                        },
                    ),
                    ("right".to_string(), Expr::IntLit(2)),
                ],
                span: None,
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        assert!(
            matches!(ir_decl.body, CompExpr::Let { .. }),
            "effectful record field should ANF-normalize through let, got {:?}",
            ir_decl.body
        );
    }

    #[test]
    fn lower_mut_binding() {
        let decl = decl_with_body(
            "mut_bind",
            vec![Stmt::MutBinding {
                name: "x".into(),
                value: Expr::IntLit(1),
                span: None,
            }],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        match &m.decls[0].body {
            CompExpr::LetMut {
                name, value, body, ..
            } => {
                assert_eq!(name, "x");
                assert!(
                    matches!(value.as_ref(), CompExpr::Value(ValueExpr::IntLit(1))),
                    "mutable binding value should lower directly, got {:?}",
                    value
                );
                assert!(
                    matches!(body.as_ref(), CompExpr::Value(ValueExpr::Var(v)) if v == "x"),
                    "single mutable binding should yield the bound variable, got {:?}",
                    body
                );
            }
            other => panic!("expected LetMut, got {:?}", other),
        }
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
                    parsed_body: Some(vec![expr_stmt(Expr::Resume {
                        value: Box::new(Expr::StringLit("hi".into())),
                    })]),
                    span: Span::point(1, 1),
                }],
            })],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_assign() {
        let decl = decl_with_body(
            "assign_test",
            vec![
                Stmt::MutBinding {
                    name: "x".into(),
                    value: Expr::IntLit(1),
                    span: None,
                },
                Stmt::Assign {
                    name: "x".into(),
                    value: Expr::IntLit(2),
                    span: None,
                },
            ],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        match &m.decls[0].body {
            CompExpr::LetMut { name, body, .. } => {
                assert_eq!(name, "x");
                match body.as_ref() {
                    CompExpr::Assign { name, value } => {
                        assert_eq!(name, "x");
                        assert!(
                            matches!(value.as_ref(), CompExpr::Value(ValueExpr::IntLit(2))),
                            "expected assignment value to lower directly, got {:?}",
                            value
                        );
                    }
                    other => panic!("expected Assign after mutable binding, got {:?}", other),
                }
            }
            other => panic!("expected outer LetMut, got {:?}", other),
        }
    }

    #[test]
    fn reject_assign_to_non_local_target() {
        let decl = decl_with_body(
            "assign_test",
            vec![Stmt::Assign {
                name: "print".into(),
                value: Expr::IntLit(1),
                span: None,
            }],
        );
        let err = lower_declaration(&decl).unwrap_err();
        assert!(err.message.contains("assignment target"), "{}", err.message);
    }

    // --- lone binding semantics test ---

    #[test]
    fn lower_lone_binding_yields_bound_value() {
        // A body consisting only of a single binding `x = 5` should lower to
        // `let x: Int = 5 in x`, yielding the bound value as the block result.
        let decl = decl_with_body("lone_bind", vec![binding("x", Expr::IntLit(5))]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
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
    fn lower_module_supports_lambda_decl() {
        use crate::ast::Module;
        let m = Module {
            imports: vec![],
            embed_declarations: vec![],
            type_declarations: vec![],
            effect_declarations: vec![],
            declarations: vec![
                decl_with_body("ok", vec![expr_stmt(Expr::IntLit(1))]),
                decl_with_body(
                    "lambda_fn",
                    vec![expr_stmt(Expr::Lambda {
                        param: "x".into(),
                        body: Box::new(Expr::var("x")),
                    })],
                ),
            ],
        };
        let ir = lower_module(&m).unwrap();
        assert_eq!(ir.decls.len(), 2);
        assert!(validate_ir(&ir).is_ok());
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
        let m = IrModule {
            decls: vec![ir_decl],
        };
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
            body: vec![
                binding("result", Expr::var("result")),
                expr_stmt(Expr::var("result")),
            ],
        };
        let decl = decl_with_body("main_effect", vec![expr_stmt(with_expr)]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        insta::assert_snapshot!(fmt_ir(&m));
    }

    // --- PerformEffect lowering snapshot tests (G5) ---

    #[test]
    fn lower_print_print_literal() {
        // Print.print("hello") → perform Print.print("hello")
        let call = Expr::Call {
            callee: Box::new(Expr::qualified("Print", "print")),
            arg: Box::new(Expr::StringLit("hello".into())),
            span: None,
        };
        let decl = decl_with_body("say_hello", vec![expr_stmt(call)]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        // Verify structural form.
        assert!(
            matches!(&m.decls[0].body, CompExpr::PerformEffect { effect, op, args }
                if effect == "Print" && op == "print" && args.len() == 1),
            "expected PerformEffect{{Print, print, [arg]}}, got {:?}",
            m.decls[0].body
        );
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_read_read_no_args() {
        // Read.read() → perform Read.read() with empty args
        let call = Expr::Call {
            callee: Box::new(Expr::qualified("Read", "read")),
            arg: Box::new(Expr::TupleLit(vec![])), // unit arg
            span: None,
        };
        let decl = decl_with_body("do_read", vec![expr_stmt(call)]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        // Verify args list is empty (unit arg was stripped).
        assert!(
            matches!(&m.decls[0].body, CompExpr::PerformEffect { effect, op, args }
                if effect == "Read" && op == "read" && args.is_empty()),
            "expected PerformEffect{{Read, read, []}} but got {:?}",
            m.decls[0].body
        );
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_read_read_line_no_args() {
        // Read.read_line() → perform Read.read_line() with empty args
        let call = Expr::Call {
            callee: Box::new(Expr::qualified("Read", "read_line")),
            arg: Box::new(Expr::TupleLit(vec![])),
            span: None,
        };
        let decl = decl_with_body("do_read_line", vec![expr_stmt(call)]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        assert!(
            matches!(&m.decls[0].body, CompExpr::PerformEffect { effect, op, args }
                if effect == "Read" && op == "read_line" && args.is_empty()),
            "expected PerformEffect{{Read, read_line, []}} but got {:?}",
            m.decls[0].body
        );
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_print_println_with_var() {
        // Print.println(x) → perform Print.println(x)
        let call = Expr::Call {
            callee: Box::new(Expr::qualified("Print", "println")),
            arg: Box::new(Expr::var("x")),
            span: None,
        };
        let decl = decl_with_params("say_line", vec!["x"], vec![expr_stmt(call)]);
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        assert!(
            matches!(&m.decls[0].body, CompExpr::PerformEffect { effect, op, args }
                if effect == "Print" && op == "println" && args.len() == 1),
            "expected PerformEffect{{Print, println, [x]}}, got {:?}",
            m.decls[0].body
        );
        insta::assert_snapshot!(fmt_ir(&m));
    }

    // --- Equivalence tests for the three canonical Read/Print programs (G5) ---
    //
    // These three programs are semantically equivalent but differ in surface form.
    // After G5 lowering, all three must contain explicit PerformEffect nodes for
    // Read.read and Print.print — i.e., the IR expresses "what happens", not "how
    // it was written".

    fn make_qualified_call(module: &str, op: &str, arg: Option<Expr>) -> Expr {
        Expr::Call {
            callee: Box::new(Expr::qualified(module, op)),
            arg: Box::new(arg.unwrap_or(Expr::TupleLit(vec![]))),
            span: None,
        }
    }

    /// Count `PerformEffect` nodes with matching effect/op in a CompExpr tree.
    fn count_perform(comp: &CompExpr, effect: &str, op: &str) -> usize {
        match comp {
            CompExpr::PerformEffect {
                effect: e, op: o, ..
            } if e == effect && o == op => 1,
            CompExpr::PerformEffect { .. } => 0,
            CompExpr::Value(_) => 0,
            CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
                count_perform(value, effect, op) + count_perform(body, effect, op)
            }
            CompExpr::Seq { stmts, tail } => {
                stmts
                    .iter()
                    .map(|s| count_perform(s, effect, op))
                    .sum::<usize>()
                    + count_perform(tail, effect, op)
            }
            CompExpr::If { then_, else_, .. } => {
                count_perform(then_, effect, op) + count_perform(else_, effect, op)
            }
            CompExpr::Call { .. } => 0,
            CompExpr::Assign { value, .. } => count_perform(value, effect, op),
            CompExpr::Case { arms, .. } => arms
                .iter()
                .map(|arm| count_perform(&arm.body, effect, op))
                .sum(),
            CompExpr::Handle { clauses } => clauses
                .iter()
                .map(|c| count_perform(&c.body, effect, op))
                .sum(),
            CompExpr::WithHandler { handler, body } => {
                count_perform(handler, effect, op) + count_perform(body, effect, op)
            }
            CompExpr::Resume { .. } => 0,
        }
    }

    #[test]
    fn lower_equiv_inline_effect_arg_normalizes_to_let() {
        // Program 1 (inline form): Print.print(Read.read())
        //
        // This now ANF-normalizes to the binding form:
        // `tmp = Read.read(); Print.print(tmp)`.
        let inner_read = make_qualified_call("Read", "read", None);
        let outer_print = Expr::Call {
            callee: Box::new(Expr::qualified("Print", "print")),
            arg: Box::new(inner_read),
            span: None,
        };
        let decl = decl_with_body("prog1", vec![expr_stmt(outer_print)]);
        assert!(
            matches!(lower_declaration(&decl).unwrap().body, CompExpr::Let { .. }),
            "nested effect arg should lower via ANF let binding"
        );
    }

    #[test]
    fn lower_equiv_read_bind_then_print() {
        // Program 2: text = Read.read(); Print.print(text)
        let read_call = make_qualified_call("Read", "read", None);
        let print_call = make_qualified_call("Print", "print", Some(Expr::var("text")));
        let decl = decl_with_body(
            "prog2",
            vec![binding("text", read_call), expr_stmt(print_call)],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        // Structural assertions: must have exactly one Read.read and one Print.print node.
        assert_eq!(
            count_perform(&m.decls[0].body, "Read", "read"),
            1,
            "expected 1 PerformEffect{{Read, read}}"
        );
        assert_eq!(
            count_perform(&m.decls[0].body, "Print", "print"),
            1,
            "expected 1 PerformEffect{{Print, print}}"
        );
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_bare_read_bind_then_print() {
        let read_call = Expr::Call {
            callee: Box::new(Expr::var("read")),
            arg: Box::new(Expr::TupleLit(vec![])),
            span: None,
        };
        let print_call = Expr::Call {
            callee: Box::new(Expr::var("print")),
            arg: Box::new(Expr::var("text")),
            span: None,
        };
        let decl = decl_with_body(
            "prog_bare",
            vec![binding("text", read_call), expr_stmt(print_call)],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        assert_eq!(count_perform(&m.decls[0].body, "Read", "read"), 1);
        assert_eq!(count_perform(&m.decls[0].body, "Print", "print"), 1);
    }

    #[test]
    fn lower_bare_print_with_nested_read_introduces_temp_binding() {
        let read_call = Expr::Call {
            callee: Box::new(Expr::var("read")),
            arg: Box::new(Expr::TupleLit(vec![])),
            span: None,
        };
        let print_call = Expr::Call {
            callee: Box::new(Expr::var("print")),
            arg: Box::new(read_call),
            span: None,
        };
        let decl = decl_with_body("prog_nested", vec![expr_stmt(print_call)]);
        let ir_decl = lower_declaration(&decl).expect("nested bare effect should lower via let");
        assert!(
            matches!(ir_decl.body, CompExpr::Let { .. }),
            "expected ANF let binding around nested effect arg, got {:?}",
            ir_decl.body
        );
    }

    #[test]
    fn lower_ordinary_call_with_list_index_argument_introduces_temp_binding() {
        let module = crate::parse_module(
            r#"
import goby/list

main lines =
  list.each (lines[1]) println
"#,
        )
        .expect("module should parse");
        let ir_module = lower_module(&module).expect("ordinary call arg ANF lowering should work");
        let main_decl = ir_module
            .decls
            .iter()
            .find(|decl| decl.name == "main")
            .expect("main decl should be present");
        assert!(
            matches!(main_decl.body, CompExpr::Let { .. }),
            "expected ANF let binding around list-index call arg, got {:?}",
            main_decl.body
        );
    }

    #[test]
    fn lower_ordinary_call_with_non_value_callee_introduces_temp_binding() {
        let choose_callee = Expr::If {
            condition: Box::new(Expr::BoolLit(true)),
            then_expr: Box::new(Expr::var("f")),
            else_expr: Box::new(Expr::var("g")),
        };
        let call = Expr::Call {
            callee: Box::new(choose_callee),
            arg: Box::new(Expr::IntLit(1)),
            span: None,
        };
        let decl = decl_with_params("main", vec!["f", "g"], vec![expr_stmt(call)]);
        let ir_decl =
            lower_declaration(&decl).expect("ordinary call callee ANF lowering should work");
        assert!(
            matches!(ir_decl.body, CompExpr::Let { .. }),
            "expected ANF let binding around non-value call callee, got {:?}",
            ir_decl.body
        );
    }

    #[test]
    fn lower_list_index_to_list_get_call() {
        let expr = Expr::ListIndex {
            list: Box::new(Expr::var("lines")),
            index: Box::new(Expr::IntLit(1)),
        };
        let decl = decl_with_body("idx", vec![expr_stmt(expr)]);
        let ir_decl = lower_declaration(&decl).expect("list index should lower");
        assert!(
            matches!(
                &ir_decl.body,
                CompExpr::Call { callee, args }
                    if matches!(callee.as_ref(), ValueExpr::GlobalRef { module, name }
                        if module == "list" && name == "get")
                        && args.len() == 2
            ),
            "expected resolved-form canonicalization to list.get IR call, got {:?}",
            ir_decl.body
        );
    }

    #[test]
    fn lower_bare_and_qualified_effect_calls_converge_to_same_ir() {
        let bare_decl = decl_with_body(
            "bare",
            vec![
                binding(
                    "text",
                    Expr::Call {
                        callee: Box::new(Expr::var("read")),
                        arg: Box::new(Expr::TupleLit(vec![])),
                        span: None,
                    },
                ),
                expr_stmt(Expr::Call {
                    callee: Box::new(Expr::var("print")),
                    arg: Box::new(Expr::var("text")),
                    span: None,
                }),
            ],
        );
        let qualified_decl = decl_with_body(
            "qualified",
            vec![
                binding("text", make_qualified_call("Read", "read", None)),
                expr_stmt(make_qualified_call(
                    "Print",
                    "print",
                    Some(Expr::var("text")),
                )),
            ],
        );

        let bare_ir = lower_declaration(&bare_decl).unwrap();
        let qualified_ir = lower_declaration(&qualified_decl).unwrap();
        assert_eq!(bare_ir.body, qualified_ir.body);
    }

    #[test]
    fn lower_imported_and_qualified_effect_calls_converge_to_same_ir() {
        let imported_module = crate::ast::Module {
            imports: vec![crate::ast::ImportDecl {
                module_path: "goby/prelude".to_string(),
                kind: crate::ast::ImportKind::Selective(vec![
                    "read".to_string(),
                    "print".to_string(),
                ]),
                module_path_span: None,
                kind_span: None,
            }],
            embed_declarations: vec![],
            type_declarations: vec![],
            effect_declarations: vec![],
            declarations: vec![Declaration {
                name: "main".to_string(),
                type_annotation: None,
                params: vec![],
                body: String::new(),
                parsed_body: Some(vec![
                    binding(
                        "text",
                        Expr::Call {
                            callee: Box::new(Expr::var("read")),
                            arg: Box::new(Expr::TupleLit(vec![])),
                            span: None,
                        },
                    ),
                    expr_stmt(Expr::Call {
                        callee: Box::new(Expr::var("print")),
                        arg: Box::new(Expr::var("text")),
                        span: None,
                    }),
                ]),
                line: 1,
                col: 1,
            }],
        };
        let qualified_module = crate::ast::Module {
            imports: vec![],
            embed_declarations: vec![],
            type_declarations: vec![],
            effect_declarations: vec![],
            declarations: vec![Declaration {
                name: "main".to_string(),
                type_annotation: None,
                params: vec![],
                body: String::new(),
                parsed_body: Some(vec![
                    binding("text", make_qualified_call("Read", "read", None)),
                    expr_stmt(make_qualified_call(
                        "Print",
                        "print",
                        Some(Expr::var("text")),
                    )),
                ]),
                line: 1,
                col: 1,
            }],
        };

        let imported_ir = lower_module(&imported_module).unwrap();
        let qualified_ir = lower_module(&qualified_module).unwrap();
        assert_eq!(imported_ir.decls[0].body, qualified_ir.decls[0].body);
    }

    #[test]
    fn lower_list_index_and_canonical_helper_call_converge_to_same_ir() {
        let indexed_decl = decl_with_body(
            "indexed",
            vec![expr_stmt(Expr::ListIndex {
                list: Box::new(Expr::var("lines")),
                index: Box::new(Expr::IntLit(1)),
            })],
        );
        let helper_decl = decl_with_body(
            "helper",
            vec![expr_stmt(Expr::call(
                Expr::call(Expr::qualified("list", "get"), Expr::var("lines")),
                Expr::IntLit(1),
            ))],
        );

        let indexed_ir = lower_declaration(&indexed_decl).unwrap();
        let helper_ir = lower_declaration(&helper_decl).unwrap();
        assert_eq!(indexed_ir.body, helper_ir.body);
    }

    #[test]
    fn lower_inline_list_index_call_argument_matches_explicit_anf_form() {
        let inline_module = crate::parse_module(
            r#"
import goby/list ( each, map )
import goby/string ( split, graphemes )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  rolls = map lines graphemes
  each (rolls[2]) println
"#,
        )
        .expect("inline module should parse");
        let explicit_anf_module = crate::parse_module(
            r#"
import goby/list ( each, map )
import goby/string ( split, graphemes )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  rolls = map lines graphemes
  row2 = rolls[2]
  each row2 println
"#,
        )
        .expect("ANF module should parse");

        let inline_ir = lower_module(&inline_module).expect("inline module should lower");
        let explicit_anf_ir =
            lower_module(&explicit_anf_module).expect("explicit ANF module should lower");
        let inline_main = inline_ir
            .decls
            .iter()
            .find(|decl| decl.name == "main")
            .expect("inline main should exist");
        let explicit_main = explicit_anf_ir
            .decls
            .iter()
            .find(|decl| decl.name == "main")
            .expect("explicit main should exist");

        assert_eq!(inline_main.residual_effects, explicit_main.residual_effects);
        match (&inline_main.body, &explicit_main.body) {
            (
                CompExpr::Let {
                    body: inline_text_body,
                    ..
                },
                CompExpr::Let {
                    body: explicit_text_body,
                    ..
                },
            ) => match (inline_text_body.as_ref(), explicit_text_body.as_ref()) {
                (
                    CompExpr::Let {
                        body: inline_lines_body,
                        ..
                    },
                    CompExpr::Let {
                        body: explicit_lines_body,
                        ..
                    },
                ) => match (inline_lines_body.as_ref(), explicit_lines_body.as_ref()) {
                    (
                        CompExpr::Let {
                            body: inline_rolls_body,
                            ..
                        },
                        CompExpr::Let {
                            body: explicit_rolls_body,
                            ..
                        },
                    ) => match (inline_rolls_body.as_ref(), explicit_rolls_body.as_ref()) {
                        (
                            CompExpr::Let {
                                value: inline_value,
                                body: inline_each_body,
                                ..
                            },
                            CompExpr::Let {
                                value: explicit_value,
                                body: explicit_each_body,
                                ..
                            },
                        ) => {
                            assert_eq!(inline_value, explicit_value);
                            assert!(
                                matches!(
                                    inline_each_body.as_ref(),
                                    CompExpr::Call { callee, args }
                                        if matches!(
                                            callee.as_ref(),
                                            ValueExpr::GlobalRef { module, name }
                                                if module == "list" && name == "each"
                                        ) && args.len() == 2
                                ),
                                "inline body should end in list.each call, got {:?}",
                                inline_each_body
                            );
                            assert!(
                                matches!(
                                    explicit_each_body.as_ref(),
                                    CompExpr::Call { callee, args }
                                        if matches!(
                                            callee.as_ref(),
                                            ValueExpr::GlobalRef { module, name }
                                                if module == "list" && name == "each"
                                        ) && args.len() == 2
                                ),
                                "explicit body should end in list.each call, got {:?}",
                                explicit_each_body
                            );
                        }
                        other => panic!(
                            "expected matching ANF let before list.each, got {:?}",
                            other
                        ),
                    },
                    other => panic!("expected matching rolls let, got {:?}", other),
                },
                other => panic!("expected matching lines let, got {:?}", other),
            },
            other => panic!("expected matching text let, got {:?}", other),
        }
    }

    #[test]
    fn lower_equiv_read_bind_interp_then_print() {
        // Program 3: text = Read.read(); decorated = "${text}!"; Print.print(decorated)
        let read_call = make_qualified_call("Read", "read", None);
        let interp = Expr::InterpolatedString(vec![
            InterpolatedPart::Expr(Box::new(Expr::var("text"))),
            InterpolatedPart::Text("!".into()),
        ]);
        let print_call = make_qualified_call("Print", "print", Some(Expr::var("decorated")));
        let decl = decl_with_body(
            "prog3",
            vec![
                binding("text", read_call),
                binding("decorated", interp),
                expr_stmt(print_call),
            ],
        );
        let ir_decl = lower_declaration(&decl).unwrap();
        let m = IrModule {
            decls: vec![ir_decl],
        };
        assert!(validate_ir(&m).is_ok());
        // Structural assertions: must have exactly one Read.read and one Print.print node.
        assert_eq!(
            count_perform(&m.decls[0].body, "Read", "read"),
            1,
            "expected 1 PerformEffect{{Read, read}}"
        );
        assert_eq!(
            count_perform(&m.decls[0].body, "Print", "print"),
            1,
            "expected 1 PerformEffect{{Print, print}}"
        );
        insta::assert_snapshot!(fmt_ir(&m));
    }

    #[test]
    fn lower_non_effect_qualified_call_stays_as_call() {
        // Math.sqrt(x) is not a known effect op — must lower to Call, not PerformEffect.
        let call = Expr::Call {
            callee: Box::new(Expr::qualified("Math", "sqrt")),
            arg: Box::new(Expr::var("x")),
            span: None,
        };
        let decl = decl_with_params("use_math", vec!["x"], vec![expr_stmt(call)]);
        let ir_decl = lower_declaration(&decl).unwrap();
        assert!(
            matches!(ir_decl.body, CompExpr::Call { .. }),
            "non-effect qualified call must lower to Call, not PerformEffect; got {:?}",
            ir_decl.body
        );
    }

    #[test]
    fn lower_binop_with_call_right_operand_introduces_anf_let() {
        // `1 + count (n - 1)` — right operand is a Call, not a pure value.
        // ir_lower must ANF-hoist it into a Let binding.
        let module = crate::parse_module(
            r#"
count n =
  if n <= 0
    0
  else
    1 + count (n - 1)
"#,
        )
        .expect("module should parse");
        let ir_module = lower_module(&module).expect("BinOp with call right operand should lower");
        let count_decl = ir_module
            .decls
            .iter()
            .find(|d| d.name == "count")
            .expect("count decl should be present");
        // The else branch should be: Let { name: __goby_ir_binop_right_*, value: Call(...), body: Value(BinOp) }
        fn find_binop_let(expr: &CompExpr) -> bool {
            match expr {
                CompExpr::Let {
                    name, value, body, ..
                } => {
                    // Found the ANF let for the binop right operand.
                    if name.starts_with("__goby_ir_binop_right_")
                        && matches!(value.as_ref(), CompExpr::Call { .. })
                        && matches!(body.as_ref(), CompExpr::Value(ValueExpr::BinOp { .. }))
                    {
                        return true;
                    }
                    // Otherwise recurse into the body (e.g. ANF-wrapped if-condition).
                    find_binop_let(body)
                }
                CompExpr::If { else_, .. } => find_binop_let(else_),
                _ => false,
            }
        }
        assert!(
            find_binop_let(&count_decl.body),
            "expected ANF Let for binop right operand; got {:?}",
            count_decl.body
        );
    }

    #[test]
    fn lower_module_resolves_selective_imported_helper_before_ir() {
        let module = crate::ast::Module {
            imports: vec![crate::ast::ImportDecl {
                module_path: "goby/list".to_string(),
                kind: crate::ast::ImportKind::Selective(vec!["get".to_string()]),
                module_path_span: None,
                kind_span: None,
            }],
            embed_declarations: vec![],
            type_declarations: vec![],
            effect_declarations: vec![],
            declarations: vec![Declaration {
                name: "main".to_string(),
                type_annotation: None,
                params: vec![],
                body: String::new(),
                parsed_body: Some(vec![expr_stmt(Expr::call(
                    Expr::call(Expr::var("get"), Expr::var("xs")),
                    Expr::IntLit(1),
                ))]),
                line: 1,
                col: 1,
            }],
        };

        let ir = lower_module(&module).unwrap();
        assert!(
            matches!(
                &ir.decls[0].body,
                CompExpr::Call {
                    callee,
                    args
                } if matches!(callee.as_ref(), ValueExpr::GlobalRef { module, name } if module == "list" && name == "get")
                    && args.len() == 2
            ),
            "expected selective import to canonicalize to list.get; got {:?}",
            ir.decls[0].body
        );
    }
}
