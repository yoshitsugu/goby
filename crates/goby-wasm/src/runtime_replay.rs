use super::*;
use crate::runtime_flow::RcCallables;
use std::rc::Rc;

impl<'m> RuntimeOutputResolver<'m> {
    /// Execute a single AST statement inside a unit-returning function body.
    pub(super) fn execute_unit_ast_stmt(
        &mut self,
        stmt: &Stmt,
        locals: &mut RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<()> {
        match stmt {
            Stmt::Binding { name, value, .. } => {
                let v = match self.eval_expr(value, locals, callables, evaluators, depth) {
                    Out::Done(v) => v,
                    Out::Suspend(cont) => return Out::Suspend(cont),
                    Out::Escape(escape) => return Out::Escape(escape),
                    Out::Err(e) => return Out::Err(e),
                };
                locals.store(name, v);
                Out::Done(())
            }
            Stmt::MutBinding { name, value, .. } => {
                let v = match self.eval_expr(value, locals, callables, evaluators, depth) {
                    Out::Done(v) => v,
                    Out::Suspend(cont) => return Out::Suspend(cont),
                    Out::Escape(escape) => return Out::Escape(escape),
                    Out::Err(e) => return Out::Err(e),
                };
                locals.store_mut(name, v);
                Out::Done(())
            }
            Stmt::Assign { name, value, .. } => {
                if !locals.contains(name) {
                    return Out::Err(RuntimeError::Unsupported);
                }
                let v = match self.eval_expr(value, locals, callables, evaluators, depth) {
                    Out::Done(v) => v,
                    Out::Suspend(cont) => return Out::Suspend(cont),
                    Out::Escape(escape) => return Out::Escape(escape),
                    Out::Err(e) => return Out::Err(e),
                };
                if !locals.assign(name, v) {
                    return Out::Err(RuntimeError::Unsupported);
                }
                Out::Done(())
            }
            Stmt::Expr(expr, _) => {
                self.execute_unit_expr_ast(expr, locals, callables, evaluators, depth)
            }
        }
    }

    #[allow(dead_code)]
    pub(super) fn execute_ingest_ast_stmt_sequence(
        &mut self,
        stmts: &[Stmt],
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        self.pending_caller_cont_stack.push(None);
        let mut i = 0;
        while i < stmts.len() {
            let remaining = &stmts[i + 1..];
            let did_set_slot = !remaining.is_empty() && matches!(&stmts[i], Stmt::Expr(_, _));
            if did_set_slot && let Some(slot) = self.pending_caller_cont_stack.last_mut() {
                *slot = Some(Cont::StmtSeq {
                    pending: None,
                    store: None,
                    remaining: remaining.to_vec(),
                    locals: RuntimeLocals::default(),
                    callables: Rc::new(HashMap::new()),
                    depth: 0,
                    handler_stack: self.active_inline_handler_stack.clone(),
                    finish: FinishKind::Ingest,
                });
            }
            let ok = self.ingest_ast_statement(&stmts[i], evaluators);
            if ok.is_none() {
                self.pending_caller_cont_stack.pop();
                return None;
            }
            let consumed = did_set_slot
                && self
                    .pending_caller_cont_stack
                    .last()
                    .is_some_and(|s| s.is_none());
            if consumed {
                self.pending_caller_cont_stack.pop();
                return Some(());
            }
            if let Some(slot) = self.pending_caller_cont_stack.last_mut() {
                *slot = None;
            }
            i += 1;
        }
        self.pending_caller_cont_stack.pop();
        Some(())
    }

    pub(super) fn execute_unit_ast_stmt_sequence(
        &mut self,
        stmts: &[Stmt],
        locals: &mut RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
        finish: FinishKind,
    ) -> Out<()> {
        self.pending_caller_cont_stack.push(None);
        let mut i = 0;
        while i < stmts.len() {
            let remaining = stmts[i + 1..].to_vec();
            let did_set_slot = !remaining.is_empty() && matches!(&stmts[i], Stmt::Expr(_, _));
            if did_set_slot && let Some(slot) = self.pending_caller_cont_stack.last_mut() {
                *slot = Some(Cont::StmtSeq {
                    pending: None,
                    store: None,
                    remaining: remaining.clone(),
                    locals: locals.clone(),
                    callables: Rc::clone(callables),
                    depth,
                    handler_stack: self.active_inline_handler_stack.clone(),
                    finish: finish.clone(),
                });
            }

            match self.execute_unit_ast_stmt(&stmts[i], locals, callables, evaluators, depth + 1) {
                Out::Done(()) => {
                    let consumed = did_set_slot
                        && self
                            .pending_caller_cont_stack
                            .last()
                            .is_some_and(|slot| slot.is_none());
                    if consumed {
                        self.pending_caller_cont_stack.pop();
                        return Out::Done(());
                    }
                    if let Some(slot) = self.pending_caller_cont_stack.last_mut() {
                        *slot = None;
                    }
                    i += 1;
                }
                Out::Suspend(cont) => {
                    self.pending_caller_cont_stack.pop();
                    return Out::Suspend(cont);
                }
                Out::Escape(escape) => {
                    self.pending_caller_cont_stack.pop();
                    return Out::Escape(escape);
                }
                Out::Err(e) => {
                    self.pending_caller_cont_stack.pop();
                    return Out::Err(e);
                }
            }
        }
        self.pending_caller_cont_stack.pop();
        Out::Done(())
    }

    pub(super) fn eval_stmts(
        &mut self,
        stmts: &[Stmt],
        mut locals: RuntimeLocals,
        callables: RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
        finish: FinishKind,
    ) -> Out<(Option<RuntimeValue>, RuntimeLocals)> {
        let mut last_value: Option<RuntimeValue> = None;
        for (i, stmt) in stmts.iter().enumerate() {
            let remaining = stmts[i + 1..].to_vec();
            match stmt {
                Stmt::Binding { name, value, .. } => {
                    match self.eval_expr(value, &locals, &callables, evaluators, depth + 1) {
                        Out::Done(v) => {
                            locals.store(name, v);
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Suspend(cont) => {
                            return Out::Suspend(Cont::StmtSeq {
                                pending: Some(Box::new(cont)),
                                store: Some(StoreOp::Bind { name: name.clone() }),
                                remaining,
                                locals,
                                callables,
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                                finish,
                            });
                        }
                        Out::Err(e) => return Out::Err(e),
                    }
                }
                Stmt::MutBinding { name, value, .. } => {
                    match self.eval_expr(value, &locals, &callables, evaluators, depth + 1) {
                        Out::Done(v) => {
                            locals.store_mut(name, v);
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Suspend(cont) => {
                            return Out::Suspend(Cont::StmtSeq {
                                pending: Some(Box::new(cont)),
                                store: Some(StoreOp::BindMut { name: name.clone() }),
                                remaining,
                                locals,
                                callables,
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                                finish,
                            });
                        }
                        Out::Err(e) => return Out::Err(e),
                    }
                }
                Stmt::Assign { name, value, .. } => {
                    if !locals.contains(name) {
                        return Out::Err(RuntimeError::Abort {
                            kind: "assign_missing_var".into(),
                        });
                    }
                    match self.eval_expr(value, &locals, &callables, evaluators, depth + 1) {
                        Out::Done(v) => {
                            if !locals.assign(name, v) {
                                return Out::Err(RuntimeError::Abort {
                                    kind: "assign_missing_var".into(),
                                });
                            }
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Suspend(cont) => {
                            return Out::Suspend(Cont::StmtSeq {
                                pending: Some(Box::new(cont)),
                                store: Some(StoreOp::Assign { name: name.clone() }),
                                remaining,
                                locals,
                                callables,
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                                finish,
                            });
                        }
                        Out::Err(e) => return Out::Err(e),
                    }
                }
                Stmt::Expr(expr, _) => {
                    if matches!(expr, Expr::If { .. } | Expr::Block(_) | Expr::With { .. }) {
                        match self.eval_stmt_expr_with_local_effects(
                            expr,
                            &mut locals,
                            &callables,
                            evaluators,
                            depth + 1,
                        ) {
                            Out::Done(v) => {
                                if remaining.is_empty() {
                                    last_value = Some(v);
                                }
                                continue;
                            }
                            Out::Escape(escape) => return Out::Escape(escape),
                            Out::Suspend(cont) => {
                                return Out::Suspend(Cont::StmtSeq {
                                    pending: Some(Box::new(cont)),
                                    store: None,
                                    remaining,
                                    locals,
                                    callables,
                                    depth,
                                    handler_stack: self.active_inline_handler_stack.clone(),
                                    finish,
                                });
                            }
                            Out::Err(e) => return Out::Err(e),
                        }
                    }
                    if !remaining.is_empty()
                        && matches!(
                            expr,
                            Expr::If { .. }
                                | Expr::Case { .. }
                                | Expr::Block(_)
                                | Expr::With { .. }
                        )
                    {
                        let mut fallback_locals = locals.clone();
                        let fallback_callables = Rc::clone(&callables);
                        match self.execute_unit_ast_stmt(
                            stmt,
                            &mut fallback_locals,
                            &fallback_callables,
                            evaluators,
                            depth + 1,
                        ) {
                            Out::Done(()) => {
                                locals = fallback_locals;
                                continue;
                            }
                            Out::Suspend(cont) => {
                                return Out::Suspend(Cont::StmtSeq {
                                    pending: Some(Box::new(cont)),
                                    store: None,
                                    remaining,
                                    locals,
                                    callables,
                                    depth,
                                    handler_stack: self.active_inline_handler_stack.clone(),
                                    finish,
                                });
                            }
                            Out::Escape(escape) => return Out::Escape(escape),
                            Out::Err(RuntimeError::Abort { .. }) => {
                                return Out::Err(RuntimeError::Abort {
                                    kind: "aborted".into(),
                                });
                            }
                            Out::Err(RuntimeError::Unsupported) => {
                                return Out::Err(RuntimeError::Unsupported);
                            }
                        }
                    }
                    match self.eval_expr(expr, &locals, &callables, evaluators, depth + 1) {
                        Out::Done(v) => {
                            if remaining.is_empty() {
                                last_value = Some(v);
                            }
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(RuntimeError::Unsupported) if self.runtime_error.is_none() => {
                            let mut fallback_locals = locals.clone();
                            let fallback_callables = Rc::clone(&callables);
                            match self.execute_unit_ast_stmt(
                                stmt,
                                &mut fallback_locals,
                                &fallback_callables,
                                evaluators,
                                depth + 1,
                            ) {
                                Out::Done(()) => {
                                    locals = fallback_locals;
                                    if remaining.is_empty() {
                                        last_value = Some(RuntimeValue::Unit);
                                    }
                                }
                                Out::Suspend(cont) => {
                                    return Out::Suspend(Cont::StmtSeq {
                                        pending: Some(Box::new(cont)),
                                        store: None,
                                        remaining,
                                        locals,
                                        callables,
                                        depth,
                                        handler_stack: self.active_inline_handler_stack.clone(),
                                        finish,
                                    });
                                }
                                Out::Escape(escape) => return Out::Escape(escape),
                                Out::Err(RuntimeError::Abort { .. }) => {
                                    return Out::Err(RuntimeError::Abort {
                                        kind: "aborted".into(),
                                    });
                                }
                                Out::Err(RuntimeError::Unsupported) => {
                                    return Out::Err(RuntimeError::Unsupported);
                                }
                            }
                        }
                        Out::Suspend(cont) => {
                            return Out::Suspend(Cont::StmtSeq {
                                pending: Some(Box::new(cont)),
                                store: None,
                                remaining,
                                locals,
                                callables,
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                                finish,
                            });
                        }
                        Out::Err(e) => return Out::Err(e),
                    }
                }
            }
        }
        Out::Done((last_value, locals))
    }

    pub(super) fn eval_stmt_expr_with_local_effects(
        &mut self,
        expr: &Expr,
        locals: &mut RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        match expr {
            Expr::Block(stmts) => {
                match self.eval_stmts(
                    stmts,
                    locals.clone(),
                    Rc::clone(callables),
                    evaluators,
                    depth + 1,
                    FinishKind::Block,
                ) {
                    Out::Done((value, updated_locals)) => {
                        *locals = updated_locals;
                        Out::Done(value.unwrap_or(RuntimeValue::Unit))
                    }
                    Out::Suspend(cont) => Out::Suspend(cont),
                    Out::Escape(escape) => Out::Escape(escape),
                    Out::Err(e) => Out::Err(e),
                }
            }
            Expr::If {
                condition,
                then_expr,
                else_expr,
            } => {
                let cond_val =
                    match self.eval_expr(condition, locals, callables, evaluators, depth + 1) {
                        Out::Done(v) => v,
                        Out::Suspend(cont) => return Out::Suspend(cont),
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    };
                let branch = match cond_val {
                    RuntimeValue::Bool(true) => then_expr.as_ref(),
                    RuntimeValue::Bool(false) => else_expr.as_ref(),
                    _ => return Out::Err(RuntimeError::Unsupported),
                };
                match branch {
                    Expr::If { .. } | Expr::Block(_) | Expr::With { .. } => self
                        .eval_stmt_expr_with_local_effects(
                            branch,
                            locals,
                            callables,
                            evaluators,
                            depth + 1,
                        ),
                    _ => self.eval_expr(branch, locals, callables, evaluators, depth + 1),
                }
            }
            Expr::With { handler, body } => {
                let mut inline_handler =
                    match self.eval_expr(handler, locals, callables, evaluators, depth + 1) {
                        Out::Done(RuntimeValue::Handler(inline_handler)) => inline_handler,
                        Out::Done(_) => return Out::Err(RuntimeError::Unsupported),
                        Out::Suspend(cont) => return Out::Suspend(cont),
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    };
                let with_id = self.fresh_with_id();
                inline_handler.with_id = Some(with_id);
                self.active_inline_handler_stack.push(inline_handler);
                let result = self.eval_stmts(
                    body,
                    locals.clone(),
                    Rc::clone(callables),
                    evaluators,
                    depth + 1,
                    FinishKind::WithBody { with_id },
                );
                let updated_handler = self.active_inline_handler_stack.pop();
                match result {
                    Out::Done((value, updated_locals)) => {
                        *locals = updated_locals;
                        if let Some(updated_handler) = updated_handler {
                            locals.apply_selected_from(
                                &updated_handler.captured_locals,
                                &updated_handler.changed_outer_names,
                            );
                        }
                        Out::Done(value.unwrap_or(RuntimeValue::Unit))
                    }
                    Out::Escape(escape) => match escape {
                        Escape::WithScope {
                            with_id: target_id,
                            value,
                        } if target_id == with_id => {
                            if let Some(updated_handler) = updated_handler {
                                locals.apply_selected_from(
                                    &updated_handler.captured_locals,
                                    &updated_handler.changed_outer_names,
                                );
                            }
                            Out::Done(value)
                        }
                        other => Out::Escape(other),
                    },
                    Out::Suspend(cont) => Out::Suspend(cont),
                    Out::Err(e) => Out::Err(e),
                }
            }
            _ => self.eval_expr(expr, locals, callables, evaluators, depth + 1),
        }
    }

    pub(super) fn apply_cont(
        &mut self,
        cont: Cont,
        value: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Out<RuntimeValue> {
        match cont {
            Cont::StmtSeq {
                pending,
                store,
                remaining,
                mut locals,
                callables,
                depth,
                handler_stack,
                finish,
            } => {
                self.active_inline_handler_stack = handler_stack;
                let value = if let Some(pending) = pending {
                    match self.apply_cont(*pending, value, evaluators) {
                        Out::Done(value) => {
                            if let Some(updated_locals) = self.completed_stmt_seq_locals.take() {
                                locals = updated_locals;
                            }
                            value
                        }
                        Out::Suspend(next_pending) => {
                            return Out::Suspend(Cont::StmtSeq {
                                pending: Some(Box::new(next_pending)),
                                store,
                                remaining,
                                locals,
                                callables,
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                                finish,
                            });
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    }
                } else {
                    value
                };
                if let Some(store_op) = store {
                    match store_op {
                        StoreOp::Bind { name } => locals.store(&name, value),
                        StoreOp::BindMut { name } => locals.store_mut(&name, value),
                        StoreOp::Assign { name } => {
                            if !locals.contains(&name) {
                                return Out::Err(RuntimeError::Abort {
                                    kind: "assign_missing_var".into(),
                                });
                            }
                            if !locals.assign(&name, value) {
                                return Out::Err(RuntimeError::Abort {
                                    kind: "assign_missing_var".into(),
                                });
                            }
                        }
                    }
                }
                if matches!(finish, FinishKind::Ingest) {
                    for stmt in &remaining {
                        if self.ingest_ast_statement(stmt, evaluators).is_none() {
                            return Out::Err(RuntimeError::Unsupported);
                        }
                    }
                    return Out::Done(RuntimeValue::Unit);
                }
                match self.eval_stmts(
                    &remaining,
                    locals,
                    callables,
                    evaluators,
                    depth,
                    finish.clone(),
                ) {
                    Out::Done((last_val, final_locals)) => match finish {
                        FinishKind::Block | FinishKind::Ingest | FinishKind::WithBody { .. } => {
                            self.completed_stmt_seq_locals = Some(final_locals);
                            Out::Done(last_val.unwrap_or(RuntimeValue::Unit))
                        }
                        FinishKind::HandlerBody {
                            token_idx, with_id, ..
                        } => {
                            if let Some(resumed) = self.current_handler_resume_value(token_idx) {
                                Out::Done(resumed)
                            } else if self.handler_has_suspended_cont(token_idx) {
                                self.set_handler_token_state_suspended_placeholder(token_idx);
                                Out::Suspend(Cont::Resume)
                            } else {
                                Out::Escape(Escape::WithScope {
                                    with_id,
                                    value: last_val.unwrap_or(RuntimeValue::Unit),
                                })
                            }
                        }
                    },
                    Out::Escape(escape) => match finish {
                        FinishKind::WithBody { with_id } => match escape {
                            Escape::WithScope {
                                with_id: target_id,
                                value,
                            } if target_id == with_id => Out::Done(value),
                            other => Out::Escape(other),
                        },
                        _ => Out::Escape(escape),
                    },
                    Out::Suspend(c) => Out::Suspend(c),
                    Out::Err(e) => Out::Err(e),
                }
            }
            Cont::Apply {
                step,
                locals,
                callables,
                depth,
                handler_stack,
            } => {
                self.active_inline_handler_stack = handler_stack;
                match step {
                    ApplyStep::Pipeline { callee } => self
                        .apply_pipeline_out(&callee, value, &locals, &callables, evaluators, depth),
                    ApplyStep::SingleArgCall { fn_name } => self.apply_named_value_call_out(
                        &fn_name, value, &locals, &callables, evaluators, depth,
                    ),
                    ApplyStep::ReceiverMethod { receiver, member } => self
                        .apply_receiver_method_value_call_out(
                            &receiver, &member, value, evaluators, depth,
                        ),
                    ApplyStep::MultiArgCall {
                        fn_name,
                        mut evaluated,
                        remaining,
                    } => {
                        evaluated.push(value);
                        let mut i = 0;
                        while i < remaining.len() {
                            match self.eval_expr(
                                &remaining[i],
                                &locals,
                                &callables,
                                evaluators,
                                depth + 1,
                            ) {
                                Out::Done(v) => {
                                    evaluated.push(v);
                                    i += 1;
                                }
                                Out::Suspend(_) => {
                                    return Out::Suspend(Cont::Apply {
                                        step: ApplyStep::MultiArgCall {
                                            fn_name,
                                            evaluated,
                                            remaining: remaining[i + 1..].to_vec(),
                                        },
                                        locals,
                                        callables,
                                        depth,
                                        handler_stack: self.active_inline_handler_stack.clone(),
                                    });
                                }
                                Out::Escape(escape) => return Out::Escape(escape),
                                Out::Err(e) => return Out::Err(e),
                            }
                        }
                        self.apply_named_value_call_args_out(
                            &fn_name, &evaluated, &locals, &callables, evaluators, depth,
                        )
                    }
                    ApplyStep::CaseSelect { arms } => {
                        let Some((arm_body, arm_locals)) =
                            self.select_case_arm(&value, &arms, &locals)
                        else {
                            return Out::Err(RuntimeError::Unsupported);
                        };
                        self.eval_expr(&arm_body, &arm_locals, &callables, evaluators, depth + 1)
                    }
                    ApplyStep::IfBranch {
                        then_expr,
                        else_expr,
                    } => {
                        let branch = match value {
                            RuntimeValue::Bool(true) => then_expr,
                            RuntimeValue::Bool(false) => else_expr,
                            _ => return Out::Err(RuntimeError::Unsupported),
                        };
                        self.eval_expr(&branch, &locals, &callables, evaluators, depth + 1)
                    }
                    ApplyStep::BinOpLeft { op, right } => {
                        let left = value;
                        match self.eval_expr(&right, &locals, &callables, evaluators, depth + 1) {
                            Out::Done(right_value) => {
                                match self.apply_binop_runtime_value(op, left, right_value) {
                                    Some(v) => Out::Done(v),
                                    None => Out::Err(RuntimeError::Unsupported),
                                }
                            }
                            Out::Suspend(_) => Out::Suspend(Cont::Apply {
                                step: ApplyStep::BinOpRight { op, left },
                                locals,
                                callables,
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                            }),
                            Out::Escape(escape) => Out::Escape(escape),
                            Out::Err(e) => Out::Err(e),
                        }
                    }
                    ApplyStep::BinOpRight { op, left } => {
                        match self.apply_binop_runtime_value(op, left, value) {
                            Some(v) => Out::Done(v),
                            None => Out::Err(RuntimeError::Unsupported),
                        }
                    }
                    ApplyStep::InterpolatedPart {
                        mut accumulated,
                        remaining,
                    } => {
                        accumulated.push_str(&value.to_output_text());
                        let mut idx = 0;
                        while idx < remaining.len() {
                            match &remaining[idx] {
                                InterpolatedPart::Text(text) => {
                                    accumulated.push_str(text);
                                    idx += 1;
                                }
                                InterpolatedPart::Expr(expr) => {
                                    match self.eval_expr(
                                        expr,
                                        &locals,
                                        &callables,
                                        evaluators,
                                        depth + 1,
                                    ) {
                                        Out::Done(v) => {
                                            accumulated.push_str(&v.to_output_text());
                                            idx += 1;
                                        }
                                        Out::Suspend(_) => {
                                            return Out::Suspend(Cont::Apply {
                                                step: ApplyStep::InterpolatedPart {
                                                    accumulated,
                                                    remaining: remaining[idx + 1..].to_vec(),
                                                },
                                                locals,
                                                callables,
                                                depth,
                                                handler_stack: self
                                                    .active_inline_handler_stack
                                                    .clone(),
                                            });
                                        }
                                        Out::Escape(escape) => return Out::Escape(escape),
                                        Out::Err(e) => return Out::Err(e),
                                    }
                                }
                            }
                        }
                        Out::Done(RuntimeValue::String(accumulated))
                    }
                    ApplyStep::TupleLitElement {
                        mut evaluated,
                        remaining,
                    } => {
                        evaluated.push(value);
                        let mut idx = 0;
                        while idx < remaining.len() {
                            match self.eval_expr(
                                &remaining[idx],
                                &locals,
                                &callables,
                                evaluators,
                                depth + 1,
                            ) {
                                Out::Done(v) => {
                                    evaluated.push(v);
                                    idx += 1;
                                }
                                Out::Suspend(_) => {
                                    return Out::Suspend(Cont::Apply {
                                        step: ApplyStep::TupleLitElement {
                                            evaluated,
                                            remaining: remaining[idx + 1..].to_vec(),
                                        },
                                        locals,
                                        callables,
                                        depth,
                                        handler_stack: self.active_inline_handler_stack.clone(),
                                    });
                                }
                                Out::Escape(escape) => return Out::Escape(escape),
                                Out::Err(e) => return Out::Err(e),
                            }
                        }
                        Out::Done(RuntimeValue::Tuple(evaluated))
                    }
                    ApplyStep::ListLitElement {
                        mut evaluated,
                        remaining,
                        spread,
                        resuming_spread,
                    } => {
                        if resuming_spread {
                            match value {
                                RuntimeValue::ListInt(mut ints) => {
                                    for item in evaluated.into_iter().rev() {
                                        match item {
                                            RuntimeValue::Int(n) => ints.insert(0, n),
                                            RuntimeValue::String(_) => {
                                                return Out::Err(RuntimeError::Unsupported);
                                            }
                                            _ => return Out::Err(RuntimeError::Unsupported),
                                        }
                                    }
                                    return Out::Done(RuntimeValue::ListInt(ints));
                                }
                                RuntimeValue::ListString(mut strings) => {
                                    for item in evaluated.into_iter().rev() {
                                        match item {
                                            RuntimeValue::String(text) => strings.insert(0, text),
                                            RuntimeValue::Int(_) => {
                                                return Out::Err(RuntimeError::Unsupported);
                                            }
                                            _ => return Out::Err(RuntimeError::Unsupported),
                                        }
                                    }
                                    return Out::Done(RuntimeValue::ListString(strings));
                                }
                                _ => return Out::Err(RuntimeError::Unsupported),
                            }
                        }

                        evaluated.push(value);
                        let mut idx = 0;
                        while idx < remaining.len() {
                            match self.eval_expr(
                                &remaining[idx],
                                &locals,
                                &callables,
                                evaluators,
                                depth + 1,
                            ) {
                                Out::Done(v) => {
                                    evaluated.push(v);
                                    idx += 1;
                                }
                                Out::Suspend(_) => {
                                    return Out::Suspend(Cont::Apply {
                                        step: ApplyStep::ListLitElement {
                                            evaluated,
                                            remaining: remaining[idx + 1..].to_vec(),
                                            spread,
                                            resuming_spread: false,
                                        },
                                        locals,
                                        callables,
                                        depth,
                                        handler_stack: self.active_inline_handler_stack.clone(),
                                    });
                                }
                                Out::Escape(escape) => return Out::Escape(escape),
                                Out::Err(e) => return Out::Err(e),
                            }
                        }
                        if let Some(tail) = spread {
                            match self.eval_expr(&tail, &locals, &callables, evaluators, depth + 1)
                            {
                                Out::Done(tail_value) => {
                                    return match (self.build_runtime_list(evaluated), tail_value) {
                                        (
                                            Out::Done(RuntimeValue::ListInt(mut ints)),
                                            RuntimeValue::ListInt(mut tail_ints),
                                        ) => {
                                            ints.append(&mut tail_ints);
                                            Out::Done(RuntimeValue::ListInt(ints))
                                        }
                                        (
                                            Out::Done(RuntimeValue::ListString(mut strings)),
                                            RuntimeValue::ListString(mut tail_strings),
                                        ) => {
                                            strings.append(&mut tail_strings);
                                            Out::Done(RuntimeValue::ListString(strings))
                                        }
                                        (
                                            Out::Done(RuntimeValue::ListInt(ints)),
                                            RuntimeValue::ListString(tail_strings),
                                        ) => {
                                            if ints.is_empty() && tail_strings.is_empty() {
                                                Out::Done(RuntimeValue::ListInt(ints))
                                            } else {
                                                Out::Err(RuntimeError::Unsupported)
                                            }
                                        }
                                        (
                                            Out::Done(RuntimeValue::ListString(strings)),
                                            RuntimeValue::ListInt(tail_ints),
                                        ) => {
                                            if strings.is_empty() && tail_ints.is_empty() {
                                                Out::Done(RuntimeValue::ListString(strings))
                                            } else {
                                                Out::Err(RuntimeError::Unsupported)
                                            }
                                        }
                                        (Out::Done(_), _) => Out::Err(RuntimeError::Unsupported),
                                        (Out::Err(e), _) => Out::Err(e),
                                        (Out::Suspend(_), _) => Out::Err(RuntimeError::Unsupported),
                                        (Out::Escape(escape), _) => Out::Escape(escape),
                                    };
                                }
                                Out::Suspend(_) => {
                                    return Out::Suspend(Cont::Apply {
                                        step: ApplyStep::ListLitElement {
                                            evaluated,
                                            remaining: vec![],
                                            spread: Some(tail),
                                            resuming_spread: true,
                                        },
                                        locals,
                                        callables,
                                        depth,
                                        handler_stack: self.active_inline_handler_stack.clone(),
                                    });
                                }
                                Out::Escape(escape) => return Out::Escape(escape),
                                Out::Err(e) => return Out::Err(e),
                            }
                        }
                        self.build_runtime_list(evaluated)
                    }
                    ApplyStep::RecordField {
                        constructor,
                        mut evaluated,
                        pending_field,
                        remaining,
                    } => {
                        evaluated.push((pending_field, value));
                        let mut idx = 0;
                        while idx < remaining.len() {
                            let (field_name, field_expr) = &remaining[idx];
                            match self.eval_expr(
                                field_expr,
                                &locals,
                                &callables,
                                evaluators,
                                depth + 1,
                            ) {
                                Out::Done(v) => {
                                    evaluated.push((field_name.clone(), v));
                                    idx += 1;
                                }
                                Out::Suspend(_) => {
                                    return Out::Suspend(Cont::Apply {
                                        step: ApplyStep::RecordField {
                                            constructor,
                                            evaluated,
                                            pending_field: field_name.clone(),
                                            remaining: remaining[idx + 1..].to_vec(),
                                        },
                                        locals,
                                        callables,
                                        depth,
                                        handler_stack: self.active_inline_handler_stack.clone(),
                                    });
                                }
                                Out::Escape(escape) => return Out::Escape(escape),
                                Out::Err(e) => return Out::Err(e),
                            }
                        }
                        Out::Done(RuntimeValue::Record {
                            constructor,
                            fields: evaluated.into_iter().collect(),
                        })
                    }
                    ApplyStep::WithBody { .. } => Out::Err(RuntimeError::Unsupported),
                }
            }
            Cont::Resume => self.resume_through_active_continuation_out(value, evaluators),
        }
    }
}
