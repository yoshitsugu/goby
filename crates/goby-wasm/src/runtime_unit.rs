use std::collections::HashMap;

use goby_core::Expr;

use crate::call::flatten_named_call;
use crate::runtime_eval::{AstLambdaCallable, IntCallable};
use crate::runtime_flow::{Escape, FinishKind, Out, RuntimeError, RuntimeEvaluators};
use crate::runtime_value::{RuntimeLocals, RuntimeValue};
use crate::{ERR_CALLABLE_DISPATCH_DECL_PARAM, RuntimeOutputResolver};

impl<'m> RuntimeOutputResolver<'m> {
    pub(crate) fn execute_unit_expr_ast(
        &mut self,
        expr: &Expr,
        locals: &mut RuntimeLocals,
        callables: &mut HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<()> {
        if let Expr::With { handler, body } = expr {
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
            let result = self.execute_unit_ast_stmt_sequence(
                body,
                locals,
                callables,
                evaluators,
                depth + 1,
                FinishKind::WithBody { with_id },
            );
            if let Some(updated_handler) = self.active_inline_handler_stack.pop() {
                locals.apply_selected_from(
                    &updated_handler.captured_locals,
                    &updated_handler.changed_outer_names,
                );
            }
            return match result {
                Out::Done(()) => Out::Done(()),
                Out::Suspend(cont) => Out::Suspend(cont),
                Out::Escape(escape) => match escape {
                    Escape::WithScope {
                        with_id: target_id, ..
                    } if target_id == with_id => Out::Done(()),
                    other => Out::Escape(other),
                },
                Out::Err(e) => Out::Err(e),
            };
        }

        if self
            .try_execute_imported_decl_call_as_side_effect(
                expr,
                locals,
                callables,
                evaluators,
                depth + 1,
            )
            .is_some()
        {
            return Out::Done(());
        }

        if let Some((fn_name, args)) = flatten_named_call(expr)
            && args.len() > 1
            && self
                .execute_decl_call_chain_as_side_effect(
                    fn_name,
                    args.as_slice(),
                    locals,
                    callables,
                    evaluators,
                    depth + 1,
                )
                .is_some()
        {
            return Out::Done(());
        }

        if let Expr::Call { callee, arg, .. } = expr
            && let Expr::Qualified { receiver, member, .. } = callee.as_ref()
        {
            let arg_val = match self.eval_expr(arg, locals, callables, evaluators, depth + 1) {
                Out::Done(v) => v,
                Out::Suspend(cont) => return Out::Suspend(cont),
                Out::Escape(escape) => return Out::Escape(escape),
                Out::Err(e) => return Out::Err(e),
            };
            if let Some(handler) = self.resolve_qualified_effect_handler(receiver, member) {
                return self.dispatch_effect_handler_as_side_effect(
                    handler,
                    arg_val,
                    evaluators,
                    depth + 1,
                );
            }
        }

        if let Expr::Call { callee, arg, .. } = expr
            && let Expr::Var { name: fn_name, .. } = callee.as_ref()
        {
            if let Expr::Lambda { param, body } = arg.as_ref() {
                let callable = IntCallable::AstLambda(Box::new(AstLambdaCallable {
                    parameter: param.clone(),
                    body: *body.clone(),
                    captured_locals: locals.clone(),
                    captured_callables: callables.clone(),
                }));
                if self
                    .execute_decl_with_callable_as_side_effect(
                        fn_name,
                        callable,
                        evaluators,
                        depth + 1,
                    )
                    .is_some()
                {
                    return Out::Done(());
                }
            }
            if let Expr::Var { name: arg_name, .. } = arg.as_ref()
                && self.declaration_expects_callable_param(fn_name)
                && self
                    .execute_decl_with_callable_as_side_effect(
                        fn_name,
                        self.resolve_callable_argument(arg_name, callables),
                        evaluators,
                        depth + 1,
                    )
                    .is_some()
            {
                return Out::Done(());
            }
            if self.declaration_expects_callable_param(fn_name)
                && !matches!(arg.as_ref(), Expr::Lambda { .. } | Expr::Var { name: _, .. })
            {
                self.set_runtime_error_once(ERR_CALLABLE_DISPATCH_DECL_PARAM);
                return Out::Err(RuntimeError::Unsupported);
            }
            let arg_val = match self.eval_expr(arg, locals, callables, evaluators, depth) {
                Out::Done(v) => v,
                Out::Suspend(cont) => return Out::Suspend(cont),
                Out::Escape(escape) => return Out::Escape(escape),
                Out::Err(e) => return Out::Err(e),
            };
            if let Some(callable) = callables.get(fn_name)
                && self
                    .dispatch_callable_side_effect(
                        callable,
                        arg_val.clone(),
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    )
                    .is_some()
            {
                return Out::Done(());
            }
            let bare_method = self.find_handler_method_by_name(fn_name);
            if let Some(method) = bare_method {
                return self.dispatch_handler_method(&method, arg_val, evaluators, depth + 1);
            }
            if fn_name.starts_with("__goby_")
                && self
                    .apply_runtime_intrinsic_ast(
                        fn_name,
                        std::slice::from_ref(&arg_val),
                        evaluators,
                        depth + 1,
                    )
                    .is_some()
            {
                return Out::Done(());
            }
            if let Some(handler) = self.resolve_bare_effect_handler(fn_name) {
                return self.dispatch_effect_handler_as_side_effect(
                    handler,
                    arg_val.clone(),
                    evaluators,
                    depth + 1,
                );
            }
            if self
                .execute_unit_call_ast(
                    fn_name,
                    arg_val.clone(),
                    locals,
                    callables,
                    evaluators,
                    depth,
                )
                .is_some()
            {
                return Out::Done(());
            }
            if self
                .execute_decl_as_side_effect(fn_name, arg_val.clone(), evaluators, depth + 1)
                .is_some()
            {
                return Out::Done(());
            }
            if self.unique_effect_name_for_operation(fn_name).is_some() {
                self.set_unhandled_effect_error(fn_name);
            }
            let Some(repr) = expr.to_str_repr() else {
                return Out::Err(RuntimeError::Unsupported);
            };
            return self.execute_unit_call_out(&repr, locals, callables, evaluators);
        }

        if let Expr::Pipeline { value, callee } = expr {
            match self.eval_expr(value, locals, callables, evaluators, depth) {
                Out::Done(v) => {
                    let bare_method = self.find_handler_method_by_name(callee);
                    if let Some(method) = bare_method {
                        return self.dispatch_handler_method(&method, v, evaluators, depth + 1);
                    }
                    if let Some(handler) = self.resolve_bare_effect_handler(callee) {
                        return self.dispatch_effect_handler_as_side_effect(
                            handler,
                            v.clone(),
                            evaluators,
                            depth + 1,
                        );
                    }
                    if self
                        .execute_unit_call_ast(
                            callee,
                            v.clone(),
                            locals,
                            callables,
                            evaluators,
                            depth,
                        )
                        .is_some()
                    {
                        return Out::Done(());
                    }
                    if self.unique_effect_name_for_operation(callee).is_some() {
                        self.set_unhandled_effect_error(callee);
                    }
                }
                Out::Suspend(cont) => return Out::Suspend(cont),
                Out::Escape(escape) => return Out::Escape(escape),
                Out::Err(_) => {}
            };
            let Some(repr) = expr.to_str_repr() else {
                return Out::Err(RuntimeError::Unsupported);
            };
            return self.execute_unit_call_out(&repr, locals, callables, evaluators);
        }

        if let Expr::Var { name: _, .. } | Expr::IntLit(_) | Expr::StringLit(_) | Expr::BoolLit(_) = expr {
            return Out::Done(());
        }

        if let Expr::Case { .. } = expr {
            let Expr::Case { scrutinee, arms } = expr else {
                unreachable!();
            };
            let scrutinee_val =
                match self.eval_expr(scrutinee, locals, callables, evaluators, depth) {
                    Out::Done(v) => v,
                    Out::Suspend(cont) => return Out::Suspend(cont),
                    Out::Escape(escape) => return Out::Escape(escape),
                    Out::Err(e) => return Out::Err(e),
                };
            let Some((arm_body, mut arm_locals)) =
                self.select_case_arm(&scrutinee_val, arms, locals)
            else {
                return Out::Err(RuntimeError::Unsupported);
            };
            let mut arm_callables = callables.clone();
            return self.execute_unit_expr_ast(
                &arm_body,
                &mut arm_locals,
                &mut arm_callables,
                evaluators,
                depth + 1,
            );
        }

        if let Expr::If {
            condition,
            then_expr,
            else_expr,
        } = expr
        {
            let cond_val = match self.eval_expr(condition, locals, callables, evaluators, depth) {
                Out::Done(v) => v,
                Out::Suspend(cont) => return Out::Suspend(cont),
                Out::Escape(escape) => return Out::Escape(escape),
                Out::Err(e) => return Out::Err(e),
            };
            let branch = match cond_val {
                RuntimeValue::Bool(true) => then_expr,
                RuntimeValue::Bool(false) => else_expr,
                _ => return Out::Err(RuntimeError::Unsupported),
            };
            return self.execute_unit_expr_ast(branch, locals, callables, evaluators, depth + 1);
        }

        if let Expr::Block(stmts) = expr {
            return self.execute_unit_ast_stmt_sequence(
                stmts,
                locals,
                callables,
                evaluators,
                depth + 1,
                FinishKind::Block,
            );
        }

        match self.eval_expr(expr, locals, callables, evaluators, depth + 1) {
            Out::Done(_) => return Out::Done(()),
            Out::Suspend(cont) => return Out::Suspend(cont),
            Out::Escape(escape) => return Out::Escape(escape),
            Out::Err(_) => {}
        }

        if let Expr::Call { .. } | Expr::Pipeline { .. } = expr {
            let Some(repr) = expr.to_str_repr() else {
                return Out::Err(RuntimeError::Unsupported);
            };
            return self.execute_unit_call_out(&repr, locals, callables, evaluators);
        }

        Out::Err(RuntimeError::Unsupported)
    }
}
