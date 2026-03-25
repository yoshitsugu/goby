use std::collections::HashMap;

use goby_core::{Expr, ast::InterpolatedPart};

use crate::runtime_eval::IntCallable;
use crate::runtime_flow::{
    ApplyStep, Cont, DirectCallHead, Escape, FinishKind, Out, RuntimeError, RuntimeEvaluators,
};
use crate::runtime_support::flatten_direct_call;
use crate::runtime_value::{RuntimeLocals, RuntimeValue};
use crate::{
    MAX_EVAL_DEPTH, RuntimeOutputResolver, effective_runtime_imports, runtime_import_selects_name,
};

impl<'m> RuntimeOutputResolver<'m> {
    pub(crate) fn apply_receiver_method_value_call_out(
        &mut self,
        receiver: &str,
        member: &str,
        arg_value: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        match self.eval_imported_decl_as_value_with_args_out(
            &DirectCallHead::Qualified {
                receiver: receiver.to_string(),
                member: member.to_string(),
            },
            std::slice::from_ref(&arg_value),
            evaluators,
            depth + 1,
        ) {
            Out::Err(RuntimeError::Unsupported) => {}
            other => return other,
        }
        if let Some(handler) = self.resolve_qualified_effect_handler(receiver, member) {
            return self.dispatch_effect_handler_as_value_flow(
                handler,
                arg_value,
                evaluators,
                depth + 1,
            );
        }
        Out::Err(RuntimeError::Unsupported)
    }

    pub(crate) fn eval_imported_decl_as_value_with_args_out(
        &mut self,
        head: &DirectCallHead,
        args: &[RuntimeValue],
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return Out::Err(RuntimeError::Unsupported);
        }
        let Some(decl) = self.resolve_imported_runtime_decl(head) else {
            return Out::Err(RuntimeError::Unsupported);
        };
        if decl
            .callable_param_mask
            .iter()
            .any(|is_callable| *is_callable)
        {
            return Out::Err(RuntimeError::Unsupported);
        }
        let accepts_unit_arg_as_zero_arity =
            decl.params.is_empty() && matches!(args, [RuntimeValue::Unit]);
        if decl.params.len() != args.len() && !accepts_unit_arg_as_zero_arity {
            return Out::Err(RuntimeError::Unsupported);
        }

        let mut fn_locals = RuntimeLocals::default();
        if !accepts_unit_arg_as_zero_arity {
            for (param, arg) in decl.params.iter().zip(args.iter()) {
                fn_locals.store(param, arg.clone());
            }
        }
        let fn_callables = HashMap::new();
        self.eval_runtime_decl_body_out(&decl, fn_locals, fn_callables, evaluators, depth + 1)
    }

    pub(crate) fn operation_has_conflicting_effect(
        &self,
        op_name: &str,
        expected_effect: &str,
    ) -> bool {
        let mut effect_names = self.visible_effect_names_for_operation_in(self.module, op_name);
        if !self.current_module_stack.is_empty() {
            effect_names.extend(self.visible_effect_names_for_operation(op_name));
        }
        effect_names.retain(|name| name != expected_effect);
        !effect_names.is_empty()
    }

    pub(crate) fn eval_expr_to_option(
        &mut self,
        expr: &Expr,
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        let mut out = self.eval_expr(expr, locals, callables, evaluators, depth);
        loop {
            match out {
                Out::Done(v) => return Some(v),
                Out::Suspend(cont) => {
                    out = self.apply_cont(cont, RuntimeValue::Unit, evaluators);
                }
                Out::Escape(_) => return None,
                Out::Err(RuntimeError::Abort { .. }) => {
                    self.mark_runtime_abort();
                    return None;
                }
                Out::Err(RuntimeError::Unsupported) => return None,
            }
        }
    }

    pub(crate) fn complete_value_out(
        &mut self,
        mut out: Out<RuntimeValue>,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<RuntimeValue> {
        loop {
            match out {
                Out::Done(value) => return Some(value),
                Out::Suspend(cont) => {
                    out = self.apply_cont(cont, RuntimeValue::Unit, evaluators);
                }
                Out::Escape(_) | Out::Err(_) => return None,
            }
        }
    }

    #[allow(dead_code)]
    pub(crate) fn eval_expr(
        &mut self,
        expr: &Expr,
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return Out::Err(RuntimeError::Unsupported);
        }

        if let Some(value) = self.try_eval_imported_decl_call_as_value(
            expr,
            locals,
            callables,
            evaluators,
            depth + 1,
        ) {
            return Out::Done(value);
        }

        if let Some((DirectCallHead::Bare(name), args)) = flatten_direct_call(expr)
            && name == "split"
            && args.len() == 2
            && effective_runtime_imports(self.current_runtime_module())
                .into_iter()
                .any(|import| {
                    import.module_path == "goby/string"
                        && runtime_import_selects_name(&import.kind, "split")
                })
        {
            let value = match self.eval_expr(args[0], locals, callables, evaluators, depth + 1) {
                Out::Done(value) => value,
                Out::Suspend(cont) => return Out::Suspend(cont),
                Out::Escape(escape) => return Out::Escape(escape),
                Out::Err(e) => return Out::Err(e),
            };
            let delim = match self.eval_expr(args[1], locals, callables, evaluators, depth + 1) {
                Out::Done(value) => value,
                Out::Suspend(cont) => return Out::Suspend(cont),
                Out::Escape(escape) => return Out::Escape(escape),
                Out::Err(e) => return Out::Err(e),
            };
            return match (value, delim) {
                (RuntimeValue::String(s), RuntimeValue::String(delim)) => {
                    let parts = if s.is_empty() {
                        Vec::new()
                    } else {
                        s.split(delim.as_str())
                            .map(|part| part.to_string())
                            .collect()
                    };
                    Out::Done(RuntimeValue::ListString(parts))
                }
                _ => Out::Err(RuntimeError::Unsupported),
            };
        }

        match expr {
            Expr::IntLit(n) => Out::Done(RuntimeValue::Int(*n)),
            Expr::BoolLit(b) => Out::Done(RuntimeValue::Bool(*b)),
            Expr::StringLit(s) => Out::Done(RuntimeValue::String(s.clone())),
            Expr::Var { name, .. } => match locals.get(name) {
                Some(v) => Out::Done(v),
                None => match self.eval_zero_arity_decl_value(name, evaluators, depth + 1) {
                    Some(v) => Out::Done(v),
                    None if self.runtime_error_is_abort_marker() => Out::Err(RuntimeError::Abort {
                        kind: "aborted".into(),
                    }),
                    None => Out::Err(RuntimeError::Unsupported),
                },
            },
            Expr::Handler { clauses } => Out::Done(RuntimeValue::Handler(
                self.inline_handler_from_clauses(clauses, locals, callables),
            )),
            Expr::Lambda { .. } => Out::Err(RuntimeError::Unsupported),
            Expr::InterpolatedString(parts) => {
                let mut out_so_far = String::new();
                let parts_slice = parts.as_slice();
                let mut i = 0;
                while i < parts_slice.len() {
                    match &parts_slice[i] {
                        InterpolatedPart::Text(text) => {
                            out_so_far.push_str(text);
                            i += 1;
                        }
                        InterpolatedPart::Expr(inner_expr) => {
                            match self.eval_expr(
                                inner_expr,
                                locals,
                                callables,
                                evaluators,
                                depth + 1,
                            ) {
                                Out::Done(value) => {
                                    out_so_far.push_str(&value.to_output_text());
                                    i += 1;
                                }
                                Out::Suspend(_) => {
                                    let remaining = parts_slice[i + 1..].to_vec();
                                    return Out::Suspend(Cont::Apply {
                                        step: ApplyStep::InterpolatedPart {
                                            accumulated: out_so_far,
                                            remaining,
                                        },
                                        locals: locals.clone(),
                                        callables: callables.clone(),
                                        depth,
                                        handler_stack: self.active_inline_handler_stack.clone(),
                                    });
                                }
                                Out::Escape(escape) => return Out::Escape(escape),
                                Out::Err(e) => return Out::Err(e),
                            }
                        }
                    }
                }
                Out::Done(RuntimeValue::String(out_so_far))
            }
            Expr::BinOp { op, left, right } => {
                let lv = match self.eval_expr(left, locals, callables, evaluators, depth + 1) {
                    Out::Done(v) => v,
                    Out::Suspend(_) => {
                        return Out::Suspend(Cont::Apply {
                            step: ApplyStep::BinOpLeft {
                                op: op.clone(),
                                right: (**right).clone(),
                            },
                            locals: locals.clone(),
                            callables: callables.clone(),
                            depth,
                            handler_stack: self.active_inline_handler_stack.clone(),
                        });
                    }
                    Out::Escape(escape) => return Out::Escape(escape),
                    Out::Err(e) => return Out::Err(e),
                };
                let rv = match self.eval_expr(right, locals, callables, evaluators, depth + 1) {
                    Out::Done(v) => v,
                    Out::Suspend(_) => {
                        return Out::Suspend(Cont::Apply {
                            step: ApplyStep::BinOpRight {
                                op: op.clone(),
                                left: lv,
                            },
                            locals: locals.clone(),
                            callables: callables.clone(),
                            depth,
                            handler_stack: self.active_inline_handler_stack.clone(),
                        });
                    }
                    Out::Escape(escape) => return Out::Escape(escape),
                    Out::Err(e) => return Out::Err(e),
                };
                match self.apply_binop_runtime_value(op.clone(), lv, rv) {
                    Some(v) => Out::Done(v),
                    None => Out::Err(RuntimeError::Unsupported),
                }
            }
            Expr::ListLit { elements, spread } => {
                let mut int_items: Vec<i64> = Vec::with_capacity(elements.len());
                let mut string_items: Vec<String> = Vec::with_capacity(elements.len());
                let mut list_kind: Option<&'static str> = None;
                let elems_slice = elements.as_slice();
                let mut i = 0;
                while i < elems_slice.len() {
                    match self.eval_expr(&elems_slice[i], locals, callables, evaluators, depth + 1)
                    {
                        Out::Done(value) => {
                            match &value {
                                RuntimeValue::Int(n) => {
                                    if list_kind == Some("string") {
                                        return Out::Err(RuntimeError::Unsupported);
                                    }
                                    list_kind = Some("int");
                                    int_items.push(*n);
                                }
                                RuntimeValue::String(text) => {
                                    if list_kind == Some("int") {
                                        return Out::Err(RuntimeError::Unsupported);
                                    }
                                    list_kind = Some("string");
                                    string_items.push(text.clone());
                                }
                                _ => return Out::Err(RuntimeError::Unsupported),
                            }
                            i += 1;
                        }
                        Out::Suspend(_) => {
                            let evaluated: Vec<RuntimeValue> = if list_kind == Some("string") {
                                string_items
                                    .iter()
                                    .map(|s| RuntimeValue::String(s.clone()))
                                    .collect()
                            } else {
                                int_items.iter().map(|n| RuntimeValue::Int(*n)).collect()
                            };
                            let remaining = elems_slice[i + 1..].to_vec();
                            return Out::Suspend(Cont::Apply {
                                step: ApplyStep::ListLitElement {
                                    evaluated,
                                    remaining,
                                    spread: spread.clone().map(|s| *s),
                                    resuming_spread: false,
                                },
                                locals: locals.clone(),
                                callables: callables.clone(),
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                            });
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    }
                }
                if let Some(tail) = spread {
                    match self.eval_expr(tail, locals, callables, evaluators, depth + 1) {
                        Out::Done(tail_value) => {
                            return match tail_value {
                                RuntimeValue::ListInt(mut values) => {
                                    if list_kind == Some("string") {
                                        if values.is_empty() {
                                            Out::Done(RuntimeValue::ListString(string_items))
                                        } else {
                                            Out::Err(RuntimeError::Unsupported)
                                        }
                                    } else {
                                        int_items.append(&mut values);
                                        Out::Done(RuntimeValue::ListInt(int_items))
                                    }
                                }
                                RuntimeValue::ListString(mut values) => {
                                    if list_kind == Some("int") {
                                        if values.is_empty() {
                                            Out::Done(RuntimeValue::ListInt(int_items))
                                        } else {
                                            Out::Err(RuntimeError::Unsupported)
                                        }
                                    } else {
                                        string_items.append(&mut values);
                                        Out::Done(RuntimeValue::ListString(string_items))
                                    }
                                }
                                _ => Out::Err(RuntimeError::Unsupported),
                            };
                        }
                        Out::Suspend(_) => {
                            let evaluated: Vec<RuntimeValue> = if list_kind == Some("string") {
                                string_items
                                    .iter()
                                    .map(|s| RuntimeValue::String(s.clone()))
                                    .collect()
                            } else {
                                int_items.iter().map(|n| RuntimeValue::Int(*n)).collect()
                            };
                            return Out::Suspend(Cont::Apply {
                                step: ApplyStep::ListLitElement {
                                    evaluated,
                                    remaining: vec![],
                                    spread: Some((**tail).clone()),
                                    resuming_spread: true,
                                },
                                locals: locals.clone(),
                                callables: callables.clone(),
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                            });
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    }
                }
                match list_kind {
                    Some("string") => Out::Done(RuntimeValue::ListString(string_items)),
                    _ => Out::Done(RuntimeValue::ListInt(int_items)),
                }
            }
            Expr::TupleLit(items) => {
                if items.is_empty() {
                    return Out::Done(RuntimeValue::Unit);
                }
                let mut values: Vec<RuntimeValue> = Vec::with_capacity(items.len());
                let items_slice = items.as_slice();
                let mut i = 0;
                while i < items_slice.len() {
                    match self.eval_expr(&items_slice[i], locals, callables, evaluators, depth + 1)
                    {
                        Out::Done(v) => {
                            values.push(v);
                            i += 1;
                        }
                        Out::Suspend(_) => {
                            let remaining = items_slice[i + 1..].to_vec();
                            return Out::Suspend(Cont::Apply {
                                step: ApplyStep::TupleLitElement {
                                    evaluated: values,
                                    remaining,
                                },
                                locals: locals.clone(),
                                callables: callables.clone(),
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                            });
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    }
                }
                Out::Done(RuntimeValue::Tuple(values))
            }
            Expr::Block(stmts) => match self.eval_stmts(
                stmts,
                locals.clone(),
                callables.clone(),
                evaluators,
                depth + 1,
                FinishKind::Block,
            ) {
                Out::Done((value, _updated_locals)) => {
                    Out::Done(value.unwrap_or(RuntimeValue::Unit))
                }
                Out::Suspend(cont) => Out::Suspend(cont),
                Out::Escape(escape) => Out::Escape(escape),
                Out::Err(e) => Out::Err(e),
            },
            Expr::Case { scrutinee, arms } => {
                let scrutinee_val =
                    match self.eval_expr(scrutinee, locals, callables, evaluators, depth + 1) {
                        Out::Done(v) => v,
                        Out::Suspend(_) => {
                            return Out::Suspend(Cont::Apply {
                                step: ApplyStep::CaseSelect { arms: arms.clone() },
                                locals: locals.clone(),
                                callables: callables.clone(),
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                            });
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    };
                let Some((arm_body, arm_locals)) =
                    self.select_case_arm(&scrutinee_val, arms, locals)
                else {
                    return Out::Err(RuntimeError::Unsupported);
                };
                match self.eval_expr(&arm_body, &arm_locals, callables, evaluators, depth + 1) {
                    Out::Done(v) => Out::Done(v),
                    Out::Suspend(s) => Out::Suspend(s),
                    Out::Escape(escape) => Out::Escape(escape),
                    Out::Err(RuntimeError::Unsupported) => {
                        let mut arm_locals_for_unit = arm_locals;
                        let mut arm_callables = callables.clone();
                        match self.execute_unit_expr_ast(
                            &arm_body,
                            &mut arm_locals_for_unit,
                            &mut arm_callables,
                            evaluators,
                            depth + 1,
                        ) {
                            Out::Done(()) => Out::Done(RuntimeValue::Unit),
                            Out::Suspend(s) => Out::Suspend(s),
                            Out::Escape(escape) => Out::Escape(escape),
                            Out::Err(e) => Out::Err(e),
                        }
                    }
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
                        Out::Suspend(_) => {
                            return Out::Suspend(Cont::Apply {
                                step: ApplyStep::IfBranch {
                                    then_expr: (**then_expr).clone(),
                                    else_expr: (**else_expr).clone(),
                                },
                                locals: locals.clone(),
                                callables: callables.clone(),
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                            });
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    };
                let branch: &Expr = match cond_val {
                    RuntimeValue::Bool(true) => then_expr,
                    RuntimeValue::Bool(false) => else_expr,
                    _ => return Out::Err(RuntimeError::Unsupported),
                };
                match self.eval_expr(branch, locals, callables, evaluators, depth + 1) {
                    Out::Done(v) => Out::Done(v),
                    Out::Suspend(s) => Out::Suspend(s),
                    Out::Escape(escape) => Out::Escape(escape),
                    Out::Err(RuntimeError::Unsupported) => {
                        let mut branch_locals = locals.clone();
                        let mut branch_callables = callables.clone();
                        match self.execute_unit_expr_ast(
                            branch,
                            &mut branch_locals,
                            &mut branch_callables,
                            evaluators,
                            depth + 1,
                        ) {
                            Out::Done(()) => Out::Done(RuntimeValue::Unit),
                            Out::Suspend(s) => Out::Suspend(s),
                            Out::Escape(escape) => Out::Escape(escape),
                            Out::Err(e) => Out::Err(e),
                        }
                    }
                    Out::Err(e) => Out::Err(e),
                }
            }
            Expr::Call { callee, arg, .. } => {
                if let Some((fn_name, args)) = crate::call::flatten_named_call(expr)
                    && args.len() > 1
                {
                    if let Some(value) = self.eval_decl_call_chain_as_value(
                        fn_name,
                        args.as_slice(),
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    ) {
                        return Out::Done(value);
                    }

                    let mut evaluated: Vec<RuntimeValue> = Vec::new();
                    let args_owned: Vec<Expr> = args.into_iter().cloned().collect();
                    let mut i = 0;
                    loop {
                        if i >= args_owned.len() {
                            break;
                        }
                        match self.eval_expr(
                            &args_owned[i],
                            locals,
                            callables,
                            evaluators,
                            depth + 1,
                        ) {
                            Out::Done(v) => {
                                evaluated.push(v);
                                i += 1;
                            }
                            Out::Suspend(_) => {
                                let remaining = args_owned[i + 1..].to_vec();
                                return Out::Suspend(Cont::Apply {
                                    step: ApplyStep::MultiArgCall {
                                        fn_name: fn_name.to_string(),
                                        evaluated,
                                        remaining,
                                    },
                                    locals: locals.clone(),
                                    callables: callables.clone(),
                                    depth,
                                    handler_stack: self.active_inline_handler_stack.clone(),
                                });
                            }
                            Out::Escape(escape) => return Out::Escape(escape),
                            Out::Err(e) => return Out::Err(e),
                        }
                    }
                    return self.apply_named_value_call_args_out(
                        fn_name,
                        &evaluated,
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    );
                }

                if let Expr::Qualified {
                    receiver, member, ..
                } = callee.as_ref()
                {
                    let arg_value =
                        match self.eval_expr(arg, locals, callables, evaluators, depth + 1) {
                            Out::Done(v) => v,
                            Out::Suspend(_) => {
                                return Out::Suspend(Cont::Apply {
                                    step: ApplyStep::ReceiverMethod {
                                        receiver: receiver.clone(),
                                        member: member.clone(),
                                    },
                                    locals: locals.clone(),
                                    callables: callables.clone(),
                                    depth,
                                    handler_stack: self.active_inline_handler_stack.clone(),
                                });
                            }
                            Out::Escape(escape) => return Out::Escape(escape),
                            Out::Err(e) => return Out::Err(e),
                        };
                    return self.apply_receiver_method_value_call_out(
                        receiver,
                        member,
                        arg_value,
                        evaluators,
                        depth + 1,
                    );
                }

                if let Expr::Var {
                    name: ctor_name, ..
                } = callee.as_ref()
                    && let Some(field_name) = self.single_field_constructor_field(ctor_name)
                {
                    let field_value =
                        match self.eval_expr(arg, locals, callables, evaluators, depth + 1) {
                            Out::Done(v) => v,
                            Out::Suspend(s) => return Out::Suspend(s),
                            Out::Escape(escape) => return Out::Escape(escape),
                            Out::Err(e) => return Out::Err(e),
                        };
                    let mut fields = HashMap::new();
                    fields.insert(field_name, field_value);
                    return Out::Done(RuntimeValue::Record {
                        constructor: ctor_name.clone(),
                        fields,
                    });
                }

                if let Expr::Var { name: fn_name, .. } = callee.as_ref() {
                    let arg_value =
                        match self.eval_expr(arg, locals, callables, evaluators, depth + 1) {
                            Out::Done(v) => v,
                            Out::Suspend(_) => {
                                return Out::Suspend(Cont::Apply {
                                    step: ApplyStep::SingleArgCall {
                                        fn_name: fn_name.clone(),
                                    },
                                    locals: locals.clone(),
                                    callables: callables.clone(),
                                    depth,
                                    handler_stack: self.active_inline_handler_stack.clone(),
                                });
                            }
                            Out::Escape(escape) => return Out::Escape(escape),
                            Out::Err(e) => return Out::Err(e),
                        };
                    return self.apply_named_value_call_out(
                        fn_name,
                        arg_value,
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    );
                }

                let value = self.eval_expr_ast(expr, locals, callables, evaluators, depth);
                match value {
                    Some(v) => Out::Done(v),
                    None => Out::Err(RuntimeError::Unsupported),
                }
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } if args.len() == 1 => {
                let arg_value =
                    match self.eval_expr(&args[0], locals, callables, evaluators, depth + 1) {
                        Out::Done(v) => v,
                        Out::Suspend(_) => {
                            return Out::Suspend(Cont::Apply {
                                step: ApplyStep::ReceiverMethod {
                                    receiver: receiver.clone(),
                                    member: method.clone(),
                                },
                                locals: locals.clone(),
                                callables: callables.clone(),
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                            });
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    };
                self.apply_receiver_method_value_call_out(
                    receiver,
                    method,
                    arg_value,
                    evaluators,
                    depth + 1,
                )
            }
            Expr::Pipeline { value, callee } => {
                let pipeline_value =
                    match self.eval_expr(value, locals, callables, evaluators, depth + 1) {
                        Out::Done(v) => v,
                        Out::Suspend(_) => {
                            return Out::Suspend(Cont::Apply {
                                step: ApplyStep::Pipeline {
                                    callee: callee.clone(),
                                },
                                locals: locals.clone(),
                                callables: callables.clone(),
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                            });
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    };
                self.apply_pipeline_out(
                    callee,
                    pipeline_value,
                    locals,
                    callables,
                    evaluators,
                    depth + 1,
                )
            }
            Expr::RecordConstruct {
                constructor,
                fields,
            } => {
                if fields.is_empty() {
                    match self.apply_named_value_call_out(
                        constructor,
                        RuntimeValue::Unit,
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    ) {
                        Out::Err(RuntimeError::Unsupported) => {}
                        other => return other,
                    }
                }
                let mut field_map: HashMap<String, RuntimeValue> = HashMap::new();
                let fields_vec: Vec<(String, Expr)> =
                    fields.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                let mut i = 0;
                while i < fields_vec.len() {
                    let (field_name, field_expr) = &fields_vec[i];
                    match self.eval_expr(field_expr, locals, callables, evaluators, depth + 1) {
                        Out::Done(v) => {
                            field_map.insert(field_name.clone(), v);
                            i += 1;
                        }
                        Out::Suspend(_) => {
                            let evaluated: Vec<(String, RuntimeValue)> =
                                field_map.into_iter().collect();
                            let remaining = fields_vec[i + 1..].to_vec();
                            return Out::Suspend(Cont::Apply {
                                step: ApplyStep::RecordField {
                                    constructor: constructor.clone(),
                                    evaluated,
                                    pending_field: field_name.clone(),
                                    remaining,
                                },
                                locals: locals.clone(),
                                callables: callables.clone(),
                                depth,
                                handler_stack: self.active_inline_handler_stack.clone(),
                            });
                        }
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    }
                }
                Out::Done(RuntimeValue::Record {
                    constructor: constructor.clone(),
                    fields: field_map,
                })
            }
            Expr::Resume { value } => {
                let resumed = match self.eval_expr(value, locals, callables, evaluators, depth + 1)
                {
                    Out::Done(v) => v,
                    Out::Suspend(s) => return Out::Suspend(s),
                    Out::Escape(escape) => return Out::Escape(escape),
                    Out::Err(e) => return Out::Err(e),
                };
                self.resume_through_active_continuation_out(resumed, evaluators)
            }
            Expr::With { handler, body } => {
                let mut inline_handler =
                    match self.eval_expr(handler, locals, callables, evaluators, depth + 1) {
                        Out::Done(RuntimeValue::Handler(inline_handler)) => inline_handler,
                        Out::Done(_) => return Out::Err(RuntimeError::Unsupported),
                        Out::Suspend(_) => return Out::Err(RuntimeError::Unsupported),
                        Out::Escape(escape) => return Out::Escape(escape),
                        Out::Err(e) => return Out::Err(e),
                    };
                let with_id = self.fresh_with_id();
                inline_handler.with_id = Some(with_id);
                self.active_inline_handler_stack.push(inline_handler);
                let result = self.eval_stmts(
                    body,
                    locals.clone(),
                    callables.clone(),
                    evaluators,
                    depth + 1,
                    FinishKind::WithBody { with_id },
                );
                self.active_inline_handler_stack.pop();
                match result {
                    Out::Done((value, _locals)) => Out::Done(value.unwrap_or(RuntimeValue::Unit)),
                    Out::Escape(escape) => match escape {
                        Escape::WithScope {
                            with_id: target_id,
                            value,
                        } if target_id == with_id => Out::Done(value),
                        other => Out::Escape(other),
                    },
                    Out::Suspend(cont) => Out::Suspend(cont),
                    Out::Err(e) => Out::Err(e),
                }
            }
            _ => {
                let value = self.eval_expr_ast(expr, locals, callables, evaluators, depth);
                match value {
                    Some(v) => Out::Done(v),
                    None => Out::Err(RuntimeError::Unsupported),
                }
            }
        }
    }
}
