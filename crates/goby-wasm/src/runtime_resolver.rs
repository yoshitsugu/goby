use super::*;
use crate::call::flatten_named_call;
use crate::grapheme_semantics::{ControlFlow, GraphemeSpan, for_each_extended_grapheme_span};
use crate::runtime_eval::{AstLambdaCallable, IntCallable};
use crate::runtime_flow::RcCallables;
use goby_core::ast::UnaryOpKind;
use std::rc::Rc;

pub(super) struct ResolvedRuntimeOutput {
    pub(super) output: Option<String>,
    pub(super) runtime_error: Option<String>,
}

impl<'m> RuntimeOutputResolver<'m> {
    pub(super) fn resolve_detailed(
        module: &'m Module,
        body: Option<&str>,
        parsed_stmts: Option<&[Stmt]>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        execution_mode: lower::EffectExecutionMode,
        stdin_seed: Option<String>,
        allow_live_stdin: bool,
    ) -> ResolvedRuntimeOutput {
        let runtime_imports = load_runtime_import_context(module);
        let mut resolver = Self {
            locals: RuntimeLocals::default(),
            module,
            runtime_imports,
            embedded_effect_runtime: EmbeddedEffectRuntime::new(stdin_seed, allow_live_stdin),
            current_module_stack: Vec::new(),
            current_decl_stack: Vec::new(),
            active_inline_handler_stack: Vec::new(),
            resume_tokens: Vec::new(),
            optimized_resume_tokens: Vec::new(),
            pending_caller_cont_stack: Vec::new(),
            completed_stmt_seq_locals: None,
            runtime_error: None,
            next_with_id: 1,
            execution_mode,
        };

        if let Some(stmts) = parsed_stmts {
            if resolver
                .execute_ingest_ast_stmt_sequence(stmts, evaluators)
                .is_none()
                && resolver.runtime_error.is_none()
            {
                return ResolvedRuntimeOutput {
                    output: None,
                    runtime_error: None,
                };
            }
        } else if let Some(body) = body {
            for statement in statements(body) {
                if resolver.ingest_statement(statement, evaluators).is_none() {
                    if resolver.runtime_error.is_some() {
                        break;
                    }
                    return ResolvedRuntimeOutput {
                        output: None,
                        runtime_error: None,
                    };
                }
            }
        } else {
            return ResolvedRuntimeOutput {
                output: None,
                runtime_error: None,
            };
        }

        if resolver.runtime_error_is_abort_marker() {
            return ResolvedRuntimeOutput {
                output: if resolver.embedded_effect_runtime.outputs_are_empty() {
                    None
                } else {
                    Some(resolver.embedded_effect_runtime.concat_outputs())
                },
                runtime_error: None,
            };
        }

        ResolvedRuntimeOutput {
            output: if resolver.embedded_effect_runtime.outputs_are_empty() {
                None
            } else {
                Some(resolver.embedded_effect_runtime.concat_outputs())
            },
            runtime_error: resolver.runtime_error,
        }
    }

    #[allow(dead_code)]
    pub(super) fn ingest_ast_statement(
        &mut self,
        stmt: &Stmt,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        match stmt {
            Stmt::Binding { name, value, .. } => {
                // Propagate None so the caller can fall back to the string path
                // rather than silently dropping the binding.
                let runtime_val = self
                    .eval_expr_to_option(
                        value,
                        &self.locals.clone(),
                        &Rc::new(HashMap::new()),
                        evaluators,
                        1,
                    )
                    .or_else(|| {
                        let repr = value.to_str_repr()?;
                        self.eval_value_with_context(
                            &repr,
                            &self.locals.clone(),
                            &Rc::new(HashMap::new()),
                            evaluators,
                        )
                    })?;
                self.locals.store(name, runtime_val);
                Some(())
            }
            Stmt::MutBinding { name, value, .. } => {
                let runtime_val = self
                    .eval_expr_to_option(
                        value,
                        &self.locals.clone(),
                        &Rc::new(HashMap::new()),
                        evaluators,
                        1,
                    )
                    .or_else(|| {
                        let repr = value.to_str_repr()?;
                        self.eval_value_with_context(
                            &repr,
                            &self.locals.clone(),
                            &Rc::new(HashMap::new()),
                            evaluators,
                        )
                    })?;
                self.locals.store_mut(name, runtime_val);
                Some(())
            }
            Stmt::Assign { name, value, .. } => {
                self.locals.contains(name).then_some(())?;
                let runtime_val = self
                    .eval_expr_to_option(
                        value,
                        &self.locals.clone(),
                        &Rc::new(HashMap::new()),
                        evaluators,
                        1,
                    )
                    .or_else(|| {
                        let repr = value.to_str_repr()?;
                        self.eval_value_with_context(
                            &repr,
                            &self.locals.clone(),
                            &Rc::new(HashMap::new()),
                            evaluators,
                        )
                    })?;
                self.locals.assign(name, runtime_val).then_some(())?;
                Some(())
            }
            Stmt::Expr(expr, _) => {
                let mut locals = self.locals.clone();
                let callables = Rc::new(HashMap::new());
                let outputs_before = self.embedded_effect_runtime.output_len();
                match self.execute_unit_expr_ast(expr, &mut locals, &callables, evaluators, 0) {
                    Out::Done(()) => {
                        self.locals = locals;
                        Some(())
                    }
                    Out::Err(RuntimeError::Abort { .. })
                        if self.runtime_error_is_abort_marker() =>
                    {
                        self.clear_runtime_abort_marker();
                        Some(())
                    }
                    Out::Err(RuntimeError::Unsupported)
                        if self.runtime_error.is_none()
                            && self.embedded_effect_runtime.output_len() == outputs_before =>
                    {
                        let repr = expr.to_str_repr()?;
                        self.eval_side_effect(&repr, evaluators)
                    }
                    Out::Suspend(_) | Out::Escape(_) | Out::Err(_) => None,
                }
            }
        }
    }

    pub(super) fn ingest_statement(
        &mut self,
        statement: Statement<'_>,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        match statement {
            Statement::Binding { name, expr } => self.bind_local(name, expr, evaluators),
            Statement::MutBinding { name, expr } => self.bind_local_mut(name, expr, evaluators),
            Statement::Assign { name, expr } => {
                self.locals.contains(name).then_some(())?;
                self.assign_local(name, expr, evaluators)
            }
            Statement::Print(expr) => self.capture_print(expr, evaluators),
            Statement::Expr(expr) => self.eval_side_effect(expr, evaluators),
        }
    }

    pub(super) fn bind_local(
        &mut self,
        name: &str,
        expr: &str,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        // Propagate None on eval failure rather than silently clearing the binding.
        let value = self.eval_value(expr, evaluators)?;
        self.locals.store(name, value);
        Some(())
    }

    pub(super) fn bind_local_mut(
        &mut self,
        name: &str,
        expr: &str,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        let value = self.eval_value(expr, evaluators)?;
        self.locals.store_mut(name, value);
        Some(())
    }

    pub(super) fn assign_local(
        &mut self,
        name: &str,
        expr: &str,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        let value = self.eval_value(expr, evaluators)?;
        self.locals.assign(name, value).then_some(())
    }

    pub(super) fn capture_print(
        &mut self,
        expr: &str,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        self.capture_output_from_expr(expr, evaluators)
    }

    pub(super) fn eval_side_effect(
        &mut self,
        expr: &str,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        if let Some((value_expr, callee)) = parse_pipeline(expr) {
            if callee != BUILTIN_PRINT {
                return None;
            }
            return self.capture_output_from_expr(value_expr, evaluators);
        }

        self.execute_unit_call(
            expr,
            &RuntimeLocals::default(),
            &Rc::new(HashMap::new()),
            evaluators,
        )
    }

    pub(super) fn capture_output_from_expr(
        &mut self,
        expr: &str,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        let value = self.eval_value(expr, evaluators)?;
        self.embedded_effect_runtime
            .emit_output_text(value.to_output_text());
        Some(())
    }

    pub(super) fn eval_value(
        &mut self,
        expr: &str,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<RuntimeValue> {
        let callables = Rc::new(HashMap::new());
        let locals = self.locals.clone();
        self.eval_value_with_context(expr, &locals, &callables, evaluators)
    }

    pub(super) fn eval_value_with_context(
        &mut self,
        expr: &str,
        locals: &RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<RuntimeValue> {
        if let Some((left, callee)) = parse_pipeline(expr) {
            let left_value = self.eval_value_with_context(left, locals, callables, evaluators)?;
            return self.apply_pipeline(callee, left_value, locals, callables, evaluators, 0);
        }

        if let Some((callee, arg)) = parse_call(expr)
            && matches!(arg, "()")
        {
            let out = self.apply_named_value_call_out(
                callee,
                RuntimeValue::Unit,
                locals,
                callables,
                evaluators,
                0,
            );
            return self.complete_value_out(out, evaluators);
        }

        let string_locals = locals.string_values();
        if let Some(text) = eval_string_expr(expr, &string_locals) {
            return Some(RuntimeValue::String(text));
        }

        let int_locals = locals.int_values();
        if let Some(value) = evaluators.int.eval_expr(expr, &int_locals, callables) {
            return Some(RuntimeValue::Int(value));
        }

        let list_int_locals = locals.list_int_values();
        if let Some(values) = evaluators.list.eval_expr(expr, &list_int_locals) {
            return Some(RuntimeValue::list_from_ints(values));
        }

        None
    }

    pub(super) fn apply_pipeline(
        &mut self,
        callee: &str,
        value: RuntimeValue,
        locals: &RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        _depth: usize,
    ) -> Option<RuntimeValue> {
        if callee == BUILTIN_PRINT {
            return None;
        }
        // NOTE: delegates to the string-based path which uses its own depth
        // counter (IntEvaluator::depth), so _depth is not propagated into that
        // chain. This is a known limitation until the string path is removed.
        let call_expr = format!("{} {}", callee, value.to_expression_text());
        self.eval_value_with_context(&call_expr, locals, callables, evaluators)
    }

    pub(super) fn apply_pipeline_out(
        &mut self,
        callee: &str,
        value: RuntimeValue,
        locals: &RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        self.apply_named_value_call_out(callee, value, locals, callables, evaluators, depth)
    }

    pub(super) fn apply_runtime_intrinsic_ast(
        &mut self,
        name: &str,
        args: &[RuntimeValue],
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        match name {
            "__goby_env_fetch_env_var" => {
                if args.len() != 1 {
                    return None;
                }
                let RuntimeValue::String(var_name) = &args[0] else {
                    return None;
                };
                Some(RuntimeValue::String(
                    std::env::var(var_name).unwrap_or_default(),
                ))
            }
            "__goby_string_length" => {
                if args.len() != 1 {
                    return None;
                }
                let RuntimeValue::String(value) = &args[0] else {
                    return None;
                };
                let len = i64::try_from(value.chars().count()).ok()?;
                Some(RuntimeValue::Int(len))
            }
            "__goby_string_each_grapheme" => match args {
                [RuntimeValue::String(value)] => {
                    let method = self.find_handler_method_by_name("yield")?;
                    if method.method.params.len() != 2 {
                        return None;
                    };
                    let mut yielded_count: i64 = 0;
                    let mut state = RuntimeValue::Unit;
                    let mut ok = true;
                    let _ = for_each_extended_grapheme_span(value, |span| {
                        let grapheme = self.grapheme_from_span(value, span);
                        let resumed = match self.dispatch_handler_method_as_value_with_args_flow(
                            &method,
                            &[RuntimeValue::String(grapheme.to_string()), state.clone()],
                            evaluators,
                            depth + 1,
                        ) {
                            Out::Done(value) => value,
                            Out::Err(RuntimeError::Abort { .. }) => {
                                self.mark_runtime_abort();
                                ok = false;
                                return ControlFlow::Break;
                            }
                            Out::Suspend(_)
                            | Out::Escape(_)
                            | Out::Err(RuntimeError::Unsupported) => {
                                ok = false;
                                return ControlFlow::Break;
                            }
                        };
                        let RuntimeValue::Tuple(items) = resumed else {
                            ok = false;
                            return ControlFlow::Break;
                        };
                        if items.len() != 2 {
                            ok = false;
                            return ControlFlow::Break;
                        }
                        let RuntimeValue::Bool(keep_going) = items[0] else {
                            ok = false;
                            return ControlFlow::Break;
                        };
                        state = items[1].clone();
                        let Some(next_count) = yielded_count.checked_add(1) else {
                            ok = false;
                            return ControlFlow::Break;
                        };
                        yielded_count = next_count;
                        if !keep_going {
                            return ControlFlow::Break;
                        }
                        ControlFlow::Continue
                    });
                    if !ok {
                        return None;
                    }
                    Some(RuntimeValue::Int(yielded_count))
                }
                [RuntimeValue::String(value), initial_state] => {
                    let method = self.find_handler_method_by_name("yield")?;
                    if method.method.params.len() != 2 {
                        return None;
                    };
                    let mut state = initial_state.clone();
                    let mut ok = true;
                    let _ = for_each_extended_grapheme_span(value, |span| {
                        let grapheme = self.grapheme_from_span(value, span);
                        let resumed = match self.dispatch_handler_method_as_value_with_args_flow(
                            &method,
                            &[RuntimeValue::String(grapheme.to_string()), state.clone()],
                            evaluators,
                            depth + 1,
                        ) {
                            Out::Done(value) => value,
                            Out::Err(RuntimeError::Abort { .. }) => {
                                self.mark_runtime_abort();
                                ok = false;
                                return ControlFlow::Break;
                            }
                            Out::Suspend(_)
                            | Out::Escape(_)
                            | Out::Err(RuntimeError::Unsupported) => {
                                ok = false;
                                return ControlFlow::Break;
                            }
                        };
                        let RuntimeValue::Tuple(items) = resumed else {
                            ok = false;
                            return ControlFlow::Break;
                        };
                        if items.len() != 2 {
                            ok = false;
                            return ControlFlow::Break;
                        }
                        let RuntimeValue::Bool(keep_going) = items[0] else {
                            ok = false;
                            return ControlFlow::Break;
                        };
                        state = items[1].clone();
                        if !keep_going {
                            return ControlFlow::Break;
                        }
                        ControlFlow::Continue
                    });
                    if !ok {
                        return None;
                    }
                    Some(state)
                }
                _ => None,
            },
            "__goby_list_push_string" => {
                if args.len() != 2 {
                    return None;
                }
                let list = &args[0];
                let RuntimeValue::String(value) = &args[1] else {
                    return None;
                };
                match list.as_list() {
                    Some(items) => {
                        if let Some(mut next) = list.as_string_list() {
                            next.push(value.clone());
                            Some(RuntimeValue::list_from_strings(next))
                        } else if items.is_empty() {
                            Some(RuntimeValue::list_from_strings(vec![value.clone()]))
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
            _ => None,
        }
    }

    fn grapheme_from_span<'a>(&self, value: &'a str, span: GraphemeSpan) -> &'a str {
        &value[span.start..span.end]
    }

    pub(super) fn match_list_pattern_int(
        &self,
        items: &[ListPatternItem],
        tail: Option<&ListPatternTail>,
        values: &[RuntimeValue],
        arm_locals: &mut RuntimeLocals,
    ) -> bool {
        if values.len() < items.len() {
            return false;
        }
        if tail.is_none() && values.len() != items.len() {
            return false;
        }
        for (item, value) in items.iter().zip(values.iter()) {
            match item {
                ListPatternItem::IntLit(n) => {
                    if !matches!(value, RuntimeValue::Int(v) if n == v) {
                        return false;
                    }
                }
                ListPatternItem::Bind(name) => {
                    if name != "_" {
                        arm_locals.store(name, value.clone());
                    }
                }
                ListPatternItem::Wildcard => {}
                _ => return false,
            }
        }
        if let Some(ListPatternTail::Bind(name)) = tail
            && name != "_"
        {
            arm_locals.store(name, RuntimeValue::List(values[items.len()..].to_vec()));
        }
        true
    }

    pub(super) fn match_list_pattern_string(
        &self,
        items: &[ListPatternItem],
        tail: Option<&ListPatternTail>,
        values: &[RuntimeValue],
        arm_locals: &mut RuntimeLocals,
    ) -> bool {
        if values.len() < items.len() {
            return false;
        }
        if tail.is_none() && values.len() != items.len() {
            return false;
        }
        for (item, value) in items.iter().zip(values.iter()) {
            match item {
                ListPatternItem::StringLit(s) => {
                    if !matches!(value, RuntimeValue::String(v) if s == v) {
                        return false;
                    }
                }
                ListPatternItem::Bind(name) => {
                    if name != "_" {
                        arm_locals.store(name, value.clone());
                    }
                }
                ListPatternItem::Wildcard => {}
                _ => return false,
            }
        }
        if let Some(ListPatternTail::Bind(name)) = tail
            && name != "_"
        {
            arm_locals.store(name, RuntimeValue::List(values[items.len()..].to_vec()));
        }
        true
    }

    /// Evaluate an `Expr` node directly, without calling `to_str_repr()`.
    ///
    /// Returns `None` when the expression is not yet supported by the native
    /// evaluator (caller should fall back to the string path).
    pub(super) fn eval_expr_ast(
        &mut self,
        expr: &Expr,
        locals: &RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }

        match expr {
            Expr::Spanned { expr, .. } => {
                self.eval_expr_ast(expr, locals, callables, evaluators, depth)
            }
            Expr::IntLit(n) => Some(RuntimeValue::Int(*n)),
            Expr::BoolLit(b) => Some(RuntimeValue::Bool(*b)),
            Expr::StringLit(s) => Some(RuntimeValue::String(s.clone())),
            Expr::InterpolatedString(_) => {
                self.eval_expr_to_option(expr, locals, callables, evaluators, depth)
            }
            Expr::Var { name, .. } => locals
                .get(name)
                .or_else(|| self.eval_zero_arity_decl_value(name, evaluators, depth + 1)),
            Expr::Handler { clauses } => Some(RuntimeValue::Handler(
                self.inline_handler_from_clauses(clauses, locals, callables),
            )),
            Expr::Lambda { param, body } => Some(RuntimeValue::Callable(Box::new(
                IntCallable::AstLambda(Box::new(AstLambdaCallable {
                    parameter: param.clone(),
                    body: (*body.clone()),
                    captured_locals: locals.clone(),
                    captured_callables: Rc::clone(callables),
                })),
            ))),
            Expr::UnaryOp { op, expr } => {
                let inner = self.eval_expr_ast(expr, locals, callables, evaluators, depth + 1)?;
                match (op, inner) {
                    (UnaryOpKind::Not, RuntimeValue::Bool(value)) => {
                        Some(RuntimeValue::Bool(!value))
                    }
                    _ => None,
                }
            }
            Expr::BinOp { op, left, right } => {
                let lv = self.eval_expr_ast(left, locals, callables, evaluators, depth + 1);
                let lv = lv?;
                let rv = self.eval_expr_ast(right, locals, callables, evaluators, depth + 1);
                self.apply_binop_runtime_value(op.clone(), lv, rv?)
            }
            Expr::ListLit { .. } => {
                self.eval_expr_to_option(expr, locals, callables, evaluators, depth)
            }
            Expr::TupleLit(_) => {
                self.eval_expr_to_option(expr, locals, callables, evaluators, depth)
            }
            Expr::Call { callee, arg: _, .. } => {
                if let Expr::Var {
                    name: ctor_name, ..
                } = callee.as_ref()
                    && self.single_field_constructor_field(ctor_name).is_some()
                {
                    return self.eval_expr_to_option(expr, locals, callables, evaluators, depth);
                }

                if let Some((_fn_name, args)) = flatten_named_call(expr)
                    && args.len() > 1
                {
                    return self.eval_expr_to_option(expr, locals, callables, evaluators, depth);
                }

                if let Expr::Var { name: _, .. } = callee.as_ref() {
                    return self.eval_expr_to_option(expr, locals, callables, evaluators, depth);
                }
                if let Expr::Qualified { .. } = callee.as_ref() {
                    return self.eval_expr_to_option(expr, locals, callables, evaluators, depth);
                }
                None
            }
            Expr::Pipeline { .. } => {
                self.eval_expr_to_option(expr, locals, callables, evaluators, depth)
            }
            Expr::RecordConstruct { .. } => {
                self.eval_expr_to_option(expr, locals, callables, evaluators, depth)
            }
            Expr::Qualified {
                receiver, member, ..
            } => match locals.get(receiver) {
                Some(RuntimeValue::Record { fields, .. }) => fields.get(member).cloned(),
                Some(RuntimeValue::Tuple(items)) => {
                    let index = member.parse::<usize>().ok()?;
                    items.get(index).cloned()
                }
                None => {
                    if member.chars().all(|c| c.is_ascii_digit()) {
                        return None;
                    }
                    Some(RuntimeValue::String(member.clone()))
                }
                Some(_) => None,
            },
            Expr::Block(_) | Expr::Case { .. } => {
                self.eval_expr_to_option(expr, locals, callables, evaluators, depth)
            }
            Expr::If { .. } => self.eval_expr_to_option(expr, locals, callables, evaluators, depth),
            Expr::With { handler, body } => {
                let RuntimeValue::Handler(inline_handler) =
                    self.eval_expr_ast(handler, locals, callables, evaluators, depth + 1)?
                else {
                    return None;
                };
                self.active_inline_handler_stack.push(inline_handler);
                let body_expr = Expr::Block(body.clone());
                let result =
                    self.eval_expr_ast(&body_expr, locals, callables, evaluators, depth + 1);
                self.active_inline_handler_stack.pop();
                result
            }
            Expr::Resume { .. } => {
                self.eval_expr_to_option(expr, locals, callables, evaluators, depth)
            }
            Expr::MethodCall { method, args, .. } if method == "join" && args.len() == 2 => {
                let list_v =
                    self.eval_expr_ast(&args[0], locals, callables, evaluators, depth + 1)?;
                let sep_v =
                    self.eval_expr_ast(&args[1], locals, callables, evaluators, depth + 1)?;
                match (list_v, sep_v) {
                    (list_value, RuntimeValue::String(sep))
                        if list_value.as_string_list().is_some() =>
                    {
                        let parts = list_value.as_string_list().expect("checked above");
                        Some(RuntimeValue::String(parts.join(&sep)))
                    }
                    (RuntimeValue::List(parts), RuntimeValue::String(_sep)) if parts.is_empty() => {
                        Some(RuntimeValue::String(String::new()))
                    }
                    _ => None,
                }
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
                ..
            } if args.len() == 1 => {
                let arg_val =
                    self.eval_expr_ast(&args[0], locals, callables, evaluators, depth + 1)?;
                let out = self.apply_receiver_method_value_call_out(
                    receiver,
                    method,
                    arg_val,
                    evaluators,
                    depth + 1,
                );
                self.complete_value_out(out, evaluators)
            }
            Expr::MethodCall { .. } => None,
            Expr::ListIndex { list, index } => {
                let list_val =
                    self.eval_expr_ast(list, locals, callables, evaluators, depth + 1)?;
                let index_val =
                    self.eval_expr_ast(index, locals, callables, evaluators, depth + 1)?;
                let RuntimeValue::Int(i) = index_val else {
                    return None;
                };
                match list_val {
                    RuntimeValue::List(items) => {
                        // Use `i >= items.len() as i64` rather than `i as usize >= items.len()`
                        // to avoid silent truncation on 32-bit WASM targets where usize is 32-bit.
                        if i < 0 || i >= items.len() as i64 {
                            self.mark_runtime_abort();
                            return None;
                        }
                        Some(items[i as usize].clone())
                    }
                    _ => {
                        // The type system should have rejected a non-list receiver,
                        // but if we reach here at runtime the program is in an invalid
                        // state — abort consistently with OOB handling.
                        self.mark_runtime_abort();
                        None
                    }
                }
            }
        }
    }

    pub(super) fn select_case_arm(
        &mut self,
        scrutinee_val: &RuntimeValue,
        arms: &[goby_core::CaseArm],
        locals: &RuntimeLocals,
    ) -> Option<(Expr, RuntimeLocals)> {
        for arm in arms {
            let mut arm_locals = locals.clone();
            let matched = match (&arm.pattern, scrutinee_val) {
                (CasePattern::Wildcard, _) => true,
                (CasePattern::IntLit(n), RuntimeValue::Int(v)) => n == v,
                (CasePattern::StringLit(s), RuntimeValue::String(v)) => s == v,
                (CasePattern::BoolLit(b), RuntimeValue::Bool(v)) => b == v,
                (CasePattern::EmptyList, RuntimeValue::List(values)) => values.is_empty(),
                (CasePattern::ListPattern { items, tail }, RuntimeValue::List(values)) => {
                    self.match_list_pattern_int(items, tail.as_ref(), values, &mut arm_locals)
                        || self.match_list_pattern_string(
                            items,
                            tail.as_ref(),
                            values,
                            &mut arm_locals,
                        )
                }
                _ => false,
            };
            if matched {
                return Some(((*arm.body).clone(), arm_locals));
            }
        }
        None
    }
}
