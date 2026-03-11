use super::*;

impl<'m> RuntimeOutputResolver<'m> {
    pub(super) fn resolve(
        module: &'m Module,
        body: &str,
        parsed_stmts: Option<&[Stmt]>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        execution_mode: lower::EffectExecutionMode,
        stdin_seed: Option<String>,
    ) -> Option<String> {
        let runtime_imports = load_runtime_import_context(module);
        let mut resolver = Self {
            locals: RuntimeLocals::default(),
            module,
            runtime_imports,
            embedded_effect_runtime: EmbeddedEffectRuntime::new(stdin_seed),
            current_module_stack: Vec::new(),
            current_decl_stack: Vec::new(),
            active_inline_handler_stack: Vec::new(),
            resume_tokens: Vec::new(),
            optimized_resume_tokens: Vec::new(),
            pending_caller_cont_stack: Vec::new(),
            runtime_error: None,
            next_with_id: 1,
            execution_mode,
        };

        if let Some(stmts) = parsed_stmts {
            // AST-based path (preferred when parsed_body is available).
            // Keep statement-sequence continuation wiring so resume() can replay
            // remaining top-level unit statements before control returns.
            if resolver
                .execute_ingest_ast_stmt_sequence(stmts, evaluators)
                .is_none()
                && resolver.runtime_error.is_none()
            {
                return None;
            }
        } else {
            // String-based fallback path
            for statement in statements(body) {
                if resolver.ingest_statement(statement, evaluators).is_none() {
                    if resolver.runtime_error.is_some() {
                        break;
                    }
                    return None;
                }
            }
        }

        if resolver.runtime_error_is_abort_marker() {
            return if resolver.embedded_effect_runtime.outputs_are_empty() {
                None
            } else {
                Some(resolver.embedded_effect_runtime.concat_outputs())
            };
        }

        if let Some(err) = &resolver.runtime_error {
            let err_line = format!("runtime error: {}", err);
            if resolver.embedded_effect_runtime.outputs_are_empty() {
                return Some(err_line);
            }
            let mut out = resolver.embedded_effect_runtime.concat_outputs();
            if !out.ends_with('\n') {
                out.push('\n');
            }
            out.push_str(&err_line);
            return Some(out);
        }

        if resolver.embedded_effect_runtime.outputs_are_empty() {
            None
        } else {
            Some(resolver.embedded_effect_runtime.concat_outputs())
        }
    }

    #[allow(dead_code)]
    pub(super) fn ingest_ast_statement(
        &mut self,
        stmt: &Stmt,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        match stmt {
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                // Propagate None so the caller can fall back to the string path
                // rather than silently dropping the binding.
                let runtime_val = self
                    .eval_expr_to_option(
                        value,
                        &self.locals.clone(),
                        &HashMap::new(),
                        evaluators,
                        1,
                    )
                    .or_else(|| {
                        let repr = value.to_str_repr()?;
                        self.eval_value_with_context(
                            &repr,
                            &self.locals.clone(),
                            &HashMap::new(),
                            evaluators,
                        )
                    })?;
                self.locals.store(name, runtime_val);
                Some(())
            }
            Stmt::Assign { name, value } => {
                self.locals.get(name)?;
                let runtime_val = self
                    .eval_expr_to_option(
                        value,
                        &self.locals.clone(),
                        &HashMap::new(),
                        evaluators,
                        1,
                    )
                    .or_else(|| {
                        let repr = value.to_str_repr()?;
                        self.eval_value_with_context(
                            &repr,
                            &self.locals.clone(),
                            &HashMap::new(),
                            evaluators,
                        )
                    })?;
                self.locals.store(name, runtime_val);
                Some(())
            }
            Stmt::Expr(expr) => {
                let mut locals = self.locals.clone();
                let mut callables = HashMap::new();
                let outputs_before = self.embedded_effect_runtime.output_len();
                match self.execute_unit_expr_ast(expr, &mut locals, &mut callables, evaluators, 0) {
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
            Statement::MutBinding { name, expr } => self.bind_local(name, expr, evaluators),
            Statement::Assign { name, expr } => {
                self.locals.get(name)?;
                self.bind_local(name, expr, evaluators)
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

        self.execute_unit_call(expr, &RuntimeLocals::default(), &HashMap::new(), evaluators)
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
        let callables = HashMap::new();
        let locals = self.locals.clone();
        self.eval_value_with_context(expr, &locals, &callables, evaluators)
    }

    pub(super) fn eval_value_with_context(
        &mut self,
        expr: &str,
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
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

        if let Some(text) = eval_string_expr(expr, locals.string_values()) {
            return Some(RuntimeValue::String(text));
        }

        if let Some(value) = evaluators
            .int
            .eval_expr(expr, locals.int_values(), callables)
        {
            return Some(RuntimeValue::Int(value));
        }

        if let Some(values) = evaluators.list.eval_expr(expr, locals.list_int_values()) {
            return Some(RuntimeValue::ListInt(values));
        }

        None
    }

    pub(super) fn apply_pipeline(
        &mut self,
        callee: &str,
        value: RuntimeValue,
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
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
        callables: &HashMap<String, IntCallable>,
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
                    for grapheme in value.graphemes(true) {
                        let resumed = match self.dispatch_handler_method_as_value_with_args_flow(
                            &method,
                            &[RuntimeValue::String(grapheme.to_string()), state],
                            evaluators,
                            depth + 1,
                        ) {
                            Out::Done(value) => value,
                            Out::Err(RuntimeError::Abort { .. }) => {
                                self.mark_runtime_abort();
                                return None;
                            }
                            Out::Suspend(_)
                            | Out::Escape(_)
                            | Out::Err(RuntimeError::Unsupported) => return None,
                        };
                        let RuntimeValue::Tuple(items) = resumed else {
                            return None;
                        };
                        if items.len() != 2 {
                            return None;
                        }
                        let RuntimeValue::Bool(keep_going) = items[0] else {
                            return None;
                        };
                        state = items[1].clone();
                        yielded_count = yielded_count.checked_add(1)?;
                        if !keep_going {
                            break;
                        }
                    }
                    Some(RuntimeValue::Int(yielded_count))
                }
                [RuntimeValue::String(value), initial_state] => {
                    let method = self.find_handler_method_by_name("yield")?;
                    if method.method.params.len() != 2 {
                        return None;
                    };
                    let mut state = initial_state.clone();
                    for grapheme in value.graphemes(true) {
                        let resumed = match self.dispatch_handler_method_as_value_with_args_flow(
                            &method,
                            &[RuntimeValue::String(grapheme.to_string()), state],
                            evaluators,
                            depth + 1,
                        ) {
                            Out::Done(value) => value,
                            Out::Err(RuntimeError::Abort { .. }) => {
                                self.mark_runtime_abort();
                                return None;
                            }
                            Out::Suspend(_)
                            | Out::Escape(_)
                            | Out::Err(RuntimeError::Unsupported) => return None,
                        };
                        let RuntimeValue::Tuple(items) = resumed else {
                            return None;
                        };
                        if items.len() != 2 {
                            return None;
                        }
                        let RuntimeValue::Bool(keep_going) = items[0] else {
                            return None;
                        };
                        state = items[1].clone();
                        if !keep_going {
                            break;
                        }
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
                match list {
                    RuntimeValue::ListString(items) => {
                        let mut next = items.clone();
                        next.push(value.clone());
                        Some(RuntimeValue::ListString(next))
                    }
                    RuntimeValue::ListInt(items) if items.is_empty() => {
                        Some(RuntimeValue::ListString(vec![value.clone()]))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub(super) fn match_list_pattern_int(
        &self,
        items: &[ListPatternItem],
        tail: Option<&ListPatternTail>,
        values: &[i64],
        arm_locals: &mut RuntimeLocals,
    ) -> bool {
        if values.len() < items.len() {
            return false;
        }
        for (item, value) in items.iter().zip(values.iter()) {
            match item {
                ListPatternItem::IntLit(n) => {
                    if *n != *value {
                        return false;
                    }
                }
                ListPatternItem::Bind(name) => {
                    if name != "_" {
                        arm_locals.store(name, RuntimeValue::Int(*value));
                    }
                }
                ListPatternItem::Wildcard => {}
                _ => return false,
            }
        }
        if let Some(ListPatternTail::Bind(name)) = tail
            && name != "_"
        {
            arm_locals.store(name, RuntimeValue::ListInt(values[items.len()..].to_vec()));
        }
        true
    }

    pub(super) fn match_list_pattern_string(
        &self,
        items: &[ListPatternItem],
        tail: Option<&ListPatternTail>,
        values: &[String],
        arm_locals: &mut RuntimeLocals,
    ) -> bool {
        if values.len() < items.len() {
            return false;
        }
        for (item, value) in items.iter().zip(values.iter()) {
            match item {
                ListPatternItem::StringLit(s) => {
                    if s != value {
                        return false;
                    }
                }
                ListPatternItem::Bind(name) => {
                    if name != "_" {
                        arm_locals.store(name, RuntimeValue::String(value.clone()));
                    }
                }
                ListPatternItem::Wildcard => {}
                _ => return false,
            }
        }
        if let Some(ListPatternTail::Bind(name)) = tail
            && name != "_"
        {
            arm_locals.store(
                name,
                RuntimeValue::ListString(values[items.len()..].to_vec()),
            );
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
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }

        match expr {
            Expr::IntLit(n) => Some(RuntimeValue::Int(*n)),
            Expr::BoolLit(b) => Some(RuntimeValue::Bool(*b)),
            Expr::StringLit(s) => Some(RuntimeValue::String(s.clone())),
            Expr::InterpolatedString(_) => {
                self.eval_expr_to_option(expr, locals, callables, evaluators, depth)
            }
            Expr::Var(name) => locals
                .get(name)
                .or_else(|| self.eval_zero_arity_decl_value(name, evaluators, depth + 1)),
            Expr::Handler { clauses } => Some(RuntimeValue::Handler(
                self.inline_handler_from_clauses(clauses, locals, callables),
            )),
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
            Expr::Call { callee, arg: _ } => {
                if let Expr::Var(ctor_name) = callee.as_ref()
                    && self.single_field_constructor_field(ctor_name).is_some()
                {
                    return self.eval_expr_to_option(expr, locals, callables, evaluators, depth);
                }

                if let Some((_fn_name, args)) = flatten_named_call(expr)
                    && args.len() > 1
                {
                    return self.eval_expr_to_option(expr, locals, callables, evaluators, depth);
                }

                if let Expr::Var(_) = callee.as_ref() {
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
            Expr::Qualified { receiver, member } => match locals.get(receiver) {
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
            Expr::MethodCall {
                receiver,
                method,
                args,
            } if method == "split" && receiver == "string" && args.len() == 2 => {
                let sv = self.eval_expr_ast(&args[0], locals, callables, evaluators, depth + 1)?;
                let dv = self.eval_expr_ast(&args[1], locals, callables, evaluators, depth + 1)?;
                match (sv, dv) {
                    (RuntimeValue::String(s), RuntimeValue::String(delim)) => {
                        let parts: Vec<String> = if s.is_empty() {
                            Vec::new()
                        } else {
                            s.split(delim.as_str()).map(|p| p.to_string()).collect()
                        };
                        Some(RuntimeValue::ListString(parts))
                    }
                    _ => None,
                }
            }
            Expr::MethodCall { method, args, .. } if method == "join" && args.len() == 2 => {
                let list_v =
                    self.eval_expr_ast(&args[0], locals, callables, evaluators, depth + 1)?;
                let sep_v =
                    self.eval_expr_ast(&args[1], locals, callables, evaluators, depth + 1)?;
                match (list_v, sep_v) {
                    (RuntimeValue::ListString(parts), RuntimeValue::String(sep)) => {
                        Some(RuntimeValue::String(parts.join(&sep)))
                    }
                    _ => None,
                }
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
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
            Expr::Lambda { .. } | Expr::MethodCall { .. } => None,
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
                (CasePattern::EmptyList, RuntimeValue::ListInt(values)) => values.is_empty(),
                (CasePattern::EmptyList, RuntimeValue::ListString(values)) => values.is_empty(),
                (CasePattern::ListPattern { items, tail }, RuntimeValue::ListInt(values)) => {
                    self.match_list_pattern_int(items, tail.as_ref(), values, &mut arm_locals)
                }
                (CasePattern::ListPattern { items, tail }, RuntimeValue::ListString(values)) => {
                    self.match_list_pattern_string(items, tail.as_ref(), values, &mut arm_locals)
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
