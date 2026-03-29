use super::*;
use crate::runtime_flow::RcCallables;
use std::rc::Rc;

impl<'m> RuntimeOutputResolver<'m> {
    pub(super) fn build_runtime_list(&self, values: Vec<RuntimeValue>) -> Out<RuntimeValue> {
        let mut ints = Vec::with_capacity(values.len());
        let mut strings = Vec::with_capacity(values.len());
        let mut kind: Option<&'static str> = None;
        for value in values {
            match value {
                RuntimeValue::Int(n) => {
                    if kind == Some("string") {
                        return Out::Err(RuntimeError::Unsupported);
                    }
                    kind = Some("int");
                    ints.push(n);
                }
                RuntimeValue::String(text) => {
                    if kind == Some("int") {
                        return Out::Err(RuntimeError::Unsupported);
                    }
                    kind = Some("string");
                    strings.push(text);
                }
                _ => return Out::Err(RuntimeError::Unsupported),
            }
        }
        match kind {
            Some("string") => Out::Done(RuntimeValue::ListString(strings)),
            _ => Out::Done(RuntimeValue::ListInt(ints)),
        }
    }

    pub(super) fn execute_unit_call(
        &mut self,
        expr: &str,
        caller_locals: &RuntimeLocals,
        caller_callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        let (callee, arg_expr) = match parse_call(expr) {
            Some((callee, arg_expr)) => (callee, Some(arg_expr)),
            None if is_identifier(expr) => (expr.trim(), None),
            None => return None,
        };

        let function = evaluators.unit.get(callee)?;
        let mut function_locals = RuntimeLocals::default();
        let mut function_callables_map = HashMap::new();

        if let Some(parameter) = function.parameter.as_deref() {
            let arg_expr = arg_expr?;
            if let Some(callable) = parse_int_callable(arg_expr) {
                function_callables_map.insert(parameter.to_string(), callable);
            } else if let Some(RuntimeValue::Int(value)) =
                self.eval_value_with_context(arg_expr, caller_locals, caller_callables, evaluators)
            {
                function_locals.store(parameter, RuntimeValue::Int(value));
            } else {
                return None;
            }
        }
        let function_callables = Rc::new(function_callables_map);

        if let Some(stmts) = function.parsed_stmts.as_deref() {
            for (i, stmt) in stmts.iter().enumerate() {
                match self.execute_unit_ast_stmt(
                    stmt,
                    &mut function_locals,
                    &function_callables,
                    evaluators,
                    i + 1,
                ) {
                    Out::Done(()) => {}
                    Out::Suspend(_) | Out::Escape(_) | Out::Err(_) => return None,
                }
            }
        } else {
            for statement in statements(function.body.as_deref()?) {
                match statement {
                    Statement::Binding { name, expr } | Statement::MutBinding { name, expr } => {
                        let value = self.eval_value_with_context(
                            expr,
                            &function_locals,
                            &function_callables,
                            evaluators,
                        )?;
                        function_locals.store(name, value);
                    }
                    Statement::Assign { name, expr } => {
                        function_locals.get(name)?;
                        let value = self.eval_value_with_context(
                            expr,
                            &function_locals,
                            &function_callables,
                            evaluators,
                        )?;
                        function_locals.store(name, value);
                    }
                    Statement::Print(print_expr) => {
                        let value = self.eval_value_with_context(
                            print_expr,
                            &function_locals,
                            &function_callables,
                            evaluators,
                        )?;
                        self.embedded_effect_runtime
                            .emit_output_text(value.to_output_text());
                    }
                    Statement::Expr(inner_expr) => {
                        self.execute_unit_call(
                            inner_expr,
                            &function_locals,
                            &function_callables,
                            evaluators,
                        )?;
                    }
                }
            }
        }

        Some(())
    }

    pub(super) fn execute_unit_call_out(
        &mut self,
        expr: &str,
        caller_locals: &RuntimeLocals,
        caller_callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Out<()> {
        match self.execute_unit_call(expr, caller_locals, caller_callables, evaluators) {
            Some(()) => Out::Done(()),
            None if self.runtime_error_is_abort_marker() => Out::Err(RuntimeError::Abort {
                kind: "aborted".into(),
            }),
            None => Out::Err(RuntimeError::Unsupported),
        }
    }

    pub(super) fn execute_unit_call_ast(
        &mut self,
        fn_name: &str,
        arg_val: RuntimeValue,
        caller_locals: &RuntimeLocals,
        caller_callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        let function = evaluators.unit.get(fn_name)?;
        let mut function_locals = RuntimeLocals::default();
        let function_callables = Rc::new(HashMap::new());

        if let Some(stmts) = function.parsed_stmts.as_deref() {
            if let Some(parameter) = function.parameter.as_deref() {
                function_locals.store(parameter, arg_val);
            }
            match self.eval_stmts(
                stmts,
                function_locals,
                function_callables,
                evaluators,
                depth + 1,
                FinishKind::Block,
            ) {
                Out::Done(_) => Some(()),
                Out::Suspend(_) | Out::Escape(_) | Out::Err(_) => None,
            }
        } else {
            let call_expr = if function.parameter.is_some() {
                format!("{} {}", fn_name, arg_val.to_expression_text())
            } else {
                fn_name.to_string()
            };
            self.execute_unit_call(&call_expr, caller_locals, caller_callables, evaluators)
        }
    }
}
