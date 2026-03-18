use std::collections::HashMap;

use crate::runtime_eval::{AstLambdaCallable, IntCallable};
use crate::runtime_flow::{
    DirectCallHead, Out, ResolvedHandlerMethod, RuntimeError, RuntimeEvaluators,
};
use crate::runtime_value::{RuntimeLocals, RuntimeValue, runtime_value_eq};
use crate::{MAX_EVAL_DEPTH, RuntimeOutputResolver};

impl<'m> RuntimeOutputResolver<'m> {
    pub(crate) fn find_handler_method_for_effect(
        &self,
        effect_name: &str,
        method_name: &str,
    ) -> Option<ResolvedHandlerMethod> {
        for inline in self.active_inline_handler_stack.iter().rev() {
            if let Some(method) = inline.methods.iter().find(|m| {
                m.method.name == method_name && m.effect_name.as_deref() == Some(effect_name)
            }) {
                return Some(ResolvedHandlerMethod {
                    method: method.method.clone(),
                    with_id: inline.with_id,
                });
            }
        }
        None
    }

    pub(crate) fn single_field_constructor_field(&self, ctor_name: &str) -> Option<String> {
        for ty_decl in &self.current_runtime_module().type_declarations {
            if let goby_core::TypeDeclaration::Record {
                constructor,
                fields,
                ..
            } = ty_decl
                && constructor == ctor_name
                && fields.len() == 1
            {
                return Some(fields[0].name.clone());
            }
        }
        None
    }

    pub(crate) fn eval_zero_arity_decl_value(
        &mut self,
        name: &str,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        if let Some(value) = self.eval_decl_as_value_with_args_ast(name, &[], evaluators, depth + 1)
        {
            return Some(value);
        }

        let decl = self.resolve_imported_runtime_decl(&DirectCallHead::Bare(name.to_string()))?;
        if !decl.params.is_empty() {
            return None;
        }
        let fn_locals = RuntimeLocals::default();
        let fn_callables = HashMap::new();
        if let Some(owner_module) = &decl.owner_module {
            self.current_module_stack.push(owner_module.clone());
        }
        self.push_runtime_decl_context(&decl);
        let result = self.eval_stmts(
            &decl.stmts,
            fn_locals,
            fn_callables,
            evaluators,
            depth + 1,
            crate::runtime_flow::FinishKind::Block,
        );
        self.pop_runtime_decl_context();
        if decl.owner_module.is_some() {
            self.current_module_stack.pop();
        }
        match result {
            Out::Done((value, _)) => Some(value.unwrap_or(RuntimeValue::Unit)),
            Out::Suspend(_) | Out::Escape(_) | Out::Err(_) => None,
        }
    }

    pub(crate) fn eval_decl_as_value_with_args_ast(
        &mut self,
        fn_name: &str,
        args: &[RuntimeValue],
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let decl = self.resolve_local_runtime_decl(fn_name)?;
        let params = decl.params.clone();
        let stmts = decl.stmts.clone();
        let accepts_unit_arg_as_zero_arity =
            params.is_empty() && matches!(args, [RuntimeValue::Unit]);
        if params.len() != args.len() && !accepts_unit_arg_as_zero_arity {
            return None;
        }
        let mut fn_locals = RuntimeLocals::default();
        if !accepts_unit_arg_as_zero_arity {
            for (param, arg) in params.iter().zip(args.iter()) {
                fn_locals.store(param, arg.clone());
            }
        }
        let fn_callables = HashMap::new();
        if let Some(owner_module) = &decl.owner_module {
            self.current_module_stack.push(owner_module.clone());
        }
        self.push_runtime_decl_context(&decl);
        let result = self.eval_expr_to_option(
            &goby_core::Expr::Block(stmts),
            &fn_locals,
            &fn_callables,
            evaluators,
            depth + 1,
        );
        self.pop_runtime_decl_context();
        if decl.owner_module.is_some() {
            self.current_module_stack.pop();
        }
        result
    }

    pub(crate) fn eval_decl_as_value_with_args_out(
        &mut self,
        fn_name: &str,
        args: &[RuntimeValue],
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return Out::Err(RuntimeError::Unsupported);
        }
        let Some(decl) = self.resolve_local_runtime_decl(fn_name) else {
            return Out::Err(RuntimeError::Unsupported);
        };
        let params = decl.params.clone();
        let stmts = decl.stmts.clone();
        let accepts_unit_arg_as_zero_arity =
            params.is_empty() && matches!(args, [RuntimeValue::Unit]);
        if params.len() != args.len() && !accepts_unit_arg_as_zero_arity {
            return Out::Err(RuntimeError::Unsupported);
        }
        let mut fn_locals = RuntimeLocals::default();
        if !accepts_unit_arg_as_zero_arity {
            for (param, arg) in params.iter().zip(args.iter()) {
                fn_locals.store(param, arg.clone());
            }
        }
        let fn_callables = HashMap::new();
        if let Some(owner_module) = &decl.owner_module {
            self.current_module_stack.push(owner_module.clone());
        }
        self.push_runtime_decl_context(&decl);
        let result = self.eval_stmts(
            &stmts,
            fn_locals,
            fn_callables,
            evaluators,
            depth + 1,
            crate::runtime_flow::FinishKind::Block,
        );
        self.pop_runtime_decl_context();
        if decl.owner_module.is_some() {
            self.current_module_stack.pop();
        }
        match result {
            Out::Done((value, _locals)) => Out::Done(value.unwrap_or(RuntimeValue::Unit)),
            Out::Suspend(cont) => Out::Suspend(cont),
            Out::Escape(escape) => Out::Escape(escape),
            Out::Err(e) => Out::Err(e),
        }
    }

    pub(crate) fn apply_named_value_call_ast(
        &mut self,
        fn_name: &str,
        arg_values: &[RuntimeValue],
        _locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        if fn_name.starts_with("__goby_") {
            return self
                .apply_runtime_intrinsic_ast(fn_name, arg_values, evaluators, depth + 1)
                .map_or(Out::Err(RuntimeError::Unsupported), Out::Done);
        }
        if arg_values.len() > 1
            && let Some(method) = self.find_handler_method_by_name(fn_name)
        {
            return self.dispatch_handler_method_as_value_with_args_flow(
                &method,
                arg_values,
                evaluators,
                depth + 1,
            );
        }
        if let Some(value) =
            self.eval_decl_as_value_with_args_ast(fn_name, arg_values, evaluators, depth + 1)
        {
            return Out::Done(value);
        }
        if arg_values.len() != 1 {
            return Out::Err(RuntimeError::Unsupported);
        }
        let arg_val = arg_values[0].clone();
        if let Some(handler) = self.resolve_bare_effect_handler(fn_name) {
            return self.dispatch_effect_handler_as_value_flow(
                handler,
                arg_val.clone(),
                evaluators,
                depth + 1,
            );
        }
        if fn_name == "__goby_env_fetch_env_var" {
            return self
                .apply_runtime_intrinsic_ast(
                    "__goby_env_fetch_env_var",
                    &[arg_val],
                    evaluators,
                    depth + 1,
                )
                .map_or(Out::Err(RuntimeError::Unsupported), Out::Done);
        }
        if fn_name == "__goby_string_length" {
            return self
                .apply_runtime_intrinsic_ast(
                    "__goby_string_length",
                    &[arg_val],
                    evaluators,
                    depth + 1,
                )
                .map_or(Out::Err(RuntimeError::Unsupported), Out::Done);
        }
        if fn_name == "__goby_string_each_grapheme" {
            return self
                .apply_runtime_intrinsic_ast(
                    "__goby_string_each_grapheme",
                    &[arg_val],
                    evaluators,
                    depth + 1,
                )
                .map_or(Out::Err(RuntimeError::Unsupported), Out::Done);
        }
        if self.unique_effect_name_for_operation(fn_name).is_none()
            && let Some(method) = self.find_handler_method_by_name(fn_name)
        {
            return self.dispatch_handler_method_as_value_flow(
                &method,
                arg_val,
                evaluators,
                depth + 1,
            );
        }
        if self.unique_effect_name_for_operation(fn_name).is_some() {
            self.set_unhandled_effect_error(fn_name);
            return Out::Err(RuntimeError::Unsupported);
        }
        if let RuntimeValue::Int(arg_int) = arg_val.clone() {
            if let Some(callable) = callables.get(fn_name).cloned() {
                match callable {
                    IntCallable::AstLambda(callable) => {
                        let AstLambdaCallable {
                            parameter,
                            body,
                            captured_locals,
                            captured_callables,
                        } = *callable;
                        let mut lambda_locals = captured_locals;
                        lambda_locals.store(&parameter, RuntimeValue::Int(arg_int));
                        if let Some(RuntimeValue::Int(value)) = self.eval_expr_to_option(
                            &body,
                            &lambda_locals,
                            &captured_callables,
                            evaluators,
                            depth + 1,
                        ) {
                            return Out::Done(RuntimeValue::Int(value));
                        }
                    }
                    other => {
                        return evaluators
                            .int
                            .eval_callable(&other, arg_int, callables)
                            .map_or(Out::Err(RuntimeError::Unsupported), |value| {
                                Out::Done(RuntimeValue::Int(value))
                            });
                    }
                }
            }
            if let Some(function) = evaluators.int.functions.get(fn_name) {
                return evaluators
                    .int
                    .eval_function(function, Some(arg_int))
                    .map_or(Out::Err(RuntimeError::Unsupported), |value| {
                        Out::Done(RuntimeValue::Int(value))
                    });
            }
        } else if let RuntimeValue::ListInt(arg_list) = arg_val
            && let Some(function) = evaluators.list.functions.get(fn_name)
        {
            return evaluators
                .list
                .eval_function(function, Some(arg_list))
                .map_or(Out::Err(RuntimeError::Unsupported), |values| {
                    Out::Done(RuntimeValue::ListInt(values))
                });
        }
        Out::Err(RuntimeError::Unsupported)
    }

    pub(crate) fn apply_named_value_call_out(
        &mut self,
        fn_name: &str,
        arg_value: RuntimeValue,
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        if let Some(callable) = callables.get(fn_name) {
            return self.eval_callable_value(
                callable,
                arg_value,
                locals,
                callables,
                evaluators,
                depth + 1,
            );
        }
        if let Some(handler) = self.resolve_bare_effect_handler(fn_name) {
            return self.dispatch_effect_handler_as_value_flow(
                handler,
                arg_value,
                evaluators,
                depth + 1,
            );
        } else if self.unique_effect_name_for_operation(fn_name).is_none()
            && let Some(method) = self.find_handler_method_by_name(fn_name)
        {
            return self.dispatch_handler_method_as_value_flow(
                &method,
                arg_value,
                evaluators,
                depth + 1,
            );
        }
        if fn_name.starts_with("__goby_")
            && let Some(value) = self.apply_runtime_intrinsic_ast(
                fn_name,
                std::slice::from_ref(&arg_value),
                evaluators,
                depth + 1,
            )
        {
            return Out::Done(value);
        }
        match self.eval_decl_as_value_with_args_out(
            fn_name,
            std::slice::from_ref(&arg_value),
            evaluators,
            depth + 1,
        ) {
            Out::Err(RuntimeError::Unsupported) => {}
            other => return other,
        }
        self.apply_named_value_call_ast(fn_name, &[arg_value], locals, callables, evaluators, depth)
    }

    pub(crate) fn apply_named_value_call_args_out(
        &mut self,
        fn_name: &str,
        arg_values: &[RuntimeValue],
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        debug_assert!(
            arg_values.len() >= 2,
            "use apply_named_value_call_out for single-arg calls"
        );
        if let Some(method) = self.find_handler_method_by_name(fn_name) {
            return self.dispatch_handler_method_as_value_with_args_flow(
                &method,
                arg_values,
                evaluators,
                depth + 1,
            );
        }
        if fn_name.starts_with("__goby_")
            && let Some(value) =
                self.apply_runtime_intrinsic_ast(fn_name, arg_values, evaluators, depth + 1)
        {
            return Out::Done(value);
        }
        match self.eval_decl_as_value_with_args_out(fn_name, arg_values, evaluators, depth + 1) {
            Out::Err(RuntimeError::Unsupported) => {}
            other => return other,
        }
        self.apply_named_value_call_ast(fn_name, arg_values, locals, callables, evaluators, depth)
    }

    pub(crate) fn apply_binop_runtime_value(
        &self,
        op: goby_core::BinOpKind,
        lv: RuntimeValue,
        rv: RuntimeValue,
    ) -> Option<RuntimeValue> {
        if matches!(op, goby_core::BinOpKind::Eq) {
            return Some(RuntimeValue::Bool(runtime_value_eq(&lv, &rv)));
        }
        match (lv, rv) {
            (RuntimeValue::Bool(l), RuntimeValue::Bool(r)) => match op {
                goby_core::BinOpKind::And => Some(RuntimeValue::Bool(l && r)),
                _ => None,
            },
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => match op {
                goby_core::BinOpKind::Add => l.checked_add(r).map(RuntimeValue::Int),
                goby_core::BinOpKind::Sub => l.checked_sub(r).map(RuntimeValue::Int),
                goby_core::BinOpKind::Mul => l.checked_mul(r).map(RuntimeValue::Int),
                goby_core::BinOpKind::Div => (r != 0).then_some(RuntimeValue::Int(l / r)),
                goby_core::BinOpKind::Mod => (r != 0).then_some(RuntimeValue::Int(l % r)),
                goby_core::BinOpKind::Lt => Some(RuntimeValue::Bool(l < r)),
                goby_core::BinOpKind::Gt => Some(RuntimeValue::Bool(l > r)),
                goby_core::BinOpKind::Le => Some(RuntimeValue::Bool(l <= r)),
                goby_core::BinOpKind::Ge => Some(RuntimeValue::Bool(l >= r)),
                _ => None,
            },
            _ => None,
        }
    }
}
