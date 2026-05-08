use std::collections::HashMap;
use std::rc::Rc;

use crate::runtime_eval::{AstLambdaCallable, IntCallable};
use crate::runtime_flow::{
    DirectCallHead, Out, RcCallables, ResolvedHandlerMethod, RuntimeError, RuntimeEvaluators,
};
use crate::runtime_value::{RuntimeLocals, RuntimeValue, runtime_value_eq};
use crate::{MAX_EVAL_DEPTH, RuntimeOutputResolver};

impl<'m> RuntimeOutputResolver<'m> {
    pub(crate) fn apply_imported_runtime_intrinsic(
        &mut self,
        module_path: &str,
        member: &str,
        args: &[RuntimeValue],
        _evaluators: &RuntimeEvaluators<'_, '_>,
        _depth: usize,
    ) -> Option<RuntimeValue> {
        match (module_path, member, args) {
            ("goby/string", "graphemes", [RuntimeValue::String(value)]) => {
                Some(RuntimeValue::list_from_strings(
                    crate::grapheme_semantics::collect_extended_graphemes(value),
                ))
            }
            _ => None,
        }
    }

    pub(super) fn apply_callable_args_out(
        &mut self,
        callable: &IntCallable,
        arg_values: &[RuntimeValue],
        locals: &RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        let mut current = RuntimeValue::Callable(Box::new(callable.clone()));
        for (idx, arg_value) in arg_values.iter().enumerate() {
            let RuntimeValue::Callable(next_callable) = current else {
                return Out::Err(RuntimeError::Unsupported);
            };
            current = match self.eval_callable_value(
                &next_callable,
                arg_value.clone(),
                locals,
                callables,
                evaluators,
                depth + 1,
            ) {
                Out::Done(value) => value,
                Out::Suspend(cont) => return Out::Suspend(cont),
                Out::Escape(escape) => return Out::Escape(escape),
                Out::Err(err) => return Out::Err(err),
            };
            if idx + 1 < arg_values.len() && !matches!(current, RuntimeValue::Callable(_)) {
                return Out::Err(RuntimeError::Unsupported);
            }
        }
        Out::Done(current)
    }

    pub(crate) fn eval_runtime_decl_body_out(
        &mut self,
        decl: &crate::runtime_flow::RuntimeDeclInfo,
        fn_locals: RuntimeLocals,
        fn_callables: RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return Out::Err(RuntimeError::Unsupported);
        }
        if let Some(owner_module) = &decl.owner_module {
            self.current_module_stack.push(owner_module.clone());
        }
        self.push_runtime_decl_context(decl);
        let result = self.eval_expr(
            &goby_core::Expr::Block(decl.stmts.clone()),
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
        let fn_callables = Rc::new(HashMap::new());
        let out =
            self.eval_runtime_decl_body_out(&decl, fn_locals, fn_callables, evaluators, depth + 1);
        self.complete_value_out(out, evaluators)
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
        let fn_callables = Rc::new(HashMap::new());
        let decl = crate::runtime_flow::RuntimeDeclInfo { stmts, ..decl };
        let out =
            self.eval_runtime_decl_body_out(&decl, fn_locals, fn_callables, evaluators, depth + 1);
        self.complete_value_out(out, evaluators)
    }

    pub(crate) fn apply_scoped_decl_value_call_out(
        &mut self,
        owner_module: Option<&str>,
        fn_name: &str,
        arg_value: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return Out::Err(RuntimeError::Unsupported);
        }
        let Some(decl) = self.resolve_scoped_local_runtime_decl(owner_module, fn_name) else {
            return Out::Err(RuntimeError::Unsupported);
        };
        let accepts_unit_arg_as_zero_arity =
            decl.params.is_empty() && matches!(arg_value, RuntimeValue::Unit);
        if decl
            .callable_param_mask
            .iter()
            .any(|is_callable| *is_callable)
            || (decl.params.len() != 1 && !accepts_unit_arg_as_zero_arity)
        {
            return Out::Err(RuntimeError::Unsupported);
        }
        let mut fn_locals = RuntimeLocals::default();
        if !accepts_unit_arg_as_zero_arity {
            fn_locals.store(&decl.params[0], arg_value);
        }
        let fn_callables = Rc::new(HashMap::new());
        self.eval_runtime_decl_body_out(&decl, fn_locals, fn_callables, evaluators, depth + 1)
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
        let fn_callables = Rc::new(HashMap::new());
        self.eval_runtime_decl_body_out(
            &crate::runtime_flow::RuntimeDeclInfo { stmts, ..decl },
            fn_locals,
            fn_callables,
            evaluators,
            depth + 1,
        )
    }

    pub(crate) fn apply_named_value_call_ast(
        &mut self,
        fn_name: &str,
        arg_values: &[RuntimeValue],
        _locals: &RuntimeLocals,
        callables: &RcCallables,
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
        }
        Out::Err(RuntimeError::Unsupported)
    }

    pub(crate) fn apply_named_value_call_out(
        &mut self,
        fn_name: &str,
        arg_value: RuntimeValue,
        locals: &RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        if let Some(RuntimeValue::Callable(callable)) = locals.get(fn_name) {
            return self.eval_callable_value(
                &callable,
                arg_value,
                locals,
                callables,
                evaluators,
                depth + 1,
            );
        }
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

    pub(crate) fn apply_imported_callable_value_call_out(
        &mut self,
        module_path: &str,
        member: &str,
        arg_value: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return Out::Err(RuntimeError::Unsupported);
        }
        if let Some(value) = self.apply_imported_runtime_intrinsic(
            module_path,
            member,
            std::slice::from_ref(&arg_value),
            evaluators,
            depth + 1,
        ) {
            return Out::Done(value);
        }
        let Some(decl) = self.resolve_runtime_decl_from_module_path(module_path, member) else {
            return Out::Err(RuntimeError::Unsupported);
        };
        let accepts_unit_arg_as_zero_arity =
            decl.params.is_empty() && matches!(arg_value, RuntimeValue::Unit);
        if decl
            .callable_param_mask
            .iter()
            .any(|is_callable| *is_callable)
            || (decl.params.len() != 1 && !accepts_unit_arg_as_zero_arity)
        {
            return Out::Err(RuntimeError::Unsupported);
        }
        let mut fn_locals = RuntimeLocals::default();
        if !accepts_unit_arg_as_zero_arity {
            fn_locals.store(&decl.params[0], arg_value);
        }
        let fn_callables = Rc::new(HashMap::new());
        self.eval_runtime_decl_body_out(&decl, fn_locals, fn_callables, evaluators, depth + 1)
    }

    pub(crate) fn apply_named_value_call_args_out(
        &mut self,
        fn_name: &str,
        arg_values: &[RuntimeValue],
        locals: &RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        debug_assert!(
            arg_values.len() >= 2,
            "use apply_named_value_call_out for single-arg calls"
        );
        if let Some(RuntimeValue::Callable(callable)) = locals.get(fn_name) {
            return self.apply_callable_args_out(
                &callable,
                arg_values,
                locals,
                callables,
                evaluators,
                depth + 1,
            );
        }
        if let Some(callable) = callables.get(fn_name) {
            return self.apply_callable_args_out(
                callable,
                arg_values,
                locals,
                callables,
                evaluators,
                depth + 1,
            );
        }
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
        apply_binop_runtime_value_pure(op, lv, rv)
    }
}

/// Pure dispatch table for binary operators on `RuntimeValue`. Lives as a
/// free function so the table is independently testable without
/// constructing a full `RuntimeOutputResolver` (which has a heavy
/// `&Module` borrow + many runtime fields). Module-private — production
/// callers go through the `apply_binop_runtime_value` impl method.
fn apply_binop_runtime_value_pure(
    op: goby_core::BinOpKind,
    lv: RuntimeValue,
    rv: RuntimeValue,
) -> Option<RuntimeValue> {
    if matches!(op, goby_core::BinOpKind::Eq) {
        // runtime_value_eq already implements IEEE 754 for Float, so the
        // Eq short-circuit handles Float correctly without a special-case
        // branch here.
        return Some(RuntimeValue::Bool(runtime_value_eq(&lv, &rv)));
    }
    match (lv, rv) {
        (RuntimeValue::Bool(l), RuntimeValue::Bool(r)) => match op {
            goby_core::BinOpKind::Or => Some(RuntimeValue::Bool(l || r)),
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
        (RuntimeValue::Float(l), RuntimeValue::Float(r)) => match op {
            // IEEE 754: ÷0 produces ±Infinity / NaN naturally; do not
            // short-circuit on r == 0.0.
            goby_core::BinOpKind::Add => Some(RuntimeValue::Float(l + r)),
            goby_core::BinOpKind::Sub => Some(RuntimeValue::Float(l - r)),
            goby_core::BinOpKind::Mul => Some(RuntimeValue::Float(l * r)),
            goby_core::BinOpKind::Div => Some(RuntimeValue::Float(l / r)),
            // f64 ordering already returns false for any NaN comparand.
            goby_core::BinOpKind::Lt => Some(RuntimeValue::Bool(l < r)),
            goby_core::BinOpKind::Gt => Some(RuntimeValue::Bool(l > r)),
            goby_core::BinOpKind::Le => Some(RuntimeValue::Bool(l <= r)),
            goby_core::BinOpKind::Ge => Some(RuntimeValue::Bool(l >= r)),
            _ => None,
        },
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::apply_binop_runtime_value_pure as apply;
    use crate::runtime_value::RuntimeValue;
    use goby_core::BinOpKind;

    fn float(v: f64) -> RuntimeValue {
        RuntimeValue::Float(v)
    }

    fn expect_float(value: Option<RuntimeValue>) -> f64 {
        match value {
            Some(RuntimeValue::Float(v)) => v,
            Some(_) => panic!("expected RuntimeValue::Float, got non-Float"),
            None => panic!("expected RuntimeValue::Float, got None"),
        }
    }

    fn expect_bool(value: Option<RuntimeValue>) -> bool {
        match value {
            Some(RuntimeValue::Bool(b)) => b,
            Some(_) => panic!("expected RuntimeValue::Bool, got non-Bool"),
            None => panic!("expected RuntimeValue::Bool, got None"),
        }
    }

    #[test]
    fn float_arithmetic_basic() {
        assert_eq!(
            expect_float(apply(BinOpKind::Add, float(1.5), float(2.5))),
            4.0
        );
        assert_eq!(
            expect_float(apply(BinOpKind::Sub, float(1.0), float(2.5))),
            -1.5
        );
        assert_eq!(
            expect_float(apply(BinOpKind::Mul, float(2.0), float(3.5))),
            7.0
        );
        assert_eq!(
            expect_float(apply(BinOpKind::Div, float(7.0), float(2.0))),
            3.5
        );
    }

    #[test]
    fn float_division_by_zero_follows_ieee_754() {
        assert_eq!(
            expect_float(apply(BinOpKind::Div, float(1.0), float(0.0))),
            f64::INFINITY
        );
        assert_eq!(
            expect_float(apply(BinOpKind::Div, float(-1.0), float(0.0))),
            f64::NEG_INFINITY
        );
        assert!(expect_float(apply(BinOpKind::Div, float(0.0), float(0.0))).is_nan());
    }

    #[test]
    fn float_eq_uses_ieee_754_via_runtime_value_eq() {
        // NaN != NaN
        assert!(!expect_bool(apply(
            BinOpKind::Eq,
            float(f64::NAN),
            float(f64::NAN)
        )));
        // -0.0 == 0.0
        assert!(expect_bool(apply(BinOpKind::Eq, float(-0.0), float(0.0))));
        // 1.5 == 1.5
        assert!(expect_bool(apply(BinOpKind::Eq, float(1.5), float(1.5))));
    }

    #[test]
    fn float_ordering_returns_false_for_any_nan_operand() {
        for op in [BinOpKind::Lt, BinOpKind::Gt, BinOpKind::Le, BinOpKind::Ge] {
            assert!(
                !expect_bool(apply(op.clone(), float(f64::NAN), float(1.0))),
                "NaN {op:?} 1.0 should be False",
            );
            assert!(
                !expect_bool(apply(op.clone(), float(1.0), float(f64::NAN))),
                "1.0 {op:?} NaN should be False",
            );
            assert!(
                !expect_bool(apply(op.clone(), float(f64::NAN), float(f64::NAN))),
                "NaN {op:?} NaN should be False",
            );
        }
    }

    #[test]
    fn float_ordering_basic() {
        assert!(expect_bool(apply(BinOpKind::Lt, float(1.0), float(2.0))));
        assert!(expect_bool(apply(BinOpKind::Le, float(2.0), float(2.0))));
        assert!(expect_bool(apply(BinOpKind::Gt, float(2.0), float(1.0))));
        assert!(expect_bool(apply(BinOpKind::Ge, float(2.0), float(2.0))));
    }

    #[test]
    fn float_int_mixed_dispatch_returns_none() {
        // Type checker rejects mixed Float/Int operands; the fallback
        // mirrors that by failing the dispatch instead of coercing.
        assert!(apply(BinOpKind::Add, float(1.0), RuntimeValue::Int(2)).is_none());
        assert!(apply(BinOpKind::Add, RuntimeValue::Int(2), float(1.0)).is_none());
    }

    #[test]
    fn float_unsupported_ops_return_none() {
        // Spec §3 doesn't list `%` / bit ops / boolean ops for Float.
        assert!(apply(BinOpKind::Mod, float(1.0), float(2.0)).is_none());
        assert!(apply(BinOpKind::BitXor, float(1.0), float(2.0)).is_none());
        assert!(apply(BinOpKind::Or, float(1.0), float(2.0)).is_none());
        assert!(apply(BinOpKind::And, float(1.0), float(2.0)).is_none());
    }

    #[test]
    fn float_int_eq_is_false_via_runtime_value_eq() {
        // runtime_value_eq's catch-all returns false for cross-type pairs,
        // so `1.0 == 1` is False — locks the spec contract that Float and
        // Int are distinct primitives.
        assert!(!expect_bool(apply(
            BinOpKind::Eq,
            float(1.0),
            RuntimeValue::Int(1)
        )));
    }
}
