use super::*;
use std::rc::Rc;
use crate::runtime_flow::RcCallables;

impl<'m> RuntimeOutputResolver<'m> {
    pub(super) fn current_handler_resume_value(&self, token_idx: usize) -> Option<RuntimeValue> {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => self
                .resume_tokens
                .get(token_idx)
                .and_then(|token| match &token.state {
                    HandlerContinuationState::Resumed(value) => Some((**value).clone()),
                    _ => None,
                }),
            lower::EffectExecutionMode::TypedContinuationOptimized => self
                .optimized_resume_tokens
                .get(token_idx)
                .and_then(|token| match &token.state {
                    HandlerContinuationState::Resumed(value) => Some((**value).clone()),
                    _ => None,
                }),
        }
    }

    pub(super) fn handler_has_suspended_cont(&self, token_idx: usize) -> bool {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => self
                .resume_tokens
                .get(token_idx)
                .and_then(|token| token.cont.as_ref())
                .is_some(),
            lower::EffectExecutionMode::TypedContinuationOptimized => self
                .optimized_resume_tokens
                .get(token_idx)
                .and_then(|token| token.cont.as_ref())
                .is_some(),
        }
    }

    pub(super) fn set_handler_token_state_suspended_placeholder(&mut self, token_idx: usize) {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => {
                if let Some(token) = self.resume_tokens.get_mut(token_idx) {
                    token.state = HandlerContinuationState::Suspended;
                }
            }
            lower::EffectExecutionMode::TypedContinuationOptimized => {
                if let Some(token) = self.optimized_resume_tokens.get_mut(token_idx) {
                    token.state = HandlerContinuationState::Suspended;
                }
            }
        }
    }

    pub(super) fn find_handler_method_by_name(
        &self,
        method_name: &str,
    ) -> Option<ResolvedHandlerMethod> {
        for inline in self.active_inline_handler_stack.iter().rev() {
            if let Some(method) = inline.methods.iter().find(|m| m.method.name == method_name) {
                return Some(ResolvedHandlerMethod {
                    method: method.method.clone(),
                    with_id: inline.with_id,
                });
            }
        }
        None
    }

    pub(super) fn resolve_qualified_effect_handler(
        &self,
        effect_name: &str,
        method_name: &str,
    ) -> Option<ResolvedEffectHandler> {
        if let Some(method) = self.find_handler_method_for_effect(effect_name, method_name) {
            return Some(ResolvedEffectHandler::Explicit(method));
        }
        if let Some(method) = self.find_handler_method_by_name(method_name) {
            return Some(ResolvedEffectHandler::Explicit(method));
        }
        self.runtime_imports
            .embedded_default_handlers
            .get(effect_name)
            .map(|handler_kind| ResolvedEffectHandler::EmbeddedDefault {
                handler_kind: *handler_kind,
                method_name: method_name.to_string(),
            })
    }

    pub(super) fn resolve_bare_effect_handler(
        &self,
        op_name: &str,
    ) -> Option<ResolvedEffectHandler> {
        let effect_name = self.unique_effect_name_for_operation(op_name)?;
        if let Some(method) = self.find_handler_method_for_effect(&effect_name, op_name) {
            return Some(ResolvedEffectHandler::Explicit(method));
        }
        if !self.operation_has_conflicting_effect(op_name, &effect_name)
            && let Some(method) = self.find_handler_method_by_name(op_name)
        {
            return Some(ResolvedEffectHandler::Explicit(method));
        }
        self.runtime_imports
            .embedded_default_handlers
            .get(&effect_name)
            .map(|handler_kind| ResolvedEffectHandler::EmbeddedDefault {
                handler_kind: *handler_kind,
                method_name: op_name.to_string(),
            })
    }

    pub(super) fn dispatch_effect_handler_as_value_flow(
        &mut self,
        handler: ResolvedEffectHandler,
        arg_value: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        match handler {
            ResolvedEffectHandler::Explicit(method) => self.dispatch_handler_method_as_value_flow(
                &method,
                arg_value,
                evaluators,
                depth + 1,
            ),
            ResolvedEffectHandler::EmbeddedDefault {
                handler_kind,
                method_name,
            } => match self
                .embedded_effect_runtime
                .invoke(handler_kind, &method_name, arg_value)
            {
                Ok(Some(value)) => Out::Done(value),
                Ok(None) => Out::Err(RuntimeError::Unsupported),
                Err(err) => {
                    self.set_runtime_error_once(err);
                    Out::Err(RuntimeError::Unsupported)
                }
            },
        }
    }

    pub(super) fn dispatch_effect_handler_as_side_effect(
        &mut self,
        handler: ResolvedEffectHandler,
        arg_value: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<()> {
        match handler {
            ResolvedEffectHandler::Explicit(method) => {
                self.dispatch_handler_method(&method, arg_value, evaluators, depth + 1)
            }
            ResolvedEffectHandler::EmbeddedDefault {
                handler_kind,
                method_name,
            } => match self
                .embedded_effect_runtime
                .invoke(handler_kind, &method_name, arg_value)
            {
                Ok(Some(_)) => Out::Done(()),
                Ok(None) => Out::Err(RuntimeError::Unsupported),
                Err(err) => {
                    self.set_runtime_error_once(err);
                    Out::Err(RuntimeError::Unsupported)
                }
            },
        }
    }

    pub(super) fn inline_handler_from_clauses(
        &self,
        clauses: &[HandlerClause],
        locals: &RuntimeLocals,
        callables: &RcCallables,
    ) -> InlineHandlerValue {
        let methods = clauses
            .iter()
            .enumerate()
            .map(|(clause_index, clause)| InlineHandlerMethod {
                effect_name: self.unique_effect_name_for_operation(&clause.name),
                method: RuntimeHandlerMethod {
                    clause_index,
                    name: clause.name.clone(),
                    params: clause.params.clone(),
                    parsed_body: clause.parsed_body.clone(),
                },
            })
            .collect();
        InlineHandlerValue {
            methods,
            captured_locals: locals.clone(),
            changed_outer_names: HashSet::new(),
            captured_callables: Rc::clone(callables),
            with_id: None,
        }
    }

    pub(super) fn visible_effect_names_for_operation_in(
        &self,
        module: &Module,
        op_name: &str,
    ) -> HashSet<String> {
        let mut names: HashSet<String> = module
            .effect_declarations
            .iter()
            .filter(|effect| effect.members.iter().any(|member| member.name == op_name))
            .map(|effect| effect.name.clone())
            .collect();

        for import in effective_runtime_imports(module) {
            let Some(imported) = self.runtime_imports.modules.get(&import.module_path) else {
                continue;
            };
            for effect in &imported.effect_declarations {
                if !runtime_import_selects_name(&import.kind, &effect.name) {
                    continue;
                }
                if effect.members.iter().any(|member| member.name == op_name) {
                    names.insert(effect.name.clone());
                }
            }
        }

        names
    }

    pub(super) fn visible_effect_names_for_operation(&self, op_name: &str) -> HashSet<String> {
        self.visible_effect_names_for_operation_in(self.current_runtime_module(), op_name)
    }

    pub(super) fn unique_effect_name_for_operation(&self, op_name: &str) -> Option<String> {
        let mut matches = self
            .visible_effect_names_for_operation(op_name)
            .into_iter()
            .collect::<Vec<_>>();
        matches.sort();
        matches.dedup();
        if matches.len() == 1 {
            matches.into_iter().next()
        } else {
            None
        }
    }

    pub(super) fn set_runtime_error_once(&mut self, message: impl Into<String>) {
        if self.runtime_error.is_none() {
            self.runtime_error = Some(message.into());
        }
    }

    pub(super) fn fresh_with_id(&mut self) -> WithId {
        let with_id = self.next_with_id;
        self.next_with_id += 1;
        with_id
    }

    pub(super) fn mark_runtime_abort(&mut self) {
        if self.runtime_error.is_none() {
            self.runtime_error = Some(INTERNAL_ABORT_MARKER.to_string());
        }
    }

    pub(super) fn runtime_error_is_abort_marker(&self) -> bool {
        self.runtime_error
            .as_deref()
            .is_some_and(|msg| msg == INTERNAL_ABORT_MARKER)
    }

    pub(super) fn clear_runtime_abort_marker(&mut self) {
        if self.runtime_error_is_abort_marker() {
            self.runtime_error = None;
        }
    }

    pub(super) fn update_active_inline_handler_locals(
        &mut self,
        with_id: WithId,
        locals: RuntimeLocals,
    ) {
        if let Some(inline) = self
            .active_inline_handler_stack
            .iter_mut()
            .rev()
            .find(|inline| inline.with_id == Some(with_id))
        {
            for name in inline.captured_locals.binding_names() {
                let old_value = inline.captured_locals.get(&name);
                let new_value = locals.get(&name);
                if !runtime_value_option_eq(old_value.as_ref(), new_value.as_ref()) {
                    inline.changed_outer_names.insert(name.clone());
                }
                match new_value {
                    Some(value) => inline.captured_locals.store(&name, value),
                    None => inline.captured_locals.clear(&name),
                }
            }
        }
    }

    pub(super) fn push_resume_token_for_handler(&mut self, take_caller_cont: bool) -> usize {
        let caller_cont = if take_caller_cont {
            self.pending_caller_cont_stack
                .last_mut()
                .and_then(|s| s.take())
        } else {
            None
        };
        self.resume_tokens.push(ResumeToken {
            continuation: Continuation { consumed: false },
            state: HandlerContinuationState::Pending,
            cont: caller_cont,
        });
        self.resume_tokens.len() - 1
    }

    pub(super) fn take_resume_token_result(
        &mut self,
        token_idx: usize,
    ) -> Option<HandlerCompletion> {
        if token_idx + 1 != self.resume_tokens.len() {
            self.set_runtime_error_once(ERR_RESUME_STACK_MISMATCH);
            return None;
        }
        let token = self.resume_tokens.pop()?;
        Some(match token.state {
            HandlerContinuationState::Pending
                if token.cont.is_some() && token.continuation.consumed =>
            {
                HandlerCompletion::Suspended
            }
            HandlerContinuationState::Pending => HandlerCompletion::Aborted,
            HandlerContinuationState::Resumed(value) => HandlerCompletion::Resumed(value),
            HandlerContinuationState::Suspended => HandlerCompletion::Suspended,
        })
    }

    pub(super) fn current_resume_token_mut(&mut self) -> Option<&mut ResumeToken> {
        self.resume_tokens.last_mut()
    }

    pub(super) fn push_optimized_resume_token_for_handler(
        &mut self,
        take_caller_cont: bool,
    ) -> usize {
        let caller_cont = if take_caller_cont {
            self.pending_caller_cont_stack
                .last_mut()
                .and_then(|s| s.take())
        } else {
            None
        };
        self.optimized_resume_tokens.push(OptimizedResumeToken {
            consumed: false,
            state: HandlerContinuationState::Pending,
            cont: caller_cont,
        });
        self.optimized_resume_tokens.len() - 1
    }

    pub(super) fn take_optimized_resume_token_result(
        &mut self,
        token_idx: usize,
    ) -> Option<HandlerCompletion> {
        if token_idx + 1 != self.optimized_resume_tokens.len() {
            self.set_runtime_error_once(ERR_RESUME_STACK_MISMATCH);
            return None;
        }
        let token = self.optimized_resume_tokens.pop()?;
        Some(match token.state {
            HandlerContinuationState::Pending if token.cont.is_some() && token.consumed => {
                HandlerCompletion::Suspended
            }
            HandlerContinuationState::Pending => HandlerCompletion::Aborted,
            HandlerContinuationState::Resumed(value) => HandlerCompletion::Resumed(value),
            HandlerContinuationState::Suspended => HandlerCompletion::Suspended,
        })
    }

    pub(super) fn current_optimized_resume_token_mut(
        &mut self,
    ) -> Option<&mut OptimizedResumeToken> {
        self.optimized_resume_tokens.last_mut()
    }

    #[allow(dead_code)]
    pub(super) fn handler_token_state_tag(&self, token_idx: usize) -> u8 {
        let state = match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => {
                self.resume_tokens.get(token_idx).map(|t| &t.state)
            }
            lower::EffectExecutionMode::TypedContinuationOptimized => self
                .optimized_resume_tokens
                .get(token_idx)
                .map(|t| &t.state),
        };
        match state {
            Some(HandlerContinuationState::Pending) | None => 0,
            Some(HandlerContinuationState::Resumed(_)) => 1,
            Some(HandlerContinuationState::Suspended) => 2,
        }
    }

    pub(super) fn set_handler_token_cont(&mut self, token_idx: usize, cont: Cont) {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => {
                if let Some(token) = self.resume_tokens.get_mut(token_idx) {
                    token.cont = Some(cont);
                }
            }
            lower::EffectExecutionMode::TypedContinuationOptimized => {
                if let Some(token) = self.optimized_resume_tokens.get_mut(token_idx) {
                    token.cont = Some(cont);
                }
            }
        }
    }

    pub(super) fn begin_handler_continuation_bridge(&mut self, take_caller_cont: bool) -> usize {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => {
                self.push_resume_token_for_handler(take_caller_cont)
            }
            lower::EffectExecutionMode::TypedContinuationOptimized => {
                self.push_optimized_resume_token_for_handler(take_caller_cont)
            }
        }
    }

    pub(super) fn finish_handler_continuation_bridge(
        &mut self,
        token_idx: usize,
    ) -> Option<HandlerCompletion> {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => {
                self.take_resume_token_result(token_idx)
            }
            lower::EffectExecutionMode::TypedContinuationOptimized => {
                self.take_optimized_resume_token_result(token_idx)
            }
        }
    }

    pub(super) fn resume_through_active_continuation_out(
        &mut self,
        resumed: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Out<RuntimeValue> {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => {
                self.resume_through_active_continuation_fallback_out(resumed, evaluators)
            }
            lower::EffectExecutionMode::TypedContinuationOptimized => {
                self.resume_through_active_continuation_optimized_out(resumed, evaluators)
            }
        }
    }

    pub(super) fn resume_through_active_continuation_fallback_out(
        &mut self,
        resumed: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Out<RuntimeValue> {
        let Some(token_ro) = self.resume_tokens.last() else {
            self.set_runtime_error_once(ERR_RESUME_MISSING);
            return Out::Err(RuntimeError::Unsupported);
        };
        if token_ro.continuation.consumed && token_ro.cont.is_none() {
            self.set_runtime_error_once(ERR_RESUME_CONSUMED);
            return Out::Err(RuntimeError::Unsupported);
        }
        let Some(token) = self.current_resume_token_mut() else {
            self.set_runtime_error_once(ERR_RESUME_MISSING);
            return Out::Err(RuntimeError::Unsupported);
        };
        if let Some(cont) = token.cont.take() {
            token.continuation.consumed = true;
            let out = self.apply_cont(cont, resumed, evaluators);
            match &out {
                Out::Done(value) => {
                    if let Some(token) = self.current_resume_token_mut() {
                        token.state = HandlerContinuationState::Resumed(Box::new(value.clone()));
                    }
                }
                Out::Suspend(c) => {
                    if let Some(token) = self.current_resume_token_mut() {
                        token.cont = Some(c.clone());
                    }
                }
                Out::Escape(_) | Out::Err(_) => {}
            }
            return out;
        }
        token.continuation.consumed = true;
        if let Some(token) = self.current_resume_token_mut() {
            token.state = HandlerContinuationState::Resumed(Box::new(resumed.clone()));
        }
        Out::Done(resumed)
    }

    pub(super) fn resume_through_active_continuation_optimized_out(
        &mut self,
        resumed: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Out<RuntimeValue> {
        let Some(token_ro) = self.optimized_resume_tokens.last() else {
            self.set_runtime_error_once(ERR_RESUME_MISSING);
            return Out::Err(RuntimeError::Unsupported);
        };
        if token_ro.consumed && token_ro.cont.is_none() {
            self.set_runtime_error_once(ERR_RESUME_CONSUMED);
            return Out::Err(RuntimeError::Unsupported);
        }
        let Some(token) = self.current_optimized_resume_token_mut() else {
            self.set_runtime_error_once(ERR_RESUME_MISSING);
            return Out::Err(RuntimeError::Unsupported);
        };
        if let Some(cont) = token.cont.take() {
            token.consumed = true;
            let out = self.apply_cont(cont, resumed, evaluators);
            match &out {
                Out::Done(value) => {
                    if let Some(token) = self.current_optimized_resume_token_mut() {
                        token.state = HandlerContinuationState::Resumed(Box::new(value.clone()));
                    }
                }
                Out::Suspend(c) => {
                    if let Some(token) = self.current_optimized_resume_token_mut() {
                        token.cont = Some(c.clone());
                    }
                }
                Out::Escape(_) | Out::Err(_) => {}
            }
            return out;
        }
        token.consumed = true;
        if let Some(token) = self.current_optimized_resume_token_mut() {
            token.state = HandlerContinuationState::Resumed(Box::new(resumed.clone()));
        }
        Out::Done(resumed)
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn eval_handler_body(
        &mut self,
        stmts: &[Stmt],
        locals: RuntimeLocals,
        callables: RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
        token_idx: usize,
        produce_value: bool,
        with_id: WithId,
    ) -> Out<(Option<RuntimeValue>, RuntimeLocals)> {
        self.eval_stmts(
            stmts,
            locals,
            callables,
            evaluators,
            depth,
            FinishKind::HandlerBody {
                token_idx,
                produce_value,
                with_id,
            },
        )
    }

    pub(super) fn dispatch_handler_method_core(
        &mut self,
        method: &ResolvedHandlerMethod,
        args: &[RuntimeValue],
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
        produce_value: bool,
        take_caller_cont: bool,
    ) -> Option<HandlerCompletion> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let stmts = method.method.parsed_body.as_deref()?.to_vec();
        if args.len() != method.method.params.len() {
            return None;
        }
        let token_idx = self.begin_handler_continuation_bridge(take_caller_cont);
        let mut handler_locals = self
            .active_inline_handler_stack
            .iter()
            .rev()
            .find_map(|inline| {
                inline
                    .methods
                    .iter()
                    .find(|m| {
                        inline.with_id == method.with_id
                            && m.method.clause_index == method.method.clause_index
                    })
                    .map(|_| inline.captured_locals.clone())
            })
            .unwrap_or_default();
        for (param, arg) in method.method.params.iter().zip(args.iter()) {
            handler_locals.store(param, arg.clone());
        }
        let handler_callables = self
            .active_inline_handler_stack
            .iter()
            .rev()
            .find_map(|inline| {
                inline
                    .methods
                    .iter()
                    .find(|m| {
                        inline.with_id == method.with_id
                            && m.method.clause_index == method.method.clause_index
                    })
                    .map(|_| Rc::clone(&inline.captured_callables))
            })
            .unwrap_or_else(|| Rc::new(HashMap::new()));
        match self.eval_handler_body(
            &stmts,
            handler_locals,
            handler_callables,
            evaluators,
            depth + 1,
            token_idx,
            produce_value,
            method.with_id?,
        ) {
            Out::Done((last_value, updated_locals)) => {
                self.update_active_inline_handler_locals(method.with_id?, updated_locals);
                if self.current_handler_resume_value(token_idx).is_some() {
                    self.finish_handler_continuation_bridge(token_idx)
                } else {
                    let _ = self.finish_handler_continuation_bridge(token_idx);
                    Some(HandlerCompletion::Escaped(Escape::WithScope {
                        with_id: method.with_id?,
                        value: last_value.unwrap_or(RuntimeValue::Unit),
                    }))
                }
            }
            Out::Escape(escape) => Some(HandlerCompletion::Escaped(escape)),
            Out::Suspend(cont) => {
                self.set_handler_token_cont(token_idx, cont);
                self.finish_handler_continuation_bridge(token_idx)
            }
            Out::Err(RuntimeError::Abort { .. }) => Some(HandlerCompletion::Aborted),
            Out::Err(RuntimeError::Unsupported) => None,
        }
    }

    pub(super) fn dispatch_handler_method_as_value_flow(
        &mut self,
        method: &ResolvedHandlerMethod,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        match self.dispatch_handler_method_core(method, &[arg_val], evaluators, depth, true, false)
        {
            Some(HandlerCompletion::Resumed(value)) => Out::Done(*value),
            Some(HandlerCompletion::Escaped(escape)) => Out::Escape(escape),
            Some(HandlerCompletion::Suspended) => Out::Suspend(Cont::Resume),
            Some(HandlerCompletion::Aborted) => Out::Err(RuntimeError::Abort {
                kind: "aborted".into(),
            }),
            None => Out::Err(RuntimeError::Unsupported),
        }
    }

    pub(super) fn dispatch_handler_method_as_value_with_args_flow(
        &mut self,
        method: &ResolvedHandlerMethod,
        args: &[RuntimeValue],
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        match self.dispatch_handler_method_core(method, args, evaluators, depth, true, false) {
            Some(HandlerCompletion::Resumed(value)) => Out::Done(*value),
            Some(HandlerCompletion::Escaped(escape)) => Out::Escape(escape),
            Some(HandlerCompletion::Suspended) => Out::Suspend(Cont::Resume),
            Some(HandlerCompletion::Aborted) => Out::Err(RuntimeError::Abort {
                kind: "aborted".into(),
            }),
            None => Out::Err(RuntimeError::Unsupported),
        }
    }

    pub(super) fn dispatch_handler_method(
        &mut self,
        method: &ResolvedHandlerMethod,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<()> {
        match self.dispatch_handler_method_core(method, &[arg_val], evaluators, depth, false, true)
        {
            Some(HandlerCompletion::Resumed(_)) => Out::Done(()),
            Some(HandlerCompletion::Escaped(_escape)) => Out::Done(()),
            Some(HandlerCompletion::Suspended) => Out::Suspend(Cont::Resume),
            Some(HandlerCompletion::Aborted) => Out::Err(RuntimeError::Abort {
                kind: "aborted".into(),
            }),
            None => Out::Err(RuntimeError::Unsupported),
        }
    }
}
