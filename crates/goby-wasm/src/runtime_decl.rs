use super::*;
use std::rc::Rc;
use crate::runtime_flow::RcCallables;
use crate::grapheme_semantics::collect_extended_grapheme_spans;
use crate::runtime_support::flatten_direct_call;
use crate::wasm_exec_plan::decl_exec_plan;

impl<'m> RuntimeOutputResolver<'m> {
    fn runtime_value_to_expr(value: &RuntimeValue) -> Option<Expr> {
        match value {
            RuntimeValue::String(text) => Some(Expr::StringLit(text.clone())),
            RuntimeValue::Int(number) => Some(Expr::IntLit(*number)),
            RuntimeValue::Unit => Some(Expr::unit_value()),
            RuntimeValue::Bool(value) => Some(Expr::BoolLit(*value)),
            RuntimeValue::Tuple(items) => Some(Expr::TupleLit(
                items
                    .iter()
                    .map(Self::runtime_value_to_expr)
                    .collect::<Option<Vec<_>>>()?,
            )),
            RuntimeValue::ListInt(values) => Some(Expr::ListLit {
                elements: values.iter().copied().map(Expr::IntLit).collect(),
                spread: None,
            }),
            RuntimeValue::ListString(values) => Some(Expr::ListLit {
                elements: values.iter().cloned().map(Expr::StringLit).collect(),
                spread: None,
            }),
            RuntimeValue::Handler(_) | RuntimeValue::Record { .. } => None,
        }
    }

    fn resolve_callable_expr(
        &mut self,
        arg: &Expr,
        caller_locals: &RuntimeLocals,
        caller_callables: &RcCallables,
    ) -> Option<IntCallable> {
        match arg {
            Expr::Lambda { param, body } => {
                Some(IntCallable::AstLambda(Box::new(AstLambdaCallable {
                    parameter: param.clone(),
                    body: (*body.clone()),
                    captured_locals: caller_locals.clone(),
                    captured_callables: Rc::clone(caller_callables),
                })))
            }
            Expr::Var { name, .. } => Some(self.resolve_callable_argument(name, caller_callables)),
            Expr::Qualified {
                receiver, member, ..
            } => {
                let head = DirectCallHead::Qualified {
                    receiver: receiver.clone(),
                    member: member.clone(),
                };
                if let Some(decl) = self.resolve_imported_runtime_decl(&head)
                    && let Some(module_path) = decl.owner_module
                {
                    return Some(IntCallable::Imported {
                        module_path,
                        member: member.clone(),
                    });
                }
                Some(IntCallable::Qualified {
                    receiver: receiver.clone(),
                    member: member.clone(),
                })
            }
            _ => {
                self.set_runtime_error_once(ERR_CALLABLE_DISPATCH_DECL_PARAM);
                None
            }
        }
    }

    pub(super) fn try_eval_imported_decl_call_as_value(
        &mut self,
        expr: &Expr,
        caller_locals: &RuntimeLocals,
        caller_callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let (head, args) = flatten_direct_call(expr)?;
        if self.imported_head_matches_string_graphemes(&head) && args.len() == 1 {
            let value = self.eval_expr_to_option(
                args[0],
                caller_locals,
                caller_callables,
                evaluators,
                depth + 1,
            )?;
            return match value {
                RuntimeValue::String(s) => Some(RuntimeValue::ListString(
                    collect_extended_grapheme_spans(&s)
                        .into_iter()
                        .map(|span| s[span.start..span.end].to_string())
                        .collect(),
                )),
                _ => None,
            };
        }
        let decl = self.resolve_imported_runtime_decl(&head)?;
        if decl.params.len() != args.len() {
            return None;
        }

        let mut fn_locals = RuntimeLocals::default();
        let mut fn_callables = HashMap::new();
        for (idx, (param, arg)) in decl.params.iter().zip(args.iter()).enumerate() {
            if decl.callable_param_mask.get(idx).copied().unwrap_or(false) {
                let callable = self.resolve_callable_expr(arg, caller_locals, caller_callables)?;
                fn_callables.insert(param.clone(), callable);
                continue;
            }
            let arg_val = self.eval_expr_to_option(
                arg,
                caller_locals,
                caller_callables,
                evaluators,
                depth + 1,
            )?;
            fn_locals.store(param, arg_val);
        }
        let fn_callables = Rc::new(fn_callables);

        let out =
            self.eval_runtime_decl_body_out(&decl, fn_locals, fn_callables, evaluators, depth + 1);
        self.complete_value_out(out, evaluators)
    }

    fn imported_head_matches_string_graphemes(&self, head: &DirectCallHead) -> bool {
        match head {
            DirectCallHead::Bare(name) if name == "graphemes" => {
                effective_runtime_imports(self.current_runtime_module())
                    .into_iter()
                    .any(|import| {
                        import.module_path == "goby/string"
                            && runtime_import_selects_name(&import.kind, "graphemes")
                    })
            }
            DirectCallHead::Qualified { receiver, member } if member == "graphemes" => {
                if receiver == "string"
                    && effective_runtime_imports(self.current_runtime_module())
                        .into_iter()
                        .any(|import| import.module_path == "goby/string")
                {
                    return true;
                }
                effective_runtime_imports(self.current_runtime_module())
                    .into_iter()
                    .any(|import| {
                        if import.module_path != "goby/string" {
                            return false;
                        }
                        match import.kind {
                            goby_core::ImportKind::Plain => import
                                .module_path
                                .rsplit('/')
                                .next()
                                .is_some_and(|qualifier| qualifier == receiver),
                            goby_core::ImportKind::Alias(alias) => alias == *receiver,
                            goby_core::ImportKind::Selective(_) => false,
                        }
                    })
            }
            _ => false,
        }
    }

    pub(super) fn try_execute_imported_decl_call_as_side_effect(
        &mut self,
        expr: &Expr,
        caller_locals: &RuntimeLocals,
        caller_callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let (head, args) = flatten_direct_call(expr)?;
        let decl = self.resolve_imported_runtime_decl(&head)?;
        if decl.params.len() != args.len() {
            return None;
        }

        let mut fn_locals = RuntimeLocals::default();
        let mut fn_callables = HashMap::new();
        for (idx, (param, arg)) in decl.params.iter().zip(args.iter()).enumerate() {
            if decl.callable_param_mask.get(idx).copied().unwrap_or(false) {
                let callable = self.resolve_callable_expr(arg, caller_locals, caller_callables)?;
                fn_callables.insert(param.clone(), callable);
                continue;
            }
            let arg_val = self.eval_expr_to_option(
                arg,
                caller_locals,
                caller_callables,
                evaluators,
                depth + 1,
            )?;
            fn_locals.store(param, arg_val);
        }
        let fn_callables = Rc::new(fn_callables);

        let filtered_stmts: Vec<Stmt> = decl
            .stmts
            .iter()
            .filter(|stmt| {
                !matches!(
                    stmt,
                    Stmt::Expr(
                        Expr::Var { name: _, .. }
                            | Expr::IntLit(_)
                            | Expr::StringLit(_)
                            | Expr::BoolLit(_),
                        _
                    )
                )
            })
            .cloned()
            .collect();
        if let Some(owner_module) = &decl.owner_module {
            self.current_module_stack.push(owner_module.clone());
        }
        self.push_runtime_decl_context(&decl);
        let result = self.eval_stmts(
            &filtered_stmts,
            fn_locals,
            fn_callables,
            evaluators,
            depth + 1,
            FinishKind::Block,
        );
        self.pop_runtime_decl_context();
        if decl.owner_module.is_some() {
            self.current_module_stack.pop();
        }
        match result {
            Out::Done(_) => Some(()),
            Out::Suspend(_) | Out::Escape(_) | Out::Err(_) => None,
        }
    }

    pub(super) fn execute_decl_call_chain_as_side_effect(
        &mut self,
        fn_name: &str,
        args: &[&Expr],
        caller_locals: &RuntimeLocals,
        caller_callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let decl = self.resolve_local_runtime_decl(fn_name)?;
        if decl.params.len() != args.len() {
            return None;
        }
        let params = decl.params.clone();
        let callable_param_mask = decl.callable_param_mask.clone();
        let stmts = decl.stmts.clone();
        let mut fn_locals = RuntimeLocals::default();
        let mut fn_callables = HashMap::new();

        for (idx, (param, arg)) in params.iter().zip(args.iter()).enumerate() {
            if callable_param_mask.get(idx).copied().unwrap_or(false) {
                let callable = self.resolve_callable_expr(arg, caller_locals, caller_callables)?;
                fn_callables.insert(param.clone(), callable);
                continue;
            }
            let arg_val = self.eval_expr_to_option(
                arg,
                caller_locals,
                caller_callables,
                evaluators,
                depth + 1,
            )?;
            fn_locals.store(param, arg_val);
        }
        let fn_callables = Rc::new(fn_callables);

        let filtered_stmts: Vec<Stmt> = stmts
            .iter()
            .filter(|stmt| {
                !matches!(
                    stmt,
                    Stmt::Expr(
                        Expr::Var { name: _, .. }
                            | Expr::IntLit(_)
                            | Expr::StringLit(_)
                            | Expr::BoolLit(_),
                        _
                    )
                )
            })
            .cloned()
            .collect();
        if let Some(owner_module) = &decl.owner_module {
            self.current_module_stack.push(owner_module.clone());
        }
        self.push_runtime_decl_context(&decl);
        let result = self.eval_stmts(
            &filtered_stmts,
            fn_locals,
            fn_callables,
            evaluators,
            depth + 1,
            FinishKind::Block,
        );
        self.pop_runtime_decl_context();
        if decl.owner_module.is_some() {
            self.current_module_stack.pop();
        }
        match result {
            Out::Done(_) => Some(()),
            Out::Suspend(_) | Out::Escape(_) | Out::Err(_) => None,
        }
    }

    pub(super) fn eval_decl_call_chain_as_value(
        &mut self,
        fn_name: &str,
        args: &[&Expr],
        caller_locals: &RuntimeLocals,
        caller_callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let decl = self.resolve_local_runtime_decl(fn_name)?;
        if decl.params.len() != args.len() {
            return None;
        }
        let params = decl.params.clone();
        let callable_param_mask = decl.callable_param_mask.clone();
        let mut fn_locals = RuntimeLocals::default();
        let mut fn_callables = HashMap::new();

        for (idx, (param, arg)) in params.iter().zip(args.iter()).enumerate() {
            if callable_param_mask.get(idx).copied().unwrap_or(false) {
                let callable = self.resolve_callable_expr(arg, caller_locals, caller_callables)?;
                fn_callables.insert(param.clone(), callable);
                continue;
            }
            let arg_val = self.eval_expr_to_option(
                arg,
                caller_locals,
                caller_callables,
                evaluators,
                depth + 1,
            )?;
            fn_locals.store(param, arg_val);
        }
        let fn_callables = Rc::new(fn_callables);

        let out =
            self.eval_runtime_decl_body_out(&decl, fn_locals, fn_callables, evaluators, depth + 1);
        self.complete_value_out(out, evaluators)
    }

    pub(super) fn execute_decl_as_side_effect(
        &mut self,
        fn_name: &str,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let decl = self.resolve_local_runtime_decl(fn_name)?;
        let param_name = decl.params.first().cloned();
        let stmts = decl.stmts.clone();
        let mut fn_locals = RuntimeLocals::default();
        if let Some(param) = param_name {
            fn_locals.store(&param, arg_val);
        }
        let fn_callables = Rc::new(HashMap::new());
        let filtered_stmts: Vec<Stmt> = stmts
            .iter()
            .filter(|stmt| {
                !matches!(
                    stmt,
                    Stmt::Expr(
                        Expr::Var { name: _, .. }
                            | Expr::IntLit(_)
                            | Expr::StringLit(_)
                            | Expr::BoolLit(_),
                        _
                    )
                )
            })
            .cloned()
            .collect();
        if let Some(owner_module) = &decl.owner_module {
            self.current_module_stack.push(owner_module.clone());
        }
        self.push_runtime_decl_context(&decl);
        let result = self.eval_stmts(
            &filtered_stmts,
            fn_locals,
            fn_callables,
            evaluators,
            depth + 1,
            FinishKind::Block,
        );
        self.pop_runtime_decl_context();
        if decl.owner_module.is_some() {
            self.current_module_stack.pop();
        }
        match result {
            Out::Done(_) => Some(()),
            Out::Suspend(_) | Out::Escape(_) | Out::Err(_) => None,
        }
    }

    pub(super) fn execute_scoped_decl_as_side_effect(
        &mut self,
        owner_module: Option<&str>,
        fn_name: &str,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let decl = self.resolve_scoped_local_runtime_decl(owner_module, fn_name)?;
        let accepts_unit_arg_as_zero_arity =
            decl.params.is_empty() && matches!(arg_val, RuntimeValue::Unit);
        if decl.params.len() != 1 && !accepts_unit_arg_as_zero_arity {
            return None;
        }
        let mut fn_locals = RuntimeLocals::default();
        if !accepts_unit_arg_as_zero_arity {
            fn_locals.store(&decl.params[0], arg_val);
        }
        let fn_callables = Rc::new(HashMap::new());
        let filtered_stmts: Vec<Stmt> = decl
            .stmts
            .iter()
            .filter(|stmt| {
                !matches!(
                    stmt,
                    Stmt::Expr(
                        Expr::Var { name: _, .. }
                            | Expr::IntLit(_)
                            | Expr::StringLit(_)
                            | Expr::BoolLit(_),
                        _
                    )
                )
            })
            .cloned()
            .collect();
        if let Some(owner_module) = &decl.owner_module {
            self.current_module_stack.push(owner_module.clone());
        }
        self.push_runtime_decl_context(&decl);
        let result = self.eval_stmts(
            &filtered_stmts,
            fn_locals,
            fn_callables,
            evaluators,
            depth + 1,
            FinishKind::Block,
        );
        self.pop_runtime_decl_context();
        if decl.owner_module.is_some() {
            self.current_module_stack.pop();
        }
        match result {
            Out::Done(_) => Some(()),
            Out::Suspend(_) | Out::Escape(_) | Out::Err(_) => None,
        }
    }

    pub(super) fn execute_imported_decl_as_side_effect_from_module(
        &mut self,
        module_path: &str,
        member: &str,
        arg_val: RuntimeValue,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let decl = self.resolve_runtime_decl_from_module_path(module_path, member)?;
        let accepts_unit_arg_as_zero_arity =
            decl.params.is_empty() && matches!(arg_val, RuntimeValue::Unit);
        if decl.params.len() != 1 && !accepts_unit_arg_as_zero_arity {
            return None;
        }
        let mut fn_locals = RuntimeLocals::default();
        if !accepts_unit_arg_as_zero_arity {
            fn_locals.store(&decl.params[0], arg_val);
        }
        let fn_callables = Rc::clone(callables);
        let filtered_stmts: Vec<Stmt> = decl
            .stmts
            .iter()
            .filter(|stmt| {
                !matches!(
                    stmt,
                    Stmt::Expr(
                        Expr::Var { name: _, .. }
                            | Expr::IntLit(_)
                            | Expr::StringLit(_)
                            | Expr::BoolLit(_),
                        _
                    )
                )
            })
            .cloned()
            .collect();
        if let Some(owner_module) = &decl.owner_module {
            self.current_module_stack.push(owner_module.clone());
        }
        self.push_runtime_decl_context(&decl);
        let result = self.eval_stmts(
            &filtered_stmts,
            fn_locals,
            fn_callables,
            evaluators,
            depth + 1,
            FinishKind::Block,
        );
        self.pop_runtime_decl_context();
        if decl.owner_module.is_some() {
            self.current_module_stack.pop();
        }
        match result {
            Out::Done(_) => Some(()),
            Out::Suspend(_) | Out::Escape(_) | Out::Err(_) => None,
        }
    }

    pub(super) fn execute_decl_with_callable_as_side_effect(
        &mut self,
        fn_name: &str,
        callable: IntCallable,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let decl = self.resolve_local_runtime_decl(fn_name)?;
        let param_name = decl.params.first().cloned();
        let stmts = decl.stmts.clone();
        let fn_locals = RuntimeLocals::default();
        let mut fn_callables_map = HashMap::new();
        let param = param_name?;
        fn_callables_map.insert(param, callable);
        let fn_callables = Rc::new(fn_callables_map);
        let filtered_stmts: Vec<Stmt> = stmts
            .iter()
            .filter(|stmt| {
                !matches!(
                    stmt,
                    Stmt::Expr(
                        Expr::Var { name: _, .. }
                            | Expr::IntLit(_)
                            | Expr::StringLit(_)
                            | Expr::BoolLit(_),
                        _
                    )
                )
            })
            .cloned()
            .collect();
        if let Some(owner_module) = &decl.owner_module {
            self.current_module_stack.push(owner_module.clone());
        }
        self.push_runtime_decl_context(&decl);
        let result = self.eval_stmts(
            &filtered_stmts,
            fn_locals,
            fn_callables,
            evaluators,
            depth + 1,
            FinishKind::Block,
        );
        self.pop_runtime_decl_context();
        if decl.owner_module.is_some() {
            self.current_module_stack.pop();
        }
        match result {
            Out::Done(_) => Some(()),
            Out::Suspend(_) | Out::Escape(_) | Out::Err(_) => None,
        }
    }

    pub(super) fn dispatch_callable_side_effect(
        &mut self,
        callable: &IntCallable,
        arg_val: RuntimeValue,
        _locals: &RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        match callable {
            IntCallable::Named(name) => {
                self.execute_decl_as_side_effect(name, arg_val, evaluators, depth + 1)
            }
            IntCallable::LocalDecl { owner_module, name } => self
                .execute_scoped_decl_as_side_effect(
                    owner_module.as_deref(),
                    name,
                    arg_val,
                    evaluators,
                    depth + 1,
                ),
            IntCallable::Imported {
                module_path,
                member,
            } => self.execute_imported_decl_as_side_effect_from_module(
                module_path,
                member,
                arg_val,
                callables,
                evaluators,
                depth + 1,
            ),
            IntCallable::Qualified { receiver, member } => self
                .try_execute_imported_decl_call_as_side_effect(
                    &Expr::call(
                        Expr::qualified(receiver.clone(), member.clone()),
                        Self::runtime_value_to_expr(&arg_val)?,
                    ),
                    &RuntimeLocals::default(),
                    callables,
                    evaluators,
                    depth + 1,
                ),
            IntCallable::Lambda(lambda) => {
                if let RuntimeValue::Int(n) = arg_val
                    && evaluators.int.eval_lambda(lambda, n, callables).is_some()
                {
                    return Some(());
                }
                None
            }
            IntCallable::AstLambda(callable) => {
                let mut lambda_locals = callable.captured_locals.clone();
                lambda_locals.store(&callable.parameter, arg_val);
                let lambda_callables = Rc::clone(&callable.captured_callables);
                match self.execute_unit_expr_ast(
                    &callable.body,
                    &mut lambda_locals,
                    &lambda_callables,
                    evaluators,
                    depth + 1,
                ) {
                    Out::Done(()) => Some(()),
                    Out::Suspend(_) | Out::Escape(_) | Out::Err(_) => self
                        .eval_expr_to_option(
                            &callable.body,
                            &lambda_locals,
                            &lambda_callables,
                            evaluators,
                            depth + 1,
                        )
                        .map(|_| ()),
                }
            }
        }
    }

    pub(super) fn eval_callable_value(
        &mut self,
        callable: &IntCallable,
        arg_val: RuntimeValue,
        locals: &RuntimeLocals,
        callables: &RcCallables,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        match callable {
            IntCallable::Named(name) => self.apply_named_value_call_out(
                name,
                arg_val,
                locals,
                callables,
                evaluators,
                depth + 1,
            ),
            IntCallable::LocalDecl { owner_module, name } => self.apply_scoped_decl_value_call_out(
                owner_module.as_deref(),
                name,
                arg_val,
                evaluators,
                depth + 1,
            ),
            IntCallable::Imported {
                module_path,
                member,
            } => self.apply_imported_callable_value_call_out(
                module_path,
                member,
                arg_val,
                evaluators,
                depth + 1,
            ),
            IntCallable::Qualified { receiver, member } => self
                .apply_receiver_method_value_call_out(
                    receiver,
                    member,
                    arg_val,
                    evaluators,
                    depth + 1,
                ),
            IntCallable::Lambda(lambda) => {
                let RuntimeValue::Int(arg_int) = arg_val else {
                    return Out::Err(RuntimeError::Unsupported);
                };
                evaluators
                    .int
                    .eval_lambda(lambda, arg_int, callables)
                    .map_or(Out::Err(RuntimeError::Unsupported), |value| {
                        Out::Done(RuntimeValue::Int(value))
                    })
            }
            IntCallable::AstLambda(callable) => {
                let mut lambda_locals = callable.captured_locals.clone();
                lambda_locals.store(&callable.parameter, arg_val);
                let lambda_callables = Rc::clone(&callable.captured_callables);
                self.eval_expr(
                    &callable.body,
                    &lambda_locals,
                    &lambda_callables,
                    evaluators,
                    depth + 1,
                )
            }
        }
    }

    pub(super) fn resolve_callable_argument(
        &self,
        arg_name: &str,
        callables: &RcCallables,
    ) -> IntCallable {
        if let Some(callable) = callables.get(arg_name) {
            return callable.clone();
        }
        let owner_module = self.current_module_stack.last().cloned();
        if self
            .resolve_scoped_local_runtime_decl(owner_module.as_deref(), arg_name)
            .is_some()
        {
            return IntCallable::LocalDecl {
                owner_module,
                name: arg_name.to_string(),
            };
        }
        IntCallable::Named(arg_name.to_string())
    }

    pub(super) fn declaration_expects_callable_param(&self, fn_name: &str) -> bool {
        let Some(decl) = self.resolve_local_runtime_decl(fn_name) else {
            return false;
        };
        decl.callable_param_mask.first().copied().unwrap_or(false)
    }

    pub(super) fn current_runtime_module(&self) -> &Module {
        self.current_module_stack
            .last()
            .and_then(|path| self.runtime_imports.modules.get(path))
            .unwrap_or(self.module)
    }

    pub(super) fn push_runtime_decl_context(&mut self, decl: &RuntimeDeclInfo) {
        let label = match &decl.owner_module {
            Some(module) => format!("{module}.{}", decl.name),
            None => decl.name.clone(),
        };
        self.current_decl_stack.push(label);
    }

    pub(super) fn pop_runtime_decl_context(&mut self) {
        self.current_decl_stack.pop();
    }

    pub(super) fn set_unhandled_effect_error(&mut self, op_name: &str) {
        let source = self
            .current_decl_stack
            .last()
            .cloned()
            .unwrap_or_else(|| "<runtime>".to_string());
        self.set_runtime_error_once(format!(
            "unhandled effect operation `{op_name}` from {source}"
        ));
    }

    pub(super) fn resolve_local_runtime_decl(&self, fn_name: &str) -> Option<RuntimeDeclInfo> {
        let decl = self
            .current_runtime_module()
            .declarations
            .iter()
            .find(|d| d.name == fn_name)?;
        Self::runtime_decl_info_from_decl(decl, self.current_module_stack.last().cloned())
    }

    pub(super) fn resolve_scoped_local_runtime_decl(
        &self,
        owner_module: Option<&str>,
        fn_name: &str,
    ) -> Option<RuntimeDeclInfo> {
        match owner_module {
            Some(module_path) => self.resolve_runtime_decl_from_module_path(module_path, fn_name),
            None => {
                let decl = self
                    .module
                    .declarations
                    .iter()
                    .find(|d| d.name == fn_name)?;
                Self::runtime_decl_info_from_decl(decl, None)
            }
        }
    }

    pub(super) fn resolve_runtime_decl_from_module_path(
        &self,
        module_path: &str,
        member: &str,
    ) -> Option<RuntimeDeclInfo> {
        let module = self.runtime_imports.modules.get(module_path)?;
        let decl = module.declarations.iter().find(|d| d.name == member)?;
        Self::runtime_decl_info_from_decl(decl, Some(module_path.to_string()))
    }

    pub(super) fn resolve_imported_runtime_decl(
        &self,
        head: &DirectCallHead,
    ) -> Option<RuntimeDeclInfo> {
        match head {
            DirectCallHead::Bare(name) => {
                for import in effective_runtime_imports(self.current_runtime_module()) {
                    let goby_core::ImportKind::Selective(selected) = import.kind else {
                        continue;
                    };
                    if !selected.iter().any(|symbol| symbol == name) {
                        continue;
                    }
                    let module = self.runtime_imports.modules.get(&import.module_path)?;
                    let decl = module.declarations.iter().find(|d| d.name == *name)?;
                    return Self::runtime_decl_info_from_decl(decl, Some(import.module_path));
                }
                None
            }
            DirectCallHead::Qualified { receiver, member } => {
                for import in effective_runtime_imports(self.current_runtime_module()) {
                    let canonical_receiver = import.module_path.rsplit('/').next()?;
                    if receiver == canonical_receiver {
                        let module = self.runtime_imports.modules.get(&import.module_path)?;
                        let decl = module.declarations.iter().find(|d| d.name == *member)?;
                        return Self::runtime_decl_info_from_decl(decl, Some(import.module_path));
                    }
                    let matches_receiver = match &import.kind {
                        goby_core::ImportKind::Plain => import
                            .module_path
                            .rsplit('/')
                            .next()
                            .is_some_and(|qualifier| qualifier == receiver),
                        goby_core::ImportKind::Alias(alias) => alias == receiver,
                        goby_core::ImportKind::Selective(_) => false,
                    };
                    if !matches_receiver {
                        continue;
                    }
                    let module = self.runtime_imports.modules.get(&import.module_path)?;
                    let decl = module.declarations.iter().find(|d| d.name == *member)?;
                    return Self::runtime_decl_info_from_decl(decl, Some(import.module_path));
                }
                None
            }
        }
    }

    pub(super) fn runtime_decl_info_from_decl(
        decl: &goby_core::Declaration,
        owner_module: Option<String>,
    ) -> Option<RuntimeDeclInfo> {
        let stmts = decl_exec_plan(decl).runtime?.stmts.into_owned();
        let callable_param_mask = match decl.type_annotation.as_deref() {
            Some(annotation) => match parse_function_type(annotation) {
                Some(function_type) => function_type
                    .arguments
                    .iter()
                    .map(|arg| arg.contains("->"))
                    .collect(),
                None => vec![false; decl.params.len()],
            },
            None => vec![false; decl.params.len()],
        };
        Some(RuntimeDeclInfo {
            name: decl.name.clone(),
            owner_module,
            params: decl.params.clone(),
            callable_param_mask,
            stmts,
        })
    }
}
