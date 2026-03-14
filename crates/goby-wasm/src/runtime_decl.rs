use super::*;
use crate::runtime_support::flatten_direct_call;
use unicode_segmentation::UnicodeSegmentation;

impl<'m> RuntimeOutputResolver<'m> {
    pub(super) fn try_eval_imported_decl_call_as_value(
        &mut self,
        expr: &Expr,
        caller_locals: &RuntimeLocals,
        caller_callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let (head, args) = flatten_direct_call(expr)?;
        if self.imported_head_matches_string_split(&head) && args.len() == 2 {
            let value = self.eval_expr_to_option(
                &args[0],
                caller_locals,
                caller_callables,
                evaluators,
                depth + 1,
            )?;
            let delim = self.eval_expr_to_option(
                &args[1],
                caller_locals,
                caller_callables,
                evaluators,
                depth + 1,
            )?;
            return match (value, delim) {
                (RuntimeValue::String(s), RuntimeValue::String(delim)) => {
                    let parts = if s.is_empty() {
                        Vec::new()
                    } else {
                        s.split(delim.as_str())
                            .map(|part| part.to_string())
                            .collect()
                    };
                    Some(RuntimeValue::ListString(parts))
                }
                _ => None,
            };
        }
        if self.imported_head_matches_string_graphemes(&head) && args.len() == 1 {
            let value = self.eval_expr_to_option(
                &args[0],
                caller_locals,
                caller_callables,
                evaluators,
                depth + 1,
            )?;
            return match value {
                RuntimeValue::String(s) => Some(RuntimeValue::ListString(
                    s.graphemes(true).map(|part| part.to_string()).collect(),
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
                let callable = match arg {
                    Expr::Lambda { param, body } => {
                        IntCallable::AstLambda(Box::new(AstLambdaCallable {
                            parameter: param.clone(),
                            body: (*body.clone()),
                            captured_locals: caller_locals.clone(),
                            captured_callables: caller_callables.clone(),
                        }))
                    }
                    Expr::Var(name) => self.resolve_callable_argument(name, caller_callables),
                    _ => {
                        self.set_runtime_error_once(ERR_CALLABLE_DISPATCH_DECL_PARAM);
                        return None;
                    }
                };
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
            FinishKind::Block,
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

    fn imported_head_matches_string_split(&self, head: &DirectCallHead) -> bool {
        match head {
            DirectCallHead::Bare(name) if name == "split" => {
                effective_runtime_imports(self.current_runtime_module())
                    .into_iter()
                    .any(|import| {
                        import.module_path == "goby/string"
                            && runtime_import_selects_name(&import.kind, "split")
                    })
            }
            DirectCallHead::Qualified { receiver, member } if member == "split" => {
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
        caller_callables: &HashMap<String, IntCallable>,
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
                let callable = match arg {
                    Expr::Lambda { param, body } => {
                        IntCallable::AstLambda(Box::new(AstLambdaCallable {
                            parameter: param.clone(),
                            body: (*body.clone()),
                            captured_locals: caller_locals.clone(),
                            captured_callables: caller_callables.clone(),
                        }))
                    }
                    Expr::Var(name) => self.resolve_callable_argument(name, caller_callables),
                    _ => {
                        self.set_runtime_error_once(ERR_CALLABLE_DISPATCH_DECL_PARAM);
                        return None;
                    }
                };
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

        let filtered_stmts: Vec<Stmt> = decl
            .stmts
            .iter()
            .filter(|stmt| {
                !matches!(
                    stmt,
                    Stmt::Expr(
                        Expr::Var(_) | Expr::IntLit(_) | Expr::StringLit(_) | Expr::BoolLit(_)
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
        caller_callables: &HashMap<String, IntCallable>,
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
                let callable = match arg {
                    Expr::Lambda { param, body } => {
                        IntCallable::AstLambda(Box::new(AstLambdaCallable {
                            parameter: param.clone(),
                            body: (*body.clone()),
                            captured_locals: caller_locals.clone(),
                            captured_callables: caller_callables.clone(),
                        }))
                    }
                    Expr::Var(name) => self.resolve_callable_argument(name, caller_callables),
                    _ => {
                        self.set_runtime_error_once(ERR_CALLABLE_DISPATCH_DECL_PARAM);
                        return None;
                    }
                };
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

        let filtered_stmts: Vec<Stmt> = stmts
            .iter()
            .filter(|stmt| {
                !matches!(
                    stmt,
                    Stmt::Expr(
                        Expr::Var(_) | Expr::IntLit(_) | Expr::StringLit(_) | Expr::BoolLit(_)
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
        caller_callables: &HashMap<String, IntCallable>,
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
                let callable = match arg {
                    Expr::Lambda { param, body } => {
                        IntCallable::AstLambda(Box::new(AstLambdaCallable {
                            parameter: param.clone(),
                            body: (*body.clone()),
                            captured_locals: caller_locals.clone(),
                            captured_callables: caller_callables.clone(),
                        }))
                    }
                    Expr::Var(name) => self.resolve_callable_argument(name, caller_callables),
                    _ => {
                        self.set_runtime_error_once(ERR_CALLABLE_DISPATCH_DECL_PARAM);
                        return None;
                    }
                };
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
            FinishKind::Block,
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
        let fn_callables = HashMap::new();
        let filtered_stmts: Vec<Stmt> = stmts
            .iter()
            .filter(|stmt| {
                !matches!(
                    stmt,
                    Stmt::Expr(
                        Expr::Var(_) | Expr::IntLit(_) | Expr::StringLit(_) | Expr::BoolLit(_)
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
        let mut fn_callables = HashMap::new();
        let param = param_name?;
        fn_callables.insert(param, callable);
        let filtered_stmts: Vec<Stmt> = stmts
            .iter()
            .filter(|stmt| {
                !matches!(
                    stmt,
                    Stmt::Expr(
                        Expr::Var(_) | Expr::IntLit(_) | Expr::StringLit(_) | Expr::BoolLit(_)
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
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        match callable {
            IntCallable::Named(name) => {
                self.execute_decl_as_side_effect(name, arg_val, evaluators, depth + 1)
            }
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
                let mut lambda_callables = callable.captured_callables.clone();
                match self.execute_unit_expr_ast(
                    &callable.body,
                    &mut lambda_locals,
                    &mut lambda_callables,
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
        callables: &HashMap<String, IntCallable>,
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
                let lambda_callables = callable.captured_callables.clone();
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
        callables: &HashMap<String, IntCallable>,
    ) -> IntCallable {
        callables
            .get(arg_name)
            .cloned()
            .unwrap_or_else(|| IntCallable::Named(arg_name.to_string()))
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
        let stmts = decl.parsed_body.as_ref()?.clone();
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
