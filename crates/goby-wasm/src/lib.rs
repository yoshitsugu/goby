mod backend;
mod call;
mod fallback;
mod layout;
mod lower;
mod planning;
mod runtime_decl;
mod runtime_dispatch;
mod runtime_env;
mod runtime_eval;
mod runtime_exec;
mod runtime_flow;
mod runtime_replay;
mod runtime_resolver;
mod runtime_value;
mod support;

use std::collections::{HashMap, HashSet};
use unicode_segmentation::UnicodeSegmentation;

use crate::call::flatten_named_call;
use crate::runtime_env::{
    EmbeddedEffectRuntime, RuntimeImportContext, effective_runtime_imports,
    load_runtime_import_context, runtime_import_selects_name,
};
use crate::runtime_eval::{
    AstLambdaCallable, IntCallable, IntEvaluator, ListIntEvaluator, Statement,
    collect_functions_with_result, collect_unit_functions, is_identifier, is_string_literal,
    parse_call, parse_int_callable, statements,
};
use crate::runtime_flow::{
    ApplyStep, Cont, Continuation, DirectCallHead, Escape, FinishKind, HandlerCompletion,
    HandlerContinuationState, InlineHandlerMethod, InlineHandlerValue, OptimizedResumeToken, Out,
    ResolvedEffectHandler, ResolvedHandlerMethod, ResumeToken, RuntimeDeclInfo, RuntimeError,
    RuntimeEvaluators, RuntimeHandlerMethod, StoreOp, WithId,
};
use crate::runtime_value::{RuntimeLocals, RuntimeValue, runtime_value_option_eq};
use goby_core::{
    CasePattern, Expr, HandlerClause, ListPatternItem, ListPatternTail, Module, Stmt,
    ast::InterpolatedPart, types::parse_function_type,
};
const ERR_MISSING_MAIN: &str = "Wasm codegen requires a `main` declaration";
pub(crate) const BUILTIN_PRINT: &str = "print";
const PRELUDE_MODULE_PATH: &str = "goby/prelude";
pub(crate) const MAX_EVAL_DEPTH: usize = 32;
const ERR_RESUME_MISSING: &str = "resume used without an active continuation [E-RESUME-MISSING]: `resume` can only be called while executing a handler operation body";
const ERR_RESUME_CONSUMED: &str = "resume continuation already consumed [E-RESUME-CONSUMED]: continuations are one-shot; call `resume` at most once per handled operation";
const ERR_RESUME_STACK_MISMATCH: &str = "internal resume token stack mismatch [E-RESUME-STACK-MISMATCH]: continuation token stack became unbalanced";
const INTERNAL_ABORT_MARKER: &str = "__goby_runtime_abort__";
const ERR_CALLABLE_DISPATCH_DECL_PARAM: &str = "unsupported callable dispatch [E-CALLABLE-DISPATCH]: callable parameter requires a lambda or function name argument";

#[derive(Debug, Clone, PartialEq, Eq)]
/// Error returned by [`compile_module`] when Wasm emission fails.
pub struct CodegenError {
    pub message: String,
}

/// Compile a parsed Goby [`Module`] into a WASI Preview 1 Wasm binary.
///
/// # Errors
///
/// Returns [`CodegenError`] when:
/// - `main` declaration is missing.
/// - `main` body contains constructs that are neither natively lowerable nor
///   resolvable as static print output.
/// - Internal Wasm encoding fails (e.g. string literal too large).
pub fn compile_module(module: &Module) -> Result<Vec<u8>, CodegenError> {
    let Some(main) = module.declarations.iter().find(|d| d.name == "main") else {
        return Err(CodegenError {
            message: ERR_MISSING_MAIN.to_string(),
        });
    };

    let native_attempt = lower::try_emit_native_module_with_handoff(module)?;
    let mut effect_boundary_handoff: Option<lower::EffectBoundaryHandoff> = None;
    match native_attempt {
        lower::NativeLoweringResult::Emitted(wasm) => return Ok(wasm),
        lower::NativeLoweringResult::EffectBoundaryHandoff(handoff) => {
            if handoff.main_style == planning::LoweringStyle::DirectStyle {
                return Err(CodegenError {
                    message:
                        "internal lowering invariant violation: direct-style main produced boundary handoff"
                            .to_string(),
                });
            }
            if let Some(main_req) = handoff.main_requirement
                && main_req.style == planning::LoweringStyle::DirectStyle
                && main_req.passes_evidence
            {
                return Err(CodegenError {
                    message: "internal lowering invariant violation: direct-style main requirement marked as evidence-passing".to_string(),
                });
            }
            effect_boundary_handoff = Some(handoff);
        }
        _ => {}
    }

    let runtime_mode = effect_boundary_handoff
        .as_ref()
        .map(|handoff| handoff.selected_mode)
        .unwrap_or(lower::EffectExecutionMode::PortableFallback);
    if let Some(text) = resolve_main_runtime_output_with_mode(
        module,
        &main.body,
        main.parsed_body.as_deref(),
        runtime_mode,
    ) {
        return compile_print_module(&text);
    }

    if let Some(handoff) = effect_boundary_handoff {
        return Err(CodegenError {
            message: format!(
                "main lowered as effect boundary (style={:?}, selected_mode={:?}, selected_mode_fallback_reason={:?}, runtime_profile={:?}, typed_continuation_ir_present={}, handlers_resume={}, evidence_ops={}, evidence_requirements={}, evidence_fingerprint_hint={}); fallback runtime output could not be resolved",
                handoff.main_style,
                handoff.selected_mode,
                handoff.selected_mode_fallback_reason,
                handoff.runtime_profile,
                handoff.typed_continuation_ir.is_some(),
                handoff.handler_resume_present,
                handoff.evidence_operation_table_len,
                handoff.evidence_requirements_len,
                handoff.evidence_fingerprint_hint,
            ),
        });
    }

    if let Some(reason) = fallback::native_unsupported_reason(module) {
        return Err(CodegenError {
            message: format!(
                "main body contains unsupported constructs that cannot be lowered natively or resolved as static output (native_unsupported_reason={})",
                reason
            ),
        });
    }

    Err(CodegenError {
        message: "main body contains unsupported constructs that cannot be lowered natively or resolved as static output".to_string(),
    })
}

#[cfg(test)]
fn resolve_main_runtime_output(
    module: &Module,
    body: &str,
    parsed_stmts: Option<&[Stmt]>,
) -> Option<String> {
    resolve_main_runtime_output_with_mode(
        module,
        body,
        parsed_stmts,
        lower::EffectExecutionMode::PortableFallback,
    )
}

fn resolve_main_runtime_output_with_mode(
    module: &Module,
    body: &str,
    parsed_stmts: Option<&[Stmt]>,
    execution_mode: lower::EffectExecutionMode,
) -> Option<String> {
    resolve_main_runtime_output_with_mode_and_stdin(
        module,
        body,
        parsed_stmts,
        execution_mode,
        None,
    )
}

fn resolve_main_runtime_output_with_mode_and_stdin(
    module: &Module,
    body: &str,
    parsed_stmts: Option<&[Stmt]>,
    execution_mode: lower::EffectExecutionMode,
    stdin_seed: Option<String>,
) -> Option<String> {
    let int_functions = collect_functions_with_result(module, "Int");
    let list_functions = collect_functions_with_result(module, "List Int");
    let unit_functions = collect_unit_functions(module);
    let int_evaluator = IntEvaluator::root(&int_functions);
    let list_evaluator = ListIntEvaluator::root(
        &list_functions,
        module_has_selective_import_symbol(module, "goby/list", "map"),
    );
    let evaluators = RuntimeEvaluators {
        int: &int_evaluator,
        list: &list_evaluator,
        unit: &unit_functions,
    };
    RuntimeOutputResolver::resolve(
        module,
        body,
        parsed_stmts,
        &evaluators,
        execution_mode,
        stdin_seed,
    )
}

#[cfg(test)]
fn resolve_main_runtime_output_with_stdin(
    module: &Module,
    body: &str,
    parsed_stmts: Option<&[Stmt]>,
    stdin_text: &str,
) -> Option<String> {
    resolve_main_runtime_output_with_mode_and_stdin(
        module,
        body,
        parsed_stmts,
        lower::EffectExecutionMode::PortableFallback,
        Some(stdin_text.to_string()),
    )
}

struct RuntimeOutputResolver<'m> {
    locals: RuntimeLocals,
    module: &'m Module,
    runtime_imports: RuntimeImportContext,
    embedded_effect_runtime: EmbeddedEffectRuntime,
    current_module_stack: Vec<String>,
    current_decl_stack: Vec<String>,
    /// Active inline handlers installed via `with` / `with`.
    active_inline_handler_stack: Vec<InlineHandlerValue>,
    resume_tokens: Vec<ResumeToken>,
    optimized_resume_tokens: Vec<OptimizedResumeToken>,
    /// Phase 4: stack of caller's remaining continuations, one entry per active
    /// execute_ast_stmt_sequence invocation. Top entry is consumed by
    /// begin_handler_continuation_bridge during handler dispatch.
    pending_caller_cont_stack: Vec<Option<Cont>>,
    runtime_error: Option<String>,
    next_with_id: u64,
    execution_mode: lower::EffectExecutionMode,
}

impl<'m> RuntimeOutputResolver<'m> {
    fn apply_receiver_method_value_call_out(
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

    fn eval_imported_decl_as_value_with_args_out(
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
            Out::Done((value, _locals)) => Out::Done(value.unwrap_or(RuntimeValue::Unit)),
            Out::Suspend(cont) => Out::Suspend(cont),
            Out::Escape(escape) => Out::Escape(escape),
            Out::Err(e) => Out::Err(e),
        }
    }

    fn operation_has_conflicting_effect(&self, op_name: &str, expected_effect: &str) -> bool {
        let mut effect_names = self.visible_effect_names_for_operation_in(self.module, op_name);
        if !self.current_module_stack.is_empty() {
            effect_names.extend(self.visible_effect_names_for_operation(op_name));
        }
        effect_names.retain(|name| name != expected_effect);
        !effect_names.is_empty()
    }

    /// Evaluate `expr` and run any suspended `Cont` to completion, returning the result as
    /// `Option<RuntimeValue>`. Replaces `eval_expr_ast_outcome` + `complete_ast_value_outcome`.
    fn eval_expr_to_option(
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

    fn complete_value_out(
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

    // ── Phase 2: New continuation-based eval_expr ─────────────────────────────

    #[allow(dead_code)]
    fn eval_expr(
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

        match expr {
            Expr::IntLit(n) => Out::Done(RuntimeValue::Int(*n)),
            Expr::BoolLit(b) => Out::Done(RuntimeValue::Bool(*b)),
            Expr::StringLit(s) => Out::Done(RuntimeValue::String(s.clone())),
            Expr::Var(name) => match locals.get(name) {
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
                            // Collect currently evaluated items
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

            Expr::Block(stmts) => {
                let mut block_locals = locals.clone();
                let block_callables = callables.clone();
                let mut last_value: Option<RuntimeValue> = None;
                let stmts_slice = stmts.as_slice();
                let mut i = 0;
                while i < stmts_slice.len() {
                    let stmt = &stmts_slice[i];
                    match stmt {
                        Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                            match self.eval_expr(
                                value,
                                &block_locals,
                                &block_callables,
                                evaluators,
                                depth + 1,
                            ) {
                                Out::Done(v) => {
                                    block_locals.store(name, v);
                                    last_value = None;
                                    i += 1;
                                }
                                Out::Suspend(_) => {
                                    let remaining = stmts_slice[i + 1..].to_vec();
                                    return Out::Suspend(Cont::StmtSeq {
                                        store: Some(StoreOp::Bind { name: name.clone() }),
                                        remaining,
                                        locals: block_locals,
                                        callables: block_callables,
                                        depth,
                                        handler_stack: self.active_inline_handler_stack.clone(),
                                        finish: FinishKind::Block,
                                    });
                                }
                                Out::Escape(escape) => return Out::Escape(escape),
                                Out::Err(e) => return Out::Err(e),
                            }
                        }
                        Stmt::Assign { name, value } => {
                            if block_locals.get(name).is_none() {
                                return Out::Err(RuntimeError::Unsupported);
                            }
                            match self.eval_expr(
                                value,
                                &block_locals,
                                &block_callables,
                                evaluators,
                                depth + 1,
                            ) {
                                Out::Done(v) => {
                                    block_locals.store(name, v);
                                    last_value = None;
                                    i += 1;
                                }
                                Out::Suspend(_) => {
                                    let remaining = stmts_slice[i + 1..].to_vec();
                                    return Out::Suspend(Cont::StmtSeq {
                                        store: Some(StoreOp::Assign { name: name.clone() }),
                                        remaining,
                                        locals: block_locals,
                                        callables: block_callables,
                                        depth,
                                        handler_stack: self.active_inline_handler_stack.clone(),
                                        finish: FinishKind::Block,
                                    });
                                }
                                Out::Escape(escape) => return Out::Escape(escape),
                                Out::Err(e) => return Out::Err(e),
                            }
                        }
                        Stmt::Expr(inner_expr) => {
                            match self.eval_expr(
                                inner_expr,
                                &block_locals,
                                &block_callables,
                                evaluators,
                                depth + 1,
                            ) {
                                Out::Done(v) => {
                                    last_value = Some(v);
                                    i += 1;
                                }
                                Out::Suspend(_) => {
                                    let remaining = stmts_slice[i + 1..].to_vec();
                                    return Out::Suspend(Cont::StmtSeq {
                                        store: None,
                                        remaining,
                                        locals: block_locals,
                                        callables: block_callables,
                                        depth,
                                        handler_stack: self.active_inline_handler_stack.clone(),
                                        finish: FinishKind::Block,
                                    });
                                }
                                Out::Escape(escape) => return Out::Escape(escape),
                                Out::Err(e) => return Out::Err(e),
                            }
                        }
                    }
                }
                match last_value {
                    Some(v) => Out::Done(v),
                    None => Out::Done(RuntimeValue::Unit),
                }
            }

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

            Expr::Call { callee, arg } => {
                // Multi-arg named call
                if let Some((fn_name, args)) = flatten_named_call(expr)
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

                // Qualified receiver method call
                if let Expr::Qualified { receiver, member } = callee.as_ref() {
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

                // Single-field constructor
                if let Expr::Var(ctor_name) = callee.as_ref()
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

                // Single-arg named call
                if let Expr::Var(fn_name) = callee.as_ref() {
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

                // Fallback
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

    // ── End Phase 2 ───────────────────────────────────────────────────────────

    fn execute_unit_expr_ast(
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

        // print <arg>
        if let Expr::Call { callee, arg } = expr
            && matches!(callee.as_ref(), Expr::Var(n) if n == BUILTIN_PRINT)
        {
            let value = match self.eval_expr(arg, locals, callables, evaluators, depth) {
                Out::Done(v) => v,
                Out::Suspend(cont) => return Out::Suspend(cont),
                Out::Escape(escape) => return Out::Escape(escape),
                Out::Err(e) => return Out::Err(e),
            };
            self.embedded_effect_runtime
                .emit_output_text(value.to_output_text());
            return Out::Done(());
        }
        if let Expr::Call { callee, arg } = expr
            && matches!(callee.as_ref(), Expr::Var(n) if n == "println")
        {
            let value = match self.eval_expr(arg, locals, callables, evaluators, depth) {
                Out::Done(v) => v,
                Out::Suspend(cont) => return Out::Suspend(cont),
                Out::Escape(escape) => return Out::Escape(escape),
                Out::Err(e) => return Out::Err(e),
            };
            self.embedded_effect_runtime
                .emit_output_line(value.to_output_text());
            return Out::Done(());
        }

        // value |> print
        if let Expr::Pipeline { value, callee } = expr
            && callee == BUILTIN_PRINT
        {
            let v = match self.eval_expr(value, locals, callables, evaluators, depth) {
                Out::Done(v) => v,
                Out::Suspend(cont) => return Out::Suspend(cont),
                Out::Escape(escape) => return Out::Escape(escape),
                Out::Err(e) => return Out::Err(e),
            };
            self.embedded_effect_runtime
                .emit_output_text(v.to_output_text());
            return Out::Done(());
        }

        // Qualified effect call: Effect.method arg  (e.g. Log.log result)
        // Evaluate the argument through the new Out path so suspended values can replay.
        if let Expr::Call { callee, arg } = expr
            && let Expr::Qualified { receiver, member } = callee.as_ref()
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

        // Other expression statements: try AST unit-call path.
        if let Expr::Call { callee, arg } = expr
            && let Expr::Var(fn_name) = callee.as_ref()
        {
            // Callable argument forms that cannot be materialized as RuntimeValue:
            // pass them directly into declaration-local callable environment.
            if let Expr::Lambda { param, body } = arg.as_ref() {
                let callable = IntCallable::AstLambda(Box::new(AstLambdaCallable {
                    parameter: param.clone(),
                    body: (*body.clone()),
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
            if let Expr::Var(arg_name) = arg.as_ref()
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
                && !matches!(arg.as_ref(), Expr::Lambda { .. } | Expr::Var(_))
            {
                self.set_runtime_error_once(ERR_CALLABLE_DISPATCH_DECL_PARAM);
                return Out::Err(RuntimeError::Unsupported);
            }
            // Evaluate arg through the new Out path so suspended values can replay.
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
            // Bare effect method call: e.g. `log env_var`.
            let bare_method = self.find_handler_method_by_name(fn_name);
            if let Some(method) = bare_method {
                return self.dispatch_handler_method(&method, arg_val, evaluators, depth + 1);
            }
            if fn_name == "println" {
                let mut text = arg_val.to_output_text();
                if !text.ends_with('\n') {
                    text.push('\n');
                }
                self.embedded_effect_runtime.emit_output_line(text);
                return Out::Done(());
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
            // Try executing as general declaration for side effects.
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
                Out::Err(_) => {} // fall through to execute_unit_call string-repr path
            };
            let Some(repr) = expr.to_str_repr() else {
                return Out::Err(RuntimeError::Unsupported);
            };
            return self.execute_unit_call_out(&repr, locals, callables, evaluators);
        }

        // Bare literal/var expression: evaluate and discard.
        if let Expr::Var(_) | Expr::IntLit(_) | Expr::StringLit(_) | Expr::BoolLit(_) = expr {
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

        // Fallback: evaluating to a value is fine in unit position (discarded).
        match self.eval_expr(expr, locals, callables, evaluators, depth + 1) {
            Out::Done(_) => return Out::Done(()),
            Out::Suspend(cont) => return Out::Suspend(cont),
            Out::Escape(escape) => return Out::Escape(escape),
            Out::Err(_) => {}
        }

        // Preserve existing call/pipeline string fallback behavior for uncovered forms.
        if let Expr::Call { .. } | Expr::Pipeline { .. } = expr {
            let Some(repr) = expr.to_str_repr() else {
                return Out::Err(RuntimeError::Unsupported);
            };
            return self.execute_unit_call_out(&repr, locals, callables, evaluators);
        }

        Out::Err(RuntimeError::Unsupported)
    }

    /// Find the handler method for a qualified effect call like `Log.log`.
    /// Returns resolved handler info with declaration index.
    fn find_handler_method_for_effect(
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

    /// If `ctor_name` is a record constructor with exactly one field, return that field's name.
    /// Used to support positional single-field constructor sugar: `Ctor(value)` → `Ctor(field: value)`.
    fn single_field_constructor_field(&self, ctor_name: &str) -> Option<String> {
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

    fn eval_zero_arity_decl_value(
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

    fn eval_decl_as_value_with_args_ast(
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
            &Expr::Block(stmts),
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

    fn eval_decl_as_value_with_args_out(
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
            FinishKind::Block,
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

    fn apply_named_value_call_ast(
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
        if fn_name == "println" {
            let mut text = arg_val.to_output_text();
            if !text.ends_with('\n') {
                text.push('\n');
            }
            self.embedded_effect_runtime.emit_output_line(text);
            return Out::Done(RuntimeValue::Unit);
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

    fn apply_named_value_call_out(
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

    fn apply_named_value_call_args_out(
        &mut self,
        fn_name: &str,
        arg_values: &[RuntimeValue],
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Out<RuntimeValue> {
        // Called only when args.len() >= 2 (single-arg paths use apply_named_value_call_out).
        debug_assert!(
            arg_values.len() >= 2,
            "use apply_named_value_call_out for single-arg calls"
        );
        // Out-first path mirrors apply_named_value_call_out ordering.
        // handler dispatch takes priority over __goby_ intrinsics (consistent with single-arg version).
        // 1. handler dispatch
        if let Some(method) = self.find_handler_method_by_name(fn_name) {
            return self.dispatch_handler_method_as_value_with_args_flow(
                &method,
                arg_values,
                evaluators,
                depth + 1,
            );
        }
        // 2. __goby_ intrinsic
        if fn_name.starts_with("__goby_")
            && let Some(value) =
                self.apply_runtime_intrinsic_ast(fn_name, arg_values, evaluators, depth + 1)
        {
            return Out::Done(value);
        }
        // 3. declaration body via Out path
        match self.eval_decl_as_value_with_args_out(fn_name, arg_values, evaluators, depth + 1) {
            Out::Err(RuntimeError::Unsupported) => {}
            other => return other,
        }
        // 4. AST fallback
        self.apply_named_value_call_ast(fn_name, arg_values, locals, callables, evaluators, depth)
    }

    fn apply_binop_runtime_value(
        &self,
        op: goby_core::BinOpKind,
        lv: RuntimeValue,
        rv: RuntimeValue,
    ) -> Option<RuntimeValue> {
        match (lv, rv) {
            (RuntimeValue::Bool(l), RuntimeValue::Bool(r)) => match op {
                goby_core::BinOpKind::And => Some(RuntimeValue::Bool(l && r)),
                goby_core::BinOpKind::Eq => Some(RuntimeValue::Bool(l == r)),
                _ => None,
            },
            (RuntimeValue::Int(l), RuntimeValue::Int(r)) => match op {
                goby_core::BinOpKind::Add => l.checked_add(r).map(RuntimeValue::Int),
                goby_core::BinOpKind::Mul => l.checked_mul(r).map(RuntimeValue::Int),
                goby_core::BinOpKind::Eq => Some(RuntimeValue::Bool(l == r)),
                goby_core::BinOpKind::Lt => Some(RuntimeValue::Bool(l < r)),
                goby_core::BinOpKind::Gt => Some(RuntimeValue::Bool(l > r)),
                _ => None,
            },
            (RuntimeValue::String(l), RuntimeValue::String(r))
                if matches!(op, goby_core::BinOpKind::Eq) =>
            {
                Some(RuntimeValue::Bool(l == r))
            }
            _ => None,
        }
    }
}

fn flatten_direct_call(expr: &Expr) -> Option<(DirectCallHead, Vec<&Expr>)> {
    let mut args = Vec::new();
    let mut cur = expr;
    loop {
        match cur {
            Expr::Call { callee, arg } => {
                args.push(arg.as_ref());
                cur = callee.as_ref();
            }
            Expr::Var(name) => {
                args.reverse();
                return Some((DirectCallHead::Bare(name.clone()), args));
            }
            Expr::Qualified { receiver, member } => {
                args.reverse();
                return Some((
                    DirectCallHead::Qualified {
                        receiver: receiver.clone(),
                        member: member.clone(),
                    },
                    args,
                ));
            }
            _ => return None,
        }
    }
}

fn module_has_selective_import_symbol(module: &Module, module_path: &str, symbol: &str) -> bool {
    module.imports.iter().any(|import| {
        if import.module_path != module_path {
            return false;
        }
        match &import.kind {
            goby_core::ImportKind::Selective(selected) => {
                selected.iter().any(|name| name == symbol)
            }
            _ => false,
        }
    })
}

fn parse_pipeline(expr: &str) -> Option<(&str, &str)> {
    let (left, right) = expr.split_once("|>")?;
    let left = left.trim();
    let right = right.trim();
    if left.is_empty() || !is_identifier(right) {
        return None;
    }
    Some((left, right))
}

fn eval_string_expr(expr: &str, locals: &HashMap<String, String>) -> Option<String> {
    let expr = expr.trim();

    if is_string_literal(expr) {
        return Some(expr[1..expr.len() - 1].to_string());
    }

    if is_identifier(expr) {
        return locals.get(expr).cloned();
    }

    None
}

fn compile_print_module(text: &str) -> Result<Vec<u8>, CodegenError> {
    backend::WasmProgramBuilder::new(layout::MemoryLayout::default()).emit_static_print_module(text)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::sync::Mutex;

    use goby_core::parse_module;

    use super::*;

    /// Serializes tests that read or write process-wide environment variables.
    static ENV_MUTEX: Mutex<()> = Mutex::new(());

    fn assert_valid_wasm_module(wasm: &[u8]) {
        assert!(wasm.len() >= 8, "module too short: {} bytes", wasm.len());
        assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d], "bad wasm magic");
        assert_eq!(&wasm[4..8], &[0x01, 0x00, 0x00, 0x00], "bad wasm version");
    }

    fn read_example(name: &str) -> String {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("..");
        path.push("..");
        path.push("examples");
        path.push(name);
        std::fs::read_to_string(path).expect("example file should exist")
    }

    fn main_body(module: &Module) -> &str {
        module
            .declarations
            .iter()
            .find(|decl| decl.name == "main")
            .map(|decl| decl.body.as_str())
            .expect("main should exist")
    }

    fn main_parsed_body(module: &Module) -> Option<&[Stmt]> {
        module
            .declarations
            .iter()
            .find(|decl| decl.name == "main")
            .and_then(|decl| decl.parsed_body.as_deref())
    }

    fn runtime_output_for_mode(
        module: &Module,
        mode: lower::EffectExecutionMode,
    ) -> Option<String> {
        resolve_main_runtime_output_with_mode(
            module,
            main_body(module),
            main_parsed_body(module),
            mode,
        )
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct ParityOutcome {
        stdout: Option<String>,
        runtime_error_kind: Option<&'static str>,
    }

    fn parity_outcome_for_mode(module: &Module, mode: lower::EffectExecutionMode) -> ParityOutcome {
        parity_outcome_from_runtime_output(runtime_output_for_mode(module, mode))
    }

    fn parity_outcome_from_runtime_output(output: Option<String>) -> ParityOutcome {
        let Some(text) = output else {
            return ParityOutcome {
                stdout: None,
                runtime_error_kind: None,
            };
        };
        let mut lines = text.lines().map(str::to_string).collect::<Vec<_>>();
        if let Some(last) = lines.last()
            && let Some(kind) = runtime_error_kind_from_output_line(last)
        {
            lines.pop();
            let stdout = if lines.is_empty() {
                None
            } else {
                Some(lines.join("\n"))
            };
            return ParityOutcome {
                stdout,
                runtime_error_kind: Some(kind),
            };
        }
        ParityOutcome {
            stdout: Some(text),
            runtime_error_kind: None,
        }
    }

    fn runtime_error_kind_from_output_line(line: &str) -> Option<&'static str> {
        let msg = line.strip_prefix("runtime error: ")?;
        if msg.contains("[E-RESUME-MISSING]")
            || msg.starts_with("resume used without an active continuation")
        {
            return Some("continuation_missing");
        }
        if msg.contains("[E-RESUME-CONSUMED]")
            || msg.starts_with("resume continuation already consumed")
        {
            return Some("continuation_consumed");
        }
        if msg.contains("[E-RESUME-HANDLER-MISMATCH]")
            || msg.starts_with("internal resume token handler mismatch")
        {
            return Some("token_handler_mismatch");
        }
        if msg.contains("[E-RESUME-STACK-MISMATCH]")
            || msg.starts_with("internal resume token stack mismatch")
        {
            return Some("token_stack_mismatch");
        }
        Some("unknown_runtime_error")
    }

    #[derive(Debug, Clone, Copy)]
    struct PerfStats {
        p50_micros: u128,
        p95_micros: u128,
    }

    fn measure_runtime_mode_micros(
        module: &Module,
        mode: lower::EffectExecutionMode,
        warmup_runs: usize,
        measured_runs: usize,
    ) -> PerfStats {
        for _ in 0..warmup_runs {
            let _ = runtime_output_for_mode(module, mode);
        }
        let mut samples = Vec::with_capacity(measured_runs);
        for _ in 0..measured_runs {
            let start = std::time::Instant::now();
            let _ = runtime_output_for_mode(module, mode);
            samples.push(start.elapsed().as_micros());
        }
        samples.sort_unstable();
        PerfStats {
            p50_micros: percentile_micros(&samples, 50),
            p95_micros: percentile_micros(&samples, 95),
        }
    }

    fn percentile_micros(sorted_samples: &[u128], percentile: usize) -> u128 {
        assert!(!sorted_samples.is_empty(), "samples must not be empty");
        assert!(percentile <= 100, "percentile out of range");
        let n = sorted_samples.len();
        let rank = ((n - 1) * percentile) / 100;
        sorted_samples[rank]
    }

    fn assert_perf_within_threshold(
        sample_name: &str,
        fallback: PerfStats,
        typed: PerfStats,
        max_slowdown_ratio: f64,
    ) {
        let p50_ratio = if fallback.p50_micros == 0 {
            1.0
        } else {
            typed.p50_micros as f64 / fallback.p50_micros as f64
        };
        let p95_ratio = if fallback.p95_micros == 0 {
            1.0
        } else {
            typed.p95_micros as f64 / fallback.p95_micros as f64
        };
        assert!(
            p50_ratio <= max_slowdown_ratio,
            "sample `{}` exceeded p50 slowdown threshold: fallback={}us typed={}us ratio={:.4} limit={:.4}",
            sample_name,
            fallback.p50_micros,
            typed.p50_micros,
            p50_ratio,
            max_slowdown_ratio
        );
        assert!(
            p95_ratio <= max_slowdown_ratio,
            "sample `{}` exceeded p95 slowdown threshold: fallback={}us typed={}us ratio={:.4} limit={:.4}",
            sample_name,
            fallback.p95_micros,
            typed.p95_micros,
            p95_ratio,
            max_slowdown_ratio
        );
    }

    fn assert_mode_parity(module: &Module, context: &str) -> ParityOutcome {
        // Step 8.5 note:
        // parity checks must stay green even though modes now use distinct continuation
        // token bridges internally.
        let fallback =
            parity_outcome_for_mode(module, lower::EffectExecutionMode::PortableFallback);
        let typed = parity_outcome_for_mode(
            module,
            lower::EffectExecutionMode::TypedContinuationOptimized,
        );
        assert_ne!(
            fallback.runtime_error_kind,
            Some("unknown_runtime_error"),
            "fallback produced unmapped runtime error kind in {}",
            context
        );
        assert_ne!(
            typed.runtime_error_kind,
            Some("unknown_runtime_error"),
            "typed mode produced unmapped runtime error kind in {}",
            context
        );
        assert_eq!(typed, fallback, "mode parity mismatch in {}", context);
        typed
    }

    #[test]
    fn emits_valid_wasm_for_long_print_literal() {
        let long_text = "x".repeat(128);
        let source = format!("main : Unit -> Unit\nmain = print \"{}\"\n", long_text);
        let module = parse_module(&source).expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn emits_valid_wasm_for_print_via_local_binding() {
        let source = r#"
main : Unit -> Unit
main =
  greeting = "Hello from local"
  print greeting
"#;
        let module = parse_module(source).expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn emits_valid_wasm_for_print_int_binding() {
        let source = r#"
main : Unit -> Unit
main =
  n = 10
  print n
"#;
        let module = parse_module(source).expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn resolves_runtime_output_for_pipeline_print() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  [1, 2, 3] |> print
"#;
        let module = parse_module(source).expect("parse should work");
        assert!(
            main_parsed_body(&module).is_some(),
            "main parsed_body should exist for case arm block source"
        );
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[1, 2, 3]");
    }

    #[test]
    fn resolves_runtime_output_for_interpolated_string_with_mixed_values() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  n = 42
  greeting = "Goby"
  print "value=${n}, hello=${greeting}"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "value=42, hello=Goby");
    }

    #[test]
    fn interpolated_string_replays_handled_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print "value=${next 0}"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "value=1");
    }

    #[test]
    fn typed_mode_matches_fallback_for_interpolated_string_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    print "value=${next 0}"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "interpolated string replay");
        assert_eq!(typed.stdout.as_deref(), Some("value=2"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn locks_runtime_output_for_function_example() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = read_example("function.gb");
        let module = parse_module(&source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "90[30, 40, 50][60, 70]something15");
    }

    #[test]
    fn resolves_runtime_output_for_function_argument_call() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
callback_after_print : (Int -> Int) -> Unit
callback_after_print f =
  print "something"
  i = f 10
  print i

main : Unit -> Unit
main =
  callback_after_print (|n| -> n + 5)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "something15");
    }

    #[test]
    fn resolves_runtime_output_for_unit_callback_argument_inline_lambda() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

main : Unit -> Unit
main =
  each_two (|n| -> print "${n}")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "12");
    }

    #[test]
    fn resolves_runtime_output_for_unit_callback_argument_named_function() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
log_num : Int -> Unit
log_num n =
  print "${n}"

each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

main : Unit -> Unit
main =
  each_two log_num
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "12");
    }

    #[test]
    fn resolves_runtime_output_for_unit_callback_argument_forwarded_alias() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
log_num : Int -> Unit
log_num n =
  print "${n}"

each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

wrapper : (Int -> Unit) -> Unit
wrapper g =
  each_two g

main : Unit -> Unit
main =
  wrapper log_num
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "12");
    }

    #[test]
    fn resolves_runtime_output_for_unit_callback_argument_inline_lambda_with_capture() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

main : Unit -> Unit
main =
  base = 40
  each_two (|n| -> print "${n + base}")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "4142");
    }

    #[test]
    fn resolves_runtime_output_for_list_each_style_callback_dispatch() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect ListYield
  yield : Int -> Bool

iter : List Int -> Unit can ListYield
iter xs =
  case xs
    [] -> ()
    [x, ..xxs] ->
      if yield x
        iter xxs
      else
        ()

each : List Int -> (Int -> Unit) -> Unit
each xs f =
  emit_handler = handler
    yield x ->
      f x
      resume True
  with emit_handler
  in
    iter xs

main : Unit -> Unit
main =
  each [3, 5] (|n| -> print "${n}")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "35");
    }

    #[test]
    fn resolves_runtime_output_for_list_each_with_plain_import() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/list

main : Unit -> Unit
main =
  list.each [2, 4] (|n| -> print "${n}")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "24");
    }

    #[test]
    fn resolves_runtime_output_for_list_each_with_alias_import() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/list as l

main : Unit -> Unit
main =
  l.each [6, 8] (|n| -> print "${n}")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "68");
    }

    #[test]
    fn resolves_runtime_output_for_list_each_with_selective_import() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/list ( each )

main : Unit -> Unit
main =
  each [10, 12] (|n| -> print "${n}")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "1012");
    }

    #[test]
    fn resolves_runtime_output_for_effectful_callback_with_list_each_plain_import() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/list

effect Log
  log : Int -> Unit

main : Unit -> Unit
main =
  with
    log n ->
      print "${n}"
      resume ()
  in
    list.each [1, 3] (|n| -> log n)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "13");
    }

    #[test]
    fn resolves_runtime_output_for_effectful_callback_with_list_each_alias_import() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/list as l

effect Log
  log : Int -> Unit

main : Unit -> Unit
main =
  with
    log n ->
      print "${n}"
      resume ()
  in
    l.each [5, 7] (|n| -> log n)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "57");
    }

    #[test]
    fn resolves_runtime_output_for_effectful_callback_with_list_each_selective_import() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/list ( each )

effect Log
  log : Int -> Unit

main : Unit -> Unit
main =
  with
    log n ->
      print "${n}"
      resume ()
  in
    each [9, 11] (|n| -> log n)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "911");
    }

    #[test]
    fn resolves_runtime_output_for_list_each_callback_with_bare_println() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/list

main : Unit -> Unit can Print
main =
  ns = [1, 2, 3]
  list.each ns (|i| -> println(i * 10))
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "10\n20\n30\n");
    }

    #[test]
    fn resolves_runtime_output_for_list_each_with_string_callback() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/list

main : Unit -> Unit can Print
main =
  list.each ["go", "by"] (|s| -> println s)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "go\nby\n");
    }

    #[test]
    fn resolves_runtime_output_for_imported_list_map_with_ints() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/list ( map )

main : Unit -> Unit
main =
  print (map [1, 2, 3] (|n| -> n * 10))
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[10, 20, 30]");
    }

    #[test]
    fn resolves_runtime_output_for_imported_list_map_with_strings() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/list

main : Unit -> Unit
main =
  print (list.map ["go", "by"] (|s| -> "${s}!"))
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[\"go!\", \"by!\"]");
    }

    #[test]
    fn reports_callable_dispatch_error_for_list_each_non_callable_callback() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/list

main : Unit -> Unit
main =
  list.each [1, 2] 1
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some(
                "runtime error: unsupported callable dispatch [E-CALLABLE-DISPATCH]: callable parameter requires a lambda or function name argument"
            )
        );
    }

    #[test]
    fn reports_callable_dispatch_error_for_decl_callable_param_non_callable_arg() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

main : Unit -> Unit
main =
  each_two 1
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some(
                "runtime error: unsupported callable dispatch [E-CALLABLE-DISPATCH]: callable parameter requires a lambda or function name argument"
            )
        );
    }

    #[test]
    fn resolves_runtime_output_for_intrinsic_string_length_call() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  print (__goby_string_length "hello")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "5");
    }

    #[test]
    fn resolves_runtime_output_for_intrinsic_env_fetch_call() {
        let _guard = ENV_MUTEX.lock().unwrap();
        // SAFETY: serialized by ENV_MUTEX; no concurrent env access while lock is held.
        unsafe { std::env::set_var("GOBY_INTRINSIC_TEST_PATH", "intrinsic-ok") };
        let source = r#"
main : Unit -> Unit
main =
  print (__goby_env_fetch_env_var "GOBY_INTRINSIC_TEST_PATH")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        // Clean up before asserting so env is restored even if the assertion panics.
        unsafe { std::env::remove_var("GOBY_INTRINSIC_TEST_PATH") };
        assert_eq!(output, "intrinsic-ok");
    }

    #[test]
    fn runtime_resolves_goby_env_fetch_via_imported_decl() {
        let _guard = ENV_MUTEX.lock().unwrap();
        // SAFETY: serialized by ENV_MUTEX; no concurrent env access while lock is held.
        unsafe { std::env::set_var("GOBY_ENV_BRIDGE_TEST_PATH", "env-bridge-ok") };
        let source = r#"
import goby/env ( fetch_env_var )
main : Unit -> Unit
main =
  print (fetch_env_var "GOBY_ENV_BRIDGE_TEST_PATH")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        unsafe { std::env::remove_var("GOBY_ENV_BRIDGE_TEST_PATH") };
        assert_eq!(output, "env-bridge-ok");
    }

    #[test]
    fn runtime_resolves_goby_string_length_via_qualified_imported_decl() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/string
main : Unit -> Unit
main =
  print (string.length "hello")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "5");
    }

    #[test]
    fn runtime_resolves_goby_string_length_via_selective_imported_decl() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/string ( length )
main : Unit -> Unit
main =
  print (length "hello")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "5");
    }

    #[test]
    fn runtime_resolves_goby_int_parse_via_selective_imported_declaration_execution() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/int ( parse )

effect StringParseError
  invalid_integer : String -> Int

main : Unit -> Unit can Print, StringParseError
main =
  with
    invalid_integer _ ->
      resume -1
  in
    print parse("42")
    print parse("x")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(output.as_deref(), Some("42-1"));
    }

    #[test]
    fn resolves_runtime_output_for_intrinsic_each_grapheme_count_mode_unified_contract() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/iterator

effect Iterator a b
  yield : a -> b -> (Bool, b)

main : Unit -> Unit can Print
main =
  with
    yield _ _ ->
      resume (True, ())
  in
    n = __goby_string_each_grapheme "a👨‍👩‍👧‍👦b"
    print n
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "3");
    }

    #[test]
    fn resolves_runtime_output_for_intrinsic_each_grapheme_unified_iterator_contract() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
type GraphemeState = GraphemeState(grapheme: String, current: String)

effect Iterator a b
  yield : a -> b -> (Bool, b)

main : Unit -> Unit can Print
main =
  state = GraphemeState(grapheme: "", current: "")
  out = state
  with
    yield grapheme step ->
      next = "${step.current}${grapheme}"
      resume (True, GraphemeState(grapheme: grapheme, current: next))
  in
    out = __goby_string_each_grapheme "a👨‍👩‍👧‍👦b" state
  print out.current
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "a👨\u{200d}👩\u{200d}👧\u{200d}👦b");
    }

    #[test]
    fn resolves_runtime_output_for_intrinsic_each_grapheme_unified_early_stop() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

main : Unit -> Unit can Print
main =
  with
    yield _ _ ->
      resume (False, ())
  in
    n = __goby_string_each_grapheme "abc"
    print n
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "1");
    }

    #[test]
    fn resolves_runtime_output_for_multi_arg_effect_op_call() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

main : Unit -> Unit can Print
main =
  with
    yield _ step ->
      resume (True, step + 1)
  in
    print (yield "x" 41)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "(True, 42)");
    }

    #[test]
    fn resolves_runtime_output_for_string_equality_operator() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  print ("alpha" == "alpha")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "True");
    }

    #[test]
    fn resolves_runtime_output_for_unit_literal_value() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main = ()
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert!(
            output.is_none(),
            "unit literal main without print should produce no runtime output"
        );
    }

    #[test]
    fn resolves_runtime_output_for_tuple_member_access_by_index() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  pair = (True, 42)
  if pair.0
    print pair.1
  else
    print 0
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "42");
    }

    #[test]
    fn tuple_literal_replays_handled_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    pair = (next 0, 2)
    print pair.0
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "1");
    }

    #[test]
    fn typed_mode_matches_fallback_for_tuple_literal_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    pair = (next 0, 2)
    print pair.0
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "tuple literal replay");
        assert_eq!(typed.stdout.as_deref(), Some("2"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn rejects_runtime_output_for_numeric_member_on_non_tuple_receiver() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  print Status.0
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert!(
            output.is_none(),
            "numeric member access on non-tuple receiver should not produce runtime output"
        );
    }

    #[test]
    fn resolves_runtime_output_for_intrinsic_list_push_string_call() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  xs = __goby_list_push_string [] "a"
  ys = __goby_list_push_string xs "b"
  print ys
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[\"a\", \"b\"]");
    }

    #[test]
    fn locks_runtime_output_for_effect_gb() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // SAFETY: serialized by ENV_MUTEX; no concurrent env access while lock is held.
        unsafe { std::env::set_var("GOBY_PATH", "hello") };
        let source = read_example("effect.gb");
        let module = parse_module(&source).expect("effect.gb should parse");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        // Clean up before asserting so env is restored even if the assertion panics.
        unsafe { std::env::remove_var("GOBY_PATH") };
        assert_eq!(output, "13donedevelopment");
    }

    #[test]
    fn locks_runtime_output_for_iterator_gb() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = read_example("iterator.gb");
        let module = parse_module(&source).expect("iterator.gb should parse");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "tick:atick:btick:c");
    }

    #[test]
    fn locks_runtime_output_for_iterator_unified_gb() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = read_example("iterator_unified.gb");
        let module = parse_module(&source).expect("iterator_unified.gb should parse");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "tick:atick:btick:c31");
    }

    #[test]
    fn case_with_no_matching_arm_produces_no_output() {
        // When no case arm matches and there is no wildcard, resolve_main_runtime_output
        // returns None (silent — no output emitted).
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
describe_number : Int -> String
describe_number n =
  n
    case 1 -> "one"
    case 2 -> "two"

main : Unit -> Unit
main =
  r = describe_number 99
  print r
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        // 99 matches no arm → eval returns None → print has no argument → output is None.
        assert!(
            output.is_none(),
            "case with no matching arm should produce no runtime output"
        );
    }

    #[test]
    fn resolves_runtime_output_for_case_list_pattern_tail_binding() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  xs = [1, 2, 3]
  print
    case xs
      [] -> []
      [x, ..xxs] -> xxs
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[2, 3]");
    }

    #[test]
    fn resolves_runtime_output_for_case_list_pattern_wildcard_head() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  xs = [1, 2, 3]
  print
    case xs
      [] -> []
      [_, ..tail] -> tail
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[2, 3]");
    }

    #[test]
    fn resolves_runtime_output_for_case_arm_block_body() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  x = 0
  print
    case x
      0 ->
        y = 1
        y + 10
      _ -> 0
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert!(
            output.is_some(),
            "runtime output should resolve; parsed={:#?}",
            main_parsed_body(&module)
        );
        let output = output.expect("checked Some above");
        assert_eq!(output, "11");
    }

    #[test]
    fn resolves_runtime_output_for_case_value_bound_then_printed() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit can Print
main =
  a = 10
  b = case a
    10 ->
      1 + 10
    _ ->
      20 + 30
  print b
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(output, Some("11".to_string()));
    }

    #[test]
    fn resolves_runtime_output_for_standalone_case_with_effectful_arm_bodies() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit can Print
main =
  a = 10
  case a
    10 ->
      print "Ten"
    _ ->
      print "Other"
"#;
        let module = parse_module(source).expect("parse should work");
        assert!(
            main_parsed_body(&module).is_some(),
            "parsed_body should exist"
        );
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(output, Some("Ten".to_string()));
    }

    #[test]
    fn resolves_runtime_output_for_all_list_pattern_forms() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  xs0 = []
  print
    case xs0
      [] -> "Empty list"
      _ -> "Some other list"
  xs1 = [1]
  print
    case xs1
      [1] -> "List of just 1"
      _ -> "Some other list"
  xs2 = [4, 9, 10]
  print
    case xs2
      [4, ..] -> "List starting with 4"
      _ -> "Some other list"
  xs3 = [2, 3, 5]
  print
    case xs3
      [a, ..b] -> "List of at least 1 elements with binding"
      _ -> "Some other list"
  xs4 = [9, 8]
  print
    case xs4
      [_, _] -> "List of 2 elements"
      _ -> "Some other list"
  xs5 = [0]
  print
    case xs5
      [_, _] -> "List of 2 elements"
      _ -> "Some other list"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(
            output,
            "Empty listList of just 1List starting with 4List of at least 1 elements with bindingList of 2 elementsSome other list"
        );
    }

    #[test]
    fn resolves_runtime_output_for_prefix_list_pattern_semantics() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  xs = [1, 2]
  print
    case xs
      [1] -> "prefix"
      _ -> "other"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "prefix");
    }

    // --- bare effect call dispatch in main body (§4.1 patch) ---

    #[test]
    fn bare_effect_call_in_main_with_dispatches_to_handler() {
        // Bug: eval_ast_side_effect (main/top-level path) did NOT route bare Call through
        // find_handler_method_by_name, so `log "hello"` inside `with` produced no output.
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    log "hello"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("hello"),
            "bare effect call inside main `with` block should dispatch to handler"
        );
    }

    #[test]
    fn qualified_effect_call_in_main_with_dispatches_to_handler() {
        // Bug: eval_ast_side_effect had no arm for Expr::Call { callee: Expr::Qualified },
        // so `Log.log "hello"` inside main body fell through to string-based eval_side_effect.
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    Log.log "world"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("world"),
            "qualified effect call inside main `with` block should dispatch to handler"
        );
    }

    #[test]
    fn bare_call_without_handler_falls_through_to_existing_path() {
        // When no active handler matches the bare name, dispatch must fall through to
        // execute_unit_call_ast / execute_decl_as_side_effect (existing behaviour preserved).
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
greet : String -> Unit
greet name =
  print name

main : Unit -> Unit
main =
  greet "fallback"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("fallback"),
            "bare call without active handler should fall through to unit-call path"
        );
    }

    #[test]
    fn explicit_handler_overrides_embedded_default_handler() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Print
  print: String -> Unit
  println: String -> Unit

@embed Print __goby_embeded_effect_stdout_handler

main : Unit -> Unit can Print
main =
  with
    print msg ->
      resume ()
    println msg ->
      resume ()
  in
    Print.println "fallback"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            None,
            "explicit handler must win over embedded default handler for println"
        );
    }

    #[test]
    fn embedded_default_handler_handles_print_without_explicit_handler() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Print
  print: String -> Unit
  println: String -> Unit

@embed Print __goby_embeded_effect_stdout_handler

main : Unit -> Unit can Print
main =
  Print.print "fallback"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("fallback"),
            "embedded default handler should handle Print.print when no explicit handler exists"
        );
    }

    #[test]
    fn embedded_default_handler_handles_println_without_explicit_handler() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Print
  print: String -> Unit
  println: String -> Unit

@embed Print __goby_embeded_effect_stdout_handler

main : Unit -> Unit can Print
main =
  Print.println "fallback"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("fallback\n"),
            "embedded default handler should ensure trailing newline for Print.println"
        );
    }

    #[test]
    fn embedded_default_handler_println_keeps_existing_trailing_newline() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Print
  print: String -> Unit
  println: String -> Unit

@embed Print __goby_embeded_effect_stdout_handler

main : Unit -> Unit can Print
main =
  Print.println "fallback\n"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("fallback\n"),
            "embedded default handler should not duplicate trailing newline for Print.println"
        );
    }

    #[test]
    fn embedded_default_handler_is_loaded_from_implicit_prelude() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit can Print
main =
  Print.print "from-prelude"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("from-prelude"),
            "embedded default handler should be discoverable via implicit prelude import"
        );
    }

    #[test]
    fn embedded_println_handler_is_loaded_from_implicit_prelude() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit can Print
main =
  Print.println "from-prelude"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("from-prelude\n"),
            "embedded default handler should support Print.println via implicit prelude import"
        );
    }

    #[test]
    fn embedded_read_handler_is_loaded_from_implicit_prelude() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit can Print, Read
main =
  line = Read.read_line ()
  tail = Read.read ()
  tail2 = Read.read ()
  print "line=${line}"
  print "tail=${tail}"
  print "tail2=${tail2}"
"#;
        let module = parse_module(source).expect("parse should work");
        let output = resolve_main_runtime_output_with_stdin(
            &module,
            main_body(&module),
            main_parsed_body(&module),
            "alpha\nbeta\ngamma",
        );
        assert_eq!(
            output.as_deref(),
            Some("line=alphatail=beta\ngammatail2="),
            "implicit prelude embedded Read handler should serve read_line/read and consume stdin"
        );
    }

    #[test]
    fn embedded_read_handler_supports_bare_calls_via_implicit_prelude() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit can Print, Read
main =
  line = read_line ()
  tail = read ()
  print "line=${line}"
  print "tail=${tail}"
"#;
        let module = parse_module(source).expect("parse should work");
        let output = resolve_main_runtime_output_with_stdin(
            &module,
            main_body(&module),
            main_parsed_body(&module),
            "alpha\nbeta",
        );
        assert_eq!(
            output.as_deref(),
            Some("line=alphatail=beta"),
            "bare read_line/read should resolve through implicit prelude embedded Read"
        );
    }

    #[test]
    fn spaced_unit_argument_read_line_is_typechecked_and_runs() {
        use goby_core::{parse_module, typecheck::typecheck_module};
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit can Print, Read
main =
  line = read_line ()
  print "line=${line}"
"#;
        let module = parse_module(source).expect("parse should work");
        typecheck_module(&module).expect("read_line () should typecheck as Unit-arg call");
        let output = resolve_main_runtime_output_with_stdin(
            &module,
            main_body(&module),
            main_parsed_body(&module),
            "alpha\n",
        );
        assert_eq!(
            output.as_deref(),
            Some("line=alpha"),
            "read_line () should be parsed and executed as a Unit-arg call"
        );
    }

    #[test]
    fn parenthesized_unit_argument_read_calls_run_via_fallback_runtime() {
        use goby_core::{parse_module, typecheck::typecheck_module};
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit can Print, Read
main =
  line = read_line()
  tail = read()
  print "line=${line}"
  print "tail=${tail}"
"#;
        let module = parse_module(source).expect("parse should work");
        typecheck_module(&module).expect("read_line()/read() should typecheck as Unit-arg calls");
        let output = resolve_main_runtime_output_with_stdin(
            &module,
            main_body(&module),
            main_parsed_body(&module),
            "alpha\nbeta",
        );
        assert_eq!(
            output.as_deref(),
            Some("line=alphatail=beta"),
            "read_line()/read() should execute through fallback runtime as Unit-arg calls"
        );
    }

    #[test]
    fn parenthesized_unit_argument_qualified_read_calls_run_via_fallback_runtime() {
        use goby_core::{parse_module, typecheck::typecheck_module};
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit can Print, Read
main =
  line = Read.read_line()
  tail = Read.read()
  print "line=${line}"
  print "tail=${tail}"
"#;
        let module = parse_module(source).expect("parse should work");
        typecheck_module(&module)
            .expect("qualified read_line()/read() should typecheck as Unit-arg calls");
        let output = resolve_main_runtime_output_with_stdin(
            &module,
            main_body(&module),
            main_parsed_body(&module),
            "alpha\nbeta",
        );
        assert_eq!(
            output.as_deref(),
            Some("line=alphatail=beta"),
            "Read.read_line()/Read.read() should execute as Unit-arg calls"
        );
    }

    #[test]
    fn parenthesized_unit_argument_user_defined_function_runs() {
        use goby_core::{parse_module, typecheck::typecheck_module};
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
value : Unit -> String
value = "ok"

main : Unit -> Unit can Print
main =
  print value()
"#;
        let module = parse_module(source).expect("parse should work");
        typecheck_module(&module).expect("user-defined value() should typecheck as Unit-arg call");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "ok");
    }

    #[test]
    fn embedded_read_line_trims_lf_crlf_and_cr() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit can Print, Read
main =
  a = Read.read_line ()
  b = Read.read_line ()
  c = Read.read_line ()
  d = Read.read_line ()
  e = Read.read_line ()
  print "a=${a}"
  print "b=${b}"
  print "c=${c}"
  print "d=${d}"
  print "e=${e}"
"#;
        let module = parse_module(source).expect("parse should work");
        let output = resolve_main_runtime_output_with_stdin(
            &module,
            main_body(&module),
            main_parsed_body(&module),
            "a\r\nb\nc\rd",
        );
        assert_eq!(
            output.as_deref(),
            Some("a=ab=bc=cd=de="),
            "read_line should trim CRLF/LF/CR and return empty string at EOF"
        );
    }

    #[test]
    fn runtime_resolves_goby_int_parse_via_module_alias_imported_declaration_execution() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/int as int
main : Unit -> Unit can Print, StringParseError
main =
  with
    invalid_integer _ ->
      resume -1
  in
    print int.parse("42")
    print int.parse("-7")
    print int.parse("12x")
    print int.parse("9223372036854775808")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(output.as_deref(), Some("42-7-1-1"));
    }

    #[test]
    fn runtime_reports_unhandled_invalid_integer_from_goby_int_parse() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/int
main : Unit -> Unit can Print, StringParseError
main =
  print int.parse("x")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("runtime error: unhandled effect operation `invalid_integer` from goby/int.parse")
        );
    }

    #[test]
    fn runtime_does_not_dispatch_invalid_integer_to_other_effect_same_op_name() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
import goby/int as int
effect OtherError
  invalid_integer : String -> Int
main : Unit -> Unit can Print, StringParseError, OtherError
main =
  with
    invalid_integer _ ->
      resume -99
  in
    print int.parse("x")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("runtime error: unhandled effect operation `invalid_integer` from goby/int.parse")
        );
    }

    #[test]
    fn two_handlers_with_same_method_name_dispatches_to_nearest_handler() {
        // When two active handlers both provide a method with the same bare name,
        // lexical stack order should win (nearest/enclosed handler first).
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Alpha
  greet: String -> String

effect Beta
  greet: String -> String

main : Unit -> Unit
main =
  with
    greet s ->
      resume "from-alpha"
  in
    with
      greet s ->
        resume "from-beta"
    in
      print (greet "x")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("from-beta"),
            "bare-name dispatch should choose the nearest active handler in lexical stack order"
        );
    }

    #[test]
    fn nested_with_same_effect_prefers_inner_handler() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "outer"
      resume ()
  in
    with
      log msg ->
        print "inner"
        resume ()
    in
      log "x"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("inner"),
            "nested `with` should dispatch to nearest enclosing handler"
        );
    }

    #[test]
    fn pipeline_effect_call_in_main_with_dispatches_to_handler() {
        // `"msg" |> log` inside main `with` block should dispatch to the active handler.
        // The Pipeline arm in eval_ast_side_effect now checks find_handler_method_by_name.
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    "piped" |> log
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("piped"),
            "pipeline effect call inside main `with` block should dispatch to handler"
        );
    }

    #[test]
    fn qualified_effect_call_without_active_handler_falls_through() {
        // `Log.log "x"` when no handler is active → no output (silent fallthrough to string path).
        // This documents the expected behaviour: no crash, no output.
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  Log.log "unreachable"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert!(
            output.is_none(),
            "qualified effect call with no active handler should produce no output"
        );
    }

    #[test]
    fn positional_single_field_constructor_dispatches_handler() {
        // Bug (BUG-002): `raise Error("msg")` parsed as Expr::Call not RecordConstruct,
        // so the handler received a wrong value and produced no output.
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
type Error = Error(message: String)

effect RaiseError
  raise: Error -> Unit

main : Unit -> Unit
main =
  with
    raise e ->
      print e.message
      resume ()
  in
    raise Error("oops")
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("oops"),
            "positional single-field constructor should dispatch handler with correct record value"
        );
    }

    #[test]
    fn resume_in_handler_returns_value_to_effect_call_site() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume 7
  in
    print (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("7"),
            "resume should return value to the operation call site"
        );
    }

    #[test]
    fn one_shot_resume_guard_rejects_second_resume_on_same_token() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (resume 1)
  in
    print (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref().map(|s| s.contains("[E-RESUME-CONSUMED]")),
            Some(true),
            "second resume on one-shot token should surface a deterministic runtime error"
        );
    }

    #[test]
    fn no_resume_in_value_position_exits_current_with_scope() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  result =
    with
      next n ->
        print "handled"
        42
    in
      next 0 + 1
  print result
  print "after"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("handled42after"),
            "no-resume in value position should exit only the current with body and yield the clause result"
        );
    }

    #[test]
    fn no_resume_in_unit_position_skips_remaining_with_body_only() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "handled:${msg}"
  in
    log "hello"
    print "inner-after"
  print "outer-after"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("handled:helloouter-after"),
            "no-resume in unit position should skip only the remaining statements in the current with body"
        );
    }

    #[test]
    fn nested_scoped_exit_only_leaves_inner_with_scope() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Outer
  op: String -> Unit

effect Inner
  boom: String -> Int

main : Unit -> Unit
main =
  with
    op msg ->
      inner =
        with
          boom inner ->
            print "inner:${inner}"
            7
        in
          boom msg + 1
      print "outer:${inner}"
      resume ()
  in
    op "x"
    print "main-after"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("inner:xouter:7main-after"),
            "nested no-resume should exit only the targeted inner with scope and let outer execution continue"
        );
    }

    #[test]
    fn resume_outside_handler_surfaces_runtime_error() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
main : Unit -> Unit
main =
  print (resume 1)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref().map(|s| s.contains("[E-RESUME-MISSING]")),
            Some(true),
            "resume outside handler should report runtime error in fallback runtime"
        );
    }

    #[test]
    fn with_dispatches_effect_operation_in_runtime() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    log "hello"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("hello"),
            "with should install inline handler and dispatch operation"
        );
    }

    #[test]
    fn with_variable_dispatches_effect_operation_in_runtime() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  h = handler
    log msg ->
      print msg
      resume ()
  with h
  in
    log "world"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("world"),
            "with <handler-var> should install stored handler value and dispatch operation"
        );
    }

    #[test]
    fn with_captures_lexical_local_in_runtime() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  prefix = "pre:"
  with
    log msg ->
      print "${prefix}${msg}"
      resume ()
  in
    log "hello"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("pre:hello"),
            "handler value should capture lexical locals used inside clause body"
        );
    }

    #[test]
    fn handler_mutation_persists_across_repeated_calls_in_same_with() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Counter
  next: Unit -> Int

main : Unit -> Unit
main =
  mut counter = 0
  with
    next _ ->
      counter := counter + 1
      resume counter
  in
    print (next ())
    print (next ())
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("12"),
            "handler mutation should persist across repeated calls in the same with scope"
        );
    }

    #[test]
    fn nested_with_prefers_nearest_inline_handler() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "outer"
      resume ()
  in
    with
      log msg ->
        print "inner"
        resume ()
    in
      log "x"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("inner"),
            "nearest inline handler should win under nested with blocks"
        );
    }

    #[test]
    fn inner_with_overrides_outer_with() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "outer"
      resume ()
  in
    with
      log msg ->
        print "inline"
        resume ()
    in
      log "x"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("inline"),
            "inner with should take precedence over outer with"
        );
    }

    #[test]
    fn with_dispatches_qualified_effect_call_in_runtime() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    Log.log "qualified"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("qualified"),
            "qualified effect call should dispatch to active inline handler for that effect"
        );
    }

    #[test]
    fn typed_mode_matches_fallback_for_resume_success_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume 7
  in
    print (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "resume success path");
        assert_eq!(typed.stdout.as_deref(), Some("7"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_no_resume_value_scope_exit() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  result =
    with
      next n ->
        print "handled"
        42
    in
      next 0 + 1
  print result
  print "after"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "no-resume value scope exit");
        assert_eq!(typed.stdout.as_deref(), Some("handled42after"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_no_resume_unit_scope_exit() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "handled:${msg}"
  in
    log "hello"
    print "inner-after"
  print "outer-after"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "no-resume unit scope exit");
        assert_eq!(typed.stdout.as_deref(), Some("handled:helloouter-after"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_nested_scoped_exit() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Outer
  op: String -> Unit

effect Inner
  boom: String -> Int

main : Unit -> Unit
main =
  with
    op msg ->
      inner =
        with
          boom inner ->
            print "inner:${inner}"
            7
        in
          boom msg + 1
      print "outer:${inner}"
      resume ()
  in
    op "x"
    print "main-after"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "nested scoped-exit handler path");
        assert_eq!(typed.stdout.as_deref(), Some("inner:xouter:7main-after"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_double_resume_error() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (resume 1)
  in
    print (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "double-resume deterministic error path");
        assert_eq!(typed.stdout, None);
        assert_eq!(typed.runtime_error_kind, Some("continuation_consumed"));
    }

    #[test]
    fn resume_replays_remaining_unit_statements_before_second_resume_error() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "handled:${msg}"
      resume ()
      print "after-resume"
      resume ()
  in
    log "hello"
    print "continued"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert!(
            output.starts_with("handled:hellocontinuedafter-resume"),
            "first resume should replay remaining unit statements before handler continues"
        );
        assert!(
            output.contains("[E-RESUME-CONSUMED]"),
            "second resume after replayed continuation completion should report exhaustion"
        );
    }

    #[test]
    fn typed_mode_matches_fallback_for_resume_replay_then_exhaustion() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "handled:${msg}"
      resume ()
      print "after-resume"
      resume ()
  in
    log "hello"
    print "continued"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "resume replay then exhaustion");
        assert_eq!(
            typed.stdout.as_deref(),
            Some("handled:hellocontinuedafter-resume")
        );
        assert_eq!(typed.runtime_error_kind, Some("continuation_consumed"));
    }

    #[test]
    fn resume_replays_binding_value_continuation_into_following_statements() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    x = next 0
    y = next x
    print y
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(
            output, "2",
            "resume should bind the resumed value and continue through later statements"
        );
    }

    #[test]
    fn typed_mode_matches_fallback_for_binding_value_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    x = next 0
    y = next x
    print y
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "binding value replay");
        assert_eq!(typed.stdout.as_deref(), Some("2"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn declaration_value_call_replays_nested_binding_progression() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

yield_int_state : String -> Int -> (Bool, Int) can Iterator
yield_int_state value state = yield value state

count_values : Unit -> Int can Iterator
count_values =
  s1 = yield_int_state "a" 0
  s2 = if s1.0
    yield_int_state "b" s1.1
  else
    s1
  s3 = if s2.0
    yield_int_state "c" s2.1
  else
    s2
  s3.1

main : Unit -> Unit
main =
  with
    yield _ step ->
      resume (True, step + 1)
  in
    print (count_values ())
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "3");
    }

    #[test]
    fn typed_mode_matches_fallback_for_declaration_value_call_progression() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

yield_int_state : String -> Int -> (Bool, Int) can Iterator
yield_int_state value state = yield value state

count_values : Unit -> Int can Iterator
count_values =
  s1 = yield_int_state "a" 0
  s2 = if s1.0
    yield_int_state "b" s1.1
  else
    s1
  s3 = if s2.0
    yield_int_state "c" s2.1
  else
    s2
  s3.1

main : Unit -> Unit
main =
  with
    yield _ step ->
      resume (True, step + 1)
  in
    print (count_values ())
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "declaration value call progression");
        assert_eq!(typed.stdout.as_deref(), Some("3"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_iterator_unified_example_shape() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = read_example("iterator_unified.gb");
        let module = parse_module(&source).expect("iterator_unified.gb should parse");
        let typed = assert_mode_parity(&module, "iterator unified progression shape");
        assert_eq!(typed.stdout.as_deref(), Some("tick:atick:btick:c31"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn resume_replays_single_arg_call_continuation_in_value_position() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

id : Int -> Int
id x = x

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (id (next 0))
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "1");
    }

    #[test]
    fn typed_mode_matches_fallback_for_single_arg_call_value_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

id : Int -> Int
id x = x

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (id (next 0))
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "single-arg call value replay");
        assert_eq!(typed.stdout.as_deref(), Some("1"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn resume_replays_binop_left_operand_continuation() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (next 0 + 4)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "5");
    }

    #[test]
    fn resume_replays_binop_right_operand_continuation() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (4 + next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "5");
    }

    #[test]
    fn typed_mode_matches_fallback_for_binop_operand_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (4 + next 0)
    print (next 0 + 4)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "binop operand replay");
        assert_eq!(typed.stdout.as_deref(), Some("55"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_binop_both_operands_suspend() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Exercises BinOpLeft -> BinOpRight replay: left operand suspends first,
        // then during BinOpLeft replay the right operand also suspends.
        // This confirms that the BinOpRight continuation pushed during BinOpLeft replay
        // is correctly captured and resumed.
        //
        // The handler prints the input `n` before resuming, so we can observe
        // evaluation order: left (next 0, n=0) then right (next 10, n=10).
        // A bug that swapped operands or reused the wrong resumed value would
        // produce wrong output or wrong order.
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      print n
      resume (n + 3)
  in
    print (next 0 + next 10)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "binop both operands suspend");
        // Evaluation order: next 0 (prints 0, resumes 3), next 10 (prints 10, resumes 13), result 16
        assert_eq!(typed.stdout.as_deref(), Some("01016"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn resume_replays_if_condition_continuation() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

choose : Int -> Int
choose n =
  if flag n
    10
  else
    20

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
  in
    print (choose 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "10");
    }

    #[test]
    fn typed_mode_matches_fallback_for_if_condition_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

choose : Int -> Int
choose n =
  if flag n
    10
  else
    20

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
  in
    print (choose 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "if condition replay");
        assert_eq!(typed.stdout.as_deref(), Some("20"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn unit_position_if_condition_replay_uses_suspended_frame_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
  in
    if flag 0
      print 10
    else
      print 20
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "10");
    }

    #[test]
    fn typed_mode_matches_fallback_for_unit_position_if_condition_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
  in
    if flag 0
      print 10
    else
      print 20
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "unit-position if condition replay");
        assert_eq!(typed.stdout.as_deref(), Some("20"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn unit_position_if_selected_branch_replays_value_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
    next n ->
      resume (n + 1)
  in
    if flag 0
      print (next 0)
    else
      print 99
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "1");
    }

    #[test]
    fn typed_mode_matches_fallback_for_unit_position_if_branch_value_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
    next n ->
      resume (n + 1)
  in
    if flag 0
      print 99
    else
      print (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "unit-position if branch value replay");
        assert_eq!(typed.stdout.as_deref(), Some("1"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn binding_rhs_if_replays_through_outcome_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
    next n ->
      resume (n + 1)
  in
    value = if flag 0
      next 0
    else
      99
    print value
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "1");
    }

    #[test]
    fn typed_mode_matches_fallback_for_binding_rhs_if_outcome_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
    next n ->
      resume (n + 2)
  in
    value = if flag 0
      99
    else
      next 0
    print value
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "binding rhs if outcome path");
        assert_eq!(typed.stdout.as_deref(), Some("2"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn assignment_rhs_if_replays_through_outcome_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
    next n ->
      resume (n + 1)
  in
    value = 0
    value = if flag 0
      next 0
    else
      99
    print value
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "1");
    }

    #[test]
    fn typed_mode_matches_fallback_for_assignment_rhs_if_outcome_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
    next n ->
      resume (n + 3)
  in
    value = 0
    value = if flag 0
      99
    else
      next 0
    print value
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "assignment rhs if outcome path");
        assert_eq!(typed.stdout.as_deref(), Some("3"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn resume_replays_case_scrutinee_continuation() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

choose : Int -> Int
choose n =
  case flag n
    True -> 10
    False -> 20

main : Unit -> Unit
main =
  with
    flag n ->
      resume True
  in
    print (choose 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "10");
    }

    #[test]
    fn typed_mode_matches_fallback_for_case_scrutinee_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Pred
  flag: Int -> Bool

choose : Int -> Int
choose n =
  case flag n
    True -> 10
    False -> 20

main : Unit -> Unit
main =
  with
    flag n ->
      resume False
  in
    print (choose 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "case scrutinee replay");
        assert_eq!(typed.stdout.as_deref(), Some("20"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn parenthesized_multiline_case_call_uses_parsed_body() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (
      case 0
        0 -> next 0
        _ -> 99
    )
"#;
        let module = parse_module(source).expect("parse should work");
        assert!(
            main_parsed_body(&module).is_some(),
            "main parsed_body should exist for parenthesized multiline case call"
        );
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "1");
    }

    #[test]
    fn typed_mode_matches_fallback_for_parenthesized_multiline_case_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    print (
      case 0
        0 -> next 0
        _ -> 99
    )
"#;
        let module = parse_module(source).expect("parse should work");
        assert!(
            main_parsed_body(&module).is_some(),
            "main parsed_body should exist for parenthesized multiline case call"
        );
        let typed = assert_mode_parity(&module, "parenthesized multiline case call");
        assert_eq!(typed.stdout.as_deref(), Some("2"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn parenthesized_multiline_case_block_body_replays_value_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (
      case 0
        0 ->
          x = next 0
          x + 10
        _ -> 99
    )
"#;
        let module = parse_module(source).expect("parse should work");
        assert!(
            main_parsed_body(&module).is_some(),
            "main parsed_body should exist for parenthesized multiline case block call"
        );
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "11");
    }

    #[test]
    fn typed_mode_matches_fallback_for_parenthesized_multiline_case_block_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    print (
      case 0
        0 ->
          x = next 0
          x + 10
        _ -> 99
    )
"#;
        let module = parse_module(source).expect("parse should work");
        assert!(
            main_parsed_body(&module).is_some(),
            "main parsed_body should exist for parenthesized multiline case block call"
        );
        let typed = assert_mode_parity(&module, "parenthesized multiline case block call");
        assert_eq!(typed.stdout.as_deref(), Some("12"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn resume_replays_multi_arg_named_call_arguments() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

sum3 : Int -> Int -> Int -> Int
sum3 a b c = a + b + c

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (sum3 (next 0) 2 3)
    print (sum3 1 (next 0) 3)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "65");
    }

    #[test]
    fn typed_mode_matches_fallback_for_multi_arg_named_call_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

sum3 : Int -> Int -> Int -> Int
sum3 a b c = a + b + c

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    print (sum3 (next 0) 2 3)
    print (sum3 1 (next 0) 3)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "multi-arg named call replay");
        assert_eq!(typed.stdout.as_deref(), Some("76"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn resume_replays_receiver_method_call_argument() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

effect Log
  log: Int -> Int

pick : Int -> Int
pick n =
  Log.log (next n)

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
    log n ->
      resume (n + 10)
  in
    print (pick 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "11");
    }

    #[test]
    fn typed_mode_matches_fallback_for_receiver_method_call_argument_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

effect Log
  log: Int -> Int

pick : Int -> Int
pick n =
  Log.log (next n)

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
    log n ->
      resume (n + 20)
  in
    print (pick 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "receiver method call argument replay");
        assert_eq!(typed.stdout.as_deref(), Some("22"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn resume_replays_pipeline_value_continuation() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

id : Int -> Int
id x = x

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (next 0 |> id)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "1");
    }

    #[test]
    fn resume_replays_bare_var_call_arg_in_side_effect_position() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Exercises the bare var-callee call arm in eval_ast_side_effect:
        // `log (next 0)` is a statement-position call whose argument suspends through a resume.
        let source = r#"
effect Iter
  next: Int -> Int

effect Log
  log: Int -> Unit

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 5)
    log v ->
      print v
      resume ()
  in
    log (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "bare var call arg replay in side-effect position");
        assert_eq!(typed.stdout.as_deref(), Some("5"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn resume_replays_bare_var_call_arg_in_execute_unit_expr_ast_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Exercises the bare var-callee call arm in execute_unit_expr_ast (not eval_ast_side_effect).
        // `log (next 0)` appears inside a declaration body, so it is evaluated via
        // execute_unit_ast_stmt -> execute_unit_expr_ast rather than eval_ast_side_effect.
        let source = r#"
effect Iter
  next: Int -> Int

effect Log
  log: Int -> Unit

run_log : Unit -> Unit
run_log _ =
  log (next 0)

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 7)
    log v ->
      print v
      resume ()
  in
    run_log ()
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(
            &module,
            "bare var call arg replay in execute_unit_expr_ast path",
        );
        assert_eq!(typed.stdout.as_deref(), Some("7"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_pipeline_value_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

id : Int -> Int
id x = x

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    print (next 0 |> id)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "pipeline value replay");
        assert_eq!(typed.stdout.as_deref(), Some("2"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_nearest_handler_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect A
  next: Int -> Int

effect B
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1
  in
    with
      next y ->
        resume 2
    in
    print (B.next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "qualified nearest-handler dispatch path");
        assert_eq!(typed.stdout.as_deref(), Some("2"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_nested_same_effect_nearest_handler_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> String

main : Unit -> Unit
main =
  with
    log msg ->
      resume "outer"
  in
    with
      log msg ->
        resume "inner"
    in
      print (log "x")
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "nested same-effect nearest-handler dispatch path");
        assert_eq!(typed.stdout.as_deref(), Some("inner"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    #[ignore = "performance acceptance protocol (Step 8.6); run explicitly with --ignored"]
    fn step8_perf_acceptance_resume_heavy_samples() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Step 8.6 note:
        // keep this acceptance harness stable as optimized bridge internals evolve.
        let warmup_runs = 5usize;
        let measured_runs = 30usize;
        let max_slowdown_ratio = 1.03f64;
        let perf_samples = [
            (
                "resume_success_path",
                r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume 7
  in
    print (next 0)
"#,
            ),
            (
                "double_resume_error_path",
                r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (resume 1)
  in
    print (next 0)
"#,
            ),
            (
                "nested_handler_dispatch_path",
                r#"
effect Log
  log: String -> String

main : Unit -> Unit
main =
  with
    log msg ->
      resume "outer"
  in
    with
      log msg ->
        resume "inner"
    in
      print (log "x")
"#,
            ),
        ];

        for (name, source) in perf_samples {
            let module = parse_module(source).expect("performance sample should parse");
            let fallback = measure_runtime_mode_micros(
                &module,
                lower::EffectExecutionMode::PortableFallback,
                warmup_runs,
                measured_runs,
            );
            let typed = measure_runtime_mode_micros(
                &module,
                lower::EffectExecutionMode::TypedContinuationOptimized,
                warmup_runs,
                measured_runs,
            );
            assert_perf_within_threshold(name, fallback, typed, max_slowdown_ratio);
        }
    }

    #[test]
    fn qualified_resume_with_overlapping_method_names_uses_target_handler() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect A
  next: Int -> Int

effect B
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1
  in
    with
      next y ->
        resume 2
    in
    print (B.next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("2"),
            "qualified call should dispatch to the matching effect handler even with overlapping method names"
        );
    }

    #[test]
    fn resume_does_not_leak_handler_context_between_calls() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect A
  next: Int -> Int

effect B
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next x ->
      resume 1
  in
    with
      next y ->
        resume 2
    in
    print (B.next 0)
    print (B.next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("22"),
            "handler context should remain stable after resume across multiple qualified calls"
        );
    }

    #[test]
    fn native_codegen_capability_checker_rejects_hello_effect_boundary_subset() {
        let source = read_example("hello.gb");
        let module = parse_module(&source).expect("hello.gb should parse");
        assert!(
            !fallback::supports_native_codegen(&module),
            "hello.gb should be rejected by native capability checker when main is EffectBoundary"
        );
        assert_eq!(
            fallback::native_unsupported_reason_kind(&module),
            Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported)
        );
        assert_eq!(
            fallback::native_unsupported_reason(&module),
            Some("call_target_body_not_native_supported")
        );
    }

    #[test]
    fn native_codegen_capability_checker_rejects_effect_example() {
        let source = read_example("effect.gb");
        let module = parse_module(&source).expect("effect.gb should parse");
        assert!(
            !fallback::supports_native_codegen(&module),
            "effect.gb should remain on fallback path in Phase 0"
        );
        assert_eq!(
            fallback::native_unsupported_reason_kind(&module),
            Some(fallback::UnsupportedReason::MainAnnotationNotUnitToUnit),
            "effect example should expose typed fallback reason"
        );
        assert_eq!(
            fallback::native_unsupported_reason(&module),
            Some("main_annotation_not_unit_to_unit"),
            "effect example should expose explicit fallback reason"
        );
    }

    #[test]
    fn compile_module_emits_valid_wasm_for_phase1_subset_via_fallback() {
        let source = read_example("hello.gb");
        let module = parse_module(&source).expect("hello.gb should parse");
        assert!(
            !fallback::supports_native_codegen(&module),
            "phase-1 hello subset should take fallback path because main is EffectBoundary"
        );
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn native_codegen_capability_checker_accepts_phase2_int_bool_subset() {
        let source = r#"
main : Unit -> Unit
main =
  x = 6 * 7
  ok = x == 42
  print x
  print ok
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            fallback::supports_native_codegen(&module),
            "phase-2 int/bool subset should be accepted by native capability checker"
        );
    }

    #[test]
    fn compile_module_uses_native_emitter_for_phase2_int_bool_subset() {
        let source = r#"
main : Unit -> Unit
main =
  x = 6 * 7
  ok = x == 42
  print x
  print ok
"#;
        let module = parse_module(source).expect("source should parse");
        let wasm = compile_module(&module).expect("codegen should succeed");
        let expected_text =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(expected_text, "42True");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn native_codegen_capability_checker_accepts_direct_function_call_subset() {
        let source = r#"
double : Int -> Int
double n = n * 2

main : Unit -> Unit
main =
  print (double 21)
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            fallback::supports_native_codegen(&module),
            "direct single-arg function call subset should be accepted"
        );
    }

    #[test]
    fn compile_module_uses_native_emitter_for_direct_function_call_subset() {
        let source = r#"
double : Int -> Int
double n = n * 2

main : Unit -> Unit
main =
  print (double 21)
"#;
        let module = parse_module(source).expect("source should parse");
        let wasm = compile_module(&module).expect("codegen should succeed");
        let expected_text =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(expected_text, "42");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn compile_module_uses_native_emitter_for_multi_arg_direct_function_call_subset() {
        let source = r#"
add4 : Int -> Int -> Int -> Int -> Int
add4 a b c d = a + b + c + d

main : Unit -> Unit
main =
  print (add4 1 2 3 4)
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            fallback::supports_native_codegen(&module),
            "multi-arg direct function call subset should be accepted"
        );
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn compile_module_uses_native_emitter_for_function_example_first_order_subset() {
        let source = r#"
add_ten : Int -> Int
add_ten x = x + 10

add_ten_mul_three : Int -> Int
add_ten_mul_three a =
  b = a + 10
  b * 3

main : Unit -> Unit
main =
  b = add_ten 10
  c = add_ten_mul_three b
  print c
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            fallback::supports_native_codegen(&module),
            "first-order subset derived from function.gb should be accepted"
        );
        assert_eq!(fallback::native_unsupported_reason_kind(&module), None);
        let wasm = compile_module(&module).expect("codegen should succeed");
        let expected_text =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(expected_text, "90");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn native_codegen_ignores_unused_hof_declaration() {
        let source = r#"
import goby/list ( map )

mul_tens : List Int -> List Int
mul_tens ns = map ns (|n| -> n * 10)

main : Unit -> Unit
main =
  print 42
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            fallback::supports_native_codegen(&module),
            "unused HOF declaration should not block native path"
        );
        assert_eq!(fallback::native_unsupported_reason_kind(&module), None);
        let wasm = compile_module(&module).expect("codegen should succeed");
        let expected_text =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(expected_text, "42");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn native_codegen_accepts_transitively_required_hof_declaration() {
        let source = r#"
import goby/list ( map )

mul_tens : List Int -> List Int
mul_tens ns = map ns (|n| -> n * 10)

wrapped_mul_tens : List Int -> List Int
wrapped_mul_tens ns = mul_tens ns

main : Unit -> Unit
main =
  print (wrapped_mul_tens [1, 2])
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            fallback::supports_native_codegen(&module),
            "transitively required HOF declaration should be accepted by native lowering"
        );
        assert_eq!(fallback::native_unsupported_reason_kind(&module), None);
        assert_eq!(fallback::native_unsupported_reason(&module), None);
        let wasm = compile_module(&module).expect("codegen should succeed");
        let expected_text =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(expected_text, "[10, 20]");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn compile_module_uses_native_emitter_for_list_int_print_subset() {
        let source = r#"
main : Unit -> Unit
main =
  xs = [1, 2, 3]
  print xs
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            fallback::supports_native_codegen(&module),
            "list-int print subset should be accepted by native capability checker"
        );
        let wasm = compile_module(&module).expect("codegen should succeed");
        let expected_text =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(expected_text, "[1, 2, 3]");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn resolves_runtime_output_for_list_spread_int_values() {
        let source = r#"
main : Unit -> Unit
main =
  rest = [2, 3]
  print [1, ..rest]
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            !fallback::supports_native_codegen(&module),
            "list spread currently routes through fallback runtime path"
        );
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[1, 2, 3]");
    }

    #[test]
    fn resolves_runtime_output_for_list_spread_string_values() {
        let source = r#"
main : Unit -> Unit
main =
  rest = ["b", "c"]
  print ["a", ..rest]
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            !fallback::supports_native_codegen(&module),
            "list spread currently routes through fallback runtime path"
        );
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[\"a\", \"b\", \"c\"]");
    }

    #[test]
    fn list_literal_replays_handled_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    xs = [next 0, 2]
    print xs
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "[1, 2]");
    }

    #[test]
    fn typed_mode_matches_fallback_for_list_literal_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    xs = [next 0, 2]
    print xs
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "list literal replay");
        assert_eq!(typed.stdout.as_deref(), Some("[2, 2]"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn record_constructor_replays_handled_field_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
type Box = Box(value: Int)

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    box = Box(value: next 0)
    print box.value
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "1");
    }

    #[test]
    fn typed_mode_matches_fallback_for_record_constructor_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
type Box = Box(value: Int)

effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 2)
  in
    box = Box(value: next 0)
    print box.value
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "record constructor replay");
        assert_eq!(typed.stdout.as_deref(), Some("2"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn positional_single_field_constructor_replays_handled_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
type Error = Error(message: String)

effect Msg
  next: Unit -> String

effect RaiseError
  raise: Error -> Unit

main : Unit -> Unit
main =
  with
    next _ ->
      resume "oops"
  in
    with
      raise e ->
        print e.message
        resume ()
    in
      raise Error(next ())
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "oops");
    }

    #[test]
    fn typed_mode_matches_fallback_for_positional_single_field_constructor_replay() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
type Error = Error(message: String)

effect Msg
  next: Unit -> String

effect RaiseError
  raise: Error -> Unit

main : Unit -> Unit
main =
  with
    next _ ->
      resume "later"
  in
    with
      raise e ->
        print e.message
        resume ()
    in
      raise Error(next ())
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "positional single-field constructor replay");
        assert_eq!(typed.stdout.as_deref(), Some("later"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn compile_module_uses_native_emitter_for_list_int_pipeline_print_subset() {
        let source = r#"
main : Unit -> Unit
main =
  [4, 5, 6] |> print
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            fallback::supports_native_codegen(&module),
            "list-int pipeline print subset should be accepted by native capability checker"
        );
        let wasm = compile_module(&module).expect("codegen should succeed");
        let expected_text =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(expected_text, "[4, 5, 6]");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn compile_module_uses_native_emitter_for_if_print_subset() {
        let source = r#"
main : Unit -> Unit
main =
  a = 10
  b = 20
  print
    if a + b == 30
      "30"
    else
      "other"
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            fallback::supports_native_codegen(&module),
            "if-print subset should be accepted by native capability checker"
        );
        let wasm = compile_module(&module).expect("codegen should succeed");
        let expected_text =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(expected_text, "30");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn compile_module_uses_native_emitter_for_case_print_subset() {
        let source = r#"
main : Unit -> Unit
main =
  x = 5
  print
    case x
      5 -> "Five!"
      3 -> "Three!"
      _ -> "Other"
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(
            fallback::supports_native_codegen(&module),
            "case-print subset should be accepted by native capability checker"
        );
        let wasm = compile_module(&module).expect("codegen should succeed");
        let expected_text =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(expected_text, "Five!");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn compile_module_emits_valid_wasm_for_control_flow_example_via_fallback() {
        let source = read_example("control_flow.gb");
        let module = parse_module(&source).expect("control_flow.gb should parse");
        assert!(
            !fallback::supports_native_codegen(&module),
            "control_flow.gb should take fallback path because main is EffectBoundary"
        );
        let wasm = compile_module(&module).expect("codegen should succeed");
        let expected_text =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(expected_text, "Five!5030");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn native_codegen_capability_checker_accepts_function_example_with_hof_lambda() {
        let source = read_example("function.gb");
        let module = parse_module(&source).expect("function.gb should parse");
        assert!(
            fallback::supports_native_codegen(&module),
            "function.gb should be accepted by native lowering after lambda/HOF support"
        );
        assert_eq!(
            fallback::native_unsupported_reason_kind(&module),
            None,
            "function.gb should no longer report a native fallback reason"
        );
        assert_eq!(
            fallback::native_unsupported_reason(&module),
            None,
            "function.gb should no longer report a native fallback reason"
        );
    }

    #[test]
    fn native_codegen_capability_checker_reports_expected_call_reasons() {
        let cases = [
            (
                "non_direct_callee",
                r#"
main : Unit -> Unit
main =
  print (Foo.bar 1)
"#,
                Some(fallback::UnsupportedReason::CallCalleeNotDirectName),
            ),
            (
                "arity_mismatch",
                r#"
id : Int -> Int
id x = x

main : Unit -> Unit
main =
  print (id 1 2)
"#,
                Some(fallback::UnsupportedReason::CallArityMismatch),
            ),
            (
                "target_missing",
                r#"
main : Unit -> Unit
main =
  print (unknown 1)
"#,
                Some(fallback::UnsupportedReason::CallTargetNotDeclaration),
            ),
            (
                "target_body_not_supported",
                r#"
uses_with : Int -> Int
uses_with x =
  with
    log v ->
      resume ()
  in
    x

main : Unit -> Unit
main =
  print (uses_with 1)
"#,
                Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
            ),
            (
                "target_body_not_supported_with_lambda",
                r#"
uses_with_callback : (Int -> Int) -> Int
uses_with_callback f =
  with
    log v ->
      resume ()
  in
    f 1

main : Unit -> Unit
main =
  print (uses_with_callback (|x| -> x + 1))
"#,
                Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
            ),
            (
                "target_body_not_supported_due_to_effect_boundary",
                r#"
tick : Int -> Int can Tick
tick n = n

main : Unit -> Unit
main =
  print (tick 1)
"#,
                Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
            ),
        ];

        for (name, source, expected_kind) in cases {
            let module = parse_module(source).expect("source should parse");
            let expected_str = expected_kind.map(fallback::UnsupportedReason::as_str);
            assert_eq!(
                fallback::native_unsupported_reason_kind(&module),
                expected_kind,
                "unexpected typed fallback reason for case: {}",
                name
            );
            assert_eq!(
                fallback::native_unsupported_reason(&module),
                expected_str,
                "unexpected fallback reason for case: {}",
                name
            );
        }
    }

    #[test]
    fn native_codegen_capability_checker_prioritizes_body_reason_over_arity_mismatch() {
        let source = r#"
uses_with : Int -> Int
uses_with x =
  with
    log v ->
      resume ()
  in
    x

main : Unit -> Unit
main =
  print (uses_with 1 2)
"#;
        let module = parse_module(source).expect("source should parse");
        assert_eq!(
            fallback::native_unsupported_reason_kind(&module),
            Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
            "typed reason should prefer unsupported declaration body over arity mismatch"
        );
        assert_eq!(
            fallback::native_unsupported_reason(&module),
            Some("call_target_body_not_native_supported"),
            "unsupported declaration body reason should win when call-shape mismatch coexists"
        );
    }

    #[test]
    fn native_fallback_path_matrix_for_examples() {
        let cases = [
            (
                "hello.gb",
                false,
                Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
                Some("call_target_body_not_native_supported"),
            ),
            (
                "control_flow.gb",
                false,
                Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
                Some("call_target_body_not_native_supported"),
            ),
            (
                "effect.gb",
                false,
                Some(fallback::UnsupportedReason::MainAnnotationNotUnitToUnit),
                Some("main_annotation_not_unit_to_unit"),
            ),
            ("function.gb", true, None, None),
        ];

        for (name, expect_native, expected_reason_kind, expected_reason) in cases {
            let source = read_example(name);
            let module = parse_module(&source).expect("example should parse");
            let reason_kind = fallback::native_unsupported_reason_kind(&module);
            let reason = fallback::native_unsupported_reason(&module);
            let supports_native = fallback::supports_native_codegen(&module);
            assert_eq!(
                reason_kind, expected_reason_kind,
                "unexpected typed fallback reason for {}",
                name
            );
            assert_eq!(
                reason, expected_reason,
                "unexpected fallback reason for {}",
                name
            );
            assert_eq!(
                supports_native, expect_native,
                "unexpected native capability result for {}",
                name
            );

            let wasm = compile_module(&module).expect("codegen should succeed");
            assert_valid_wasm_module(&wasm);
        }
    }

    #[test]
    fn handler_body_sequential_value_binding_with_inner_effect_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape A: handler body has a sequential value-position binding where
        // the RHS calls a different effect whose handler also lives in the same
        // `with` block.
        //
        //   outer `next _ ->` handler body:
        //     x = get ()     <- calls the `get` handler; resolves synchronously
        //     resume (x + 10)
        //
        // Because `get _ -> resume 5` resolves inline, dispatch_handler_method_core
        // returns HandlerCompletion::Resumed(5), which is converted to
        // AstEvalOutcome::Complete(5) at the call site. The Suspended branch inside
        // the Stmt::Binding arm is NOT reached here. This test locks the happy-path
        // where both effects coexist in one `with` block and the inner one completes
        // synchronously.
        //
        // Two different effects (and therefore two distinct handler methods) are used
        // so that the outer `next` handler body does not invoke itself.
        let source = r#"
effect Source
  next: Unit -> Int

effect Store
  get: Unit -> Int

main : Unit -> Unit
main =
  with
    next _ ->
      x = get ()
      resume (x + 10)
    get _ ->
      resume 5
  in
    print (next ())
"#;
        let module = parse_module(source).expect("parse should work");
        // get() resolves to 5, then next resumes with 5+10=15
        let typed = assert_mode_parity(
            &module,
            "handler body sequential value binding with inner effect call",
        );
        assert_eq!(typed.stdout.as_deref(), Some("15"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn in_block_calls_declaration_that_invokes_effect() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape B (in-block variant): the `in` block calls a declaration
        // `get_plus` whose body calls an effect and adds to the result.
        // (See `handler_body_sequential_value_binding_with_inner_effect_call`
        // for Shape A.)
        //
        // `get_plus n = next n + 5`, handler resumes with `n + 5`.
        // `get_plus 0`: next(0) fires handler → resumes 5 →
        //   declaration continues 5 + 5 = 10. Output: "10".
        //
        // This exercises the path where a declaration called from the `in`
        // block suspends at an inner effect call, then resumes and continues
        // with the remaining computation.
        //
        // Note: calling get_plus from the handler body itself (not the `in`
        // block) currently does not work because the handler body statement
        // executor drops the continuation when an inner eval suspends (see
        // TODO comments in dispatch_handler_method_core). This test covers
        // the supported in-block variant.
        let source = r#"
effect Eff
  next: Int -> Int

get_plus : Int -> Int
get_plus n = next n + 5

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 5)
  in
    print (get_plus 0)
"#;
        let module = parse_module(source).expect("parse should work");
        // get_plus 0 -> next 0 (handler resumes 5) -> 5 + 5 = 10
        let typed = assert_mode_parity(&module, "in-block call to declaration that invokes effect");
        assert_eq!(typed.stdout.as_deref(), Some("10"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn handler_body_binding_resumes_via_outer_with_block_synchronous_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Handler body sequential value binding where the inner effect is
        // handled by an OUTER `with` block (nested `with` blocks).
        //
        // Inner `next` handler body: `x = get (); resume (x + 10)`.
        // Outer handler for `get` resumes with 5.
        //
        // When the handler body calls `get ()`, dispatch_handler_method_core
        // for `get` runs synchronously within the call stack. The resume token
        // created for the `get` handler dispatch has frame=None (no pending
        // stmt/value continuation at that dispatch depth), so the resume path
        // takes the synchronous branch:
        //   resume_through_active_continuation_out returns Out::Done(5).
        // Thus eval_expr_to_option(get ()) inside the handler body returns
        // Some(5), x=5 is stored, then resume(5+10)=15.
        //
        // The Suspended branch of Stmt::Binding in dispatch_handler_method_core
        // is NOT reached here (synchronous dispatch only; the async suspension
        // path where Suspended is returned is a known TODO at that site).
        //
        // Expected output: "15".
        let source = r#"
effect Source
  next: Unit -> Int

effect Store
  get: Unit -> Int

main : Unit -> Unit
main =
  with
    get _ ->
      resume 5
  in
    with
      next _ ->
        x = get ()
        resume (x + 10)
    in
      print (next ())
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(
            &module,
            "handler body binding resumes via outer with block (synchronous dispatch)",
        );
        assert_eq!(typed.stdout.as_deref(), Some("15"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn declaration_body_two_binding_progression_value_combination() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape C: declaration body with two sequential value-position bindings
        // both suspending at effect calls, whose results are combined in the
        // final expression.
        //
        // `sum_two`: a = next(0) → 1, b = next(a) → 2, returns a + b = 3.
        //
        // This exercises two sequential Stmt::Binding suspensions in a
        // declaration body. Each binding goes through the stmt continuation
        // mechanism:
        //   1st: push BindValue{a}, eval next(n) suspends, stmt cont captures
        //        remaining [b=next(a), a+b]. Resume → a=1 stored, remaining runs.
        //   2nd: push BindValue{b}, eval next(a) suspends, stmt cont captures
        //        remaining [a+b]. Resume → b=2 stored, a+b=3 returned.
        let source = r#"
effect Iter
  next: Int -> Int

sum_two : Int -> Int
sum_two n =
  a = next n
  b = next a
  a + b

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (sum_two 0)
"#;
        let module = parse_module(source).expect("parse should work");
        // sum_two 0: a=next(0)=1, b=next(1)=2, a+b=3
        let typed = assert_mode_parity(
            &module,
            "declaration body two binding progression value combination",
        );
        assert_eq!(typed.stdout.as_deref(), Some("3"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn three_step_in_block_binding_progression() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape E: 3 sequential in-block binding suspensions driven by the same handler.
        // (Shape D = handler_body_binding_resumes_via_outer_with_block_synchronous_dispatch.)
        // Extends `typed_mode_matches_fallback_for_binding_value_replay` (2-step) to confirm
        // the stmt continuation mechanism handles an arbitrary chain of suspensions.
        //
        // x=next(0)=1, y=next(1)=2, z=next(2)=3. print z. Output: "3".
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    x = next 0
    y = next x
    z = next y
    print z
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "three-step in-block binding progression");
        assert_eq!(typed.stdout.as_deref(), Some("3"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn handler_body_with_inner_with_block_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape F: nested `with` as the binding-RHS inside a handler body.
        // The outer handler for `run` binds `result` to the value produced by an inner
        // `with` block that handles `step`. The inner handler drives two sequential
        // bindings; `resume result` delivers the combined value to the outer continuation.
        //
        // a=step(1)=2, b=step(2)=4. result=a+b=6. resume 6. Output: "6".
        let source = r#"
effect Outer
  run: Unit -> Int

effect Inner
  step: Int -> Int

main : Unit -> Unit
main =
  with
    run _ ->
      result =
        with
          step n ->
            resume (n * 2)
        in
          a = step 1
          b = step a
          a + b
      resume result
  in
    print (run ())
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "handler body with inner with block value");
        assert_eq!(typed.stdout.as_deref(), Some("6"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn assignment_rhs_next_line_with_block_value() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape G: `name :=\n  with ...` (assignment from next-line with block).
        // Exercises the Assign branch in parse_stmts_from_lines for next-line `with` RHS,
        // plus runtime evaluation of Stmt::Assign where the value comes from a with block.
        //
        // mut result = 0
        // result :=
        //   with step n -> resume (n * 3)
        //   in a = step 1; a + 5
        // a = step(1) = 3; result = 3 + 5 = 8. Output: "8".
        let source = r#"
effect Iter
  step: Int -> Int

main : Unit -> Unit
main =
  with
    step n ->
      resume (n * 3)
  in
    mut result = 0
    result :=
      with
        step n ->
          resume (n * 3)
      in
        a = step 1
        a + 5
    print result
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "assignment rhs next-line with block value");
        assert_eq!(typed.stdout.as_deref(), Some("8"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn case_arm_body_calls_effect_operation() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape H: effect call inside a `case` arm body (not the scrutinee).
        // Covers the arm-body effect-call path in the AST evaluator, which is distinct
        // from the scrutinee-replay path tested by resume_replays_case_scrutinee_continuation.
        //
        // get_next_from_case 0 → case arm `0 -> next 0` → next(0)=10. Output: "10".
        // get_next_from_case 5 → case arm `_ -> next n` → next(5)=15. Output: "15".
        let source = r#"
effect Iter
  next: Int -> Int

get_next_from_case : Int -> Int
get_next_from_case n =
  case n
    0 -> next 0
    _ -> next n

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 10)
  in
    print (get_next_from_case 0)
    print (get_next_from_case 5)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "case arm body calls effect operation");
        assert_eq!(typed.stdout.as_deref(), Some("1015"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn declaration_block_body_with_binding_and_effect_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape I: declaration body (Expr::Block) has a Binding + effect call, called via
        // `print (get_advanced 5)`. Exercises eval_expr_to_option in the print arg path
        // (migrated from eval_ast_value so declaration-body effect calls are supported).
        //
        // get_advanced n: a = next(n) = n+1, returns a+10. With n=5: a=6, a+10=16.
        let source = r#"
effect Iter
  next: Int -> Int

get_advanced : Int -> Int
get_advanced n =
  a = next n
  a + 10

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print (get_advanced 5)
"#;
        let module = parse_module(source).expect("parse should work");
        // a=next(5)=6, a+10=16. Output: "16".
        let typed = assert_mode_parity(
            &module,
            "declaration block body with binding and effect call",
        );
        assert_eq!(typed.stdout.as_deref(), Some("16"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn interpolated_string_with_declaration_body_effect_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape J: string interpolation embeds a declaration call that invokes an effect.
        // `get_val n` returns next(n). `print "result=${get_val 3}"` → "result=4".
        // Exercises eval_expr_to_option for InterpolatedString segment evaluation
        // when the segment is a call whose declaration body calls an effect operation.
        let source = r#"
effect Iter
  next: Int -> Int

get_val : Int -> Int
get_val n =
  next n

main : Unit -> Unit
main =
  with
    next n ->
      resume (n + 1)
  in
    print "result=${get_val 3}"
"#;
        let module = parse_module(source).expect("parse should work");
        // get_val(3) = next(3) = 4. Output: "result=4".
        let typed = assert_mode_parity(
            &module,
            "interpolated string with declaration body effect call",
        );
        assert_eq!(typed.stdout.as_deref(), Some("result=4"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn pipeline_value_with_declaration_body_effect_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Shape K: pipeline left-hand side is a declaration call whose body invokes an effect.
        // Exercises eval_expr_to_option for the Pipeline value arm in eval_ast_side_effect
        // (migrated from eval_ast_value).
        //
        // `(get_val 3) |> log` where get_val n = next n. next(3)=4. log("4") prints "4".
        let source = r#"
effect Iter
  next: Int -> Int

get_val : Int -> Int
get_val n =
  next n

main : Unit -> Unit
main =
  with
    next n -> resume (n + 1)
  in
    print (get_val 3 |> get_val)
"#;
        let module = parse_module(source).expect("parse should work");
        // get_val(3) = next(3) = 4, get_val(4) = next(4) = 5. Output: "5".
        let typed = assert_mode_parity(&module, "pipeline value with declaration body effect call");
        assert_eq!(typed.stdout.as_deref(), Some("5"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_list_each_with_effect_callback() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for list.each with effectful lambda callback.
        // Covers the typed-mode path for execute_decl_with_callable_as_side_effect
        // when the callable invokes an effect operation inside the lambda body.
        let source = r#"
import goby/list

effect Log
  log : Int -> Unit

main : Unit -> Unit
main =
  with
    log n ->
      print "${n}"
      resume ()
  in
    list.each [2, 4, 6] (|n| -> log n)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "list each with effect callback");
        assert_eq!(typed.stdout.as_deref(), Some("246"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_with_handler_variable() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for `with <handler-var> in ...` where the handler is stored
        // in a local variable before being installed. Covers the handler-value path
        // in both PortableFallback and TypedContinuationOptimized modes.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  h = handler
    log msg ->
      print msg
      resume ()
  with h
  in
    log "parity"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "with handler variable");
        assert_eq!(typed.stdout.as_deref(), Some("parity"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_with_captures_lexical_local() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for handler clause that captures a lexical local (`prefix`)
        // defined before the `with` block. Covers closure-capture in both modes.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  prefix = "pre:"
  with
    log msg ->
      print "${prefix}${msg}"
      resume ()
  in
    log "hello"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "with captures lexical local");
        assert_eq!(typed.stdout.as_deref(), Some("pre:hello"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_nested_with_nearest_handler_wins() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for nested `with` blocks on the same effect: the inner handler
        // should win over the outer one in both modes.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print "outer"
      resume ()
  in
    with
      log msg ->
        print "inner"
        resume ()
    in
      log "x"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "nested with nearest handler wins");
        assert_eq!(typed.stdout.as_deref(), Some("inner"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_qualified_effect_call_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for `Effect.op arg` (qualified call) dispatching to an active
        // inline handler in both PortableFallback and TypedContinuationOptimized modes.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    Log.log "qualified"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "qualified effect call dispatch");
        assert_eq!(typed.stdout.as_deref(), Some("qualified"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_pipeline_effect_call_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for `value |> effect_op` (pipeline into an effect operation)
        // dispatching to the active handler in both modes.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    "piped" |> log
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "pipeline effect call dispatch");
        assert_eq!(typed.stdout.as_deref(), Some("piped"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_basic_with_inline_handler_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for the most basic `with` form: inline handler, single operation call.
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  with
    log msg ->
      print msg
      resume ()
  in
    log "hello"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "basic with inline handler dispatch");
        assert_eq!(typed.stdout.as_deref(), Some("hello"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_resume_outside_handler_error() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for `resume` used outside any handler context.
        // Both modes should surface a deterministic runtime error (E-RESUME-MISSING).
        let source = r#"
main : Unit -> Unit
main =
  print (resume 1)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "resume outside handler error");
        assert_eq!(typed.runtime_error_kind, Some("continuation_missing"));
    }

    #[test]
    fn typed_mode_matches_fallback_for_multi_arg_effect_op_call() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for a multi-argument effect operation: `yield _ step -> resume (True, step+1)`.
        // Exercises the named-call path with two-arg dispatch in both modes.
        let source = r#"
effect Iterator a b
  yield : a -> b -> (Bool, b)

main : Unit -> Unit
main =
  with
    yield _ step ->
      resume (True, step + 1)
  in
    print (yield "x" 41)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "multi-arg effect op call");
        assert_eq!(typed.stdout.as_deref(), Some("(True, 42)"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_qualified_call_without_active_handler() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test: `Log.log "x"` with no active handler for `Log` should produce no
        // output and no runtime error in both modes (silent fall-through).
        let source = r#"
effect Log
  log: String -> Unit

main : Unit -> Unit
main =
  Log.log "unreachable"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "qualified call without active handler");
        assert_eq!(typed.stdout, None);
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_lambda_closure_capture() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for inline lambda that captures a lexical local from the enclosing scope.
        // `each_two (|n| -> print "${n + base}")` where `base = 40`.
        let source = r#"
each_two : (Int -> Unit) -> Unit
each_two f =
  f 1
  f 2

main : Unit -> Unit
main =
  base = 40
  each_two (|n| -> print "${n + base}")
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "lambda closure capture");
        assert_eq!(typed.stdout.as_deref(), Some("4142"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_list_each_style_callback_dispatch() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Parity test for list-each-style iteration using a custom `each` that installs a
        // handler variable internally. Covers recursive `iter` + handler-variable `with` path.
        let source = r#"
effect ListYield
  yield : Int -> Bool

iter : List Int -> Unit can ListYield
iter xs =
  case xs
    [] -> ()
    [x, ..xxs] ->
      if yield x
        iter xxs
      else
        ()

each : List Int -> (Int -> Unit) -> Unit
each xs f =
  emit_handler = handler
    yield x ->
      f x
      resume True
  with emit_handler
  in
    iter xs

main : Unit -> Unit
main =
  each [3, 5] (|n| -> print "${n}")
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "list each style callback dispatch");
        assert_eq!(typed.stdout.as_deref(), Some("35"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn case_scrutinee_suspends_and_arm_body_calls_effect() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Double-suspension pattern: the case scrutinee is an effect call (suspends), then
        // the selected arm body is also an effect call (suspends again).
        // Exercises the Out-path `ApplyStep::CaseSelect` continuation (eval_expr path),
        // which is the active path for declarations supported by eval_decl_as_value_with_args_out.
        //
        // flag 0 → resume True; case arm `True -> next 5`; next 5 → resume 6. Output: "6".
        let source = r#"
effect Pred
  flag: Int -> Bool
effect Iter
  next: Int -> Int

choose : Int -> Int
choose n =
  case flag n
    True  -> next 5
    False -> next 0

main : Unit -> Unit
main =
  with
    flag _ -> resume True
    next n -> resume (n + 1)
  in
    print (choose 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed =
            assert_mode_parity(&module, "case scrutinee suspends and arm body calls effect");
        assert_eq!(typed.stdout.as_deref(), Some("6"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn if_condition_suspends_and_branch_body_calls_effect() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // Double-suspension pattern: the if condition is an effect call (suspends), then
        // the selected branch body is also an effect call (suspends again).
        // Exercises the Out-path `ApplyStep::IfBranch` continuation (eval_expr path),
        // which is the active path for declarations supported by eval_decl_as_value_with_args_out.
        //
        // flag 0 → resume True; then-branch `next 5`; next 5 → resume 6. Output: "6".
        let source = r#"
effect Pred
  flag: Int -> Bool
effect Iter
  next: Int -> Int

choose : Int -> Int
choose n =
  if flag n
    next 5
  else
    next 0

main : Unit -> Unit
main =
  with
    flag _ -> resume True
    next n -> resume (n + 1)
  in
    print (choose 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(
            &module,
            "if condition suspends and branch body calls effect",
        );
        assert_eq!(typed.stdout.as_deref(), Some("6"));
        assert_eq!(typed.runtime_error_kind, None);
    }
}
