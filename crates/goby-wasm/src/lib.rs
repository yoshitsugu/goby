mod backend;
mod call;
mod fallback;
mod layout;
mod lower;
mod planning;
mod support;

use std::collections::{HashMap, HashSet};

use goby_core::{
    CasePattern, Expr, HandlerMethod, Module, Stmt, ast::InterpolatedPart,
    str_util::parse_string_concat_call, types::parse_function_type,
};
const ERR_MISSING_MAIN: &str = "Wasm codegen requires a `main` declaration";
const BUILTIN_PRINT: &str = "print";
const MAX_EVAL_DEPTH: usize = 32;
const ERR_RESUME_MISSING: &str = "resume used without an active continuation [E-RESUME-MISSING]: `resume` can only be called while executing a handler operation body";
const ERR_RESUME_CONSUMED: &str = "resume continuation already consumed [E-RESUME-CONSUMED]: continuations are one-shot; call `resume` at most once per handled operation";
const ERR_RESUME_HANDLER_MISMATCH: &str = "internal resume token handler mismatch [E-RESUME-HANDLER-MISMATCH]: continuation token points to an unknown handler";
const ERR_RESUME_STACK_MISMATCH: &str = "internal resume token stack mismatch [E-RESUME-STACK-MISMATCH]: continuation token stack became unbalanced";

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

fn split_binding(line: &str) -> Option<(&str, &str)> {
    let idx = line.find('=')?;
    let name = line[..idx].trim();
    if !is_identifier(name) {
        return None;
    }
    Some((name, line[idx + 1..].trim()))
}

fn parse_print_call(line: &str) -> Option<&str> {
    let rest = line.strip_prefix("print")?;
    let first = rest.chars().next()?;
    if !first.is_whitespace() {
        return None;
    }
    Some(rest.trim())
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
    let int_functions = collect_functions_with_result(module, "Int");
    let list_functions = collect_functions_with_result(module, "List Int");
    let unit_functions = collect_unit_functions(module);
    let int_evaluator = IntEvaluator::root(&int_functions);
    let list_evaluator = ListIntEvaluator::root(&list_functions);
    let evaluators = RuntimeEvaluators {
        int: &int_evaluator,
        list: &list_evaluator,
        unit: &unit_functions,
    };
    RuntimeOutputResolver::resolve(module, body, parsed_stmts, &evaluators, execution_mode)
}

type EvaluatedFunctions<'a> = HashMap<&'a str, EvaluatedFunction<'a>>;

fn collect_functions_with_result<'a>(
    module: &'a Module,
    expected_result_type: &str,
) -> EvaluatedFunctions<'a> {
    let declaration_names: HashSet<&str> = module
        .declarations
        .iter()
        .map(|decl| decl.name.as_str())
        .collect();

    let mut functions = HashMap::new();
    for decl in &module.declarations {
        if decl.name == "main" {
            continue;
        }

        let Some(annotation) = decl.type_annotation.as_deref() else {
            continue;
        };
        let Some(function_type) = parse_function_type(annotation) else {
            continue;
        };
        if function_type.result != expected_result_type || function_type.arguments.len() > 1 {
            continue;
        }

        let parameter = infer_single_parameter_name(&decl.body, &declaration_names);
        functions.insert(
            decl.name.as_str(),
            EvaluatedFunction {
                body: &decl.body,
                parameter,
                parsed_stmts: decl.parsed_body.as_deref(),
            },
        );
    }

    functions
}

fn collect_unit_functions<'a>(module: &'a Module) -> EvaluatedFunctions<'a> {
    let declaration_names: HashSet<&str> = module
        .declarations
        .iter()
        .map(|decl| decl.name.as_str())
        .collect();

    let mut functions = HashMap::new();
    for decl in &module.declarations {
        if decl.name == "main" {
            continue;
        }

        let Some(annotation) = decl.type_annotation.as_deref() else {
            continue;
        };
        let Some(function_type) = parse_function_type(annotation) else {
            continue;
        };
        if function_type.result != "Unit" {
            continue;
        }

        let parameter = infer_single_parameter_name(&decl.body, &declaration_names);
        functions.insert(
            decl.name.as_str(),
            EvaluatedFunction {
                body: &decl.body,
                parameter,
                parsed_stmts: decl.parsed_body.as_deref(),
            },
        );
    }

    functions
}

#[derive(Clone, Debug)]
enum IntCallable {
    Lambda(IntLambda),
    Named(String),
}

#[derive(Clone, Debug)]
struct IntLambda {
    parameter: String,
    body: String,
}

struct IntEvaluator<'a> {
    functions: &'a EvaluatedFunctions<'a>,
    depth: usize,
}

impl<'a> IntEvaluator<'a> {
    fn root(functions: &'a EvaluatedFunctions<'a>) -> Self {
        Self {
            functions,
            depth: 0,
        }
    }

    fn descend(&self) -> Option<Self> {
        if self.depth >= MAX_EVAL_DEPTH {
            return None;
        }

        Some(Self {
            functions: self.functions,
            depth: self.depth + 1,
        })
    }

    fn eval_expr(
        &self,
        expr: &str,
        locals: &HashMap<String, i64>,
        callables: &HashMap<String, IntCallable>,
    ) -> Option<i64> {
        let expr = expr.trim();
        if expr.is_empty() {
            return None;
        }

        if let Some(value) = self.eval_binary_expr(expr, locals, callables) {
            return Some(value);
        }

        if is_int_literal(expr) {
            return expr.parse().ok();
        }

        if let Some(value) = locals.get(expr) {
            return Some(*value);
        }

        if let Some((callee, arg_expr)) = parse_call(expr) {
            let nested = self.descend()?;
            let arg = nested.eval_expr(arg_expr, locals, callables)?;

            if let Some(callable) = callables.get(callee) {
                return nested.eval_callable(callable, arg, callables);
            }

            let function = self.functions.get(callee)?;
            return nested.eval_function(function, Some(arg));
        }

        let function = self.functions.get(expr)?;
        self.descend()?.eval_function(function, None)
    }

    fn eval_binary_expr(
        &self,
        expr: &str,
        locals: &HashMap<String, i64>,
        callables: &HashMap<String, IntCallable>,
    ) -> Option<i64> {
        if let Some((left, right)) = expr.split_once(" + ") {
            return self.eval_binary_operands(left, right, locals, callables, i64::checked_add);
        }

        if let Some((left, right)) = expr.split_once(" * ") {
            return self.eval_binary_operands(left, right, locals, callables, i64::checked_mul);
        }

        None
    }

    fn eval_binary_operands(
        &self,
        left: &str,
        right: &str,
        locals: &HashMap<String, i64>,
        callables: &HashMap<String, IntCallable>,
        op: fn(i64, i64) -> Option<i64>,
    ) -> Option<i64> {
        let nested = self.descend()?;
        let left_value = nested.eval_expr(left, locals, callables)?;
        let right_value = nested.eval_expr(right, locals, callables)?;
        op(left_value, right_value)
    }

    fn eval_function(&self, function: &EvaluatedFunction<'a>, arg: Option<i64>) -> Option<i64> {
        let mut locals = HashMap::new();
        let callables = HashMap::new();
        seed_locals_from_parameter(&mut locals, function.parameter, arg);

        let mut result_expr = None;
        for line in code_lines(function.body) {
            if let Some((name, expr)) = split_binding(line) {
                let value = self.descend()?.eval_expr(expr, &locals, &callables);
                assign_local(name, value, &mut locals);
                continue;
            }

            if parse_print_call(line).is_some() {
                return None;
            }

            result_expr = Some(line);
        }

        let expr = result_expr?;
        self.descend()?.eval_expr(expr, &locals, &callables)
    }

    fn eval_callable(
        &self,
        callable: &IntCallable,
        arg: i64,
        callables: &HashMap<String, IntCallable>,
    ) -> Option<i64> {
        match callable {
            IntCallable::Lambda(lambda) => self.eval_lambda(lambda, arg, callables),
            IntCallable::Named(name) => {
                let expr = format!("{} {}", name, arg);
                self.eval_expr(&expr, &HashMap::new(), callables)
            }
        }
    }

    fn eval_lambda(
        &self,
        lambda: &IntLambda,
        arg: i64,
        callables: &HashMap<String, IntCallable>,
    ) -> Option<i64> {
        let mut locals = HashMap::new();
        locals.insert(lambda.parameter.clone(), arg);
        self.descend()?.eval_expr(&lambda.body, &locals, callables)
    }
}

enum Statement<'a> {
    Binding { name: &'a str, expr: &'a str },
    Print(&'a str),
    Expr(&'a str),
}

fn parse_statement(line: &str) -> Statement<'_> {
    if let Some((name, expr)) = split_binding(line) {
        return Statement::Binding { name, expr };
    }

    if let Some(expr) = parse_print_call(line) {
        return Statement::Print(expr);
    }

    Statement::Expr(line)
}

fn statements(body: &str) -> impl Iterator<Item = Statement<'_>> {
    code_lines(body).map(parse_statement)
}

struct RuntimeOutputResolver<'m> {
    locals: RuntimeLocals,
    outputs: Vec<String>,
    module: &'m Module,
    /// Active handlers in lexical order; later entries are more deeply nested.
    active_handler_stack: Vec<usize>,
    resume_tokens: Vec<ResumeToken>,
    optimized_resume_tokens: Vec<OptimizedResumeToken>,
    runtime_error: Option<String>,
    execution_mode: lower::EffectExecutionMode,
}

#[derive(Clone)]
struct Continuation {
    consumed: bool,
}

#[derive(Clone)]
struct ResumeToken {
    handler_decl_idx: usize,
    continuation: Continuation,
    resumed_value: Option<RuntimeValue>,
}

#[derive(Clone)]
struct OptimizedResumeToken {
    handler_decl_idx: usize,
    consumed: bool,
    resumed_value: Option<RuntimeValue>,
}

#[derive(Clone)]
struct ResolvedHandlerMethod {
    handler_decl_idx: usize,
    method: HandlerMethod,
}

struct RuntimeEvaluators<'a, 'b> {
    int: &'b IntEvaluator<'a>,
    list: &'b ListIntEvaluator<'a>,
    unit: &'b EvaluatedFunctions<'a>,
}

impl<'m> RuntimeOutputResolver<'m> {
    fn resolve(
        module: &'m Module,
        body: &str,
        parsed_stmts: Option<&[Stmt]>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        execution_mode: lower::EffectExecutionMode,
    ) -> Option<String> {
        let mut resolver = Self {
            locals: RuntimeLocals::default(),
            outputs: Vec::new(),
            module,
            active_handler_stack: Vec::new(),
            resume_tokens: Vec::new(),
            optimized_resume_tokens: Vec::new(),
            runtime_error: None,
            execution_mode,
        };

        if let Some(stmts) = parsed_stmts {
            // AST-based path (preferred when parsed_body is available)
            for stmt in stmts {
                if resolver.ingest_ast_statement(stmt, evaluators).is_none() {
                    if resolver.runtime_error.is_some() {
                        break;
                    }
                    return None;
                }
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

        if let Some(err) = &resolver.runtime_error {
            let err_line = format!("runtime error: {}", err);
            if resolver.outputs.is_empty() {
                return Some(err_line);
            }
            return Some(format!("{}\n{}", resolver.outputs.join("\n"), err_line));
        }

        if resolver.outputs.is_empty() {
            None
        } else {
            Some(resolver.outputs.join("\n"))
        }
    }

    fn ingest_ast_statement(
        &mut self,
        stmt: &Stmt,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        match stmt {
            Stmt::Binding { name, value } => {
                // Propagate None so the caller can fall back to the string path
                // rather than silently dropping the binding.
                let runtime_val = self.eval_ast_value(value, evaluators)?;
                self.locals.store(name, runtime_val);
                Some(())
            }
            Stmt::Expr(expr) => self.eval_ast_side_effect(expr, evaluators),
            Stmt::Using { handlers, body } => {
                let pushed = self.push_handlers_by_name(handlers);
                let result = (|| -> Option<()> {
                    for stmt in body {
                        self.ingest_ast_statement(stmt, evaluators)?;
                    }
                    Some(())
                })();
                self.pop_active_handlers(pushed);
                result
            }
        }
    }

    fn push_handlers_by_name(&mut self, handlers: &[String]) -> usize {
        let mut pushed = 0;
        for handler_name in handlers {
            if let Some(idx) = self
                .module
                .handler_declarations
                .iter()
                .position(|h| &h.name == handler_name)
            {
                self.active_handler_stack.push(idx);
                pushed += 1;
            }
        }
        pushed
    }

    fn pop_active_handlers(&mut self, count: usize) {
        let next_len = self.active_handler_stack.len().saturating_sub(count);
        self.active_handler_stack.truncate(next_len);
    }

    fn eval_ast_side_effect(
        &mut self,
        expr: &Expr,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        match expr {
            // print <arg>  —  handle before delegating to string path because
            // `eval_side_effect` routes through `execute_unit_call` which does not
            // know about the `print` builtin.
            Expr::Call { callee, arg } if matches!(callee.as_ref(), Expr::Var(n) if n == BUILTIN_PRINT) =>
            {
                let value = self.eval_ast_value(arg, evaluators)?;
                self.outputs.push(value.to_output_text());
                Some(())
            }
            // value |> print
            Expr::Pipeline { value, callee } if callee == BUILTIN_PRINT => {
                let v = self.eval_ast_value(value, evaluators)?;
                self.outputs.push(v.to_output_text());
                Some(())
            }
            // Qualified effect call: Effect.method arg  (e.g. Log.log "msg")
            // Must come before the bare-Var arm so the Qualified guard takes precedence.
            Expr::Call { callee, arg } if matches!(callee.as_ref(), Expr::Qualified { .. }) => {
                let Expr::Qualified { receiver, member } = callee.as_ref() else {
                    unreachable!()
                };
                let method = self.find_handler_method_for_effect(receiver, member);
                if let Some(method) = method {
                    let arg_val = self.eval_ast_value(arg, evaluators)?;
                    // depth=0: this is a top-level call; dispatch_handler_method adds 1 internally.
                    return self.dispatch_handler_method(&method, arg_val, evaluators, 0);
                }
                // No active handler for this qualified call; fall through to string path.
                let repr = expr.to_str_repr()?;
                self.eval_side_effect(&repr, evaluators)
            }
            // Other expression statements: try AST unit-call path.
            Expr::Call { callee, arg } if matches!(callee.as_ref(), Expr::Var(_)) => {
                let Expr::Var(fn_name) = callee.as_ref() else {
                    unreachable!()
                };
                if let Some(arg_val) = self.eval_ast_value(arg, evaluators) {
                    // Bare effect method call (e.g. `log "msg"`) — check active handlers first.
                    let bare_method = self.find_handler_method_by_name(fn_name);
                    if let Some(method) = bare_method {
                        // depth=0: top-level call; dispatch_handler_method adds 1 internally.
                        return self.dispatch_handler_method(&method, arg_val, evaluators, 0);
                    }
                    if self
                        .execute_unit_call_ast(
                            fn_name,
                            arg_val.clone(),
                            &RuntimeLocals::default(),
                            &HashMap::new(),
                            evaluators,
                            0,
                        )
                        .is_some()
                    {
                        return Some(());
                    }
                    // Fallback: execute any declaration (including non-Unit return) for side effects.
                    if self
                        .execute_decl_as_side_effect(fn_name, arg_val, evaluators, 0)
                        .is_some()
                    {
                        return Some(());
                    }
                }
                let repr = expr.to_str_repr()?;
                self.eval_side_effect(&repr, evaluators)
            }
            Expr::Pipeline { value, callee } => {
                if let Some(v) = self.eval_ast_value(value, evaluators) {
                    // Pipeline into bare effect method call: e.g. `"msg" |> log`.
                    let bare_method = self.find_handler_method_by_name(callee);
                    if let Some(method) = bare_method {
                        // depth=0: top-level call; dispatch_handler_method adds 1 internally.
                        return self.dispatch_handler_method(&method, v, evaluators, 0);
                    }
                    if self
                        .execute_unit_call_ast(
                            callee,
                            v,
                            &RuntimeLocals::default(),
                            &HashMap::new(),
                            evaluators,
                            0,
                        )
                        .is_some()
                    {
                        return Some(());
                    }
                }
                let repr = expr.to_str_repr()?;
                self.eval_side_effect(&repr, evaluators)
            }
            // All other expression statements: delegate to string-based path.
            _ => {
                let repr = expr.to_str_repr()?;
                self.eval_side_effect(&repr, evaluators)
            }
        }
    }

    fn eval_ast_value(
        &mut self,
        expr: &Expr,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<RuntimeValue> {
        self.eval_expr_ast(expr, &self.locals.clone(), &HashMap::new(), evaluators, 0)
    }

    fn ingest_statement(
        &mut self,
        statement: Statement<'_>,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        match statement {
            Statement::Binding { name, expr } => self.bind_local(name, expr, evaluators),
            Statement::Print(expr) => self.capture_print(expr, evaluators),
            Statement::Expr(expr) => self.eval_side_effect(expr, evaluators),
        }
    }

    fn bind_local(
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

    fn capture_print(&mut self, expr: &str, evaluators: &RuntimeEvaluators<'_, '_>) -> Option<()> {
        self.capture_output_from_expr(expr, evaluators)
    }

    fn eval_side_effect(
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

    fn capture_output_from_expr(
        &mut self,
        expr: &str,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        let value = self.eval_value(expr, evaluators)?;
        self.outputs.push(value.to_output_text());
        Some(())
    }

    fn eval_value(
        &self,
        expr: &str,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<RuntimeValue> {
        let callables = HashMap::new();
        self.eval_value_with_context(expr, &self.locals, &callables, evaluators)
    }

    fn eval_value_with_context(
        &self,
        expr: &str,
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<RuntimeValue> {
        if let Some((left, callee)) = parse_pipeline(expr) {
            let left_value = self.eval_value_with_context(left, locals, callables, evaluators)?;
            return self.apply_pipeline(callee, left_value, locals, callables, evaluators, 0);
        }

        if let Some(text) = eval_string_expr(expr, &locals.string_values) {
            return Some(RuntimeValue::String(text));
        }

        if let Some(value) = evaluators
            .int
            .eval_expr(expr, &locals.int_values, callables)
        {
            return Some(RuntimeValue::Int(value));
        }

        if let Some(values) = evaluators.list.eval_expr(expr, &locals.list_int_values) {
            return Some(RuntimeValue::ListInt(values));
        }

        None
    }

    fn apply_pipeline(
        &self,
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

    /// Evaluate an `Expr` node directly, without calling `to_str_repr()`.
    ///
    /// Returns `None` when the expression is not yet supported by the native
    /// evaluator (caller should fall back to the string path).
    fn eval_expr_ast(
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
            Expr::InterpolatedString(parts) => {
                let mut out = String::new();
                for part in parts {
                    match part {
                        InterpolatedPart::Text(text) => out.push_str(text),
                        InterpolatedPart::Expr(expr) => {
                            let value =
                                self.eval_expr_ast(expr, locals, callables, evaluators, depth + 1)?;
                            out.push_str(&value.to_output_text());
                        }
                    }
                }
                Some(RuntimeValue::String(out))
            }
            Expr::Var(name) => locals.get(name),
            Expr::BinOp { op, left, right } => {
                let lv = self.eval_expr_ast(left, locals, callables, evaluators, depth + 1)?;
                let rv = self.eval_expr_ast(right, locals, callables, evaluators, depth + 1)?;
                match (lv, rv) {
                    (RuntimeValue::Int(l), RuntimeValue::Int(r)) => match op {
                        goby_core::BinOpKind::Add => Some(RuntimeValue::Int(l.checked_add(r)?)),
                        goby_core::BinOpKind::Mul => Some(RuntimeValue::Int(l.checked_mul(r)?)),
                        goby_core::BinOpKind::Eq => Some(RuntimeValue::Bool(l == r)),
                    },
                    _ => None,
                }
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } if method == "concat" && receiver == "string" && args.len() == 2 => {
                let av = self.eval_expr_ast(&args[0], locals, callables, evaluators, depth + 1)?;
                let bv = self.eval_expr_ast(&args[1], locals, callables, evaluators, depth + 1)?;
                match (av, bv) {
                    (RuntimeValue::String(a), RuntimeValue::String(b)) => {
                        Some(RuntimeValue::String(format!("{}{}", a, b)))
                    }
                    _ => None,
                }
            }
            Expr::ListLit(items) => {
                let mut out = Vec::with_capacity(items.len());
                for item in items {
                    match self.eval_expr_ast(item, locals, callables, evaluators, depth + 1)? {
                        RuntimeValue::Int(n) => out.push(n),
                        _ => return None,
                    }
                }
                Some(RuntimeValue::ListInt(out))
            }
            Expr::Call { callee, arg } => {
                // Positional single-field record constructor sugar: `Ctor(value)` → `Ctor(field: value)`.
                // Apply when callee is a bare name that matches a known single-field record constructor.
                if let Expr::Var(ctor_name) = callee.as_ref()
                    && let Some(field_name) = self.single_field_constructor_field(ctor_name)
                {
                    let val = self.eval_expr_ast(arg, locals, callables, evaluators, depth + 1)?;
                    let mut fields = HashMap::new();
                    fields.insert(field_name, val);
                    return Some(RuntimeValue::Record {
                        constructor: ctor_name.clone(),
                        fields,
                    });
                }

                if let Expr::Var(fn_name) = callee.as_ref() {
                    // fetch_env_var "VAR_NAME" -> read from process environment
                    if fn_name == "fetch_env_var" {
                        let av =
                            self.eval_expr_ast(arg, locals, callables, evaluators, depth + 1)?;
                        if let RuntimeValue::String(var_name) = av {
                            let val = std::env::var(&var_name).unwrap_or_default();
                            return Some(RuntimeValue::String(val));
                        }
                        return None;
                    }
                    if fn_name == "__goby_env_fetch_env_var" {
                        let av =
                            self.eval_expr_ast(arg, locals, callables, evaluators, depth + 1)?;
                        if let RuntimeValue::String(var_name) = av {
                            let val = std::env::var(&var_name).unwrap_or_default();
                            return Some(RuntimeValue::String(val));
                        }
                        return None;
                    }
                    if fn_name == "__goby_string_length" {
                        let av =
                            self.eval_expr_ast(arg, locals, callables, evaluators, depth + 1)?;
                        if let RuntimeValue::String(value) = av {
                            let len = i64::try_from(value.chars().count()).ok()?;
                            return Some(RuntimeValue::Int(len));
                        }
                        return None;
                    }

                    let arg_val =
                        self.eval_expr_ast(arg, locals, callables, evaluators, depth + 1)?;
                    // Bare handler method call in value position (e.g. `next 0` inside `using`).
                    // Check this before Int/List function paths so effect ops can take Int/List args.
                    if let Some(method) = self.find_handler_method_by_name(fn_name) {
                        return self.dispatch_handler_method_as_value(
                            &method,
                            arg_val,
                            evaluators,
                            depth + 1,
                        );
                    }
                    // Int function path
                    if let RuntimeValue::Int(arg_int) = arg_val {
                        if let Some(callable) = callables.get(fn_name) {
                            return evaluators
                                .int
                                .eval_callable(callable, arg_int, callables)
                                .map(RuntimeValue::Int);
                        }
                        if let Some(function) = evaluators.int.functions.get(fn_name.as_str()) {
                            return evaluators
                                .int
                                .eval_function(function, Some(arg_int))
                                .map(RuntimeValue::Int);
                        }
                    } else if let RuntimeValue::ListInt(arg_list) = arg_val {
                        // List function path
                        if let Some(function) = evaluators.list.functions.get(fn_name.as_str()) {
                            return evaluators
                                .list
                                .eval_function(function, Some(arg_list))
                                .map(RuntimeValue::ListInt);
                        }
                    }
                }
                // Qualified callee: Effect.method arg  (e.g. Log.log result, env.from_env name)
                // Try exact effect-name match first, then member-name-only scan.
                if let Expr::Qualified { receiver, member } = callee.as_ref() {
                    let method = self
                        .find_handler_method_for_effect(receiver, member)
                        .or_else(|| self.find_handler_method_by_name(member));
                    if let Some(method) = method {
                        let arg_val =
                            self.eval_expr_ast(arg, locals, callables, evaluators, depth + 1)?;
                        return self.dispatch_handler_method_as_value(
                            &method,
                            arg_val,
                            evaluators,
                            depth + 1,
                        );
                    }
                }
                // callee is not a plain Var (e.g. a curried call or lambda
                // application) — not yet supported by the native evaluator.
                None // NOTE: qualified callee dispatch is handled above
            }
            Expr::Pipeline { value, callee } => {
                let v = self.eval_expr_ast(value, locals, callables, evaluators, depth + 1)?;
                self.apply_pipeline(callee, v, locals, callables, evaluators, depth + 1)
            }
            // Record construction: evaluate each field, build RuntimeValue::Record.
            Expr::RecordConstruct {
                constructor,
                fields,
            } => {
                let mut field_map = HashMap::new();
                for (field_name, field_expr) in fields {
                    let field_val =
                        self.eval_expr_ast(field_expr, locals, callables, evaluators, depth + 1)?;
                    field_map.insert(field_name.clone(), field_val);
                }
                Some(RuntimeValue::Record {
                    constructor: constructor.clone(),
                    fields: field_map,
                })
            }
            // Qualified access: `receiver.member`
            // If `receiver` is a local record variable, return the field value.
            // If `receiver` is absent from locals (e.g. a union type name), return
            // the member name as a string (e.g. `UserStatus.Activated` → `"Activated"`).
            // If `receiver` is present but not a Record, fall back to None.
            Expr::Qualified { receiver, member } => {
                match locals.get(receiver) {
                    Some(RuntimeValue::Record { fields, .. }) => fields.get(member).cloned(),
                    None => {
                        // Treat as a type/module-qualified constructor name.
                        Some(RuntimeValue::String(member.clone()))
                    }
                    Some(_) => None,
                }
            }
            Expr::Case { scrutinee, arms } => {
                let scrutinee_val =
                    self.eval_expr_ast(scrutinee, locals, callables, evaluators, depth + 1)?;
                for arm in arms {
                    let matched = match (&arm.pattern, &scrutinee_val) {
                        (CasePattern::Wildcard, _) => true,
                        (CasePattern::IntLit(n), RuntimeValue::Int(v)) => n == v,
                        (CasePattern::StringLit(s), RuntimeValue::String(v)) => s == v,
                        (CasePattern::BoolLit(b), RuntimeValue::Bool(v)) => b == v,
                        _ => false,
                    };
                    if matched {
                        return self.eval_expr_ast(
                            &arm.body,
                            locals,
                            callables,
                            evaluators,
                            depth + 1,
                        );
                    }
                }
                None
            }
            Expr::If {
                condition,
                then_expr,
                else_expr,
            } => {
                let cond_val =
                    self.eval_expr_ast(condition, locals, callables, evaluators, depth + 1)?;
                match cond_val {
                    RuntimeValue::Bool(true) => {
                        self.eval_expr_ast(then_expr, locals, callables, evaluators, depth + 1)
                    }
                    RuntimeValue::Bool(false) => {
                        self.eval_expr_ast(else_expr, locals, callables, evaluators, depth + 1)
                    }
                    _ => None,
                }
            }
            Expr::Resume { value } => {
                let resumed =
                    self.eval_expr_ast(value, locals, callables, evaluators, depth + 1)?;
                self.resume_through_active_continuation_bridge(resumed)
            }
            // <module>.fetch_env_var(str) -> String  (e.g. env.fetch_env_var(str))
            Expr::MethodCall { method, args, .. }
                if method == "fetch_env_var" && args.len() == 1 =>
            {
                let av = self.eval_expr_ast(&args[0], locals, callables, evaluators, depth + 1)?;
                if let RuntimeValue::String(var_name) = av {
                    let val = std::env::var(&var_name).unwrap_or_default();
                    return Some(RuntimeValue::String(val));
                }
                None
            }
            // string.split(s, delim) -> ListString
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
            // <alias>.join(list, sep) -> String  (alias is the import alias for goby/list)
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
            // Lambda as top-level value — not needed in main, return None to fall back.
            Expr::Lambda { .. } | Expr::TupleLit(_) | Expr::MethodCall { .. } => None,
        }
    }

    /// Execute a single AST statement inside a unit-returning function body.
    fn execute_unit_ast_stmt(
        &mut self,
        stmt: &Stmt,
        locals: &mut RuntimeLocals,
        callables: &mut HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        match stmt {
            Stmt::Binding { name, value } => {
                let v = self.eval_expr_ast(value, locals, callables, evaluators, depth)?;
                locals.store(name, v);
                Some(())
            }
            Stmt::Expr(expr) => {
                // print <arg>
                if let Expr::Call { callee, arg } = expr
                    && matches!(callee.as_ref(), Expr::Var(n) if n == BUILTIN_PRINT)
                {
                    let value = self.eval_expr_ast(arg, locals, callables, evaluators, depth)?;
                    self.outputs.push(value.to_output_text());
                    return Some(());
                }
                // value |> print
                if let Expr::Pipeline { value, callee } = expr
                    && callee == BUILTIN_PRINT
                {
                    let v = self.eval_expr_ast(value, locals, callables, evaluators, depth)?;
                    self.outputs.push(v.to_output_text());
                    return Some(());
                }
                // Qualified effect call: Effect.method arg  (e.g. Log.log result)
                if let Expr::Call { callee, arg } = expr
                    && let Expr::Qualified { receiver, member } = callee.as_ref()
                {
                    let method = self.find_handler_method_for_effect(receiver, member);
                    if let Some(method) = method {
                        let arg_val =
                            self.eval_expr_ast(arg, locals, callables, evaluators, depth)?;
                        return self.dispatch_handler_method(
                            &method,
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
                    // Evaluate arg once.
                    let arg_val = self.eval_expr_ast(arg, locals, callables, evaluators, depth)?;
                    // Bare effect method call: e.g. `log env_var`.
                    let bare_method = self.find_handler_method_by_name(fn_name);
                    if let Some(method) = bare_method {
                        return self.dispatch_handler_method(
                            &method,
                            arg_val,
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
                        return Some(());
                    }
                    // Try executing as general declaration for side effects.
                    if self
                        .execute_decl_as_side_effect(fn_name, arg_val, evaluators, depth + 1)
                        .is_some()
                    {
                        return Some(());
                    }
                    let repr = expr.to_str_repr()?;
                    return self.execute_unit_call(&repr, locals, callables, evaluators);
                }
                // Bare Var expression: discarded return value (e.g. last line of Int fn body).
                if let Expr::Var(_) | Expr::IntLit(_) | Expr::StringLit(_) | Expr::BoolLit(_) = expr
                {
                    return Some(());
                }
                if let Expr::Pipeline { value, callee } = expr {
                    if let Some(v) = self.eval_expr_ast(value, locals, callables, evaluators, depth)
                        && self
                            .execute_unit_call_ast(callee, v, locals, callables, evaluators, depth)
                            .is_some()
                    {
                        return Some(());
                    }
                    let repr = expr.to_str_repr()?;
                    return self.execute_unit_call(&repr, locals, callables, evaluators);
                }
                let repr = expr.to_str_repr()?;
                self.execute_unit_call(&repr, locals, callables, evaluators)
            }
            Stmt::Using { handlers, body } => {
                let pushed = self.push_handlers_by_name(handlers);
                let result = (|| -> Option<()> {
                    for stmt in body {
                        self.execute_unit_ast_stmt(stmt, locals, callables, evaluators, depth + 1)?;
                    }
                    Some(())
                })();
                self.pop_active_handlers(pushed);
                result
            }
        }
    }

    fn execute_unit_call(
        &mut self,
        expr: &str,
        caller_locals: &RuntimeLocals,
        caller_callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        let (callee, arg_expr) = match parse_call(expr) {
            Some((callee, arg_expr)) => (callee, Some(arg_expr)),
            None if is_identifier(expr) => (expr.trim(), None),
            None => return None,
        };

        let function = evaluators.unit.get(callee)?;
        let mut function_locals = RuntimeLocals::default();
        let mut function_callables = HashMap::new();

        if let Some(parameter) = function.parameter {
            let arg_expr = arg_expr?;
            if let Some(callable) = parse_int_callable(arg_expr) {
                function_callables.insert(parameter.to_string(), callable);
            } else if let Some(RuntimeValue::Int(value)) =
                self.eval_value_with_context(arg_expr, caller_locals, caller_callables, evaluators)
            {
                function_locals
                    .int_values
                    .insert(parameter.to_string(), value);
            } else {
                return None;
            }
        }

        if let Some(stmts) = function.parsed_stmts {
            for (i, stmt) in stmts.iter().enumerate() {
                self.execute_unit_ast_stmt(
                    stmt,
                    &mut function_locals,
                    &mut function_callables,
                    evaluators,
                    i + 1,
                )?;
            }
        } else {
            for statement in statements(function.body) {
                match statement {
                    Statement::Binding { name, expr } => {
                        // Propagate None on eval failure rather than silently
                        // clearing the binding and continuing.
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
                        self.outputs.push(value.to_output_text());
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

    /// Execute a unit-returning function call from the AST path.
    ///
    /// `fn_name` is the callee name; `arg_val` is the already-evaluated argument
    /// value.
    fn execute_unit_call_ast(
        &mut self,
        fn_name: &str,
        arg_val: RuntimeValue,
        caller_locals: &RuntimeLocals,
        caller_callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        let function = evaluators.unit.get(fn_name)?;
        let mut function_locals = RuntimeLocals::default();
        let mut function_callables = HashMap::new();

        if let Some(stmts) = function.parsed_stmts {
            // AST path: move arg_val directly into locals (no clone needed).
            if let Some(parameter) = function.parameter {
                function_locals.store(parameter, arg_val);
            }
            for stmt in stmts {
                self.execute_unit_ast_stmt(
                    stmt,
                    &mut function_locals,
                    &mut function_callables,
                    evaluators,
                    depth + 1,
                )?;
            }
            Some(())
        } else {
            // Fall back to the string-based path for functions without parsed AST.
            // arg_text is computed here only, not on the hot AST path.
            let call_expr = if function.parameter.is_some() {
                format!("{} {}", fn_name, arg_val.to_expression_text())
            } else {
                fn_name.to_string()
            };
            self.execute_unit_call(&call_expr, caller_locals, caller_callables, evaluators)
        }
    }

    /// Find the handler method for a qualified effect call like `Log.log`.
    /// Returns resolved handler info with declaration index.
    fn find_handler_method_for_effect(
        &self,
        effect_name: &str,
        method_name: &str,
    ) -> Option<ResolvedHandlerMethod> {
        for &idx in self.active_handler_stack.iter().rev() {
            let handler = &self.module.handler_declarations[idx];
            if handler.effect != effect_name {
                continue;
            }
            if let Some(method) = handler.methods.iter().find(|m| m.name == method_name) {
                return Some(ResolvedHandlerMethod {
                    handler_decl_idx: idx,
                    method: method.clone(),
                });
            }
        }
        None
    }

    /// If `ctor_name` is a record constructor with exactly one field, return that field's name.
    /// Used to support positional single-field constructor sugar: `Ctor(value)` → `Ctor(field: value)`.
    fn single_field_constructor_field(&self, ctor_name: &str) -> Option<String> {
        for ty_decl in &self.module.type_declarations {
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

    /// Find a handler method by bare name (e.g. `log`) from nearest to outermost.
    fn find_handler_method_by_name(&self, method_name: &str) -> Option<ResolvedHandlerMethod> {
        for &idx in self.active_handler_stack.iter().rev() {
            let handler = &self.module.handler_declarations[idx];
            if let Some(m) = handler.methods.iter().find(|m| m.name == method_name) {
                return Some(ResolvedHandlerMethod {
                    handler_decl_idx: idx,
                    method: m.clone(),
                });
            }
        }
        None
    }

    fn set_runtime_error_once(&mut self, message: impl Into<String>) {
        if self.runtime_error.is_none() {
            self.runtime_error = Some(message.into());
        }
    }

    fn push_resume_token_for_handler(&mut self, handler_decl_idx: usize) -> usize {
        self.resume_tokens.push(ResumeToken {
            handler_decl_idx,
            continuation: Continuation { consumed: false },
            resumed_value: None,
        });
        self.resume_tokens.len() - 1
    }

    fn take_resume_token_result(&mut self, token_idx: usize) -> Option<Option<RuntimeValue>> {
        if token_idx + 1 != self.resume_tokens.len() {
            self.set_runtime_error_once(ERR_RESUME_STACK_MISMATCH);
            return None;
        }
        let token = self.resume_tokens.pop()?;
        Some(token.resumed_value)
    }

    fn current_resume_token_mut(&mut self) -> Option<&mut ResumeToken> {
        self.resume_tokens.last_mut()
    }

    fn push_optimized_resume_token_for_handler(&mut self, handler_decl_idx: usize) -> usize {
        self.optimized_resume_tokens.push(OptimizedResumeToken {
            handler_decl_idx,
            consumed: false,
            resumed_value: None,
        });
        self.optimized_resume_tokens.len() - 1
    }

    fn take_optimized_resume_token_result(
        &mut self,
        token_idx: usize,
    ) -> Option<Option<RuntimeValue>> {
        if token_idx + 1 != self.optimized_resume_tokens.len() {
            self.set_runtime_error_once(ERR_RESUME_STACK_MISMATCH);
            return None;
        }
        let token = self.optimized_resume_tokens.pop()?;
        Some(token.resumed_value)
    }

    fn current_optimized_resume_token_mut(&mut self) -> Option<&mut OptimizedResumeToken> {
        self.optimized_resume_tokens.last_mut()
    }

    fn resume_token_has_value(&self, token_idx: usize) -> bool {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => self
                .resume_tokens
                .get(token_idx)
                .and_then(|token| token.resumed_value.as_ref())
                .is_some(),
            lower::EffectExecutionMode::TypedContinuationOptimized => self
                .optimized_resume_tokens
                .get(token_idx)
                .and_then(|token| token.resumed_value.as_ref())
                .is_some(),
        }
    }

    fn begin_handler_continuation_bridge(&mut self, handler_decl_idx: usize) -> usize {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => {
                self.push_resume_token_for_handler(handler_decl_idx)
            }
            lower::EffectExecutionMode::TypedContinuationOptimized => {
                self.push_optimized_resume_token_for_handler(handler_decl_idx)
            }
        }
    }

    fn finish_handler_continuation_bridge(
        &mut self,
        token_idx: usize,
    ) -> Option<Option<RuntimeValue>> {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => {
                self.take_resume_token_result(token_idx)
            }
            lower::EffectExecutionMode::TypedContinuationOptimized => {
                self.take_optimized_resume_token_result(token_idx)
            }
        }
    }

    fn resume_through_active_continuation_bridge(
        &mut self,
        resumed: RuntimeValue,
    ) -> Option<RuntimeValue> {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => {
                self.resume_through_active_continuation_fallback(resumed)
            }
            lower::EffectExecutionMode::TypedContinuationOptimized => {
                self.resume_through_active_continuation_optimized(resumed)
            }
        }
    }

    fn resume_through_active_continuation_fallback(
        &mut self,
        resumed: RuntimeValue,
    ) -> Option<RuntimeValue> {
        let Some(token_ro) = self.resume_tokens.last() else {
            self.set_runtime_error_once(ERR_RESUME_MISSING);
            return None;
        };
        if token_ro.continuation.consumed {
            self.set_runtime_error_once(ERR_RESUME_CONSUMED);
            return None;
        }
        if token_ro.handler_decl_idx >= self.module.handler_declarations.len() {
            self.set_runtime_error_once(ERR_RESUME_HANDLER_MISMATCH);
            return None;
        }
        let Some(token) = self.current_resume_token_mut() else {
            self.set_runtime_error_once(ERR_RESUME_MISSING);
            return None;
        };
        token.continuation.consumed = true;
        token.resumed_value = Some(resumed.clone());
        Some(resumed)
    }

    fn resume_through_active_continuation_optimized(
        &mut self,
        resumed: RuntimeValue,
    ) -> Option<RuntimeValue> {
        let Some(token_ro) = self.optimized_resume_tokens.last() else {
            self.set_runtime_error_once(ERR_RESUME_MISSING);
            return None;
        };
        if token_ro.consumed {
            self.set_runtime_error_once(ERR_RESUME_CONSUMED);
            return None;
        }
        if token_ro.handler_decl_idx >= self.module.handler_declarations.len() {
            self.set_runtime_error_once(ERR_RESUME_HANDLER_MISMATCH);
            return None;
        }
        let Some(token) = self.current_optimized_resume_token_mut() else {
            self.set_runtime_error_once(ERR_RESUME_MISSING);
            return None;
        };
        token.consumed = true;
        token.resumed_value = Some(resumed.clone());
        Some(resumed)
    }

    fn dispatch_handler_method_core(
        &mut self,
        method: &ResolvedHandlerMethod,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
        produce_value: bool,
    ) -> Option<Option<RuntimeValue>> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let stmts = method.method.parsed_body.as_deref()?;
        let token_idx = self.begin_handler_continuation_bridge(method.handler_decl_idx);
        let previous_stack_len = self.active_handler_stack.len();
        let run_result = (|| -> Option<Option<RuntimeValue>> {
            let mut handler_locals = RuntimeLocals::default();
            if let Some(param) = method.method.params.first() {
                handler_locals.store(param, arg_val);
            }
            let mut handler_callables = HashMap::new();
            let mut last_val: Option<RuntimeValue> = None;
            for stmt in stmts {
                if produce_value {
                    match stmt {
                        Stmt::Binding { name, value } => {
                            let v = self.eval_expr_ast(
                                value,
                                &handler_locals,
                                &handler_callables,
                                evaluators,
                                depth + 1,
                            )?;
                            handler_locals.store(name, v);
                        }
                        Stmt::Expr(expr) => {
                            last_val = self.eval_expr_ast(
                                expr,
                                &handler_locals,
                                &handler_callables,
                                evaluators,
                                depth + 1,
                            );
                            if last_val.is_none() {
                                // Side-effect stmt — try as unit side effect.
                                self.execute_unit_ast_stmt(
                                    stmt,
                                    &mut handler_locals,
                                    &mut handler_callables,
                                    evaluators,
                                    depth + 1,
                                )?;
                                last_val = None;
                            }
                        }
                        Stmt::Using { handlers, body } => {
                            let pushed = self.push_handlers_by_name(handlers);
                            let using_result = (|| -> Option<()> {
                                for inner_stmt in body {
                                    match inner_stmt {
                                        Stmt::Binding { name, value } => {
                                            let v = self.eval_expr_ast(
                                                value,
                                                &handler_locals,
                                                &handler_callables,
                                                evaluators,
                                                depth + 1,
                                            )?;
                                            handler_locals.store(name, v);
                                        }
                                        Stmt::Expr(expr) => {
                                            last_val = self.eval_expr_ast(
                                                expr,
                                                &handler_locals,
                                                &handler_callables,
                                                evaluators,
                                                depth + 1,
                                            );
                                            if last_val.is_none() {
                                                self.execute_unit_ast_stmt(
                                                    inner_stmt,
                                                    &mut handler_locals,
                                                    &mut handler_callables,
                                                    evaluators,
                                                    depth + 1,
                                                )?;
                                                last_val = None;
                                            }
                                        }
                                        Stmt::Using { .. } => {
                                            // Nested using: delegate to execute_unit_ast_stmt for full handling.
                                            self.execute_unit_ast_stmt(
                                                inner_stmt,
                                                &mut handler_locals,
                                                &mut handler_callables,
                                                evaluators,
                                                depth + 1,
                                            )?;
                                        }
                                    }
                                }
                                Some(())
                            })();
                            self.pop_active_handlers(pushed);
                            using_result?;
                        }
                    }
                } else {
                    match stmt {
                        // In unit-position handler execution, still evaluate expressions via AST first
                        // so `resume` works for bare operation statements like `yield "x"`.
                        Stmt::Expr(expr) => {
                            let evaluated = self.eval_expr_ast(
                                expr,
                                &handler_locals,
                                &handler_callables,
                                evaluators,
                                depth + 1,
                            );
                            if evaluated.is_none() {
                                self.execute_unit_ast_stmt(
                                    stmt,
                                    &mut handler_locals,
                                    &mut handler_callables,
                                    evaluators,
                                    depth + 1,
                                )?;
                            }
                        }
                        _ => {
                            self.execute_unit_ast_stmt(
                                stmt,
                                &mut handler_locals,
                                &mut handler_callables,
                                evaluators,
                                depth + 1,
                            )?;
                        }
                    }
                }
                if self.resume_token_has_value(token_idx) {
                    break;
                }
            }
            if produce_value {
                Some(last_val)
            } else {
                Some(None)
            }
        })();
        let resumed = self.finish_handler_continuation_bridge(token_idx);
        // Never leak handler context changes outside an effect call.
        self.active_handler_stack.truncate(previous_stack_len);
        let resumed = resumed?;
        let run_result = run_result?;
        if produce_value {
            Some(resumed.or(run_result))
        } else {
            Some(None)
        }
    }

    /// Execute a handler method and return the last evaluated value.
    /// Used when the handler method produces a value (e.g. `env.from_env`).
    fn dispatch_handler_method_as_value(
        &mut self,
        method: &ResolvedHandlerMethod,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        self.dispatch_handler_method_core(method, arg_val, evaluators, depth, true)?
    }

    /// Execute a handler method body with the given argument.
    fn dispatch_handler_method(
        &mut self,
        method: &ResolvedHandlerMethod,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        let _ = self.dispatch_handler_method_core(method, arg_val, evaluators, depth, false)?;
        Some(())
    }

    /// Execute any declaration (including Int-returning ones) as a side-effect call.
    /// Used when calling functions like `plus_ten_with_log` from a `using` block.
    fn execute_decl_as_side_effect(
        &mut self,
        fn_name: &str,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        // Collect param name and stmts (clone to release the borrow on self.module).
        let (param_name, stmts) = {
            let decl = self
                .module
                .declarations
                .iter()
                .find(|d| d.name == fn_name)?;
            let stmts = decl.parsed_body.as_ref()?.clone();
            let param = decl.params.first().cloned();
            (param, stmts)
        };
        let mut fn_locals = RuntimeLocals::default();
        if let Some(param) = param_name {
            fn_locals.store(&param, arg_val);
        }
        let mut fn_callables = HashMap::new();
        for stmt in &stmts {
            // Bare value expressions at the end of an Int-returning fn: discard silently.
            if let Stmt::Expr(
                Expr::Var(_) | Expr::IntLit(_) | Expr::StringLit(_) | Expr::BoolLit(_),
            ) = stmt
            {
                continue;
            }
            self.execute_unit_ast_stmt(
                stmt,
                &mut fn_locals,
                &mut fn_callables,
                evaluators,
                depth + 1,
            )?;
        }
        Some(())
    }
}

#[derive(Default, Clone)]
struct RuntimeLocals {
    string_values: HashMap<String, String>,
    int_values: HashMap<String, i64>,
    list_int_values: HashMap<String, Vec<i64>>,
    record_values: HashMap<String, RuntimeValue>,
}

impl RuntimeLocals {
    fn store(&mut self, name: &str, value: RuntimeValue) {
        self.clear(name);
        match value {
            RuntimeValue::String(text) => {
                self.string_values.insert(name.to_string(), text);
            }
            RuntimeValue::Int(number) => {
                self.int_values.insert(name.to_string(), number);
            }
            RuntimeValue::ListInt(values) => {
                self.list_int_values.insert(name.to_string(), values);
            }
            record @ RuntimeValue::Record { .. } => {
                self.record_values.insert(name.to_string(), record);
            }
            b @ RuntimeValue::Bool(_) => {
                self.record_values.insert(name.to_string(), b);
            }
            ls @ RuntimeValue::ListString(_) => {
                self.record_values.insert(name.to_string(), ls);
            }
        }
    }

    fn clear(&mut self, name: &str) {
        self.string_values.remove(name);
        self.int_values.remove(name);
        self.list_int_values.remove(name);
        self.record_values.remove(name);
    }

    fn get(&self, name: &str) -> Option<RuntimeValue> {
        if let Some(v) = self.int_values.get(name) {
            return Some(RuntimeValue::Int(*v));
        }
        if let Some(v) = self.string_values.get(name) {
            return Some(RuntimeValue::String(v.clone()));
        }
        if let Some(v) = self.list_int_values.get(name) {
            return Some(RuntimeValue::ListInt(v.clone()));
        }
        if let Some(v) = self.record_values.get(name) {
            return Some(v.clone());
        }
        None
    }
}

#[derive(Clone)]
enum RuntimeValue {
    String(String),
    Int(i64),
    Bool(bool),
    ListInt(Vec<i64>),
    ListString(Vec<String>),
    Record {
        constructor: String,
        fields: HashMap<String, RuntimeValue>,
    },
}

impl RuntimeValue {
    fn to_output_text(&self) -> String {
        match self {
            Self::String(text) => text.clone(),
            Self::Int(value) => value.to_string(),
            Self::Bool(b) => if *b { "True" } else { "False" }.to_string(),
            Self::ListInt(values) => format_list_int(values),
            Self::ListString(values) => {
                let parts: Vec<String> = values.iter().map(|s| format!("\"{}\"", s)).collect();
                format!("[{}]", parts.join(", "))
            }
            // Print the constructor name as a placeholder; field access
            // resolves the actual field value before printing in practice.
            Self::Record { constructor, .. } => constructor.clone(),
        }
    }

    fn to_expression_text(&self) -> String {
        match self {
            Self::String(text) => format!("\"{}\"", text),
            Self::Int(value) => value.to_string(),
            Self::Bool(b) => if *b { "True" } else { "False" }.to_string(),
            Self::ListInt(values) => format_list_int(values),
            Self::ListString(values) => {
                let parts: Vec<String> = values.iter().map(|s| format!("\"{}\"", s)).collect();
                format!("[{}]", parts.join(", "))
            }
            Self::Record { constructor, .. } => constructor.clone(),
        }
    }
}

fn format_list_int(values: &[i64]) -> String {
    let joined = values
        .iter()
        .map(i64::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{}]", joined)
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

    if let Some((left, right)) = parse_string_concat_call(expr) {
        let left_text = eval_string_expr(left, locals)?;
        let right_text = eval_string_expr(right, locals)?;
        return Some(format!("{}{}", left_text, right_text));
    }

    None
}

struct ListIntEvaluator<'a> {
    functions: &'a EvaluatedFunctions<'a>,
    depth: usize,
}

impl<'a> ListIntEvaluator<'a> {
    fn root(functions: &'a EvaluatedFunctions<'a>) -> Self {
        Self {
            functions,
            depth: 0,
        }
    }

    fn descend(&self) -> Option<Self> {
        if self.depth >= MAX_EVAL_DEPTH {
            return None;
        }

        Some(Self {
            functions: self.functions,
            depth: self.depth + 1,
        })
    }

    fn eval_expr(&self, expr: &str, locals: &HashMap<String, Vec<i64>>) -> Option<Vec<i64>> {
        let expr = expr.trim();
        if expr.is_empty() {
            return None;
        }

        if let Some(values) = parse_list_int_literal(expr) {
            return Some(values);
        }

        if let Some(value) = locals.get(expr) {
            return Some(value.clone());
        }

        if let Some((list_expr, lambda_expr)) = parse_map_call(expr) {
            let list_values = self.descend()?.eval_expr(list_expr, locals)?;
            let lambda = parse_map_lambda(lambda_expr)?;
            return apply_map_lambda(&list_values, &lambda);
        }

        if let Some((callee, arg_expr)) = parse_call(expr) {
            let function = self.functions.get(callee)?;
            let arg_values = self.descend()?.eval_expr(arg_expr, locals)?;
            return self.descend()?.eval_function(function, Some(arg_values));
        }

        let function = self.functions.get(expr)?;
        self.descend()?.eval_function(function, None)
    }

    fn eval_function(
        &self,
        function: &EvaluatedFunction<'a>,
        arg: Option<Vec<i64>>,
    ) -> Option<Vec<i64>> {
        let mut locals = HashMap::new();
        seed_locals_from_parameter(&mut locals, function.parameter, arg);

        let mut result_expr = None;
        for line in code_lines(function.body) {
            if let Some((name, expr)) = split_binding(line) {
                let value = self.descend()?.eval_expr(expr, &locals);
                assign_local(name, value, &mut locals);
                continue;
            }

            if parse_print_call(line).is_some() {
                return None;
            }

            result_expr = Some(line);
        }

        let expr = result_expr?;
        self.descend()?.eval_expr(expr, &locals)
    }
}

struct MapLambda {
    parameter: String,
    body: String,
}

fn parse_map_call(expr: &str) -> Option<(&str, &str)> {
    let rest = expr.strip_prefix("map ")?;
    let split_idx = rest.rfind(" (")?;
    let list_expr = rest[..split_idx].trim();
    let lambda_group = rest[split_idx + 1..].trim();
    if list_expr.is_empty() || !lambda_group.starts_with('(') || !lambda_group.ends_with(')') {
        return None;
    }
    let lambda_expr = lambda_group[1..lambda_group.len() - 1].trim();
    if lambda_expr.is_empty() {
        return None;
    }
    Some((list_expr, lambda_expr))
}

fn parse_map_lambda(expr: &str) -> Option<MapLambda> {
    let lambda = parse_inline_lambda(expr)?;
    Some(MapLambda {
        parameter: lambda.parameter,
        body: lambda.body,
    })
}

fn apply_map_lambda(values: &[i64], lambda: &MapLambda) -> Option<Vec<i64>> {
    let empty_functions = HashMap::new();
    let empty_callables = HashMap::new();
    let evaluator = IntEvaluator::root(&empty_functions);
    let mut out = Vec::with_capacity(values.len());
    for value in values {
        let mut locals = HashMap::new();
        locals.insert(lambda.parameter.to_string(), *value);
        let mapped = evaluator.eval_expr(&lambda.body, &locals, &empty_callables)?;
        out.push(mapped);
    }
    Some(out)
}

fn parse_inline_lambda(expr: &str) -> Option<IntLambda> {
    let expr = expr.trim();
    if let Some((head, body)) = expr.split_once("->") {
        let head = head.trim();
        if !head.starts_with('|') || !head.ends_with('|') {
            return None;
        }
        let parameter = head[1..head.len() - 1].trim();
        if !is_identifier(parameter) {
            return None;
        }
        let body = body.trim();
        if body.is_empty() {
            return None;
        }
        return Some(IntLambda {
            parameter: parameter.to_string(),
            body: body.to_string(),
        });
    }

    if expr.contains('_') {
        return Some(IntLambda {
            parameter: "_".to_string(),
            body: expr.to_string(),
        });
    }

    None
}

fn parse_int_callable(expr: &str) -> Option<IntCallable> {
    if let Some(lambda) = parse_inline_lambda(expr) {
        return Some(IntCallable::Lambda(lambda));
    }

    let name = expr.trim();
    if is_identifier(name) {
        return Some(IntCallable::Named(name.to_string()));
    }

    None
}

fn parse_list_int_literal(expr: &str) -> Option<Vec<i64>> {
    if !is_list_literal(expr) {
        return None;
    }

    let inner = &expr[1..expr.len() - 1];
    let inner = inner.trim();
    if inner.is_empty() {
        return Some(Vec::new());
    }

    let mut out = Vec::new();
    for part in inner.split(',') {
        let item = part.trim();
        if !is_int_literal(item) {
            return None;
        }
        out.push(item.parse().ok()?);
    }
    Some(out)
}

#[derive(Clone)]
struct EvaluatedFunction<'a> {
    body: &'a str,
    parameter: Option<&'a str>,
    /// Pre-parsed AST statements; `None` means fall back to string-based evaluation.
    parsed_stmts: Option<&'a [Stmt]>,
}

fn assign_local<T>(name: &str, value: Option<T>, locals: &mut HashMap<String, T>) {
    if let Some(value) = value {
        locals.insert(name.to_string(), value);
    } else {
        locals.remove(name);
    }
}

fn seed_locals_from_parameter<T>(
    locals: &mut HashMap<String, T>,
    parameter: Option<&str>,
    arg: Option<T>,
) {
    if let Some(parameter) = parameter
        && let Some(value) = arg
    {
        locals.insert(parameter.to_string(), value);
    }
}

fn infer_single_parameter_name<'a>(
    body: &'a str,
    declaration_names: &HashSet<&str>,
) -> Option<&'a str> {
    let mut assigned: HashSet<&str> = HashSet::new();
    let mut referenced: HashSet<&str> = HashSet::new();

    for line in code_lines(body) {
        if let Some((name, expr)) = split_binding(line) {
            assigned.insert(name);
            collect_referenced_identifiers(expr, &mut referenced);
            continue;
        }

        if let Some(expr) = parse_print_call(line) {
            collect_referenced_identifiers(expr, &mut referenced);
            continue;
        }

        collect_referenced_identifiers(line, &mut referenced);
    }

    let candidates: Vec<&str> = referenced
        .into_iter()
        .filter(|name| !assigned.contains(name))
        .filter(|name| !declaration_names.contains(name))
        .filter(|name| *name != BUILTIN_PRINT)
        .filter(|name| *name != "map")
        .filter(|name| *name != "_")
        .collect();

    if candidates.len() == 1 {
        Some(candidates[0])
    } else {
        None
    }
}

fn collect_referenced_identifiers<'a>(expr: &'a str, out: &mut HashSet<&'a str>) {
    if let Some((list_expr, _lambda_expr)) = parse_map_call(expr) {
        collect_identifiers(list_expr, out);
        return;
    }
    collect_identifiers(expr, out);
}

fn collect_identifiers<'a>(expr: &'a str, out: &mut HashSet<&'a str>) {
    let mut start = None;
    let mut in_string = false;
    let mut escaped = false;

    for (idx, byte) in expr.as_bytes().iter().copied().enumerate() {
        if in_string {
            if escaped {
                escaped = false;
                continue;
            }
            if byte == b'\\' {
                escaped = true;
                continue;
            }
            if byte == b'"' {
                in_string = false;
            }
            continue;
        }

        if byte == b'"' {
            maybe_insert_identifier(expr, start.take(), idx, out);
            in_string = true;
            continue;
        }

        let is_ident_char = byte.is_ascii_alphanumeric() || byte == b'_';
        if is_ident_char {
            if start.is_none() {
                start = Some(idx);
            }
            continue;
        }

        maybe_insert_identifier(expr, start.take(), idx, out);
    }

    maybe_insert_identifier(expr, start, expr.len(), out);
}

fn maybe_insert_identifier<'a>(
    source: &'a str,
    start: Option<usize>,
    end: usize,
    out: &mut HashSet<&'a str>,
) {
    let Some(start_idx) = start else {
        return;
    };

    let candidate = &source[start_idx..end];
    if is_identifier(candidate) && !is_int_literal(candidate) {
        out.insert(candidate);
    }
}

fn parse_call(expr: &str) -> Option<(&str, &str)> {
    if let Some(open) = expr.find('(')
        && expr.ends_with(')')
    {
        let callee = expr[..open].trim();
        let inner = expr[open + 1..expr.len() - 1].trim();
        if is_identifier(callee) && !inner.is_empty() {
            return Some((callee, inner));
        }
    }

    let mut chars = expr.char_indices();
    let end = chars.find_map(|(idx, ch)| ch.is_whitespace().then_some(idx))?;
    let callee = expr[..end].trim();
    let arg = expr[end..].trim();
    if is_identifier(callee) && !arg.is_empty() {
        Some((callee, arg))
    } else {
        None
    }
}

fn is_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn is_string_literal(expr: &str) -> bool {
    expr.starts_with('"') && expr.ends_with('"') && expr.len() >= 2
}

fn is_int_literal(expr: &str) -> bool {
    let raw = expr.strip_prefix('-').unwrap_or(expr);
    !raw.is_empty() && raw.chars().all(|c| c.is_ascii_digit())
}

fn is_list_literal(expr: &str) -> bool {
    expr.starts_with('[') && expr.ends_with(']')
}

fn code_lines(body: &str) -> impl Iterator<Item = &str> {
    body.lines().map(str::trim).filter(|line| {
        let line = *line;
        !line.is_empty() && !line.starts_with('#')
    })
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
    fn locks_runtime_output_for_function_example() {
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = read_example("function.gb");
        let module = parse_module(&source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module))
                .expect("runtime output should resolve");
        assert_eq!(output, "90\n[30, 40, 50]\n[60, 70]\nsomething\n15");
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
        assert_eq!(output, "something\n15");
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
        assert_eq!(output, "13\nhello");
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
        assert_eq!(output, "tick\ntick\ntick");
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

    // --- bare effect call dispatch in main body (§4.1 patch) ---

    #[test]
    fn bare_effect_call_in_main_using_dispatches_to_handler() {
        // Bug: eval_ast_side_effect (main/top-level path) did NOT route bare Call through
        // find_handler_method_by_name, so `log "hello"` inside `using` produced no output.
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

handler ConsoleLog for Log
  log msg =
    print msg

main : Unit -> Unit
main =
  using ConsoleLog
    log "hello"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("hello"),
            "bare effect call inside main `using` block should dispatch to handler"
        );
    }

    #[test]
    fn qualified_effect_call_in_main_using_dispatches_to_handler() {
        // Bug: eval_ast_side_effect had no arm for Expr::Call { callee: Expr::Qualified },
        // so `Log.log "hello"` inside main body fell through to string-based eval_side_effect.
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

handler ConsoleLog for Log
  log msg =
    print msg

main : Unit -> Unit
main =
  using ConsoleLog
    Log.log "world"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("world"),
            "qualified effect call inside main `using` block should dispatch to handler"
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

handler HandlerA for Alpha
  greet s =
    "from-alpha"

handler HandlerB for Beta
  greet s =
    "from-beta"

main : Unit -> Unit
main =
  using HandlerA, HandlerB
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
    fn nested_using_same_effect_prefers_inner_handler() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

handler OuterLog for Log
  log msg =
    print "outer"

handler InnerLog for Log
  log msg =
    print "inner"

main : Unit -> Unit
main =
  using OuterLog
    using InnerLog
      log "x"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("inner"),
            "nested `using` should dispatch to nearest enclosing handler"
        );
    }

    #[test]
    fn pipeline_effect_call_in_main_using_dispatches_to_handler() {
        // `"msg" |> log` inside main `using` block should dispatch to the active handler.
        // The Pipeline arm in eval_ast_side_effect now checks find_handler_method_by_name.
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Log
  log: String -> Unit

handler ConsoleLog for Log
  log msg =
    print msg

main : Unit -> Unit
main =
  using ConsoleLog
    "piped" |> log
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("piped"),
            "pipeline effect call inside main `using` block should dispatch to handler"
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

handler ConsoleLog for Log
  log msg =
    print msg

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

handler PrintErrorHandler for RaiseError
  raise e =
    print e.message

main : Unit -> Unit
main =
  using PrintErrorHandler
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

handler IterHandler for Iter
  next n =
    resume 7

main : Unit -> Unit
main =
  using IterHandler
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

handler IterHandler for Iter
  next n =
    resume (resume 1)

main : Unit -> Unit
main =
  using IterHandler
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
    fn no_resume_in_value_position_takes_abortive_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

handler IterHandler for Iter
  next n =
    print "handled"

main : Unit -> Unit
main =
  using IterHandler
    print (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output, None,
            "when a value-position operation is handled without `resume`, evaluation follows the abortive path"
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
    fn typed_mode_matches_fallback_for_resume_success_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

handler IterHandler for Iter
  next n =
    resume 7

main : Unit -> Unit
main =
  using IterHandler
    print (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "resume success path");
        assert_eq!(typed.stdout.as_deref(), Some("7"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_no_resume_abortive_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

handler IterHandler for Iter
  next n =
    print "handled"

main : Unit -> Unit
main =
  using IterHandler
    print (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "no-resume abortive path");
        // Current runtime contract: no `resume` in value-position operation takes the abortive
        // path, so handler-body print output is not emitted as final program output.
        assert_eq!(typed.stdout, None);
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_double_resume_error() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

handler IterHandler for Iter
  next n =
    resume (resume 1)

main : Unit -> Unit
main =
  using IterHandler
    print (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "double-resume deterministic error path");
        assert_eq!(typed.stdout, None);
        assert_eq!(typed.runtime_error_kind, Some("continuation_consumed"));
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

handler AHandler for A
  next x =
    resume 1

handler BHandler for B
  next y =
    resume 2

main : Unit -> Unit
main =
  using AHandler, BHandler
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

handler OuterLog for Log
  log msg =
    resume "outer"

handler InnerLog for Log
  log msg =
    resume "inner"

main : Unit -> Unit
main =
  using OuterLog
    using InnerLog
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

handler IterHandler for Iter
  next n =
    resume 7

main : Unit -> Unit
main =
  using IterHandler
    print (next 0)
"#,
            ),
            (
                "double_resume_error_path",
                r#"
effect Iter
  next: Int -> Int

handler IterHandler for Iter
  next n =
    resume (resume 1)

main : Unit -> Unit
main =
  using IterHandler
    print (next 0)
"#,
            ),
            (
                "nested_handler_dispatch_path",
                r#"
effect Log
  log: String -> String

handler OuterLog for Log
  log msg =
    resume "outer"

handler InnerLog for Log
  log msg =
    resume "inner"

main : Unit -> Unit
main =
  using OuterLog
    using InnerLog
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

handler AHandler for A
  next x =
    resume 1

handler BHandler for B
  next y =
    resume 2

main : Unit -> Unit
main =
  using AHandler, BHandler
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

handler AHandler for A
  next x =
    resume 1

handler BHandler for B
  next y =
    resume 2

main : Unit -> Unit
main =
  using AHandler, BHandler
    print (B.next 0)
    print (B.next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("2\n2"),
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
        assert_eq!(expected_text, "42\nTrue");
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
        assert_eq!(expected_text, "Five!\n50\n30");
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
uses_using : Int -> Int
uses_using x =
  using Console
    x

main : Unit -> Unit
main =
  print (uses_using 1)
"#,
                Some(fallback::UnsupportedReason::CallTargetBodyNotNativeSupported),
            ),
            (
                "target_body_not_supported_with_lambda",
                r#"
uses_using_callback : (Int -> Int) -> Int
uses_using_callback f =
  using Console
    f 1

main : Unit -> Unit
main =
  print (uses_using_callback (|x| -> x + 1))
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
uses_using : Int -> Int
uses_using x =
  using Console
    x

main : Unit -> Unit
main =
  print (uses_using 1 2)
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
}
