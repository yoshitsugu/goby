mod backend;
mod call;
mod fallback;
mod layout;
mod lower;
mod planning;
mod support;

use std::collections::{HashMap, HashSet};
use std::io::Read as _;
use std::path::PathBuf;
use unicode_segmentation::UnicodeSegmentation;

use crate::call::flatten_named_call;
use goby_core::{
    CasePattern, Expr, HandlerClause, ListPatternItem, ListPatternTail, Module, Stmt,
    ast::InterpolatedPart, stdlib::StdlibResolver, types::parse_function_type,
};
const ERR_MISSING_MAIN: &str = "Wasm codegen requires a `main` declaration";
const BUILTIN_PRINT: &str = "print";
const PRELUDE_MODULE_PATH: &str = "goby/prelude";
const MAX_EVAL_DEPTH: usize = 32;
const ERR_RESUME_MISSING: &str = "resume used without an active continuation [E-RESUME-MISSING]: `resume` can only be called while executing a handler operation body";
const ERR_RESUME_CONSUMED: &str = "resume continuation already consumed [E-RESUME-CONSUMED]: continuations are one-shot; call `resume` at most once per handled operation";
const ERR_RESUME_STACK_MISMATCH: &str = "internal resume token stack mismatch [E-RESUME-STACK-MISMATCH]: continuation token stack became unbalanced";
const ERR_CALLABLE_DISPATCH_LIST_EACH_CALLBACK: &str = "unsupported callable dispatch [E-CALLABLE-DISPATCH]: goby/list.each callback must be a lambda or function name";
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

fn split_binding(line: &str) -> Option<(&str, &str)> {
    let idx = line.find('=')?;
    let name = line[..idx].trim();
    if !is_identifier(name) {
        return None;
    }
    Some((name, line[idx + 1..].trim()))
}

fn split_mut_binding(line: &str) -> Option<(&str, &str)> {
    let rest = line.strip_prefix("mut ")?;
    split_binding(rest)
}

fn split_assignment(line: &str) -> Option<(&str, &str)> {
    let (name, expr) = line.split_once(":=")?;
    let name = name.trim();
    let expr = expr.trim();
    if !is_identifier(name) || expr.is_empty() {
        return None;
    }
    Some((name, expr))
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

#[derive(Clone)]
enum IntCallable {
    Lambda(IntLambda),
    Named(String),
    AstLambda(Box<AstLambdaCallable>),
}

#[derive(Clone)]
struct AstLambdaCallable {
    parameter: String,
    body: Expr,
    captured_locals: RuntimeLocals,
    captured_callables: HashMap<String, IntCallable>,
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
            IntCallable::AstLambda(_) => None,
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
    MutBinding { name: &'a str, expr: &'a str },
    Assign { name: &'a str, expr: &'a str },
    Print(&'a str),
    Expr(&'a str),
}

fn parse_statement(line: &str) -> Statement<'_> {
    if let Some((name, expr)) = split_mut_binding(line) {
        return Statement::MutBinding { name, expr };
    }
    if let Some((name, expr)) = split_assignment(line) {
        return Statement::Assign { name, expr };
    }
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
    embedded_default_handlers: HashMap<String, String>,
    runtime_bridges: RuntimeBridgeRegistry,
    /// Active inline handlers installed via `with` / `with`.
    active_inline_handler_stack: Vec<InlineHandlerValue>,
    resume_tokens: Vec<ResumeToken>,
    optimized_resume_tokens: Vec<OptimizedResumeToken>,
    runtime_error: Option<String>,
    runtime_aborted: bool,
    execution_mode: lower::EffectExecutionMode,
    stdin_buffer: Option<String>,
    stdin_cursor: usize,
}

#[derive(Clone)]
struct Continuation {
    consumed: bool,
}

#[derive(Clone)]
struct ResumeToken {
    continuation: Continuation,
    state: HandlerContinuationState,
}

#[derive(Clone)]
struct OptimizedResumeToken {
    consumed: bool,
    state: HandlerContinuationState,
}

#[derive(Clone)]
enum HandlerContinuationState {
    Pending,
    Resumed(Box<RuntimeValue>),
}

enum HandlerCompletion {
    Aborted,
    Resumed(Box<RuntimeValue>),
}

#[derive(Clone)]
struct ResolvedHandlerMethod {
    method: RuntimeHandlerMethod,
}

#[derive(Clone)]
struct InlineHandlerMethod {
    effect_name: Option<String>,
    method: RuntimeHandlerMethod,
}

#[derive(Clone)]
struct InlineHandlerValue {
    methods: Vec<InlineHandlerMethod>,
    captured_locals: RuntimeLocals,
    captured_callables: HashMap<String, IntCallable>,
}

#[derive(Clone)]
struct RuntimeHandlerMethod {
    name: String,
    params: Vec<String>,
    body: String,
    parsed_body: Option<Vec<Stmt>>,
}

struct RuntimeEvaluators<'a, 'b> {
    int: &'b IntEvaluator<'a>,
    list: &'b ListIntEvaluator<'a>,
    unit: &'b EvaluatedFunctions<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RuntimeBridgeKind {
    Function,
    EffectOperation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RuntimeBridgeTypeShape {
    UnitToString,
    StringToString,
    StringToInt,
    StringToIntCanStringParseError,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RuntimeBridgeIntrinsic {
    EmbeddedDefaultHandler {
        effect_name: &'static str,
        method_name: &'static str,
    },
    EnvFetchEnvVar,
    IntParse,
    StringLength,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RuntimeBridgeMetadata {
    module: &'static str,
    symbol: &'static str,
    kind: RuntimeBridgeKind,
    type_shape: RuntimeBridgeTypeShape,
    intrinsic: RuntimeBridgeIntrinsic,
}

const RUNTIME_BRIDGE_CATALOG: [RuntimeBridgeMetadata; 5] = [
    RuntimeBridgeMetadata {
        module: PRELUDE_MODULE_PATH,
        symbol: "read",
        kind: RuntimeBridgeKind::EffectOperation,
        type_shape: RuntimeBridgeTypeShape::UnitToString,
        intrinsic: RuntimeBridgeIntrinsic::EmbeddedDefaultHandler {
            effect_name: "Read",
            method_name: "read",
        },
    },
    RuntimeBridgeMetadata {
        module: PRELUDE_MODULE_PATH,
        symbol: "read_line",
        kind: RuntimeBridgeKind::EffectOperation,
        type_shape: RuntimeBridgeTypeShape::UnitToString,
        intrinsic: RuntimeBridgeIntrinsic::EmbeddedDefaultHandler {
            effect_name: "Read",
            method_name: "read_line",
        },
    },
    RuntimeBridgeMetadata {
        module: "goby/env",
        symbol: "fetch_env_var",
        kind: RuntimeBridgeKind::Function,
        type_shape: RuntimeBridgeTypeShape::StringToString,
        intrinsic: RuntimeBridgeIntrinsic::EnvFetchEnvVar,
    },
    RuntimeBridgeMetadata {
        module: "goby/string",
        symbol: "length",
        kind: RuntimeBridgeKind::Function,
        type_shape: RuntimeBridgeTypeShape::StringToInt,
        intrinsic: RuntimeBridgeIntrinsic::StringLength,
    },
    RuntimeBridgeMetadata {
        module: "goby/int",
        symbol: "parse",
        kind: RuntimeBridgeKind::Function,
        type_shape: RuntimeBridgeTypeShape::StringToIntCanStringParseError,
        intrinsic: RuntimeBridgeIntrinsic::IntParse,
    },
];

#[derive(Default)]
struct RuntimeBridgeRegistry {
    bare_symbols: HashMap<String, RuntimeBridgeMetadata>,
    receiver_symbols: HashMap<(String, String), RuntimeBridgeMetadata>,
}

impl RuntimeBridgeRegistry {
    fn build(module: &Module) -> Self {
        validate_runtime_bridge_catalog();
        let mut registry = Self::default();

        for import in effective_runtime_imports(module) {
            for bridge in RUNTIME_BRIDGE_CATALOG {
                if import.module_path != bridge.module {
                    continue;
                }
                match &import.kind {
                    goby_core::ImportKind::Plain => {
                        if let Some(receiver) = import.module_path.rsplit('/').next() {
                            registry.insert_receiver(receiver, bridge.symbol, bridge);
                        }
                        if import.module_path == PRELUDE_MODULE_PATH
                            && bridge.kind == RuntimeBridgeKind::EffectOperation
                        {
                            registry.insert_bare(bridge.symbol, bridge);
                        }
                    }
                    goby_core::ImportKind::Alias(alias) => {
                        registry.insert_receiver(alias, bridge.symbol, bridge);
                    }
                    goby_core::ImportKind::Selective(selected) => {
                        if selected.iter().any(|name| name == bridge.symbol) {
                            registry.insert_bare(bridge.symbol, bridge);
                        }
                    }
                }
            }
        }

        registry
    }

    fn resolve_bare(&self, symbol: &str) -> Option<RuntimeBridgeMetadata> {
        self.bare_symbols.get(symbol).copied()
    }

    fn resolve_receiver(&self, receiver: &str, symbol: &str) -> Option<RuntimeBridgeMetadata> {
        self.receiver_symbols
            .get(&(receiver.to_string(), symbol.to_string()))
            .copied()
    }

    fn insert_bare(&mut self, symbol: &str, bridge: RuntimeBridgeMetadata) {
        self.bare_symbols
            .entry(symbol.to_string())
            .or_insert(bridge);
    }

    fn insert_receiver(&mut self, receiver: &str, symbol: &str, bridge: RuntimeBridgeMetadata) {
        self.receiver_symbols
            .entry((receiver.to_string(), symbol.to_string()))
            .or_insert(bridge);
    }
}

fn validate_runtime_bridge_catalog() {
    let mut seen = HashSet::new();
    for bridge in RUNTIME_BRIDGE_CATALOG {
        debug_assert!(
            seen.insert((bridge.module, bridge.symbol)),
            "duplicate runtime bridge metadata for {}/{}",
            bridge.module,
            bridge.symbol
        );
    }
}

impl<'m> RuntimeOutputResolver<'m> {
    fn resolve(
        module: &'m Module,
        body: &str,
        parsed_stmts: Option<&[Stmt]>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        execution_mode: lower::EffectExecutionMode,
        stdin_seed: Option<String>,
    ) -> Option<String> {
        let mut resolver = Self {
            locals: RuntimeLocals::default(),
            outputs: Vec::new(),
            module,
            embedded_default_handlers: collect_embedded_default_handlers(module),
            runtime_bridges: RuntimeBridgeRegistry::build(module),
            active_inline_handler_stack: Vec::new(),
            resume_tokens: Vec::new(),
            optimized_resume_tokens: Vec::new(),
            runtime_error: None,
            runtime_aborted: false,
            execution_mode,
            stdin_buffer: stdin_seed,
            stdin_cursor: 0,
        };

        if let Some(stmts) = parsed_stmts {
            // AST-based path (preferred when parsed_body is available)
            for stmt in stmts {
                if resolver.ingest_ast_statement(stmt, evaluators).is_none() {
                    if resolver.runtime_error.is_some() || resolver.runtime_aborted {
                        break;
                    }
                    return None;
                }
            }
        } else {
            // String-based fallback path
            for statement in statements(body) {
                if resolver.ingest_statement(statement, evaluators).is_none() {
                    if resolver.runtime_error.is_some() || resolver.runtime_aborted {
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
            let mut out = resolver.outputs.concat();
            if !out.ends_with('\n') {
                out.push('\n');
            }
            out.push_str(&err_line);
            return Some(out);
        }

        if resolver.runtime_aborted {
            return if resolver.outputs.is_empty() {
                None
            } else {
                Some(resolver.outputs.concat())
            };
        }

        if resolver.outputs.is_empty() {
            None
        } else {
            Some(resolver.outputs.concat())
        }
    }

    fn ingest_ast_statement(
        &mut self,
        stmt: &Stmt,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        match stmt {
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                // Propagate None so the caller can fall back to the string path
                // rather than silently dropping the binding.
                let runtime_val = self.eval_ast_value(value, evaluators)?;
                self.locals.store(name, runtime_val);
                Some(())
            }
            Stmt::Assign { name, value } => {
                self.locals.get(name)?;
                let runtime_val = self.eval_ast_value(value, evaluators)?;
                self.locals.store(name, runtime_val);
                Some(())
            }
            Stmt::Expr(expr) => self.eval_ast_side_effect(expr, evaluators),
        }
    }

    fn eval_ast_side_effect(
        &mut self,
        expr: &Expr,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<()> {
        match expr {
            Expr::With { handler, body } => {
                let RuntimeValue::Handler(inline_handler) =
                    self.eval_ast_value(handler, evaluators)?
                else {
                    return None;
                };
                self.active_inline_handler_stack.push(inline_handler);
                let result = (|| -> Option<()> {
                    for stmt in body {
                        self.ingest_ast_statement(stmt, evaluators)?;
                    }
                    Some(())
                })();
                self.active_inline_handler_stack.pop();
                result
            }
            // print <arg>  —  handle before delegating to string path because
            // `eval_side_effect` routes through `execute_unit_call` which does not
            // know about the `print` builtin.
            Expr::Call { callee, arg } if matches!(callee.as_ref(), Expr::Var(n) if n == BUILTIN_PRINT) =>
            {
                let value = self.eval_ast_value(arg, evaluators)?;
                self.outputs.push(value.to_output_text());
                Some(())
            }
            Expr::Call { callee, arg } if matches!(callee.as_ref(), Expr::Var(n) if n == "println") =>
            {
                let value = self.eval_ast_value(arg, evaluators)?;
                let mut text = value.to_output_text();
                if !text.ends_with('\n') {
                    text.push('\n');
                }
                self.outputs.push(text);
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
                let arg_val = self.eval_ast_value(arg, evaluators)?;
                let method = self.find_handler_method_for_effect(receiver, member);
                if let Some(method) = method {
                    // depth=0: this is a top-level call; dispatch_handler_method adds 1 internally.
                    return self.dispatch_handler_method(&method, arg_val, evaluators, 0);
                }
                if self
                    .apply_embedded_default_handler(receiver, member, arg_val.clone())
                    .is_some()
                {
                    return Some(());
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
                if let Expr::Lambda { param, body } = arg.as_ref() {
                    let callable = IntCallable::AstLambda(Box::new(AstLambdaCallable {
                        parameter: param.clone(),
                        body: (*body.clone()),
                        captured_locals: self.locals.clone(),
                        captured_callables: HashMap::new(),
                    }));
                    if self
                        .execute_decl_with_callable_as_side_effect(fn_name, callable, evaluators, 0)
                        .is_some()
                    {
                        return Some(());
                    }
                }
                if let Expr::Var(arg_name) = arg.as_ref()
                    && self.declaration_expects_callable_param(fn_name)
                    && self
                        .execute_decl_with_callable_as_side_effect(
                            fn_name,
                            IntCallable::Named(arg_name.clone()),
                            evaluators,
                            0,
                        )
                        .is_some()
                {
                    return Some(());
                }
                if self.declaration_expects_callable_param(fn_name)
                    && !matches!(arg.as_ref(), Expr::Lambda { .. } | Expr::Var(_))
                {
                    self.set_runtime_error_once(ERR_CALLABLE_DISPATCH_DECL_PARAM);
                    return None;
                }
                if let Some(arg_val) = self.eval_ast_value(arg, evaluators) {
                    // Bare effect method call (e.g. `log "msg"`) — check active handlers first.
                    let bare_method = self.find_handler_method_by_name(fn_name);
                    eprintln!(
                        "eval_ast_side_effect bare call fn={} handler_found={}",
                        fn_name,
                        bare_method.is_some()
                    );
                    if let Some(method) = bare_method {
                        // depth=0: top-level call; dispatch_handler_method adds 1 internally.
                        return self.dispatch_handler_method(&method, arg_val, evaluators, 0);
                    }
                    if fn_name == "println" {
                        let mut text = arg_val.to_output_text();
                        if !text.ends_with('\n') {
                            text.push('\n');
                        }
                        self.outputs.push(text);
                        return Some(());
                    }
                    if let Some(effect_name) = self.unique_effect_name_for_operation(fn_name)
                        && self
                            .apply_embedded_default_handler(&effect_name, fn_name, arg_val.clone())
                            .is_some()
                    {
                        return Some(());
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
                        .execute_decl_as_side_effect(fn_name, arg_val.clone(), evaluators, 0)
                        .is_some()
                    {
                        return Some(());
                    }
                    if self
                        .try_apply_bare_runtime_bridge_side_effect(
                            fn_name,
                            arg_val,
                            &self.locals.clone(),
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
            Expr::Pipeline { value, callee } => {
                if let Some(v) = self.eval_ast_value(value, evaluators) {
                    // Pipeline into bare effect method call: e.g. `"msg" |> log`.
                    let bare_method = self.find_handler_method_by_name(callee);
                    if let Some(method) = bare_method {
                        // depth=0: top-level call; dispatch_handler_method adds 1 internally.
                        return self.dispatch_handler_method(&method, v, evaluators, 0);
                    }
                    if let Some(effect_name) = self.unique_effect_name_for_operation(callee)
                        && self
                            .apply_embedded_default_handler(&effect_name, callee, v.clone())
                            .is_some()
                    {
                        return Some(());
                    }
                    if self
                        .execute_unit_call_ast(
                            callee,
                            v.clone(),
                            &RuntimeLocals::default(),
                            &HashMap::new(),
                            evaluators,
                            0,
                        )
                        .is_some()
                    {
                        return Some(());
                    }
                    if self
                        .try_apply_bare_runtime_bridge_side_effect(
                            callee,
                            v,
                            &self.locals.clone(),
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
                let mut locals = self.locals.clone();
                let mut callables = HashMap::new();
                if self
                    .execute_unit_expr_ast(expr, &mut locals, &mut callables, evaluators, 0)
                    .is_some()
                {
                    return Some(());
                }
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
            Statement::MutBinding { name, expr } => self.bind_local(name, expr, evaluators),
            Statement::Assign { name, expr } => {
                self.locals.get(name)?;
                self.bind_local(name, expr, evaluators)
            }
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
        &mut self,
        expr: &str,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> Option<RuntimeValue> {
        let callables = HashMap::new();
        let locals = self.locals.clone();
        self.eval_value_with_context(expr, &locals, &callables, evaluators)
    }

    fn eval_value_with_context(
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
            && matches!(arg, "()" | "Unit")
            && let Some(value) = self.try_apply_bare_runtime_bridge_value(
                callee,
                RuntimeValue::Unit,
                locals,
                callables,
                evaluators,
                0,
            )
        {
            return Some(value);
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

    fn apply_runtime_intrinsic_ast(
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
            "__goby_string_each_grapheme" => {
                match args {
                    // Count mode:
                    // iterate graphemes with Unit state.
                    // contract: `yield : String -> state -> (Bool, state)`
                    [RuntimeValue::String(value)] => {
                        let method = self.find_handler_method_by_name("yield")?;
                        if method.method.params.len() != 2 {
                            return None;
                        }
                        let mut yielded_count: i64 = 0;
                        let mut state = RuntimeValue::Unit;
                        for grapheme in value.graphemes(true) {
                            let resumed = self.dispatch_handler_method_as_value_with_args(
                                &method,
                                &[RuntimeValue::String(grapheme.to_string()), state],
                                evaluators,
                                depth + 1,
                            )?;
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
                    // State-thread mode:
                    // iterate graphemes and thread state.
                    // contract: `yield : String -> state -> (Bool, state)`
                    [RuntimeValue::String(value), initial_state] => {
                        let method = self.find_handler_method_by_name("yield")?;
                        if method.method.params.len() != 2 {
                            return None;
                        }
                        let mut state = initial_state.clone();
                        for grapheme in value.graphemes(true) {
                            let resumed = self.dispatch_handler_method_as_value_with_args(
                                &method,
                                &[RuntimeValue::String(grapheme.to_string()), state],
                                evaluators,
                                depth + 1,
                            )?;
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
                }
            }
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
                    // Allow `[]` (currently represented as ListInt([])) to seed List String accumulation.
                    RuntimeValue::ListInt(items) if items.is_empty() => {
                        Some(RuntimeValue::ListString(vec![value.clone()]))
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn match_list_pattern_int(
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

    fn match_list_pattern_string(
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
            Expr::Var(name) => {
                if let Some(value) = locals.get(name) {
                    Some(value)
                } else if name == "Unit" {
                    Some(RuntimeValue::Unit)
                } else {
                    None
                }
            }
            Expr::Handler { clauses } => Some(RuntimeValue::Handler(
                self.inline_handler_from_clauses(clauses, locals, callables),
            )),
            Expr::BinOp { op, left, right } => {
                let lv = self.eval_expr_ast(left, locals, callables, evaluators, depth + 1)?;
                let rv = self.eval_expr_ast(right, locals, callables, evaluators, depth + 1)?;
                match (lv, rv) {
                    (RuntimeValue::Int(l), RuntimeValue::Int(r)) => match op {
                        goby_core::BinOpKind::Add => Some(RuntimeValue::Int(l.checked_add(r)?)),
                        goby_core::BinOpKind::Mul => Some(RuntimeValue::Int(l.checked_mul(r)?)),
                        goby_core::BinOpKind::Eq => Some(RuntimeValue::Bool(l == r)),
                    },
                    (RuntimeValue::String(l), RuntimeValue::String(r))
                        if matches!(op, goby_core::BinOpKind::Eq) =>
                    {
                        Some(RuntimeValue::Bool(l == r))
                    }
                    _ => None,
                }
            }
            Expr::ListLit { elements, spread } => {
                let mut int_items = Vec::with_capacity(elements.len());
                let mut string_items = Vec::with_capacity(elements.len());
                let mut list_kind: Option<&'static str> = None;
                for item in elements {
                    match self.eval_expr_ast(item, locals, callables, evaluators, depth + 1)? {
                        RuntimeValue::Int(n) => {
                            if list_kind == Some("string") {
                                return None;
                            }
                            list_kind = Some("int");
                            int_items.push(n);
                        }
                        RuntimeValue::String(text) => {
                            if list_kind == Some("int") {
                                return None;
                            }
                            list_kind = Some("string");
                            string_items.push(text);
                        }
                        _ => return None,
                    }
                }
                if let Some(tail) = spread {
                    match self.eval_expr_ast(tail, locals, callables, evaluators, depth + 1)? {
                        RuntimeValue::ListInt(mut values) => {
                            if list_kind == Some("string") {
                                if values.is_empty() {
                                    return Some(RuntimeValue::ListString(string_items));
                                }
                                return None;
                            }
                            int_items.append(&mut values);
                            return Some(RuntimeValue::ListInt(int_items));
                        }
                        RuntimeValue::ListString(mut values) => {
                            if list_kind == Some("int") {
                                if values.is_empty() {
                                    return Some(RuntimeValue::ListInt(int_items));
                                }
                                return None;
                            }
                            string_items.append(&mut values);
                            return Some(RuntimeValue::ListString(string_items));
                        }
                        _ => return None,
                    }
                }
                match list_kind {
                    Some("string") => Some(RuntimeValue::ListString(string_items)),
                    _ => Some(RuntimeValue::ListInt(int_items)),
                }
            }
            Expr::TupleLit(items) => {
                if items.is_empty() {
                    return Some(RuntimeValue::Unit);
                }
                let values: Option<Vec<RuntimeValue>> = items
                    .iter()
                    .map(|item| self.eval_expr_ast(item, locals, callables, evaluators, depth + 1))
                    .collect();
                Some(RuntimeValue::Tuple(values?))
            }
            Expr::Call { callee, arg } => {
                if let Some((fn_name, args)) = flatten_named_call(expr) {
                    let arg_values: Option<Vec<RuntimeValue>> = args
                        .iter()
                        .map(|arg_expr| {
                            self.eval_expr_ast(arg_expr, locals, callables, evaluators, depth + 1)
                        })
                        .collect();
                    let arg_values = arg_values?;
                    if fn_name.starts_with("__goby_") {
                        return self.apply_runtime_intrinsic_ast(
                            fn_name,
                            &arg_values,
                            evaluators,
                            depth + 1,
                        );
                    }
                    if args.len() > 1
                        && let Some(method) = self.find_handler_method_by_name(fn_name)
                    {
                        return self.dispatch_handler_method_as_value_with_args(
                            &method,
                            &arg_values,
                            evaluators,
                            depth + 1,
                        );
                    }
                }

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
                    if fn_name == "__goby_env_fetch_env_var" {
                        let av =
                            self.eval_expr_ast(arg, locals, callables, evaluators, depth + 1)?;
                        return self.apply_runtime_intrinsic_ast(
                            "__goby_env_fetch_env_var",
                            &[av],
                            evaluators,
                            depth + 1,
                        );
                    }
                    if fn_name == "__goby_string_length" {
                        let av =
                            self.eval_expr_ast(arg, locals, callables, evaluators, depth + 1)?;
                        return self.apply_runtime_intrinsic_ast(
                            "__goby_string_length",
                            &[av],
                            evaluators,
                            depth + 1,
                        );
                    }
                    if fn_name == "__goby_string_each_grapheme" {
                        let av =
                            self.eval_expr_ast(arg, locals, callables, evaluators, depth + 1)?;
                        return self.apply_runtime_intrinsic_ast(
                            "__goby_string_each_grapheme",
                            &[av],
                            evaluators,
                            depth + 1,
                        );
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
                    if fn_name == "println" {
                        let mut text = arg_val.to_output_text();
                        if !text.ends_with('\n') {
                            text.push('\n');
                        }
                        self.outputs.push(text);
                        return Some(RuntimeValue::Unit);
                    }
                    if let Some(effect_name) = self.unique_effect_name_for_operation(fn_name)
                        && let Some(value) = self.apply_embedded_default_handler(
                            &effect_name,
                            fn_name,
                            arg_val.clone(),
                        )
                    {
                        return Some(value);
                    }
                    if let Some(value) = self.try_apply_bare_runtime_bridge_value(
                        fn_name,
                        arg_val.clone(),
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    ) {
                        return Some(value);
                    }
                    // Int function path
                    if let RuntimeValue::Int(arg_int) = arg_val {
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
                                    if let Some(RuntimeValue::Int(value)) = self.eval_expr_ast(
                                        &body,
                                        &lambda_locals,
                                        &captured_callables,
                                        evaluators,
                                        depth + 1,
                                    ) {
                                        return Some(RuntimeValue::Int(value));
                                    }
                                }
                                other => {
                                    return evaluators
                                        .int
                                        .eval_callable(&other, arg_int, callables)
                                        .map(RuntimeValue::Int);
                                }
                            }
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
                    let arg_val =
                        self.eval_expr_ast(arg, locals, callables, evaluators, depth + 1)?;
                    if let Some(value) = self.try_apply_receiver_runtime_bridge_value(
                        receiver,
                        member,
                        arg_val.clone(),
                        evaluators,
                        depth + 1,
                    ) {
                        return Some(value);
                    }
                    let method = self
                        .find_handler_method_for_effect(receiver, member)
                        .or_else(|| self.find_handler_method_by_name(member));
                    if let Some(method) = method {
                        return self.dispatch_handler_method_as_value(
                            &method,
                            arg_val,
                            evaluators,
                            depth + 1,
                        );
                    }
                    if let Some(value) =
                        self.apply_embedded_default_handler(receiver, member, arg_val)
                    {
                        return Some(value);
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
                if fields.is_empty()
                    && let Some(value) = self.try_apply_bare_runtime_bridge_value(
                        constructor,
                        RuntimeValue::Unit,
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    )
                {
                    return Some(value);
                }
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
            // If `receiver` is a local tuple, numeric member access returns the indexed item.
            // If `receiver` is absent from locals (e.g. a union type name), return
            // the member name as a string (e.g. `UserStatus.Activated` → `"Activated"`).
            // If `receiver` is present but not a Record, fall back to None.
            Expr::Qualified { receiver, member } => {
                match locals.get(receiver) {
                    Some(RuntimeValue::Record { fields, .. }) => fields.get(member).cloned(),
                    Some(RuntimeValue::Tuple(items)) => {
                        let index = member.parse::<usize>().ok()?;
                        items.get(index).cloned()
                    }
                    None => {
                        if member.chars().all(|c| c.is_ascii_digit()) {
                            return None;
                        }
                        // Treat as a type/module-qualified constructor name.
                        Some(RuntimeValue::String(member.clone()))
                    }
                    Some(_) => None,
                }
            }
            Expr::Block(stmts) => {
                let mut block_locals = locals.clone();
                let mut block_callables = callables.clone();
                let mut last_value: Option<RuntimeValue> = None;
                for stmt in stmts {
                    match stmt {
                        Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                            let v = self.eval_expr_ast(
                                value,
                                &block_locals,
                                &block_callables,
                                evaluators,
                                depth + 1,
                            )?;
                            block_locals.store(name, v);
                            last_value = None;
                        }
                        Stmt::Assign { name, value } => {
                            block_locals.get(name)?;
                            let v = self.eval_expr_ast(
                                value,
                                &block_locals,
                                &block_callables,
                                evaluators,
                                depth + 1,
                            )?;
                            block_locals.store(name, v);
                            last_value = None;
                        }
                        Stmt::Expr(expr) => {
                            if let Some(v) = self.eval_expr_ast(
                                expr,
                                &block_locals,
                                &block_callables,
                                evaluators,
                                depth + 1,
                            ) {
                                last_value = Some(v);
                            } else {
                                self.execute_unit_expr_ast(
                                    expr,
                                    &mut block_locals,
                                    &mut block_callables,
                                    evaluators,
                                    depth + 1,
                                )?;
                                last_value = Some(RuntimeValue::Unit);
                            }
                        }
                    }
                }
                last_value
            }
            Expr::Case { scrutinee, arms } => {
                let scrutinee_val =
                    self.eval_expr_ast(scrutinee, locals, callables, evaluators, depth + 1)?;
                for arm in arms {
                    let mut arm_locals = locals.clone();
                    let matched = match (&arm.pattern, &scrutinee_val) {
                        (CasePattern::Wildcard, _) => true,
                        (CasePattern::IntLit(n), RuntimeValue::Int(v)) => n == v,
                        (CasePattern::StringLit(s), RuntimeValue::String(v)) => s == v,
                        (CasePattern::BoolLit(b), RuntimeValue::Bool(v)) => b == v,
                        (CasePattern::EmptyList, RuntimeValue::ListInt(values)) => {
                            values.is_empty()
                        }
                        (CasePattern::EmptyList, RuntimeValue::ListString(values)) => {
                            values.is_empty()
                        }
                        (
                            CasePattern::ListPattern { items, tail },
                            RuntimeValue::ListInt(values),
                        ) => self.match_list_pattern_int(
                            items,
                            tail.as_ref(),
                            values,
                            &mut arm_locals,
                        ),
                        (
                            CasePattern::ListPattern { items, tail },
                            RuntimeValue::ListString(values),
                        ) => self.match_list_pattern_string(
                            items,
                            tail.as_ref(),
                            values,
                            &mut arm_locals,
                        ),
                        _ => false,
                    };
                    if matched {
                        if let Some(v) = self.eval_expr_ast(
                            &arm.body,
                            &arm_locals,
                            callables,
                            evaluators,
                            depth + 1,
                        ) {
                            return Some(v);
                        }
                        let mut arm_callables = callables.clone();
                        let mut arm_locals_for_unit = arm_locals;
                        self.execute_unit_expr_ast(
                            &arm.body,
                            &mut arm_locals_for_unit,
                            &mut arm_callables,
                            evaluators,
                            depth + 1,
                        )?;
                        return Some(RuntimeValue::Unit);
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
                    RuntimeValue::Bool(true) => match self.eval_expr_ast(
                        then_expr,
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    ) {
                        Some(v) => Some(v),
                        None => {
                            let mut branch_locals = locals.clone();
                            let mut branch_callables = callables.clone();
                            self.execute_unit_expr_ast(
                                then_expr,
                                &mut branch_locals,
                                &mut branch_callables,
                                evaluators,
                                depth + 1,
                            )?;
                            Some(RuntimeValue::Unit)
                        }
                    },
                    RuntimeValue::Bool(false) => match self.eval_expr_ast(
                        else_expr,
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    ) {
                        Some(v) => Some(v),
                        None => {
                            let mut branch_locals = locals.clone();
                            let mut branch_callables = callables.clone();
                            self.execute_unit_expr_ast(
                                else_expr,
                                &mut branch_locals,
                                &mut branch_callables,
                                evaluators,
                                depth + 1,
                            )?;
                            Some(RuntimeValue::Unit)
                        }
                    },
                    _ => None,
                }
            }
            Expr::Resume { value } => {
                let resumed =
                    self.eval_expr_ast(value, locals, callables, evaluators, depth + 1)?;
                self.resume_through_active_continuation_bridge(resumed)
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
            Expr::MethodCall {
                receiver,
                method,
                args,
            } if args.len() == 1 => {
                let arg_val =
                    self.eval_expr_ast(&args[0], locals, callables, evaluators, depth + 1)?;
                if let Some(value) = self.try_apply_receiver_runtime_bridge_value(
                    receiver,
                    method,
                    arg_val,
                    evaluators,
                    depth + 1,
                ) {
                    return Some(value);
                }
                None
            }
            // Lambda as top-level value — not needed in main, return None to fall back.
            Expr::Lambda { .. } | Expr::MethodCall { .. } | Expr::With { .. } => None,
        }
    }

    fn execute_unit_expr_ast(
        &mut self,
        expr: &Expr,
        locals: &mut RuntimeLocals,
        callables: &mut HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        if let Expr::With { handler, body } = expr {
            let RuntimeValue::Handler(inline_handler) =
                self.eval_expr_ast(handler, locals, callables, evaluators, depth)?
            else {
                return None;
            };
            self.active_inline_handler_stack.push(inline_handler);
            let result = (|| -> Option<()> {
                for stmt in body {
                    self.execute_unit_ast_stmt(stmt, locals, callables, evaluators, depth + 1)?;
                }
                Some(())
            })();
            self.active_inline_handler_stack.pop();
            return result;
        }

        if self
            .try_execute_imported_list_each_call(expr, locals, callables, evaluators, depth + 1)
            .is_some()
        {
            return Some(());
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
            return Some(());
        }

        // print <arg>
        if let Expr::Call { callee, arg } = expr
            && matches!(callee.as_ref(), Expr::Var(n) if n == BUILTIN_PRINT)
        {
            let value = self.eval_expr_ast(arg, locals, callables, evaluators, depth)?;
            self.outputs.push(value.to_output_text());
            return Some(());
        }
        if let Expr::Call { callee, arg } = expr
            && matches!(callee.as_ref(), Expr::Var(n) if n == "println")
        {
            let value = self.eval_expr_ast(arg, locals, callables, evaluators, depth)?;
            let mut text = value.to_output_text();
            if !text.ends_with('\n') {
                text.push('\n');
            }
            self.outputs.push(text);
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
            let arg_val = self.eval_expr_ast(arg, locals, callables, evaluators, depth)?;
            let method = self.find_handler_method_for_effect(receiver, member);
            if let Some(method) = method {
                return self.dispatch_handler_method(&method, arg_val, evaluators, depth + 1);
            }
            if self
                .apply_embedded_default_handler(receiver, member, arg_val.clone())
                .is_some()
            {
                return Some(());
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
                    return Some(());
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
                return Some(());
            }
            if self.declaration_expects_callable_param(fn_name)
                && !matches!(arg.as_ref(), Expr::Lambda { .. } | Expr::Var(_))
            {
                self.set_runtime_error_once(ERR_CALLABLE_DISPATCH_DECL_PARAM);
                return None;
            }
            // Evaluate arg once.
            let arg_val = self.eval_expr_ast(arg, locals, callables, evaluators, depth)?;
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
                return Some(());
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
                self.outputs.push(text);
                return Some(());
            }
            if let Some(effect_name) = self.unique_effect_name_for_operation(fn_name)
                && self
                    .apply_embedded_default_handler(&effect_name, fn_name, arg_val.clone())
                    .is_some()
            {
                return Some(());
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
                .execute_decl_as_side_effect(fn_name, arg_val.clone(), evaluators, depth + 1)
                .is_some()
            {
                return Some(());
            }
            if self
                .try_apply_bare_runtime_bridge_side_effect(
                    fn_name,
                    arg_val,
                    locals,
                    callables,
                    evaluators,
                    depth + 1,
                )
                .is_some()
            {
                return Some(());
            }
            let repr = expr.to_str_repr()?;
            return self.execute_unit_call(&repr, locals, callables, evaluators);
        }

        if let Expr::Pipeline { value, callee } = expr {
            if let Some(v) = self.eval_expr_ast(value, locals, callables, evaluators, depth) {
                if let Some(effect_name) = self.unique_effect_name_for_operation(callee)
                    && self
                        .apply_embedded_default_handler(&effect_name, callee, v.clone())
                        .is_some()
                {
                    return Some(());
                }
                if self
                    .execute_unit_call_ast(callee, v.clone(), locals, callables, evaluators, depth)
                    .is_some()
                {
                    return Some(());
                }
                if self
                    .try_apply_bare_runtime_bridge_side_effect(
                        callee,
                        v,
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    )
                    .is_some()
                {
                    return Some(());
                }
            }
            let repr = expr.to_str_repr()?;
            return self.execute_unit_call(&repr, locals, callables, evaluators);
        }

        // Bare literal/var expression: evaluate and discard.
        if let Expr::Var(_) | Expr::IntLit(_) | Expr::StringLit(_) | Expr::BoolLit(_) = expr {
            return Some(());
        }

        if let Expr::Case { scrutinee, arms } = expr {
            let scrutinee_val =
                self.eval_expr_ast(scrutinee, locals, callables, evaluators, depth)?;
            for arm in arms {
                let mut arm_locals = locals.clone();
                let matched = match (&arm.pattern, &scrutinee_val) {
                    (CasePattern::Wildcard, _) => true,
                    (CasePattern::IntLit(n), RuntimeValue::Int(v)) => n == v,
                    (CasePattern::StringLit(s), RuntimeValue::String(v)) => s == v,
                    (CasePattern::BoolLit(b), RuntimeValue::Bool(v)) => b == v,
                    (CasePattern::EmptyList, RuntimeValue::ListInt(values)) => values.is_empty(),
                    (CasePattern::EmptyList, RuntimeValue::ListString(values)) => values.is_empty(),
                    (CasePattern::ListPattern { items, tail }, RuntimeValue::ListInt(values)) => {
                        self.match_list_pattern_int(items, tail.as_ref(), values, &mut arm_locals)
                    }
                    (
                        CasePattern::ListPattern { items, tail },
                        RuntimeValue::ListString(values),
                    ) => self.match_list_pattern_string(
                        items,
                        tail.as_ref(),
                        values,
                        &mut arm_locals,
                    ),
                    _ => false,
                };
                if matched {
                    if self
                        .eval_expr_ast(&arm.body, &arm_locals, callables, evaluators, depth + 1)
                        .is_some()
                    {
                        return Some(());
                    }
                    let mut arm_callables = callables.clone();
                    return self.execute_unit_expr_ast(
                        &arm.body,
                        &mut arm_locals,
                        &mut arm_callables,
                        evaluators,
                        depth + 1,
                    );
                }
            }
            return Some(());
        }

        if let Expr::If {
            condition,
            then_expr,
            else_expr,
        } = expr
        {
            let cond_val = self.eval_expr_ast(condition, locals, callables, evaluators, depth)?;
            match cond_val {
                RuntimeValue::Bool(true) => {
                    if self
                        .eval_expr_ast(then_expr, locals, callables, evaluators, depth + 1)
                        .is_some()
                    {
                        return Some(());
                    }
                    return self.execute_unit_expr_ast(
                        then_expr,
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    );
                }
                RuntimeValue::Bool(false) => {
                    if self
                        .eval_expr_ast(else_expr, locals, callables, evaluators, depth + 1)
                        .is_some()
                    {
                        return Some(());
                    }
                    return self.execute_unit_expr_ast(
                        else_expr,
                        locals,
                        callables,
                        evaluators,
                        depth + 1,
                    );
                }
                _ => return None,
            }
        }

        if let Expr::Block(stmts) = expr {
            let mut block_locals = locals.clone();
            let mut block_callables = callables.clone();
            for stmt in stmts {
                self.execute_unit_ast_stmt(
                    stmt,
                    &mut block_locals,
                    &mut block_callables,
                    evaluators,
                    depth + 1,
                )?;
            }
            return Some(());
        }

        // Fallback: evaluating to a value is fine in unit position (discarded).
        if self
            .eval_expr_ast(expr, locals, callables, evaluators, depth + 1)
            .is_some()
        {
            return Some(());
        }

        // Preserve existing call/pipeline string fallback behavior for uncovered forms.
        if let Expr::Call { .. } | Expr::Pipeline { .. } = expr {
            let repr = expr.to_str_repr()?;
            return self.execute_unit_call(&repr, locals, callables, evaluators);
        }

        None
    }

    fn try_execute_imported_list_each_call(
        &mut self,
        expr: &Expr,
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        let Expr::Call {
            callee,
            arg: callback,
        } = expr
        else {
            return None;
        };
        let Expr::Call {
            callee: head,
            arg: list_arg,
        } = callee.as_ref()
        else {
            return None;
        };

        let is_list_each = match head.as_ref() {
            Expr::Qualified { receiver, member } => {
                member == "each" && self.resolves_module_receiver(receiver, "goby/list")
            }
            Expr::Var(name) => {
                name == "each" && self.has_selective_import_symbol("goby/list", "each")
            }
            _ => false,
        };
        if !is_list_each {
            return None;
        }

        let RuntimeValue::ListInt(values) =
            self.eval_expr_ast(list_arg, locals, callables, evaluators, depth + 1)?
        else {
            return None;
        };
        let callback_callable = match callback.as_ref() {
            Expr::Lambda { param, body } => IntCallable::AstLambda(Box::new(AstLambdaCallable {
                parameter: param.clone(),
                body: (*body.clone()),
                captured_locals: locals.clone(),
                captured_callables: callables.clone(),
            })),
            Expr::Var(name) => self.resolve_callable_argument(name, callables),
            _ => {
                self.set_runtime_error_once(ERR_CALLABLE_DISPATCH_LIST_EACH_CALLBACK);
                return None;
            }
        };

        for n in values {
            self.dispatch_callable_side_effect(
                &callback_callable,
                RuntimeValue::Int(n),
                locals,
                callables,
                evaluators,
                depth + 1,
            )?;
        }
        Some(())
    }

    fn execute_decl_call_chain_as_side_effect(
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
        let (params, callable_param_mask, stmts) = {
            let decl = self
                .module
                .declarations
                .iter()
                .find(|d| d.name == fn_name)?;
            if decl.params.len() != args.len() {
                return None;
            }
            let callable_param_mask = self.declaration_callable_param_mask(decl);
            let stmts = decl.parsed_body.as_ref()?.clone();
            (decl.params.clone(), callable_param_mask, stmts)
        };
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
            let arg_val =
                self.eval_expr_ast(arg, caller_locals, caller_callables, evaluators, depth + 1)?;
            fn_locals.store(param, arg_val);
        }

        for stmt in &stmts {
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
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                let v = self.eval_expr_ast(value, locals, callables, evaluators, depth)?;
                locals.store(name, v);
                Some(())
            }
            Stmt::Assign { name, value } => {
                locals.get(name)?;
                let v = self.eval_expr_ast(value, locals, callables, evaluators, depth)?;
                locals.store(name, v);
                Some(())
            }
            Stmt::Expr(expr) => {
                self.execute_unit_expr_ast(expr, locals, callables, evaluators, depth)
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
                    Statement::Binding { name, expr } | Statement::MutBinding { name, expr } => {
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
        for inline in self.active_inline_handler_stack.iter().rev() {
            if let Some(method) = inline.methods.iter().find(|m| {
                m.method.name == method_name && m.effect_name.as_deref() == Some(effect_name)
            }) {
                return Some(ResolvedHandlerMethod {
                    method: method.method.clone(),
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

    fn is_record_constructor_name(&self, name: &str) -> bool {
        self.module.type_declarations.iter().any(|ty_decl| {
            matches!(
                ty_decl,
                goby_core::TypeDeclaration::Record { constructor, .. } if constructor == name
            )
        })
    }

    fn has_declaration_name(&self, name: &str) -> bool {
        self.module
            .declarations
            .iter()
            .any(|decl| decl.name == name)
    }

    fn can_apply_bare_runtime_bridge(
        &self,
        symbol: &str,
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
    ) -> bool {
        !self.has_declaration_name(symbol)
            && locals.get(symbol).is_none()
            && !callables.contains_key(symbol)
            && !evaluators.int.functions.contains_key(symbol)
            && !evaluators.list.functions.contains_key(symbol)
            && !evaluators.unit.contains_key(symbol)
            && !self.is_record_constructor_name(symbol)
    }

    fn apply_runtime_bridge_value(
        &mut self,
        bridge: RuntimeBridgeMetadata,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        match bridge.intrinsic {
            RuntimeBridgeIntrinsic::EmbeddedDefaultHandler {
                effect_name,
                method_name,
            } => self.apply_embedded_default_handler(effect_name, method_name, arg_val),
            RuntimeBridgeIntrinsic::EnvFetchEnvVar => {
                let RuntimeValue::String(var_name) = arg_val else {
                    return None;
                };
                Some(RuntimeValue::String(
                    std::env::var(&var_name).unwrap_or_default(),
                ))
            }
            RuntimeBridgeIntrinsic::IntParse => {
                let RuntimeValue::String(input) = arg_val else {
                    return None;
                };
                self.eval_int_parse_runtime(input, evaluators, depth + 1)
            }
            RuntimeBridgeIntrinsic::StringLength => {
                let RuntimeValue::String(value) = arg_val else {
                    return None;
                };
                let len = i64::try_from(value.chars().count()).ok()?;
                Some(RuntimeValue::Int(len))
            }
        }
    }

    fn try_apply_bare_runtime_bridge_value(
        &mut self,
        symbol: &str,
        arg_val: RuntimeValue,
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        if !self.can_apply_bare_runtime_bridge(symbol, locals, callables, evaluators) {
            return None;
        }
        let bridge = self.runtime_bridges.resolve_bare(symbol)?;
        self.apply_runtime_bridge_value(bridge, arg_val, evaluators, depth + 1)
    }

    fn try_apply_bare_runtime_bridge_side_effect(
        &mut self,
        symbol: &str,
        arg_val: RuntimeValue,
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        self.try_apply_bare_runtime_bridge_value(
            symbol,
            arg_val,
            locals,
            callables,
            evaluators,
            depth + 1,
        )?;
        Some(())
    }

    fn try_apply_receiver_runtime_bridge_value(
        &mut self,
        receiver: &str,
        symbol: &str,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        let bridge = self.runtime_bridges.resolve_receiver(receiver, symbol)?;
        self.apply_runtime_bridge_value(bridge, arg_val, evaluators, depth + 1)
    }

    fn resolves_module_receiver(&self, receiver: &str, module_path: &str) -> bool {
        self.module.imports.iter().any(|import| {
            if import.module_path != module_path {
                return false;
            }
            match &import.kind {
                goby_core::ImportKind::Plain => import
                    .module_path
                    .rsplit('/')
                    .next()
                    .is_some_and(|qualifier| qualifier == receiver),
                goby_core::ImportKind::Alias(alias) => alias == receiver,
                goby_core::ImportKind::Selective(_) => false,
            }
        })
    }

    fn has_selective_import_symbol(&self, module_path: &str, symbol: &str) -> bool {
        self.module.imports.iter().any(|import| {
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

    fn eval_int_parse_runtime(
        &mut self,
        input: String,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        match parse_goby_int_text(&input) {
            Some(value) => Some(RuntimeValue::Int(value)),
            None => {
                if let Some(method) =
                    self.find_handler_method_for_effect("StringParseError", "invalid_integer")
                {
                    return self.dispatch_handler_method_as_value(
                        &method,
                        RuntimeValue::String(input),
                        evaluators,
                        depth + 1,
                    );
                }
                if !self.operation_has_conflicting_effect("invalid_integer", "StringParseError")
                    && let Some(method) = self.find_handler_method_by_name("invalid_integer")
                {
                    return self.dispatch_handler_method_as_value(
                        &method,
                        RuntimeValue::String(input),
                        evaluators,
                        depth + 1,
                    );
                }
                self.set_runtime_error_once(
                    "unhandled effect operation `invalid_integer` from goby/int.parse",
                );
                None
            }
        }
    }

    fn operation_has_conflicting_effect(&self, op_name: &str, expected_effect: &str) -> bool {
        let mut effect_names: HashSet<String> = self
            .module
            .effect_declarations
            .iter()
            .filter(|effect| effect.members.iter().any(|member| member.name == op_name))
            .map(|effect| effect.name.clone())
            .collect();

        let resolver = StdlibResolver::new(resolve_runtime_stdlib_root());
        for import in &self.module.imports {
            let Ok(path) = resolver.module_file_path(&import.module_path) else {
                continue;
            };
            let Ok(source) = std::fs::read_to_string(path) else {
                continue;
            };
            let Ok(parsed) = goby_core::parse_module(&source) else {
                continue;
            };
            for effect in parsed.effect_declarations {
                if let goby_core::ImportKind::Selective(selected) = &import.kind
                    && !selected.iter().any(|name| name == &effect.name)
                {
                    continue;
                }
                if effect.members.iter().any(|member| member.name == op_name) {
                    effect_names.insert(effect.name);
                }
            }
        }

        effect_names.retain(|name| name != expected_effect);
        !effect_names.is_empty()
    }

    /// Find a handler method by bare name (e.g. `log`) from nearest to outermost.
    fn find_handler_method_by_name(&self, method_name: &str) -> Option<ResolvedHandlerMethod> {
        for inline in self.active_inline_handler_stack.iter().rev() {
            if let Some(method) = inline.methods.iter().find(|m| m.method.name == method_name) {
                return Some(ResolvedHandlerMethod {
                    method: method.method.clone(),
                });
            }
        }
        None
    }

    fn apply_embedded_default_handler(
        &mut self,
        effect_name: &str,
        method_name: &str,
        arg_val: RuntimeValue,
    ) -> Option<RuntimeValue> {
        let handler_name = self.embedded_default_handlers.get(effect_name)?;
        match (handler_name.as_str(), method_name) {
            ("__goby_embeded_effect_stdout_handler", "print") => {
                self.outputs.push(arg_val.to_output_text());
                Some(RuntimeValue::Unit)
            }
            ("__goby_embeded_effect_stdout_handler", "println") => {
                let mut text = arg_val.to_output_text();
                if !text.ends_with('\n') {
                    text.push('\n');
                }
                self.outputs.push(text);
                Some(RuntimeValue::Unit)
            }
            ("__goby_embeded_effect_stdin_handler", "read")
                if matches!(arg_val, RuntimeValue::Unit) =>
            {
                Some(RuntimeValue::String(self.read_stdin_remaining("read")?))
            }
            ("__goby_embeded_effect_stdin_handler", "read_line")
                if matches!(arg_val, RuntimeValue::Unit) =>
            {
                Some(RuntimeValue::String(self.read_stdin_line("read_line")?))
            }
            _ => None,
        }
    }

    fn ensure_stdin_loaded(&mut self, op_name: &str) -> Option<()> {
        if self.stdin_buffer.is_some() {
            return Some(());
        }
        let mut bytes = Vec::new();
        match std::io::stdin().read_to_end(&mut bytes) {
            Ok(_) => {
                self.stdin_buffer = Some(String::from_utf8_lossy(&bytes).into_owned());
                Some(())
            }
            Err(err) => {
                self.set_runtime_error_once(format!("Read.{op_name} failed to read stdin: {err}"));
                None
            }
        }
    }

    fn read_stdin_remaining(&mut self, op_name: &str) -> Option<String> {
        self.ensure_stdin_loaded(op_name)?;
        let content = self.stdin_buffer.as_ref()?;
        if self.stdin_cursor >= content.len() {
            return Some(String::new());
        }
        let tail = content[self.stdin_cursor..].to_string();
        self.stdin_cursor = content.len();
        Some(tail)
    }

    fn read_stdin_line(&mut self, op_name: &str) -> Option<String> {
        self.ensure_stdin_loaded(op_name)?;
        let content = self.stdin_buffer.as_ref()?;
        if self.stdin_cursor >= content.len() {
            return Some(String::new());
        }
        let bytes = content.as_bytes();
        let start = self.stdin_cursor;
        let mut idx = start;
        while idx < bytes.len() {
            match bytes[idx] {
                b'\n' => {
                    let line = content[start..idx].to_string();
                    self.stdin_cursor = idx + 1;
                    return Some(line);
                }
                b'\r' => {
                    let line = content[start..idx].to_string();
                    if idx + 1 < bytes.len() && bytes[idx + 1] == b'\n' {
                        self.stdin_cursor = idx + 2;
                    } else {
                        self.stdin_cursor = idx + 1;
                    }
                    return Some(line);
                }
                _ => {
                    idx += 1;
                }
            }
        }
        let line = content[start..].to_string();
        self.stdin_cursor = content.len();
        Some(line)
    }

    fn inline_handler_from_clauses(
        &self,
        clauses: &[HandlerClause],
        locals: &RuntimeLocals,
        callables: &HashMap<String, IntCallable>,
    ) -> InlineHandlerValue {
        let methods = clauses
            .iter()
            .map(|clause| InlineHandlerMethod {
                effect_name: self.unique_effect_name_for_operation(&clause.name),
                method: RuntimeHandlerMethod {
                    name: clause.name.clone(),
                    params: clause.params.clone(),
                    body: clause.body.clone(),
                    parsed_body: clause.parsed_body.clone(),
                },
            })
            .collect();
        InlineHandlerValue {
            methods,
            captured_locals: locals.clone(),
            captured_callables: callables.clone(),
        }
    }

    fn unique_effect_name_for_operation(&self, op_name: &str) -> Option<String> {
        let mut matches = self
            .module
            .effect_declarations
            .iter()
            .filter(|effect| effect.members.iter().any(|member| member.name == op_name))
            .map(|effect| effect.name.clone())
            .collect::<Vec<_>>();
        matches.sort();
        matches.dedup();
        if matches.len() == 1 {
            matches.into_iter().next()
        } else {
            None
        }
    }

    fn set_runtime_error_once(&mut self, message: impl Into<String>) {
        if self.runtime_error.is_none() {
            self.runtime_error = Some(message.into());
        }
    }

    fn set_runtime_abort_once(&mut self) {
        if self.runtime_error.is_none() {
            self.runtime_aborted = true;
        }
    }

    fn has_abort_without_error(&self) -> bool {
        self.runtime_aborted && self.runtime_error.is_none()
    }

    fn push_resume_token_for_handler(&mut self) -> usize {
        self.resume_tokens.push(ResumeToken {
            continuation: Continuation { consumed: false },
            state: HandlerContinuationState::Pending,
        });
        self.resume_tokens.len() - 1
    }

    fn take_resume_token_result(&mut self, token_idx: usize) -> Option<HandlerCompletion> {
        if token_idx + 1 != self.resume_tokens.len() {
            self.set_runtime_error_once(ERR_RESUME_STACK_MISMATCH);
            return None;
        }
        let token = self.resume_tokens.pop()?;
        Some(match token.state {
            HandlerContinuationState::Pending => HandlerCompletion::Aborted,
            HandlerContinuationState::Resumed(value) => HandlerCompletion::Resumed(value),
        })
    }

    fn current_resume_token_mut(&mut self) -> Option<&mut ResumeToken> {
        self.resume_tokens.last_mut()
    }

    fn push_optimized_resume_token_for_handler(&mut self) -> usize {
        self.optimized_resume_tokens.push(OptimizedResumeToken {
            consumed: false,
            state: HandlerContinuationState::Pending,
        });
        self.optimized_resume_tokens.len() - 1
    }

    fn take_optimized_resume_token_result(
        &mut self,
        token_idx: usize,
    ) -> Option<HandlerCompletion> {
        if token_idx + 1 != self.optimized_resume_tokens.len() {
            self.set_runtime_error_once(ERR_RESUME_STACK_MISMATCH);
            return None;
        }
        let token = self.optimized_resume_tokens.pop()?;
        Some(match token.state {
            HandlerContinuationState::Pending => HandlerCompletion::Aborted,
            HandlerContinuationState::Resumed(value) => HandlerCompletion::Resumed(value),
        })
    }

    fn current_optimized_resume_token_mut(&mut self) -> Option<&mut OptimizedResumeToken> {
        self.optimized_resume_tokens.last_mut()
    }

    fn resume_token_has_value(&self, token_idx: usize) -> bool {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => self
                .resume_tokens
                .get(token_idx)
                .is_some_and(|token| matches!(token.state, HandlerContinuationState::Resumed(_))),
            lower::EffectExecutionMode::TypedContinuationOptimized => self
                .optimized_resume_tokens
                .get(token_idx)
                .is_some_and(|token| matches!(token.state, HandlerContinuationState::Resumed(_))),
        }
    }

    fn begin_handler_continuation_bridge(&mut self) -> usize {
        match self.execution_mode {
            lower::EffectExecutionMode::PortableFallback => self.push_resume_token_for_handler(),
            lower::EffectExecutionMode::TypedContinuationOptimized => {
                self.push_optimized_resume_token_for_handler()
            }
        }
    }

    fn finish_handler_continuation_bridge(
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
        let Some(token) = self.current_resume_token_mut() else {
            self.set_runtime_error_once(ERR_RESUME_MISSING);
            return None;
        };
        token.continuation.consumed = true;
        token.state = HandlerContinuationState::Resumed(Box::new(resumed.clone()));
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
        let Some(token) = self.current_optimized_resume_token_mut() else {
            self.set_runtime_error_once(ERR_RESUME_MISSING);
            return None;
        };
        token.consumed = true;
        token.state = HandlerContinuationState::Resumed(Box::new(resumed.clone()));
        Some(resumed)
    }

    fn dispatch_handler_method_core(
        &mut self,
        method: &ResolvedHandlerMethod,
        args: &[RuntimeValue],
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
        produce_value: bool,
    ) -> Option<HandlerCompletion> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let stmts = method.method.parsed_body.as_deref()?;
        if args.len() != method.method.params.len() {
            return None;
        }
        let token_idx = self.begin_handler_continuation_bridge();
        let run_result = (|| -> Option<()> {
            let mut handler_locals = self
                .active_inline_handler_stack
                .iter()
                .rev()
                .find_map(|inline| {
                    inline
                        .methods
                        .iter()
                        .find(|m| {
                            m.method.name == method.method.name
                                && m.method.params == method.method.params
                                && m.method.body == method.method.body
                        })
                        .map(|_| inline.captured_locals.clone())
                })
                .unwrap_or_default();
            for (param, arg) in method.method.params.iter().zip(args.iter()) {
                handler_locals.store(param, arg.clone());
            }
            let mut handler_callables = self
                .active_inline_handler_stack
                .iter()
                .rev()
                .find_map(|inline| {
                    inline
                        .methods
                        .iter()
                        .find(|m| {
                            m.method.name == method.method.name
                                && m.method.params == method.method.params
                                && m.method.body == method.method.body
                        })
                        .map(|_| inline.captured_callables.clone())
                })
                .unwrap_or_default();
            for stmt in stmts {
                if produce_value {
                    match stmt {
                        Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => {
                            let v = self.eval_expr_ast(
                                value,
                                &handler_locals,
                                &handler_callables,
                                evaluators,
                                depth + 1,
                            )?;
                            handler_locals.store(name, v);
                        }
                        Stmt::Assign { name, value } => {
                            handler_locals.get(name)?;
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
                            let value = self.eval_expr_ast(
                                expr,
                                &handler_locals,
                                &handler_callables,
                                evaluators,
                                depth + 1,
                            );
                            if value.is_none() {
                                // Side-effect stmt — try as unit side effect.
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
            Some(())
        })();
        let completion = self.finish_handler_continuation_bridge(token_idx)?;
        if run_result.is_none() {
            if self.has_abort_without_error() {
                return Some(HandlerCompletion::Aborted);
            }
            return None;
        }
        Some(completion)
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
        match self.dispatch_handler_method_core(method, &[arg_val], evaluators, depth, true)? {
            HandlerCompletion::Resumed(value) => Some(*value),
            HandlerCompletion::Aborted => {
                self.set_runtime_abort_once();
                None
            }
        }
    }

    fn dispatch_handler_method_as_value_with_args(
        &mut self,
        method: &ResolvedHandlerMethod,
        args: &[RuntimeValue],
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        match self.dispatch_handler_method_core(method, args, evaluators, depth, true)? {
            HandlerCompletion::Resumed(value) => Some(*value),
            HandlerCompletion::Aborted => {
                self.set_runtime_abort_once();
                None
            }
        }
    }

    /// Execute a handler method body with the given argument.
    fn dispatch_handler_method(
        &mut self,
        method: &ResolvedHandlerMethod,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        match self.dispatch_handler_method_core(method, &[arg_val], evaluators, depth, false)? {
            HandlerCompletion::Resumed(_) => Some(()),
            HandlerCompletion::Aborted => {
                self.set_runtime_abort_once();
                None
            }
        }
    }

    /// Execute any declaration (including Int-returning ones) as a side-effect call.
    /// Used when calling functions like `plus_ten_with_log` from a `with` block.
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

    fn execute_decl_with_callable_as_side_effect(
        &mut self,
        fn_name: &str,
        callable: IntCallable,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
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
        let mut fn_callables = HashMap::new();
        let param = param_name?;
        fn_callables.insert(param, callable);
        for stmt in &stmts {
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

    fn dispatch_callable_side_effect(
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
                self.execute_unit_expr_ast(
                    &callable.body,
                    &mut lambda_locals,
                    &mut lambda_callables,
                    evaluators,
                    depth + 1,
                )
                .or_else(|| {
                    self.eval_expr_ast(
                        &callable.body,
                        &lambda_locals,
                        &lambda_callables,
                        evaluators,
                        depth + 1,
                    )
                    .map(|_| ())
                })
            }
        }
    }

    fn resolve_callable_argument(
        &self,
        arg_name: &str,
        callables: &HashMap<String, IntCallable>,
    ) -> IntCallable {
        callables
            .get(arg_name)
            .cloned()
            .unwrap_or_else(|| IntCallable::Named(arg_name.to_string()))
    }

    fn declaration_expects_callable_param(&self, fn_name: &str) -> bool {
        let Some(decl) = self.module.declarations.iter().find(|d| d.name == fn_name) else {
            return false;
        };
        self.declaration_callable_param_mask(decl)
            .first()
            .copied()
            .unwrap_or(false)
    }

    fn declaration_callable_param_mask(&self, decl: &goby_core::Declaration) -> Vec<bool> {
        let Some(annotation) = decl.type_annotation.as_deref() else {
            return vec![false; decl.params.len()];
        };
        let Some(function_type) = parse_function_type(annotation) else {
            return vec![false; decl.params.len()];
        };
        function_type
            .arguments
            .iter()
            .map(|arg| arg.contains("->"))
            .collect()
    }
}

fn collect_embedded_default_handlers(module: &Module) -> HashMap<String, String> {
    let mut defaults: HashMap<String, String> = module
        .embed_declarations
        .iter()
        .map(|embed| (embed.effect_name.clone(), embed.handler_name.clone()))
        .collect();
    let resolver = StdlibResolver::new(resolve_runtime_stdlib_root());
    for import in effective_runtime_imports(module) {
        let Ok(resolved) = resolver.resolve_module(&import.module_path) else {
            continue;
        };
        for embed in resolved.embedded_defaults {
            defaults
                .entry(embed.effect_name)
                .or_insert(embed.handler_name);
        }
    }
    defaults
}

fn effective_runtime_imports(module: &Module) -> Vec<goby_core::ImportDecl> {
    let mut imports = module.imports.clone();
    let has_prelude = imports
        .iter()
        .any(|import| import.module_path == PRELUDE_MODULE_PATH);
    if has_prelude {
        return imports;
    }
    let resolver = StdlibResolver::new(resolve_runtime_stdlib_root());
    let prelude_available = resolver
        .module_file_path(PRELUDE_MODULE_PATH)
        .ok()
        .is_some_and(|path| path.exists());
    if prelude_available {
        imports.push(goby_core::ImportDecl {
            module_path: PRELUDE_MODULE_PATH.to_string(),
            kind: goby_core::ImportKind::Plain,
        });
    }
    imports
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

fn resolve_runtime_stdlib_root() -> PathBuf {
    std::env::var_os("GOBY_STDLIB_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../..")
                .join("stdlib")
        })
}

fn parse_goby_int_text(input: &str) -> Option<i64> {
    // Runtime shortcut for stdlib `goby/int.parse`.
    // Keep this behavior aligned with `stdlib/goby/int.gb`:
    // optional leading `-`, then one or more ASCII digits, with overflow rejected.
    if input.is_empty() {
        return None;
    }
    let (negative, digits) = if let Some(rest) = input.strip_prefix('-') {
        (true, rest)
    } else {
        (false, input)
    };
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }

    let mut acc: i64 = 0;
    for b in digits.bytes() {
        let digit = (b - b'0') as i64;
        acc = acc.checked_mul(10)?;
        acc = if negative {
            acc.checked_sub(digit)?
        } else {
            acc.checked_add(digit)?
        };
    }
    Some(acc)
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
            u @ RuntimeValue::Unit => {
                self.record_values.insert(name.to_string(), u);
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
            t @ RuntimeValue::Tuple(_) => {
                self.record_values.insert(name.to_string(), t);
            }
            h @ RuntimeValue::Handler(_) => {
                self.record_values.insert(name.to_string(), h);
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
    Unit,
    Bool(bool),
    Tuple(Vec<RuntimeValue>),
    ListInt(Vec<i64>),
    ListString(Vec<String>),
    Handler(InlineHandlerValue),
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
            Self::Unit => "Unit".to_string(),
            Self::Bool(b) => if *b { "True" } else { "False" }.to_string(),
            Self::Tuple(items) => {
                let parts: Vec<String> = items.iter().map(RuntimeValue::to_output_text).collect();
                format!("({})", parts.join(", "))
            }
            Self::ListInt(values) => format_list_int(values),
            Self::ListString(values) => {
                let parts: Vec<String> = values.iter().map(|s| format!("\"{}\"", s)).collect();
                format!("[{}]", parts.join(", "))
            }
            Self::Handler(_) => "<handler>".to_string(),
            // Print the constructor name as a placeholder; field access
            // resolves the actual field value before printing in practice.
            Self::Record { constructor, .. } => constructor.clone(),
        }
    }

    fn to_expression_text(&self) -> String {
        match self {
            Self::String(text) => format!("\"{}\"", text),
            Self::Int(value) => value.to_string(),
            Self::Unit => "Unit".to_string(),
            Self::Bool(b) => if *b { "True" } else { "False" }.to_string(),
            Self::Tuple(items) => {
                let parts: Vec<String> =
                    items.iter().map(RuntimeValue::to_expression_text).collect();
                format!("({})", parts.join(", "))
            }
            Self::ListInt(values) => format_list_int(values),
            Self::ListString(values) => {
                let parts: Vec<String> = values.iter().map(|s| format!("\"{}\"", s)).collect();
                format!("[{}]", parts.join(", "))
            }
            Self::Handler(_) => "<handler>".to_string(),
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

    None
}

struct ListIntEvaluator<'a> {
    functions: &'a EvaluatedFunctions<'a>,
    allow_imported_list_map: bool,
    depth: usize,
}

impl<'a> ListIntEvaluator<'a> {
    fn root(functions: &'a EvaluatedFunctions<'a>, allow_imported_list_map: bool) -> Self {
        Self {
            functions,
            allow_imported_list_map,
            depth: 0,
        }
    }

    fn descend(&self) -> Option<Self> {
        if self.depth >= MAX_EVAL_DEPTH {
            return None;
        }

        Some(Self {
            functions: self.functions,
            allow_imported_list_map: self.allow_imported_list_map,
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

        if self.allow_imported_list_map
            && let Some((list_expr, lambda_expr)) = parse_map_call(expr)
        {
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
        assert!(
            main_parsed_body(&module).is_some(),
            "main parsed_body should exist for case arm block source"
        );
        eprintln!("parsed main body: {:#?}", main_parsed_body(&module));
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
      resume Unit
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
      resume Unit
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
      resume Unit
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
                "runtime error: unsupported callable dispatch [E-CALLABLE-DISPATCH]: goby/list.each callback must be a lambda or function name"
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
    fn runtime_resolves_goby_env_fetch_via_selective_import_bridge() {
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
    fn runtime_resolves_goby_string_length_via_module_receiver_bridge() {
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
    fn runtime_resolves_goby_string_length_via_selective_import_bridge() {
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
    fn runtime_resolves_goby_int_parse_via_selective_import_bridge() {
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
        assert_eq!(output, "13done");
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
      resume Unit
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
      resume Unit
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
      resume Unit
    println msg ->
      resume Unit
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
    fn runtime_resolves_goby_int_parse_via_module_alias() {
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
    fn parse_goby_int_text_matches_locked_boundary_cases() {
        assert_eq!(parse_goby_int_text("0"), Some(0));
        assert_eq!(parse_goby_int_text("-0"), Some(0));
        assert_eq!(parse_goby_int_text("9223372036854775807"), Some(i64::MAX));
        assert_eq!(parse_goby_int_text("-9223372036854775808"), Some(i64::MIN));
        assert_eq!(parse_goby_int_text("9223372036854775808"), None);
        assert_eq!(parse_goby_int_text("-9223372036854775809"), None);
        assert_eq!(parse_goby_int_text(""), None);
        assert_eq!(parse_goby_int_text("-"), None);
        assert_eq!(parse_goby_int_text("+1"), None);
        assert_eq!(parse_goby_int_text("1_000"), None);
        assert_eq!(parse_goby_int_text("12x"), None);
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
      resume Unit
  in
    with
      log msg ->
        print "inner"
        resume Unit
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
      resume Unit
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
      resume Unit
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
    fn no_resume_in_value_position_takes_abortive_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      print "handled"
  in
    print (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("handled"),
            "handler side effects before abort should be preserved, while caller continuation does not run"
        );
    }

    #[test]
    fn no_resume_in_unit_position_aborts_following_statements() {
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
    print "after"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("handled:hello"),
            "unit-position handled operation should abort before later statements execute"
        );
    }

    #[test]
    fn nested_abortive_handler_stops_outer_continuations() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Outer
  op: String -> Unit

effect Inner
  boom: String -> Unit

main : Unit -> Unit
main =
  with
    op msg ->
      with
        boom inner ->
          print "inner:${inner}"
      in
        boom msg
      print "outer-after"
      resume Unit
  in
    op "x"
    print "main-after"
"#;
        let module = parse_module(source).expect("parse should work");
        let output =
            resolve_main_runtime_output(&module, main_body(&module), main_parsed_body(&module));
        assert_eq!(
            output.as_deref(),
            Some("inner:x"),
            "abortive inner handler should stop outer handler and main continuations at the handled boundary"
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
      resume Unit
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
      resume Unit
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
      resume Unit
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
      resume Unit
  in
    with
      log msg ->
        print "inner"
        resume Unit
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
      resume Unit
  in
    with
      log msg ->
        print "inline"
        resume Unit
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
      resume Unit
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
    fn typed_mode_matches_fallback_for_no_resume_abortive_path() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Iter
  next: Int -> Int

main : Unit -> Unit
main =
  with
    next n ->
      print "handled"
  in
    print (next 0)
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "no-resume abortive path");
        assert_eq!(typed.stdout.as_deref(), Some("handled"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_no_resume_unit_position_abort() {
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
    print "after"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "no-resume unit-position abort path");
        assert_eq!(typed.stdout.as_deref(), Some("handled:hello"));
        assert_eq!(typed.runtime_error_kind, None);
    }

    #[test]
    fn typed_mode_matches_fallback_for_nested_abortive_handlers() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        let source = r#"
effect Outer
  op: String -> Unit

effect Inner
  boom: String -> Unit

main : Unit -> Unit
main =
  with
    op msg ->
      with
        boom inner ->
          print "inner:${inner}"
      in
        boom msg
      print "outer-after"
      resume Unit
  in
    op "x"
    print "main-after"
"#;
        let module = parse_module(source).expect("parse should work");
        let typed = assert_mode_parity(&module, "nested abortive handler path");
        assert_eq!(typed.stdout.as_deref(), Some("inner:x"));
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
      resume Unit
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
      resume Unit
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
      resume Unit
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
}
