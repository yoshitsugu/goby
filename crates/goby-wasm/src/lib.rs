use std::collections::{BTreeMap, HashMap, HashSet};

use goby_core::{
    CasePattern, Expr, HandlerMethod, Module, Stmt,
    str_util::parse_string_concat_call, types::parse_function_type,
};

const IOVEC_OFFSET: u32 = 0;
const NWRITTEN_OFFSET: u32 = 8;
const TEXT_OFFSET: u32 = 16;
const ERR_MISSING_MAIN: &str = "Wasm codegen requires a `main` declaration";
const ERR_UNSUPPORTED_PRINT_UNKNOWN: &str =
    "print argument is unsupported for Wasm codegen (only String is supported)";
const BUILTIN_PRINT: &str = "print";
const MAX_EVAL_DEPTH: usize = 32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodegenError {
    pub message: String,
}

pub fn compile_module(module: &Module) -> Result<Vec<u8>, CodegenError> {
    let Some(main) = module.declarations.iter().find(|d| d.name == "main") else {
        return Err(CodegenError {
            message: ERR_MISSING_MAIN.to_string(),
        });
    };

    if let Some(text) = resolve_main_runtime_output(module, &main.body, main.parsed_body.as_deref())
    {
        return compile_print_module(&text);
    }

    if let Some(message) = find_unsupported_form(module, &main.body) {
        return Err(CodegenError {
            message: message.to_string(),
        });
    }

    // Minimal MVP codegen target: emit a valid module that exports an empty `main`.
    // This is a temporary bridge until expression-level lowering is implemented.
    Ok(minimal_main_module())
}

fn find_unsupported_form(module: &Module, main_body: &str) -> Option<&'static str> {
    let mut analyzer = UnsupportedFormAnalyzer::new(module);
    analyzer.find_unsupported_print(main_body)
}

struct UnsupportedFormAnalyzer<'a> {
    locals: HashMap<String, KnownType>,
    result_types: HashMap<&'a str, KnownType>,
}

impl<'a> UnsupportedFormAnalyzer<'a> {
    fn new(module: &'a Module) -> Self {
        Self {
            locals: HashMap::new(),
            result_types: declaration_result_types(module),
        }
    }

    fn find_unsupported_print(&mut self, main_body: &str) -> Option<&'static str> {
        for statement in statements(main_body) {
            match statement {
                Statement::Binding { name, expr } => self.bind(name, expr),
                Statement::Print(expr) => {
                    if let Some(message) = unsupported_print_message(self.infer_expr_type(expr)) {
                        return Some(message);
                    }
                }
                Statement::Expr(_) => {}
            }
        }

        None
    }

    fn bind(&mut self, name: &str, expr: &str) {
        let ty = self.infer_expr_type(expr);
        self.locals.insert(name.to_string(), ty);
    }

    fn infer_expr_type(&self, expr: &str) -> KnownType {
        infer_expr_type(expr, &self.locals, &self.result_types)
    }
}

fn declaration_result_types(module: &Module) -> HashMap<&str, KnownType> {
    let mut map = HashMap::new();
    for decl in &module.declarations {
        let Some(annotation) = decl.type_annotation.as_deref() else {
            continue;
        };
        let Some(function_type) = parse_function_type(annotation) else {
            continue;
        };
        let known_type = KnownType::from_annotation_result(&function_type.result);
        map.insert(decl.name.as_str(), known_type);
    }
    map
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum KnownType {
    String,
    Int,
    List,
    Unknown,
}

impl KnownType {
    fn from_annotation_result(result: &str) -> Self {
        match result {
            "Int" => Self::Int,
            "String" => Self::String,
            _ if result.starts_with("List") => Self::List,
            _ => Self::Unknown,
        }
    }
}

fn infer_expr_type(
    expr: &str,
    locals: &HashMap<String, KnownType>,
    result_types: &HashMap<&str, KnownType>,
) -> KnownType {
    let expr = expr.trim();
    if expr.is_empty() {
        return KnownType::Unknown;
    }

    if is_string_literal(expr) || expr.starts_with("string.concat(") {
        return KnownType::String;
    }

    if is_int_literal(expr) || is_int_expression(expr) {
        return KnownType::Int;
    }

    if is_list_literal(expr) {
        return KnownType::List;
    }

    if let Some(local_ty) = locals.get(expr) {
        return *local_ty;
    }

    if let Some(result_type) = result_types.get(call_head(expr)) {
        return *result_type;
    }

    KnownType::Unknown
}

fn unsupported_print_message(known_type: KnownType) -> Option<&'static str> {
    match known_type {
        KnownType::String | KnownType::Int | KnownType::List => None,
        KnownType::Unknown => Some(ERR_UNSUPPORTED_PRINT_UNKNOWN),
    }
}

fn call_head(expr: &str) -> &str {
    expr.split(|c: char| c.is_whitespace() || c == '(')
        .next()
        .unwrap_or(expr)
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

fn resolve_main_runtime_output(
    module: &Module,
    body: &str,
    parsed_stmts: Option<&[Stmt]>,
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
    RuntimeOutputResolver::resolve(module, body, parsed_stmts, &evaluators)
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
    /// Maps effect name (e.g. "Log") to handler_declarations index.
    /// BTreeMap gives deterministic iteration order (alphabetical by effect name).
    active_handlers: BTreeMap<String, usize>,
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
    ) -> Option<String> {
        let mut resolver = Self {
            locals: RuntimeLocals::default(),
            outputs: Vec::new(),
            module,
            active_handlers: BTreeMap::new(),
        };

        if let Some(stmts) = parsed_stmts {
            // AST-based path (preferred when parsed_body is available)
            for stmt in stmts {
                resolver.ingest_ast_statement(stmt, evaluators)?;
            }
        } else {
            // String-based fallback path
            for statement in statements(body) {
                resolver.ingest_statement(statement, evaluators)?;
            }
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
                // Install handlers: map effect name → handler_declarations index.
                let previous = self.active_handlers.clone();
                for handler_name in handlers {
                    if let Some(idx) = self
                        .module
                        .handler_declarations
                        .iter()
                        .position(|h| &h.name == handler_name)
                    {
                        let effect = self.module.handler_declarations[idx].effect.clone();
                        self.active_handlers.insert(effect, idx);
                    }
                }
                // Execute body with active handlers.
                for stmt in body {
                    self.ingest_ast_statement(stmt, evaluators)?;
                }
                // Restore previous handler context.
                self.active_handlers = previous;
                Some(())
            }
        }
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
            // Other expression statements: try AST unit-call path.
            Expr::Call { callee, arg }
                if matches!(callee.as_ref(), Expr::Var(_)) =>
            {
                let Expr::Var(fn_name) = callee.as_ref() else {
                    unreachable!()
                };
                if let Some(arg_val) = self.eval_ast_value(arg, evaluators) {
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
                        .execute_decl_as_side_effect(fn_name, arg_val, evaluators, 1)
                        .is_some()
                    {
                        return Some(());
                    }
                }
                let repr = expr.to_str_repr()?;
                self.eval_side_effect(&repr, evaluators)
            }
            Expr::Pipeline { value, callee } => {
                if let Some(v) = self.eval_ast_value(value, evaluators)
                    && self
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
                let av =
                    self.eval_expr_ast(&args[0], locals, callables, evaluators, depth + 1)?;
                let bv =
                    self.eval_expr_ast(&args[1], locals, callables, evaluators, depth + 1)?;
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

                    let arg_val =
                        self.eval_expr_ast(arg, locals, callables, evaluators, depth + 1)?;
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
                            &method, arg_val, evaluators, depth + 1,
                        );
                    }
                }
                // callee is not a plain Var (e.g. a curried call or lambda
                // application) — not yet supported by the native evaluator.
                None  // NOTE: qualified callee dispatch is handled above
            }
            Expr::Pipeline { value, callee } => {
                let v = self.eval_expr_ast(value, locals, callables, evaluators, depth + 1)?;
                self.apply_pipeline(callee, v, locals, callables, evaluators, depth + 1)
            }
            // Record construction: evaluate each field, build RuntimeValue::Record.
            Expr::RecordConstruct { constructor, fields } => {
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
                    Some(RuntimeValue::Record { fields, .. }) => {
                        fields.get(member).cloned()
                    }
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
            Expr::MethodCall { receiver, method, args }
                if method == "split" && receiver == "string" && args.len() == 2 =>
            {
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
                        return self.dispatch_handler_method(&method, arg_val, evaluators, depth + 1);
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
                        return self.dispatch_handler_method(&method, arg_val, evaluators, depth + 1);
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
                            .execute_unit_call_ast(
                                callee,
                                v,
                                locals,
                                callables,
                                evaluators,
                                depth,
                            )
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
                let previous = self.active_handlers.clone();
                for handler_name in handlers {
                    if let Some(idx) = self
                        .module
                        .handler_declarations
                        .iter()
                        .position(|h| &h.name == handler_name)
                    {
                        let effect = self.module.handler_declarations[idx].effect.clone();
                        self.active_handlers.insert(effect, idx);
                    }
                }
                for stmt in body {
                    self.execute_unit_ast_stmt(stmt, locals, callables, evaluators, depth + 1)?;
                }
                self.active_handlers = previous;
                Some(())
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
    /// Returns a cloned `HandlerMethod` if found.
    fn find_handler_method_for_effect(
        &self,
        effect_name: &str,
        method_name: &str,
    ) -> Option<HandlerMethod> {
        let idx = *self.active_handlers.get(effect_name)?;
        let handler = &self.module.handler_declarations[idx];
        handler
            .methods
            .iter()
            .find(|m| m.name == method_name)
            .cloned()
    }

    /// Find a handler method by bare name (e.g. `log`) across all active handlers.
    /// Iteration order is deterministic (BTreeMap: alphabetical by effect name).
    fn find_handler_method_by_name(&self, method_name: &str) -> Option<HandlerMethod> {
        for &idx in self.active_handlers.values() {
            let handler = &self.module.handler_declarations[idx];
            if let Some(m) = handler.methods.iter().find(|m| m.name == method_name) {
                return Some(m.clone());
            }
        }
        None
    }

    /// Execute a handler method and return the last evaluated value.
    /// Used when the handler method produces a value (e.g. `env.from_env`).
    fn dispatch_handler_method_as_value(
        &mut self,
        method: &HandlerMethod,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<RuntimeValue> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let stmts = method.parsed_body.as_deref()?;
        let mut handler_locals = RuntimeLocals::default();
        if let Some(param) = method.params.first() {
            handler_locals.store(param, arg_val);
        }
        let mut handler_callables = HashMap::new();
        let mut last_val: Option<RuntimeValue> = None;
        for stmt in stmts {
            match stmt {
                Stmt::Binding { name, value } => {
                    let v =
                        self.eval_expr_ast(value, &handler_locals, &handler_callables, evaluators, depth + 1)?;
                    handler_locals.store(name, v);
                }
                Stmt::Expr(expr) => {
                    last_val =
                        self.eval_expr_ast(expr, &handler_locals, &handler_callables, evaluators, depth + 1);
                    if last_val.is_none() {
                        // Side-effect stmt — try as unit side effect
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
                    let previous = self.active_handlers.clone();
                    for handler_name in handlers {
                        if let Some(idx) = self
                            .module
                            .handler_declarations
                            .iter()
                            .position(|h| &h.name == handler_name)
                        {
                            let effect = self.module.handler_declarations[idx].effect.clone();
                            self.active_handlers.insert(effect, idx);
                        }
                    }
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
                    self.active_handlers = previous;
                }
            }
        }
        last_val
    }

    /// Execute a handler method body with the given argument.
    fn dispatch_handler_method(
        &mut self,
        method: &HandlerMethod,
        arg_val: RuntimeValue,
        evaluators: &RuntimeEvaluators<'_, '_>,
        depth: usize,
    ) -> Option<()> {
        if depth >= MAX_EVAL_DEPTH {
            return None;
        }
        let stmts = method.parsed_body.as_deref()?;
        let mut handler_locals = RuntimeLocals::default();
        if let Some(param) = method.params.first() {
            handler_locals.store(param, arg_val);
        }
        let mut handler_callables = HashMap::new();
        for stmt in stmts {
            self.execute_unit_ast_stmt(
                stmt,
                &mut handler_locals,
                &mut handler_callables,
                evaluators,
                depth + 1,
            )?;
        }
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
            let decl = self.module.declarations.iter().find(|d| d.name == fn_name)?;
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

fn is_int_expression(expr: &str) -> bool {
    expr.contains(" + ") || expr.contains(" * ")
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

fn minimal_main_module() -> Vec<u8> {
    const MINIMAL_MAIN_MODULE: &[u8] = &[
        0x00, 0x61, 0x73, 0x6d, // magic
        0x01, 0x00, 0x00, 0x00, // version
        // type section: 1 function type () -> ()
        0x01, 0x04, 0x01, 0x60, 0x00, 0x00, // function section: 1 function with type 0
        0x03, 0x02, 0x01, 0x00, // export section: export func 0 as "main"
        0x07, 0x08, 0x01, 0x04, 0x6d, 0x61, 0x69, 0x6e, 0x00, 0x00,
        // code section: 1 body with empty instructions + end
        0x0a, 0x04, 0x01, 0x02, 0x00, 0x0b,
    ];
    MINIMAL_MAIN_MODULE.to_vec()
}

fn compile_print_module(text: &str) -> Result<Vec<u8>, CodegenError> {
    let text_bytes = text.as_bytes();

    let text_len = u32::try_from(text_bytes.len()).map_err(|_| CodegenError {
        message: "print literal is too large to encode".to_string(),
    })?;

    let iovec_bytes = encode_iovec(TEXT_OFFSET, text_len);

    let wat_source = format!(
        r#"(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory (export "memory") 1)
  (data (i32.const {text_offset}) "{text_data}")
  (data (i32.const {iovec_offset}) "{iovec_data}")
  (func (export "main")
    (drop
      (call $fd_write
        (i32.const 1)
        (i32.const {iovec_offset})
        (i32.const 1)
        (i32.const {nwritten_offset})))))"#,
        text_offset = TEXT_OFFSET,
        iovec_offset = IOVEC_OFFSET,
        nwritten_offset = NWRITTEN_OFFSET,
        text_data = wat_bytes(text_bytes),
        iovec_data = wat_bytes(&iovec_bytes),
    );

    wat::parse_str(&wat_source).map_err(|err| CodegenError {
        message: format!("failed to build wasm module: {}", err),
    })
}

fn wat_bytes(bytes: &[u8]) -> String {
    let mut out = String::new();
    for b in bytes {
        out.push('\\');
        out.push_str(&format!("{:02x}", b));
    }
    out
}

fn encode_iovec(base: u32, length: u32) -> [u8; 8] {
    let mut bytes = [0u8; 8];
    bytes[..4].copy_from_slice(&base.to_le_bytes());
    bytes[4..].copy_from_slice(&length.to_le_bytes());
    bytes
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
        assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d]);
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

    #[test]
    fn emits_valid_wasm_header_for_main_module() {
        let module = parse_module("main : Unit -> Unit\nmain = 0\n").expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn emits_valid_wasm_for_print_literal() {
        let module = parse_module("main : Unit -> Unit\nmain = print \"Hello Goby!\"\n")
            .expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
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
    fn encodes_iovec_in_little_endian() {
        let bytes = encode_iovec(16, 5);
        assert_eq!(bytes, [16, 0, 0, 0, 5, 0, 0, 0]);
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
    fn emits_valid_wasm_for_function_example_int_calls() {
        let source = read_example("function.gb");
        let module = parse_module(&source).expect("parse should work");
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
    fn locks_runtime_output_for_effect_gb() {
        use goby_core::parse_module;
        let _guard = ENV_MUTEX.lock().unwrap();
        // SAFETY: serialized by ENV_MUTEX; no concurrent env access while lock is held.
        unsafe { std::env::set_var("GOBY_PATH", "hello") };
        let source = read_example("effect.gb");
        let module = parse_module(&source).expect("effect.gb should parse");
        let output = resolve_main_runtime_output(
            &module,
            main_body(&module),
            main_parsed_body(&module),
        )
        .expect("runtime output should resolve");
        // Clean up before asserting so env is restored even if the assertion panics.
        unsafe { std::env::remove_var("GOBY_PATH") };
        assert_eq!(output, "13\nhello");
    }
}
