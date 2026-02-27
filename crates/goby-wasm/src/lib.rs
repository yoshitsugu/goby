use std::collections::{HashMap, HashSet};

use goby_core::{Module, resolve_print_text, types::parse_function_type};

const IOVEC_OFFSET: u32 = 0;
const NWRITTEN_OFFSET: u32 = 8;
const TEXT_OFFSET: u32 = 16;
const ERR_MISSING_MAIN: &str = "Wasm codegen requires a `main` declaration";
const ERR_UNSUPPORTED_PIPELINE: &str =
    "pipeline operator (`|>`) is not yet supported in Wasm codegen";
const ERR_UNSUPPORTED_PRINT_INT: &str = "print Int is not yet supported in Wasm codegen";
const ERR_UNSUPPORTED_PRINT_LIST: &str = "print List is not yet supported in Wasm codegen";
const ERR_UNSUPPORTED_PRINT_UNKNOWN: &str =
    "print argument is unsupported for Wasm codegen (only String is supported)";
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

    if let Some(text) = resolve_print_text(&main.body).ok().flatten() {
        return compile_print_module(&text);
    }

    if let Some(value) = resolve_print_int(module, &main.body) {
        return compile_print_module(&value.to_string());
    }

    if resolve_print_list_int(module, &main.body).is_some() {
        return Err(CodegenError {
            message: ERR_UNSUPPORTED_PRINT_LIST.to_string(),
        });
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
    if let Some(message) = analyzer.find_unsupported_print(main_body) {
        return Some(message);
    }

    if contains_pipeline(main_body) {
        return Some(ERR_UNSUPPORTED_PIPELINE);
    }

    None
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
                Statement::Other => {}
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
        KnownType::String => None,
        KnownType::Int => Some(ERR_UNSUPPORTED_PRINT_INT),
        KnownType::List => Some(ERR_UNSUPPORTED_PRINT_LIST),
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

fn resolve_print_int(module: &Module, body: &str) -> Option<i64> {
    let functions = collect_functions_with_result(module, "Int");
    IntPrintResolver::resolve(body, &functions)
}

fn resolve_print_list_int(module: &Module, body: &str) -> Option<Vec<i64>> {
    let list_functions = collect_functions_with_result(module, "List Int");
    ListIntPrintResolver::resolve(body, &list_functions)
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
            },
        );
    }

    functions
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

    fn eval_expr(&self, expr: &str, locals: &HashMap<String, i64>) -> Option<i64> {
        let expr = expr.trim();
        if expr.is_empty() {
            return None;
        }

        if let Some(value) = self.eval_binary_expr(expr, locals) {
            return Some(value);
        }

        if is_int_literal(expr) {
            return expr.parse().ok();
        }

        if let Some(value) = locals.get(expr) {
            return Some(*value);
        }

        if let Some((callee, arg_expr)) = parse_call(expr) {
            let function = self.functions.get(callee)?;
            let nested = self.descend()?;
            let arg = nested.eval_expr(arg_expr, locals)?;
            return nested.eval_function(function, Some(arg));
        }

        let function = self.functions.get(expr)?;
        self.descend()?.eval_function(function, None)
    }

    fn eval_binary_expr(&self, expr: &str, locals: &HashMap<String, i64>) -> Option<i64> {
        if let Some((left, right)) = expr.split_once(" + ") {
            return self.eval_binary_operands(left, right, locals, i64::checked_add);
        }

        if let Some((left, right)) = expr.split_once(" * ") {
            return self.eval_binary_operands(left, right, locals, i64::checked_mul);
        }

        None
    }

    fn eval_binary_operands(
        &self,
        left: &str,
        right: &str,
        locals: &HashMap<String, i64>,
        op: fn(i64, i64) -> Option<i64>,
    ) -> Option<i64> {
        let nested = self.descend()?;
        let left_value = nested.eval_expr(left, locals)?;
        let right_value = nested.eval_expr(right, locals)?;
        op(left_value, right_value)
    }

    fn eval_function(&self, function: &EvaluatedFunction<'a>, arg: Option<i64>) -> Option<i64> {
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

enum Statement<'a> {
    Binding { name: &'a str, expr: &'a str },
    Print(&'a str),
    Other,
}

fn parse_statement(line: &str) -> Statement<'_> {
    if let Some((name, expr)) = split_binding(line) {
        return Statement::Binding { name, expr };
    }

    if let Some(expr) = parse_print_call(line) {
        return Statement::Print(expr);
    }

    Statement::Other
}

fn statements(body: &str) -> impl Iterator<Item = Statement<'_>> {
    code_lines(body).map(parse_statement)
}

struct IntPrintResolver {
    locals: HashMap<String, i64>,
    resolved_print: Option<i64>,
}

struct ListIntPrintResolver {
    locals: HashMap<String, Vec<i64>>,
    resolved_print: Option<Vec<i64>>,
}

impl ListIntPrintResolver {
    fn resolve(body: &str, functions: &EvaluatedFunctions<'_>) -> Option<Vec<i64>> {
        let mut resolver = Self {
            locals: HashMap::new(),
            resolved_print: None,
        };
        let evaluator = ListIntEvaluator::root(functions);

        for statement in statements(body) {
            resolver.ingest_statement(statement, &evaluator)?;
        }

        resolver.resolved_print
    }

    fn ingest_statement(
        &mut self,
        statement: Statement<'_>,
        evaluator: &ListIntEvaluator<'_>,
    ) -> Option<()> {
        match statement {
            Statement::Binding { name, expr } => {
                self.bind_local(name, expr, evaluator);
                Some(())
            }
            Statement::Print(expr) => self.capture_print(expr, evaluator),
            Statement::Other => Some(()),
        }
    }

    fn bind_local(&mut self, name: &str, expr: &str, evaluator: &ListIntEvaluator<'_>) {
        let value = evaluator.eval_expr(expr, &self.locals);
        assign_local(name, value, &mut self.locals);
    }

    fn capture_print(&mut self, expr: &str, evaluator: &ListIntEvaluator<'_>) -> Option<()> {
        let value = evaluator.eval_expr(expr, &self.locals)?;
        capture_single_print(&mut self.resolved_print, value)
    }
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

struct MapLambda<'a> {
    parameter: &'a str,
    body: &'a str,
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

fn parse_map_lambda(expr: &str) -> Option<MapLambda<'_>> {
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
        return Some(MapLambda { parameter, body });
    }

    if expr.contains('_') {
        return Some(MapLambda {
            parameter: "_",
            body: expr,
        });
    }

    None
}

fn apply_map_lambda(values: &[i64], lambda: &MapLambda<'_>) -> Option<Vec<i64>> {
    let empty_functions = HashMap::new();
    let evaluator = IntEvaluator::root(&empty_functions);
    let mut out = Vec::with_capacity(values.len());
    for value in values {
        let mut locals = HashMap::new();
        locals.insert(lambda.parameter.to_string(), *value);
        let mapped = evaluator.eval_expr(lambda.body, &locals)?;
        out.push(mapped);
    }
    Some(out)
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

impl IntPrintResolver {
    fn resolve(body: &str, functions: &EvaluatedFunctions<'_>) -> Option<i64> {
        let mut resolver = Self {
            locals: HashMap::new(),
            resolved_print: None,
        };
        let evaluator = IntEvaluator::root(functions);

        for statement in statements(body) {
            resolver.ingest_statement(statement, &evaluator)?;
        }

        resolver.resolved_print
    }

    fn ingest_statement(
        &mut self,
        statement: Statement<'_>,
        evaluator: &IntEvaluator<'_>,
    ) -> Option<()> {
        match statement {
            Statement::Binding { name, expr } => {
                self.bind_local(name, expr, evaluator);
                Some(())
            }
            Statement::Print(expr) => self.capture_print(expr, evaluator),
            Statement::Other => Some(()),
        }
    }

    fn bind_local(&mut self, name: &str, expr: &str, evaluator: &IntEvaluator<'_>) {
        let value = evaluator.eval_expr(expr, &self.locals);
        assign_local(name, value, &mut self.locals);
    }

    fn capture_print(&mut self, expr: &str, evaluator: &IntEvaluator<'_>) -> Option<()> {
        let value = evaluator.eval_expr(expr, &self.locals)?;
        capture_single_print(&mut self.resolved_print, value)
    }
}

#[derive(Clone, Copy)]
struct EvaluatedFunction<'a> {
    body: &'a str,
    parameter: Option<&'a str>,
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

fn capture_single_print<T>(resolved_print: &mut Option<T>, value: T) -> Option<()> {
    if resolved_print.is_some() {
        return None;
    }
    *resolved_print = Some(value);
    Some(())
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
            collect_identifiers(expr, &mut referenced);
            continue;
        }

        if let Some(expr) = parse_print_call(line) {
            collect_identifiers(expr, &mut referenced);
            continue;
        }

        collect_identifiers(line, &mut referenced);
    }

    let candidates: Vec<&str> = referenced
        .into_iter()
        .filter(|name| !assigned.contains(name))
        .filter(|name| !declaration_names.contains(name))
        .filter(|name| *name != "print")
        .collect();

    if candidates.len() == 1 {
        Some(candidates[0])
    } else {
        None
    }
}

fn collect_identifiers<'a>(expr: &'a str, out: &mut HashSet<&'a str>) {
    let mut start = None;

    for (idx, byte) in expr.as_bytes().iter().copied().enumerate() {
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

fn contains_pipeline(body: &str) -> bool {
    code_lines(body).any(|line| line.contains("|>"))
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

    use goby_core::parse_module;

    use super::*;

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

    #[test]
    fn emits_valid_wasm_header_for_main_module() {
        let module = parse_module("main : void -> void\nmain = 0\n").expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn emits_valid_wasm_for_print_literal() {
        let module = parse_module("main : void -> void\nmain = print \"Hello Goby!\"\n")
            .expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn emits_valid_wasm_for_long_print_literal() {
        let long_text = "x".repeat(128);
        let source = format!("main : void -> void\nmain = print \"{}\"\n", long_text);
        let module = parse_module(&source).expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_valid_wasm_module(&wasm);
    }

    #[test]
    fn emits_valid_wasm_for_print_via_local_binding() {
        let source = r#"
main : void -> void
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
main : void -> void
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
    fn reports_pipeline_as_unsupported() {
        let source = r#"
main : void -> void
main =
  [1, 2, 3] |> print
"#;
        let module = parse_module(source).expect("parse should work");
        let err = compile_module(&module).expect_err("codegen should fail");
        assert_eq!(
            err.message,
            "pipeline operator (`|>`) is not yet supported in Wasm codegen"
        );
    }

    #[test]
    fn reports_print_list_for_map_with_named_lambda() {
        let source = r#"
main : void -> void
main =
  print map [3, 4, 5] (|n| -> n * 10)
"#;
        let module = parse_module(source).expect("parse should work");
        let err = compile_module(&module).expect_err("codegen should fail");
        assert_eq!(
            err.message,
            "print List is not yet supported in Wasm codegen"
        );
    }

    #[test]
    fn reports_print_list_for_map_with_placeholder_lambda() {
        let source = r#"
main : void -> void
main =
  print map [6, 7] (_ * 10)
"#;
        let module = parse_module(source).expect("parse should work");
        let err = compile_module(&module).expect_err("codegen should fail");
        assert_eq!(
            err.message,
            "print List is not yet supported in Wasm codegen"
        );
    }

    #[test]
    fn reports_print_list_for_list_function_call_with_spaces() {
        let source = r#"
mul_tens : List Int -> List Int
mul_tens ns = map ns (|n| -> n * 10)

main : void -> void
main =
  print mul_tens [3, 4, 5]
"#;
        let module = parse_module(source).expect("parse should work");
        let err = compile_module(&module).expect_err("codegen should fail");
        assert_eq!(
            err.message,
            "print List is not yet supported in Wasm codegen"
        );
    }
}
