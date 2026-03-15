use std::collections::{HashMap, HashSet};

use goby_core::{Expr, Module, Stmt, types::parse_function_type};

use crate::{BUILTIN_PRINT, MAX_EVAL_DEPTH, RuntimeLocals};

pub(crate) type EvaluatedFunctions<'a> = HashMap<&'a str, EvaluatedFunction<'a>>;

pub(crate) fn collect_functions_with_result<'a>(
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

pub(crate) fn collect_unit_functions<'a>(module: &'a Module) -> EvaluatedFunctions<'a> {
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
pub(crate) enum IntCallable {
    Lambda(IntLambda),
    Named(String),
    AstLambda(Box<AstLambdaCallable>),
}

#[derive(Clone)]
pub(crate) struct AstLambdaCallable {
    pub(crate) parameter: String,
    pub(crate) body: Expr,
    pub(crate) captured_locals: RuntimeLocals,
    pub(crate) captured_callables: HashMap<String, IntCallable>,
}

#[derive(Clone, Debug)]
pub(crate) struct IntLambda {
    pub(crate) parameter: String,
    pub(crate) body: String,
}

pub(crate) struct IntEvaluator<'a> {
    pub(crate) functions: &'a EvaluatedFunctions<'a>,
    depth: usize,
}

impl<'a> IntEvaluator<'a> {
    pub(crate) fn root(functions: &'a EvaluatedFunctions<'a>) -> Self {
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

    pub(crate) fn eval_expr(
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

    pub(crate) fn eval_function(
        &self,
        function: &EvaluatedFunction<'a>,
        arg: Option<i64>,
    ) -> Option<i64> {
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

    pub(crate) fn eval_callable(
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

    pub(crate) fn eval_lambda(
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

pub(crate) struct ListIntEvaluator<'a> {
    pub(crate) functions: &'a EvaluatedFunctions<'a>,
    allow_imported_list_map: bool,
    depth: usize,
}

impl<'a> ListIntEvaluator<'a> {
    pub(crate) fn root(
        functions: &'a EvaluatedFunctions<'a>,
        allow_imported_list_map: bool,
    ) -> Self {
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

    pub(crate) fn eval_expr(
        &self,
        expr: &str,
        locals: &HashMap<String, Vec<i64>>,
    ) -> Option<Vec<i64>> {
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

    pub(crate) fn eval_function(
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

pub(crate) enum Statement<'a> {
    Binding { name: &'a str, expr: &'a str },
    MutBinding { name: &'a str, expr: &'a str },
    Assign { name: &'a str, expr: &'a str },
    Print(&'a str),
    Expr(&'a str),
}

pub(crate) fn statements(body: &str) -> impl Iterator<Item = Statement<'_>> {
    code_lines(body).map(parse_statement)
}

struct MapLambda {
    parameter: String,
    body: String,
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

pub(crate) fn parse_int_callable(expr: &str) -> Option<IntCallable> {
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
pub(crate) struct EvaluatedFunction<'a> {
    pub(crate) body: &'a str,
    pub(crate) parameter: Option<&'a str>,
    pub(crate) parsed_stmts: Option<&'a [Stmt]>,
}

pub(crate) fn code_lines(body: &str) -> impl Iterator<Item = &str> {
    body.lines().map(str::trim).filter(|line| {
        let line = *line;
        !line.is_empty() && !line.starts_with('#')
    })
}

pub(crate) fn parse_call(expr: &str) -> Option<(&str, &str)> {
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

pub(crate) use goby_core::is_identifier;

pub(crate) fn is_string_literal(expr: &str) -> bool {
    expr.starts_with('"') && expr.ends_with('"') && expr.len() >= 2
}

pub(crate) fn is_int_literal(expr: &str) -> bool {
    let raw = expr.strip_prefix('-').unwrap_or(expr);
    !raw.is_empty() && raw.chars().all(|c| c.is_ascii_digit())
}

pub(crate) fn is_list_literal(expr: &str) -> bool {
    expr.starts_with('[') && expr.ends_with(']')
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
