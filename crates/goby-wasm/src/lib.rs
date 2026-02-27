use std::collections::HashMap;

use goby_core::{Module, resolve_print_text, types::parse_function_type};

const IOVEC_OFFSET: u32 = 0;
const NWRITTEN_OFFSET: u32 = 8;
const TEXT_OFFSET: u32 = 16;
const ERR_MISSING_MAIN: &str = "Wasm codegen requires a `main` declaration";
const ERR_UNSUPPORTED_PIPELINE: &str =
    "pipeline operator (`|>`) is not yet supported in Wasm codegen";
const ERR_UNSUPPORTED_HIGHER_ORDER: &str =
    "higher-order calls are not yet supported in Wasm codegen";
const ERR_UNSUPPORTED_PRINT_INT: &str = "print Int is not yet supported in Wasm codegen";
const ERR_UNSUPPORTED_PRINT_LIST: &str = "print List is not yet supported in Wasm codegen";
const ERR_UNSUPPORTED_PRINT_UNKNOWN: &str =
    "print argument is unsupported for Wasm codegen (only String is supported)";

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

    if let Some(value) = resolve_print_int(&main.body) {
        return compile_print_module(&value.to_string());
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

    for decl in &module.declarations {
        if looks_like_higher_order_usage(&decl.body) {
            return Some(ERR_UNSUPPORTED_HIGHER_ORDER);
        }
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

fn resolve_print_int(body: &str) -> Option<i64> {
    IntPrintResolver::resolve(body)
}

fn eval_int_expr(expr: &str, locals: &HashMap<String, i64>) -> Option<i64> {
    let expr = expr.trim();
    if expr.is_empty() {
        return None;
    }

    if let Some((left, right)) = expr.split_once(" + ") {
        return eval_int_expr(left, locals)?.checked_add(eval_int_expr(right, locals)?);
    }

    if let Some((left, right)) = expr.split_once(" * ") {
        return eval_int_expr(left, locals)?.checked_mul(eval_int_expr(right, locals)?);
    }

    if is_int_literal(expr) {
        return expr.parse().ok();
    }

    locals.get(expr).copied()
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

impl IntPrintResolver {
    fn resolve(body: &str) -> Option<i64> {
        let mut resolver = Self {
            locals: HashMap::new(),
            resolved_print: None,
        };

        for statement in statements(body) {
            resolver.ingest_statement(statement)?;
        }

        resolver.resolved_print
    }

    fn ingest_statement(&mut self, statement: Statement<'_>) -> Option<()> {
        match statement {
            Statement::Binding { name, expr } => {
                self.bind_local(name, expr);
                Some(())
            }
            Statement::Print(expr) => self.capture_print(expr),
            Statement::Other => Some(()),
        }
    }

    fn bind_local(&mut self, name: &str, expr: &str) {
        if let Some(value) = eval_int_expr(expr, &self.locals) {
            self.locals.insert(name.to_string(), value);
        } else {
            self.locals.remove(name);
        }
    }

    fn capture_print(&mut self, expr: &str) -> Option<()> {
        let value = eval_int_expr(expr, &self.locals)?;
        if self.resolved_print.is_some() {
            return None;
        }
        self.resolved_print = Some(value);
        Some(())
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

fn looks_like_higher_order_usage(body: &str) -> bool {
    code_lines(body)
        .any(|line| line.contains("| ->") || line.contains("|n| ->") || line.contains("(_"))
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
    use goby_core::parse_module;

    use super::*;

    fn assert_valid_wasm_module(wasm: &[u8]) {
        assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d]);
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
    fn reports_higher_order_usage_as_unsupported() {
        let source = r#"
f : List Int -> List Int
f ns = map ns (|n| -> n * 10)

main : void -> void
main = 0
"#;
        let module = parse_module(source).expect("parse should work");
        let err = compile_module(&module).expect_err("codegen should fail");
        assert_eq!(
            err.message,
            "higher-order calls are not yet supported in Wasm codegen"
        );
    }
}
