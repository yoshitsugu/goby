use goby_core::Module;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodegenError {
    pub message: String,
}

pub fn compile_module(module: &Module) -> Result<Vec<u8>, CodegenError> {
    let Some(main) = module.declarations.iter().find(|d| d.name == "main") else {
        return Err(CodegenError {
            message: "Wasm codegen requires a `main` declaration".to_string(),
        });
    };

    if let Some(text) = parse_print_literal(&main.body) {
        return compile_print_module(&text);
    }

    // Minimal MVP codegen target: emit a valid module that exports an empty `main`.
    // This is a temporary bridge until expression-level lowering is implemented.
    Ok(minimal_main_module())
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

fn parse_print_literal(body: &str) -> Option<String> {
    let trimmed = body.trim();
    let prefix = "print \"";
    if !trimmed.starts_with(prefix) || !trimmed.ends_with('"') {
        return None;
    }
    let content = &trimmed[prefix.len()..trimmed.len() - 1];
    if content.contains('"') {
        return None;
    }
    Some(content.to_string())
}

fn compile_print_module(text: &str) -> Result<Vec<u8>, CodegenError> {
    let text_bytes = text.as_bytes();
    let text_offset = 8u32;
    let iovec_offset = 0u32;
    let nwritten_offset = 20u32;

    let mut iovec_bytes = Vec::with_capacity(8);
    iovec_bytes.extend_from_slice(&text_offset.to_le_bytes());
    iovec_bytes.extend_from_slice(&(text_bytes.len() as u32).to_le_bytes());

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
        text_offset = text_offset,
        iovec_offset = iovec_offset,
        nwritten_offset = nwritten_offset,
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

#[cfg(test)]
mod tests {
    use goby_core::parse_module;

    use super::*;

    #[test]
    fn emits_valid_wasm_header_for_main_module() {
        let module = parse_module("main : void -> void\nmain = 0\n").expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d]);
    }

    #[test]
    fn emits_valid_wasm_for_print_literal() {
        let module = parse_module("main : void -> void\nmain = print \"Hello Goby!\"\n")
            .expect("parse should work");
        let wasm = compile_module(&module).expect("codegen should succeed");
        assert_eq!(&wasm[..4], &[0x00, 0x61, 0x73, 0x6d]);
    }
}
