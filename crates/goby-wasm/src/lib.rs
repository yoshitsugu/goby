use goby_core::Module;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodegenError {
    pub message: String,
}

pub fn compile_module(module: &Module) -> Result<Vec<u8>, CodegenError> {
    if !module.declarations.iter().any(|d| d.name == "main") {
        return Err(CodegenError {
            message: "Wasm codegen requires a `main` declaration".to_string(),
        });
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
}
