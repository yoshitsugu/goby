use std::collections::HashMap;

use goby_core::{Expr, Module, Stmt};

use crate::{CodegenError, backend::WasmProgramBuilder, layout::MemoryLayout};

pub(crate) fn try_emit_native_module(module: &Module) -> Result<Option<Vec<u8>>, CodegenError> {
    if module.declarations.is_empty() {
        return Err(CodegenError {
            message: "module has no declarations".to_string(),
        });
    }

    let builder = WasmProgramBuilder::new(MemoryLayout::default());
    let layout = builder.layout();
    let _ = (
        layout.iovec_offset,
        layout.nwritten_offset,
        layout.heap_base,
    );

    let Some(main_decl) = module.declarations.iter().find(|decl| decl.name == "main") else {
        return Ok(None);
    };
    let Some(stmts) = main_decl.parsed_body.as_deref() else {
        return Ok(None);
    };

    let output_text = collect_phase1_output_text(stmts)?;
    if output_text.is_empty() {
        return Ok(None);
    }

    let wasm = builder.emit_static_print_module(&output_text)?;
    Ok(Some(wasm))
}

fn collect_phase1_output_text(stmts: &[Stmt]) -> Result<String, CodegenError> {
    let mut bindings: HashMap<&str, String> = HashMap::new();
    let mut outputs: Vec<String> = Vec::new();
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } => {
                let Some(text) = as_string_expr(value, &bindings) else {
                    return Err(CodegenError {
                        message: format!("unsupported binding in native phase-1 lowerer: {}", name),
                    });
                };
                bindings.insert(name.as_str(), text);
            }
            Stmt::Expr(expr) => {
                let Some(text) = as_print_expr(expr, &bindings) else {
                    return Err(CodegenError {
                        message: "unsupported expression in native phase-1 lowerer".to_string(),
                    });
                };
                outputs.push(text);
            }
            Stmt::Using { .. } => {
                return Err(CodegenError {
                    message: "using blocks are not supported in native phase-1 lowerer".to_string(),
                });
            }
        }
    }
    Ok(outputs.join("\n"))
}

fn as_print_expr(expr: &Expr, bindings: &HashMap<&str, String>) -> Option<String> {
    let Expr::Call { callee, arg } = expr else {
        return None;
    };
    let Expr::Var(name) = callee.as_ref() else {
        return None;
    };
    if name != "print" {
        return None;
    }
    as_string_expr(arg, bindings)
}

fn as_string_expr(expr: &Expr, bindings: &HashMap<&str, String>) -> Option<String> {
    match expr {
        Expr::StringLit(s) => Some(s.clone()),
        Expr::Var(name) => bindings.get(name.as_str()).cloned(),
        _ => None,
    }
}
