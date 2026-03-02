use std::collections::HashMap;

use goby_core::{BinOpKind, Expr, Module, Stmt};

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

    let Some(output_text) = collect_phase2_output_text(stmts) else {
        return Ok(None);
    };
    if output_text.is_empty() {
        return Ok(None);
    }

    let wasm = builder.emit_static_print_module(&output_text)?;
    Ok(Some(wasm))
}

#[derive(Clone)]
enum NativeValue {
    String(String),
    Int(i64),
    Bool(bool),
}

impl NativeValue {
    fn as_output_text(&self) -> String {
        match self {
            Self::String(s) => s.clone(),
            Self::Int(n) => n.to_string(),
            Self::Bool(true) => "True".to_string(),
            Self::Bool(false) => "False".to_string(),
        }
    }
}

fn collect_phase2_output_text(stmts: &[Stmt]) -> Option<String> {
    let mut bindings: HashMap<&str, NativeValue> = HashMap::new();
    let mut outputs: Vec<String> = Vec::new();
    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value } => {
                let val = eval_expr(value, &bindings)?;
                bindings.insert(name.as_str(), val);
            }
            Stmt::Expr(expr) => {
                let val = as_print_expr(expr, &bindings)?;
                outputs.push(val.as_output_text());
            }
            Stmt::Using { .. } => return None,
        }
    }
    Some(outputs.join("\n"))
}

fn as_print_expr(expr: &Expr, bindings: &HashMap<&str, NativeValue>) -> Option<NativeValue> {
    let Expr::Call { callee, arg } = expr else {
        return None;
    };
    let Expr::Var(name) = callee.as_ref() else {
        return None;
    };
    if name != "print" {
        return None;
    }
    eval_expr(arg, bindings)
}

fn eval_expr(expr: &Expr, bindings: &HashMap<&str, NativeValue>) -> Option<NativeValue> {
    match expr {
        Expr::StringLit(s) => Some(NativeValue::String(s.clone())),
        Expr::IntLit(n) => Some(NativeValue::Int(*n)),
        Expr::BoolLit(b) => Some(NativeValue::Bool(*b)),
        Expr::Var(name) => bindings.get(name.as_str()).cloned(),
        Expr::BinOp { op, left, right } => {
            let lhs = eval_expr(left, bindings)?;
            let rhs = eval_expr(right, bindings)?;
            match (op, lhs, rhs) {
                (BinOpKind::Add, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Int(a.checked_add(b)?))
                }
                (BinOpKind::Mul, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Int(a.checked_mul(b)?))
                }
                (BinOpKind::Eq, NativeValue::Int(a), NativeValue::Int(b)) => {
                    Some(NativeValue::Bool(a == b))
                }
                _ => None,
            }
        }
        _ => None,
    }
}
