//! Goby IR → `WasmBackendInstr` lowering for the general Wasm path.
//!
//! See `doc/wasm_runtime_architecture.md §3` for the IR → Backend IR mapping.

use goby_core::ir::{CompExpr, ValueExpr};

use crate::gen_lower::backend_ir::WasmBackendInstr;
use crate::gen_lower::value::{ValueError, encode_bool, encode_int, encode_unit};

/// Error produced when the general lowering path encounters an unsupported IR node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum LowerError {
    /// IR node that the general lowering path does not yet support.
    UnsupportedForm { node: String },
    /// Integer literal outside the 60-bit representable range.
    IntOutOfRange(i64),
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LowerError::UnsupportedForm { node } => {
                write!(f, "unsupported IR form in general lowering path: {node}")
            }
            LowerError::IntOutOfRange(n) => {
                write!(f, "integer {n} is outside the 60-bit representable range")
            }
        }
    }
}

impl From<ValueError> for LowerError {
    fn from(e: ValueError) -> Self {
        match e {
            ValueError::IntOutOfRange(n) => LowerError::IntOutOfRange(n),
        }
    }
}

/// Lower a `CompExpr` to a flat sequence of `WasmBackendInstr`.
///
/// Supported IR nodes: `Value`, `Let`, `Seq`, `PerformEffect`, `Call` (GlobalRef callee only).
/// All other nodes produce `Err(LowerError::UnsupportedForm)`.
pub(crate) fn lower_comp(comp: &CompExpr) -> Result<Vec<WasmBackendInstr>, LowerError> {
    match comp {
        CompExpr::Value(v) => lower_value(v),

        CompExpr::Let { name, value, body, .. } => {
            let mut instrs = vec![WasmBackendInstr::DeclareLocal { name: name.clone() }];
            instrs.extend(lower_comp(value)?);
            instrs.push(WasmBackendInstr::StoreLocal { name: name.clone() });
            instrs.extend(lower_comp(body)?);
            Ok(instrs)
        }

        CompExpr::Seq { stmts, tail } => {
            let mut instrs = Vec::new();
            for stmt in stmts {
                instrs.extend(lower_comp(stmt)?);
                instrs.push(WasmBackendInstr::Drop);
            }
            instrs.extend(lower_comp(tail)?);
            Ok(instrs)
        }

        CompExpr::PerformEffect { effect, op, args } => {
            let mut instrs = Vec::new();
            for arg in args {
                instrs.extend(lower_value(arg)?);
            }
            instrs.push(WasmBackendInstr::EffectOp {
                effect: effect.clone(),
                op: op.clone(),
            });
            Ok(instrs)
        }

        CompExpr::Call { callee, args } => {
            if let ValueExpr::GlobalRef { module, name } = callee.as_ref() {
                let mut instrs = Vec::new();
                for arg in args {
                    instrs.extend(lower_value(arg)?);
                }
                instrs.push(WasmBackendInstr::CallHelper {
                    name: format!("{}.{}", module, name),
                    arg_count: args.len(),
                });
                Ok(instrs)
            } else {
                Err(LowerError::UnsupportedForm {
                    node: format!("Call with non-GlobalRef callee: {:?}", callee),
                })
            }
        }

        other => Err(LowerError::UnsupportedForm {
            node: format!("{:?}", other),
        }),
    }
}

/// Lower a `ValueExpr` to a flat sequence of `WasmBackendInstr`.
///
/// `StrLit` returns `UnsupportedForm` — string heap allocation is F4.
pub(crate) fn lower_value(v: &ValueExpr) -> Result<Vec<WasmBackendInstr>, LowerError> {
    match v {
        ValueExpr::Unit => Ok(vec![WasmBackendInstr::I64Const(encode_unit())]),
        ValueExpr::IntLit(n) => Ok(vec![WasmBackendInstr::I64Const(encode_int(*n)?)]),
        ValueExpr::BoolLit(b) => Ok(vec![WasmBackendInstr::I64Const(encode_bool(*b))]),
        ValueExpr::StrLit(_) => Err(LowerError::UnsupportedForm {
            node: "StrLit (string heap allocation is F4)".to_string(),
        }),
        ValueExpr::Var(name) => Ok(vec![WasmBackendInstr::LoadLocal { name: name.clone() }]),
        ValueExpr::GlobalRef { module, name } => Ok(vec![WasmBackendInstr::LoadLocal {
            name: format!("{}.{}", module, name),
        }]),
        other => Err(LowerError::UnsupportedForm {
            node: format!("{:?}", other),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gen_lower::backend_ir::WasmBackendInstr as I;

    #[test]
    fn lower_perform_read() {
        let comp = CompExpr::PerformEffect {
            effect: "Read".to_string(),
            op: "read".to_string(),
            args: vec![],
        };
        let instrs = lower_comp(&comp).expect("lower should succeed");
        assert_eq!(
            instrs,
            vec![I::EffectOp {
                effect: "Read".to_string(),
                op: "read".to_string(),
            }]
        );
    }

    #[test]
    fn lower_perform_print_with_var() {
        let comp = CompExpr::PerformEffect {
            effect: "Print".to_string(),
            op: "print".to_string(),
            args: vec![ValueExpr::Var("x".to_string())],
        };
        let instrs = lower_comp(&comp).expect("lower should succeed");
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal { name: "x".to_string() },
                I::EffectOp {
                    effect: "Print".to_string(),
                    op: "print".to_string(),
                },
            ]
        );
    }

    #[test]
    fn lower_let_read_then_print() {
        // let text = Read.read(); Print.print(text)
        let comp = CompExpr::Let {
            name: "text".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::PerformEffect {
                effect: "Read".to_string(),
                op: "read".to_string(),
                args: vec![],
            }),
            body: Box::new(CompExpr::PerformEffect {
                effect: "Print".to_string(),
                op: "print".to_string(),
                args: vec![ValueExpr::Var("text".to_string())],
            }),
        };
        let instrs = lower_comp(&comp).expect("lower should succeed");
        assert_eq!(
            instrs,
            vec![
                I::DeclareLocal { name: "text".to_string() },
                I::EffectOp { effect: "Read".to_string(), op: "read".to_string() },
                I::StoreLocal { name: "text".to_string() },
                I::LoadLocal { name: "text".to_string() },
                I::EffectOp { effect: "Print".to_string(), op: "print".to_string() },
            ]
        );
    }

    #[test]
    fn lower_with_handler_returns_err() {
        let comp = CompExpr::WithHandler {
            handler: Box::new(CompExpr::Value(ValueExpr::Unit)),
            body: Box::new(CompExpr::Value(ValueExpr::Unit)),
        };
        assert!(matches!(
            lower_comp(&comp),
            Err(LowerError::UnsupportedForm { .. })
        ));
    }

    #[test]
    fn lower_int_lit_zero() {
        let v = ValueExpr::IntLit(0);
        let instrs = lower_value(&v).expect("should encode 0");
        assert_eq!(instrs, vec![I::I64Const(encode_int(0).unwrap())]);
    }

    #[test]
    fn lower_str_lit_returns_err() {
        let v = ValueExpr::StrLit("x".to_string());
        assert!(matches!(
            lower_value(&v),
            Err(LowerError::UnsupportedForm { .. })
        ));
    }
}
