//! Goby IR → `WasmBackendInstr` lowering for the general Wasm path.
//!
//! See `doc/wasm_runtime_architecture.md §3` for the IR → Backend IR mapping.

use goby_core::ir::{CompExpr, ValueExpr};

use crate::gen_lower::backend_ir::{SplitIndexOperand, WasmBackendInstr};
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

        CompExpr::Let {
            name, value, body, ..
        } => {
            // F4: detect fused split-each pattern before general Let lowering.
            if let Some(result) = try_lower_split_each(name, value, body) {
                return result;
            }
            if let Some(result) = try_lower_split_get_print(name, value, body) {
                return result;
            }
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

/// Try to match the fused split-each pattern:
///
/// ```text
/// let <lines_name> = string.split(<text_var>, <sep_lit>)
/// in each <lines_name> <Effect>.<op>
/// ```
///
/// Returns `Some(Ok([SplitEachPrint {...}]))` when matched,
/// `Some(Err(UnsupportedForm))` when partially matched but not fully supported,
/// `None` when the pattern does not apply (fall through to general Let lowering).
fn try_lower_split_each(
    let_name: &str,
    value: &CompExpr,
    body: &CompExpr,
) -> Option<Result<Vec<WasmBackendInstr>, LowerError>> {
    // value must be Call(GlobalRef("string","split"), [Var(text), StrLit(sep)])
    let (text_name, sep_str) = match value {
        CompExpr::Call { callee, args }
            if matches!(
                callee.as_ref(),
                ValueExpr::GlobalRef { module, name }
                    if module == "string" && name == "split"
            ) && args.len() == 2 =>
        {
            match (&args[0], &args[1]) {
                (ValueExpr::Var(text), ValueExpr::StrLit(sep)) => (text.as_str(), sep.as_str()),
                _ => return None,
            }
        }
        _ => return None,
    };

    // body must be Call(Var("each"), [Var(lines_name), GlobalRef(effect, op)])
    // where lines_name == let_name
    let (effect, op) = match body {
        CompExpr::Call { callee, args }
            if matches!(callee.as_ref(), ValueExpr::Var(n) if n == "each") && args.len() == 2 =>
        {
            match (&args[0], &args[1]) {
                (
                    ValueExpr::Var(list_name),
                    ValueExpr::GlobalRef {
                        module: eff,
                        name: op,
                    },
                ) if list_name == let_name => (eff.as_str(), op.as_str()),
                _ => return None,
            }
        }
        _ => return None,
    };

    // Restriction: sep must be exactly 1 byte for F4.
    let sep_bytes = sep_str.as_bytes().to_vec();
    if sep_bytes.len() != 1 {
        return Some(Err(LowerError::UnsupportedForm {
            node: format!(
                "SplitEachPrint: multi-byte separator '{sep_str}' is not yet supported (F5+)"
            ),
        }));
    }

    Some(Ok(vec![WasmBackendInstr::SplitEachPrint {
        text_local: text_name.to_string(),
        sep_bytes,
        effect: effect.to_string(),
        op: op.to_string(),
    }]))
}

fn try_lower_split_get_print(
    let_name: &str,
    value: &CompExpr,
    body: &CompExpr,
) -> Option<Result<Vec<WasmBackendInstr>, LowerError>> {
    let (text_name, sep_str) = match value {
        CompExpr::Call { callee, args }
            if matches!(
                callee.as_ref(),
                ValueExpr::GlobalRef { module, name }
                    if module == "string" && name == "split"
            ) && args.len() == 2 =>
        {
            match (&args[0], &args[1]) {
                (ValueExpr::Var(text), ValueExpr::StrLit(sep)) => (text.as_str(), sep.as_str()),
                _ => return None,
            }
        }
        _ => return None,
    };

    let (index, op) = match body {
        CompExpr::Let {
            name: item_name,
            value,
            body,
            ..
        } => {
            let index = match value.as_ref() {
                CompExpr::Call { callee, args }
                    if matches!(
                        callee.as_ref(),
                        ValueExpr::GlobalRef { module, name }
                            if module == "list" && name == "get"
                    ) && args.len() == 2 =>
                {
                    match (&args[0], &args[1]) {
                        (ValueExpr::Var(list_name), ValueExpr::IntLit(index))
                            if list_name == let_name =>
                        {
                            SplitIndexOperand::Const(*index)
                        }
                        (ValueExpr::Var(list_name), ValueExpr::Var(index_name))
                            if list_name == let_name =>
                        {
                            SplitIndexOperand::Local(index_name.clone())
                        }
                        _ => return None,
                    }
                }
                _ => return None,
            };

            match body.as_ref() {
                CompExpr::PerformEffect { effect, op, args }
                    if effect == "Print"
                        && (op == "print" || op == "println")
                        && args.len() == 1 =>
                {
                    match &args[0] {
                        ValueExpr::Var(name) if name == item_name => (index, op.as_str()),
                        _ => return None,
                    }
                }
                _ => return None,
            }
        }
        _ => return None,
    };

    let sep_bytes = sep_str.as_bytes().to_vec();
    if sep_bytes.len() != 1 {
        return Some(Err(LowerError::UnsupportedForm {
            node: format!(
                "SplitGetPrint: multi-byte separator '{sep_str}' is not yet supported (F5+)"
            ),
        }));
    }

    Some(Ok(vec![WasmBackendInstr::SplitGetPrint {
        text_local: text_name.to_string(),
        sep_bytes,
        index,
        op: op.to_string(),
    }]))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gen_lower::backend_ir::{SplitIndexOperand, WasmBackendInstr as I};

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
                I::LoadLocal {
                    name: "x".to_string()
                },
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
                I::DeclareLocal {
                    name: "text".to_string()
                },
                I::EffectOp {
                    effect: "Read".to_string(),
                    op: "read".to_string()
                },
                I::StoreLocal {
                    name: "text".to_string()
                },
                I::LoadLocal {
                    name: "text".to_string()
                },
                I::EffectOp {
                    effect: "Print".to_string(),
                    op: "print".to_string()
                },
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

    #[test]
    fn lower_split_each_emits_fused_instr() {
        // let lines = string.split(text, "\n") in each lines Print.println
        let comp = CompExpr::Let {
            name: "lines".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::GlobalRef {
                    module: "string".to_string(),
                    name: "split".to_string(),
                }),
                args: vec![
                    ValueExpr::Var("text".to_string()),
                    ValueExpr::StrLit("\n".to_string()),
                ],
            }),
            body: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::Var("each".to_string())),
                args: vec![
                    ValueExpr::Var("lines".to_string()),
                    ValueExpr::GlobalRef {
                        module: "Print".to_string(),
                        name: "println".to_string(),
                    },
                ],
            }),
        };
        let instrs = lower_comp(&comp).expect("fused pattern should succeed");
        assert_eq!(instrs.len(), 1);
        assert!(
            matches!(&instrs[0], I::SplitEachPrint { text_local, sep_bytes, effect, op }
                if text_local == "text"
                    && sep_bytes == b"\n"
                    && effect == "Print"
                    && op == "println"),
            "expected SplitEachPrint, got {:?}",
            instrs[0]
        );
    }

    #[test]
    fn lower_split_each_multichar_sep_returns_err() {
        let comp = CompExpr::Let {
            name: "lines".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::GlobalRef {
                    module: "string".to_string(),
                    name: "split".to_string(),
                }),
                args: vec![
                    ValueExpr::Var("text".to_string()),
                    ValueExpr::StrLit("\r\n".to_string()), // 2-byte sep
                ],
            }),
            body: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::Var("each".to_string())),
                args: vec![
                    ValueExpr::Var("lines".to_string()),
                    ValueExpr::GlobalRef {
                        module: "Print".to_string(),
                        name: "println".to_string(),
                    },
                ],
            }),
        };
        assert!(matches!(
            lower_comp(&comp),
            Err(LowerError::UnsupportedForm { .. })
        ));
    }

    #[test]
    fn lower_split_get_println_emits_fused_instr() {
        let comp = CompExpr::Let {
            name: "lines".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::GlobalRef {
                    module: "string".to_string(),
                    name: "split".to_string(),
                }),
                args: vec![
                    ValueExpr::Var("text".to_string()),
                    ValueExpr::StrLit("\n".to_string()),
                ],
            }),
            body: Box::new(CompExpr::Let {
                name: "line".to_string(),
                ty: goby_core::ir::IrType::Unknown,
                value: Box::new(CompExpr::Call {
                    callee: Box::new(ValueExpr::GlobalRef {
                        module: "list".to_string(),
                        name: "get".to_string(),
                    }),
                    args: vec![ValueExpr::Var("lines".to_string()), ValueExpr::IntLit(1)],
                }),
                body: Box::new(CompExpr::PerformEffect {
                    effect: "Print".to_string(),
                    op: "println".to_string(),
                    args: vec![ValueExpr::Var("line".to_string())],
                }),
            }),
        };
        let instrs = lower_comp(&comp).expect("fused pattern should succeed");
        assert_eq!(
            instrs,
            vec![I::SplitGetPrint {
                text_local: "text".to_string(),
                sep_bytes: b"\n".to_vec(),
                index: SplitIndexOperand::Const(1),
                op: "println".to_string(),
            }]
        );
    }

    #[test]
    fn lower_split_get_print_with_local_index_emits_fused_instr() {
        let comp = CompExpr::Let {
            name: "lines".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::GlobalRef {
                    module: "string".to_string(),
                    name: "split".to_string(),
                }),
                args: vec![
                    ValueExpr::Var("text".to_string()),
                    ValueExpr::StrLit("\n".to_string()),
                ],
            }),
            body: Box::new(CompExpr::Let {
                name: "line".to_string(),
                ty: goby_core::ir::IrType::Unknown,
                value: Box::new(CompExpr::Call {
                    callee: Box::new(ValueExpr::GlobalRef {
                        module: "list".to_string(),
                        name: "get".to_string(),
                    }),
                    args: vec![
                        ValueExpr::Var("lines".to_string()),
                        ValueExpr::Var("idx".to_string()),
                    ],
                }),
                body: Box::new(CompExpr::PerformEffect {
                    effect: "Print".to_string(),
                    op: "print".to_string(),
                    args: vec![ValueExpr::Var("line".to_string())],
                }),
            }),
        };
        let instrs = lower_comp(&comp).expect("fused pattern should succeed");
        assert_eq!(
            instrs,
            vec![I::SplitGetPrint {
                text_local: "text".to_string(),
                sep_bytes: b"\n".to_vec(),
                index: SplitIndexOperand::Local("idx".to_string()),
                op: "print".to_string(),
            }]
        );
    }
}
