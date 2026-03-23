//! Goby IR → `WasmBackendInstr` lowering for the general Wasm path.
//!
//! The IR → backend-IR mapping is documented in `backend_ir.rs`.

use std::collections::HashMap;

use goby_core::ir::{CompExpr, ValueExpr};

use crate::gen_lower::backend_ir::{BackendIntrinsic, SplitIndexOperand, WasmBackendInstr};
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
    lower_comp_with_aliases(comp, &HashMap::new())
}

fn lower_comp_with_aliases(
    comp: &CompExpr,
    aliases: &HashMap<String, AliasValue>,
) -> Result<Vec<WasmBackendInstr>, LowerError> {
    match comp {
        CompExpr::Value(v) => lower_value(v),

        CompExpr::Let {
            name, value, body, ..
        } => {
            // Detect fused split-each pattern before general Let lowering.
            if let Some(result) = try_lower_split_each(name, value, body, aliases) {
                return result;
            }
            if let Some(result) = try_lower_split_get_print(name, value, body, aliases) {
                return result;
            }
            if let Some(result) = try_lower_graphemes_get_print(name, value, body, aliases) {
                return result;
            }
            if let Some(alias) = alias_value_from_comp(value) {
                let mut scoped_aliases = aliases.clone();
                scoped_aliases.insert(name.clone(), alias);
                let body_instrs = lower_comp_with_aliases(body, &scoped_aliases)?;
                if !instrs_load_local(&body_instrs, name) {
                    return Ok(body_instrs);
                }
            }
            let mut instrs = vec![WasmBackendInstr::DeclareLocal { name: name.clone() }];
            instrs.extend(lower_comp_with_aliases(value, aliases)?);
            instrs.push(WasmBackendInstr::StoreLocal { name: name.clone() });
            if let Some(alias) = alias_value_from_comp(value) {
                let mut scoped_aliases = aliases.clone();
                scoped_aliases.insert(name.clone(), alias);
                instrs.extend(lower_comp_with_aliases(body, &scoped_aliases)?);
            } else {
                instrs.extend(lower_comp_with_aliases(body, aliases)?);
            }
            Ok(instrs)
        }

        CompExpr::Seq { stmts, tail } => {
            let mut instrs = Vec::new();
            for stmt in stmts {
                instrs.extend(lower_comp_with_aliases(stmt, aliases)?);
                instrs.push(WasmBackendInstr::Drop);
            }
            instrs.extend(lower_comp_with_aliases(tail, aliases)?);
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
            if let Some((effect, op)) = resolve_effect_call_target(callee, aliases) {
                let mut instrs = Vec::new();
                for arg in args {
                    instrs.extend(lower_value(arg)?);
                }
                instrs.push(WasmBackendInstr::EffectOp { effect, op });
                Ok(instrs)
            } else if let Some(intrinsic) =
                resolve_intrinsic_call_target(callee, aliases, args.len())
            {
                let mut instrs = Vec::new();
                for arg in args {
                    instrs.extend(lower_value(arg)?);
                }
                if args.len() != intrinsic.arity() {
                    return Err(LowerError::UnsupportedForm {
                        node: format!(
                            "Intrinsic call with wrong arity: {:?} expected {}, got {}",
                            intrinsic,
                            intrinsic.arity(),
                            args.len()
                        ),
                    });
                }
                instrs.push(WasmBackendInstr::Intrinsic { intrinsic });
                Ok(instrs)
            } else {
                Err(LowerError::UnsupportedForm {
                    node: format!("Call with unsupported callee: {:?}", callee),
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
pub(crate) fn lower_value(v: &ValueExpr) -> Result<Vec<WasmBackendInstr>, LowerError> {
    match v {
        ValueExpr::Unit => Ok(vec![WasmBackendInstr::I64Const(encode_unit())]),
        ValueExpr::IntLit(n) => Ok(vec![WasmBackendInstr::I64Const(encode_int(*n)?)]),
        ValueExpr::BoolLit(b) => Ok(vec![WasmBackendInstr::I64Const(encode_bool(*b))]),
        ValueExpr::StrLit(text) => Ok(vec![WasmBackendInstr::PushStaticString {
            text: text.clone(),
        }]),
        ValueExpr::ListLit { .. } => Err(LowerError::UnsupportedForm {
            node: "ListLit".to_string(),
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
/// let* <aliases_before>
/// let <lines_name> = string.split(<text_var>, <sep>)
/// let* <aliases_after>
/// in each <lines_name-or-alias> <callback-or-alias>
/// ```
///
/// Returns `Some(Ok([SplitEachPrint {...}]))` when matched,
/// `Some(Err(UnsupportedForm))` when partially matched but not fully supported,
/// `None` when the pattern does not apply (fall through to general Let lowering).
fn try_lower_split_each(
    let_name: &str,
    value: &CompExpr,
    body: &CompExpr,
    aliases: &HashMap<String, AliasValue>,
) -> Option<Result<Vec<WasmBackendInstr>, LowerError>> {
    let (text_local, sep) = split_call_parts(value, aliases)?;
    let mut body_aliases = aliases.clone();
    let (effect, op) = find_split_each_callback(let_name, body, &mut body_aliases)?;

    // Current restriction: sep must be exactly 1 byte.
    let sep_bytes = sep.as_bytes().to_vec();
    if sep_bytes.len() != 1 {
        return Some(Err(LowerError::UnsupportedForm {
            node: format!(
                "SplitEachPrint: multi-byte separator '{}' is not yet supported",
                sep
            ),
        }));
    }

    Some(Ok(vec![WasmBackendInstr::SplitEachPrint {
        text_local,
        sep_bytes,
        effect,
        op,
    }]))
}

fn try_lower_split_get_print(
    let_name: &str,
    value: &CompExpr,
    body: &CompExpr,
    aliases: &HashMap<String, AliasValue>,
) -> Option<Result<Vec<WasmBackendInstr>, LowerError>> {
    let (text_local, sep) = split_call_parts(value, aliases)?;
    let mut body_aliases = aliases.clone();
    let (index, op) = find_split_get_print(let_name, body, &mut body_aliases)?;

    let sep_bytes = sep.as_bytes().to_vec();
    if sep_bytes.len() != 1 {
        return Some(Err(LowerError::UnsupportedForm {
            node: format!(
                "SplitGetPrint: multi-byte separator '{}' is not yet supported",
                sep
            ),
        }));
    }

    Some(Ok(vec![WasmBackendInstr::SplitGetPrint {
        text_local,
        sep_bytes,
        index,
        op,
    }]))
}

/// Try to match the fused graphemes-index-print pattern:
///
/// ```text
/// let <parts_name> = graphemes(<text_var>)
/// let <item_name> = list.get(<parts_name>, <N>)
/// in Print.op(<item_name>)
/// ```
///
/// Lowers to: `[LoadLocal(text), I64Const(encode_int(N)), Intrinsic(StringEachGraphemeState),
///              EffectOp(Print, op)]`
///
/// Returns `Some(Ok(...))` when matched,
/// `None` when the pattern does not apply (fall through to general Let lowering).
fn try_lower_graphemes_get_print(
    let_name: &str,
    value: &CompExpr,
    body: &CompExpr,
    aliases: &HashMap<String, AliasValue>,
) -> Option<Result<Vec<WasmBackendInstr>, LowerError>> {
    let text_local = graphemes_call_text(value, aliases)?;
    let mut body_aliases = aliases.clone();
    let (index, op) = find_graphemes_get_print(let_name, body, &mut body_aliases)?;

    let encoded_index = match encode_int(index) {
        Ok(v) => v,
        Err(e) => return Some(Err(e.into())),
    };

    Some(Ok(vec![
        WasmBackendInstr::LoadLocal {
            name: text_local.to_string(),
        },
        WasmBackendInstr::I64Const(encoded_index),
        WasmBackendInstr::Intrinsic {
            intrinsic: BackendIntrinsic::StringEachGraphemeState,
        },
        WasmBackendInstr::EffectOp {
            effect: "Print".to_string(),
            op,
        },
    ]))
}

/// Extract the text local from a `Call(graphemes, [Var(text)])` node.
fn graphemes_call_text<'a>(
    comp: &'a CompExpr,
    aliases: &'a HashMap<String, AliasValue>,
) -> Option<&'a str> {
    match comp {
        CompExpr::Call { callee, args }
            if (is_helper_global(callee, aliases, "string", "graphemes")
                || matches!(callee.as_ref(), ValueExpr::Var(name) if name == "graphemes"))
                && args.len() == 1 =>
        {
            resolve_local_name(&args[0], aliases)
        }
        _ => None,
    }
}

fn find_graphemes_get_print(
    parts_name: &str,
    comp: &CompExpr,
    aliases: &mut HashMap<String, AliasValue>,
) -> Option<(i64, String)> {
    match comp {
        CompExpr::Let {
            name: item_name,
            value,
            body,
            ..
        } => {
            let index = match value.as_ref() {
                CompExpr::Call { callee, args }
                    if is_helper_global(callee, aliases, "list", "get") && args.len() == 2 =>
                {
                    let list_name = resolve_local_name(&args[0], aliases)?;
                    if list_name != parts_name {
                        return None;
                    }
                    match &args[1] {
                        ValueExpr::IntLit(n) => *n,
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
                    let printed_name = resolve_local_name(&args[0], aliases)?;
                    if printed_name == item_name {
                        Some((index, op.clone()))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AliasValue {
    Var(String),
    Str(String),
    GlobalRef { module: String, name: String },
}

fn find_split_each_callback(
    lines_name: &str,
    comp: &CompExpr,
    aliases: &mut HashMap<String, AliasValue>,
) -> Option<(String, String)> {
    match comp {
        CompExpr::Let {
            name, value, body, ..
        } => {
            if let Some(alias) = alias_value_from_comp(value) {
                aliases.insert(name.clone(), alias);
                let result = find_split_each_callback(lines_name, body, aliases);
                aliases.remove(name);
                return result;
            }
            None
        }
        CompExpr::Call { callee, args } if is_each_callee(callee, aliases) && args.len() == 2 => {
            let list_name = resolve_local_name(&args[0], aliases)?;
            if list_name != lines_name {
                return None;
            }
            resolve_print_callback(&args[1], aliases)
        }
        _ => None,
    }
}

fn find_split_get_print(
    lines_name: &str,
    comp: &CompExpr,
    aliases: &mut HashMap<String, AliasValue>,
) -> Option<(SplitIndexOperand, String)> {
    match comp {
        CompExpr::Let {
            name: item_name,
            value,
            body,
            ..
        } => {
            let index = match value.as_ref() {
                CompExpr::Call { callee, args }
                    if is_helper_global(callee, aliases, "list", "get") && args.len() == 2 =>
                {
                    let list_name = resolve_local_name(&args[0], aliases)?;
                    if list_name != lines_name {
                        return None;
                    }
                    match &args[1] {
                        ValueExpr::IntLit(index) => SplitIndexOperand::Const(*index),
                        ValueExpr::Var(index_name) => SplitIndexOperand::Local(index_name.clone()),
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
                    let printed_name = resolve_local_name(&args[0], aliases)?;
                    if printed_name == item_name {
                        Some((index, op.clone()))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn alias_value_from_comp(comp: &CompExpr) -> Option<AliasValue> {
    match comp {
        CompExpr::Value(ValueExpr::Var(name)) => Some(AliasValue::Var(name.clone())),
        CompExpr::Value(ValueExpr::StrLit(text)) => Some(AliasValue::Str(text.clone())),
        CompExpr::Value(ValueExpr::GlobalRef { module, name }) => Some(AliasValue::GlobalRef {
            module: module.clone(),
            name: name.clone(),
        }),
        _ => None,
    }
}

fn split_call_parts(
    comp: &CompExpr,
    aliases: &HashMap<String, AliasValue>,
) -> Option<(String, String)> {
    match comp {
        CompExpr::Call { callee, args }
            if is_helper_global(callee, aliases, "string", "split") && args.len() == 2 =>
        {
            let text_local = resolve_local_name(&args[0], aliases)?.to_string();
            let sep = resolve_str_arg(&args[1], aliases)?.to_string();
            Some((text_local, sep))
        }
        _ => None,
    }
}

fn is_each_callee(callee: &ValueExpr, aliases: &HashMap<String, AliasValue>) -> bool {
    matches!(callee, ValueExpr::Var(name) if name == "each")
        || is_helper_global(callee, aliases, "list", "each")
}

fn is_helper_global(
    callee: &ValueExpr,
    aliases: &HashMap<String, AliasValue>,
    module: &str,
    name: &str,
) -> bool {
    match callee {
        ValueExpr::GlobalRef {
            module: callee_module,
            name: callee_name,
        } => callee_module == module && callee_name == name,
        ValueExpr::Var(var) => {
            resolve_global_ref(var, aliases).is_some_and(|(callee_module, callee_name)| {
                callee_module == module && callee_name == name
            })
        }
        _ => false,
    }
}

fn resolve_print_callback(
    callback: &ValueExpr,
    aliases: &HashMap<String, AliasValue>,
) -> Option<(String, String)> {
    match callback {
        ValueExpr::GlobalRef { module, name }
            if module == "Print" && (name == "print" || name == "println") =>
        {
            Some((module.clone(), name.clone()))
        }
        ValueExpr::Var(name) if name == "print" || name == "println" => {
            Some(("Print".to_string(), name.clone()))
        }
        ValueExpr::Var(name) => {
            if let Some(op) = resolve_var_alias(name, aliases)
                && (op == "print" || op == "println")
            {
                return Some(("Print".to_string(), op.to_string()));
            }
            let (module, op) = resolve_global_ref(name, aliases)?;
            if module == "Print" && (op == "print" || op == "println") {
                Some(("Print".to_string(), op.to_string()))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn resolve_local_name<'a>(
    value: &'a ValueExpr,
    aliases: &'a HashMap<String, AliasValue>,
) -> Option<&'a str> {
    match value {
        ValueExpr::Var(name) => resolve_var_alias(name, aliases),
        _ => None,
    }
}

fn resolve_var_alias<'a>(
    name: &'a str,
    aliases: &'a HashMap<String, AliasValue>,
) -> Option<&'a str> {
    match aliases.get(name) {
        Some(AliasValue::Var(next)) => resolve_var_alias(next.as_str(), aliases),
        Some(AliasValue::Str(_)) | Some(AliasValue::GlobalRef { .. }) => None,
        None => Some(name),
    }
}

fn resolve_str_arg<'a>(
    value: &'a ValueExpr,
    aliases: &'a HashMap<String, AliasValue>,
) -> Option<&'a str> {
    match value {
        ValueExpr::StrLit(text) => Some(text.as_str()),
        ValueExpr::Var(name) => resolve_str_alias(name, aliases),
        _ => None,
    }
}

fn resolve_str_alias<'a>(
    name: &'a str,
    aliases: &'a HashMap<String, AliasValue>,
) -> Option<&'a str> {
    match aliases.get(name) {
        Some(AliasValue::Str(text)) => Some(text.as_str()),
        Some(AliasValue::Var(next)) => resolve_str_alias(next.as_str(), aliases),
        Some(AliasValue::GlobalRef { .. }) | None => None,
    }
}

fn resolve_global_ref<'a>(
    name: &'a str,
    aliases: &'a HashMap<String, AliasValue>,
) -> Option<(&'a str, &'a str)> {
    match aliases.get(name) {
        Some(AliasValue::GlobalRef { module, name }) => Some((module.as_str(), name.as_str())),
        Some(AliasValue::Var(next)) => resolve_global_ref(next.as_str(), aliases),
        Some(AliasValue::Str(_)) | None => None,
    }
}

fn resolve_effect_call_target(
    callee: &ValueExpr,
    aliases: &HashMap<String, AliasValue>,
) -> Option<(String, String)> {
    match callee {
        ValueExpr::GlobalRef { module, name } if is_effect_pair(module.as_str(), name.as_str()) => {
            Some((module.clone(), name.clone()))
        }
        ValueExpr::Var(name) if name == "print" || name == "println" => {
            Some(("Print".to_string(), name.clone()))
        }
        ValueExpr::Var(name) if name == "read" || name == "read_line" => {
            Some(("Read".to_string(), name.clone()))
        }
        ValueExpr::Var(name) => {
            if let Some(op) = resolve_var_alias(name, aliases)
                && (op == "print" || op == "println")
            {
                return Some(("Print".to_string(), op.to_string()));
            }
            if let Some(op) = resolve_var_alias(name, aliases)
                && (op == "read" || op == "read_line")
            {
                return Some(("Read".to_string(), op.to_string()));
            }
            let (module, op) = resolve_global_ref(name, aliases)?;
            if is_effect_pair(module, op) {
                Some((module.to_string(), op.to_string()))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn resolve_intrinsic_call_target(
    callee: &ValueExpr,
    aliases: &HashMap<String, AliasValue>,
    arg_count: usize,
) -> Option<BackendIntrinsic> {
    match callee {
        ValueExpr::GlobalRef { module, name } => {
            backend_intrinsic_for(module.as_str(), name.as_str())
        }
        ValueExpr::Var(name) => {
            if let Some(intrinsic) = backend_intrinsic_for_bare(name, arg_count) {
                return Some(intrinsic);
            }
            let (module, name) = resolve_global_ref(name, aliases)?;
            backend_intrinsic_for(module, name)
        }
        _ => None,
    }
}

fn backend_intrinsic_for(module: &str, name: &str) -> Option<BackendIntrinsic> {
    match (module, name) {
        ("string", "split") => Some(BackendIntrinsic::StringSplit),
        ("list", "get") => Some(BackendIntrinsic::ListGet),
        ("string", "length") => Some(BackendIntrinsic::StringLength),
        _ => None,
    }
}

fn backend_intrinsic_for_bare(name: &str, arg_count: usize) -> Option<BackendIntrinsic> {
    match name {
        "__goby_string_each_grapheme" => match arg_count {
            1 => Some(BackendIntrinsic::StringEachGraphemeCount),
            2 => Some(BackendIntrinsic::StringEachGraphemeState),
            _ => None,
        },
        "__goby_list_push_string" => Some(BackendIntrinsic::ListPushString),
        "__goby_string_length" => Some(BackendIntrinsic::StringLength),
        _ => None,
    }
}

fn is_effect_pair(module: &str, op: &str) -> bool {
    matches!(
        (module, op),
        ("Print", "print") | ("Print", "println") | ("Read", "read") | ("Read", "read_line")
    )
}

fn instrs_load_local(instrs: &[WasmBackendInstr], name: &str) -> bool {
    instrs.iter().any(|instr| {
        matches!(
            instr,
            WasmBackendInstr::LoadLocal { name: local_name } if local_name == name
        )
    })
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
    fn lower_local_print_alias_call_emits_effect_op() {
        let comp = CompExpr::Let {
            name: "printer".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Value(ValueExpr::Var("print".to_string()))),
            body: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::Var("printer".to_string())),
                args: vec![ValueExpr::Var("text".to_string())],
            }),
        };
        let instrs = lower_comp(&comp).expect("local print alias call should lower");
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal {
                    name: "text".to_string()
                },
                I::EffectOp {
                    effect: "Print".to_string(),
                    op: "print".to_string(),
                },
            ]
        );
    }

    #[test]
    fn lower_int_lit_zero() {
        let v = ValueExpr::IntLit(0);
        let instrs = lower_value(&v).expect("should encode 0");
        assert_eq!(instrs, vec![I::I64Const(encode_int(0).unwrap())]);
    }

    #[test]
    fn lower_str_lit_emits_static_string_push() {
        let v = ValueExpr::StrLit("x".to_string());
        let instrs = lower_value(&v).expect("static string lowering should succeed");
        assert_eq!(
            instrs,
            vec![I::PushStaticString {
                text: "x".to_string()
            }]
        );
    }

    #[test]
    fn lower_string_length_call_emits_explicit_intrinsic() {
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::GlobalRef {
                module: "string".to_string(),
                name: "length".to_string(),
            }),
            args: vec![ValueExpr::StrLit("hello".to_string())],
        };
        let instrs = lower_comp(&comp).expect("string.length should lower");
        assert_eq!(
            instrs,
            vec![
                I::PushStaticString {
                    text: "hello".to_string()
                },
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::StringLength
                },
            ]
        );
    }

    #[test]
    fn lower_runtime_intrinsic_bare_name_emits_explicit_intrinsic() {
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("__goby_string_each_grapheme".to_string())),
            args: vec![ValueExpr::Var("text".to_string())],
        };
        let instrs = lower_comp(&comp).expect("runtime intrinsic should lower");
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal {
                    name: "text".to_string()
                },
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::StringEachGraphemeCount
                },
            ]
        );
    }

    #[test]
    fn lower_runtime_intrinsic_bare_name_state_mode_emits_explicit_intrinsic() {
        let comp = CompExpr::Call {
            callee: Box::new(ValueExpr::Var("__goby_string_each_grapheme".to_string())),
            args: vec![
                ValueExpr::Var("text".to_string()),
                ValueExpr::Var("state".to_string()),
            ],
        };
        let instrs = lower_comp(&comp).expect("runtime intrinsic state mode should lower");
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal {
                    name: "text".to_string()
                },
                I::LoadLocal {
                    name: "state".to_string()
                },
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::StringEachGraphemeState
                },
            ]
        );
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
    fn lower_split_each_with_bare_println_callback_emits_fused_instr() {
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
                callee: Box::new(ValueExpr::GlobalRef {
                    module: "list".to_string(),
                    name: "each".to_string(),
                }),
                args: vec![
                    ValueExpr::Var("lines".to_string()),
                    ValueExpr::Var("println".to_string()),
                ],
            }),
        };
        let instrs = lower_comp(&comp).expect("fused bare-callback pattern should succeed");
        assert!(
            matches!(&instrs[0], I::SplitEachPrint { effect, op, .. }
                if effect == "Print" && op == "println"),
            "expected bare println callback to lower to SplitEachPrint, got {:?}",
            instrs
        );
    }

    #[test]
    fn lower_split_each_with_prebound_callback_alias_emits_fused_instr() {
        let comp = CompExpr::Let {
            name: "printer".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Value(ValueExpr::Var("println".to_string()))),
            body: Box::new(CompExpr::Let {
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
                    callee: Box::new(ValueExpr::GlobalRef {
                        module: "list".to_string(),
                        name: "each".to_string(),
                    }),
                    args: vec![
                        ValueExpr::Var("lines".to_string()),
                        ValueExpr::Var("printer".to_string()),
                    ],
                }),
            }),
        };
        let instrs = lower_comp(&comp).expect("prebound callback alias pattern should succeed");
        assert!(
            matches!(&instrs[0], I::SplitEachPrint { effect, op, .. }
                if effect == "Print" && op == "println"),
            "expected prebound callback alias to lower to SplitEachPrint, got {:?}",
            instrs
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

    #[test]
    fn lower_graphemes_index_emits_state_intrinsic() {
        use crate::gen_lower::backend_ir::BackendIntrinsic;
        use crate::gen_lower::value::encode_int;
        // let parts = graphemes(text); let item = list.get(parts, 1); Print.println(item)
        let comp = CompExpr::Let {
            name: "parts".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::GlobalRef {
                    module: "string".to_string(),
                    name: "graphemes".to_string(),
                }),
                args: vec![ValueExpr::Var("text".to_string())],
            }),
            body: Box::new(CompExpr::Let {
                name: "item".to_string(),
                ty: goby_core::ir::IrType::Unknown,
                value: Box::new(CompExpr::Call {
                    callee: Box::new(ValueExpr::GlobalRef {
                        module: "list".to_string(),
                        name: "get".to_string(),
                    }),
                    args: vec![
                        ValueExpr::Var("parts".to_string()),
                        ValueExpr::IntLit(1),
                    ],
                }),
                body: Box::new(CompExpr::PerformEffect {
                    effect: "Print".to_string(),
                    op: "println".to_string(),
                    args: vec![ValueExpr::Var("item".to_string())],
                }),
            }),
        };
        let instrs = lower_comp(&comp).expect("fused graphemes-index pattern should lower");
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal {
                    name: "text".to_string(),
                },
                I::I64Const(encode_int(1).expect("1 is in range")),
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::StringEachGraphemeState,
                },
                I::EffectOp {
                    effect: "Print".to_string(),
                    op: "println".to_string(),
                },
            ]
        );
    }

    #[test]
    fn lower_graphemes_index_with_bare_callee_emits_state_intrinsic() {
        use crate::gen_lower::backend_ir::BackendIntrinsic;
        use crate::gen_lower::value::encode_int;
        // Bare-name form: let parts = graphemes text; ...
        let comp = CompExpr::Let {
            name: "parts".to_string(),
            ty: goby_core::ir::IrType::Unknown,
            value: Box::new(CompExpr::Call {
                callee: Box::new(ValueExpr::Var("graphemes".to_string())),
                args: vec![ValueExpr::Var("text".to_string())],
            }),
            body: Box::new(CompExpr::Let {
                name: "item".to_string(),
                ty: goby_core::ir::IrType::Unknown,
                value: Box::new(CompExpr::Call {
                    callee: Box::new(ValueExpr::GlobalRef {
                        module: "list".to_string(),
                        name: "get".to_string(),
                    }),
                    args: vec![
                        ValueExpr::Var("parts".to_string()),
                        ValueExpr::IntLit(1),
                    ],
                }),
                body: Box::new(CompExpr::PerformEffect {
                    effect: "Print".to_string(),
                    op: "println".to_string(),
                    args: vec![ValueExpr::Var("item".to_string())],
                }),
            }),
        };
        let instrs = lower_comp(&comp).expect("bare-callee graphemes-index pattern should lower");
        assert_eq!(
            instrs,
            vec![
                I::LoadLocal {
                    name: "text".to_string(),
                },
                I::I64Const(encode_int(1).expect("1 is in range")),
                I::Intrinsic {
                    intrinsic: BackendIntrinsic::StringEachGraphemeState,
                },
                I::EffectOp {
                    effect: "Print".to_string(),
                    op: "println".to_string(),
                },
            ]
        );
    }
}
