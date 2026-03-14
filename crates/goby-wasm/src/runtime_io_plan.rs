use goby_core::ast::InterpolatedPart;
use goby_core::{Expr, Module, Stmt};

use crate::CodegenError;
use crate::backend::WasmProgramBuilder;
use crate::layout::MemoryLayout;
use crate::runtime_flow::DirectCallHead;
use crate::runtime_support::{flatten_direct_call, module_has_selective_import_symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InputReadMode {
    ReadAll,
    ReadLine,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum OutputReadMode {
    Print,
    Println,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StaticPrintSuffix {
    pub(crate) text: String,
    pub(crate) output_mode: OutputReadMode,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum RuntimeIoPlan {
    Echo {
        input_mode: InputReadMode,
        output_mode: OutputReadMode,
        suffix_prints: Vec<StaticPrintSuffix>,
    },
    SplitLinesEach {
        output_mode: OutputReadMode,
        suffix_prints: Vec<StaticPrintSuffix>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum RuntimeIoClassification {
    DynamicWasiIo(RuntimeIoPlan),
    StaticOutput(String),
    InterpreterBridge,
    Unsupported,
    NotRuntimeIo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeIoExecutionKind {
    DynamicWasiIo,
    StaticOutput,
    InterpreterBridge,
    Unsupported,
    NotRuntimeIo,
}

impl RuntimeIoPlan {
    pub(crate) fn emit_wasm(self) -> Result<Vec<u8>, CodegenError> {
        let builder = WasmProgramBuilder::new(MemoryLayout::default());
        match self {
            RuntimeIoPlan::Echo {
                input_mode,
                output_mode,
                suffix_prints,
            } => {
                let append_newline = matches!(output_mode, OutputReadMode::Println);
                match input_mode {
                    InputReadMode::ReadAll => {
                        builder.emit_read_all_to_stdout_module(append_newline, &suffix_prints)
                    }
                    InputReadMode::ReadLine => {
                        builder.emit_read_line_to_stdout_module(append_newline, &suffix_prints)
                    }
                }
            }
            RuntimeIoPlan::SplitLinesEach {
                output_mode,
                suffix_prints,
            } => builder.emit_read_split_lines_each_print_module(
                matches!(output_mode, OutputReadMode::Println),
                &suffix_prints,
            ),
        }
    }
}

impl RuntimeIoClassification {
    pub(crate) fn compile_module_wasm_or_error(self) -> Result<Option<Vec<u8>>, CodegenError> {
        match self {
            RuntimeIoClassification::DynamicWasiIo(plan) => plan.emit_wasm().map(Some),
            RuntimeIoClassification::StaticOutput(text) => {
                crate::print_codegen::compile_print_module(&text).map(Some)
            }
            RuntimeIoClassification::InterpreterBridge => Err(CodegenError {
                message:
                    "compile-time fallback cannot consume stdin; use the runtime stdin execution path"
                        .to_string(),
            }),
            // TODO(F3b): return a user-facing error here once classify_runtime_io
            // assigns Unsupported to genuinely unsupported programs, so callers get a
            // clear diagnostic instead of falling through to resolve_main_runtime_output_for_compile.
            RuntimeIoClassification::Unsupported => Ok(None),
            RuntimeIoClassification::NotRuntimeIo => Ok(None),
        }
    }

    pub(crate) fn require_interpreter_bridge_stdin(self) -> Result<(), CodegenError> {
        match self {
            RuntimeIoClassification::InterpreterBridge => Ok(()),
            RuntimeIoClassification::DynamicWasiIo(_) => Err(CodegenError {
                message:
                    "runtime stdin execution path is only for interpreter-bridge programs; compile this program to Wasm instead"
                        .to_string(),
            }),
            RuntimeIoClassification::StaticOutput(_) => Err(CodegenError {
                message:
                    "runtime stdin execution path is only for interpreter-bridge programs; this program has static output and should be compiled to Wasm instead"
                        .to_string(),
            }),
            // TODO(F3b): once Unsupported is assigned by classify_runtime_io, give a
            // distinct message so callers can distinguish it from NotRuntimeIo.
            RuntimeIoClassification::Unsupported => Err(CodegenError {
                message: "runtime stdin execution path is only for interpreter-bridge programs"
                    .to_string(),
            }),
            RuntimeIoClassification::NotRuntimeIo => Err(CodegenError {
                message: "runtime stdin execution path is only for interpreter-bridge programs"
                    .to_string(),
            }),
        }
    }
}

/// Classify a `main` body's runtime I/O posture into one of five explicit categories.
///
/// The classification determines which execution path `compile_module` and the CLI
/// will choose.  The boundaries are:
///
/// | Variant | Condition |
/// |---------|-----------|
/// | `DynamicWasiIo(plan)` | Body contains `Read.read`/`Read.read_line` usage **and** matches a known [`RuntimeIoPlan`] shape that can be lowered to a WASI Wasm module. |
/// | `StaticOutput(text)` | Body contains **no** runtime-read calls **and** every statement is a direct `print`/`println` with a string-literal argument.  The output is statically known at compile time. |
/// | `InterpreterBridge` | Body contains runtime-read calls **but** no known `RuntimeIoPlan` shape matches.  The program is executed at runtime via the interpreter bridge (temporary migration mode). |
/// | `Unsupported` | Reserved for F3b: runtime-read programs that neither match a `DynamicWasiIo` plan nor are expected to be executed via the interpreter bridge. Currently not assigned by this function. |
/// | `NotRuntimeIo` | Body contains **no** runtime-read calls and is not a `StaticOutput` program (e.g. complex static evaluation via local bindings, arithmetic, etc.).  Falls through to `resolve_main_runtime_output_for_compile`. |
///
/// # Ordering
///
/// The function checks in priority order: `DynamicWasiIo` → `InterpreterBridge` →
/// `StaticOutput` → `NotRuntimeIo`.  `Unsupported` is not yet assigned here; it will
/// be wired in during F3b once the bridge surface is explicitly bounded.
///
/// # Stopping rule (F3a)
///
/// All runtime-I/O AST-pattern matching lives in this module — **not** in `lib.rs` or
/// any other call site.  When adding support for a new runtime-I/O program shape:
///
/// 1. Extend [`RuntimeIoPlan`] with a new variant (or extend an existing plan variant).
/// 2. Add or extend a lowering arm in [`RuntimeIoPlan::emit_wasm`].
/// 3. Add a detection branch in [`plan_runtime_io`] (called from this function).
///    Note: `StaticOutput` detection is handled by [`plan_static_output`] (also called
///    directly from `classify_runtime_io`), not through `plan_runtime_io`.  New
///    static-output shapes should extend `plan_static_output` instead.
///
/// Do **not** add new AST-pattern conditionals anywhere outside this module —
/// not in `compile_module`, `execute_module_with_stdin`, the CLI, or any other caller.
/// Those entry points must stay policy-free; all shape decisions belong here.
pub(crate) fn classify_runtime_io(
    module: &Module,
    parsed_body: Option<&[Stmt]>,
) -> RuntimeIoClassification {
    let Some(stmts) = parsed_body else {
        return RuntimeIoClassification::NotRuntimeIo;
    };
    if let Some(plan) = plan_runtime_io(module, stmts) {
        return RuntimeIoClassification::DynamicWasiIo(plan);
    }
    if stmts.iter().any(stmt_contains_runtime_read) {
        return RuntimeIoClassification::InterpreterBridge;
    }
    if let Some(text) = plan_static_output(stmts) {
        return RuntimeIoClassification::StaticOutput(text);
    }
    RuntimeIoClassification::NotRuntimeIo
}

/// Detect programs where every statement is a direct `print`/`println` call with a
/// string-literal argument.  Returns the concatenated output string (with newlines
/// appended for `println` calls).  Returns `None` if the body contains anything that
/// cannot be determined purely by AST shape.
///
/// Scope: intentionally limited to `StringLit` arguments only.  Programs that call
/// `print` with non-literal arguments (variables, integer literals, arithmetic, etc.)
/// return `None` and fall through to `resolve_main_runtime_output_for_compile` for
/// interpreter-based static evaluation.
fn plan_static_output(stmts: &[Stmt]) -> Option<String> {
    if stmts.is_empty() {
        return None;
    }
    let mut output = String::new();
    for stmt in stmts {
        let Stmt::Expr(Expr::Call { callee, arg }) = stmt else {
            return None;
        };
        let Expr::Var(name) = callee.as_ref() else {
            return None;
        };
        let Expr::StringLit(text) = arg.as_ref() else {
            return None;
        };
        match name.as_str() {
            "print" => output.push_str(text),
            "println" => {
                output.push_str(text);
                output.push('\n');
            }
            _ => return None,
        }
    }
    Some(output)
}

pub(crate) fn classify_runtime_io_main(
    module: &Module,
) -> Result<RuntimeIoClassification, CodegenError> {
    let main = module
        .declarations
        .iter()
        .find(|d| d.name == "main")
        .ok_or_else(|| CodegenError {
            message: "Wasm codegen requires a `main` declaration".to_string(),
        })?;
    Ok(classify_runtime_io(module, main.parsed_body.as_deref()))
}

pub fn runtime_io_execution_kind(module: &Module) -> Result<RuntimeIoExecutionKind, CodegenError> {
    Ok(match classify_runtime_io_main(module)? {
        RuntimeIoClassification::DynamicWasiIo(_) => RuntimeIoExecutionKind::DynamicWasiIo,
        RuntimeIoClassification::StaticOutput(_) => RuntimeIoExecutionKind::StaticOutput,
        RuntimeIoClassification::InterpreterBridge => RuntimeIoExecutionKind::InterpreterBridge,
        RuntimeIoClassification::Unsupported => RuntimeIoExecutionKind::Unsupported,
        RuntimeIoClassification::NotRuntimeIo => RuntimeIoExecutionKind::NotRuntimeIo,
    })
}

fn plan_runtime_io(module: &Module, stmts: &[Stmt]) -> Option<RuntimeIoPlan> {
    if let Some(plan) = plan_echo_runtime_io(stmts) {
        return Some(plan);
    }
    if let Some((output_mode, suffix_prints)) = split_lines_each_plan(module, stmts) {
        return Some(RuntimeIoPlan::SplitLinesEach {
            output_mode,
            suffix_prints,
        });
    }
    None
}

fn plan_echo_runtime_io(stmts: &[Stmt]) -> Option<RuntimeIoPlan> {
    let (echo_stmts, suffix_prints) = split_trailing_static_print_suffixes(stmts)?;
    match echo_stmts {
        [Stmt::Expr(expr)] => {
            output_read_mode(expr).map(|(input_mode, output_mode)| RuntimeIoPlan::Echo {
                input_mode,
                output_mode,
                suffix_prints,
            })
        }
        [
            read_stmt,
            middle @ ..,
            Stmt::Expr(Expr::Call { callee, arg }),
        ] => {
            let (source_name, input_mode) = read_binding_mode(read_stmt)?;
            let printed_name = resolve_alias_chain_terminal_name(middle, source_name)?;
            plan_bound_echo_shape(input_mode, printed_name, callee, arg, middle, suffix_prints)
        }
        _ => None,
    }
}

fn plan_bound_echo_shape(
    input_mode: InputReadMode,
    printed_name: &str,
    callee: &Expr,
    arg: &Expr,
    scope_stmts: &[Stmt],
    suffix_prints: Vec<StaticPrintSuffix>,
) -> Option<RuntimeIoPlan> {
    if !matches!(arg, Expr::Var(var_name) if var_name == printed_name) {
        return None;
    }
    match callee_output_mode(callee, scope_stmts)? {
        "print" => Some(RuntimeIoPlan::Echo {
            input_mode,
            output_mode: OutputReadMode::Print,
            suffix_prints,
        }),
        "println" => Some(RuntimeIoPlan::Echo {
            input_mode,
            output_mode: OutputReadMode::Println,
            suffix_prints,
        }),
        _ => None,
    }
}

fn callee_output_mode<'a>(callee: &'a Expr, scope_stmts: &'a [Stmt]) -> Option<&'a str> {
    let Expr::Var(output_name) = callee else {
        return None;
    };
    let resolved_name =
        resolve_alias_chain_source_name(scope_stmts, output_name).unwrap_or(output_name);
    matches!(resolved_name, "print" | "println").then_some(resolved_name)
}

fn stmt_contains_runtime_read(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Binding { value, .. }
        | Stmt::MutBinding { value, .. }
        | Stmt::Assign { value, .. } => expr_contains_runtime_read(value),
        Stmt::Expr(expr) => expr_contains_runtime_read(expr),
    }
}

fn expr_contains_runtime_read(expr: &Expr) -> bool {
    match expr {
        Expr::Call { callee, arg } => {
            is_read_all_expr(expr)
                || is_read_line_expr(expr)
                || expr_contains_runtime_read(callee)
                || expr_contains_runtime_read(arg)
        }
        Expr::InterpolatedString(parts) => parts.iter().any(|part| match part {
            goby_core::ast::InterpolatedPart::Text(_) => false,
            goby_core::ast::InterpolatedPart::Expr(expr) => expr_contains_runtime_read(expr),
        }),
        Expr::ListLit { elements, spread } => {
            elements.iter().any(expr_contains_runtime_read)
                || spread.as_deref().is_some_and(expr_contains_runtime_read)
        }
        Expr::TupleLit(items) => items.iter().any(expr_contains_runtime_read),
        Expr::RecordConstruct { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_contains_runtime_read(value)),
        Expr::BinOp { left, right, .. } => {
            expr_contains_runtime_read(left) || expr_contains_runtime_read(right)
        }
        Expr::MethodCall { args, .. } => args.iter().any(expr_contains_runtime_read),
        Expr::Pipeline { value, .. } => expr_contains_runtime_read(value),
        Expr::Lambda { body, .. } => expr_contains_runtime_read(body),
        Expr::Handler { clauses } => clauses.iter().any(|clause| {
            clause
                .parsed_body
                .as_deref()
                .is_some_and(|body| body.iter().any(stmt_contains_runtime_read))
        }),
        Expr::With { handler, body } => {
            expr_contains_runtime_read(handler) || body.iter().any(stmt_contains_runtime_read)
        }
        Expr::Resume { value } => expr_contains_runtime_read(value),
        Expr::Block(stmts) => stmts.iter().any(stmt_contains_runtime_read),
        Expr::Case { scrutinee, arms } => {
            expr_contains_runtime_read(scrutinee)
                || arms.iter().any(|arm| expr_contains_runtime_read(&arm.body))
        }
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            expr_contains_runtime_read(condition)
                || expr_contains_runtime_read(then_expr)
                || expr_contains_runtime_read(else_expr)
        }
        Expr::IntLit(_)
        | Expr::BoolLit(_)
        | Expr::StringLit(_)
        | Expr::Var(_)
        | Expr::Qualified { .. } => false,
    }
}

fn is_read_all_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Call { callee, arg } if arg.is_unit_value() => match callee.as_ref() {
            Expr::Var(name) => name == "read",
            Expr::Qualified { receiver, member } => receiver == "Read" && member == "read",
            _ => false,
        },
        _ => false,
    }
}

fn is_read_line_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Call { callee, arg } if arg.is_unit_value() => match callee.as_ref() {
            Expr::Var(name) => name == "read_line",
            Expr::Qualified { receiver, member } => receiver == "Read" && member == "read_line",
            _ => false,
        },
        _ => false,
    }
}

fn output_read_mode(expr: &Expr) -> Option<(InputReadMode, OutputReadMode)> {
    let Expr::Call { callee, arg } = expr else {
        return None;
    };
    let Expr::Var(name) = callee.as_ref() else {
        return None;
    };
    let input_mode = if is_read_all_expr(arg) {
        InputReadMode::ReadAll
    } else if is_read_line_expr(arg) {
        InputReadMode::ReadLine
    } else {
        return None;
    };
    match name.as_str() {
        "print" => Some((input_mode, OutputReadMode::Print)),
        "println" => Some((input_mode, OutputReadMode::Println)),
        _ => None,
    }
}

fn imported_head_matches_symbol(
    module: &Module,
    head: &DirectCallHead,
    module_path: &str,
    symbol: &str,
) -> bool {
    match head {
        DirectCallHead::Bare(name) if name == symbol => {
            module_has_selective_import_symbol(module, module_path, symbol)
        }
        DirectCallHead::Qualified { receiver, member } if member == symbol => {
            module.imports.iter().any(|import| {
                if import.module_path != module_path {
                    return false;
                }
                match &import.kind {
                    goby_core::ImportKind::Plain => import
                        .module_path
                        .rsplit('/')
                        .next()
                        .is_some_and(|qualifier| qualifier == receiver),
                    goby_core::ImportKind::Alias(alias) => alias == receiver,
                    goby_core::ImportKind::Selective(_) => false,
                }
            })
        }
        _ => false,
    }
}

fn stmt_binding_parts(stmt: &Stmt) -> Option<(&str, &Expr)> {
    match stmt {
        Stmt::Binding { name, value } | Stmt::MutBinding { name, value } => Some((name, value)),
        _ => None,
    }
}

fn read_binding_mode(stmt: &Stmt) -> Option<(&str, InputReadMode)> {
    let (name, value) = stmt_binding_parts(stmt)?;
    if is_read_all_expr(value) {
        Some((name, InputReadMode::ReadAll))
    } else if is_read_line_expr(value) {
        Some((name, InputReadMode::ReadLine))
    } else {
        None
    }
}

fn alias_binding_name<'a>(stmt: &'a Stmt, source_name: &str) -> Option<&'a str> {
    let (alias_name, alias_value) = stmt_binding_parts(stmt)?;
    if matches!(alias_value, Expr::Var(var_name) if var_name == source_name) {
        Some(alias_name)
    } else {
        None
    }
}

fn resolve_alias_chain_terminal_name<'a>(
    stmts: &'a [Stmt],
    source_name: &'a str,
) -> Option<&'a str> {
    let mut current_name = source_name;
    for stmt in stmts {
        match stmt {
            Stmt::Binding { .. } | Stmt::MutBinding { .. } => {
                if let Some(next_name) = alias_binding_name(stmt, current_name) {
                    current_name = next_name;
                }
            }
            _ => return None,
        }
    }
    Some(current_name)
}

fn resolve_alias_chain_source_name<'a>(stmts: &'a [Stmt], target_name: &'a str) -> Option<&'a str> {
    let mut current_name = target_name;
    for stmt in stmts.iter().rev() {
        match stmt {
            Stmt::Binding { .. } | Stmt::MutBinding { .. } => {
                let Some((alias_name, alias_value)) = stmt_binding_parts(stmt) else {
                    continue;
                };
                if alias_name == current_name
                    && let Expr::Var(source_name) = alias_value
                {
                    current_name = source_name;
                }
            }
            _ => return None,
        }
    }
    Some(current_name)
}

fn binding_value_by_name<'a>(stmts: &'a [Stmt], target_name: &str) -> Option<&'a Expr> {
    stmts.iter().rev().find_map(|stmt| {
        let (name, value) = stmt_binding_parts(stmt)?;
        (name == target_name).then_some(value)
    })
}

fn name_resolves_to_newline_literal(stmts: &[Stmt], target_name: &str) -> bool {
    let Some(root_name) = resolve_alias_chain_source_name(stmts, target_name) else {
        return false;
    };
    matches!(
        binding_value_by_name(stmts, root_name),
        Some(Expr::StringLit(delim)) if delim == "\n"
    )
}

fn split_lines_binding_name<'a>(
    module: &Module,
    pre_split_stmts: &[Stmt],
    stmt: &'a Stmt,
    text_name: &str,
) -> Option<&'a str> {
    let (lines_name, split_value) = stmt_binding_parts(stmt)?;
    let Some((split_head, split_args)) = flatten_direct_call(split_value) else {
        return None;
    };
    if !imported_head_matches_symbol(module, &split_head, "goby/string", "split")
        || split_args.len() != 2
        || !expr_resolves_to_name(split_args[0], pre_split_stmts, text_name)
        || !expr_is_newline_delimiter(split_args[1], pre_split_stmts)
    {
        return None;
    }
    Some(lines_name)
}

fn expr_resolves_to_name(expr: &Expr, stmts: &[Stmt], expected_name: &str) -> bool {
    match expr {
        Expr::Var(name) => resolve_alias_chain_source_name(stmts, name) == Some(expected_name),
        _ => false,
    }
}

fn expr_is_newline_delimiter(expr: &Expr, stmts: &[Stmt]) -> bool {
    matches!(expr, Expr::StringLit(delim) if delim == "\n")
        || matches!(expr, Expr::Var(name) if name_resolves_to_newline_literal(stmts, name))
}

fn split_lines_each_output_mode_from_callback(
    module: &Module,
    callback_scope_stmts: &[Stmt],
    lines_name: &str,
    each_expr: &Expr,
) -> Option<OutputReadMode> {
    let Some((each_head, each_args)) = flatten_direct_call(each_expr) else {
        return None;
    };
    if !imported_head_matches_symbol(module, &each_head, "goby/list", "each")
        || each_args.len() != 2
        || !matches!(each_args[0], Expr::Var(name) if name == lines_name)
    {
        return None;
    }

    match each_args[1] {
        Expr::Var(name) => {
            callback_output_mode_name(resolve_alias_chain_source_name(callback_scope_stmts, name)?)
        }
        Expr::Lambda { param, body } => match body.as_ref() {
            Expr::Call { callee, arg } if callback_arg_matches_line_passthrough(arg, param) => {
                match callee.as_ref() {
                    Expr::Var(name) => callback_output_mode_name(resolve_alias_chain_source_name(
                        callback_scope_stmts,
                        name,
                    )?),
                    _ => None,
                }
            }
            _ => None,
        },
        _ => None,
    }
}

fn callback_arg_matches_line_passthrough(arg: &Expr, param: &str) -> bool {
    matches!(arg, Expr::Var(name) if name == param)
        || matches_passthrough_interpolated_string(arg, param)
}

fn matches_passthrough_interpolated_string(arg: &Expr, param: &str) -> bool {
    let Expr::InterpolatedString(parts) = arg else {
        return false;
    };
    let mut saw_param_expr = false;
    for part in parts {
        match part {
            InterpolatedPart::Text(text) if text.is_empty() => {}
            InterpolatedPart::Expr(expr)
                if !saw_param_expr && matches!(expr.as_ref(), Expr::Var(name) if name == param) =>
            {
                saw_param_expr = true;
            }
            _ => return false,
        }
    }
    saw_param_expr
}

fn callback_output_mode_name(name: &str) -> Option<OutputReadMode> {
    match name {
        "print" => Some(OutputReadMode::Print),
        "println" => Some(OutputReadMode::Println),
        _ => None,
    }
}

fn static_print_suffix(stmt: &Stmt) -> Option<StaticPrintSuffix> {
    let Stmt::Expr(Expr::Call { callee, arg }) = stmt else {
        return None;
    };
    let Expr::Var(name) = callee.as_ref() else {
        return None;
    };
    let output_mode = callback_output_mode_name(name)?;
    let Expr::StringLit(text) = arg.as_ref() else {
        return None;
    };
    Some(StaticPrintSuffix {
        text: text.clone(),
        output_mode,
    })
}

fn collect_static_print_suffixes(stmts: &[Stmt]) -> Option<Vec<StaticPrintSuffix>> {
    stmts.iter().map(static_print_suffix).collect()
}

fn split_trailing_static_print_suffixes(
    stmts: &[Stmt],
) -> Option<(&[Stmt], Vec<StaticPrintSuffix>)> {
    if stmts.is_empty() {
        return None;
    }
    let suffix_len = stmts
        .iter()
        .rev()
        .take_while(|stmt| static_print_suffix(stmt).is_some())
        .count();
    let split_index = stmts.len().checked_sub(suffix_len)?;
    let (prefix, suffix_stmts) = stmts.split_at(split_index);
    if prefix.is_empty() {
        return None;
    }
    Some((prefix, collect_static_print_suffixes(suffix_stmts)?))
}

fn split_lines_each_plan(
    module: &Module,
    stmts: &[Stmt],
) -> Option<(OutputReadMode, Vec<StaticPrintSuffix>)> {
    let [read_stmt, rest @ ..] = stmts else {
        return None;
    };
    let Some((text_name, InputReadMode::ReadAll)) = read_binding_mode(read_stmt) else {
        return None;
    };

    for (each_index, each_stmt) in rest.iter().enumerate() {
        let Stmt::Expr(each_expr) = each_stmt else {
            continue;
        };
        let Some(suffix_prints) = collect_static_print_suffixes(&rest[each_index + 1..]) else {
            continue;
        };
        let pre_each_stmts = &rest[..each_index];

        for (split_index, split_stmt) in pre_each_stmts.iter().enumerate() {
            let pre_split_stmts = &pre_each_stmts[..split_index];
            let post_split_stmts = &pre_each_stmts[split_index + 1..];
            let Some(lines_name) =
                split_lines_binding_name(module, pre_split_stmts, split_stmt, text_name)
            else {
                continue;
            };
            let Some(iterated_name) =
                resolve_alias_chain_terminal_name(post_split_stmts, lines_name)
            else {
                continue;
            };
            if let Some(output_mode) = split_lines_each_output_mode_from_callback(
                module,
                pre_each_stmts,
                iterated_name,
                each_expr,
            ) {
                return Some((output_mode, suffix_prints));
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use goby_core::parse_module;

    use super::{
        InputReadMode, OutputReadMode, RuntimeIoClassification, RuntimeIoPlan, StaticPrintSuffix,
        classify_runtime_io,
    };

    fn main_stmts(source: &str) -> (goby_core::Module, Option<Vec<Stmt>>) {
        let module = parse_module(source).expect("parse should work");
        let body = module
            .declarations
            .iter()
            .find(|decl| decl.name == "main")
            .and_then(|decl| decl.parsed_body.clone());
        (module, body)
    }

    use goby_core::Stmt;

    #[test]
    fn plans_simple_read_echo() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print, Read
main =
  print (read())
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                input_mode: InputReadMode::ReadAll,
                output_mode: OutputReadMode::Print,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_bound_newline_split_each_println() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  each lines (|line| -> println(line))
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_split_each_with_static_print_suffixes() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  lines = split(text, "\n")
  each lines (|line| -> println(line))
  println "test"
  print "done"
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![
                    StaticPrintSuffix {
                        text: "test".to_string(),
                        output_mode: OutputReadMode::Println,
                    },
                    StaticPrintSuffix {
                        text: "done".to_string(),
                        output_mode: OutputReadMode::Print,
                    },
                ],
            })
        );
    }

    #[test]
    fn plans_split_each_with_delimiter_alias_chain() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  newline = "\n"
  delim = newline
  lines = split(text, delim)
  each lines println
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_bound_newline_split_each_println_with_forwarded_lines_binding() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  copied = lines
  each copied (|line| -> println(line))
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_split_each_named_println_callback() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  each lines println
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_split_each_named_print_callback() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  each lines print
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Print,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_split_each_interpolated_passthrough_println_callback() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  each lines (|line| -> println "${line}")
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_split_each_local_println_callback_alias() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  printer = println
  each lines printer
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_split_each_forwarded_local_print_callback_alias() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  printer = print
  writer = printer
  each lines writer
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Print,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_split_each_lambda_calling_local_println_alias() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  printer = println
  each lines (|line| -> printer line)
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_split_each_lambda_calling_forwarded_local_print_alias() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  printer = print
  writer = printer
  each lines (|line| -> writer line)
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Print,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_split_each_with_callback_alias_before_split() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  printer = println
  delim = "\n"
  lines = split(text, delim)
  each lines printer
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_read_echo_with_forwarded_local_binding() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  copied = text
  print copied
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                input_mode: InputReadMode::ReadAll,
                output_mode: OutputReadMode::Print,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_read_line_echo_with_forwarded_local_binding() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print, Read
main =
  line = read_line()
  copied = line
  println copied
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                input_mode: InputReadMode::ReadLine,
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_read_echo_with_static_print_suffixes() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print, Read
main =
  print (read())
  println "done"
  print "!"
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                input_mode: InputReadMode::ReadAll,
                output_mode: OutputReadMode::Print,
                suffix_prints: vec![
                    StaticPrintSuffix {
                        text: "done".to_string(),
                        output_mode: OutputReadMode::Println,
                    },
                    StaticPrintSuffix {
                        text: "!".to_string(),
                        output_mode: OutputReadMode::Print,
                    },
                ],
            })
        );
    }

    #[test]
    fn plans_bound_read_line_echo_with_static_print_suffixes() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print, Read
main =
  line = read_line()
  copied = line
  println copied
  print "done"
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                input_mode: InputReadMode::ReadLine,
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![StaticPrintSuffix {
                    text: "done".to_string(),
                    output_mode: OutputReadMode::Print,
                }],
            })
        );
    }

    #[test]
    fn plans_bound_read_echo_with_local_print_alias() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  printer = print
  copied = text
  printer copied
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                input_mode: InputReadMode::ReadAll,
                output_mode: OutputReadMode::Print,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn plans_bound_read_line_echo_with_forwarded_output_alias() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print, Read
main =
  line = read_line()
  printer = println
  writer = printer
  copied = line
  writer copied
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                input_mode: InputReadMode::ReadLine,
                output_mode: OutputReadMode::Println,
                suffix_prints: vec![],
            })
        );
    }

    #[test]
    fn does_not_plan_bridge_only_shape() {
        let (module, body) = main_stmts(
            r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  copied = lines
  forwarded = copied
  each forwarded (|line| -> println "${line}!")
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::InterpreterBridge
        );
    }

    #[test]
    fn non_read_program_with_var_arg_is_not_runtime_io() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print
main =
  msg = "hello"
  println msg
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::NotRuntimeIo
        );
    }

    #[test]
    fn pure_print_literal_is_static_output() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print
main =
  println "hello"
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::StaticOutput("hello\n".to_string())
        );
    }

    #[test]
    fn multiple_print_literals_are_static_output() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print
main =
  print "a"
  println "b"
  print "c"
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::StaticOutput("ab\nc".to_string())
        );
    }

    #[test]
    fn print_with_var_arg_is_not_static_output() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print
main =
  n = 42
  print n
"#,
        );
        // var arg is not a string literal, so not StaticOutput
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::NotRuntimeIo
        );
    }

    #[test]
    fn empty_string_print_is_static_output() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print
main =
  print ""
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::StaticOutput("".to_string())
        );
    }

    #[test]
    fn empty_string_println_is_static_output_with_newline() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print
main =
  println ""
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::StaticOutput("\n".to_string())
        );
    }

    #[test]
    // TODO(F3b): when F3b wires Unsupported assignment into classify_runtime_io,
    // compile_module_wasm_or_error for Unsupported should return a user-facing Err
    // rather than Ok(None). At that point this test assertion must be inverted to
    // assert!(result.is_err()).
    fn unsupported_classification_does_not_produce_wasm() {
        let result = RuntimeIoClassification::Unsupported.compile_module_wasm_or_error();
        assert_eq!(result, Ok(None));
    }

    #[test]
    // TODO(F3b): add an integration-level test in wasm_exports_and_smoke.rs that calls
    // execute_module_with_stdin on a program classified as Unsupported, once
    // classify_runtime_io actually assigns Unsupported to programs.
    fn unsupported_classification_rejects_interpreter_bridge_stdin() {
        let result = RuntimeIoClassification::Unsupported.require_interpreter_bridge_stdin();
        assert!(result.is_err());
    }
}
