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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RuntimeIoPlan {
    Echo {
        input_mode: InputReadMode,
        output_mode: OutputReadMode,
    },
    SplitLinesEach {
        output_mode: OutputReadMode,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RuntimeIoClassification {
    DynamicWasiIo(RuntimeIoPlan),
    InterpreterBridge,
    NotRuntimeIo,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeIoExecutionKind {
    DynamicWasiIo,
    InterpreterBridge,
    NotRuntimeIo,
}

impl RuntimeIoPlan {
    pub(crate) fn emit_wasm(self) -> Result<Vec<u8>, CodegenError> {
        let builder = WasmProgramBuilder::new(MemoryLayout::default());
        match self {
            RuntimeIoPlan::Echo {
                input_mode,
                output_mode,
            } => {
                let append_newline = matches!(output_mode, OutputReadMode::Println);
                match input_mode {
                    InputReadMode::ReadAll => {
                        builder.emit_read_all_to_stdout_module(append_newline)
                    }
                    InputReadMode::ReadLine => {
                        builder.emit_read_line_to_stdout_module(append_newline)
                    }
                }
            }
            RuntimeIoPlan::SplitLinesEach { output_mode } => builder
                .emit_read_split_lines_each_print_module(matches!(
                    output_mode,
                    OutputReadMode::Println
                )),
        }
    }
}

impl RuntimeIoClassification {
    pub(crate) fn compile_module_wasm_or_error(self) -> Result<Option<Vec<u8>>, CodegenError> {
        match self {
            RuntimeIoClassification::DynamicWasiIo(plan) => plan.emit_wasm().map(Some),
            RuntimeIoClassification::InterpreterBridge => Err(CodegenError {
                message:
                    "compile-time fallback cannot consume stdin; use the runtime stdin execution path"
                        .to_string(),
            }),
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
            RuntimeIoClassification::NotRuntimeIo => Err(CodegenError {
                message: "runtime stdin execution path is only for interpreter-bridge programs"
                    .to_string(),
            }),
        }
    }
}

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
    RuntimeIoClassification::NotRuntimeIo
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
        RuntimeIoClassification::InterpreterBridge => RuntimeIoExecutionKind::InterpreterBridge,
        RuntimeIoClassification::NotRuntimeIo => RuntimeIoExecutionKind::NotRuntimeIo,
    })
}

fn plan_runtime_io(module: &Module, stmts: &[Stmt]) -> Option<RuntimeIoPlan> {
    if let Some(plan) = plan_echo_runtime_io(stmts) {
        return Some(plan);
    }
    if let Some(output_mode) = split_lines_each_output_mode(module, stmts) {
        return Some(RuntimeIoPlan::SplitLinesEach { output_mode });
    }
    None
}

fn plan_echo_runtime_io(stmts: &[Stmt]) -> Option<RuntimeIoPlan> {
    match stmts {
        [Stmt::Expr(expr)] => {
            output_read_mode(expr).map(|(input_mode, output_mode)| RuntimeIoPlan::Echo {
                input_mode,
                output_mode,
            })
        }
        [
            read_stmt,
            middle @ ..,
            Stmt::Expr(Expr::Call { callee, arg }),
        ] => {
            let (source_name, input_mode) = read_binding_mode(read_stmt)?;
            let printed_name = resolve_alias_chain_terminal_name(middle, source_name)?;
            plan_bound_echo_shape(input_mode, printed_name, callee, arg)
        }
        _ => None,
    }
}

fn plan_bound_echo_shape(
    input_mode: InputReadMode,
    printed_name: &str,
    callee: &Expr,
    arg: &Expr,
) -> Option<RuntimeIoPlan> {
    if !matches!(arg, Expr::Var(var_name) if var_name == printed_name) {
        return None;
    }
    let Expr::Var(output_name) = callee else {
        return None;
    };
    match output_name.as_str() {
        "print" => Some(RuntimeIoPlan::Echo {
            input_mode,
            output_mode: OutputReadMode::Print,
        }),
        "println" => Some(RuntimeIoPlan::Echo {
            input_mode,
            output_mode: OutputReadMode::Println,
        }),
        _ => None,
    }
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
        current_name = alias_binding_name(stmt, current_name)?;
    }
    Some(current_name)
}

fn newline_delimiter_binding_name(stmt: &Stmt) -> Option<&str> {
    let (name, value) = stmt_binding_parts(stmt)?;
    if matches!(value, Expr::StringLit(delim) if delim == "\n") {
        Some(name)
    } else {
        None
    }
}

fn split_lines_binding_name<'a>(
    module: &Module,
    stmt: &'a Stmt,
    text_name: &str,
    delimiter_name: Option<&str>,
) -> Option<&'a str> {
    let (lines_name, split_value) = stmt_binding_parts(stmt)?;
    let Some((split_head, split_args)) = flatten_direct_call(split_value) else {
        return None;
    };
    if !imported_head_matches_symbol(module, &split_head, "goby/string", "split")
        || split_args.len() != 2
        || !matches!(split_args[0], Expr::Var(name) if name == text_name)
        || !expr_is_newline_delimiter(split_args[1], delimiter_name)
    {
        return None;
    }
    Some(lines_name)
}

fn expr_is_newline_delimiter(expr: &Expr, delimiter_name: Option<&str>) -> bool {
    matches!(expr, Expr::StringLit(delim) if delim == "\n")
        || delimiter_name
            .is_some_and(|name| matches!(expr, Expr::Var(var_name) if var_name == name))
}

fn split_lines_each_output_mode_from_callback(
    module: &Module,
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
        Expr::Var(name) => callback_output_mode_name(name),
        Expr::Lambda { param, body } => match body.as_ref() {
            Expr::Call { callee, arg } if matches!(arg.as_ref(), Expr::Var(name) if name == param) => {
                match callee.as_ref() {
                    Expr::Var(name) => callback_output_mode_name(name),
                    _ => None,
                }
            }
            _ => None,
        },
        _ => None,
    }
}

fn callback_output_mode_name(name: &str) -> Option<OutputReadMode> {
    match name {
        "print" => Some(OutputReadMode::Print),
        "println" => Some(OutputReadMode::Println),
        _ => None,
    }
}

fn split_lines_each_output_mode(module: &Module, stmts: &[Stmt]) -> Option<OutputReadMode> {
    match stmts {
        [read_stmt, split_stmt, middle @ .., Stmt::Expr(each_expr)]
            if newline_delimiter_binding_name(split_stmt).is_none() =>
        {
            let Some((text_name, InputReadMode::ReadAll)) = read_binding_mode(read_stmt) else {
                return None;
            };
            let Some(lines_name) = split_lines_binding_name(module, split_stmt, text_name, None)
            else {
                return None;
            };
            let Some(iterated_name) = resolve_alias_chain_terminal_name(middle, lines_name) else {
                return None;
            };
            split_lines_each_output_mode_from_callback(module, iterated_name, each_expr)
        }
        [
            read_stmt,
            delim_stmt,
            split_stmt,
            middle @ ..,
            Stmt::Expr(each_expr),
        ] if newline_delimiter_binding_name(delim_stmt).is_some() => {
            let Some((text_name, InputReadMode::ReadAll)) = read_binding_mode(read_stmt) else {
                return None;
            };
            let Some(delimiter_name) = newline_delimiter_binding_name(delim_stmt) else {
                return None;
            };
            let Some(lines_name) =
                split_lines_binding_name(module, split_stmt, text_name, Some(delimiter_name))
            else {
                return None;
            };
            let Some(iterated_name) = resolve_alias_chain_terminal_name(middle, lines_name) else {
                return None;
            };
            split_lines_each_output_mode_from_callback(module, iterated_name, each_expr)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use goby_core::parse_module;

    use super::{
        InputReadMode, OutputReadMode, RuntimeIoClassification, RuntimeIoPlan, classify_runtime_io,
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
  each forwarded (|line| -> println "${line}")
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::InterpreterBridge
        );
    }

    #[test]
    fn non_read_program_is_not_runtime_io() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print
main =
  println "hello"
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::NotRuntimeIo
        );
    }
}
