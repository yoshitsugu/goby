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
    SplitLinesEachPrintln,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RuntimeIoClassification {
    DynamicWasiIo(RuntimeIoPlan),
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
            RuntimeIoPlan::SplitLinesEachPrintln => {
                builder.emit_read_split_lines_each_println_module()
            }
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

fn plan_runtime_io(module: &Module, stmts: &[Stmt]) -> Option<RuntimeIoPlan> {
    match stmts {
        [Stmt::Expr(expr)] => {
            output_read_mode(expr).map(|(input_mode, output_mode)| RuntimeIoPlan::Echo {
                input_mode,
                output_mode,
            })
        }
        [
            Stmt::Binding { name, value } | Stmt::MutBinding { name, value },
            Stmt::Expr(Expr::Call { callee, arg }),
        ] => plan_bound_echo_shape(name, value, name, callee, arg),
        _ if is_split_lines_each_println_shape(module, stmts) => {
            Some(RuntimeIoPlan::SplitLinesEachPrintln)
        }
        [
            Stmt::Binding {
                name: source_name,
                value: source_value,
            }
            | Stmt::MutBinding {
                name: source_name,
                value: source_value,
            },
            Stmt::Binding {
                name: alias_name,
                value: alias_value,
            }
            | Stmt::MutBinding {
                name: alias_name,
                value: alias_value,
            },
            Stmt::Expr(Expr::Call { callee, arg }),
        ] => {
            if !matches!(alias_value, Expr::Var(var_name) if var_name == source_name) {
                return None;
            }
            plan_bound_echo_shape(source_name, source_value, alias_name, callee, arg)
        }
        _ => None,
    }
}

fn plan_bound_echo_shape(
    _source_name: &str,
    source_value: &Expr,
    printed_name: &str,
    callee: &Expr,
    arg: &Expr,
) -> Option<RuntimeIoPlan> {
    let input_mode = if is_read_all_expr(source_value) {
        Some(InputReadMode::ReadAll)
    } else if is_read_line_expr(source_value) {
        Some(InputReadMode::ReadLine)
    } else {
        None
    };
    if input_mode.is_none() {
        None
    } else if !matches!(arg, Expr::Var(var_name) if var_name == printed_name) {
        None
    } else if let Expr::Var(output_name) = callee {
        match output_name.as_str() {
            "print" => Some(RuntimeIoPlan::Echo {
                input_mode: input_mode.expect("checked above"),
                output_mode: OutputReadMode::Print,
            }),
            "println" => Some(RuntimeIoPlan::Echo {
                input_mode: input_mode.expect("checked above"),
                output_mode: OutputReadMode::Println,
            }),
            _ => None,
        }
    } else {
        None
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

fn expr_is_newline_delimiter(expr: &Expr, delimiter_name: Option<&str>) -> bool {
    matches!(expr, Expr::StringLit(delim) if delim == "\n")
        || delimiter_name
            .is_some_and(|name| matches!(expr, Expr::Var(var_name) if var_name == name))
}

fn split_lines_each_println_matches(
    module: &Module,
    text_name: &str,
    delimiter_name: Option<&str>,
    lines_name: &str,
    split_value: &Expr,
    each_expr: &Expr,
) -> bool {
    let Some((split_head, split_args)) = flatten_direct_call(split_value) else {
        return false;
    };
    if !imported_head_matches_symbol(module, &split_head, "goby/string", "split")
        || split_args.len() != 2
        || !matches!(split_args[0], Expr::Var(name) if name == text_name)
        || !expr_is_newline_delimiter(split_args[1], delimiter_name)
    {
        return false;
    }

    let Some((each_head, each_args)) = flatten_direct_call(each_expr) else {
        return false;
    };
    if !imported_head_matches_symbol(module, &each_head, "goby/list", "each")
        || each_args.len() != 2
        || !matches!(each_args[0], Expr::Var(name) if name == lines_name)
    {
        return false;
    }

    let Expr::Lambda { param, body } = each_args[1] else {
        return false;
    };

    matches!(
        body.as_ref(),
        Expr::Call { callee, arg }
            if matches!(callee.as_ref(), Expr::Var(name) if name == "println")
                && matches!(arg.as_ref(), Expr::Var(name) if name == param)
    )
}

fn is_split_lines_each_println_shape(module: &Module, stmts: &[Stmt]) -> bool {
    match stmts {
        [read_stmt, split_stmt, Stmt::Expr(each_expr)] => {
            let Some((text_name, read_value)) = stmt_binding_parts(read_stmt) else {
                return false;
            };
            let Some((lines_name, split_value)) = stmt_binding_parts(split_stmt) else {
                return false;
            };
            is_read_all_expr(read_value)
                && split_lines_each_println_matches(
                    module,
                    text_name,
                    None,
                    lines_name,
                    split_value,
                    each_expr,
                )
        }
        [read_stmt, delim_stmt, split_stmt, Stmt::Expr(each_expr)]
            if stmt_binding_parts(delim_stmt).is_some_and(
                |(_, value)| matches!(value, Expr::StringLit(delim) if delim == "\n"),
            ) =>
        {
            let Some((text_name, read_value)) = stmt_binding_parts(read_stmt) else {
                return false;
            };
            let Some((delimiter_name, delimiter_value)) = stmt_binding_parts(delim_stmt) else {
                return false;
            };
            let Some((lines_name, split_value)) = stmt_binding_parts(split_stmt) else {
                return false;
            };
            is_read_all_expr(read_value)
                && matches!(delimiter_value, Expr::StringLit(delim) if delim == "\n")
                && split_lines_each_println_matches(
                    module,
                    text_name,
                    Some(delimiter_name),
                    lines_name,
                    split_value,
                    each_expr,
                )
        }
        [read_stmt, split_stmt, alias_stmt, Stmt::Expr(each_expr)]
            if !stmt_binding_parts(split_stmt).is_some_and(
                |(_, value)| matches!(value, Expr::StringLit(delim) if delim == "\n"),
            ) =>
        {
            let Some((text_name, read_value)) = stmt_binding_parts(read_stmt) else {
                return false;
            };
            let Some((lines_name, split_value)) = stmt_binding_parts(split_stmt) else {
                return false;
            };
            let Some((alias_name, alias_value)) = stmt_binding_parts(alias_stmt) else {
                return false;
            };
            is_read_all_expr(read_value)
                && matches!(alias_value, Expr::Var(var_name) if var_name == lines_name)
                && split_lines_each_println_matches(
                    module,
                    text_name,
                    None,
                    alias_name,
                    split_value,
                    each_expr,
                )
        }
        [
            read_stmt,
            delim_stmt,
            split_stmt,
            alias_stmt,
            Stmt::Expr(each_expr),
        ] => {
            let Some((text_name, read_value)) = stmt_binding_parts(read_stmt) else {
                return false;
            };
            let Some((delimiter_name, delimiter_value)) = stmt_binding_parts(delim_stmt) else {
                return false;
            };
            let Some((lines_name, split_value)) = stmt_binding_parts(split_stmt) else {
                return false;
            };
            let Some((alias_name, alias_value)) = stmt_binding_parts(alias_stmt) else {
                return false;
            };
            is_read_all_expr(read_value)
                && matches!(delimiter_value, Expr::StringLit(delim) if delim == "\n")
                && matches!(alias_value, Expr::Var(var_name) if var_name == lines_name)
                && split_lines_each_println_matches(
                    module,
                    text_name,
                    Some(delimiter_name),
                    alias_name,
                    split_value,
                    each_expr,
                )
        }
        _ => false,
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
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEachPrintln)
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
            RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEachPrintln)
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
  each forwarded (|line| -> println(line))
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
