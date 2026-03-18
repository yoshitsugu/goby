use goby_core::ast::InterpolatedPart;
use goby_core::ir::{CompExpr, IrModule, ValueExpr};
use goby_core::{Expr, Module, Stmt};

use crate::CodegenError;
use crate::backend::WasmProgramBuilder;
use crate::layout::MemoryLayout;
use crate::runtime_flow::DirectCallHead;
use crate::runtime_support::{flatten_direct_call, module_has_selective_import_symbol};
use crate::wasm_exec_plan::main_exec_plan;

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
        /// Interpolation transform applied to each line before output.
        /// `None` = passthrough (emit the line as-is).
        /// `Some((prefix, suffix))` = emit `prefix + line + suffix` per line.
        transform: Option<(String, String)>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum RuntimeIoClassification {
    DynamicWasiIo(RuntimeIoPlan),
    StaticOutput(String),
    #[allow(dead_code)] // Intentional extension point; see STATE.md
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
                transform,
            } => {
                let append_newline = matches!(output_mode, OutputReadMode::Println);
                match transform {
                    None => builder
                        .emit_read_split_lines_each_print_module(append_newline, &suffix_prints),
                    Some((prefix, suffix)) => builder.emit_read_split_lines_each_transform_module(
                        &prefix,
                        &suffix,
                        append_newline,
                        &suffix_prints,
                    ),
                }
            }
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
            RuntimeIoClassification::Unsupported => Err(CodegenError {
                message:
                    "runtime I/O program shape is currently unsupported: no dynamic Wasm lowering exists and it is outside the temporary interpreter-bridge subset"
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
            RuntimeIoClassification::StaticOutput(_) => Err(CodegenError {
                message:
                    "runtime stdin execution path is only for interpreter-bridge programs; this program has static output and should be compiled to Wasm instead"
                        .to_string(),
            }),
            RuntimeIoClassification::Unsupported => Err(CodegenError {
                message:
                    "runtime stdin execution path is only for interpreter-bridge programs; this runtime I/O shape is currently unsupported"
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
/// | `DynamicWasiIo(plan)` | Body contains `Read.read`/`Read.read_line` usage **and** matches a recognized [`RuntimeIoPlan`] form that can be lowered to a WASI Wasm module. |
/// | `StaticOutput(text)` | Body contains **no** runtime-read calls **and** every statement is a direct `print`/`println` with a string-literal argument.  The output is statically known at compile time. |
/// | `InterpreterBridge` | Body contains runtime-read calls that fall into the narrow temporary interpreter-bridge subset.  The current detection surface is empty — no programs route here now that the transformed split-callback family was promoted to `DynamicWasiIo`.  The variant and CLI fallback path are retained as an extension point for future interpreter-backed forms during Wasm lowering development. |
/// | `Unsupported` | Body contains runtime-read calls but matches neither a recognized [`RuntimeIoPlan`] nor the narrow temporary interpreter-bridge subset. |
/// | `NotRuntimeIo` | Body contains **no** runtime-read calls and is not a `StaticOutput` program (e.g. complex static evaluation via local bindings, arithmetic, etc.).  Falls through to `resolve_main_runtime_output_for_compile`. |
///
/// # Safety contract
///
/// - `Print` effects are **compile-time-safe**: static output collapse (`StaticOutput`) is
///   valid because `Print` does not observe host-environment state.
/// - `Read` effects are **runtime-host-dependent**: they must not be resolved during
///   compilation.  Programs that use `Read` must produce a `DynamicWasiIo` Wasm module or
///   (in the future, if an interpreter-bridge shape is re-introduced) an `InterpreterBridge`
///   execution — never a collapsed `StaticOutput` artifact.  Currently, programs with `Read`
///   that do not match a `DynamicWasiIo` plan are classified `Unsupported`.
///   The minimum supported runtime is WASI Preview 1 (`wasmtime run`).
///
/// # Ordering
///
/// The function checks in priority order: `DynamicWasiIo` → `Unsupported` (via
/// runtime-read detection) → `StaticOutput` → `NotRuntimeIo`.
/// `InterpreterBridge` is currently unreachable because no shapes are detected for it;
/// the detection step has been removed until a concrete future shape requires it.
///
/// # Stopping rule (F3a)
///
/// All runtime-I/O fallback form recognition lives in this module — **not** in `lib.rs` or
/// any other call site.  When adding support for a new runtime-I/O program form:
///
/// 1. Extend [`RuntimeIoPlan`] with a new variant (or extend an existing plan variant).
/// 2. Add or extend a lowering arm in [`RuntimeIoPlan::emit_wasm`].
/// 3. Add a detection branch in [`plan_runtime_io`] (called from this function).
///    Note: `StaticOutput` detection is handled by [`plan_static_output`] (also called
///    directly from `classify_runtime_io`), not through `plan_runtime_io`.  New
///    static-output shapes should extend `plan_static_output` instead.
///
/// Do **not** add new runtime-I/O capability conditionals anywhere outside this module —
/// not in `compile_module`, `execute_module_with_stdin`, the CLI, or any other caller.
/// Those entry points must stay policy-free; all classification decisions belong here.
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
        return RuntimeIoClassification::Unsupported;
    }
    if let Some(text) = plan_static_output(stmts) {
        return RuntimeIoClassification::StaticOutput(text);
    }
    RuntimeIoClassification::NotRuntimeIo
}

/// Detect programs where every statement is a direct `print`/`println` call with a
/// string-literal argument.  Returns the concatenated output string (with newlines
/// appended for `println` calls).  Returns `None` if the body contains anything that
/// cannot be determined purely by the fallback statement form.
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
        let Stmt::Expr(Expr::Call { callee, arg, .. }, _) = stmt else {
            return None;
        };
        let Expr::Var { name, .. } = callee.as_ref() else {
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

pub fn runtime_io_execution_kind(module: &Module) -> Result<RuntimeIoExecutionKind, CodegenError> {
    // G6: IR-based classification with AST fallback.
    let classification = classify_runtime_io_with_ir_fallback(module);
    Ok(match classification {
        RuntimeIoClassification::DynamicWasiIo(_) => RuntimeIoExecutionKind::DynamicWasiIo,
        RuntimeIoClassification::StaticOutput(_) => RuntimeIoExecutionKind::StaticOutput,
        RuntimeIoClassification::InterpreterBridge => RuntimeIoExecutionKind::InterpreterBridge,
        RuntimeIoClassification::Unsupported => RuntimeIoExecutionKind::Unsupported,
        RuntimeIoClassification::NotRuntimeIo => RuntimeIoExecutionKind::NotRuntimeIo,
    })
}

// ---------------------------------------------------------------------------
// IR-based classification (G6)
// ---------------------------------------------------------------------------

/// Walk a `CompExpr` tree and return `true` if any `PerformEffect` node has
/// `effect == "Read"` (i.e. the program performs a runtime read operation).
///
/// Only `PerformEffect` nodes are checked — bare `Var("read")` or `Call { callee: Var("read") }`
/// (unqualified calls not lowered to `PerformEffect`) are not detected here.
/// Programs using bare `read()` will fail IR lowering and fall back to AST classification.
fn ir_has_read_op(comp: &CompExpr) -> bool {
    match comp {
        CompExpr::PerformEffect { effect, .. } if effect == "Read" => true,
        CompExpr::PerformEffect { .. } => false,
        CompExpr::Value(_) => false,
        CompExpr::Let { value, body, .. } => ir_has_read_op(value) || ir_has_read_op(body),
        CompExpr::Seq { stmts, tail } => stmts.iter().any(ir_has_read_op) || ir_has_read_op(tail),
        CompExpr::If { then_, else_, .. } => ir_has_read_op(then_) || ir_has_read_op(else_),
        CompExpr::Call { .. } => false,
        CompExpr::Handle { clauses } => clauses.iter().any(|c| ir_has_read_op(&c.body)),
        CompExpr::WithHandler { handler, body } => ir_has_read_op(handler) || ir_has_read_op(body),
        CompExpr::Resume { .. } => false,
    }
}

/// Attempt to detect an Echo plan from the IR body of `main`.
///
/// Recognizes the binding form (G5-lowerable), including alias chains of pure
/// variable bindings between the Read and Print:
/// ```text
/// let text: ? = perform Read.read()|Read.read_line()
/// in let alias: ? = text       -- optional pure alias chain
/// in perform Print.print(alias)|Print.println(alias)
/// ```
///
/// The inline form `print(read())` fails IR lowering entirely and falls back to AST
/// classification, so it is handled outside this function.
///
/// `suffix_prints` are not detected in the IR path; programs with suffix prints that
/// successfully lower will be detected by `ir_has_read_op` and return `Unsupported` if
/// they don't match here. The `classify_runtime_io_with_ir_fallback` caller should then
/// try the AST fallback when IR returns `Unsupported` for a known-alias-chain program.
fn ir_plan_echo(comp: &CompExpr) -> Option<RuntimeIoPlan> {
    // Match: let <name>: ? = perform Read.{read|read_line}()
    //        [in let <alias>: ? = <name>]* (pure alias chain)
    //        in perform Print.{print|println}(<terminal_name>)
    let CompExpr::Let {
        name: bound_name,
        value,
        body,
        ..
    } = comp
    else {
        return None;
    };
    // value must be a PerformEffect for Read.read or Read.read_line (no args)
    let input_mode = match value.as_ref() {
        CompExpr::PerformEffect { effect, op, args } if effect == "Read" && args.is_empty() => {
            match op.as_str() {
                "read" => InputReadMode::ReadAll,
                "read_line" => InputReadMode::ReadLine,
                _ => return None,
            }
        }
        _ => return None,
    };
    // Walk through any pure alias-chain Let bindings to find the terminal name.
    // Each intermediate Let must bind a variable directly to another variable.
    let terminal_name = ir_resolve_echo_terminal_name(body, bound_name);
    // Final Print must be PerformEffect with the terminal name as the single arg.
    let output_mode = match body_after_alias_chain(body, bound_name) {
        CompExpr::PerformEffect { effect, op, args } if effect == "Print" && args.len() == 1 => {
            if !matches!(&args[0], ValueExpr::Var(v) if v == terminal_name) {
                return None;
            }
            match op.as_str() {
                "print" => OutputReadMode::Print,
                "println" => OutputReadMode::Println,
                _ => return None,
            }
        }
        _ => return None,
    };
    Some(RuntimeIoPlan::Echo {
        input_mode,
        output_mode,
        suffix_prints: vec![],
    })
}

/// Walk a `Let`-chain that consists only of pure alias bindings (`let x: ? = y`) and
/// return the terminal variable name to be passed to Print.
///
/// Stops and returns the current name as soon as a `Let` whose value is not a pure
/// variable alias (or a non-`Let` node) is encountered.  The return is infallible;
/// `Option` is not used because there is no error path.
fn ir_resolve_echo_terminal_name<'a>(comp: &'a CompExpr, initial: &'a str) -> &'a str {
    let mut current = initial;
    let mut node = comp;
    loop {
        match node {
            CompExpr::Let {
                name, value, body, ..
            } => {
                match value.as_ref() {
                    CompExpr::Value(ValueExpr::Var(src)) if src == current => {
                        // Pure alias: `let name = current`
                        current = name;
                        node = body;
                    }
                    _ => return current,
                }
            }
            _ => return current,
        }
    }
}

/// Walk through pure alias-chain `Let` bindings to reach the tail computation.
fn body_after_alias_chain<'a>(comp: &'a CompExpr, initial: &str) -> &'a CompExpr {
    let mut current_name = initial;
    let mut node = comp;
    loop {
        match node {
            CompExpr::Let {
                name, value, body, ..
            } => {
                if matches!(value.as_ref(), CompExpr::Value(ValueExpr::Var(src)) if src == current_name)
                {
                    current_name = name;
                    node = body;
                } else {
                    return node;
                }
            }
            _ => return node,
        }
    }
}

/// Attempt to detect a StaticOutput plan from the IR body of `main`.
///
/// Recognizes bodies where every effectful statement is a bare `print`/`println` call
/// (`Call { callee: Var("print"|"println"), args: [StrLit(text)] }`) and there are no
/// Read operations.
///
/// NOTE: qualified `Print.print(...)` becomes `PerformEffect` in the IR and is NOT
/// matched here. Programs using qualified Print calls that lower successfully but are
/// not static will fall through to `NotRuntimeIo`.
fn ir_plan_static_output(comp: &CompExpr) -> Option<String> {
    let mut out = String::new();
    let mut print_count = 0usize;
    if !ir_collect_static_prints(comp, &mut out, &mut print_count) {
        return None;
    }
    // A body with no print calls at all (e.g. `Value(Unit)`) is not StaticOutput.
    // Note: `print ""` has one print call and produces an empty string — that IS
    // a valid StaticOutput with empty text. We distinguish by counting calls, not
    // by checking whether the accumulated string is empty.
    if print_count == 0 {
        return None;
    }
    Some(out)
}

/// Walk a `CompExpr` collecting static print output.
///
/// Returns `true` if the entire computation is either:
/// - a `Call { callee: Var("print"|"println"), args: [StrLit] }` node, or
/// - a `Seq` whose stmts are all such calls and whose tail is `Value(Unit)`, or
/// - a `Value(Unit)` (empty / unit-returning tail).
///
/// Returns `false` if any node cannot be determined statically.
/// `print_count` is incremented for each accepted print/println call.
fn ir_collect_static_prints(comp: &CompExpr, out: &mut String, print_count: &mut usize) -> bool {
    match comp {
        CompExpr::Value(ValueExpr::Unit) => true,
        CompExpr::Call { callee, args } => {
            let ValueExpr::Var(name) = callee.as_ref() else {
                return false;
            };
            if args.len() != 1 {
                return false;
            }
            let ValueExpr::StrLit(text) = &args[0] else {
                return false;
            };
            match name.as_str() {
                "print" => {
                    out.push_str(text);
                    *print_count += 1;
                    true
                }
                "println" => {
                    out.push_str(text);
                    out.push('\n');
                    *print_count += 1;
                    true
                }
                _ => false,
            }
        }
        CompExpr::Seq { stmts, tail } => {
            for s in stmts {
                if !ir_collect_static_prints(s, out, print_count) {
                    return false;
                }
            }
            ir_collect_static_prints(tail, out, print_count)
        }
        _ => false,
    }
}

/// Classify a Goby `IrModule`'s `main` body into a [`RuntimeIoClassification`].
///
/// This is the IR-based counterpart to [`classify_runtime_io`]. It consumes a
/// successfully-lowered IR rather than raw parsed AST statement lists.
///
/// Classification order:
/// 1. If the main body contains a `PerformEffect { effect: "Read", .. }` node:
///    - Try to detect `DynamicWasiIo(Echo)` plan.
///    - If no known plan matches → `Unsupported`.
/// 2. If no Read op is present:
///    - Try to detect `StaticOutput` (all-static bare print calls).
///    - Otherwise → `NotRuntimeIo`.
///
/// # Limitations (G6 scope)
///
/// - Bare `read()` / `print x` calls are NOT lowered to `PerformEffect`; they produce
///   `Call { callee: Var("read") }` IR nodes. Programs using bare effect names therefore
///   return `NotRuntimeIo` from this function. Callers should fall back to statement-form
///   classification via [`classify_runtime_io_with_ir_fallback`] for the complete result.
/// - `SplitLinesEach` programs use `Pipeline`/`MethodCall` constructs that fail lowering.
///   They are handled by the statement-form fallback in [`classify_runtime_io_with_ir_fallback`].
/// - Echo plans with `suffix_prints` are not detected here. `ir_has_read_op` will still
///   detect the Read op and the IR returns `Unsupported`. The
///   [`classify_runtime_io_with_ir_fallback`] caller then promotes that to statement-form fallback.
#[allow(dead_code)]
pub(crate) fn classify_runtime_io_from_ir(ir_module: &IrModule) -> RuntimeIoClassification {
    let Some(main_decl) = ir_module.decls.iter().find(|d| d.name == "main") else {
        return RuntimeIoClassification::NotRuntimeIo;
    };
    classify_runtime_io_from_ir_decl(main_decl)
}

fn classify_runtime_io_from_ir_decl(main_decl: &goby_core::ir::IrDecl) -> RuntimeIoClassification {
    let body = &main_decl.body;
    if ir_has_read_op(body) {
        if let Some(plan) = ir_plan_echo(body) {
            return RuntimeIoClassification::DynamicWasiIo(plan);
        }
        return RuntimeIoClassification::Unsupported;
    }
    if let Some(text) = ir_plan_static_output(body) {
        return RuntimeIoClassification::StaticOutput(text);
    }
    RuntimeIoClassification::NotRuntimeIo
}

/// Classify runtime I/O with IR-based analysis preferred, falling back to runtime statement
/// forms derived from `wasm_exec_plan` when needed.
///
/// Strategy (G6):
/// 1. Build `main`'s `WasmDeclExecPlan`.
/// 2. If IR lowering for `main` fails → statement-form classification (covers inline echo,
///    SplitLinesEach, etc.).
/// 3. If lowering succeeds → IR classification.
///    - If IR returns `DynamicWasiIo` or `StaticOutput` → definitive; use it.
///    - If IR returns `NotRuntimeIo` or `Unsupported` → fall back to statement-form classification.
///      `NotRuntimeIo` occurs when IR doesn't see effect ops (bare names); `Unsupported`
///      occurs when IR detects Read but cannot match a known plan (e.g., echo with
///      suffix prints, or alias-chain forms the IR classifier doesn't cover).  The fallback
///      classifier may succeed for these cases.
///    - `InterpreterBridge` is currently unreachable but treated as definitive if returned.
pub(crate) fn classify_runtime_io_with_ir_fallback(module: &Module) -> RuntimeIoClassification {
    let Some(main_plan) = main_exec_plan(module) else {
        return RuntimeIoClassification::NotRuntimeIo;
    };
    let parsed_body = main_plan
        .runtime
        .as_ref()
        .map(|runtime| runtime.stmts.as_ref());
    let ir_result = main_plan
        .ir_decl
        .as_ref()
        .map(classify_runtime_io_from_ir_decl);
    match ir_result {
        None => classify_runtime_io(module, parsed_body),
        Some(RuntimeIoClassification::NotRuntimeIo | RuntimeIoClassification::Unsupported) => {
            // IR may have missed bare-name effect calls or unrecognised runtime-I/O forms;
            // consult the statement-form classifier for a potentially better result.
            classify_runtime_io(module, parsed_body)
        }
        Some(other) => other,
    }
}

fn plan_runtime_io(module: &Module, stmts: &[Stmt]) -> Option<RuntimeIoPlan> {
    if let Some(plan) = plan_echo_runtime_io(stmts) {
        return Some(plan);
    }
    if let Some((output_mode, suffix_prints, transform)) = split_lines_each_plan(module, stmts) {
        return Some(RuntimeIoPlan::SplitLinesEach {
            output_mode,
            suffix_prints,
            transform,
        });
    }
    None
}

fn plan_echo_runtime_io(stmts: &[Stmt]) -> Option<RuntimeIoPlan> {
    let (echo_stmts, suffix_prints) = split_trailing_static_print_suffixes(stmts)?;
    match echo_stmts {
        [Stmt::Expr(expr, _)] => {
            output_read_mode(expr).map(|(input_mode, output_mode)| RuntimeIoPlan::Echo {
                input_mode,
                output_mode,
                suffix_prints,
            })
        }
        [
            read_stmt,
            middle @ ..,
            Stmt::Expr(Expr::Call { callee, arg, .. }, _),
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
    if !matches!(arg, Expr::Var { name: var_name, .. } if var_name == printed_name) {
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
    let Expr::Var {
        name: output_name, ..
    } = callee
    else {
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
        Stmt::Expr(expr, _) => expr_contains_runtime_read(expr),
    }
}

fn expr_contains_runtime_read(expr: &Expr) -> bool {
    match expr {
        Expr::Call { callee, arg, .. } => {
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
        | Expr::Var { name: _, .. }
        | Expr::Qualified { .. } => false,
        Expr::ListIndex { list, index } => {
            expr_contains_runtime_read(list) || expr_contains_runtime_read(index)
        }
    }
}

fn is_read_all_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Call { callee, arg, .. } if arg.is_unit_value() => match callee.as_ref() {
            Expr::Var { name, .. } => name == "read",
            Expr::Qualified {
                receiver, member, ..
            } => receiver == "Read" && member == "read",
            _ => false,
        },
        _ => false,
    }
}

fn is_read_line_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Call { callee, arg, .. } if arg.is_unit_value() => match callee.as_ref() {
            Expr::Var { name, .. } => name == "read_line",
            Expr::Qualified {
                receiver, member, ..
            } => receiver == "Read" && member == "read_line",
            _ => false,
        },
        _ => false,
    }
}

fn output_read_mode(expr: &Expr) -> Option<(InputReadMode, OutputReadMode)> {
    let Expr::Call { callee, arg, .. } = expr else {
        return None;
    };
    let Expr::Var { name, .. } = callee.as_ref() else {
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
        Stmt::Binding { name, value, .. } | Stmt::MutBinding { name, value, .. } => {
            Some((name, value))
        }
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
    if matches!(alias_value, Expr::Var { name: var_name, .. } if var_name == source_name) {
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
                    && let Expr::Var {
                        name: source_name, ..
                    } = alias_value
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
    let (split_head, split_args) = flatten_direct_call(split_value)?;
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
        Expr::Var { name, .. } => {
            resolve_alias_chain_source_name(stmts, name) == Some(expected_name)
        }
        _ => false,
    }
}

fn expr_is_newline_delimiter(expr: &Expr, stmts: &[Stmt]) -> bool {
    matches!(expr, Expr::StringLit(delim) if delim == "\n")
        || matches!(expr, Expr::Var { name, .. } if name_resolves_to_newline_literal(stmts, name))
}

/// Returns `(output_mode, transform)` for the `each` callback:
/// - `transform = None`:  passthrough (emit each line as-is)
/// - `transform = Some((prefix, suffix))`:  wrap each line with static prefix/suffix
fn split_lines_each_callback_plan(
    module: &Module,
    callback_scope_stmts: &[Stmt],
    lines_name: &str,
    each_expr: &Expr,
) -> Option<(OutputReadMode, Option<(String, String)>)> {
    let (each_head, each_args) = flatten_direct_call(each_expr)?;
    if !imported_head_matches_symbol(module, &each_head, "goby/list", "each")
        || each_args.len() != 2
        || !matches!(each_args[0], Expr::Var { name, .. } if name == lines_name)
    {
        return None;
    }

    match each_args[1] {
        Expr::Var { name, .. } => {
            let mode = callback_output_mode_name(resolve_alias_chain_source_name(
                callback_scope_stmts,
                name,
            )?)?;
            Some((mode, None))
        }
        Expr::Lambda { param, body } => match body.as_ref() {
            Expr::Call { callee, arg, .. } => {
                let mode = match callee.as_ref() {
                    Expr::Var { name, .. } => callback_output_mode_name(
                        resolve_alias_chain_source_name(callback_scope_stmts, name)?,
                    )?,
                    _ => return None,
                };
                if callback_arg_matches_line_passthrough(arg, param) {
                    Some((mode, None))
                } else if let Some((prefix, suffix)) = extract_transform(arg, param) {
                    Some((mode, Some((prefix, suffix))))
                } else {
                    None
                }
            }
            _ => None,
        },
        _ => None,
    }
}

/// Extract `(prefix, suffix)` from a transformed interpolated string like `"${param}!"`.
///
/// - Concatenates all `Text` parts before the `Var(param)` expr into `prefix`.
/// - Concatenates all `Text` parts after into `suffix`.
/// - Returns `None` if the interpolated string is not exactly `prefix + param + suffix`
///   (e.g., has extra expressions, or is a plain passthrough with no non-empty text).
fn extract_transform(arg: &Expr, param: &str) -> Option<(String, String)> {
    let Expr::InterpolatedString(parts) = arg else {
        return None;
    };
    let mut prefix = String::new();
    let mut suffix = String::new();
    let mut saw_param = false;
    for part in parts {
        match part {
            InterpolatedPart::Text(t) => {
                if saw_param {
                    suffix.push_str(t);
                } else {
                    prefix.push_str(t);
                }
            }
            InterpolatedPart::Expr(e)
                if !saw_param && matches!(e.as_ref(), Expr::Var { name: n, .. } if n == param) =>
            {
                saw_param = true;
            }
            _ => return None,
        }
    }
    if !saw_param {
        return None;
    }
    // Passthrough (pure `"${param}"`) has no non-empty text parts; skip it so the
    // passthrough path in `split_lines_each_callback_plan` handles it instead.
    if prefix.is_empty() && suffix.is_empty() {
        return None;
    }
    Some((prefix, suffix))
}

fn callback_arg_matches_line_passthrough(arg: &Expr, param: &str) -> bool {
    matches!(arg, Expr::Var { name, .. } if name == param)
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
                if !saw_param_expr
                    && matches!(expr.as_ref(), Expr::Var { name, .. } if name == param) =>
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
    let Stmt::Expr(Expr::Call { callee, arg, .. }, _) = stmt else {
        return None;
    };
    let Expr::Var { name, .. } = callee.as_ref() else {
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

#[allow(clippy::type_complexity)]
fn split_lines_each_plan(
    module: &Module,
    stmts: &[Stmt],
) -> Option<(
    OutputReadMode,
    Vec<StaticPrintSuffix>,
    Option<(String, String)>,
)> {
    let [read_stmt, rest @ ..] = stmts else {
        return None;
    };
    let Some((text_name, InputReadMode::ReadAll)) = read_binding_mode(read_stmt) else {
        return None;
    };

    for (each_index, each_stmt) in rest.iter().enumerate() {
        let Stmt::Expr(each_expr, _) = each_stmt else {
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
            if let Some((output_mode, transform)) =
                split_lines_each_callback_plan(module, pre_each_stmts, iterated_name, each_expr)
            {
                return Some((output_mode, suffix_prints, transform));
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
                transform: None,
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
                transform: None,
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
                transform: None,
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
                transform: None,
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
                transform: None,
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
                transform: None,
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
                transform: None,
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
                transform: None,
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
                transform: None,
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
                transform: None,
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
                transform: None,
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
                transform: None,
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
    fn plans_transformed_split_callback_as_dynamic_wasi_io() {
        // Previously classified as InterpreterBridge; now promotes to DynamicWasiIo.
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
        let classification = classify_runtime_io(&module, body.as_deref());
        assert!(
            matches!(
                &classification,
                RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::SplitLinesEach {
                    output_mode: OutputReadMode::Println,
                    transform: Some((prefix, suffix)),
                    ..
                }) if suffix == "!" && prefix.is_empty()
            ),
            "expected DynamicWasiIo with transform suffix '!', got: {:?}",
            classification
        );
    }

    #[test]
    fn classifies_non_bridge_read_transform_as_unsupported() {
        let (module, body) = main_stmts(
            r#"
main : Unit -> Unit can Print, Read
main =
  text = read()
  decorated = "${text}!"
  print decorated
"#,
        );
        assert_eq!(
            classify_runtime_io(&module, body.as_deref()),
            RuntimeIoClassification::Unsupported
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
    fn unsupported_classification_does_not_produce_wasm() {
        let result = RuntimeIoClassification::Unsupported.compile_module_wasm_or_error();
        assert!(result.is_err());
    }

    #[test]
    fn unsupported_classification_rejects_interpreter_bridge_stdin() {
        let result = RuntimeIoClassification::Unsupported.require_interpreter_bridge_stdin();
        assert!(result.is_err());
    }

    // --- IR-based classification tests (G6) ---

    mod ir_classify {
        use goby_core::ir::{CompExpr, IrDecl, IrModule, IrType, ValueExpr};

        use super::super::{
            InputReadMode, OutputReadMode, RuntimeIoClassification, RuntimeIoPlan,
            classify_runtime_io_from_ir,
        };

        fn ir_module_with_main(body: CompExpr) -> IrModule {
            IrModule {
                decls: vec![IrDecl {
                    name: "main".into(),
                    params: vec![],
                    result_ty: IrType::Unit,
                    residual_effects: vec!["Read".into(), "Print".into()],
                    body,
                }],
            }
        }

        // --- ir_has_read_op / read detection ---

        #[test]
        fn ir_classify_read_binding_then_print_is_dynamic_wasi_io_echo() {
            // IR for: text = Read.read(); Print.print(text)
            // → Let { name: "text", value: PerformEffect(Read.read), body: PerformEffect(Print.print(text)) }
            let body = CompExpr::Let {
                name: "text".into(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::PerformEffect {
                    effect: "Read".into(),
                    op: "read".into(),
                    args: vec![],
                }),
                body: Box::new(CompExpr::PerformEffect {
                    effect: "Print".into(),
                    op: "print".into(),
                    args: vec![ValueExpr::Var("text".into())],
                }),
            };
            let ir = ir_module_with_main(body);
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                    input_mode: InputReadMode::ReadAll,
                    output_mode: OutputReadMode::Print,
                    suffix_prints: vec![],
                })
            );
        }

        #[test]
        fn ir_classify_read_line_binding_then_println_is_dynamic_wasi_io_echo() {
            // IR for: text = Read.read_line(); Print.println(text)
            let body = CompExpr::Let {
                name: "line".into(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::PerformEffect {
                    effect: "Read".into(),
                    op: "read_line".into(),
                    args: vec![],
                }),
                body: Box::new(CompExpr::PerformEffect {
                    effect: "Print".into(),
                    op: "println".into(),
                    args: vec![ValueExpr::Var("line".into())],
                }),
            };
            let ir = ir_module_with_main(body);
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                    input_mode: InputReadMode::ReadLine,
                    output_mode: OutputReadMode::Println,
                    suffix_prints: vec![],
                })
            );
        }

        #[test]
        fn ir_classify_read_with_no_known_plan_is_unsupported() {
            // IR with a Read op but no recognizable plan
            let body = CompExpr::PerformEffect {
                effect: "Read".into(),
                op: "read".into(),
                args: vec![],
            };
            let ir = ir_module_with_main(body);
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::Unsupported
            );
        }

        #[test]
        fn ir_classify_static_print_is_static_output() {
            // IR for: print "hello"
            // bare print → Call { callee: Var("print"), args: [StrLit("hello")] }
            let body = CompExpr::Call {
                callee: Box::new(ValueExpr::Var("print".into())),
                args: vec![ValueExpr::StrLit("hello".into())],
            };
            let ir = ir_module_with_main(body);
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::StaticOutput("hello".to_string())
            );
        }

        #[test]
        fn ir_classify_static_println_is_static_output_with_newline() {
            let body = CompExpr::Call {
                callee: Box::new(ValueExpr::Var("println".into())),
                args: vec![ValueExpr::StrLit("hi".into())],
            };
            let ir = ir_module_with_main(body);
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::StaticOutput("hi\n".to_string())
            );
        }

        #[test]
        fn ir_classify_seq_static_prints_is_static_output() {
            // IR for: print "a"; println "b"
            let body = CompExpr::Seq {
                stmts: vec![CompExpr::Call {
                    callee: Box::new(ValueExpr::Var("print".into())),
                    args: vec![ValueExpr::StrLit("a".into())],
                }],
                tail: Box::new(CompExpr::Call {
                    callee: Box::new(ValueExpr::Var("println".into())),
                    args: vec![ValueExpr::StrLit("b".into())],
                }),
            };
            let ir = ir_module_with_main(body);
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::StaticOutput("ab\n".to_string())
            );
        }

        #[test]
        fn ir_classify_unit_value_is_not_runtime_io() {
            let body = CompExpr::Value(ValueExpr::Unit);
            let ir = ir_module_with_main(body);
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::NotRuntimeIo
            );
        }

        #[test]
        fn ir_classify_no_main_is_not_runtime_io() {
            let ir = IrModule { decls: vec![] };
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::NotRuntimeIo
            );
        }

        #[test]
        fn ir_classify_print_with_var_arg_is_not_runtime_io() {
            // print x — not a StrLit, so not StaticOutput; no Read so not Unsupported
            let body = CompExpr::Call {
                callee: Box::new(ValueExpr::Var("print".into())),
                args: vec![ValueExpr::Var("x".into())],
            };
            let ir = ir_module_with_main(body);
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::NotRuntimeIo
            );
        }

        #[test]
        fn ir_classify_print_empty_string_is_static_output() {
            // print "" — one print call with empty string arg; should be StaticOutput("")
            let body = CompExpr::Call {
                callee: Box::new(ValueExpr::Var("print".into())),
                args: vec![ValueExpr::StrLit("".into())],
            };
            let ir = ir_module_with_main(body);
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::StaticOutput("".to_string())
            );
        }

        #[test]
        fn ir_classify_read_echo_with_alias_chain_is_dynamic_wasi_io() {
            // IR for: text = Read.read(); alias = text; Print.print(alias)
            // → Let { name: "text", value: PerformEffect(Read.read),
            //         body: Let { name: "alias", value: Value(Var("text")),
            //                     body: PerformEffect(Print.print(alias)) } }
            let body = CompExpr::Let {
                name: "text".into(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::PerformEffect {
                    effect: "Read".into(),
                    op: "read".into(),
                    args: vec![],
                }),
                body: Box::new(CompExpr::Let {
                    name: "alias".into(),
                    ty: IrType::Unknown,
                    value: Box::new(CompExpr::Value(ValueExpr::Var("text".into()))),
                    body: Box::new(CompExpr::PerformEffect {
                        effect: "Print".into(),
                        op: "print".into(),
                        args: vec![ValueExpr::Var("alias".into())],
                    }),
                }),
            };
            let ir = ir_module_with_main(body);
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                    input_mode: InputReadMode::ReadAll,
                    output_mode: OutputReadMode::Print,
                    suffix_prints: vec![],
                })
            );
        }

        #[test]
        fn ir_classify_read_echo_wrong_bound_name_in_print_is_unsupported() {
            // let text = Read.read(); Print.print(other) — wrong variable → Unsupported
            let body = CompExpr::Let {
                name: "text".into(),
                ty: IrType::Unknown,
                value: Box::new(CompExpr::PerformEffect {
                    effect: "Read".into(),
                    op: "read".into(),
                    args: vec![],
                }),
                body: Box::new(CompExpr::PerformEffect {
                    effect: "Print".into(),
                    op: "print".into(),
                    args: vec![ValueExpr::Var("other".into())],
                }),
            };
            let ir = ir_module_with_main(body);
            assert_eq!(
                classify_runtime_io_from_ir(&ir),
                RuntimeIoClassification::Unsupported
            );
        }

        // --- classify_runtime_io_with_ir_fallback integration tests ---

        mod fallback {
            use goby_core::parse_module;

            use super::super::super::{
                InputReadMode, OutputReadMode, RuntimeIoClassification, RuntimeIoPlan,
                classify_runtime_io_with_ir_fallback,
            };

            #[test]
            fn fallback_bare_read_echo_is_dynamic_wasi_io_via_ast() {
                // `print (read())` uses bare names → IR lowering fails → AST fallback
                let source = r#"
main : Unit -> Unit can Print, Read
main =
  print (read())
"#;
                let module = parse_module(source).expect("parse should work");
                let _body = module
                    .declarations
                    .iter()
                    .find(|d| d.name == "main")
                    .and_then(|d| d.parsed_body.clone());
                assert_eq!(
                    classify_runtime_io_with_ir_fallback(&module),
                    RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                        input_mode: InputReadMode::ReadAll,
                        output_mode: OutputReadMode::Print,
                        suffix_prints: vec![],
                    }),
                    "bare print(read()) should route through AST fallback to DynamicWasiIo"
                );
            }

            #[test]
            fn fallback_qualified_read_echo_is_dynamic_wasi_io_via_ir() {
                // `Read.read_line()` + `Print.println` uses qualified names → IR lowering succeeds
                let source = r#"
main : Unit -> Unit can Print, Read
main =
  line = Read.read_line ()
  Print.println line
"#;
                let module = parse_module(source).expect("parse should work");
                let _body = module
                    .declarations
                    .iter()
                    .find(|d| d.name == "main")
                    .and_then(|d| d.parsed_body.clone());
                assert_eq!(
                    classify_runtime_io_with_ir_fallback(&module),
                    RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                        input_mode: InputReadMode::ReadLine,
                        output_mode: OutputReadMode::Println,
                        suffix_prints: vec![],
                    }),
                    "qualified Read.read_line() + Print.println should classify via IR path"
                );
            }

            #[test]
            fn fallback_bare_echo_with_suffix_print_is_dynamic_wasi_io_via_ast() {
                // Bare `read_line()` lowers to a Call (not PerformEffect), so IR returns
                // NotRuntimeIo, which triggers AST fallback → DynamicWasiIo(Echo with suffix).
                let source = r#"
main : Unit -> Unit can Print, Read
main =
  line = read_line()
  println line
  print "done"
"#;
                let module = parse_module(source).expect("parse should work");
                let _body = module
                    .declarations
                    .iter()
                    .find(|d| d.name == "main")
                    .and_then(|d| d.parsed_body.clone());
                assert_eq!(
                    classify_runtime_io_with_ir_fallback(&module),
                    RuntimeIoClassification::DynamicWasiIo(RuntimeIoPlan::Echo {
                        input_mode: InputReadMode::ReadLine,
                        output_mode: OutputReadMode::Println,
                        suffix_prints: vec![super::super::super::StaticPrintSuffix {
                            text: "done".into(),
                            output_mode: OutputReadMode::Print,
                        }],
                    }),
                    "bare echo with suffix should fall back to AST DynamicWasiIo"
                );
            }

            #[test]
            fn fallback_bare_not_runtime_io_uses_ast_result() {
                // A non-IO program with bare print and local binding → NotRuntimeIo from IR,
                // then AST fallback also returns NotRuntimeIo.
                let source = r#"
main : Unit -> Unit
main =
  x = 42
  println x
"#;
                let module = parse_module(source).expect("parse should work");
                let _body = module
                    .declarations
                    .iter()
                    .find(|d| d.name == "main")
                    .and_then(|d| d.parsed_body.clone());
                // Both IR and AST should agree on NotRuntimeIo (not StaticOutput since arg is Var)
                assert_eq!(
                    classify_runtime_io_with_ir_fallback(&module),
                    RuntimeIoClassification::NotRuntimeIo
                );
            }
        }
    }
}
