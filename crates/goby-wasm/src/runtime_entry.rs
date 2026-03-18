use goby_core::{Module, Stmt};

use crate::RuntimeOutputResolver;
use crate::lower;
use crate::runtime_eval::{
    IntEvaluator, ListIntEvaluator, collect_functions_with_result, collect_unit_functions,
};
use crate::runtime_flow::RuntimeEvaluators;
use crate::runtime_resolver::ResolvedRuntimeOutput;
use crate::runtime_support::module_has_selective_import_symbol;
use crate::wasm_exec_plan::main_exec_plan;

#[cfg(test)]
pub(crate) fn resolve_module_runtime_output(module: &Module) -> Option<String> {
    resolve_module_runtime_output_with_mode(module, lower::EffectExecutionMode::PortableFallback)
}

#[cfg(test)]
pub(crate) fn resolve_module_runtime_output_with_mode(
    module: &Module,
    execution_mode: lower::EffectExecutionMode,
) -> Option<String> {
    let runtime = main_exec_plan(module)?.runtime?;
    resolve_main_runtime_output_with_mode_internal(
        module,
        runtime.body.as_deref(),
        Some(runtime.stmts.as_ref()),
        execution_mode,
        None,
        true,
    )
}

pub(crate) fn resolve_module_runtime_output_with_mode_and_stdin(
    module: &Module,
    execution_mode: lower::EffectExecutionMode,
    stdin_seed: Option<String>,
) -> Option<String> {
    let runtime = main_exec_plan(module)?.runtime?;
    resolve_main_runtime_output_with_mode_and_stdin_internal(
        module,
        runtime.body.as_deref(),
        Some(runtime.stmts.as_ref()),
        execution_mode,
        stdin_seed,
    )
}

pub(crate) fn resolve_module_runtime_output_for_compile(
    module: &Module,
    execution_mode: lower::EffectExecutionMode,
) -> Result<Option<String>, String> {
    let Some(runtime) = main_exec_plan(module).and_then(|plan| plan.runtime) else {
        return Ok(None);
    };
    resolve_main_runtime_output_for_compile(
        module,
        runtime.body.as_deref(),
        Some(runtime.stmts.as_ref()),
        execution_mode,
    )
}

fn resolve_main_runtime_output_with_mode_and_stdin_internal(
    module: &Module,
    body: Option<&str>,
    parsed_stmts: Option<&[Stmt]>,
    execution_mode: lower::EffectExecutionMode,
    stdin_seed: Option<String>,
) -> Option<String> {
    resolve_main_runtime_output_with_mode_internal(
        module,
        body,
        parsed_stmts,
        execution_mode,
        stdin_seed,
        true,
    )
}

/// Attempt to resolve `main`'s output purely at compile time.
///
/// # Host-environment access policy (Sweep F5)
///
/// This function passes `stdin_seed = None` and `allow_live_stdin = false` to the
/// interpreter.  As a result, `EmbeddedEffectRuntime::ensure_stdin_loaded` will
/// return `Err` for any `Read.read` / `Read.read_line` call, preventing the
/// compiler from consuming host stdin.
///
/// COMPILE-TIME-SAFE for `Print` and `Read`-guarded paths: the interpreter will
/// fail (return `None` or `Err`) rather than silently consuming stdin or network state.
///
/// Note: `fetch_env_var` reads the **compiler-process** environment at compile
/// time regardless of the program path taken:
/// - In direct-style programs, it is evaluated in `lower.rs`'s
///   `collect_phase2_output_text` before this function is called.
/// - In effect-boundary programs, it is evaluated inside this function via
///   `runtime_resolver`'s `apply_runtime_intrinsic_ast`.
///   Either way, the env value is read at compile time and baked into the output.
///   See `apply_runtime_intrinsic` in `lower.rs` for the policy discussion.
pub(crate) fn resolve_main_runtime_output_for_compile(
    module: &Module,
    body: Option<&str>,
    parsed_stmts: Option<&[Stmt]>,
    execution_mode: lower::EffectExecutionMode,
) -> Result<Option<String>, String> {
    let resolved = resolve_main_runtime_output_with_mode_internal_detailed(
        module,
        body,
        parsed_stmts,
        execution_mode,
        None,
        false,
    );
    if let Some(err) = resolved.runtime_error {
        return Err(err);
    }
    Ok(resolved.output)
}

fn resolve_main_runtime_output_with_mode_internal(
    module: &Module,
    body: Option<&str>,
    parsed_stmts: Option<&[Stmt]>,
    execution_mode: lower::EffectExecutionMode,
    stdin_seed: Option<String>,
    allow_live_stdin: bool,
) -> Option<String> {
    let resolved = resolve_main_runtime_output_with_mode_internal_detailed(
        module,
        body,
        parsed_stmts,
        execution_mode,
        stdin_seed,
        allow_live_stdin,
    );
    if let Some(err) = resolved.runtime_error {
        let err_line = format!("runtime error: {}", err);
        if let Some(mut out) = resolved.output {
            if !out.ends_with('\n') {
                out.push('\n');
            }
            out.push_str(&err_line);
            return Some(out);
        }
        return Some(err_line);
    }
    resolved.output
}

fn resolve_main_runtime_output_with_mode_internal_detailed(
    module: &Module,
    body: Option<&str>,
    parsed_stmts: Option<&[Stmt]>,
    execution_mode: lower::EffectExecutionMode,
    stdin_seed: Option<String>,
    allow_live_stdin: bool,
) -> ResolvedRuntimeOutput {
    let int_functions = collect_functions_with_result(module, "Int");
    let list_functions = collect_functions_with_result(module, "List Int");
    let unit_functions = collect_unit_functions(module);
    let int_evaluator = IntEvaluator::root(&int_functions);
    let list_evaluator = ListIntEvaluator::root(
        &list_functions,
        module_has_selective_import_symbol(module, "goby/list", "map"),
    );
    let evaluators = RuntimeEvaluators {
        int: &int_evaluator,
        list: &list_evaluator,
        unit: &unit_functions,
    };
    RuntimeOutputResolver::resolve_detailed(
        module,
        body,
        parsed_stmts,
        &evaluators,
        execution_mode,
        stdin_seed,
        allow_live_stdin,
    )
}
