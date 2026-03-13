use goby_core::{Module, Stmt};

use crate::RuntimeOutputResolver;
use crate::lower;
use crate::runtime_eval::{
    IntEvaluator, ListIntEvaluator, collect_functions_with_result, collect_unit_functions,
};
use crate::runtime_flow::RuntimeEvaluators;
use crate::runtime_resolver::ResolvedRuntimeOutput;
use crate::runtime_support::module_has_selective_import_symbol;

#[cfg(test)]
pub(crate) fn resolve_main_runtime_output(
    module: &Module,
    body: &str,
    parsed_stmts: Option<&[Stmt]>,
) -> Option<String> {
    resolve_main_runtime_output_with_mode_internal(
        module,
        body,
        parsed_stmts,
        lower::EffectExecutionMode::PortableFallback,
        None,
        true,
    )
}

#[cfg(test)]
pub(crate) fn resolve_main_runtime_output_with_mode(
    module: &Module,
    body: &str,
    parsed_stmts: Option<&[Stmt]>,
    execution_mode: lower::EffectExecutionMode,
) -> Option<String> {
    resolve_main_runtime_output_with_mode_internal(
        module,
        body,
        parsed_stmts,
        execution_mode,
        None,
        true,
    )
}

pub(crate) fn resolve_main_runtime_output_with_mode_and_stdin(
    module: &Module,
    body: &str,
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

pub(crate) fn resolve_main_runtime_output_for_compile(
    module: &Module,
    body: &str,
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
    body: &str,
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
    body: &str,
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
