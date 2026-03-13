use goby_core::{Module, Stmt};

use crate::RuntimeOutputResolver;
use crate::lower;
use crate::runtime_eval::{
    IntEvaluator, ListIntEvaluator, collect_functions_with_result, collect_unit_functions,
};
use crate::runtime_flow::RuntimeEvaluators;
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

pub(crate) fn resolve_main_runtime_output_for_compile(
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
        false,
    )
}

fn resolve_main_runtime_output_with_mode_internal(
    module: &Module,
    body: &str,
    parsed_stmts: Option<&[Stmt]>,
    execution_mode: lower::EffectExecutionMode,
    stdin_seed: Option<String>,
    allow_live_stdin: bool,
) -> Option<String> {
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
    RuntimeOutputResolver::resolve(
        module,
        body,
        parsed_stmts,
        &evaluators,
        execution_mode,
        stdin_seed,
        allow_live_stdin,
    )
}
