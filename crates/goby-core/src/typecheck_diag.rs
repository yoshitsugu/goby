//! Constructors for the frequent diagnostic families produced by the typechecker.
//!
//! ## Span-selection policy
//!
//! Every helper accepts an explicit `span: Option<Span>` so the caller decides what to
//! highlight.  The recommended sources, in preference order, are:
//!
//! 1. `best_available_name_use_span(expr)` — narrowest token span covering the problematic name.
//! 2. `best_available_expr_span(expr)` — broader expression span when a whole sub-expression
//!    should be underlined (e.g. argument mismatch).
//! 3. `None` — only when no AST span is yet threaded to the call site; leave a
//!    `// expr span not yet available` comment so future work can find the gap.
//!
//! Do not derive spans from string searches or line counts inside these helpers.

use std::path::Path;

use crate::ast::Span;
use crate::typecheck::TypecheckError;

/// Produce an "unknown function or constructor" diagnostic.
///
/// Used for unresolved bare names, qualified names, and pipeline callees where the name
/// resolves to `Unknown`.
pub(crate) fn err_unknown_callable(
    decl_name: &str,
    name: &str,
    span: Option<Span>,
) -> TypecheckError {
    TypecheckError {
        declaration: Some(decl_name.to_string()),
        span,
        message: format!("unknown function or constructor `{}`", name),
    }
}

/// Produce a "name is ambiguous due to name resolution collision" diagnostic.
///
/// `decl_name` is `None` when the ambiguity is detected at global-env level before any
/// declaration is in scope (see `typecheck_build::ensure_no_ambiguous_globals`).
pub(crate) fn err_name_ambiguous(
    decl_name: Option<&str>,
    name: &str,
    sources: &[String],
    span: Option<Span>,
) -> TypecheckError {
    TypecheckError {
        declaration: decl_name.map(|s| s.to_string()),
        span,
        message: format!(
            "name `{}` is ambiguous due to name resolution collision: {}",
            name,
            sources.join(", ")
        ),
    }
}

/// Produce an import-time "unknown module" diagnostic.
pub(crate) fn err_unknown_module(
    module_path: &str,
    attempted_path: &Path,
    span: Option<Span>,
) -> TypecheckError {
    TypecheckError {
        declaration: None,
        span,
        message: format!(
            "unknown module `{}` (attempted stdlib path: {})",
            module_path,
            attempted_path.display()
        ),
    }
}

/// Produce an import-time "unknown symbol in import" diagnostic.
pub(crate) fn err_unknown_import_symbol(
    symbol: &str,
    module_path: &str,
    span: Option<Span>,
) -> TypecheckError {
    TypecheckError {
        declaration: None,
        span,
        message: format!(
            "unknown symbol `{}` in import from `{}`",
            symbol, module_path
        ),
    }
}

/// Produce an import-time "failed to resolve stdlib module" diagnostic.
pub(crate) fn err_failed_stdlib_module_resolve(
    module_path: &str,
    detail: &str,
    span: Option<Span>,
) -> TypecheckError {
    TypecheckError {
        declaration: None,
        span,
        message: format!(
            "failed to resolve stdlib module `{}`: {}",
            module_path, detail
        ),
    }
}
