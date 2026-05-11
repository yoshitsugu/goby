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

/// GU-S3 ambiguity resolution: a qualified constructor form `T.Ctor` clashes
/// with the scrutinee's concretely-known union type `U`, or a bare `Ctor`
/// resolves under a pinned scrutinee type `U` that does not declare `Ctor`.
///
/// `requested_type` is `Some("T")` when the source wrote `T.Ctor` and the
/// scrutinee was a different union, `None` when the source wrote the bare
/// form and the scrutinee pinned the union (i.e. the qualifier was inferred,
/// not written).
///
/// Wording aligns with PLAN_GU §6 GU-S3 D-2.
pub(crate) fn err_ctor_does_not_belong(
    decl_name: &str,
    requested_type: Option<&str>,
    ctor: &str,
    scrutinee_type_name: &str,
    span: Option<Span>,
) -> TypecheckError {
    let message = match requested_type {
        Some(t) => format!(
            "constructor `{}.{}` does not belong to scrutinee type `{}`",
            t, ctor, scrutinee_type_name
        ),
        None => format!(
            "constructor `{}` does not belong to scrutinee type `{}`",
            ctor, scrutinee_type_name
        ),
    };
    TypecheckError {
        declaration: Some(decl_name.to_string()),
        span,
        message,
    }
}

/// GU-S3 ambiguity resolution: a qualified constructor form `T.Ctor`
/// references either an unknown type `T` or a type `T` that does not
/// declare `Ctor`. Distinct from the unqualified "unknown function or
/// constructor" diagnostic because the qualified form carries strictly
/// more information about user intent.
pub(crate) fn err_qualified_ctor_unknown(
    decl_name: &str,
    qualifier: &str,
    ctor: &str,
    span: Option<Span>,
) -> TypecheckError {
    TypecheckError {
        declaration: Some(decl_name.to_string()),
        span,
        message: format!(
            "qualified constructor `{}.{}` is not declared by any union type",
            qualifier, ctor
        ),
    }
}

/// GU-S3 ambiguity resolution: a bare `Ctor` matches more than one union
/// declaration in scope. `candidate_type_names` must be sorted (the caller
/// receives them sorted from `TypeEnv::resolve_ctor`); we render the first
/// `TypeName.Ctor` suggestion so the diagnostic doubles as a fix hint.
pub(crate) fn err_ctor_ambiguous(
    decl_name: &str,
    ctor: &str,
    candidate_type_names: &[String],
    span: Option<Span>,
) -> TypecheckError {
    let listed = candidate_type_names
        .iter()
        .map(|t| format!("`{}.{}`", t, ctor))
        .collect::<Vec<_>>()
        .join(", ");
    let suggest = candidate_type_names
        .first()
        .map(|t| format!(" — use a qualified form like `{}.{}`", t, ctor))
        .unwrap_or_default();
    TypecheckError {
        declaration: Some(decl_name.to_string()),
        span,
        message: format!(
            "constructor `{}` is ambiguous; candidates: {}{}",
            ctor, listed, suggest
        ),
    }
}
