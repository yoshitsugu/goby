//! Core language infrastructure for Goby.
//!
//! This crate currently provides a minimal AST, parser, and typechecker for MVP bootstrapping.

pub mod analysis;
pub mod ast;
pub mod parser;
mod parser_expr;
mod parser_pattern;
mod parser_stmt;
#[cfg(test)]
mod parser_test_support;
mod parser_top;
mod parser_util;
pub mod stdlib;
pub mod str_util;
pub mod typecheck;
mod typecheck_annotation;
mod typecheck_build;
mod typecheck_check;
mod typecheck_effect;
mod typecheck_env;
mod typecheck_phase;
mod typecheck_types;
mod typecheck_validate;
pub mod types;

pub use analysis::resolve_print_text;
pub use ast::{
    BinOpKind, CaseArm, CasePattern, Declaration, EmbedDecl, Expr, HandlerClause, ImportDecl,
    ImportKind, ListPatternItem, ListPatternTail, Module, RecordField, Span, Stmt, TypeDeclaration,
};
pub use parser::{ParseError, parse_body_stmts, parse_module};
pub use typecheck::{TypecheckError, typecheck_module, typecheck_module_with_context};
