//! Core language infrastructure for Goby.
//!
//! This crate currently provides a minimal AST, parser, and typechecker for MVP bootstrapping.

pub mod analysis;
pub mod ast;
pub mod diagnostic;
pub mod symbol_index;
pub mod parser;
mod parser_expr;
mod parser_pattern;
mod parser_stmt;
#[cfg(test)]
mod parser_test_support;
mod parser_top;
mod parser_util;
mod path_util;
pub mod span;
pub mod stdlib;
pub mod str_util;
pub mod typecheck;
mod typecheck_ambiguity;
mod typecheck_annotation;
mod typecheck_branch;
mod typecheck_build;
mod typecheck_check;
mod typecheck_effect;
mod typecheck_effect_usage;
mod typecheck_env;
mod typecheck_phase;
mod typecheck_render;
mod typecheck_resume;
mod typecheck_stmt;
mod typecheck_types;
mod typecheck_unify;
mod typecheck_validate;
pub mod types;

pub use analysis::resolve_print_text;
pub use ast::{
    BinOpKind, CaseArm, CasePattern, Declaration, EmbedDecl, Expr, HandlerClause, ImportDecl,
    ImportKind, ListPatternItem, ListPatternTail, Module, RecordField, Span, Stmt, TypeDeclaration,
};
pub use diagnostic::{Diagnostic, Severity};
pub use symbol_index::{
    DeclSymbol, EffectMemberSymbol, SymbolIndex, SymbolInfo, build_symbol_index,
};
pub use parser::{ParseError, parse_body_stmts, parse_module};
pub use parser_util::is_identifier;
pub use typecheck_annotation::find_can_keyword_index;
pub use span::{line_col_to_offset, offset_to_line_col};
pub use typecheck::{
    TypecheckError, typecheck_module, typecheck_module_collect,
    typecheck_module_collect_with_context, typecheck_module_with_context,
};
