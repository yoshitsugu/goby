//! Core language infrastructure for Goby.
//!
//! This crate currently provides a minimal AST, parser, and typechecker for MVP bootstrapping.

pub mod analysis;
pub mod ast;
pub mod parser;
pub mod str_util;
pub mod typecheck;
pub mod types;

pub use analysis::resolve_print_text;
pub use ast::{BinOpKind, Declaration, Expr, Module, Stmt};
pub use parser::{ParseError, parse_body_stmts, parse_module};
pub use typecheck::{TypecheckError, typecheck_module};
