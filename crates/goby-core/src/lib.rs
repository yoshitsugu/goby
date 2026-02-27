//! Core language infrastructure for Goby.
//!
//! This crate currently provides a minimal AST, parser, and typechecker for MVP bootstrapping.

pub mod analysis;
pub mod ast;
pub mod parser;
pub mod typecheck;
pub mod types;

pub use analysis::resolve_print_text;
pub use ast::{Declaration, Module};
pub use parser::{ParseError, parse_module};
pub use typecheck::{TypecheckError, typecheck_module};
