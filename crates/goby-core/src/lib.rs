//! Core language infrastructure for Goby.
//!
//! This crate currently provides a minimal AST and parser for MVP bootstrapping.

pub mod ast;
pub mod parser;

pub use ast::{Declaration, Module};
pub use parser::{parse_module, ParseError};
