//! Symbol index for top-level declarations and effect members.
//!
//! Built from a typechecked (or parsed) `Module`, this index maps identifier names to their
//! source spans and type information.  It covers only top-level declarations and effect
//! operation members; local bindings are deferred to D3b.

use std::collections::HashMap;

use crate::ast::{Declaration, Module, Span};

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/// Information about a top-level function or value declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclSymbol {
    /// Span of the *definition* line (the `name params = ...` line).
    /// When a type annotation is present, `Declaration.line` points to the annotation line;
    /// this field always points to the definition line.
    pub span: Span,
    /// The raw type-annotation string as written in source, if present.
    pub annotation: Option<String>,
}

/// Information about an effect operation member.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectMemberSymbol {
    /// Span of the member line (file-relative for top-level effect declarations).
    pub span: Span,
    /// The member's type annotation string (e.g. `"String -> Int"`).
    pub signature: String,
}

/// Index of all top-level symbols in a module.
///
/// Keyed by identifier name.  When a module contains duplicate names (which the
/// typechecker rejects), only the last entry is kept.
#[derive(Debug, Default, Clone)]
pub struct SymbolIndex {
    /// Top-level function / value declarations.
    pub decls: HashMap<String, DeclSymbol>,
    /// Effect operation members (keyed by operation name).
    pub effect_members: HashMap<String, EffectMemberSymbol>,
}

impl SymbolIndex {
    /// Look up a name in both `decls` and `effect_members`.
    /// Returns `None` when the name is not in the index.
    pub fn lookup(&self, name: &str) -> Option<SymbolInfo<'_>> {
        if let Some(sym) = self.decls.get(name) {
            return Some(SymbolInfo::Decl(sym));
        }
        if let Some(sym) = self.effect_members.get(name) {
            return Some(SymbolInfo::EffectMember(sym));
        }
        None
    }
}

/// Result of a [`SymbolIndex::lookup`] call.
#[derive(Debug)]
pub enum SymbolInfo<'a> {
    Decl(&'a DeclSymbol),
    EffectMember(&'a EffectMemberSymbol),
}

// ---------------------------------------------------------------------------
// Builder
// ---------------------------------------------------------------------------

/// Compute the 1-indexed source line of the *definition* line for a declaration.
///
/// `Declaration.line` points to the type-annotation line when an annotation is present;
/// the actual `name params = ...` line is one line lower in that case.
fn decl_def_line(decl: &Declaration) -> usize {
    if decl.type_annotation.is_some() {
        decl.line + 1
    } else {
        decl.line
    }
}

/// Build a `SymbolIndex` from a parsed (or typechecked) `Module`.
pub fn build_symbol_index(module: &Module) -> SymbolIndex {
    let mut index = SymbolIndex::default();

    for decl in &module.declarations {
        let def_line = decl_def_line(decl);
        index.decls.insert(
            decl.name.clone(),
            DeclSymbol {
                span: Span::point(def_line, decl.col),
                annotation: decl.type_annotation.clone(),
            },
        );
    }

    for effect_decl in &module.effect_declarations {
        for member in &effect_decl.members {
            index.effect_members.insert(
                member.name.clone(),
                EffectMemberSymbol {
                    span: member.span,
                    signature: member.type_annotation.clone(),
                },
            );
        }
    }

    index
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_module;

    fn idx(src: &str) -> SymbolIndex {
        let module = parse_module(src).expect("parse failed");
        build_symbol_index(&module)
    }

    #[test]
    fn top_level_function_with_annotation() {
        let src = "add : Int -> Int -> Int\nadd x y = x + y\n";
        let index = idx(src);
        let sym = index.decls.get("add").expect("add not found");
        // annotation line = 1, definition line = 2
        assert_eq!(sym.span.line, 2);
        assert_eq!(sym.span.col, 1);
        assert_eq!(sym.annotation.as_deref(), Some("Int -> Int -> Int"));
    }

    #[test]
    fn top_level_function_without_annotation() {
        let src = "double x = x + x\n";
        let index = idx(src);
        let sym = index.decls.get("double").expect("double not found");
        // no annotation, so line = 1 (the definition line)
        assert_eq!(sym.span.line, 1);
        assert_eq!(sym.span.col, 1);
        assert!(sym.annotation.is_none());
    }

    #[test]
    fn effect_member_indexed() {
        let src = "effect Print\n  println : String -> ()\n";
        let index = idx(src);
        let sym = index
            .effect_members
            .get("println")
            .expect("println not found");
        assert_eq!(sym.signature, "String -> ()");
        // member span should be on line 2 (the member line)
        assert_eq!(sym.span.line, 2);
    }

    #[test]
    fn lookup_unknown_returns_none() {
        let src = "foo x = x\n";
        let index = idx(src);
        assert!(index.lookup("bar").is_none());
    }

    #[test]
    fn empty_module_is_empty() {
        let src = "";
        let index = idx(src);
        assert!(index.decls.is_empty());
        assert!(index.effect_members.is_empty());
    }

    #[test]
    fn lookup_decl_returns_decl_variant() {
        let src = "foo x = x\n";
        let index = idx(src);
        match index.lookup("foo") {
            Some(SymbolInfo::Decl(sym)) => {
                assert_eq!(sym.span.line, 1);
            }
            _ => panic!("expected Decl"),
        }
    }

    #[test]
    fn lookup_effect_member_returns_effect_variant() {
        let src = "effect Print\n  println : String -> ()\n";
        let index = idx(src);
        match index.lookup("println") {
            Some(SymbolInfo::EffectMember(sym)) => {
                assert!(!sym.signature.is_empty());
            }
            _ => panic!("expected EffectMember"),
        }
    }
}
