//! Symbol index for top-level declarations and effect members.
//!
//! Built from a typechecked (or parsed) `Module`, this index maps identifier names to their
//! source spans and type information.  It covers only top-level declarations and effect
//! operation members; local bindings are deferred to D3b.

use std::collections::HashMap;

use crate::ast::{Declaration, Module, Span, Stmt};
use crate::typecheck_annotation::declaration_param_types;
use crate::typecheck_check::check_expr;
use crate::typecheck_env::{Ty, TypeEnv};
use crate::typecheck_render::ty_name;

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
// Local binding index
// ---------------------------------------------------------------------------

/// Information about a single local binding (`name = expr` or `mut name = expr`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalBindingSymbol {
    /// Binding name.
    pub name: String,
    /// Body-relative line number (1-indexed).
    ///
    /// The body string begins with a leading `'\n'` inserted by `collect_indented_body`,
    /// so the first content line is always at body-relative line **2** (not 1).
    /// To convert to a source-file line number: `source_line = def_line_of(decl) + body_relative_line - 1`.
    pub body_relative_line: usize,
    /// Human-readable inferred type string (e.g. `"Int"`, `"List String"`).
    pub ty_str: String,
}

/// Walk the parsed body of a single declaration and collect all local bindings
/// with their inferred types.
///
/// Returns a (possibly empty) list of [`LocalBindingSymbol`] values.  Bindings
/// whose type cannot be inferred (`Ty::Unknown`) are omitted.
///
/// Uses a minimal `TypeEnv` seeded with the declaration's parameter types.
/// Globals are not available in this context, so bindings that depend on
/// global functions will have `Ty::Unknown` and will be omitted.
pub fn infer_local_bindings(decl: &Declaration) -> Vec<LocalBindingSymbol> {
    let stmts = match &decl.parsed_body {
        Some(s) if !s.is_empty() => s,
        _ => return vec![],
    };

    // Build a minimal TypeEnv seeded with declared parameter types.
    let param_tys: Vec<(String, Ty)> = declaration_param_types(decl).unwrap_or_default();

    let mut local_env = TypeEnv {
        globals: HashMap::new(),
        locals: param_tys.iter().map(|(n, t)| (n.clone(), t.clone())).collect(),
        type_aliases: HashMap::new(),
        record_types: HashMap::new(),
    };

    let mut result = Vec::new();

    for stmt in stmts {
        match stmt {
            Stmt::Binding { name, value, span } | Stmt::MutBinding { name, value, span } => {
                let ty = check_expr(value, &local_env);
                // Update local env so later bindings can reference this one.
                local_env.locals.insert(name.clone(), ty.clone());
                // Only emit symbols with a known type and a span.
                if ty != Ty::Unknown {
                    if let Some(sp) = span {
                        result.push(LocalBindingSymbol {
                            name: name.clone(),
                            body_relative_line: sp.line,
                            ty_str: ty_name(&ty),
                        });
                    }
                }
            }
            Stmt::Assign { name, value, .. } => {
                // Re-assignment: update the env but don't emit a new symbol.
                let ty = check_expr(value, &local_env);
                if ty != Ty::Unknown {
                    local_env.locals.insert(name.clone(), ty);
                }
            }
            Stmt::Expr(_, _) => {}
        }
    }

    result
}

// ---------------------------------------------------------------------------
// Builder
// ---------------------------------------------------------------------------

/// Return the 1-indexed source line of the `name params = ...` definition line.
///
/// `Declaration.line` points to the type-annotation line when an annotation is
/// present; the actual definition line is one line below it in that case.
pub fn def_line_of(decl: &Declaration) -> usize {
    if decl.type_annotation.is_some() { decl.line + 1 } else { decl.line }
}

/// Build a `SymbolIndex` from a parsed (or typechecked) `Module`.
pub fn build_symbol_index(module: &Module) -> SymbolIndex {
    let mut index = SymbolIndex::default();

    for decl in &module.declarations {
        let def_line = def_line_of(decl);
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

    // --- infer_local_bindings ---

    fn decl_bindings(src: &str) -> Vec<LocalBindingSymbol> {
        let module = parse_module(src).expect("parse failed");
        let decl = module.declarations.first().expect("no declaration");
        infer_local_bindings(decl)
    }

    #[test]
    fn local_binding_int_param() {
        // `y = x + 1` where x : Int  →  y : Int
        let src = "add : Int -> Int\nadd x =\n  y = x + 1\n  y\n";
        let bindings = decl_bindings(src);
        assert_eq!(bindings.len(), 1, "expected one binding, got: {:?}", bindings);
        assert_eq!(bindings[0].name, "y");
        assert_eq!(bindings[0].ty_str, "Int");
        // body string starts with a leading '\n' (from collect_indented_body),
        // so "y = x + 1" is at body.lines() index 1 → stmt_line = 2.
        assert_eq!(bindings[0].body_relative_line, 2);
    }

    #[test]
    fn local_binding_mut_int() {
        // `mut z = 0`  →  z : Int
        let src = "foo : Int -> Int\nfoo x =\n  mut z = 0\n  z\n";
        let bindings = decl_bindings(src);
        assert_eq!(bindings.len(), 1, "expected one binding, got: {:?}", bindings);
        assert_eq!(bindings[0].name, "z");
        assert_eq!(bindings[0].ty_str, "Int");
    }

    #[test]
    fn local_binding_no_annotation_no_params() {
        // No annotation → no param types → literals still work
        let src = "answer =\n  x = 42\n  x\n";
        let bindings = decl_bindings(src);
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].name, "x");
        assert_eq!(bindings[0].ty_str, "Int");
    }

    #[test]
    fn local_binding_unknown_type_omitted() {
        // `y = some_global_fn x` — global not in env → Unknown → omitted
        let src = "foo : Int -> Int\nfoo x =\n  y = some_global_fn x\n  y\n";
        let bindings = decl_bindings(src);
        // y depends on some_global_fn which is not in locals → Ty::Unknown → omitted
        assert!(bindings.is_empty(), "expected empty, got: {:?}", bindings);
    }
}
