//! Resolved front-end form for AST-to-IR lowering.
//!
//! This layer sits between the parsed AST and the shared IR. Its job is to
//! normalize callable/effect identity before `ir_lower` constructs shared IR.

use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::ast::{
    BinOpKind, CaseArm, CasePattern, Declaration, Expr, HandlerClause, ImportKind,
    InterpolatedPart, Module, Span, Stmt, UnaryOpKind,
};
use crate::stdlib::StdlibResolver;
use crate::typecheck::PRELUDE_MODULE_PATH;
use crate::typecheck_validate::{default_stdlib_root, effective_imports, import_selects_name};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedModule {
    pub declarations: Vec<ResolvedDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedDeclaration {
    pub name: String,
    pub type_annotation: Option<String>,
    pub params: Vec<String>,
    pub body: Vec<ResolvedStmt>,
}

/// The resolved form of an assignment left-hand side.
///
/// Mirrors `ast::AssignTarget` but with identifiers resolved to `ResolvedRef`.
/// Note: the `index` expressions inside `ListIndex` are `ResolvedExpr`
/// (resolved from `ast::Expr`) so they can be lowered without re-running
/// name resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedTarget {
    /// Plain mutable variable assignment: `x := expr`.
    Var(ResolvedRef),
    /// List-index assignment: `base[index] := expr`.
    ListIndex {
        base: Box<ResolvedTarget>,
        index: Box<ResolvedExpr>,
    },
}

impl ResolvedTarget {
    /// Return the root `ResolvedRef` (the variable at the base of the target chain).
    pub fn root_ref(&self) -> &ResolvedRef {
        match self {
            ResolvedTarget::Var(r) => r,
            ResolvedTarget::ListIndex { base, .. } => base.root_ref(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedStmt {
    Binding {
        name: String,
        value: ResolvedExpr,
        span: Option<Span>,
    },
    MutBinding {
        name: String,
        value: ResolvedExpr,
        span: Option<Span>,
    },
    Assign {
        target: ResolvedTarget,
        value: ResolvedExpr,
        span: Option<Span>,
    },
    Expr(ResolvedExpr, Option<Span>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedHandlerClause {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<ResolvedStmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedCaseArm {
    pub pattern: CasePattern,
    pub body: Box<ResolvedExpr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedInterpolatedPart {
    Text(String),
    Expr(Box<ResolvedExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedRef {
    Local(String),
    Decl(String),
    Helper { module: String, name: String },
    EffectOp { effect: String, op: String },
    Global { module: String, name: String },
    ValueName(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedExpr {
    IntLit(i64),
    BoolLit(bool),
    StringLit(String),
    InterpolatedString(Vec<ResolvedInterpolatedPart>),
    ListLit {
        elements: Vec<ResolvedExpr>,
        spread: Option<Box<ResolvedExpr>>,
    },
    TupleLit(Vec<ResolvedExpr>),
    Ref(ResolvedRef),
    RecordConstruct {
        constructor: String,
        fields: Vec<(String, ResolvedExpr)>,
    },
    UnaryOp {
        op: UnaryOpKind,
        expr: Box<ResolvedExpr>,
    },
    BinOp {
        op: BinOpKind,
        left: Box<ResolvedExpr>,
        right: Box<ResolvedExpr>,
    },
    Call {
        callee: Box<ResolvedExpr>,
        arg: Box<ResolvedExpr>,
        span: Option<Span>,
    },
    MethodCall {
        receiver: String,
        method: String,
        args: Vec<ResolvedExpr>,
    },
    Pipeline {
        value: Box<ResolvedExpr>,
        callee: String,
    },
    Lambda {
        param: String,
        body: Box<ResolvedExpr>,
    },
    Handler {
        clauses: Vec<ResolvedHandlerClause>,
    },
    With {
        handler: Box<ResolvedExpr>,
        body: Vec<ResolvedStmt>,
    },
    Resume {
        value: Box<ResolvedExpr>,
    },
    Block(Vec<ResolvedStmt>),
    Case {
        scrutinee: Box<ResolvedExpr>,
        arms: Vec<ResolvedCaseArm>,
    },
    If {
        condition: Box<ResolvedExpr>,
        then_expr: Box<ResolvedExpr>,
        else_expr: Box<ResolvedExpr>,
    },
    /// Tuple member projection: `receiver.index` where receiver is a local variable.
    TupleProject {
        receiver: String,
        index: usize,
    },
}

pub fn resolve_module(module: &Module) -> ResolvedModule {
    resolve_module_with_stdlib(module, &default_stdlib_root())
}

pub fn resolve_module_with_stdlib(module: &Module, stdlib_root: &Path) -> ResolvedModule {
    let decl_names = declaration_names(module);
    let resolver_metadata = ResolverMetadata::collect(module, stdlib_root);
    ResolvedModule {
        declarations: module
            .declarations
            .iter()
            .map(|decl| {
                resolve_declaration_with_names(module, &decl_names, decl, &resolver_metadata)
            })
            .collect(),
    }
}

pub fn resolve_declaration(decl: &Declaration) -> ResolvedDeclaration {
    resolve_declaration_with_stdlib(decl, &default_stdlib_root())
}

pub fn resolve_declaration_with_stdlib(
    decl: &Declaration,
    stdlib_root: &Path,
) -> ResolvedDeclaration {
    let module = Module {
        imports: vec![],
        embed_declarations: vec![],
        type_declarations: vec![],
        effect_declarations: vec![],
        declarations: vec![decl.clone()],
    };
    let decl_names = declaration_names(&module);
    let resolver_metadata = ResolverMetadata::collect(&module, stdlib_root);
    resolve_declaration_with_names(&module, &decl_names, decl, &resolver_metadata)
}

fn resolve_declaration_with_names(
    module: &Module,
    decl_names: &HashSet<String>,
    decl: &Declaration,
    metadata: &ResolverMetadata,
) -> ResolvedDeclaration {
    let mut resolver = Resolver::new(module, decl_names, metadata);
    resolver.push_scope();
    for param in &decl.params {
        resolver.bind_local(param.clone());
    }
    let body = resolver.resolve_stmt_sequence(
        decl.parsed_body.as_deref().unwrap_or_default(),
        ScopeMode::Current,
    );
    resolver.pop_scope();
    ResolvedDeclaration {
        name: decl.name.clone(),
        type_annotation: decl.type_annotation.clone(),
        params: decl.params.clone(),
        body,
    }
}

fn declaration_names(module: &Module) -> HashSet<String> {
    module
        .declarations
        .iter()
        .map(|decl| decl.name.clone())
        .collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScopeMode {
    Current,
    Child,
}

struct Resolver {
    decl_names: HashSet<String>,
    selective_imports: HashMap<String, ResolvedRef>,
    qualified_receivers: HashMap<String, String>,
    bare_effect_ops: HashMap<String, String>,
    qualified_effect_ops: HashMap<String, HashSet<String>>,
    scopes: Vec<HashSet<String>>,
}

impl Resolver {
    fn new(module: &Module, decl_names: &HashSet<String>, metadata: &ResolverMetadata) -> Self {
        let mut selective_imports = HashMap::new();
        let mut qualified_receivers = HashMap::new();

        for import in &module.imports {
            let canonical_module = module_basename(&import.module_path).to_string();
            match &import.kind {
                ImportKind::Plain => {
                    qualified_receivers.insert(canonical_module.clone(), canonical_module.clone());
                }
                ImportKind::Alias(alias) => {
                    qualified_receivers.insert(alias.clone(), canonical_module.clone());
                }
                ImportKind::Selective(names) => {
                    for name in names {
                        selective_imports.insert(
                            name.clone(),
                            imported_symbol_ref(&canonical_module, name, metadata),
                        );
                    }
                }
            }
        }

        Self {
            decl_names: decl_names.clone(),
            selective_imports,
            qualified_receivers,
            bare_effect_ops: metadata.bare_effect_ops.clone(),
            qualified_effect_ops: metadata.qualified_effect_ops.clone(),
            scopes: vec![],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn bind_local(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name);
        }
    }

    fn is_local(&self, name: &str) -> bool {
        self.scopes.iter().rev().any(|scope| scope.contains(name))
    }

    fn resolve_stmt_sequence(
        &mut self,
        stmts: &[Stmt],
        scope_mode: ScopeMode,
    ) -> Vec<ResolvedStmt> {
        if scope_mode == ScopeMode::Child {
            self.push_scope();
        }

        let resolved = stmts
            .iter()
            .map(|stmt| self.resolve_stmt(stmt))
            .collect::<Vec<_>>();

        if scope_mode == ScopeMode::Child {
            self.pop_scope();
        }

        resolved
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> ResolvedStmt {
        match stmt {
            Stmt::Binding { name, value, span } => {
                let resolved = ResolvedStmt::Binding {
                    name: name.clone(),
                    value: self.resolve_expr(value),
                    span: *span,
                };
                self.bind_local(name.clone());
                resolved
            }
            Stmt::MutBinding { name, value, span } => {
                let resolved = ResolvedStmt::MutBinding {
                    name: name.clone(),
                    value: self.resolve_expr(value),
                    span: *span,
                };
                self.bind_local(name.clone());
                resolved
            }
            Stmt::Assign { target, value, span } => ResolvedStmt::Assign {
                target: self.resolve_assign_target(target),
                value: self.resolve_expr(value),
                span: *span,
            },
            Stmt::Expr(expr, span) => ResolvedStmt::Expr(self.resolve_expr(expr), *span),
        }
    }

    /// Resolve an `ast::AssignTarget` into a `ResolvedTarget`.
    ///
    /// Important: the `base` of a `ListIndex` target is resolved recursively as a
    /// `ResolvedTarget`, NOT through `resolve_list_index` / `resolve_expr`. The index
    /// expression IS resolved through `resolve_expr` because it is a read expression.
    fn resolve_assign_target(&mut self, target: &crate::ast::AssignTarget) -> ResolvedTarget {
        match target {
            crate::ast::AssignTarget::Var(name) => ResolvedTarget::Var(self.resolve_name(name)),
            crate::ast::AssignTarget::ListIndex { base, index } => ResolvedTarget::ListIndex {
                base: Box::new(self.resolve_assign_target(base)),
                index: Box::new(self.resolve_expr(index)),
            },
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> ResolvedExpr {
        match expr {
            Expr::Spanned { expr, .. } => self.resolve_expr(expr),
            Expr::IntLit(n) => ResolvedExpr::IntLit(*n),
            Expr::BoolLit(b) => ResolvedExpr::BoolLit(*b),
            Expr::StringLit(s) => ResolvedExpr::StringLit(s.clone()),
            Expr::InterpolatedString(parts) => ResolvedExpr::InterpolatedString(
                parts
                    .iter()
                    .map(|part| match part {
                        InterpolatedPart::Text(text) => {
                            ResolvedInterpolatedPart::Text(text.clone())
                        }
                        InterpolatedPart::Expr(inner) => {
                            ResolvedInterpolatedPart::Expr(Box::new(self.resolve_expr(inner)))
                        }
                    })
                    .collect(),
            ),
            Expr::ListLit { elements, spread } => ResolvedExpr::ListLit {
                elements: elements
                    .iter()
                    .map(|expr| self.resolve_expr(expr))
                    .collect(),
                spread: spread
                    .as_ref()
                    .map(|expr| Box::new(self.resolve_expr(expr.as_ref()))),
            },
            Expr::TupleLit(items) => {
                ResolvedExpr::TupleLit(items.iter().map(|expr| self.resolve_expr(expr)).collect())
            }
            Expr::Var { name, .. } => ResolvedExpr::Ref(self.resolve_name(name)),
            Expr::Qualified {
                receiver, member, ..
            } => {
                if self.is_local(receiver)
                    && !member.is_empty()
                    && member.chars().all(|c| c.is_ascii_digit())
                    && let Ok(index) = member.parse::<usize>()
                {
                    ResolvedExpr::TupleProject {
                        receiver: receiver.clone(),
                        index,
                    }
                } else {
                    ResolvedExpr::Ref(self.resolve_qualified(receiver, member))
                }
            }
            Expr::RecordConstruct {
                constructor,
                fields,
                ..
            } => ResolvedExpr::RecordConstruct {
                constructor: constructor.clone(),
                fields: fields
                    .iter()
                    .map(|(name, value)| (name.clone(), self.resolve_expr(value)))
                    .collect(),
            },
            Expr::UnaryOp { op, expr } => ResolvedExpr::UnaryOp {
                op: op.clone(),
                expr: Box::new(self.resolve_expr(expr)),
            },
            Expr::BinOp { op, left, right } => ResolvedExpr::BinOp {
                op: op.clone(),
                left: Box::new(self.resolve_expr(left)),
                right: Box::new(self.resolve_expr(right)),
            },
            Expr::Call { callee, arg, span } => ResolvedExpr::Call {
                callee: Box::new(self.resolve_expr(callee)),
                arg: Box::new(self.resolve_expr(arg)),
                span: *span,
            },
            Expr::MethodCall {
                receiver,
                method,
                args,
                ..
            } => ResolvedExpr::MethodCall {
                receiver: receiver.clone(),
                method: method.clone(),
                args: args.iter().map(|arg| self.resolve_expr(arg)).collect(),
            },
            Expr::Pipeline { value, callee, .. } => ResolvedExpr::Pipeline {
                value: Box::new(self.resolve_expr(value)),
                callee: callee.clone(),
            },
            Expr::Lambda { param, body } => {
                self.push_scope();
                self.bind_local(param.clone());
                let resolved_body = self.resolve_expr(body);
                self.pop_scope();
                ResolvedExpr::Lambda {
                    param: param.clone(),
                    body: Box::new(resolved_body),
                }
            }
            Expr::Handler { clauses } => ResolvedExpr::Handler {
                clauses: clauses
                    .iter()
                    .map(|clause| self.resolve_handler_clause(clause))
                    .collect(),
            },
            Expr::With { handler, body } => ResolvedExpr::With {
                handler: Box::new(self.resolve_expr(handler)),
                body: self.resolve_stmt_sequence(body, ScopeMode::Child),
            },
            Expr::Resume { value } => ResolvedExpr::Resume {
                value: Box::new(self.resolve_expr(value)),
            },
            Expr::Block(stmts) => {
                ResolvedExpr::Block(self.resolve_stmt_sequence(stmts, ScopeMode::Child))
            }
            Expr::Case { scrutinee, arms } => ResolvedExpr::Case {
                scrutinee: Box::new(self.resolve_expr(scrutinee)),
                arms: arms.iter().map(|arm| self.resolve_case_arm(arm)).collect(),
            },
            Expr::If {
                condition,
                then_expr,
                else_expr,
            } => ResolvedExpr::If {
                condition: Box::new(self.resolve_expr(condition)),
                then_expr: Box::new(self.resolve_expr(then_expr)),
                else_expr: Box::new(self.resolve_expr(else_expr)),
            },
            Expr::ListIndex { list, index } => self.resolve_list_index(list, index),
        }
    }

    fn resolve_list_index(&mut self, list: &Expr, index: &Expr) -> ResolvedExpr {
        let callee = ResolvedExpr::Call {
            callee: Box::new(ResolvedExpr::Ref(ResolvedRef::Helper {
                module: "list".to_string(),
                name: "get".to_string(),
            })),
            arg: Box::new(self.resolve_expr(list)),
            span: None,
        };
        ResolvedExpr::Call {
            callee: Box::new(callee),
            arg: Box::new(self.resolve_expr(index)),
            span: None,
        }
    }

    fn resolve_handler_clause(&mut self, clause: &HandlerClause) -> ResolvedHandlerClause {
        self.push_scope();
        for param in &clause.params {
            self.bind_local(param.clone());
        }
        let body = self.resolve_stmt_sequence(
            clause.parsed_body.as_deref().unwrap_or_default(),
            ScopeMode::Current,
        );
        self.pop_scope();
        ResolvedHandlerClause {
            name: clause.name.clone(),
            params: clause.params.clone(),
            body,
        }
    }

    fn resolve_case_arm(&mut self, arm: &CaseArm) -> ResolvedCaseArm {
        ResolvedCaseArm {
            pattern: arm.pattern.clone(),
            body: Box::new(self.resolve_expr(arm.body.as_ref())),
            span: arm.span,
        }
    }

    fn resolve_name(&self, name: &str) -> ResolvedRef {
        if self.is_local(name) {
            return ResolvedRef::Local(name.to_string());
        }
        if self.decl_names.contains(name) {
            return ResolvedRef::Decl(name.to_string());
        }
        if let Some(imported) = self.selective_imports.get(name) {
            return imported.clone();
        }
        if let Some(effect) = self.bare_effect_ops.get(name) {
            return ResolvedRef::EffectOp {
                effect: effect.clone(),
                op: name.to_string(),
            };
        }
        if let Some(helper) = helper_bare_ref(name) {
            return helper;
        }
        ResolvedRef::ValueName(name.to_string())
    }

    fn resolve_qualified(&self, receiver: &str, member: &str) -> ResolvedRef {
        let canonical_module = self
            .qualified_receivers
            .get(receiver)
            .map(String::as_str)
            .unwrap_or(receiver);
        if let Some(ops) = self.qualified_effect_ops.get(canonical_module)
            && ops.contains(member)
        {
            return ResolvedRef::EffectOp {
                effect: canonical_module.to_string(),
                op: member.to_string(),
            };
        }
        if let Some(helper) = helper_qualified_ref(canonical_module, member) {
            return helper;
        }
        ResolvedRef::Global {
            module: canonical_module.to_string(),
            name: member.to_string(),
        }
    }
}

#[derive(Default, Clone)]
struct ResolverMetadata {
    bare_effect_ops: HashMap<String, String>,
    qualified_effect_ops: HashMap<String, HashSet<String>>,
}

impl ResolverMetadata {
    fn collect(module: &Module, stdlib_root: &Path) -> Self {
        let resolver = StdlibResolver::new(stdlib_root.to_path_buf());
        let mut metadata = Self::default();

        for import in effective_imports(module, &resolver) {
            let Ok(resolved) = resolver.resolve_module(&import.module_path) else {
                continue;
            };
            if import.module_path == PRELUDE_MODULE_PATH {
                for export in &resolved.embedded_effect_exports {
                    let ops = export
                        .decl
                        .members
                        .iter()
                        .map(|member| member.name.clone())
                        .collect::<HashSet<_>>();
                    metadata
                        .qualified_effect_ops
                        .entry(export.effect_name.clone())
                        .or_default()
                        .extend(ops.iter().cloned());
                    match import.kind {
                        ImportKind::Plain => {
                            for op in ops {
                                metadata
                                    .bare_effect_ops
                                    .insert(op, export.effect_name.clone());
                            }
                        }
                        ImportKind::Selective(_) | ImportKind::Alias(_) => {}
                    }
                }
                if let ImportKind::Selective(names) = &import.kind {
                    for export in &resolved.embedded_effect_exports {
                        for member in &export.decl.members {
                            if names.iter().any(|selected| selected == &member.name) {
                                metadata
                                    .bare_effect_ops
                                    .insert(member.name.clone(), export.effect_name.clone());
                            }
                        }
                    }
                }
                continue;
            }

            for effect in resolved.visible_effects {
                if import_selects_name(&import.kind, &effect.decl.name) {
                    metadata
                        .qualified_effect_ops
                        .entry(effect.decl.name.clone())
                        .or_default()
                        .extend(effect.decl.members.iter().map(|member| member.name.clone()));
                }
            }
        }

        metadata
    }
}

fn module_basename(module_path: &str) -> &str {
    module_path.rsplit('/').next().unwrap_or(module_path)
}

fn imported_symbol_ref(module: &str, name: &str, metadata: &ResolverMetadata) -> ResolvedRef {
    if module == "prelude"
        && let Some(effect) = metadata.bare_effect_ops.get(name)
    {
        return ResolvedRef::EffectOp {
            effect: effect.clone(),
            op: name.to_string(),
        };
    }
    helper_qualified_ref(module, name).unwrap_or_else(|| ResolvedRef::Global {
        module: module.to_string(),
        name: name.to_string(),
    })
}

fn helper_bare_ref(name: &str) -> Option<ResolvedRef> {
    match name {
        "split" => Some(ResolvedRef::Helper {
            module: "string".to_string(),
            name: "split".to_string(),
        }),
        "each" => Some(ResolvedRef::Helper {
            module: "list".to_string(),
            name: "each".to_string(),
        }),
        "get" => Some(ResolvedRef::Helper {
            module: "list".to_string(),
            name: "get".to_string(),
        }),
        _ => None,
    }
}

fn helper_qualified_ref(receiver: &str, member: &str) -> Option<ResolvedRef> {
    match (receiver, member) {
        ("string", "split") | ("list", "each") | ("list", "get") => Some(ResolvedRef::Helper {
            module: receiver.to_string(),
            name: member.to_string(),
        }),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::*;
    use crate::ast::{Expr, ImportDecl, ImportKind};

    struct TempDirGuard {
        path: PathBuf,
    }

    impl TempDirGuard {
        fn new(label: &str) -> Self {
            let nanos = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("clock should be monotonic enough for tests")
                .as_nanos();
            let path = std::env::temp_dir().join(format!(
                "goby_core_resolved_{}_{}_{}",
                label,
                std::process::id(),
                nanos
            ));
            fs::create_dir_all(&path).expect("temp directory should be creatable");
            Self { path }
        }
    }

    impl Drop for TempDirGuard {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn module_with_imports(imports: Vec<ImportDecl>, body: Vec<Stmt>) -> Module {
        Module {
            imports,
            embed_declarations: vec![],
            type_declarations: vec![],
            effect_declarations: vec![],
            declarations: vec![Declaration {
                name: "main".to_string(),
                type_annotation: None,
                params: vec![],
                body: String::new(),
                parsed_body: Some(body),
                line: 1,
                col: 1,
            }],
        }
    }

    #[test]
    fn resolves_bare_effect_op_to_semantic_identity() {
        let decl = Declaration {
            name: "main".to_string(),
            type_annotation: None,
            params: vec![],
            body: String::new(),
            parsed_body: Some(vec![Stmt::Expr(Expr::var("read"), None)]),
            line: 1,
            col: 1,
        };

        let resolved = resolve_declaration(&decl);
        assert_eq!(
            resolved.body,
            vec![ResolvedStmt::Expr(
                ResolvedExpr::Ref(ResolvedRef::EffectOp {
                    effect: "Read".to_string(),
                    op: "read".to_string(),
                }),
                None,
            )]
        );
    }

    #[test]
    fn resolves_selective_import_to_helper_identity() {
        let module = module_with_imports(
            vec![ImportDecl {
                module_path: "goby/string".to_string(),
                kind: ImportKind::Selective(vec!["split".to_string()]),
                module_path_span: None,
                kind_span: None,
            }],
            vec![Stmt::Expr(Expr::var("split"), None)],
        );

        let resolved = resolve_module(&module);
        assert_eq!(
            resolved.declarations[0].body,
            vec![ResolvedStmt::Expr(
                ResolvedExpr::Ref(ResolvedRef::Helper {
                    module: "string".to_string(),
                    name: "split".to_string(),
                }),
                None,
            )]
        );
    }

    #[test]
    fn resolves_selective_import_from_prelude_to_effect_identity() {
        let module = module_with_imports(
            vec![ImportDecl {
                module_path: "goby/prelude".to_string(),
                kind: ImportKind::Selective(vec!["read".to_string(), "print".to_string()]),
                module_path_span: None,
                kind_span: None,
            }],
            vec![
                Stmt::Expr(Expr::var("read"), None),
                Stmt::Expr(Expr::var("print"), None),
            ],
        );

        let resolved = resolve_module(&module);
        assert_eq!(
            resolved.declarations[0].body,
            vec![
                ResolvedStmt::Expr(
                    ResolvedExpr::Ref(ResolvedRef::EffectOp {
                        effect: "Read".to_string(),
                        op: "read".to_string(),
                    }),
                    None,
                ),
                ResolvedStmt::Expr(
                    ResolvedExpr::Ref(ResolvedRef::EffectOp {
                        effect: "Print".to_string(),
                        op: "print".to_string(),
                    }),
                    None,
                ),
            ]
        );
    }

    #[test]
    fn resolves_bare_effect_op_via_prelude_embed_imported_from_stdio() {
        let sandbox = TempDirGuard::new("prelude_embed_imported_stdio");
        let stdlib_root = sandbox.path.join("stdlib");
        fs::create_dir_all(stdlib_root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            stdlib_root.join("goby/stdio.gb"),
            "effect Print\n  print : String -> Unit\n  println : String -> Unit\n",
        )
        .expect("stdio file should be writable");
        fs::write(
            stdlib_root.join("goby/prelude.gb"),
            "import goby/stdio\n@embed Print __goby_embeded_effect_stdout_handler\n",
        )
        .expect("prelude file should be writable");

        let decl = Declaration {
            name: "main".to_string(),
            type_annotation: None,
            params: vec![],
            body: String::new(),
            parsed_body: Some(vec![Stmt::Expr(Expr::var("print"), None)]),
            line: 1,
            col: 1,
        };

        let resolved = resolve_declaration_with_stdlib(&decl, &stdlib_root);
        assert_eq!(
            resolved.body,
            vec![ResolvedStmt::Expr(
                ResolvedExpr::Ref(ResolvedRef::EffectOp {
                    effect: "Print".to_string(),
                    op: "print".to_string(),
                }),
                None,
            )]
        );
    }

    #[test]
    fn resolves_qualified_effect_op_via_explicit_imported_effect_owner() {
        let sandbox = TempDirGuard::new("qualified_imported_effect_owner");
        let stdlib_root = sandbox.path.join("stdlib");
        fs::create_dir_all(stdlib_root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            stdlib_root.join("goby/stdio.gb"),
            "effect Print\n  print : String -> Unit\n  println : String -> Unit\n",
        )
        .expect("stdio file should be writable");

        let module = module_with_imports(
            vec![ImportDecl {
                module_path: "goby/stdio".to_string(),
                kind: ImportKind::Plain,
                module_path_span: None,
                kind_span: None,
            }],
            vec![Stmt::Expr(Expr::qualified("Print", "print"), None)],
        );

        let resolved = resolve_module_with_stdlib(&module, &stdlib_root);
        assert_eq!(
            resolved.declarations[0].body,
            vec![ResolvedStmt::Expr(
                ResolvedExpr::Ref(ResolvedRef::EffectOp {
                    effect: "Print".to_string(),
                    op: "print".to_string(),
                }),
                None,
            )]
        );
    }

    #[test]
    fn local_binding_shadows_bare_prelude_name() {
        let decl = Declaration {
            name: "main".to_string(),
            type_annotation: None,
            params: vec![],
            body: String::new(),
            parsed_body: Some(vec![
                Stmt::Binding {
                    name: "read".to_string(),
                    value: Expr::IntLit(1),
                    span: None,
                },
                Stmt::Expr(Expr::var("read"), None),
            ]),
            line: 1,
            col: 1,
        };

        let resolved = resolve_declaration(&decl);
        assert_eq!(
            resolved.body[1],
            ResolvedStmt::Expr(
                ResolvedExpr::Ref(ResolvedRef::Local("read".to_string())),
                None,
            )
        );
    }

    #[test]
    fn resolves_alias_qualified_helper_to_canonical_module() {
        let module = module_with_imports(
            vec![ImportDecl {
                module_path: "goby/list".to_string(),
                kind: ImportKind::Alias("l".to_string()),
                module_path_span: None,
                kind_span: None,
            }],
            vec![Stmt::Expr(Expr::qualified("l", "get"), None)],
        );

        let resolved = resolve_module(&module);
        assert_eq!(
            resolved.declarations[0].body,
            vec![ResolvedStmt::Expr(
                ResolvedExpr::Ref(ResolvedRef::Helper {
                    module: "list".to_string(),
                    name: "get".to_string(),
                }),
                None,
            )]
        );
    }

    #[test]
    fn resolves_list_index_to_canonical_list_get_call() {
        let decl = Declaration {
            name: "main".to_string(),
            type_annotation: None,
            params: vec![],
            body: String::new(),
            parsed_body: Some(vec![Stmt::Expr(
                Expr::ListIndex {
                    list: Box::new(Expr::var("lines")),
                    index: Box::new(Expr::IntLit(1)),
                },
                None,
            )]),
            line: 1,
            col: 1,
        };

        let resolved = resolve_declaration(&decl);
        assert_eq!(
            resolved.body,
            vec![ResolvedStmt::Expr(
                ResolvedExpr::Call {
                    callee: Box::new(ResolvedExpr::Call {
                        callee: Box::new(ResolvedExpr::Ref(ResolvedRef::Helper {
                            module: "list".to_string(),
                            name: "get".to_string(),
                        })),
                        arg: Box::new(ResolvedExpr::Ref(ResolvedRef::ValueName(
                            "lines".to_string(),
                        ))),
                        span: None,
                    }),
                    arg: Box::new(ResolvedExpr::IntLit(1)),
                    span: None,
                },
                None,
            )]
        );
    }

    /// Verify that `AssignTarget::ListIndex` is resolved to `ResolvedTarget::ListIndex`
    /// (NOT desugared into a `list.get` call chain as `Expr::ListIndex` would be).
    #[test]
    fn resolve_assign_target_list_index_does_not_desugar_to_list_get() {
        use crate::ast::{AssignTarget, Stmt};
        let decl = Declaration {
            name: "main".to_string(),
            type_annotation: None,
            params: vec![],
            body: String::new(),
            parsed_body: Some(vec![
                Stmt::MutBinding {
                    name: "xs".to_string(),
                    value: Expr::ListLit {
                        elements: vec![Expr::IntLit(1), Expr::IntLit(2)],
                        spread: None,
                    },
                    span: None,
                },
                Stmt::Assign {
                    target: AssignTarget::ListIndex {
                        base: Box::new(AssignTarget::Var("xs".to_string())),
                        index: Box::new(Expr::IntLit(0)),
                    },
                    value: Expr::IntLit(99),
                    span: None,
                },
            ]),
            line: 1,
            col: 1,
        };
        let resolved = resolve_declaration(&decl);

        // The Assign stmt target must be ResolvedTarget::ListIndex, not a call to list.get.
        match &resolved.body[1] {
            ResolvedStmt::Assign { target, .. } => {
                match target {
                    ResolvedTarget::ListIndex { base, index } => {
                        // base must be Var(Local("xs")), not a list.get call.
                        assert!(
                            matches!(base.as_ref(), ResolvedTarget::Var(ResolvedRef::Local(n)) if n == "xs"),
                            "base should be Var(Local(\"xs\")), got {:?}",
                            base
                        );
                        // index must be IntLit(0).
                        assert!(
                            matches!(index.as_ref(), ResolvedExpr::IntLit(0)),
                            "index should be IntLit(0), got {:?}",
                            index
                        );
                    }
                    other => panic!(
                        "expected ResolvedTarget::ListIndex, got {:?}",
                        other
                    ),
                }
            }
            other => panic!("expected Assign stmt, got {:?}", other),
        }
    }
}
