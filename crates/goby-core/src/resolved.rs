//! Resolved front-end form for AST-to-IR lowering.
//!
//! This layer sits between the parsed AST and the shared IR. Its job is to
//! normalize callable/effect identity before `ir_lower` constructs shared IR.

use std::collections::{HashMap, HashSet};

use crate::ast::{
    BinOpKind, CaseArm, CasePattern, Declaration, Expr, HandlerClause, ImportKind,
    InterpolatedPart, Module, Span, Stmt, UnaryOpKind,
};

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
        target: ResolvedRef,
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
    let decl_names = declaration_names(module);
    ResolvedModule {
        declarations: module
            .declarations
            .iter()
            .map(|decl| resolve_declaration_with_names(module, &decl_names, decl))
            .collect(),
    }
}

pub fn resolve_declaration(decl: &Declaration) -> ResolvedDeclaration {
    let module = Module {
        imports: vec![],
        embed_declarations: vec![],
        type_declarations: vec![],
        effect_declarations: vec![],
        declarations: vec![decl.clone()],
    };
    let decl_names = declaration_names(&module);
    resolve_declaration_with_names(&module, &decl_names, decl)
}

fn resolve_declaration_with_names(
    module: &Module,
    decl_names: &HashSet<String>,
    decl: &Declaration,
) -> ResolvedDeclaration {
    let mut resolver = Resolver::new(module, decl_names);
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
    scopes: Vec<HashSet<String>>,
}

impl Resolver {
    fn new(module: &Module, decl_names: &HashSet<String>) -> Self {
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
                        selective_imports
                            .insert(name.clone(), imported_symbol_ref(&canonical_module, name));
                    }
                }
            }
        }

        Self {
            decl_names: decl_names.clone(),
            selective_imports,
            qualified_receivers,
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
            Stmt::Assign { name, value, span } => ResolvedStmt::Assign {
                target: self.resolve_name(name),
                value: self.resolve_expr(value),
                span: *span,
            },
            Stmt::Expr(expr, span) => ResolvedStmt::Expr(self.resolve_expr(expr), *span),
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> ResolvedExpr {
        match expr {
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
        if let Some(builtin) = builtin_bare_ref(name) {
            return builtin;
        }
        ResolvedRef::ValueName(name.to_string())
    }

    fn resolve_qualified(&self, receiver: &str, member: &str) -> ResolvedRef {
        let canonical_module = self
            .qualified_receivers
            .get(receiver)
            .map(String::as_str)
            .unwrap_or(receiver);
        if let Some(builtin) = builtin_qualified_ref(canonical_module, member) {
            return builtin;
        }
        ResolvedRef::Global {
            module: canonical_module.to_string(),
            name: member.to_string(),
        }
    }
}

fn module_basename(module_path: &str) -> &str {
    module_path.rsplit('/').next().unwrap_or(module_path)
}

fn imported_symbol_ref(module: &str, name: &str) -> ResolvedRef {
    if module == "prelude"
        && let Some(builtin) = builtin_bare_ref(name)
    {
        return builtin;
    }
    builtin_qualified_ref(module, name).unwrap_or_else(|| ResolvedRef::Global {
        module: module.to_string(),
        name: name.to_string(),
    })
}

fn builtin_bare_ref(name: &str) -> Option<ResolvedRef> {
    match name {
        "read" => Some(ResolvedRef::EffectOp {
            effect: "Read".to_string(),
            op: "read".to_string(),
        }),
        "read_line" => Some(ResolvedRef::EffectOp {
            effect: "Read".to_string(),
            op: "read_line".to_string(),
        }),
        "read_lines" => Some(ResolvedRef::EffectOp {
            effect: "Read".to_string(),
            op: "read_lines".to_string(),
        }),
        "print" => Some(ResolvedRef::EffectOp {
            effect: "Print".to_string(),
            op: "print".to_string(),
        }),
        "println" => Some(ResolvedRef::EffectOp {
            effect: "Print".to_string(),
            op: "println".to_string(),
        }),
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

fn builtin_qualified_ref(receiver: &str, member: &str) -> Option<ResolvedRef> {
    match (receiver, member) {
        ("Read", "read") | ("Read", "read_line") | ("Read", "read_lines") => {
            Some(ResolvedRef::EffectOp {
                effect: "Read".to_string(),
                op: member.to_string(),
            })
        }
        ("Print", "print") | ("Print", "println") => Some(ResolvedRef::EffectOp {
            effect: "Print".to_string(),
            op: member.to_string(),
        }),
        ("string", "split") | ("list", "each") | ("list", "get") => Some(ResolvedRef::Helper {
            module: receiver.to_string(),
            name: member.to_string(),
        }),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, ImportDecl, ImportKind};

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
}
