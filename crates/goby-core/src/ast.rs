#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub imports: Vec<ImportDecl>,
    pub type_declarations: Vec<TypeDeclaration>,
    pub effect_declarations: Vec<EffectDecl>,
    pub handler_declarations: Vec<HandlerDecl>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectDecl {
    pub name: String,
    pub members: Vec<EffectMember>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectMember {
    pub name: String,
    pub type_annotation: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandlerDecl {
    pub name: String,
    pub effect: String,
    pub methods: Vec<HandlerMethod>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandlerMethod {
    pub name: String,
    pub params: Vec<String>,
    pub body: String,
    /// Pre-parsed body statements; `None` if the body failed to parse.
    pub parsed_body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDecl {
    pub module_path: String,
    pub kind: ImportKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportKind {
    Plain,
    Alias(String),
    Selective(Vec<String>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDeclaration {
    Alias {
        name: String,
        target: String,
    },
    Union {
        name: String,
        constructors: Vec<String>,
    },
    Record {
        name: String,
        constructor: String,
        fields: Vec<RecordField>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordField {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Declaration {
    pub name: String,
    pub type_annotation: Option<String>,
    /// Parameter names extracted from the definition line (e.g. `f x y = ...` → `["x", "y"]`).
    pub params: Vec<String>,
    pub body: String,
    pub parsed_body: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Mul,
    Eq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CasePattern {
    IntLit(i64),
    StringLit(String),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseArm {
    pub pattern: CasePattern,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    IntLit(i64),
    BoolLit(bool),
    StringLit(String),
    ListLit(Vec<Expr>),
    TupleLit(Vec<Expr>),
    Var(String),
    Qualified {
        receiver: String,
        member: String,
    },
    RecordConstruct {
        constructor: String,
        fields: Vec<(String, Expr)>,
    },
    BinOp {
        op: BinOpKind,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arg: Box<Expr>,
    },
    MethodCall {
        receiver: String,
        method: String,
        args: Vec<Expr>,
    },
    Pipeline {
        value: Box<Expr>,
        callee: String,
    },
    Lambda {
        param: String,
        body: Box<Expr>,
    },
    Case {
        scrutinee: Box<Expr>,
        arms: Vec<CaseArm>,
    },
    If {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Binding { name: String, value: Expr },
    Expr(Expr),
    Using {
        handlers: Vec<String>,
        body: Vec<Stmt>,
    },
}

impl Expr {
    /// Returns true if this expression needs parentheses when used as a
    /// sub-expression (e.g. as an argument or operand) to preserve meaning.
    fn needs_parens_as_subexpr(&self) -> bool {
        matches!(
            self,
            Expr::BinOp { .. }
                | Expr::Call { .. }
                | Expr::RecordConstruct { .. }
                | Expr::MethodCall { .. }
                | Expr::Pipeline { .. }
                | Expr::Lambda { .. }
                | Expr::Case { .. }
                | Expr::If { .. }
        )
    }

    /// Convert an `Expr` back to a source-like string representation.
    /// Used as a bridge to the legacy string-based evaluator during the migration.
    /// Returns `None` if the expression cannot be represented as a simple string.
    pub fn to_str_repr(&self) -> Option<String> {
        match self {
            Expr::IntLit(n) => Some(n.to_string()),
            Expr::BoolLit(v) => Some(if *v { "True" } else { "False" }.to_string()),
            Expr::StringLit(s) => Some(format!("\"{}\"", s)),
            Expr::Var(name) => Some(name.clone()),
            Expr::Qualified { receiver, member } => Some(format!("{}.{}", receiver, member)),
            Expr::RecordConstruct {
                constructor,
                fields,
            } => {
                let parts: Option<Vec<String>> = fields
                    .iter()
                    .map(|(name, value)| Some(format!("{}: {}", name, value.to_str_repr()?)))
                    .collect();
                Some(format!("{}({})", constructor, parts?.join(", ")))
            }
            Expr::ListLit(items) => {
                let parts: Option<Vec<String>> = items.iter().map(|i| i.to_str_repr()).collect();
                Some(format!("[{}]", parts?.join(", ")))
            }
            Expr::TupleLit(items) => {
                let parts: Option<Vec<String>> = items.iter().map(|i| i.to_str_repr()).collect();
                Some(format!("({})", parts?.join(", ")))
            }
            Expr::BinOp { op, left, right } => {
                let op_str = match op {
                    BinOpKind::Add => "+",
                    BinOpKind::Mul => "*",
                    BinOpKind::Eq => "==",
                };
                let l_raw = left.to_str_repr()?;
                let r_raw = right.to_str_repr()?;
                let l = if left.needs_parens_as_subexpr() {
                    format!("({})", l_raw)
                } else {
                    l_raw
                };
                let r = if right.needs_parens_as_subexpr() {
                    format!("({})", r_raw)
                } else {
                    r_raw
                };
                Some(format!("{} {} {}", l, op_str, r))
            }
            Expr::Call { callee, arg } => {
                let c = callee.to_str_repr()?;
                let a_raw = arg.to_str_repr()?;
                let a = if arg.needs_parens_as_subexpr() {
                    format!("({})", a_raw)
                } else {
                    a_raw
                };
                Some(format!("{} {}", c, a))
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let arg_strs: Option<Vec<String>> = args.iter().map(|a| a.to_str_repr()).collect();
                Some(format!("{}.{}({})", receiver, method, arg_strs?.join(", ")))
            }
            Expr::Pipeline { value, callee } => {
                let v = value.to_str_repr()?;
                Some(format!("{} |> {}", v, callee))
            }
            Expr::Lambda { param, body } => {
                if param == "_" {
                    // Placeholder form: reconstruct as `_ op rhs`
                    body.to_str_repr()
                } else {
                    let b = body.to_str_repr()?;
                    Some(format!("|{}| -> {}", param, b))
                }
            }
            Expr::Case { .. } | Expr::If { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_str_repr_wraps_binop_arg_in_parens() {
        // `double (1 + 2)` — the BinOp argument must be parenthesised so that
        // the legacy evaluator does not see `double 1 + 2` (wrong precedence).
        let expr = Expr::Call {
            callee: Box::new(Expr::Var("double".to_string())),
            arg: Box::new(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::IntLit(1)),
                right: Box::new(Expr::IntLit(2)),
            }),
        };
        assert_eq!(expr.to_str_repr(), Some("double (1 + 2)".to_string()));
    }

    #[test]
    fn to_str_repr_wraps_nested_call_arg_in_parens() {
        // `print (double (1 + 2))` — both the outer and inner arg need parens.
        let inner = Expr::Call {
            callee: Box::new(Expr::Var("double".to_string())),
            arg: Box::new(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::IntLit(1)),
                right: Box::new(Expr::IntLit(2)),
            }),
        };
        let outer = Expr::Call {
            callee: Box::new(Expr::Var("print".to_string())),
            arg: Box::new(inner),
        };
        assert_eq!(
            outer.to_str_repr(),
            Some("print (double (1 + 2))".to_string())
        );
    }

    #[test]
    fn to_str_repr_wraps_method_call_arg_in_parens() {
        // `f string.concat("a", "b")` — MethodCall as a Call argument must be
        // wrapped so the legacy evaluator does not misparse the expression.
        let expr = Expr::Call {
            callee: Box::new(Expr::Var("f".to_string())),
            arg: Box::new(Expr::MethodCall {
                receiver: "string".to_string(),
                method: "concat".to_string(),
                args: vec![
                    Expr::StringLit("a".to_string()),
                    Expr::StringLit("b".to_string()),
                ],
            }),
        };
        assert_eq!(
            expr.to_str_repr(),
            Some("f (string.concat(\"a\", \"b\"))".to_string())
        );
    }

    #[test]
    fn to_str_repr_binop_operands_get_parens_when_needed() {
        // `(a + b) * c` — the left BinOp operand must be wrapped.
        let expr = Expr::BinOp {
            op: BinOpKind::Mul,
            left: Box::new(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::Var("a".to_string())),
                right: Box::new(Expr::Var("b".to_string())),
            }),
            right: Box::new(Expr::Var("c".to_string())),
        };
        assert_eq!(expr.to_str_repr(), Some("(a + b) * c".to_string()));
    }
}
