/// Source location within a source file (or body sub-string — see individual fields).
///
/// Represents a range from `(line, col)` to `(end_line, end_col)`.
/// For point spans (single position), `end_line == line` and `end_col == col`.
///
/// # Column-1 sentinel convention
/// `TypecheckError` uses `col = 1` as a sentinel meaning "unknown column" when no
/// precise location is available. This convention does **not** apply to parser-produced
/// spans: `col = 1` in a parser-produced `Span` always means the first byte of the line.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// 1-indexed line number of the start position.
    pub line: usize,
    /// 1-indexed byte offset within the start line.
    pub col: usize,
    /// 1-indexed line number of the end position.
    pub end_line: usize,
    /// 1-indexed byte offset within the end line.
    pub end_col: usize,
}

impl Span {
    /// Create a point span (start == end) at the given line and column.
    pub fn point(line: usize, col: usize) -> Self {
        Self {
            line,
            col,
            end_line: line,
            end_col: col,
        }
    }

    /// Create a range span from start to end positions.
    pub fn new(line: usize, col: usize, end_line: usize, end_col: usize) -> Self {
        Self {
            line,
            col,
            end_line,
            end_col,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub imports: Vec<ImportDecl>,
    pub embed_declarations: Vec<EmbedDecl>,
    pub type_declarations: Vec<TypeDeclaration>,
    pub effect_declarations: Vec<EffectDecl>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmbedDecl {
    pub effect_name: String,
    pub handler_name: String,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectDecl {
    pub name: String,
    pub type_params: Vec<String>,
    pub members: Vec<EffectMember>,
    /// Span of the `effect <Name>` header line.
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectMember {
    pub name: String,
    pub type_annotation: String,
    /// Span of this effect member line.
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandlerClause {
    pub name: String,
    pub params: Vec<String>,
    pub body: String,
    /// Pre-parsed body statements; `None` if the body failed to parse.
    pub parsed_body: Option<Vec<Stmt>>,
    /// Span of the handler clause header line (e.g. `name params ->`).
    ///
    /// **Note:** line/col are relative to the declaration *body sub-string*, not the
    /// original source file. The body sub-string starts at the line following the
    /// definition line (the line with `name params = ...`). To obtain a source-file
    /// line number, add the 1-indexed line number of the *definition line* (not
    /// `Declaration.line`, which may point to the type annotation instead).
    pub span: Span,
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
    /// The field's type as a raw annotation string (e.g. `"Int"`, `"List String"`).
    pub type_annotation: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Declaration {
    pub name: String,
    pub type_annotation: Option<String>,
    /// Parameter names extracted from the definition line (e.g. `f x y = ...` → `["x", "y"]`).
    pub params: Vec<String>,
    pub body: String,
    pub parsed_body: Option<Vec<Stmt>>,
    /// 1-indexed source line where this declaration begins (type annotation line if present,
    /// otherwise the definition line).
    pub line: usize,
    /// 1-indexed byte offset of the first non-whitespace byte on the definition line.
    /// For all currently-valid top-level declarations this is always 1 (the grammar rejects
    /// indented top-level items), but the field is provided for future use.
    pub col: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOpKind {
    And,
    Add,
    Mul,
    Eq,
    Lt,
    Gt,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ListPatternItem {
    IntLit(i64),
    StringLit(String),
    Bind(String),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ListPatternTail {
    Ignore,
    Bind(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CasePattern {
    IntLit(i64),
    StringLit(String),
    BoolLit(bool),
    EmptyList,
    ListPattern {
        items: Vec<ListPatternItem>,
        tail: Option<ListPatternTail>,
    },
    Wildcard,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseArm {
    pub pattern: CasePattern,
    pub body: Box<Expr>,
    /// Span of the `pattern ->` arm header.
    ///
    /// **Note:** line/col are relative to the declaration *body sub-string*, not the
    /// original source file. See `HandlerClause.span` for details on the coordinate system.
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InterpolatedPart {
    Text(String),
    Expr(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    IntLit(i64),
    BoolLit(bool),
    StringLit(String),
    InterpolatedString(Vec<InterpolatedPart>),
    ListLit {
        elements: Vec<Expr>,
        spread: Option<Box<Expr>>,
    },
    TupleLit(Vec<Expr>),
    Var {
        name: String,
        span: Option<Span>,
    },
    Qualified {
        receiver: String,
        member: String,
        span: Option<Span>,
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
        span: Option<Span>,
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
    Handler {
        clauses: Vec<HandlerClause>,
    },
    With {
        handler: Box<Expr>,
        body: Vec<Stmt>,
    },
    Resume {
        value: Box<Expr>,
    },
    Block(Vec<Stmt>),
    Case {
        scrutinee: Box<Expr>,
        arms: Vec<CaseArm>,
    },
    If {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    ListIndex {
        list: Box<Expr>,
        index: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Binding { name: String, value: Expr, span: Option<Span> },
    MutBinding { name: String, value: Expr, span: Option<Span> },
    Assign { name: String, value: Expr, span: Option<Span> },
    Expr(Expr, Option<Span>),
}

impl Expr {
    /// Construct a `Var` expression with no span (most common case in the parser
    /// and test code; spans are populated later where source position is known).
    pub fn var(name: impl Into<String>) -> Self {
        Expr::Var { name: name.into(), span: None }
    }

    /// Construct a `Qualified` expression with no span.
    pub fn qualified(receiver: impl Into<String>, member: impl Into<String>) -> Self {
        Expr::Qualified { receiver: receiver.into(), member: member.into(), span: None }
    }

    /// Construct a curried `Call` expression with no span.
    pub fn call(callee: Expr, arg: Expr) -> Self {
        Expr::Call { callee: Box::new(callee), arg: Box::new(arg), span: None }
    }

    pub fn unit_value() -> Self {
        Expr::TupleLit(Vec::new())
    }

    pub fn is_unit_value(&self) -> bool {
        matches!(self, Expr::TupleLit(items) if items.is_empty())
    }

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
                | Expr::Handler { .. }
                | Expr::With { .. }
                | Expr::Resume { .. }
                | Expr::Block(..)
                | Expr::Case { .. }
                | Expr::If { .. }
                | Expr::ListIndex { .. }
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
            Expr::InterpolatedString(_) => None,
            Expr::Var { name, .. } => Some(name.clone()),
            Expr::Qualified { receiver, member, .. } => Some(format!("{}.{}", receiver, member)),
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
            Expr::ListLit { elements, spread } => {
                let parts: Option<Vec<String>> = elements.iter().map(|i| i.to_str_repr()).collect();
                let mut repr = parts?.join(", ");
                if let Some(tail) = spread {
                    if !repr.is_empty() {
                        repr.push_str(", ");
                    }
                    repr.push_str(&format!("..{}", tail.to_str_repr()?));
                }
                Some(format!("[{}]", repr))
            }
            Expr::TupleLit(items) => {
                let parts: Option<Vec<String>> = items.iter().map(|i| i.to_str_repr()).collect();
                Some(format!("({})", parts?.join(", ")))
            }
            Expr::BinOp { op, left, right } => {
                let op_str = match op {
                    BinOpKind::And => "&&",
                    BinOpKind::Add => "+",
                    BinOpKind::Mul => "*",
                    BinOpKind::Eq => "==",
                    BinOpKind::Lt => "<",
                    BinOpKind::Gt => ">",
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
            Expr::Call { callee, arg, .. } => {
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
            Expr::Handler { .. } | Expr::With { .. } | Expr::Resume { .. } | Expr::Block(..) => {
                None
            }
            Expr::Case { .. } | Expr::If { .. } => None,
            // The F1b parser handles `expr[i]` syntax, so this string is parseable.
            // However, the legacy string-based evaluators (IntEvaluator,
            // eval_value_with_context, etc.) do not handle `xs[i]` notation —
            // callers that fall back to the string evaluator will receive None,
            // which is the correct safe-fallback behaviour.
            Expr::ListIndex { list, index } => {
                let l_raw = list.to_str_repr()?;
                let i = index.to_str_repr()?;
                // In the receiver position, `ListIndex` itself does NOT need parens:
                // `xs[0][1]` round-trips correctly without `(xs[0])[1]`.
                // Other complex sub-expressions (Call, BinOp, …) do need parens.
                let l = if list.needs_parens_as_subexpr()
                    && !matches!(list.as_ref(), Expr::ListIndex { .. })
                {
                    format!("({})", l_raw)
                } else {
                    l_raw
                };
                Some(format!("{}[{}]", l, i))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_point_sets_end_equal_to_start() {
        let s = Span::point(5, 3);
        assert_eq!(s.line, 5);
        assert_eq!(s.col, 3);
        assert_eq!(s.end_line, 5);
        assert_eq!(s.end_col, 3);
    }

    #[test]
    fn span_new_sets_all_fields() {
        let s = Span::new(1, 2, 3, 4);
        assert_eq!(s.line, 1);
        assert_eq!(s.col, 2);
        assert_eq!(s.end_line, 3);
        assert_eq!(s.end_col, 4);
    }

    #[test]
    fn to_str_repr_wraps_binop_arg_in_parens() {
        // `double (1 + 2)` — the BinOp argument must be parenthesised so that
        // the legacy evaluator does not see `double 1 + 2` (wrong precedence).
        let expr = Expr::call(
            Expr::var("double"),
            Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::IntLit(1)),
                right: Box::new(Expr::IntLit(2)),
            },
        );
        assert_eq!(expr.to_str_repr(), Some("double (1 + 2)".to_string()));
    }

    #[test]
    fn to_str_repr_wraps_nested_call_arg_in_parens() {
        // `print (double (1 + 2))` — both the outer and inner arg need parens.
        let inner = Expr::call(
            Expr::var("double"),
            Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::IntLit(1)),
                right: Box::new(Expr::IntLit(2)),
            },
        );
        let outer = Expr::call(Expr::var("print"), inner);
        assert_eq!(
            outer.to_str_repr(),
            Some("print (double (1 + 2))".to_string())
        );
    }

    #[test]
    fn to_str_repr_wraps_method_call_arg_in_parens() {
        // `f string.split("a", ",")` — MethodCall as a Call argument must be
        // wrapped so the legacy evaluator does not misparse the expression.
        let expr = Expr::call(
            Expr::var("f"),
            Expr::MethodCall {
                receiver: "string".to_string(),
                method: "split".to_string(),
                args: vec![
                    Expr::StringLit("a".to_string()),
                    Expr::StringLit(",".to_string()),
                ],
            },
        );
        assert_eq!(
            expr.to_str_repr(),
            Some("f (string.split(\"a\", \",\"))".to_string())
        );
    }

    #[test]
    fn to_str_repr_binop_operands_get_parens_when_needed() {
        // `(a + b) * c` — the left BinOp operand must be wrapped.
        let expr = Expr::BinOp {
            op: BinOpKind::Mul,
            left: Box::new(Expr::BinOp {
                op: BinOpKind::Add,
                left: Box::new(Expr::var("a")),
                right: Box::new(Expr::var("b")),
            }),
            right: Box::new(Expr::var("c")),
        };
        assert_eq!(expr.to_str_repr(), Some("(a + b) * c".to_string()));
    }

    #[test]
    fn to_str_repr_list_with_spread() {
        let expr = Expr::ListLit {
            elements: vec![Expr::IntLit(1), Expr::IntLit(2)],
            spread: Some(Box::new(Expr::var("xs"))),
        };
        assert_eq!(expr.to_str_repr(), Some("[1, 2, ..xs]".to_string()));
    }

    #[test]
    fn to_str_repr_list_index_simple() {
        let expr = Expr::ListIndex {
            list: Box::new(Expr::var("xs")),
            index: Box::new(Expr::IntLit(0)),
        };
        assert_eq!(expr.to_str_repr(), Some("xs[0]".to_string()));
    }

    #[test]
    fn to_str_repr_list_index_var_index() {
        let expr = Expr::ListIndex {
            list: Box::new(Expr::var("xs")),
            index: Box::new(Expr::var("i")),
        };
        assert_eq!(expr.to_str_repr(), Some("xs[i]".to_string()));
    }

    #[test]
    fn to_str_repr_list_index_call_receiver_gets_parens() {
        // `(f 1)[0]` — Call receiver must be parenthesised to avoid `f 1[0]`
        let expr = Expr::ListIndex {
            list: Box::new(Expr::call(Expr::var("f"), Expr::IntLit(1))),
            index: Box::new(Expr::IntLit(0)),
        };
        assert_eq!(expr.to_str_repr(), Some("(f 1)[0]".to_string()));
    }

    #[test]
    fn to_str_repr_list_index_binop_index_no_parens() {
        // Index expressions are delimited by `[…]` so they never need parens.
        // `xs[a + b]` must NOT become `xs[(a + b)]`.
        let expr = Expr::ListIndex {
            list: Box::new(Expr::var("xs")),
            index: Box::new(Expr::BinOp {
                op: crate::ast::BinOpKind::Add,
                left: Box::new(Expr::var("a")),
                right: Box::new(Expr::var("b")),
            }),
        };
        assert_eq!(expr.to_str_repr(), Some("xs[a + b]".to_string()));
    }

    #[test]
    fn to_str_repr_list_index_chained() {
        // `xs[0][1]`
        let inner = Expr::ListIndex {
            list: Box::new(Expr::var("xs")),
            index: Box::new(Expr::IntLit(0)),
        };
        let outer = Expr::ListIndex {
            list: Box::new(inner),
            index: Box::new(Expr::IntLit(1)),
        };
        assert_eq!(outer.to_str_repr(), Some("xs[0][1]".to_string()));
    }
}
