use goby_core::Expr;

/// Flatten left-associative call chains into `(callee_name, args)` when the
/// callee is a direct named function.
///
/// Examples:
/// - `f a` -> `("f", [a])`
/// - `f a b c` -> `("f", [a, b, c])`
///
/// Returns `None` for non-direct callee forms such as:
/// - `(g f) a`
/// - `obj.method a`
pub(crate) fn flatten_named_call(expr: &Expr) -> Option<(&str, Vec<&Expr>)> {
    let mut args = Vec::new();
    let mut cur = expr;
    loop {
        match cur {
            Expr::Call { callee, arg } => {
                args.push(arg.as_ref());
                cur = callee.as_ref();
            }
            Expr::Var(name) => {
                args.reverse();
                return Some((name.as_str(), args));
            }
            _ => return None,
        }
    }
}
