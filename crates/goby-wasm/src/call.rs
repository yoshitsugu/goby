use goby_core::{Declaration, Expr, Module};

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
            Expr::Call { callee, arg, .. } => {
                args.push(arg.as_ref());
                cur = callee.as_ref();
            }
            Expr::Var { name, .. } => {
                args.reverse();
                return Some((name.as_str(), args));
            }
            _ => return None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DirectCallTargetError {
    NotDeclaration,
    ArityMismatch,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PrintCallError {
    NotPrint,
    ArityNotOne,
    NonDirectCallee,
}

pub(crate) fn resolve_direct_call_target<'a>(
    module: &'a Module,
    name: &str,
    arity: usize,
) -> Result<&'a Declaration, DirectCallTargetError> {
    let Some(decl) = module.declarations.iter().find(|d| d.name == name) else {
        return Err(DirectCallTargetError::NotDeclaration);
    };
    if decl.params.len() != arity {
        return Err(DirectCallTargetError::ArityMismatch);
    }
    Ok(decl)
}

pub(crate) fn extract_direct_print_call_arg(expr: &Expr) -> Result<&Expr, PrintCallError> {
    let Some((name, args)) = flatten_named_call(expr) else {
        return Err(PrintCallError::NonDirectCallee);
    };
    if name != "print" {
        return Err(PrintCallError::NotPrint);
    }
    if args.len() != 1 {
        return Err(PrintCallError::ArityNotOne);
    }
    Ok(args[0])
}

#[cfg(test)]
mod tests {
    use goby_core::{Expr, Stmt, parse_module};

    use super::*;

    fn parse_expr(source: &str) -> Expr {
        let module = parse_module(source).expect("source should parse");
        let main = module
            .declarations
            .iter()
            .find(|d| d.name == "main")
            .expect("main declaration should exist");
        let stmts = main
            .parsed_body
            .as_deref()
            .expect("main parsed body should exist");
        match &stmts[0] {
            Stmt::Expr(expr) => expr.clone(),
            other => panic!("expected first stmt to be expression, got {:?}", other),
        }
    }

    #[test]
    fn flatten_named_call_flattens_multi_arg_chain() {
        let expr = parse_expr(
            r#"
main : Unit -> Unit
main =
  add 1 2 3
"#,
        );
        let (name, args) = flatten_named_call(&expr).expect("call should flatten");
        assert_eq!(name, "add");
        assert_eq!(args.len(), 3);
    }

    #[test]
    fn resolve_direct_call_target_reports_expected_outcomes() {
        let source = r#"
add : Int -> Int -> Int
add a b = a + b

main : Unit -> Unit
main =
  add 1 2
"#;
        let module = parse_module(source).expect("source should parse");
        assert!(resolve_direct_call_target(&module, "add", 2).is_ok());
        assert_eq!(
            resolve_direct_call_target(&module, "missing", 1),
            Err(DirectCallTargetError::NotDeclaration)
        );
        assert_eq!(
            resolve_direct_call_target(&module, "add", 1),
            Err(DirectCallTargetError::ArityMismatch)
        );
    }

    #[test]
    fn extract_direct_print_call_arg_reports_expected_outcomes() {
        let source = r#"
id : Int -> Int
id x = x

main : Unit -> Unit
main =
  print (id 1)
"#;
        let expr = parse_expr(source);
        assert!(extract_direct_print_call_arg(&expr).is_ok());

        let not_print = parse_expr(
            r#"
id : Int -> Int
id x = x

main : Unit -> Unit
main =
  id 1
"#,
        );
        assert_eq!(
            extract_direct_print_call_arg(&not_print),
            Err(PrintCallError::NotPrint)
        );

        let arity_two = parse_expr(
            r#"
main : Unit -> Unit
main =
  print 1 2
"#,
        );
        assert_eq!(
            extract_direct_print_call_arg(&arity_two),
            Err(PrintCallError::ArityNotOne)
        );

        let non_direct = parse_expr(
            r#"
main : Unit -> Unit
main =
  (Foo.bar 1)
"#,
        );
        assert_eq!(
            extract_direct_print_call_arg(&non_direct),
            Err(PrintCallError::NonDirectCallee)
        );
    }
}
