use crate::ir::{IrInterpPart, IrModule, ValueExpr};

/// Returns true when `value` is a closed aggregate literal that can be hoisted.
///
/// Current M1 implementation is intentionally conservative:
/// - accepts `ListLit` / `TupleLit` / `RecordLit`
/// - requires list literals to have no spread tail
/// - allows only scalar literal leaves (`IntLit`, `BoolLit`, `StrLit`, `Unit`)
///   or nested closed aggregate literals
///
/// This is a strict subset of the broader Perceus-plan predicate and keeps the
/// static-arena emitter simple for the first slice.
pub fn is_closed_literal(value: &ValueExpr) -> bool {
    matches!(
        value,
        ValueExpr::ListLit { .. } | ValueExpr::TupleLit(_) | ValueExpr::RecordLit { .. }
    ) && closed_literal_shape(value)
}

/// Collect every hoistable closed literal reachable from the module.
pub fn collect_closed_literals(module: &IrModule) -> Vec<ValueExpr> {
    let mut out = Vec::new();
    for decl in &module.decls {
        collect_closed_literals_in_comp(&decl.body, &mut out);
    }
    out
}

fn closed_literal_shape(value: &ValueExpr) -> bool {
    match value {
        ValueExpr::IntLit(_) | ValueExpr::BoolLit(_) | ValueExpr::StrLit(_) | ValueExpr::Unit => {
            true
        }
        ValueExpr::ListLit { elements, spread } => {
            spread.is_none() && elements.iter().all(closed_literal_shape)
        }
        ValueExpr::TupleLit(items) => items.iter().all(closed_literal_shape),
        ValueExpr::RecordLit { fields, .. } => {
            fields.iter().all(|(_, value)| closed_literal_shape(value))
        }
        ValueExpr::Interp(parts) => parts.iter().all(|part| match part {
            IrInterpPart::Text(_) => true,
            IrInterpPart::Expr(value) => closed_literal_shape(value),
        }),
        ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Lambda { .. }
        | ValueExpr::BinOp { .. }
        | ValueExpr::TupleProject { .. }
        | ValueExpr::ListGet { .. } => false,
    }
}

fn collect_closed_literals_in_comp(comp: &crate::ir::CompExpr, out: &mut Vec<ValueExpr>) {
    use crate::ir::CompExpr;

    match comp {
        CompExpr::Value(value) => collect_closed_literals_in_value(value, out),
        CompExpr::Let { value, body, .. } | CompExpr::LetMut { value, body, .. } => {
            collect_closed_literals_in_comp(value, out);
            collect_closed_literals_in_comp(body, out);
        }
        CompExpr::Seq { stmts, tail } => {
            for stmt in stmts {
                collect_closed_literals_in_comp(stmt, out);
            }
            collect_closed_literals_in_comp(tail, out);
        }
        CompExpr::If { cond, then_, else_ } => {
            collect_closed_literals_in_value(cond, out);
            collect_closed_literals_in_comp(then_, out);
            collect_closed_literals_in_comp(else_, out);
        }
        CompExpr::Call { callee, args } => {
            collect_closed_literals_in_value(callee, out);
            for arg in args {
                collect_closed_literals_in_value(arg, out);
            }
        }
        CompExpr::Assign { value, .. } => collect_closed_literals_in_comp(value, out),
        CompExpr::AssignIndex { path, value, .. } => {
            for item in path {
                collect_closed_literals_in_value(item, out);
            }
            collect_closed_literals_in_comp(value, out);
        }
        CompExpr::Case { scrutinee, arms } => {
            collect_closed_literals_in_value(scrutinee, out);
            for arm in arms {
                collect_closed_literals_in_comp(&arm.body, out);
            }
        }
        CompExpr::PerformEffect { args, .. } => {
            for arg in args {
                collect_closed_literals_in_value(arg, out);
            }
        }
        CompExpr::Handle { clauses } => {
            for clause in clauses {
                collect_closed_literals_in_comp(&clause.body, out);
            }
        }
        CompExpr::WithHandler { handler, body } => {
            collect_closed_literals_in_comp(handler, out);
            collect_closed_literals_in_comp(body, out);
        }
        CompExpr::Resume { value } => collect_closed_literals_in_value(value, out),
    }
}

fn collect_closed_literals_in_value(value: &ValueExpr, out: &mut Vec<ValueExpr>) {
    if is_closed_literal(value) {
        out.push(value.clone());
    }
    match value {
        ValueExpr::ListLit { elements, spread } => {
            for value in elements {
                collect_closed_literals_in_value(value, out);
            }
            if let Some(tail) = spread {
                collect_closed_literals_in_value(tail, out);
            }
        }
        ValueExpr::TupleLit(items) => {
            for value in items {
                collect_closed_literals_in_value(value, out);
            }
        }
        ValueExpr::RecordLit { fields, .. } => {
            for (_, value) in fields {
                collect_closed_literals_in_value(value, out);
            }
        }
        ValueExpr::Lambda { body, .. } => collect_closed_literals_in_comp(body, out),
        ValueExpr::Interp(parts) => {
            for part in parts {
                if let IrInterpPart::Expr(value) = part {
                    collect_closed_literals_in_value(value, out);
                }
            }
        }
        ValueExpr::BinOp { left, right, .. } => {
            collect_closed_literals_in_value(left, out);
            collect_closed_literals_in_value(right, out);
        }
        ValueExpr::TupleProject { tuple, .. } => collect_closed_literals_in_value(tuple, out),
        ValueExpr::ListGet { list, index } => {
            collect_closed_literals_in_value(list, out);
            collect_closed_literals_in_value(index, out);
        }
        ValueExpr::IntLit(_)
        | ValueExpr::BoolLit(_)
        | ValueExpr::StrLit(_)
        | ValueExpr::Var(_)
        | ValueExpr::GlobalRef { .. }
        | ValueExpr::Unit => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::{CompExpr, IrDecl, IrModule, IrType, ValueExpr};

    use super::{collect_closed_literals, is_closed_literal};

    #[test]
    fn detects_nested_closed_record_literal() {
        let value = ValueExpr::RecordLit {
            constructor: "Pair".to_string(),
            fields: vec![
                (
                    "left".to_string(),
                    ValueExpr::TupleLit(vec![ValueExpr::IntLit(1), ValueExpr::BoolLit(true)]),
                ),
                (
                    "right".to_string(),
                    ValueExpr::ListLit {
                        elements: vec![ValueExpr::StrLit("x".to_string())],
                        spread: None,
                    },
                ),
            ],
        };

        assert!(is_closed_literal(&value));
    }

    #[test]
    fn rejects_list_spread_and_var_references() {
        let spread = ValueExpr::ListLit {
            elements: vec![ValueExpr::IntLit(1)],
            spread: Some(Box::new(ValueExpr::Var("rest".to_string()))),
        };
        let with_var = ValueExpr::TupleLit(vec![ValueExpr::Var("x".to_string())]);

        assert!(!is_closed_literal(&spread));
        assert!(!is_closed_literal(&with_var));
    }

    #[test]
    fn collects_closed_literals_from_module() {
        let module = IrModule {
            decls: vec![IrDecl {
                name: "main".to_string(),
                params: Vec::new(),
                result_ty: IrType::Unit,
                residual_effects: Vec::new(),
                body: CompExpr::Value(ValueExpr::TupleLit(vec![
                    ValueExpr::IntLit(1),
                    ValueExpr::ListLit {
                        elements: vec![ValueExpr::BoolLit(false)],
                        spread: None,
                    },
                ])),
            }],
        };

        let found = collect_closed_literals(&module);
        assert_eq!(found.len(), 2);
    }
}
