use std::collections::{HashMap, HashSet};

use goby_core::{Expr, Module, Stmt, types::strip_effect};

use crate::call::flatten_named_call;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LoweringStyle {
    DirectStyle,
    EffectBoundary,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct LoweringPlan {
    declaration_styles: HashMap<String, LoweringStyle>,
    pub(crate) handler_resume_present: bool,
}

impl LoweringPlan {
    pub(crate) fn style_for(&self, declaration_name: &str) -> Option<LoweringStyle> {
        self.declaration_styles.get(declaration_name).copied()
    }

    pub(crate) fn is_direct_style(&self, declaration_name: &str) -> bool {
        self.style_for(declaration_name) == Some(LoweringStyle::DirectStyle)
    }

    pub(crate) fn handler_resume_present(&self) -> bool {
        self.handler_resume_present
    }
}

pub(crate) fn build_lowering_plan(module: &Module) -> LoweringPlan {
    let declaration_names: HashSet<String> =
        module.declarations.iter().map(|d| d.name.clone()).collect();

    let mut declaration_styles: HashMap<String, LoweringStyle> = HashMap::new();
    let mut declaration_calls: HashMap<String, HashSet<String>> = HashMap::new();

    for decl in &module.declarations {
        let mut is_effect_boundary = false;
        if let Some(annotation) = decl.type_annotation.as_deref()
            && has_effect_clause(annotation)
        {
            is_effect_boundary = true;
        }

        let mut calls = HashSet::new();
        if let Some(stmts) = decl.parsed_body.as_deref() {
            let mut inspection = StmtInspection::default();
            inspect_stmts(stmts, &mut inspection, &declaration_names);
            if inspection.contains_using || inspection.contains_resume {
                is_effect_boundary = true;
            }
            calls = inspection.called_declarations;
        } else {
            // Missing parsed body forces boundary mode until lowering coverage expands.
            is_effect_boundary = true;
        }

        declaration_styles.insert(
            decl.name.clone(),
            if is_effect_boundary {
                LoweringStyle::EffectBoundary
            } else {
                LoweringStyle::DirectStyle
            },
        );
        declaration_calls.insert(decl.name.clone(), calls);
    }

    // Transitive propagation: callers of effect-boundary declarations become boundaries.
    loop {
        let mut changed = false;
        for (decl_name, callees) in &declaration_calls {
            if declaration_styles.get(decl_name) == Some(&LoweringStyle::EffectBoundary) {
                continue;
            }
            let caller_becomes_boundary = callees.iter().any(|callee| {
                declaration_styles.get(callee) == Some(&LoweringStyle::EffectBoundary)
            });
            if caller_becomes_boundary {
                declaration_styles.insert(decl_name.clone(), LoweringStyle::EffectBoundary);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    let handler_resume_present = module
        .handler_declarations
        .iter()
        .flat_map(|h| h.methods.iter())
        .filter_map(|m| m.parsed_body.as_deref())
        .any(stmts_contain_resume);

    LoweringPlan {
        declaration_styles,
        handler_resume_present,
    }
}

#[derive(Default)]
struct StmtInspection {
    contains_using: bool,
    contains_resume: bool,
    called_declarations: HashSet<String>,
}

fn inspect_stmts(stmts: &[Stmt], out: &mut StmtInspection, declaration_names: &HashSet<String>) {
    for stmt in stmts {
        match stmt {
            Stmt::Binding { value, .. } | Stmt::Expr(value) => {
                inspect_expr(value, out, declaration_names);
            }
            Stmt::Using { body, .. } => {
                out.contains_using = true;
                inspect_stmts(body, out, declaration_names);
            }
        }
    }
}

fn inspect_expr(expr: &Expr, out: &mut StmtInspection, declaration_names: &HashSet<String>) {
    if let Expr::Resume { .. } = expr {
        out.contains_resume = true;
    }
    if let Some((callee_name, _)) = flatten_named_call(expr)
        && declaration_names.contains(callee_name)
    {
        out.called_declarations.insert(callee_name.to_string());
    }

    match expr {
        Expr::IntLit(_)
        | Expr::BoolLit(_)
        | Expr::StringLit(_)
        | Expr::Var(_)
        | Expr::Qualified { .. } => {}
        Expr::ListLit(items) | Expr::TupleLit(items) => {
            for item in items {
                inspect_expr(item, out, declaration_names);
            }
        }
        Expr::RecordConstruct { fields, .. } => {
            for (_, value) in fields {
                inspect_expr(value, out, declaration_names);
            }
        }
        Expr::BinOp { left, right, .. } => {
            inspect_expr(left, out, declaration_names);
            inspect_expr(right, out, declaration_names);
        }
        Expr::Call { callee, arg } => {
            inspect_expr(callee, out, declaration_names);
            inspect_expr(arg, out, declaration_names);
        }
        Expr::MethodCall { args, .. } => {
            for arg in args {
                inspect_expr(arg, out, declaration_names);
            }
        }
        Expr::Pipeline { value, .. } => inspect_expr(value, out, declaration_names),
        Expr::Lambda { body, .. } => inspect_expr(body, out, declaration_names),
        Expr::Resume { value } => inspect_expr(value, out, declaration_names),
        Expr::If {
            condition,
            then_expr,
            else_expr,
        } => {
            inspect_expr(condition, out, declaration_names);
            inspect_expr(then_expr, out, declaration_names);
            inspect_expr(else_expr, out, declaration_names);
        }
        Expr::Case { scrutinee, arms } => {
            inspect_expr(scrutinee, out, declaration_names);
            for arm in arms {
                inspect_expr(&arm.body, out, declaration_names);
            }
        }
    }
}

fn has_effect_clause(annotation: &str) -> bool {
    let trimmed = annotation.trim();
    strip_effect(trimmed) != trimmed
}

fn stmts_contain_resume(stmts: &[Stmt]) -> bool {
    let mut inspection = StmtInspection::default();
    let declaration_names = HashSet::new();
    inspect_stmts(stmts, &mut inspection, &declaration_names);
    inspection.contains_resume
}

#[cfg(test)]
mod tests {
    use goby_core::parse_module;

    use super::*;

    #[test]
    fn classifies_pure_declarations_as_direct_style() {
        let source = r#"
add : Int -> Int -> Int
add a b = a + b

main : Unit -> Unit
main =
  print (add 1 2)
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        assert_eq!(plan.style_for("add"), Some(LoweringStyle::DirectStyle));
        assert_eq!(plan.style_for("main"), Some(LoweringStyle::DirectStyle));
        assert!(!plan.handler_resume_present);
    }

    #[test]
    fn marks_can_clause_declaration_and_callers_as_effect_boundary() {
        let source = r#"
effect Log
  log: String -> Unit

fx : Int -> Int can Log
fx x = x

main : Unit -> Unit
main =
  print (fx 1)
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        assert_eq!(plan.style_for("fx"), Some(LoweringStyle::EffectBoundary));
        assert_eq!(plan.style_for("main"), Some(LoweringStyle::EffectBoundary));
    }

    #[test]
    fn marks_using_declaration_as_effect_boundary() {
        let source = r#"
effect Log
  log: String -> Unit

handler H for Log
  log msg =
    print msg

main : Unit -> Unit
main =
  using H
    log "x"
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        assert_eq!(plan.style_for("main"), Some(LoweringStyle::EffectBoundary));
    }

    #[test]
    fn marks_resume_usage_as_effect_boundary() {
        let source = r#"
f : Int -> Int
f x =
  resume x

main : Unit -> Unit
main =
  print (f 1)
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        assert_eq!(plan.style_for("f"), Some(LoweringStyle::EffectBoundary));
        assert_eq!(plan.style_for("main"), Some(LoweringStyle::EffectBoundary));
    }

    #[test]
    fn records_handler_resume_presence() {
        let source = r#"
effect Iter
  next: Int -> Int

handler H for Iter
  next n =
    resume n

main : Unit -> Unit
main =
  print 1
"#;
        let module = parse_module(source).expect("source should parse");
        let plan = build_lowering_plan(&module);
        assert!(plan.handler_resume_present);
    }
}
