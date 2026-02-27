use std::collections::HashSet;

use crate::{Module, types::parse_function_type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypecheckError {
    pub declaration: Option<String>,
    pub message: String,
}

pub fn typecheck_module(module: &Module) -> Result<(), TypecheckError> {
    let mut names = HashSet::new();

    for decl in &module.declarations {
        if !names.insert(decl.name.clone()) {
            return Err(TypecheckError {
                declaration: Some(decl.name.clone()),
                message: "duplicate top-level declaration".to_string(),
            });
        }
    }

    if let Some(main) = module.declarations.iter().find(|d| d.name == "main") {
        let annotation = main
            .type_annotation
            .as_deref()
            .ok_or_else(|| TypecheckError {
                declaration: Some("main".to_string()),
                message: "main must have an explicit type annotation".to_string(),
            })?;

        let ty = parse_function_type(annotation).ok_or_else(|| TypecheckError {
            declaration: Some("main".to_string()),
            message: "main type annotation must be a function type".to_string(),
        })?;

        if ty.arguments != vec!["void".to_string()] || ty.result != "void" {
            return Err(TypecheckError {
                declaration: Some("main".to_string()),
                message: "main type must be `void -> void` in MVP".to_string(),
            });
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::parse_module;

    use super::*;

    #[test]
    fn typechecks_examples() {
        let hello = std::fs::read_to_string(format!(
            "{}/../../examples/hello.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("hello example should exist");
        let basic = std::fs::read_to_string(format!(
            "{}/../../examples/basic_types.gb",
            env!("CARGO_MANIFEST_DIR")
        ))
        .expect("basic_types example should exist");

        let hello_module = parse_module(&hello).expect("hello should parse");
        let basic_module = parse_module(&basic).expect("basic_types should parse");

        typecheck_module(&hello_module).expect("hello should typecheck");
        typecheck_module(&basic_module).expect("basic_types should typecheck");
    }
}
