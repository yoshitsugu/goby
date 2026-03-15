use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Ty {
    Int,
    Bool,
    Str,
    Unit,
    List(Box<Ty>),
    Tuple(Vec<Ty>),
    Fun { params: Vec<Ty>, result: Box<Ty> },
    Var(String),
    Con { name: String, args: Vec<Ty> },
    Handler { covered_ops: HashSet<String> },
    Unknown,
}

#[derive(Clone)]
pub(crate) struct TypeEnv {
    pub(crate) globals: HashMap<String, GlobalBinding>,
    pub(crate) locals: HashMap<String, Ty>,
    pub(crate) type_aliases: HashMap<String, Ty>,
    pub(crate) record_types: HashMap<String, RecordTypeInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum GlobalBinding {
    Resolved { ty: Ty, source: String },
    Ambiguous { sources: Vec<String> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RecordTypeInfo {
    pub(crate) type_name: String,
    pub(crate) fields: HashMap<String, Ty>,
}

#[derive(Debug, Clone)]
pub(crate) struct ResumeContext {
    pub(crate) expected_arg_ty: Option<Ty>,
}

pub(crate) struct EffectMap {
    pub(crate) effect_to_ops: HashMap<String, HashSet<String>>,
    pub(crate) op_to_effects: HashMap<String, HashSet<String>>,
}


#[derive(Debug, Clone)]
pub(crate) struct ImportedEffectDecl {
    pub(crate) source_module: String,
    pub(crate) decl: crate::ast::EffectDecl,
}

pub(crate) type TypeSubst = HashMap<String, Ty>;

impl TypeEnv {
    pub(crate) fn lookup(&self, name: &str) -> Ty {
        if let Some(ty) = self.locals.get(name) {
            return ty.clone();
        }
        if let Some(binding) = self.globals.get(name) {
            return match binding {
                GlobalBinding::Resolved { ty, .. } => ty.clone(),
                GlobalBinding::Ambiguous { .. } => Ty::Unknown,
            };
        }
        Ty::Unknown
    }

    pub(crate) fn ambiguous_sources(&self, name: &str) -> Option<&[String]> {
        let binding = self.globals.get(name)?;
        if let GlobalBinding::Ambiguous { sources } = binding {
            Some(sources)
        } else {
            None
        }
    }

    pub(crate) fn with_local(&self, name: &str, ty: Ty) -> TypeEnv {
        let mut locals = self.locals.clone();
        locals.insert(name.to_string(), ty);
        TypeEnv {
            globals: self.globals.clone(),
            locals,
            type_aliases: self.type_aliases.clone(),
            record_types: self.record_types.clone(),
        }
    }

    pub(crate) fn lookup_record_by_constructor(
        &self,
        constructor: &str,
    ) -> Option<&RecordTypeInfo> {
        self.record_types.get(constructor)
    }

    pub(crate) fn record_field_ty(&self, type_name: &str, field: &str) -> Option<Ty> {
        self.record_types
            .values()
            .find(|info| info.type_name == type_name)
            .and_then(|info| info.fields.get(field).cloned())
    }

    pub(crate) fn is_effect_op(&self, name: &str) -> bool {
        if self.locals.contains_key(name) {
            return false;
        }
        match self.globals.get(name) {
            Some(GlobalBinding::Resolved { source, .. }) => source.starts_with("effect `"),
            Some(GlobalBinding::Ambiguous { sources }) => {
                sources.iter().all(|s| s.starts_with("effect `"))
            }
            None => false,
        }
    }

    pub(crate) fn are_compatible(&self, expected: &Ty, actual: &Ty) -> bool {
        let expected = self.resolve_alias(expected, 0);
        let actual = self.resolve_alias(actual, 0);
        if let Ty::Con { name, args } = &expected
            && name == "Handler"
            && let Ty::Handler { covered_ops } = &actual
            && let Some(expected_ops) = self.handler_ops_from_type_args(args)
        {
            return expected_ops == *covered_ops;
        }
        expected == actual
    }

    pub(crate) fn handler_ops_from_type_args(&self, args: &[Ty]) -> Option<HashSet<String>> {
        let mut covered = HashSet::new();
        for arg in args {
            let Ty::Con { name, args } = self.resolve_alias(arg, 0) else {
                return None;
            };
            if !args.is_empty() {
                return None;
            }
            for (symbol, binding) in &self.globals {
                if let GlobalBinding::Resolved { source, .. } = binding {
                    let expected_source = format!("effect `{}` member", name);
                    if source == &expected_source {
                        covered.insert(symbol.clone());
                    }
                }
            }
        }
        Some(covered)
    }

    pub(crate) fn resolve_alias(&self, ty: &Ty, depth: usize) -> Ty {
        if depth > 32 {
            return ty.clone();
        }
        match ty {
            Ty::List(inner) => Ty::List(Box::new(self.resolve_alias(inner, depth + 1))),
            Ty::Tuple(items) => Ty::Tuple(
                items
                    .iter()
                    .map(|item| self.resolve_alias(item, depth + 1))
                    .collect(),
            ),
            Ty::Fun { params, result } => Ty::Fun {
                params: params
                    .iter()
                    .map(|param| self.resolve_alias(param, depth + 1))
                    .collect(),
                result: Box::new(self.resolve_alias(result, depth + 1)),
            },
            Ty::Con { name, args } => {
                if args.is_empty()
                    && let Some(target) = self.type_aliases.get(name)
                {
                    return self.resolve_alias(target, depth + 1);
                }
                Ty::Con {
                    name: name.clone(),
                    args: args
                        .iter()
                        .map(|arg| self.resolve_alias(arg, depth + 1))
                        .collect(),
                }
            }
            Ty::Handler { covered_ops } => Ty::Handler {
                covered_ops: covered_ops.clone(),
            },
            _ => ty.clone(),
        }
    }
}
