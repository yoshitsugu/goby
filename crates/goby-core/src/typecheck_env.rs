use std::collections::{BTreeSet, HashMap, HashSet};

/// Identifier for an effect-row variable. Surface form is `{e}`; the inner
/// string keeps the user-written name for diagnostics. Fresh row variables at
/// instantiation use names of the form `__goby_fresh_row_N`.
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RowVarId(pub(crate) String);

/// An effect row in the type system. Set + optional row tail variable.
/// `tail = None` denotes a closed row (exact set of effects).
/// `tail = Some(_)` denotes an open row (additional effects allowed via the var).
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EffectRow {
    pub(crate) fixed: BTreeSet<String>,
    pub(crate) tail: Option<RowVarId>,
}

// EP-1a introduces these types ahead of EP-1b/EP-1c wiring; the `allow(dead_code)`
// attributes scattered through this section suppress unused-warning noise on
// `cargo check` (lib profile) until parser/Ty integration consumes them.
#[allow(dead_code)]
impl EffectRow {
    pub(crate) fn closed_empty() -> Self {
        EffectRow {
            fixed: BTreeSet::new(),
            tail: None,
        }
    }

    pub(crate) fn closed_from<I: IntoIterator<Item = String>>(effects: I) -> Self {
        EffectRow {
            fixed: effects.into_iter().collect(),
            tail: None,
        }
    }

    pub(crate) fn is_closed(&self) -> bool {
        self.tail.is_none()
    }

    pub(crate) fn is_empty_closed(&self) -> bool {
        self.fixed.is_empty() && self.tail.is_none()
    }
}

/// Substitution map for effect-row variables. Kept separate from `TypeSubst`
/// because row variables substitute to rows, not to types.
#[allow(dead_code)]
pub(crate) type RowSubst = HashMap<RowVarId, EffectRow>;

/// Parsed `can` clause (EP-0 surface): a set of fixed effect names plus an
/// optional row variable. `explicit_empty` distinguishes `can {}` from omission
/// at the source level (both denote the closed-empty row, but the former is
/// written explicitly).
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CanClause {
    pub(crate) fixed: Vec<String>,
    pub(crate) row_var: Option<String>,
    pub(crate) explicit_empty: bool,
}

#[allow(dead_code)]
impl CanClause {
    pub(crate) fn empty_closed() -> Self {
        CanClause {
            fixed: Vec::new(),
            row_var: None,
            explicit_empty: false,
        }
    }
}

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
    pub(crate) imported_via_prelude: bool,
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
        match (&expected, &actual) {
            (Ty::Unknown, _) | (_, Ty::Unknown) => return true,
            (Ty::List(expected_inner), Ty::List(actual_inner)) => {
                return self.are_compatible(expected_inner, actual_inner);
            }
            (Ty::Tuple(expected_items), Ty::Tuple(actual_items))
                if expected_items.len() == actual_items.len() =>
            {
                return expected_items
                    .iter()
                    .zip(actual_items.iter())
                    .all(|(expected, actual)| self.are_compatible(expected, actual));
            }
            _ => {}
        }
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

#[cfg(test)]
mod effect_row_tests {
    use super::*;

    #[test]
    fn closed_empty_has_no_fixed_or_tail() {
        let row = EffectRow::closed_empty();
        assert!(row.fixed.is_empty());
        assert!(row.tail.is_none());
        assert!(row.is_closed());
        assert!(row.is_empty_closed());
    }

    #[test]
    fn closed_from_collects_into_set() {
        let row = EffectRow::closed_from(["Print".to_string(), "Read".to_string()]);
        assert_eq!(row.fixed.len(), 2);
        assert!(row.fixed.contains("Print"));
        assert!(row.fixed.contains("Read"));
        assert!(row.is_closed());
        assert!(!row.is_empty_closed());
    }

    #[test]
    fn closed_rows_with_same_effects_are_equal_regardless_of_input_order() {
        let a = EffectRow::closed_from(["Print".to_string(), "Read".to_string()]);
        let b = EffectRow::closed_from(["Read".to_string(), "Print".to_string()]);
        assert_eq!(a, b);
    }

    #[test]
    fn closed_rows_normalize_duplicates_idempotently() {
        let row = EffectRow::closed_from([
            "Print".to_string(),
            "Print".to_string(),
            "Read".to_string(),
        ]);
        assert_eq!(row.fixed.len(), 2);
    }

    #[test]
    fn open_row_carries_tail_variable() {
        let row = EffectRow {
            fixed: BTreeSet::from(["Print".to_string()]),
            tail: Some(RowVarId("e".to_string())),
        };
        assert!(!row.is_closed());
        assert_eq!(row.tail.as_ref().map(|v| v.0.as_str()), Some("e"));
    }

    #[test]
    fn open_rows_with_same_fixed_and_same_tail_are_equal() {
        let a = EffectRow {
            fixed: BTreeSet::from(["Print".to_string(), "Read".to_string()]),
            tail: Some(RowVarId("e".to_string())),
        };
        let b = EffectRow {
            fixed: BTreeSet::from(["Read".to_string(), "Print".to_string()]),
            tail: Some(RowVarId("e".to_string())),
        };
        assert_eq!(a, b);
    }

    #[test]
    fn open_rows_with_different_tails_are_unequal() {
        let a = EffectRow {
            fixed: BTreeSet::from(["Print".to_string()]),
            tail: Some(RowVarId("e".to_string())),
        };
        let b = EffectRow {
            fixed: BTreeSet::from(["Print".to_string()]),
            tail: Some(RowVarId("r".to_string())),
        };
        assert_ne!(a, b);
    }

    #[test]
    fn empty_can_clause_is_closed_without_explicit_braces() {
        let clause = CanClause::empty_closed();
        assert!(clause.fixed.is_empty());
        assert!(clause.row_var.is_none());
        assert!(!clause.explicit_empty);
    }

    #[test]
    fn row_subst_maps_row_var_to_row() {
        let mut subst: RowSubst = RowSubst::new();
        subst.insert(
            RowVarId("e".to_string()),
            EffectRow::closed_from(["Print".to_string()]),
        );
        let bound = subst.get(&RowVarId("e".to_string())).cloned();
        assert_eq!(bound, Some(EffectRow::closed_from(["Print".to_string()])));
    }
}
