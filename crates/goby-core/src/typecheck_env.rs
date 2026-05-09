use std::collections::{BTreeSet, HashMap, HashSet};

/// Identifier for an effect-row variable. Surface form is `{e}`; the inner
/// string keeps the user-written name for diagnostics. Fresh row variables at
/// instantiation use names of the form `__goby_fresh_row_N`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RowVarId(pub(crate) String);

/// An effect row in the type system. Set + optional row tail variable.
/// `tail = None` denotes a closed row (exact set of effects).
/// `tail = Some(_)` denotes an open row (additional effects allowed via the var).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EffectRow {
    pub(crate) fixed: BTreeSet<String>,
    pub(crate) tail: Option<RowVarId>,
}

impl EffectRow {
    pub(crate) fn closed_empty() -> Self {
        EffectRow {
            fixed: BTreeSet::new(),
            tail: None,
        }
    }

    // EP-1c: only consumed from tests so far. EP-1d / EP-2 will use this
    // constructor when synthesizing rows during unification and for stdlib
    // HOF row-polymorphic signatures.
    #[allow(dead_code)]
    pub(crate) fn closed_from<I: IntoIterator<Item = String>>(effects: I) -> Self {
        EffectRow {
            fixed: effects.into_iter().collect(),
            tail: None,
        }
    }

    /// Lift a parsed surface `CanClause` into the internal `EffectRow`
    /// representation. `explicit_empty` collapses to the closed-empty row;
    /// the distinction only exists at the surface level for diagnostics.
    pub(crate) fn from_can_clause(clause: &CanClause) -> EffectRow {
        EffectRow {
            fixed: clause.fixed.iter().cloned().collect(),
            tail: clause.row_var.as_ref().map(|name| RowVarId(name.clone())),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn is_closed(&self) -> bool {
        self.tail.is_none()
    }

    pub(crate) fn is_empty_closed(&self) -> bool {
        self.fixed.is_empty() && self.tail.is_none()
    }
}

/// Substitution map for effect-row variables. Kept separate from `TypeSubst`
/// because row variables substitute to rows, not to types.
pub(crate) type RowSubst = HashMap<RowVarId, EffectRow>;

/// Parsed `can` clause (EP-0 surface): a set of fixed effect names plus an
/// optional row variable. `explicit_empty` distinguishes `can {}` from omission
/// at the source level (both denote the closed-empty row, but the former is
/// written explicitly).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CanClause {
    pub(crate) fixed: Vec<String>,
    pub(crate) row_var: Option<String>,
    pub(crate) explicit_empty: bool,
}

impl CanClause {
    #[allow(dead_code)]
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
    Float,
    Bool,
    Str,
    Unit,
    List(Box<Ty>),
    Tuple(Vec<Ty>),
    Fun {
        params: Vec<Ty>,
        result: Box<Ty>,
        /// EP-1c: residual effect row of the function. Defaults to closed-empty
        /// for synthetic / annotation-less function types. EP-1d will integrate
        /// this into row unification.
        effects: EffectRow,
    },
    Var(String),
    Con {
        name: String,
        args: Vec<Ty>,
    },
    Handler {
        covered_ops: HashSet<String>,
    },
    Unknown,
}

#[derive(Clone)]
pub(crate) struct TypeEnv {
    pub(crate) globals: HashMap<String, GlobalBinding>,
    pub(crate) locals: HashMap<String, Ty>,
    pub(crate) type_aliases: HashMap<String, Ty>,
    pub(crate) record_types: HashMap<String, RecordTypeInfo>,
    /// Generic and non-generic union declarations indexed by **union
    /// type name**. Keying on the type name (not on a constructor
    /// name) keeps the storage unambiguous when two unions share a
    /// constructor name; resolving such ambiguity is the typecheck-
    /// side resolver's job.
    pub(crate) union_types: HashMap<String, UnionTypeInfo>,
}

impl TypeEnv {
    /// Construct an empty `TypeEnv`. Test helpers across the
    /// typecheck modules use this (often via `..TypeEnv::empty()`)
    /// to avoid threading every new field through every literal.
    #[allow(dead_code)]
    pub(crate) fn empty() -> TypeEnv {
        TypeEnv {
            globals: HashMap::new(),
            locals: HashMap::new(),
            type_aliases: HashMap::new(),
            record_types: HashMap::new(),
            union_types: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum GlobalBinding {
    Resolved { ty: Ty, source: String },
    Ambiguous { sources: Vec<String> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RecordTypeInfo {
    pub(crate) type_name: String,
    /// Declared type parameters; empty for non-generic records.
    /// Field type templates carry these as `Ty::Var(name)`.
    pub(crate) type_params: Vec<String>,
    pub(crate) constructor: String,
    pub(crate) fields: HashMap<String, Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct UnionTypeInfo {
    pub(crate) type_name: String,
    /// Declared type parameters; empty for non-generic unions.
    /// Variant arg type templates carry these as `Ty::Var(name)`.
    pub(crate) type_params: Vec<String>,
    pub(crate) variants: Vec<UnionVariantInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct UnionVariantInfo {
    pub(crate) ctor: String,
    /// Position of the variant in the declaration's variant list,
    /// used as the runtime tag value during lowering.
    pub(crate) variant_index: u32,
    pub(crate) arg_types: Vec<Ty>,
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
            union_types: self.union_types.clone(),
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
        // EP-1d: keep a single RowSubst across the whole structural walk so
        // nested `Ty::Fun` instances within one comparison share row-variable
        // bindings (e.g. expected `(_ -> _ can {e}) -> _ can {e}` must reject
        // `(_ -> _ can Print) -> _ can Read` because the same `{e}` cannot
        // bind to both `Print` and `Read`).
        //
        // Seed `next_id` past the highest `__goby_fresh_row_N` already
        // present in either input so a fresh row introduced during
        // `unify_open_with_open` cannot collide with a previously freshened
        // tail and look self-referential.
        let mut row_subst = crate::typecheck_env::RowSubst::new();
        let mut next_id = max_fresh_row_id(expected).max(max_fresh_row_id(actual));
        self.are_compatible_with_row_subst(expected, actual, &mut row_subst, &mut next_id)
    }

    fn are_compatible_with_row_subst(
        &self,
        expected: &Ty,
        actual: &Ty,
        row_subst: &mut crate::typecheck_env::RowSubst,
        next_id: &mut usize,
    ) -> bool {
        let expected = self.resolve_alias(expected, 0);
        let actual = self.resolve_alias(actual, 0);
        match (&expected, &actual) {
            (Ty::Unknown, _) | (_, Ty::Unknown) => return true,
            (Ty::List(expected_inner), Ty::List(actual_inner)) => {
                return self.are_compatible_with_row_subst(
                    expected_inner,
                    actual_inner,
                    row_subst,
                    next_id,
                );
            }
            (Ty::Tuple(expected_items), Ty::Tuple(actual_items))
                if expected_items.len() == actual_items.len() =>
            {
                return expected_items
                    .iter()
                    .zip(actual_items.iter())
                    .all(|(expected, actual)| {
                        self.are_compatible_with_row_subst(expected, actual, row_subst, next_id)
                    });
            }
            (
                Ty::Fun {
                    params: ep,
                    result: er,
                    effects: e_eff,
                },
                Ty::Fun {
                    params: ap,
                    result: ar,
                    effects: a_eff,
                },
            ) if ep.len() == ap.len() => {
                if !ep
                    .iter()
                    .zip(ap.iter())
                    .all(|(e, a)| self.are_compatible_with_row_subst(e, a, row_subst, next_id))
                    || !self.are_compatible_with_row_subst(er, ar, row_subst, next_id)
                {
                    return false;
                }
                return crate::typecheck_unify::unify_effect_rows(e_eff, a_eff, row_subst, next_id);
            }
            // EP-1d: also recurse through `Ty::Con` arguments so a function
            // type buried inside e.g. `List (Int -> Int can {e})` shares the
            // surrounding RowSubst. Without this, a row-polymorphic callback
            // wrapped in a Con would fall through to the structural equality
            // check below and bypass `unify_effect_rows`.
            (
                Ty::Con {
                    name: e_name,
                    args: e_args,
                },
                Ty::Con {
                    name: a_name,
                    args: a_args,
                },
            ) if e_name == a_name && e_args.len() == a_args.len() => {
                return e_args
                    .iter()
                    .zip(a_args.iter())
                    .all(|(e, a)| self.are_compatible_with_row_subst(e, a, row_subst, next_id));
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
            Ty::Fun {
                params,
                result,
                effects,
            } => Ty::Fun {
                params: params
                    .iter()
                    .map(|param| self.resolve_alias(param, depth + 1))
                    .collect(),
                result: Box::new(self.resolve_alias(result, depth + 1)),
                effects: effects.clone(),
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

/// Walk a `Ty` and return one past the largest numeric suffix found in any
/// `__goby_fresh_row_N` row variable. Used by `are_compatible` to seed
/// `next_id` so freshly introduced row variables cannot collide with rows
/// that were already freshened earlier in the typecheck pipeline.
fn max_fresh_row_id(ty: &Ty) -> usize {
    fn extract(name: &str) -> Option<usize> {
        const PREFIX: &str = "__goby_fresh_row_";
        name.strip_prefix(PREFIX)
            .and_then(|rest| rest.parse::<usize>().ok())
            .map(|n| n + 1)
    }
    fn walk_row(row: &EffectRow, acc: &mut usize) {
        if let Some(tail) = &row.tail
            && let Some(next) = extract(&tail.0)
            && next > *acc
        {
            *acc = next;
        }
    }
    fn walk(ty: &Ty, acc: &mut usize) {
        match ty {
            Ty::List(inner) => walk(inner, acc),
            Ty::Tuple(items) => items.iter().for_each(|t| walk(t, acc)),
            Ty::Fun {
                params,
                result,
                effects,
            } => {
                params.iter().for_each(|p| walk(p, acc));
                walk(result, acc);
                walk_row(effects, acc);
            }
            Ty::Con { args, .. } => args.iter().for_each(|a| walk(a, acc)),
            Ty::Var(_)
            | Ty::Handler { .. }
            | Ty::Int
            | Ty::Float
            | Ty::Bool
            | Ty::Str
            | Ty::Unit
            | Ty::Unknown => {}
        }
    }
    let mut acc = 0usize;
    walk(ty, &mut acc);
    acc
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
        let row =
            EffectRow::closed_from(["Print".to_string(), "Print".to_string(), "Read".to_string()]);
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

    #[test]
    fn from_can_clause_empty_closed_collapses_to_closed_empty() {
        let clause = CanClause::empty_closed();
        let row = EffectRow::from_can_clause(&clause);
        assert_eq!(row, EffectRow::closed_empty());
    }

    #[test]
    fn from_can_clause_explicit_empty_collapses_to_closed_empty() {
        // `can {}` records explicit_empty=true at the surface but folds
        // to the closed-empty row internally.
        let clause = CanClause {
            fixed: Vec::new(),
            row_var: None,
            explicit_empty: true,
        };
        let row = EffectRow::from_can_clause(&clause);
        assert_eq!(row, EffectRow::closed_empty());
    }

    #[test]
    fn from_can_clause_fixed_only_yields_closed_row() {
        let clause = CanClause {
            fixed: vec!["Print".to_string(), "Read".to_string()],
            row_var: None,
            explicit_empty: false,
        };
        let row = EffectRow::from_can_clause(&clause);
        assert_eq!(
            row,
            EffectRow::closed_from(["Print".to_string(), "Read".to_string()])
        );
    }

    #[test]
    fn from_can_clause_with_row_variable_records_tail() {
        let clause = CanClause {
            fixed: vec!["Print".to_string()],
            row_var: Some("e".to_string()),
            explicit_empty: false,
        };
        let row = EffectRow::from_can_clause(&clause);
        assert_eq!(row.fixed.len(), 1);
        assert!(row.fixed.contains("Print"));
        assert_eq!(row.tail, Some(RowVarId("e".to_string())));
        assert!(!row.is_closed());
    }
}
