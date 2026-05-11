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

/// GU-S3 CA-3b: shared constants tying constructor-binding `source` strings
/// in `typecheck_build` to the `is_ctor_binding` detector in `TypeEnv`.
/// A constructor binding's `source` is rendered as
/// `format!("{}{}{}", CTOR_SOURCE_PREFIX, type_name, CTOR_SOURCE_SUFFIX)`,
/// e.g. `` "type `Maybe` constructor" ``. The detector matches by
/// `starts_with(PREFIX) && ends_with(SUFFIX)` so both sides stay in sync
/// through these constants and the test suite pins the wiring.
pub(crate) const CTOR_SOURCE_PREFIX: &str = "type `";
pub(crate) const CTOR_SOURCE_SUFFIX: &str = "` constructor";

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

/// GU-S3 constructor-name ambiguity resolution: structured outcome of looking
/// up a union variant by `(type_qualifier, ctor)`, optionally pinned by the
/// scrutinee's already-known type.
///
/// Resolution priority (PLAN_GU §6 GU-S3):
///   1. `Some(t)` → restrict to union `t`; `DoesNotBelongToScrutinee` when the
///      scrutinee is concrete and clashes, `MissingQualifiedCtor` when `t`
///      itself or its `Ctor` is missing.
///   2. concrete scrutinee `Ty::Con { name, .. }` → restrict to that union.
///   3. otherwise: unique match wins, ≥2 matches collapse into `Ambiguous`.
///
/// `Unknown` covers "no union declares this ctor at all" so callers can keep
/// the existing tolerant `Ty::Unknown` fallback for unknown constructors
/// (the dedicated diagnostic for that case lives elsewhere).
pub(crate) enum CtorLookupResult<'a> {
    Resolved {
        union: &'a UnionTypeInfo,
        variant: &'a UnionVariantInfo,
    },
    /// Qualified form `T.Ctor` where `T` does declare `Ctor`, but the
    /// scrutinee is concretely typed as another union `U`; or a bare
    /// `Ctor` where the scrutinee is concretely typed as `U` and `U` does
    /// not declare `Ctor`.
    ///
    /// Fields are consumed by the Step 3 diagnostic helpers
    /// (`err_ctor_does_not_belong`) — until then the suite would flag them
    /// as `dead_code`, so the variant-level `allow` keeps the warning-free
    /// invariant. The `allow` will be removed in Step 3.
    #[allow(dead_code)]
    DoesNotBelongToScrutinee {
        requested: Option<String>,
        ctor: String,
        scrutinee: Ty,
    },
    /// Qualified form `T.Ctor` where `T` is not a known union, or where
    /// `T` is known but does not declare `Ctor` (no scrutinee pinning).
    /// See note on `DoesNotBelongToScrutinee` re: the `allow(dead_code)`.
    #[allow(dead_code)]
    MissingQualifiedCtor {
        qualifier: String,
        ctor: String,
    },
    /// Two or more unions declare `ctor`; the caller must emit the
    /// dedicated "use `TypeName.Ctor`" diagnostic and fall back to
    /// `Ty::Unknown`. Candidates are sorted by `type_name` for stable
    /// diagnostic wording.
    /// See note on `DoesNotBelongToScrutinee` re: the `allow(dead_code)`.
    #[allow(dead_code)]
    Ambiguous {
        ctor: String,
        candidates: Vec<CtorCandidate>,
    },
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CtorCandidate {
    pub(crate) type_name: String,
    /// Origin tag retained so a later cross-module sub-task can extend the
    /// disambiguation rules (local-shadows-imported) without churning the
    /// helper signature. Today every candidate is `Local`.
    pub(crate) origin: CtorOrigin,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CtorOrigin {
    Local,
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

    /// GU-S3 CA-3b: returns true when the global `name` is bound to a type
    /// constructor (union variant ctor or record constructor) registered via
    /// `typecheck_build::inject_type_constructors`. The detector matches the
    /// `source` shape `format!("{PREFIX}{type_name}{SUFFIX}")` rendered by
    /// `insert_global_symbol`; both sides share `CTOR_SOURCE_PREFIX` /
    /// `CTOR_SOURCE_SUFFIX` to avoid drifting string literals.
    ///
    /// Locals shadowing the same name are treated as non-ctor (the local
    /// binding wins for lookup purposes, mirroring `is_effect_op`).
    /// `Ambiguous` entries are only ctors when **every** source matches the
    /// prefix/suffix, so a constructor name shadowing an imported value
    /// stays non-ctor.
    pub(crate) fn is_ctor_binding(&self, name: &str) -> bool {
        if self.locals.contains_key(name) {
            return false;
        }
        let is_ctor_source = |source: &str| -> bool {
            source.starts_with(CTOR_SOURCE_PREFIX) && source.ends_with(CTOR_SOURCE_SUFFIX)
        };
        match self.globals.get(name) {
            Some(GlobalBinding::Resolved { source, .. }) => is_ctor_source(source),
            Some(GlobalBinding::Ambiguous { sources }) => sources.iter().all(|s| is_ctor_source(s)),
            None => false,
        }
    }

    /// GU-S3 constructor-pattern binder inference: locate the union variant
    /// for a `CasePattern::Ctor { type_qualifier, ctor, .. }`.
    ///
    /// - If `type_qualifier` is `Some(t)`, restrict the search to `union_types[t]`.
    ///   `None` (no such union) and "the union has no variant named `ctor`"
    ///   both return `None`.
    /// - If `type_qualifier` is `None`, walk every union (sorted by
    ///   `type_name` so the iteration order is deterministic across
    ///   `HashMap` rehashes) and return the first variant whose `ctor`
    ///   matches.
    ///
    /// GU-S3 ambiguity-resolution-era wrapper: kept as a thin facade that
    /// returns `Some(_)` only on a clean `Resolved` outcome. Existing
    /// constructor-pattern paths that still want a tolerant "give me a
    /// match or fall back to `Ty::Unknown`" semantics route through this.
    ///
    /// New callers should use `resolve_ctor` directly so they can branch on
    /// `DoesNotBelongToScrutinee` / `MissingQualifiedCtor` / `Ambiguous`
    /// without losing information.
    pub(crate) fn lookup_union_variant(
        &self,
        type_qualifier: Option<&str>,
        ctor: &str,
    ) -> Option<(&UnionTypeInfo, &UnionVariantInfo)> {
        match self.resolve_ctor(type_qualifier, ctor, None) {
            CtorLookupResult::Resolved { union, variant } => Some((union, variant)),
            _ => None,
        }
    }

    /// GU-S3 constructor-name ambiguity resolution entry point.
    ///
    /// Resolution priority (PLAN_GU §6 GU-S3):
    ///   1. `type_qualifier = Some(t)` restricts the candidate set to union `t`.
    ///      - Unknown `t` → `MissingQualifiedCtor`.
    ///      - `t` has no variant `ctor` → `MissingQualifiedCtor`.
    ///      - `scrutinee = Some(Ty::Con { name: u, .. })` with `u != t` →
    ///        `DoesNotBelongToScrutinee`.
    ///   2. `scrutinee = Some(Ty::Con { name: u, .. })` pins the union; if `u`
    ///      does not declare `ctor`, that's `DoesNotBelongToScrutinee` too.
    ///   3. Otherwise we walk every union in `type_name`-sorted order. A
    ///      unique match resolves; ≥2 matches collapse into `Ambiguous`
    ///      with the candidate union names (sorted, deduped).
    ///   4. No match anywhere → `Unknown`.
    ///
    /// **Caller contract** (Codex pass-2 follow-up): the helper compares
    /// `scrutinee` shape via direct `match` and does NOT walk
    /// `type_aliases`. Callers must pass an already-alias-resolved scrutinee
    /// (`env.resolve_alias(scrutinee_ty, 0)`) when the scrutinee may be a
    /// `Ty::Var` alias to a concrete `Ty::Con`. The CP path in
    /// `typecheck_check.rs::resolve_ctor_pattern_binder_tys` already
    /// resolves aliases before calling; new callers must do the same.
    ///
    /// The helper never emits diagnostics; it is a pure lookup. Callers
    /// translate the structured outcome into diagnostics using
    /// `typecheck_diag::err_ctor_*` helpers.
    pub(crate) fn resolve_ctor(
        &self,
        type_qualifier: Option<&str>,
        ctor: &str,
        scrutinee: Option<&Ty>,
    ) -> CtorLookupResult<'_> {
        let scrutinee_union_name: Option<&str> = match scrutinee {
            Some(Ty::Con { name, .. }) => Some(name.as_str()),
            _ => None,
        };

        if let Some(t) = type_qualifier {
            // Step 1: qualified form `T.Ctor` strictly restricts the candidate.
            let Some(info) = self.union_types.get(t) else {
                return CtorLookupResult::MissingQualifiedCtor {
                    qualifier: t.to_string(),
                    ctor: ctor.to_string(),
                };
            };
            if !info.variants.iter().any(|v| v.ctor == ctor) {
                return CtorLookupResult::MissingQualifiedCtor {
                    qualifier: t.to_string(),
                    ctor: ctor.to_string(),
                };
            }
            // If the scrutinee is concretely typed as a different union,
            // emit D-2 before the freshen/unify path silently passes.
            if let Some(scrut_name) = scrutinee_union_name
                && scrut_name != t
            {
                return CtorLookupResult::DoesNotBelongToScrutinee {
                    requested: Some(t.to_string()),
                    ctor: ctor.to_string(),
                    scrutinee: scrutinee.cloned().unwrap_or(Ty::Unknown),
                };
            }
            let variant = info
                .variants
                .iter()
                .find(|v| v.ctor == ctor)
                .expect("variant existence checked above");
            return CtorLookupResult::Resolved {
                union: info,
                variant,
            };
        }

        // Step 2: bare `Ctor` with a concrete scrutinee pin.
        if let Some(scrut_name) = scrutinee_union_name {
            // Known union: pin restricts the candidate set.
            if let Some(info) = self.union_types.get(scrut_name) {
                return match info.variants.iter().find(|v| v.ctor == ctor) {
                    Some(variant) => CtorLookupResult::Resolved {
                        union: info,
                        variant,
                    },
                    None => CtorLookupResult::DoesNotBelongToScrutinee {
                        requested: None,
                        ctor: ctor.to_string(),
                        scrutinee: scrutinee.cloned().unwrap_or(Ty::Unknown),
                    },
                };
            }
            // Codex pass-1 指摘: `Ty::Con` が既知 record 型 (e.g. `Box`,
            // `Pair`) を指している場合は「scrutinee は別ユニオン型」と等価
            // なので、union ctor を global search に逃がしてはいけない。
            // 注意: `record_types` の **key は constructor** であり
            // `type_name` ではない (`type Wrapped a = Box(value: a)` だと
            // key は "Box"、`type_name` は "Wrapped")。よって
            // `contains_key(scrut_name)` ではなく values の `type_name`
            // を走査する。それ以外 (= 完全に未登録の `Ty::Con` 名) は
            // global search にフォールバックして既存挙動を維持する。
            if self.record_types.values().any(|r| r.type_name == scrut_name) {
                return CtorLookupResult::DoesNotBelongToScrutinee {
                    requested: None,
                    ctor: ctor.to_string(),
                    scrutinee: scrutinee.cloned().unwrap_or(Ty::Unknown),
                };
            }
            // Unknown `Ty::Con` name — fall through to global ctor search
            // (`Unknown` or `Ambiguous` exactly as for the no-scrutinee
            // case). Preserves the pre-ambiguity tolerant behaviour for
            // partially-inferred scrutinees.
            return self.resolve_ctor_without_pin(ctor);
        }

        // Step 3: bare `Ctor`, no scrutinee pin — global search.
        self.resolve_ctor_without_pin(ctor)
    }

    fn resolve_ctor_without_pin(&self, ctor: &str) -> CtorLookupResult<'_> {
        let mut names: Vec<&String> = self.union_types.keys().collect();
        names.sort();
        let mut hits: Vec<&str> = Vec::new();
        for name in &names {
            let info = &self.union_types[*name];
            if info.variants.iter().any(|v| v.ctor == ctor) {
                hits.push(name.as_str());
            }
        }
        match hits.len() {
            0 => CtorLookupResult::Unknown,
            1 => {
                let info = &self.union_types[hits[0]];
                let variant = info
                    .variants
                    .iter()
                    .find(|v| v.ctor == ctor)
                    .expect("variant existence checked above");
                CtorLookupResult::Resolved {
                    union: info,
                    variant,
                }
            }
            _ => CtorLookupResult::Ambiguous {
                ctor: ctor.to_string(),
                candidates: hits
                    .into_iter()
                    .map(|n| CtorCandidate {
                        type_name: n.to_string(),
                        origin: CtorOrigin::Local,
                    })
                    .collect(),
            },
        }
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
/// CA-2: Elevated to `pub(crate)` so its `next_id` seed can be reused by
/// `typecheck_unify::unifies_with_annotation`. The existing use within
/// `are_compatible` is unchanged. Returns one past the max `N` found across
/// every `__goby_fresh_row_N` row variable in `ty` ("one past max").
pub(crate) fn max_fresh_row_id(ty: &Ty) -> usize {
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

/// GU-S3 constructor-name ambiguity resolution pins for `TypeEnv::resolve_ctor`.
///
/// These cover the rule table from PLAN_GU §6 — qualified > scrutinee-pinned >
/// unique-match > ambiguous, with a separate signal for "qualifier exists but
/// the variant is missing" (`MissingQualifiedCtor`) vs "qualifier conflicts
/// with the scrutinee" (`DoesNotBelongToScrutinee`).
#[cfg(test)]
mod resolve_ctor_tests {
    use super::*;

    fn variant(ctor: &str, idx: u32, args: Vec<Ty>) -> UnionVariantInfo {
        UnionVariantInfo {
            ctor: ctor.to_string(),
            variant_index: idx,
            arg_types: args,
        }
    }

    fn union(name: &str, type_params: Vec<&str>, variants: Vec<UnionVariantInfo>) -> UnionTypeInfo {
        UnionTypeInfo {
            type_name: name.to_string(),
            type_params: type_params.iter().map(|s| s.to_string()).collect(),
            variants,
        }
    }

    fn env_with_unions(unions: Vec<UnionTypeInfo>) -> TypeEnv {
        let mut env = TypeEnv::empty();
        for u in unions {
            env.union_types.insert(u.type_name.clone(), u);
        }
        env
    }

    #[test]
    fn unique_bare_ctor_resolves_to_only_union() {
        let env = env_with_unions(vec![union(
            "Maybe",
            vec!["a"],
            vec![
                variant("Just", 0, vec![Ty::Var("a".to_string())]),
                variant("Nothing", 1, vec![]),
            ],
        )]);
        match env.resolve_ctor(None, "Just", None) {
            CtorLookupResult::Resolved { union, variant } => {
                assert_eq!(union.type_name, "Maybe");
                assert_eq!(variant.ctor, "Just");
            }
            other => panic!("expected Resolved, got {:?}", debug_label(&other)),
        }
    }

    #[test]
    fn unknown_ctor_returns_unknown() {
        let env = env_with_unions(vec![union(
            "Color",
            vec![],
            vec![variant("Red", 0, vec![]), variant("Blue", 1, vec![])],
        )]);
        match env.resolve_ctor(None, "Green", None) {
            CtorLookupResult::Unknown => {}
            other => panic!("expected Unknown, got {:?}", debug_label(&other)),
        }
    }

    #[test]
    fn qualified_lookup_unknown_type_returns_missing_qualified_ctor() {
        let env = env_with_unions(vec![]);
        match env.resolve_ctor(Some("Maybe"), "Just", None) {
            CtorLookupResult::MissingQualifiedCtor { qualifier, ctor } => {
                assert_eq!(qualifier, "Maybe");
                assert_eq!(ctor, "Just");
            }
            other => panic!(
                "expected MissingQualifiedCtor, got {:?}",
                debug_label(&other)
            ),
        }
    }

    #[test]
    fn qualified_lookup_known_type_missing_ctor_returns_missing_qualified_ctor() {
        let env = env_with_unions(vec![union(
            "Maybe",
            vec!["a"],
            vec![variant("Just", 0, vec![Ty::Var("a".to_string())])],
        )]);
        match env.resolve_ctor(Some("Maybe"), "Nothing", None) {
            CtorLookupResult::MissingQualifiedCtor { qualifier, ctor } => {
                assert_eq!(qualifier, "Maybe");
                assert_eq!(ctor, "Nothing");
            }
            other => panic!(
                "expected MissingQualifiedCtor, got {:?}",
                debug_label(&other)
            ),
        }
    }

    #[test]
    fn qualified_lookup_succeeds_when_scrutinee_matches() {
        let env = env_with_unions(vec![union(
            "Maybe",
            vec!["a"],
            vec![variant("Just", 0, vec![Ty::Var("a".to_string())])],
        )]);
        let scrutinee = Ty::Con {
            name: "Maybe".to_string(),
            args: vec![Ty::Int],
        };
        match env.resolve_ctor(Some("Maybe"), "Just", Some(&scrutinee)) {
            CtorLookupResult::Resolved { union, .. } => assert_eq!(union.type_name, "Maybe"),
            other => panic!("expected Resolved, got {:?}", debug_label(&other)),
        }
    }

    #[test]
    fn qualified_lookup_conflicting_scrutinee_returns_does_not_belong() {
        let env = env_with_unions(vec![
            union(
                "Maybe",
                vec!["a"],
                vec![variant("Just", 0, vec![Ty::Var("a".to_string())])],
            ),
            union(
                "Result",
                vec!["a", "b"],
                vec![
                    variant("Ok", 0, vec![Ty::Var("a".to_string())]),
                    variant("Err", 1, vec![Ty::Var("b".to_string())]),
                ],
            ),
        ]);
        let scrutinee = Ty::Con {
            name: "Result".to_string(),
            args: vec![Ty::Int, Ty::Str],
        };
        match env.resolve_ctor(Some("Maybe"), "Just", Some(&scrutinee)) {
            CtorLookupResult::DoesNotBelongToScrutinee {
                requested,
                ctor,
                scrutinee: s,
            } => {
                assert_eq!(requested.as_deref(), Some("Maybe"));
                assert_eq!(ctor, "Just");
                match s {
                    Ty::Con { name, .. } => assert_eq!(name, "Result"),
                    _ => panic!("scrutinee should be Ty::Con"),
                }
            }
            other => panic!(
                "expected DoesNotBelongToScrutinee, got {:?}",
                debug_label(&other)
            ),
        }
    }

    #[test]
    fn scrutinee_pinned_bare_ctor_resolves_to_pinned_union() {
        let env = env_with_unions(vec![
            union(
                "Result",
                vec!["a", "b"],
                vec![
                    variant("Ok", 0, vec![Ty::Var("a".to_string())]),
                    variant("Err", 1, vec![Ty::Var("b".to_string())]),
                ],
            ),
            union(
                "ParseResult",
                vec!["a"],
                vec![
                    variant("Ok", 0, vec![Ty::Var("a".to_string())]),
                    variant("Failed", 1, vec![Ty::Str]),
                ],
            ),
        ]);
        let scrutinee = Ty::Con {
            name: "ParseResult".to_string(),
            args: vec![Ty::Int],
        };
        match env.resolve_ctor(None, "Ok", Some(&scrutinee)) {
            CtorLookupResult::Resolved { union, .. } => {
                assert_eq!(union.type_name, "ParseResult");
            }
            other => panic!("expected Resolved, got {:?}", debug_label(&other)),
        }
    }

    #[test]
    fn scrutinee_pinned_bare_ctor_missing_in_pin_returns_does_not_belong() {
        let env = env_with_unions(vec![
            union(
                "Color",
                vec![],
                vec![variant("Red", 0, vec![]), variant("Blue", 1, vec![])],
            ),
            // Another union declares `Green` so a fall-through global walk
            // would find it; the scrutinee pin must prevent that.
            union("Hue", vec![], vec![variant("Green", 0, vec![])]),
        ]);
        let scrutinee = Ty::Con {
            name: "Color".to_string(),
            args: vec![],
        };
        match env.resolve_ctor(None, "Green", Some(&scrutinee)) {
            CtorLookupResult::DoesNotBelongToScrutinee {
                requested,
                ctor,
                scrutinee: s,
            } => {
                assert!(requested.is_none());
                assert_eq!(ctor, "Green");
                match s {
                    Ty::Con { name, .. } => assert_eq!(name, "Color"),
                    _ => panic!("scrutinee should be Ty::Con"),
                }
            }
            other => panic!(
                "expected DoesNotBelongToScrutinee, got {:?}",
                debug_label(&other)
            ),
        }
    }

    #[test]
    fn bare_ctor_with_two_unions_returns_ambiguous_with_sorted_candidates() {
        // Insert in reverse alphabetical order to verify the helper sorts.
        let env = env_with_unions(vec![
            union(
                "Result",
                vec!["a", "b"],
                vec![variant("Err", 0, vec![Ty::Var("b".to_string())])],
            ),
            union(
                "ParseResult",
                vec!["a"],
                vec![variant("Err", 0, vec![Ty::Str])],
            ),
        ]);
        match env.resolve_ctor(None, "Err", None) {
            CtorLookupResult::Ambiguous { ctor, candidates } => {
                assert_eq!(ctor, "Err");
                let names: Vec<String> = candidates.iter().map(|c| c.type_name.clone()).collect();
                assert_eq!(names, vec!["ParseResult".to_string(), "Result".to_string()]);
                for c in &candidates {
                    assert_eq!(c.origin, CtorOrigin::Local);
                }
            }
            other => panic!("expected Ambiguous, got {:?}", debug_label(&other)),
        }
    }

    #[test]
    fn scrutinee_pin_to_known_record_does_not_fall_back_to_global_walk() {
        // Codex pass-1/-2 follow-up: when the scrutinee is
        // `Ty::Con { name: R, .. }` and `R` is a known record (not a
        // union), the helper must NOT fall back to global ctor search.
        // The user-facing behaviour: pattern-match on a `Wrapped` value
        // with `Just x ->` is a type error, not "Just resolves via the
        // Maybe union".
        //
        // **Type name != constructor name** is the load-bearing shape
        // here: `record_types` is keyed on `constructor`, so a naive
        // `contains_key(scrut_name)` check would silently miss this
        // case. Pin the `type_name`-based check via `record_types.values()`.
        let mut env = TypeEnv::empty();
        env.record_types.insert(
            "MakeWrapped".to_string(),
            RecordTypeInfo {
                type_name: "Wrapped".to_string(),
                type_params: vec!["a".to_string()],
                constructor: "MakeWrapped".to_string(),
                fields: HashMap::new(),
            },
        );
        env.union_types.insert(
            "Maybe".to_string(),
            union(
                "Maybe",
                vec!["a"],
                vec![variant("Just", 0, vec![Ty::Var("a".to_string())])],
            ),
        );
        let scrutinee = Ty::Con {
            name: "Wrapped".to_string(),
            args: vec![Ty::Int],
        };
        match env.resolve_ctor(None, "Just", Some(&scrutinee)) {
            CtorLookupResult::DoesNotBelongToScrutinee {
                requested,
                ctor,
                scrutinee: s,
            } => {
                assert!(requested.is_none());
                assert_eq!(ctor, "Just");
                match s {
                    Ty::Con { name, .. } => assert_eq!(name, "Wrapped"),
                    _ => panic!("scrutinee should be Ty::Con"),
                }
            }
            other => panic!(
                "expected DoesNotBelongToScrutinee, got {:?}",
                debug_label(&other)
            ),
        }
    }

    #[test]
    fn scrutinee_pin_with_alias_resolved_to_concrete_pins() {
        // Codex pass-2 follow-up: helper expects an already-alias-resolved
        // scrutinee. Document the contract by pinning the post-resolve
        // shape — a `Ty::Con { name: "Maybe", .. }` (presumed to come from
        // a `resolve_alias`-applied alias) does pin to the Maybe union,
        // while an unresolved `Ty::Var("AliasName")` falls through to
        // global search.
        let env = env_with_unions(vec![
            union(
                "Maybe",
                vec!["a"],
                vec![variant("Just", 0, vec![Ty::Var("a".to_string())])],
            ),
            union(
                "Result",
                vec!["a", "b"],
                vec![variant("Just", 0, vec![Ty::Var("a".to_string())])],
            ),
        ]);
        // post-alias-resolve concrete pin: deterministic resolve to Maybe.
        let resolved = Ty::Con {
            name: "Maybe".to_string(),
            args: vec![Ty::Int],
        };
        match env.resolve_ctor(None, "Just", Some(&resolved)) {
            CtorLookupResult::Resolved { union, .. } => assert_eq!(union.type_name, "Maybe"),
            other => panic!("expected Resolved, got {:?}", debug_label(&other)),
        }
        // unresolved alias-name `Ty::Var` is treated as no pin.
        let unresolved = Ty::Var("Alias".to_string());
        match env.resolve_ctor(None, "Just", Some(&unresolved)) {
            CtorLookupResult::Ambiguous { .. } => {}
            other => panic!(
                "expected Ambiguous (no concrete pin via Var), got {:?}",
                debug_label(&other)
            ),
        }
    }

    #[test]
    fn non_concrete_scrutinee_falls_back_to_global_walk() {
        // Codex pass-1 follow-up: `Ty::Unknown` / `Ty::Var` / `Ty::Fun`
        // do not pin the union — the helper should behave exactly as if
        // `scrutinee = None`.
        let env = env_with_unions(vec![
            union("A", vec![], vec![variant("X", 0, vec![])]),
            union("B", vec![], vec![variant("X", 0, vec![])]),
        ]);
        for scrutinee in [
            Ty::Unknown,
            Ty::Var("__goby_fresh_ty_0".to_string()),
            Ty::Fun {
                params: vec![Ty::Int],
                result: Box::new(Ty::Int),
                effects: EffectRow::closed_empty(),
            },
        ] {
            match env.resolve_ctor(None, "X", Some(&scrutinee)) {
                CtorLookupResult::Ambiguous { ctor, candidates } => {
                    assert_eq!(ctor, "X");
                    let names: Vec<String> =
                        candidates.iter().map(|c| c.type_name.clone()).collect();
                    assert_eq!(names, vec!["A".to_string(), "B".to_string()]);
                }
                other => panic!(
                    "expected Ambiguous (no concrete pin), got {:?}",
                    debug_label(&other)
                ),
            }
        }
    }

    #[test]
    fn scrutinee_pin_with_unknown_union_falls_back_to_global_walk() {
        // Scrutinee references `MysteryUnion` that the env does not know
        // about. The helper should fall back to the no-scrutinee path and
        // still return Ambiguous if multiple unions declare the ctor —
        // mirroring the resolved CP path that tolerates an unknown
        // scrutinee shape.
        let env = env_with_unions(vec![
            union("A", vec![], vec![variant("X", 0, vec![])]),
            union("B", vec![], vec![variant("X", 0, vec![])]),
        ]);
        let scrutinee = Ty::Con {
            name: "MysteryUnion".to_string(),
            args: vec![],
        };
        match env.resolve_ctor(None, "X", Some(&scrutinee)) {
            CtorLookupResult::Ambiguous { ctor, candidates } => {
                assert_eq!(ctor, "X");
                let names: Vec<String> = candidates.iter().map(|c| c.type_name.clone()).collect();
                assert_eq!(names, vec!["A".to_string(), "B".to_string()]);
            }
            other => panic!("expected Ambiguous, got {:?}", debug_label(&other)),
        }
    }

    #[test]
    fn lookup_union_variant_wrapper_returns_some_only_for_resolved() {
        let env = env_with_unions(vec![
            union("A", vec![], vec![variant("X", 0, vec![])]),
            union("B", vec![], vec![variant("X", 0, vec![])]),
        ]);
        // Ambiguous bare ctor must collapse to None through the wrapper.
        assert!(env.lookup_union_variant(None, "X").is_none());
        // Qualified-with-typeknown still returns Some.
        let resolved = env
            .lookup_union_variant(Some("A"), "X")
            .expect("qualified path should resolve");
        assert_eq!(resolved.0.type_name, "A");
        // Missing qualified ctor must collapse to None.
        assert!(env.lookup_union_variant(Some("A"), "Z").is_none());
    }

    /// Helper that converts a `CtorLookupResult` into a `&'static str` so the
    /// `panic!` formatters above can render a stable label without requiring
    /// `Debug` on the variant payload (which would force `Debug` on `&UnionInfo`).
    fn debug_label(result: &CtorLookupResult<'_>) -> &'static str {
        match result {
            CtorLookupResult::Resolved { .. } => "Resolved",
            CtorLookupResult::DoesNotBelongToScrutinee { .. } => "DoesNotBelongToScrutinee",
            CtorLookupResult::MissingQualifiedCtor { .. } => "MissingQualifiedCtor",
            CtorLookupResult::Ambiguous { .. } => "Ambiguous",
            CtorLookupResult::Unknown => "Unknown",
        }
    }
}
