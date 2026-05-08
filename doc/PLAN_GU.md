# Goby Track GU — Generic User-Defined Types (Unions with Constructor Args + Generic Records)

Last updated: 2026-05-08

This document is the active implementation plan for **Track GU**: extending
Goby's `type` declaration so that *any* user-defined type — union or record
— may carry type parameters, and union constructors may carry positional
arguments. Motivating examples:

- `type Maybe a = Just(a) | Nothing` (generic union, the canonical PC-M0 case)
- `type ParseResult a = Ok(a) | Err(ParseError)` (generic union, used by PC)
- `type Parser a = Parser(run: Unit -> a can Token, Fail)` (generic record,
  used by PC-P1)

The feature is general: it unblocks any user-defined ADT (`Either`, `Tree`,
`Result`, parser values, AST nodes, …) plus any user-defined record that
needs a type parameter.

**Why both shapes are in the same track.** Generic unions and generic
records share one mechanism: a type parameter list on the declaration,
substituted into argument types and the result type at the call site.
Splitting the two would mean implementing the same parameter-substitution
machinery twice and would force PC to wait on a second cross-layer
rewrite. Bundling them keeps the design self-similar and lets PC start as
soon as GU closes.

Related documents:

- `doc/PLAN.md` §4.5a — top-level Track GU entry. Cross-references this plan.
- `doc/PLAN_PC.md` — Track PC hard-depends on GU. This plan owns the GU↔PC
  ordering rationale (§7).
- `doc/LANGUAGE_SPEC.md` §3 — current type-declaration spec; will gain GU
  language.
- `doc/STATE.md` — phase progress hub during the rewrite (see §8).

Plan-label hygiene:

- Phase IDs (`GU-D0`, `GU-S1`, …) are planning-only metadata. Do not copy
  them into code comments, test names, diagnostics, or user-visible
  strings. Describe the technical purpose directly.

---

## 1. Approach: Design-First, Destructive Rewrite

Track GU is a cross-layer rewrite (parser, AST, resolved form, IR,
typecheck, lowering). Layered, additive milestones would leave the repo
in long-lived inconsistent states (old + new union shape coexisting,
half the typecheck on one form and half on the other, diagnostics frozen
in a transitional wording). That pattern is how cross-layer rewrites
fail to land cleanly.

The user has explicitly authorised:

- **No backwards-compatibility considerations.** The old shape goes away
  the moment the new shape lands.
- **Temporary build breakage is acceptable.** Phases are allowed to land
  the repo in `cargo build` failure; the next phase repairs it.

The plan is therefore:

1. **Freeze the final design** (this document, §3–§5). No code yet.
2. **Swap the AST destructively.** The repo stops compiling.
3. **Walk the layers in dependency order**, file by file, repairing
   each module to use the new AST. The repo reaches `cargo build`
   green again at the end of this group.
4. **Add the new feature semantics** (type-parameter unification,
   constructor patterns, lowering). The repo reaches `cargo test`
   green here.
5. **Final polish** (diagnostics, fixtures, spec sync).

Each phase ends with a commit so the state is recoverable from
`git log` / `git diff` even across context resets. STATE.md is
updated at every phase boundary so a fresh session can resume from the
last committed point without re-reading the conversation history.

---

## 2. Scope and Non-Goals

### In scope

- Type declarations with type parameters (both unions and records):
  - `type Maybe a = ...`, `type Either a b = ...`, `type Tree a = ...`
    (generic unions).
  - `type Parser a = Parser(run: Unit -> a can Token, Fail)` (generic
    record).
  - `type Pair a b = Pair(fst: a, snd: b)` (generic record).
- Union variants with positional constructor arguments:
  `Just(a)`, `Cons(a, List a)`, `Node(a, Tree a, Tree a)`.
- `case` pattern matching against constructor variants:
  `Just x -> ...`, `Cons head tail -> ...`, `Nothing -> ...`.
- Type checking and inference for both:
  - Applying a union constructor (`Just 42`) instantiates the type's
    parameters.
  - Applying a record constructor (`Parser run_fn`) instantiates the
    type's parameters.
  - Pattern matching binds the unified arg types.
  - Field access on a generic record (`p.run`) uses the instantiated
    field type.
- Cross-module use: a generic type declared in module M is usable from
  module N via `import goby/m ( Maybe, Just, Nothing )` and similarly
  for record constructors.
- Updating existing user-facing surfaces:
  - `stdlib/goby/iterator.gb`'s `GraphemeState` is a non-generic record
    and continues to work under the new (parameterised but empty)
    representation.
  - `examples/type.gb`'s `User = User(...)` and
    `UserStatus = Activated | Deactivated` continue to work.

### Explicitly out of scope (handled in §10 follow-ups)

- Type aliases with parameters (`type StrMap a = List (String, a)`).
- Nested constructor patterns (`Just (Cons head tail) -> ...`).
- **Exhaustiveness checking for `case`** — handled in **Track EX**
  (`doc/PLAN.md` §4.5c), which is a hard prerequisite for Track PC.
  During the GU window, non-exhaustive `case` is an interim runtime
  trap (see GU-S4); Track EX lifts it to a compile-time error before
  PC starts.
- Named record fields inside union variants (`Just { value: a }`) —
  per user direction (Goby keeps positional union variants for
  simplicity).
- Named positional arguments on union variants (`Just(value: 42)`) —
  per user direction. Record construction continues to use the
  existing named-field form (`User(id: ..., name: ...)`).
- GADTs / existentials / higher-kinded types.
- Singleton sharing for nullary union variants (heap-layout
  optimisation).

Other than Track EX (a hard PC prerequisite), these are bounded
follow-ups; none are load-bearing for Track PC.

---

## 3. Final AST (Locked Before Coding)

This is the **target** shape the AST swap (§6 GU-S1) installs. Every
later phase is mechanically derived from this section.

### 3.1 Type declarations (`crates/goby-core/src/ast.rs`)

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDeclaration {
    Alias {
        name: String,
        target: String,
    },
    Union {
        name: String,
        type_params: Vec<String>,
        variants: Vec<UnionVariant>,
    },
    Record {
        name: String,
        type_params: Vec<String>, // NEW: empty for non-generic records; populated for generic records
        constructor: String,
        fields: Vec<RecordField>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnionVariant {
    pub ctor: String,
    /// Positional arg type annotations as raw strings, mirroring `RecordField::type_annotation`.
    /// Empty for nullary variants like `Nothing` / `Leaf`.
    pub args: Vec<String>,
}
```

Notes:

- Existing `type UserStatus = Activated | Deactivated` parses as a
  `Union` with `type_params: vec![]` and two `UnionVariant`s with
  `args: vec![]` each.
- Existing non-generic records (`type User = User(id, name, status)`,
  `type GraphemeState = GraphemeState(...)`) parse with
  `type_params: vec![]`. No semantic change.
- Generic record syntax (`type Parser a = Parser(run: Unit -> a can ...)`)
  is fully supported in Track GU. Type parameters substitute into
  field type annotations, the constructor's signature is freshened
  per call site (§3.6), and field access on a generic record
  instance uses the instantiated field type.
- `RecordField` is unchanged.

### 3.2 Case patterns (`crates/goby-core/src/ast.rs`)

```rust
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
    /// Constructor pattern: `Ctor`, `Ctor x`, `Ctor x y _`, plus the
    /// type-qualified form `TypeName.Ctor`, `TypeName.Ctor x`.
    /// For nullary variants, `args` is empty.
    /// `type_qualifier` is `Some("TypeName")` when the source used the
    /// qualified form, `None` for the bare form. The bare and qualified
    /// forms are semantically equivalent; `type_qualifier` is preserved
    /// only so that GU-S3's name-resolution / ambiguity-resolution rule
    /// can use the qualifier to disambiguate when both forms are
    /// otherwise indistinguishable. Walkers that do not care about
    /// resolution treat the two cases uniformly.
    /// Nested constructor patterns are out of scope for Track GU
    /// (each `CtorPatternArg` is a binder or wildcard, not a sub-pattern).
    Ctor {
        type_qualifier: Option<String>,
        ctor: String,
        args: Vec<CtorPatternArg>,
    },
    Wildcard,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CtorPatternArg {
    Bind(String),
    Wildcard,
}
```

### 3.3 Resolved form (`crates/goby-core/src/resolved.rs`)

Current state: `ResolvedCaseArm.pattern` is `ast::CasePattern` directly
(no separate `ResolvedCasePattern` exists). Constructor *resolution*
(deciding which union type a `Ctor` pattern belongs to) happens during
typecheck and IR lowering, not in `resolved.rs`.

**Decision for Track GU**: keep that arrangement. The resolved form
continues to carry AST patterns; the new `CasePattern::Ctor` flows
through `resolved.rs` mechanically (one cloned reference, no shape
change). Constructor-to-type association is decided in `typecheck_check`
(scrutinee unification picks the type) and serialised into IR by
`ir_lower`.

This means GU-S2c (resolved-form repair) is genuinely small — a
mechanical pattern clone — and no new resolved-form types are
introduced. If a future track wants a richer resolved form (e.g. for
incremental typechecking), the migration path is unchanged.

### 3.4 IR (`crates/goby-core/src/ir.rs`)

```rust
pub enum IrCasePattern {
    IntLit(i64),
    StringLit(String),
    BoolLit(bool),
    EmptyList,
    ListPattern { items: Vec<IrListPatternItem>, tail: Option<IrListPatternTail> },
    /// Lowered constructor pattern.
    /// `type_name` and `variant_index` are populated by `ir_lower` after
    /// typecheck has unified the scrutinee. `variant_index` is the
    /// position of this variant in the type's `variants` list, used as
    /// the runtime tag.
    Ctor {
        type_name: String,
        ctor: String,
        variant_index: u32,
        args: Vec<IrCtorPatternArg>,
    },
    Wildcard,
}

pub enum IrCtorPatternArg {
    Bind(String),
    Wildcard,
}
```

`type_name` and `variant_index` come from typecheck's record of which
union type each `Ctor` pattern matched. The IR-lowering pass is
responsible for threading that information; it requires no change to
`resolved.rs` (see §3.3).

### 3.5 Type representation (`crates/goby-core/src/typecheck_types.rs`)

The existing `Ty::Con { name, args }` already supports type
application. Generic unions are represented as:

```rust
Ty::Con {
    name: "Maybe".to_string(),
    args: vec![Ty::Var("a".to_string())],     // declaration-side
    // or
    args: vec![Ty::Int],                       // applied-side (Maybe Int)
}
```

No new `Ty` variant is required.

### 3.6 Type environment (`crates/goby-core/src/typecheck_env.rs`)

```rust
pub struct UnionTypeInfo {
    pub type_name: String,
    pub type_params: Vec<String>,
    pub variants: Vec<UnionVariantInfo>,
}

pub struct UnionVariantInfo {
    pub ctor: String,
    pub variant_index: u32,
    pub arg_types: Vec<Ty>,           // resolved against the type's params
}

pub struct RecordTypeInfo {
    pub type_name: String,
    pub type_params: Vec<String>,     // NEW: empty for non-generic records
    pub constructor: String,
    pub fields: Vec<(String, Ty)>,    // field types resolved against the type's params
}
```

`UnionTypeInfo` and the extended `RecordTypeInfo` live in the same
`HashMap`-shaped environment.

**Fresh instantiation per use site.** Constructor schemes must be
freshened on every lookup so that two different call sites
(`Just 42` and `Just "hi"`, or two `Parser` constructions) get
independent `Ty::Var` instances. The same rule applies to record-field
access on a generic record value.

Concretely:

- For unions: `UnionTypeInfo::variants[i].arg_types` and the implied
  result `Ty::Con { name, args: <type_params as Ty::Var> }` are
  *templates* with parameter names as `Ty::Var(...)`.
- For records: `RecordTypeInfo::fields` types and the result
  `Ty::Con { name, args: <type_params as Ty::Var> }` are templates.
- At the call site / pattern site / field-access site,
  `typecheck_call` / `typecheck_check` calls a single
  `freshen_type_scheme` helper that substitutes each declared type
  parameter with a fresh inference variable, then uses the result as
  the constructor's `Ty::Fun` signature or the field's type.
- The same shape is already used elsewhere (e.g. effect-member
  generic instantiation in `typecheck_effect.rs`); GU adopts the same
  pattern for both unions and records.

This is a GU-S3 deliverable, not GU-S2 — the fresh-instantiation
helper is only meaningful once parametric types begin to type-check.

### 3.7 Memory layout (lowering)

**Both generic records and generic unions reuse the existing
`Record` payload layout
unchanged.** `crates/goby-wasm/src/gen_lower/value.rs` already
documents `Record` as `(ctor_tag: i64, fields: [i64]...)` in linear
memory, and `alloc_static_record` (`gen_lower/emit.rs`) already
allocates `8 + 8 * fields.len()` bytes — i.e. the leading `ctor_tag`
slot exists today, and a zero-field record (8-byte allocation, tag
only) is already valid. Track GU therefore introduces **no new
allocation primitive**: a union value is laid out as

```
slot 0 (= existing ctor_tag slot) : i64    // variant_index in low 32 bits, high 32 bits zero
slot 1 (= existing field[0])      : i64    // arg0  (variant-specific; absent for nullary)
slot 2 (= existing field[1])      : i64    // arg1
   ...
slot N                            : i64    // argN-1
```

Constants:

- `UNION_TAG_SLOT = 0` — the slot holding the tag (same word as the
  existing record `ctor_tag`).
- `UNION_ARG_BASE = 1` — the slot of the first variant argument
  (same offset as the existing record `field[0]`).
- The tag is read by loading slot 0 and masking the low 32 bits
  (or just comparing as `i64` against the variant index, since the
  high 32 bits are always zero by construction).

Layout rules:

- The full slot-0 word is `i64` even though the tag value fits in
  `i32`. This reuses the existing uniform `i64` slot machinery and
  avoids a second allocation shape.
- Nullary variants (`Nothing`, `Leaf`, `Activated`) lower to a
  zero-field record (8-byte allocation, tag only). This is exactly
  the `fields.len() == 0` case of the existing record allocator and
  needs no new code path.
- The variant-index encoding (low 32 bits, high 32 bits zero) leaves
  the high half free for a future singleton-sharing optimisation
  (§10 follow-up #5): a shared heap value per nullary variant has the
  same payload bits as a freshly allocated one, so sharing is purely
  an allocator-level change and does not affect this layout.

For generic records (`type Parser a = Parser(run: ...)`,
`type Box a = Box(value: a)`): type parameters affect only typecheck
and ergonomics; they leave no trace in the runtime layout. A generic
record allocates exactly the same shape as a non-generic record with
the same field count.

### 3.8 Spec text (`doc/LANGUAGE_SPEC.md` §3)

Replace the current §3 union and record grammars with:

> ```
> type Name = Ctor | Ctor | ...                    # nullary union
> type Name a b ... = Ctor(t, t, ...) | Ctor | ... # generic / non-nullary union
> type Name = Ctor(field: t, ...)                  # record (current form, still valid)
> type Name a b ... = Ctor(field: t, ...)          # generic record
> ```
>
> - Type parameters are zero-or-more lowercase-start identifiers,
>   separated by spaces, between the type name and `=`. Both unions
>   and records may carry them.
> - For unions: each variant is either a bare `CamelCase` constructor
>   name (nullary) or `CamelCase(t1, t2, …)` with positional type
>   arguments.
> - For records: a single `CamelCase(field: t, …)` constructor declares
>   named fields.
> - Inside variant or field type annotations, the surrounding
>   declaration's type parameters are in scope; other capitalised
>   names refer to imported or locally declared types.
> - Constructors are applied like ordinary functions:
>   `Just 42`, `Node 1 Leaf Leaf`, `Parser run_fn`,
>   `Pair(fst: 1, snd: 2)`. The type-qualified form
>   `Maybe.Just 42` is also accepted and equivalent to `Just 42`.

Add a new "Constructor patterns" subsection:

> A constructor pattern matches a value of the corresponding union type:
>
> ```
> case m
>   Just x  -> ...
>   Nothing -> ...
> ```
>
> Each binder position is either a lowercase-start identifier (binds
> the value) or `_` (ignores the value). Nested patterns are not
> supported in Track GU.
>
> Constructor patterns also accept the type-qualified form, e.g.
> `Maybe.Just x -> ...` / `Maybe.Nothing -> ...`. The qualified and
> bare forms are equivalent; the qualified form exists so that the
> diagnostic suggested by the §6 GU-S3 ambiguity-resolution rule
> (`use the qualified form TypeName.Ctor`) is actually writable in
> patterns. This mirrors the existing `Iterator.yield` precedent in
> effect clauses (`doc/LANGUAGE_SPEC.md` §3 effects).

Field access on a generic record uses the existing `value.field`
syntax; the field's type is determined by instantiating the record's
type parameters with the value's actual type arguments.

---

## 4. Files Touched (Authoritative Walk-List)

This is the closed set of files that change in Track GU. Phases in §6
walk this list in dependency order.

### 4.1 AST + parser layer

| File | Why it changes |
| ---- | -------------- |
| `crates/goby-core/src/ast.rs` | `TypeDeclaration::Union/Record` reshape; `CasePattern::Ctor`; new `UnionVariant`, `CtorPatternArg` |
| `crates/goby-core/src/parser_top.rs` | `parse_type_declaration_line` parses type params + variant args |
| `crates/goby-core/src/parser_pattern.rs` | `parse_case_pattern` recognises constructor patterns (`Ctor`, `Ctor x`, `Ctor x _`) and the qualified form (`TypeName.Ctor`, `TypeName.Ctor x`); reuses `parser_util::is_qualified_name` |
| `crates/goby-core/src/parser_util.rs` | Helpers for type-param list and variant-arg list parsing |
| `crates/goby-core/src/formatter.rs` | Re-emit type declarations in the new shape; format constructor patterns |
| `crates/goby-core/src/lib.rs` | Re-exports of `CasePattern`, `TypeDeclaration` etc.; mechanical update if any new names need to be public |

### 4.2 Resolved-form layer

| File | Why it changes |
| ---- | -------------- |
| `crates/goby-core/src/resolved.rs` | Mechanically carries the new AST `CasePattern::Ctor` and the new `TypeDeclaration::Union { type_params, variants }` / extended `Record { type_params, .. }` shapes. Per §3.3, no new resolved-pattern type is introduced; constructor-to-type association is resolved during typecheck and IR lowering. |

### 4.3 Typecheck layer

| File | Why it changes |
| ---- | -------------- |
| `crates/goby-core/src/typecheck_types.rs` | `validate_type_declarations` validates type params and variant arg types |
| `crates/goby-core/src/typecheck_build.rs` | `inject_type_constructors` emits `Ty::Fun` signatures from `UnionTypeInfo` |
| `crates/goby-core/src/typecheck_env.rs` | New `UnionTypeInfo` in the type env |
| `crates/goby-core/src/typecheck_validate.rs` | Validate cross-module imports of generic unions |
| `crates/goby-core/src/typecheck_call.rs` | Constructor application threads type-param unification |
| `crates/goby-core/src/typecheck_check.rs` | Constructor-pattern type-checking arm: unify scrutinee, bind args |
| `crates/goby-core/src/typecheck_branch.rs` | Branch joins consider constructor-pattern arm types |
| `crates/goby-core/src/typecheck_diag.rs` | New diagnostic shapes for constructor / pattern errors (reuse existing wording where possible) |

### 4.4 IR layer

| File | Why it changes |
| ---- | -------------- |
| `crates/goby-core/src/ir.rs` | `IrCasePattern::Ctor`; the IR-level pattern variants and walkers |
| `crates/goby-core/src/ir_lower.rs` | `lower_case_pattern` handles constructor patterns (AST → IR) |
| `crates/goby-core/src/perceus.rs` | Refcount lowering recurses into `Ctor` pattern args |
| `crates/goby-core/src/closure_capture.rs` | Recurse into `Ctor` pattern args when collecting captures |
| `crates/goby-core/src/tail_analysis.rs` | Recurse into `Ctor` pattern args when classifying tail position |
| `crates/goby-core/src/lint.rs` | Recognise constructor patterns in lints that walk patterns |

### 4.5 Stdlib / module loader

| File | Why it changes |
| ---- | -------------- |
| `crates/goby-core/src/stdlib.rs` | Match arm for `TypeDeclaration::*` (mechanical) |

### 4.6 Wasm backend

| File | Why it changes |
| ---- | -------------- |
| `crates/goby-wasm/src/lib.rs` | Re-export updates if needed; mechanical |
| `crates/goby-wasm/src/gen_lower/mod.rs` | Top-level dispatch for new IR variants |
| `crates/goby-wasm/src/gen_lower/lower.rs` | Constructor application lowers to tagged-record allocation; constructor pattern lowers to tag dispatch + field projection |
| `crates/goby-wasm/src/gen_lower/value.rs` | Tag slot in heap layout |
| `crates/goby-wasm/src/gen_lower/emit.rs` | Emit instructions for tag read / branch / arg load |
| `crates/goby-wasm/src/gen_lower/backend_ir.rs` | Backend-IR variants for tag dispatch (or reuse `Case` shape with extended pattern) |
| `crates/goby-wasm/src/gen_lower/closure_env.rs` | Capture path through constructor patterns |
| `crates/goby-wasm/src/gen_lower/ptr.rs` | Possibly: tag offset constants if added |
| `crates/goby-wasm/src/runtime_apply.rs` | `match` arm on `TypeDeclaration::Record` follows new shape (mechanical) |
| `crates/goby-wasm/src/runtime_resolver.rs` | Interpreter-fallback pattern matcher recognises `CasePattern::Ctor` |
| `crates/goby-wasm/src/wasm_exec_plan.rs` | `ir_case_pattern_to_ast` and consumers handle the new variant |
| `crates/goby-wasm/src/support.rs` | `is_supported_case_pattern` classifies `Ctor` (initially: only nullary supported until GU-S4 finishes) |
| `crates/goby-wasm/src/fallback.rs` | `UnsupportedCasePattern` reason wiring still applies; new variant routed through it where lowering is incomplete |

### 4.7 Spec / examples / docs

| File | Why it changes |
| ---- | -------------- |
| `doc/LANGUAGE_SPEC.md` §3 | New union grammar + constructor patterns |
| `doc/STATE.md` | Phase progress hub (updated at every phase boundary) |
| `doc/PLAN.md` §4.5a | Already added; no further changes |
| `doc/PLAN_PC.md` §2 | Track PC's lock table notes GU enables `Maybe = Just(a) | Nothing` |

### 4.8 Tests / fixtures

| Path | Purpose |
| ---- | ------- |
| `crates/goby-core/src/parser_top.rs` test mod | Parser positive/negative tests for new union grammar and constructor patterns |
| `crates/goby-core/src/typecheck_*.rs` test mods | Type-param unification, constructor application, pattern type-checking |
| `crates/goby-wasm/tests/` | End-to-end fixtures (see §6 GU-X1, GU-X2) |
| `examples/_gu_smoke/just_nothing.gb`, `examples/_gu_smoke/tree_sum.gb` | Acceptance fixtures with `.expected` files |
| `examples/type.gb`, `stdlib/goby/iterator.gb`, etc. | Verified to still parse / typecheck / run unchanged after the rewrite |

---

## 5. Deletions (Explicit Removal List)

These constructs **disappear** during Track GU. Listing them now means
later phases can grep for residue and confirm nothing was left behind.

- `TypeDeclaration::Union { name, constructors: Vec<String> }` —
  replaced by the new `Union` variant with `type_params` and
  `variants: Vec<UnionVariant>`. The field `constructors: Vec<String>`
  no longer exists.
- `TypeDeclaration::Record { name, constructor, fields }` without
  `type_params` — replaced by the variant carrying
  `type_params: Vec<String>`. Existing records pass `vec![]`.
- `RecordTypeInfo { type_name, fields }` without `type_params` (current
  shape in `typecheck_env.rs`) — replaced by the extended shape
  `RecordTypeInfo { type_name, type_params, constructor, fields }`
  (§3.6).
- The path in `inject_type_constructors`
  (`crates/goby-core/src/typecheck_build.rs`) that registers
  nullary-only union constructors as `Ty::Con { name, args: Vec::new() }`
  — replaced by the unified `UnionVariantInfo` registration in §3.6.
- Any `match` arm in the touched files (§4) that exhaustively
  destructures the old `TypeDeclaration::Union { name, constructors }`
  or the old `TypeDeclaration::Record { name, constructor, fields }`
  (without `type_params`) — every such arm is a rewrite target.

Search commands to verify no residue at the end of GU-S2:

```
rg 'constructors:\s*Vec<String>' crates/
rg 'TypeDeclaration::Union\s*\{\s*name,\s*constructors\s*\}' crates/
rg 'TypeDeclaration::Record\s*\{\s*name,\s*constructor,\s*fields\s*\}' crates/
```

All three must return zero matches.

---

## 6. Phases

Each phase ends with a `git commit`. STATE.md is updated at every phase
boundary.

### GU-D0: Design freeze (this document)

- Complete §3 (final AST), §4 (file walk-list), §5 (deletions).
- Get a Codex review of this plan before any code changes.
- **No code yet.**

**Acceptance**: this document is reviewed, agreed, and committed.

### GU-D1: Spec freeze

- Apply §3.8 to `doc/LANGUAGE_SPEC.md` §3.
- The spec change lands *before* any code change so subsequent code
  reviews can compare against a written rule.

**Acceptance**: spec edit committed.

### GU-S1: Destructive AST swap

**This phase intentionally breaks the build.**

- Edit `crates/goby-core/src/ast.rs` to the §3.1 / §3.2 final shape.
  Remove the old `Union { constructors }` and the old `CasePattern`
  enum entirely.
- Touch only `ast.rs`. Do not attempt to repair downstream files in
  this phase.

**Acceptance**:

- `git commit` lands with an explicit message:
  `WIP GU-S1: AST swap to UnionVariant + Ctor pattern; build broken`.
- `cargo build -p goby-core` fails with errors localised to the
  files in §4.

### GU-S2: Layer-by-layer follow-up (mechanical)

The largest phase. Walk the §4 file list in dependency order, repairing
each file to compile against the new AST. **No new semantics yet** — every
arm that previously read `constructors: Vec<String>` is rewritten to
loop over `variants: Vec<UnionVariant>`, treating `args.is_empty()`
variants exactly as the old nullary case.

Sub-task ordering. **Important rule**: a sub-task is "done" when *all*
files in its layer compile and existing tests pass. Single-file sub-tasks
are not allowed inside a layer that has cross-file pattern walkers
(notably the IR layer). Each sub-task ends with a commit.

- **GU-S2a — Parser layer**: `parser_top.rs`, `parser_pattern.rs`,
  `parser_util.rs`. Parse the new grammar (type params + variant args
  + constructor patterns). Existing nullary unions still parse.
  `cargo build -p goby-core` does not yet succeed (downstream layers
  still reference old shapes), but the parser layer's own tests pass.
- **GU-S2b — Formatter / re-export**: `formatter.rs`, `lib.rs`.
  Mechanical re-emission in the new shape; re-exports refreshed.
- **GU-S2c — Resolved form**: `resolved.rs`. Resolved form follows new
  AST. (See §3.3 for the resolved-pattern decision.)
- **GU-S2d — Typecheck layer (data only)**: `typecheck_types.rs`,
  `typecheck_build.rs`, `typecheck_env.rs`. Both `UnionTypeInfo` and
  the **extended `RecordTypeInfo` (with `type_params: Vec<String>`)**
  are introduced here. `inject_type_constructors` and the imported
  variants (`inject_imported_*`) register both shapes. Existing
  non-generic records keep working with `type_params: vec![]`.
  **At this point the env stores the new shapes but does no
  type-param unification yet** — that lands in GU-S3. Nullary unions
  and existing records continue to type-check exactly as today.
- **GU-S2e — Typecheck layer (walkers)**: `typecheck_validate.rs`,
  `typecheck_call.rs`, `typecheck_check.rs`, `typecheck_branch.rs`,
  `typecheck_diag.rs`. All exhaustive `match` arms over `CasePattern`
  recognise the new `Ctor` variant. All record constructor / field
  access call sites read from the extended `RecordTypeInfo` and
  treat `type_params` as empty for the existing records (no
  freshening yet). **The whole layer compiles by the end of this
  sub-task**, not file-by-file. Constructor application of *nullary*
  union variants and *non-generic* record constructors goes through
  the new path with no behaviour change.
- **GU-S2f — IR layer (all walkers, atomically)**: `ir.rs`,
  `ir_lower.rs`, `perceus.rs`, `closure_capture.rs`, `tail_analysis.rs`,
  `lint.rs`. **All six files must be updated together**: `ir_lower.rs`
  produces `IrCasePattern::Ctor`, every walker (perceus, closure capture,
  tail analysis, lint) handles it. Variants with args may emit
  `BackendLimitation` semantically; the IR-level walkers must be
  exhaustive over the new variant from the moment it exists.
- **GU-S2g — Stdlib resolver**: `stdlib.rs`. Mechanical match-arm update.
- **GU-S2h — Wasm backend (all consumers, atomically)**:
  `gen_lower/*`, `runtime_apply.rs`, `runtime_resolver.rs`,
  `wasm_exec_plan.rs`, `support.rs`, `fallback.rs`, `lib.rs`.
  **Backend AST↔IR converters (`wasm_exec_plan.rs::ir_case_pattern_to_ast`)
  and the interpreter-fallback resolver (`runtime_resolver.rs`) must be
  updated in the same sub-task as the gen_lower changes.** Otherwise
  the fallback path silently rejects or evaluates stale shapes.
  `runtime_apply.rs`'s `TypeDeclaration::Record` match arm is
  mechanically updated to read the new `type_params` field
  (treated as empty for existing records).
  Nullary union variants lower to a single-word tagged record;
  union variants-with-args route to `UnsupportedCasePattern`
  deliberately until GU-S4 lifts it. Existing record lowering is
  unchanged behaviourally; generic-record-with-non-empty-type-params
  may or may not work end-to-end at this point — full coverage is
  GU-S4's job.

**Acceptance at end of GU-S2**:

- `cargo build -p goby-core` and `cargo build -p goby-wasm` succeed.
- `cargo test -p goby-core --lib` passes for everything that worked
  before GU started. `cargo nextest run -p goby-wasm` passes.
- §5 residue-check commands return zero matches.
- §8 boundary verification commands return zero matches outside §4
  (every grep hit must correspond to a §4 file).
- All existing examples (`examples/type.gb`,
  `stdlib/goby/iterator.gb`, etc.) still `goby check` and `goby run`.
- **No new feature works yet.** `Maybe a = Just(a) | Nothing` parses,
  but its variants-with-args path deliberately routes to
  `UnsupportedCasePattern` (a clean compile-time / setup-time error,
  not a silent miscompile). A targeted negative test asserts that
  `Just(1)` triggers exactly that reason. GU-S3 / GU-S4 lift it.

### GU-S3: Type-checking semantics for parametric user-defined types

Covers both parametric unions and parametric records. Field-typed
records (e.g. `Parser a = Parser(run: Unit -> a can Token, Fail)`)
follow the same fresh-instantiation rule as union constructors.

- **GU-S3 first sub-task — extract `freshen_type_scheme`** (D-6).
  Before adding any GU-specific semantics, lift the existing fresh
  instantiation logic used for effect-member generic instantiation
  (`typecheck_effect.rs`, related helpers in `typecheck_unify.rs`)
  into a single shared helper `freshen_type_scheme` in a suitable
  module (likely `typecheck_unify.rs` or a new
  `typecheck_freshen.rs`). Migrate the effect-side call site to use
  it first, with no behaviour change. Only after that does GU-S3
  proceed to wire union constructors, record constructors, and
  record-field access through the same helper. The pre-extraction
  keeps GU and effect instantiation on one rule and avoids a future
  duplicate-ruleset cleanup track.
- **Fresh type-scheme instantiation** per call / pattern / field-access
  site (§3.6). Route every constructor lookup *and* every
  record-field-access lookup through `freshen_type_scheme`. Function-typed fields with effect rows
  (`run: Unit -> a can Token, Fail`) are freshened as `Ty::Fun` whose
  param/result types may contain the type parameter `a`; the effect
  row itself is unaffected by GU.
- Union constructor application: type parameter unification
  (`typecheck_call.rs`, `typecheck_unify.rs` if needed).
- Constructor-pattern type checking with binder type inference
  (`typecheck_check.rs`):
  - Look up the constructor in the type env.
  - Unify the scrutinee's type with the constructor's freshened result
    type (`Ty::Con { name, args }` with fresh arg vars).
  - Bind each pattern arg to the unified arg type.
- **Record constructor application**: a generic record's constructor
  is freshened the same way as a union variant's. `Parser run_fn`
  produces a value of `Parser a` where `a` is unified with the type
  flowing into `run_fn`'s result.
- **Record field access on generic records**: `p.run` looks up the
  field's type template in `RecordTypeInfo`, freshens it against the
  receiver's actual `Ty::Con { name: "Parser", args: [ty_a] }`, and
  yields the instantiated field type. The receiver may be a value
  stored in a list (`xs[0].run`), returned from a function
  (`make_parser().run`), captured by a lambda, or otherwise reached
  through inference; the same `freshen_type_scheme` path covers all
  of these cases because field access reads `Ty` from inference,
  not from syntactic context.
- **Constructor-name ambiguity resolution.** When two unions
  expose the same constructor name (e.g. `Result.Err` and
  `ParseResult.Err`), the typecheck rule is:
  1. **If `type_qualifier` is `Some(t)`** (the source wrote
     `T.Ctor`): only the constructor of type `t` is a candidate. If
     `t` does not declare `Ctor`, or if the scrutinee type is already
     known concretely as `Ty::Con { name: u, .. }` with `u != t`,
     emit the dedicated diagnostic *"constructor `t.Ctor` does not
     belong to scrutinee type `u`"* (D-2). This case never falls
     through to step 2/3.
  2. If the scrutinee's type is already known concretely
     (`Ty::Con { name, .. }`), use that to disambiguate — only
     constructors of that type are candidates.
  3. Otherwise, **local declarations shadow imported declarations**:
     if exactly one *local* constructor matches by name, use it; only
     if no local match exists do imported constructors come into
     scope. This is the lexical-scope rule used by Rust / Haskell and
     mirrors the existing import resolution in `parser_top.rs`. (D-3)
  4. Otherwise, if exactly one constructor matches by name across
     the locally-visible set, use it.
  5. Otherwise, emit a diagnostic naming all candidate types and
     suggesting the qualified form `TypeName.Ctor`.
  Constructor *application* (not patterns) follows the same rule with
  expected-type information from the surrounding context.
- Cross-module use of generic unions and generic records
  (`typecheck_validate.rs`, `typecheck_build.rs::inject_imported_*`).
- Diagnostic wording for arity / type mismatches on constructor
  application and patterns (reuse existing function-call diagnostic
  where possible — see `typecheck_diag.rs`).

**Acceptance**:

- A new test fixture exercises:
  - `type Maybe a = Just(a) | Nothing` with `Just 42 : Maybe Int`.
  - `case m Just x -> ... ; Nothing -> ...` with binder type inference.
  - Two `Just 42` and `Just "hi"` call sites in the same module
    type-check independently (fresh-instantiation regression test for
    union constructors).
  - Two unions sharing a constructor name produce the disambiguation
    diagnostic in the both-ambiguous case, and the bare form works
    where the scrutinee or expected type pins the choice.
  - cross-module: module M declares the type, module N uses it.
  - **Generic record**: `type Box a = Box(value: a)` with
    `Box 42 : Box Int` and `Box "hi" : Box String` in the same
    module. Field access `(Box 42).value : Int`, with the type
    correctly instantiated.
  - **Field access through inference boundaries** (regression
    fixtures for stale-template paths):
    - `xs : List (Box Int)` then `xs[0].value : Int` — inference
      survives list element extraction.
    - A function `make_box : a -> Box a` whose call site pins the
      type, then `(make_box 7).value : Int`.
    - A lambda capturing a `Box a` value, then reading `.value`
      inside the lambda body.
  - **PC-shaped generic record**: a record whose field is a function
    type with an effect row, e.g.
    `type EffectfulBox a = EB(run: Unit -> a can Print)`.
    Constructing and invoking `(EB f).run ()` typechecks with the
    `Print` row preserved. This is the structural shape of `Parser
    a`'s `run` field; confirms the generic-record machinery is not
    accidentally restricted to first-order field types.
  - cross-module: module M declares `type Box a = Box(value: a)`;
    module N imports `Box`, constructs values, and accesses `.value`.
- `cargo test -p goby-core --lib` passes.
- `cargo nextest run -p goby-wasm` may still route variants-with-args
  through `UnsupportedCasePattern` at runtime; that is repaired in
  GU-S4.

### GU-S4: Lowering semantics for parametric user-defined types

Covers both parametric unions and parametric records. Type parameters
leave no trace in the runtime layout (§3.7); GU-S4 is mostly the
union-side work plus confirming that the existing record lowering path
handles generic records unchanged.

- **Union constructor application** lowers to tagged-record allocation
  (`gen_lower/lower.rs`, `value.rs`, `emit.rs`). Heap layout per
  §3.7 (slot 0 = tag, slots 1.. = args).
- **Union constructor pattern** lowers to tag dispatch + field
  projection (same files, plus `backend_ir.rs`).
- **Generic record constructor application** lowers via the existing
  record path. Confirm: a `type Box a = Box(value: a)` instance
  allocates exactly the same shape as a non-generic single-field
  record. No new lowering code, but a regression fixture is required.
- **Generic record field access** uses the existing record-field
  lowering. The runtime offset of `value` in `Box a` is a constant
  (slot 0 of a single-field record), independent of `a`. Confirm
  with a fixture.
- Function-typed record fields (`run: Unit -> a can ...`) lower as
  ordinary function values; the existing record-field lowering path
  treats them uniformly with other field types.
- **Non-exhaustive `case` is a runtime trap (interim)** (D-4). When
  no arm matches at runtime, lowering emits the Wasm `unreachable`
  instruction so execution stops at a clear point rather than
  silently producing a stale value. This is the **interim contract
  for the GU window only**: Track EX (`doc/PLAN.md` §4.5c) lifts it
  to a compile-time error before Track PC starts. `case` writers in
  the GU window should still cover every variant; the trap exists
  so that an oversight crashes loudly rather than miscompiles.

**Acceptance**:

- **Generic union fixtures** `examples/_gu_smoke/just_nothing.gb` and
  `examples/_gu_smoke/tree_sum.gb` `goby check` and `goby run`
  cleanly, with `goby run` byte-matching recorded `.expected`
  fixtures.
- **Generic record fixtures** `examples/_gu_smoke/box_value.gb`
  (basic field access) and `examples/_gu_smoke/effectful_box.gb`
  (function-typed field with effect row) `goby check` and
  `goby run` cleanly with byte-matched fixtures.
- `cargo nextest run -p goby-wasm` passes.

### GU-X0: Diagnostic polish

- Source-span info on constructor / pattern errors matches existing
  typecheck diagnostic quality.
- Negative-path fixtures cover wrong-arity, wrong-type, unknown-ctor,
  ctor-from-different-type-in-pattern.
- **Arity-mismatch wording reuses the existing function-call
  diagnostic** (`typecheck_call.rs`'s `expects argument of type ...`
  / HOF callback arity messages); no constructor-specific wording is
  introduced. The diagnostic span is anchored on the constructor
  reference rather than the whole call so that `Just()` / `Just 1 2`
  underline `Just`. Track PC fixtures must assert structurally
  ("diagnostic mentions both candidate types" for ambiguity, "wrong
  number of arguments" for arity), not by byte-match — see
  `doc/PLAN_PC.md` §4.5 PC-P1.

**Acceptance**:

- Diagnostic fixtures match recorded wording.
- `goby-invariants` skill reports no GU-attributable drift.

### GU-X1: Extend `examples/type.gb` to demonstrate generic types

- **Keep the existing `User` (record) and `UserStatus` (nullary
  union) sections unchanged** so the canonical sample remains a
  continuous reference for non-generic types.
- **Add a generic section** to the same file showing one generic
  union (`type Maybe a = Just(a) | Nothing` or similar) and one
  generic record (`type Box a = Box(value: a)`), plus a small
  `case` over the generic union and a field-access on the generic
  record. Keep the section short (one screen) so the file stays
  readable.
- The canonical sample then covers all four shapes (non-generic
  union, non-generic record, generic union, generic record) in one
  place, which is what GU advertises. (D-5)

**Acceptance**:

- `goby check` and `goby run` clean. Output fixture pinned (the
  existing fixture is extended rather than replaced).

### GU-X2: Final acceptance gate

The closed-form check that everything in §1 and §2 holds:

- [ ] `doc/LANGUAGE_SPEC.md` §3 documents the new surface (unions and
      records, both with optional type parameters).
- [ ] `cargo test -p goby-core --lib` and
      `cargo nextest run -p goby-wasm` are green.
- [ ] Every file in §4 has been touched as predicted; no §4 file was
      missed (spot-check via `rg` of expected new identifiers like
      `UnionVariant`, `CtorPatternArg`).
- [ ] §5 deletion verifications return zero matches.
- [ ] **Generic union fixtures**: `examples/_gu_smoke/just_nothing.gb`
      and `examples/_gu_smoke/tree_sum.gb` pass with byte-matched
      `.expected` fixtures.
- [ ] **Generic record fixtures**: `examples/_gu_smoke/box_value.gb`
      (basic field access on `Box a`) and
      `examples/_gu_smoke/effectful_box.gb` (record with a function
      field carrying an effect row, `EB(run: Unit -> a can Print)`)
      pass with byte-matched `.expected` fixtures.
- [ ] **Cross-module fixtures (one each)**: a generic union declared
      in module A and used in module B; a generic record declared in
      module A and used in module B. Both compile and run.
- [ ] `examples/type.gb` and `stdlib/goby/iterator.gb` continue to
      work unchanged in semantics (the AST representation of their
      declarations changes, but `goby check` / `goby run` outputs are
      identical).
- [ ] `doc/PLAN_PC.md` §2 lock table notes GU enables both
      `Maybe = Just(a) | Nothing` and `Parser a = Parser(run: ...)`.
- [ ] `doc/STATE.md` records the GU completion timestamp.

---

## 7. Cross-Track Ordering

### 7.1 Track GU → Track PC

- **PC-M0 hard-depends on GU-S3 (typecheck) + GU-S4 (lowering).** PC-M0
  introduces `stdlib/goby/maybe.gb` as `type Maybe a = Just(a) | Nothing`
  and exercises a `case` matching `Just x -> ...` / `Nothing -> ...`.
  Without GU-S3 the program does not type-check; without GU-S4 the
  `Just 42` allocation hits `UnsupportedCasePattern` at runtime.
- **PC-P1 hard-depends on GU-S3 + GU-S4** for two reasons:
  1. `ParseResult a = Ok(a) | Err(ParseError)` is a generic union with
     a constructor argument.
  2. `Parser a = Parser(run: Unit -> a can Token, Fail)` is a **generic
     record** (record with type parameter `a`). Generic records are in
     scope for Track GU (see §2 in scope, §3.1, §3.6, §3.7) precisely
     because PC-P1 needs them. They share the type-parameter machinery
     with generic unions; there is no "old Record path" that would
     handle `Parser a`.
- **The operational start gate for Track PC is GU-X2.** Even though
  PC-M0 only needs GU-S3 + GU-S4 in principle, GU-X2 is the closed-form
  green check across both groups (typecheck + lowering + diagnostics).
  PC does not start until GU-X2 closes.

The dependency edges in `doc/PLAN_PC.md` §6 milestone table reflect
this; this plan's §7.1 is the rationale.

### 7.2 Track GU and Track RP (relative-path imports)

Track GU is independent of Track RP. GU's cross-module fixtures use
the current stdlib resolver, but **do not occupy the
`stdlib/goby/maybe.gb` slot** — that path is reserved for Track PC
(PC-M0). GU's cross-module test instead uses an in-tree test fixture
under `crates/goby-core/tests/` (or an internal-only path like
`stdlib/goby/_gu_test_pair.gb` with a header comment marking it as
test-only and scheduled for removal at GU-X2 close).

The two tracks (GU, RP) can land in either order; PC pre-flight
(PLAN_PC §4.2) consumes both.

### 7.3 Track GU and §3.3 (multi-shot effects)

Unrelated. GU is purely a data / pattern feature; it does not touch
effect-handler semantics.

### 7.4 In-flight stdlib work

During GU's window, any concurrent stdlib edits must not assume the
new union form until GU-S3 has landed.

---

## 8. Context-Loss Mitigation

This is a long, multi-phase rewrite. The following mechanisms keep
progress recoverable across context resets and across days:

- **STATE.md is the resume hub.** Every phase boundary updates STATE.md
  with one or two lines naming: which phase just finished, which file
  is the next sub-task, and what build state the repo is in. A fresh
  session reads STATE.md + this document and resumes without conversation
  history.
- **Phase boundaries are commits.** Even when the build is broken
  (notably at GU-S1 end), the commit lands with an explicit
  `WIP GU-S{n}: ... build broken` message. `git log` is then the
  authoritative phase tracker.
- **§4 walk-list is the change checklist.** Each sub-task in GU-S2
  corresponds to one or two files in §4. After every sub-task, mark
  the file as done in a working comment in STATE.md. The file list
  is closed: there are no surprise files outside §4.
- **§5 deletion list is the residue check.** Run the §5 search
  commands at the end of GU-S2 (and again at GU-X2) to confirm the
  old shape is gone. This catches half-rewrites that compile but
  carry stale paths.
- **Boundary verification at every GU-S2 sub-task end.** Before
  committing a sub-task, run:

  ```
  rg 'TypeDeclaration::Union|TypeDeclaration::Record|CasePattern|IrCasePattern' \
     crates/goby-core/src crates/goby-wasm/src
  ```

  Every hit must correspond to a file in §4. A hit outside §4 means a
  file was missed and the walk-list itself needs amending before the
  sub-task is complete. Run the §5 residue checks alongside.
- **Codex review at GU-D0 freeze.** Before any code lands, the design
  goes through Codex; the answer is recorded in this document or a
  follow-up note. Subsequent phases follow the design without
  re-deciding.

---

## 9. Risks

| Risk | Mitigation |
| ---- | ---------- |
| GU-S2 takes longer than expected; intermediate commits accumulate | Sub-tasks are small (one or two files each); each is independently committable. STATE.md tracks progress. |
| Diagnostic wording drifts during the rewrite | GU-X0 is a dedicated polish phase; existing wording is preserved through GU-S2. |
| Heap layout (§3.7) collides with an unstated invariant in `gen_lower` | GU-S4 lands behind a smoke fixture (`just_nothing.gb`) before any complex case (`tree_sum.gb`); a layout issue surfaces in the smaller fixture first. |
| Tag-slot overhead noticed only at fixture time | The layout is record-shaped, matching the existing precedent. If a future profiling pass shows overhead, singleton-sharing for nullary variants (§10 follow-up) is the natural optimisation. |
| A sibling track (Float, RP) edits a §4 file mid-rewrite | STATE.md announces GU's active phase; sibling tracks coordinate via that. Worst case, GU rebases over the sibling change. |
| Track PC begins assuming GU semantics that have not yet landed | PC's milestone table (`doc/PLAN_PC.md` §6) explicitly hard-depends on GU-S3 / GU-S4. PC cannot start before GU-X2. |

---

## 10. Deferred Follow-ups (Not Track GU)

Captured here so PC-2 design and future stdlib work knows what to
expect:

1. **Type aliases with parameters** (`type StrMap a = List (String,
   a)`).
2. **Nested constructor patterns** (`Just (Cons head tail) -> ...`).
3. **Exhaustiveness checking for `case`**.
4. **Named record fields inside union variants**
   (`Just { value: a }`).
5. **Singleton sharing for nullary variants** (heap-allocation
   optimisation; see §3.7).
6. **GADTs / existentials / higher-kinded types** — explicitly *not*
   on the roadmap; mentioned only to record the rank-1-parametric
   commitment.

---

## 11. Open Questions (Resolved at GU-D0)

All four questions are closed at GU-D0. Q1–Q3 were taken to a Codex
review on 2026-05-08; the recorded resolution feeds §3.7, §3.8, and
§6 GU-X0.

1. ~~**Bare `Ctor` vs. qualified `TypeName.Ctor`**~~ **Resolved
   (2026-05-08)**: both forms are accepted at GU entry, in **expression
   and pattern position**. This is forced by §6 GU-S3's ambiguity
   diagnostic — suggesting `TypeName.Ctor` is meaningless if patterns
   cannot accept it — and is consistent with the existing
   `Iterator.yield` precedent for effect clauses. Pattern parsing
   reuses the existing `is_qualified_name` helper
   (`crates/goby-core/src/parser_util.rs`); the additional scope is a
   qualified-form arm in `parser_pattern.rs` and matching fixtures in
   `parser_top.rs`. See §3.8.
2. ~~**Diagnostic wording for arity mismatch on constructor
   application**~~ **Resolved (2026-05-08)**: reuse the existing
   function-call arity diagnostic (`typecheck_call.rs`); do not
   introduce constructor-specific wording. The diagnostic span is
   anchored on the constructor reference. PC fixtures must assert
   structurally rather than byte-match GU's wording. See §6 GU-X0 and
   `doc/PLAN_PC.md` §4.5 PC-P1.
3. ~~**Heap layout for variants with args**~~ **Resolved
   (2026-05-08)**: §3.7's layout reuses the existing `Record` payload
   shape unchanged. `gen_lower/value.rs` already documents `Record`
   as `(ctor_tag: i64, fields: [i64]...)`, and `alloc_static_record`
   in `gen_lower/emit.rs` already supports the `fields.len() == 0`
   case (8-byte allocation, tag only). No new allocation primitive is
   introduced; nullary variants land on the existing zero-field path;
   the variant-index encoding leaves room for the §10 follow-up #5
   singleton-sharing optimisation without surface change. See §3.7.
4. ~~`Record::type_params` introduction in GU-S1 vs. follow-up~~
   **Resolved**: generic records are in scope for Track GU (§2);
   `Record::type_params` is populated by the parser for generic
   records and remains empty for non-generic records. No follow-up
   churn is deferred.
5. ~~**AST representation of qualified ctor patterns**~~ **Resolved
   (2026-05-08)**: §3.2 `CasePattern::Ctor` carries
   `type_qualifier: Option<String>` alongside `ctor: String`. Bare
   form sets `type_qualifier = None`; qualified form sets it to the
   type name. The two forms are semantically equivalent; the field
   exists so GU-S3's name-resolution rule (Q5 below) can use it.
6. ~~**Diagnostic for qualified-ctor / scrutinee-type
   mismatch**~~ **Resolved (2026-05-08)**: when
   `type_qualifier = Some(t)` and the scrutinee is `Ty::Con { name:
   u, .. }` with `u != t`, emit a dedicated diagnostic *"constructor
   `t.Ctor` does not belong to scrutinee type `u`"* rather than
   falling through to unknown-ctor or generic type-mismatch wording.
   See §6 GU-S3 step 1.
7. ~~**Local vs imported ctor name shadowing**~~ **Resolved
   (2026-05-08)**: local declarations shadow imported declarations
   (Rust / Haskell convention). See §6 GU-S3 step 3.
8. ~~**Non-exhaustive `case` runtime contract**~~ **Resolved
   (2026-05-08)**: interim runtime trap via Wasm `unreachable` for
   the GU window; Track EX (`doc/PLAN.md` §4.5c) lifts it to a
   compile-time error before PC starts. See §2 out-of-scope and
   §6 GU-S4.
9. ~~**Canonical sample migration (`examples/type.gb`)**~~
   **Resolved (2026-05-08)**: keep the existing `User` /
   `UserStatus` sections; add a short generic-union + generic-record
   section to the same file. See §6 GU-X1.
10. ~~**`freshen_type_scheme` extraction timing**~~ **Resolved
    (2026-05-08)**: extracted in GU-S3's first sub-task as a shared
    helper, with the existing effect-side call site migrated first
    (no behaviour change). Union ctors / record ctors / record
    field access then route through the same helper. See §6 GU-S3.
