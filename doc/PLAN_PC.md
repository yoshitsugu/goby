# Goby Track PC — Maybe + Parser Combinator Implementation Plan

Last updated: 2026-05-08

This document is the active implementation plan for Track PC. It refines the
high-level scope in `doc/PLAN.md` §4.6 with the surface-lock decisions captured
in `tmp/pc.md` and turns them into concrete, ordered milestones.

Related documents:

- `doc/PLAN.md` §4.6 — top-level Track PC roadmap.
- `doc/LANGUAGE_SPEC.md` §3 (type declarations), §5 (effect rows).
- `doc/STATE.md` — current focus and restart notes.
- `tmp/pc.md` — surface-lock design notes (decisions feed this plan).

Plan-label hygiene:

- Phase IDs (`PC-M0`, `PC-P1`, …) are planning-only metadata. Do not copy
  them into code comments, test names, diagnostics, or user-visible strings.
- When implementation notes need context, describe the technical purpose
  directly.

---

## 1. Two Independent Tracks

Track PC is split into two ordered, independently shippable tracks:

```
Track PC-M : stdlib/goby/maybe.gb           (general-purpose ADT)
Track PC-P : examples/parser/ (combinator)  (depends on PC-M)
```

`Maybe` is a general-purpose ADT and is useful well outside parsing
(`int.parse`, future `Result`, optional-field records, …). It must land first
and be stable on its own merits. The parser combinator is built on top.

The parser combinator library lives under `examples/parser/` rather than
`stdlib/goby/parser.gb`, because parser combinators are an *application*
of the effect system rather than a language primitive (cf. Rust's `nom`,
Haskell's `parsec`). The intent is that, once Goby grows a package
mechanism (Rubygems / Cargo equivalent), the `examples/parser/` directory
migrates verbatim to an external package.

**Hard prerequisite: Track RP (`doc/PLAN.md` §4.5b).** The current Goby
import resolver (`crates/goby-core/src/stdlib.rs`) only resolves under
`stdlib/`, and the grammar rejects relative paths outright
(`crates/goby-core/src/parser_util.rs::is_module_path`). Without
relative-path or workspace-rooted imports, no multi-file library can
live outside `stdlib/`. Track RP introduces that import surface;
Track PC consumes it. **Track PC does not start until RP-3 has
landed.** Stdlib placement of the parser library is explicitly *not* a
fallback — it would compromise the long-term goal of a clean,
package-ready library.

Demonstrations that *use* the library (number parser, arithmetic
expression parser, JSON-lite parser) sit at `examples/` top level
(`examples/parser_number.gb`, etc.), matching the existing `examples/`
convention. They import the parser library via the Track RP surface and
double as `goby check` / `goby run` acceptance fixtures. After migration
to an external package, these top-level demos remain in the repo as
"usage examples for the external parser package".

**Quality bar**: even at examples placement, the parser combinator code is
to be written and documented at *library* quality — public type signatures,
docstring-style comments on combinators, no example-only shortcuts. The
expectation is that the same files migrate to a future package without
rewriting.

---

## 2. Locked Decisions (Inputs from `tmp/pc.md`)

The design surface is already locked. The plan below assumes these decisions:

| Item                | Decision                                                                |
| ------------------- | ----------------------------------------------------------------------- |
| `Maybe` shape       | `type Maybe a = Just(a) \| Nothing` (Haskell-style; generic union, enabled by Track GU) |
| `Maybe` placement   | `stdlib/goby/maybe.gb`, general-purpose                                 |
| `Parser a` shape    | thin wrapper: `Parser(run: Unit -> a can Token, Fail)` (generic record, enabled by Track GU) |
| `ParseResult a` shape | `Ok(a) \| Err(ParseError)` (generic union, enabled by Track GU)        |
| Combinator surface  | `pure / map / bind / alt / many / optional / seq / between / eof`       |
| Token granularity   | grapheme-`String` tokens, public entry `String -> ParseResult a`        |
| Effect protocol     | `Token` + `Fail` public; `Choice` reserved by name only (PC-2)          |
| Backtracking        | uncommitted `alt`; PC-1: explicit cursor threading. PC-2: §3.3 multi-shot |
| Error type          | `ParseError(position, expected, message)`                               |
| Parser library      | `examples/parser/` (multi-file). Depends on Track RP (`doc/PLAN.md` §4.5b). Stdlib placement is rejected by lock |
| Parser demos        | `examples/parser_*.gb` at top level (uses the library via Track RP imports) |
| `Cursor` visibility | internal-only; never appears in any public type or constructor         |
| Operator aliases    | not introduced in PC-0/PC-1                                             |

If a decision needs to change, update `tmp/pc.md` first and revise this plan
accordingly.

---

## 3. Track PC-M: `Maybe` (stdlib/goby/maybe.gb)

### 3.1 Goal

Ship a small, idiomatic `Maybe a = Just(a) | Nothing` module under
`stdlib/goby/maybe.gb`, usable from any other Goby code without circular
dependency on parser concepts.

### 3.2 Surface

```goby
type Maybe a = Just(a) | Nothing

# Constructor smart helpers (optional; type constructors already work).
# Provided only if surface use proves the typed `Just`/`Nothing` form
# inconvenient — default is "no extra constructors".

# Inspection
is_just    : Maybe a -> Bool
is_nothing : Maybe a -> Bool

# Elimination
unwrap_or  : Maybe a -> a -> a              # default value on Nothing
map        : Maybe a -> (a -> b) -> Maybe b
and_then   : Maybe a -> (a -> Maybe b) -> Maybe b   # monadic bind

# Conversion
to_list    : Maybe a -> List a              # [] / [x]
```

The minimum viable surface for Track PC-P is `Just` / `Nothing` constructors
plus pattern matching. `unwrap_or` / `map` / `and_then` / `to_list` are added
because they are immediately useful for `Token.peek` consumers and for
non-parser code; they do not add language risk.

`map` and `and_then` are written row-polymorphic (`can {e}` on the
callback) so that effectful callbacks compose without per-callsite
annotations, mirroring `stdlib/goby/list.gb`:

```goby
map      : Maybe a -> (a -> b can {e}) -> Maybe b can {e}
and_then : Maybe a -> (a -> Maybe b can {e}) -> Maybe b can {e}
```

**Why this row-poly shape works for `Maybe` but not for `Parser`** —
`Maybe.map` runs its callback *immediately* against the held value (if
any). The `{e}` row variable on the return is therefore a faithful
description of the effects performed during the call. `Parser.map`, in
contrast, builds a *deferred* computation: the callback only runs when
the resulting parser is later driven, so its effects belong inside the
deferred `run` row, not the outer return. This is why §4.4's parser
combinator signatures keep callbacks pure under PC-1.

### 3.3 Phases

#### PC-M0: ADT introduction (no helpers)

- Add `stdlib/goby/maybe.gb` with the type declaration alone:
  `type Maybe a = Just(a) | Nothing`.
- Verify the type checks, the constructors are usable from another module via
  `import goby/maybe ( Maybe, Just, Nothing )`, and pattern matching works:

  ```goby
  case m
    Just x  -> ...
    Nothing -> ...
  ```

- No combinators yet. The goal is to confirm that union-type cross-module
  use is healthy before adding helpers.

**Acceptance**:

- `cargo test -p goby-core --lib` passes with the new module loaded.
- A throwaway example file under `examples/` (or an in-repo test fixture)
  pattern-matches `Just`/`Nothing` and round-trips a value via `Just 42`.

**Deliverables**:

- `stdlib/goby/maybe.gb` with the type declaration.
- One acceptance fixture exercising construction + pattern match.
- `doc/LANGUAGE_SPEC.md` mention if cross-module ADT export needs spec
  language; otherwise none.

#### PC-M1: Inspection helpers

- Add `is_just`, `is_nothing`.
- Add `unwrap_or`.

These are pure, total, do not exercise the effect row machinery, and unblock
the PC-P plan's `Token.peek`-driven combinators without forcing every site
to spell `case` matches.

**Deliverables**:

- Edits to `stdlib/goby/maybe.gb` adding `is_just`, `is_nothing`,
  `unwrap_or`.
- New `examples/maybe_inspect.gb` exercising each helper on both `Just`
  and `Nothing`, with stdout pinned to `examples/maybe_inspect.expected`.

**Acceptance**:

- `cargo test -p goby-core --lib` passes.
- `goby check examples/maybe_inspect.gb` succeeds; `goby run` output
  byte-matches `examples/maybe_inspect.expected`.

#### PC-M2: Row-polymorphic `map` / `and_then` / `to_list`

- Add `map`, `and_then` with `can {e}` on the callback.
- Add `to_list`.
- Confirm that an effectful callback (e.g. one that performs `Print`)
  propagates the row through `map` / `and_then` without explicit `can`
  annotations on the call site, matching the behaviour of `list.map`.

**Deliverables**:

- Edits to `stdlib/goby/maybe.gb` adding `map`, `and_then`, `to_list`.
- New `examples/maybe_pure.gb` (pure callbacks) with
  `examples/maybe_pure.expected`.
- New `examples/maybe_effect.gb` (`Print`-performing callback) with
  `examples/maybe_effect.expected`.

**Acceptance**:

- `cargo test -p goby-core --lib` passes.
- `goby check examples/maybe_pure.gb` and
  `goby check examples/maybe_effect.gb` succeed.
- `goby run examples/maybe_pure.gb` byte-matches
  `examples/maybe_pure.expected`.
- `goby run examples/maybe_effect.gb` byte-matches
  `examples/maybe_effect.expected` if the wasm backend can host it; if
  gated, the gating reason is recorded in `doc/STATE.md`.

#### PC-M3: Documentation and stdlib invariants

- Spec note in `doc/LANGUAGE_SPEC.md` (or stdlib catalogue, if one exists)
  describing `Maybe` as a stable, public ADT.
- Run the `goby-invariants` skill: confirm `examples/`, diagnostics, and
  spec are in sync. No diagnostic wording changes expected.

**Deliverables**:

- Edits to `doc/LANGUAGE_SPEC.md` (or the stdlib catalogue file, if
  introduced separately) mentioning `Maybe`.
- An `examples/maybe.md` (or section in an existing stdlib README)
  giving a 1-screen overview: type, constructors, helper signatures,
  one pure example and one `Print`-effect example.

**Acceptance**:

- `goby-invariants` skill output reports no spec/examples/diagnostics
  drift attributable to PC-M.
- `cargo test -p goby-core --lib` and `cargo nextest run -p goby-wasm`
  both pass.

**Track PC-M acceptance checklist** (final gate):

- [ ] `stdlib/goby/maybe.gb` exports `Maybe`, `Just`, `Nothing`,
      `is_just`, `is_nothing`, `unwrap_or`, `map`, `and_then`, `to_list`.
- [ ] All of `examples/maybe_inspect.gb`, `examples/maybe_pure.gb`,
      `examples/maybe_effect.gb` `goby check` cleanly.
- [ ] `goby run` byte-matches the recorded `.expected` files for
      `maybe_inspect` and `maybe_pure`. `maybe_effect` either runs and
      byte-matches, or is recorded as gated in `doc/STATE.md` with a
      named backend reason.
- [ ] `cargo test -p goby-core --lib` and `cargo nextest run -p goby-wasm`
      pass.
- [ ] `doc/LANGUAGE_SPEC.md` (or the stdlib catalogue) names `Maybe` as
      a stable public ADT.
- [ ] `goby-invariants` reports no PC-M-attributable drift.

---

## 4. Track PC-P: Parser Combinator (examples/parser/)

### 4.1 Goal

Deliver a small, library-quality parser combinator surface on top of Goby's
effect system, placed in `examples/parser/` so it can be promoted to an
external package later without rewriting.

### 4.2 Pre-flight Check (PC-P0)

Before writing parser code, verify three preconditions that the lock in
`tmp/pc.md` assumes but cannot enforce on its own:

- [ ] **Track GU-X2 has closed.** PC's locked surface uses
      `type Maybe a = Just(a) | Nothing`, `type ParseResult a = Ok(a)
      | Err(ParseError)`, and `type Parser a = Parser(run: ...)`.
      All three require GU's typecheck (GU-S3) and lowering (GU-S4)
      machinery; GU-X2 is the closed-form acceptance gate
      (`doc/PLAN_GU.md` §6 GU-X2). PC-M0 already gates on this in §6
      milestone table; the pre-flight verifies it explicitly to
      surface scheduling drift early.
- [ ] **Track RP-3 has landed.** Specifically, a `.gb` file under
      `examples/` can import another `.gb` file under `examples/`
      using the surface form chosen in RP-0. This is the prerequisite
      for the §4.3 multi-file layout. Without it, Track PC does not
      start; revisit `doc/PLAN.md` §4.5b instead.
- [ ] **Effectful return-type polymorphism**: confirm
      `fail : ParseError -> a` is supported. The `int.parse` precedent
      (`invalid_integer : String -> Int`) is identical in shape, but PC-P
      uses a free type variable in the return slot rather than a
      concrete `Int`. If only concrete returns are currently supported,
      treat this as a blocker and lift it before PC-P1.

PC-P0 deliverables (concrete probes, not just a note):

- Probe fixtures live under `examples/_pc_preflight/` (or
  `tmp/pc_probe/`). This is intentionally separate from
  `examples/_gu_smoke/` (used by Track GU per `doc/PLAN_GU.md` §6) so
  the two tracks' fixtures do not collide.
- A throwaway fixture under `examples/_pc_preflight/`
  containing two `.gb` files where one imports a type and an effect
  from the other using the Track RP import surface;
  verify `goby check` succeeds. (If RP includes its own smoke fixture
  at `examples/_rp_smoke/`, this probe may consist of pointing at that
  fixture and confirming it covers the parser library's planned use.)
- A second fixture importing `Maybe` (from `stdlib/goby/maybe.gb`) into
  an `examples/`-rooted file via `import goby/maybe ( Maybe, Just, Nothing )`
  — confirms the parser code's stdlib import path is unaffected by Track
  RP changes.
- A third fixture defining `effect E` with `op : ParseError -> a` and
  invoking it; confirms `fail : ParseError -> a` (free type variable in
  the return slot) is supported in current Goby.
- A short note recording any spec gaps surfaced by the probes.

### 4.3 File Layout

Assumes Track RP-3 has landed (see §4.2). The library lives under
`examples/parser/`. Files that *use* the library sit at `examples/` top
level, so they appear in the same listing as existing demos
(`examples/iterator.gb`, `examples/hof_effect.gb`, …) and serve as the
canonical "how to use parser combinators" entry points.

```
examples/parser/                # the library (migration target for a future package)
  README.md                     # 1-page overview + public surface
  types.gb                      # Parser, Position, Token, ParseError, ParseResult (public types)
  internal.gb                   # Cursor and other PC-1 internals (not exported)
  effects.gb                    # effect Token, effect Fail (Choice reserved by name)
  core.gb                       # pure / map / bind / alt / eof
  combinators.gb                # many / optional / seq / between
  driver.gb                     # parse : Parser a -> String -> ParseResult a

examples/parser_number.gb       # uses examples/parser/ via Track RP
examples/parser_arith.gb
examples/parser_json_lite.gb
```

This split has two benefits:

- The library directory `examples/parser/` is a self-contained unit
  that migrates verbatim to an external package once Goby grows a
  package mechanism. Nothing in the library imports from sibling
  `examples/*.gb`.
- Top-level demo files (`examples/parser_*.gb`) match the existing
  `examples/` convention and are discoverable to anyone browsing the
  repo's example list. After migration, these stay behind as "usage
  examples for the external package".

If Track RP-3 surfaces unexpected limitations (e.g. cycle detection
reveals a structural issue with the proposed file split), the response
is to address it inside Track RP, not to fall back to stdlib placement.
Stdlib placement is rejected by the lock in `tmp/pc.md`.

### 4.4 Public Surface

#### Types

```goby
type Position = Position(offset: Int, line: Int, column: Int)

type Token = Token(value: String, position: Position)

type ParseError = ParseError(
  position: Position,
  expected: List String,
  message:  String
)

type ParseResult a = Ok(a) | Err(ParseError)

type Parser a = Parser(run: Unit -> a can Token, Fail)
```

**`Cursor` is internal-only and never appears in any public type.**
PC-1 implements explicit cursor threading inside the library
(`driver.gb`, `core.gb`'s `alt`), but `Cursor` is not exported, not
embedded in `ParseResult`, and not visible to users. This boundary is a
hard public/private rule, not a default. Reasoning:

- PC-2 will replace explicit cursor threading with branch-local handler
  state. Embedding `Cursor` in any public type would require a breaking
  change at PC-2.
- Users who need "remaining input after a partial parse" (REPL,
  incremental parse) will get a separate, post-PC-2 API
  (e.g. `parse_partial : Parser a -> String -> ParseResult (a, String)`)
  introduced once the long-term shape is clear. PC-1 deliberately does
  not ship that API.

Implementation hint: the parser library may use a `Cursor`-shaped record
internally, but its declaration lives in a non-exported module
(or is omitted from `examples/parser/README.md`'s public surface
listing). The §4.5 fixtures must not pattern-match `Cursor`.

#### Effects

```goby
effect Token
  peek    : Unit -> Maybe Token
  advance : Unit -> Unit

effect Fail
  fail : ParseError -> a

# PC-2 (reserved by name only):
# effect Choice
#   choose : Unit -> Bool
```

#### Core combinators (`core.gb`)

```goby
pure : a -> Parser a
map  : Parser a -> (a -> b) -> Parser b
bind : Parser a -> (a -> Parser b) -> Parser b
alt  : Parser a -> Parser a -> Parser a
eof  : Parser Unit
```

**On callback effects** — `list.map`'s row-polymorphic shape
(`(a -> b can {e}) -> List b can {e}`) does not transfer directly to
`Parser`. A `Parser`'s `run` is a *deferred* computation:
`run : Unit -> a can Token, Fail`. A callback's effects do not occur at
combinator-call time; they occur when the parser is later driven. The
correct place for callback effects is therefore inside the produced
parser's `run` row, not the outer combinator return.

PC-1 stance: keep `map` / `bind` callbacks pure in their public
signatures:

```goby
map  : Parser a -> (a -> b)        -> Parser b
bind : Parser a -> (a -> Parser b) -> Parser b
```

If `run`'s row needs to carry additional effects (e.g. user `Print`
inside a parser action), those are provided by the combinator's
implementation when it builds the new `Parser(run: ...)` value, not by
threading `{e}` through the public type. Lifting parser callbacks to
arbitrary effect rows requires representing `Parser`'s effect row as a
parameter (e.g. `Parser a {e}`), which is a surface change. **That work
is out of scope for PC-1 and is recorded as an open question for PC-2**
(see §7).

#### Extended combinators (`combinators.gb`)

```goby
many     : Parser a -> Parser (List a)        # zero or more
optional : Parser a -> Parser (Maybe a)
seq      : Parser a -> Parser b -> Parser (a, b)
between  : Parser open -> Parser close -> Parser a -> Parser a
```

`many` must reject (or runtime-error on) parsers that succeed without
consuming input, to avoid infinite loops. The check is documented and
enforced inside `many`.

#### Driver (`driver.gb`)

```goby
parse : Parser a -> String -> ParseResult a
```

The driver tokenises the input via `goby/string.graphemes`, installs the
`Token` and `Fail` handlers, and returns `ParseResult a`. The internal
`Cursor` is managed inside the driver and the `alt` combinator and is
never returned, never embedded in `ParseResult`, and never named in any
public signature (see §4.4 — `Cursor is internal-only`).

Backtracking semantics (publicly documented):

> `alt p q` is uncommitted choice: if `p` fails (regardless of how much
> input it consumed), the cursor is restored to the position where `alt`
> began and `q` is tried. PC-1 implements this via explicit cursor
> threading (no multi-shot resume). PC-2 will reimplement on top of the
> branch-local-state surface (§3.3) without changing the public API.
> Committed choice / `try` / `cut` are deferred to PC-2 or later.

### 4.5 Phases

#### PC-P0: Pre-flight (see §4.2)

- Run the three concrete probes listed in §4.2 (Track RP-3 multi-file
  import, cross-module `Maybe` import, free-type-variable return-type
  effect operation).
- Confirm §4.3 file layout is unblocked. Stdlib placement is rejected
  by lock and is not an option (see §1).

**Acceptance** — each probe has a defined pass / blocker outcome:

| Probe | Pass outcome | Failure outcome |
| ----- | ------------ | --------------- |
| Track GU-X2 closed (`Maybe`, `ParseResult`, `Parser` shapes type-check and run) | PC-P1 unblocked | **Blocker** — return to `doc/PLAN_GU.md` |
| Track RP-3 landed (multi-file `examples/` import) | PC-P1 unblocked | **Blocker** — return to `doc/PLAN.md` §4.5b |
| `Maybe` cross-module import (stdlib path) | PC-P1 unblocked | **Blocker** — fix import resolution before PC-P1 |
| `fail : ParseError -> a` polymorphism | PC-P1 unblocked | **Blocker** — fix in compiler before PC-P1 |

- All three probe fixtures live under `examples/_pc_preflight/` (or
  `tmp/pc_probe/`) and `goby check` is run on each.
- Outcomes are recorded in `tmp/pc.md` (PC-P0 section). Blockers are
  filed against the relevant track (RP, compiler, or this plan) with
  the specific change required.

#### PC-P1: Types and effects (no combinators)

- Add `examples/parser/types.gb` and `examples/parser/effects.gb`.
- Confirm the `Parser` record type, the two effects, and `ParseError` /
  `ParseResult` all type-check together.
- A trivial fixture constructs a `Parser Int` whose `run` literally returns
  `42` and is invoked through a hand-written driver-equivalent harness
  (without yet exposing `parse`). This proves the `Parser(run: ...)` shape
  is valid before any combinators are written.
- **Naming / export review** — before any of these names fossilise as a
  public API, audit them against likely user-code and future-stdlib
  collisions:
  - `Token`, `Fail`: very generic effect names. Decide whether to keep
    them flat (relying on `import goby/parser ( Token, Fail )` selective
    imports), prefix them (`ParserToken`, `ParserFail`), or keep flat
    while documenting the collision policy in `examples/parser/README.md`.
  - `Ok` / `Err` constructors of `ParseResult`: same audit. Track GU
    provides a constructor-name disambiguation rule
    (`doc/PLAN_GU.md` §6 GU-S3): scrutinee type → single-import →
    diagnostic with qualified suggestion. So a future `Result` stdlib
    track sharing `Ok` / `Err` is **not a hard problem** — qualified
    `ParseResult.Ok` works, and bare `Ok` is disambiguated by context
    where possible. PC-P1 records this rationale in
    `examples/parser/README.md`. **PC tests must not byte-match GU's
    ambiguous-constructor diagnostic wording** unless GU-X0 has pinned
    that fixture; until then, PC tests assert structurally (e.g.
    "diagnostic mentions both candidate types") rather than by exact
    string match.
  - `Position`: likely to be reused outside parsing (diagnostics,
    incremental tooling). Decide whether `Position` belongs in
    `stdlib/goby/position.gb` long-term, with the parser library
    importing it.
  - Resolution policy: the audit produces either a concrete rename
    decision or a documented "stay flat" rationale, recorded in
    `examples/parser/README.md`. Anything left unresolved at PC-P1 close
    is filed as a follow-up before PC-P3 (where `alt` makes
    `ParseResult`'s constructors load-bearing).

**Acceptance**:

- `cargo test -p goby-core --lib` passes.
- The PC-P1 fixture compiles via `goby check`.
- `examples/parser/README.md` exists and contains a "Public API"
  section listing every name (`Parser`, `ParseResult`, `ParseError`,
  `Position`, `Token` (effect), `Fail` (effect), `Ok`, `Err`) with
  the resolution from the naming audit.

#### PC-P2: Core combinators + driver MVP

- Add `core.gb` with `pure / map / bind / eof` (NOT `alt` yet).
- Add `driver.gb` with `parse : Parser a -> String -> ParseResult a`.
- The driver installs `Token` (cursor-as-explicit-state) and `Fail` handlers.
- First end-to-end demo: parse a single grapheme and `eof`, returning `Ok`
  on the empty tail and `Err` otherwise.

`alt` is intentionally deferred to PC-P3 because it is the combinator that
introduces the cursor-rollback discipline.

**Acceptance**:

- `examples/parser_number.gb` parses one digit via
  `pure` + `bind` + `eof`, importing combinators from
  `examples/parser/` via the Track RP import surface.
- `goby check` succeeds across all parser library files
  (`examples/parser/*.gb`) and the top-level demo
  (`examples/parser_number.gb`). Substitute the project's actual driver
  command (e.g. `cargo run -p goby-cli -- check ...`) if the binary name
  differs at PC-P0 time.
- If the wasm backend can host the example (lambda-in-handler limit
  permitting; see `examples/hof_effect.gb` gating), `goby run` produces a
  recorded expected output (fixture file, byte-compared). Otherwise the
  example is gated with a comment matching the existing convention and
  recorded in `doc/STATE.md`.

#### PC-P3: `alt` with explicit cursor threading

- Add `alt : Parser a -> Parser a -> Parser a` to `core.gb`.
- Implement uncommitted-choice semantics by saving the `Cursor` before
  running `p`, and restoring it before running `q` if `p` fails.
- Implement `alt`'s error merge: on both-failed, return the failure with
  the larger `position.offset`; on equal offsets, concatenate `expected`
  lists. Ordering of `expected` is documented as left-then-right.
- Demo: parse `"yes"` or `"no"` via `alt`.

**Acceptance**:

- `examples/parser_number.gb` extended (or a sibling) parses a
  signed integer via `alt(sign, pure unit)` then digits. Inputs `"42"`
  and `"-7"` produce `Ok(42)` and `Ok(-7)` respectively, compared
  against a recorded fixture.
- `goby check` succeeds.
- Backtracking probe: an in-library test (under `examples/parser/` —
  not user-visible) where `alt(p, q)` is invoked with `p` consuming
  exactly one grapheme then failing, and `q` is a parser that succeeds
  only when the cursor has been rolled back. Library-internal cursor
  inspection is allowed here precisely because it lives behind the
  public boundary established in §4.4. The probe is required before
  PC-P4 begins. End-user demos must never mention `Cursor`.

#### PC-P4: Extended combinators

- Add `many / optional / seq / between` in `combinators.gb`.
- `many` enforces the no-progress check at runtime; on detection it raises
  `Fail` with a message that names the offending combinator (the message
  is finalised here so PC-3 polish does not have to revisit it).
- `seq` and `between` are written via `bind` for consistency.

**Acceptance**:

- `examples/parser_arith.gb` parses an arithmetic expression
  grammar without left recursion (e.g. `expr := term ('+' term)*`).
  Inputs `"1+2*3"` and `"(1+2)*3"` produce results compared against a
  recorded fixture (`examples/parser_arith.expected`).
- `examples/parser_json_lite.gb` parses a chosen JSON subset
  (numbers, strings, arrays of those) end to end. At least one of:
  `[]`, `[1, 2, 3]`, `[\"a\"]`, `[1, [2, 3]]` is in the fixture.
- **`many` no-progress test (required, deterministic, no timeout
  reliance)**: a fixture parser of shape `many (pure unit)` is invoked.
  The expected outcome is a `Fail` carrying the finalised no-progress
  message string, byte-compared against the fixture. The check completes
  in well under one second; reliance on `nextest.toml`'s slow-timeout
  for safety is explicitly disallowed.

#### PC-P5: Diagnostics polish

- Position-aware rendering of `ParseError` in demos:
  `expected <a> | <b> | ... at line:col, got <token>`.
- A small helper inside the parser library (`examples/parser/`,
  not `stdlib/`) for assembling the `expected ... got ...` message,
  so demos stay readable.
- No new combinators land here; this phase is presentation only.

**Acceptance**:

- For `number.gb` and `arith.gb`, an intentionally-failing input
  (e.g. `"1++2"` for arith, `"-x"` for number) produces stdout that
  byte-matches a recorded `.expected` fixture file. The fixture
  literally contains the rendered `expected ... got ... at line:col`
  string.

#### PC-P6: Multi-shot migration plan note (no code)

This phase is a doc gate, not code. It exists to make sure the public
API chosen in PC-P1–P4 will not need to break when PC-2 begins.

**Deliverables**:

- The `examples/parser/README.md` created in PC-P1 (with its existing
  `## Public API` section) gains three new sections:
  - `## Stable public API` — every public type, effect, and function;
    each marked "stable across PC-2" or "may change at PC-2".
  - `## PC-1 internals` — `Cursor` threading, the `alt` cursor-rollback
    pattern, the no-progress check inside `many`. Items expected to
    disappear or move into a handler at PC-2 are tagged.
  - `## PC-2 migration` — concrete changes when §3.3 branch-local state
    lands. Maps each PC-1 internal item to its PC-2 replacement.
- `doc/PLAN.md` §4.6 gains a bullet list "Stable vs. replaced at PC-2"
  mirroring the README sections, so the top-level roadmap stays in sync.

**Acceptance**:

- The three named sections exist in `examples/parser/README.md`.
- For every public name listed in the PC-P1 naming-audit roster (see
  the §4.5 PC-P1 acceptance), the README "Stable public API" section
  has a one-line entry tagged either "stable" or "may change".
- `doc/PLAN.md` §4.6 contains the matching bullet list.

**Track PC-P acceptance checklist** (final gate, not a restatement of
phase criteria):

- [ ] `goby check` succeeds on every file under `examples/parser/` and
      every `examples/parser_*.gb` demo.
- [ ] `goby run examples/parser_arith.gb` byte-matches
      `examples/parser_arith.expected`. If wasm-gated, the gating reason
      is recorded in `doc/STATE.md` and at least one non-trivial demo
      (target: `arith`) runs end to end on the interpreter path.
- [ ] `examples/parser/README.md` exists and contains the
      "Public API", "Stable public API", "PC-1 internals", and
      "PC-2 migration" sections (the first from PC-P1, the rest from
      PC-P6).
- [ ] No public type, effect, function, or constructor mentions
      `Cursor`. Library-internal use is allowed; user-visible surface
      is not.
- [ ] Every public combinator has a behaviour note (one or two lines)
      directly above its declaration in the source.
- [ ] `doc/PLAN.md` §4.6 lists stable vs. replaced PC-2 pieces and
      cross-references this plan.
- [ ] `doc/STATE.md` records, by name, every parser demo that is
      `goby check`-only.

---

## 5. Cross-cutting Concerns

### 5.1 Backend Reach

The wasm backend currently has known limitations around lambdas inside
handler-lowered code (`examples/hof_effect.gb` is gated for `goby run`).
The parser combinator is squarely in that territory.

Mitigation:

- Each demo records, at the top of the file, whether it is `goby check`-only
  or `goby run`-clean.
- `doc/STATE.md` tracks gated demos so the WB roadmap can lift them.
- The plan does **not** require `goby run` parity for every demo to call
  PC-P "done". It does require it for at least one non-trivial demo
  (target: `arith`).

### 5.2 Spec / Examples / Diagnostics Sync

For each shipping milestone:

- Spec changes (if any) land in the same change as the code.
- Diagnostic wording is preserved unless the change is explicitly about
  diagnostics (PC-P5 only).
- The `goby-invariants` skill runs before each commit.

### 5.3 Test Runner Choice

- `goby-core` lib tests run via `cargo test -p goby-core --lib`
  (sub-second).
- `goby-wasm` integration runs via `cargo nextest run -p goby-wasm`
  (necessary to keep wall time tractable; `nextest.toml` per-test
  slow-timeout already prevents infinite-loop hangs).
- For localised parser changes, prefer narrowed runs:
  `cargo test -p goby-wasm --lib <module>::tests`.

### 5.4 Out of Scope (Track PC)

These belong to later tracks and are explicitly excluded from PC-M and
PC-P deliverables:

- `Result a e` ADT (a separate stdlib track; do not bundle).
- Operator aliases (`>>=`, `<|>`, `*>`, `<*`, `<$>`) — PC-3 or later.
- `chainl` / `chainr` / `sep_by` / `many1` — added once PC-2 lands.
- Generic `Parser token a` over arbitrary token streams — long-term
  follow-up.
- `try` / `cut` / committed choice — depends on §3.3 branch-local state.
- Byte-level tokenisation — depends on a Goby byte-API surface that does
  not yet exist.

---

## 6. Milestone Summary

| ID    | Name                                  | Depends on  | Output                                  |
| ----- | ------------------------------------- | ----------- | --------------------------------------- |
| PC-M0 | `Maybe` ADT introduction              | **Track GU-X2 + Track EX-S1** | `stdlib/goby/maybe.gb` (type only) + 1 fixture |
| PC-M1 | `Maybe` inspection helpers            | PC-M0       | `is_just`, `is_nothing`, `unwrap_or` + `examples/maybe_inspect.gb` |
| PC-M2 | `Maybe` row-polymorphic combinators   | PC-M1       | `map`, `and_then`, `to_list` + `examples/maybe_pure.gb`, `examples/maybe_effect.gb` |
| PC-M3 | `Maybe` docs / invariants gate        | PC-M2       | `LANGUAGE_SPEC.md` mention + `examples/maybe.md` overview + `goby-invariants` pass |
| PC-P0 | Pre-flight probes                     | PC-M0, **Track RP-3** (Track GU-X2 already gated by PC-M0) | 3 probe fixtures + recorded layout decision |
| PC-P1 | Parser types + effects + naming audit | PC-P0 (transitively requires GU-X2 for `Parser a` generic record + `ParseResult a` generic union) | `types.gb`, `effects.gb`, `examples/parser/README.md` (`## Public API`) |
| PC-P2 | Core combinators + driver (no `alt`)  | PC-P1, PC-M1 | `pure / map / bind / eof`, `parse`, `examples/parser_number.gb` |
| PC-P3 | `alt` with explicit cursor            | PC-P2       | `alt`, error merge, in-library backtracking probe |
| PC-P4 | Extended combinators                  | PC-P3, PC-M2 | `many / optional / seq / between`, `examples/parser_arith.gb`, `examples/parser_json_lite.gb`, `many` no-progress fixture |
| PC-P5 | Diagnostics polish                    | PC-P4       | `expected … got …` rendering + failing-input fixtures |
| PC-P6 | PC-2 migration note (doc-only)        | PC-P4, PC-M3 | README sections (`## Stable public API`, `## PC-1 internals`, `## PC-2 migration`) + `PLAN.md` §4.6 update |

Track ordering across Track GU, Track RP, PC-M, and PC-P (rationale
for each cross-track edge in the table above):

- **Track GU-X2 → PC-M0** — `type Maybe a = Just(a) | Nothing` requires
  generic union with constructor args (GU-S3 typecheck + GU-S4 lowering).
  GU-X2 is the closed-form green check across both groups; PC-M0 is
  gated on it. See `doc/PLAN_GU.md` §7.1.
- **Track EX-S1 → PC-M0** — Track EX (`doc/PLAN.md` §4.5c) lifts
  non-exhaustive `case` from a runtime trap (the GU-S4 interim
  contract) to a compile-time error. Track PC's `case` matches over
  `Maybe` / `ParseResult` rely on this static guarantee; PC does not
  start until EX-S1 (the typecheck pass) closes. The user direction
  recorded 2026-05-08 is that PC must inherit static exhaustiveness,
  not the runtime-trap interim. See `doc/PLAN.md` §4.5c.
- **Track GU-X2 → PC-P1** — `Parser a = Parser(run: ...)` is a generic
  record; `ParseResult a = Ok(a) | Err(ParseError)` is a generic union.
  Both shapes are in scope for Track GU (`doc/PLAN_GU.md` §2). Without
  GU-X2 the parser library cannot be declared. PC-M0 already gates on
  GU-X2, so PC-P1 inherits the dependency transitively, but the
  rationale is independent.
- **Track RP-3 → PC-P0** — without relative-path / workspace-rooted
  imports, the §4.3 multi-file layout under `examples/parser/` is
  unreachable. Stdlib placement is rejected by lock, so PC cannot
  start until RP-3 lands. See `doc/PLAN.md` §4.5b.
- **PC-M0 → PC-P0** — `Token.peek : Unit -> Maybe Token` uses `Maybe`.
  PC-P0's `Maybe` cross-module probe verifies the resolver path the
  parser code will rely on; that probe needs `stdlib/goby/maybe.gb` to
  exist.
- **PC-M1 → PC-P2** — `is_just` / `unwrap_or` keep parser code readable
  without forcing `case` matches at every `Token.peek` site.
- **PC-M2 → PC-P4** — `optional : Parser a -> Parser (Maybe a)` returns
  `Maybe`; downstream user code benefits from having `Maybe.map` /
  `and_then` already available.
- **PC-M3 → PC-P6** — the migration-note doc gate references the
  finalised stdlib `Maybe` documentation.

Within each track, milestones are strictly ordered.

---

## 7. Open Questions Carried Forward

These remain open. Each names the milestone where it must be resolved:

1. **Resolve in PC-P1** — `Either` / `Result` for the driver return vs.
   parser-local `ParseResult`. Default: keep `ParseResult` parser-local
   until a separate `Result` stdlib track lands.
2. **Resolve empirically in PC-P4** — wasm backend reach for `arith` /
   `json_lite` demos under the current lambda-in-handler limits.
3. **Deferred to PC-2 surface review** — lifting `Parser` to an
   effect-row-parameterised form (e.g. `Parser a {e}`) so combinator
   callbacks can perform user-defined effects. This is *not* a PC-1
   question; PC-1 keeps callbacks pure (see §4.4). Recorded here so the
   PC-2 surface review does not forget it.
4. **Deferred to a post-PC-2 follow-up** — a partial-parse API
   (e.g. `parse_partial : Parser a -> String -> ParseResult (a, String)`)
   that exposes "remaining input" for REPL / incremental use cases.
   Intentionally not shipped in PC-1 (would force `Cursor` or its
   replacement into the public surface).

### Implementation note on `Parser.run`

The public `Parser` shape — `Parser(run: Unit -> a can Token, Fail)` — is
**locked** by `tmp/pc.md` and is not an open question. PC-1
implementations are free to use `ParseResult`-returning *internal helpers*
(easier to write under explicit cursor threading) as long as the public
`Parser(run: ...)` wrapper is preserved. If experience during PC-P2/P3
shows the locked shape is wrong, update `tmp/pc.md` first, then this plan.
