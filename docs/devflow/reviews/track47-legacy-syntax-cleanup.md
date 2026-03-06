# Step Review

## Step

- Remove remaining legacy `with_handler` syntax and legacy `Unit` value
  expression support; require `with` and `()`.

## Contradiction Check

- Current language behavior is already standardized on `with` and `()`, so
  leaving `with_handler` or expression-form `Unit` in stdlib/tests/docs created
  inconsistency.
- Runtime still accepted expression-form `Unit` in a few compatibility paths,
  which contradicted the current syntax direction.
- The change removes those compatibility paths and adds an explicit typecheck
  error for legacy `Unit` value syntax.

## Extensibility Check

- Removing legacy syntax reduces branching in parser/runtime behavior and keeps
  future diagnostics/LSP work focused on one canonical surface.
- The explicit `legacy_unit_value_syntax` error gives a stable migration target
  if more fixtures accidentally reintroduce the old spelling.

## Maintainability Check

- Stdlib/examples/tests now consistently use `with` and `()`.
- Runtime no longer special-cases `Unit` as a value identifier.
- Tooling syntax packs/docs no longer advertise `with_handler`.

## Security Check

- No new unsafe or I/O-affecting behavior was introduced.
- The main risk was accidental semantic drift from leaving hidden compatibility
  paths in place; this change reduces that risk.

## Issues Found

- `stdlib/goby/string.gb` and `stdlib/goby/int.gb` still used `with_handler`.
- Runtime still treated `Unit` as a value expression in AST and string fallback
  paths.
- Tooling docs/syntax metadata still advertised `with_handler`.

## Fixes Applied

- Replaced remaining `with_handler` occurrences with `with`.
- Replaced remaining expression-form `Unit` values with `()`.
- Added typecheck rejection for legacy `Unit` value expressions.
- Updated parser expectations and syntax/tooling docs to the canonical forms.
