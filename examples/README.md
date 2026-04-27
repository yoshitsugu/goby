# Examples

Small Goby programs intended for users to read and run.

## First Run

- `hello.gb`: minimal runnable program.
- `function.gb`: canonical function, lambda, list, and higher-order function sample.
- `control_flow.gb`: `case` and `if` expressions.
- `type.gb`: aliases, union variants, records, and field access.
- `effect.gb`: defining, using, and handling effects.
- `import.gb`: imports, aliases, and selected imports.

## Language Features

- `basic_types.gb`: integers, strings, tuples, and interpolation.
- `generic_types.gb`: generic functions and nested generic types.
- `operators.gb`: arithmetic, comparison, equality, and boolean operators.
- `case_arm_block.gb`: indented `case` arm block bodies.
- `mut.gb`: mutable local bindings.
- `closure_capture.gb`: immutable closure capture.
- `closure_mut.gb`: mutable closure capture.
- `tco.gb`: direct tail-call optimization shapes.

## Lists

- `list_case.gb`: list pattern matching.
- `list_index.gb`: indexed access.
- `list_set.gb`: immutable point update with `list.set`.
- `list_spread.gb`: expression-side list spread.
- `fold.gb`: left fold with named callbacks.
- `function_reference.gb`: passing a named function as a callback.
- `list_iterator_effect.gb`: list traversal with the iterator effect.

## Stdlib And Effects

- `read.gb`: `Read.read_line` and `Read.read` surface.
- `to_integer.gb`: `int.parse` with an error handler.
- `to_string.gb`: explicit `Int` to `String` conversion.
- `string_graphemes.gb`: Unicode grapheme splitting.
- `iterator.gb`: minimal iterator effect handler.
- `iterator_unified.gb`: iterator state threading and early stop.
- `effect_generic.gb`: generic effect declaration.

## Print Variants

`print/` contains focused `print` / `println` and qualified `goby/stdio`
examples for tooling and stdlib surface checks.
