# Examples

This directory contains small Goby programs grouped by purpose.

## Start Here

- `hello.gb`: minimal runnable example (`main` + `println`)
- `basic_types.gb`: type and syntax showcase (parse/typecheck target)
- `function.gb`: canonical function/lambda/list example used as run-parity target
- `function_reference.gb`: named-function callback sample (`map xs add_ten`)
- `closure_capture.gb`: immutable/by-value closure capture sample (direct call plus captured `map` callback)
- `closure_mut.gb`: mutable capture sample (outer mutation remains visible through a closure)
- `read.gb`: minimal stdin sample using prelude `Read.read_line` and `Read.read`
- `to_integer.gb`: `int.parse` sample with `StringParseError` handler
- `to_string.gb`: `int.to_string` sample for explicit `Int` to `String` conversion
- `string_graphemes.gb`: `string.graphemes` sample returning Unicode grapheme clusters
- `list_case.gb`: list `case` pattern sample (`[]`, `[1]`, `[4, ..]`, `[a, ..b]`, `[_, _]`, `_`)
- `list_spread.gb`: expression-side list spread sample (`[1, ..xs]`, `[1, ..map xs f]`)
- `operators.gb`: arithmetic / comparison / equality / boolean operator sample
- `case_arm_block.gb`: `case` arm block sample (`pattern ->` + indented block body)
- `effect_generic.gb`: generic effect header sample (`effect Stream a b`)

## Grouped Samples

- `print/`: focused print-related examples
  - `local_binding.gb`: println a local string binding
  - `concat.gb`: println interpolated string result
  - `println.gb`: `print` (no newline) と `println` (末尾改行あり) の差分
- `parser/`: parser behavior examples
  - `mixed_indent.gb`: mixed tabs/spaces indentation sample
- `iterator.gb`: unified iterator runtime contract sample (`import goby/iterator`)
  - minimal state-less mode sample used by runtime-lock tests
  - emits:
    - `tick:a`
    - `tick:b`
    - `tick:c`
- `iterator_unified.gb`: unified iterator contract modes sample
  - demonstrates:
    - state-less mode (`b = Unit` style)
    - state-threaded mode (`b = Int`)
    - early-stop via `(False, state)`
  - emits:
    - `tick:a`
    - `tick:b`
    - `tick:c`
    - `3`
    - `1`

## Planned Stdlib Syntax (Not Implemented Yet)

- Standard-library planning includes a `goby/stdio` module and a stdlib-only
  embed annotation for runtime-bridged effects.
- Illustrative target shape:
  - `@embed Print __goby_embeded_effect_stdout_handler`
  - `print : String -> Unit can Print`
  - `println : String -> Unit can Print`
- Important: `@embed` is planned as stdlib-only and must be rejected in user code.
