# Examples

This directory contains small Goby programs grouped by purpose.

## Start Here

- `hello.gb`: minimal runnable example (`main` + `print`)
- `basic_types.gb`: type and syntax showcase (parse/typecheck target)
- `function.gb`: canonical function/lambda/list example used as run-parity target
- `read.gb`: minimal stdin sample using prelude `Read.read_line` and `Read.read`
- `to_integer.gb`: `int.parse` sample with `StringParseError` handler
- `list_case.gb`: list `case` pattern sample (`[]`, `[1]`, `[4, ..]`, `[a, ..b]`, `[_, _]`, `_`)
- `case_arm_block.gb`: `case` arm block sample (`pattern ->` + indented block body)

## Grouped Samples

- `print/`: focused print-related examples
  - `local_binding.gb`: print a local string binding
  - `concat.gb`: print interpolated string result
- `parser/`: parser behavior examples
  - `mixed_indent.gb`: mixed tabs/spaces indentation sample
- `iterator.gb`: iterator-like effect/handler sample using `with_handler` with lexical capture
  - emits:
    - `tick:a`
    - `tick:b`
    - `tick:c`

## Planned Stdlib Syntax (Not Implemented Yet)

- Standard-library planning includes a `goby/stdio` module and a stdlib-only
  embed annotation for runtime-bridged effects.
- Illustrative target shape:
  - `@embed Print __goby_embeded_effect_stdout_handler`
  - `print : String -> Unit can Print`
- Important: `@embed` is planned as stdlib-only and must be rejected in user code.
