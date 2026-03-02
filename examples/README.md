# Examples

This directory contains small Goby programs grouped by purpose.

## Start Here

- `hello.gb`: minimal runnable example (`main` + `print`)
- `basic_types.gb`: type and syntax showcase (parse/typecheck target)
- `function.gb`: canonical function/lambda/list example used as run-parity target

## Grouped Samples

- `print/`: focused print-related examples
  - `local_binding.gb`: print a local string binding
  - `concat.gb`: print `string.concat(...)` result
- `parser/`: parser behavior examples
  - `mixed_indent.gb`: mixed tabs/spaces indentation sample
- `iterator.gb`: iterator-like effect/handler sample using `resume`
  - emits:
    - `tick`
    - `tick`
    - `tick`

## Planned Stdlib Syntax (Not Implemented Yet)

- Standard-library planning includes a `goby/stdio` module and a stdlib-only
  embed annotation for runtime-bridged effects.
- Illustrative target shape:
  - `@embed effect Print`
  - `print : String -> Unit can Print`
- Important: `@embed` is planned as stdlib-only and must be rejected in user code.
