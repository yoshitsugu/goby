## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

### Nested `List` literals fail under `goby run`

- Status: reproducible
- Affected area: `goby run` fallback/static runtime-output resolution

#### Reproduction

Source:

```goby
main : Unit -> Unit can Print
main =
  a = [[1,2,3], [4,5,6], [7, 8, 9]]
  b = a[0][1]
  println("${b}")
```

Command:

```sh
goby run /home/yoshitsugu/current/test2.gb
```

Observed result:

```text
codegen error: main lowered as effect boundary (style=EffectBoundary, selected_mode=PortableFallback, selected_mode_fallback_reason=Some(RuntimeProfileNotSupported), runtime_profile=Unknown, typed_continuation_ir_present=false, handlers_resume=false, handler_with_count=0, handler_with_unsupported=false, evidence_ops=0, evidence_requirements=1, evidence_fingerprint_hint=5); fallback runtime output could not be resolved
```

Expected result:

```text
2
```

#### Notes

The program type-checks, so this appears to be a backend/runtime limitation rather than
a source-language error.

Current hypothesis:

- The fallback runtime/static output resolver only models `ListInt` and `ListString`.
- Nested list values such as `List (List Int)` are not representable in that runtime value model.
- As a result, `[[1,2,3], [4,5,6], [7,8,9]]` or the chained lookup `a[0][1]` fails during
  compile-time output resolution, and `goby run` aborts before Wasm execution.

### Nested `List` index inside interpolation also fails under `goby run`

- Status: reproducible
- Affected area: `goby run` fallback/static runtime-output resolution for interpolated strings

#### Reproduction

Source:

```goby
main : Unit -> Unit can Print
main =
  a = [[1,2,3], [4,5,6], [7, 8, 9]]
  println("${a[0][1]}")
```

Command:

```sh
goby run /home/yoshitsugu/current/test2_inline.gb
```

Observed result:

```text
codegen error: main lowered as effect boundary (style=EffectBoundary, selected_mode=PortableFallback, selected_mode_fallback_reason=Some(RuntimeProfileNotSupported), runtime_profile=Unknown, typed_continuation_ir_present=false, handlers_resume=false, handler_with_count=0, handler_with_unsupported=false, evidence_ops=0, evidence_requirements=1, evidence_fingerprint_hint=5); fallback runtime output could not be resolved
```

Expected result:

```text
2
```

#### Notes

This does not appear to be a generic interpolation bug. A non-nested variant such as:

```goby
main : Unit -> Unit can Print
main =
  a = [1,2,3]
  println("${a[1]}")
```

executes successfully and prints `2`.

Current hypothesis:

- Interpolation itself can handle indexed `List Int` values.
- The failure still originates earlier, when the fallback runtime tries to construct or traverse
  the nested value `List (List Int)`.
- In this shape, the nested-list limitation is surfaced through interpolated-string evaluation,
  but the underlying missing capability is still the lack of nested-list runtime values in the
  fallback/static resolver.
