## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

No confirmed open bugs are currently tracked here.

Resolved on 2026-04-06:

- nested `List` reads under `goby run`
- nested `List` index interpolation under `goby run`
- mutable nested-list rooted updates under `goby run`

Current boundary note:

- rooted mutable list updates are intentionally routed through the `GeneralLowered`
  Goby-owned Wasm path for normal `goby run` planning.
- fallback/interpreter execution now has centralized rooted-update semantics for the supported
  recursive-aggregate subset, so parity tests can exercise the same language behavior there too.
- further recursive-aggregate fallback convergence remains planning work, but it is
  no longer allowed to surface as a late `fallback runtime output could not be resolved`
  failure for well-typed mutable-list programs.
