## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

No confirmed open bugs are currently tracked here.

Resolved on 2026-04-06:

- nested `List` reads under `goby run`
- nested `List` index interpolation under `goby run`
- mutable nested-list rooted updates under `goby run`

Current boundary note:

- rooted mutable list updates are intentionally routed through the `GeneralLowered`
  Goby-owned Wasm path rather than the fallback/static runtime-output resolver.
- further recursive-aggregate fallback convergence remains planning work, but it is
  no longer allowed to surface as a late `fallback runtime output could not be resolved`
  failure for well-typed mutable-list programs.
