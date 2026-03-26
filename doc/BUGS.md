## Known Bugs

This document tracks confirmed, reproducible bugs in the current Goby toolchain.

At the moment, there are no active bugs recorded here.

Notes:

- The former Track H bugs are now covered by regression fixtures under
  [examples/bugs/](/home/yoshitsugu/src/github.com/yoshitsugu/goby/examples/bugs):
  - [runtime_read_captured_lambda.gb](/home/yoshitsugu/src/github.com/yoshitsugu/goby/examples/bugs/runtime_read_captured_lambda.gb)
    now fails with a precise closure-capture diagnostic instead of a generic
    runtime-I/O unsupported-shape error.
  - [runtime_read_tuple_member.gb](/home/yoshitsugu/src/github.com/yoshitsugu/goby/examples/bugs/runtime_read_tuple_member.gb)
    now executes successfully and prints `1`.
