//! Pointer-width–aware Wasm instruction helpers for the memory64 migration.
//!
//! All linear-memory address operations must go through these helpers so that
//! switching `PtrWidth` from `W32` to `W64` (memory64) is a single-flag change.
//!
//! **What changes between W32 and W64:**
//! - `I32Load*` / `I32Store*` → `I64Load*` / `I64Store*`
//! - `I32Const <addr>` → `I64Const <addr>`
//! - `I32Add` / `I32Sub` / `I32Mul` / `I32DivU` on address operands → 64-bit equivalents
//! - `MemoryGrow` / `MemorySize` produce/consume i64 in memory64 (instruction unchanged;
//!   callers must push/pop the correct type).
//!
//! **What does NOT change:**
//! - Tag-bit / payload i32 arithmetic (`I32Const TAG_INT`, `I32And`, `I32ShrS`, …)
//! - WASI ABI call arguments (always i32 even under memory64)
//! - `MemArg.offset` and `MemArg.align` fields

use wasm_encoder::{Instruction, MemArg};

/// Whether the module's linear memory uses 32-bit or 64-bit addresses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PtrWidth {
    W32,
    W64,
}

impl PtrWidth {
    pub(crate) fn from_memory64(memory64: bool) -> Self {
        if memory64 {
            PtrWidth::W64
        } else {
            PtrWidth::W32
        }
    }
}

/// Load an i32/i64 (pointer-sized) value from linear memory.
pub(crate) fn ptr_load(pw: PtrWidth, mem_arg: MemArg) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32Load(mem_arg),
        PtrWidth::W64 => Instruction::I64Load(mem_arg),
    }
}

/// Store an i32/i64 (pointer-sized) value to linear memory.
pub(crate) fn ptr_store(pw: PtrWidth, mem_arg: MemArg) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32Store(mem_arg),
        PtrWidth::W64 => Instruction::I64Store(mem_arg),
    }
}

/// Load an unsigned byte from linear memory, zero-extended to the pointer width.
pub(crate) fn ptr_load_8u(pw: PtrWidth, mem_arg: MemArg) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32Load8U(mem_arg),
        PtrWidth::W64 => Instruction::I64Load8U(mem_arg),
    }
}

/// Push an address constant onto the stack.
pub(crate) fn ptr_const(pw: PtrWidth, v: u64) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32Const(v as i32),
        PtrWidth::W64 => Instruction::I64Const(v as i64),
    }
}

/// Add two pointer-width values.
pub(crate) fn ptr_add(pw: PtrWidth) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32Add,
        PtrWidth::W64 => Instruction::I64Add,
    }
}

/// Subtract two pointer-width values.
pub(crate) fn ptr_sub(pw: PtrWidth) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32Sub,
        PtrWidth::W64 => Instruction::I64Sub,
    }
}

/// Multiply two pointer-width values.
pub(crate) fn ptr_mul(pw: PtrWidth) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32Mul,
        PtrWidth::W64 => Instruction::I64Mul,
    }
}

/// Unsigned division of two pointer-width values.
pub(crate) fn ptr_div_u(pw: PtrWidth) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32DivU,
        PtrWidth::W64 => Instruction::I64DivU,
    }
}

/// Store a byte value to linear memory (address is pointer-width; value is i32 regardless).
pub(crate) fn ptr_store_8(pw: PtrWidth, mem_arg: MemArg) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32Store8(mem_arg),
        PtrWidth::W64 => Instruction::I64Store8(mem_arg),
    }
}

/// Equality comparison: produces an i32 boolean (0 or 1) in both modes.
pub(crate) fn ptr_eq(pw: PtrWidth) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32Eq,
        PtrWidth::W64 => Instruction::I64Eq,
    }
}

/// Unsigned less-than comparison: produces an i32 boolean in both modes.
pub(crate) fn ptr_lt_u(pw: PtrWidth) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32LtU,
        PtrWidth::W64 => Instruction::I64LtU,
    }
}

/// The sentinel value returned by `memory.grow` on failure (-1 in the pointer width).
pub(crate) fn ptr_neg_one(pw: PtrWidth) -> Instruction<'static> {
    match pw {
        PtrWidth::W32 => Instruction::I32Const(-1),
        PtrWidth::W64 => Instruction::I64Const(-1),
    }
}

/// Extend a pointer-width value to i64 (no-op in W64, `I32WrapI64` inverse:
/// `I64ExtendI32U` in W32).  Used when a host-side i64 is needed from a Wasm address.
pub(crate) fn ptr_extend_to_i64(pw: PtrWidth) -> Option<Instruction<'static>> {
    match pw {
        PtrWidth::W32 => Some(Instruction::I64ExtendI32U),
        PtrWidth::W64 => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_encoder::{
        CodeSection, ExportKind, ExportSection, Function, FunctionSection, MemorySection,
        MemoryType, Module, TypeSection, ValType,
    };
    use wasmparser::Validator;

    fn default_mem_arg() -> MemArg {
        MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }
    }

    /// Build a minimal Wasm module with one function that executes `body_instrs`.
    /// `param` / `result` describe the function signature (ValType).
    /// Used to validate that a sequence of instructions type-checks correctly.
    fn validate_fn(
        memory64: bool,
        params: &[ValType],
        results: &[ValType],
        body: impl Fn(&mut Function),
    ) {
        let mut types = TypeSection::new();
        types
            .ty()
            .function(params.iter().copied(), results.iter().copied());

        let mut functions = FunctionSection::new();
        functions.function(0);

        let mut memories = MemorySection::new();
        memories.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64,
            shared: false,
            page_size_log2: None,
        });

        let mut exports = ExportSection::new();
        exports.export("f", ExportKind::Func, 0);

        let mut function = Function::new([]);
        body(&mut function);
        function.instruction(&Instruction::End);

        let mut code = CodeSection::new();
        code.function(&function);

        let mut module = Module::new();
        module.section(&types);
        module.section(&functions);
        module.section(&memories);
        module.section(&exports);
        module.section(&code);

        let wasm = module.finish();
        Validator::new()
            .validate_all(&wasm)
            .expect("module should pass wasm validation");
    }

    #[test]
    fn ptr_load_w32_validates() {
        // (i32) -> i32 : i32.load from address on stack
        validate_fn(false, &[ValType::I32], &[ValType::I32], |f| {
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&ptr_load(PtrWidth::W32, default_mem_arg()));
        });
    }

    #[test]
    fn ptr_load_w64_validates() {
        // memory64: (i64) -> i64 : i64.load from address on stack
        validate_fn(true, &[ValType::I64], &[ValType::I64], |f| {
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&ptr_load(PtrWidth::W64, default_mem_arg()));
        });
    }

    #[test]
    fn ptr_store_w32_validates() {
        validate_fn(false, &[ValType::I32, ValType::I32], &[], |f| {
            f.instruction(&Instruction::LocalGet(0)); // address
            f.instruction(&Instruction::LocalGet(1)); // value
            f.instruction(&ptr_store(PtrWidth::W32, default_mem_arg()));
        });
    }

    #[test]
    fn ptr_store_w64_validates() {
        validate_fn(true, &[ValType::I64, ValType::I64], &[], |f| {
            f.instruction(&Instruction::LocalGet(0)); // address
            f.instruction(&Instruction::LocalGet(1)); // value
            f.instruction(&ptr_store(PtrWidth::W64, default_mem_arg()));
        });
    }

    #[test]
    fn ptr_const_w32_validates() {
        validate_fn(false, &[], &[ValType::I32], |f| {
            f.instruction(&ptr_const(PtrWidth::W32, 0x1234));
        });
    }

    #[test]
    fn ptr_const_w64_validates() {
        validate_fn(true, &[], &[ValType::I64], |f| {
            f.instruction(&ptr_const(PtrWidth::W64, 0x1234));
        });
    }

    #[test]
    fn ptr_add_w32_validates() {
        validate_fn(false, &[ValType::I32, ValType::I32], &[ValType::I32], |f| {
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&ptr_add(PtrWidth::W32));
        });
    }

    #[test]
    fn ptr_add_w64_validates() {
        validate_fn(true, &[ValType::I64, ValType::I64], &[ValType::I64], |f| {
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&ptr_add(PtrWidth::W64));
        });
    }

    #[test]
    fn ptr_neg_one_w32_validates() {
        validate_fn(false, &[], &[ValType::I32], |f| {
            f.instruction(&ptr_neg_one(PtrWidth::W32));
        });
    }

    #[test]
    fn ptr_neg_one_w64_validates() {
        validate_fn(true, &[], &[ValType::I64], |f| {
            f.instruction(&ptr_neg_one(PtrWidth::W64));
        });
    }

    #[test]
    fn from_memory64_false_is_w32() {
        assert!(matches!(PtrWidth::from_memory64(false), PtrWidth::W32));
    }

    #[test]
    fn from_memory64_true_is_w64() {
        assert!(matches!(PtrWidth::from_memory64(true), PtrWidth::W64));
    }
}
