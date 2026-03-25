use crate::gen_lower::backend_ir::BackendIntrinsic;

/// Backend ownership split for grapheme-related host intrinsics.
///
/// Host-backed intrinsics cross an explicit Wasm import boundary owned by
/// `goby-wasm`; in-Wasm intrinsics remain emitter-owned on the tagged runtime ABI.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IntrinsicExecutionBoundary {
    HostImport,
    InWasm,
}

/// Fixed grapheme host import ABI owned by `goby-wasm`.
///
/// The value-level contract is intentionally narrow:
/// - arguments and returns use Goby's tagged `i64` runtime value ABI,
/// - only grapheme iteration crosses the host boundary,
/// - list accumulation stays in Wasm (`ListPushString`).
// All variants use the `String` prefix intentionally — these are string-domain host imports.
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HostIntrinsicImport {
    StringEachGraphemeCount,
    StringEachGraphemeState,
    StringConcat,
    StringGraphemesList,
}

impl HostIntrinsicImport {
    pub(crate) const MODULE: &'static str = "goby:runtime/track-e";

    pub(crate) const fn module(self) -> &'static str {
        let _ = self;
        Self::MODULE
    }

    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::StringEachGraphemeCount => "__goby_string_each_grapheme_count",
            Self::StringEachGraphemeState => "__goby_string_each_grapheme_state",
            Self::StringConcat => "__goby_string_concat",
            Self::StringGraphemesList => "__goby_string_graphemes_list",
        }
    }

    pub(crate) const fn params(self) -> &'static [wasm_encoder::ValType] {
        use wasm_encoder::ValType;
        match self {
            Self::StringEachGraphemeCount => &[ValType::I64],
            Self::StringEachGraphemeState => &[ValType::I64, ValType::I64],
            Self::StringConcat => &[ValType::I64, ValType::I64],
            Self::StringGraphemesList => &[ValType::I64],
        }
    }

    pub(crate) const fn results(self) -> &'static [wasm_encoder::ValType] {
        use wasm_encoder::ValType;
        &[ValType::I64]
    }
}

pub(crate) const HOST_INTRINSIC_IMPORTS: [HostIntrinsicImport; 4] = [
    HostIntrinsicImport::StringEachGraphemeCount,
    HostIntrinsicImport::StringEachGraphemeState,
    HostIntrinsicImport::StringConcat,
    HostIntrinsicImport::StringGraphemesList,
];

pub(crate) const fn host_import_for_intrinsic(
    intrinsic: BackendIntrinsic,
) -> Option<HostIntrinsicImport> {
    match intrinsic {
        BackendIntrinsic::StringEachGraphemeCount => {
            Some(HostIntrinsicImport::StringEachGraphemeCount)
        }
        BackendIntrinsic::StringEachGraphemeState => {
            Some(HostIntrinsicImport::StringEachGraphemeState)
        }
        BackendIntrinsic::StringConcat => Some(HostIntrinsicImport::StringConcat),
        BackendIntrinsic::StringGraphemesList => Some(HostIntrinsicImport::StringGraphemesList),
        BackendIntrinsic::StringSplit
        | BackendIntrinsic::ListGet
        | BackendIntrinsic::StringLength
        | BackendIntrinsic::ListPushString => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn host_intrinsic_imports_use_fixed_tagged_i64_abi() {
        use wasm_encoder::ValType;

        assert_eq!(
            HostIntrinsicImport::StringEachGraphemeCount.params(),
            &[ValType::I64]
        );
        assert_eq!(
            HostIntrinsicImport::StringEachGraphemeState.params(),
            &[ValType::I64, ValType::I64]
        );
        assert_eq!(
            HostIntrinsicImport::StringEachGraphemeCount.results(),
            &[ValType::I64]
        );
        assert_eq!(HostIntrinsicImport::MODULE, "goby:runtime/track-e");
    }
}
