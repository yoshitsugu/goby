use crate::gen_lower::backend_ir::BackendIntrinsic;
use crate::memory_config::DEFAULT_WASM_MEMORY_CONFIG;

/// Number of bytes reserved at the top of linear memory for host-owned
/// temporary string/list allocations during Goby-managed Wasm execution.
pub(crate) const HOST_BUMP_RESERVED_BYTES: u32 =
    DEFAULT_WASM_MEMORY_CONFIG.host_bump_reserved_bytes;

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
    ValueToString,
    StringEachGraphemeCount,
    StringEachGraphemeState,
    StringConcat,
    ListJoinString,
    StringGraphemesList,
    StringSplitLines,
}

impl HostIntrinsicImport {
    pub(crate) const MODULE: &'static str = "goby:runtime/track-e";

    pub(crate) const fn module(self) -> &'static str {
        let _ = self;
        Self::MODULE
    }

    pub(crate) const fn name(self) -> &'static str {
        match self {
            Self::ValueToString => "__goby_value_to_string",
            Self::StringEachGraphemeCount => "__goby_string_each_grapheme_count",
            Self::StringEachGraphemeState => "__goby_string_each_grapheme_state",
            Self::StringConcat => "__goby_string_concat",
            Self::ListJoinString => "__goby_list_join_string",
            Self::StringGraphemesList => "__goby_string_graphemes_list",
            Self::StringSplitLines => "__goby_string_split_lines",
        }
    }

    pub(crate) const fn params(self) -> &'static [wasm_encoder::ValType] {
        use wasm_encoder::ValType;
        match self {
            Self::ValueToString => &[ValType::I64],
            Self::StringEachGraphemeCount => &[ValType::I64],
            Self::StringEachGraphemeState => &[ValType::I64, ValType::I64],
            Self::StringConcat | Self::ListJoinString => &[ValType::I64, ValType::I64],
            Self::StringGraphemesList | Self::StringSplitLines => &[ValType::I64],
        }
    }

    pub(crate) const fn results(self) -> &'static [wasm_encoder::ValType] {
        use wasm_encoder::ValType;
        &[ValType::I64]
    }
}

pub(crate) const HOST_INTRINSIC_IMPORTS: [HostIntrinsicImport; 7] = [
    HostIntrinsicImport::ValueToString,
    HostIntrinsicImport::StringEachGraphemeCount,
    HostIntrinsicImport::StringEachGraphemeState,
    HostIntrinsicImport::StringConcat,
    HostIntrinsicImport::ListJoinString,
    HostIntrinsicImport::StringGraphemesList,
    HostIntrinsicImport::StringSplitLines,
];

pub(crate) const fn host_import_for_intrinsic(
    intrinsic: BackendIntrinsic,
) -> Option<HostIntrinsicImport> {
    match intrinsic {
        BackendIntrinsic::ValueToString => Some(HostIntrinsicImport::ValueToString),
        BackendIntrinsic::StringEachGraphemeCount => {
            Some(HostIntrinsicImport::StringEachGraphemeCount)
        }
        BackendIntrinsic::StringEachGraphemeState => {
            Some(HostIntrinsicImport::StringEachGraphemeState)
        }
        BackendIntrinsic::StringConcat => Some(HostIntrinsicImport::StringConcat),
        BackendIntrinsic::ListJoinString => Some(HostIntrinsicImport::ListJoinString),
        BackendIntrinsic::StringGraphemesList => Some(HostIntrinsicImport::StringGraphemesList),
        BackendIntrinsic::StringSplitLines => Some(HostIntrinsicImport::StringSplitLines),
        BackendIntrinsic::StringSplit
        | BackendIntrinsic::ListGet
        | BackendIntrinsic::StringLength
        | BackendIntrinsic::ListPushString
        | BackendIntrinsic::ListSet
        | BackendIntrinsic::ListConcat
        | BackendIntrinsic::ListLength
        | BackendIntrinsic::ListFold
        | BackendIntrinsic::ListMap => None,
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
