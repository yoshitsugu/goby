//! Runtime value representation for the general Wasm lowering path.
//!
//! All runtime values are encoded as tagged `i64` words:
//!
//! - Tag in bits 63–60 (high 4 bits).
//! - Payload in bits 59–0 (lower 60 bits).
//!
//! # Scalar immediates
//! - `Unit`   tag=0x0, payload=0
//! - `Int`    tag=0x1, payload=60-bit two's complement [-2^59, 2^59-1]
//! - `Bool`   tag=0x2, payload=0 (false) or 1 (true)
//!
//! # Pointer-bearing tags
//! - `String` tag=0x3, payload=u32 pointer to `(len: i32, bytes...)` in linear memory
//! - `List`   tag=0x4, payload=u32 pointer to `(len: i32, items: [i64]...)` in linear memory
//! - `Tuple`  tag=0x6, payload=u32 pointer to `(len: i32, items: [i64]...)` in linear memory
//! - `Record` tag=0x7, payload=u32 pointer to `(ctor_tag: i64, fields: [i64]...)` in linear memory

/// Type tag for `Unit` values.
pub(crate) const TAG_UNIT: u8 = 0x0;
/// Type tag for `Int` values.
pub(crate) const TAG_INT: u8 = 0x1;
/// Type tag for `Bool` values.
pub(crate) const TAG_BOOL: u8 = 0x2;
/// Type tag for `String` values (pointer-bearing).
pub(crate) const TAG_STRING: u8 = 0x3;
/// Type tag for `List` values (pointer-bearing).
pub(crate) const TAG_LIST: u8 = 0x4;
/// Type tag for function-handle values (funcref table slot index).
///
/// A `Func` value encodes a Wasm funcref table *slot index* (not a raw Wasm function index)
/// in the lower 32 bits.  The funcref table maps slots → aux-decl function indices.
pub(crate) const TAG_FUNC: u8 = 0x5;
/// Type tag for `Tuple` values (pointer-bearing).
pub(crate) const TAG_TUPLE: u8 = 0x6;
/// Type tag for `Record` values (pointer-bearing).
pub(crate) const TAG_RECORD: u8 = 0x7;
/// Type tag for closure record values (pointer-bearing).
///
/// A closure record is laid out as `(func_handle: i64, slots: [i64; N])` in linear memory.
/// The `func_handle` at offset 0 is a TAG_FUNC-encoded i64 (funcref table slot index).
/// Slots follow at offsets `8 + 8*i`.
pub(crate) const TAG_CLOSURE: u8 = 0x8;
/// Type tag for mutable cell values (pointer-bearing).
///
/// A mutable cell is an 8-byte heap allocation: `(value: i64)` at offset 0.
/// The cell holds the current value of a shared mutable binding.
pub(crate) const TAG_CELL: u8 = 0x9;

/// Bit mask for the lower 60 bits (payload region).
const PAYLOAD_MASK: i64 = (1i64 << 60) - 1;

/// Minimum representable `Int` value (60-bit two's complement).
pub(crate) const INT_MIN: i64 = -(1i64 << 59);
/// Maximum representable `Int` value (60-bit two's complement).
pub(crate) const INT_MAX: i64 = (1i64 << 59) - 1;

/// Error type for value encoding failures.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ValueError {
    /// Integer value is outside the 60-bit representable range.
    IntOutOfRange(i64),
}

impl std::fmt::Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueError::IntOutOfRange(n) => write!(
                f,
                "integer {n} is outside the representable range [{INT_MIN}, {INT_MAX}]"
            ),
        }
    }
}

// ---------------------------------------------------------------------------
// Encoding
// ---------------------------------------------------------------------------

/// Encode a `Unit` value.
#[inline]
pub(crate) fn encode_unit() -> i64 {
    (TAG_UNIT as i64) << 60
}

/// Encode an `Int` value.
///
/// # Errors
/// Returns [`ValueError::IntOutOfRange`] if `n` is outside `[INT_MIN, INT_MAX]`.
pub(crate) fn encode_int(n: i64) -> Result<i64, ValueError> {
    if !(INT_MIN..=INT_MAX).contains(&n) {
        return Err(ValueError::IntOutOfRange(n));
    }
    // Mask to 60 bits to avoid sign-bit bleed into the tag region.
    let payload = n & PAYLOAD_MASK;
    Ok((TAG_INT as i64) << 60 | payload)
}

/// Encode a `Bool` value.
#[inline]
pub(crate) fn encode_bool(b: bool) -> i64 {
    (TAG_BOOL as i64) << 60 | (b as i64)
}

/// Encode a `String` pointer value.
///
/// `ptr` is a u32 address into Wasm linear memory pointing to a
/// `(len: i32, bytes...)` layout.
#[inline]
pub(crate) fn encode_string_ptr(ptr: u32) -> i64 {
    (TAG_STRING as i64) << 60 | (ptr as i64)
}

/// Encode a `List` pointer value.
///
/// `ptr` is a u32 address into Wasm linear memory pointing to a
/// `(len: i32, items: [i64]...)` layout.
#[inline]
pub(crate) fn encode_list_ptr(ptr: u32) -> i64 {
    (TAG_LIST as i64) << 60 | (ptr as i64)
}

/// Encode a function-handle value.
///
/// `table_slot` is the zero-based index into the Wasm funcref table.
/// The funcref table contains aux-decl function indices in declaration order;
/// slot 0 = first aux decl, slot 1 = second aux decl, etc.
#[inline]
pub(crate) fn encode_func_handle(table_slot: u32) -> i64 {
    (TAG_FUNC as i64) << 60 | (table_slot as i64)
}

/// Encode a `Tuple` pointer value.
///
/// `ptr` is a u32 address into Wasm linear memory pointing to a
/// `(len: i32, items: [i64]...)` layout.
#[inline]
pub(crate) fn encode_tuple_ptr(ptr: u32) -> i64 {
    (TAG_TUPLE as i64) << 60 | (ptr as i64)
}

/// Encode a `Record` pointer value.
///
/// `ptr` is a u32 address into Wasm linear memory pointing to a
/// `(ctor_tag: i64, fields: [i64]...)` layout.
#[inline]
pub(crate) fn encode_record_ptr(ptr: u32) -> i64 {
    (TAG_RECORD as i64) << 60 | (ptr as i64)
}

/// Encode a closure record pointer value.
///
/// `ptr` is a u32 address into Wasm linear memory pointing to a
/// `(func_handle: i64, slots: [i64; N])` layout.
#[inline]
pub(crate) fn encode_closure_ptr(ptr: u32) -> i64 {
    (TAG_CLOSURE as i64) << 60 | (ptr as i64)
}

/// Encode a mutable cell pointer value.
///
/// `ptr` is a u32 address into Wasm linear memory pointing to a
/// `(value: i64)` layout (8 bytes).
#[inline]
pub(crate) fn encode_cell_ptr(ptr: u32) -> i64 {
    (TAG_CELL as i64) << 60 | (ptr as i64)
}

/// Extract the funcref table slot index from an encoded `Func` value.
///
/// Only valid when `decode_tag(v) == TAG_FUNC`.
#[inline]
pub(crate) fn decode_func_slot(v: i64) -> u32 {
    (v & 0xFFFF_FFFF) as u32
}

/// Extract the u32 pointer from an encoded closure record value.
///
/// Only valid when `decode_tag(v) == TAG_CLOSURE`.
#[inline]
pub(crate) fn decode_closure_ptr(v: i64) -> u32 {
    decode_payload_ptr(v)
}

/// Extract the u32 pointer from an encoded mutable cell value.
///
/// Only valid when `decode_tag(v) == TAG_CELL`.
#[inline]
pub(crate) fn decode_cell_ptr(v: i64) -> u32 {
    decode_payload_ptr(v)
}

// ---------------------------------------------------------------------------
// Decoding
// ---------------------------------------------------------------------------

/// Extract the type tag from an encoded value (bits 63–60).
#[inline]
pub(crate) fn decode_tag(v: i64) -> u8 {
    ((v as u64) >> 60) as u8
}

/// Extract the 60-bit integer payload, sign-extended to i64.
///
/// Only valid when `decode_tag(v) == TAG_INT`.
#[inline]
pub(crate) fn decode_payload_int(v: i64) -> i64 {
    // Shift left by 4 to move bit 59 into sign position, then arithmetic shift right.
    (v << 4) >> 4
}

/// Extract the u32 pointer payload (lower 32 bits).
///
/// Only valid when `decode_tag(v)` is `TAG_STRING`, `TAG_LIST`, `TAG_TUPLE`, or `TAG_RECORD`.
#[inline]
pub(crate) fn decode_payload_ptr(v: i64) -> u32 {
    (v & 0xFFFF_FFFF) as u32
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unit_round_trip() {
        let v = encode_unit();
        assert_eq!(decode_tag(v), TAG_UNIT);
    }

    #[test]
    fn bool_true_round_trip() {
        let v = encode_bool(true);
        assert_eq!(decode_tag(v), TAG_BOOL);
        assert_eq!(v & 1, 1);
    }

    #[test]
    fn bool_false_round_trip() {
        let v = encode_bool(false);
        assert_eq!(decode_tag(v), TAG_BOOL);
        assert_eq!(v & 1, 0);
    }

    #[test]
    fn int_zero_round_trip() {
        let v = encode_int(0).expect("0 should encode");
        assert_eq!(decode_tag(v), TAG_INT);
        assert_eq!(decode_payload_int(v), 0);
    }

    #[test]
    fn int_one_round_trip() {
        let v = encode_int(1).expect("1 should encode");
        assert_eq!(decode_tag(v), TAG_INT);
        assert_eq!(decode_payload_int(v), 1);
    }

    #[test]
    fn int_negative_one_round_trip() {
        let v = encode_int(-1).expect("-1 should encode");
        // Tag must be TAG_INT even for negative numbers (sign bits must not bleed into tag).
        assert_eq!(decode_tag(v), TAG_INT, "tag must be TAG_INT for -1");
        assert_eq!(decode_payload_int(v), -1);
    }

    #[test]
    fn int_max_round_trip() {
        let v = encode_int(INT_MAX).expect("INT_MAX should encode");
        assert_eq!(decode_tag(v), TAG_INT);
        assert_eq!(decode_payload_int(v), INT_MAX);
    }

    #[test]
    fn int_min_round_trip() {
        let v = encode_int(INT_MIN).expect("INT_MIN should encode");
        assert_eq!(decode_tag(v), TAG_INT);
        assert_eq!(decode_payload_int(v), INT_MIN);
    }

    #[test]
    fn int_out_of_range_above() {
        assert_eq!(
            encode_int(INT_MAX + 1),
            Err(ValueError::IntOutOfRange(INT_MAX + 1))
        );
    }

    #[test]
    fn int_out_of_range_below() {
        assert_eq!(
            encode_int(INT_MIN - 1),
            Err(ValueError::IntOutOfRange(INT_MIN - 1))
        );
    }

    #[test]
    fn string_ptr_zero_round_trip() {
        let v = encode_string_ptr(0);
        assert_eq!(decode_tag(v), TAG_STRING);
        assert_eq!(decode_payload_ptr(v), 0);
    }

    #[test]
    fn string_ptr_max_round_trip() {
        let v = encode_string_ptr(u32::MAX);
        assert_eq!(decode_tag(v), TAG_STRING);
        assert_eq!(decode_payload_ptr(v), u32::MAX);
    }

    #[test]
    fn list_ptr_heap_base_round_trip() {
        // HEAP_BASE = 16 per layout.rs
        let v = encode_list_ptr(16);
        assert_eq!(decode_tag(v), TAG_LIST);
        assert_eq!(decode_payload_ptr(v), 16);
    }

    #[test]
    fn list_ptr_max_round_trip() {
        let v = encode_list_ptr(u32::MAX);
        assert_eq!(decode_tag(v), TAG_LIST);
        assert_eq!(decode_payload_ptr(v), u32::MAX);
    }

    #[test]
    fn func_handle_zero_round_trip() {
        let v = encode_func_handle(0);
        assert_eq!(decode_tag(v), TAG_FUNC);
        assert_eq!(decode_func_slot(v), 0);
    }

    #[test]
    fn func_handle_nonzero_round_trip() {
        let v = encode_func_handle(7);
        assert_eq!(decode_tag(v), TAG_FUNC);
        assert_eq!(decode_func_slot(v), 7);
    }

    #[test]
    fn func_handle_max_u32_round_trip() {
        let v = encode_func_handle(u32::MAX);
        assert_eq!(decode_tag(v), TAG_FUNC);
        assert_eq!(decode_func_slot(v), u32::MAX);
    }

    #[test]
    fn tuple_ptr_heap_base_round_trip() {
        let v = encode_tuple_ptr(16);
        assert_eq!(decode_tag(v), TAG_TUPLE);
        assert_eq!(decode_payload_ptr(v), 16);
    }

    #[test]
    fn tuple_ptr_max_round_trip() {
        let v = encode_tuple_ptr(u32::MAX);
        assert_eq!(decode_tag(v), TAG_TUPLE);
        assert_eq!(decode_payload_ptr(v), u32::MAX);
    }

    #[test]
    fn record_ptr_heap_base_round_trip() {
        let v = encode_record_ptr(16);
        assert_eq!(decode_tag(v), TAG_RECORD);
        assert_eq!(decode_payload_ptr(v), 16);
    }

    #[test]
    fn record_ptr_max_round_trip() {
        let v = encode_record_ptr(u32::MAX);
        assert_eq!(decode_tag(v), TAG_RECORD);
        assert_eq!(decode_payload_ptr(v), u32::MAX);
    }

    #[test]
    fn tag_orthogonality() {
        let unit_tag = decode_tag(encode_unit());
        let int_tag = decode_tag(encode_int(0).unwrap());
        let bool_tag = decode_tag(encode_bool(false));
        let str_tag = decode_tag(encode_string_ptr(0));
        let list_tag = decode_tag(encode_list_ptr(0));
        let func_tag = decode_tag(encode_func_handle(0));
        let tuple_tag = decode_tag(encode_tuple_ptr(0));
        let record_tag = decode_tag(encode_record_ptr(0));
        let closure_tag = decode_tag(encode_closure_ptr(0));
        let cell_tag = decode_tag(encode_cell_ptr(0));
        // All runtime tags must be distinct.
        let tags = [
            unit_tag, int_tag, bool_tag, str_tag, list_tag, func_tag, tuple_tag, record_tag,
            closure_tag, cell_tag,
        ];
        for i in 0..tags.len() {
            for j in (i + 1)..tags.len() {
                assert_ne!(tags[i], tags[j], "tags[{i}] == tags[{j}]: not orthogonal");
            }
        }
    }

    #[test]
    fn closure_ptr_round_trip() {
        let v = encode_closure_ptr(16);
        assert_eq!(decode_tag(v), TAG_CLOSURE);
        assert_eq!(decode_closure_ptr(v), 16);
    }

    #[test]
    fn closure_ptr_max_round_trip() {
        let v = encode_closure_ptr(u32::MAX);
        assert_eq!(decode_tag(v), TAG_CLOSURE);
        assert_eq!(decode_closure_ptr(v), u32::MAX);
    }

    #[test]
    fn cell_ptr_round_trip() {
        let v = encode_cell_ptr(24);
        assert_eq!(decode_tag(v), TAG_CELL);
        assert_eq!(decode_cell_ptr(v), 24);
    }

    #[test]
    fn cell_ptr_max_round_trip() {
        let v = encode_cell_ptr(u32::MAX);
        assert_eq!(decode_tag(v), TAG_CELL);
        assert_eq!(decode_cell_ptr(v), u32::MAX);
    }
}
