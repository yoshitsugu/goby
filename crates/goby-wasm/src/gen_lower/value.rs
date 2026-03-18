//! Runtime value representation for the general Wasm lowering path.
//!
//! All runtime values are encoded as tagged `i64` words per
//! `doc/wasm_runtime_architecture.md §2`:
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
    if n < INT_MIN || n > INT_MAX {
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
/// Only valid when `decode_tag(v)` is `TAG_STRING` or `TAG_LIST`.
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
        assert_eq!(encode_int(INT_MAX + 1), Err(ValueError::IntOutOfRange(INT_MAX + 1)));
    }

    #[test]
    fn int_out_of_range_below() {
        assert_eq!(encode_int(INT_MIN - 1), Err(ValueError::IntOutOfRange(INT_MIN - 1)));
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
    fn tag_orthogonality() {
        let unit_tag = decode_tag(encode_unit());
        let int_tag = decode_tag(encode_int(0).unwrap());
        let bool_tag = decode_tag(encode_bool(false));
        let str_tag = decode_tag(encode_string_ptr(0));
        let list_tag = decode_tag(encode_list_ptr(0));
        // All five tags must be distinct.
        let tags = [unit_tag, int_tag, bool_tag, str_tag, list_tag];
        for i in 0..tags.len() {
            for j in (i + 1)..tags.len() {
                assert_ne!(tags[i], tags[j], "tags[{i}] == tags[{j}]: not orthogonal");
            }
        }
    }
}
