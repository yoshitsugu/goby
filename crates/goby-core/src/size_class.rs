//! Perceus size-class enumeration (shared IR-level).
//!
//! Lives in `goby-core` so that the shared IR (in particular
//! `CompExpr::AllocReuse`) and both backends can agree on a single
//! classification. Backends are free to layer their own per-class
//! slot/bucket arithmetic on top — see
//! `goby-wasm/src/size_class.rs` for the Wasm free-list slot mapping.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SizeClass {
    /// List chunks — one fixed size.
    Chunk,
    /// List header with `n_chunks` chunk pointers, n_chunks ∈ {1,2,4,8,16,32,64,128}.
    Header(u32),
    /// Tuple of arity `a`, a ∈ {1..=8}.
    Tuple(u32),
    /// Record of arity `a`, a ∈ {1..=8}.
    Record(u32),
    /// Closure environment with `s` captured slots, s ∈ {0..=8}.
    Closure(u32),
    /// Mutable cell (one boxed slot).
    Cell,
    /// Boxed string rounded up to one of the buckets ∈ {8,16,32,64,128,256,512}.
    String(u32),
    /// Overflow: does not map to a free-list slot; bump-allocated and not recycled.
    Large,
}

/// Legal bucket values for `SizeClass::String`, covering the 8..=512 byte
/// buckets. Kept in sync with the backend's string bucketing (see
/// `goby-wasm/src/size_class.rs`).
pub const STRING_BUCKETS: [u32; 7] = [8, 16, 32, 64, 128, 256, 512];

/// Legal `n_chunks` values for `SizeClass::Header`.
pub const HEADER_CHUNK_COUNTS: [u32; 8] = [1, 2, 4, 8, 16, 32, 64, 128];

impl SizeClass {
    /// List header size class with `n_chunks` chunk pointers, falling back to
    /// `Large` when `n_chunks` is outside the small-class buckets.
    pub fn for_list_header(n_chunks: u32) -> SizeClass {
        if HEADER_CHUNK_COUNTS.contains(&n_chunks) {
            SizeClass::Header(n_chunks)
        } else {
            SizeClass::Large
        }
    }

    pub fn for_tuple(arity: u32) -> SizeClass {
        if (1..=8).contains(&arity) {
            SizeClass::Tuple(arity)
        } else {
            SizeClass::Large
        }
    }

    pub fn for_record(arity: u32) -> SizeClass {
        if (1..=8).contains(&arity) {
            SizeClass::Record(arity)
        } else {
            SizeClass::Large
        }
    }

    pub fn for_closure(slots: u32) -> SizeClass {
        if slots <= 8 {
            SizeClass::Closure(slots)
        } else {
            SizeClass::Large
        }
    }

    /// Returns true when this value is one of the legal reusable classes.
    /// Used as a validator when a `SizeClass` is constructed directly (for
    /// instance inside `CompExpr::AllocReuse` emitted by later passes) rather
    /// than via `for_*` helpers.
    pub fn is_valid(self) -> bool {
        match self {
            SizeClass::Chunk | SizeClass::Cell | SizeClass::Large => true,
            SizeClass::Header(n) => HEADER_CHUNK_COUNTS.contains(&n),
            SizeClass::Tuple(a) | SizeClass::Record(a) => (1..=8).contains(&a),
            SizeClass::Closure(s) => s <= 8,
            SizeClass::String(b) => STRING_BUCKETS.contains(&b),
        }
    }

    /// Returns true when this class is safe to use as the static key that
    /// pairs a `DropReuse` with an `AllocReuse`. `Large` is excluded because
    /// it does not carry an exact byte size, so two `Large` allocations are
    /// not guaranteed to match in size.
    pub fn is_reusable(self) -> bool {
        self.is_valid() && !matches!(self, SizeClass::Large)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_classification_rounds_large() {
        assert_eq!(SizeClass::for_list_header(1), SizeClass::Header(1));
        assert_eq!(SizeClass::for_list_header(128), SizeClass::Header(128));
        assert_eq!(SizeClass::for_list_header(3), SizeClass::Large);
        assert_eq!(SizeClass::for_list_header(256), SizeClass::Large);
    }

    #[test]
    fn tuple_classification_rounds_large() {
        assert_eq!(SizeClass::for_tuple(1), SizeClass::Tuple(1));
        assert_eq!(SizeClass::for_tuple(8), SizeClass::Tuple(8));
        assert_eq!(SizeClass::for_tuple(9), SizeClass::Large);
        assert_eq!(SizeClass::for_tuple(0), SizeClass::Large);
    }

    #[test]
    fn record_classification_rounds_large() {
        assert_eq!(SizeClass::for_record(1), SizeClass::Record(1));
        assert_eq!(SizeClass::for_record(8), SizeClass::Record(8));
        assert_eq!(SizeClass::for_record(9), SizeClass::Large);
    }

    #[test]
    fn closure_classification_rounds_large() {
        assert_eq!(SizeClass::for_closure(0), SizeClass::Closure(0));
        assert_eq!(SizeClass::for_closure(8), SizeClass::Closure(8));
        assert_eq!(SizeClass::for_closure(9), SizeClass::Large);
    }

    #[test]
    fn zero_arity_variants_are_always_valid() {
        assert!(SizeClass::Chunk.is_valid());
        assert!(SizeClass::Cell.is_valid());
        assert!(SizeClass::Large.is_valid());
    }

    #[test]
    fn header_validity_enumerates_buckets() {
        for &n in &HEADER_CHUNK_COUNTS {
            assert!(
                SizeClass::Header(n).is_valid(),
                "Header({n}) should be valid"
            );
        }
        assert!(!SizeClass::Header(0).is_valid());
        assert!(!SizeClass::Header(3).is_valid());
        assert!(!SizeClass::Header(129).is_valid());
    }

    #[test]
    fn tuple_record_validity_requires_1_through_8() {
        for a in 1..=8 {
            assert!(SizeClass::Tuple(a).is_valid());
            assert!(SizeClass::Record(a).is_valid());
        }
        assert!(!SizeClass::Tuple(0).is_valid());
        assert!(!SizeClass::Tuple(9).is_valid());
        assert!(!SizeClass::Record(0).is_valid());
        assert!(!SizeClass::Record(9).is_valid());
    }

    #[test]
    fn closure_validity_allows_0_through_8() {
        for s in 0..=8 {
            assert!(SizeClass::Closure(s).is_valid());
        }
        assert!(!SizeClass::Closure(9).is_valid());
    }

    #[test]
    fn string_validity_enumerates_buckets() {
        for &b in &STRING_BUCKETS {
            assert!(
                SizeClass::String(b).is_valid(),
                "String({b}) should be valid"
            );
        }
        assert!(!SizeClass::String(0).is_valid());
        assert!(!SizeClass::String(24).is_valid());
        assert!(!SizeClass::String(1024).is_valid());
    }

    #[test]
    fn large_is_valid_but_not_reusable() {
        assert!(SizeClass::Large.is_valid());
        assert!(!SizeClass::Large.is_reusable());
    }

    #[test]
    fn small_classes_are_reusable() {
        assert!(SizeClass::Chunk.is_reusable());
        assert!(SizeClass::Cell.is_reusable());
        assert!(SizeClass::Header(1).is_reusable());
        assert!(SizeClass::Tuple(3).is_reusable());
        assert!(SizeClass::Record(2).is_reusable());
        assert!(SizeClass::Closure(0).is_reusable());
        assert!(SizeClass::String(64).is_reusable());
    }

    #[test]
    fn invalid_classes_are_not_reusable() {
        assert!(!SizeClass::Header(3).is_reusable());
        assert!(!SizeClass::Tuple(9).is_reusable());
        assert!(!SizeClass::String(24).is_reusable());
    }
}
