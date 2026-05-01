//! Size-class enumeration and free-list head address table for Perceus M3.
//!
//! Each `SizeClass` variant maps to exactly one 8-byte slot in the free-list
//! head table that starts at `FREE_LIST_TABLE_BASE` in linear memory.
//! Slot layout is documented in `layout.rs`.

use crate::gen_lower::emit::{chunk_alloc_size_pw, header_alloc_size_pw};
use crate::gen_lower::ptr::PtrWidth;
use crate::layout::{
    FREE_LIST_SLOT_CELL, FREE_LIST_SLOT_CHUNK, FREE_LIST_SLOT_CLOSURE_BASE,
    FREE_LIST_SLOT_HEADER_BASE, FREE_LIST_SLOT_RECORD_BASE, FREE_LIST_SLOT_STRING_BASE,
    FREE_LIST_SLOT_TUPLE_BASE,
};

/// Stable Perceus size classes shared with the core IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SizeClass {
    /// List chunks — one fixed size: `CHUNK_SIZE * 8 + meta`.
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

/// Header-arity index: maps n_chunks to slot index (0-based within header group).
fn header_slot_index(n_chunks: u32) -> Option<u32> {
    match n_chunks {
        1 => Some(0),
        2 => Some(1),
        4 => Some(2),
        8 => Some(3),
        16 => Some(4),
        32 => Some(5),
        64 => Some(6),
        128 => Some(7),
        _ => None,
    }
}

/// String bucket index: maps byte size to slot index (0-based within string group).
fn string_slot_index(bytes: u32) -> Option<u32> {
    match bytes {
        b if b <= 8 => Some(0),
        b if b <= 16 => Some(1),
        b if b <= 32 => Some(2),
        b if b <= 64 => Some(3),
        b if b <= 128 => Some(4),
        b if b <= 256 => Some(5),
        b if b <= 512 => Some(6),
        _ => None, // large
    }
}

impl SizeClass {
    /// Returns the linear-memory offset of the free-list head slot for this class,
    /// or `None` for `SizeClass::Large` (no free-list slot).
    pub(crate) fn free_list_head_offset(self) -> Option<u32> {
        match self {
            SizeClass::Chunk => Some(FREE_LIST_SLOT_CHUNK),
            SizeClass::Header(n) => {
                header_slot_index(n).map(|i| FREE_LIST_SLOT_HEADER_BASE + i * 8)
            }
            SizeClass::Tuple(a) if (1..=8).contains(&a) => {
                Some(FREE_LIST_SLOT_TUPLE_BASE + (a - 1) * 8)
            }
            SizeClass::Record(a) if (1..=8).contains(&a) => {
                Some(FREE_LIST_SLOT_RECORD_BASE + (a - 1) * 8)
            }
            SizeClass::Closure(s) if s <= 8 => Some(FREE_LIST_SLOT_CLOSURE_BASE + s * 8),
            SizeClass::Cell => Some(FREE_LIST_SLOT_CELL),
            SizeClass::String(b) => {
                string_slot_index(b).map(|i| FREE_LIST_SLOT_STRING_BASE + i * 8)
            }
            SizeClass::Large
            | SizeClass::Tuple(_)
            | SizeClass::Record(_)
            | SizeClass::Closure(_) => None,
        }
    }

    /// Payload byte size (excluding 8-byte refcount header) for this size class.
    /// Returns `None` for `Large` (variable size).
    pub(crate) fn payload_bytes(self, pw: PtrWidth) -> Option<u32> {
        match self {
            SizeClass::Chunk => Some(chunk_alloc_size_pw(pw)),
            SizeClass::Header(n) => Some(header_alloc_size_pw(pw, n)),
            SizeClass::Tuple(a) => {
                // Tuple payload: meta_slot(arity word) + a × 8-byte tagged values.
                Some(8 + a * 8)
            }
            SizeClass::Record(a) => {
                // Record payload: ctor_tag word + a × 8-byte fields.
                // Arity is not stored at runtime; child-drop deferred.
                Some(8 + a * 8)
            }
            SizeClass::Closure(s) => {
                // Closure payload: func_handle word + s × 8-byte slots.
                // Slot count is not stored at runtime; child-drop deferred.
                Some(8 + s * 8)
            }
            SizeClass::Cell => Some(8),
            SizeClass::String(b) => Some(b),
            SizeClass::Large => None,
        }
    }
}

impl From<goby_core::size_class::SizeClass> for SizeClass {
    fn from(sc: goby_core::size_class::SizeClass) -> Self {
        match sc {
            goby_core::size_class::SizeClass::Chunk => SizeClass::Chunk,
            goby_core::size_class::SizeClass::Header(n) => SizeClass::Header(n),
            goby_core::size_class::SizeClass::Tuple(a) => SizeClass::Tuple(a),
            goby_core::size_class::SizeClass::Record(a) => SizeClass::Record(a),
            goby_core::size_class::SizeClass::Closure(s) => SizeClass::Closure(s),
            goby_core::size_class::SizeClass::Cell => SizeClass::Cell,
            goby_core::size_class::SizeClass::String(b) => SizeClass::String(b),
            goby_core::size_class::SizeClass::Large => SizeClass::Large,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::layout::{FREE_LIST_SLOT_COUNT, FREE_LIST_TABLE_BASE, HEAP_BASE};

    #[test]
    fn heap_base_follows_free_list_table() {
        assert_eq!(
            HEAP_BASE,
            FREE_LIST_TABLE_BASE + FREE_LIST_SLOT_COUNT * 8,
            "HEAP_BASE must immediately follow the free-list table"
        );
    }

    #[test]
    fn chunk_slot_offset() {
        assert_eq!(
            SizeClass::Chunk.free_list_head_offset(),
            Some(FREE_LIST_TABLE_BASE)
        );
    }

    #[test]
    fn header_slot_offsets() {
        let base = FREE_LIST_SLOT_HEADER_BASE;
        assert_eq!(SizeClass::Header(1).free_list_head_offset(), Some(base));
        assert_eq!(SizeClass::Header(2).free_list_head_offset(), Some(base + 8));
        assert_eq!(
            SizeClass::Header(128).free_list_head_offset(),
            Some(base + 7 * 8)
        );
        assert_eq!(SizeClass::Header(3).free_list_head_offset(), None); // not a power
    }

    #[test]
    fn tuple_slot_offsets() {
        let base = FREE_LIST_SLOT_TUPLE_BASE;
        assert_eq!(SizeClass::Tuple(1).free_list_head_offset(), Some(base));
        assert_eq!(
            SizeClass::Tuple(8).free_list_head_offset(),
            Some(base + 7 * 8)
        );
        assert_eq!(SizeClass::Tuple(9).free_list_head_offset(), None);
    }

    #[test]
    fn cell_slot_offset() {
        assert_eq!(
            SizeClass::Cell.free_list_head_offset(),
            Some(FREE_LIST_SLOT_CELL)
        );
    }

    #[test]
    fn large_has_no_slot() {
        assert_eq!(SizeClass::Large.free_list_head_offset(), None);
    }
}
