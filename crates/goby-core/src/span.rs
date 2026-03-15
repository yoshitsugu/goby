//! Position conversion helpers for `Span`.
//!
//! All functions assume LF-only line endings (no CRLF handling in MVP).
//! Line numbers and column offsets are 1-indexed.
//! Column values represent byte offsets within the line, not character counts.

/// Convert a 1-indexed (line, col) pair to a byte offset from the start of `source`.
///
/// Returns `None` if `line` or `col` is 0, or if the position is out of range.
pub fn line_col_to_offset(source: &str, line: usize, col: usize) -> Option<usize> {
    if line == 0 || col == 0 {
        return None;
    }
    let target_line = line - 1; // convert to 0-indexed
    let mut current_line_start = 0;
    for (i, src_line) in source.split('\n').enumerate() {
        if i == target_line {
            let col_offset = col - 1; // convert to 0-indexed byte offset within line
            if col_offset > src_line.len() {
                return None;
            }
            return Some(current_line_start + col_offset);
        }
        current_line_start += src_line.len() + 1; // +1 for '\n'
    }
    None
}

/// Convert a byte offset from the start of `source` to a 1-indexed (line, col) pair.
///
/// Returns `Some` for `offset == source.len()` (past-the-end position), which is
/// useful for end-of-span markers pointing just after the last character.
/// Returns `None` if `offset` is strictly beyond `source.len()`.
pub fn offset_to_line_col(source: &str, offset: usize) -> Option<(usize, usize)> {
    if offset > source.len() {
        return None;
    }
    let mut current_line_start = 0;
    for (i, line) in source.split('\n').enumerate() {
        let line_end = current_line_start + line.len(); // exclusive, before '\n'
        if offset <= line_end {
            let col = offset - current_line_start + 1; // convert to 1-indexed
            return Some((i + 1, col));
        }
        current_line_start = line_end + 1; // skip '\n'
    }
    // offset == source.len() when source ends without trailing '\n' is handled above
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- line_col_to_offset ---

    #[test]
    fn ascii_single_line() {
        let src = "hello";
        assert_eq!(line_col_to_offset(src, 1, 1), Some(0));
        assert_eq!(line_col_to_offset(src, 1, 3), Some(2));
        assert_eq!(line_col_to_offset(src, 1, 6), Some(5)); // one past last char (end position)
    }

    #[test]
    fn ascii_multi_line() {
        let src = "abc\ndef\nghi";
        assert_eq!(line_col_to_offset(src, 1, 1), Some(0)); // 'a'
        assert_eq!(line_col_to_offset(src, 2, 1), Some(4)); // 'd'
        assert_eq!(line_col_to_offset(src, 3, 1), Some(8)); // 'g'
        assert_eq!(line_col_to_offset(src, 3, 3), Some(10)); // 'i'
    }

    #[test]
    fn two_byte_codepoint() {
        // 'é' is U+00E9, encoded as 0xC3 0xA9 (2 bytes)
        let src = "aé";
        assert_eq!(line_col_to_offset(src, 1, 1), Some(0)); // 'a'
        assert_eq!(line_col_to_offset(src, 1, 2), Some(1)); // first byte of 'é'
        assert_eq!(line_col_to_offset(src, 1, 4), Some(3)); // one past end
    }

    #[test]
    fn three_byte_codepoint() {
        // 'あ' is U+3042, encoded as 0xE3 0x81 0x82 (3 bytes)
        let src = "aあb";
        assert_eq!(line_col_to_offset(src, 1, 1), Some(0)); // 'a'
        assert_eq!(line_col_to_offset(src, 1, 2), Some(1)); // first byte of 'あ'
        assert_eq!(line_col_to_offset(src, 1, 5), Some(4)); // 'b'
    }

    #[test]
    fn out_of_range_line_zero() {
        assert_eq!(line_col_to_offset("hello", 0, 1), None);
    }

    #[test]
    fn out_of_range_col_zero() {
        assert_eq!(line_col_to_offset("hello", 1, 0), None);
    }

    #[test]
    fn out_of_range_line_too_large() {
        assert_eq!(line_col_to_offset("hello", 2, 1), None);
    }

    #[test]
    fn out_of_range_col_too_large() {
        assert_eq!(line_col_to_offset("hello", 1, 7), None);
    }

    #[test]
    fn trailing_newline_forward() {
        let src = "hello\n";
        assert_eq!(line_col_to_offset(src, 1, 1), Some(0)); // 'h'
        assert_eq!(line_col_to_offset(src, 2, 1), Some(6)); // start of empty second line
    }

    #[test]
    fn empty_source() {
        assert_eq!(line_col_to_offset("", 1, 1), Some(0)); // offset 0, end of empty line
        assert_eq!(line_col_to_offset("", 1, 2), None);
    }

    // --- offset_to_line_col ---

    #[test]
    fn offset_ascii_single_line() {
        let src = "hello";
        assert_eq!(offset_to_line_col(src, 0), Some((1, 1)));
        assert_eq!(offset_to_line_col(src, 4), Some((1, 5)));
        assert_eq!(offset_to_line_col(src, 5), Some((1, 6))); // end position
    }

    #[test]
    fn offset_ascii_multi_line() {
        let src = "abc\ndef\nghi";
        assert_eq!(offset_to_line_col(src, 0), Some((1, 1))); // 'a'
        assert_eq!(offset_to_line_col(src, 3), Some((1, 4))); // '\n' at end of line 1
        assert_eq!(offset_to_line_col(src, 4), Some((2, 1))); // 'd'
        assert_eq!(offset_to_line_col(src, 8), Some((3, 1))); // 'g'
    }

    #[test]
    fn offset_two_byte_codepoint() {
        let src = "aé";
        assert_eq!(offset_to_line_col(src, 0), Some((1, 1))); // 'a'
        assert_eq!(offset_to_line_col(src, 1), Some((1, 2))); // first byte of 'é'
        assert_eq!(offset_to_line_col(src, 3), Some((1, 4))); // end
    }

    #[test]
    fn offset_three_byte_codepoint() {
        let src = "aあb";
        assert_eq!(offset_to_line_col(src, 0), Some((1, 1))); // 'a'
        assert_eq!(offset_to_line_col(src, 1), Some((1, 2))); // first byte of 'あ'
        assert_eq!(offset_to_line_col(src, 4), Some((1, 5))); // 'b'
    }

    #[test]
    fn offset_beyond_end() {
        assert_eq!(offset_to_line_col("hello", 6), None);
    }

    #[test]
    fn offset_empty_source() {
        assert_eq!(offset_to_line_col("", 0), Some((1, 1))); // end of empty source
        assert_eq!(offset_to_line_col("", 1), None);
    }

    #[test]
    fn trailing_newline_boundary() {
        let src = "hello\n";
        // offset 5 is the '\n'
        assert_eq!(offset_to_line_col(src, 5), Some((1, 6)));
        // offset 6 is start of the empty second line
        assert_eq!(offset_to_line_col(src, 6), Some((2, 1)));
    }

    // --- round-trip ---

    #[test]
    fn round_trip_ascii() {
        let src = "abc\ndef\nghi";
        for line in 1..=3 {
            for col in 1..=4 {
                if let Some(offset) = line_col_to_offset(src, line, col) {
                    assert_eq!(
                        offset_to_line_col(src, offset),
                        Some((line, col)),
                        "round-trip failed for line={line}, col={col}, offset={offset}"
                    );
                }
            }
        }
    }

    #[test]
    fn round_trip_multibyte() {
        let src = "aéあ\nb";
        // Valid positions: line 1 cols 1,2,4,7; line 2 col 1,2
        let valid = [(1, 1), (1, 2), (1, 4), (1, 7), (2, 1), (2, 2)];
        for &(line, col) in &valid {
            if let Some(offset) = line_col_to_offset(src, line, col) {
                assert_eq!(
                    offset_to_line_col(src, offset),
                    Some((line, col)),
                    "round-trip failed for line={line}, col={col}"
                );
            }
        }
    }
}
