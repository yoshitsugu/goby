use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct GraphemeSpan {
    pub(crate) start: usize,
    pub(crate) end: usize,
}

/// Single semantic authority for Unicode Extended Grapheme Cluster iteration in
/// the Wasm backend/runtime layer.
///
/// The backend and runtime paths share one segmentation
/// definition instead of open-coding separate interpretations.
#[allow(dead_code)]
pub(crate) fn collect_extended_graphemes(value: &str) -> Vec<String> {
    collect_extended_grapheme_spans(value)
        .into_iter()
        .map(|span| value[span.start..span.end].to_string())
        .collect()
}

pub(crate) fn collect_extended_grapheme_spans(value: &str) -> Vec<GraphemeSpan> {
    let mut spans = Vec::new();
    let mut iter = value.grapheme_indices(true).peekable();
    while let Some((start, grapheme)) = iter.next() {
        let end = iter.peek().map(|(next, _)| *next).unwrap_or(value.len());
        debug_assert_eq!(&value[start..end], grapheme);
        spans.push(GraphemeSpan { start, end });
    }
    spans
}

#[allow(dead_code)]
pub(crate) fn for_each_extended_grapheme(
    value: &str,
    mut f: impl FnMut(&str) -> ControlFlow,
) -> ControlFlow {
    for span in collect_extended_grapheme_spans(value) {
        if matches!(f(&value[span.start..span.end]), ControlFlow::Break) {
            return ControlFlow::Break;
        }
    }
    ControlFlow::Continue
}

pub(crate) fn for_each_extended_grapheme_span(
    value: &str,
    mut f: impl FnMut(GraphemeSpan) -> ControlFlow,
) -> ControlFlow {
    for span in collect_extended_grapheme_spans(value) {
        if matches!(f(span), ControlFlow::Break) {
            return ControlFlow::Break;
        }
    }
    ControlFlow::Continue
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ControlFlow {
    Continue,
    Break,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn collects_extended_grapheme_clusters() {
        assert_eq!(collect_extended_graphemes("a👨‍👩‍👧‍👦b"), vec!["a", "👨‍👩‍👧‍👦", "b"]);
    }

    #[test]
    fn collects_extended_grapheme_spans() {
        assert_eq!(
            collect_extended_grapheme_spans("a👨‍👩‍👧‍👦b"),
            vec![
                GraphemeSpan { start: 0, end: 1 },
                GraphemeSpan {
                    start: 1,
                    end: "a👨‍👩‍👧‍👦".len()
                },
                GraphemeSpan {
                    start: "a👨‍👩‍👧‍👦".len(),
                    end: "a👨‍👩‍👧‍👦b".len()
                }
            ]
        );
    }

    #[test]
    fn for_each_can_stop_early_on_grapheme_boundaries() {
        let mut seen = Vec::new();
        let flow = for_each_extended_grapheme("a👨‍👩‍👧‍👦b", |grapheme| {
            seen.push(grapheme.to_string());
            if grapheme == "👨‍👩‍👧‍👦" {
                ControlFlow::Break
            } else {
                ControlFlow::Continue
            }
        });
        assert_eq!(flow, ControlFlow::Break);
        assert_eq!(seen, vec!["a", "👨‍👩‍👧‍👦"]);
    }

    #[test]
    fn for_each_span_can_stop_early_on_boundaries() {
        let value = "a👨‍👩‍👧‍👦b";
        let mut seen = Vec::new();
        let flow = for_each_extended_grapheme_span(value, |span| {
            seen.push(value[span.start..span.end].to_string());
            if &value[span.start..span.end] == "👨‍👩‍👧‍👦" {
                ControlFlow::Break
            } else {
                ControlFlow::Continue
            }
        });
        assert_eq!(flow, ControlFlow::Break);
        assert_eq!(seen, vec!["a", "👨‍👩‍👧‍👦"]);
    }
}
