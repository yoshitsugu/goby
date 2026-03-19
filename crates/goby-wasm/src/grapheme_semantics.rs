use unicode_segmentation::UnicodeSegmentation;

/// Single semantic authority for Unicode Extended Grapheme Cluster iteration in
/// the Wasm backend/runtime layer.
///
/// Track E requires backend and runtime paths to share one segmentation
/// definition instead of open-coding separate interpretations.
pub(crate) fn collect_extended_graphemes(value: &str) -> Vec<String> {
    value.graphemes(true).map(|part| part.to_string()).collect()
}

pub(crate) fn for_each_extended_grapheme(
    value: &str,
    mut f: impl FnMut(&str) -> ControlFlow,
) -> ControlFlow {
    for grapheme in value.graphemes(true) {
        if matches!(f(grapheme), ControlFlow::Break) {
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
}
