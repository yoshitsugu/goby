use crate::ast::{CasePattern, ListPatternItem, ListPatternTail};
use crate::parser_util::is_non_reserved_identifier;
use crate::str_util::split_top_level_commas;

pub(crate) fn parse_case_pattern(src: &str) -> Option<CasePattern> {
    let src = src.trim();
    if src == "_" {
        return Some(CasePattern::Wildcard);
    }
    if src == "[]" {
        return Some(CasePattern::EmptyList);
    }
    if let Some(inner) = src.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
        let parts = split_top_level_commas(inner.trim());
        if parts.is_empty() {
            return None;
        }

        let mut items: Vec<ListPatternItem> = Vec::new();
        let mut tail: Option<ListPatternTail> = None;
        let mut binders = std::collections::HashSet::new();

        for (idx, raw_part) in parts.iter().enumerate() {
            let part = raw_part.trim();
            if let Some(rest_src) = part.strip_prefix("..") {
                if idx != parts.len() - 1 {
                    return None;
                }
                let rest = rest_src.trim();
                tail = if rest.is_empty() {
                    Some(ListPatternTail::Ignore)
                } else {
                    if !is_non_reserved_identifier(rest) {
                        return None;
                    }
                    if rest != "_" && !binders.insert(rest.to_string()) {
                        return None;
                    }
                    Some(ListPatternTail::Bind(rest.to_string()))
                };
                continue;
            }

            let item = parse_list_pattern_item(part)?;
            if let ListPatternItem::Bind(name) = &item
                && name != "_"
                && !binders.insert(name.clone())
            {
                return None;
            }
            items.push(item);
        }

        if items.is_empty() {
            return None;
        }

        return Some(CasePattern::ListPattern { items, tail });
    }
    if src == "True" {
        return Some(CasePattern::BoolLit(true));
    }
    if src == "False" {
        return Some(CasePattern::BoolLit(false));
    }
    if let Ok(n) = src.parse::<i64>() {
        return Some(CasePattern::IntLit(n));
    }
    if src.starts_with('"') && src.ends_with('"') && src.len() >= 2 {
        let inner = &src[1..src.len() - 1];
        if !inner.contains('"') {
            return Some(CasePattern::StringLit(inner.to_string()));
        }
    }
    None
}

fn parse_list_pattern_item(src: &str) -> Option<ListPatternItem> {
    if src == "_" {
        return Some(ListPatternItem::Wildcard);
    }
    if let Ok(n) = src.parse::<i64>() {
        return Some(ListPatternItem::IntLit(n));
    }
    if src.starts_with('"') && src.ends_with('"') && src.len() >= 2 {
        let inner = &src[1..src.len() - 1];
        if !inner.contains('"') {
            return Some(ListPatternItem::StringLit(inner.to_string()));
        }
    }
    if is_non_reserved_identifier(src) {
        return Some(ListPatternItem::Bind(src.to_string()));
    }
    None
}
