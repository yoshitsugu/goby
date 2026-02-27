#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub arguments: Vec<String>,
    pub result: String,
}

pub fn parse_function_type(annotation: &str) -> Option<FunctionType> {
    let base = strip_effect(annotation);
    let parts: Vec<String> = base
        .split("->")
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .map(ToString::to_string)
        .collect();

    if parts.len() < 2 {
        return None;
    }

    let result = parts.last()?.clone();
    let arguments = parts[..parts.len() - 1].to_vec();
    Some(FunctionType { arguments, result })
}

pub fn strip_effect(annotation: &str) -> &str {
    match annotation.find(" can ") {
        Some(idx) => annotation[..idx].trim_end(),
        None => annotation.trim(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_function_type_with_effect_annotation() {
        let ty = parse_function_type("Unit -> Unit can Print").expect("should parse");
        assert_eq!(ty.arguments, vec!["Unit"]);
        assert_eq!(ty.result, "Unit");
    }
}
