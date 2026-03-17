use std::collections::{HashMap, HashSet};

use crate::InlineHandlerValue;

#[derive(Default, Clone)]
pub(crate) struct RuntimeLocals {
    string_values: HashMap<String, String>,
    int_values: HashMap<String, i64>,
    list_int_values: HashMap<String, Vec<i64>>,
    record_values: HashMap<String, RuntimeValue>,
}

impl RuntimeLocals {
    pub(crate) fn string_values(&self) -> &HashMap<String, String> {
        &self.string_values
    }

    pub(crate) fn int_values(&self) -> &HashMap<String, i64> {
        &self.int_values
    }

    pub(crate) fn list_int_values(&self) -> &HashMap<String, Vec<i64>> {
        &self.list_int_values
    }

    pub(crate) fn store(&mut self, name: &str, value: RuntimeValue) {
        self.clear(name);
        let key = name.to_string();
        match value {
            RuntimeValue::String(text) => {
                self.string_values.insert(key, text);
            }
            RuntimeValue::Int(number) => {
                self.int_values.insert(key, number);
            }
            RuntimeValue::ListInt(values) => {
                self.list_int_values.insert(key, values);
            }
            other => {
                self.record_values.insert(key, other);
            }
        }
    }

    pub(crate) fn clear(&mut self, name: &str) {
        self.string_values.remove(name);
        self.int_values.remove(name);
        self.list_int_values.remove(name);
        self.record_values.remove(name);
    }

    pub(crate) fn get(&self, name: &str) -> Option<RuntimeValue> {
        if let Some(v) = self.int_values.get(name) {
            return Some(RuntimeValue::Int(*v));
        }
        if let Some(v) = self.string_values.get(name) {
            return Some(RuntimeValue::String(v.clone()));
        }
        if let Some(v) = self.list_int_values.get(name) {
            return Some(RuntimeValue::ListInt(v.clone()));
        }
        if let Some(v) = self.record_values.get(name) {
            return Some(v.clone());
        }
        None
    }

    pub(crate) fn binding_names(&self) -> Vec<String> {
        let mut names: HashSet<String> = self.int_values.keys().cloned().collect();
        names.extend(self.string_values.keys().cloned());
        names.extend(self.list_int_values.keys().cloned());
        names.extend(self.record_values.keys().cloned());
        names.into_iter().collect()
    }

    pub(crate) fn apply_selected_from(&mut self, other: &Self, names: &HashSet<String>) {
        for name in names {
            match other.get(name) {
                Some(value) => self.store(name, value),
                None => self.clear(name),
            }
        }
    }
}

pub(crate) fn runtime_value_option_eq(
    left: Option<&RuntimeValue>,
    right: Option<&RuntimeValue>,
) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => runtime_value_eq(left, right),
        (None, None) => true,
        _ => false,
    }
}

pub(crate) fn runtime_value_eq(left: &RuntimeValue, right: &RuntimeValue) -> bool {
    match (left, right) {
        (RuntimeValue::String(a), RuntimeValue::String(b)) => a == b,
        (RuntimeValue::Int(a), RuntimeValue::Int(b)) => a == b,
        (RuntimeValue::Unit, RuntimeValue::Unit) => true,
        (RuntimeValue::Bool(a), RuntimeValue::Bool(b)) => a == b,
        (RuntimeValue::Tuple(a), RuntimeValue::Tuple(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(a, b)| runtime_value_eq(a, b))
        }
        (RuntimeValue::ListInt(a), RuntimeValue::ListInt(b)) => a == b,
        (RuntimeValue::ListString(a), RuntimeValue::ListString(b)) => a == b,
        (
            RuntimeValue::Record {
                constructor: a_ctor,
                fields: a_fields,
            },
            RuntimeValue::Record {
                constructor: b_ctor,
                fields: b_fields,
            },
        ) => {
            a_ctor == b_ctor
                && a_fields.len() == b_fields.len()
                && a_fields.iter().all(|(name, a_value)| {
                    b_fields
                        .get(name)
                        .is_some_and(|b_value| runtime_value_eq(a_value, b_value))
                })
        }
        (RuntimeValue::Handler(_), RuntimeValue::Handler(_)) => false,
        _ => false,
    }
}

#[derive(Clone)]
#[allow(clippy::large_enum_variant)]
pub(crate) enum RuntimeValue {
    String(String),
    Int(i64),
    Unit,
    Bool(bool),
    Tuple(Vec<RuntimeValue>),
    ListInt(Vec<i64>),
    ListString(Vec<String>),
    Handler(InlineHandlerValue),
    Record {
        constructor: String,
        fields: HashMap<String, RuntimeValue>,
    },
}

impl RuntimeValue {
    /// Format value for runtime output (e.g. `print`/`println`).
    /// Strings are emitted as-is; use `to_expression_text` for quoted form.
    pub(crate) fn to_output_text(&self) -> String {
        self.format_text(false)
    }

    /// Format value as a Goby expression literal (strings are double-quoted).
    pub(crate) fn to_expression_text(&self) -> String {
        self.format_text(true)
    }

    fn format_text(&self, quoted_strings: bool) -> String {
        match self {
            Self::String(text) => {
                if quoted_strings {
                    format!("\"{}\"", text)
                } else {
                    text.clone()
                }
            }
            Self::Int(value) => value.to_string(),
            Self::Unit => "Unit".to_string(),
            Self::Bool(b) => if *b { "True" } else { "False" }.to_string(),
            Self::Tuple(items) => {
                let parts: Vec<String> =
                    items.iter().map(|v| v.format_text(quoted_strings)).collect();
                format!("({})", parts.join(", "))
            }
            Self::ListInt(values) => format_list_int(values),
            Self::ListString(values) => {
                let parts: Vec<String> = values.iter().map(|s| format!("\"{}\"", s)).collect();
                format!("[{}]", parts.join(", "))
            }
            Self::Handler(_) => "<handler>".to_string(),
            Self::Record { constructor, .. } => constructor.clone(),
        }
    }
}

fn format_list_int(values: &[i64]) -> String {
    let joined = values
        .iter()
        .map(i64::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{}]", joined)
}

#[cfg(test)]
mod tests {
    use super::{RuntimeLocals, RuntimeValue, runtime_value_eq, runtime_value_option_eq};
    use std::collections::HashMap;

    #[test]
    fn runtime_locals_round_trip_int_and_string_bindings() {
        let mut locals = RuntimeLocals::default();
        locals.store("n", RuntimeValue::Int(42));
        locals.store("s", RuntimeValue::String("ok".to_string()));

        assert!(matches!(locals.get("n"), Some(RuntimeValue::Int(42))));
        assert!(matches!(
            locals.get("s"),
            Some(RuntimeValue::String(text)) if text == "ok"
        ));
        assert_eq!(locals.int_values().get("n"), Some(&42));
        assert_eq!(
            locals.string_values().get("s").map(String::as_str),
            Some("ok")
        );
    }

    #[test]
    fn runtime_value_record_equality_checks_nested_fields() {
        let left = RuntimeValue::Record {
            constructor: "Pair".to_string(),
            fields: HashMap::from([
                ("x".to_string(), RuntimeValue::Int(1)),
                ("y".to_string(), RuntimeValue::Bool(true)),
            ]),
        };
        let right = RuntimeValue::Record {
            constructor: "Pair".to_string(),
            fields: HashMap::from([
                ("x".to_string(), RuntimeValue::Int(1)),
                ("y".to_string(), RuntimeValue::Bool(true)),
            ]),
        };

        assert!(runtime_value_eq(&left, &right));
        assert!(runtime_value_option_eq(Some(&left), Some(&right)));
    }
}
