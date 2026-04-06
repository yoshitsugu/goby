use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::InlineHandlerValue;
use crate::runtime_eval::IntCallable;

#[derive(Default, Clone)]
pub(crate) struct RuntimeLocals {
    values: HashMap<String, RuntimeValue>,
    mut_values: HashMap<String, Rc<RefCell<RuntimeValue>>>,
}

impl RuntimeLocals {
    /// Return a filtered view of Int-typed bindings for the IntEvaluator.
    pub(crate) fn int_view(&self) -> HashMap<String, i64> {
        let mut result = HashMap::new();
        for (name, value) in &self.values {
            if let RuntimeValue::Int(number) = value {
                result.insert(name.clone(), *number);
            }
        }
        for (name, cell) in &self.mut_values {
            if let RuntimeValue::Int(number) = &*cell.borrow() {
                result.insert(name.clone(), *number);
            }
        }
        result
    }

    pub(crate) fn store(&mut self, name: &str, value: RuntimeValue) {
        self.clear(name);
        self.values.insert(name.to_string(), value);
    }

    pub(crate) fn store_mut(&mut self, name: &str, value: RuntimeValue) {
        self.clear(name);
        self.mut_values
            .insert(name.to_string(), Rc::new(RefCell::new(value)));
    }

    pub(crate) fn assign(&mut self, name: &str, value: RuntimeValue) -> bool {
        if let Some(cell) = self.mut_values.get(name) {
            *cell.borrow_mut() = value;
            true
        } else if self.contains(name) {
            self.store(name, value);
            true
        } else {
            false
        }
    }

    pub(crate) fn assign_rooted(
        &mut self,
        name: &str,
        indices: &[usize],
        value: RuntimeValue,
    ) -> RootedAssignResult {
        let Some(root_value) = self.get(name) else {
            return RootedAssignResult::MissingRoot;
        };
        let Some(updated_root) = assign_runtime_value_at_path(root_value, indices, value) else {
            return RootedAssignResult::InvalidPath;
        };
        if self.assign(name, updated_root) {
            RootedAssignResult::Applied
        } else {
            RootedAssignResult::MissingRoot
        }
    }

    pub(crate) fn clear(&mut self, name: &str) {
        self.values.remove(name);
        self.mut_values.remove(name);
    }

    pub(crate) fn get(&self, name: &str) -> Option<RuntimeValue> {
        if let Some(cell) = self.mut_values.get(name) {
            return Some(cell.borrow().clone());
        }
        if let Some(v) = self.values.get(name) {
            return Some(v.clone());
        }
        None
    }

    pub(crate) fn contains(&self, name: &str) -> bool {
        self.mut_values.contains_key(name) || self.values.contains_key(name)
    }

    pub(crate) fn binding_names(&self) -> Vec<String> {
        let mut names: HashSet<String> = self.values.keys().cloned().collect();
        names.extend(self.mut_values.keys().cloned());
        names.into_iter().collect()
    }

    pub(crate) fn apply_selected_from(&mut self, other: &Self, names: &HashSet<String>) {
        for name in names {
            if let Some(cell) = other.mut_values.get(name) {
                self.clear(name);
                self.mut_values.insert(name.clone(), Rc::clone(cell));
                continue;
            }
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
        (RuntimeValue::List(a), RuntimeValue::List(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(a, b)| runtime_value_eq(a, b))
        }
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
        (RuntimeValue::Callable(_), RuntimeValue::Callable(_)) => false,
        _ => false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RootedAssignResult {
    Applied,
    MissingRoot,
    InvalidPath,
}

#[derive(Clone)]
#[allow(clippy::large_enum_variant)]
pub(crate) enum RuntimeValue {
    String(String),
    Int(i64),
    Unit,
    Bool(bool),
    Tuple(Vec<RuntimeValue>),
    List(Vec<RuntimeValue>),
    Handler(InlineHandlerValue),
    Callable(Box<IntCallable>),
    Record {
        constructor: String,
        fields: HashMap<String, RuntimeValue>,
    },
}

impl RuntimeValue {
    pub(crate) fn list_from_strings(values: Vec<String>) -> Self {
        Self::List(values.into_iter().map(Self::String).collect())
    }

    pub(crate) fn as_list(&self) -> Option<&[RuntimeValue]> {
        match self {
            Self::List(values) => Some(values.as_slice()),
            _ => None,
        }
    }

    pub(crate) fn into_list(self) -> Option<Vec<RuntimeValue>> {
        match self {
            Self::List(values) => Some(values),
            _ => None,
        }
    }

    pub(crate) fn as_string_list(&self) -> Option<Vec<String>> {
        self.as_list().and_then(runtime_list_slice_as_strings)
    }

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
                let parts: Vec<String> = items
                    .iter()
                    .map(|v| v.format_text(quoted_strings))
                    .collect();
                format!("({})", parts.join(", "))
            }
            Self::List(values) => {
                let parts: Vec<String> = values.iter().map(|v| v.format_text(true)).collect();
                format!("[{}]", parts.join(", "))
            }
            Self::Handler(_) => "<handler>".to_string(),
            Self::Callable(_) => "<callable>".to_string(),
            Self::Record { constructor, .. } => constructor.clone(),
        }
    }
}

fn runtime_list_slice_as_strings(values: &[RuntimeValue]) -> Option<Vec<String>> {
    values
        .iter()
        .map(|value| match value {
            RuntimeValue::String(text) => Some(text.clone()),
            _ => None,
        })
        .collect()
}

fn assign_runtime_value_at_path(
    value: RuntimeValue,
    indices: &[usize],
    replacement: RuntimeValue,
) -> Option<RuntimeValue> {
    let Some((&head, tail)) = indices.split_first() else {
        return Some(replacement);
    };
    let RuntimeValue::List(mut items) = value else {
        return None;
    };
    let slot = items.get_mut(head)?;
    let updated = assign_runtime_value_at_path(slot.clone(), tail, replacement)?;
    *slot = updated;
    Some(RuntimeValue::List(items))
}

#[cfg(test)]
mod tests {
    use super::{
        RootedAssignResult, RuntimeLocals, RuntimeValue, runtime_value_eq, runtime_value_option_eq,
    };
    use std::collections::HashMap;

    fn list_from_ints(values: Vec<i64>) -> RuntimeValue {
        RuntimeValue::List(values.into_iter().map(RuntimeValue::Int).collect())
    }

    fn as_int_list(value: &RuntimeValue) -> Option<Vec<i64>> {
        value.as_list().and_then(|values| {
            values
                .iter()
                .map(|v| match v {
                    RuntimeValue::Int(n) => Some(*n),
                    _ => None,
                })
                .collect()
        })
    }

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
        // Derived views still work
        assert!(matches!(locals.get("n"), Some(RuntimeValue::Int(42))));
        assert!(matches!(
            locals.get("s"),
            Some(RuntimeValue::String(ref text)) if text == "ok"
        ));
    }

    #[test]
    fn cloned_runtime_locals_share_mut_binding_cells() {
        let mut locals = RuntimeLocals::default();
        locals.store_mut("count", RuntimeValue::Int(1));

        let mut cloned = locals.clone();
        assert!(cloned.assign("count", RuntimeValue::Int(7)));

        assert!(matches!(locals.get("count"), Some(RuntimeValue::Int(7))));
        assert!(matches!(cloned.get("count"), Some(RuntimeValue::Int(7))));
        assert!(matches!(locals.get("count"), Some(RuntimeValue::Int(7))));
    }

    #[test]
    fn runtime_locals_round_trip_bool_and_unit() {
        let mut locals = RuntimeLocals::default();
        locals.store("flag", RuntimeValue::Bool(true));
        locals.store("u", RuntimeValue::Unit);
        assert!(matches!(
            locals.get("flag"),
            Some(RuntimeValue::Bool(true))
        ));
        assert!(matches!(locals.get("u"), Some(RuntimeValue::Unit)));
        let names = locals.binding_names();
        assert!(names.contains(&"flag".to_string()));
        assert!(names.contains(&"u".to_string()));
    }

    #[test]
    fn runtime_locals_mixed_list_round_trips() {
        let mut locals = RuntimeLocals::default();
        locals.store(
            "xs",
            RuntimeValue::List(vec![
                RuntimeValue::Int(1),
                RuntimeValue::String("a".to_string()),
            ]),
        );
        let val = locals.get("xs").unwrap();
        assert!(matches!(val, RuntimeValue::List(_)));
        // Mixed list should not be representable as pure int list
        assert!(as_int_list(&val).is_none());
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

    #[test]
    fn runtime_value_formats_nested_lists_recursively() {
        let value = RuntimeValue::List(vec![
            list_from_ints(vec![1, 2, 3]),
            list_from_ints(vec![4, 5, 6]),
        ]);

        assert_eq!(value.to_output_text(), "[[1, 2, 3], [4, 5, 6]]");
        assert_eq!(value.to_expression_text(), "[[1, 2, 3], [4, 5, 6]]");
    }

    #[test]
    fn rooted_assign_updates_nested_list_via_path_copy() {
        let mut locals = RuntimeLocals::default();
        locals.store_mut(
            "xs",
            RuntimeValue::List(vec![
                list_from_ints(vec![1, 2]),
                list_from_ints(vec![3, 4]),
            ]),
        );

        assert_eq!(
            locals.assign_rooted("xs", &[1, 1], RuntimeValue::Int(30)),
            RootedAssignResult::Applied
        );
        assert_eq!(
            locals
                .get("xs")
                .expect("root should exist")
                .to_expression_text(),
            "[[1, 2], [3, 30]]"
        );
    }

    #[test]
    fn rooted_assign_rejects_invalid_list_path() {
        let mut locals = RuntimeLocals::default();
        locals.store_mut("xs", list_from_ints(vec![1, 2]));

        assert_eq!(
            locals.assign_rooted("xs", &[1, 0], RuntimeValue::Int(30)),
            RootedAssignResult::InvalidPath
        );
        assert_eq!(
            locals
                .get("xs")
                .expect("root should exist")
                .to_expression_text(),
            "[1, 2]"
        );
    }
}
