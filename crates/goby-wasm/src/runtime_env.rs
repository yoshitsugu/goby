use std::collections::HashMap;
use std::io::Read as _;
use std::path::PathBuf;

use goby_core::{
    ImportDecl, ImportKind, Module,
    stdlib::{EmbeddedRuntimeHandlerKind, StdlibResolver},
};

use crate::{PRELUDE_MODULE_PATH, RuntimeValue};

pub(crate) struct EmbeddedEffectRuntime {
    // Intentionally narrow runtime layer for prelude-backed Print/Read defaults.
    // This is not meant to grow into the general host-capability surface.
    outputs: Vec<String>,
    stdin_buffer: Option<String>,
    stdin_cursor: usize,
    allow_live_stdin: bool,
}

#[derive(Default)]
pub(crate) struct RuntimeImportContext {
    pub(crate) modules: HashMap<String, Module>,
    pub(crate) embedded_default_handlers: HashMap<String, EmbeddedRuntimeHandlerKind>,
}

impl EmbeddedEffectRuntime {
    pub(crate) fn new(stdin_seed: Option<String>, allow_live_stdin: bool) -> Self {
        Self {
            outputs: Vec::new(),
            stdin_buffer: stdin_seed,
            stdin_cursor: 0,
            allow_live_stdin,
        }
    }

    pub(crate) fn output_len(&self) -> usize {
        self.outputs.len()
    }

    pub(crate) fn outputs_are_empty(&self) -> bool {
        self.outputs.is_empty()
    }

    pub(crate) fn concat_outputs(&self) -> String {
        self.outputs.concat()
    }

    pub(crate) fn emit_output_text(&mut self, text: String) {
        self.outputs.push(text);
    }

    pub(crate) fn emit_output_line(&mut self, mut text: String) {
        if !text.ends_with('\n') {
            text.push('\n');
        }
        self.outputs.push(text);
    }

    pub(crate) fn invoke(
        &mut self,
        handler_kind: EmbeddedRuntimeHandlerKind,
        method_name: &str,
        arg_val: RuntimeValue,
    ) -> Result<Option<RuntimeValue>, String> {
        match (handler_kind, method_name) {
            (EmbeddedRuntimeHandlerKind::Stdout, "print") => {
                self.emit_output_text(arg_val.to_output_text());
                Ok(Some(RuntimeValue::Unit))
            }
            (EmbeddedRuntimeHandlerKind::Stdout, "println") => {
                self.emit_output_line(arg_val.to_output_text());
                Ok(Some(RuntimeValue::Unit))
            }
            (EmbeddedRuntimeHandlerKind::Stdin, "read")
                if matches!(arg_val, RuntimeValue::Unit) =>
            {
                self.read_stdin_remaining("read")
                    .map(|text| Some(RuntimeValue::String(text)))
            }
            (EmbeddedRuntimeHandlerKind::Stdin, "read_line")
                if matches!(arg_val, RuntimeValue::Unit) =>
            {
                self.read_stdin_line("read_line")
                    .map(|text| Some(RuntimeValue::String(text)))
            }
            (EmbeddedRuntimeHandlerKind::Stdin, "read_lines")
                if matches!(arg_val, RuntimeValue::Unit) =>
            {
                self.read_stdin_lines("read_lines")
                    .map(|lines| Some(RuntimeValue::ListString(lines)))
            }
            _ => Ok(None),
        }
    }

    fn ensure_stdin_loaded(&mut self, op_name: &str) -> Result<(), String> {
        if self.stdin_buffer.is_some() {
            return Ok(());
        }
        if !self.allow_live_stdin {
            return Err(format!(
                "Read.{op_name} requires runtime stdin-backed Wasm execution; compile-time fallback cannot consume stdin"
            ));
        }
        let mut bytes = Vec::new();
        match std::io::stdin().read_to_end(&mut bytes) {
            Ok(_) => {
                self.stdin_buffer = Some(String::from_utf8_lossy(&bytes).into_owned());
                Ok(())
            }
            Err(err) => Err(format!("Read.{op_name} failed to read stdin: {err}")),
        }
    }

    fn read_stdin_remaining(&mut self, op_name: &str) -> Result<String, String> {
        self.ensure_stdin_loaded(op_name)?;
        let content = self.stdin_buffer.as_ref().expect("stdin should be loaded");
        if self.stdin_cursor >= content.len() {
            return Ok(String::new());
        }
        let tail = content[self.stdin_cursor..].to_string();
        self.stdin_cursor = content.len();
        Ok(tail)
    }

    fn read_stdin_line(&mut self, op_name: &str) -> Result<String, String> {
        self.ensure_stdin_loaded(op_name)?;
        let content = self.stdin_buffer.as_ref().expect("stdin should be loaded");
        if self.stdin_cursor >= content.len() {
            return Ok(String::new());
        }
        let bytes = content.as_bytes();
        let start = self.stdin_cursor;
        let mut idx = start;
        while idx < bytes.len() {
            match bytes[idx] {
                b'\n' => {
                    let line = content[start..idx].to_string();
                    self.stdin_cursor = idx + 1;
                    return Ok(line);
                }
                b'\r' => {
                    let line = content[start..idx].to_string();
                    if idx + 1 < bytes.len() && bytes[idx + 1] == b'\n' {
                        self.stdin_cursor = idx + 2;
                    } else {
                        self.stdin_cursor = idx + 1;
                    }
                    return Ok(line);
                }
                _ => idx += 1,
            }
        }
        let line = content[start..].to_string();
        self.stdin_cursor = content.len();
        Ok(line)
    }

    fn read_stdin_lines(&mut self, op_name: &str) -> Result<Vec<String>, String> {
        let tail = self.read_stdin_remaining(op_name)?;
        Ok(split_input_lines(&tail))
    }
}

pub(crate) fn split_input_lines(text: &str) -> Vec<String> {
    let bytes = text.as_bytes();
    let mut lines = Vec::new();
    let mut start = 0usize;
    let mut idx = 0usize;

    while idx < bytes.len() {
        match bytes[idx] {
            b'\n' => {
                lines.push(text[start..idx].to_string());
                idx += 1;
                start = idx;
            }
            b'\r' => {
                lines.push(text[start..idx].to_string());
                idx += 1;
                if idx < bytes.len() && bytes[idx] == b'\n' {
                    idx += 1;
                }
                start = idx;
            }
            _ => idx += 1,
        }
    }

    if start < bytes.len() {
        lines.push(text[start..].to_string());
    }

    lines
}

pub(crate) fn load_runtime_import_context(module: &Module) -> RuntimeImportContext {
    let mut context = RuntimeImportContext {
        modules: HashMap::new(),
        embedded_default_handlers: module
            .embed_declarations
            .iter()
            .filter_map(|embed| {
                EmbeddedRuntimeHandlerKind::from_handler_name(&embed.handler_name)
                    .map(|kind| (embed.effect_name.clone(), kind))
            })
            .collect(),
    };
    let resolver = StdlibResolver::new(resolve_runtime_stdlib_root());
    let mut pending = effective_runtime_imports(module);
    while let Some(import) = pending.pop() {
        if context.modules.contains_key(&import.module_path) {
            continue;
        }
        let Ok(resolved) = resolver.resolve_module(&import.module_path) else {
            continue;
        };
        pending.extend(effective_runtime_imports(&resolved.module));
        for embed in resolved.embedded_defaults {
            let Some(kind) = embed.runtime_kind else {
                continue;
            };
            context
                .embedded_default_handlers
                .entry(embed.effect_name)
                .or_insert(kind);
        }
        context.modules.insert(import.module_path, resolved.module);
    }
    context
}

pub(crate) fn runtime_import_selects_name(kind: &ImportKind, name: &str) -> bool {
    match kind {
        ImportKind::Selective(names) => names.iter().any(|selected| selected == name),
        ImportKind::Plain | ImportKind::Alias(_) => true,
    }
}

pub(crate) fn effective_runtime_imports(module: &Module) -> Vec<ImportDecl> {
    let mut imports = module.imports.clone();
    let has_prelude = imports
        .iter()
        .any(|import| import.module_path == PRELUDE_MODULE_PATH);
    if has_prelude {
        return imports;
    }
    let resolver = StdlibResolver::new(resolve_runtime_stdlib_root());
    let prelude_available = resolver
        .module_file_path(PRELUDE_MODULE_PATH)
        .ok()
        .is_some_and(|path| path.exists());
    if prelude_available {
        imports.push(ImportDecl {
            module_path: PRELUDE_MODULE_PATH.to_string(),
            kind: ImportKind::Plain,
            module_path_span: None,
            kind_span: None,
        });
    }
    imports
}

fn resolve_runtime_stdlib_root() -> PathBuf {
    std::env::var_os("GOBY_STDLIB_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../..")
                .join("stdlib")
        })
}

#[cfg(test)]
mod tests {
    use super::{
        EmbeddedEffectRuntime, effective_runtime_imports, runtime_import_selects_name,
        split_input_lines,
    };
    use goby_core::{ImportKind, parse_module, stdlib::EmbeddedRuntimeHandlerKind};

    #[test]
    fn effective_runtime_imports_adds_implicit_prelude_once() {
        let module = parse_module("main = Unit").expect("module should parse");
        let imports = effective_runtime_imports(&module);
        let prelude_count = imports
            .iter()
            .filter(|import| import.module_path == "goby/prelude")
            .count();

        assert_eq!(prelude_count, 1);
    }

    #[test]
    fn runtime_import_selective_filter_matches_expected_names() {
        let kind = ImportKind::Selective(vec!["map".to_string(), "each".to_string()]);

        assert!(runtime_import_selects_name(&kind, "map"));
        assert!(!runtime_import_selects_name(&kind, "join"));
    }

    #[test]
    fn embedded_effect_runtime_reads_crlf_line_by_line() {
        let mut runtime = EmbeddedEffectRuntime::new(Some("a\r\nb\n".to_string()), true);

        let first = runtime
            .invoke(
                EmbeddedRuntimeHandlerKind::Stdin,
                "read_line",
                crate::RuntimeValue::Unit,
            )
            .expect("stdin read should succeed");
        let second = runtime
            .invoke(
                EmbeddedRuntimeHandlerKind::Stdin,
                "read_line",
                crate::RuntimeValue::Unit,
            )
            .expect("stdin read should succeed");

        assert!(matches!(
            first,
            Some(crate::RuntimeValue::String(text)) if text == "a"
        ));
        assert!(matches!(
            second,
            Some(crate::RuntimeValue::String(text)) if text == "b"
        ));
    }

    #[test]
    fn embedded_effect_runtime_read_returns_empty_after_exhaustion() {
        let mut runtime = EmbeddedEffectRuntime::new(Some("abc".to_string()), true);

        let first = runtime
            .invoke(
                EmbeddedRuntimeHandlerKind::Stdin,
                "read",
                crate::RuntimeValue::Unit,
            )
            .expect("stdin read should succeed");
        let second = runtime
            .invoke(
                EmbeddedRuntimeHandlerKind::Stdin,
                "read",
                crate::RuntimeValue::Unit,
            )
            .expect("stdin read should succeed");

        assert!(matches!(
            first,
            Some(crate::RuntimeValue::String(text)) if text == "abc"
        ));
        assert!(matches!(
            second,
            Some(crate::RuntimeValue::String(text)) if text.is_empty()
        ));
    }

    #[test]
    fn embedded_effect_runtime_mixes_read_line_then_read_remaining() {
        let mut runtime = EmbeddedEffectRuntime::new(Some("a\nb\nc".to_string()), true);

        let first = runtime
            .invoke(
                EmbeddedRuntimeHandlerKind::Stdin,
                "read_line",
                crate::RuntimeValue::Unit,
            )
            .expect("stdin read_line should succeed");
        let second = runtime
            .invoke(
                EmbeddedRuntimeHandlerKind::Stdin,
                "read",
                crate::RuntimeValue::Unit,
            )
            .expect("stdin read should succeed");
        let third = runtime
            .invoke(
                EmbeddedRuntimeHandlerKind::Stdin,
                "read_line",
                crate::RuntimeValue::Unit,
            )
            .expect("stdin read_line after exhaustion should succeed");

        assert!(matches!(
            first,
            Some(crate::RuntimeValue::String(text)) if text == "a"
        ));
        assert!(matches!(
            second,
            Some(crate::RuntimeValue::String(text)) if text == "b\nc"
        ));
        assert!(matches!(
            third,
            Some(crate::RuntimeValue::String(text)) if text.is_empty()
        ));
    }

    #[test]
    fn split_input_lines_normalizes_all_newline_forms() {
        assert_eq!(
            split_input_lines("a\r\nb\nc\rd\r\n"),
            vec!["a", "b", "c", "d"]
        );
        assert_eq!(split_input_lines(""), Vec::<String>::new());
        assert_eq!(split_input_lines("\n"), vec![""]);
    }

    #[test]
    fn embedded_effect_runtime_read_lines_consumes_remaining_input() {
        let mut runtime = EmbeddedEffectRuntime::new(Some("a\r\nb\nc\rd\r\n".to_string()), true);

        let lines = runtime
            .invoke(
                EmbeddedRuntimeHandlerKind::Stdin,
                "read_lines",
                crate::RuntimeValue::Unit,
            )
            .expect("stdin read_lines should succeed");
        let rest = runtime
            .invoke(
                EmbeddedRuntimeHandlerKind::Stdin,
                "read",
                crate::RuntimeValue::Unit,
            )
            .expect("stdin read after read_lines should succeed");

        assert!(matches!(
            lines,
            Some(crate::RuntimeValue::ListString(lines))
                if lines == vec!["a", "b", "c", "d"]
        ));
        assert!(matches!(
            rest,
            Some(crate::RuntimeValue::String(text)) if text.is_empty()
        ));
    }

    #[test]
    fn embedded_effect_runtime_rejects_live_stdin_when_disallowed() {
        let mut runtime = EmbeddedEffectRuntime::new(None, false);

        let err = match runtime.invoke(
            EmbeddedRuntimeHandlerKind::Stdin,
            "read",
            crate::RuntimeValue::Unit,
        ) {
            Ok(_) => panic!("live stdin should be rejected in compile-time fallback mode"),
            Err(err) => err,
        };

        assert!(
            err.contains("compile-time fallback cannot consume stdin"),
            "unexpected error: {err}"
        );
    }
}
