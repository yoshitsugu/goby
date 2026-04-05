use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use crate::ast::ImportKind;
use crate::parse_module;
use crate::parser_util::is_identifier;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StdlibResolver {
    root: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedStdlibModule {
    pub module_path: String,
    pub module: crate::ast::Module,
    pub exports: HashMap<String, String>,
    pub types: Vec<String>,
    pub effects: Vec<String>,
    pub visible_effects: Vec<VisibleEffectDecl>,
    pub embedded_defaults: Vec<EmbeddedDefaultHandlerDecl>,
    pub embedded_effect_exports: Vec<EmbeddedEffectExport>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmbeddedRuntimeHandlerKind {
    Stdout,
    Stdin,
}

impl EmbeddedRuntimeHandlerKind {
    pub fn from_handler_name(handler_name: &str) -> Option<Self> {
        match handler_name {
            "__goby_embeded_effect_stdout_handler" => Some(Self::Stdout),
            "__goby_embeded_effect_stdin_handler" => Some(Self::Stdin),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmbeddedDefaultHandlerDecl {
    pub effect_name: String,
    pub handler_name: String,
    pub runtime_kind: Option<EmbeddedRuntimeHandlerKind>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VisibleEffectDecl {
    pub source_module: String,
    pub decl: crate::ast::EffectDecl,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmbeddedEffectExport {
    pub effect_name: String,
    pub handler_name: String,
    pub runtime_kind: Option<EmbeddedRuntimeHandlerKind>,
    pub source_module: String,
    pub decl: crate::ast::EffectDecl,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StdlibResolveError {
    InvalidModulePath(String),
    ModuleNotFound {
        module_path: String,
        attempted_path: PathBuf,
    },
    ReadFailed {
        path: PathBuf,
        message: String,
    },
    ParseFailed {
        module_path: String,
        message: String,
    },
    DuplicateExport {
        module_path: String,
        symbol: String,
    },
    DuplicateEmbeddedEffect {
        module_path: String,
        effect_name: String,
    },
    ExportTypeMissing {
        module_path: String,
        symbol: String,
    },
}

impl StdlibResolver {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn resolve_module(
        &self,
        module_path: &str,
    ) -> Result<ResolvedStdlibModule, StdlibResolveError> {
        let mut visiting = HashSet::new();
        self.resolve_module_inner(module_path, &mut visiting)
    }

    fn resolve_module_inner(
        &self,
        module_path: &str,
        visiting: &mut HashSet<String>,
    ) -> Result<ResolvedStdlibModule, StdlibResolveError> {
        if !visiting.insert(module_path.to_string()) {
            return Err(StdlibResolveError::ParseFailed {
                module_path: module_path.to_string(),
                message: "cyclic stdlib import in resolver metadata".to_string(),
            });
        }
        let attempted_path = self.module_file_path(module_path)?;
        let source =
            std::fs::read_to_string(&attempted_path).map_err(|read_err| match read_err.kind() {
                std::io::ErrorKind::NotFound => StdlibResolveError::ModuleNotFound {
                    module_path: module_path.to_string(),
                    attempted_path: attempted_path.clone(),
                },
                _ => StdlibResolveError::ReadFailed {
                    path: attempted_path.clone(),
                    message: read_err.to_string(),
                },
            })?;
        let module =
            parse_module(&source).map_err(|parse_err| StdlibResolveError::ParseFailed {
                module_path: module_path.to_string(),
                message: parse_err.message,
            })?;
        let exports = collect_exports(module_path, &module.declarations)?;
        let types = collect_types(&module.type_declarations);
        let embedded_defaults = collect_embedded_defaults(module_path, &module.embed_declarations)?;
        let visible_effects = collect_visible_effects(self, module_path, &module, visiting)?;
        let effects = collect_effect_names(&visible_effects);
        let embedded_effect_exports =
            collect_embedded_effect_exports(&embedded_defaults, &visible_effects);
        visiting.remove(module_path);
        Ok(ResolvedStdlibModule {
            module_path: module_path.to_string(),
            module,
            exports,
            types,
            effects,
            visible_effects,
            embedded_defaults,
            embedded_effect_exports,
        })
    }

    pub fn module_file_path(&self, module_path: &str) -> Result<PathBuf, StdlibResolveError> {
        if !is_valid_module_path(module_path) {
            return Err(StdlibResolveError::InvalidModulePath(
                module_path.to_string(),
            ));
        }
        Ok(self.root.join(format!("{module_path}.gb")))
    }
}

fn is_valid_module_path(module_path: &str) -> bool {
    if module_path.is_empty() {
        return false;
    }

    let segments: Vec<&str> = module_path.split('/').collect();
    if segments.is_empty() {
        return false;
    }

    segments.iter().all(|segment| is_identifier(segment))
}

fn collect_exports(
    module_path: &str,
    declarations: &[crate::ast::Declaration],
) -> Result<HashMap<String, String>, StdlibResolveError> {
    let mut exports = HashMap::new();
    for declaration in declarations {
        let ty = declaration.type_annotation.as_deref().ok_or_else(|| {
            StdlibResolveError::ExportTypeMissing {
                module_path: module_path.to_string(),
                symbol: declaration.name.clone(),
            }
        })?;
        if exports
            .insert(declaration.name.clone(), ty.trim().to_string())
            .is_some()
        {
            return Err(StdlibResolveError::DuplicateExport {
                module_path: module_path.to_string(),
                symbol: declaration.name.clone(),
            });
        }
    }
    Ok(exports)
}

fn collect_embedded_defaults(
    module_path: &str,
    embeds: &[crate::ast::EmbedDecl],
) -> Result<Vec<EmbeddedDefaultHandlerDecl>, StdlibResolveError> {
    let mut seen = HashSet::new();
    let mut defaults = Vec::new();
    for embed in embeds {
        if !seen.insert(embed.effect_name.clone()) {
            return Err(StdlibResolveError::DuplicateEmbeddedEffect {
                module_path: module_path.to_string(),
                effect_name: embed.effect_name.clone(),
            });
        }
        defaults.push(EmbeddedDefaultHandlerDecl {
            effect_name: embed.effect_name.clone(),
            handler_name: embed.handler_name.clone(),
            runtime_kind: EmbeddedRuntimeHandlerKind::from_handler_name(&embed.handler_name),
        });
    }
    Ok(defaults)
}

fn collect_visible_effects(
    resolver: &StdlibResolver,
    module_path: &str,
    module: &crate::ast::Module,
    visiting: &mut HashSet<String>,
) -> Result<Vec<VisibleEffectDecl>, StdlibResolveError> {
    let mut visible = module
        .effect_declarations
        .iter()
        .cloned()
        .map(|decl| VisibleEffectDecl {
            source_module: module_path.to_string(),
            decl,
        })
        .collect::<Vec<_>>();

    for import in &module.imports {
        let imported = resolver.resolve_module_inner(&import.module_path, visiting)?;
        visible.extend(
            imported
                .visible_effects
                .into_iter()
                .filter(|effect| import_selects_name(&import.kind, &effect.decl.name)),
        );
    }

    Ok(visible)
}

fn collect_effect_names(visible_effects: &[VisibleEffectDecl]) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut names = Vec::new();
    for effect in visible_effects {
        if seen.insert(effect.decl.name.clone()) {
            names.push(effect.decl.name.clone());
        }
    }
    names
}

fn collect_embedded_effect_exports(
    embedded_defaults: &[EmbeddedDefaultHandlerDecl],
    visible_effects: &[VisibleEffectDecl],
) -> Vec<EmbeddedEffectExport> {
    let mut exports = Vec::new();
    for embed in embedded_defaults {
        let mut matches = visible_effects
            .iter()
            .filter(|effect| effect.decl.name == embed.effect_name);
        let Some(effect) = matches.next() else {
            continue;
        };
        if matches.next().is_some() {
            continue;
        }
        exports.push(EmbeddedEffectExport {
            effect_name: embed.effect_name.clone(),
            handler_name: embed.handler_name.clone(),
            runtime_kind: embed.runtime_kind,
            source_module: effect.source_module.clone(),
            decl: effect.decl.clone(),
        });
    }
    exports
}

fn import_selects_name(kind: &ImportKind, name: &str) -> bool {
    match kind {
        ImportKind::Selective(names) => names.iter().any(|selected| selected == name),
        ImportKind::Plain | ImportKind::Alias(_) => true,
    }
}

fn collect_types(types: &[crate::ast::TypeDeclaration]) -> Vec<String> {
    types
        .iter()
        .map(|decl| match decl {
            crate::ast::TypeDeclaration::Alias { name, .. } => name.clone(),
            crate::ast::TypeDeclaration::Union { name, .. } => name.clone(),
            crate::ast::TypeDeclaration::Record { name, .. } => name.clone(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::{EmbeddedDefaultHandlerDecl, StdlibResolveError, StdlibResolver};

    struct TempDirGuard {
        path: PathBuf,
    }

    impl TempDirGuard {
        fn new(label: &str) -> Self {
            let nanos = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("clock should be monotonic enough for tests")
                .as_nanos();
            let path = std::env::temp_dir().join(format!(
                "goby_core_stdlib_{}_{}_{}",
                label,
                std::process::id(),
                nanos
            ));
            fs::create_dir_all(&path).expect("temp directory should be creatable");
            Self { path }
        }
    }

    impl Drop for TempDirGuard {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    #[test]
    fn maps_module_path_to_stdlib_file_path() {
        let resolver = StdlibResolver::new(PathBuf::from("/repo/stdlib"));
        let path = resolver
            .module_file_path("goby/string")
            .expect("path mapping should work");
        assert_eq!(path, PathBuf::from("/repo/stdlib/goby/string.gb"));
    }

    #[test]
    fn maps_nested_module_path_to_stdlib_file_path() {
        let resolver = StdlibResolver::new(PathBuf::from("/repo/stdlib"));
        let path = resolver
            .module_file_path("goby/internal/debug")
            .expect("nested path mapping should work");
        assert_eq!(path, PathBuf::from("/repo/stdlib/goby/internal/debug.gb"));
    }

    #[test]
    fn rejects_invalid_module_path_for_path_mapping() {
        let resolver = StdlibResolver::new(PathBuf::from("/repo/stdlib"));
        let err = resolver
            .module_file_path("goby//string")
            .expect_err("invalid path should fail");
        assert_eq!(
            err,
            StdlibResolveError::InvalidModulePath("goby//string".to_string())
        );
    }

    #[test]
    fn resolves_module_exports_from_file() {
        let sandbox = TempDirGuard::new("resolve_success");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/string.gb"),
            "type Token = Token(value: String)\neffect StringOps\n  split_op : String -> String -> List String\nsplit : String -> String -> List String\nsplit value sep = []\n",
        )
        .expect("stdlib file should be writable");

        let resolver = StdlibResolver::new(root);
        let resolved = resolver
            .resolve_module("goby/string")
            .expect("stdlib module should resolve");
        assert_eq!(resolved.module_path, "goby/string");
        assert_eq!(
            resolved.exports.get("split"),
            Some(&"String -> String -> List String".to_string())
        );
        assert_eq!(resolved.types, vec!["Token".to_string()]);
        assert_eq!(resolved.effects, vec!["StringOps".to_string()]);
        assert!(resolved.embedded_defaults.is_empty());
    }

    #[test]
    fn resolves_embedded_effect_metadata_from_file() {
        let sandbox = TempDirGuard::new("resolve_embed_metadata");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/stdio.gb"),
            "effect Print\n  print : String -> Unit\n  println : String -> Unit\n\n@embed Print __goby_embeded_effect_stdout_handler\n",
        )
        .expect("stdlib file should be writable");

        let resolver = StdlibResolver::new(root);
        let resolved = resolver
            .resolve_module("goby/stdio")
            .expect("stdlib module should resolve");
        assert_eq!(
            resolved.embedded_defaults,
            vec![EmbeddedDefaultHandlerDecl {
                effect_name: "Print".to_string(),
                handler_name: "__goby_embeded_effect_stdout_handler".to_string(),
                runtime_kind: Some(super::EmbeddedRuntimeHandlerKind::Stdout),
            }]
        );
    }

    #[test]
    fn resolves_visible_imported_effect_for_prelude_embed() {
        let sandbox = TempDirGuard::new("resolve_prelude_embed_import");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/stdio.gb"),
            "effect Print\n  print : String -> Unit\n  println : String -> Unit\n",
        )
        .expect("stdlib file should be writable");
        fs::write(
            root.join("goby/prelude.gb"),
            "import goby/stdio\n@embed Print __goby_embeded_effect_stdout_handler\n",
        )
        .expect("stdlib file should be writable");

        let resolver = StdlibResolver::new(root);
        let resolved = resolver
            .resolve_module("goby/prelude")
            .expect("prelude module should resolve");
        assert_eq!(resolved.effects, vec!["Print".to_string()]);
        assert_eq!(resolved.visible_effects.len(), 1);
        assert_eq!(resolved.visible_effects[0].source_module, "goby/stdio");
        assert_eq!(resolved.embedded_effect_exports.len(), 1);
        assert_eq!(
            resolved.embedded_effect_exports[0].source_module,
            "goby/stdio"
        );
        assert_eq!(resolved.embedded_effect_exports[0].effect_name, "Print");
    }

    #[test]
    fn resolve_reports_duplicate_embedded_effect() {
        let sandbox = TempDirGuard::new("resolve_duplicate_embed");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/stdio.gb"),
            "effect Print\n  print : String -> Unit\n  println : String -> Unit\n\n@embed Print __goby_embeded_effect_stdout_handler\n@embed Print __goby_embeded_effect_stdout_handler\n",
        )
        .expect("stdlib file should be writable");

        let resolver = StdlibResolver::new(root);
        let err = resolver
            .resolve_module("goby/stdio")
            .expect_err("duplicate embed effect should fail");
        assert_eq!(
            err,
            StdlibResolveError::DuplicateEmbeddedEffect {
                module_path: "goby/stdio".to_string(),
                effect_name: "Print".to_string(),
            }
        );
    }

    #[test]
    fn resolve_reports_module_not_found_with_attempted_path() {
        let sandbox = TempDirGuard::new("resolve_not_found");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(&root).expect("stdlib root should be creatable");
        let resolver = StdlibResolver::new(root.clone());
        let err = resolver
            .resolve_module("goby/string")
            .expect_err("missing module should fail");
        assert_eq!(
            err,
            StdlibResolveError::ModuleNotFound {
                module_path: "goby/string".to_string(),
                attempted_path: root.join("goby/string.gb"),
            }
        );
    }

    #[test]
    fn resolve_reports_parse_failed() {
        let sandbox = TempDirGuard::new("resolve_parse_failed");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(root.join("goby/string.gb"), "split : String ->\n")
            .expect("stdlib file should be writable");

        let resolver = StdlibResolver::new(root);
        let err = resolver
            .resolve_module("goby/string")
            .expect_err("parse failure should be reported");
        match err {
            StdlibResolveError::ParseFailed { module_path, .. } => {
                assert_eq!(module_path, "goby/string");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn resolve_reports_duplicate_export() {
        let sandbox = TempDirGuard::new("resolve_duplicate_export");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/string.gb"),
            "x : Int\nx = 1\nx : Int\nx = 2\n",
        )
        .expect("stdlib file should be writable");

        let resolver = StdlibResolver::new(root);
        let err = resolver
            .resolve_module("goby/string")
            .expect_err("duplicate export should fail");
        assert_eq!(
            err,
            StdlibResolveError::DuplicateExport {
                module_path: "goby/string".to_string(),
                symbol: "x".to_string(),
            }
        );
    }

    #[test]
    fn resolve_reports_export_type_missing() {
        let sandbox = TempDirGuard::new("resolve_missing_type");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(root.join("goby/string.gb"), "x = 1\n").expect("stdlib file should be writable");

        let resolver = StdlibResolver::new(root);
        let err = resolver
            .resolve_module("goby/string")
            .expect_err("missing type annotation should fail");
        assert_eq!(
            err,
            StdlibResolveError::ExportTypeMissing {
                module_path: "goby/string".to_string(),
                symbol: "x".to_string(),
            }
        );
    }
}
