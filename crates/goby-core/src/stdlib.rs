use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use crate::parse_module;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StdlibResolver {
    root: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedStdlibModule {
    pub module_path: String,
    pub exports: HashMap<String, String>,
    pub embedded_effects: Vec<String>,
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
        let embedded_effects = collect_embedded_effects(module_path, &module.embed_declarations)?;
        Ok(ResolvedStdlibModule {
            module_path: module_path.to_string(),
            exports,
            embedded_effects,
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

fn is_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
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

fn collect_embedded_effects(
    module_path: &str,
    embeds: &[crate::ast::EmbedDecl],
) -> Result<Vec<String>, StdlibResolveError> {
    let mut seen = HashSet::new();
    let mut names = Vec::new();
    for embed in embeds {
        if !seen.insert(embed.effect_name.clone()) {
            return Err(StdlibResolveError::DuplicateEmbeddedEffect {
                module_path: module_path.to_string(),
                effect_name: embed.effect_name.clone(),
            });
        }
        names.push(embed.effect_name.clone());
    }
    Ok(names)
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::{StdlibResolveError, StdlibResolver};

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
            "concat : String -> String -> String\nconcat a b = a\n",
        )
        .expect("stdlib file should be writable");

        let resolver = StdlibResolver::new(root);
        let resolved = resolver
            .resolve_module("goby/string")
            .expect("stdlib module should resolve");
        assert_eq!(resolved.module_path, "goby/string");
        assert_eq!(
            resolved.exports.get("concat"),
            Some(&"String -> String -> String".to_string())
        );
        assert!(resolved.embedded_effects.is_empty());
    }

    #[test]
    fn resolves_embedded_effect_metadata_from_file() {
        let sandbox = TempDirGuard::new("resolve_embed_metadata");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/stdio.gb"),
            "effect Print\n  print : String -> Unit\n\n@embed Print\n",
        )
        .expect("stdlib file should be writable");

        let resolver = StdlibResolver::new(root);
        let resolved = resolver
            .resolve_module("goby/stdio")
            .expect("stdlib module should resolve");
        assert_eq!(resolved.embedded_effects, vec!["Print".to_string()]);
    }

    #[test]
    fn resolve_reports_duplicate_embedded_effect() {
        let sandbox = TempDirGuard::new("resolve_duplicate_embed");
        let root = sandbox.path.join("stdlib");
        fs::create_dir_all(root.join("goby")).expect("stdlib/goby should be creatable");
        fs::write(
            root.join("goby/stdio.gb"),
            "effect Print\n  print : String -> Unit\n\n@embed Print\n@embed Print\n",
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
        fs::write(root.join("goby/string.gb"), "concat : String ->\n")
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
