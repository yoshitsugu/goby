use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StdlibResolver {
    root: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedStdlibModule {
    pub module_path: String,
    pub exports: HashMap<String, String>,
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
        Err(StdlibResolveError::ModuleNotFound {
            module_path: module_path.to_string(),
            attempted_path,
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

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::{StdlibResolveError, StdlibResolver};

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
}
