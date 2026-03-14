use std::path::{Path, PathBuf};

pub(crate) fn workspace_root() -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    find_workspace_root(&manifest_dir).unwrap_or_else(|| manifest_dir.join("../.."))
}

fn find_workspace_root(start: &Path) -> Option<PathBuf> {
    for ancestor in start.ancestors() {
        if ancestor.join("Cargo.toml").exists() && ancestor.join("stdlib").is_dir() {
            return Some(ancestor.to_path_buf());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::workspace_root;

    #[test]
    fn resolves_workspace_root_with_stdlib_and_examples() {
        let root = workspace_root();
        assert!(
            root.join("Cargo.toml").exists(),
            "workspace Cargo.toml missing"
        );
        assert!(
            root.join("stdlib").is_dir(),
            "workspace stdlib directory missing"
        );
        assert!(
            root.join("examples").is_dir(),
            "workspace examples directory missing"
        );
    }
}
