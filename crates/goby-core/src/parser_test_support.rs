#[cfg(test)]
pub(crate) fn read_example(name: &str) -> String {
    let mut path = crate::path_util::workspace_root();
    path.push("examples");
    path.push(name);
    std::fs::read_to_string(path).expect("example file should exist")
}
