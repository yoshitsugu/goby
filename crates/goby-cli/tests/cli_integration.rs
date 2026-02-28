use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};
use goby_core::parse_module;

struct TempDirGuard {
    path: PathBuf,
}

impl TempDirGuard {
    fn new(label: &str) -> Self {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be monotonic enough for tests")
            .as_nanos();
        let path =
            env::temp_dir().join(format!("goby_cli_{}_{}_{}", label, std::process::id(), nanos));
        fs::create_dir_all(&path).expect("temp directory should be creatable");
        Self { path }
    }

    fn join<P: AsRef<Path>>(&self, p: P) -> PathBuf {
        self.path.join(p)
    }
}

impl Drop for TempDirGuard {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

fn command_for_goby_cli() -> Command {
    Command::new(env!("CARGO_BIN_EXE_goby-cli"))
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root should exist")
}

#[test]
fn check_command_succeeds_for_function_example() {
    let root = repo_root();
    let output = command_for_goby_cli()
        .arg("check")
        .arg("examples/function.gb")
        .current_dir(&root)
        .output()
        .expect("cli should execute");

    assert!(
        output.status.success(),
        "expected success, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("parsed and typechecked"),
        "unexpected stdout: {}",
        stdout
    );
}

#[cfg(unix)]
fn install_fake_wasmtime(script_path: &Path) {
    use std::os::unix::fs::PermissionsExt;

    let script = r#"#!/bin/sh
if [ "$1" = "run" ] && [ "$2" = "--invoke" ] && [ "$3" = "main" ]; then
  printf "90\n[30, 40, 50]\n[60, 70]\nsomething\n15\n"
  exit 0
fi
echo "unexpected args: $*" >&2
exit 1
"#;
    fs::write(script_path, script).expect("fake wasmtime should be writable");
    let mut perms = fs::metadata(script_path)
        .expect("fake wasmtime metadata should be readable")
        .permissions();
    perms.set_mode(0o755);
    fs::set_permissions(script_path, perms).expect("fake wasmtime should be executable");
}

#[test]
#[cfg(unix)]
fn run_command_emits_locked_function_output_via_wasmtime() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_function");
    let fake_bin = sandbox.join("bin");
    fs::create_dir_all(&fake_bin).expect("bin directory should be creatable");
    install_fake_wasmtime(&fake_bin.join("wasmtime"));

    let source = fs::read_to_string(root.join("examples/function.gb"))
        .expect("function example should be readable");
    let declaration_count = parse_module(&source)
        .expect("function example should parse")
        .declarations
        .len();
    let input = sandbox.join("function.gb");
    fs::write(&input, source).expect("temporary input should be writable");
    let mut path_entries = vec![fake_bin.clone()];
    if let Some(existing) = env::var_os("PATH") {
        path_entries.extend(env::split_paths(&existing));
    }
    let merged_path = env::join_paths(path_entries).expect("PATH should be joinable");

    let output = command_for_goby_cli()
        .arg("run")
        .arg(&input)
        .env("PATH", merged_path)
        .current_dir(&root)
        .output()
        .expect("cli should execute");

    assert!(
        output.status.success(),
        "expected success, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let expected = vec![
        format!(
            "parsed and typechecked {} declarations from {}",
            declaration_count,
            input.display()
        ),
        format!("generated wasm: {}", input.with_extension("wasm").display()),
        "90".to_string(),
        "[30, 40, 50]".to_string(),
        "[60, 70]".to_string(),
        "something".to_string(),
        "15".to_string(),
        "executed wasm via wasmtime".to_string(),
    ];
    let actual: Vec<String> = stdout.lines().map(|line| line.to_string()).collect();
    assert_eq!(actual, expected);

    let wasm_out = input.with_extension("wasm");
    assert!(wasm_out.exists(), "expected generated wasm at {:?}", wasm_out);
}

#[test]
fn run_command_skips_when_wasmtime_is_missing() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_no_wasmtime");
    let empty_bin = sandbox.join("empty-bin");
    fs::create_dir_all(&empty_bin).expect("empty bin should be creatable");
    let source = fs::read_to_string(root.join("examples/function.gb"))
        .expect("function example should be readable");
    let input = sandbox.join("function.gb");
    fs::write(&input, source).expect("temporary input should be writable");

    let output = command_for_goby_cli()
        .arg("run")
        .arg(&input)
        .env("PATH", &empty_bin)
        .current_dir(&root)
        .output()
        .expect("cli should execute");

    assert!(
        output.status.success(),
        "expected success, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("wasmtime not found; skipped wasm execution"));
}
