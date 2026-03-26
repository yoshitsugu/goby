use goby_core::parse_module;
use std::env;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

struct TempDirGuard {
    path: PathBuf,
}

impl TempDirGuard {
    fn new(label: &str) -> Self {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be monotonic enough for tests")
            .as_nanos();
        let path = env::temp_dir().join(format!(
            "goby_cli_{}_{}_{}",
            label,
            std::process::id(),
            nanos
        ));
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
    Command::new(env!("CARGO_BIN_EXE_goby"))
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root should exist")
}

fn run_goby_with_stdin(root: &Path, input: &Path, stdin: &[u8]) -> Output {
    let mut child = command_for_goby_cli()
        .arg("run")
        .arg(input)
        .current_dir(root)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("cli should execute");
    child
        .stdin
        .as_mut()
        .expect("stdin pipe should exist")
        .write_all(stdin)
        .expect("stdin should be writable");
    child
        .wait_with_output()
        .expect("cli output should be readable")
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

#[test]
fn lint_command_succeeds_for_clean_source() {
    let root = repo_root();
    let output = command_for_goby_cli()
        .arg("lint")
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

#[test]
fn lint_command_reports_unreachable_case_arm_warning() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("lint_unreachable_case_arm");
    let input = sandbox.join("lint_warning.gb");
    fs::write(
        &input,
        r#"f : Int -> Int
f n =
  case n
    _ -> 0
    1 -> 1
"#,
    )
    .expect("temporary input should be writable");

    let output = command_for_goby_cli()
        .arg("lint")
        .arg(&input)
        .current_dir(&root)
        .output()
        .expect("cli should execute");

    assert!(
        !output.status.success(),
        "expected lint warning to produce non-zero exit"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("warning: unreachable case arm [unreachable-arm]"),
        "unexpected stderr: {}",
        stderr
    );
    assert!(
        stderr.contains("in 'f'"),
        "expected declaration context in stderr: {}",
        stderr
    );
}

#[test]
fn check_command_uses_default_stdlib_root_for_file_based_symbol() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("check_default_stdlib_root");
    let input = sandbox.join("uses_stdlib_length.gb");
    fs::write(
        &input,
        "import goby/string ( length )\nf : Unit -> Int\nf = length(\"abc\")\n",
    )
    .expect("temporary input should be writable");

    let output = command_for_goby_cli()
        .arg("check")
        .arg(&input)
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

#[test]
fn check_command_reports_invalid_stdlib_root_env_path() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("check_invalid_stdlib_root");
    let missing_root = sandbox.join("missing-stdlib-root");
    let output = command_for_goby_cli()
        .arg("check")
        .arg("examples/function.gb")
        .env("GOBY_STDLIB_ROOT", &missing_root)
        .current_dir(&root)
        .output()
        .expect("cli should execute");

    assert!(
        !output.status.success(),
        "expected failure for invalid stdlib root"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("stdlib root does not exist"),
        "unexpected stderr: {}",
        stderr
    );
}

#[cfg(unix)]
fn install_fake_wasmtime(script_path: &Path) {
    use std::os::unix::fs::PermissionsExt;

    let script = r#"#!/bin/sh
if [ "$1" = "run" ] && [ -n "$2" ]; then
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

#[cfg(unix)]
fn install_fake_wasmtime_script(script_path: &Path, script: &str) {
    use std::os::unix::fs::PermissionsExt;

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
    ];
    let actual: Vec<String> = stdout.lines().map(|line| line.to_string()).collect();
    assert_eq!(actual, expected);

    let wasm_out = input.with_extension("wasm");
    assert!(
        wasm_out.exists(),
        "expected generated wasm at {:?}",
        wasm_out
    );
}

#[test]
#[cfg(unix)]
fn run_command_executes_dynamic_read_program_via_wasmtime_stdin() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_dynamic_read_via_fake_wasmtime");
    let fake_bin = sandbox.join("bin");
    fs::create_dir_all(&fake_bin).expect("bin directory should be creatable");
    install_fake_wasmtime_script(
        &fake_bin.join("wasmtime"),
        r#"#!/bin/sh
if [ "$1" = "run" ] && [ -n "$2" ]; then
  cat
  exit 0
fi
echo "unexpected args: $*" >&2
exit 1
"#,
    );

    let input = sandbox.join("read_all.gb");
    fs::write(
        &input,
        r#"
main : Unit -> Unit can Print, Read
main =
  print (read())
"#,
    )
    .expect("temporary input should be writable");

    let mut path_entries = vec![fake_bin.clone()];
    if let Some(existing) = env::var_os("PATH") {
        path_entries.extend(env::split_paths(&existing));
    }
    let merged_path = env::join_paths(path_entries).expect("PATH should be joinable");

    let mut child = command_for_goby_cli()
        .arg("run")
        .arg(&input)
        .env("PATH", merged_path)
        .current_dir(&root)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("cli should execute");
    child
        .stdin
        .as_mut()
        .expect("stdin pipe should exist")
        .write_all(b"a\nb")
        .expect("stdin should be writable");
    let output = child
        .wait_with_output()
        .expect("cli output should be readable");

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
    // GeneralLowered programs now execute via the Goby-owned Wasm runtime
    // (run_wasm_bytes_with_stdin), so they do not generate a wasm file or
    // invoke wasmtime externally.
    assert!(
        stdout.contains("a\nb"),
        "expected stdin to be echoed by print(read()), stdout: {}",
        stdout
    );
}

#[test]
#[cfg(unix)]
fn run_command_rejects_mixed_read_line_then_read_shape_with_explicit_boundary_error() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_mixed_read_line_then_read_boundary");
    let input = sandbox.join("mixed_read.gb");
    fs::write(
        &input,
        r#"
main : Unit -> Unit can Print, Read
main =
  first = read_line()
  rest = read()
  print "${first}|${rest}"
"#,
    )
    .expect("temporary input should be writable");

    let mut child = command_for_goby_cli()
        .arg("run")
        .arg(&input)
        .current_dir(&root)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("cli should execute");
    child
        .stdin
        .as_mut()
        .expect("stdin pipe should exist")
        .write_all(b"alpha\nbeta\ngamma")
        .or_else(|err| {
            if err.kind() == std::io::ErrorKind::BrokenPipe {
                Ok(())
            } else {
                Err(err)
            }
        })
        .expect("stdin should be writable or the process should fail early");
    let output = child
        .wait_with_output()
        .expect("cli output should be readable");

    assert!(
        !output.status.success(),
        "expected unsupported-shape failure, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("general lowering unsupported: read-line instructions not supported"),
        "expected explicit unsupported-boundary error, stderr: {}",
        stderr
    );
}

#[test]
#[cfg(unix)]
fn run_command_reports_precise_helper_closure_capture_error() {
    let root = repo_root();
    let input = root.join("examples/bugs/runtime_read_captured_lambda.gb");

    let output = run_goby_with_stdin(&root, &input, b"");

    assert!(
        !output.status.success(),
        "expected closure-capture failure, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("general lowering unsupported: unsupported IR form"),
        "expected precise general-lowering boundary error, stderr: {}",
        stderr
    );
    assert!(
        stderr.contains("Lambda with free variables"),
        "expected closure-capture detail in stderr, got: {}",
        stderr
    );
}

#[test]
#[cfg(unix)]
fn run_command_executes_split_index_runtime_program_via_wasmtime_stdin() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_split_index_runtime_program");
    let input = sandbox.join("split_index.gb");
    fs::write(
        &input,
        r#"
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = split text "\n"
  println(lines[1])
"#,
    )
    .expect("temporary input should be writable");

    let output = run_goby_with_stdin(&root, &input, b"alpha\nbeta\n");

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
    // split + index programs are now GeneralLowered and execute via the Goby-owned
    // Wasm runtime (run_wasm_bytes_with_stdin), so "generated wasm" is not printed.
    assert!(
        stdout.contains("beta\n"),
        "expected second split line to be printed, stdout: {}",
        stdout
    );
}

#[test]
fn run_command_executes_non_fused_split_helper_runtime_program_via_wasmtime_stdin() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_non_fused_split_helper_runtime_program");
    let input = sandbox.join("split_helper.gb");
    fs::write(
        &input,
        r#"
import goby/string

main : Unit -> Unit can Print, Read
main =
  text = read ()
  _lines = string.split text "\n"
  print "ok"
"#,
    )
    .expect("temporary input should be writable");

    let output = run_goby_with_stdin(&root, &input, b"alpha\nbeta\n");

    assert!(
        output.status.success(),
        "expected success, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("ok"),
        "expected static print after helper split to succeed, stdout: {}",
        stdout
    );
}

#[test]
fn run_command_executes_non_fused_split_get_helper_runtime_program_via_wasmtime_stdin() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_non_fused_split_get_helper_runtime_program");
    let input = sandbox.join("split_get_helper.gb");
    fs::write(
        &input,
        r#"
import goby/string
import goby/list

main : Unit -> Unit can Print, Read
main =
  text = read ()
  lines = string.split text "\n"
  line = list.get lines 1
  echoed = line
  println echoed
"#,
    )
    .expect("temporary input should be writable");

    let output = run_goby_with_stdin(&root, &input, b"alpha\nbeta\n");

    assert!(
        output.status.success(),
        "expected success, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("beta\n"),
        "expected second split line via non-fused helpers, stdout: {}",
        stdout
    );
}

#[test]
#[cfg(unix)]
fn run_command_executes_repeated_read_after_exhaustion_shape() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_repeated_reads_boundary");
    let input = sandbox.join("repeated_reads.gb");
    fs::write(
        &input,
        r#"
main : Unit -> Unit can Print, Read
main =
  first = read()
  second = read()
  print "${first}|${second}"
"#,
    )
    .expect("temporary input should be writable");

    let output = run_goby_with_stdin(&root, &input, b"payload");

    assert!(
        output.status.success(),
        "expected repeated-read shape to execute, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("|"),
        "expected repeated-read runtime execution in stdout, stdout: {}",
        stdout
    );
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

#[test]
fn check_command_accepts_case_arm_block_program() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("check_case_arm_block");
    let input = sandbox.join("case_arm_block.gb");
    fs::write(
        &input,
        r#"
main : Unit -> Unit can Print
main =
  x = 0
  print
    case x
      0 ->
        y = "11"
        y
      _ -> "0"
"#,
    )
    .expect("temporary input should be writable");

    let output = command_for_goby_cli()
        .arg("check")
        .arg(&input)
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

#[test]
fn run_command_accepts_named_function_reference_higher_order_program() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_named_function_reference");
    let input = sandbox.join("function_reference.gb");
    fs::write(
        &input,
        r#"
import goby/list ( map )

plus_ten : Int -> Int
plus_ten x = x + 10

main : Unit -> Unit
main =
  map [1, 2] plus_ten |> print
"#,
    )
    .expect("temporary input should be writable");

    let output = command_for_goby_cli()
        .arg("run")
        .arg(&input)
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
        stdout.contains("[11, 12]"),
        "expected named function reference output, stdout: {}",
        stdout
    );
}

#[test]
fn run_command_accepts_case_arm_block_program_without_wasmtime() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_case_arm_block_no_wasmtime");
    let empty_bin = sandbox.join("empty-bin");
    fs::create_dir_all(&empty_bin).expect("empty bin should be creatable");
    let input = sandbox.join("case_arm_block.gb");
    fs::write(
        &input,
        r#"
main : Unit -> Unit can Print
main =
  x = 0
  print
    case x
      0 ->
        y = "11"
        y
      _ -> "0"
"#,
    )
    .expect("temporary input should be writable");

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
    assert!(stdout.contains("parsed and typechecked"));
    assert!(stdout.contains("wasmtime not found; skipped wasm execution"));
}

#[test]
fn run_command_executes_read_program_with_runtime_stdin() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_read_runtime_stdin");
    let input = sandbox.join("read.gb");
    fs::write(
        &input,
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  copied = lines
  forwarded = copied
  each forwarded (|line| -> println "${line}!")
"#,
    )
    .expect("temporary input should be writable");

    let output = run_goby_with_stdin(&root, &input, b"hogehoge\nfugafuga");

    assert!(
        output.status.success(),
        "expected runtime stdin-backed execution to succeed, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("parsed and typechecked"),
        "unexpected stdout: {}",
        stdout
    );
    assert!(
        stdout.contains("hogehoge!\nfugafuga!"),
        "expected runtime stdin output, stdout: {}",
        stdout
    );
}

#[test]
fn run_command_executes_transformed_split_callback_with_empty_runtime_stdin() {
    // The transformed split-callback shape is now GeneralLowered.
    // With empty stdin: read() returns "", and split("", "\n") currently preserves
    // a single empty segment for this path, so the callback prints just "!" once.
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_read_runtime_stdin_empty");
    let input = sandbox.join("read_empty.gb");
    fs::write(
        &input,
        r#"
import goby/list ( each )
import goby/string ( split )

main : Unit -> Unit can Print, Read
main =
  text = read()
  delim = "\n"
  lines = split(text, delim)
  each lines (|line| -> println "${line}!")
"#,
    )
    .expect("temporary input should be writable");

    let output = run_goby_with_stdin(&root, &input, b"");

    assert!(
        output.status.success(),
        "expected dynamic Wasm execution to succeed with empty stdin, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("parsed and typechecked"),
        "unexpected stdout: {}",
        stdout
    );
    assert!(
        stdout.contains("!\n"),
        "empty stdin should preserve one empty segment in transformed output, stdout: {}",
        stdout
    );
}

#[test]
fn run_command_executes_graphemes_program_with_runtime_stdin() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_graphemes_interpreter_bridge");
    let input = sandbox.join("graphemes.gb");
    fs::write(
        &input,
        r#"
import goby/string ( graphemes )

main : Unit -> Unit can Print, Read
main =
  text = read ()
  parts = graphemes text
  println(parts[1])
"#,
    )
    .expect("temporary input should be writable");

    let output = run_goby_with_stdin(&root, &input, "a👨‍👩‍👧‍👦b".as_bytes());

    assert!(
        output.status.success(),
        "expected runtime stdin execution to succeed, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("parsed and typechecked"),
        "unexpected stdout: {}",
        stdout
    );
    assert!(
        stdout.contains("👨‍👩‍👧‍👦"),
        "expected grapheme-aware output, stdout: {}",
        stdout
    );
    assert!(
        !stdout.contains("generated wasm"),
        "runtime-owned execution should not claim Wasm generation, stdout: {}",
        stdout
    );
}

#[test]
fn run_command_does_not_wait_for_eof_on_general_lowered_program_without_read() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_general_lowered_without_read");
    let input = sandbox.join("lambda_no_read.gb");
    fs::write(
        &input,
        r#"
import goby/list ( map, each )

main : Unit -> Unit can Print
main =
  nums = [1, 2, 3]
  rendered = map nums (|n| -> "${n + 1}")
  each rendered println
"#,
    )
    .expect("temporary input should be writable");

    let mut child = command_for_goby_cli()
        .arg("run")
        .arg(&input)
        .current_dir(&root)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("cli should execute");

    let mut exited = false;
    for _ in 0..30 {
        if child.try_wait().expect("try_wait should succeed").is_some() {
            exited = true;
            break;
        }
        thread::sleep(Duration::from_millis(100));
    }

    if !exited {
        let _ = child.kill();
        let _ = child.wait();
        panic!("cli stayed blocked waiting for stdin on a program that does not read it");
    }

    let output = child
        .wait_with_output()
        .expect("cli output should be readable");

    assert!(
        output.status.success(),
        "expected runtime execution to succeed, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("parsed and typechecked"),
        "unexpected stdout: {}",
        stdout
    );
    assert!(
        stdout.contains("2\n3\n4"),
        "expected map/each output without stdin EOF, stdout: {}",
        stdout
    );
}

#[test]
fn check_command_rejects_legacy_syntax_by_default() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("check_legacy_warnings");
    let input = sandbox.join("legacy_effect.gb");
    fs::write(
        &input,
        "effect Log\n  log: String -> Unit\nhandler H for Log\n  log s =\n    print s\nmain : Unit -> Unit\nmain =\n  using H\n    log \"hello\"\n",
    )
    .expect("temporary input should be writable");

    let output = command_for_goby_cli()
        .arg("check")
        .arg(&input)
        .current_dir(&root)
        .output()
        .expect("cli should execute");

    assert!(
        !output.status.success(),
        "expected failure for legacy syntax in default mode"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("legacy top-level `handler ... for ...` is no longer supported"),
        "expected legacy rejection message, got stderr: {}",
        stderr
    );
}

#[test]
#[cfg(unix)]
fn run_command_rejects_legacy_syntax_by_default() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("run_legacy_warnings");
    let fake_bin = sandbox.join("bin");
    fs::create_dir_all(&fake_bin).expect("bin directory should be creatable");
    install_fake_wasmtime(&fake_bin.join("wasmtime"));

    let input = sandbox.join("legacy_effect.gb");
    fs::write(
        &input,
        "effect Log\n  log: String -> Unit\nhandler H for Log\n  log s =\n    print s\nmain : Unit -> Unit\nmain =\n  using H\n    log \"hello\"\n",
    )
    .expect("temporary input should be writable");

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
        !output.status.success(),
        "expected failure for legacy syntax in default mode"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("legacy top-level `handler ... for ...` is no longer supported"),
        "expected legacy rejection message, got stderr: {}",
        stderr
    );
}

#[test]
fn check_command_rejects_legacy_syntax_even_with_legacy_env_var() {
    let root = repo_root();
    let sandbox = TempDirGuard::new("check_legacy_deny");
    let input = sandbox.join("legacy_effect.gb");
    fs::write(
        &input,
        "effect Log\n  log: String -> Unit\nhandler H for Log\n  log s =\n    print s\nmain : Unit -> Unit\nmain =\n  using H\n    log \"hello\"\n",
    )
    .expect("temporary input should be writable");

    let output = command_for_goby_cli()
        .arg("check")
        .arg(&input)
        .env("GOBY_LEGACY_EFFECT_SYNTAX", "deny")
        .current_dir(&root)
        .output()
        .expect("cli should execute");

    assert!(
        !output.status.success(),
        "expected failure in deny mode for legacy syntax"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("legacy top-level `handler ... for ...` is no longer supported"),
        "expected legacy rejection message, got stderr: {}",
        stderr
    );
}

// --- Golden fixture tests (D1d-3) ---
//
// These tests run `goby check` on a fixed repo-relative source file and compare
// stderr byte-for-byte against the expected fixture. The file paths in the fixture
// text are repo-relative (run from repo root), so the `file:` prefix is stable.

#[test]
fn parse_error_output_matches_fixture() {
    let root = repo_root();
    let input = "crates/goby-cli/tests/fixtures/parse_error_input.gb";
    let expected_path = root.join("crates/goby-cli/tests/fixtures/parse_error_expected.txt");
    assert!(
        root.join(input).exists(),
        "fixture input must exist at {}",
        input
    );
    assert!(
        expected_path.exists(),
        "expected fixture must exist at {:?}",
        expected_path
    );
    let expected_raw = fs::read_to_string(&expected_path).expect("fixture should be readable");

    let output = command_for_goby_cli()
        .arg("check")
        .arg(input)
        .current_dir(&root)
        .output()
        .expect("cli should execute");

    assert!(!output.status.success(), "parse error input should fail");
    let actual = String::from_utf8_lossy(&output.stderr);
    // Strip all trailing newlines from both sides for stable comparison.
    assert_eq!(
        actual.trim_end_matches('\n'),
        expected_raw.trim_end_matches('\n'),
        "stderr does not match parse_error_expected.txt"
    );
}

#[test]
fn typecheck_error_output_matches_fixture() {
    let root = repo_root();
    let input = "crates/goby-cli/tests/fixtures/typecheck_error_input.gb";
    let expected_path = root.join("crates/goby-cli/tests/fixtures/typecheck_error_expected.txt");
    assert!(
        root.join(input).exists(),
        "fixture input must exist at {}",
        input
    );
    assert!(
        expected_path.exists(),
        "expected fixture must exist at {:?}",
        expected_path
    );
    let expected_raw = fs::read_to_string(&expected_path).expect("fixture should be readable");

    let output = command_for_goby_cli()
        .arg("check")
        .arg(input)
        .current_dir(&root)
        .output()
        .expect("cli should execute");

    assert!(
        !output.status.success(),
        "typecheck error input should fail"
    );
    let actual = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        actual.trim_end_matches('\n'),
        expected_raw.trim_end_matches('\n'),
        "stderr does not match typecheck_error_expected.txt"
    );
}

#[test]
fn two_typecheck_errors_output_matches_fixture() {
    let root = repo_root();
    let input = "crates/goby-cli/tests/fixtures/two_typecheck_errors_input.gb";
    let expected_path =
        root.join("crates/goby-cli/tests/fixtures/two_typecheck_errors_expected.txt");
    assert!(
        root.join(input).exists(),
        "fixture input must exist at {}",
        input
    );
    assert!(
        expected_path.exists(),
        "expected fixture must exist at {:?}",
        expected_path
    );
    let expected_raw = fs::read_to_string(&expected_path).expect("fixture should be readable");

    let output = command_for_goby_cli()
        .arg("check")
        .arg(input)
        .current_dir(&root)
        .output()
        .expect("cli should execute");

    assert!(!output.status.success(), "two-error input should fail");
    let actual = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        actual.trim_end_matches('\n'),
        expected_raw.trim_end_matches('\n'),
        "stderr does not match two_typecheck_errors_expected.txt"
    );
}

// --- fmt command tests ---

#[test]
fn fmt_check_passes_on_already_formatted_file() {
    // Write the expected fixture (already formatted) to a temp file and verify --check passes.
    let tmpdir = TempDirGuard::new("fmt_check_pass");
    let expected_src = include_str!("fixtures/fmt_expected.gb");
    let tmp_file = tmpdir.join("already_fmt.gb");
    fs::write(&tmp_file, expected_src).expect("should write formatted file");

    let output = command_for_goby_cli()
        .arg("fmt")
        .arg("--check")
        .arg(&tmp_file)
        .output()
        .expect("cli should execute");
    assert!(
        output.status.success(),
        "fmt --check should pass on already-formatted file; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn fmt_check_fails_on_unformatted_file() {
    let fixtures_dir = repo_root().join("crates/goby-cli/tests/fixtures");
    let input_path = fixtures_dir.join("fmt_input.gb");
    assert!(input_path.exists(), "fmt_input.gb fixture must exist");

    let output = command_for_goby_cli()
        .arg("fmt")
        .arg("--check")
        .arg(&input_path)
        .output()
        .expect("cli should execute");
    assert!(
        !output.status.success(),
        "fmt --check should fail on unformatted file"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("not formatted"),
        "stderr should mention 'not formatted': {}",
        stderr
    );
}

#[test]
fn fmt_rewrites_file_in_place() {
    let tmpdir = TempDirGuard::new("fmt_rewrite");
    let input_src = include_str!("fixtures/fmt_input.gb");
    let expected_src = include_str!("fixtures/fmt_expected.gb");

    let tmp_file = tmpdir.join("test.gb");
    fs::write(&tmp_file, input_src).expect("should write input");

    let output = command_for_goby_cli()
        .arg("fmt")
        .arg(&tmp_file)
        .output()
        .expect("cli should execute");
    assert!(
        output.status.success(),
        "fmt should succeed; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let actual = fs::read_to_string(&tmp_file).expect("should read result");
    assert_eq!(
        actual, expected_src,
        "file should be rewritten to formatted form"
    );
}
