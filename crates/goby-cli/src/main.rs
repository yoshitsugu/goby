use goby_core::{Module, Stmt};
use std::env;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::process::Command as ProcessCommand;

const USAGE: &str = "usage: goby-cli <run|check> <file.gb>";

/// Returns a two-line snippet:
///   "  {source_line}"
///   "  {spaces}^"
///
/// `line` and `col` are 1-indexed. `col = 1` means "start of line" and is also
/// used as an "unknown column" sentinel by `TypecheckError`; the caret will land
/// on the first byte, which is the best available position when column is unknown.
/// Returns an empty string when `line` is out of range.
fn format_snippet(source: &str, line: usize, col: usize) -> String {
    let src_line = match source.lines().nth(line.saturating_sub(1)) {
        Some(l) => l,
        None => return String::new(),
    };
    let spaces = " ".repeat(col.saturating_sub(1));
    format!("  {}\n  {}^", src_line, spaces)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Command {
    Run,
    Check,
}

#[derive(Debug)]
struct CliArgs {
    command: Command,
    file: String,
}

#[derive(Debug)]
enum CliError {
    Usage(String),
    Runtime(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LegacySyntaxMode {
    Warn,
    Deny,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExecutionOutcome {
    Executed,
    SkippedNoWasmtime,
}

fn main() {
    match run() {
        Ok(()) => {}
        Err(CliError::Usage(message)) => {
            eprintln!("{}", message);
            eprintln!("{}", USAGE);
            std::process::exit(2);
        }
        Err(CliError::Runtime(message)) => {
            eprintln!("{}", message);
            std::process::exit(1);
        }
    }
}

fn run() -> Result<(), CliError> {
    let cli = parse_args()?;
    let stdlib_root = resolve_stdlib_root()?;
    let source = std::fs::read_to_string(&cli.file)
        .map_err(|err| CliError::Runtime(format!("failed to read {}: {}", cli.file, err)))?;

    let module = goby_core::parse_module(&source).map_err(|err| {
        let snippet = format_snippet(&source, err.line, err.col);
        let suffix = if snippet.is_empty() {
            String::new()
        } else {
            format!("\n{}", snippet)
        };
        CliError::Runtime(format!(
            "{}:{}:{}: parse error: {}{}",
            cli.file, err.line, err.col, err.message, suffix
        ))
    })?;

    // TODO: unify parse/typecheck error format ("file:line:col: msg" GCC-style vs
    // "file: typecheck error in X at line Y:Z: msg" prose-style); deferred post-MVP.
    goby_core::typecheck_module_with_context(
        &module,
        Some(Path::new(&cli.file)),
        Some(stdlib_root.as_path()),
    )
    .map_err(|err| {
        let snippet = err
            .span
            .as_ref()
            .map(|s| format_snippet(&source, s.line, s.col))
            .filter(|s| !s.is_empty())
            .map(|s| format!("\n{}", s))
            .unwrap_or_default();
        CliError::Runtime(format!("{}: {}{}", cli.file, err, snippet))
    })?;

    let legacy_usage = analyze_legacy_syntax_usage(&module);
    match resolve_legacy_syntax_mode() {
        LegacySyntaxMode::Warn => {
            print_legacy_syntax_warnings(&legacy_usage);
        }
        LegacySyntaxMode::Deny => {
            if legacy_usage.has_any() {
                return Err(CliError::Runtime(format!(
                    "legacy effect syntax is denied by GOBY_LEGACY_EFFECT_SYNTAX=deny (handler_for={}, using={}); migrate to `handler` + `with`/`with_handler` (see doc/EFFECT_RENEWAL_MIGRATION.md)",
                    legacy_usage.handler_for_count, legacy_usage.using_count
                )));
            }
        }
    }

    match cli.command {
        Command::Run => {
            let bytes = goby_wasm::compile_module(&module)
                .map_err(|err| CliError::Runtime(format!("codegen error: {}", err.message)))?;
            let output = output_wasm_path(&cli.file);
            std::fs::write(&output, bytes)
                .map_err(|err| CliError::Runtime(format!("failed to write {}: {}", output, err)))?;

            print_parse_summary(module.declarations.len(), &cli.file);
            println!("generated wasm: {}", output);
            match execute_wasm(&output)? {
                ExecutionOutcome::Executed => println!("executed wasm via wasmtime"),
                ExecutionOutcome::SkippedNoWasmtime => {
                    println!("wasmtime not found; skipped wasm execution")
                }
            }
        }
        Command::Check => {
            print_parse_summary(module.declarations.len(), &cli.file);
        }
    }

    Ok(())
}

fn parse_args() -> Result<CliArgs, CliError> {
    parse_args_from(env::args())
}

fn parse_args_from<I>(mut args: I) -> Result<CliArgs, CliError>
where
    I: Iterator<Item = String>,
{
    let _program = args.next();

    let command_raw = args
        .next()
        .ok_or_else(|| CliError::Usage("missing command".to_string()))?;

    let command = match command_raw.as_str() {
        "run" => Command::Run,
        "check" => Command::Check,
        _ => return Err(CliError::Usage(format!("unknown command: {}", command_raw))),
    };

    let file = args
        .next()
        .ok_or_else(|| CliError::Usage("missing input file".to_string()))?;

    if let Some(extra) = args.next() {
        return Err(CliError::Usage(format!("unexpected argument: {}", extra)));
    }

    Ok(CliArgs { command, file })
}

fn output_wasm_path(input: &str) -> String {
    let input_path = Path::new(input);
    input_path.with_extension("wasm").display().to_string()
}

fn default_stdlib_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join("stdlib")
}

fn resolve_stdlib_root() -> Result<PathBuf, CliError> {
    let path = match env::var_os("GOBY_STDLIB_ROOT") {
        Some(raw) => PathBuf::from(raw),
        None => default_stdlib_root(),
    };
    if !path.exists() {
        return Err(CliError::Runtime(format!(
            "stdlib root does not exist: {}",
            path.display()
        )));
    }
    if !path.is_dir() {
        return Err(CliError::Runtime(format!(
            "stdlib root is not a directory: {}",
            path.display()
        )));
    }
    Ok(path)
}

fn print_parse_summary(declaration_count: usize, file: &str) {
    println!(
        "parsed and typechecked {} declarations from {}",
        declaration_count, file
    );
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LegacySyntaxUsage {
    handler_for_count: usize,
    using_count: usize,
}

impl LegacySyntaxUsage {
    fn has_any(self) -> bool {
        self.handler_for_count > 0 || self.using_count > 0
    }
}

fn resolve_legacy_syntax_mode() -> LegacySyntaxMode {
    match env::var("GOBY_LEGACY_EFFECT_SYNTAX")
        .ok()
        .as_deref()
        .map(str::trim)
    {
        Some("deny") => LegacySyntaxMode::Deny,
        _ => LegacySyntaxMode::Warn,
    }
}

fn print_legacy_syntax_warnings(usage: &LegacySyntaxUsage) {
    if usage.handler_for_count > 0 {
        println!(
            "warning: legacy syntax `handler ... for ...` is temporary compatibility syntax ({} use(s)); migrate to `handler` + `with`/`with_handler` (see doc/EFFECT_RENEWAL_MIGRATION.md)",
            usage.handler_for_count
        );
    }
    if usage.using_count > 0 {
        println!(
            "warning: legacy syntax `using` is temporary compatibility syntax ({} use(s)); migrate to `with`/`with_handler` (see doc/EFFECT_RENEWAL_MIGRATION.md)",
            usage.using_count
        );
    }
}

fn analyze_legacy_syntax_usage(module: &Module) -> LegacySyntaxUsage {
    let mut count = 0usize;
    for decl in &module.declarations {
        if let Some(stmts) = decl.parsed_body.as_deref() {
            count += count_using_stmts(stmts);
        }
    }
    for handler in &module.handler_declarations {
        for method in &handler.methods {
            if let Some(stmts) = method.parsed_body.as_deref() {
                count += count_using_stmts(stmts);
            }
        }
    }
    LegacySyntaxUsage {
        handler_for_count: module.handler_declarations.len(),
        using_count: count,
    }
}

fn count_using_stmts(stmts: &[Stmt]) -> usize {
    let mut count = 0usize;
    for stmt in stmts {
        if let Stmt::Using { body, .. } = stmt {
            count += 1;
            count += count_using_stmts(body);
        }
    }
    count
}

fn execute_wasm(wasm_path: &str) -> Result<ExecutionOutcome, CliError> {
    let status = match ProcessCommand::new("wasmtime")
        .arg("run")
        .arg(wasm_path)
        .status()
    {
        Ok(status) => status,
        Err(err) if err.kind() == ErrorKind::NotFound => {
            return Ok(ExecutionOutcome::SkippedNoWasmtime);
        }
        Err(err) => {
            return Err(CliError::Runtime(format!(
                "failed to run wasmtime: {}",
                err
            )));
        }
    };

    if !status.success() {
        let exit_desc = match status.code() {
            Some(code) => format!("exit code {}", code),
            None => "terminated by signal".to_string(),
        };
        return Err(CliError::Runtime(format!(
            "wasmtime execution failed ({})",
            exit_desc
        )));
    }

    Ok(ExecutionOutcome::Executed)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_args<'a>(items: &'a [&'a str]) -> impl Iterator<Item = String> + 'a {
        items.iter().map(|s| s.to_string())
    }

    #[test]
    fn format_snippet_normal() {
        let source = "line one\nline two\nline three";
        // line 2, col 6 → caret under 't' of "two"
        let snippet = format_snippet(source, 2, 6);
        assert_eq!(snippet, "  line two\n       ^");
    }

    #[test]
    fn format_snippet_col_past_end() {
        let source = "abc";
        // col 20 is past the 3-byte line — caret falls beyond text, no panic.
        // indent "  " + 19 spaces (col-1) + "^"
        let snippet = format_snippet(source, 1, 20);
        assert_eq!(snippet, "  abc\n                     ^");
    }

    #[test]
    fn format_snippet_line_out_of_range() {
        let source = "abc";
        // line 5 does not exist → empty string
        let snippet = format_snippet(source, 5, 1);
        assert_eq!(snippet, "");
    }

    #[test]
    fn format_snippet_line_zero() {
        let source = "abc";
        // line 0 → saturating_sub(1) = 0 → nth(0) = "abc" (first line)
        // This is a fallback for misbehaving callers; we just verify no panic.
        let snippet = format_snippet(source, 0, 1);
        assert_eq!(snippet, "  abc\n  ^");
    }

    #[test]
    fn parses_run_args() {
        let cli = parse_args_from(to_args(&["goby-cli", "run", "examples/hello.gb"]))
            .expect("run args should parse");
        assert_eq!(cli.command, Command::Run);
        assert_eq!(cli.file, "examples/hello.gb");
    }

    #[test]
    fn rejects_unknown_command() {
        let err = parse_args_from(to_args(&["goby-cli", "build", "examples/hello.gb"]))
            .expect_err("unknown command should fail");
        match err {
            CliError::Usage(message) => assert!(message.contains("unknown command")),
            CliError::Runtime(_) => panic!("expected usage error"),
        }
    }

    #[test]
    fn rejects_extra_argument() {
        let err = parse_args_from(to_args(&["goby-cli", "check", "a.gb", "extra"]))
            .expect_err("extra argument should fail");
        match err {
            CliError::Usage(message) => assert!(message.contains("unexpected argument")),
            CliError::Runtime(_) => panic!("expected usage error"),
        }
    }

    #[test]
    fn rejects_missing_command() {
        let err = parse_args_from(to_args(&["goby-cli"])).expect_err("missing command should fail");
        match err {
            CliError::Usage(message) => assert!(message.contains("missing command")),
            CliError::Runtime(_) => panic!("expected usage error"),
        }
    }

    #[test]
    fn rejects_missing_file() {
        let err = parse_args_from(to_args(&["goby-cli", "run"]))
            .expect_err("missing input file should fail");
        match err {
            CliError::Usage(message) => assert!(message.contains("missing input file")),
            CliError::Runtime(_) => panic!("expected usage error"),
        }
    }

    #[test]
    fn computes_wasm_output_path() {
        assert_eq!(output_wasm_path("examples/hello.gb"), "examples/hello.wasm");
    }

    #[test]
    fn counts_nested_using_statements_in_module() {
        let source = "\
effect Log
  log: String -> Unit
handler H for Log
  log x =
    print x
main : Unit -> Unit
main =
  using H
    using H
      log \"x\"
";
        let module = goby_core::parse_module(source).expect("source should parse");
        let usage = analyze_legacy_syntax_usage(&module);
        assert_eq!(usage.using_count, 2);
        assert_eq!(usage.handler_for_count, 1);
    }

    #[test]
    fn resolves_legacy_syntax_mode_from_env() {
        // SAFETY: test process controls env access in this test.
        unsafe { env::set_var("GOBY_LEGACY_EFFECT_SYNTAX", "deny") };
        assert_eq!(resolve_legacy_syntax_mode(), LegacySyntaxMode::Deny);
        // SAFETY: cleanup after temporary test env override.
        unsafe { env::remove_var("GOBY_LEGACY_EFFECT_SYNTAX") };
        assert_eq!(resolve_legacy_syntax_mode(), LegacySyntaxMode::Warn);
    }
}
