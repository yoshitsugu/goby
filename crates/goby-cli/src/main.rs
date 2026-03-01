use std::env;
use std::io::ErrorKind;
use std::path::Path;
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
    goby_core::typecheck_module(&module).map_err(|err| {
        let snippet = err
            .span
            .as_ref()
            .map(|s| format_snippet(&source, s.line, s.col))
            .filter(|s| !s.is_empty())
            .map(|s| format!("\n{}", s))
            .unwrap_or_default();
        CliError::Runtime(format!("{}: {}{}", cli.file, err, snippet))
    })?;

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

fn print_parse_summary(declaration_count: usize, file: &str) {
    println!(
        "parsed and typechecked {} declarations from {}",
        declaration_count, file
    );
}

fn execute_wasm(wasm_path: &str) -> Result<ExecutionOutcome, CliError> {
    let status = match ProcessCommand::new("wasmtime")
        .arg("run")
        .arg("--invoke")
        .arg("main")
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
}
