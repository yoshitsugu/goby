use std::env;
use std::io::ErrorKind;
use std::io::Read as _;
use std::path::{Path, PathBuf};
use std::process::Command as ProcessCommand;

use goby_core::Span;

const USAGE: &str = "\
usage: goby <command> [options] <file.gb>

commands:
  run    <file.gb>           compile and run a Goby program
  check  <file.gb>           parse and typecheck a Goby program
  lint   <file.gb>           parse, typecheck, and run lint checks
  fmt    [--check] <file.gb> format a Goby source file in-place
                             (comments are stripped — Option A policy)
                             with --check: exit 1 if the file is not formatted";

/// Render the error header line.
///
/// With a span: `file:line:col: error: message` (plus optional ` in 'declaration'`).
/// Without a span: `file: error: message` (plus optional ` in 'declaration'`).
fn render_header(
    file: &str,
    severity: goby_core::Severity,
    span: Option<&Span>,
    declaration: Option<&str>,
    message: &str,
) -> String {
    let severity_label = match severity {
        goby_core::Severity::Error => "error",
        goby_core::Severity::Warning => "warning",
    };
    let decl_suffix = match declaration {
        Some(name) => format!(" in '{}'", name),
        None => String::new(),
    };
    match span {
        Some(s) => format!(
            "{}:{}:{}: {}: {}{}",
            file, s.line, s.col, severity_label, message, decl_suffix
        ),
        None => format!("{}: {}: {}{}", file, severity_label, message, decl_suffix),
    }
}

/// Render a source snippet with line-number gutter and caret/underline.
///
/// `span.line` and `span.col` are 1-indexed. Returns empty string when `span.line`
/// is 0 or out of range.
///
/// Underline width:
/// - `end_line == line` and `end_col > col` → width `end_col - col` (range span)
/// - otherwise → width 1 (point span, multiline span, or same-position sentinel)
///
/// Gutter format: right-aligned line number padded to the width of the total line count,
/// followed by ` | `. Example for a 50-line file: `" 2 | source"`.
fn render_snippet(source: &str, span: &Span) -> String {
    let line = span.line;
    let col = span.col;
    if line == 0 {
        return String::new();
    }
    let all_lines: Vec<&str> = source.lines().collect();
    let src_line = match all_lines.get(line - 1) {
        Some(l) => l,
        None => return String::new(),
    };
    let total_lines = all_lines.len();
    let gutter_width = total_lines.to_string().len();
    let underline_width = if span.end_line == line && span.end_col > col {
        span.end_col - col
    } else {
        1
    };
    let caret_spaces = " ".repeat(col.saturating_sub(1));
    let carets = "^".repeat(underline_width);
    let gutter_pad = " ".repeat(gutter_width);
    format!(
        "{:>width$} | {}\n{} | {}{}",
        line,
        src_line,
        gutter_pad,
        caret_spaces,
        carets,
        width = gutter_width
    )
}

/// Render a complete diagnostic: header plus optional snippet.
///
/// When `span` is `None`, only the header is rendered (no snippet block).
fn render_diagnostic(
    file: &str,
    source: &str,
    severity: goby_core::Severity,
    span: Option<&Span>,
    declaration: Option<&str>,
    message: &str,
) -> String {
    let header = render_header(file, severity, span, declaration, message);
    match span {
        None => header,
        Some(s) => {
            let snippet = render_snippet(source, s);
            if snippet.is_empty() {
                header
            } else {
                format!("{}\n{}", header, snippet)
            }
        }
    }
}

/// Convenience wrapper: convert a `Diagnostic` and render it.
fn render_diag(file: &str, source: &str, diag: goby_core::Diagnostic) -> String {
    render_diagnostic(
        file,
        source,
        diag.severity,
        diag.span.as_ref(),
        diag.declaration.as_deref(),
        &diag.message,
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Command {
    Run,
    Check,
    Lint,
    Fmt { check: bool },
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

    // `fmt` only requires parsing — skip typecheck and wasm paths.
    if let Command::Fmt { check } = cli.command {
        return run_fmt(&cli.file, &source, check);
    }

    let stdlib_root = resolve_stdlib_root()?;

    let module = goby_core::parse_module(&source).map_err(|err| {
        // Normalise col==1 sentinel (unknown column) to span: None via Diagnostic.
        CliError::Runtime(render_diag(
            &cli.file,
            &source,
            goby_core::Diagnostic::from(err),
        ))
    })?;

    let typecheck_errors = goby_core::typecheck_module_collect_with_context(
        &module,
        Some(Path::new(&cli.file)),
        Some(stdlib_root.as_path()),
    );
    if !typecheck_errors.is_empty() {
        let rendered: Vec<String> = typecheck_errors
            .into_iter()
            .map(|err| render_diag(&cli.file, &source, goby_core::Diagnostic::from(err)))
            .collect();
        return Err(CliError::Runtime(rendered.join("\n\n")));
    }

    match cli.command {
        Command::Run => run_command(&module, &cli.file)?,
        Command::Check => {
            print_parse_summary(module.declarations.len(), &cli.file);
        }
        Command::Lint => run_lint_command(&module, &cli.file, &source)?,
        Command::Fmt { .. } => unreachable!("handled above"),
    }

    Ok(())
}

/// Implement `goby fmt [--check] <file>`.
fn run_fmt(file: &str, source: &str, check: bool) -> Result<(), CliError> {
    let module = goby_core::parse_module(source).map_err(|err| {
        CliError::Runtime(render_diag(file, source, goby_core::Diagnostic::from(err)))
    })?;
    let formatted = goby_core::format_module(&module);

    if check {
        if source == formatted {
            Ok(())
        } else {
            Err(CliError::Runtime(format!(
                "{}: not formatted (run `goby fmt {}` to fix)",
                file, file
            )))
        }
    } else {
        std::fs::write(file, &formatted)
            .map_err(|err| CliError::Runtime(format!("failed to write {}: {}", file, err)))?;
        Ok(())
    }
}

fn run_command(module: &goby_core::Module, file: &str) -> Result<(), CliError> {
    // Runtime execution ownership lives in `goby-wasm`.
    // Some modules must execute through the Goby-owned runtime even when they do not
    // consume stdin (for example lambda-driven GeneralLowered programs).  Ask the
    // runtime boundary separately whether stdin must be pre-seeded.
    let needs_stdin_execution = matches!(
        goby_wasm::runtime_io_execution_kind(module)
            .map_err(|err| CliError::Runtime(format!("classification error: {}", err.message)))?,
        goby_wasm::RuntimeIoExecutionKind::GeneralLowered
            | goby_wasm::RuntimeIoExecutionKind::InterpreterBridge
    );
    let needs_seeded_stdin = goby_wasm::runtime_execution_needs_stdin(module)
        .map_err(|err| CliError::Runtime(format!("stdin classification error: {}", err.message)))?;

    if needs_stdin_execution {
        let stdin_text = if needs_seeded_stdin {
            Some(read_stdin_to_string()?)
        } else {
            None
        };
        match goby_wasm::execute_runtime_module_with_stdin(module, stdin_text)
            .map_err(|err| CliError::Runtime(format!("runtime error: {}", err.message)))?
        {
            Some(output) => {
                print_parse_summary(module.declarations.len(), file);
                print!("{}", output);
            }
            None => {
                return Err(CliError::Runtime(
                    "runtime-stdin execution produced no output".to_string(),
                ));
            }
        }
    } else {
        // Not a runtime-stdin program: compile and execute via file-based Wasm.
        match goby_wasm::compile_module(module) {
            Ok(bytes) => {
                let output = output_wasm_path(file);
                std::fs::write(&output, &bytes).map_err(|err| {
                    CliError::Runtime(format!("failed to write {}: {}", output, err))
                })?;
                print_parse_summary(module.declarations.len(), file);
                eprintln!("generated wasm: {}", output);
                match execute_wasm(&output)? {
                    ExecutionOutcome::Executed => {}
                    ExecutionOutcome::SkippedNoWasmtime => {
                        eprintln!("wasmtime not found; skipped wasm execution")
                    }
                }
            }
            Err(err) => {
                return Err(CliError::Runtime(format!("codegen error: {}", err.message)));
            }
        }
    }
    Ok(())
}

fn run_lint_command(module: &goby_core::Module, file: &str, source: &str) -> Result<(), CliError> {
    let diagnostics = goby_core::lint_module(module);
    if diagnostics.is_empty() {
        print_parse_summary(module.declarations.len(), file);
        return Ok(());
    }
    let rendered = diagnostics
        .into_iter()
        .map(|diag| render_diag(file, source, diag))
        .collect::<Vec<_>>()
        .join("\n\n");
    Err(CliError::Runtime(rendered))
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

    match command_raw.as_str() {
        "run" | "check" | "lint" => {
            let command = if command_raw == "run" {
                Command::Run
            } else if command_raw == "lint" {
                Command::Lint
            } else {
                Command::Check
            };
            let file = args
                .next()
                .ok_or_else(|| CliError::Usage("missing input file".to_string()))?;
            if let Some(extra) = args.next() {
                return Err(CliError::Usage(format!("unexpected argument: {}", extra)));
            }
            Ok(CliArgs { command, file })
        }
        "fmt" => {
            // Optional --check flag before the file argument.
            let next = args
                .next()
                .ok_or_else(|| CliError::Usage("missing input file".to_string()))?;
            let (check, file) = if next == "--check" {
                let f = args
                    .next()
                    .ok_or_else(|| CliError::Usage("missing input file".to_string()))?;
                (true, f)
            } else {
                (false, next)
            };
            if let Some(extra) = args.next() {
                return Err(CliError::Usage(format!("unexpected argument: {}", extra)));
            }
            Ok(CliArgs {
                command: Command::Fmt { check },
                file,
            })
        }
        _ => Err(CliError::Usage(format!("unknown command: {}", command_raw))),
    }
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
    eprintln!(
        "parsed and typechecked {} declarations from {}",
        declaration_count, file
    );
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

fn read_stdin_to_string() -> Result<String, CliError> {
    let mut bytes = Vec::new();
    std::io::stdin()
        .read_to_end(&mut bytes)
        .map_err(|err| CliError::Runtime(format!("failed to read stdin: {}", err)))?;
    Ok(String::from_utf8_lossy(&bytes).into_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_args<'a>(items: &'a [&'a str]) -> impl Iterator<Item = String> + 'a {
        items.iter().map(|s| s.to_string())
    }

    #[test]
    fn render_snippet_point_span() {
        // 3-line source: gutter_width=1, line 2 col 6 (point span → single ^)
        let source = "line one\nline two\nline three";
        let span = Span::point(2, 6);
        let snippet = render_snippet(source, &span);
        assert_eq!(snippet, "2 | line two\n  |      ^");
    }

    #[test]
    fn render_snippet_range_span_same_line() {
        // Range span on same line: end_col > col → ^^^ of width end_col - col
        let source = "let x = 42";
        let span = Span::new(1, 5, 1, 10); // "x = 4" (5..10)
        let snippet = render_snippet(source, &span);
        assert_eq!(snippet, "1 | let x = 42\n  |     ^^^^^");
    }

    #[test]
    fn render_snippet_unresolved_name_span_underlines_entire_token() {
        let source = "  map [1, 2] print";
        let span = Span::new(1, 3, 1, 6);
        let snippet = render_snippet(source, &span);
        assert_eq!(snippet, "1 |   map [1, 2] print\n  |   ^^^");
    }

    #[test]
    fn render_snippet_multiline_span_falls_back_to_single_caret() {
        // Multiline span: end_line != line → single ^ at start col
        let source = "abc\ndef";
        let span = Span::new(1, 2, 2, 3);
        let snippet = render_snippet(source, &span);
        assert_eq!(snippet, "1 | abc\n  |  ^");
    }

    #[test]
    fn render_snippet_col_past_end() {
        let source = "abc";
        // col 20 is past the 3-byte line — caret falls beyond text, no panic.
        let span = Span::point(1, 20);
        let snippet = render_snippet(source, &span);
        assert_eq!(snippet, "1 | abc\n  |                    ^");
    }

    #[test]
    fn render_snippet_line_out_of_range() {
        let source = "abc";
        // line 5 does not exist → empty string
        let span = Span::point(5, 1);
        let snippet = render_snippet(source, &span);
        assert_eq!(snippet, "");
    }

    #[test]
    fn render_snippet_end_col_less_than_col_defensive_single_caret() {
        // Malformed span where end_col < col: defensively renders a single ^ at col.
        // This cannot happen from well-formed parser output but is exercised as a
        // regression guard for any future span construction code.
        let source = "hello world";
        let span = Span::new(1, 7, 1, 3); // inverted: end_col(3) < col(7)
        let snippet = render_snippet(source, &span);
        // end_col(3) is not > col(7), so falls back to width 1
        assert_eq!(snippet, "1 | hello world\n  |       ^");
    }

    #[test]
    fn render_snippet_sentinel_col1_end_col1_renders_single_caret() {
        // col==1 && end_col==1 is the "unknown position" sentinel for TypecheckErrors.
        // It falls into the default branch (width 1) — single ^ at col 1.
        // NOTE: This is a known false-negative: a genuine point span at column 1 also
        // renders a single ^ here, which is correct behaviour (col 1 is still a valid
        // position; the sentinel heuristic only suppresses the *width*, not the snippet).
        let source = "foo = bar";
        let span = Span::new(1, 1, 1, 1);
        let snippet = render_snippet(source, &span);
        assert_eq!(snippet, "1 | foo = bar\n  | ^");
    }

    #[test]
    fn render_snippet_line_zero_returns_empty() {
        // line=0 is undefined/invalid; render_snippet returns "" rather than silently
        // rendering the first source line.
        let source = "abc";
        let span = Span::point(0, 1);
        let snippet = render_snippet(source, &span);
        assert_eq!(snippet, "");
    }

    #[test]
    fn render_snippet_gutter_width_matches_line_count_digits() {
        // 10-line source: max line number is 10 → gutter_width=2, padded to 2 digits.
        let source = "a\nb\nc\nd\ne\nf\ng\nh\ni\nj";
        let span = Span::point(2, 1);
        let snippet = render_snippet(source, &span);
        assert_eq!(snippet, " 2 | b\n   | ^");
    }

    #[test]
    fn parses_run_args() {
        let cli = parse_args_from(to_args(&["goby", "run", "examples/hello.gb"]))
            .expect("run args should parse");
        assert_eq!(cli.command, Command::Run);
        assert_eq!(cli.file, "examples/hello.gb");
    }

    #[test]
    fn parses_lint_args() {
        let cli = parse_args_from(to_args(&["goby", "lint", "examples/hello.gb"]))
            .expect("lint args should parse");
        assert_eq!(cli.command, Command::Lint);
        assert_eq!(cli.file, "examples/hello.gb");
    }

    #[test]
    fn rejects_unknown_command() {
        let err = parse_args_from(to_args(&["goby", "build", "examples/hello.gb"]))
            .expect_err("unknown command should fail");
        match err {
            CliError::Usage(message) => assert!(message.contains("unknown command")),
            CliError::Runtime(_) => panic!("expected usage error"),
        }
    }

    #[test]
    fn rejects_extra_argument() {
        let err = parse_args_from(to_args(&["goby", "check", "a.gb", "extra"]))
            .expect_err("extra argument should fail");
        match err {
            CliError::Usage(message) => assert!(message.contains("unexpected argument")),
            CliError::Runtime(_) => panic!("expected usage error"),
        }
    }

    #[test]
    fn rejects_missing_command() {
        let err = parse_args_from(to_args(&["goby"])).expect_err("missing command should fail");
        match err {
            CliError::Usage(message) => assert!(message.contains("missing command")),
            CliError::Runtime(_) => panic!("expected usage error"),
        }
    }

    #[test]
    fn rejects_missing_file() {
        let err =
            parse_args_from(to_args(&["goby", "run"])).expect_err("missing input file should fail");
        match err {
            CliError::Usage(message) => assert!(message.contains("missing input file")),
            CliError::Runtime(_) => panic!("expected usage error"),
        }
    }

    #[test]
    fn computes_wasm_output_path() {
        assert_eq!(output_wasm_path("examples/hello.gb"), "examples/hello.wasm");
    }

    // --- render_header tests ---

    #[test]
    fn render_header_span_present_no_declaration() {
        let span = Span::point(3, 5);
        let h = render_header(
            "foo.gb",
            goby_core::Severity::Error,
            Some(&span),
            None,
            "type mismatch",
        );
        assert_eq!(h, "foo.gb:3:5: error: type mismatch");
    }

    #[test]
    fn render_header_span_present_with_declaration() {
        let span = Span::point(3, 5);
        let h = render_header(
            "foo.gb",
            goby_core::Severity::Error,
            Some(&span),
            Some("add"),
            "type mismatch",
        );
        assert_eq!(h, "foo.gb:3:5: error: type mismatch in 'add'");
    }

    #[test]
    fn render_header_span_absent_no_declaration() {
        let h = render_header(
            "foo.gb",
            goby_core::Severity::Error,
            None,
            None,
            "unknown symbol",
        );
        assert_eq!(h, "foo.gb: error: unknown symbol");
    }

    #[test]
    fn render_header_span_absent_with_declaration() {
        let h = render_header(
            "foo.gb",
            goby_core::Severity::Error,
            None,
            Some("main"),
            "body type mismatch",
        );
        assert_eq!(h, "foo.gb: error: body type mismatch in 'main'");
    }

    #[test]
    fn render_header_warning_span_present() {
        let span = Span::point(3, 5);
        let h = render_header(
            "foo.gb",
            goby_core::Severity::Warning,
            Some(&span),
            Some("f"),
            "lint warning",
        );
        assert_eq!(h, "foo.gb:3:5: warning: lint warning in 'f'");
    }

    // --- render_diagnostic smoke tests ---

    #[test]
    fn render_diagnostic_span_absent_returns_header_only() {
        let source = "f = 1\n";
        let result = render_diagnostic(
            "f.gb",
            source,
            goby_core::Severity::Error,
            None,
            Some("f"),
            "type error",
        );
        assert_eq!(result, "f.gb: error: type error in 'f'");
    }

    #[test]
    fn render_diagnostic_span_present_includes_snippet() {
        let source = "f = 1\n";
        let span = Span::point(1, 5);
        let result = render_diagnostic(
            "f.gb",
            source,
            goby_core::Severity::Error,
            Some(&span),
            Some("f"),
            "type error",
        );
        assert!(result.starts_with("f.gb:1:5: error: type error in 'f'\n"));
        assert!(result.contains("| f = 1"));
        assert!(result.contains('^'));
    }
}
