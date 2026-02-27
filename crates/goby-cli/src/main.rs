use std::env;
use std::path::Path;
use std::process::Command as ProcessCommand;

const USAGE: &str = "usage: goby-cli <run|check> <file.gb>";

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
        CliError::Runtime(format!("parse error at line {}: {}", err.line, err.message))
    })?;

    goby_core::typecheck_module(&module).map_err(|err| {
        let target = err.declaration.as_deref().unwrap_or("<module>");
        CliError::Runtime(format!("typecheck error in {}: {}", target, err.message))
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
            maybe_execute_wasm(&output)?;
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

fn maybe_execute_wasm(wasm_path: &str) -> Result<(), CliError> {
    let version_check = ProcessCommand::new("wasmtime").arg("--version").output();
    if version_check.is_err() {
        println!("wasmtime not found; skipped wasm execution");
        return Ok(());
    }

    let output = ProcessCommand::new("wasmtime")
        .arg("run")
        .arg("--invoke")
        .arg("main")
        .arg(wasm_path)
        .output()
        .map_err(|err| CliError::Runtime(format!("failed to run wasmtime: {}", err)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(CliError::Runtime(format!(
            "wasmtime execution failed: {}",
            stderr.trim()
        )));
    }

    println!("executed wasm via wasmtime");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn to_args<'a>(items: &'a [&'a str]) -> impl Iterator<Item = String> + 'a {
        items.iter().map(|s| s.to_string())
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
    fn computes_wasm_output_path() {
        assert_eq!(output_wasm_path("examples/hello.gb"), "examples/hello.wasm");
    }
}
