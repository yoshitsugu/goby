use std::env;

fn main() {
    let mut args = env::args();
    let _program = args.next();

    let first = match args.next() {
        Some(arg) => arg,
        None => {
            eprintln!("usage: goby-cli run <file.gb>");
            std::process::exit(2);
        }
    };

    let file = if first == "run" {
        match args.next() {
            Some(path) => path,
            None => {
                eprintln!("usage: goby-cli run <file.gb>");
                std::process::exit(2);
            }
        }
    } else {
        // Temporary compatibility path for early MVP development.
        first
    };

    let source = match std::fs::read_to_string(&file) {
        Ok(s) => s,
        Err(err) => {
            eprintln!("failed to read {}: {}", file, err);
            std::process::exit(1);
        }
    };

    match goby_core::parse_module(&source) {
        Ok(module) => match goby_core::typecheck_module(&module) {
            Ok(()) => {
                println!(
                    "parsed and typechecked {} declarations from {}",
                    module.declarations.len(),
                    file
                );
            }
            Err(err) => {
                let target = err.declaration.as_deref().unwrap_or("<module>");
                eprintln!("typecheck error in {}: {}", target, err.message);
                std::process::exit(1);
            }
        },
        Err(err) => {
            eprintln!("parse error at line {}: {}", err.line, err.message);
            std::process::exit(1);
        }
    }
}
