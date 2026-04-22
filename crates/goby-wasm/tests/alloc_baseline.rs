use std::path::PathBuf;

use goby_core::parse_module;
use goby_wasm::{
    CompileOptions, RuntimeIoExecutionKind,
    execute_runtime_module_with_stdin_config_and_options_captured, runtime_io_execution_kind,
};

fn read_example(name: &str) -> String {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("..");
    path.push("..");
    path.push("examples");
    path.push(name);
    std::fs::read_to_string(path).expect("example file should exist")
}

fn parse_stats_total(stderr: &str) -> u64 {
    let line = stderr
        .lines()
        .find(|line| line.starts_with("alloc-stats:"))
        .unwrap_or_else(|| panic!("missing alloc-stats line in stderr:\n{stderr}"));
    line.split_whitespace()
        .find_map(|part| {
            part.strip_prefix("total_bytes=")
                .and_then(|value| value.parse::<u64>().ok())
        })
        .unwrap_or_else(|| panic!("missing total_bytes field in stats line: {line}"))
}

fn baseline_entries() -> Vec<(&'static str, u64)> {
    include_str!("alloc_baseline.txt")
        .lines()
        .filter_map(|line| {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                return None;
            }
            let mut parts = line.split_whitespace();
            let example = parts.next().expect("baseline row must have example name");
            let max_total = parts
                .next()
                .expect("baseline row must have max_total_bytes")
                .parse::<u64>()
                .expect("max_total_bytes must be an integer");
            assert!(
                parts.next().is_none(),
                "baseline row must have exactly two columns: {line}"
            );
            Some((example, max_total))
        })
        .collect()
}

#[test]
fn alloc_baseline() {
    let mut observed = Vec::new();
    for (example, _max_total) in baseline_entries() {
        let source = read_example(example);
        let module = parse_module(&source).expect("baseline example should parse");
        assert_eq!(
            runtime_io_execution_kind(&module).expect("execution kind should classify"),
            RuntimeIoExecutionKind::GeneralLowered,
            "{example} must stay on the GeneralLowered path for alloc stats"
        );
        let output = execute_runtime_module_with_stdin_config_and_options_captured(
            &module,
            Some(String::new()),
            None,
            CompileOptions {
                debug_alloc_stats: true,
            },
        )
        .expect("baseline example should execute")
        .expect("baseline example should use runtime-owned Wasm");
        let total = parse_stats_total(&output.stderr);
        observed.push((example, total));

        #[cfg(not(feature = "alloc_baseline_record"))]
        assert!(
            total <= _max_total,
            "{example} allocated {total} bytes, above baseline budget {_max_total}; stderr:\n{}",
            output.stderr
        );
    }

    #[cfg(feature = "alloc_baseline_record")]
    {
        for (example, total) in observed {
            let budget = total.saturating_mul(11).div_ceil(10);
            println!("{example} {budget}");
        }
    }
}
