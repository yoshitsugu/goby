use goby_core::Module;

use crate::lower;
use crate::resolve_module_runtime_output_with_mode;

fn runtime_output_for_mode(module: &Module, mode: lower::EffectExecutionMode) -> Option<String> {
    resolve_module_runtime_output_with_mode(module, mode)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ParityOutcome {
    pub(crate) stdout: Option<String>,
    pub(crate) runtime_error_kind: Option<&'static str>,
}

fn parity_outcome_for_mode(module: &Module, mode: lower::EffectExecutionMode) -> ParityOutcome {
    parity_outcome_from_runtime_output(runtime_output_for_mode(module, mode))
}

fn parity_outcome_from_runtime_output(output: Option<String>) -> ParityOutcome {
    let Some(text) = output else {
        return ParityOutcome {
            stdout: None,
            runtime_error_kind: None,
        };
    };
    let mut lines = text.lines().map(str::to_string).collect::<Vec<_>>();
    if let Some(last) = lines.last()
        && let Some(kind) = runtime_error_kind_from_output_line(last)
    {
        lines.pop();
        let stdout = if lines.is_empty() {
            None
        } else {
            Some(lines.join("\n"))
        };
        return ParityOutcome {
            stdout,
            runtime_error_kind: Some(kind),
        };
    }
    ParityOutcome {
        stdout: Some(text),
        runtime_error_kind: None,
    }
}

fn runtime_error_kind_from_output_line(line: &str) -> Option<&'static str> {
    let msg = line.strip_prefix("runtime error: ")?;
    if msg.contains("[E-RESUME-MISSING]")
        || msg.starts_with("resume used without an active continuation")
    {
        return Some("continuation_missing");
    }
    if msg.contains("[E-RESUME-CONSUMED]")
        || msg.starts_with("resume continuation already consumed")
    {
        return Some("continuation_consumed");
    }
    if msg.contains("[E-RESUME-HANDLER-MISMATCH]")
        || msg.starts_with("internal resume token handler mismatch")
    {
        return Some("token_handler_mismatch");
    }
    if msg.contains("[E-RESUME-STACK-MISMATCH]")
        || msg.starts_with("internal resume token stack mismatch")
    {
        return Some("token_stack_mismatch");
    }
    Some("unknown_runtime_error")
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PerfStats {
    pub(crate) p50_micros: u128,
    pub(crate) p95_micros: u128,
}

pub(crate) fn measure_runtime_mode_micros(
    module: &Module,
    mode: lower::EffectExecutionMode,
    warmup_runs: usize,
    measured_runs: usize,
) -> PerfStats {
    for _ in 0..warmup_runs {
        let _ = runtime_output_for_mode(module, mode);
    }
    let mut samples = Vec::with_capacity(measured_runs);
    for _ in 0..measured_runs {
        let start = std::time::Instant::now();
        let _ = runtime_output_for_mode(module, mode);
        samples.push(start.elapsed().as_micros());
    }
    samples.sort_unstable();
    PerfStats {
        p50_micros: percentile_micros(&samples, 50),
        p95_micros: percentile_micros(&samples, 95),
    }
}

fn percentile_micros(sorted_samples: &[u128], percentile: usize) -> u128 {
    assert!(!sorted_samples.is_empty(), "samples must not be empty");
    assert!(percentile <= 100, "percentile out of range");
    let n = sorted_samples.len();
    let rank = ((n - 1) * percentile) / 100;
    sorted_samples[rank]
}

pub(crate) fn assert_perf_within_threshold(
    sample_name: &str,
    fallback: PerfStats,
    typed: PerfStats,
    max_slowdown_ratio: f64,
) {
    let p50_ratio = if fallback.p50_micros == 0 {
        1.0
    } else {
        typed.p50_micros as f64 / fallback.p50_micros as f64
    };
    let p95_ratio = if fallback.p95_micros == 0 {
        1.0
    } else {
        typed.p95_micros as f64 / fallback.p95_micros as f64
    };
    assert!(
        p50_ratio <= max_slowdown_ratio,
        "sample `{}` exceeded p50 slowdown threshold: fallback={}us typed={}us ratio={:.4} limit={:.4}",
        sample_name,
        fallback.p50_micros,
        typed.p50_micros,
        p50_ratio,
        max_slowdown_ratio
    );
    assert!(
        p95_ratio <= max_slowdown_ratio,
        "sample `{}` exceeded p95 slowdown threshold: fallback={}us typed={}us ratio={:.4} limit={:.4}",
        sample_name,
        fallback.p95_micros,
        typed.p95_micros,
        p95_ratio,
        max_slowdown_ratio
    );
}

pub(crate) fn assert_mode_parity(module: &Module, context: &str) -> ParityOutcome {
    let fallback = parity_outcome_for_mode(module, lower::EffectExecutionMode::PortableFallback);
    let typed = parity_outcome_for_mode(
        module,
        lower::EffectExecutionMode::TypedContinuationOptimized,
    );
    assert_ne!(
        fallback.runtime_error_kind,
        Some("unknown_runtime_error"),
        "fallback produced unmapped runtime error kind in {}",
        context
    );
    assert_ne!(
        typed.runtime_error_kind,
        Some("unknown_runtime_error"),
        "typed mode produced unmapped runtime error kind in {}",
        context
    );
    assert_eq!(typed, fallback, "mode parity mismatch in {}", context);
    typed
}
