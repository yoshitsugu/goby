//! goby-lsp — Language Server Protocol server for the Goby language.
//!
//! # Usage
//!
//!   goby-lsp
//!
//! The server communicates over stdio using the LSP protocol. Configure your
//! editor to launch `goby-lsp` as a language server for `*.gb` files.
//!
//! # Environment variables
//!
//!   GOBY_STDLIB_ROOT   Path to the Goby stdlib directory (default: bundled path
//!                      relative to the build location; set this for installed
//!                      binaries or non-standard layouts).
//!
//! # Limitations (MVP)
//!
//!   - At most one diagnostic per analysis run (multi-error collection is D2b).
//!   - source_path is not passed to the typechecker; stdlib-relative imports from
//!     user source are unsupported until D2b.
//!   - No re-analysis debounce; analysis runs synchronously on every didChange.
//!     TODO(D2a-debounce): add 200ms idle debounce.
//!   - When GOBY_STDLIB_ROOT is not set and the bundled stdlib path is unavailable,
//!     all diagnostics are replaced by a single "stdlib root not found" error; parse
//!     errors are not reported in that state. Set GOBY_STDLIB_ROOT to restore
//!     full diagnostics.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use lsp_server::{Connection, Message, Notification, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, PublishDiagnostics,
};
use lsp_types::request::Shutdown;
use lsp_types::{
    DiagnosticSeverity, Position, PublishDiagnosticsParams, Range, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};

fn main() {
    eprintln!("goby-lsp starting");

    let (connection, io_threads) = Connection::stdio();

    let server_caps = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::FULL,
        )),
        ..ServerCapabilities::default()
    })
    .expect("server capabilities serialization failed");

    if let Err(e) = connection.initialize(server_caps) {
        eprintln!("goby-lsp: LSP initialize failed: {e}");
        std::process::exit(1);
    }

    run_server(connection);

    if let Err(e) = io_threads.join() {
        eprintln!("goby-lsp: I/O threads failed: {e}");
        std::process::exit(1);
    }
}

fn run_server(connection: Connection) {
    let mut store = DocumentStore::default();
    let stdlib_root = resolve_stdlib_root();

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if req.method == <Shutdown as lsp_types::request::Request>::METHOD {
                    if let Err(e) = connection.handle_shutdown(&req) {
                        eprintln!("goby-lsp: shutdown error: {e}");
                    }
                    return;
                }
                eprintln!("goby-lsp: unhandled request: {}", req.method);
                let resp = Response::new_err(
                    req.id,
                    lsp_server::ErrorCode::MethodNotFound as i32,
                    format!("method not found: {}", req.method),
                );
                connection.sender.send(resp.into()).ok();
            }
            Message::Notification(notif) => {
                handle_notification(&connection, &mut store, stdlib_root.as_deref(), notif);
            }
            Message::Response(_) => {
                eprintln!("goby-lsp: unexpected response from client");
            }
        }
    }
}

fn handle_notification(
    connection: &Connection,
    store: &mut DocumentStore,
    stdlib_root: Option<&Path>,
    notif: Notification,
) {
    match notif.method.as_str() {
        <DidOpenTextDocument as lsp_types::notification::Notification>::METHOD => {
            let params: lsp_types::DidOpenTextDocumentParams =
                match serde_json::from_value(notif.params) {
                    Ok(p) => p,
                    Err(e) => {
                        eprintln!("goby-lsp: malformed didOpen params: {e}");
                        return;
                    }
                };
            let uri = params.text_document.uri;
            let text = params.text_document.text;
            store.open(uri.clone(), text.clone());
            publish_diagnostics(connection, uri, &text, stdlib_root);
        }
        <DidChangeTextDocument as lsp_types::notification::Notification>::METHOD => {
            let params: lsp_types::DidChangeTextDocumentParams =
                match serde_json::from_value(notif.params) {
                    Ok(p) => p,
                    Err(e) => {
                        eprintln!("goby-lsp: malformed didChange params: {e}");
                        return;
                    }
                };
            let uri = params.text_document.uri;
            // FULL sync: each didChange contains the complete document text.
            if let Some(change) = params.content_changes.into_iter().last() {
                store.change(uri.clone(), change.text.clone());
                publish_diagnostics(connection, uri, &change.text, stdlib_root);
            } else {
                eprintln!("goby-lsp: didChange with empty content_changes; diagnostics not updated");
            }
        }
        <DidCloseTextDocument as lsp_types::notification::Notification>::METHOD => {
            let params: lsp_types::DidCloseTextDocumentParams =
                match serde_json::from_value(notif.params) {
                    Ok(p) => p,
                    Err(e) => {
                        eprintln!("goby-lsp: malformed didClose params: {e}");
                        return;
                    }
                };
            let uri = params.text_document.uri;
            store.close(&uri);
            // Publish empty diagnostics to clear editor markers.
            let publish_params = PublishDiagnosticsParams {
                uri,
                diagnostics: vec![],
                version: None,
            };
            let notif = Notification::new(
                <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string(),
                publish_params,
            );
            connection.sender.send(notif.into()).ok();
        }
        method => {
            eprintln!("goby-lsp: unhandled notification: {method}");
        }
    }
}

fn publish_diagnostics(
    connection: &Connection,
    uri: Url,
    source: &str,
    stdlib_root: Option<&Path>,
) {
    let diagnostics = analyze(source, stdlib_root);
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };
    let notif = Notification::new(
        <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string(),
        params,
    );
    connection.sender.send(notif.into()).ok();
}

/// Analyze `source` and return LSP diagnostics.
///
/// At most one diagnostic is returned per run (multi-error collection is D2b).
/// `source_path` is passed as `None` to the typechecker; stdlib-relative imports
/// from user source are unsupported until D2b.
///
/// If `stdlib_root` is `None` (stdlib not found), a synthetic diagnostic is
/// returned rather than panicking.
fn analyze(source: &str, stdlib_root: Option<&Path>) -> Vec<lsp_types::Diagnostic> {
    let Some(stdlib) = stdlib_root else {
        return vec![lsp_types::Diagnostic {
            range: Range {
                start: Position { line: 0, character: 0 },
                end: Position { line: 0, character: 0 },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            message: "goby-lsp: stdlib root not found; set GOBY_STDLIB_ROOT".to_string(),
            ..lsp_types::Diagnostic::default()
        }];
    };

    let module = match goby_core::parse_module(source) {
        Ok(m) => m,
        Err(err) => {
            let diag = goby_core::Diagnostic::from(err);
            return vec![to_lsp_diagnostic(source, &diag)];
        }
    };

    match goby_core::typecheck_module_with_context(&module, None, Some(stdlib)) {
        Ok(()) => vec![],
        Err(err) => {
            let diag = goby_core::Diagnostic::from(err);
            vec![to_lsp_diagnostic(source, &diag)]
        }
    }
}

/// Convert a `goby_core::Diagnostic` to an `lsp_types::Diagnostic`.
///
/// Note: TypecheckError diagnostics with `col = 1` (the "unknown position" sentinel from D1c)
/// produce `character: 0` in the LSP range — the column is approximate, not exact.
/// This is a known limitation documented in `goby-core/src/diagnostic.rs`.
fn to_lsp_diagnostic(source: &str, diag: &goby_core::Diagnostic) -> lsp_types::Diagnostic {
    let range = span_to_lsp_range(source, diag.span.as_ref());
    lsp_types::Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        message: diag.message.clone(),
        ..lsp_types::Diagnostic::default()
    }
}

/// Convert a Goby `Span` (1-indexed, byte-offset columns) to an LSP `Range`
/// (0-indexed, UTF-16 code-unit columns).
///
/// When `span` is `None`, returns `Range { start: 0:0, end: 0:0 }`.
// TODO(D2b): move to a shared goby-lsp-util module if the LSP server grows.
fn span_to_lsp_range(source: &str, span: Option<&goby_core::Span>) -> Range {
    let Some(span) = span else {
        return Range {
            start: Position { line: 0, character: 0 },
            end: Position { line: 0, character: 0 },
        };
    };

    let start = span_point_to_lsp_position(source, span.line, span.col);
    // end_line == 0 is not produced by any current Span constructor (Span::point
    // and Span::new both require valid 1-indexed lines); treat it as "same as start"
    // as a defensive fallback.
    let end = if span.end_line == 0 {
        start
    } else {
        span_point_to_lsp_position(source, span.end_line, span.end_col)
    };
    Range { start, end }
}

/// Convert a 1-indexed (line, col-byte-offset) pair to an LSP `Position`
/// (0-indexed line, UTF-16 character count).
///
/// Uses `split('\n')` to match `goby-core/src/span.rs` line-splitting convention
/// (treats `\r` as part of line text, preserving byte-offset parity on CRLF inputs).
fn span_point_to_lsp_position(source: &str, line: usize, col: usize) -> Position {
    if line == 0 {
        return Position { line: 0, character: 0 };
    }
    let lsp_line = (line - 1) as u32;

    // Use split('\n') instead of lines() to match goby-core's line-splitting
    // convention and avoid CRLF off-by-one in byte-offset columns.
    let src_line = source.split('\n').nth(line - 1).unwrap_or("");

    // col is a 1-indexed byte offset; we want the UTF-16 code-unit count of
    // the first (col - 1) bytes of the line.
    // Floor to the nearest valid char boundary to avoid a panic if a span ever
    // lands mid-codepoint (e.g., from an approximate typechecker position).
    let raw_offset = col.saturating_sub(1).min(src_line.len());
    // Floor to nearest valid char boundary (stable since Rust 1.73).
    let byte_offset = src_line.floor_char_boundary(raw_offset);
    let prefix = &src_line[..byte_offset];
    let character = prefix.chars().map(|c| c.len_utf16() as u32).sum();

    Position { line: lsp_line, character }
}

/// In-memory document store: tracks the current text of open documents.
///
/// In FULL sync mode the document text is always delivered in the notification
/// payload itself, so `get` is only used by tests to verify store state.
#[derive(Default)]
struct DocumentStore {
    docs: HashMap<Url, String>,
}

impl DocumentStore {
    fn open(&mut self, uri: Url, text: String) {
        self.docs.insert(uri, text);
    }

    fn change(&mut self, uri: Url, text: String) {
        self.docs.insert(uri, text);
    }

    fn close(&mut self, uri: &Url) {
        self.docs.remove(uri);
    }

    #[cfg(test)]
    fn get(&self, uri: &Url) -> Option<&str> {
        self.docs.get(uri).map(|s| s.as_str())
    }
}

fn default_stdlib_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join("stdlib")
}

fn resolve_stdlib_root() -> Option<PathBuf> {
    let path = match std::env::var_os("GOBY_STDLIB_ROOT") {
        Some(raw) => PathBuf::from(raw),
        None => default_stdlib_root(),
    };
    if path.is_dir() { Some(path) } else { None }
}

#[cfg(test)]
mod tests {
    use super::*;
    use goby_core::Span;
    use lsp_server::{Connection, Message, Notification as LspNotification, Request as LspRequest};
    use lsp_types::request::Initialize as InitializeRequest;
    use lsp_types::{
        ClientCapabilities, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, InitializeParams, TextDocumentContentChangeEvent,
        TextDocumentIdentifier, TextDocumentItem, VersionedTextDocumentIdentifier, Url,
        notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
        request::Request as _,
    };

    // --- span_to_lsp_range ---

    fn make_span(line: usize, col: usize, end_line: usize, end_col: usize) -> Span {
        Span { line, col, end_line, end_col }
    }

    #[test]
    fn span_to_lsp_range_none_returns_zero() {
        let r = span_to_lsp_range("hello", None);
        assert_eq!(r.start, Position { line: 0, character: 0 });
        assert_eq!(r.end, Position { line: 0, character: 0 });
    }

    #[test]
    fn span_to_lsp_range_ascii_point() {
        // line 1, col 3 → lsp line 0, char 2
        let source = "hello world";
        let span = make_span(1, 3, 1, 3);
        let r = span_to_lsp_range(source, Some(&span));
        assert_eq!(r.start, Position { line: 0, character: 2 });
    }

    #[test]
    fn span_to_lsp_range_ascii_range() {
        // line 2, col 1..5 → lsp line 1, char 0..4
        let source = "abc\ndefg\nhij";
        let span = make_span(2, 1, 2, 5);
        let r = span_to_lsp_range(source, Some(&span));
        assert_eq!(r.start, Position { line: 1, character: 0 });
        assert_eq!(r.end, Position { line: 1, character: 4 });
    }

    #[test]
    fn span_to_lsp_range_multiline() {
        // span crosses lines: start line 1 col 2, end line 2 col 3
        let source = "abc\ndefg";
        let span = make_span(1, 2, 2, 3);
        let r = span_to_lsp_range(source, Some(&span));
        assert_eq!(r.start, Position { line: 0, character: 1 });
        assert_eq!(r.end, Position { line: 1, character: 2 });
    }

    #[test]
    fn span_to_lsp_range_two_byte_utf8() {
        // "é" is U+00E9, 2 UTF-8 bytes, 1 UTF-16 code unit.
        // source: "aé b", prefix bytes [0..3] = "aé" → 2 UTF-16 code units
        let source = "aé b";
        let span = make_span(1, 4, 1, 4); // byte offset 3 = after 'a'(1) + 'é'(2)
        let r = span_to_lsp_range(source, Some(&span));
        assert_eq!(r.start, Position { line: 0, character: 2 });
    }

    #[test]
    fn span_to_lsp_range_three_byte_utf8() {
        // "あ" is U+3042, 3 UTF-8 bytes, 1 UTF-16 code unit.
        // source: "あ!", col 4 (byte 3, after 'あ') → char 1
        let source = "あ!";
        let span = make_span(1, 4, 1, 4);
        let r = span_to_lsp_range(source, Some(&span));
        // prefix bytes [0..3] = "あ" → 1 UTF-16 code unit
        assert_eq!(r.start, Position { line: 0, character: 1 });
    }

    #[test]
    fn span_to_lsp_range_four_byte_utf8_emoji() {
        // "😀" is U+1F600, 4 UTF-8 bytes, 2 UTF-16 code units (surrogate pair).
        // source: "😀x", col 5 (byte 4, after emoji) → char 2
        let source = "😀x";
        let span = make_span(1, 5, 1, 5);
        let r = span_to_lsp_range(source, Some(&span));
        // prefix bytes [0..4] = "😀" → 2 UTF-16 code units
        assert_eq!(r.start, Position { line: 0, character: 2 });
    }

    #[test]
    fn span_to_lsp_range_end_line_zero_collapses_to_start() {
        // end_line == 0 is not produced by current Span constructors but is handled
        // defensively: collapse end to start.
        let source = "abc";
        let span = Span { line: 1, col: 2, end_line: 0, end_col: 0 };
        let r = span_to_lsp_range(source, Some(&span));
        assert_eq!(r.start, r.end, "end should collapse to start when end_line == 0");
    }

    // --- DocumentStore ---

    #[test]
    fn doc_store_open_change_close() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///test.gb").unwrap();

        store.open(uri.clone(), "hello".to_string());
        assert_eq!(store.get(&uri), Some("hello"));

        // change fully replaces (does not append)
        store.change(uri.clone(), "world".to_string());
        assert_eq!(store.get(&uri), Some("world"));

        store.close(&uri);
        assert_eq!(store.get(&uri), None);
    }

    // --- analyze ---

    fn stdlib_root() -> Option<PathBuf> {
        let p = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join("stdlib");
        if p.is_dir() { Some(p) } else { None }
    }

    #[test]
    fn analyze_valid_source_returns_empty() {
        let Some(root) = stdlib_root() else {
            // stdlib not available in this environment; skip
            return;
        };
        let source = "main : Unit -> Unit can Print\nmain = print \"hello\"\n";
        let diags = analyze(source, Some(&root));
        assert!(diags.is_empty(), "expected no diagnostics, got: {:?}", diags);
    }

    #[test]
    fn analyze_parse_error_returns_one_diagnostic() {
        // "= broken" is not a valid top-level definition.
        // Pass stdlib_root explicitly to ensure we test the parse-error path
        // regardless of whether stdlib is available in this environment.
        let Some(root) = stdlib_root() else {
            // Without stdlib the parse error is suppressed; skip this unit test.
            // The integration test did_open_parse_error_publishes_diagnostic also
            // covers the end-to-end path and does not require stdlib.
            // Note: parse errors don't need stdlib, but analyze() returns the
            // synthetic "stdlib not found" error first when root is None.
            // This is documented in the module-level Limitations section.
            return;
        };
        let source = "= broken\n";
        let diags = analyze(source, Some(&root));
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
        // Verify this is a parse error, not the synthetic stdlib-missing error.
        assert!(!diags[0].message.contains("stdlib root not found"), "got stdlib error instead of parse error");
        assert!(!diags[0].message.is_empty());
    }

    #[test]
    fn analyze_typecheck_error_returns_one_diagnostic() {
        // Type mismatch: main returns Int instead of Unit
        let Some(root) = stdlib_root() else {
            return;
        };
        let source = "main : Unit -> Unit\nmain = 42\n";
        let diags = analyze(source, Some(&root));
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
        assert!(!diags[0].message.is_empty());
    }

    #[test]
    fn analyze_no_stdlib_returns_synthetic_diagnostic() {
        let source = "main : Unit -> Unit can Print\nmain = print \"hello\"\n";
        let diags = analyze(source, None);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("stdlib root not found"));
    }

    // --- lifecycle: end-to-end via in-memory connection ---

    fn make_initialize_request() -> LspRequest {
        LspRequest {
            id: lsp_server::RequestId::from(1),
            method: InitializeRequest::METHOD.to_string(),
            params: serde_json::to_value(InitializeParams {
                capabilities: ClientCapabilities::default(),
                ..InitializeParams::default()
            })
            .unwrap(),
        }
    }

    fn make_initialized_notif() -> LspNotification {
        LspNotification {
            method: <lsp_types::notification::Initialized as lsp_types::notification::Notification>::METHOD.to_string(),
            params: serde_json::json!({}),
        }
    }

    fn make_shutdown_request() -> LspRequest {
        LspRequest {
            id: lsp_server::RequestId::from(2),
            method: <lsp_types::request::Shutdown as lsp_types::request::Request>::METHOD
                .to_string(),
            params: serde_json::json!(null),
        }
    }

    fn make_exit_notif() -> LspNotification {
        LspNotification {
            method: <lsp_types::notification::Exit as lsp_types::notification::Notification>::METHOD
                .to_string(),
            params: serde_json::json!(null),
        }
    }

    fn start_server() -> (Connection, std::thread::JoinHandle<()>) {
        let (server_conn, client_conn) = Connection::memory();
        let server_thread = std::thread::spawn(move || {
            // Use the same capabilities as production to keep test fidelity.
            let server_caps = serde_json::to_value(ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                ..ServerCapabilities::default()
            })
            .unwrap();
            server_conn.initialize(server_caps).unwrap();
            run_server(server_conn);
        });
        client_conn.sender.send(make_initialize_request().into()).unwrap();
        let _ = client_conn.receiver.recv().unwrap(); // init response
        client_conn.sender.send(make_initialized_notif().into()).unwrap();
        (client_conn, server_thread)
    }

    fn shutdown_server(client_conn: Connection, server_thread: std::thread::JoinHandle<()>) {
        client_conn.sender.send(make_shutdown_request().into()).unwrap();
        let _ = client_conn.receiver.recv().unwrap(); // shutdown response
        client_conn.sender.send(make_exit_notif().into()).unwrap();
        server_thread.join().unwrap();
    }

    #[test]
    fn lifecycle_initialize_shutdown_exit() {
        let (client_conn, server_thread) = start_server();
        shutdown_server(client_conn, server_thread);
    }

    #[test]
    fn unknown_notification_does_not_panic() {
        let (client_conn, server_thread) = start_server();

        // Unknown notification — no response expected, server must stay alive.
        let unknown = LspNotification {
            method: "unknown/fooBar".to_string(),
            params: serde_json::json!({}),
        };
        client_conn.sender.send(unknown.into()).unwrap();

        shutdown_server(client_conn, server_thread);
    }

    fn recv_publish_diagnostics(client_conn: &Connection) -> PublishDiagnosticsParams {
        let msg = client_conn.receiver.recv().unwrap();
        if let Message::Notification(n) = msg {
            assert_eq!(
                n.method,
                <PublishDiagnostics as lsp_types::notification::Notification>::METHOD
            );
            serde_json::from_value(n.params).unwrap()
        } else {
            panic!("expected publishDiagnostics notification, got: {:?}", msg);
        }
    }

    #[test]
    fn did_open_valid_source_publishes_empty_diagnostics() {
        let Some(_root) = stdlib_root() else {
            return; // stdlib not available in this environment
        };
        let (client_conn, server_thread) = start_server();

        let source = "main : Unit -> Unit can Print\nmain = print \"hi\"\n";
        let open_notif = LspNotification {
            method: <DidOpenTextDocument as lsp_types::notification::Notification>::METHOD
                .to_string(),
            params: serde_json::to_value(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Url::parse("file:///test.gb").unwrap(),
                    language_id: "goby".to_string(),
                    version: 1,
                    text: source.to_string(),
                },
            })
            .unwrap(),
        };
        client_conn.sender.send(open_notif.into()).unwrap();

        let params = recv_publish_diagnostics(&client_conn);
        assert!(params.diagnostics.is_empty());

        shutdown_server(client_conn, server_thread);
    }

    #[test]
    fn did_open_parse_error_publishes_diagnostic() {
        let (client_conn, server_thread) = start_server();

        let open_notif = LspNotification {
            method: <DidOpenTextDocument as lsp_types::notification::Notification>::METHOD
                .to_string(),
            params: serde_json::to_value(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: Url::parse("file:///bad.gb").unwrap(),
                    language_id: "goby".to_string(),
                    version: 1,
                    text: "= broken\n".to_string(),
                },
            })
            .unwrap(),
        };
        client_conn.sender.send(open_notif.into()).unwrap();

        let params = recv_publish_diagnostics(&client_conn);
        assert_eq!(params.diagnostics.len(), 1);
        assert_eq!(params.diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
        assert!(!params.diagnostics[0].message.is_empty());

        shutdown_server(client_conn, server_thread);
    }

    #[test]
    fn did_change_publishes_updated_diagnostics() {
        let (client_conn, server_thread) = start_server();

        let uri = Url::parse("file:///change_test.gb").unwrap();

        // Open with valid source — expect empty diagnostics
        let open_notif = LspNotification {
            method: <DidOpenTextDocument as lsp_types::notification::Notification>::METHOD
                .to_string(),
            params: serde_json::to_value(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "goby".to_string(),
                    version: 1,
                    // Simple source that always parses (no stdlib needed for parse check)
                    text: "add x = x + 1\n".to_string(),
                },
            })
            .unwrap(),
        };
        client_conn.sender.send(open_notif.into()).unwrap();
        let _ = recv_publish_diagnostics(&client_conn); // discard open diagnostics

        // Change to broken source — expect one error diagnostic
        let change_notif = LspNotification {
            method: <DidChangeTextDocument as lsp_types::notification::Notification>::METHOD
                .to_string(),
            params: serde_json::to_value(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: "= broken\n".to_string(),
                }],
            })
            .unwrap(),
        };
        client_conn.sender.send(change_notif.into()).unwrap();

        let params = recv_publish_diagnostics(&client_conn);
        // One diagnostic: parse error (or "stdlib not found" when stdlib is absent).
        // Either way exactly one ERROR is expected.
        assert_eq!(params.diagnostics.len(), 1, "broken source should produce one diagnostic");
        assert_eq!(params.diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));

        shutdown_server(client_conn, server_thread);
    }

    #[test]
    fn did_close_publishes_empty_diagnostics() {
        let (client_conn, server_thread) = start_server();

        let uri = Url::parse("file:///close_test.gb").unwrap();

        // Open a broken document first
        let open_notif = LspNotification {
            method: <DidOpenTextDocument as lsp_types::notification::Notification>::METHOD
                .to_string(),
            params: serde_json::to_value(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "goby".to_string(),
                    version: 1,
                    text: "= broken\n".to_string(),
                },
            })
            .unwrap(),
        };
        client_conn.sender.send(open_notif.into()).unwrap();
        let _ = recv_publish_diagnostics(&client_conn); // discard open diagnostics

        // Close the document — expect empty diagnostics
        let close_notif = LspNotification {
            method: <DidCloseTextDocument as lsp_types::notification::Notification>::METHOD
                .to_string(),
            params: serde_json::to_value(DidCloseTextDocumentParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
            })
            .unwrap(),
        };
        client_conn.sender.send(close_notif.into()).unwrap();

        let params = recv_publish_diagnostics(&client_conn);
        assert!(params.diagnostics.is_empty(), "expected empty on close");

        shutdown_server(client_conn, server_thread);
    }
}
