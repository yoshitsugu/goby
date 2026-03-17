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
//!   - source_path is not passed to the typechecker; stdlib-relative imports from
//!     user source are not resolved.
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
use lsp_types::request::{GotoDefinition, HoverRequest, Shutdown};
use lsp_types::{
    DiagnosticSeverity, GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability,
    Location, MarkupContent, MarkupKind, OneOf, Position, PublishDiagnosticsParams, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};

fn main() {
    eprintln!("goby-lsp starting");

    let (connection, io_threads) = Connection::stdio();

    let server_caps = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::FULL,
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
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
                let resp = handle_request(&store, stdlib_root.as_deref(), req);
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

/// Deserialize request params, returning an `InvalidParams` error response on failure.
fn parse_request_params<T, F>(
    id: &lsp_server::RequestId,
    params: serde_json::Value,
    label: &str,
    parse: F,
) -> Result<T, Response>
where
    F: FnOnce(serde_json::Value) -> Result<T, serde_json::Error>,
{
    parse(params).map_err(|e| {
        Response::new_err(
            id.clone(),
            lsp_server::ErrorCode::InvalidParams as i32,
            format!("invalid {label} params: {e}"),
        )
    })
}

/// Dispatch an incoming LSP request to the appropriate handler.
fn handle_request(
    store: &DocumentStore,
    stdlib_root: Option<&Path>,
    req: lsp_server::Request,
) -> Response {
    if req.method == <HoverRequest as lsp_types::request::Request>::METHOD {
        let params: lsp_types::HoverParams =
            match parse_request_params(&req.id, req.params, "hover", serde_json::from_value) {
                Ok(p) => p,
                Err(e) => return e,
            };
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let source = store.docs.get(uri).map(|s| s.as_str()).unwrap_or("");
        let result = hover_at(source, stdlib_root, pos);
        let value = serde_json::to_value(result).unwrap_or(serde_json::Value::Null);
        return Response::new_ok(req.id, value);
    }

    if req.method == <GotoDefinition as lsp_types::request::Request>::METHOD {
        let params: lsp_types::GotoDefinitionParams =
            match parse_request_params(&req.id, req.params, "definition", serde_json::from_value) {
                Ok(p) => p,
                Err(e) => return e,
            };
        let uri = params.text_document_position_params.text_document.uri.clone();
        let pos = params.text_document_position_params.position;
        let source = store.docs.get(&uri).map(|s| s.as_str()).unwrap_or("");
        let result = definition_at(source, &uri, stdlib_root, pos);
        let value = serde_json::to_value(result).unwrap_or(serde_json::Value::Null);
        return Response::new_ok(req.id, value);
    }

    eprintln!("goby-lsp: unhandled request: {}", req.method);
    Response::new_err(
        req.id,
        lsp_server::ErrorCode::MethodNotFound as i32,
        format!("method not found: {}", req.method),
    )
}

/// Return the hover result for the given cursor position, or `None` for no-hover.
fn hover_at(source: &str, stdlib_root: Option<&Path>, pos: Position) -> Option<Hover> {
    let (index, module) = build_index(source, stdlib_root)?;
    let word = word_at_position(source, pos)?;

    // 1. Try local bindings first.
    //
    // Check local bindings before top-level symbols so that a local binding
    // whose name shadows a top-level declaration shows the inferred local type
    // instead of the top-level annotation when the cursor is on the definition line.
    //
    // LSP pos.line is 0-indexed; source lines are 1-indexed.
    // LSP pos.character is 0-indexed UTF-16; Span::col is 1-indexed byte offset.
    // For ASCII identifiers UTF-16 == byte, so: cursor_col_1indexed = pos.character + 1.
    let cursor_source_line = pos.line as usize + 1;
    let cursor_col_1indexed = pos.character as usize + 1;
    for decl in &module.declarations {
        let def_line = goby_core::def_line_of(decl);
        let bindings = goby_core::infer_local_bindings(decl);
        for sym in &bindings {
            if sym.name != word {
                continue;
            }
            // body-relative line → source file line:
            // body string starts with a leading '\n', so body line N maps to
            // source file line: def_line + body_relative_line - 1
            let source_line = def_line + sym.body_relative_line - 1;
            if source_line != cursor_source_line {
                continue;
            }
            // Column check: cursor must be within the LHS identifier range.
            // This prevents false positives when the same name appears on the RHS
            // of the same line (e.g. `x = x + 1` — RHS `x` should return None).
            let sym_col = sym.body_relative_col;
            // sym.name.len() is byte length; safe because Goby identifiers are ASCII-only.
            if cursor_col_1indexed >= sym_col && cursor_col_1indexed < sym_col + sym.name.len() {
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::PlainText,
                        value: format!("{} : {}", sym.name, sym.ty_str),
                    }),
                    range: None,
                });
            }
        }
    }

    // 2. Fall back to top-level symbols and effect members.
    let sym_info = index.lookup(&word)?;
    let text = match sym_info {
        goby_core::SymbolInfo::Decl(sym) => match &sym.annotation {
            Some(ann) => format!("{} : {}", word, ann),
            None => word,
        },
        goby_core::SymbolInfo::EffectMember(sym) => {
            format!("{} : {}", word, sym.signature)
        }
    };
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::PlainText,
            value: text,
        }),
        range: None,
    })
}

/// Return the definition location for the given cursor position, or `None` for no-definition.
fn definition_at(
    source: &str,
    uri: &Url,
    stdlib_root: Option<&Path>,
    pos: Position,
) -> Option<GotoDefinitionResponse> {
    let (index, _module) = build_index(source, stdlib_root)?;
    let word = word_at_position(source, pos)?;
    let span = match index.lookup(&word)? {
        goby_core::SymbolInfo::Decl(sym) => sym.span,
        goby_core::SymbolInfo::EffectMember(sym) => sym.span,
    };
    let range = span_to_lsp_range(source, Some(&span));
    Some(GotoDefinitionResponse::Scalar(Location {
        uri: uri.clone(),
        range,
    }))
}

/// Parse `source` and build a `SymbolIndex`.
///
/// Returns `None` if the source fails to parse.
/// When `stdlib_root` is `None` we still build the index from the parse result.
fn build_index(
    source: &str,
    _stdlib_root: Option<&Path>,
) -> Option<(goby_core::SymbolIndex, goby_core::Module)> {
    let module = goby_core::parse_module(source).ok()?;
    let index = goby_core::build_symbol_index(&module);
    Some((index, module))
}

/// Extract the identifier word that surrounds `pos` in `source`.
///
/// Uses ASCII identifier rules: `[a-zA-Z0-9_]`.  Returns `None` when the
/// character under the cursor is not part of an identifier.
///
/// `pos` is an LSP `Position` (0-indexed line, UTF-16 char offset).
fn word_at_position(source: &str, pos: Position) -> Option<String> {
    let line_text = source.split('\n').nth(pos.line as usize)?;
    // Convert UTF-16 character offset to a byte offset.
    let mut byte_offset = 0usize;
    let mut utf16_count = 0u32;
    for ch in line_text.chars() {
        if utf16_count >= pos.character {
            break;
        }
        utf16_count += ch.len_utf16() as u32;
        byte_offset += ch.len_utf8();
    }
    // byte_offset now points to the start of the character at pos.character.
    // Check whether the character at that byte offset is an identifier character.
    let bytes = line_text.as_bytes();
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
    if byte_offset >= bytes.len() || !is_ident(bytes[byte_offset]) {
        return None;
    }
    // Scan left to the start of the word.
    let mut start = byte_offset;
    while start > 0 && is_ident(bytes[start - 1]) {
        start -= 1;
    }
    // Scan right to the end of the word.
    let mut end = byte_offset;
    while end < bytes.len() && is_ident(bytes[end]) {
        end += 1;
    }
    // Safety: start..end spans complete ASCII bytes so the slice is valid UTF-8.
    Some(line_text[start..end].to_string())
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
            store.upsert(uri.clone(), text.clone());
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
                store.upsert(uri.clone(), change.text.clone());
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
            send_diagnostics(connection, uri, vec![]);
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
    send_diagnostics(connection, uri, analyze(source, stdlib_root));
}

fn send_diagnostics(connection: &Connection, uri: Url, diagnostics: Vec<lsp_types::Diagnostic>) {
    let params = PublishDiagnosticsParams { uri, diagnostics, version: None };
    let notif = Notification::new(
        <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string(),
        params,
    );
    connection.sender.send(notif.into()).ok();
}

/// Analyze `source` and return LSP diagnostics.
///
/// All per-declaration typecheck errors are collected and returned (D2b).
/// `source_path` is passed as `None` to the typechecker.
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

    goby_core::typecheck_module_collect_with_context(&module, None, Some(stdlib))
        .into_iter()
        .map(|err| {
            let diag = goby_core::Diagnostic::from(err);
            to_lsp_diagnostic(source, &diag)
        })
        .collect()
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
    /// Insert or fully replace the text for `uri` (used for both open and change in FULL sync).
    fn upsert(&mut self, uri: Url, text: String) {
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
    fn doc_store_upsert_close() {
        let mut store = DocumentStore::default();
        let uri = Url::parse("file:///test.gb").unwrap();

        store.upsert(uri.clone(), "hello".to_string());
        assert_eq!(store.get(&uri), Some("hello"));

        // upsert fully replaces (does not append)
        store.upsert(uri.clone(), "world".to_string());
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
    fn analyze_two_typecheck_errors_returns_two_diagnostics() {
        let Some(root) = stdlib_root() else {
            return;
        };
        // Two independent declarations, each with a type mismatch.
        let source = "add : Int -> Int\nadd x = \"hello\"\n\nmul : Int -> Int\nmul x = True\n";
        let diags = analyze(source, Some(&root));
        assert_eq!(diags.len(), 2, "expected two diagnostics, got: {:?}", diags);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diags[1].severity, Some(DiagnosticSeverity::ERROR));
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
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
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

    // --- word_at_position ---

    #[test]
    fn word_at_position_on_identifier() {
        let source = "add x = x + 1\n";
        // "add" starts at LSP col 0; cursor on col 1 (middle of "add")
        let word = word_at_position(source, Position { line: 0, character: 1 });
        assert_eq!(word.as_deref(), Some("add"));
    }

    #[test]
    fn word_at_position_on_whitespace_returns_none() {
        let source = "add x = x\n";
        // space between "add" and "x"
        let word = word_at_position(source, Position { line: 0, character: 3 });
        assert!(word.is_none());
    }

    #[test]
    fn word_at_position_past_end_of_line_returns_none() {
        let source = "foo\n";
        let word = word_at_position(source, Position { line: 0, character: 100 });
        assert!(word.is_none());
    }

    // --- hover_at ---

    #[test]
    fn hover_at_top_level_function_with_annotation() {
        let source = "add : Int -> Int -> Int\nadd x y = x + y\n";
        // cursor on "add" definition line (LSP line 1, char 0)
        let hover = hover_at(source, None, Position { line: 1, character: 0 });
        let hover = hover.expect("expected hover");
        if let HoverContents::Markup(mc) = hover.contents {
            assert_eq!(mc.value, "add : Int -> Int -> Int");
        } else {
            panic!("expected Markup hover contents");
        }
    }

    #[test]
    fn hover_at_unknown_position_returns_none() {
        let source = "add x = x + 1\n";
        // cursor on whitespace
        let hover = hover_at(source, None, Position { line: 0, character: 3 });
        assert!(hover.is_none());
    }

    #[test]
    fn hover_at_unknown_name_returns_none() {
        let source = "add x = x + 1\n";
        // "x" is a parameter, not in SymbolIndex
        let hover = hover_at(source, None, Position { line: 0, character: 4 });
        assert!(hover.is_none());
    }

    #[test]
    fn hover_at_effect_member() {
        let source = "effect Print\n  println : String -> ()\n";
        // cursor on "println" (LSP line 1, char 2)
        let hover = hover_at(source, None, Position { line: 1, character: 2 });
        let hover = hover.expect("expected hover for effect member");
        if let HoverContents::Markup(mc) = hover.contents {
            assert!(mc.value.contains("println"), "hover text: {}", mc.value);
            assert!(mc.value.contains("String -> ()"), "hover text: {}", mc.value);
        } else {
            panic!("expected Markup hover contents");
        }
    }

    // --- definition_at ---

    #[test]
    fn definition_at_top_level_function() {
        let source = "add : Int -> Int -> Int\nadd x y = x + y\n";
        let uri = Url::parse("file:///test.gb").unwrap();
        // cursor on "add" definition line (LSP line 1, char 0)
        let result = definition_at(source, &uri, None, Position { line: 1, character: 0 });
        let result = result.expect("expected definition response");
        if let GotoDefinitionResponse::Scalar(loc) = result {
            assert_eq!(loc.uri, uri);
            // definition line is line 2 (1-indexed) → LSP line 1
            assert_eq!(loc.range.start.line, 1);
        } else {
            panic!("expected Scalar definition response");
        }
    }

    #[test]
    fn definition_at_unknown_name_returns_none() {
        let source = "add x = x + 1\n";
        let uri = Url::parse("file:///test.gb").unwrap();
        // "x" is not in SymbolIndex
        let result = definition_at(source, &uri, None, Position { line: 0, character: 4 });
        assert!(result.is_none());
    }

    // --- end-to-end hover via in-memory connection ---

    fn recv_response(client_conn: &Connection) -> lsp_server::Response {
        let msg = client_conn.receiver.recv().unwrap();
        if let Message::Response(resp) = msg {
            resp
        } else {
            panic!("expected response, got: {:?}", msg);
        }
    }

    fn open_document(client_conn: &Connection, uri: &Url, source: &str) {
        let notif = LspNotification {
            method: <DidOpenTextDocument as lsp_types::notification::Notification>::METHOD
                .to_string(),
            params: serde_json::to_value(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "goby".to_string(),
                    version: 1,
                    text: source.to_string(),
                },
            })
            .unwrap(),
        };
        client_conn.sender.send(notif.into()).unwrap();
        let _ = recv_publish_diagnostics(client_conn); // discard open diagnostics
    }

    #[test]
    fn e2e_hover_returns_annotation() {
        let (client_conn, server_thread) = start_server();
        let uri = Url::parse("file:///hover_test.gb").unwrap();
        let source = "double : Int -> Int\ndouble x = x + x\n";
        open_document(&client_conn, &uri, source);

        // Send hover request on "double" definition line (LSP line 1, char 0)
        let hover_req = LspRequest {
            id: lsp_server::RequestId::from(10),
            method: <HoverRequest as lsp_types::request::Request>::METHOD.to_string(),
            params: serde_json::to_value(lsp_types::HoverParams {
                text_document_position_params: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    position: Position { line: 1, character: 0 },
                },
                work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
            })
            .unwrap(),
        };
        client_conn.sender.send(hover_req.into()).unwrap();

        let resp = recv_response(&client_conn);
        assert!(resp.error.is_none(), "unexpected error: {:?}", resp.error);
        let hover: Option<Hover> = serde_json::from_value(resp.result.unwrap()).unwrap();
        let hover = hover.expect("expected hover result");
        if let HoverContents::Markup(mc) = hover.contents {
            assert!(mc.value.contains("double"), "hover: {}", mc.value);
            assert!(mc.value.contains("Int -> Int"), "hover: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }

        shutdown_server(client_conn, server_thread);
    }

    #[test]
    fn e2e_definition_returns_location() {
        let (client_conn, server_thread) = start_server();
        let uri = Url::parse("file:///def_test.gb").unwrap();
        let source = "double : Int -> Int\ndouble x = x + x\n";
        open_document(&client_conn, &uri, source);

        // Send definition request on "double" definition line
        let def_req = LspRequest {
            id: lsp_server::RequestId::from(11),
            method: <GotoDefinition as lsp_types::request::Request>::METHOD.to_string(),
            params: serde_json::to_value(lsp_types::GotoDefinitionParams {
                text_document_position_params: lsp_types::TextDocumentPositionParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                    position: Position { line: 1, character: 0 },
                },
                work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
                partial_result_params: lsp_types::PartialResultParams::default(),
            })
            .unwrap(),
        };
        client_conn.sender.send(def_req.into()).unwrap();

        let resp = recv_response(&client_conn);
        assert!(resp.error.is_none(), "unexpected error: {:?}", resp.error);
        let def: Option<GotoDefinitionResponse> =
            serde_json::from_value(resp.result.unwrap()).unwrap();
        let def = def.expect("expected definition result");
        if let GotoDefinitionResponse::Scalar(loc) = def {
            assert_eq!(loc.uri, uri);
            assert_eq!(loc.range.start.line, 1); // LSP line 1 = source line 2
        } else {
            panic!("expected Scalar definition");
        }

        shutdown_server(client_conn, server_thread);
    }

    // --- local binding hover ---

    #[test]
    fn local_binding_hover_basic() {
        // Source:
        //   line 0 (LSP): "add : Int -> Int"
        //   line 1 (LSP): "add x ="
        //   line 2 (LSP): "  y = x + 1"   ← cursor here on "y"
        //   line 3 (LSP): "  y"
        //
        // def_line_of(decl) = 2 (annotation at line 1, def at line 2, 1-indexed)
        // body_relative_line for "y = x + 1" = 2 (body starts with '\n')
        // source_line = 2 + 2 - 1 = 3; cursor_source_line = 2 + 1 = 3 ✓
        let source = "add : Int -> Int\nadd x =\n  y = x + 1\n  y\n";
        let pos = Position { line: 2, character: 2 }; // "y" on line 2 (0-indexed)
        let result = hover_at(source, None, pos);
        assert!(result.is_some(), "expected hover for local binding 'y'");
        if let Some(hover) = result {
            if let HoverContents::Markup(mc) = hover.contents {
                assert_eq!(mc.value, "y : Int");
            } else {
                panic!("expected Markup hover");
            }
        }
    }

    #[test]
    fn local_binding_hover_mut() {
        // Source:
        //   line 0 (LSP): "foo : Int -> Int"
        //   line 1 (LSP): "foo x ="
        //   line 2 (LSP): "  mut z = 0"   ← cursor here on "z"
        //   line 3 (LSP): "  z"
        let source = "foo : Int -> Int\nfoo x =\n  mut z = 0\n  z\n";
        let pos = Position { line: 2, character: 6 }; // "z" in "  mut z = 0"
        let result = hover_at(source, None, pos);
        assert!(result.is_some(), "expected hover for local binding 'z'");
        if let Some(hover) = result {
            if let HoverContents::Markup(mc) = hover.contents {
                assert_eq!(mc.value, "z : Int");
            } else {
                panic!("expected Markup hover");
            }
        }
    }

    #[test]
    fn local_binding_hover_use_site_returns_none() {
        // Cursor on the USE site of "y" (line 3), not the definition (line 2).
        // Use-site hover is out of scope; should return None.
        let source = "add : Int -> Int\nadd x =\n  y = x + 1\n  y\n";
        let pos = Position { line: 3, character: 2 }; // "y" on line 3 (the tail expr)
        let result = hover_at(source, None, pos);
        assert!(result.is_none(), "use-site hover should return None, got: {:?}", result);
    }

    #[test]
    fn local_binding_hover_unknown_type_returns_none() {
        // Binding depends on an unknown global → Ty::Unknown → not emitted.
        let source = "foo : Int -> Int\nfoo x =\n  y = some_global_fn x\n  y\n";
        let pos = Position { line: 2, character: 2 };
        let result = hover_at(source, None, pos);
        assert!(result.is_none(), "unknown-type binding should return None, got: {:?}", result);
    }

    #[test]
    fn local_binding_hover_no_annotation() {
        // Function with no type annotation: literals still produce a known type.
        // Source:
        //   line 0 (LSP): "answer ="
        //   line 1 (LSP): "  x = 42"   ← cursor on "x"
        //   line 2 (LSP): "  x"
        //
        // def_line_of(decl) = 1 (no annotation, def line is line 1, 1-indexed)
        // body_relative_line = 2 (body starts with '\n')
        // source_line = 1 + 2 - 1 = 2; cursor_source_line = 1 + 1 = 2 ✓
        let source = "answer =\n  x = 42\n  x\n";
        let pos = Position { line: 1, character: 2 }; // "x" on line 1 (0-indexed)
        let result = hover_at(source, None, pos);
        assert!(result.is_some(), "expected hover for local binding 'x'");
        if let Some(hover) = result {
            if let HoverContents::Markup(mc) = hover.contents {
                assert_eq!(mc.value, "x : Int");
            } else {
                panic!("expected Markup hover");
            }
        }
    }

    #[test]
    fn local_binding_hover_multi_binding_chain() {
        // Second binding depends on first: b = a * 2 where a : Int → b : Int.
        // Source:
        //   line 0 (LSP): "f : Int -> Int"
        //   line 1 (LSP): "f x ="
        //   line 2 (LSP): "  a = x + 1"
        //   line 3 (LSP): "  b = a + 2"   ← cursor on "b"
        //   line 4 (LSP): "  b"
        let source = "f : Int -> Int\nf x =\n  a = x + 1\n  b = a + 2\n  b\n";
        let pos = Position { line: 3, character: 2 }; // "b" on line 3
        let result = hover_at(source, None, pos);
        assert!(result.is_some(), "expected hover for local binding 'b'");
        if let Some(hover) = result {
            if let HoverContents::Markup(mc) = hover.contents {
                assert_eq!(mc.value, "b : Int");
            } else {
                panic!("expected Markup hover");
            }
        }
    }

    #[test]
    fn local_binding_hover_reassign_lhs_returns_none() {
        // Hovering on `n` in `n := n + 1` (reassignment, not definition) → None.
        // Source:
        //   line 0 (LSP): "count : Int -> Int"
        //   line 1 (LSP): "count x ="
        //   line 2 (LSP): "  mut n = x"    ← definition (returns "n : Int")
        //   line 3 (LSP): "  n := n + 1"   ← reassignment (cursor here → None)
        //   line 4 (LSP): "  n"
        let source = "count : Int -> Int\ncount x =\n  mut n = x\n  n := n + 1\n  n\n";
        let pos = Position { line: 3, character: 2 }; // "n" on the Assign line
        let result = hover_at(source, None, pos);
        assert!(result.is_none(), "reassignment LHS hover should return None, got: {:?}", result);
    }

    #[test]
    fn local_binding_hover_lhs_col_match() {
        // Cursor exactly on the LHS "x" in "  x = n + 1" should return hover.
        // LHS "x": indent 2, col 3 (1-indexed), character 2 (0-indexed).
        let source = "f : Int -> Int\nf n =\n  x = n + 1\n  x\n";
        let pos = Position { line: 2, character: 2 }; // LHS "x"
        let result = hover_at(source, None, pos);
        assert!(result.is_some(), "expected hover on LHS 'x', got None");
        if let Some(hover) = result {
            if let HoverContents::Markup(mc) = hover.contents {
                assert_eq!(mc.value, "x : Int");
            } else {
                panic!("expected Markup hover");
            }
        }
    }

    #[test]
    fn local_binding_hover_rhs_col_out_of_range() {
        // Column-range test: binding "ab" at col 3-4 (chars 2-3, 0-indexed).
        // Cursor at char 7 (on the "x" in "x + 1") should return None.
        // Source:
        //   line 2: "  ab = x + 1"  ← LHS "ab" col 3, len 2 → col range [3,5)
        let source = "f : Int -> Int\nf x =\n  ab = x + 1\n  ab\n";
        let pos_lhs_start = Position { line: 2, character: 2 }; // 'a' of "ab"
        let pos_lhs_end = Position { line: 2, character: 3 };   // 'b' of "ab"
        // character: 4 is one past end of "ab" (exclusive upper bound test)
        let pos_just_past = Position { line: 2, character: 4 };
        // character: 7 is 'x' in "x + 1" — different name, filtered by name check (not col guard)
        let pos_rhs = Position { line: 2, character: 7 };
        assert!(hover_at(source, None, pos_lhs_start).is_some(), "LHS start should hover");
        assert!(hover_at(source, None, pos_lhs_end).is_some(), "LHS end char should hover");
        assert!(hover_at(source, None, pos_just_past).is_none(), "one past name end should not hover");
        assert!(hover_at(source, None, pos_rhs).is_none(), "RHS 'x' (different name) should not hover");
    }

    #[test]
    fn local_binding_hover_rhs_same_name_returns_none() {
        // The critical column-guard test: same name appears on both LHS and RHS of a binding.
        // Source:
        //   line 0: "f : Int -> Int"
        //   line 1: "f x ="
        //   line 2: "  y = x + 1"   ← but use a case where binding name == param name
        //
        // Use "f n =\n  n = n + 1": LHS 'n' at col 3 (char 2), RHS 'n' at col 7 (char 6).
        // Both have the same word "n", so the name filter does NOT eliminate the RHS case —
        // only the column guard can distinguish them.
        //   line 2: "  n = n + 1"
        //   LHS 'n': col 3, char 2. Range [3, 4).
        //   RHS 'n': col 7, char 6. Outside range → None.
        let source = "f : Int -> Int\nf n =\n  n = n + 1\n  n\n";
        let pos_lhs = Position { line: 2, character: 2 }; // LHS 'n' at col 3
        let pos_rhs = Position { line: 2, character: 6 }; // RHS 'n' at col 7
        let lhs_result = hover_at(source, None, pos_lhs);
        assert!(lhs_result.is_some(), "LHS 'n' should hover, got None");
        if let Some(hover) = lhs_result {
            if let HoverContents::Markup(mc) = hover.contents {
                assert_eq!(mc.value, "n : Int");
            } else {
                panic!("expected Markup hover");
            }
        }
        assert!(
            hover_at(source, None, pos_rhs).is_none(),
            "RHS 'n' (same name, different col) should return None"
        );
    }

    #[test]
    fn local_binding_hover_with_body() {
        // Binding inside a `with` body should be visible to hover.
        // Source (0-indexed lines):
        //   0: "main ="
        //   1: "  with"
        //   2: "    log str ->"
        //   3: "      resume ()"
        //   4: "  in"
        //   5: "    y = 42"   ← def_line=1, body_relative_line=6, source_line=6, LSP line=5
        //   6: "    y"
        // "    y = 42": indent=4, body_relative_col=5, LSP character=4.
        let source = "main =\n  with\n    log str ->\n      resume ()\n  in\n    y = 42\n    y\n";
        let pos = Position { line: 5, character: 4 }; // 'y' in "    y = 42"
        let result = hover_at(source, None, pos);
        assert!(result.is_some(), "expected hover on 'y' in with body, got None");
        if let Some(hover) = result {
            if let HoverContents::Markup(mc) = hover.contents {
                assert_eq!(mc.value, "y : Int");
            } else {
                panic!("expected Markup hover");
            }
        }
    }
}
