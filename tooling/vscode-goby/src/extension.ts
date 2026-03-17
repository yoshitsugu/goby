import * as vscode from "vscode";
import * as path from "path";
import * as fs from "fs";
import { execFile } from "child_process";
import { promisify } from "util";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

const execFileAsync = promisify(execFile);

let client: LanguageClient | undefined;

/** Paths of files currently being formatted (concurrent-save guard). */
const formattingInFlight = new Set<string>();

// ---------------------------------------------------------------------------
// Binary resolution helpers
// ---------------------------------------------------------------------------

/** Search PATH for an executable binary. Returns the full path or undefined. */
function whichSync(name: string): string | undefined {
  const dirs = (process.env.PATH ?? "").split(path.delimiter);
  for (const dir of dirs) {
    const candidate = path.join(dir, name);
    try {
      fs.accessSync(candidate, fs.constants.X_OK);
      return candidate;
    } catch {
      // not executable or not found — continue
    }
  }
  return undefined;
}

/** Return workspace-local debug or release path if executable, else undefined. */
function workspaceLocalBin(workspaceRoot: string, name: string): string | undefined {
  // workspaceRoot is fetched fresh by callers; read it as passed.
  for (const sub of ["debug", "release"]) {
    const p = path.join(workspaceRoot, "target", sub, name);
    try { fs.accessSync(p, fs.constants.X_OK); return p; } catch { /* continue */ }
  }
  return undefined;
}

/**
 * Resolve the `goby-lsp` binary path.
 * Order: `goby.serverPath` setting → PATH → workspace-local target/debug → target/release
 */
function resolveServerPath(workspaceRoot: string | undefined): string | undefined {
  const cfg = vscode.workspace.getConfiguration("goby");
  const setting = cfg.get<string>("serverPath", "").trim();
  if (setting) {
    try { fs.accessSync(setting, fs.constants.X_OK); return setting; } catch {
      vscode.window.showWarningMessage(
        `goby: serverPath "${setting}" is not executable; falling back to PATH / workspace.`
      );
    }
  }
  const onPath = whichSync("goby-lsp");
  if (onPath) { return onPath; }
  if (workspaceRoot) { return workspaceLocalBin(workspaceRoot, "goby-lsp"); }
  return undefined;
}

/**
 * Resolve the `goby` CLI binary path.
 * Order: `goby.executablePath` setting → PATH → workspace-local target/debug → target/release
 */
function resolveGobyExecutable(workspaceRoot: string | undefined): string | undefined {
  const cfg = vscode.workspace.getConfiguration("goby");
  const setting = cfg.get<string>("executablePath", "").trim();
  if (setting) {
    try { fs.accessSync(setting, fs.constants.X_OK); return setting; } catch {
      vscode.window.showWarningMessage(
        `goby: executablePath "${setting}" is not executable; falling back to PATH / workspace.`
      );
    }
  }
  const onPath = whichSync("goby");
  if (onPath) { return onPath; }
  if (workspaceRoot) { return workspaceLocalBin(workspaceRoot, "goby"); }
  return undefined;
}

/** Build the process environment for goby-lsp / goby CLI. */
function buildServerEnv(): NodeJS.ProcessEnv {
  const env: NodeJS.ProcessEnv = { ...process.env };
  const stdlibRoot = vscode.workspace.getConfiguration("goby").get<string>("stdlibRoot", "").trim();
  if (stdlibRoot) {
    env["GOBY_STDLIB_ROOT"] = stdlibRoot;
  }
  return env;
}

// ---------------------------------------------------------------------------
// Activation
// ---------------------------------------------------------------------------

export function activate(context: vscode.ExtensionContext): void {
  // --- LSP client ---
  const startClient = () => {
    // Resolve workspaceRoot fresh each time (handles folder-add/remove).
    const wsRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
    const serverPath = resolveServerPath(wsRoot);
    if (!serverPath) {
      vscode.window.showInformationMessage(
        "goby-lsp not found. Set `goby.serverPath` or add goby-lsp to your PATH. " +
          "Build with: cargo build -p goby-lsp"
      );
      return;
    }

    const serverOptions: ServerOptions = {
      command: serverPath,
      transport: TransportKind.stdio,
      options: { env: buildServerEnv() },
    };
    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ scheme: "file", language: "goby" }],
    };

    client = new LanguageClient("goby-lsp", "Goby Language Server", serverOptions, clientOptions);
    // Push to subscriptions so VS Code disposes it automatically on extension deactivation.
    context.subscriptions.push(client);
    client.start();
  };

  startClient();

  // Restart command: rebuilds env + re-resolves binary from current settings.
  context.subscriptions.push(
    vscode.commands.registerCommand("goby.restartServer", async () => {
      if (client) {
        try { await client.stop(); } catch { /* already stopped */ }
        client = undefined;
      }
      startClient();
    })
  );

  // --- fmt-on-save (independent of LSP being available) ---
  // TODO: switch to `goby fmt --stdout` once the CLI supports piped output,
  // to avoid the brief window where on-disk content diverges from the buffer.
  context.subscriptions.push(
    vscode.workspace.onDidSaveTextDocument(async (doc) => {
      if (doc.languageId !== "goby") { return; }
      if (!vscode.workspace.getConfiguration("goby").get<boolean>("formatOnSave", true)) { return; }

      // Resolve workspaceRoot fresh (handles folder changes).
      const wsRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
      const gobyBin = resolveGobyExecutable(wsRoot);
      if (!gobyBin) { return; }

      const filePath = doc.uri.fsPath;

      // Concurrent-save guard: skip if a format is already in flight for this file.
      if (formattingInFlight.has(filePath)) { return; }
      formattingInFlight.add(filePath);
      try {
        await execFileAsync(gobyBin, ["fmt", filePath], {
          timeout: 10_000,
          env: buildServerEnv(),
        });
      } catch (err: unknown) {
        const e = err as { killed?: boolean; message?: string };
        const msg = e.killed ? "timed out" : (e.message ?? String(err)).split("\n")[0].slice(0, 100);
        vscode.window.setStatusBarMessage(`goby fmt: ${msg}`, 3000);
        return;
      } finally {
        formattingInFlight.delete(filePath);
      }

      // Read the formatted content from disk.
      let formatted: string;
      try {
        formatted = fs.readFileSync(filePath, "utf8");
      } catch {
        return;
      }

      // If the buffer already matches, nothing to do (prevents the double-save loop).
      if (doc.getText() === formatted) { return; }

      // Guard against empty document to avoid doc.lineAt(0) throwing.
      if (doc.lineCount === 0) { return; }

      const edit = new vscode.WorkspaceEdit();
      const fullRange = new vscode.Range(
        doc.lineAt(0).range.start,
        doc.lineAt(doc.lineCount - 1).range.end
      );
      edit.replace(doc.uri, fullRange, formatted);
      const applied = await vscode.workspace.applyEdit(edit);
      if (!applied) {
        vscode.window.setStatusBarMessage("goby fmt: failed to apply edit", 3000);
      }
    })
  );
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
