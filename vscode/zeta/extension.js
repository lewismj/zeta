const vscode = require('vscode');
const net = require('net');
const cp = require('child_process');
const path = require('path');

let replClient = null;
let lsProcess = null;
let outputChannel = null;

function startLanguageServer(context) {
  if (lsProcess) {
    return;
  }
  const python = process.env.PYTHON || 'python';
  // Launch zeta_lsp.server over stdio
  lsProcess = cp.spawn(python, ['-m', 'zeta_lsp.server'], {
    stdio: ['pipe', 'pipe', 'pipe']
  });
  lsProcess.on('exit', (code) => {
    lsProcess = null;
  });
}

function startRepl(context) {
  if (replClient) {
    vscode.window.showInformationMessage('Zeta REPL already running.');
    return;
  }
  const python = process.env.PYTHON || 'python';
  const serverModule = 'zeta_lsp.repl_server';
  const proc = cp.spawn(python, ['-m', serverModule]);
  proc.on('error', (err) => console.error('REPL server error', err));

  // retry connect a few times
  const tryConnect = (attempts = 20) => new Promise((resolve, reject) => {
    const socket = new net.Socket();
    socket.connect(8765, '127.0.0.1', () => resolve(socket));
    socket.on('error', (e) => {
      if (attempts <= 0) {
        reject(e);
      } else {
        setTimeout(() => tryConnect(attempts - 1).then(resolve).catch(reject), 200);
      }
    });
  });

  tryConnect().then((socket) => {
    replClient = socket;
    socket.setEncoding('utf8');
    vscode.window.showInformationMessage('Zeta REPL connected.');
  }).catch((e) => {
    vscode.window.showErrorMessage('Failed to connect to Zeta REPL: ' + e.message);
  });
}

function sendEval(code) {
  if (!replClient) {
    vscode.window.showWarningMessage('REPL not running. Use "Zeta: Start REPL".');
    return;
  }
  return new Promise((resolve, reject) => {
    let buffer = '';
    const onData = (chunk) => {
      buffer += chunk;
      const nl = buffer.indexOf('\n');
      if (nl !== -1) {
        const line = buffer.slice(0, nl);
        buffer = buffer.slice(nl + 1);
        replClient.off('data', onData);
        try {
          const resp = JSON.parse(line);
          if (resp.ok) resolve(resp.result);
          else reject(new Error(resp.error));
        } catch (e) {
          reject(e);
        }
      }
    };
    replClient.on('data', onData);
    replClient.write(JSON.stringify({ cmd: 'eval', code }) + '\n');
  });
}

function activate(context) {
  outputChannel = vscode.window.createOutputChannel('Zeta');
  context.subscriptions.push(outputChannel);

  startLanguageServer(context);

  context.subscriptions.push(vscode.commands.registerCommand('zeta.startRepl', () => startRepl(context)));
  context.subscriptions.push(vscode.commands.registerCommand('zeta.evalSelection', async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) { return; }
    const code = editor.document.getText(editor.selection);
    if (!code) { return; }
    try {
      const result = await sendEval(code);
      outputChannel.appendLine('> ' + code);
      outputChannel.appendLine(result);
      outputChannel.show(true);
    } catch (e) {
      vscode.window.showErrorMessage('Zeta eval error: ' + e.message);
    }
  }));
  context.subscriptions.push(vscode.commands.registerCommand('zeta.evalFile', async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor) { return; }
    const code = editor.document.getText();
    try {
      const result = await sendEval(code);
      outputChannel.appendLine('> (file) ' + editor.document.fileName);
      outputChannel.appendLine(result);
      outputChannel.show(true);
    } catch (e) {
      vscode.window.showErrorMessage('Zeta eval error: ' + e.message);
    }
  }));
}

function deactivate() {
  if (replClient) {
    try { replClient.destroy(); } catch {}
    replClient = null;
  }
  if (lsProcess) {
    try { lsProcess.kill(); } catch {}
    lsProcess = null;
  }
}

module.exports = { activate, deactivate };
