from __future__ import annotations

"""
A minimal pygls-based Language Server for Zeta Lisp.

Features:
- Initialize/Shutdown/Exit
- Text synchronization and document store
- Diagnostics: parser errors, unmatched parens, unmatched quotes, unknown pkg alias
- Hover: builtin signatures and locally defined symbols
- Completion: locals, builtins; after 'pkg:' suggest package symbols if imported
- Signature Help: for known builtins
- Document Symbols: from indexer

Note: We avoid evaluating the buffer. We build a static index per document.
"""

import json
from typing import Dict, Optional, List
from dataclasses import dataclass

from pygls.server import LanguageServer
from lsprotocol.types import (
    InitializeParams,
    InitializeResult,
    TextDocumentSyncKind,
    DidOpenTextDocumentParams,
    DidChangeTextDocumentParams,
    DidCloseTextDocumentParams,
    Diagnostic,
    DiagnosticSeverity,
    Position,
    Range,
    Hover,
    MarkupContent,
    MarkupKind,
    CompletionItem,
    CompletionItemKind,
    CompletionList,
    CompletionParams,
    HoverParams,
    DocumentSymbolParams,
    DocumentSymbol,
    SymbolKind,
    SignatureHelp,
    SignatureInformation,
    ParameterInformation,
    SignatureHelpParams,
)

from zeta_lsp.indexer import build_index, BUILTIN_SIGNATURES, DocumentIndex


@dataclass
class DocumentState:
    text: str
    index: DocumentIndex


class ZetaLanguageServer(LanguageServer):
    CMD_NAME = "zeta-ls"

    def __init__(self):
        super().__init__(self.CMD_NAME)
        self.documents: Dict[str, DocumentState] = {}


ls = ZetaLanguageServer()


@ls.feature("initialize")
def on_initialize(params: InitializeParams):
    return InitializeResult(
        capabilities={
            "textDocumentSync": TextDocumentSyncKind.Incremental,
            "hoverProvider": True,
            "completionProvider": {"resolveProvider": False, "triggerCharacters": [":", "("]},
            "signatureHelpProvider": {"triggerCharacters": ["(", " "]},
            "documentSymbolProvider": True,
        }
    )


@ls.feature("shutdown")
def on_shutdown(*_):
    return None


@ls.feature("exit")
def on_exit(*_):
    return None


# --- Text sync ---
@ls.feature("textDocument/didOpen")
def did_open(params: DidOpenTextDocumentParams):
    uri = params.text_document.uri
    text = params.text_document.text or ""
    idx = build_index(text)
    ls.documents[uri] = DocumentState(text=text, index=idx)
    _publish_diagnostics(uri, idx)


@ls.feature("textDocument/didChange")
def did_change(params: DidChangeTextDocumentParams):
    uri = params.text_document.uri
    if params.content_changes:
        text = params.content_changes[-1].text
    else:
        text = ls.documents.get(uri, DocumentState("", build_index(""))).text
    idx = build_index(text)
    ls.documents[uri] = DocumentState(text=text, index=idx)
    _publish_diagnostics(uri, idx)


@ls.feature("textDocument/didClose")
def did_close(params: DidCloseTextDocumentParams):
    uri = params.text_document.uri
    if uri in ls.documents:
        del ls.documents[uri]
    ls.publish_diagnostics(uri, [])


# --- Diagnostics ---
def _mk_range(text: str, line: int, col: int) -> Range:
    return Range(start=Position(line=line, character=col), end=Position(line=line, character=col+1))


def _publish_diagnostics(uri: str, idx: DocumentIndex):
    text = ls.documents[uri].text
    diags: List[Diagnostic] = []

    # Unmatched parens
    if idx.paren_balance != 0:
        diags.append(
            Diagnostic(
                range=_mk_range(text, 0, 0),
                message="Unmatched parentheses detected",
                severity=DiagnosticSeverity.Warning,
                source="zeta-ls",
            )
        )

    # Unmatched quotes
    if idx.has_unmatched_quote:
        diags.append(
            Diagnostic(
                range=_mk_range(text, 0, 0),
                message="Unmatched quote detected",
                severity=DiagnosticSeverity.Warning,
                source="zeta-ls",
            )
        )

    # Unknown pkg alias in definitions like pkg:sym without import
    for pkg in idx.pkg_symbols.keys():
        if not any(imp.alias == pkg for imp in idx.imports):
            # mark first occurrence
            sym_map = idx.pkg_symbols[pkg]
            any_sym = next(iter(sym_map.values()))
            diags.append(
                Diagnostic(
                    range=_mk_range(text, any_sym.line, any_sym.col),
                    message=f"Unknown package alias '{pkg}'. Consider (import \"<module>\" as \"{pkg}\").",
                    severity=DiagnosticSeverity.Information,
                    source="zeta-ls",
                )
            )

    ls.publish_diagnostics(uri, diags)


# --- Hover ---
@ls.feature("textDocument/hover")
def on_hover(params: HoverParams) -> Optional[Hover]:
    uri = params.text_document.uri
    state = ls.documents.get(uri)
    if not state:
        return None

    word, pos = _extract_word_at(state.text, params.position)
    if not word:
        return None

    contents = None

    if word in BUILTIN_SIGNATURES:
        contents = BUILTIN_SIGNATURES[word]
    elif word in state.index.symbols:
        sdef = state.index.symbols[word]
        contents = f"{word} — {sdef.kind} (defined at {sdef.line+1}:{sdef.col+1})"
    else:
        # package-qualified?
        if ':' in word:
            pkg, sym = word.split(':', 1)
            if pkg in state.index.pkg_symbols and sym in state.index.pkg_symbols[pkg]:
                sdef = state.index.pkg_symbols[pkg][sym]
                contents = f"{word} — {sdef.kind} (defined at {sdef.line+1}:{sdef.col+1})"

    if contents is None:
        return None
    return Hover(contents=MarkupContent(kind=MarkupKind.PlainText, value=contents))


# --- Completion ---
@ls.feature("textDocument/completion")
def on_completion(params: CompletionParams) -> CompletionList:
    uri = params.text_document.uri
    state = ls.documents.get(uri)
    items: List[CompletionItem] = []
    if not state:
        return CompletionList(is_incomplete=False, items=items)

    # After a 'pkg:' prefix, suggest symbols within that package if known
    prefix_text = _get_line_prefix(state.text, params.position)
    pkg_prefix = _detect_pkg_prefix(prefix_text)
    if pkg_prefix:
        pkg = pkg_prefix
        for sym, sdef in state.index.pkg_symbols.get(pkg, {}).items():
            items.append(CompletionItem(label=f"{pkg}:{sym}", kind=CompletionItemKind.Function))
        return CompletionList(is_incomplete=False, items=items)

    # Otherwise: builtins and local symbols
    for name, sig in BUILTIN_SIGNATURES.items():
        items.append(CompletionItem(label=name, kind=CompletionItemKind.Function, detail=sig))
    for name, sdef in state.index.symbols.items():
        items.append(CompletionItem(label=name, kind=CompletionItemKind.Variable))

    return CompletionList(is_incomplete=False, items=items)


# --- Signature Help ---
@ls.feature("textDocument/signatureHelp")
def on_signature_help(params: SignatureHelpParams) -> Optional[SignatureHelp]:
    uri = params.text_document.uri
    state = ls.documents.get(uri)
    if not state:
        return None

    # crude: find current word before '(' on the current line
    line_text = _get_line_prefix(state.text, params.position)
    callee = _extract_callee_name(line_text)
    if not callee:
        return None

    sig = BUILTIN_SIGNATURES.get(callee)
    if not sig:
        return None

    # Split rendering into name and params between parentheses
    label = sig
    open_paren = label.find('(')
    close_paren = label.find(')')
    params_text = label[open_paren+1:close_paren]
    params_list = [p.strip() for p in params_text.split(' ') if p.strip()]
    parameters = [ParameterInformation(label=p) for p in params_list]

    return SignatureHelp(signatures=[SignatureInformation(label=label, parameters=parameters)], activeSignature=0, activeParameter=0)


# --- Document Symbols ---
@ls.feature("textDocument/documentSymbol")
def on_document_symbols(params: DocumentSymbolParams) -> Optional[List[DocumentSymbol]]:
    uri = params.text_document.uri
    state = ls.documents.get(uri)
    if not state:
        return None
    symbols: List[DocumentSymbol] = []

    for name, sdef in state.index.symbols.items():
        rng = Range(
            start=Position(line=sdef.line, character=sdef.col),
            end=Position(line=sdef.line, character=sdef.col + len(name)),
        )
        symbols.append(
            DocumentSymbol(
                name=name,
                kind=SymbolKind.Function if sdef.kind == 'function' else (SymbolKind.Variable if sdef.kind == 'var' else SymbolKind.Function),
                range=rng,
                selection_range=rng,
            )
        )
    for pkg, syms in state.index.pkg_symbols.items():
        for name, sdef in syms.items():
            full = f"{pkg}:{name}"
            rng = Range(
                start=Position(line=sdef.line, character=sdef.col),
                end=Position(line=sdef.line, character=sdef.col + len(full)),
            )
            symbols.append(
                DocumentSymbol(
                    name=full,
                    kind=SymbolKind.Function if sdef.kind == 'function' else (SymbolKind.Variable if sdef.kind == 'var' else SymbolKind.Function),
                    range=rng,
                    selection_range=rng,
                )
            )
    return symbols


# --- Helpers ---

def _get_line_prefix(text: str, pos: Position) -> str:
    # Return the text from start of line up to pos
    lines = text.splitlines(True)
    if pos.line >= len(lines):
        return ""
    line_text = lines[pos.line]
    return line_text[: pos.character]


def _detect_pkg_prefix(prefix: str) -> Optional[str]:
    # Return pkg if line ends with "pkg:"
    if ':' in prefix:
        # find last token containing ':'
        parts = re_split_tokens(prefix)
        for p in reversed(parts):
            if ':' in p:
                if p.endswith(':'):
                    return p[:-1]
                # possibly already typing symbol; we still return pkg for filtering
                return p.split(':', 1)[0]
    return None


def re_split_tokens(line: str) -> List[str]:
    # naive split on whitespace and parens
    import re as _re
    return [t for t in _re.split(r"[\s()]+", line) if t]


def _extract_word_at(text: str, pos: Position) -> tuple[Optional[str], Position]:
    lines = text.splitlines(True)
    if pos.line >= len(lines):
        return None, pos
    line = lines[pos.line]
    i = pos.character
    # expand to word boundaries (letters, digits, :, -, ?, !, etc.)
    start = i
    while start > 0 and line[start - 1] not in " \t()\n\r":
        start -= 1
    end = i
    while end < len(line) and line[end] not in " \t()\n\r":
        end += 1
    word = line[start:end]
    return (word if word else None), Position(line=pos.line, character=start)


def _extract_callee_name(prefix: str) -> Optional[str]:
    # find last '(' and take following token
    lp = prefix.rfind('(')
    if lp == -1:
        return None
    tail = prefix[lp + 1 :].strip()
    if not tail:
        return None
    # first token
    for sep in [' ', '\t', '\n', '\r', ')']:
        p = tail.find(sep)
        if p != -1:
            tail = tail[:p]
    # strip package qualifier does not change callee name lookup for builtins
    return tail


if __name__ == "__main__":
    # Run the language server over stdio
    ls.start_io()
