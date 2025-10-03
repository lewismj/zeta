from __future__ import annotations

"""
Lightweight indexer for Zeta Lisp files without evaluating code.

We scan for top-level forms and build an index for:
- definitions: (define name ...), (defun name ...), (defmacro name ...)
- package-qualified defines: (define pkg:symbol ...)
- imports: (import "module" as "alias" helpers "module") to track aliases

The parser is tolerant: it uses a lightweight s-expression scanner to avoid
crashing on partial/incomplete buffers. We only extract enough structure to
power LSP features (document symbols, completion, diagnostics for unknown pkg aliases).
"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional, Tuple
import re

# Simple token patterns for scanning
TOKEN_REGEX = re.compile(
    r"\s+|;.*$|\(|\)|'|`|,|,@|\"(?:\\.|[^\"])*\"|[^\s()]+",
    re.MULTILINE,
)


@dataclass
class SymbolDef:
    name: str
    kind: str  # "var" | "function" | "macro"
    line: int
    col: int


@dataclass
class ImportAlias:
    module: str
    alias: Optional[str]


@dataclass
class DocumentIndex:
    symbols: Dict[str, SymbolDef] = field(default_factory=dict)
    pkg_symbols: Dict[str, Dict[str, SymbolDef]] = field(default_factory=dict)  # pkg -> sym -> def
    imports: List[ImportAlias] = field(default_factory=list)
    paren_balance: int = 0
    has_unmatched_quote: bool = False


def _iter_tokens(text: str):
    for m in TOKEN_REGEX.finditer(text):
        tok = m.group(0)
        if not tok or tok.isspace():
            continue
        yield tok, m.start(), m.end()


def _position_from_offset(text: str, offset: int) -> Tuple[int, int]:
    # Return (line, col), 0-based
    line = text.count("\n", 0, offset)
    last_nl = text.rfind("\n", 0, offset)
    col = offset if last_nl == -1 else offset - last_nl - 1
    return line, col


def build_index(text: str) -> DocumentIndex:
    idx = DocumentIndex()
    tokens = list(_iter_tokens(text))

    i = 0
    while i < len(tokens):
        tok, start, end = tokens[i]
        if tok == '(':
            idx.paren_balance += 1
            # Look ahead for a potential top-level form
            # Extract head symbol if available
            head = None
            head_pos = i + 1
            if head_pos < len(tokens):
                head_tok, hs, he = tokens[head_pos]
                if head_tok not in ('(', ')') and not head_tok.startswith(';'):
                    head = head_tok
            # Handle selected forms
            if head in ("define", "defun", "defmacro", "import"):
                if head == "import":
                    # (import "module" as "alias" helpers "mod")
                    module_name = None
                    alias = None
                    # Collect up to next ')'
                    j = head_pos + 1
                    while j < len(tokens):
                        t, s, e = tokens[j]
                        if t == ')':
                            break
                        if module_name is None and t.startswith('"'):
                            module_name = t.strip('"')
                        elif t == 'as' and (j + 1) < len(tokens):
                            alias_tok, *_ = tokens[j + 1]
                            alias = alias_tok.strip('"')
                        j += 1
                    if module_name:
                        idx.imports.append(ImportAlias(module=module_name, alias=alias))
                else:
                    # Pull the next symbol as definition name
                    # (define name ...)
                    j = head_pos + 1
                    while j < len(tokens):
                        t, s, e = tokens[j]
                        if t in ('(', ')'):
                            break
                        # strip quotes for strings; we want symbols only
                        if t and not t.startswith('"'):
                            name = t
                            line, col = _position_from_offset(text, s)
                            kind = 'var' if head == 'define' else ('function' if head == 'defun' else 'macro')
                            # Support package-qualified names
                            if ':' in name:
                                pkg, sym = name.split(':', 1)
                                pkg_map = idx.pkg_symbols.setdefault(pkg, {})
                                pkg_map[sym] = SymbolDef(name=name, kind=kind, line=line, col=col)
                            else:
                                idx.symbols[name] = SymbolDef(name=name, kind=kind, line=line, col=col)
                            break
                        j += 1
        elif tok == ')':
            idx.paren_balance -= 1
        elif tok == '"':
            # not produced by tokenizer; strings are matched as one token
            pass
        i += 1

    # simple unmatched quote detection: count unescaped quotes
    # (already tokenized so strings are single tokens; we approximate by scanning raw text)
    raw = text
    quote_count = 0
    esc = False
    for ch in raw:
        if esc:
            esc = False
            continue
        if ch == '\\':
            esc = True
        elif ch == '"':
            quote_count ^= 1  # toggle open/close
    idx.has_unmatched_quote = (quote_count == 1)

    return idx


# Builtin signatures for quick hover/signature help without eval
BUILTIN_SIGNATURES: Dict[str, str] = {
    "+": "(+ &rest nums)",
    "-": "(- x &optional y ...)",
    "*": "(* &rest nums)",
    "/": "(/ x &optional y ...)",
    "mod": "(mod n d)",
    "cons": "(cons x xs)",
    "car": "(car xs)",
    "cdr": "(cdr xs)",
    "list": "(list &rest xs)",
    "apply": "(apply f args)",
    "format": "(format tmpl &rest args)",
}
