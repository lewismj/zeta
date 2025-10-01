"""
  Lisp Reader, Lexer and Parser

- Streaming, lazy parsing
- Emits Python primitives instead of Cons cells:

   n.b.No point implementing Cons cells when running in Python,
   A Rust version may implement Nan-boxed primitives, HeapObjects, Cons, paged heap, gc, etc.

    - nil -> None
    - lists -> Python list
    - dotted lists -> (list_part, tail)
    - symbols -> str (wrapped in Symbol)
    - strings -> str
    - numbers -> int/float
    - vectors -> list
    - bitstrings -> list[int]
    - structs -> dict {"type": name, "fields": {...}}
    - complex -> complex
    - quote forms -> ["quote", expr], etc.
    - function shorthand -> ["function", expr]
    - read-time eval -> {"read-eval": expr}
"""

from __future__ import annotations

import re
from typing import Iterator, Optional, Iterable
from zeta.types.errors import ZetaSyntaxError
from zeta import SExpression
from zeta.types.nil import Nil
from zeta.types.symbol import Symbol
from zeta.reader.reader_macros import reader_macros, QUOTE_FORMS


TOKEN_RE = re.compile(
    r"\s*("
    r"(?P<comment>;[^\n]*)"  # single-line comment
    r"|(?P<ml_start>#\|)"  # multi-line comment start
    r"|(?P<quote>[\'`])"  # ' and `
    r"|(?P<unquote>,@|,)"  # , and ,@
    r"|(?P<lparen>\()"  # (
    r"|(?P<rparen>\))"  # )
    r"|(?P<lbrace>\{)"  # {
    r"|(?P<rbrace>\})"  # }
    r'|(?P<string>"(?:\\.|[^\\"])*?")'  # double-quoted strings
    r"|(?P<shared_def>#\d+=)"  # shared structure definition
    r"|(?P<shared_ref>#\d+#)"  # shared structure reference
    r"|(?P<char>#\\(?:newline|space|tab|return|.))"  # character literals, named or single-char
    r"|(?P<vector>#\()"  # vector reader macro
    r"|(?P<set_macro>#\{)"  # set reader macro
    r"|(?P<bitstring>#\*[01]+)"  # bitstring
    r"|(?P<func_shorthand>#\')"  # function shorthand #'
    r"|(?P<read_eval>#\.)"  # read-eval macro
    r"|(?P<complex>#C)"  # complex number
    r"|(?P<radix>#b[01]+|#o[0-7]+|#x[0-9A-Fa-f]+)"  # binary, octal, hex
    r'|(?P<symbol>[^\s(){}\'",;]+)'  # fallback: symbols
    r")",
    re.DOTALL,
)

NAMED_CHARS: dict[str, str] = {
    "space": " ",
    "newline": "\n",
    "tab": "\t",
    "return": "\r",
}


def lex(source: str) -> Iterator[tuple[str, str]]:
    """Token generator: yields (token_type, token_value) tuples."""
    pos = 0
    n = len(source)

    def skip_whitespace_and_comments():
        nonlocal pos
        while pos < n:
            match = TOKEN_RE.match(source, pos)
            if not match:
                if source[pos].isspace():
                    pos += 1
                    continue
                raise SyntaxError(f"Unexpected char at {pos}: {source[pos]!r}")
            if match.group("comment"):
                pos = match.end()
            elif match.group("ml_start"):
                pos = match.end()
                depth = 1
                while depth > 0:
                    if pos >= n:
                        raise SyntaxError("Unterminated multi-line comment")
                    if source.startswith("#|", pos):
                        depth += 1
                        pos += 2
                    elif source.startswith("|#", pos):
                        depth -= 1
                        pos += 2
                    else:
                        pos += 1
            else:
                break

    while pos < n:
        skip_whitespace_and_comments()
        if pos >= n:
            break

        current_char = source[pos]

        # ----------------------
        # Handle backtick / quote
        # ----------------------
        if current_char == "`":
            yield "quote", "`"
            pos += 1
            # suppress a quote token if immediately followed by unquote
            if pos < n and source[pos] == "'":
                if pos + 1 < n and source[pos + 1] in ",@":
                    # skip the quote token, let unquote handle it
                    pos += 1
            continue

        if current_char == "'":
            yield "quote", "'"
            pos += 1
            continue

        # ----------------------
        # Handle unquote / unquote-splicing
        # ----------------------
        if current_char == ",":
            if pos + 1 < n and source[pos + 1] == "@":
                yield "unquote", ",@"
                pos += 2
            else:
                yield "unquote", ","
                pos += 1
            continue

        # ----------------------
        # Regex-based tokens
        # ----------------------
        m = TOKEN_RE.match(source, pos)
        if not m:
            raise SyntaxError(f"Unknown token at {pos}")
        for nm in TOKEN_RE.groupindex:
            if m.group(nm):
                yield nm, m.group(nm)
                pos = m.end()
                break


class TokenStream:
    def __init__(self, token_iter: Iterator[tuple[str, str]]):
        self.tokens = iter(token_iter)
        self.buffer: list[tuple[str, str]] = []
        self.shared_table: dict[int, SExpression] = {}

    def peek(self) -> tuple[Optional[str], Optional[str]]:
        if not self.buffer:
            try:
                self.buffer.append(next(self.tokens))
            except StopIteration:
                return None, None
        return self.buffer[0]

    def advance(self) -> tuple[Optional[str], Optional[str]]:
        if self.buffer:
            return self.buffer.pop(0)
        return next(self.tokens, (None, None))

    def parse_expr(self) -> SExpression:
        tok_type, tok_val = self.peek()
        if tok_type is None:
            return None

        # ------------------------
        # Dispatch reader macros first
        # ------------------------
        if tok_val in reader_macros.macros:
            self.advance()  # consume the macro token
            return reader_macros.dispatch(tok_val, self)

        if tok_type == "symbol":
            self.advance()
            # Special case for nil
            if tok_val.lower() == "nil":
                return Nil
            # Numbers
            if tok_val.isdigit() or (tok_val.startswith("-") and tok_val[1:].isdigit()):
                return int(tok_val)
            try:
                return float(tok_val)
            except ValueError:
                return Symbol(tok_val)

        # Quote forms
        if tok_type in ("quote", "unquote"):
            self.advance()
            expr = self.parse_expr()
            return [QUOTE_FORMS[tok_val], expr]

        # Function shorthand
        if tok_type == "func_shorthand":
            self.advance()
            expr = self.parse_expr()
            return ["function", expr]

        # Read-time eval
        if tok_type == "read_eval":
            self.advance()
            expr = self.parse_expr()
            return {"read-eval": expr}

        # List or dotted list
        if tok_type == "lparen":
            self.advance()
            items = []
            while True:
                if self.peek()[0] == "rparen":
                    self.advance()
                    break
                if self.peek()[0] is None:
                    raise ZetaSyntaxError("Unmatched '('")
                if self.peek()[0] == "symbol" and self.peek()[1] == ".":
                    self.advance()
                    cdr_expr = self.parse_expr()
                    if self.peek()[0] != "rparen":
                        raise ZetaSyntaxError("Expected ')' after dotted cdr")
                    self.advance()
                    return items, cdr_expr  # tuple for a dotted list
                items.append(self.parse_expr())
            return items

        # Shared structures
        if tok_type == "shared_def":
            n_idx = int(tok_val[1:-1])
            self.advance()
            expr = self.parse_expr()
            self.shared_table[n_idx] = expr
            return expr

        if tok_type == "shared_ref":
            n_idx = int(tok_val[1:-1])
            self.advance()
            if n_idx not in self.shared_table:
                raise SyntaxError(f"Undefined shared object #{n_idx}#")
            return self.shared_table[n_idx]

        if tok_type == "char":
            self.advance()
            val = tok_val[2:]  # strip off "#\"
            # Normalize single-character literals and named literals
            if len(val) == 1:
                return val
            return NAMED_CHARS.get(val.lower(), val)

        # String
        if tok_type == "string":
            self.advance()
            return eval(tok_val)

        if tok_type == "vector":
            self.advance()  # consume #(
            vec = []
            while True:
                tok_type, _ = self.peek()
                if tok_type is None:
                    raise ZetaSyntaxError("Unexpected EOF while reading vector")
                if tok_type == "rparen":
                    self.advance()
                    break
                vec.append(self.parse_expr())
            return vec

        # Bitstring
        if tok_type == "bitstring":
            self.advance()
            return [int(c) for c in tok_val[2:]]

        # Radix numbers
        if tok_type == "radix":
            radix_val = tok_val
            self.advance()
            if radix_val.startswith("#b"):
                return int(radix_val[2:], 2)
            elif radix_val.startswith("#o"):
                return int(radix_val[2:], 8)
            elif radix_val.startswith("#x"):
                return int(radix_val[2:], 16)

        # Complex
        if tok_type == "complex":
            self.advance()
            if self.peek()[0] != "lparen":
                raise SyntaxError("Expected '(' after #C")
            self.advance()
            real_part = self.parse_expr()
            imag_part = self.parse_expr()
            if self.peek()[0] != "rparen":
                raise SyntaxError("Expected ')' after #C arguments")
            self.advance()
            return complex(real_part, imag_part)

        raise SyntaxError(f"Unknown token: {tok_type} {tok_val}")

    def parse_all(self) -> Iterator[SExpression]:
        while True:
            tok_type, _ = self.peek()
            if tok_type is None:
                break
            yield self.parse_expr()
