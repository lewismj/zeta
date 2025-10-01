from typing import Callable
from zeta import SExpression
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaSyntaxError


ReaderMacroFunction = Callable[['TokenStream'], SExpression]

class ReaderMacros:
    """
    Registry of reader macros.
    Maps special characters or sequences (like ', `, #') to
    functions that consume a TokenStream and return an s-expression.
    """
    def __init__(self):
        self.macros: dict[str, ReaderMacroFunction] = {}

    def define(self, char: str, fn: ReaderMacroFunction) -> None:
        """Register a reader macro for a given character or sequence."""
        self.macros[char] = fn

    def is_macro(self, char: str) -> bool:
        return char in self.macros

    def dispatch(self, char: str, stream: 'TokenStream') -> SExpression:
        """Invoke the reader macro on the TokenStream."""
        if char not in self.macros:
            raise ValueError(f"No reader macro defined for {char!r}")
        return self.macros[char](stream)


# -------------------------
# Single global instance
# -------------------------
reader_macros = ReaderMacros()

QUOTE_FORMS = {
    "'": Symbol("quote"),
    "`": Symbol("quasiquote"),
    ",": Symbol("unquote"),
    ",@": Symbol("unquote-splicing")
}

# Quote forms: ', `, , ,@
for key, name in QUOTE_FORMS.items():
    reader_macros.define(key, lambda stream, n=name: [n, stream.parse_expr()])

# Function shorthand #: #'expr => (function expr)
reader_macros.define("#'", lambda stream: ["function", stream.parse_expr()])

# Read-time evaluation: #.(expr) => {"read-eval": expr}
reader_macros.define("#.", lambda stream: {"read-eval": stream.parse_expr()})


def read_struct_macro(stream: 'TokenStream') -> SExpression:
    """
    Reader macro for #s(struct-name ...).

    Expands into a constructor call list: (make-<struct-name> args...)
    """
    # Expect '(' after #s
    tok_type, _ = stream.peek()
    if tok_type != "lparen":
        raise ZetaSyntaxError("Expected '(' after #s")
    stream.advance()  # consume '('

    # Parse struct type name
    struct_name_expr = stream.parse_expr()
    if not isinstance(struct_name_expr, Symbol):
        raise ZetaSyntaxError(f"Struct name must be a Symbol, got {struct_name_expr}")
    struct_name = struct_name_expr.id

    # Parse fields: keyword-value pairs
    fields = []
    while stream.peek()[0] != "rparen":
        key_expr = stream.parse_expr()
        if not isinstance(key_expr, Symbol):
            raise ZetaSyntaxError(f"Struct field name must be a Symbol, got {key_expr}")
        value_expr = stream.parse_expr()
        fields.append((key_expr.id, value_expr))  # store as (field_name, value)

    stream.advance()  # consume ')'

    # Convert keyword-value dict into ordered positional args
    # Here we just output in the order written; defstruct_form expects positional args
    args = [value for _, value in fields]

    constructor_name = Symbol(f"make-{struct_name}")
    return [constructor_name] + args

reader_macros.define("#s", read_struct_macro)