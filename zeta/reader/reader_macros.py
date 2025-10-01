from __future__ import annotations

from zeta import SExpression
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaSyntaxError
from zeta.types.lambda_fn import Lambda


def _subst(expr: SExpression, bindings: dict[Symbol, SExpression]) -> SExpression:
    """Shallow syntactic substitution supporting lists, tuples, dicts, and Symbols."""
    if isinstance(expr, Symbol):
        return bindings.get(expr, expr)
    if isinstance(expr, list):
        return [_subst(x, bindings) for x in expr]
    if isinstance(expr, tuple) and len(expr) == 2:
        lst, tail = expr
        return ([_subst(x, bindings) for x in lst], _subst(tail, bindings))
    if isinstance(expr, dict):
        return {k: _subst(v, bindings) for k, v in expr.items()}
    return expr


class ReaderMacros:
    """
    Registry of reader macros as first-class Lambdas.
    Maps special characters or sequences (like ', `, #') to
    Lambda bodies that consume the next parsed expression(s) syntactically.
    """

    def __init__(self):
        self.macros: dict[str, Lambda] = {}

    def define(self, char: str, fn: Lambda) -> None:
        """Register a reader macro for a given character or sequence."""
        self.macros[char] = fn

    def is_macro(self, char: str) -> bool:
        return char in self.macros

    def dispatch(self, char: str, stream: "TokenStream") -> SExpression:
        """Invoke the reader macro on the TokenStream using Lambda substitution."""
        # Special-case built-in structural macro #s that must control parsing
        if char == "#s":
            # Mapping semantics for #s:
            # - Syntax: #s(<name> <field1> <val1> <field2> <val2> ...)
            # - Expansion: (make-<name> <val1> <val2> ...)
            # - Note: Field identifiers are currently used only as syntactic anchors; values
            #         are passed positionally in the order written. This matches defstruct's
            #         constructor (make-<name>) which accepts positional args in the order
            #         fields were declared. The reader does not know the field declaration
            #         order, so callers must supply values in that order.
            # - Accessors created by defstruct (<name>-<field>) operate on the resulting dict.
            #
            # Expect '(' after #s
            tok_type, _ = stream.peek()
            if tok_type != "lparen":
                raise ZetaSyntaxError("Expected '(' after #s")
            stream.advance()  # consume '('

            # Parse struct type name
            struct_name_expr = stream.parse_expr()
            if not isinstance(struct_name_expr, Symbol):
                raise ZetaSyntaxError(
                    f"Struct name must be a Symbol, got {struct_name_expr}"
                )
            struct_name = struct_name_expr.id

            # Parse fields: field-value pairs (field symbols are not evaluated here)
            fields = []
            while stream.peek()[0] != "rparen":
                key_expr = stream.parse_expr()
                if not isinstance(key_expr, Symbol):
                    raise ZetaSyntaxError(
                        f"Struct field name must be a Symbol, got {key_expr}"
                    )
                value_expr = stream.parse_expr()
                fields.append((key_expr.id, value_expr))

            stream.advance()  # consume ')'

            # Convert to positional constructor call in the written order
            args = [value for _, value in fields]
            constructor_name = Symbol(f"make-{struct_name}")
            return [constructor_name] + args

        if char not in self.macros:
            raise ValueError(f"No reader macro defined for {char!r}")

        lam = self.macros[char]
        # Parse as many arguments as the Lambda declares
        args: list[SExpression] = []
        for _ in lam.formals:
            args.append(stream.parse_expr())
        bindings = {param: arg for param, arg in zip(lam.formals, args)}
        return _subst(lam.body, bindings)


# -------------------------
# Single global instance
# -------------------------
reader_macros: ReaderMacros = ReaderMacros()

QUOTE_FORMS: dict[str, Symbol] = {
    "'": Symbol("quote"),
    "`": Symbol("quasiquote"),
    ",": Symbol("unquote"),
    ",@": Symbol("unquote-splicing"),
}

# Quote forms: ', `, , ,@
for key, name in QUOTE_FORMS.items():
    x = Symbol("x")
    reader_macros.define(key, Lambda([x], [name, x]))

# Function shorthand #: #'expr => (function expr)
_x = Symbol("x")
reader_macros.define("#'", Lambda([_x], ["function", _x]))

# Read-time evaluation: #.(expr) => {"read-eval": expr}
_y = Symbol("x")
reader_macros.define("#.", Lambda([_y], {"read-eval": _y}))

# Placeholder registration for #s so dispatch special-case triggers
reader_macros.define("#s", Lambda([], []))
