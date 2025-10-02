"""Builtin macro transformers for Zeta (implemented in Python).
"""

from typing import Any
from zeta import SExpression
from zeta.types.errors import ZetaArityError, ZetaTypeError
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol


def let_macro(args: list[SExpression], env: Any) -> SExpression:
    """
    (let ((var1 val1) (var2 val2) ...) body...)
    => ((lambda (var1 var2 ...) body...) val1 val2 ...)
    """
    if len(args) < 2:
        raise ZetaArityError("Let requires bindings and at least one body form")

    bindings = args[0]
    body = list(args[1:])

    if not isinstance(bindings, list):
        raise ZetaTypeError("Let bindings must be a list")

    vars_ = []
    vals_ = []
    for b in bindings:
        if not isinstance(b, list) or len(b) != 2:
            raise ZetaTypeError(f"Let binding must be a list of two elements, got {b}")
        var, val = b
        if not isinstance(var, Symbol):
            raise ZetaTypeError(f"Let binding name must be a Symbol, got {var}")
        vars_.append(var)
        vals_.append(val)

    return [[Symbol("lambda"), vars_] + body] + vals_


def defun_macro(args: list[SExpression], env: Any) -> SExpression:
    """(defun name (params) body...) -> define a function named `name`.

    Expands into (define name (lambda (params) body...)).

    N.B.
        This illustrates how you can 'short-circuit' macro expansion in
    the evaluation for specific cases. Here we're simply providing the
    syntax directly. We therefore don't need to quasi-quote - we're
    doing the expansion that defmacro would normally do.
    """
    if len(args) < 3:
        raise ZetaArityError(
            "defun requires at least 3 arguments: (defun name (params) body...)"
        )

    name = args[0]  # function name (symbol)
    params = args[1]  # parameter list
    body = args[2:]  # body expressions

    # Expand (defun name (params) body...)
    # into (define name (lambda (params) body...))
    return [Symbol("define"), name, [Symbol("lambda"), params] + body]


def register(macro_env: MacroEnvironment) -> None:
    """Register builtin macros in the provided MacroEnvironment."""
    macro_env.define_macro(Symbol("defun"), defun_macro)
    macro_env.define_macro(Symbol("let"), let_macro)
