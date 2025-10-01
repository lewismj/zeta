from __future__ import annotations

from typing import Any

from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil
from zeta.types.errors import ZetaArityError, ZetaTypeError


def _to_bool(value: bool) -> Symbol:
    return Symbol("#t") if value else Symbol("#f")


def display(env: Environment, args: list[Any]) -> Symbol:
    """
    (display x y ...) — print arguments without automatic newline.
    Returns #t.
    """
    for a in args:
        # Strings print as-is; others use str() representation
        end = ""
        try:
            print(a, end=end)
        except Exception:
            print(str(a), end=end)
    return Symbol("#t")


def string_ref(env: Environment, args: list[Any]) -> Any:
    """
    (string-ref s i) — return the character at index i of string s.
    """
    if len(args) != 2:
        raise ZetaArityError("string-ref expects exactly 2 arguments")
    s, i = args
    if not isinstance(s, str):
        raise ZetaTypeError("First argument to string-ref must be a string")
    if not isinstance(i, int):
        raise ZetaTypeError("Second argument to string-ref must be an integer index")
    try:
        return s[i]
    except Exception as e:
        raise ZetaTypeError(f"string-ref index out of range: {i}") from e


def char_eq(env: Environment, args: list[Any]) -> Symbol:
    """
    (char=? a b ...) — character equality. Accepts strings of length 1.
    """
    if not args:
        return Symbol("#t")
    # Normalize to 1-char strings
    norm = []
    for a in args:
        if isinstance(a, str) and len(a) == 1:
            norm.append(a)
        else:
            # allow comparing general strings as-is (useful if reader emits single-char strings)
            if isinstance(a, str):
                norm.append(a)
            else:
                return Symbol("#f")
    first = norm[0]
    for x in norm[1:]:
        if x != first:
            return Symbol("#f")
    return Symbol("#t")


def consp(env: Environment, args: list[Any]) -> Symbol:
    """
    (consp x) — true if x is a non-empty list (a cons cell)
    """
    if len(args) != 1:
        raise ZetaArityError("consp expects exactly 1 argument")
    x = args[0]
    if x is Nil:
        return Symbol("#f")
    if isinstance(x, list) and len(x) > 0:
        return Symbol("#t")
    return Symbol("#f")


def register(env: Environment):
    env.update({
        Symbol('display'): display,
        Symbol('string-ref'): string_ref,
        Symbol('char=?'): char_eq,
        Symbol('consp'): consp,
    })
