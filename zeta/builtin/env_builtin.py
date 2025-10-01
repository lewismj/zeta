from __future__ import annotations
from typing import Any

from zeta import LispValue
from zeta.types.lambda_fn import Lambda
from zeta.types.nil import Nil
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaTypeError, ZetaArityError

# -----------------------------
# Equality and basic predicates
# -------------------------------


def equals(env: Environment, expr: list[LispValue]) -> Symbol:
    if len(expr) <= 1:
        return Symbol("#t")
    first = expr[0]
    for other in expr[1:]:
        if not is_equal(first, other):
            return Symbol("#f")
    return Symbol("#t")


def not_equals(env: Environment, expr: list[LispValue]) -> Symbol:
    result = equals(env, expr)
    return Symbol("#f") if result == Symbol("#t") else Symbol("#t")


def is_equal(a, b):
    if a is b:
        return True
    if type(a) != type(b):
        return False

    if isinstance(a, list) and isinstance(b, list):
        if len(a) != len(b):
            return False
        return all(is_equal(x, y) for x, y in zip(a, b))

    return a == b


def is_nil(env: Environment, expr: list[LispValue]) -> Symbol:
    return Symbol("#t") if len(expr) == 1 and expr[0] is Nil else Symbol("#f")


def is_symbol(env: Environment, expr: list[Any]) -> Symbol:
    return (
        Symbol("#t") if len(expr) == 1 and isinstance(expr[0], Symbol) else Symbol("#f")
    )


# -------------------------------
# Arithmetic
# -------------------------------
def add(env: Environment, expr: list[LispValue]) -> LispValue:
    try:
        return sum(expr)
    except TypeError:
        raise ZetaTypeError("All arguments to + must be numbers")


def sub(env: Environment, expr: list[LispValue]) -> LispValue:
    if not expr:
        raise ZetaArityError("- requires at least 1 argument")
    try:
        if len(expr) == 1:
            return -expr[0]
        result = expr[0]
        for x in expr[1:]:
            result -= x
        return result
    except TypeError:
        raise ZetaTypeError("All arguments to - must be numbers")


def mul(env: Environment, expr: list[LispValue]) -> LispValue:
    result = 1
    try:
        for x in expr:
            result *= x
        return result
    except TypeError:
        raise ZetaTypeError("All arguments to * must be numbers")


def div(env: Environment, expr: list[LispValue]) -> LispValue:
    if not expr:
        raise ZetaArityError("/ requires at least 1 argument")
    try:
        if len(expr) == 1:
            return 1 / expr[0]
        result = expr[0]
        for x in expr[1:]:
            result /= x
        return result
    except TypeError:
        raise ZetaTypeError("All arguments to / must be numbers")
    except ZeroDivisionError:
        raise ZeroDivisionError("Division by zero")


# -------------------------------
# Comparison
# -------------------------------
def lt(env: Environment, expr: list[LispValue]) -> Symbol:
    return Symbol("#t") if all(a < b for a, b in zip(expr, expr[1:])) else Symbol("#f")


def lte(env: Environment, expr: list[LispValue]) -> Symbol:
    return Symbol("#t") if all(a <= b for a, b in zip(expr, expr[1:])) else Symbol("#f")


def gt(env: Environment, expr: list[LispValue]) -> Symbol:
    return Symbol("#t") if all(a > b for a, b in zip(expr, expr[1:])) else Symbol("#f")


def gte(env: Environment, expr: list[LispValue]) -> Symbol:
    return Symbol("#t") if all(a >= b for a, b in zip(expr, expr[1:])) else Symbol("#f")


# -------------------------------
# Boolean logic
# -------------------------------
def logical_and(env: Environment, expr: list[LispValue]) -> Symbol:
    for e in expr:
        if e in (Nil, Symbol("#f")):
            return Symbol("#f")
    return Symbol("#t")


def logical_or(env: Environment, expr: list[LispValue]) -> Symbol:
    for e in expr:
        if e not in (Nil, Symbol("#f")):
            return Symbol("#t")
    return Symbol("#f")


def logical_not(env: Environment, expr: list[LispValue]) -> Symbol:
    if len(expr) != 1:
        raise ZetaArityError("not requires exactly 1 argument")
    val = expr[0]
    return Symbol("#f") if val not in (Nil, Symbol("#f")) else Symbol("#t")


# -------------------------------
# List operations
# -------------------------------
def cons(env: Environment, expr: list[LispValue]) -> list[LispValue]:
    if len(expr) != 2:
        raise ZetaArityError("cons requires exactly 2 arguments")
    head, tail = expr
    if tail is Nil:
        return [head]
    if not hasattr(tail, "__iter__"):
        raise ZetaTypeError("Second argument to cons must be a list or Nil")
    return [head] + list(tail)


def car(env: Environment, expr: list[LispValue]) -> LispValue:
    if len(expr) != 1:
        raise ZetaArityError("car requires exactly 1 argument")
    xs = expr[0]
    if xs is Nil or not xs:
        return Nil
    return xs[0]


def cdr(env: Environment, expr: list[LispValue]) -> LispValue:
    if len(expr) != 1:
        raise ZetaArityError("cdr requires exactly 1 argument")
    xs = expr[0]
    if xs is Nil or not xs:
        return Nil
    return xs[1:] if len(xs) > 1 else Nil


def list_builtin(env: Environment, expr: list[LispValue]) -> list[LispValue]:
    return list(expr)


# -------------------------------
# Function application
# -------------------------------
def apply(env: Environment, expr: list[LispValue]) -> LispValue:
    if len(expr) < 2:
        raise ZetaArityError(
            "apply requires at least 2 arguments: func and list of args"
        )
    func = expr[0]
    args = expr[1]
    if not hasattr(args, "__iter__"):
        raise ZetaTypeError("Second argument to apply must be iterable")
    return func(env, list(args))


def join(env: Environment, expr: list[LispValue]) -> list[LispValue]:
    """
    Concatenate multiple lists.
    Nil is treated as the empty list.
    """
    result = []
    for item in expr:
        if item is Nil:
            continue
        if not isinstance(item, list):
            raise ZetaTypeError(
                f"join expects list arguments, got {type(item).__name__}"
            )
        result.extend(item)
    return result


def atom(env: Environment, args: list[LispValue]) -> Symbol:
    """
    (atom x) -> returns True if x is an atom.
    Definition of atom:
      - Not a list
      - Lambda forms (unevaluated) are atoms
      - Symbols are atoms
      - Evaluated functions (callables) are NOT atoms, as they are reducible
      - Numbers, strings, booleans are atoms
    """
    if len(args) != 1:
        return Symbol("#f")  # multiple args cannot be an atom
    x = args[0]
    match x:
        case list():
            return Symbol("#f")
        case Lambda():
            return Symbol("#t")
        case Symbol():
            return Symbol("#t")
        case _ if callable(x):
            return Symbol("#f")
        case _:
            return Symbol("#t")


def null(env: Environment, args: list[LispValue]) -> Symbol:
    if len(args) != 1:
        return Symbol("#f")
    x = args[0]
    return Symbol("#t") if x is Nil or x == [] else Symbol("#f")


def symbol_to_string(env: Environment, args: list[LispValue]) -> LispValue:
    """(symbol->string x) -> string representation of symbol x"""
    if len(args) != 1:
        return Symbol("#f")
    x = args[0]
    if not isinstance(x, Symbol):
        return Symbol("#f")
    return str(x.id)  # or x.name depending on your Symbol class


def string_to_symbol(env: Environment, args: list[LispValue]) -> LispValue:
    """(string->symbol x) -> Symbol corresponding to string x"""
    if len(args) != 1:
        return Symbol("#f")
    x = args[0]
    if not isinstance(x, str):
        return Symbol("#f")
    return Symbol(x)


# -------------------------------
# Registration
# -------------------------------
def register(env: Environment) -> None:
    env.update(
        {
            Symbol("+"): add,
            Symbol("-"): sub,
            Symbol("*"): mul,
            Symbol("/"): div,
            Symbol("="): equals,
            Symbol("=="): equals,
            Symbol("eq"): equals,
            Symbol("/="): not_equals,
            Symbol("<"): lt,
            Symbol("<="): lte,
            Symbol(">"): gt,
            Symbol(">="): gte,
            Symbol("and"): logical_and,
            Symbol("or"): logical_or,
            Symbol("not"): logical_not,
            Symbol("cons"): cons,
            Symbol("car"): car,
            Symbol("cdr"): cdr,
            Symbol("head"): car,  # alias for car
            Symbol("tail"): cdr,  # alias for cdr,
            Symbol("list"): list_builtin,
            Symbol("apply"): apply,
            Symbol("nil?"): is_nil,
            Symbol("symbol?"): is_symbol,
            Symbol("join"): join,
            Symbol("atom"): atom,
            Symbol("null?"): null,
            Symbol("symbol->string"): symbol_to_string,
            Symbol("string->symbol"): string_to_symbol,
        }
    )
    env.define(Symbol("#t"), Symbol("#t"))
    env.define(Symbol("#f"), Symbol("#f"))
