from __future__ import annotations
from typing import Any
from zeta import Nil
from zeta.types import Environment, Symbol, TransformerFunction
from zeta.errors import ZetaTypeError, ZetaArityError

# -------------------------------
# Equality and basic predicates
# -------------------------------
def equals(env: Environment, expr: list[Any]) -> bool:
    if len(expr) <= 1:
        return True
    first = expr[0]
    for other in expr[1:]:
        if not is_equal(first, other):
            return False
    return True

def not_equals(env: Environment, expr: list[Any]) -> bool:
    return not equals(env, expr)

def is_equal(a: Any, b: Any) -> bool:
    # Recursively check equality
    if a is b:
        return True
    if type(a) != type(b):
        return False
    try:
        if hasattr(a, "__iter__") and hasattr(b, "__iter__"):
            return all(is_equal(x, y) for x, y in zip(a, b)) and len(list(a)) == len(list(b))
    except TypeError:
        # Not iterable
        pass
    return a == b

def is_nil(env: Environment, expr: list[Any]) -> bool:
    return len(expr) == 1 and expr[0] is Nil

def is_symbol(env: Environment, expr: list[Any]) -> bool:
    from zeta.types import Symbol
    return len(expr) == 1 and isinstance(expr[0], Symbol)

# -------------------------------
# Arithmetic
# -------------------------------
def add(env: Environment, expr: list[Any]) -> Any:
    try:
        return sum(expr)
    except TypeError:
        raise ZetaTypeError("All arguments to + must be numbers")

def sub(env: Environment, expr: list[Any]) -> Any:
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

def mul(env: Environment, expr: list[Any]) -> Any:
    result = 1
    try:
        for x in expr:
            result *= x
        return result
    except TypeError:
        raise ZetaTypeError("All arguments to * must be numbers")

def div(env: Environment, expr: list[Any]) -> Any:
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
def lt(env: Environment, expr: list[Any]) -> bool:
    return all(a < b for a, b in zip(expr, expr[1:]))

def lte(env: Environment, expr: list[Any]) -> bool:
    return all(a <= b for a, b in zip(expr, expr[1:]))

def gt(env: Environment, expr: list[Any]) -> bool:
    return all(a > b for a, b in zip(expr, expr[1:]))

def gte(env: Environment, expr: list[Any]) -> bool:
    return all(a >= b for a, b in zip(expr, expr[1:]))

# -------------------------------
# Boolean logic
# -------------------------------
def logical_and(env: Environment, expr: list[Any]) -> bool:
    return all(expr)

def logical_or(env: Environment, expr: list[Any]) -> bool:
    return any(expr)

def logical_not(env: Environment, expr: list[Any]) -> bool:
    if len(expr) != 1:
        raise ZetaArityError("not requires exactly 1 argument")
    return not expr[0]

# -------------------------------
# List operations
# -------------------------------
def cons(env: Environment, expr: list[Any]) -> list[Any]:
    if len(expr) != 2:
        raise ZetaArityError("cons requires exactly 2 arguments")
    head, tail = expr
    if tail is Nil:
        return [head]
    if not hasattr(tail, "__iter__"):
        raise ZetaTypeError("Second argument to cons must be a list or Nil")
    return [head] + list(tail)

def car(env: Environment, expr: list[Any]) -> Any:
    if len(expr) != 1:
        raise ZetaArityError("car requires exactly 1 argument")
    if not expr[0]:
        return Nil
    return expr[0][0]

def cdr(env: Environment, expr: list[Any]) -> Any:
    if len(expr) != 1:
        raise ZetaArityError("cdr requires exactly 1 argument")
    if not expr[0]:
        return Nil
    return expr[0][1:] if len(expr[0]) > 1 else []

def list_builtin(env: Environment, expr: list[Any]) -> list[Any]:
    return list(expr)

# -------------------------------
# Function application
# -------------------------------
def apply(env: Environment, expr: list[Any]) -> Any:
    if len(expr) < 2:
        raise ZetaArityError("apply requires at least 2 arguments: func and list of args")
    func = expr[0]
    args = expr[1]
    if not hasattr(args, "__iter__"):
        raise ZetaTypeError("Second argument to apply must be iterable")
    return func(env, list(args))

# -------------------------------
# Registration
# -------------------------------
def register(env: Environment):
    env.update({
        Symbol('+'): add,
        Symbol('-'): sub,
        Symbol('*'): mul,
        Symbol('/'): div,
        Symbol('='): equals,
        Symbol('/='): not_equals,
        Symbol('<'): lt,
        Symbol('<='): lte,
        Symbol('>'): gt,
        Symbol('>='): gte,
        Symbol('and'): logical_and,
        Symbol('or'): logical_or,
        Symbol('not'): logical_not,
        Symbol('cons'): cons,
        Symbol('car'): car,
        Symbol('cdr'): cdr,
        Symbol('list'): list_builtin,
        Symbol('apply'): apply,
        Symbol('nil?'): is_nil,
        Symbol('symbol?'): is_symbol,
    })
