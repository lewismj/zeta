"""Built-in functions for the Zeta runtime environment.

This module defines core arithmetic, comparison, list processing, predicates,
application helpers, and registration utilities exposed to Lisp code.
"""
from __future__ import annotations
from typing import Any

from zeta import LispValue
from zeta.types.lambda_fn import Lambda
from zeta.types.nil import Nil
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaTypeError, ZetaArityError
from zeta.evaluation.apply import apply as apply_engine
from zeta.evaluation.evaluator import evaluate0
from zeta.types.tail_call import TailCall
from zeta.types.macro_environment import MacroEnvironment


def equals(env: Environment, expr: list[LispValue]) -> Symbol:
    """Return #t if all arguments are equal (or zero/one arg), else #f."""
    if len(expr) <= 1:
        return Symbol("#t")
    first = expr[0]
    for other in expr[1:]:
        if not is_equal(first, other):
            return Symbol("#f")
    return Symbol("#t")


def not_equals(env: Environment, expr: list[LispValue]) -> Symbol:
    """Return #t if any adjacent arguments differ; logical negation of equals."""
    result = equals(env, expr)
    return Symbol("#f") if result == Symbol("#t") else Symbol("#t")


def is_equal(a, b):
    """Deep equality for Lisp values, with element-wise comparison for lists."""
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
    """Predicate: (#t) if the single argument is Nil, else #f."""
    return Symbol("#t") if len(expr) == 1 and expr[0] is Nil else Symbol("#f")


def is_symbol(env: Environment, expr: list[Any]) -> Symbol:
    """Predicate: (#t) if the single argument is a Symbol, else #f."""
    return (
        Symbol("#t") if len(expr) == 1 and isinstance(expr[0], Symbol) else Symbol("#f")
    )


# -------------------------------
# Arithmetic
# -------------------------------
def add(env: Environment, expr: list[LispValue]) -> LispValue:
    """Return the numeric sum of all arguments; errors if any arg is non-numeric."""
    try:
        return sum(expr)
    except TypeError:
        raise ZetaTypeError("All arguments to + must be numbers")


def sub(env: Environment, expr: list[LispValue]) -> LispValue:
    """Subtract all subsequent numbers from the first; unary negation for one arg."""
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
    """Return the product of all arguments; errors if any arg is non-numeric."""
    result = 1
    try:
        for x in expr:
            result *= x
        return result
    except TypeError:
        raise ZetaTypeError("All arguments to * must be numbers")


def div(env: Environment, expr: list[LispValue]) -> LispValue:
    """Divide left-to-right; with one arg returns reciprocal; checks arity and zero division."""
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

def mod(env: Environment, expr: list[LispValue]) -> LispValue:
    """(mod n d) => n % d. Exactly 2 integer arguments."""
    if len(expr) != 2:
        raise ZetaArityError("mod requires exactly 2 arguments")
    n, d = expr
    if not isinstance(n, int) or not isinstance(d, int):
        raise ZetaTypeError("All arguments to mod must be integers")
    if d == 0:
        raise ZeroDivisionError("Modulo by zero")
    return n % d

def lt(env: Environment, expr: list[LispValue]) -> Symbol:
    """Chainable less-than: returns #t if a0 < a1 < a2 ... holds for all pairs."""
    return Symbol("#t") if all(a < b for a, b in zip(expr, expr[1:])) else Symbol("#f")


def lte(env: Environment, expr: list[LispValue]) -> Symbol:
    """Chainable less-or-equal: returns #t if a0 <= a1 <= a2 ... holds for all pairs."""
    return Symbol("#t") if all(a <= b for a, b in zip(expr, expr[1:])) else Symbol("#f")


def gt(env: Environment, expr: list[LispValue]) -> Symbol:
    """Chainable greater-than: returns #t if a0 > a1 > a2 ... holds for all pairs."""
    return Symbol("#t") if all(a > b for a, b in zip(expr, expr[1:])) else Symbol("#f")


def gte(env: Environment, expr: list[LispValue]) -> Symbol:
    """Chainable greater-or-equal: returns #t if a0 >= a1 >= a2 ... holds for all pairs."""
    return Symbol("#t") if all(a >= b for a, b in zip(expr, expr[1:])) else Symbol("#f")

def logical_not(env: Environment, expr: list[LispValue]) -> Symbol:
    """Logical NOT for a single value; only Nil and #f are considered falsey."""
    if len(expr) != 1:
        raise ZetaArityError("Not requires exactly 1 argument")
    val = expr[0]
    return Symbol("#f") if val not in (Nil, Symbol("#f")) else Symbol("#t")


def cons(env: Environment, expr: list[LispValue]) -> list[LispValue]:
    """Construct a new list by prepending head to tail (list or Nil)."""
    if len(expr) != 2:
        raise ZetaArityError("Cons requires exactly 2 arguments")
    head, tail = expr
    if tail is Nil:
        return [head]
    if not hasattr(tail, "__iter__"):
        raise ZetaTypeError("Second argument to cons must be a list or Nil")
    return [head] + list(tail)


def car(env: Environment, expr: list[LispValue]) -> LispValue:
    """Return the first element of a list; Nil for empty or Nil."""
    if len(expr) != 1:
        raise ZetaArityError("Car requires exactly 1 argument")
    xs = expr[0]
    if xs is Nil or not xs:
        return Nil
    return xs[0]


def cdr(env: Environment, expr: list[LispValue]) -> LispValue:
    """Return the tail of a list (all but first); Nil for empty or singletons."""
    if len(expr) != 1:
        raise ZetaArityError("Cdr requires exactly 1 argument")
    xs = expr[0]
    if xs is Nil or not xs:
        return Nil
    return xs[1:] if len(xs) > 1 else Nil


def list_builtin(env: Environment, expr: list[LispValue]) -> list[LispValue]:
    """Construct a list from the provided arguments (identity)."""
    return list(expr)


def apply(env: Environment, expr: list[LispValue]) -> LispValue:
    """Builtin apply: (apply f args) delegates to central engine semantics.

    Expects exactly two arguments: function and list of args. If the callee
    produces a TailCall, this function steps the trampoline using evaluate0
    until a concrete value is produced.
    """
    if len(expr) != 2:
        raise ZetaArityError(
            "apply requires exactly 2 arguments: function and list of args"
        )
    func = expr[0]
    args = expr[1]
    if not isinstance(args, list):
        raise ZetaTypeError("Second argument to apply must be a list")

    # Use a lightweight MacroEnvironment; in normal evaluation the special form
    # is used and carries the real macro env. Tests invoking the builtin directly
    # typically target builtin callables and simple lambdas.
    macros = MacroEnvironment()

    # Delegate; trampoline if a TailCall is produced
    result = apply_engine(func, list(args), env, macros, evaluate0, False)
    while isinstance(result, TailCall):
        # Step the trampoline manually using evaluate0
        result = evaluate0(result.fn.body, result.env, result.macros, True)
    return result


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
    """Predicate: (#t) if the single argument is Nil or the empty list, else #f."""
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

def _to_string(x: LispValue) -> str:
    """Convert a Lisp value to its printable string form (Nil -> "nil", Symbol -> id)."""
    if x is Nil:
        return "nil"
    if isinstance(x, Symbol):
        return x.id
    return str(x)


def print_builtin(env: Environment, args: list[LispValue]) -> LispValue:
    """Print space-separated representations of args followed by newline; returns Nil."""
    text = " ".join(_to_string(a) for a in args)
    print(text)
    return Nil


def format_builtin(env: Environment, args: list[LispValue]) -> str:
    """Format or join values.

    - No args -> empty string
    - First arg contains braces -> use Python str.format on remaining args
    - Otherwise -> join all args with spaces
    """
    if not args:
        return ""
    first = args[0]
    # If first is a template string and appears to have braces, try formatting
    if isinstance(first, str) and ("{" in first and "}" in first):
        try:
            return first.format(*[a if not isinstance(a, Symbol) else a.id for a in args[1:]])
        except Exception as e:
            raise ZetaTypeError(f"Format error: {e}")
    # Otherwise, return the space-joined string representation of all args
    return " ".join(_to_string(a) for a in args)


def register(env: Environment) -> None:
    """Register all builtin functions and constants into the given environment."""
    env.update(
        {
            Symbol("+"): add,
            Symbol("-"): sub,
            Symbol("*"): mul,
            Symbol("/"): div,
            Symbol("mod"): mod,
            Symbol("="): equals,
            Symbol("=="): equals,
            Symbol("eq"): equals,
            Symbol("/="): not_equals,
            Symbol("<"): lt,
            Symbol("<="): lte,
            Symbol(">"): gt,
            Symbol(">="): gte,
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
            Symbol("print"): print_builtin,
            Symbol("format"): format_builtin,
        }
    )
    env.define(Symbol("#t"), Symbol("#t"))
    env.define(Symbol("#f"), Symbol("#f"))
