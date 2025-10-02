from zeta import SExpression
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil


def _is_truthy(val: SExpression) -> bool:
    return not (val is Nil or (isinstance(val, Symbol) and val == Symbol("#f")))


def and_form(tail: list[SExpression], env: Environment, macros: MacroEnvironment, evaluate_fn, is_tail_call: bool=False) -> SExpression:
    """Short-circuiting logical AND special form.

    (and a b c ...) evaluates each operand left-to-right until a falsey value
    (Nil or #f) is found, which is returned immediately. If all operands are
    truthy, returns the value of the last operand. With zero operands, returns #t.
    """
    if not tail:
        return Symbol("#t")

    last_index = len(tail) - 1
    result: SExpression = Symbol("#t")
    for i, expr in enumerate(tail):
        # Only propagate tail position to the final operand
        val = evaluate_fn(expr, env, macros, is_tail_call=(is_tail_call and i == last_index))
        if not _is_truthy(val):
            return Symbol("#f")
        result = val
    return result


def or_form(tail: list[SExpression], env: Environment, macros: MacroEnvironment, evaluate_fn, is_tail_call: bool=False) -> SExpression:
    """Short-circuiting logical OR special form.

    (or a b c ...) evaluates each operand left-to-right and returns the first
    truthy value (not Nil and not #f). If none are truthy, returns #f. With zero
    operands, returns #f.
    """
    if not tail:
        return Symbol("#f")

    last_index = len(tail) - 1
    for i, expr in enumerate(tail):
        val = evaluate_fn(expr, env, macros, is_tail_call=(is_tail_call and i == last_index))
        if _is_truthy(val):
            return val
    return Symbol("#f")
