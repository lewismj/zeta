from zeta import EvaluatorFn
from zeta import SExpression, LispValue
from zeta.types.errors import ZetaArityError
from zeta.types.nil import Nil
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment


def define_form(
    tail: list[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    _: bool,
) -> LispValue:
    """
    (define name value)
    Tail-call awareness is irrelevant here, since define does not produce a value to be tail-called.
    """
    if len(tail) != 2:
        raise ZetaArityError("define requires exactly 2 arguments")

    name, val_expr = tail
    value = evaluate_fn(val_expr, env, macros)  # normal evaluation
    env.define(name, value)
    return Nil
