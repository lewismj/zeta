from zeta.types.errors import ZetaArityError
from zeta.types.nil import Nil


def define_form(tail, env, macros, evaluate_fn, _):
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
