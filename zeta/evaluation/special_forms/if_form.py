from zeta.types.errors import ZetaArityError
from zeta.types.nil import Nil
from zeta.types.symbol import Symbol


def if_form(tail, env, macros, evaluate_fn):
    if len(tail) < 2:
        raise ZetaArityError("if requires a condition and a then-expression")

    cond = evaluate_fn(tail[0], env, macros)
    # Lisp truthiness: anything not Nil or #f is true
    is_true = cond not in (Nil, Symbol("#f"))

    if is_true:
        return evaluate_fn(tail[1], env, macros)
    elif len(tail) > 2:
        return evaluate_fn(tail[2], env, macros)
    else:
        return Symbol("#f")  # default "false" if no else