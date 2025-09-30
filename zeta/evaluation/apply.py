from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.lambda_fn import Lambda
from zeta.types.errors import ZetaArityError, ZetaTypeError
from zeta.types.tail_call import TailCall


def apply_lambda(fn, args, macros, evaluate_fn, is_tail_call: bool):
    formals = list(fn.formals)
    supplied = list(args)
    remaining_formals = []

    local_env = Environment(outer=fn.env)

    while formals:
        formal = formals.pop(0)
        if formal == Symbol("&rest"):
            name = formals.pop(0)
            local_env.define(name, supplied)
            supplied = []
            break
        if supplied:
            local_env.define(formal, supplied.pop(0))
        else:
            remaining_formals.append(formal)

    if remaining_formals:
        return Lambda(remaining_formals, fn.body, local_env)

    if supplied:
        raise ZetaArityError(f"Too many arguments: {supplied}")

    if is_tail_call:
        return TailCall(fn, args, local_env, macros)
    else:
        return evaluate_fn(fn.body, local_env, macros, True)


def apply(head, args, env, macros, evaluate_fn, tail: bool = False):
    if isinstance(head, Lambda):
        return apply_lambda(head, args, macros, evaluate_fn, tail)
    elif callable(head):
        return head(env, args)
    else:
        raise ZetaTypeError(f"Cannot apply non-function {head}")
