from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.lambda_fn import Lambda
from zeta.types.errors import ZetaArityError

def apply_lambda(fn, args, macros, envaluate_fn):
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

    return envaluate_fn(fn.body, local_env, macros)