from zeta.types.errors import ZetaArityError
from zeta.types.lambda_fn import Lambda

def lambda_form(tail, env, macros, evaluate_fn, _):
    if not tail:
        raise ZetaArityError("lambda requires at least a parameter list")
    params, body = tail[0], tail[1]
    return Lambda(params, body, env)
