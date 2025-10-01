from zeta.types.errors import ZetaArityError
from zeta.types.lambda_fn import Lambda

from zeta import EvaluatorFn
from zeta import SExpression, LispValue
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.nil import Nil
from zeta.types.symbol import Symbol


def lambda_form(
    tail: list[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    _: bool,
) -> LispValue:


    # In Lisp, (lambda (params) body...) allows zero or more body forms.
    # When there are multiple forms, the body is an implicit progn.
    # When there are no body forms, the result of invoking the function is nil.
    if not tail:
        raise ZetaArityError("lambda requires at least a parameter list")

    params = tail[0]
    body_forms = tail[1:]

    if not body_forms:
        body = Nil
    elif len(body_forms) == 1:
        body = body_forms[0]
    else:
        body = [Symbol("progn"), *body_forms]

    return Lambda(params, body, env)
