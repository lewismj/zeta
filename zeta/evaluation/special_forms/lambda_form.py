from zeta.types.errors import ZetaArityError
from zeta.types.lambda_fn import Lambda

from zeta import EvaluatorFn
from zeta import SExpression, LispValue
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment


def lambda_form(
    tail: list[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    _: bool,
) -> LispValue:
    if not tail:
        raise ZetaArityError("lambda requires at least a parameter list")
    params, body = tail[0], tail[1]
    return Lambda(params, body, env)
