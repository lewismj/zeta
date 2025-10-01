from zeta import EvaluatorFn
from zeta import SExpression, LispValue
from zeta.types.errors import ZetaArityError
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment


def eval_form(
    tail: list[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    is_tail_call: bool = False,
) -> LispValue:
    if len(tail) != 1:
        raise ZetaArityError("eval expects exactly one argument")
    expr_to_eval = tail[0]
    return evaluate_fn(expr_to_eval, env, macros, is_tail_call)
