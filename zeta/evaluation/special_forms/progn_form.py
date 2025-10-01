from zeta import EvaluatorFn
from zeta import SExpression, LispValue
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment


def progn_form(
    tail: list[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    is_tail_call: bool = False,
) -> LispValue:
    result: LispValue = None  # type: ignore[assignment]
    for e in tail[:-1]:
        evaluate_fn(e, env, macros)
    if tail:
        result = evaluate_fn(tail[-1], env, macros, is_tail_call=is_tail_call)
    return result
