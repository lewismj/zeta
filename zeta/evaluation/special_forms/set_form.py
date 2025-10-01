from zeta import EvaluatorFn
from zeta import SExpression, LispValue
from zeta.types.errors import ZetaInvalidSymbol, ZetaArityError
from zeta.types.symbol import Symbol
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment


def set_form(
    tail: list[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    is_tail_call: bool = False,
) -> LispValue:
    if len(tail) != 2:
        raise ZetaArityError("set requires exactly 2 arguments: (set var value)")
    var_sym, val_expr = tail
    if not isinstance(var_sym, Symbol):
        raise ZetaInvalidSymbol(f"set first argument must be a Symbol, got {var_sym}")
    value = evaluate_fn(val_expr, env, macros, is_tail_call)
    env.set(var_sym, value)

    return value
