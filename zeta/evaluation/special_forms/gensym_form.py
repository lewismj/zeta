from zeta import EvaluatorFn, SExpression, LispValue
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaArityError, ZetaTypeError


def gensym_form(
    tail: list[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    is_tail_call: bool = False,
) -> LispValue:
    # (gensym) or (gensym prefix)
    if len(tail) > 1:
        raise ZetaArityError("gensym takes at most 1 argument: (gensym [prefix])")
    prefix = "G"
    if len(tail) == 1:
        p = tail[0]
        # Accept a raw symbol, a string, or a quoted symbol: (gensym 't)
        if isinstance(p, Symbol):
            prefix = p.id
        elif isinstance(p, str):
            prefix = p
        elif isinstance(p, list) and len(p) == 2 and isinstance(p[0], Symbol) and p[0] == Symbol("quote") and isinstance(p[1], Symbol):
            prefix = p[1].id
        else:
            raise ZetaTypeError("gensym prefix must be a Symbol or string")
    return macros.gen_sym(prefix)