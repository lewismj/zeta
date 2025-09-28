from zeta import SExpression
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaArityError

def eval_condition_case(evaluate_fn, tail: list[SExpression], env: Environment, macros: MacroEnvironment) -> SExpression:
    if not tail:
        raise ZetaArityError("condition-case requires at least a body expression")

    body_expr = tail[0]
    handlers = tail[1:]

    try:
        return evaluate_fn(body_expr, env, macros)
    except Exception as ex:
        for handler in handlers:
            if not isinstance(handler, list) or len(handler) < 2:
                continue
            cond_symbol = handler[0]
            rest = handler[1:]

            if not isinstance(cond_symbol, Symbol):
                continue

            if cond_symbol == Symbol("error"):
                local_env = Environment(outer=env)
                handler_body = rest
                if len(rest) >= 2 and isinstance(rest[0], Symbol):
                    error_var = rest[0]
                    local_env.define(error_var, ex)
                    handler_body = rest[1:]

                result = None
                for expr in handler_body:
                    result = evaluate_fn(expr, local_env, macros)
                return result

        raise ex

def condition_case_form(tail, env, macros, evaluate_fn):
    return eval_condition_case(evaluate_fn, tail, env, macros)