"""Condition handling special form: condition-case.

Provides a simple error-catching construct similar to Emacs Lisp's condition-case.
"""
from zeta import SExpression
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaArityError


def eval_condition_case(
    evaluate_fn,
    tail_exprs: list[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    is_tail_call=False,
) -> SExpression:
    """Evaluate a condition-case block with optional tail-call awareness."""
    if not tail_exprs:
        raise ZetaArityError("condition-case requires at least a body expression")

    body_expr = tail_exprs[0]
    handlers = tail_exprs[1:]

    try:
        # --- propagate tail-call flag ---
        return evaluate_fn(body_expr, env, macros, is_tail_call=is_tail_call)
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
                # --- Tail-call aware evaluation of handler body ---
                for i, expr in enumerate(handler_body):
                    is_last = i == len(handler_body) - 1
                    result = evaluate_fn(
                        expr, local_env, macros, is_tail_call=is_tail_call and is_last
                    )
                return result

        raise ex


def condition_case_form(tail, env, macros, evaluate_fn, is_tail_call=False):
    return eval_condition_case(
        evaluate_fn, tail, env, macros, is_tail_call=is_tail_call
    )
