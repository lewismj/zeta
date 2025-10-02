"""Special forms: condition-case and cond.

- condition-case: error handling construct similar to Emacs Lisp's condition-case.
- cond: multi-branch conditional form.
"""

from zeta import SExpression
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaArityError
from zeta.types.nil import Nil


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


def cond_form(tail: list[SExpression], env: Environment, macros: MacroEnvironment, evaluate_fn, is_tail_call: bool=False):
    """Evaluate a (cond (test expr...) ...).

    For each clause in order:
    - Evaluate test; if truthy (not Nil and not #f), evaluate the clause body
      sequentially and return the last value. If the clause has only the test,
      return the test's value.
    - If the test is the symbol 'else or #t, treat it as truthy without evaluating it.
    If no clause matches, return Nil.
    """
    result = Nil
    for idx, clause in enumerate(tail):
        if not isinstance(clause, list) or len(clause) == 0:
            continue
        test = clause[0]
        body = clause[1:]

        # Handle else/#t as immediate truth
        is_else = isinstance(test, Symbol) and (test == Symbol("else") or test == Symbol("#t"))
        if is_else:
            # evaluate body and return
            if not body:
                return Symbol("#t")
            for i, expr in enumerate(body):
                last = i == len(body) - 1
                result = evaluate_fn(expr, env, macros, is_tail_call=is_tail_call and last)
            return result

        # Otherwise, evaluate the test
        test_val = evaluate_fn(test, env, macros, is_tail_call=False)
        truthy = not (test_val is Nil or (isinstance(test_val, Symbol) and test_val == Symbol("#f")))
        if truthy:
            if not body:
                return test_val
            for i, expr in enumerate(body):
                last = i == len(body) - 1
                result = evaluate_fn(expr, env, macros, is_tail_call=is_tail_call and last)
            return result

    # No clause matched
    return Nil
