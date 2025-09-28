from __future__ import annotations
from typing import Any

from zeta import SExpression
from zeta.types.nil import Nil
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.lambda_fn import Lambda
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaArityError, ZetaTypeError


from zeta.evaluation.special_forms.quote_forms import QQ, UQ, UQSplice
from zeta.evaluation.special_forms import SPECIAL_FORMS

# ----------------- Core evaluation -----------------
def evaluate(expr: SExpression, env: Environment, macros: MacroEnvironment = None) -> SExpression:
    """Evaluate a Zeta expression in the given environment with macro support."""
    if macros is None:
        macros = MacroEnvironment()

    if isinstance(expr, QQ):
        return [Symbol("quasiquote"), evaluate(expr.value, env, macros)]
    if isinstance(expr, UQ):
        return evaluate(expr.value, env, macros)
    if isinstance(expr, UQSplice):
        result = evaluate(expr.value, env, macros)
        if not isinstance(result, list):
            raise ZetaTypeError("unquote-splicing must produce a list")
        return result

    if isinstance(expr, list) and not expr:
        return []

    if isinstance(expr, list) and expr:
        expr = macros.macro_expand_all(expr, evaluate, env)

    if isinstance(expr, list) and expr:
        head, *tail = expr

        # Special forms
        if isinstance(head, Symbol) and head in SPECIAL_FORMS:
            return SPECIAL_FORMS[head](tail, env, macros, evaluate)

        # Evaluate Lambda or Symbol head
        if isinstance(head, Symbol):
            head = env.lookup(head)

        # Handle Lambda head
        if isinstance(head, Lambda):
            fn = head
            args = [evaluate(arg, env, macros) for arg in tail]

            formals = list(fn.formals)
            supplied = list(args)
            remaining_formals = []

            local_env = Environment(outer=fn.env)

            while formals:
                formal = formals.pop(0)
                if formal == Symbol("&rest"):
                    name = formals.pop(0)
                    local_env.define(name, supplied)
                    supplied = []
                    break
                if supplied:
                    local_env.define(formal, supplied.pop(0))
                else:
                    remaining_formals.append(formal)

            if remaining_formals:
                # Return a new Lambda with only remaining formals
                return Lambda(remaining_formals, fn.body, local_env)

            if supplied:
                raise ZetaArityError(f"Too many arguments: {supplied}")

            # All formals supplied: evaluate the body in local_env
            return evaluate(fn.body, local_env, macros)


        if callable(head) and not isinstance(head, Lambda):
            args = [evaluate(arg, env, macros) for arg in tail]
            return head(env,args)

        if isinstance(head, list):
            head_eval = evaluate(head, env, macros)
            return evaluate([head_eval] + tail, env, macros)


    if isinstance(expr, Symbol):
        return env.lookup(expr)

    return expr


# ----------------- Apply callable -----------------
def apply_callable(fn: Any, args: list[SExpression], env: Environment, macros: MacroEnvironment):
    if callable(fn) and not isinstance(fn, Lambda):
        return fn(args, env)


# ----------------- Quasiquote evaluation -----------------
def eval_quasiquote(expr, env, macros, depth=1):
    from zeta.types.symbol import Symbol

    if isinstance(expr, QQ):
        return [Symbol("quasiquote"), eval_quasiquote(expr.value, env, macros, depth)]
    if isinstance(expr, UQ):
        if depth == 1:
            return evaluate(expr.value, env, macros)
        else:
            return UQ(eval_quasiquote(expr.value, env, macros, depth - 1))
    if isinstance(expr, UQSplice):
        if depth == 1:
            result = evaluate(expr.value, env, macros)
            if not isinstance(result, list):
                raise ZetaTypeError("unquote-splicing must produce a list")
            return result
        else:
            return UQSplice(eval_quasiquote(expr.value, env, macros, depth - 1))

    if not isinstance(expr, list):
        return expr
    if not expr:
        return []

    result = []
    for item in expr:
        if isinstance(item, list) and item:
            head, *tail = item
            if head == Symbol("quasiquote"):
                result.append([Symbol("quasiquote"), eval_quasiquote(tail[0], env, macros, depth + 1)])
                continue
            if head == Symbol("unquote") and depth == 1:
                result.append(evaluate(tail[0], env, macros))
                continue
            if head == Symbol("unquote-splicing") and depth == 1:
                spliced = evaluate(tail[0], env, macros)
                if not isinstance(spliced, list):
                    raise ZetaTypeError("unquote-splicing must produce a list")
                result.extend(spliced)
                continue
        result.append(eval_quasiquote(item, env, macros, depth))
    return result


# ----------------- Condition-case -----------------
def eval_condition_case(tail: list[SExpression], env: Environment, macros: MacroEnvironment) -> SExpression:
    if not tail:
        raise ZetaArityError("condition-case requires at least a body expression")

    body_expr = tail[0]
    handlers = tail[1:]

    try:
        return evaluate(body_expr, env, macros)
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
                    result = evaluate(expr, local_env, macros)
                return result

        raise ex
