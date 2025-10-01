"""Core evaluator and trampoline for the Zeta interpreter.

Implements macro expansion, special-form dispatch, and tail-call aware
application via a simple trampoline using TailCall objects.
"""

from __future__ import annotations

from zeta import SExpression, LispValue
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.lambda_fn import Lambda
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.evaluation.apply import apply
from zeta.evaluation.special_forms import SPECIAL_FORMS
from zeta.evaluation.py_module_util import resolve_object_path
from zeta.types.tail_call import TailCall


def evaluate(
    expr: SExpression, env: Environment, macros: MacroEnvironment | None = None, _=False
) -> LispValue:
    """
    Trampoline evaluator: tail-call aware evaluation.
    """
    if macros is None:
        macros = MacroEnvironment()

    result = evaluate0(expr, env, macros, True)  # Start in 'tail' mode.
    while isinstance(result, TailCall):
        result = evaluate0(result.fn.body, result.env, result.macros, True)
    return result


def evaluate0(
    expr: SExpression,
    env: Environment,
    macros: MacroEnvironment | None = None,
    is_tail_call: bool = False,
) -> LispValue:
    """
    Core evaluator: single-step evaluation with tail-call awareness.
    Returns either a value or a TailCall.
    """
    if macros is None:
        macros = MacroEnvironment()

    if expr == []:
        return []

    # Head-position macro handling and guarded expansion.
    if isinstance(expr, list) and expr:
        h = expr[0]
        if isinstance(h, Symbol):
            if macros.is_macro(h):  # Expand head-position macro first
                expanded = macros.expand_1(expr, evaluate0, env)
                return evaluate0(expanded, env, macros, is_tail_call)
            if (
                h not in SPECIAL_FORMS
            ):  # Do not pre-expand inside special forms (e.g., quasiquote)
                expr = macros.macro_expand_all(expr, evaluate0, env)
        else:
            expr = macros.macro_expand_all(
                expr, evaluate0, env
            )  # Non-symbol head: safe to expand recursively

    match expr:
        case [head, *tail_args]:
            if isinstance(head, Symbol):
                # --- Special forms handling ---
                if head in SPECIAL_FORMS:
                    return SPECIAL_FORMS[head](
                        tail_args, env, macros, evaluate0, is_tail_call
                    )  # <-- propagate tail
                elif ":" in head.id and head.id != "/":
                    attr = resolve_object_path(env, head)
                    args = [evaluate0(arg, env, macros) for arg in tail_args]
                    if callable(attr) and getattr(attr, "_zeta_wrapped", False):
                        return attr(env, args)
                    else:
                        return attr(*args)
                else:
                    head = env.lookup(head)

            # Lambda / callable application.
            if isinstance(head, Lambda) or callable(head):
                args = [evaluate0(arg, env, macros) for arg in tail_args]
                result = apply(
                    head, args, env, macros, evaluate0, is_tail_call
                )  # <-- pass tail flag
                return result

            # Evaluate head if it is a list and re-dispatch.
            if isinstance(head, list):
                head_eval = evaluate0(head, env, macros)
                return evaluate0([head_eval] + tail_args, env, macros, is_tail_call)

        case Symbol():
            # Treat keywords (symbols starting with ':') as self-evaluating (for named parameters in Lambda functions).
            if isinstance(expr, Symbol) and expr.id.startswith(":"):
                return expr
            return env.lookup(expr)

    # --- Atoms return as-is ---
    return expr
