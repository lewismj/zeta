from __future__ import annotations

from zeta import SExpression
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.lambda_fn import Lambda
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaTypeError
from zeta.evaluation.apply import apply

from zeta.evaluation.special_forms.quote_forms import QQ, UQ, UQSplice
from zeta.evaluation.special_forms import SPECIAL_FORMS
from zeta.evaluation.py_module_util import resolve_object_path
from zeta.types.tail_call import TailCall


def evaluate(expr: SExpression, env: Environment, macros: MacroEnvironment = None, _=False) -> SExpression:
    """
    Trampoline evaluator: tail-call aware evaluation.
    """
    if macros is None:
        macros = MacroEnvironment()

    result = evaluate0(expr, env, macros, True) # Start in 'tail' mode.
    while isinstance(result, TailCall):
        result = evaluate0(result.fn.body, result.env, result.macros, True)
    return result


def evaluate0(expr: SExpression, env: Environment, macros: MacroEnvironment = None, is_tail_call: bool = False) -> SExpression:
    """
    Core evaluator: single-step evaluation with tail-call awareness.
    Returns either a value or a TailCall.
    """
    if macros is None:
        macros = MacroEnvironment()

    # --- Macro Expansion & Quasi-quote ---
    match expr:
        case QQ(value):
            return [Symbol("quasiquote"), evaluate0(value, env, macros)]
        case UQ(value):
            return evaluate0(value, env, macros)
        case UQSplice(value):
            result = evaluate0(value, env, macros)
            if not isinstance(result, list):
                raise ZetaTypeError("Unquote-splicing must produce a list")
            return result
        case []:
            return []
        case list() as xs if xs:
            expr = macros.macro_expand_all(xs, evaluate0, env)

    # --- Expression dispatch ---
    match expr:
        case [head, *tail_args]:
            if isinstance(head, Symbol):
                # --- Special forms handling ---
                if head in SPECIAL_FORMS:
                    return SPECIAL_FORMS[head](tail_args, env, macros, evaluate0, is_tail_call)  # <-- propagate tail
                elif ':' in head.id and head.id != '/':
                    attr = resolve_object_path(env, head)
                    args = [evaluate0(arg, env, macros) for arg in tail_args]
                    if callable(attr) and getattr(attr, "_zeta_wrapped", False):
                        return attr(env, args)
                    else:
                        return attr(*args)
                else:
                    head = env.lookup(head)

            # --- Lambda / callable application ---
            if isinstance(head, Lambda) or callable(head):
                args = [evaluate0(arg, env, macros) for arg in tail_args]
                result = apply(head, args, env, macros, evaluate0, is_tail_call)  # <-- pass tail flag
                return result

            # --- Evaluate head if it is a list and re-dispatch ---
            if isinstance(head, list):
                head_eval = evaluate0(head, env, macros)
                return evaluate0([head_eval] + tail_args, env, macros, is_tail_call)

        case Symbol():
            return env.lookup(expr)

    # --- Atoms return as-is ---
    return expr