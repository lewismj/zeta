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


# def evaluate0(expr: SExpression, env: Environment, macros: MacroEnvironment = None, is_tail_call: bool = False) -> SExpression:
#     """
#     Core evaluator: single-step evaluation with tail-call awareness.
#     Returns either a value or a TailCall.
#     """
#     if macros is None:
#         macros = MacroEnvironment()
#
#     # --- Macro Expansion & Quasi-quote ---
#     match expr:
#         case QQ(value):
#             return [Symbol("quasiquote"), evaluate0(value, env, macros)]
#         case UQ(value):
#             return evaluate0(value, env, macros)
#         case UQSplice(value):
#             result = evaluate0(value, env, macros)
#             if not isinstance(result, list):
#                 raise ZetaTypeError("Unquote-splicing must produce a list")
#             return result
#         case []:
#             return []
#         case list() as xs if xs:
#             head = xs[0]
#             if not isinstance(head,list) and head not in SPECIAL_FORMS:
#                 from zeta.debug_utils.pprint import pprint_expr
#                 print(f'xs={pprint_expr(xs)}')
#                 expr = macros.macro_expand_all(xs, evaluate0, env)
#                 print(f'expr={pprint_expr(expr)}')
#
#     # --- Expression dispatch ---
#     match expr:
#         case [head, *tail_args]:
#             if isinstance(head, Symbol):
#                 # --- Special forms handling ---
#                 if head in SPECIAL_FORMS:
#                     return SPECIAL_FORMS[head](tail_args, env, macros, evaluate0, is_tail_call)  # <-- propagate tail
#                 elif ':' in head.id and head.id != '/':
#                     attr = resolve_object_path(env, head)
#                     args = [evaluate0(arg, env, macros) for arg in tail_args]
#                     if callable(attr) and getattr(attr, "_zeta_wrapped", False):
#                         return attr(env, args)
#                     else:
#                         return attr(*args)
#                 else:
#                     head = env.lookup(head)
#
#             # --- Lambda / callable application ---
#             if isinstance(head, Lambda) or callable(head):
#                 args = [evaluate0(arg, env, macros) for arg in tail_args]
#                 result = apply(head, args, env, macros, evaluate0, is_tail_call)  # <-- pass tail flag
#                 return result
#
#             # --- Evaluate head if it is a list and re-dispatch ---
#             if isinstance(head, list):
#                 head_eval = evaluate0(head, env, macros)
#                 return evaluate0([head_eval] + tail_args, env, macros, is_tail_call)
#
#         case Symbol():
#             return env.lookup(expr)
#
#     # --- Atoms return as-is ---
#     return expr

# def evaluate0(expr: SExpression, env: Environment, macros: MacroEnvironment = None, is_tail_call: bool = False) -> SExpression:
#     """
#     Core evaluator: single-step evaluation with tail-call awareness.
#     Returns either a value or a TailCall.
#     """
#     if macros is None:
#         macros = MacroEnvironment()
#
#     # Helper to expand quasiquotes: evaluate unquotes and splice results into lists.
#     def _expand_quasi(x):
#         # UQ -> evaluate and return result
#         if isinstance(x, UQ):
#             return evaluate0(x.value, env, macros)
#         # UQSplice -> evaluate and return a sentinel for splicing
#         if isinstance(x, UQSplice):
#             res = evaluate0(x.value, env, macros)
#             if not isinstance(res, list):
#                 raise ZetaTypeError("Unquote-splicing must produce a list")
#             return ("__splice__", res)
#         # Recurse lists, splicing when sentinel encountered
#         if isinstance(x, list):
#             out = []
#             for el in x:
#                 el2 = _expand_quasi(el)
#                 if isinstance(el2, tuple) and el2[0] == "__splice__":
#                     out.extend(el2[1])
#                 else:
#                     out.append(el2)
#             return out
#         # Atoms return as-is
#         return x
#
#     # --- Macro Expansion & Quasi-quote ---
#     match expr:
#         case QQ(value):
#             return _expand_quasi(value)
#         case UQ(value):
#             return evaluate0(value, env, macros)
#         case UQSplice(value):
#             res = evaluate0(value, env, macros)
#             if not isinstance(res, list):
#                 raise ZetaTypeError("Unquote-splicing must produce a list")
#             return res
#         case []:
#             return []
#         case list() as xs if xs:
#             head = xs[0]
#             if not isinstance(head, list) and head not in SPECIAL_FORMS:
#                 from zeta.debug_utils.pprint import pprint_expr
#                 print(f'xs={pprint_expr(xs)}')
#                 expr = macros.macro_expand_all(xs, evaluate0, env)
#                 print(f'expr={pprint_expr(expr)}')
#
#     # --- Expression dispatch ---
#     match expr:
#         case [head, *tail_args]:
#             if isinstance(head, Symbol):
#                 # --- Special forms handling ---
#                 if head in SPECIAL_FORMS:
#                     return SPECIAL_FORMS[head](tail_args, env, macros, evaluate0, is_tail_call)  # <-- propagate tail
#                 elif ':' in head.id and head.id != '/':
#                     attr = resolve_object_path(env, head)
#                     args = [evaluate0(arg, env, macros) for arg in tail_args]
#                     if callable(attr) and getattr(attr, "_zeta_wrapped", False):
#                         return attr(env, args)
#                     else:
#                         return attr(*args)
#                 else:
#                     head = env.lookup(head)
#
#             # --- Lambda / callable application ---
#             if isinstance(head, Lambda) or callable(head):
#                 args = [evaluate0(arg, env, macros) for arg in tail_args]
#                 result = apply(head, args, env, macros, evaluate0, is_tail_call)  # <-- pass tail flag
#                 return result
#
#             # --- Evaluate head if it is a list and re-dispatch ---
#             if isinstance(head, list):
#                 head_eval = evaluate0(head, env, macros)
#                 return evaluate0([head_eval] + tail_args, env, macros, is_tail_call)
#
#         case Symbol():
#             try:
#                 return env.lookup(expr)
#             except Exception:
#                 print(f'Could not find symbol {expr} in environment')
#                 print(f'Macros {str(macros.macros)}')
#                 raise ZetaTypeError(f"Symbol {expr} not found in environment")
#
#     # --- Atoms return as-is ---
#     return expr


def evaluate0(expr: SExpression, env: Environment, macros: MacroEnvironment = None, is_tail_call: bool = False) -> SExpression:
    """
    Core evaluator: single-step evaluation with tail-call awareness.
    Returns either a value or a TailCall.
    """
    if macros is None:
        macros = MacroEnvironment()

    # Helper to expand quasiquotes: evaluate unquotes and splice results into lists.
    def _expand_quasi(x):
        # UQ -> evaluate and return result
        if isinstance(x, UQ):
            return evaluate0(x.value, env, macros)
        # UQSplice -> evaluate and return a sentinel for splicing
        if isinstance(x, UQSplice):
            res = evaluate0(x.value, env, macros)
            if not isinstance(res, list):
                raise ZetaTypeError("Unquote-splicing must produce a list")
            return ("__splice__", res)
        # Recurse lists, splicing when sentinel encountered
        if isinstance(x, list):
            out = []
            for el in x:
                el2 = _expand_quasi(el)
                if isinstance(el2, tuple) and el2[0] == "__splice__":
                    out.extend(el2[1])
                else:
                    out.append(el2)
            return out
        # Atoms return as-is
        return x

    # --- Macro Expansion & Quasi-quote ---
    match expr:
        case QQ(value):
            return _expand_quasi(value)
        case UQ(value):
            return evaluate0(value, env, macros)
        case UQSplice(value):
            res = evaluate0(value, env, macros)
            if not isinstance(res, list):
                raise ZetaTypeError("Unquote-splicing must produce a list")
            return res
        case []:
            return []
        case list() as xs if xs:
            head = xs[0]
            if not isinstance(head, list) and head not in SPECIAL_FORMS:
                from zeta.debug_utils.pprint import pprint_expr
                print(f'xs={pprint_expr(xs)}')
                expr = macros.macro_expand_all(xs, evaluate0, env)
                print(f'expr={pprint_expr(expr)}')

    # --- Expression dispatch ---
    match expr:
        case [head, *tail_args]:
            if isinstance(head, Symbol):
                # --- Special forms handling ---
                if head in SPECIAL_FORMS:
                    # Call the special form and then re-evaluate its result.
                    # This ensures forms that return syntactic wrappers (QQ/UQ/UQSplice)
                    # are processed (e.g. quasiquote/unquote) during macro expansion.
                    special_result = SPECIAL_FORMS[head](tail_args, env, macros, evaluate0, is_tail_call)
                    return evaluate0(special_result, env, macros, is_tail_call)

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
            try:
                return env.lookup(expr)
            except Exception:
                print(f'Could not find symbol {expr} in environment')
                print(f'Macros {str(macros.macros)}')
                raise ZetaTypeError(f"Symbol {expr} not found in environment")

    # --- Atoms return as-is ---
    return expr