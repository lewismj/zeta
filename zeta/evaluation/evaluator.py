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

# ----------------- Core evaluation -----------------
def evaluate(expr: SExpression, env: Environment, macros: MacroEnvironment = None) -> SExpression:
    """
    Core evaluation function, evaluates Lisp values.
    """
    if macros is None:
        macros = MacroEnvironment()

    match expr: # Macro Expansion & Quasi-quoting.
        case QQ(value):
            return [Symbol("quasiquote"), evaluate(value, env, macros)]

        case UQ(value):
            return evaluate(value, env, macros)

        case UQSplice(value):
            result = evaluate(value, env, macros)
            if not isinstance(result, list):
                raise ZetaTypeError("Unquote-splicing must produce a list")
            return result

        case []:
            return []

        case list() as xs if xs:
            expr = macros.macro_expand_all(xs, evaluate, env)

    # Note, we need to split the expression match, the macro expansion needs to occur
    # prior to the evaluation.

    match expr:
        case [head, * tail]:
            if isinstance(head, Symbol):   # Handle special forms first.
                if head in SPECIAL_FORMS:
                    return SPECIAL_FORMS[head](tail, env, macros, evaluate)
                elif ':' in head.id or '/' in head.id:
                    # Deal with imported Python modules functions, objects and methods.
                    attr = resolve_object_path(env, head)
                    args = [evaluate(arg, env, macros) for arg in tail]
                    if callable(attr) and getattr(attr, "_zeta_wrapped", False):
                        return attr(env, args) # package level function.
                    else:
                        return attr(*args) # bound object level function.
                else: # If the head is a symbol and not a special form, lookup in env.
                    head = env.lookup(head)

            # Check if we have Lambda or Callable.
            if isinstance(head, Lambda) or callable(head):
                args = [evaluate(arg, env, macros) for arg in tail]
                return apply(head, args, env, macros, evaluate)

            # Evaluate head, then tail recursively.
            if isinstance(head, list):
                head_eval = evaluate(head, env, macros)
                return evaluate([head_eval] + tail, env, macros)

        case Symbol():
            return env.lookup(expr) # lookup symbol.

    return expr # Return atom.