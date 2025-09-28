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

        # Check head, if it is a Symbol, then move head to the lookup of the symbol.
        if isinstance(head, Symbol):
            head = env.lookup(head)

        # Handle Lambda head
        if isinstance(head, Lambda) or callable(head):
            args = [evaluate(arg, env, macros) for arg in tail]
            return apply(head, args, env, macros, evaluate)

        if isinstance(head, list):
            head_eval = evaluate(head, env, macros)
            return evaluate([head_eval] + tail, env, macros)

    if isinstance(expr, Symbol):
        return env.lookup(expr)

    # Return Atoms.
    return expr