from __future__ import annotations

from zeta import SExpression, LispValue, EvaluatorFn
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.modules.python_loader import import_module
from zeta.types.nil import Nil
from zeta.types.symbol import Symbol


def import_form(tail: list[SExpression], env: Environment, macros: MacroEnvironment, evaluate_fn: EvaluatorFn, _: bool) -> LispValue:
    """
    Usage:
        (import "module_name" as "alias" helpers "helper_module_name")
    """
    module_name = tail[0]
    alias = None
    helpers_module = None

    i = 1
    while i < len(tail):
        key = tail[i]
        if key == Symbol("as"):
            alias = tail[i + 1]
            i += 2
        elif key == Symbol("helpers"):
            helpers_module = tail[i + 1]
            i += 2
        else:
            i += 1

    import_module(env, module_name, alias=alias, register_functions_module=helpers_module)
    return Nil
