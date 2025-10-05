from __future__ import annotations

from zeta.evaluation.evaluator import evaluate as eval_expr
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta import LispValue


class EvalBackend:
    def eval(self, expr, env: Environment, macros: MacroEnvironment) -> LispValue:
        return eval_expr(expr, env, macros)
