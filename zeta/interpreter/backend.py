from __future__ import annotations
from typing import Protocol

from zeta import LispValue
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment


class Backend(Protocol):
    def eval(self, expr, env: Environment, macros: MacroEnvironment) -> LispValue: ...
