from __future__ import annotations
from typing import Any

from zeta.reader.parser import lex, TokenStream
from zeta.types.nil import Nil
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.environment import Environment
from zeta.builtin.env_builtin import register
from zeta.builtin.macro_builtin import register as register_macros
from zeta.interpreter.backend import Backend
from zeta import LispValue


class Interpreter:
    """
    Orchestrates reading and evaluating Zeta code via a pluggable backend.
    Maintains an Environment and MacroEnvironment across calls.
    """

    def __init__(self, backend: Backend | None = None, prelude: str | None = None):
        # Lazy default to avoid circular imports when possible.
        if backend is None:
            from zeta.evaluation.backend_impl import EvalBackend  # local import to avoid hard coupling at import time
            backend = EvalBackend()
        self.backend: Backend = backend

        self.env: Environment = Environment()
        register(self.env)

        self.macros: MacroEnvironment = MacroEnvironment()
        register_macros(self.macros)

        if prelude:
            self.eval_prelude(prelude)

    def eval_prelude(self, code: str) -> None:
        tokens = lex(code)
        stream = TokenStream(iter(tokens))
        while (expr := stream.parse_expr()) is not None:
            self.backend.eval(expr, self.env, self.macros)

    def eval(self, code: str) -> LispValue:
        tokens = lex(code)
        stream = TokenStream(iter(tokens))
        results: list[LispValue] = []
        while (expr := stream.parse_expr()) is not None:
            results.append(self.backend.eval(expr, self.env, self.macros))
        if not results:
            return Nil
        if len(results) == 1:
            return results[0]
        return results
