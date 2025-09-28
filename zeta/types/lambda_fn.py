from __future__ import annotations

from io import StringIO

from zeta import SExpression
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol


class Lambda:
    def __init__(self, formals: list[Symbol], body: SExpression, env: Environment = Environment()):
        self.formals = formals
        self.body = body
        self.env =env

    def __str__(self):
        with StringIO() as buffer:
            buffer.write("(λ (")
            buffer.write(" ".join(str(f) for f in self.formals))
            buffer.write(") ")
            buffer.write(str(self.body))
            buffer.write(")")
            return buffer.getvalue()

    def __repr__(self):
        """Return the Lisp-style representation of the lambda."""
        return str(self)
