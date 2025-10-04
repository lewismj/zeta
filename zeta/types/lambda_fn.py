"""Lambda function representation and argument binding utilities for Zeta."""

from __future__ import annotations

from io import StringIO

from zeta import SExpression, LispValue
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaArityError
from zeta.types.nil import Nil


class Lambda:
    """A first-class lambda with formal parameters, body, and closure env."""

    __slots__ = ("formals", "body", "env")

    def __init__(
        self, formals: list[Symbol], body: SExpression, env: Environment | None = None
    ):
        self.formals: list[Symbol] = formals
        self.body: SExpression = body
        # Avoid shared default Environment across instances
        self.env: Environment = env if env is not None else Environment()

    def __str__(self) -> str:
        with StringIO() as buffer:
            buffer.write("(λ (")
            buffer.write(" ".join(str(f) for f in self.formals))
            buffer.write(") ")
            buffer.write(str(self.body))
            buffer.write(")")
            return buffer.getvalue()

    def __repr__(self) -> str:
        """Return the Lisp-style representation of the lambda."""
        return str(self)

    # --- Evaluation helpers ---
    def extend_env(
        self,
        args: list[LispValue],
        caller_env: Environment | None = None,
        evaluate_fn=None,
        macros=None,
    ) -> Environment:
        """
        Bind the given argument values to this lambda's formal parameters and
        return a new Environment for evaluating the body.

        Delegates to the shared binder in zeta.types.bind to keep a single
        source of truth for lambda-list semantics.
        """
        # caller_env is unused in the binder because closures are captured via self.env
        from zeta.types.bind import bind_arguments
        return bind_arguments(self.formals, list(args), self.env, evaluate_fn, macros)
