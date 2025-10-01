from __future__ import annotations

from io import StringIO

from zeta import SExpression, LispValue
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaArityError


class Lambda:
    def __init__(
        self, formals: list[Symbol], body: SExpression, env: Environment = Environment()
    ):
        self.formals: list[Symbol] = formals
        self.body: SExpression = body
        self.env: Environment = env

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
        self, args: list[LispValue], caller_env: Environment | None = None
    ) -> Environment:
        """
        Bind the given argument values to this lambda's formal parameters and
        return a new Environment for evaluating the body.

        Semantics (aligned with apply.apply_lambda for full applications):
        - Positional binding of parameters.
        - Supports &rest to capture remaining arguments as a list.
        - Raises ZetaArityError if too few or too many args are supplied when
          no &rest parameter is present.
        - Partial application is not handled here; callers should ensure full
          application when using extend_env (as in the apply special form).
        """
        formals = list(self.formals)
        supplied = list(args)
        local_env = Environment(outer=self.env)

        while formals:
            formal = formals.pop(0)
            if formal == Symbol("&rest"):
                if not formals:
                    raise ZetaArityError(
                        "Malformed parameter list: &rest must be followed by a name"
                    )
                rest_name = formals.pop(0)
                local_env.define(rest_name, supplied)
                supplied = []
                break
            if supplied:
                local_env.define(formal, supplied.pop(0))
            else:
                # Too few arguments and no &rest to capture them
                missing = [formal] + formals
                raise ZetaArityError(
                    f"Too few arguments; missing {len(missing)} parameter(s): {[str(s) for s in missing]}"
                )

        if supplied:
            # Too many arguments with no &rest parameter
            raise ZetaArityError(f"Too many arguments: {supplied}")

        return local_env
