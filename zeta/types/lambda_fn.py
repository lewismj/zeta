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
        - Supports &key for named parameters provided like :name value.
        - Raises ZetaArityError if too few or too many args are supplied when
          no &rest parameter is present.
        - Partial application is not handled here; callers should ensure full
          application when using extend_env (as in the apply special form).
        """
        formals = list(self.formals)
        supplied = list(args)
        local_env = Environment(outer=self.env)

        # Analyze formals for &rest or &key
        if Symbol("&rest") in formals and Symbol("&key") in formals:
            raise ZetaArityError("Malformed parameter list: cannot mix &rest and &key")

        if Symbol("&rest") in formals:
            # &rest collects the remaining arguments into a list bound to the
            # following formal symbol.
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

        # &key handling (Common Lisp-style named parameters)
        if Symbol("&key") in formals:
            # split formals: positionals before &key, then list of keyword names
            key_index = formals.index(Symbol("&key"))
            positional_formals = formals[:key_index]
            keyword_formals = formals[key_index + 1 :]

            # Bind positional first
            for pf in positional_formals:
                if supplied:
                    local_env.define(pf, supplied.pop(0))
                else:
                    missing = [pf] + positional_formals[positional_formals.index(pf)+1:]
                    raise ZetaArityError(
                        f"Too few arguments; missing {len(missing)} parameter(s): {[str(s) for s in missing]}"
                    )

            # Remaining supplied must be keyword/value pairs
            if len(supplied) % 2 != 0:
                raise ZetaArityError("Keyword arguments must be in pairs")

            provided_keys: dict[Symbol, LispValue] = {}
            while supplied:
                key = supplied.pop(0)
                val = supplied.pop(0) if supplied else None
                if not isinstance(key, Symbol) or not key.id.startswith(":"):
                    raise ZetaArityError("Expected keyword symbol like :name in keyword arguments")
                # map :name -> name symbol
                name = key.id[1:]
                target = Symbol(name)
                if target not in keyword_formals:
                    raise ZetaArityError(f"Unknown keyword argument {key}")
                provided_keys[target] = val

            # Define all keyword formals, defaulting to Nil
            for kf in keyword_formals:
                val = provided_keys.get(kf, Nil)
                local_env.define(kf, val)

            return local_env

        # No &rest or &key: simple positional arity
        while formals:
            formal = formals.pop(0)
            if supplied:
                local_env.define(formal, supplied.pop(0))
            else:
                missing = [formal] + formals
                raise ZetaArityError(
                    f"Too few arguments; missing {len(missing)} parameter(s): {[str(s) for s in missing]}"
                )
        if supplied:
            raise ZetaArityError(f"Too many arguments: {supplied}")
        return local_env
