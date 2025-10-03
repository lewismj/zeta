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
        self,
        args: list[LispValue],
        caller_env: Environment | None = None,
        evaluate_fn=None,
        macros=None,
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

        # Helper: bind &optional specs, evaluating defaults when provided
        def _bind_optionals(optional_specs: list[Symbol | list]):
            nonlocal supplied
            for spec in optional_specs:
                if supplied:
                    # If caller supplied a value, just bind it to the name
                    name = spec if isinstance(spec, Symbol) else spec[0]
                    local_env.define(name, supplied.pop(0))
                else:
                    if isinstance(spec, Symbol):
                        # No default expr; bind Nil
                        local_env.define(spec, Nil)
                    else:
                        name = spec[0]
                        default_expr = spec[1] if len(spec) >= 2 else Nil
                        if evaluate_fn is not None and default_expr is not Nil:
                            val = evaluate_fn(default_expr, local_env, macros, False)
                        else:
                            # If no evaluator or no default provided, fall back to Nil
                            val = Nil
                        local_env.define(name, val)

        # Analyze formals for &optional, &rest or &key
        if Symbol("&rest") in formals and Symbol("&key") in formals:
            raise ZetaArityError("Malformed parameter list: cannot mix &rest and &key")

        # Helper to bind simple positional list
        def _bind_positional(fs: list[Symbol], sup: list[LispValue]):
            while fs:
                f = fs.pop(0)
                if sup:
                    local_env.define(f, sup.pop(0))
                else:
                    missing = [f] + fs
                    raise ZetaArityError(
                        f"Too few arguments; missing {len(missing)} parameter(s): {[str(s) for s in missing]}"
                    )

        # Handle &rest (may appear after required/optional)
        if Symbol("&rest") in formals:
            leading: list[Symbol] = []
            rest_name: Symbol | None = None
            # Consume until &rest
            while formals:
                formal = formals.pop(0)
                if formal == Symbol("&rest"):
                    if not formals:
                        raise ZetaArityError(
                            "Malformed parameter list: &rest must be followed by a name"
                        )
                    rest_name = formals.pop(0)
                    break
                leading.append(formal)

            # Split optional if present in leading
            if Symbol("&optional") in leading:
                opt_index = leading.index(Symbol("&optional"))
                required_formals = leading[:opt_index]
                optional_specs = leading[opt_index + 1 :]
            else:
                required_formals = leading
                optional_specs = []

            # Bind required
            _bind_positional(required_formals, supplied)
            # Bind optionals (symbol or (name default)); evaluate defaults when provided
            _bind_optionals(optional_specs)

            # Bind rest to remaining
            local_env.define(rest_name, supplied)
            supplied = []
            return local_env

        # &key handling (Common Lisp-style named parameters)
        if Symbol("&key") in formals:
            # split formals: positionals (possibly with &optional) before &key, then list of keyword names
            key_index = formals.index(Symbol("&key"))
            leading = formals[:key_index]
            keyword_formals = formals[key_index + 1 :]

            if Symbol("&optional") in leading:
                opt_index = leading.index(Symbol("&optional"))
                positional_formals = leading[:opt_index]
                optional_specs = leading[opt_index + 1 :]
            else:
                positional_formals = leading
                optional_specs = []

            # Bind required positional
            for pf in positional_formals:
                if supplied:
                    local_env.define(pf, supplied.pop(0))
                else:
                    missing = [pf] + positional_formals[positional_formals.index(pf)+1:]
                    raise ZetaArityError(
                        f"Too few arguments; missing {len(missing)} parameter(s): {[str(s) for s in missing]}"
                    )

            # Bind optionals (evaluate defaults when provided)
            _bind_optionals(optional_specs)

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

        # No &rest or &key: may have &optional or simple positional
        if Symbol("&optional") in formals:
            opt_index = formals.index(Symbol("&optional"))
            required_formals = formals[:opt_index]
            optional_specs = formals[opt_index + 1 :]

            # Bind required
            _bind_positional(required_formals, supplied)
            # Bind optionals (support defaults and evaluation via evaluate_fn)
            _bind_optionals(optional_specs)

            if supplied:
                raise ZetaArityError(f"Too many arguments: {supplied}")
            return local_env

        # Simple positional arity
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
