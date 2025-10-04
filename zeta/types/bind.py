from __future__ import annotations

from typing import List

from zeta import LispValue
from zeta.types.environment import Environment
from zeta.types.errors import ZetaArityError
from zeta.types.nil import Nil
from zeta.types.symbol import Symbol


def bind_arguments(
    formals: List[Symbol],
    supplied_args: List[LispValue],
    closure_env: Environment,
    evaluate_fn=None,
    macros=None,
) -> Environment:
    """
    Single source of truth for lambda-list binding in Zeta.

    Supports:
    - Positional required parameters
    - &optional (name) or (name default) with default evaluated via evaluate_fn if provided
    - &rest / &body (alias) capturing remaining supplied args as a list
    - &key :keyword value pairs mapped to formal names after &key, defaulting to Nil

    Returns a new Environment whose outer is the closure_env, populated with
    the bindings for evaluating the callee body.
    """
    formals = list(formals)
    supplied = list(supplied_args)
    local_env = Environment(outer=closure_env)

    # Internal: bind &optional sequence (symbols or (name default))
    def _bind_optionals(optional_specs: list[Symbol | list]):
        nonlocal supplied
        for spec in optional_specs:
            if supplied:
                # Caller provided a value for this optional
                name = spec if isinstance(spec, Symbol) else spec[0]
                local_env.define(name, supplied.pop(0))
            else:
                if isinstance(spec, Symbol):
                    local_env.define(spec, Nil)
                else:
                    name = spec[0]
                    default_expr = spec[1] if len(spec) >= 2 else Nil
                    if evaluate_fn is not None and default_expr is not Nil:
                        val = evaluate_fn(default_expr, local_env, macros, False)
                    else:
                        val = Nil
                    local_env.define(name, val)

    # Validate no illegal &rest/&key mixing
    if (Symbol("&rest") in formals or Symbol("&body") in formals) and Symbol("&key") in formals:
        raise ZetaArityError("Malformed parameter list: cannot mix &rest and &key")

    # Helper: bind a block of required positional formals
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

    # Handle &rest / &body
    if Symbol("&rest") in formals or Symbol("&body") in formals:
        leading: list[Symbol] = []
        rest_name: Symbol | None = None
        rest_marker = Symbol("&rest") if Symbol("&rest") in formals else Symbol("&body")
        # Consume until &rest/&body
        while formals:
            formal = formals.pop(0)
            if formal == rest_marker:
                if not formals:
                    raise ZetaArityError("Malformed parameter list: &rest/&body must be followed by a name")
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

        # Bind required and optionals
        _bind_positional(required_formals, supplied)
        _bind_optionals(optional_specs)

        # Bind rest to remaining args
        local_env.define(rest_name, supplied)
        supplied = []
        return local_env

    # Handle &key
    if Symbol("&key") in formals:
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

        # Bind positional required
        for pf in positional_formals:
            if supplied:
                local_env.define(pf, supplied.pop(0))
            else:
                missing = [pf] + positional_formals[positional_formals.index(pf) + 1 :]
                raise ZetaArityError(
                    f"Too few arguments; missing {len(missing)} parameter(s): {[str(s) for s in missing]}"
                )

        # Bind optionals
        _bind_optionals(optional_specs)

        # Remaining must be keyword/value pairs
        if len(supplied) % 2 != 0:
            raise ZetaArityError("Keyword arguments must be in pairs")

        provided_keys: dict[Symbol, LispValue] = {}
        while supplied:
            key = supplied.pop(0)
            val = supplied.pop(0) if supplied else None
            if not isinstance(key, Symbol) or not key.id.startswith(":"):
                raise ZetaArityError("Expected keyword symbol like :name in keyword arguments")
            # map :name -> Symbol("name")
            name = key.id[1:]
            target = Symbol(name)
            if target not in keyword_formals:
                raise ZetaArityError(f"Unknown keyword argument {key}")
            provided_keys[target] = val

        # Define all keyword formals, defaulting missing ones to Nil
        for kf in keyword_formals:
            val = provided_keys.get(kf, Nil)
            local_env.define(kf, val)

        return local_env

    # Handle &optional without &rest/&key
    if Symbol("&optional") in formals:
        opt_index = formals.index(Symbol("&optional"))
        required_formals = formals[:opt_index]
        optional_specs = formals[opt_index + 1 :]

        _bind_positional(required_formals, supplied)
        _bind_optionals(optional_specs)

        if supplied:
            raise ZetaArityError(f"Too many arguments: {supplied}")
        return local_env

    # Simple positional only
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
