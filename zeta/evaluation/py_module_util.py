from typing import Any
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol


def resolve_object_path(env: Environment, path: Symbol) -> Any:
    """
    Resolve a path like 'np:array', 'df:head', or 'df/head'.
    Handles both module-level functions and object-level methods.
    """
    parts = path.id.replace(":", ".").split(".")
    first, *rest = parts

    # Check if first part is a package alias
    if first in env.package_aliases:
        package_name = env.package_aliases[first]
        package_env = env.packages[package_name]

        # If package_env is an Environment, lookup in its vars
        obj: Any = package_env.lookup(Symbol(rest.pop(0)))

    else:
        # Not a package: normal environment lookup
        obj = env.lookup(Symbol(first))

    # Walk remaining attributes/methods
    for attr in rest:
        # If obj is a Python object, getattr works
        if not isinstance(obj, Environment):
            obj = getattr(obj, attr)
        else:
            # If obj is an Environment, lookup Zeta symbol
            obj = obj.lookup(Symbol(attr))

    return obj
