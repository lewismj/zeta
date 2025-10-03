"""Utilities for resolving Python module/object paths from Zeta symbols.

This module provides helpers that let Zeta forms refer to Python objects using
package-qualified or dotted notation (e.g., np:array, os:path.join).
"""
from typing import Any
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol


def resolve_object_path(env: Environment, path: Symbol) -> Any:
    """Resolve a qualified symbol to a Python object or Zeta binding.

    Supports package aliases (pkg:sym) and dotted traversal. If an intermediate
    object is a Zeta Environment, lookups switch to symbolic lookup; otherwise
    Python attribute access is used.
    """
    parts = path.id.replace(":", ".").split(".")
    first, *rest = parts

    # Climb to the root environment to resolve package aliases and packages
    root = env
    while root.outer is not None:
        root = root.outer

    # Check if first part is a package alias (resolved at the root env)
    if first in root.package_aliases:
        package_name = root.package_aliases[first]
        package_env = root.packages[package_name]
        # If package_env is an Environment, lookup first attr inside it
        obj: Any = package_env.lookup(Symbol(rest.pop(0))) if rest else package_env
    else:
        # Not a package: normal environment lookup (will climb as needed)
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
