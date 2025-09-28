from typing import Callable, Any
import importlib
import numpy as np

from zeta.types import Environment, MacroEnvironment, Symbol
from zeta.parser import lex
from zeta.eval import evaluate
from zeta.builtins import register
from zeta.parser import TokenStream
from zeta.packages import import_module


def import_module1(
    env: Environment,
    module_name: str,
    alias: str | None = None,
    register_functions: dict[str, Callable] | None = None,
    default_arg_converter: Callable[[list[Any]], list[Any]] | None = None,
    default_return_converter: Callable[[Any], Any] | None = None
):
    """
    Import a Python module as a Zeta package, optionally adding helpers.
    All functions are wrapped to accept (env, args) to be compatible with Zeta.
    """
    module = importlib.import_module(module_name)
    pkg_env = env.define_package(module_name)

    # Default converters are identity
    default_arg_converter = default_arg_converter or (lambda args: args)
    default_return_converter = default_return_converter or (lambda x: x)

    # Wrap a Python function to Zeta callable
    def wrap_func(f):
        def zeta_fn(env_inner, args):
            py_args = default_arg_converter(args)
            result = f(*py_args)
            return default_return_converter(result)
        return zeta_fn

    # Wrap all module callables
    for name in dir(module):
        if name.startswith("_"):
            continue
        attr = getattr(module, name)
        if callable(attr):
            pkg_env.define(Symbol(name), wrap_func(attr))

    # Register additional helpers (like to-list, to-array)
    if register_functions:
        for name, func in register_functions.items():
            # Wrap helpers to accept (env, args)
            pkg_env.define(Symbol(name), lambda env_inner, args, f=func: f(*args))

    # Alias support
    if alias:
        env.register_package_alias(alias, module_name)
# --- Optional converters for numpy ---
# Example converters
def to_array(args):
    """Convert a Zeta list/tuple to a NumPy array."""
    return [np.array(a) if isinstance(a, (list, tuple)) else a for a in args]


def to_list(x):
    if isinstance(x, np.ndarray):
        return x.tolist()
    try:
        iter(x)
        return x  # already iterable, return as-is
    except TypeError:
        return [x]  # wrap non-iterable in a list


def to_bool(x):
    """Convert a NumPy array or value to a Python boolean."""
    if isinstance(x, np.ndarray):
        # Single-element array? Take the value
        if x.size == 1:
            return bool(x.item())
        else:
            raise ValueError("Cannot convert multi-element array to bool")
    return bool(x)

def main():
    env = Environment()
    macros = MacroEnvironment()
    register(env)

    import_module1(
        env,
        "numpy",
        alias="np",
        register_functions={
            "to-array": to_array,
            "to-list": to_list,
            "to-bool": to_bool,
        }
    )

    # Example Lisp code
    examples = [
        "(np:to-list (np:dot (np:array (1 2)) (np:array (3 4))))",  # explicit conversion
    ]

    for code in examples:
        print("Code:", code)
        tokens = lex(code)
        stream = TokenStream(tokens)
        expr = stream.parse_expr()
        result = evaluate(expr, env, macros)
        print("Result:", result)
        print("---")

if __name__ == "__main__":
    main()