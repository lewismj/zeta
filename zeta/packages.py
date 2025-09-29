import importlib
import sys
import pathlib
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol

CWD = pathlib.Path.cwd()
ZETA_PACKAGE_PATH = [
    CWD / "ext"  # external helpers
]

def import_helpers_module(module_name: str):
    """Try to import a helper module from ZETA_PACKAGE_PATH."""
    for path in ZETA_PACKAGE_PATH:
        sys.path.insert(0, str(path))
        try:
            module = importlib.import_module(module_name)
            sys.path.pop(0)
            return module
        except ModuleNotFoundError:
            sys.path.pop(0)
    raise ModuleNotFoundError(f"Helper module {module_name} not found in ZETA_PACKAGE_PATH")

def import_module(
    env: Environment,
    module_name: str,
    alias: str | None = None,
    register_functions_module: str | None = None,
):
    """
    Import a Python module as a Zeta package.

    - All callable attributes of the module are exposed to Zeta.
    - Optionally load external helpers from `register_functions_module`.
    - Alias support allows access like `alias:func`.
    """
    # Import the main module
    module = importlib.import_module(module_name)
    pkg_env = env.define_package(module_name)

    # Helper to wrap functions for Zeta
    def wrap_func(f):
        def zeta_fn(env_inner, args):
            if len(args) == 1:
                return f(args[0])
            return f(*args)

        zeta_fn._zeta_wrapped = True
        return zeta_fn

    # Register all callable attributes from the main module
    for name in dir(module):
        if name.startswith("_"):
            continue
        attr = getattr(module, name)
        if callable(attr):
            pkg_env.define(Symbol(name), wrap_func(attr))

    # Register external helper functions if provided
    if register_functions_module:
        helpers = import_helpers_module(register_functions_module)
        for name in dir(helpers):
            if name.startswith("_"):
                continue
            func = getattr(helpers, name)
            if callable(func):
                pkg_env.define(Symbol(name), wrap_func(func))

    # Alias support
    if alias:
        env.register_package_alias(alias, module_name)
