import importlib
import numpy as np

from zeta.types import Environment, MacroEnvironment, Symbol
from zeta.parser import lex
from zeta.eval import evaluate
from zeta.builtins import register
from zeta.parser import TokenStream

def import_module_package0(env: Environment, module_name: str, alias: str | None = None):
    """
    Dynamically import a Python module and expose its functions as a Zeta package,
    converting Zeta lists to np.ndarray automatically and converting outputs back
    to Python lists where appropriate.
    """
    module = importlib.import_module(module_name)
    pkg_env = env.define_package(module_name)

    def wrap_func(f):
        def zeta_fn(env_inner, args):
            # Convert Zeta lists/tuples to numpy arrays where appropriate
            py_args = []
            for a in args:
                if isinstance(a, (list, tuple)):
                    py_args.append(np.array(a))
                else:
                    py_args.append(a)

            # Special handling for functions like np.array that expect a single argument
            if f.__name__ == "array" and len(py_args) > 1:
                py_args = [py_args]  # wrap all args into a single sequence

            # Call the numpy function
            result = f(*py_args)

            # Convert numpy arrays back to Python lists for Zeta
            if isinstance(result, np.ndarray):
                return result.tolist()
            return result

        return zeta_fn


    for name in dir(module):
        if name.startswith("_"):
            continue
        attr = getattr(module, name)
        if callable(attr):
            pkg_env.define(Symbol(name), wrap_func(attr))

    if alias:
        env.register_package_alias(alias, module_name)


def main():
    env = Environment()
    macros = MacroEnvironment()
    register(env)

    import numpy as np
    import_module_package0(env, "numpy", alias="np")

    code = "(np:sum (np:array 1 2 3 4))"
    tokens = lex(code)              # tokenize
    stream = TokenStream(tokens)    # stream of tokens
    expr = stream.parse_expr()      # parse to nested list/Symbol
    result = evaluate(expr, env)    # evaluate
    print(result)                   # 10

if __name__ == "__main__":
    main()