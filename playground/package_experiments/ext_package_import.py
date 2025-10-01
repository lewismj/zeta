# main.py

from zeta.types.macro_environment import MacroEnvironment
from zeta.types.environment import Environment
from zeta.evaluation.evaluator import evaluate
from zeta.builtin.env_builtin import register
from zeta.reader.parser import lex, TokenStream
from zeta.packages import import_module

def main():
    env = Environment()
    macros = MacroEnvironment()

    # Register core builtins
    register(env)

    import_module(env, "numpy", alias="np", register_functions_module="np_helpers")



    # Example Lisp code
    examples = [
        "(np:array (1 2 3))",
        "(np:sum (np:array (1 2 3 4)))",
        "(np:dot (np:array (1 2)) (np:array (3 4)))",
        # Use a helper function defined in numpy_helpers.py
        "(np:to_list (np:dot (np:array (1 2)) (np:array (3 4))))"
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
