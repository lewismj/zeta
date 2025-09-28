from zeta.types.macro_environment import MacroEnvironment
from zeta.types.environment import Environment
from zeta.parser import lex, TokenStream
from zeta.env_builtin import register
from zeta.eval import evaluate


# --- Example usage ---
def main():
    env = Environment()
    macros = MacroEnvironment()
    register(env)

    # # Example Lisp code with dynamic import and chained calls
    # code = """
    #     (import 'numpy' as 'np' helpers 'np_helpers')
    #     (np:to_list (np:dot (np:array (1 2)) (np:array (3 4))))
    # """
    examples = [
        '''
        (progn
            (import "numpy" as "np" helpers "np_helpers")
            (np:to_list (np:dot (np:array (1 2)) (np:array (3 4))))
        )
        '''
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