import os
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.environment import Environment
from zeta.reader.parser import lex, TokenStream
from zeta.builtin.env_builtin import register
from zeta.evaluation.evaluator import evaluate


# --- Example usage ---
def main():
    env = Environment()
    macros = MacroEnvironment()
    register(env)

    examples = [
        '''
        (progn
            (import "numpy" as "np" helpers "np_helpers")
            (np:to_list (np:dot (np:array (1 2)) (np:array (3 4))))
        )
        ''',
        '''
            (progn
              (import "pandas" as "pd")
              (define data (list (list 1 2) (list 3 4)))
              (define df (pd:DataFrame data))
              (define first-row (df:head 1))
              (define col-sum (df:sum))
              col-sum
            )
        ''',
        '''
        (progn
          (import "pandas" as "pd")
          (define empty (list (list 0 0 0) (list 0 0 0)))
           (pd:DataFrame empty)))
          (define col-sum (df:sum))
          col-sum
        )
        ''',
        '''
        (progn
          ;; Import Pandas
          (import "pandas" as "pd")
          (define empty (list (list 0 0 0) (list 0 0 0)))
          ;; Try reading CSV, fallback to empty DataFrame on error
          (define df
            (catch 'any
              (pd:read_csv "C://tmp//data.csv")
              (pd:DataFrame empty))) ;; optional fallback, if we have exception 'any

          ;; Sum columns
          (define col-sum (df:sum))

          ;; Return the sum
          col-sum
        )
        '''
    ]

    for code in examples:
        print("Code:", code)
        tokens = lex(code)
        stream = TokenStream(tokens)
        expr = stream.parse_expr()
        result = evaluate(expr, env, macros)
        print(f"Result:\n {result}, type:{type(result)}")
        print("---")


if __name__ == "__main__":
    os.environ["ZETA_PACKAGE_PATH"] = "C:/Users/lewis/develop/zeta/playground/ext"
    main()