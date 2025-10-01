from zeta.types.macro_environment import MacroEnvironment
from zeta.types.environment import Environment
from zeta.builtin.env_builtin import register
from zeta.builtin.macro_builtin import register as register_macros
from zeta.interpreter import Interpreter


def main():

    env = Environment()
    register(env)
    macros = MacroEnvironment()
    register_macros(macros)

    interp = Interpreter(
        prelude='''
                ;; Provide map function for use in let-bindings macro.
                (defun map (f xs)
                (if (== xs Nil)
                    Nil
                    (join (list (f (head xs))) (map f (tail xs)))
                ))''')

    # Test expressions
    tests = [
        '''
            ;; Macro definition
            (defmacro let-bindings (&rest bindings)
              `(let (,@bindings)
                 (list ,@(map car bindings))))
        ''',
        '''(let-bindings ((a 1) (b 2)) (list a b))'''
    ]

    # tests = [
    #     '''
    #         ;; Macro definition
    #         (defmacro let-bindings (&rest bindings)
    #           `(let ,@bindings
    #              (list ,@(map car bindings))))
    #     ''',
    #     '''(let-bindings ((a 1) (b 2)) (list a b))'''
    #     # '''
    #     #     (let-bindings ((a 1) (b 2)))
    #     # ''',
    #     # '''
    #     #               (list a b)
    #     # '''
    # ]

    for code in tests:
        try:
            print("Code:", code)
            result = interp.eval(code)
            print("Result:", result)
        except Exception as ex:
            print("Error:", ex)


# Example usage
if __name__ == "__main__":
    import sys
    main()