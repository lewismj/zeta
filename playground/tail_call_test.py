from zeta.types.macro_environment import MacroEnvironment
from zeta.types.environment import Environment
from zeta.builtin.env_builtin import register
from zeta.builtin.macro_builtin import register as register_macros
from zeta.interpreter import Interpreter

# You must have a prelude string defined somewhere in your environment
# For example:
prelude = """
;; Optional prelude code
"""

def py_tail_recursive_fail():
    try:
        def fact_tail(n, acc=1):
            if n <= 1:
                return acc
            return fact_tail(n - 1, n * acc)
        fact_tail(1500)
    except Exception as ex:
        print("Native Python tail-recursive:", ex)


def main():
    py_tail_recursive_fail()

    env = Environment()
    register(env)
    macros = MacroEnvironment()
    register_macros(macros)

    interp = Interpreter(prelude=prelude)

    # Test expressions
    tests = [
        """
        ;; Tail-recursive factorial - gets tail call optimized.
        (progn
        (defun fact (n acc)
          (if (= n 0)
              acc
              (fact (- n 1) (* n acc))))
        ;; Python version of Zeta will rely on Python numerics
        ;; But this won't generate a maximum recursion depth error,
        ;; as we have tail-call optimization.
        (fact 1500 1) ;; tail recursive native Python would fail with maximum recursion depth error.
        )
        """
    ]

    for code in tests:
        try:
            result = interp.eval(code)
            print("Result:", result)
        except Exception as ex:
            print("Error:", ex)


# Example usage
if __name__ == "__main__":
    import sys
    sys.set_int_max_str_digits(100_000)
    main()