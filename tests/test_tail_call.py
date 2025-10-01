import sys
import pytest

from zeta.interpreter import Interpreter


def test_large_tail_recursive_factorial_runs_without_exception():
    """Ensure tail-call optimization handles large recursion depth.

    We define a tail-recursive factorial in Zeta and compute (fact 1500 1).
    The test passes if no exception is raised and an integer result is produced.
    """
    # Allow safe string conversion if any diagnostic paths stringify big ints (unlikely here)
    if hasattr(sys, "set_int_max_str_digits"):
        try:
            sys.set_int_max_str_digits(100_000)
        except Exception:
            pass

    interp = Interpreter()

    program = """
    (progn
      (defun fact (n acc)
        (if (= n 0)
            acc
            (fact (- n 1) (* n acc))))
      (fact 1500 1))
    """

    result = interp.eval(program)

    # Basic sanity checks: it's an int and positive
    assert isinstance(result, int)
    assert result > 0
