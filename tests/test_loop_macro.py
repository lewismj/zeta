# The loop macro tests
import pytest
from pathlib import Path
from zeta.interpreter import Interpreter


def test_loop1():
    # Load std.lisp to provide the loop macro
    root = Path(__file__).resolve().parents[1]
    std = (root / "prelude" / "std.lisp").read_text(encoding="utf-8")

    itp = Interpreter()
    itp.eval_prelude('''
    (defmacro loop1 (times &body body)
  `(dotimes (i ,times)
     ,@body))
    ''')

    code = """
        (loop1 4 (list i (* i 2)))
    """
    assert itp.eval(code) == [3, 6]

def test_loop2():
    itp = Interpreter()
    itp.eval_prelude('''
        (defun append (&rest xss)
            (if (== xss Nil) Nil
            (apply join xss)))
      
        (defmacro loop1 (times form)
          `(let ((acc '()))
             (dotimes (i ,times)
               (set! acc (append acc (list ,form))))
             acc))''')

    code = '''(loop1 4 (list i (* i 2)))'''
    assert itp.eval(code) == [[0, 0], [1, 2], [2, 4], [3, 6]]
