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
    (defmacro loop (times &body body)
  `(dotimes (i ,times)
     ,@body))
    ''')

    code = """
        (loop 4 (list i (* i 2)))
    """
    assert itp.eval(code) == [3, 6]

def test_loop2():
    itp = Interpreter()
    itp.eval_prelude('''
        (defun append (&rest xss)
            (if (== xss Nil) Nil
            (apply join xss)))
      
        (defmacro loop (times form)
          `(let ((acc '()))
             (dotimes (i ,times)
               (set! acc (append acc (list ,form))))
             acc))''')

    code = '''(loop 4 (list i (* i 2)))'''
    assert itp.eval(code) == [[0, 0], [1, 2], [2, 4], [3, 6]]

def test_loop3() :
    itp = Interpreter()
    itp.eval_prelude('''
          (defun evenp (n) (= (mod n 2) 0))
          (defmacro when (test &body body) `(if ,test (progn ,@body)))
          (defmacro setf (name value) `(set! ,name ,value))
          (defun append (&rest xss) (if (== xss Nil) Nil (apply join xss)))
        
          (defmacro loop (times form filter)
            `(let ((acc '()))
               (dotimes (i ,times)
                 (when ,filter
                   (set! acc (append acc (list ,form)))))
               acc))
        ''')
    code = """(loop 10 (list i (* i 2)) (evenp i))"""
    assert itp.eval(code) ==  [[0, 0], [2, 4], [4, 8], [6, 12], [8, 16]]