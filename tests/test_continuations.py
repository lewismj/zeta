import pytest
from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol


def eval_lisp(itp: Interpreter, code: str):
    return itp.eval(code)


def test_call_cc_escape_basic():
    itp = Interpreter()
    # Immediate escape using continuation
    assert eval_lisp(itp, "(call/cc (lambda (k) (k 42) 99))") == 42


def test_call_cc_normal_return():
    itp = Interpreter()
    # If continuation not used, returns body value
    assert eval_lisp(itp, "(call/cc (lambda (k) 7))") == 7


def test_call_cc_in_expression():
    itp = Interpreter()
    # Use in arithmetic position
    assert eval_lisp(itp, "(+ 1 (call/cc (lambda (k) (k 10))))") == 11
