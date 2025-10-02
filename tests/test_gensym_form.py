import pytest
from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaTypeError, ZetaArityError


def test_gensym_basic_sequential():
    itp = Interpreter()
    s1 = itp.eval("(gensym)")
    s2 = itp.eval("(gensym)")

    assert isinstance(s1, Symbol)
    assert isinstance(s2, Symbol)
    # In a fresh interpreter, the first two should be G1 and G2
    assert s1 == Symbol("G1")
    assert s2 == Symbol("G2")


def test_gensym_with_symbol_prefix():
    itp = Interpreter()
    out = itp.eval("(gensym 't)")
    assert isinstance(out, Symbol)
    assert out == Symbol("t1")


def test_gensym_with_string_prefix():
    itp = Interpreter()
    out = itp.eval('(gensym "tmp")')
    assert isinstance(out, Symbol)
    assert out == Symbol("tmp1")


def test_gensym_errors():
    itp = Interpreter()
    # Too many args
    with pytest.raises(ZetaArityError):
        itp.eval('(gensym "a" "b")')
    # Bad type for prefix
    with pytest.raises(ZetaTypeError):
        itp.eval('(gensym 42)')


def test_gensym_in_macro_expansion():
    itp = Interpreter()
    # Minimal let/progn are special forms already available in the evaluator.
    # This macro uses gensym to create a fresh temporary binding name.
    itp.eval_prelude(r'''
      (defmacro capture (x)
        (let ((g (gensym "tmp")))
          `(let ((,g ,x)) ,g)))
    ''')
    assert itp.eval('(capture 42)') == 42
    # Ensure distinct calls don’t capture each other’s temp vars
    assert itp.eval('(list (capture 1) (capture 2) (capture 3))') == [1, 2, 3]
