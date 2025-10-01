import pytest

from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil
from zeta.types.errors import ZetaArityError
from zeta.reader.parser import lex, TokenStream
from zeta.evaluation.evaluator import evaluate
from zeta.builtin import env_builtin, macro_builtin


def eval_source(source: str):
    env = Environment()
    macros = MacroEnvironment()
    env_builtin.register(env)
    macro_builtin.register(macros)
    stream = TokenStream(lex(source))
    last = None
    for form in stream.parse_all():
        last = evaluate(form, env, macros)
    return env, macros, last


def test_lambda_key_binding_basic():
    src = """
    (define f (lambda (x &key y) y))
    (f 10 :y 7)
    """
    _, _, result = eval_source(src)
    assert result == 7


def test_lambda_key_default_nil():
    src = """
    (define f (lambda (x &key y) (nil? y)))
    (f 10)
    """
    _, _, result = eval_source(src)
    assert result == Symbol("#t")


def test_lambda_key_unknown_keyword_error():
    src = """
    (define f (lambda (x &key y) y))
    (f 1 :z 2)
    """
    with pytest.raises(ZetaArityError) as exc:
        eval_source(src)
    assert "Unknown keyword argument :z" in str(exc.value)


def test_lambda_key_odd_pairs_error():
    src = """
    (define f (lambda (x &key y) y))
    (f 1 :y)
    """
    with pytest.raises(ZetaArityError) as exc:
        eval_source(src)
    assert "Keyword arguments must be in pairs" in str(exc.value)


def test_macro_with_key_binding():
    # Define a macro that returns the value of :x
    src = """
    (defmacro mk (&key x) x)
    (mk :x 42)
    """
    _, _, result = eval_source(src)
    assert result == 42
