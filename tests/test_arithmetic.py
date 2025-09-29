import pytest
from zeta.parser import TokenStream, lex
from zeta.types.environment import Environment
from zeta.evaluation.evaluator import evaluate
from zeta.evaluation.special_forms.throw_catch_form import ThrowException
from zeta.builtin.env_builtin import register

@pytest.fixture
def env():
    """Fresh environment with builtins loaded."""
    e = Environment()
    register(e)
    return e

@pytest.mark.parametrize(
    "source,expected",
    [
        ("(+ 1 2 3)", 6),
        ("(- 10 3 2)", 5),
        ("(* 2 3 4)", 24),
        ("(/ 12 3)", 4),
        ("(+ (* 2 3) (- 10 4))", 12),
        ("(/ (+ 20 10) (* 2 5))", 3),
        ("(- (+ 10 5) (* 2 3))", 9),
        ("(+ 1 2.5 3)", 6.5),
        ("(* 1 2 3 4 5 6)", 720),
        ("(+ -1 5 -3)", 1),
        ("(- -10 -5)", -5),
        ("(* -2 3)", -6),
        ("(/ -12 3)", -4),
        ("(+)", 0),
        ("(*)", 1),
        ("(+ 1 (* 2 (+ 3 4) (- 10 6)))", 57),  # 1 + 2*(7*4)
        ("(/ (* (+ 8 2) 5) (- 20 10))", 5),
        ("(catch 'my-tag (throw 'my-tag 42))", 42),
        ("(catch 'other-tag (throw 'my-tag 100) 99)", 99),
        ("(catch 'any (/ 1 0) 999)", 999),
        ("(catch 'any (/))", {'tag': 'system-error', 'exception': 'ZetaArityError', 'message': '/ requires at least 1 argument'}),
        ("(catch 'outer (catch 'inner (throw 'inner 10)))", 10),
        ("(catch 'foo (throw 'bar 123) 999)", 999),
    ]
)
def test_lisp_arithmetic_and_throw(env, source, expected):
    tokens = TokenStream(lex(source))
    exprs = list(tokens.parse_all())
    result = None
    for expr in exprs:
        result = evaluate(expr, env)
    assert result == expected