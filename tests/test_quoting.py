import pytest
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.evaluation.evaluator import evaluate


@pytest.fixture
def env():
    """Return a fresh environment for each test."""
    e = Environment()
    # simple arithmetic primitives
    e.define(Symbol("+"), lambda env, args: sum(args))
    e.define(Symbol("-"), lambda env, args: args[0] - sum(args[1:]))
    e.define(Symbol("*"), lambda env, args: 1 if not args else eval('*'.join(map(str, args))))
    e.define(Symbol("/"), lambda env, args: 1 if not args else eval('/'.join(map(str, args))))
    return e


def test_quasiquote_simple(env):
    expr = [Symbol("quasiquote"), [1, 2, 3]]
    result = evaluate(expr, env)
    assert result == [1, 2, 3]


def test_quasiquote_with_unquote(env):
    expr = [Symbol("quasiquote"), [1, [Symbol("unquote"), 2], 3]]
    result = evaluate(expr, env)
    assert result == [1, 2, 3]


def test_quasiquote_with_unquote_splicing(env):
    expr = [Symbol("quasiquote"), [1, [Symbol("unquote-splicing"), [2, 3]], 4]]
    result = evaluate(expr, env)
    assert result == [1, 2, 3, 4]


def test_nested_quasiquote(env):
    expr = [Symbol("quasiquote"), [1, [Symbol("quasiquote"), [2, 3]], 4]]
    result = evaluate(expr, env)
    assert result == [1, [Symbol("quasiquote"), [2, 3]], 4]


def test_unquote_splicing_error(env):
    expr = [Symbol("quasiquote"), [1, [Symbol("unquote-splicing"), 42]]]
    with pytest.raises(Exception):
        evaluate(expr, env)


def test_lambda_quasiquote_unquote(env):
    lam_expr = [
        Symbol("lambda"), [Symbol("x")],
        [Symbol("quasiquote"), [1, [Symbol("unquote"), Symbol("x")], 3]]
    ]
    lam = evaluate(lam_expr, env)
    result = evaluate([lam, 10], env)
    assert result == [1, 10, 3]


def test_lambda_quasiquote_unquote_splicing(env):
    lam_expr = [
        Symbol("lambda"), [Symbol("x")],
        [Symbol("quasiquote"), [1, [Symbol("unquote-splicing"), Symbol("x")], 4]]
    ]
    lam = evaluate(lam_expr, env)
    result = evaluate([lam, [2, 3]], env)
    assert result == [1, 2, 3, 4]


def test_partial_application_quasiquote(env):
    lam_expr = [
        Symbol("lambda"), [Symbol("a"), Symbol("b")],
        [Symbol("+"), Symbol("a"), Symbol("b")]
    ]
    lam = evaluate(lam_expr, env)
    partially_applied = evaluate([lam, 5], env)  # returns Lambda
    result = evaluate([partially_applied, 10], env)
    assert result == 15


def test_nested_unquote_in_lambda(env):
    lam_expr = [
        Symbol("lambda"), [Symbol("x")],
        [Symbol("quasiquote"), [[Symbol("unquote"), Symbol("x")]]]
    ]
    lam = evaluate(lam_expr, env)
    result = evaluate([lam, 42], env)
    assert result == [42]
