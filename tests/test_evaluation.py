import pytest
from zeta.types import Environment, Symbol
from zeta.eval import  evaluate
from zeta import errors

# -----------------------------------------------------
# Fixtures
# -----------------------------------------------------

def  do_sum(_, args):
    print(args)
    return sum(args)

@pytest.fixture
def env():
    env = Environment()
    env.define(Symbol("+"), do_sum)
    env.define(Symbol("-"), lambda _, args: args[0] - sum(args[1:]))
    env.define(Symbol("*"), lambda _, args : eval("*".join(map(str, args))))
    env.define(Symbol("/"), lambda _, args: args[0] // args[1] if args[1] != 0 else None)
    env.define(Symbol("x"), 42)
    env.define(Symbol("y"), 100)
    env.define(Symbol("#t"), Symbol("#t"))
    env.define(Symbol("#f"), Symbol("#f"))
    return env

# -----------------------------------------------------
# Tests
# -----------------------------------------------------

def test_self_evaluating_literals(env):
    assert evaluate(1, env) == 1
    assert evaluate(3.14, env) == 3.14
    assert evaluate("hello", env) == "hello"
    assert evaluate(None, env) is None

def test_symbol_lookup(env):
    assert evaluate(Symbol("x"), env) == 42
    assert evaluate(Symbol("y"), env) == 100
    # assert evaluate(['x', 'y'], env) == [42, 100]
    with pytest.raises(errors.ZetaUnboundSymbol):
        evaluate(Symbol("z"), env)

def test_quote(env):
    expr = [Symbol("quote"), [1, 2, 3]]
    assert evaluate(expr, env) == [1, 2, 3]

def test_simple_expression(env):
    expr = [Symbol("+"), 1, 2]
    assert evaluate(expr, env) == 3

def test_lambda_simple(env):
    expr = [Symbol("lambda"), [Symbol("a"), Symbol("b")], [Symbol("+"), Symbol("a"), Symbol("b")]]
    lam = evaluate(expr, env)
    result = evaluate([lam, 2, 3], env)
    assert result == 5

def test_lambda_partial_application(env):
    expr = [Symbol("lambda"), [Symbol("a"), Symbol("b")], [Symbol("+"), Symbol("a"), Symbol("b")]]
    lam = evaluate(expr, env)
    partially_applied = evaluate([lam, 5], env)  # returns new Lambda
    result = evaluate([partially_applied, 10], env)
    assert result == 15

def test_define_and_lookup(env):
    expr = [Symbol("define"), Symbol("y"), 100]
    evaluate(expr, env)
    assert evaluate(Symbol("y"), env) == 100

def test_if_expression(env):
    expr = [Symbol("if"), Symbol('#t'), 1, 2]
    assert evaluate(expr, env) == 1

    expr = [Symbol("if"), Symbol('#f'), 1, 2]
    assert evaluate(expr, env) == 2

def test_progn_sequencing(env):
    expr = [Symbol("progn"),
            [Symbol("define"), Symbol("a"), 10],
            [Symbol("define"), Symbol("b"), 20],
            [Symbol("+"), Symbol("a"), Symbol("b")]]
    assert evaluate(expr, env) == 30

def test_function_call(env):
    expr = [Symbol("+"), 1, 2, 3]
    assert evaluate(expr, env) == 6

def test_errors(env):
    # too many args to subtraction
    expr = [Symbol("-"), 1]
    result = evaluate(expr, env)
    assert result == 1  # still valid: (- 1) == 1

    # unbound symbol
    with pytest.raises(errors.ZetaUnboundSymbol):
        evaluate(Symbol("not_defined"), env)

    # invalid special form usage
    with pytest.raises(errors.ZetaArityError):
        evaluate([Symbol("define")], env)
