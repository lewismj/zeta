import pytest
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol
from zeta.types.lambda_fn import Lambda
from zeta.evaluation.evaluator import evaluate


@pytest.fixture
def env():
    # Minimal environment; arithmetic not required for macroexpand structure tests
    return Environment()


@pytest.fixture
def macros():
    return MacroEnvironment()


def test_macroexpand1_simple(env, macros):
    # (defmacro inc (x) `(+ ,x 1))
    macros.define_macro(
        Symbol("inc"),
        Lambda(
            [Symbol("x")],
            [
                Symbol("quasiquote"),
                [Symbol("+"), [Symbol("unquote"), Symbol("x")], 1],
            ],
            env,
        ),
    )

    # (macroexpand-1 '(inc 5)) => (+ 5 1)
    form = [Symbol("macroexpand-1"), [Symbol("quote"), [Symbol("inc"), 5]]]
    assert evaluate(form, env, macros) == [Symbol("+"), 5, 1]


def test_macroexpand_full_nested(env, macros):
    # wrapinc -> (inc x); inc -> (+ x 1)
    macros.define_macro(
        Symbol("inc"),
        Lambda(
            [Symbol("x")],
            [
                Symbol("quasiquote"),
                [Symbol("+"), [Symbol("unquote"), Symbol("x")], 1],
            ],
            env,
        ),
    )
    macros.define_macro(
        Symbol("wrapinc"),
        Lambda(
            [Symbol("y")],
            [
                Symbol("quasiquote"),
                [Symbol("inc"), [Symbol("unquote"), Symbol("y")]],
            ],
            env,
        ),
    )

    # (macroexpand '(wrapinc 10)) => (+ 10 1)
    form = [Symbol("macroexpand"), [Symbol("quote"), [Symbol("wrapinc"), 10]]]
    assert evaluate(form, env, macros) == [Symbol("+"), 10, 1]


def test_macroexpand_does_not_descend_into_quasiquote(env, macros):
    # Macro that returns a quasiquoted template `(list ,x)
    macros.define_macro(
        Symbol("qq"),
        Lambda(
            [Symbol("x")],
            [
                Symbol("quasiquote"),
                [Symbol("list"), [Symbol("unquote"), Symbol("x")]],
            ],
            env,
        ),
    )

    # The expansion result is the quasiquoted structure; macroexpand should not
    # recurse into it to expand head positions inside the quasiquote.
    form = [Symbol("macroexpand"), [Symbol("quote"), [Symbol("qq"), [Symbol("inc"), 1]]]]
    expansion = evaluate(form, env, macros)
    # Expect constructed data: (list (inc 1)); macro body quasiquote is evaluated
    assert expansion == [Symbol("list"), [Symbol("inc"), 1]]


def test_macroexpand_non_macro_form_returns_unchanged(env, macros):
    # Non-macro symbol head: should return the same form
    form = [Symbol("macroexpand"), [Symbol("quote"), [Symbol("+"), 1, 2]]]
    assert evaluate(form, env, macros) == [Symbol("+"), 1, 2]

    # macroexpand-1 similarly should return unchanged when head is not a macro
    form1 = [Symbol("macroexpand-1"), [Symbol("quote"), [Symbol("+"), 1, 2]]]
    assert evaluate(form1, env, macros) == [Symbol("+"), 1, 2]
