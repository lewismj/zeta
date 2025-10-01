import pytest
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.builtin import env_builtin


@pytest.fixture
def env_macros():
    env = Environment()
    env_builtin.register(env)
    return env


def test_format_template_positional(env_macros):
    env = env_macros
    # Directly call builtin via environment
    fmt = env.lookup(Symbol("format"))
    assert callable(fmt)
    result = fmt(env, ["Hello {0} + {1} = {2}", 2, 3, 5])
    assert result == "Hello 2 + 3 = 5"


def test_format_join_when_no_template(env_macros):
    env = env_macros
    fmt = env.lookup(Symbol("format"))
    result = fmt(env, ["Hello", "world", 123])
    assert result == "Hello world 123"


def test_print_outputs_and_returns_nil(env_macros, capsys):
    env = env_macros
    pr = env.lookup(Symbol("print"))
    ret = pr(env, ["alpha", 42, Symbol("beta")])
    out = capsys.readouterr().out
    assert out == "alpha 42 beta\n"
    # print returns Nil sentinel
    from zeta.types.nil import Nil
    assert ret is Nil
