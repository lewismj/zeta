import pytest

from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.builtin.env_builtin import register as register_builtins
from zeta.builtin.macro_builtin import register as register_macros
from zeta.reader.parser import lex, TokenStream
from zeta.evaluation.evaluator import evaluate

# Reuse the adhoc programs to improve coverage without duplicating them here.
from playground.adhoc.adhoc_expressions import programs


@pytest.fixture(scope="module")
def runtime():
    env = Environment()
    macros = MacroEnvironment()
    register_builtins(env)
    register_macros(macros)
    return env, macros


@pytest.mark.parametrize("source,expected", programs)
def test_adhoc_programs_eval(runtime, source, expected):
    env, macros = runtime
    tokens = lex(source)
    stream = TokenStream(tokens)
    expr = stream.parse_expr()
    result = evaluate(expr, env, macros)
    assert result == expected
