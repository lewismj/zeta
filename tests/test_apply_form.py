import pytest

from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.errors import ZetaTypeError, ZetaArityError
from zeta.reader.parser import lex, TokenStream
from zeta.evaluation.evaluator import evaluate
from zeta.builtin import env_builtin
from zeta.builtin import macro_builtin


def eval_source(source: str) -> tuple[Environment, MacroEnvironment, object]:
    """Parse and evaluate all forms in source, returning last result with envs."""
    env = Environment()
    macros = MacroEnvironment()
    # Register standard builtins and macros
    env_builtin.register(env)
    macro_builtin.register(macros)

    stream = TokenStream(lex(source))
    last = None
    for form in stream.parse_all():
        last = evaluate(form, env, macros)
    return env, macros, last


def parse_only(source: str):
    stream = TokenStream(lex(source))
    return list(stream.parse_all())


# -----------------------------
# Apply form basic behaviors
# -----------------------------

def test_apply_builtin_plus_with_list():
    # (apply + (list 1 2 3)) => 6
    _, _, result = eval_source("(apply + (list 1 2 3))")
    assert result == 6


def test_apply_lambda_defined_via_define():
    # Define a function and then apply it using a list of args
    src = """
    (define add2 (lambda (a b) (+ a b)))
    (apply add2 (list 10 20))
    """
    _, _, result = eval_source(src)
    assert result == 30


def test_apply_uses_symbol_resolution():
    # Using a symbol that resolves from environment
    src = """
    (define mul2 (lambda (x y) (* x y)))
    (apply mul2 (list 4 5))
    """
    _, _, result = eval_source(src)
    assert result == 20


def test_apply_args_must_evaluate_to_list_error():
    # (apply + 123) should raise a type error
    with pytest.raises(ZetaTypeError) as exc:
        eval_source("(apply + 123)")
    assert "Apply arguments must evaluate to a list" in str(exc.value)


def test_apply_arity_error_from_lambda():
    # (apply f (list)) where f expects one arg -> arity error
    src = """
    (define id (lambda (x) x))
    (apply id (list))
    """
    with pytest.raises(ZetaArityError):
        eval_source(src)
