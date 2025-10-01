import pytest
from zeta.reader.parser import lex, TokenStream
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol
from zeta.evaluation.evaluator import evaluate


def test_reader_struct_maps_to_defstruct_constructor():
    env = Environment()
    macros = MacroEnvironment()

    # Define a struct: (defstruct point x y)
    evaluate([Symbol("defstruct"), Symbol("point"), Symbol("x"), Symbol("y")], env, macros)

    # Parse a reader struct literal: #s(point x 10 y 20) -> (make-point 10 20)
    stream = TokenStream(lex("#s(point x 10 y 20)"))
    expr = next(stream.parse_all())

    # Ensure parser expansion shape is the expected constructor call
    assert isinstance(expr, list) and expr[0] == Symbol("make-point") and expr[1:] == [10, 20]

    # Evaluate and verify contents via accessors
    instance = evaluate(expr, env, macros)
    point_x = env.lookup(Symbol("point-x"))
    point_y = env.lookup(Symbol("point-y"))
    assert point_x(env, [instance]) == 10
    assert point_y(env, [instance]) == 20


def test_reader_struct_order_is_positional():
    env = Environment()
    macros = MacroEnvironment()

    # Define struct: (defstruct pair a b)
    evaluate([Symbol("defstruct"), Symbol("pair"), Symbol("a"), Symbol("b")], env, macros)

    # Supply fields out of declaration order; mapping is positional as written
    stream = TokenStream(lex("#s(pair b 2 a 1)"))
    expr = next(stream.parse_all())

    # Expansion preserves written order of values: (make-pair 2 1)
    assert expr == [Symbol("make-pair"), 2, 1]

    inst = evaluate(expr, env, macros)
    a = env.lookup(Symbol("pair-a"))
    b = env.lookup(Symbol("pair-b"))
    assert a(env, [inst]) == 2
    assert b(env, [inst]) == 1
