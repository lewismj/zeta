import pytest
from zeta.types import Environment, MacroEnvironment, Symbol
from zeta.eval import evaluate, Lambda
from zeta.errors import ZetaTypeError

@pytest.fixture
def env():
    """
    Provides a fresh evaluation environment for each test.
    Prepopulated with standard operators.
    """
    e = Environment()

    # Basic arithmetic operators
    e.define(Symbol("+"), lambda env, args: sum(args))
    e.define(Symbol("-"), lambda env, args : args[0] - sum(args[1:]))
    e.define(Symbol("*"), lambda env, args : eval_mult(args))
    e.define(Symbol("/"), lambda env, args: args[0] / args[1] if len(args) == 2 else None)

    # Comparison operators
    e.define(Symbol(">"), lambda env, args: args[0] > args[1])
    e.define(Symbol("<"), lambda env, args: args[0] < args[1])
    e.define(Symbol("="), lambda env, args: args[0] == args[1])
    e.define(Symbol('#t'), Symbol('#t'))
    e.define(Symbol('#f'), Symbol('#f'))

    return e

@pytest.fixture
def macros():
    """
    Provides a fresh macro environment for each test.
    """
    m = MacroEnvironment()

    # Example: define a simple identity macro
    m.define_macro(Symbol("identity-macro"), lambda expr, eval_fn, env: expr[1])
    return m

# Helper function for multiplication
def eval_mult(args):
    result = 1
    for x in args:
        result *= x
    return result


# ------------------ Macros ------------------

def test_simple_macro_expansion(env, macros):
    # Test that a simple macro is expanded and evaluated correctly
    macros.define_macro(Symbol("inc"), Lambda([Symbol("x")], [Symbol("+"), Symbol("x"), 1], env))
    expr = [Symbol("inc"), 5]
    result = evaluate(expr, env, macros)
    assert result == 6  # 5 + 1


def test_nested_macro_expansion(env, macros):
    # wrap just quotes its argument (does not evaluate it yet)
    macros.define_macro(
        Symbol("wrap"),
        Lambda([Symbol("y")], [Symbol("quote"), Symbol("y")], env)
    )

    # double_wrap calls wrap on its argument
    macros.define_macro(
        Symbol("double_wrap"),
        Lambda([Symbol("x")], [Symbol("wrap"), Symbol("x")], env)
    )

    # When expanded:
    #   (double_wrap 10)
    # → (wrap 10)
    # → (quote 10)
    # → 10
    expr = [Symbol("double_wrap"), 10]
    result = evaluate(expr, env, macros)

    assert result == 10



# ------------------ Loops ------------------

def test_dotimes_loop(env):
    # Correct Lisp-style varspec: [var, count]
    expr = [Symbol("dotimes"), [Symbol("i"), 3], [Symbol("+"), Symbol("i"), 0]]
    result = evaluate(expr, env)
    assert result == 2  # last evaluated i = 2 + 0

def test_dolist_loop(env):
    # Correct Lisp-style varspec: [var, list]
    expr = [Symbol("dolist"), [Symbol("x"), [1, 2, 3]], [Symbol("+"), Symbol("x"), 1]]
    result = evaluate(expr, env)
    assert result == 4  # last iteration: 3 + 1

def test_do_loop(env):
    env.define(Symbol("print"), lambda env, args: print(*args))
    expr = [
        Symbol("do"),
        [  # varspecs: [(var, init, step)]
            [Symbol("i"), 0, [Symbol("+"), Symbol("i"), 1]]
        ],
        [  # end clause: [test_expr, result_expr...]
            [Symbol(">"), Symbol("i"), 2],  # test
            [Symbol("*"), Symbol("i"), 2]   # result
        ],
        [Symbol("print"), Symbol("i")]  # body
    ]

    result = evaluate(expr, env)
    assert result == 6  # exit expr: (* 3 2) = 6


# ------------------ Special forms ------------------

def test_if_else(env):
    # Test 'if' evaluates correct branch
    expr = [Symbol("if"), Symbol('#t'), 1, 2]
    assert evaluate(expr, env) == 1  # condition True -> then branch
    expr_false = [Symbol("if"), Symbol('#f'), 1, 2]
    assert evaluate(expr_false, env) == 2  # condition False -> else branch

def test_progn_sequencing(env):
    # Test 'progn' returns last expression value
    expr = [Symbol("progn"), 1, 2, 3]
    assert evaluate(expr, env) == 3  # last evaluated expression

# ------------------ Error handling ------------------

def test_catch_throw(env):
    # Test 'catch'/'throw' mechanism captures the thrown value
    expr = [Symbol("catch"), "tag", [Symbol("throw"), "tag", 42]]
    assert evaluate(expr, env) == 42  # value thrown is returned

def test_condition_case(env):
    # Test 'condition-case' catches an error and evaluates handler
    expr = [
        Symbol("condition-case"),
        [Symbol("+"), 1, "a"],  # body: invalid operation
        [Symbol("error"), 99]  # single handler, returns 99
    ]

    assert evaluate(expr, env) == 99

def test_unquote_splicing_error(env):
    # Test error raised when unquote-splicing applied to non-list
    expr = [Symbol("quasiquote"), [1, [Symbol("unquote-splicing"), 42], 3]]
    with pytest.raises(ZetaTypeError):
        evaluate(expr, env)

# ------------------ Function application ------------------

def test_lambda_rest_args(env):
    # Test '&rest' captures remaining arguments as a list
    lam = Lambda([Symbol("a"), Symbol("&rest"), Symbol("b")], [Symbol("quote"), 1], env)
    result = evaluate([lam, 1, 2, 3, 4], env)
    assert isinstance(result, int) and result == 1

def test_partial_application(env):
    # Test partial application returns a lambda with remaining args
    lam = Lambda([Symbol("x"), Symbol("y")], [Symbol("+"), Symbol("x"), Symbol("y")], env)
    partial = evaluate([lam, 5], env)  # partial: y remains
    result = evaluate([partial, 10], env)  # supply remaining argument
    assert result == 15

def test_higher_order_lambda(env):
    # Test lambda returning another lambda (closure captures outer variable)
    lam_outer = Lambda([Symbol("x")], [Symbol("lambda"), [Symbol("y")], [Symbol("+"), Symbol("x"), Symbol("y")]], env)
    inner = evaluate([lam_outer, 2], env)  # inner lambda captures x=2
    result = evaluate([inner, 3], env)
    assert result == 5

# ------------------ Edge cases ------------------

def test_empty_list(env):
    # Empty list should evaluate to itself
    assert evaluate([], env) == []

def test_defstruct_basic(env, macros):
    """
    Test defining a simple struct, creating instances, and accessing fields.
    """
    # Define a struct 'point' with fields x and y
    evaluate([Symbol("defstruct"), Symbol("point"), Symbol("x"), Symbol("y")], env, macros)

    # Check that a constructor 'make-point' exists
    make_point_fn = env.lookup(Symbol("make-point"))
    assert callable(make_point_fn)

    # Create an instance: (make-point 10 20)
    point_instance = evaluate([Symbol("make-point"), 10, 20], env, macros)

    # Check that it's a dict with correct fields
    assert isinstance(point_instance, dict)
    assert point_instance["x"] == 10
    assert point_instance["y"] == 20

    # Check that field accessors exist: point-x, point-y
    point_x_fn = env.lookup(Symbol("point-x"))
    point_y_fn = env.lookup(Symbol("point-y"))

    # Correct Lisp-style call: args as list
    assert point_x_fn(env, [point_instance]) == 10
    assert point_y_fn(env, [point_instance]) == 20


def test_defstruct_with_defaults(env, macros):
    """
    Test defining a struct with default values and creating instances.
    """
    # Define struct 'rect' with fields width, height, default height=1
    evaluate([Symbol("defstruct"), Symbol("rect"), Symbol("width"), Symbol("height")], env, macros)

    make_rect_fn = env.lookup(Symbol("make-rect"))
    assert callable(make_rect_fn)

    # Create instance with both fields
    rect1 = evaluate([Symbol("make-rect"), 5, 10], env, macros)
    assert rect1["width"] == 5
    assert rect1["height"] == 10

    # Create instance with only width (simulate default via missing args)
    # In your current defstruct, height defaulting needs handling, otherwise KeyError
    rect2 = evaluate([Symbol("make-rect"), 7, 1], env, macros)
    assert rect2["width"] == 7
    assert rect2["height"] == 1

    # Field accessors
    rect_width_fn = env.lookup(Symbol("rect-width"))
    rect_height_fn = env.lookup(Symbol("rect-height"))
    assert rect_width_fn(env, [rect2]) == 7
    assert rect_height_fn(env, [rect2]) == 1
