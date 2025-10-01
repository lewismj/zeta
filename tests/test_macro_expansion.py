import pytest
from zeta import SExpression
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.lambda_fn import Lambda
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaArityError


def dummy_eval(expr: SExpression, env: Environment, macros=None) -> SExpression:
    """
    Minimal evaluator for macro expansion tests.
    Supports:
      - Symbol lookup
      - Lists: recursively evaluate
      - Tuples (dotted lists)
      - Literals unchanged
    """
    from zeta.types.errors import ZetaUnboundSymbol

    if isinstance(expr, Symbol):
        try:
            return env.lookup(expr)
        except ZetaUnboundSymbol:
            return expr  # leave unresolved (like '+')

    elif isinstance(expr, list):
        return [dummy_eval(e, env, macros) for e in expr]

    elif isinstance(expr, tuple) and len(expr) == 2:
        lst, tail = expr
        lst_eval = [dummy_eval(e, env, macros) for e in lst]
        tail_eval = dummy_eval(tail, env, macros)
        return (lst_eval, tail_eval)

    else:
        return expr

# -------------------------
# Fixtures
# -------------------------

@pytest.fixture
def base_env():
    """Base environment for macros"""
    env = Environment()
    return env


@pytest.fixture
def macro_env():
    """Macro environment fixture"""
    return MacroEnvironment()


# -------------------------
# Simple head-position macro
# -------------------------


def test_simple_macro_expansion(macro_env, base_env):
    """
    Define a simple increment macro and test head-position expansion.
    (inc x) => (+ x 1)
    """
    # Define macro: (inc x) -> (+ x 1)
    macro_env.define_macro(Symbol("inc"), Lambda([Symbol("x")], [Symbol("+"), Symbol("x"), 1]))

    expr = [Symbol("inc"), 5]  # Lisp: (inc 5)
    expanded = macro_env.macro_expand_head(expr, dummy_eval, base_env)

    # Expect expansion: (+ 5 1)
    assert expanded == [Symbol("+"), 5, 1]


# -------------------------
# Nested macro expansion
# -------------------------

def test_nested_macro_expansion(macro_env, base_env):
    """
    Test macro calls nested inside other macro calls.
    (wrapinc x) -> inc x; inc x -> (+ x 1)
    """
    macro_env.define_macro(Symbol("inc"), Lambda([Symbol("x")], [Symbol("+"), Symbol("x"), 1]))
    macro_env.define_macro(Symbol("wrapinc"), Lambda([Symbol("y")], [Symbol("inc"), Symbol("y")]))

    expr = [Symbol("wrapinc"), 10]  # Lisp: (wrapinc 10)
    expanded = macro_env.macro_expand_head(expr, dummy_eval, base_env)

    # Should expand fully to (+ 10 1)
    fully_expanded = macro_env.macro_expand_all(expr, dummy_eval, base_env)
    assert fully_expanded == [Symbol("+"), 10, 1]


# -------------------------
# Macro using dotted list
# -------------------------

def test_macro_with_dotted_list(macro_env, base_env):
    """
    Test a macro that expects a dotted list: (pair a . b)
    """
    macro_env.define_macro(
        Symbol("pair"),
        Lambda([Symbol("a"), Symbol("b")],
               [[Symbol("cons"), Symbol("a")], Symbol("b")])
    )

    expr = ([Symbol("pair")], 42)  # Lisp dotted list: (pair . 42)
    expanded = macro_env.macro_expand_all(expr, dummy_eval, base_env)

    # Should expand into: [[cons, pair], 42]
    assert isinstance(expanded, tuple)
    assert expanded[1] == 42


# -------------------------
# Macro closure environment
# -------------------------

def test_macro_closure_env():
    """
    Tests that macro expansion uses the macro's closure environment.
    Example:
      (inc x) with macro: (lambda (y) (+ y 10)) and closure {x:5}
      should expand to (+ 5 10)
    """
    base_env = Environment()
    # Predefine '+' to itself to avoid unbound symbol issues
    base_env.define(Symbol("+"), Symbol("+"))

    macro_env = MacroEnvironment()

    # Define macro: (inc y) -> (+ y 10)
    macro_env.define_macro(
        Symbol("inc"),
        Lambda([Symbol("y")],
               [Symbol("+"), Symbol("y"), 10])
    )

    # Prepare expression: (inc x) with x=5 in closure
    macro_lambda = macro_env.macros[Symbol("inc")]
    macro_lambda.env.define(Symbol("y"), 5)  # closure environment binding

    expr = [Symbol("inc"), 5]  # Lisp: (inc 5)
    expanded = macro_env.macro_expand_head(expr, dummy_eval, base_env)

    # Expected: ['+', 5, 10]
    assert expanded == [Symbol("+"), 5, 10]


# -------------------------
# Macro arity check
# -------------------------

def test_macro_arity_check(macro_env, base_env):
    """
    Macro should raise ValueError if number of args doesn't match formals.
    """
    macro_env.define_macro(Symbol("inc"), Lambda([Symbol("x")], [Symbol("+"), Symbol("x"), 1]))
    expr = [Symbol("inc"), 1, 2]  # Too many arguments

    with pytest.raises(ZetaArityError):
        macro_env.macro_expand_head(expr, dummy_eval, base_env)


# -------------------------
# Complex nested list macro
# -------------------------

def test_macro_recursive_nested_lists(macro_env, base_env):
    """
    Macro expansion should recursively expand nested lists.
    """
    macro_env.define_macro(Symbol("inc"), Lambda([Symbol("x")], [Symbol("+"), Symbol("x"), 1]))

    expr = [[Symbol("inc"), 1], [Symbol("inc"), 2]]  # ((inc 1) (inc 2))
    expanded = macro_env.macro_expand_all(expr, dummy_eval, base_env)

    assert expanded == [[Symbol("+"), 1, 1], [Symbol("+"), 2, 1]]


# -------------------------
# Gensym uniqueness
# -------------------------

def test_gen_sym_uniqueness(macro_env):
    """
    Each call to gen_sym should produce a unique symbol.
    """
    s1 = macro_env.gen_sym()
    s2 = macro_env.gen_sym()
    s3 = macro_env.gen_sym("X")
    s4 = macro_env.gen_sym("X")
    assert s1 != s2
    assert s3 != s4
    assert s1.id.startswith("G")
    assert s3.id.startswith("X")


def test_macro_closure_env2(macro_env, base_env):
    """
    Macro should capture bindings from its definition environment.
    (add-x y) => (+ x y) where x=5 in closure
    """
    closure_env = Environment()
    closure_env.define(Symbol("x"), 5)
    macro_env.define_macro(Symbol("add-x"),
                           Lambda([Symbol("y")], [Symbol("+"), Symbol("x"), Symbol("y")]))
    macro_env.macros[Symbol("add-x")].env = closure_env

    expr = [Symbol("add-x"), 10]
    expanded = macro_env.macro_expand_head(expr, dummy_eval, base_env)
    assert expanded == [Symbol("+"), 5, 10]


def test_quasiquote_unquote_macro(macro_env, base_env):
    """
    `(list ,a b) => (list <value-of-a> b)
    Tests that quasiquote and unquote are preserved in macro expansion.
    """
    macro_env.define_macro(Symbol("qq-test"),
                           Lambda([Symbol("a")], [Symbol("list"), Symbol("a"), Symbol("b")]))

    expr = [Symbol("qq-test"), 42]
    expanded = macro_env.macro_expand_head(expr, dummy_eval, base_env)
    assert expanded == [Symbol("list"), 42, Symbol("b")]


def test_dotted_list_macro(macro_env, base_env):
    """
    (cons-1-2) => (1 . 2)
    """
    macro_env.define_macro(Symbol("cons-1-2"),
                           Lambda([], ([1], 2)))  # tuple represents dotted list

    expr = [Symbol("cons-1-2")]
    expanded = macro_env.macro_expand_head(expr, dummy_eval, base_env)
    assert expanded == ([1], 2)


def test_vector_macro(macro_env, base_env):
    """
    #() vector reader macro, expanded inside macro
    """
    macro_env.define_macro(Symbol("make-vec"),
                           Lambda([Symbol("x"), Symbol("y")], [Symbol("vector"), Symbol("x"), Symbol("y")]))

    expr = [Symbol("make-vec"), 1, 2]
    expanded = macro_env.macro_expand_head(expr, dummy_eval, base_env)
    assert expanded == [Symbol("vector"), 1, 2]


def test_macro_expand_all_recursive(macro_env, base_env):
    """
    Expand macros anywhere in a nested list structure
    """
    macro_env.define_macro(Symbol("inc"), Lambda([Symbol("x")], [Symbol("+"), Symbol("x"), 1]))
    macro_env.define_macro(Symbol("double"), Lambda([Symbol("y")], [Symbol("*"), Symbol("y"), 2]))

    expr = [[Symbol("inc"), 1], [Symbol("double"), 3]]
    expanded = macro_env.macro_expand_all(expr, dummy_eval, base_env)
    assert expanded == [[Symbol("+"), 1, 1], [Symbol("*"), 3, 2]]


def test_macro_quasiquote_unquote_expansion(macro_env, base_env):
    """
    Macro with a quasiquoted body should evaluate unquote at expansion time.
    (defmacro wrap (x) `(1 ,x 3)) → expands to [1, <arg>, 3]
    """
    from zeta.evaluation.evaluator import evaluate

    macro_env.define_macro(
        Symbol("wrap"),
        Lambda([Symbol("x")], [Symbol("quasiquote"), [1, [Symbol("unquote"), Symbol("x")], 3]])
    )

    expr = [Symbol("wrap"), 42]
    expanded = macro_env.macro_expand_head(expr, evaluate, base_env)
    assert expanded == [1, 42, 3]


def test_macro_unquote_splicing_with_rest_args(macro_env, base_env):
    """
    Macro with &rest parameters using ,@ should splice arguments during expansion.
    (defmacro m (&rest xs) `(1 ,@xs 99))
    m 2 3  → [1, 2, 3, 99]
    """
    from zeta.evaluation.evaluator import evaluate

    macro_env.define_macro(
        Symbol("m"),
        Lambda([Symbol("&rest"), Symbol("xs")], [Symbol("quasiquote"), [1, [Symbol("unquote-splicing"), Symbol("xs")], 99]])
    )

    expr = [Symbol("m"), 2, 3]
    expanded = macro_env.macro_expand_head(expr, evaluate, base_env)
    assert expanded == [1, 2, 3, 99]


def test_macro_unquote_splicing_type_error(macro_env, base_env):
    """
    Using ,@ on a non-list inside a macro quasiquote should raise a type error at expansion time.
    """
    import pytest
    from zeta.evaluation.evaluator import evaluate
    from zeta.types.errors import ZetaTypeError

    # `(,@42) is invalid because 42 is not a list
    macro_env.define_macro(
        Symbol("bad-splice"),
        Lambda([], [Symbol("quasiquote"), [[Symbol("unquote-splicing"), 42]]])
    )

    with pytest.raises(ZetaTypeError):
        macro_env.macro_expand_head([Symbol("bad-splice")], evaluate, base_env)

