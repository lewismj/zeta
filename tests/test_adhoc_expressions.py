import pytest

from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.builtin.env_builtin import register as register_builtins
from zeta.builtin.macro_builtin import register as register_macros
from zeta.reader.parser import lex, TokenStream
from zeta.evaluation.evaluator import evaluate
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil

programs = [
    ("(define add2 (lambda (a b) (+ a b)))", Nil),
    ("(define add2_partial (add2 5))", Nil),
    ("(add2_partial 10)", 15),
    ("(defmacro unless (cond body) `(if (not ,cond) ,body))", Nil),
    ("(define x 10)", Nil),
    ("(define y 20)", Nil),
    ("(unless (> x y) (define z 100))", Nil),
    ("z", 100),
    ("(+ x y)", 30),
    ("(defmacro inc (x) `(+ ,x 1))", Nil),
    ("(defmacro dec (x) `(+ ,x -1))", Nil),
    ("(inc x)", 11),
    ("(dec y)", 19),
    ("(* x y)", 200),
    ("(if (> x y) x y)", 20),
    ("(quote (a b c))", [Symbol('a'), Symbol('b'), Symbol('c')]),
    ("((lambda (a b) (+ a b)) 5 7)", 12),
    ("(car (list 1 2 3))", 1),
    ("(cdr (list 1 2 3))", [2, 3]),
    ("(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))", Nil),
    ("(fact 5)", 120),
    ("(defstruct person name age)", Nil),
    ("(define p (make-person \"Fred\" 30))", Nil),
    ("p", {"__type__": Symbol('person'), Symbol('name'): 'Fred', Symbol('age'): 30}),
    ("(person-name p)", "Fred"),
    ("(person-age p)", 30),
    ( '''(progn
          (define a 1)
          (define b 2)
          (+ a b)))''', 3),
    ('(define counter 0)', Nil),
    ('''
      (progn
        (dotimes (i 5) (set counter (+ counter 1)))
        counter)
    ''', 5),
    ('''
    (defun fib-iter (n)
  (let ((a 0) (b 1) (temp 0))
    (dotimes (i n a)
      (set temp b)
      (set b (+ a b))
      (set a temp))))
    ''', Nil),
    ('(fib-iter 15)',610),
    (
    '''(catch 'my-tag (/ 1 0))''',
    {'tag': 'system-error', 'exception': 'ZeroDivisionError', 'message': 'Division by zero'} )
]



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
