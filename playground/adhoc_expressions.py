from zeta.env_builtin import register
from zeta.eval import evaluate
from zeta.parser import lex, TokenStream
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.environment import Environment
from zeta.types.errors import ZetaArityError

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
    ("(quote (a b c))", ['a', 'b', 'c']),
    ("((lambda (a b) (+ a b)) 5 7)", 12),
    ("(car (list 1 2 3))", 1),
    ("(cdr (list 1 2 3))", [2, 3]),
    ("(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))", Nil),
    ("(fact 5)", 120),
    ("(defstruct person name age)", Nil),
    ("(define p (make-person \"Fred\" 30))", Nil),
    ("p", {"__type__": 'person', 'name': "Fred", 'age': 30}),
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
    ('(fib-iter 15)',610)
]

# These should be default macros.
# Slight inconsistency, def_macro transformer is fn (args, env), builtins are fn (env, args)...

def let_macro(args, env):
    """
    (let ((var1 val1) (var2 val2) ...) body...)
    => ((lambda (var1 var2 ...) body...) val1 val2 ...)
    """
    if len(args) < 2:
        raise ZetaArityError("let requires bindings and at least one body form")

    bindings = args[0]
    body = args[1:]

    vars_ = [var for var, *_ in bindings]
    vals_ = [val for _, val, *rest in bindings]

    return [['lambda', vars_] + body] + vals_


def defun_macro(args, env):
    if len(args) < 3:
        raise ZetaArityError("defun requires at least 3 arguments: (defun name (params) body...)")

    name = args[0]       # function name (symbol)
    params = args[1]     # parameter list
    body = args[2:]      # body expressions

    # Expand (defun name (params) body...)
    # into (define name (lambda (params) body...))
    return ['define', name, ['lambda', params] + body]


def main():
    env = Environment()

    # We need global macros table with some standard built-in macros.
    macros = MacroEnvironment()
    macros.define_macro(Symbol("defun"), defun_macro)
    macros.define_macro(Symbol("let"), let_macro)

    register(env)

    for program, expected in programs:
        tokens = lex(program)
        stream = TokenStream(tokens)
        expr = stream.parse_expr()
        print(expr)
        result = evaluate(expr, env, macros)
        if result == expected:
            print("Test passed: ", program)
        assert result == expected, f"Test failed: {program} => {result}, expected {expected}"


if __name__ == "__main__":
    main()
