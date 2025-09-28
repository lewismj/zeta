from zeta.builtins import register
from zeta.eval import evaluate
from zeta.parser import lex, TokenStream
from zeta.types import Environment, MacroEnvironment

programs = [
    ("(define add2 (lambda (a b) (+ a b)))", None),
    ("(define add2_partial (add2 5))", None),
    ("(add2_partial 10)", 15),
    ("(defmacro unless (cond body) `(if (not ,cond) ,body))", None),
    ("(define x 10)", None),
    ("(define y 20)", None),
    ("(unless (> x y) (define z 100))", None),
    ("z", 100),
    ("(+ x y)", 30),
    ("(defmacro inc (x) `(+ ,x 1))", None),
    ("(defmacro dec (x) `(+ ,x -1))", None),
    ("(inc x)", 11),
    ("(dec y)", 19),
    ("(* x y)", 200),
    ("(if (> x y) x y)", 20),
    ("(quote (a b c))", ['a', 'b', 'c']),
    ("((lambda (a b) (+ a b)) 5 7)", 12),
    ("(car (list 1 2 3))", 1),
    ("(cdr (list 1 2 3))", [2, 3]),
    ("(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1))))))", None),
    ("(fact 5)", 120),
    ("(defstruct person name age)", None),
    ("(define p (make-person \"Fred\" 30))", None),
    ("p", {"__type__": 'person', 'name': "Fred", 'age': 30}),
    ("(person-name p)", "Fred"),
    ("(person-age p)", 30),
    ( '''(progn 
          (define a 1) 
          (define b 2) 
          (+ a b)))''', 3),
    ('(define counter 0)', None),
    ('''
      (progn
        (dotimes (i 5) (set counter (+ counter 1)))
        counter)
    ''', 5)
]

def main():
    env = Environment()
    macros = MacroEnvironment()
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
