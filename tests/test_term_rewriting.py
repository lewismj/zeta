# Demonstration of term rewriting using the existing interpreter
# We implement a tiny symbolic differentiator and simplifier purely in Zeta Lisp
# to showcase rewriting rules over quoted S-expressions.

from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol


def test_symbolic_differentiation_and_simplification():
    itp = Interpreter()

    # Define rewriting helpers, a differentiator, and a simplifier
    itp.eval_prelude(r'''
      ;; Basic predicates and accessors over expressions
      (defun op (e) (car e))
      (defun a1 (e) (car (cdr e)))
      (defun a2 (e) (car (cdr (cdr e))))

      (defun number? (x)
        (and (atom x) (not (symbol? x))))

      (defun zero? (x) (== x 0))
      (defun one? (x)  (== x 1))

      ;; Simplifier: applies algebraic identities and constant folding for + and * (binary)
      (defun simplify (e)
        (cond
          ;; atoms (numbers, symbols) simplify to themselves
          ((atom e) e)

          ;; (+ a b) cases
          ((== (op e) '+)
            (let ((sa (simplify (a1 e)))
                  (sb (simplify (a2 e))))
              (cond
                ((zero? sa) sb)
                ((zero? sb) sa)
                ((and (number? sa) (number? sb)) (+ sa sb))
                ;; Combine identical terms: (+ x x) => (* 2 x)
                ((== sa sb) (list '* 2 sa))
                (#t (list '+ sa sb)))))

          ;; (* a b) cases
          ((== (op e) '* )
            (let ((sa (simplify (a1 e)))
                  (sb (simplify (a2 e))))
              (cond
                ((or (zero? sa) (zero? sb)) 0)
                ((one? sa) sb)
                ((one? sb) sa)
                ((and (number? sa) (number? sb)) (* sa sb))
                (#t (list '* sa sb)))))

          ;; default: return as-is (no rules for other heads in this demo)
          (#t e)))

      ;; Fixed-point simplifier: keep simplifying until no change
      (defun simplify* (e)
        (let ((s (simplify e)))
          (if (== s e) s (simplify* s))))

      ;; Symbolic derivative d/dv of expression e
      ;; Rules:
      ;;  d(c)/dv = 0 for numeric constants
      ;;  d(v)/dv = 1; d(u)/dv = 0 for other symbols
      ;;  d(a+b)/dv = d(a)/dv + d(b)/dv
      ;;  d(a*b)/dv = d(a)/dv*b + a*d(b)/dv
      ;;  Chain rule for common unary functions: sin, cos, exp, log
      ;;  Power rule for (^ u n) with numeric n: n * (^ u (n-1)) * du/dv
      (defun diff (e v)
        (cond
          ((number? e) 0)
          ((symbol? e) (if (== e v) 1 0))
          ;; sum rule
          ((== (op e) '+) (list '+ (diff (a1 e) v) (diff (a2 e) v)))
          ;; product rule
          ((== (op e) '* ) (list '+ (list '* (diff (a1 e) v) (a2 e))
                                     (list '* (a1 e) (diff (a2 e) v))))
          ;; power rule: (^ u n)
          ((== (op e) '^) (let ((u (a1 e))
                                  (n (a2 e)))
                             (if (number? n)
                                 (list '* n (list '* (list '^ u (- n 1)) (diff u v)))
                                 0)))
          ;; chain rule: sin(u)
          ((== (op e) 'sin) (let ((u (a1 e))) (list '* (list 'cos u) (diff u v))))
          ;; chain rule: cos(u) => -sin(u) * u'
          ((== (op e) 'cos) (let ((u (a1 e))) (list '* -1 (list '* (list 'sin u) (diff u v)))))
          ;; chain rule: exp(u) => exp(u) * u'
          ((== (op e) 'exp) (let ((u (a1 e))) (list '* (list 'exp u) (diff u v))))
          ;; chain rule: log(u) => (/ 1 u) * u'
          ((== (op e) 'log) (let ((u (a1 e))) (list '* (list '/ 1 u) (diff u v))))
          (#t 0)))
    ''')

    # Example 1: d/dx of (+ (* x x) 3) simplifies to (* 2 x)
    raw = itp.eval("""
      (diff '(+ (* x x) 3) 'x)
    """)
    # Raw derivative before simplification should be: (+ (+ (* 1 x) (* x 1)) 0)
    assert raw == [
        Symbol('+'),
        [Symbol('+'), [Symbol('*'), 1, Symbol('x')], [Symbol('*'), Symbol('x'), 1]],
        0,
    ]

    simplified = itp.eval("""
      (simplify* (diff '(+ (* x x) 3) 'x))
    """)
    assert simplified == [Symbol('*'), 2, Symbol('x')]

    # Example 2: d/dx of (* x (+ x 3)) — we don't implement associativity/flattening,
    # so we only check that the result simplifies the obvious identities.
    simplified2 = itp.eval("""
      (simplify* (diff '(* x (+ x 3)) 'x))
    """)
    # Accept either (+ (+ x 3) x) or an equivalent with one layer simplified to (* x 1) => x
    # Our rules reduce to (+ (+ x 3) x)
    assert simplified2 == [Symbol('+'), [Symbol('+'), Symbol('x'), 3], Symbol('x')]


    # Example 3: Chain rule with sin: d/dx of (sin (* x x))
    raw_chain = itp.eval("""
      (diff '(sin (* x x)) 'x)
    """)
    assert raw_chain == [
        Symbol('*'),
        [Symbol('cos'), [Symbol('*'), Symbol('x'), Symbol('x')]],
        [Symbol('+'), [Symbol('*'), 1, Symbol('x')], [Symbol('*'), Symbol('x'), 1]],
    ]

    simplified_chain = itp.eval("""
      (simplify* (diff '(sin (* x x)) 'x))
    """)
    # Expected: (* (cos (* x x)) (* 2 x)) given binary-only * simplifier
    assert simplified_chain == [
        Symbol('*'),
        [Symbol('cos'), [Symbol('*'), Symbol('x'), Symbol('x')]],
        [Symbol('*'), 2, Symbol('x')],
    ]

    # Example 4: Power rule: d/dx of (^ (+ x 1) 3) = 3*(^ (+ x 1) 2)*1
    raw_pow = itp.eval("""
      (diff '(^ (+ x 1) 3) 'x)
    """)
    assert raw_pow == [
        Symbol('*'),
        3,
        [Symbol('*'), [Symbol('^'), [Symbol('+'), Symbol('x'), 1], 2], [Symbol('+'), 1, 0]],
    ]

    simplified_pow = itp.eval("""
      (simplify* (diff '(^ (+ x 1) 3) 'x))
    """)
    assert simplified_pow == [
        Symbol('*'),
        3,
        [Symbol('^'), [Symbol('+'), Symbol('x'), 1], 2],
    ]
