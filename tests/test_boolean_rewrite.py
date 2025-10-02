from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol


def test_boolean_rewriting():
    itp = Interpreter()
    itp.eval_prelude(r'''
      (defun op (e) (car e))
      (defun a1 (e) (car (cdr e)))
      (defun a2 (e) (car (cdr (cdr e))))

      ;; boolean simplifier on binary trees: (and a b), (or a b), (not a)
      (defun simplify-bool (e)
        (cond
          ;; atoms (variables or constants) return as-is
          ((atom e) e)

          ;; not
          ((== (op e) 'not)
            (let ((u (simplify-bool (a1 e))))
              (cond
                ((and (atom u) (== u #t)) #f)
                ((and (atom u) (== u #f)) #t)
                ;; double negation
                ((and (not (atom u)) (== (op u) 'not)) (a1 u))
                ;; De Morgan (binary)
                ((and (not (atom u)) (== (op u) 'and)) (list 'or (list 'not (a1 u)) (list 'not (a2 u))))
                ((and (not (atom u)) (== (op u) 'or))  (list 'and (list 'not (a1 u)) (list 'not (a2 u))))
                (#t (list 'not u)))))

          ;; and
          ((== (op e) 'and)
            (let ((sa (simplify-bool (a1 e)))
                  (sb (simplify-bool (a2 e))))
              (cond
                ;; annihilator and identity
                ((== sa #f) #f)
                ((== sb #f) #f)
                ((== sa #t) sb)
                ((== sb #t) sa)
                ;; idempotence
                ((== sa sb) sa)
                (#t (list 'and sa sb)))))

          ;; or
          ((== (op e) 'or)
            (let ((sa (simplify-bool (a1 e)))
                  (sb (simplify-bool (a2 e))))
              (cond
                ;; annihilator and identity
                ((== sa #t) #t)
                ((== sb #t) #t)
                ((== sa #f) sb)
                ((== sb #f) sa)
                ;; idempotence
                ((== sa sb) sa)
                (#t (list 'or sa sb)))))

          (#t e)))

      (defun simplify-bool* (e)
        (let ((s (simplify-bool e)))
          (if (== s e) s (simplify-bool* s))))
    ''')

    # (and x #t) -> x
    assert itp.eval("(simplify-bool* '(and x #t))") == Symbol('x')
    # (and x #f) -> #f
    assert itp.eval("(simplify-bool* '(and x #f))") == Symbol('#f')
    # (or (and #t x) #f) -> x
    assert itp.eval("(simplify-bool* '(or (and #t x) #f))") == Symbol('x')
    # double negation
    assert itp.eval("(simplify-bool* '(not (not y)))") == Symbol('y')
    # De Morgan
    assert itp.eval("(simplify-bool* '(not (and a b)))") == [Symbol('or'), [Symbol('not'), Symbol('a')], [Symbol('not'), Symbol('b')]]