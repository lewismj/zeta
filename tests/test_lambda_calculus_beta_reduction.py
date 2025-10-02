from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol


def test_lambda_calculus_beta_reduction():
    itp = Interpreter()
    itp.eval_prelude(r'''
      ;; constructors/predicates
      (defun is-Lam (t) (and (not (atom t)) (== (car t) 'Lam)))
      (defun is-App (t) (and (not (atom t)) (== (car t) 'App)))
      (defun lam-var (t) (car (cdr t)))
      (defun lam-body (t) (car (cdr (cdr t))))
      (defun app-f (t) (car (cdr t)))
      (defun app-a (t) (car (cdr (cdr t))))

      ;; set ops on symbol lists (naive)
      (defun mem? (x xs)
        (if (null? xs) #f (if (== x (car xs)) #t (mem? x (cdr xs)))))
      (defun union (xs ys)
        (if (null? xs) ys
            (if (mem? (car xs) ys) (union (cdr xs) ys)
                (cons (car xs) (union (cdr xs) ys)))))

      ;; free variables
      (defun free-vars (t)
        (cond
          ((atom t) (if (symbol? t) (list t) Nil))
          ((is-Lam t) (let ((v (lam-var t)) (fv (free-vars (lam-body t))))
                        ;; remove v from fv
                        (let loop ((xs fv) (acc '()))
                          (if (null? xs) acc
                              (if (== (car xs) v)
                                  (loop (cdr xs) acc)
                                  (loop (cdr xs) (cons (car xs) acc)))))))
          ((is-App t) (union (free-vars (app-f t)) (free-vars (app-a t))))
          (#t '())))

      ;; alpha-renaming helper: replace bound var old -> new in body
      (defun alpha (t old new)
        (cond
          ((atom t) (if (and (symbol? t) (== t old)) new t))
          ((is-Lam t) (let ((v (lam-var t)) (b (lam-body t)))
                        (if (== v old)
                            (list 'Lam new (alpha b old new))
                            (list 'Lam v   (alpha b old new)))))
          ((is-App t) (list 'App (alpha (app-f t) old new) (alpha (app-a t) old new)))
          (#t t)))

      ;; capture-avoiding substitution: [t[x := val]]
      (defun subst (t x val)
        (cond
          ((atom t) (if (and (symbol? t) (== t x)) val t))
          ((is-Lam t)
            (let ((v (lam-var t)) (b (lam-body t)))
              (if (== v x)
                  t  ;; shadowed; stop
                  (if (mem? v (free-vars val))
                      (let ((g (gensym 'v)))
                        (list 'Lam g (subst (alpha b v g) x val)))
                      (list 'Lam v (subst b x val))))))
          ((is-App t) (list 'App (subst (app-f t) x val) (subst (app-a t) x val)))
          (#t t)))

      ;; one-step beta (normal order)
      (defun beta1 (t)
        (cond
          ((and (is-App t) (is-Lam (app-f t)))
            (let ((lam (app-f t)) (arg (app-a t)))
              (subst (lam-body lam) (lam-var lam) arg)))
          ((is-App t) (let ((f (beta1 (app-f t))))
                        (if (== f (app-f t))
                            (let ((a (beta1 (app-a t)))) (list 'App (app-f t) a))
                            (list 'App f (app-a t)))))
          ((is-Lam t) (let ((b (beta1 (lam-body t)))) (list 'Lam (lam-var t) b)))
          (#t t)))

      ;; reduce to normal form with a simple step bound
      (defun reduce* (t n)
        (if (== n 0) t
            (let ((t2 (beta1 t)))
              (if (== t2 t) t (reduce* t2 (- n 1))))))
    ''')

    # ((λx. x) a) -> a
    assert itp.eval("(reduce* '(App (Lam x x) a) 10)") == Symbol('a')

    # ((λx. λy. x y) z) -> (λy. z y) up to alpha-equivalence (binder name may vary)
    res = itp.eval("(reduce* '(App (Lam x (Lam y (App x y))) z) 10)")
    assert isinstance(res, list) and len(res) == 3 and res[0] == Symbol('Lam')
    binder = res[1]
    assert isinstance(binder, Symbol)
    assert res[2] == [Symbol('App'), Symbol('z'), binder]

    # Normal-order choice: (App ((Lam x x) (Lam z z)) (Lam w w)) -> (Lam w w)
    assert itp.eval("(reduce* '(App (App (Lam x x) (Lam z z)) (Lam w w)) 10)") == \
           [Symbol('Lam'), Symbol('w'), Symbol('w')]