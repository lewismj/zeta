# Lambda‑Calculus Beta‑Reduction in Zeta Lisp (Capture‑Avoiding)

This page documents a tiny lambda‑calculus reducer.  

- See [source](../tests/test_lambda_calculus_beta_reduction.py).

We encode lambda terms as quoted S‑expressions and implement normal‑order beta‑reduction (leftmost outermost redex) with alpha‑renaming to avoid variable capture 
_(demonstrates capture‑avoiding substitution using `gensym`)._

---

## Encoding

- Variables: plain symbols `x, y, z, ...`
- Abstraction: `'(Lam x body)` encodes $\lambda x.\,body$
- Application: `'(App f a)` encodes $(f\ a)$

We will implement:
- Free‑variable analysis `free-vars`
- Alpha‑renaming `alpha`
- Capture‑avoiding substitution $t[x := v]$
- One‑step beta rule and a bounded normalizer `reduce*`

---

## Beta and Alpha (math)

- Beta‑reduction:
  $$
  (\lambda x.\,b)\ a \;\Rightarrow\; b[x := a]
  $$

- Alpha‑conversion (to avoid capture): when substituting into $\lambda v.\,b$ and the bound $v$ occurs free in the value being substituted, pick a fresh symbol $g$ and rename:
  $$
  \lambda v.\,b \;\Rightarrow\; \lambda g.\,b[v := g]\quad\text{where $g$ is fresh (via gensym)}
  $$

---

## Implementation

```lisp
;; constructors/predicates for terms
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
                  (let loop ((xs fv) (acc '()))
                    (if (null? xs) acc
                        (if (== (car xs) v)
                            (loop (cdr xs) acc)
                            (loop (cdr xs) (cons (car xs) acc)))))))
    ((is-App t) (union (free-vars (app-f t)) (free-vars (app-a t))))
    (#t '())))

;; alpha-rename: replace bound var old -> new in body
(defun alpha (t old new)
  (cond
    ((atom t) (if (and (symbol? t) (== t old)) new t))
    ((is-Lam t) (let ((v (lam-var t)) (b (lam-body t)))
                  (if (== v old)
                      (list 'Lam new (alpha b old new))
                      (list 'Lam v   (alpha b old new)))))
    ((is-App t) (list 'App (alpha (app-f t) old new) (alpha (app-a t) old new)))
    (#t t)))

;; capture-avoiding substitution [t[x := val]]
(defun subst (t x val)
  (cond
    ((atom t) (if (and (symbol? t) (== t x)) val t))
    ((is-Lam t)
      (let ((v (lam-var t)) (b (lam-body t)))
        (if (== v x)
            t
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
```

Notes:
- Normal‑order (leftmost outermost) reduction is used. This avoids reducing under lambdas prematurely.
- `gensym` provides fresh symbols for alpha‑renaming to prevent variable capture during substitution.

---

## Examples

```lisp
;; ((λx. x) a) -> a
(reduce* '(App (Lam x x) a) 10)                ;; => a

;; ((λx. λy. x y) z) -> (λy. z y) (up to alpha-equivalence)
(reduce* '(App (Lam x (Lam y (App x y))) z) 10)
;; => (Lam y (App z y))            ; binder name may differ, structure matches

;; Normal-order choice:
;; (App ((Lam x x) (Lam z z)) (Lam w w)) -> (Lam w w)
(reduce* '(App (App (Lam x x) (Lam z z)) (Lam w w)) 10)
;; => (Lam w w)
```

---
