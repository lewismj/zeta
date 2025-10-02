# Term Rewriting in Zeta Lisp: A Mini Symbolic Differentiator

This page demonstrates term rewriting in Zeta Lisp using only Zeta code (no interpreter changes). It mirrors and expands the examples from `tests/test_term_rewriting.py` and shows the corresponding mathematical notation using LaTeX.

The examples implement:
- A tiny simplifier for algebraic expressions over quoted S-expressions
- A symbolic differentiator `diff` supporting +, *, power with numeric exponents, and common unary functions (sin, cos, exp, log)

GitHub renders LaTeX enclosed in `$...$` for inline math and `$$...$$` for display equations.

---

## Core Idea

We represent algebra as quoted lists and apply rewrite rules by pattern-matching on list heads.

- S-expression: `'( + (* x x) 3 )` : $x^2 + 3$

Differentiation and simplification are implemented as Lisp functions that operate over these quoted forms.

---

## Zeta Lisp Setup (helpers, simplifier, differentiator)

```lisp
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
;;  Chain rule: sin, cos, exp, log
;;  Power rule: (^ u n) with numeric n
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
```

---

## Examples

### 1) Quadratic: $\frac{d}{dx}\big(x^2 + 3\big)$

- Input S-expr: `'(+ (* x x) 3)` → $x^2 + 3$

```lisp
(diff '(+ (* x x) 3) 'x)
;; => (+ (+ (* 1 x) (* x 1)) 0)
```

$$
\frac{d}{dx}(x^2 + 3) = \frac{d}{dx}(x\cdot x) + \frac{d}{dx}(3) = 1\cdot x + x\cdot 1 + 0
$$

- Simplified:

```lisp
(simplify* (diff '(+ (* x x) 3) 'x))
;; => (* 2 x)
```

$$
\frac{d}{dx}(x^2 + 3) = 2x
$$

---

### 2) Product: $\frac{d}{dx}\big(x(x+3)\big)$

- Input S-expr: `'( * x (+ x 3) )` → $x(x + 3)$

```lisp
(simplify* (diff '(* x (+ x 3)) 'x))
;; => (+ (+ x 3) x)
```

$$
\frac{d}{dx}\big(x(x+3)\big) = (x+3) + x = 2x + 3
$$

Note: the simplifier keeps `(+ (+ x 3) x)` due to binary `*` and no flattening/associativity rules; mathematically it equals $2x+3$.

---

### 3) Chain Rule: $\frac{d}{dx}\big(\sin(x^2)\big)$

- Input S-expr: `'(sin (* x x))` → $\sin(x^2)$

```lisp
(diff '(sin (* x x)) 'x)
;; => (* (cos (* x x)) (+ (* 1 x) (* x 1)))
```

- Simplified:

```lisp
(simplify* (diff '(sin (* x x)) 'x))
;; => (* (cos (* x x)) (* 2 x))
```

$$
\frac{d}{dx}\sin(x^2) = \cos(x^2)\cdot 2x
$$

---

### 4) Power Rule with Inner Sum: $\frac{d}{dx}\big((x+1)^3\big)$

- Input S-expr: `'( ^ (+ x 1) 3 )` → $(x+1)^3$

```lisp
(diff '(^ (+ x 1) 3) 'x)
;; => (* 3 (* (^ (+ x 1) 2) (+ 1 0)))
```

```lisp
(simplify* (diff '(^ (+ x 1) 3) 'x))
;; => (* 3 (^ (+ x 1) 2))
```

$$
\frac{d}{dx}(x+1)^3 = 3(x+1)^2
$$

---

## Notes and Limitations

- The demo simplifier is intentionally small:
  - `*` and `+` are treated as binary nodes; there is no general flattening or reordering.
  - Only basic identities are implemented (zero/one rules, constant folding, duplicate-term combining for `(+ x x)`).
- The differentiator covers:
  - Sums and products (binary)
  - Power rule for numeric exponents
  - Chain rule for `sin`, `cos`, `exp`, `log`
- Everything is written in Lisp and operates over quoted S-expressions — a pure term rewriting style.

## Running the Demo

You can explore these examples interactively via the `Interpreter` in Python or consult the automated examples in `tests/test_term_rewriting.py`.

```python
from zeta.interpreter import Interpreter
itp = Interpreter()
# then eval the prelude above and the sample expressions
```

