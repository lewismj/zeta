# Boolean Algebra Rewriting in Zeta Lisp

This page documents a small, self‑contained Boolean algebra simplifier implemented entirely in Zeta Lisp. 

The simplifier operates over quoted S‑expressions using heads `and`, `or`, and `not`. The goal is to apply algebraic identities until a fixed point is reached, producing a normalized form.

- See [source](../tests/test_boolean_rewrite.py).

---

## Core idea

We represent Boolean formulas as quoted lists and apply rewrite rules by inspecting the list head. For example:

- S‑expression: `'(and x (or (not y) #t))`

The following algebraic rules are applied recursively until no further change occurs:

- Identity and annihilators:
  - $x \land \top \Rightarrow x$, $x \land \bot \Rightarrow \bot$
  - $x \lor \bot \Rightarrow x$, $x \lor \top \Rightarrow \top$
- Idempotence:
  - $x \land x \Rightarrow x$, $x \lor x \Rightarrow x$
- Double negation:
  - $\lnot(\lnot a) \Rightarrow a$
- De Morgan’s laws (binary case in this demo):
  - $\lnot(a \land b) \Rightarrow (\lnot a) \lor (\lnot b)$
  - $\lnot(a \lor b) \Rightarrow (\lnot a) \land (\lnot b)$

---

## Implementation

```lisp
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
```

Notes:
- This demo handles binary `and`/`or` for simplicity. Extending to n‑ary is straightforward by folding over argument lists.
- Truthiness follows Zeta: only `nil` and `#f` are false; everything else is true.

---

## Examples

```lisp
(simplify-bool* '(and x #t))                 ;; => x
(simplify-bool* '(and x #f))                 ;; => #f
(simplify-bool* '(or (and #t x) #f))         ;; => x
(simplify-bool* '(not (not y)))              ;; => y
(simplify-bool* '(not (and a b)))            ;; => (or (not a) (not b))
```

Mathematically:
- $x \land \top = x$
- $x \land \bot = \bot$
- $\lnot(\lnot y) = y$
- $\lnot(a \land b) = (\lnot a) \lor (\lnot b)$

---

## Testing

See `tests/test_boolean_rewrite.py` for the exact usage under the interpreter’s test harness. The implementation here is copy‑paste compatible with a fresh `Interpreter()` session via `eval_prelude`.