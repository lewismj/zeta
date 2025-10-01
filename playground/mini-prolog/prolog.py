## This will currently fail, print, format and other functions are not implemented yet.


from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil
from zeta.types.errors import ZetaArityError, ZetaTypeError


def _mini_display(env, args):
    for a in args:
        try:
            print(a, end="")
        except Exception:
            print(str(a), end="")
    return Symbol("#t")


def _mini_string_ref(env, args):
    if len(args) != 2:
        raise ZetaArityError("string-ref expects exactly 2 arguments")
    s, i = args
    if not isinstance(s, str):
        raise ZetaTypeError("First argument to string-ref must be a string")
    if not isinstance(i, int):
        raise ZetaTypeError("Second argument to string-ref must be an integer index")
    try:
        return s[i]
    except Exception as e:
        raise ZetaTypeError(f"string-ref index out of range: {i}") from e


def _mini_char_eq(env, args):
    if not args:
        return Symbol("#t")
    norm = []
    for a in args:
        if isinstance(a, str) and len(a) == 1:
            norm.append(a)
        elif isinstance(a, str):
            norm.append(a)
        else:
            return Symbol("#f")
    first = norm[0]
    for x in norm[1:]:
        if x != first:
            return Symbol("#f")
    return Symbol("#t")


def _mini_consp(env, args):
    if len(args) != 1:
        raise ZetaArityError("consp expects exactly 1 argument")
    x = args[0]
    if x is Nil:
        return Symbol("#f")
    if isinstance(x, list) and len(x) > 0:
        return Symbol("#t")
    return Symbol("#f")


def register_miniprolog(env):
    env.update({
        Symbol('display'): _mini_display,
        Symbol('string-ref'): _mini_string_ref,
        Symbol('char=?'): _mini_char_eq,
        Symbol('consp'): _mini_consp,
    })

prolog = '''
;;;; ---------------------------
;;;; Minimal Lisp Core Requirements
;;;; ---------------------------
;; Core primitives assumed to exist in Zeta Lisp:
;; defun, cons, car, cdr, null?, eq?, cond, recursion

;;;; ---------------------------
;;;; Mini-Prolog specific helpers (Lisp level)
;;;; ---------------------------

;; append (two-arg) using join
(defun append (a b)
  (join a b))

;; filter implemented in Lisp using join/list
(defun filter (f xs)
  (if (null? xs)
      '()
      (join (if (f (car xs)) (list (car xs)) '())
            (filter f (cdr xs)))))

;;;; ---------------------------
;;;; Prelude: helper functions
;;;; ---------------------------

;; assoc: lookup key in alist
(defun assoc (key alist)
  (cond
    ((null? alist) #f)
    ((eq? (car (car alist)) key) (car alist))
    (else (assoc key (cdr alist)))))

;; map helper for prelude use
(defun map (f lst)
  (cond
    ((null? lst) '())
    (else (cons (f (car lst)) (map f (cdr lst))))))

;; for-each helper
(defun for-each (f lst)
  (if (null? lst)
      '()
      (begin
        (f (car lst))
        (for-each f (cdr lst)))))

;; subst-apply: apply substitution to a term
(defun variable? (x)
  (and (symbol? x)
       (char=? (string-ref (symbol->string x) 0) #\?)))

(defun subst-apply (term subst)
  (cond
    ((variable? term)
     (let ((val (assoc term subst)))
       (if val
           (subst-apply (cdr val) subst)
           term)))
    ((consp term)
     (cons (subst-apply (car term) subst)
           (subst-apply (cdr term) subst)))
    (else term)))

;; unify two terms with a given substitution
(defun unify (x y &optional (subst '()))
  (cond
    ((assoc x subst) (unify (cdr (assoc x subst)) y subst))
    ((assoc y subst) (unify x (cdr (assoc y subst)) subst))
    ((eq? x y) subst)
    ((variable? x) (cons (cons x y) subst))
    ((variable? y) (cons (cons y x) subst))
    ((and (consp x) (consp y))
     (let ((subst1 (unify (car x) (car y) subst)))
       (and subst1 (unify (cdr x) (cdr y) subst1))))
    (else #f)))

;;;; ---------------------------
;;;; Knowledge Base
;;;; ---------------------------

(define *knowledge-base* '())

(defun assert! (fact-or-rule)
  (set! *knowledge-base* (cons fact-or-rule *knowledge-base*)))

(defun retract! (fact-or-rule)
  (set! *knowledge-base*
        (filter (lambda (x) (not (eq? x fact-or-rule)))
                *knowledge-base*)))

;;;; ---------------------------
;;;; Proving Goals
;;;; ---------------------------

(defun prove (goals &optional (subst '()))
  (if (null? goals)
      (list subst) ;; success: return current substitution
      (let ((goal (car goals))
            (rest-goals (cdr goals))
            results '())
        (for-each (lambda (rule)
                    (let ((head (if (consp rule) (car rule) rule))
                          (body (if (and (consp rule) (cdr rule)) (cdr rule) '())))
                      (let ((subst1 (unify goal head subst)))
                        (if subst1
                            (set! results (append results
                                                  (prove (append body rest-goals) subst1)))))))
                  *knowledge-base*)
        results)))

;;;; ---------------------------
;;;; Query Interface
;;;; ---------------------------

(defun query (goal)
  (let ((solutions (prove (list goal))))
    (if (null? solutions)
        (for-each (lambda (s)
                    (for-each (lambda (x)
                                (display (cons (car x) (subst-apply (cdr x) s)))
                              s))
                  solutions)))

'''

test = '''
;;;; ---------------------------
;;;; Examples / Tests
;;;; ---------------------------

;; Facts
(assert! '(parent alice bob))
(assert! '(parent bob carol))
(assert! '(parent bob david))
(assert! '(parent carol eve))

;; Rule: ancestor
(assert! '((ancestor ?X ?Y) :- ((parent ?X ?Y)))))
(assert! '((ancestor ?X ?Y) :- ((parent ?X ?Z) (ancestor ?Z ?Y)))))

;; Sample Queries
;; (query '(parent alice ?X))
;; (query '(ancestor alice ?X))

'''





def main():
    # Initialize interpreter without prelude so we can register mini-prolog builtins first
    interp = Interpreter(prelude=None)

    # Register mini-prolog specific builtins needed by the prelude
    register_miniprolog(interp.env)

    # Now evaluate the Prolog prelude and tests
    interp.eval_prelude(prolog)
    interp.eval(test)

if __name__ == "__main__":
    main()