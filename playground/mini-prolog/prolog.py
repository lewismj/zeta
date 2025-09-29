## This will currently fail, print, format and other functions are not implemented yet.


from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol

prolog = '''
;;;; ---------------------------
;;;; Minimal Lisp Core Requirements
;;;; ---------------------------
;; Core primitives assumed to exist in Zeta Lisp:
;; defun, cons, car, cdr, null?, eq?, cond, recursion

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
        (display "No solutions.\n")
        (for-each (lambda (s)
                    (for-each (lambda (x)
                                (display (cons (car x) (subst-apply (cdr x) s)))
                                (display "\n"))
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

    interp = Interpreter(prelude=prolog)
    interp.eval(test)

if __name__ == "__main__":
    main()