;; test.lisp - simple testing macros for Zeta
;; Provides (test name expected actual) and (assert= expected actual)

;; A low-level assertion that throws on failure and returns #t on success
(defmacro assert= (expected actual)
  `(let ((exp ,expected)
         (got ,actual))
     (if (== exp got)
         #t
         (throw 'assertion-error (list "assertion failed" exp got)))))

;; A nicer test form that prints pass/fail lines
(defmacro test (name expected actual)
  `(let ((n ,name)
         (exp ,expected)
         (got ,actual))
     (if (== exp got)
         (progn
           (print (format "[PASS] {0}" n))
           #t)
         (progn
           (print (format "[FAIL] {0}: expected {1} got {2}" n exp got))
           (throw 'test-fail n)))))
