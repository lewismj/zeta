;; Zeta standard prelude (std.lisp)
;; A small, practical standard library of functions and macros to bootstrap programs.

;; -----------------------
;; Basic list aliases
;; -----------------------
(defun head (xs) (car xs))
(defun tail (xs) (cdr xs))

;; -----------------------
;; Predicates and helpers
;; -----------------------
(defun empty? (xs) (== xs Nil))
(defun list? (x) (not (atom x)))
(defun identity (x) x)

;; -----------------------
;; List lengths and slicing
;; -----------------------
(defun len (xs)
  (if (== xs Nil)
      0
      (+ 1 (len (tail xs)))))

(defun take (n xs)
  (if (or (== n 0) (== xs Nil))
      Nil
      (cons (head xs) (take (- n 1) (tail xs)))))

(defun drop (n xs)
  (if (== n 0)
      xs
      (drop (- n 1) (tail xs))))

;; -----------------------
;; Folds / reduce
;; -----------------------
(defun foldl (f z xs)
  (if (== xs Nil)
      z
      (foldl f (f z (head xs)) (tail xs))))

(defun foldr (f z xs)
  (if (== xs Nil)
      z
      (f (head xs) (foldr f z (tail xs)))))

(defun fold (f z xs) (foldl f z xs))
(defun reduce (f z xs) (foldl f z xs))

;; -----------------------
;; Mapping, filtering, reversing
;; -----------------------
(defun map (f xs)
  (if (== xs Nil)
      Nil
      (cons (f (head xs)) (map f (tail xs)))))

(defun filter (f xs)
  (if (== xs Nil)
      Nil
      (let ((x (head xs)))
        (if (f x)
            (cons x (filter f (tail xs)))
            (filter f (tail xs))))))

(defun reverse (xs)
  (foldl (lambda (acc x) (cons x acc)) Nil xs))

;; -----------------------
;; List joins and append
;; -----------------------
;; join is provided by builtins; define append as a variadic alias
(defun append (&rest xss)
  (if (== xss Nil)
      Nil
      (apply join xss)))

;; -----------------------
;; Numeric helpers
;; -----------------------
(defun sum (xs) (foldl (lambda (a x) (+ a x)) 0 xs))
(defun product (xs) (foldl (lambda (a x) (* a x)) 1 xs))

;; Ranges:
;;  - (range n) -> (0 1 ... n-1)
;;  - (range a b) -> (a a+1 ... b-1)
(defun range1 (n)
  (if (<= n 0)
      Nil
      (cons 0 (map (lambda (k) (+ k 1)) (range1 (- n 1))))))

(defun range2 (a b)
  (if (>= a b)
      Nil
      (cons a (range2 (+ a 1) b))))

(defun range (&rest xs)
  (if (== xs Nil)
      Nil
      (let ((argc (len xs)))
        (if (== argc 1)
            (range1 (head xs))
            (range2 (head xs) (head (tail xs)))))))

;; -----------------------
;; Any / All
;; -----------------------
(defun any (p xs)
  (if (== xs Nil)
      #f
      (if (p (head xs))
          #t
          (any p (tail xs)))))

(defun all (p xs)
  (if (== xs Nil)
      #t
      (if (p (head xs))
          (all p (tail xs))
          #f)))

;; -----------------------
;; Function utilities
;; -----------------------
(defun compose (f g)
  (lambda (x) (f (g x))))

(defun curry2 (f x)
  (lambda (y) (f x y)))

;; -----------------------
;; Sorting (quicksort)
;; -----------------------
(defun sort (xs)
  (if (<= (len xs) 1)
      xs
      (let ((pivot (head xs)))
        (join
          (sort (filter (lambda (n) (< n pivot)) (tail xs)))
          (list pivot)
          (sort (filter (lambda (n) (>= n pivot)) (tail xs)))))))

;; -----------------------
;; Convenience predicates
;; -----------------------
(defun is_empty (xs) (== xs Nil))
(defun is_empty1 (xs) (== (len xs) 0))

;; -----------------------
;; Common macros
;; -----------------------
;; Requires quasiquote/unquote provided by the reader

(defmacro when (cond &rest body)
  `(if ,cond (progn ,@body) Nil))

(defmacro unless (cond &rest body)
  `(if (not ,cond) (progn ,@body) Nil))

;; defalias: (defalias new old)
(defmacro defalias (new old)
  `(define ,new ,old))

 ;; Helpers
(defun evenp (n) (= (mod n 2) 0))

