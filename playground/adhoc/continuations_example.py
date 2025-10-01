"""
Playground: Continuations (call/cc) examples for Zeta Lisp

This script demonstrates practical uses of call/cc (call-with-current-continuation)
with small, self-contained snippets. Each example includes inline comments that
explain what is going on, step by step. Run this file directly to see results.

How to run:
  - From the project root: `python -m playground.adhoc.continuations_example`

Note: These are educational examples meant for the playground; they do not affect
library behavior or tests.
"""

from zeta.interpreter import Interpreter


def run_examples():
    interp = Interpreter()

    examples = [
        (
            "Early exit from a computation",
            r"""
            ;; Example 1: Early exit from a computation using call/cc
            ;; ------------------------------------------------------
            ;; call/cc provides a function (continuation) that, when called, performs
            ;; a non-local exit and returns its argument as the value of the entire
            ;; call/cc expression.
            ;; Here we "abort" the computation and return 42 early.
            
            (progn
              (define result
                (call/cc (lambda (escape)
                  ;; Do some work...
                  (define a (+ 10 20))      ;; a = 30
                  (define b (* a 2))        ;; b = 60

                  ;; Decide to stop here and return 42 immediately
                  (escape 42)

                  ;; These lines never execute
                  (+ b 100)))
              result)  ;; => 42
            """,
        ),
        (
            "Using call/cc to break out of nested calls",
            r"""
            ;; Example 2: Break out of multiple nested calls
            ;; ---------------------------------------------
            ;; Without call/cc, returning out of multiple layers requires
            ;; threading flags or restructuring code. With call/cc we can
            ;; escape directly to the point around call/cc.
            
            (progn
              (define nested
                (lambda (escape)
                  (define inner1 (lambda ()
                    (define inner2 (lambda ()
                      ;; Something happens here that means we want to stop now
                      (escape "stopped from deep inside")
                      "unreached-2")
                    (inner2)
                    "unreached-1"))
                  (inner1)
                  "unreached-0"))

              (define out
                (call/cc (lambda (escape) (nested escape))))

              out)  ;; => "stopped from deep inside"
            """,
        ),
        (
            "Search the list and return the first negative number (early return)",
            r"""
            ;; Example 3: Early return during search
            ;; ------------------------------------
            ;; We'll scan a list and return the first negative number as soon as
            ;; we see it. If there is no negative number, return Nil.
            
            (progn
              (define xs (list 3 8 11 -4 7 -2))

              (define find-first-negative
                (lambda (xs)
                  (call/cc (lambda (return)
                    (define loop
                      (lambda (ys)
                        (if (== ys Nil)
                            Nil
                            (let ((x (car ys)))
                              (if (< x 0)
                                  (return x)                ;; exit immediately with x
                                  (loop (cdr ys)))))))
                    (loop xs)  ;; If not returned, list was empty -> Nil
                    Nil)))

              (find-first-negative xs))  ;; => -4
            """,
        ),
        (
            "call/cc value flows back to its call site",
            r"""
            ;; Example 4: The value you provide to the continuation becomes the
            ;; value of the call/cc expression itself, in-place.
            ;; So you can embed call/cc inside expressions.

            (+ 1 (call/cc (lambda (k) (k 10))))  ;; => 11
            """,
        ),
        (
            "Provide a default if a condition fails (escape as an alternative path)",
            r"""
            ;; Example 5: Using escape as an alternative control path
            ;; ------------------------------------------------------
            ;; Pretend we need a positive denominator. If it isn't, escape
            ;; with a default value.
            
            (progn
              (define safe-div
                (lambda (num den)
                  (call/cc (lambda (escape)
                    (if (<= den 0)
                        (escape "bad-denominator")
                        (/ num den)))))
              )

              (list
                (safe-div 10 2)     ;; normal path => 5
                (safe-div 10 0))    ;; escapes => "bad-denominator"
            )
            """,
        ),
    ]

    for title, code in examples:
        print("\n===", title, "===")
        result = interp.eval(code)
        print(result)


if __name__ == "__main__":
    run_examples()
