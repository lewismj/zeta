from zeta.parser import lex, TokenStream
from zeta.eval import evaluate
from zeta.types.nil import Nil
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.environment import Environment
from zeta.builtin.env_builtin import register
from zeta.builtin.macro_builtin import register as register_macros


class Interpreter:
    """
    A streaming interpreter for Zeta Lisp expressions.
    Allows feeding code incrementally, maintains env/macros.
    """
    def __init__(self, prelude: str | None = None):
        self.env = Environment()
        register(self.env)

        self.macros = MacroEnvironment()
        register_macros(self.macros)

        self.buffer: list = []  # buffer of tokens for partial input
        self.token_iter = iter([])

        if prelude:
            self.eval_prelude(prelude)

    def eval_prelude(self, code: str):
        """Evaluate a string of Lisp code as prelude."""
        tokens = lex(code)
        stream = TokenStream(iter(tokens))
        while True:
            expr = stream.parse_expr()
            if expr is None:
                break
            evaluate(expr, self.env, self.macros)

    def eval(self, code: str):
        """Feed code to the interpreter and evaluate expressions incrementally."""
        tokens = lex(code)
        self.token_iter = iter(tokens)  # Reset token iterator
        stream = TokenStream(self.token_iter)

        results = []
        while True:
            try:
                expr = stream.parse_expr()
            except StopIteration:
                break
            if expr is None:
                break
            result = evaluate(expr, self.env, self.macros)
            results.append(result)
        if not results:
            return Nil
        if len(results) == 1:
            return results[0]
        return results

#  Example use-age:
if __name__ == "__main__":
    prelude = """
        ;; -----------------------
        ;; Boolean constants
        ;; -----------------------
        
        ;; -----------------------
        ;; Church-style booleans
        ;; -----------------------
        (define true  (lambda (x y) x))
        (define false (lambda (x y) y))
        
        ;; -----------------------
        ;; Boolean operations
        ;; -----------------------
        (defun not (b)
            (if (= b #t) #f #t))
        
        (defun and (p q)
            (if (= p #t) q #f))
        
        (defun or (p q)
            (if (= p #t) #t q))
        
        (defun toBoolean (b)
            ;; Converts any value to #t/#f
            (if (= b #t) #t #f))
        
        ;; -----------------------
        ;; Church-style helpers
        ;; -----------------------
        (defun andC (p q)
            ((p q) p))    ;; works if p/q are Church booleans
        
        (defun orC (p q)
            ((p p) q))    ;; works if p/q are Church booleans


        (defun empty (xs) (eq xs Nil))
    """
    interp = Interpreter(prelude=prelude)

    # Test expressions
    tests = [
        "(toBoolean (or #t #f))               ;; -> #t",
        "(toBoolean (and #t #f))              ;; -> #f",
        '((true "yes" "no"))                  ;; -> "yes"',
        '((false "yes" "no"))                 ;; -> "no"',
        '((eval (andC true false)) "yes" "no")  ;; -> no (Church boolean)',
        '((eval (orC true false))  "yes" "no")  ;; -> yes (Church boolean)',
        '(define xs (1 2 3 4)) ;; -> Nil',
        '(empty xs) ;; -> #f'
    ]

    for code in tests:
        result = interp.eval(code)
        print(code, "=>", result)