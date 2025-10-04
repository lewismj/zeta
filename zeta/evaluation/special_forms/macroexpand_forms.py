"""Special forms that expose the macro expander to Lisp code.

macroexpand-1: expand a single step at the head position if it is a macro.
macroexpand: fully expand a form (except inside quote/quasiquote), repeatedly
             expanding head positions to a fixpoint and recursively expanding
             subforms.

Both return the expansion as an S-expression and do not evaluate it.

Note, these forms are used just for the prelude functions below, that
can act as an aid to debugging macros.


;; Pretty-print macro expansions (returns the expansion as value)
(defun macroexpand-print (form)
  (let ((exp (macroexpand form)))
    (print exp)
    exp))

(defun macroexpand-1-print (form)
  (let ((exp (macroexpand-1 form)))
    (print exp)
    exp))

"""

from zeta import SExpression, EvaluatorFn
from zeta.types.errors import ZetaArityError
from zeta.types.symbol import Symbol


def macroexpand1_form(
    tail: list[SExpression], env, macros, evaluate_fn: EvaluatorFn, is_tail_call: bool
):
    """(macroexpand-1 form): expand the head macro once and return the result.

    Tted; this mirrors Common Lisp's macroexpand-1.
    If the argument is a quoted form, he argument is not evaluaunwrap one leading (quote ...) and
    expand the inner form, returning the expansion (not re-wrapped).
    """
    if len(tail) != 1:
        raise ZetaArityError("macroexpand-1 expects exactly 1 argument")
    form = tail[0]
    # Unwrap a single leading (quote <form>) to match CL usage: (macroexpand-1 '(...))
    if isinstance(form, list) and form:
        head, *rest = form
        if head == Symbol("quote") and rest:
            form = rest[0]
    return macros.macro_expand_head(form, evaluate_fn, env)


def macroexpand_form(
    tail: list[SExpression], env, macros, evaluate_fn: EvaluatorFn, is_tail_call: bool
):
    """(macroexpand form): fully expand a form and return the expansion.

    The argument is not evaluated; this mirrors Common Lisp's macroexpand.
    If the argument is a quoted form, unwrap one leading (quote ...) and
    expand the inner form, returning the expansion (not re-wrapped).
    """
    if len(tail) != 1:
        raise ZetaArityError("macroexpand expects exactly 1 argument")
    form = tail[0]
    # Unwrap a single leading (quote <form>) to match CL usage: (macroexpand '(...))
    if isinstance(form, list) and form:
        head, *rest = form
        if head == Symbol("quote") and rest:
            form = rest[0]
    return macros.macro_expand_all(form, evaluate_fn, env)
