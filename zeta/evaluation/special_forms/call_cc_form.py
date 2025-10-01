"""Special form: call/cc (call-with-current-continuation).

Implements Scheme-style escape continuations for Zeta. The continuation
captured by call/cc is single-shot and delimited to the dynamic extent of
call/cc. Invoking the continuation performs a non-local exit that returns
its value as the value of the call/cc expression.

Usage examples:
  (call/cc (lambda (k) (k 42) 99))  ; => 42
  (+ 1 (call/cc (lambda (k) (k 10)))) ; => 11

Notes:
- The provided continuation `k` is represented as a Python callable that,
  when applied, raises an internal ContinuationEscape which is caught by
  the surrounding call/cc handler.
- Invoking `k` outside the dynamic extent of the original call/cc is not
  supported and will escape to the top-level as an error (single-shot).
"""

from __future__ import annotations
from typing import Any

from zeta import EvaluatorFn, LispValue
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.errors import ZetaArityError
from zeta.types.nil import Nil
from zeta.evaluation.apply import apply as apply_engine


class ContinuationEscape(Exception):
    """Internal exception used to implement escape continuations."""

    def __init__(self, value: Any):
        super().__init__("Continuation escape")
        self.value = value


def call_cc_form(
    tail: list[Any],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    is_tail_call: bool = False,
) -> LispValue:
    """(call/cc f)

    Evaluates f and applies it to a single-argument continuation function `k`.
    If `k` is invoked as (k v), then `call/cc` immediately returns v.
    """
    if len(tail) != 1:
        raise ZetaArityError("call/cc expects exactly 1 argument: a function")

    fn_expr = tail[0]
    fn_val = evaluate_fn(fn_expr, env, macros, False)

    # Build the escape continuation as a Python callable understood by apply_engine
    def k_callable(_: Environment, args: list[LispValue]) -> LispValue:
        # Accept zero or one arg; default to Nil on zero args for convenience
        value = args[0] if len(args) >= 1 else Nil
        raise ContinuationEscape(value)

    try:
        # Apply f to the continuation. Delegate to central apply to preserve
        # lambda/callable semantics and tail-call behavior. If a TailCall is
        # produced, step the trampoline here so we can still intercept the
        # escape exception within the dynamic extent of call/cc.
        result = apply_engine(fn_val, [k_callable], env, macros, evaluate_fn, is_tail_call)
        from zeta.types.tail_call import TailCall  # local import to avoid cycles
        while isinstance(result, TailCall):
            result = evaluate_fn(result.fn.body, result.env, result.macros, True)
        return result
    except ContinuationEscape as esc:
        return esc.value
