"""Application engine for Zeta.

This module centralizes function application semantics for the interpreter:
- Tail-call awareness via TailCall objects (consumed by the trampoline).
- Partial application for simple positional lambdas (no &rest/&key).
- Full application for lambdas that use &rest or &key, delegated to
  Lambda.extend_env for consistent binding semantics.
- Application of Python callables registered in the environment.

Keeping this logic in one place prevents duplication between the evaluator,
special forms, and builtin helpers.
"""

from zeta import LispValue, EvaluatorFn
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.lambda_fn import Lambda
from zeta.types.errors import ZetaArityError, ZetaTypeError
from zeta.types.tail_call import TailCall
from typing import Callable


def apply_lambda(
    fn: Lambda,
    args: list[LispValue],
    macros,
    evaluate_fn: EvaluatorFn,
    is_tail_call: bool,
    caller_env: Environment | None = None,
) -> LispValue | TailCall:
    """Apply a Lisp Lambda value.

    Parameters:
    - fn: The Lambda being applied.
    - args: The already-evaluated argument values.
    - macros: The macro environment (required if tail call occurs).
    - evaluate_fn: Evaluator function to step the trampoline if needed.
    - is_tail_call: Whether the call position is tail; if True, return a TailCall.
    - caller_env: The environment from which the call originates, used for
      delegation to extend_env so closures see caller scope where appropriate.

    Behavior:
    - If formals contain &rest or &key, full application is required and binding
      is delegated to Lambda.extend_env. Tail position returns TailCall.
    - Otherwise, support partial application by returning a new Lambda that
      closes over provided positional arguments until arity is satisfied.
    - Too many arguments will raise a ZetaArityError.
    """
    formals = list(fn.formals)

    # If lambda uses &rest, &key, or &optional, require full application via extend_env.
    if Symbol("&rest") in formals or Symbol("&key") in formals or Symbol("&optional") in formals:
        new_env = fn.extend_env(list(args), caller_env, evaluate_fn=evaluate_fn, macros=macros)
        if is_tail_call:
            return TailCall(fn, args, new_env, macros)
        else:
            # Not tail position: step evaluation immediately
            return evaluate_fn(fn.body, new_env, macros, True)

    # Simple positional parameters: support partial application.
    provided = len(args)
    arity = len(formals)

    if provided < arity:
        # Create a closure environment that binds the provided arguments.
        # The returned Lambda expects the remaining formals.
        new_env = Environment(outer=fn.env)
        for i in range(provided):
            new_env.define(formals[i], args[i])
        remaining_formals = formals[provided:]
        return Lambda(remaining_formals, fn.body, new_env)

    if provided == arity:
        new_env = fn.extend_env(list(args), caller_env)
        if is_tail_call:
            return TailCall(fn, args, new_env, macros)
        else:
            # Not tail position: step evaluation immediately.
            return evaluate_fn(fn.body, new_env, macros, True)

    # Too many arguments for a simple positional lambda.
    extra = list(args[arity:])
    raise ZetaArityError(f"Too many arguments: {extra}")


def apply(
    head: Lambda | Callable[[Environment, list[LispValue]], LispValue] | object,
    args: list[LispValue],
    env: Environment,
    macros,
    evaluate_fn: EvaluatorFn,
    tail: bool = False,
) -> LispValue | TailCall:
    """Apply either a Lambda or a Python callable.

    - For Lambda, defer to apply_lambda (handling partials, &key/&rest, and tail calls).
    - For Python callables (builtins), invoke with the runtime env and list of args.
    - Otherwise, raise a type error.
    """
    if isinstance(head, Lambda):
        return apply_lambda(head, args, macros, evaluate_fn, tail, env)
    elif callable(head):
        return head(env, args)
    else:
        raise ZetaTypeError(f"Cannot apply non-function {head}")
