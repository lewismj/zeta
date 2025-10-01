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
    """
    Apply a Lambda with tail-call awareness, supporting partial application for
    simple positional lambdas. For lambdas that declare &rest or &key, delegate
    to Lambda.extend_env (full application required).
    """
    formals = list(fn.formals)

    # If lambda uses &rest or &key, require full application via extend_env
    if Symbol("&rest") in formals or Symbol("&key") in formals:
        new_env = fn.extend_env(list(args), caller_env)
        if is_tail_call:
            return TailCall(fn, args, new_env, macros)
        else:
            return evaluate_fn(fn.body, new_env, macros, True)

    # Simple positional parameters: support partial application
    provided = len(args)
    arity = len(formals)

    if provided < arity:
        # Create a closure environment that binds the provided arguments
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
            return evaluate_fn(fn.body, new_env, macros, True)

    # Too many arguments for a simple positional lambda
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
    if isinstance(head, Lambda):
        return apply_lambda(head, args, macros, evaluate_fn, tail, env)
    elif callable(head):
        return head(env, args)
    else:
        raise ZetaTypeError(f"Cannot apply non-function {head}")
