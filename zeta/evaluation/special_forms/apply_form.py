from zeta import EvaluatorFn
from zeta import SExpression, LispValue
from zeta.types.errors import ZetaTypeError, ZetaArityError
from zeta.types.tail_call import TailCall
from zeta.types.symbol import Symbol
from zeta.types.lambda_fn import Lambda
from zeta.evaluation.apply import apply as apply_engine


def apply_form(
    tail: list[SExpression],
    env,
    macros,
    evaluate_fn: EvaluatorFn,
    is_tail_call: bool = False,
) -> LispValue | TailCall:
    """
    Tail-call aware apply special form:
      (apply fn args)
    Delegates actual application to the central application engine to keep
    semantics consistent (tail calls, partials, &key/&rest).

    Difference from general application: apply requires full application.
    It will raise an arity error if too few arguments are supplied to a
    simple positional lambda (no &rest/&key), rather than returning a
    partially applied lambda.
    """
    if len(tail) != 2:
        raise ZetaTypeError(
            "Apply form expects exactly two arguments: function and argument list"
        )

    fn_expr, args_expr = tail

    # Evaluate function and argument list
    fn_val = evaluate_fn(fn_expr, env, macros)
    args_val = evaluate_fn(args_expr, env, macros)

    # Validate arguments evaluate to a list
    if not isinstance(args_val, list):
        raise ZetaTypeError("Apply arguments must evaluate to a list")

    # Enforce full application for simple positional lambdas
    if isinstance(fn_val, Lambda):
        formals = list(fn_val.formals)
        if Symbol("&rest") not in formals and Symbol("&key") not in formals:
            provided = len(args_val)
            arity = len(formals)
            if provided < arity:
                missing_syms = [str(s) for s in formals[provided:]]
                raise ZetaArityError(
                    f"Too few arguments; missing {arity - provided} parameter(s): {missing_syms}"
                )

    # Delegate to the central engine (it handles Lambda/callable, tail calls, etc.)
    return apply_engine(fn_val, list(args_val), env, macros, evaluate_fn, is_tail_call)
