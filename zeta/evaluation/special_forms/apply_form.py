from zeta import EvaluatorFn
from zeta import SExpression, LispValue
from zeta.types.symbol import Symbol
from zeta.types.lambda_fn import Lambda
from zeta.types.errors import ZetaTypeError
from zeta.types.tail_call import TailCall


def apply_form(
    tail: list[SExpression],
    env,
    macros,
    evaluate_fn: EvaluatorFn,
    is_tail_call: bool = False,
) -> LispValue | TailCall:
    """
    Tail-call aware apply form:
      (apply fn args)
    - fn_expr: function expression
    - args_expr: evaluated list of arguments
    """
    if len(tail) != 2:
        raise ZetaTypeError(
            "Apply form expects exactly two arguments: function and argument list"
        )

    fn_expr, args_expr = tail

    # --- Evaluate the function expression ---
    fn_or_symbol = evaluate_fn(fn_expr, env, macros)

    # Resolve symbol to actual lambda or callable
    fn = env.lookup(fn_or_symbol) if isinstance(fn_or_symbol, Symbol) else fn_or_symbol

    # Evaluate arguments (must produce a list)
    args = evaluate_fn(args_expr, env, macros)
    if not isinstance(args, list):
        raise ZetaTypeError("Apply arguments must evaluate to a list")

    # --- Tail-call aware application ---
    if isinstance(fn, Lambda):
        arg_values = list(args)
        if is_tail_call:
            # Return a TailCall object instead of evaluating immediately
            new_env = fn.extend_env(arg_values, env)
            return TailCall(fn, arg_values, new_env, macros)
        else:
            # Normal evaluation
            new_env = fn.extend_env(arg_values, env)
            return evaluate_fn(fn.body, new_env, macros, is_tail_call=True)

    elif callable(fn):
        return fn(env, args)
    else:
        raise ZetaTypeError(f"Cannot apply non-function {fn}")
