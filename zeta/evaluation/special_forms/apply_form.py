from zeta.types.symbol import Symbol
from zeta.types.lambda_fn import Lambda
from zeta.types.errors import ZetaTypeError

def apply_form(tail, env, macros, evaluate_fn):
    fn_expr = tail[0]
    args_expr = tail[1]

    # Evaluate fn_expr to see if it's a symbol or a lambda
    fn_or_symbol = evaluate_fn(fn_expr, env, macros)

    # Resolve symbol to actual lambda or callable
    fn = env.lookup(fn_or_symbol) if isinstance(fn_or_symbol, Symbol) else fn_or_symbol
    args = evaluate_fn(args_expr, env, macros)  # must evaluate to a list

    if isinstance(fn, Lambda):
        arg_values = list(args)
        return evaluate_fn([fn] + arg_values, env, macros)
    elif callable(fn):
        return fn(env, args)
    else:
        raise ZetaTypeError(f"Cannot apply non-function {fn}")
