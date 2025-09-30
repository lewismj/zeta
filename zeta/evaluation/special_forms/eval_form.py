from zeta.types.errors import ZetaArityError


def eval_form(tail, env, macros, evaluate_fn, is_tail_call=False):
    if len(tail) != 1:
        raise ZetaArityError("eval expects exactly one argument")
    expr_to_eval = tail[0]
    return evaluate_fn(expr_to_eval, env, macros, is_tail_call)