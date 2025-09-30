
def progn_form(tail, env, macros, evaluate_fn, is_tail_call=False):
    result = None
    for e in tail[:-1]:
        evaluate_fn(e, env, macros)
    if tail:
        result = evaluate_fn(tail[-1], env, macros, is_tail_call=is_tail_call)
    return result