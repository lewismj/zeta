
def progn_form(tail, env, macros, evaluate_fn):
    result = None
    for e in tail:
        result = evaluate_fn(e, env, macros)
    return result