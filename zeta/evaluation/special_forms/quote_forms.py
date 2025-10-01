from zeta.types.errors import ZetaArityError, ZetaTypeError, ZetaError


def eval_quasiquote(evaluate_fn, is_tail_call, expr, env, macros, depth=1):
    from zeta.types.symbol import Symbol

    # Non-list atoms returned as-is
    if not isinstance(expr, list):
        return expr
    if not expr:
        return []

    result = []
    for item in expr:
        if isinstance(item, list) and item:
            head, *tail = item
            if head == Symbol("quasiquote"):
                result.append([Symbol("quasiquote"), eval_quasiquote(evaluate_fn, is_tail_call, tail[0], env, macros, depth + 1)])
                continue
            if head == Symbol("unquote") and depth == 1:
                result.append(evaluate_fn(tail[0], env, macros, is_tail_call))
                continue
            if head == Symbol("unquote-splicing") and depth == 1:
                # Gracefully handle malformed ',@' without an argument by splicing nothing
                if not tail:
                    # Treat as splicing an empty list (robustness for ill-formed macro expansions)
                    continue
                spliced = evaluate_fn(tail[0], env, macros, is_tail_call)
                if not isinstance(spliced, list):
                    raise ZetaTypeError("unquote-splicing must produce a list")
                result.extend(spliced)
                continue
        result.append(eval_quasiquote(evaluate_fn, is_tail_call, item, env, macros, depth))
    return result


def quote_form(tail, env, macros, evaluate_fn, _):
    if len(tail) != 1:
        raise ZetaArityError("quote expects exactly 1 argument")
    return tail[0]

def quasiquote_form(tail, env, macros, evaluate_fn, is_tail_call=False):
    if len(tail) != 1:
        raise ZetaArityError("quasiquote expects exactly 1 argument")
    # Quasiquote returns the constructed data structure; do not evaluate it here.
    # Embedded unquotes/unquote-splicing are already processed by eval_quasiquote.
    return eval_quasiquote(evaluate_fn, is_tail_call, tail[0], env, macros)

def unquote_form(tail, env, macros, evaluate_fn, _):
    raise ZetaError(f"unquote not valid outside of quasiquote")

def unquote_splice_form(tail, env, macros, evaluate_fn, _):
    raise ZetaError(f"unquote-splicing not valid outside of quasiquote")