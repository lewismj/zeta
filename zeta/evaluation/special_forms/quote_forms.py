from zeta.types.errors import ZetaArityError, ZetaTypeError, ZetaError

class QQ:
    def __init__(self, value):
        self.value = value

class UQ:
    def __init__(self, value):
        self.value = value

class UQSplice:
    def __init__(self, value):
        self.value = value


def eval_quasiquote(evaluate_fn, expr, env, macros, depth=1):
    from zeta.types.symbol import Symbol

    if isinstance(expr, QQ):
        return [Symbol("quasiquote"), eval_quasiquote(evaluate_fn, expr.value, env, macros, depth)]
    if isinstance(expr, UQ):
        if depth == 1:
            return evaluate_fn(expr.value, env, macros)
        else:
            return UQ(eval_quasiquote(evaluate_fn, expr.value, env, macros, depth - 1))
    if isinstance(expr, UQSplice):
        if depth == 1:
            result = evaluate_fn(expr.value, env, macros)
            if not isinstance(result, list):
                raise ZetaTypeError("unquote-splicing must produce a list")
            return result
        else:
            return UQSplice(eval_quasiquote(evaluate_fn, expr.value, env, macros, depth - 1))

    if not isinstance(expr, list):
        return expr
    if not expr:
        return []

    result = []
    for item in expr:
        if isinstance(item, list) and item:
            head, *tail = item
            if head == Symbol("quasiquote"):
                result.append([Symbol("quasiquote"), eval_quasiquote(evaluate_fn, tail[0], env, macros, depth + 1)])
                continue
            if head == Symbol("unquote") and depth == 1:
                result.append(evaluate_fn(tail[0], env, macros))
                continue
            if head == Symbol("unquote-splicing") and depth == 1:
                spliced = evaluate_fn(tail[0], env, macros)
                if not isinstance(spliced, list):
                    raise ZetaTypeError("unquote-splicing must produce a list")
                result.extend(spliced)
                continue
        result.append(eval_quasiquote(evaluate_fn, item, env, macros, depth))
    return result


def quote_form(tail, env, macros, evaluate_fn):
    if len(tail) != 1:
        raise ZetaArityError("quote expects exactly 1 argument")
    return tail[0]

def quasiquote_form(tail, env, macros, evaluate_fn):
    if len(tail) != 1:
        raise ZetaArityError("quasiquote expects exactly 1 argument")
    return evaluate_fn(eval_quasiquote(evaluate_fn, tail[0], env, macros), env, macros)

def unquote_form(tail, env, macros, evaluate_fn):
    raise ZetaError(f"unquote not valid outside of quasiquote")

def unquote_splice_form(tail, env, macros, evaluate_fn):
    raise ZetaError(f"unquote-splicing not valid outside of quasiquote")