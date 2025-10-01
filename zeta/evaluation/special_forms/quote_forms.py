from zeta import SExpression, LispValue, EvaluatorFn
from zeta.types.errors import ZetaArityError, ZetaTypeError, ZetaError


def eval_quasiquote(
    evaluate_fn: EvaluatorFn,
    is_tail_call: bool,
    expr: SExpression,
    env,
    macros,
    depth: int = 1,
) -> SExpression:
    from zeta.types.symbol import Symbol

    def _process_list_part(seq):
        result_list = []
        for item in seq or []:
            if isinstance(item, list) and item:
                head, *itail = item
                if head == Symbol("quasiquote"):
                    result_list.append(
                        [
                            Symbol("quasiquote"),
                            eval_quasiquote(
                                evaluate_fn,
                                is_tail_call,
                                itail[0],
                                env,
                                macros,
                                depth + 1,
                            ),
                        ]
                    )
                    continue
                if head == Symbol("unquote") and depth == 1:
                    result_list.append(evaluate_fn(itail[0], env, macros, is_tail_call))
                    continue
                if head == Symbol("unquote-splicing") and depth == 1:
                    # Gracefully handle malformed ',@' without an argument by splicing nothing
                    if not itail:
                        continue
                    spliced_val = evaluate_fn(itail[0], env, macros, is_tail_call)
                    if not isinstance(spliced_val, list):
                        raise ZetaTypeError("Unquote-splicing must produce a list")
                    result_list.extend(spliced_val)
                    continue
            result_list.append(
                eval_quasiquote(evaluate_fn, is_tail_call, item, env, macros, depth)
            )
        return result_list

    # Dotted list (tuple) support: (list_part, tail)
    if isinstance(expr, tuple) and len(expr) == 2:
        lst, tail = expr
        # Process list part with splicing support
        new_list: list = _process_list_part(lst)

        # Process tail: evaluate unquote at depth==1; do not splice in tail position
        if isinstance(tail, list) and tail:
            thead, *ttail = tail
            if thead == Symbol("quasiquote"):
                new_tail = [
                    Symbol("quasiquote"),
                    eval_quasiquote(
                        evaluate_fn, is_tail_call, ttail[0], env, macros, depth + 1
                    ),
                ]
            elif thead == Symbol("unquote") and depth == 1:
                new_tail = evaluate_fn(ttail[0], env, macros, is_tail_call)
            else:
                new_tail = eval_quasiquote(
                    evaluate_fn, is_tail_call, tail, env, macros, depth
                )
        else:
            new_tail = eval_quasiquote(
                evaluate_fn, is_tail_call, tail, env, macros, depth
            )

        return new_list, new_tail

    # Non-list atoms returned as-is
    if not isinstance(expr, list):
        return expr
    if not expr:
        return []

    result = _process_list_part(expr)
    return result


def quote_form(
    tail: list[SExpression], env, macros, evaluate_fn: EvaluatorFn, _: bool
) -> LispValue:
    if len(tail) != 1:
        raise ZetaArityError("Quote expects exactly 1 argument")
    return tail[0]


def quasiquote_form(
    tail: list[SExpression],
    env,
    macros,
    evaluate_fn: EvaluatorFn,
    is_tail_call: bool = False,
) -> LispValue:
    if len(tail) != 1:
        raise ZetaArityError("Quasiquote expects exactly 1 argument")
    # Quasiquote returns the constructed data structure; do not evaluate it here.
    # Embedded unquotes/unquote-splicing are already processed by eval_quasiquote.
    return eval_quasiquote(evaluate_fn, is_tail_call, tail[0], env, macros)


def unquote_form(
    tail: list[SExpression], env, macros, evaluate_fn: EvaluatorFn, _: bool
) -> LispValue:
    raise ZetaError(f"Unquote not valid outside of quasiquote")


def unquote_splice_form(
    tail: list[SExpression], env, macros, evaluate_fn: EvaluatorFn, _: bool
) -> LispValue:
    raise ZetaError(f"Unquote-splicing not valid outside of quasiquote")
