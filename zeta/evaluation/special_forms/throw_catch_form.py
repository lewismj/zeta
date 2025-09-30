# Try-Throw handling
# Usage:
#   ````
#   (catch 'my-tag
#   (throw 'my-tag 42)) ; => 42
#
#   (catch 'my-tag
#   (/ 1 0))
#   ; => {'exception': 'ZeroDivisionError', 'message': 'Division by zero'}


class ThrowException(Exception):
    """Custom exception for Lisp-style throw/catch non-local exit."""
    def __init__(self, tag, value):
        super().__init__(f"ThrowException(tag={tag}, value={value})")
        self.tag = tag
        self.value = value


def throw_form(tail, env, macros, evaluate_fn, is_tail_call=False):
    tag_expr, val_expr = tail[0], tail[1]
    tag = evaluate_fn(tag_expr, env, macros, False)
    val = evaluate_fn(val_expr, env, macros, is_tail_call)
    raise ThrowException(tag, val)



def catch_form(tail, env, macros, evaluate_fn, is_tail_call=False):
    """
    Lisp-style catch with fallback and Python exception embedding.

    (catch 'tag body [fallback])
    - Catches (throw 'tag value)
    - 'any' tag catches all Python exceptions
    - fallback is evaluated if throw tag doesn't match or if Python exception occurs
    """
    tag_expr = tail[0]
    body_expr = tail[1]
    fallback_expr = tail[2] if len(tail) > 2 else None

    tag = evaluate_fn(tag_expr, env, macros, False)

    try:
        return evaluate_fn(body_expr, env, macros, is_tail_call)

    except ThrowException as ex:
        # Matches tag or 'any'
        if ex.tag == tag or tag == 'any':
            return ex.value
        # Fallback for non-matching throw
        if fallback_expr is not None:
            return evaluate_fn(fallback_expr, env, macros, is_tail_call)
        # Re-raise if no fallback
        raise ex

    except Exception as py_ex:
        # Fallback if provided
        if fallback_expr is not None:
            return evaluate_fn(fallback_expr, env, macros, is_tail_call)
        # Otherwise, structured system-error as plain dict
        return {
            "tag": "system-error",
            "exception": type(py_ex).__name__,
            "message": str(py_ex)
        }
