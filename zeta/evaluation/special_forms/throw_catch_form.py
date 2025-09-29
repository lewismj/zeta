# Try-Throw handling
# Usage:
#   ````
#   (catch 'my-tag
#   (throw 'my-tag 42)) ; => 42
#
#   (catch 'my-tag
#   (/ 1 0))
#   ; => {'exception': 'ZeroDivisionError', 'message': 'Division by zero'}

from zeta.types.symbol import Symbol

class ThrowException(Exception):
    """Custom exception for Lisp-style throw/catch non-local exit."""
    def __init__(self, tag, value):
        super().__init__(f"ThrowException(tag={tag}, value={value})")
        self.tag = tag
        self.value = value


def throw_form(tail, env, macros, evaluate_fn):
    tag, val_expr = tail[0], tail[1]
    val = evaluate_fn(val_expr, env, macros)
    raise ThrowException(tag, val)


def catch_form(tail, env, macros, evaluate_fn):
    """
    Usage in Lisp:
      (catch 'my-tag
         body
         fallback) ; optional fallback if an exception occurs

      (catch 'any body fallback) ; catches all Python exceptions
    """
    tag_expr = tail[0]
    body_expr = tail[1]
    fallback_expr = tail[2] if len(tail) > 2 else None

    tag = evaluate_fn(tag_expr, env, macros)

    try:
        return evaluate_fn(body_expr, env, macros)
    except ThrowException as ex:
        if ex.tag == tag or tag == Symbol("any"):
            return ex.value
        else:
            raise ex
    except Exception as py_ex:
        # If a fallback is provided, evaluate and return it
        if fallback_expr is not None:
            return evaluate_fn(fallback_expr, env, macros)
        # Otherwise return structured error info
        return {
            "tag": "system-error",
            "exception": type(py_ex).__name__,
            "message": str(py_ex)
        }