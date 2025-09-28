

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
    tag_expr, body_expr = tail[0], tail[1]
    tag = evaluate_fn(tag_expr, env, macros)
    try:
        return evaluate_fn(body_expr, env, macros)
    except ThrowException as ex:
        if ex.tag == tag:
            return ex.value
        else:
            raise ex
