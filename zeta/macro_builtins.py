from zeta.errors import ZetaArityError
from zeta.types import MacroEnvironment, Symbol


def let_macro(args, env):
    """
    (let ((var1 val1) (var2 val2) ...) body...)
    => ((lambda (var1 var2 ...) body...) val1 val2 ...)
    """
    if len(args) < 2:
        raise ZetaArityError("let requires bindings and at least one body form")

    bindings = args[0]
    body = args[1:]

    vars_ = [var for var, *_ in bindings]
    vals_ = [val for _, val, *rest in bindings]

    return [['lambda', vars_] + body] + vals_


def defun_macro(args, env):
    if len(args) < 3:
        raise ZetaArityError("defun requires at least 3 arguments: (defun name (params) body...)")

    name = args[0]       # function name (symbol)
    params = args[1]     # parameter list
    body = args[2:]      # body expressions

    # Expand (defun name (params) body...)
    # into (define name (lambda (params) body...))
    return ['define', name, ['lambda', params] + body]


def register(macro_env: MacroEnvironment):
    macro_env.define_macro(Symbol('defun'),defun_macro)
    macro_env.define_macro(Symbol('let'),let_macro)