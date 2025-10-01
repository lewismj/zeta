from zeta.types.errors import ZetaArityError, ZetaTypeError
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol


def let_macro(args, env):
    """
    (let ((var1 val1) (var2 val2) ...) body...)
    => ((lambda (var1 var2 ...) body...) val1 val2 ...)
    """
    if len(args) < 2:
        raise ZetaArityError("let requires bindings and at least one body form")

    bindings = args[0]
    body = list(args[1:])

    if not isinstance(bindings, list):
        raise ZetaTypeError("let bindings must be a list")

    vars_ = []
    vals_ = []
    for b in bindings:
        if not isinstance(b, list) or len(b) != 2:
            raise ZetaTypeError(f"let binding must be a list of two elements, got {b}")
        var, val = b
        if not isinstance(var, Symbol):
            raise ZetaTypeError(f"let binding name must be a Symbol, got {var}")
        vars_.append(var)
        vals_.append(val)

    return [[Symbol('lambda'), vars_] + body] + vals_


def defun_macro(args, env):
    if len(args) < 3:
        raise ZetaArityError("defun requires at least 3 arguments: (defun name (params) body...)")

    name = args[0]       # function name (symbol)
    params = args[1]     # parameter list
    body = args[2:]      # body expressions

    # Expand (defun name (params) body...)
    # into (define name (lambda (params) body...))
    return [Symbol('define'), name, [Symbol('lambda'), params] + body]


def register(macro_env: MacroEnvironment):
    macro_env.define_macro(Symbol('defun'),defun_macro)
    macro_env.define_macro(Symbol('let'),let_macro)