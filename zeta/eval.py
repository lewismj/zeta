from __future__ import annotations
from typing import Any
from zeta.types import SExpression, Environment, Lambda, Symbol, MacroEnvironment, _substitute
from zeta.errors import ZetaArityError, ZetaTypeError, ZetaError, ZetaInvalidSymbol


class ThrowException(Exception):
    """Custom exception for Lisp-style throw/catch non-local exit."""
    def __init__(self, tag, value):
        super().__init__(f"ThrowException(tag={tag}, value={value})")
        self.tag = tag
        self.value = value


# ----------------- Quasiquote helpers -----------------
class QQ:
    def __init__(self, value):
        self.value = value

class UQ:
    def __init__(self, value):
        self.value = value

class UQSplice:
    def __init__(self, value):
        self.value = value


# ----------------- Loop helpers -----------------
class DoEval:
    def __init__(self, varspecs, end_clause, body):
        self.varspecs = varspecs       # list of [var, init, step?]
        self.end_clause = end_clause   # list: [test_expr, exit_exprs...]
        self.body = body               # list of body expressions

    def eval(self, env: Environment, macros: MacroEnvironment):
        from zeta.types import Symbol

        if not self.end_clause:
            raise ZetaError("do requires an end clause")

        # Use a local environment for loop variables only
        local_env = Environment(outer=env)
        steps = []
        for varspec in self.varspecs:
            var_name, init, *step = varspec + [None] * (3 - len(varspec))
            if not isinstance(var_name, Symbol):
                raise ZetaInvalidSymbol(f"do loop variable must be Symbol, got {var_name}")
            local_env.define(var_name, evaluate(init, env, macros))
            if step and step[0] is not None:
                steps.append((var_name, step[0]))

        test_expr, *exit_exprs = self.end_clause
        if not exit_exprs:
            exit_exprs = [None]

        while True:
            if evaluate(test_expr, local_env, macros):
                result = None
                for expr in exit_exprs:
                    if expr is not None:
                        result = evaluate(expr, env, macros)  # body sees outer env
                return result

            for expr in self.body:
                evaluate(expr, env, macros)  # evaluate in outer env

            for var, step_expr in steps:
                local_env.set(var, evaluate(step_expr, local_env, macros))


class DotimesEval:
    def __init__(self, varspec, body):
        self.varspec = varspec
        self.body = body

    def eval(self, env: Environment, macros: MacroEnvironment):
        var_name, count_expr, *rest = self.varspec + [None] * (2 - len(self.varspec))
        if not isinstance(var_name, Symbol):
            raise ZetaInvalidSymbol(f"dotimes variable must be Symbol, got {var_name}")
        count = evaluate(count_expr, env, macros)

        # Only loop variable is local
        local_env = Environment(outer=env)
        local_env.define(var_name, 0)

        last_value = None
        for i in range(count):
            local_env.set(var_name, i)
            for expr in self.body:
                last_value = evaluate(expr, local_env, macros)  # evaluate in outer env

        return last_value


class DolistEval:
    def __init__(self, varspec, body):
        self.varspec = varspec
        self.body = body

    def eval(self, env: Environment, macros: MacroEnvironment):
        var_name, lst_expr, *rest = self.varspec + [None] * (2 - len(self.varspec))
        if not isinstance(var_name, Symbol):
            raise ZetaInvalidSymbol(f"dolist variable must be Symbol, got {var_name}")
        lst_val = evaluate(lst_expr, env, macros)
        if not isinstance(lst_val, list):
            raise ZetaTypeError("dolist requires a list")

        last_value = None
        # Only loop variable is local
        local_env = Environment(outer=env)

        for item in lst_val:
            local_env.define(var_name, item)
            for expr in self.body:
                last_value = evaluate(expr, env, macros)  # evaluate in outer env

        return last_value



# ----------------- Core evaluation -----------------
def evaluate(expr: SExpression, env: Environment, macros: MacroEnvironment = None) -> SExpression:
    """Evaluate a Zeta expression in the given environment with macro support."""
    if macros is None:
        macros = MacroEnvironment()

    if isinstance(expr, QQ):
        return [Symbol("quasiquote"), evaluate(expr.value, env, macros)]
    if isinstance(expr, UQ):
        return evaluate(expr.value, env, macros)
    if isinstance(expr, UQSplice):
        result = evaluate(expr.value, env, macros)
        if not isinstance(result, list):
            raise ZetaTypeError("unquote-splicing must produce a list")
        return result

    if isinstance(expr, list) and not expr:
        return []

    if isinstance(expr, list) and expr:
        expr = macros.macro_expand_all(expr, evaluate, env)

    if isinstance(expr, list) and expr:
        head, *tail = expr

        # Special forms
        if head == Symbol("defmacro"):
            if len(tail) < 2:
                raise ZetaArityError("defmacro requires a name and parameter list")

            # Extract macro name, parameters, and body
            macro_name = tail[0]
            if not isinstance(macro_name, Symbol):
                raise ZetaInvalidSymbol(f"Macro name must be a Symbol, got {macro_name}")

            params = tail[1]
            body = tail[2:]

            if not isinstance(params, list):
                raise ZetaTypeError("Macro parameter list must be a list")
            if not body:
                raise ZetaArityError("Macro body cannot be empty")

            # Wrap multiple expressions in a progn
            macro_body = body[0] if len(body) == 1 else [Symbol("progn")] + body
            macro_lambda = Lambda(params, macro_body, env)

            # Create a TransformerFunction that performs syntactic substitution
            def transformer(args: list[SExpression], call_env: Environment) -> SExpression:
                if len(args) != len(macro_lambda.formals):
                    raise ZetaArityError(
                        f"Macro {macro_name} expected {len(macro_lambda.formals)} args, got {len(args)}"
                    )
                # Create a temporary environment for substitution
                subst_env = Environment(outer=macro_lambda.env)
                for param, arg in zip(macro_lambda.formals, args):
                    subst_env.define(param, arg)
                # _substitute is assumed to perform syntactic replacement without evaluating
                return _substitute(macro_lambda.body, subst_env, set(macro_lambda.formals))

            # Register the macro TransformerFunction
            macros.define_macro(macro_name, transformer)

            return None  # defmacro evaluates to None

        if head == Symbol("defstruct"):
            if not tail or not isinstance(tail[0], Symbol):
                raise ZetaArityError("defstruct requires a struct name")
            struct_name = tail[0]
            fields = tail[1:]

            def make_struct(env_inner, args):
                if len(args) != len(fields):
                    raise ZetaArityError(f"{struct_name} constructor expects {len(fields)} args")
                return {"__type__": struct_name, **dict(zip(fields, args))}

            constructor_name = Symbol(f"make-{struct_name}")
            env.define(constructor_name, make_struct)

            for field in fields:
                if not isinstance(field, Symbol):
                    raise ZetaInvalidSymbol(f"defstruct field must be Symbol, got {field}")
                accessor_name = Symbol(f"{struct_name}-{field}")
                def make_accessor(f):
                    return lambda env_inner, args: args[0][f]
                env.define(accessor_name, make_accessor(field))
            return None

        if head == Symbol("quote"):
            if len(tail) != 1:
                raise ZetaArityError("quote expects exactly 1 argument")
            return tail[0]

        if head == Symbol("quasiquote"):
            if len(tail) != 1:
                raise ZetaArityError("quasiquote expects exactly 1 argument")
            return evaluate(eval_quasiquote(tail[0], env, macros), env, macros)

        if head in (Symbol("unquote"), Symbol("unquote-splicing")):
            raise ZetaError(f"{head} not valid outside of quasiquote")

        if head == Symbol("lambda"):
            if not tail:
                raise ZetaArityError("lambda requires at least a parameter list")
            params, body = tail[0], tail[1]
            return Lambda(params, body, env)

        if head == Symbol("define"):
            if len(tail) != 2:
                raise ZetaArityError("define requires exactly 2 arguments")
            name, val_expr = tail
            value = evaluate(val_expr, env, macros)
            env.define(name, value)
            return None

        if head == Symbol("if"):
            if len(tail) < 2:
                raise ZetaArityError("if requires a condition and a then-expression")
            cond = evaluate(tail[0], env, macros)
            if cond:
                return evaluate(tail[1], env, macros)
            elif len(tail) > 2:
                return evaluate(tail[2], env, macros)
            else:
                return None

        if head == Symbol("set"):
            if len(tail) != 2:
                raise ZetaArityError("set requires exactly 2 arguments: (set var value)")
            var_sym, val_expr = tail
            if not isinstance(var_sym, Symbol):
                raise ZetaInvalidSymbol(f"set first argument must be a Symbol, got {var_sym}")
            value = evaluate(val_expr, env, macros)
            env.set(var_sym, value)  # walks env chain
            return value

        if head in (Symbol("progn"), Symbol("begin")):
            result = None
            for e in tail:
                result = evaluate(e, env, macros)
            return result

        # Loop forms
        if head == Symbol("do"):
            return DoEval(tail[0], tail[1], tail[2:]).eval(env, macros)
        if head == Symbol("dotimes"):
            return DotimesEval(tail[0], tail[1:]).eval(env, macros)
        if head == Symbol("dolist"):
            return DolistEval(tail[0], tail[1:]).eval(env, macros)

        # Exception handling
        if head == Symbol("condition-case"):
            return eval_condition_case(tail, env, macros)
        if head == Symbol("catch"):
            tag_expr, body_expr = tail[0], tail[1]
            tag = evaluate(tag_expr, env, macros)
            try:
                return evaluate(body_expr, env, macros)
            except ThrowException as ex:
                if ex.tag == tag:
                    return ex.value
                else:
                    raise ex
        if head == Symbol("throw"):
            tag, val_expr = tail[0], tail[1]
            val = evaluate(val_expr, env, macros)
            raise ThrowException(tag, val)

        if head == Symbol("apply"):
            fn = evaluate(tail[0], env, macros)
            args = evaluate(tail[1], env, macros)
            return fn(args, env)

        # Evaluate Lambda or Symbol head
        if isinstance(head, Symbol):
            head = env.lookup(head)

        # Handle Lambda head
        if isinstance(head, Lambda):
            fn = head
            args = [evaluate(arg, env, macros) for arg in tail]

            formals = list(fn.formals)
            supplied = list(args)
            remaining_formals = []

            local_env = Environment(outer=fn.env)

            while formals:
                formal = formals.pop(0)
                if formal == Symbol("&rest"):
                    name = formals.pop(0)
                    local_env.define(name, supplied)
                    supplied = []
                    break
                if supplied:
                    local_env.define(formal, supplied.pop(0))
                else:
                    remaining_formals.append(formal)

            if remaining_formals:
                # Return a new Lambda with only remaining formals
                return Lambda(remaining_formals, fn.body, local_env)

            if supplied:
                raise ZetaArityError(f"Too many arguments: {supplied}")

            # All formals supplied: evaluate the body in local_env
            return evaluate(fn.body, local_env, macros)

        if callable(head) and not isinstance(head, Lambda):
            args = [evaluate(arg, env, macros) for arg in tail]
            return head(env,args)
        if isinstance(head, list):
            # Evaluate the head expression to resolve to a function object
            head_eval = evaluate(head, env, macros)
            # Then re-evaluate as if (head_eval tail...)
            return evaluate([head_eval] + tail, env, macros)


    if isinstance(expr, Symbol):
        return env.lookup(expr)

    return expr


# ----------------- Apply callable -----------------
def apply_callable(fn: Any, args: list[SExpression], env: Environment, macros: MacroEnvironment):
    if callable(fn) and not isinstance(fn, Lambda):
        return fn(args, env)


# ----------------- Quasiquote evaluation -----------------
def eval_quasiquote(expr, env, macros, depth=1):
    from zeta.types import Symbol

    if isinstance(expr, QQ):
        return [Symbol("quasiquote"), eval_quasiquote(expr.value, env, macros, depth)]
    if isinstance(expr, UQ):
        if depth == 1:
            return evaluate(expr.value, env, macros)
        else:
            return UQ(eval_quasiquote(expr.value, env, macros, depth - 1))
    if isinstance(expr, UQSplice):
        if depth == 1:
            result = evaluate(expr.value, env, macros)
            if not isinstance(result, list):
                raise ZetaTypeError("unquote-splicing must produce a list")
            return result
        else:
            return UQSplice(eval_quasiquote(expr.value, env, macros, depth - 1))

    if not isinstance(expr, list):
        return expr
    if not expr:
        return []

    result = []
    for item in expr:
        if isinstance(item, list) and item:
            head, *tail = item
            if head == Symbol("quasiquote"):
                result.append([Symbol("quasiquote"), eval_quasiquote(tail[0], env, macros, depth + 1)])
                continue
            if head == Symbol("unquote") and depth == 1:
                result.append(evaluate(tail[0], env, macros))
                continue
            if head == Symbol("unquote-splicing") and depth == 1:
                spliced = evaluate(tail[0], env, macros)
                if not isinstance(spliced, list):
                    raise ZetaTypeError("unquote-splicing must produce a list")
                result.extend(spliced)
                continue
        result.append(eval_quasiquote(item, env, macros, depth))
    return result


# ----------------- Condition-case -----------------
def eval_condition_case(tail: list[SExpression], env: Environment, macros: MacroEnvironment) -> SExpression:
    if not tail:
        raise ZetaArityError("condition-case requires at least a body expression")

    body_expr = tail[0]
    handlers = tail[1:]

    try:
        return evaluate(body_expr, env, macros)
    except Exception as ex:
        for handler in handlers:
            if not isinstance(handler, list) or len(handler) < 2:
                continue
            cond_symbol = handler[0]
            rest = handler[1:]

            if not isinstance(cond_symbol, Symbol):
                continue

            if cond_symbol == Symbol("error"):
                local_env = Environment(outer=env)
                handler_body = rest
                if len(rest) >= 2 and isinstance(rest[0], Symbol):
                    error_var = rest[0]
                    local_env.define(error_var, ex)
                    handler_body = rest[1:]

                result = None
                for expr in handler_body:
                    result = evaluate(expr, local_env, macros)
                return result

        raise ex
