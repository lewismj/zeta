from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaError, ZetaTypeError, ZetaInvalidSymbol
from zeta.types.macro_environment import MacroEnvironment


class DoLoopEval:
    def __init__(self, varspecs, end_clause, body, evaluate_fn):
        self.varspecs = varspecs
        self.end_clause = end_clause
        self.body = body
        self.evaluate_fn = evaluate_fn

    def eval(self, env: Environment, macros: MacroEnvironment):
        if not self.end_clause:
            raise ZetaError("do requires an end clause")

        local_env = Environment(outer=env)
        steps = []

        # Step 1: define all loop variables first
        for varspec in self.varspecs:
            var_name, *_ = varspec
            if not isinstance(var_name, Symbol):
                raise ZetaInvalidSymbol(f"do loop variable must be Symbol, got {var_name}")
            local_env.define(var_name, None)

        # Step 2: initialize loop variables
        for varspec in self.varspecs:
            var_name, init, *step_expr = varspec + [None]*(3-len(varspec))
            local_env.set(var_name, self.evaluate_fn(init, local_env, macros))
            if step_expr and step_expr[0] is not None:
                steps.append((var_name, step_expr[0]))

        # Step 3: unpack end clause
        test_expr, *exit_exprs = self.end_clause
        if not exit_exprs:
            exit_exprs = [None]

        # Step 4: loop
        while True:
            if self.evaluate_fn(test_expr, local_env, macros):
                result = None
                for expr in exit_exprs:
                    if expr is not None:
                        # **Evaluate exit expressions in local_env** so loop vars exist
                        result = self.evaluate_fn(expr, local_env, macros)
                return result

            for expr in self.body:
                self.evaluate_fn(expr, local_env, macros)

            for var, step_expr in steps:
                local_env.set(var, self.evaluate_fn(step_expr, local_env, macros))


class DoTimesLoopEval:
    def __init__(self, varspec, body, evaluate_fn):
        self.varspec = varspec
        self.body = body
        self.evaluate_fn = evaluate_fn

    def eval(self, env: Environment, macros: MacroEnvironment):
        var_name, count_expr, *rest = self.varspec + [None] * (2 - len(self.varspec))
        if not isinstance(var_name, Symbol):
            raise ZetaInvalidSymbol(f"dotimes variable must be Symbol, got {var_name}")
        count = self.evaluate_fn(count_expr, env, macros)

        # Only loop variable is local
        local_env = Environment(outer=env)
        local_env.define(var_name, 0)

        last_value = None
        for i in range(count):
            local_env.set(var_name, i)
            for expr in self.body:
                last_value = self.evaluate_fn(expr, local_env, macros)  # evaluate in outer env

        return last_value

class DoListLoopEval:
    def __init__(self, varspec, body, evaluate_fn):
        self.varspec = varspec
        self.body = body
        self.evaluate_fn = evaluate_fn

    def eval(self, env: Environment, macros: MacroEnvironment):
        var_name, lst_expr, *rest = self.varspec + [None] * (2 - len(self.varspec))
        if not isinstance(var_name, Symbol):
            raise ZetaInvalidSymbol(f"dolist variable must be Symbol, got {var_name}")
        lst_val = self.evaluate_fn(lst_expr, env, macros)
        if not isinstance(lst_val, list):
            raise ZetaTypeError("dolist requires a list")

        last_value = None
        # Loop variable is local
        local_env = Environment(outer=env)

        for item in lst_val:
            local_env.define(var_name, item)
            for expr in self.body:
                # <-- evaluate in local_env so loop variable is visible
                last_value = self.evaluate_fn(expr, local_env, macros)

        return last_value

def do_loop_form(tail, env, macros, evaluate_fn):
    return DoLoopEval(tail[0], tail[1], tail[2:], evaluate_fn).eval(env, macros)

def do_times_n_loop_form(tail, env, macros, evaluate_fn):
    return DoTimesLoopEval(tail[0], tail[1:], evaluate_fn).eval(env, macros)

def do_list_loop_form(tail, env, macros, evaluate_fn):
    return DoListLoopEval(tail[0], tail[1:], evaluate_fn).eval(env, macros)