"""Looping special forms for Zeta: do, dotimes, dolist.

Each loop is implemented as a small evaluator object that closes over the loop
spec and reuses the main evaluator to execute bodies in the appropriate scope.
"""

from __future__ import annotations
from zeta import SExpression, LispValue, EvaluatorFn
from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.errors import ZetaError, ZetaTypeError, ZetaInvalidSymbol
from zeta.types.macro_environment import MacroEnvironment


class DoLoopEval:
    """Implements the (do ...) loop.

    varspecs: [(var init step?) ...]
    end_clause: [test expr*]
    body: repeated forms executed each iteration
    """

    def __init__(
        self,
        varspecs: list[list[SExpression]],
        end_clause: list[SExpression],
        body: list[SExpression],
        evaluate_fn: EvaluatorFn,
    ):
        self.varspecs: list[list[SExpression]] = varspecs
        self.end_clause: list[SExpression] = end_clause
        self.body: list[SExpression] = body
        self.evaluate_fn: EvaluatorFn = evaluate_fn

    def eval(self, env: Environment, macros: MacroEnvironment) -> LispValue:
        """Evaluate the do loop by stepping until the end test is true."""
        if not self.end_clause:
            raise ZetaError("Do requires an end clause")

        local_env = Environment(outer=env)
        steps = []

        # 1: define all loop variables first
        for varspec in self.varspecs:
            var_name, *_ = varspec
            if not isinstance(var_name, Symbol):
                raise ZetaInvalidSymbol(
                    f"Do loop variable must be Symbol, got {var_name}"
                )
            local_env.define(var_name, None)

        # 2: initialize loop variables
        for varspec in self.varspecs:
            var_name, init, *step_expr = varspec + [None] * (3 - len(varspec))
            local_env.set(var_name, self.evaluate_fn(init, local_env, macros, False))
            if step_expr and step_expr[0] is not None:
                steps.append((var_name, step_expr[0]))

        # 3: unpack the end clause.
        test_expr, *exit_exprs = self.end_clause
        if not exit_exprs:
            exit_exprs = [None]

        # 4: loop
        while True:
            if self.evaluate_fn(test_expr, local_env, macros, False):
                result = None
                for expr in exit_exprs:
                    if expr is not None:
                        # **Evaluate exit expressions in local_env** so loop vars exist
                        result = self.evaluate_fn(expr, local_env, macros, False)
                return result

            for expr in self.body:
                self.evaluate_fn(expr, local_env, macros, False)

            for var, step_expr in steps:
                local_env.set(
                    var, self.evaluate_fn(step_expr, local_env, macros, False)
                )


class DoTimesLoopEval:
    """Implements the (dotimes (var count) body...) loop."""

    def __init__(
        self,
        varspec: list[SExpression],
        body: list[SExpression],
        evaluate_fn: EvaluatorFn,
    ):
        self.varspec: list[SExpression] = varspec
        self.body: list[SExpression] = body
        self.evaluate_fn: EvaluatorFn = evaluate_fn

    def eval(self, env: Environment, macros: MacroEnvironment) -> LispValue:
        """Execute body `count` times with `var` bound from 0..count-1."""
        var_name, count_expr, *rest = self.varspec + [None] * (2 - len(self.varspec))
        if not isinstance(var_name, Symbol):
            raise ZetaInvalidSymbol(f"Dotimes variable must be Symbol, got {var_name}")
        count = self.evaluate_fn(count_expr, env, macros, False)

        # Only loop variable is local
        local_env = Environment(outer=env)
        local_env.define(var_name, 0)

        last_value = None
        for i in range(count):
            local_env.set(var_name, i)
            for expr in self.body:
                last_value = self.evaluate_fn(
                    expr, local_env, macros, False
                )  # evaluate in outer env

        return last_value


class DoListLoopEval:
    """Implements the (dolist (var list) body...) loop."""

    def __init__(
        self,
        varspec: list[SExpression],
        body: list[SExpression],
        evaluate_fn: EvaluatorFn,
    ):
        self.varspec: list[SExpression] = varspec
        self.body: list[SExpression] = body
        self.evaluate_fn: EvaluatorFn = evaluate_fn

    def eval(self, env: Environment, macros: MacroEnvironment) -> LispValue:
        """Iterate over the list, binding `var` to each element in turn."""
        var_name, lst_expr, *rest = self.varspec + [None] * (2 - len(self.varspec))
        if not isinstance(var_name, Symbol):
            raise ZetaInvalidSymbol(f"Dolist variable must be Symbol, got {var_name}")
        lst_val = self.evaluate_fn(lst_expr, env, macros, False)
        if not isinstance(lst_val, list):
            raise ZetaTypeError("dolist requires a list")

        last_value = None
        # Loop variable is local
        local_env = Environment(outer=env)

        for item in lst_val:
            local_env.define(var_name, item)
            for expr in self.body:
                # <-- evaluate in local_env so loop variable is visible
                last_value = self.evaluate_fn(expr, local_env, macros, False)

        return last_value


def do_loop_form(
    tail: list[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    _: bool,
) -> LispValue:
    """Special form (do ...): evaluate a general iteration construct."""
    return DoLoopEval(tail[0], tail[1], tail[2:], evaluate_fn).eval(env, macros)


def do_times_n_loop_form(
    tail: list[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    _: bool,
) -> LispValue:
    """Special form (dotimes (var n) ...): run body `n` times."""
    return DoTimesLoopEval(tail[0], tail[1:], evaluate_fn).eval(env, macros)


def do_list_loop_form(
    tail: list[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    _: bool,
) -> LispValue:
    """Special form (dolist (var list) ...): iterate over a list."""
    return DoListLoopEval(tail[0], tail[1:], evaluate_fn).eval(env, macros)
