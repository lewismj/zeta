from __future__ import annotations

from itertools import count
from typing import Callable

from zeta import SExpression, TransformerFunction
from zeta.types.environment import Environment
from zeta.types.errors import ZetaArityError
from zeta.types.lambda_fn import Lambda
from zeta.types.symbol import Symbol


def _substitute(expr: SExpression, call_env: Environment, formals: set[Symbol]) -> SExpression:
    """
    Substitute macro formals with arguments.
    Also resolve symbols from closure environment if present.
    """
    if isinstance(expr, Symbol):
        if expr in formals:
            return call_env.lookup(expr)
        try:
            return call_env.lookup(expr)  # closure lookup
        except Exception:
            return expr  # leave untouched if unbound

    if isinstance(expr, list):
        return [_substitute(x, call_env, formals) for x in expr]

    if isinstance(expr, tuple) and len(expr) == 2:  # dotted list
        lst, tail = expr
        return ([_substitute(x, call_env, formals) for x in lst],
                _substitute(tail, call_env, formals))

    return expr


class MacroEnvironment:
    """
    Macro environment mapping macro names (Symbols) to Lambda transformers
    or Python callable transformer functions.

    Features:
    - Head-position macro expansion
    - Recursive nested expansion
    - Dotted list support
    - Closure environment evaluation
    - Hygienic gensym support
    """

    def __init__(self):
        self.macros: dict[Symbol, TransformerFunction] = {}
        self._gensym_counter = count(1)

    # ----------------- Macro Registration -----------------
    def define_macro(self, name: Symbol, transformer: TransformerFunction):
        self.macros[name] = transformer

    def is_macro(self, sym: Symbol) -> bool:
        return sym in self.macros

    # ----------------- Gen-sym Utility -----------------
    def gen_sym(self, prefix: str = "G") -> Symbol:
        return Symbol(f"{prefix}{next(self._gensym_counter)}")

    # ----------------- Single-step head expansion -----------------
    def expand_1(
        self,
        form: SExpression,
        evaluator: Callable,
        env: Environment
    ) -> SExpression:
        """Expand only the head-position macro if present."""
        if isinstance(form, list) and form:
            head = form[0]
            if isinstance(head, Symbol) and self.is_macro(head):
                transformer = self.macros[head]
                args = form[1:]

                # Lambda macro transformer
                if isinstance(transformer, Lambda):
                    if len(args) != len(transformer.formals):
                        raise ZetaArityError(
                            f"Macro {head} expected {len(transformer.formals)} args, got {len(args)}"
                        )

                    # Create call environment for parameter substitution
                    call_env = Environment(outer=transformer.env)
                    for param, arg in zip(transformer.formals, args):
                        call_env.define(param, arg)

                    # Perform syntactic substitution instead of evaluating
                    return _substitute(transformer.body, call_env, set(transformer.formals))

                # Python callable transformer
                return transformer(args, env)

        return form  # Not a macro call, unchanged

    # ----------------- Fixed-point head expansion -----------------
    def macro_expand_head(
        self,
        form: SExpression,
        evaluator: Callable,
        env: Environment
    ) -> SExpression:
        cur = form
        while True:
            nxt = self.expand_1(cur, evaluator, env)
            if nxt is cur:
                return cur
            cur = nxt

    # ----------------- Recursive full expansion -----------------
    def macro_expand_all(
        self,
        form: SExpression,
        evaluator: Callable,
        env: Environment
    ) -> SExpression:
        expanded = self.macro_expand_head(form, evaluator, env)

        if isinstance(expanded, list):
            return [self.macro_expand_all(x, evaluator, env) for x in expanded]

        if isinstance(expanded, tuple) and len(expanded) == 2:
            lst, tail = expanded
            lst_exp = [self.macro_expand_all(x, evaluator, env) for x in lst]
            tail_exp = self.macro_expand_all(tail, evaluator, env)
            return lst_exp, tail_exp

        return expanded
