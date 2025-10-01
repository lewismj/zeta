from __future__ import annotations

from itertools import count
from typing import Callable

from zeta import SExpression, TransformerFunction
from zeta.types.environment import Environment
from zeta.types.errors import ZetaArityError
from zeta.types.lambda_fn import Lambda
from zeta.types.symbol import Symbol

# TODO - Refactor so that we get rid of this list, special_forms.SPECIAL_FORMS should be used,
# TODO - But that introduces circular dependencies.
_special_form_symbols = [
    Symbol("set"),
    Symbol("progn"),
    Symbol("begin"),
    Symbol("eval"),
    Symbol("defmacro"),
    Symbol("defstruct"),
    Symbol("import"),
    Symbol("quote"),
    Symbol("quasiquote"),
    Symbol("unquote"),
    Symbol("unquote-splicing"),
    Symbol("lambda"),
    Symbol("define"),
    Symbol("if"),
    Symbol("do"),
    Symbol("dotimes"),
    Symbol("dolist"),
    Symbol("condition-case"),
    Symbol("cond"),
    Symbol("throw"),
    Symbol("catch"),
    Symbol("apply")
]

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
        try:
            cur = form
            nxt = None
            while True:
                nxt = self.expand_1(cur, evaluator, env)
                if nxt is cur:
                    return cur
                cur = nxt
        except Exception as e:
            from zeta.debug_utils.pprint import pprint_expr
            print(f"Macro expansion error: {e}")
            print(f"cur={cur}, nxt={nxt}")
            print(f"Form: {pprint_expr(form)}")
            raise e



    def macro_expand_all(
        self,
        form: SExpression,
        evaluator: Callable,
        env: Environment
    ) -> SExpression:
        """
        Fully expand a form (head expansion fixed-point, then recurse).

        Special-case top-level `quasiquote`: delegate to the `evaluator` so
        `unquote` / `unquote-splicing` are processed during macro expansion
        (the evaluator will be called with `self` as the macro environment).

        Treat known special forms and macro calls as opaque (do not recurse).
        """
        expanded = self.macro_expand_head(form, evaluator, env)

        # If the expansion produced a quasiquote form at the top-level,
        # delegate to the evaluator to handle unquote/unquote-splicing properly.
        if isinstance(expanded, list) and expanded:
            head = expanded[0]
            if isinstance(head, Symbol) and head == Symbol("quasiquote"):
                # evaluator has signature evaluate0(expr, env, macros)
                return evaluator(expanded, env, self)

            # treat known special forms or macro calls as opaque (do not recurse into them)
            if isinstance(head, Symbol) and (head in _special_form_symbols or head in self.macros):
                return expanded

            return [self.macro_expand_all(x, evaluator, env) for x in expanded]

        if isinstance(expanded, tuple) and len(expanded) == 2:
            lst, tail = expanded
            lst_exp = [self.macro_expand_all(x, evaluator, env) for x in lst]
            tail_exp = self.macro_expand_all(tail, evaluator, env)
            return lst_exp, tail_exp

        return expanded
