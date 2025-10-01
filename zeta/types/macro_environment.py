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
        self.macros: dict[Symbol, Lambda | TransformerFunction] = {}
        self._gensym_counter = count(1)

    # ----------------- Macro Registration -----------------
    def define_macro(self, name: Symbol, transformer: Lambda | TransformerFunction):
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
                    # Bind arguments to formals with &rest support
                    formals = list(transformer.formals)
                    supplied = list(args)

                    # Validate arity (when no &rest is present)
                    if Symbol("&rest") not in formals and len(supplied) != len(formals):
                        raise ZetaArityError(
                            f"Macro {head} expected {len(formals)} args, got {len(supplied)}"
                        )

                    call_env = Environment(outer=transformer.env)
                    seen_formals: list[Symbol] = []

                    while formals:
                        formal = formals.pop(0)
                        if formal == Symbol("&rest"):
                            if not formals:
                                raise ZetaArityError("Malformed parameter list: &rest must be followed by a name")
                            rest_name = formals.pop(0)
                            call_env.define(rest_name, supplied)
                            seen_formals.append(rest_name)
                            supplied = []
                            break
                        if supplied:
                            call_env.define(formal, supplied.pop(0))
                            seen_formals.append(formal)
                        else:
                            # Too few args without &rest
                            missing = [formal] + formals
                            raise ZetaArityError(
                                f"Macro {head} missing {len(missing)} arg(s): {[str(s) for s in missing]}"
                            )

                    if supplied:
                        # Too many arguments and no &rest captured them
                        raise ZetaArityError(f"Too many arguments for macro {head}: {supplied}")

                    # If the macro body is a quasiquote form, evaluate it now so that
                    # unquote/unquote-splicing run at macro expansion time. Otherwise,
                    # perform hygienic-ish syntactic substitution only (do not execute).
                    body = transformer.body
                    if isinstance(body, list) and body and body[0] == Symbol("quasiquote"):
                        return evaluator(body, call_env, self)
                    else:
                        # Perform capture-avoiding substitution, but if the resulting form
                        # is a quasiquote, evaluate it so ', and ,@ are processed now.
                        substituted = _substitute(body, call_env, set(seen_formals))
                        if isinstance(substituted, list) and substituted and substituted[0] == Symbol("quasiquote"):
                            return evaluator(substituted, call_env, self)
                        return substituted

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
