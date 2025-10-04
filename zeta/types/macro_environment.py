from __future__ import annotations
from typing import Callable
from itertools import count
from zeta import EvaluatorFn

from zeta import SExpression
from zeta.types.environment import Environment
from zeta.types.errors import ZetaArityError
from zeta.types.lambda_fn import Lambda
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil


# Canonical macro expansion: run transformer body to produce expansion code without
# bespoke substitution. Binding of args is delegated to Lambda.extend_env.

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
        self.macros: dict[Symbol, Lambda | Callable] = {}
        self._gensym_counter = count(1)

    def define_macro(self, name: Symbol, transformer: Lambda | Callable):
        self.macros[name] = transformer

    def is_macro(self, sym: Symbol) -> bool:
        return sym in self.macros

    def gen_sym(self, prefix: str = "G") -> Symbol:
        return Symbol(f"{prefix}{next(self._gensym_counter)}")

    def _run_transformer(
        self,
        head: Symbol,
        args: list[SExpression],
        transformer: Lambda,
        evaluator: EvaluatorFn,
        macro_env: "MacroEnvironment",
    ) -> SExpression:
        """
        Canonical macro transformer invocation:
        - Bind raw, unevaluated args to the transformer's formals using Lambda.extend_env.
        - Evaluate the transformer body once to produce an expansion S-expression.
        - Do NOT evaluate the returned expansion here; just return it.
        """
        # Bind arguments without pre-evaluating them; defaults may be evaluated via evaluator
        call_env = transformer.extend_env(list(args), caller_env=None, evaluate_fn=evaluator, macros=macro_env)

        body = transformer.body
        # Execute transformer body to produce expansion (usually via quasiquote)
        expansion = evaluator(body, call_env, macro_env)
        return expansion

    # Single-step head expansion
    def expand_1(
        self, form: SExpression, evaluator: EvaluatorFn, env: Environment
    ) -> SExpression:
        """Expand only the head-position macro if present."""
        if isinstance(form, list) and form:
            head = form[0]
            if isinstance(head, Symbol) and self.is_macro(head):
                transformer = self.macros[head]
                args = form[1:]

                # We can have builtin transformers that are resolved to Python Callable
                # functions. These are invoked directly with (args, env).
                # These could be removed/replaced, useful for testing/running without a prelude.
                if isinstance(transformer, Lambda):
                    return self._run_transformer(head, args, transformer, evaluator, self)
                else:
                    # Python callable macro transformers must accept (args, env) and return an S-expression
                    return transformer(args, env)

        return form  # Not a macro call, unchanged

    # Fixed-point head expansion
    def macro_expand_head(
        self, form: SExpression, evaluator: Callable, env: Environment
    ) -> SExpression:
        cur = form
        while True:
            nxt = self.expand_1(cur, evaluator, env)
            # Use structural equality to detect fixpoint (not object identity)
            if nxt == cur:
                return cur
            cur = nxt

    # Full expansion
    def macro_expand_all(
        self, form: SExpression, evaluator: Callable, env: Environment
    ) -> SExpression:
        expanded = self.macro_expand_head(form, evaluator, env)

        # Do not recurse into (quote ...) or (quasiquote ...) templates.
        if isinstance(expanded, list):
            if expanded and isinstance(expanded[0], Symbol) and expanded[0] in (Symbol("quote"), Symbol("quasiquote")):
                return expanded
            return [self.macro_expand_all(x, evaluator, env) for x in expanded]

        if isinstance(expanded, tuple) and len(expanded) == 2:
            lst, tail = expanded
            lst_exp = [self.macro_expand_all(x, evaluator, env) for x in lst]
            tail_exp = self.macro_expand_all(tail, evaluator, env)
            return lst_exp, tail_exp

        return expanded
