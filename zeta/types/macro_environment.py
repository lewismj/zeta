from __future__ import annotations
from typing import Callable
from itertools import count
from zeta import EvaluatorFn

from zeta import SExpression
from zeta.types.environment import Environment
from zeta.types.errors import ZetaArityError
from zeta.types.lambda_fn import Lambda
from zeta.types.symbol import Symbol


def _substitute(
    expr: SExpression, call_env: Environment, formals: set[Symbol]
) -> SExpression:
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
        return (
            [_substitute(x, call_env, formals) for x in lst],
            _substitute(tail, call_env, formals),
        )

    return expr

def _expand_lambda(head, args, macro_env, evaluator, transformer):
    # Bind arguments to formals with &rest/&body and &key support
    formals = list(transformer.formals)
    supplied = list(args)

    # Normalize &body -> &rest for macro parameter lists
    has_rest = Symbol("&rest") in formals or Symbol("&body") in formals
    has_key = Symbol("&key") in formals

    if has_rest and has_key:
        raise ZetaArityError("Malformed parameter list: cannot mix &rest/&body and &key")

    call_env = Environment(outer=transformer.env)
    seen_formals: list[Symbol] = []

    if has_rest:
        # Find the rest marker and its binding name
        # Allow either &rest name  OR  &body body
        # Bind all remaining supplied args to that rest name
        # Example: (times &body body) with args (3 (print "hi"))
        i_rest = None
        for i, f in enumerate(formals):
            if f == Symbol("&rest") or f == Symbol("&body"):
                i_rest = i
                break
        assert i_rest is not None  # by has_rest invariant

        # Positional before the marker
        leading = formals[:i_rest]
        # The symbol that names the rest/body
        if i_rest + 1 >= len(formals):
            raise ZetaArityError("Malformed parameter list: &rest/&body must be followed by a name")
        rest_name = formals[i_rest + 1]

        # Bind positional
        if len(supplied) < len(leading):
            missing = [str(s) for s in leading[len(supplied):]]
            raise ZetaArityError(f"Macro {head} missing {len(missing)} arg(s): {missing}")
        for f, v in zip(leading, supplied[: len(leading)]):
            call_env.define(f, v)
            seen_formals.append(f)

        # Bind rest/name to the remaining args (possibly empty)
        rest_vals = supplied[len(leading):]
        call_env.define(rest_name, rest_vals)
        seen_formals.append(rest_name)

    elif has_key:
        key_index = formals.index(Symbol("&key"))
        positional_formals = formals[:key_index]
        keyword_formals = formals[key_index + 1:]

        # Bind positional first
        if len(supplied) < len(positional_formals):
            missing = [str(s) for s in positional_formals[len(supplied):]]
            raise ZetaArityError(f"Macro {head} missing {len(missing)} arg(s): {missing}")
        for f, v in zip(positional_formals, supplied[: len(positional_formals)]):
            call_env.define(f, v)
            seen_formals.append(f)
        supplied = supplied[len(positional_formals):]

        # Keywords must be pairs like :name value
        if len(supplied) % 2 != 0:
            raise ZetaArityError("Keyword arguments must be in pairs")
        provided_keys: dict[Symbol, SExpression] = {}
        while supplied:
            key = supplied.pop(0)
            val = supplied.pop(0)
            if not isinstance(key, Symbol) or not key.id.startswith(":"):
                raise ZetaArityError("Expected keyword symbol like :name in keyword arguments")
            target = Symbol(key.id[1:])
            if target not in keyword_formals:
                raise ZetaArityError(f"Unknown keyword argument {key}")
            provided_keys[target] = val
        for kf in keyword_formals:
            call_env.define(kf, provided_keys.get(kf, None))
            seen_formals.append(kf)

    else:
        # Simple positional: exact arity
        if len(supplied) != len(formals):
            raise ZetaArityError(
                f"Macro {head} expected {len(formals)} args, got {len(supplied)}"
            )
        for f, v in zip(formals, supplied):
            call_env.define(f, v)
            seen_formals.append(f)

    # Quasiquote-aware body handling
    body = transformer.body
    if isinstance(body, list) and body and body[0] == Symbol("quasiquote"):
        return evaluator(body, call_env, macro_env)
    else:
        substituted = _substitute(body, call_env, set(seen_formals))
        if (
            isinstance(substituted, list)
            and substituted
            and substituted[0] == Symbol("quasiquote")
        ):
            return evaluator(substituted, call_env, macro_env)
        return substituted

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

    # ----------------- Macro Registration -----------------
    def define_macro(self, name: Symbol, transformer: Lambda):
        self.macros[name] = transformer

    def is_macro(self, sym: Symbol) -> bool:
        return sym in self.macros

    # ----------------- Gen-sym Utility -----------------
    def gen_sym(self, prefix: str = "G") -> Symbol:
        return Symbol(f"{prefix}{next(self._gensym_counter)}")

    # ----------------- Single-step head expansion -----------------
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
                # These can be removed, useful for testing/running without a prelude.
                return _expand_lambda(head, args, self, evaluator, transformer) \
                    if isinstance(transformer, Lambda) else transformer(args, env)

        return form  # Not a macro call, unchanged

    # ----------------- Fixed-point head expansion -----------------
    def macro_expand_head(
        self, form: SExpression, evaluator: Callable, env: Environment
    ) -> SExpression:
        cur = form
        while True:
            nxt = self.expand_1(cur, evaluator, env)
            if nxt is cur:
                return cur
            cur = nxt

    # ----------------- Recursive full expansion -----------------
    def macro_expand_all(
        self, form: SExpression, evaluator: Callable, env: Environment
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
