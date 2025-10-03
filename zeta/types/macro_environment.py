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


def _substitute(
    expr: SExpression, call_env: Environment, formals: set[Symbol]
) -> SExpression:
    """
    Substitute macro formals with arguments.
    Also, resolve symbols from the closure environment if present.
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
    # Bind arguments to formals with &optional, &rest/&body and &key support
    formals = list(transformer.formals)
    supplied = list(args)

    has_rest = Symbol("&rest") in formals or Symbol("&body") in formals
    has_key = Symbol("&key") in formals

    if has_rest and has_key:
        raise ZetaArityError("Malformed parameter list: cannot mix &rest/&body and &key")

    call_env = Environment(outer=transformer.env)
    seen_formals: list[Symbol] = []

    # Helper to bind optionals in a list of specs (symbol or (name default))
    def bind_optionals(optional_specs: list[Symbol | list]):
        nonlocal supplied
        for spec in optional_specs:
            if supplied:
                name = spec if isinstance(spec, Symbol) else spec[0]
                call_env.define(name, supplied.pop(0))
                seen_formals.append(name)
            else:
                if isinstance(spec, Symbol):
                    call_env.define(spec, Nil)
                    seen_formals.append(spec)
                else:
                    name = spec[0]
                    default_expr = spec[1] if len(spec) >= 2 else Nil
                    if evaluator is not None and default_expr is not Nil:
                        val = evaluator(default_expr, call_env, macro_env, False)
                    else:
                        val = Nil
                    call_env.define(name, val)
                    seen_formals.append(name)

    if has_rest:
        # Find the rest/body marker
        i_rest = None
        for i, f in enumerate(formals):
            if f == Symbol("&rest") or f == Symbol("&body"):
                i_rest = i
                break
        assert i_rest is not None

        leading = formals[:i_rest]
        if i_rest + 1 >= len(formals):
            raise ZetaArityError("Malformed parameter list: &rest/&body must be followed by a name")
        rest_name = formals[i_rest + 1]

        # Split leading into required and optionals if present
        if Symbol("&optional") in leading:
            opt_index = leading.index(Symbol("&optional"))
            required_formals = leading[:opt_index]
            optional_specs = leading[opt_index + 1:]
        else:
            required_formals = leading
            optional_specs = []

        # Bind required positionals
        if len(supplied) < len(required_formals):
            missing = [str(s) for s in required_formals[len(supplied):]]
            raise ZetaArityError(f"Macro {head} missing {len(missing)} arg(s): {missing}")
        for f, v in zip(required_formals, supplied[: len(required_formals)]):
            call_env.define(f, v)
            seen_formals.append(f)
        supplied = supplied[len(required_formals):]

        # Bind optionals
        bind_optionals(optional_specs)

        # Rest gets whatever remains
        call_env.define(rest_name, supplied)
        seen_formals.append(rest_name)
        supplied = []

    elif has_key:
        key_index = formals.index(Symbol("&key"))
        leading = formals[:key_index]
        keyword_formals = formals[key_index + 1:]

        # Split leading into required positionals and optionals
        if Symbol("&optional") in leading:
            opt_index = leading.index(Symbol("&optional"))
            positional_formals = leading[:opt_index]
            optional_specs = leading[opt_index + 1:]
        else:
            positional_formals = leading
            optional_specs = []

        # Bind required positionals
        if len(supplied) < len(positional_formals):
            missing = [str(s) for s in positional_formals[len(supplied):]]
            raise ZetaArityError(f"Macro {head} missing {len(missing)} arg(s): {missing}")
        for f, v in zip(positional_formals, supplied[: len(positional_formals)]):
            call_env.define(f, v)
            seen_formals.append(f)
        supplied = supplied[len(positional_formals):]

        # Bind optionals
        bind_optionals(optional_specs)

        # Now bind keyword args from remaining supplied
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
        # No &rest or &key: allow &optional or simple positional
        if Symbol("&optional") in formals:
            opt_index = formals.index(Symbol("&optional"))
            required_formals = formals[:opt_index]
            optional_specs = formals[opt_index + 1:]

            # Bind required
            if len(supplied) < len(required_formals):
                missing = [str(s) for s in required_formals[len(supplied):]]
                raise ZetaArityError(f"Macro {head} missing {len(missing)} arg(s): {missing}")
            for f, v in zip(required_formals, supplied[: len(required_formals)]):
                call_env.define(f, v)
                seen_formals.append(f)
            supplied = supplied[len(required_formals):]

            # Bind optionals
            bind_optionals(optional_specs)

            if supplied:
                raise ZetaArityError(f"Too many arguments: {supplied}")
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
        if isinstance(substituted, list) and substituted:
            head_sym = substituted[0]
            if head_sym == Symbol("quote"):
                return substituted
            if head_sym == Symbol("quasiquote"):
                return evaluator(substituted, call_env, macro_env)
        return evaluator(substituted, call_env, macro_env)

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
                return _expand_lambda(head, args, self, evaluator, transformer) \
                    if isinstance(transformer, Lambda) else transformer(args, env)

        return form  # Not a macro call, unchanged

    # Fixed-point head expansion
    def macro_expand_head(
        self, form: SExpression, evaluator: Callable, env: Environment
    ) -> SExpression:
        cur = form
        while True:
            nxt = self.expand_1(cur, evaluator, env)
            if nxt is cur:
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
