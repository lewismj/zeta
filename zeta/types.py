from __future__ import annotations
from io import StringIO
from itertools import count
import sys
from typing import Callable, Optional

from zeta.errors import ZetaInvalidSymbol, ZetaUnboundSymbol, ZetaNameError, ZetaArityError
from zeta import SExpression, TransformerFunction


class Symbol(str):
    def __new__(cls, name: str) -> str:
        return str.__new__(cls, sys.intern(name))

class Environment:
    def __init__(self, outer: Optional[Environment] = None):
        self.vars: dict[Symbol, SExpression] = {}
        self.outer: Environment | None = outer
        self.packages: dict[str, Environment] = {}
        self.package_aliases: dict[str, str] = {}

    def define(self, name: Symbol, value: SExpression) -> None:
        if not isinstance(name, Symbol):
            raise ZetaInvalidSymbol(f'Cannot define {name} as a symbol.')
        self.vars[name] = value

    def find(self, symbol: Symbol) -> Optional[Environment]:
        env: Optional[Environment] = self
        while env is not None:
            if symbol in env.vars:
                return env
            env = env.outer
        return None

    def set(self, name: Symbol, value: SExpression) -> None:
        env = self.find(name)
        if env is None:
            raise ZetaUnboundSymbol(f'Cannot set unbound symbol {name}.')
        env.vars[name] = value

    def lookup(self, name: Symbol):
        s = str(name)
        if ":" in s:
            pkg, sym = s.split(":", 1)
            if pkg in self.package_aliases:
                pkg = self.package_aliases[pkg]
            pkg_env = self.packages.get(pkg)
            if pkg_env:
                return pkg_env.lookup(Symbol(sym))
        env = self.find(name)
        if env is None:
            raise ZetaUnboundSymbol(f'Cannot lookup unbound symbol {name}.')
        return env.vars[name]

    def define_package(self, pkg_name: str) -> Environment:
        env = self
        while env.outer is not None:
            env = env.outer

        if pkg_name not in env.packages:
            env.packages[pkg_name] = Environment()
        return env.packages[pkg_name]

    def get_package_symbol(self, pkg_name: str, sym: Symbol) -> SExpression:
        env = self
        while env.outer is not None:
            env = env.outer
        pkg_env = env.packages.get(pkg_name)
        if pkg_env is None:
            raise ZetaNameError(f"Package '{pkg_name}' not found")
        return pkg_env.lookup(sym)

    def register_package_alias(self, alias: str, pkg_name: str) -> None:
        env = self
        while env.outer is not None:
            env = env.outer
        env.package_aliases[alias] = pkg_name

    def update(self, mapping: dict[Symbol, SExpression]) -> None:
        for k, v in mapping.items():
            if not isinstance(k, Symbol):
                raise ZetaInvalidSymbol(f"Cannot define {k} as a symbol.")
            self.vars[k] = v

    def _write_vars(self, buffer: StringIO) -> None:
        buffer.write("{")
        first = True
        for k, v in self.vars.items():
            if not first:
                buffer.write(", ")
            buffer.write(f"{k}: {v!r}")
            first = False
        buffer.write("}")

    def __str__(self) -> str:
        with StringIO() as buffer:
            self._write_vars(buffer)
            if self.outer is not None:
                buffer.write(" -> ...")  # indicate parent exists
            return buffer.getvalue()

    def __repr__(self) -> str:
        with StringIO() as buffer:
            buffer.write("<Environment chain: ")
            env = self
            chain = []
            while env is not None:
                env_buf: StringIO = StringIO()
                self._write_vars(env_buf)
                env_buf.write("}")
                chain.append(env_buf.getvalue())
                env = env.outer
            buffer.write(" -> ".join(chain))
            buffer.write(">")
            return buffer.getvalue()

class Lambda:
    def __init__(self, formals: list[Symbol], body: SExpression, env: Environment = Environment()):
        self.formals = formals
        self.body = body
        self.env =env

    def __str__(self):
        with StringIO() as buffer:
            buffer.write("(λ (")
            buffer.write(" ".join(str(f) for f in self.formals))
            buffer.write(") ")
            buffer.write(str(self.body))
            buffer.write(")")
            return buffer.getvalue()

    def __repr__(self):
        """Return the Lisp-style representation of the lambda."""
        return str(self)



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