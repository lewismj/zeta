from __future__ import annotations

from io import StringIO
from typing import Optional

from zeta import LispValue
from zeta.types.errors import ZetaInvalidSymbol, ZetaUnboundSymbol, ZetaNameError
from zeta.types.symbol import Symbol


class Environment:
    def __init__(self, outer: Optional[Environment] = None):
        # Runtime environment stores evaluated LispValue(s)
        self.vars: dict[Symbol, LispValue] = {}
        self.outer: Environment | None = outer
        self.packages: dict[str, Environment] = {}
        self.package_aliases: dict[str, str] = {}

    def define(self, name: Symbol, value: LispValue) -> None:
        if not isinstance(name, Symbol):
            raise ZetaInvalidSymbol(f"Cannot define {name} as a symbol")
        self.vars[name] = value

    def find(self, symbol: Symbol) -> Optional[Environment]:
        env: Optional[Environment] = self
        while env is not None:
            if symbol in env.vars:
                return env
            env = env.outer
        return None

    def set(self, name: Symbol, value: LispValue) -> None:
        env = self.find(name)
        if env is None:
            raise ZetaUnboundSymbol(f"Cannot set unbound symbol {name}")
        env.vars[name] = value

    def lookup(self, name: Symbol) -> LispValue:
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
            raise ZetaUnboundSymbol(f"Cannot lookup unbound symbol {name}")
        return env.vars[name]

    def define_package(self, pkg_name: str) -> Environment:
        env = self
        while env.outer is not None:
            env = env.outer

        if pkg_name not in env.packages:
            env.packages[pkg_name] = Environment()
        return env.packages[pkg_name]

    def get_package_symbol(self, pkg_name: str, sym: Symbol) -> LispValue:
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

    def update(self, mapping: dict[Symbol, LispValue]) -> None:
        for k, v in mapping.items():
            if not isinstance(k, Symbol):
                raise ZetaInvalidSymbol(f"Cannot define {k} as a symbol")
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
