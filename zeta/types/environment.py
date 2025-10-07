"""Runtime environment for Zeta.

The Environment stores bindings of Symbols to evaluated Lisp values and supports
nested scopes via an `outer` link. It also implements a lightweight package
system with namespace aliases to allow qualified symbol lookups like pkg:sym.
"""

from __future__ import annotations

from io import StringIO
from typing import Optional

from zeta import LispValue
from zeta.types.errors import ZetaInvalidSymbol, ZetaUnboundSymbol, ZetaNameError
from zeta.types.symbol import Symbol
from zeta.runtime_context import get_current_package
from zeta.package_registry import get_registry


class Environment:
    """Hierarchical mapping from Symbols to Lisp values with package support."""

    __slots__ = (
        "vars",
        "outer",
        "packages",
        "package_aliases",
        "_lookup_cache",
        "_find_cache",
    )

    def __init__(self, outer: Optional[Environment] = None):
        # Runtime environment stores evaluated LispValue(s)
        self.vars: dict[Symbol, LispValue] = {}
        self.outer: Environment | None = outer
        self.packages: dict[str, Environment] = {}
        self.package_aliases: dict[str, str] = {}
        # Simple caches to speed up repeated lookups/find traversals
        self._lookup_cache: dict[Symbol, LispValue] = {}
        self._find_cache: dict[Symbol, Environment] = {}

    def _clear_caches(self) -> None:
        self._lookup_cache.clear()
        self._find_cache.clear()

    def define(self, name: Symbol, value: LispValue) -> None:
        """Bind `name` to `value`.

        Supports package-qualified symbols like pkg:sym. In that case, we resolve
        the package at the root environment (respecting existing aliases) and
        define `sym` inside that package environment. If the package does not exist,
        we create it at the root.

        Raises ZetaInvalidSymbol if `name` is not a Symbol.
        """
        if not isinstance(name, Symbol):
            raise ZetaInvalidSymbol(f"Cannot define {name} as a symbol")

        # Any mutation invalidates caches along this chain (conservatively clear here)
        self._clear_caches()

        s = str(name)
        if ":" in s:
            pkg, sym = s.split(":", 1)
            # climb to root
            root = self
            while root.outer is not None:
                root = root.outer
            # If pkg is an alias, resolve to real package name
            if pkg in root.package_aliases:
                pkg = root.package_aliases[pkg]
            # Ensure package exists, then define inside it
            pkg_env = root.packages.get(pkg)
            if pkg_env is None:
                pkg_env = root.define_package(pkg)
            pkg_env.define(Symbol(sym), value)
            return

        self.vars[name] = value

    def find(self, symbol: Symbol) -> Optional[Environment]:
        """Find the nearest environment in the chain that contains `symbol`."""
        # Cache lookup
        env_cached = self._find_cache.get(symbol)
        if env_cached is not None:
            # ensure still valid (symbol still bound there)
            if symbol in env_cached.vars:
                return env_cached
            else:
                # stale: drop and continue search
                self._find_cache.pop(symbol, None)
        env: Optional[Environment] = self
        while env is not None:
            if symbol in env.vars:
                self._find_cache[symbol] = env
                return env
            env = env.outer
        return None

    def set(self, name: Symbol, value: LispValue) -> None:
        """Update an existing binding for `name` in the environment chain.

        Raises ZetaUnboundSymbol if the symbol is not found.
        """
        env = self.find(name)
        if env is None:
            raise ZetaUnboundSymbol(f"Cannot set unbound symbol {name}")
        env.vars[name] = value
        self._clear_caches()

    def lookup(self, name: Symbol) -> LispValue:
        """Look up the value bound to `name`.

        Order of resolution:
        1) Cache
        2) Qualified pkg:symbol via root.packages (package or alias)
        3) Lexical chain (locals/closures)
        4) Package registry: current package env, then each used package's exports
        Raises ZetaUnboundSymbol if not found.
        """
        # Fast path: cached value
        cached = self._lookup_cache.get(name)
        if cached is not None:
            return cached

        s = str(name)
        if ":" in s:
            pkg, sym = s.split(":", 1)
            # Always resolve against the root environment
            root = self
            while root.outer is not None:
                root = root.outer
            # Resolve aliases at the root
            if pkg in root.package_aliases:
                pkg = root.package_aliases[pkg]
            # Look for a real package by that name
            pkg_env = root.packages.get(pkg)
            if pkg_env:
                val = pkg_env.lookup(Symbol(sym))
                self._lookup_cache[name] = val
                return val
        # 3) Lexical chain first for unqualified names (preserve scoping)
        env = self.find(name)
        if env is not None:
            val = env.vars[name]
            self._lookup_cache[name] = val
            return val
        # 4) Package registry fallback: check current package and its use_list exports
        root = self
        while root.outer is not None:
            root = root.outer
        reg = get_registry()
        current_pkg_name = get_current_package()
        pkg = reg.get(current_pkg_name)
        if pkg is not None:
            # Current package own env first (treat as globals)
            if name in pkg.env.vars:
                val = pkg.env.vars[name]
                self._lookup_cache[name] = val
                return val
            # Then each used package's exported symbols
            for used in pkg.use_list:
                up = reg.get(used)
                if up is None:
                    continue
                if name.id in up.exports:
                    if name in up.env.vars:
                        val = up.env.vars[name]
                    else:
                        # Fallback to env lookup in that package
                        val = up.env.lookup(name)
                    self._lookup_cache[name] = val
                    return val
        # As a last resort, if the symbol exists in a root package env (even without exports), let it be not found.
        raise ZetaUnboundSymbol(f"Cannot lookup unbound symbol {name}")

    def define_package(self, pkg_name: str) -> Environment:
        """Create or return a top-level package environment by name.

        Also ensures an entry exists in the global PackageRegistry for this package,
        pointing at the same Environment.
        """
        # defining packages changes resolution; clear caches at root conservatively
        self._clear_caches()

        env = self
        while env.outer is not None:
            env = env.outer

        if pkg_name not in env.packages:
            env.packages[pkg_name] = Environment()
        # Keep registry in sync
        reg = get_registry()
        reg.ensure(pkg_name, env.packages[pkg_name])
        return env.packages[pkg_name]

    def get_package_symbol(self, pkg_name: str, sym: Symbol) -> LispValue:
        """Look up `sym` inside the named package at the root environment."""
        env = self
        while env.outer is not None:
            env = env.outer
        pkg_env = env.packages.get(pkg_name)
        if pkg_env is None:
            raise ZetaNameError(f"Package '{pkg_name}' not found")
        return pkg_env.lookup(sym)

    def register_package_alias(self, alias: str, pkg_name: str) -> None:
        """Register a short alias for a package name at the root environment."""
        env = self
        while env.outer is not None:
            env = env.outer
        env.package_aliases[alias] = pkg_name
        # alias resolution changed; clear caches
        self._clear_caches()

    def update(self, mapping: dict[Symbol, LispValue]) -> None:
        """Bulk-define a mapping of Symbol -> value in the current frame."""
        # mutation: clear caches
        self._clear_caches()
        for k, v in mapping.items():
            if not isinstance(k, Symbol):
                raise ZetaInvalidSymbol(f"Cannot define {k} as a symbol")
            self.vars[k] = v

    def _write_vars(self, buffer: StringIO) -> None:
        """Write this frame's variables into the buffer in a compact form."""
        buffer.write("{")
        first = True
        for k, v in self.vars.items():
            if not first:
                buffer.write(", ")
            buffer.write(f"{k}: {v!r}")
            first = False
        buffer.write("}")

    def __str__(self) -> str:
        """Human-readable single-frame view with an indicator for parent."""
        with StringIO() as buffer:
            self._write_vars(buffer)
            if self.outer is not None:
                buffer.write(" -> ...")  # indicate parent exists
            return buffer.getvalue()

    def __repr__(self) -> str:
        """Detailed chain representation for debugging purposes."""
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
