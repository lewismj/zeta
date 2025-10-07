from __future__ import annotations

from typing import List

from zeta import EvaluatorFn, LispValue, SExpression
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil
from zeta.types.errors import ZetaArityError, ZetaNameError, ZetaTypeError
from zeta.package_registry import get_registry
from zeta.runtime_context import set_current_package, get_current_package


def _to_name(x) -> str:
    if isinstance(x, Symbol):
        return x.id
    if isinstance(x, str):
        return x
    raise ZetaTypeError(f"Expected package or symbol name, got: {x!r}")


def defpackage_form(
    tail: List[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    is_tail: bool,
) -> LispValue:
    if not tail:
        raise ZetaArityError("defpackage requires at least a name")
    name_expr, *rest = tail
    if not isinstance(name_expr, Symbol):
        raise ZetaTypeError("defpackage name must be a symbol")
    pkg_name = name_expr.id
    # Make sure an env exists for this package and in the registry
    pkg_env = env.define_package(pkg_name)
    reg = get_registry()
    pkg = reg.ensure(pkg_name, pkg_env)

    # Parse keyword options: :use, :nicknames, :export
    i = 0
    while i < len(rest):
        key = rest[i]
        val = rest[i + 1] if i + 1 < len(rest) else None
        if isinstance(key, Symbol) and key.id == ":use":
            if not isinstance(val, list):
                raise ZetaTypeError(":use expects a list of package symbols")
            for p in val:
                pkg.uses(_to_name(p))
        elif isinstance(key, Symbol) and key.id == ":nicknames":
            if not isinstance(val, list):
                raise ZetaTypeError(":nicknames expects a list of symbols")
            # Register aliases in both the registry and root env alias table
            for alias in val:
                alias_name = _to_name(alias)
                pkg.aliases.add(alias_name)
                reg.set_alias(alias_name, pkg_name)
                # also for resolve_object_path which reads root.package_aliases
                env.register_package_alias(alias_name, pkg_name)
        elif isinstance(key, Symbol) and key.id == ":export":
            if not isinstance(val, list):
                raise ZetaTypeError(":export expects a list of symbols")
            for s in val:
                if not isinstance(s, Symbol):
                    raise ZetaTypeError("export list must contain symbols")
                pkg.export(s)
        else:
            # Unknown option: ignore for forward compatibility
            pass
        i += 2

    return Nil


def in_package_form(
    tail: List[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    is_tail: bool,
) -> LispValue:
    if len(tail) != 1:
        raise ZetaArityError("in-package requires exactly 1 argument")
    name_expr = tail[0]
    if not isinstance(name_expr, Symbol):
        raise ZetaTypeError("in-package name must be a symbol")
    pkg_name = name_expr.id
    env.define_package(pkg_name)  # ensure exists and in registry
    set_current_package(pkg_name)
    return Nil


def export_form(
    tail: List[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    is_tail: bool,
) -> LispValue:
    if not tail:
        raise ZetaArityError("export requires at least one symbol or a list")
    # Support (export (a b)) and (export a b)
    syms: List[Symbol] = []
    if len(tail) == 1 and isinstance(tail[0], list):
        for s in tail[0]:
            if not isinstance(s, Symbol):
                raise ZetaTypeError("export list must contain symbols")
            syms.append(s)
    else:
        for s in tail:
            if not isinstance(s, Symbol):
                raise ZetaTypeError("export arguments must be symbols")
            syms.append(s)
    reg = get_registry()
    current = reg.get(get_current_package())
    if current is None:
        raise ZetaNameError("No current package is set")
    current.export(*syms)
    # Invalidate caches conservatively
    env._clear_caches()
    return Nil


def use_package_form(
    tail: List[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    is_tail: bool,
) -> LispValue:
    if len(tail) < 1:
        raise ZetaArityError("use-package requires a package name")
    name_expr = tail[0]
    pkg_name = _to_name(name_expr)
    reg = get_registry()
    current = reg.get(get_current_package())
    if current is None:
        raise ZetaNameError("No current package is set")
    # Ensure the used package exists too
    env.define_package(pkg_name)
    current.uses(pkg_name)
    env._clear_caches()
    return Nil


def from_form(
    tail: List[SExpression],
    env: Environment,
    macros: MacroEnvironment,
    evaluate_fn: EvaluatorFn,
    is_tail: bool,
) -> LispValue:
    if len(tail) != 2:
        raise ZetaArityError("from requires exactly 2 arguments: package and symbol")
    pkg_expr, sym_expr = tail
    pkg_name = _to_name(pkg_expr)
    if not isinstance(sym_expr, Symbol):
        raise ZetaTypeError("from second argument must be a symbol")
    reg = get_registry()
    pkg = reg.get(pkg_name)
    if pkg is None:
        raise ZetaNameError(f"Package '{pkg_name}' not found")
    # Only allow access to exported symbols
    if sym_expr.id not in pkg.exports:
        raise ZetaNameError(f"Symbol {sym_expr} is not exported from package '{pkg_name}'")
    # Fetch binding from the package env
    if sym_expr in pkg.env.vars:
        return pkg.env.vars[sym_expr]
    # Fallback to a lookup inside that env (should resolve upvalues if ever added)
    return pkg.env.lookup(sym_expr)
