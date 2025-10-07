from __future__ import annotations
from pathlib import Path
from typing import Optional, Protocol, TYPE_CHECKING

from zeta.config import get_packages_roots, get_prelude_root


class _HasEvalPrelude(Protocol):
    def eval_prelude(self, code: str) -> None: ...


# Map a dotted namespace to a .lisp file underneath a set of roots

def _ns_to_relpath(namespace: str) -> Path:
    return Path(*namespace.split('.')).with_suffix('.lisp')


def resolve_namespace(namespace: str) -> Optional[Path]:
    rel = _ns_to_relpath(namespace)
    for root in get_packages_roots():
        candidate = root / rel
        if candidate.is_file():
            return candidate
    return None


def load_namespace(itp: _HasEvalPrelude, namespace: str) -> None:
    p = resolve_namespace(namespace)
    if p is None:
        raise FileNotFoundError(f"Cannot find package '{namespace}' in ZETA_PACKAGES_PATH")
    code = p.read_text(encoding='utf-8')
    # Expect files to carry proper `(defpackage ...)(in-package ...)` headers.
    itp.eval_prelude(code)


# Prelude convenience loader (std.core then std.test)

def load_prelude(itp: _HasEvalPrelude) -> None:
    root = get_prelude_root()
    std_core = root / 'std' / 'core.lisp'
    std_test = root / 'std' / 'test.lisp'

    if std_core.exists():
        itp.eval_prelude(std_core.read_text(encoding='utf-8'))
        if std_test.exists():
            itp.eval_prelude(std_test.read_text(encoding='utf-8'))
        return

    # Back-compat: old layout had prelude\std.lisp and prelude\test.lisp at root
    legacy_std = root / 'std.lisp'
    legacy_test = root / 'test.lisp'
    if legacy_std.exists():
        itp.eval_prelude(legacy_std.read_text(encoding='utf-8'))
    if legacy_test.exists():
        itp.eval_prelude(legacy_test.read_text(encoding='utf-8'))
