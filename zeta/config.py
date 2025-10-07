from __future__ import annotations
import os
from pathlib import Path
from typing import Iterable, List


def _sep() -> str:
    return ';' if os.name == 'nt' else ':'


# Resolve installation dir (zeta package directory)
_ZETA_DIR = Path(__file__).resolve().parent

# Defaults
_DEFAULT_PACKAGES_DIRS = [_ZETA_DIR / 'packages']
_DEFAULT_PRELUDE_DIR = _ZETA_DIR / 'prelude'


def paths_from_env(var: str, defaults: Iterable[Path]) -> List[Path]:
    raw = os.environ.get(var)
    if not raw:
        return [Path(p) for p in defaults]
    sep = _sep()
    return [Path(p.strip()) for p in raw.split(sep) if p.strip()]


def get_packages_roots() -> List[Path]:
    return paths_from_env('ZETA_PACKAGES_PATH', _DEFAULT_PACKAGES_DIRS)


def get_prelude_root() -> Path:
    roots = paths_from_env('ZETA_PRELUDE_PATH', [_DEFAULT_PRELUDE_DIR])
    # treat as single directory; if a file path is set, return its parent
    p = roots[0]
    return p if p.is_dir() else p.parent
