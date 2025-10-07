from __future__ import annotations
from typing import Optional

# NOTE: For now this is process-global. If threading is introduced,
# consider switching to contextvars or threading.local.
_current_macros: Optional["MacroEnvironment"] = None
_current_package: str = "user"


def set_current_macros(m: Optional["MacroEnvironment"]) -> None:
    global _current_macros
    _current_macros = m


def get_current_macros() -> Optional["MacroEnvironment"]:
    return _current_macros


def set_current_package(name: str) -> None:
    global _current_package
    _current_package = name


def get_current_package() -> str:
    return _current_package
