from __future__ import annotations
from typing import Optional

from zeta.types.macro_environment import MacroEnvironment

# NOTE: For now this is process-global. If threading is introduced,
# consider switching to contextvars or threading.local.
_current_macros: Optional[MacroEnvironment] = None


def set_current_macros(m: MacroEnvironment | None) -> None:
    global _current_macros
    _current_macros = m


def get_current_macros() -> Optional[MacroEnvironment]:
    return _current_macros
