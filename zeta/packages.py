# Backward-compatibility shim. Deprecated: use zeta.modules.python_loader instead.
from zeta.modules.python_loader import import_module, import_helpers_module  # re-export

__all__ = [
    "import_module",
    "import_helpers_module",
]
