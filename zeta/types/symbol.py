from __future__ import annotations

import sys

class Symbol(str):
    def __new__(cls, name: str) -> str:
        return str.__new__(cls, sys.intern(name))
