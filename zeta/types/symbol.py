from __future__ import annotations

import sys

# class Symbol(str):
#     def __new__(cls, name: str) -> str:
#         return str.__new__(cls, sys.intern(name))

class Symbol:
    def __init__(self, name: str):
        self.id = sys.intern(name)

    def __eq__(self, other: Symbol) -> bool:
        return isinstance(other, Symbol) and self.id == other.id

    def __hash__(self) -> int:
        return hash(self.id)

    def __repr__(self):
        return f"Symbol({self.id!r})"

    def __str__(self):
        return self.id


