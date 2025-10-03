from __future__ import annotations
import sys


class Symbol:
    __slots__ = ("id",)

    def __init__(self, name: str):
        # Intern to ensure fast equality/hash and reduce memory
        self.id = sys.intern(name)

    def __eq__(self, other: Symbol) -> bool:
        return isinstance(other, Symbol) and self.id == other.id

    def __hash__(self) -> int:
        return hash(self.id)

    def __repr__(self):
        return f"Symbol({self.id!r})"

    def __str__(self):
        return self.id
