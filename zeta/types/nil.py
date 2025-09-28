from __future__ import annotations


class NilType:
    def __repr__(self): return "nil"
    def __bool__(self): return False

    # Comparisons: Nil is always less than numbers, and equal only to Nil
    def __eq__(self, other):
        return isinstance(other, NilType)

    def __lt__(self, other):
        return not isinstance(other, NilType)

    def __le__(self, other):
        return True  # Nil <= anything

    def __gt__(self, other):
        return False

    def __ge__(self, other):
        return isinstance(other, NilType)


Nil = NilType()
