# Use Python objects for SExpressions, we're *not* going to define Cons lists etc.
# for something running within Python itself.

from typing import Any, Callable


SExpression = Any

class NilType:
    def __repr__(self): return "nil"
    def __bool__(self): return False

Nil = NilType()

TransformerFunction = Callable[[list[SExpression], 'Environment'], SExpression]

