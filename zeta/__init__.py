# Use Python objects for SExpressions, we're *not* going to define Cons lists etc.
# for something running within Python itself.

from typing import Any, Callable

SExpression = Any
Nil = None
TransformerFunction = Callable[[list[SExpression], 'Environment'], SExpression]