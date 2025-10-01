# Core type aliases for Zeta's data model.
# We use plain Python types (int, float, str, list, tuple-for-dotted-lists, dict, etc.)
# to represent both code (forms) and runtime values. No explicit Cons type is defined.
#
# Naming guidance:
# - SExpression: Use in reader/parser/macro code to denote syntactic forms (code-as-data).
# - LispValue:  Use in evaluator/runtime code to denote evaluated values.
# Both aliases currently resolve to `Any` for flexibility, and are interchangeable. Keeping
# SExpression preserves backward compatibility with existing imports and annotations.

from typing import Any, Callable

# Runtime value alias
LispValue = Any
# Back-compat: forms alias (often used interchangeably in this codebase)
SExpression = LispValue

# Evaluator function type: Python evaluator used inside special forms/macros
EvaluatorFn = Callable[..., LispValue]


