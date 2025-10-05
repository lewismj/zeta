from __future__ import annotations

# Public surface for the compiler package
from .opcodes import Opcode
from .chunk import Chunk
from .function import Function, Closure, Cell, UpvalueDescriptor
from .compiler import compile_module, compile_expr

__all__ = [
    "Opcode",
    "Chunk",
    "Function",
    "Closure",
    "Cell",
    "UpvalueDescriptor",
    "compile_module",
    "compile_expr",
]
