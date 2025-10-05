from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, List, Optional


@dataclass
class UpvalueDescriptor:
    is_local: bool
    index: int


@dataclass
class Function:
    chunk: Any  # Chunk
    arity: int = 0
    rest: bool = False
    kw_params: Optional[List[str]] = None
    local_slots: int = 0
    max_stack: int = 0
    upvalues: List[UpvalueDescriptor] = field(default_factory=list)
    name: str | None = None


@dataclass
class Cell:
    value: Any


@dataclass
class Closure:
    fn: Function
    upvalues: List[Cell]

    def __call__(self, *args, **kwargs):  # convenience for host calls
        raise TypeError("Closure objects are not directly callable by Python; use VM CALL")
