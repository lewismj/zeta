from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, List

from zeta.compiler.opcodes import Opcode


@dataclass
class Chunk:
    """A chunk of bytecode with a constants table and optional line info.

    For simplicity we use fixed-size operands: u8, u16, s16, u32, s32 as raw bytes.
    """

    code: bytearray = field(default_factory=bytearray)
    constants: List[Any] = field(default_factory=list)
    lines: list[tuple[int, int, int]] = field(default_factory=list)  # (ip, line, col)
    # Quickening/IC support (optional). Optimizer may fill these.
    quick_imms: list[Any] = field(default_factory=list)
    inline_caches: list[Any] = field(default_factory=list)

    def add_const(self, value: Any) -> int:
        try:
            idx = self.constants.index(value)
            return idx
        except ValueError:
            self.constants.append(value)
            return len(self.constants) - 1

    # --- Emit helpers ---
    def emit_op(self, op: Opcode) -> int:
        self.code.append(int(op))
        return len(self.code) - 1

    def emit_u8(self, v: int) -> None:
        self.code.append(v & 0xFF)

    def emit_u16(self, v: int) -> None:
        self.code.extend(((v >> 8) & 0xFF, v & 0xFF))

    def emit_s16(self, v: int) -> None:
        if v < 0:
            v = (1 << 16) + v
        self.emit_u16(v)

    def emit_u32(self, v: int) -> None:
        self.code.extend(((v >> 24) & 0xFF, (v >> 16) & 0xFF, (v >> 8) & 0xFF, v & 0xFF))

    def emit_s32(self, v: int) -> None:
        if v < 0:
            v = (1 << 32) + v
        self.emit_u32(v)

    # --- high-level convenience ---
    def emit_const(self, value: Any) -> None:
        idx = self.add_const(value)
        self.emit_op(Opcode.PUSH_CONST)
        self.emit_u16(idx)

    def patch_s16_at(self, ip: int, rel: int) -> None:
        # ip points to the first byte after the opcode
        if rel < 0:
            rel = (1 << 16) + rel
        self.code[ip] = (rel >> 8) & 0xFF
        self.code[ip + 1] = rel & 0xFF
