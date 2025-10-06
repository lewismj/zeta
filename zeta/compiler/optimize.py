from __future__ import annotations

import os
from typing import Any

from .chunk import Chunk
from .opcodes import Opcode


def optimize_chunk(chunk: Chunk) -> Chunk:
    """
    Minimal optimizer/quickener pass.
    - If ZETA_OPT is falsy, return the chunk unchanged.
    - If enabled, perform a simple quickening: rewrite PUSH_CONST (u16) to
      PUSH_CONST_Q (u16) and place the referenced constant into chunk.quick_imms.
    This establishes the infrastructure for future passes while being semantics-preserving.
    """
    opt_flag = os.getenv("ZETA_OPT", "0")
    if not opt_flag or opt_flag in ("0", "false", "False", "no", "off"):
        return chunk

    code = chunk.code
    consts = chunk.constants

    # Build mapping from constant index to quick_imms slot to avoid duplicates.
    qmap: dict[int, int] = {}
    quick_imms: list[Any] = list(getattr(chunk, "quick_imms", []) or [])

    i = 0
    # Helper readers
    def u16(ix: int) -> int:
        return (code[ix] << 8) | code[ix + 1]

    while i < len(code):
        op = code[i]
        i += 1
        if op in (Opcode.PUSH_CONST,):
            idx = u16(i)
            i += 2
            # Install in quick_imms table
            if idx not in qmap:
                qmap[idx] = len(quick_imms)
                quick_imms.append(consts[idx])
            qidx = qmap[idx]
            # Rewrite in place to quickened variant
            code[i - 3] = int(Opcode.PUSH_CONST_Q)
            code[i - 2] = (qidx >> 8) & 0xFF
            code[i - 1] = qidx & 0xFF
        elif op in (Opcode.LOAD_LOCAL,):
            idx = u16(i)
            i += 2
            code[i - 3] = int(Opcode.LOAD_LOCAL_Q)
        elif op in (Opcode.STORE_LOCAL,):
            idx = u16(i)
            i += 2
            code[i - 3] = int(Opcode.STORE_LOCAL_Q)
        elif op in (Opcode.LOAD_UPVALUE,):
            uidx = u16(i)
            i += 2
            code[i - 3] = int(Opcode.LOAD_UPVALUE_Q)
        elif op in (Opcode.STORE_UPVALUE,):
            uidx = u16(i)
            i += 2
            code[i - 3] = int(Opcode.STORE_UPVALUE_Q)
        elif op in (Opcode.JUMP,):
            # s16
            i += 2
            code[i - 3] = int(Opcode.JUMP_Q)
        elif op in (Opcode.LOAD_GLOBAL, Opcode.STORE_GLOBAL,
                    Opcode.MAKE_LAMBDA, Opcode.LOAD_ATTR, Opcode.STORE_ATTR,
                    Opcode.INVOKE):
            # Skip u16
            i += 2
        elif op in (Opcode.MAKE_CLOSURE,):
            fidx = u16(i); i += 2
            ucount = code[i]; i += 1
            for _ in range(ucount):
                # is_local (u8), index (u16)
                i += 1
                i += 2
        elif op in (Opcode.JUMP_IF_TRUE, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_NIL):
            # s16
            i += 2
        elif op in (Opcode.PUSH_INT,):
            # i32
            i += 4
        elif op in (Opcode.CALL, Opcode.TAILCALL, Opcode.CALL_VAR, Opcode.CALL_KW, Opcode.APPLY, Opcode.LIST):
            argc = code[i]; i += 1
            if op == Opcode.CALL_KW:
                i += 1  # kwc
        else:
            # No operands
            pass

    # Save quick_imms table back to chunk
    chunk.quick_imms = quick_imms
    return chunk
