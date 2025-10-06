from __future__ import annotations
from typing import Any

from .chunk import Chunk
from .opcodes import Opcode
from .function import Function


def disassemble_chunk(chunk: Chunk) -> str:
    code = chunk.code
    consts = chunk.constants
    out = []
    i = 0
    def u8(ix):
        return code[ix]
    def u16(ix):
        return (code[ix] << 8) | code[ix+1]
    def s16(ix):
        v = u16(ix)
        return v - (1 << 16) if v & 0x8000 else v
    def i32(ix):
        v = (code[ix] << 24) | (code[ix+1] << 16) | (code[ix+2] << 8) | code[ix+3]
        return v - (1 << 32) if v & 0x80000000 else v
    while i < len(code):
        op = code[i]
        try:
            opname = Opcode(op).name
        except Exception:
            opname = f"OP_{op:02X}"
        line = f"{i:04d}: {opname}"
        i += 1
        if op in (Opcode.PUSH_CONST, Opcode.LOAD_GLOBAL, Opcode.STORE_GLOBAL,
                  Opcode.LOAD_LOCAL, Opcode.STORE_LOCAL,
                  Opcode.LOAD_UPVALUE, Opcode.STORE_UPVALUE,
                  Opcode.MAKE_LAMBDA, Opcode.MAKE_CLOSURE, Opcode.LOAD_ATTR, Opcode.STORE_ATTR,
                  Opcode.INVOKE):
            if op == Opcode.MAKE_CLOSURE:
                fidx = u16(i); i += 2
                ucount = u8(i); i += 1
                line += f" {fidx} (ucount={ucount})"
                for _ in range(ucount):
                    is_local = u8(i); i += 1
                    idx = u16(i); i += 2
                    line += f" [{'L' if is_local else 'U'}:{idx}]"
            elif op == Opcode.INVOKE:
                mname_idx = u16(i); i += 2
                argc = u8(i); i += 1
                line += f" mconst={mname_idx} argc={argc}"
            else:
                val = u16(i); i += 2
                line += f" {val}"
        elif op in (Opcode.JUMP, Opcode.JUMP_IF_TRUE, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_NIL):
            rel = s16(i); i += 2
            line += f" {rel:+d} -> {i + rel}"
        elif op in (Opcode.PUSH_INT,):
            val = i32(i); i += 4
            line += f" {val}"
        elif op in (Opcode.CALL, Opcode.TAILCALL, Opcode.CALL_VAR, Opcode.CALL_KW, Opcode.APPLY, Opcode.LIST):
            argc = u8(i); i += 1
            line += f" argc={argc}"
            if op == Opcode.CALL_KW:
                kwc = u8(i); i += 1
                line += f" kwc={kwc}"
        out.append(line)
    # Append constants info
    out.append("-- constants --")
    for idx, c in enumerate(consts):
        if isinstance(c, Function):
            out.append(f"[{idx}] <Function arity={c.arity} rest={c.rest} upvalues={len(c.upvalues)}>")
            out.append(disassemble_chunk(c.chunk))
        else:
            out.append(f"[{idx}] {repr(c)}")
    return "\n".join(out)
