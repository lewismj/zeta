from __future__ import annotations

import os
from typing import Any

from .chunk import Chunk
from .opcodes import Opcode

# --- Helpers for instruction decoding/stepping ---

def _u16(code: list[int], ix: int) -> int:
    return (code[ix] << 8) | code[ix + 1]


def _advance_index(code: list[int], i: int, op: int) -> int:
    # i currently points just after the opcode byte
    if op in (Opcode.PUSH_CONST, Opcode.LOAD_LOCAL, Opcode.STORE_LOCAL,
              Opcode.LOAD_GLOBAL, Opcode.STORE_GLOBAL,
              Opcode.LOAD_UPVALUE, Opcode.STORE_UPVALUE,
              Opcode.MAKE_LAMBDA, Opcode.LOAD_ATTR, Opcode.STORE_ATTR,
              Opcode.INVOKE):
        # u16 (and INVOKE also has an extra u8 argc we handle below)
        i += 2
        if op == Opcode.INVOKE:
            i += 1  # argc
        return i
    if op in (Opcode.JUMP, Opcode.JUMP_IF_TRUE, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_NIL,
              Opcode.SETUP_CATCH):
        # s16
        return i + 2
    if op in (Opcode.PUSH_INT,):
        # i32
        return i + 4
    if op in (Opcode.CALL, Opcode.TAILCALL, Opcode.CALL_VAR, Opcode.CALL_KW, Opcode.APPLY, Opcode.LIST):
        # u8 argc (CALL_KW has extra u8 kwc)
        i += 1
        if op == Opcode.CALL_KW:
            i += 1
        return i
    if op in (Opcode.MAKE_CLOSURE,):
        fidx = _u16(code, i); i += 2
        ucount = code[i]; i += 1
        for _ in range(ucount):
            # is_local (u8), index (u16)
            i += 1
            i += 2
        return i
    # Quickened variants keep same operand sizes as their base
    if op in (Opcode.PUSH_CONST_Q, Opcode.LOAD_LOCAL_Q, Opcode.STORE_LOCAL_Q,
              Opcode.LOAD_UPVALUE_Q, Opcode.STORE_UPVALUE_Q, Opcode.JUMP_Q):
        # All of these carry a u16 immediate
        return i + 2
    # All others: no operands
    return i


# --- Individual optimization passes ---

def _opt_quicken_push_const(chunk: Chunk) -> None:
    code = chunk.code
    consts = chunk.constants
    qmap: dict[int, int] = {}
    quick_imms: list[Any] = list(getattr(chunk, "quick_imms", []) or [])

    i = 0
    while i < len(code):
        op = code[i]
        ni = i + 1
        if op == Opcode.PUSH_CONST:
            idx = _u16(code, ni)
            # Install in quick_imms table
            if idx not in qmap:
                qmap[idx] = len(quick_imms)
                quick_imms.append(consts[idx])
            qidx = qmap[idx]
            # Rewrite in place to quickened variant and patch immediate
            code[i] = int(Opcode.PUSH_CONST_Q)
            code[ni] = (qidx >> 8) & 0xFF
            code[ni + 1] = qidx & 0xFF
        i = _advance_index(code, ni, op)

    chunk.quick_imms = quick_imms


def _opt_quicken_locals(chunk: Chunk) -> None:
    code = chunk.code
    i = 0
    while i < len(code):
        op = code[i]
        ni = i + 1
        if op == Opcode.LOAD_LOCAL:
            code[i] = int(Opcode.LOAD_LOCAL_Q)
        elif op == Opcode.STORE_LOCAL:
            code[i] = int(Opcode.STORE_LOCAL_Q)
        i = _advance_index(code, ni, op)


def _opt_quicken_upvalues(chunk: Chunk) -> None:
    code = chunk.code
    i = 0
    while i < len(code):
        op = code[i]
        ni = i + 1
        if op == Opcode.LOAD_UPVALUE:
            code[i] = int(Opcode.LOAD_UPVALUE_Q)
        elif op == Opcode.STORE_UPVALUE:
            code[i] = int(Opcode.STORE_UPVALUE_Q)
        i = _advance_index(code, ni, op)


def _opt_quicken_jumps(chunk: Chunk) -> None:
    code = chunk.code
    i = 0
    while i < len(code):
        op = code[i]
        ni = i + 1
        if op == Opcode.JUMP:
            code[i] = int(Opcode.JUMP_Q)
        i = _advance_index(code, ni, op)


# --- Top-level optimizer entry point ---

def quicken_opcodes(chunk: Chunk) -> Chunk:
    """
    Grouped quickening optimizations.
    """
    _opt_quicken_push_const(chunk)
    _opt_quicken_locals(chunk)
    _opt_quicken_upvalues(chunk)
    _opt_quicken_jumps(chunk)
    return chunk


# --- Decoding/assembly helpers for structural passes ---

def _s16(code: list[int], ix: int) -> int:
    v = (code[ix] << 8) | code[ix + 1]
    return v - 0x10000 if v & 0x8000 else v


def _u32(code: list[int], ix: int) -> int:
    return (code[ix] << 24) | (code[ix + 1] << 16) | (code[ix + 2] << 8) | code[ix + 3]


def _s32(code: list[int], ix: int) -> int:
    v = _u32(code, ix)
    return v - 0x1_0000_0000 if v & 0x8000_0000 else v


def _decode_instructions(code: bytearray) -> list[dict[str, Any]]:
    ins: list[dict[str, Any]] = []
    i = 0
    while i < len(code):
        op = code[i]
        ni = i + 1
        next_i = _advance_index(code, ni, op)
        operands = list(code[ni:next_i])
        size = 1 + (next_i - ni)
        item: dict[str, Any] = {"op": op, "start": i, "size": size, "operands": operands}
        if op in (Opcode.JUMP, Opcode.JUMP_Q, Opcode.JUMP_IF_TRUE, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_NIL, Opcode.SETUP_CATCH):
            rel = _s16(code, ni)
            target = next_i + rel
            item["is_jump"] = True
            item["target"] = target
        else:
            item["is_jump"] = False
        ins.append(item)
        i = next_i
    return ins


def _assemble_instructions(chunk: Chunk, ins: list[dict[str, Any]]) -> None:
    # Map old starts to new starts
    new_code = bytearray()
    old_to_new: dict[int, int] = {}
    # First pass: write op + operands (placeholders for jumps) and remember new positions
    for it in ins:
        old_to_new[it["start"]] = len(new_code)
        op = it["op"]
        new_code.append(int(op))
        # For jumps, we'll rewrite operand later, but keep 2-byte placeholder
        if op in (Opcode.JUMP, Opcode.JUMP_Q, Opcode.JUMP_IF_TRUE, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_NIL, Opcode.SETUP_CATCH):
            new_code.extend((0, 0))
        else:
            ops = it.get("operands", [])
            new_code.extend(ops)
    # Second pass: patch jumps with new relative offsets
    for it in ins:
        op = it["op"]
        if op in (Opcode.JUMP, Opcode.JUMP_Q, Opcode.JUMP_IF_TRUE, Opcode.JUMP_IF_FALSE, Opcode.JUMP_IF_NIL, Opcode.SETUP_CATCH):
            old_start = it["start"]
            new_start = old_to_new[old_start]
            size = 3  # opcode + s16
            base = new_start + size
            target_old = it.get("target")
            if target_old is None or target_old not in old_to_new:
                # If target was removed, convert to NOP
                new_code[new_start] = int(Opcode.NOP)
                new_code[new_start + 1] = 0
                new_code[new_start + 2] = 0
                continue
            target_new = old_to_new[target_old]
            rel = target_new - base
            rel_enc = rel & 0xFFFF
            new_code[new_start + 1] = (rel_enc >> 8) & 0xFF
            new_code[new_start + 2] = rel_enc & 0xFF
    chunk.code = new_code


def _is_const_push(op: int) -> bool:
    return op in (Opcode.PUSH_CONST, Opcode.PUSH_CONST_Q, Opcode.PUSH_TRUE, Opcode.PUSH_FALSE, Opcode.PUSH_NIL, Opcode.PUSH_INT)


def _const_value_from_instr(it: dict[str, Any], chunk: Chunk) -> tuple[bool, Any]:
    op = it["op"]
    ops = it.get("operands", [])
    if op == Opcode.PUSH_CONST:
        idx = (ops[0] << 8) | ops[1]
        return True, chunk.constants[idx]
    if op == Opcode.PUSH_CONST_Q:
        idx = (ops[0] << 8) | ops[1]
        qi = getattr(chunk, "quick_imms", [])
        if 0 <= idx < len(qi):
            return True, qi[idx]
        return False, None
    if op == Opcode.PUSH_TRUE:
        return True, True
    if op == Opcode.PUSH_FALSE:
        return True, False
    if op == Opcode.PUSH_NIL:
        from zeta.types.nil import Nil
        return True, Nil
    if op == Opcode.PUSH_INT:
        # operands are 4 bytes
        b = ops
        if len(b) == 4:
            v = _s32(b, 0) if isinstance(b, (list, bytearray)) else None
            return (v is not None), v
        return False, None
    return False, None


# --- Additional optimization passes ---

__inline_threshold = 8

def inline_simple_functions(chunk: Chunk) -> Chunk:
    """
    Inline very small, argument-free lambdas at call sites.

    Conservative MVP: only inline MAKE_LAMBDA fidx immediately followed by
    CALL/TAILCALL with argc == 0, when the callee Function has:
      - no upvalues
      - arity == 0, rest == False, no kw params
      - straight-line code (no jumps/handlers) ending with RETURN
      - total code size <= __inline_threshold

    Constants referenced by the callee are remapped into the caller's
    constants table.
    """
    from .function import Function

    ins = _decode_instructions(chunk.code)

    def _is_straight_line(fn: Function) -> bool:
        sub = _decode_instructions(fn.chunk.code)
        if not sub:
            return True
        # must end with RETURN or HALT
        if sub[-1]["op"] not in (Opcode.RETURN, Opcode.HALT):
            return False
        # no jumps/handlers in body
        if any(it.get("is_jump") for it in sub):
            return False
        # No local/upvalue accesses (since we only inline arity-0, there should be none)
        banned = {Opcode.LOAD_LOCAL, Opcode.STORE_LOCAL, Opcode.LOAD_LOCAL_Q, Opcode.STORE_LOCAL_Q,
                  Opcode.LOAD_UPVALUE, Opcode.STORE_UPVALUE, Opcode.LOAD_UPVALUE_Q, Opcode.STORE_UPVALUE_Q,
                  Opcode.CLOSE_UPVALUE, Opcode.MAKE_CLOSURE, Opcode.MAKE_LAMBDA,
                  Opcode.CALL, Opcode.CALL_VAR, Opcode.CALL_KW, Opcode.APPLY, Opcode.TAILCALL,
                  Opcode.SETUP_CATCH, Opcode.POP_CATCH, Opcode.THROW,
                  Opcode.CAPTURE_CONT, Opcode.RESUME_CONT, Opcode.CALL_CC}
        return not any(it["op"] in banned for it in sub)

    def _remap_and_copy(fn: Function) -> list[dict[str, Any]]:
        sub = _decode_instructions(fn.chunk.code)
        out: list[dict[str, Any]] = []
        # Drop trailing RETURN/HALT
        if sub and sub[-1]["op"] in (Opcode.RETURN, Opcode.HALT):
            sub = sub[:-1]
        # Unique negative starts so assembler can map without clashes
        neg_start = -1
        def _u16_ops_to_const_index(ops: list[int]) -> int:
            return (ops[0] << 8) | ops[1]
        for it in sub:
            op = it["op"]
            ops = list(it.get("operands", []))
            # Remap constant-indexed operands from callee chunk to caller chunk
            if op in (Opcode.PUSH_CONST, Opcode.LOAD_GLOBAL, Opcode.STORE_GLOBAL,
                      Opcode.INTERN_SYM, Opcode.LOAD_ATTR, Opcode.STORE_ATTR, Opcode.INVOKE):
                idx = _u16_ops_to_const_index(ops)
                # Fetch value from callee constants
                try:
                    val = fn.chunk.constants[idx]
                except Exception:
                    val = None
                new_idx = chunk.add_const(val)
                ops[0] = (new_idx >> 8) & 0xFF
                ops[1] = new_idx & 0xFF
            # Quickened consts are not expected in function chunks; ignore if present
            out.append({
                "op": op,
                "start": neg_start,
                "size": it.get("size", 1 + len(ops)),
                "operands": ops,
                "is_jump": False,
            })
            neg_start -= 1
        return out

    out: list[dict[str, Any]] = []
    i = 0
    n = len(ins)
    while i < n:
        it = ins[i]
        op = it["op"]
        if op == Opcode.MAKE_LAMBDA and (i + 1) < n:
            # Next must be a CALL/TAILCALL with argc 0
            next_it = ins[i + 1]
            if next_it["op"] in (Opcode.CALL, Opcode.TAILCALL) and next_it.get("operands", [255])[0] == 0:
                fidx = (it["operands"][0] << 8) | it["operands"][1]
                # Resolve function from caller's constants
                if 0 <= fidx < len(chunk.constants):
                    fn_obj = chunk.constants[fidx]
                    if isinstance(fn_obj, Function):
                        fn = fn_obj
                        # Threshold and shape checks
                        if len(fn.chunk.code) <= __inline_threshold and not fn.rest and not fn.kw_params and fn.arity == 0 and not fn.upvalues and _is_straight_line(fn):
                            # Inline callee body
                            body_ins = _remap_and_copy(fn)
                            out.extend(body_ins)
                            # Skip MAKE_LAMBDA and CALL
                            i += 2
                            continue
        out.append(it)
        i += 1

    _assemble_instructions(chunk, out)
    return chunk


def peephole_optimize(chunk: Chunk) -> Chunk:
    """Apply small local rewrites that don't need global analysis.
    Patterns:
    - const followed by POP => remove both
    - DUP followed by POP => both removed
    - SWAP followed by SWAP => both removed
    """
    code = chunk.code
    ins = _decode_instructions(code)
    # Collect jump targets to avoid removing targeted instructions
    targets = {it["target"] for it in ins if it.get("is_jump")}
    out: list[dict[str, Any]] = []
    i = 0
    n = len(ins)
    while i < n:
        it = ins[i]
        op = it["op"]
        # const; POP
        if i + 1 < n and it["start"] not in targets:
            nxt = ins[i + 1]
            if _is_const_push(op) and nxt["op"] == Opcode.POP:
                i += 2
                continue
            if op == Opcode.DUP and nxt["op"] == Opcode.POP:
                i += 2
                continue
            if op == Opcode.SWAP and nxt["op"] == Opcode.SWAP and it["start"] not in targets and nxt["start"] not in targets:
                i += 2
                continue
        out.append(it)
        i += 1
    _assemble_instructions(chunk, out)
    return chunk


def constant_folding(chunk: Chunk) -> Chunk:
    """Fold operations on literal constants where safe.
    Supports binary ops: ADD,SUB,MUL,DIV,MOD,LT,LE,GT,GE,EQ,NEQ,AND,OR
    and unary op: NOT (on booleans).
    """
    ins = _decode_instructions(chunk.code)
    targets = {it["target"] for it in ins if it.get("is_jump")}
    out: list[dict[str, Any]] = []
    i = 0
    def make_push_const(val: Any) -> dict[str, Any]:
        idx = chunk.add_const(val)
        return {"op": int(Opcode.PUSH_CONST), "start": -1, "size": 3, "operands": [(idx >> 8) & 0xFF, idx & 0xFF], "is_jump": False}
    def _raise():
        raise ValueError
    bin_ops = {
        Opcode.ADD: lambda a, b: a + b,
        Opcode.SUB: lambda a, b: a - b,
        Opcode.MUL: lambda a, b: a * b,
        Opcode.DIV: lambda a, b: a / b,
        Opcode.MOD: lambda a, b: a % b,
        Opcode.LT: lambda a, b: a < b,
        Opcode.LE: lambda a, b: a <= b,
        Opcode.GT: lambda a, b: a > b,
        Opcode.GE: lambda a, b: a >= b,
        Opcode.EQ: lambda a, b: a == b,
        Opcode.NEQ: lambda a, b: a != b,
        Opcode.AND: lambda a, b: (a and b) if isinstance(a, bool) and isinstance(b, bool) else _raise(),
        Opcode.OR: lambda a, b: (a or b) if isinstance(a, bool) and isinstance(b, bool) else _raise(),
    }
    while i < len(ins):
        it = ins[i]
        op = it["op"]
        # Unary NOT folding
        if op == Opcode.NOT and len(out) >= 1:
            prev = out[-1]
            if prev["start"] not in targets:
                ok, v = _const_value_from_instr(prev, chunk)
                if ok and isinstance(v, bool):
                    out.pop()
                    out.append(make_push_const(not v))
                    i += 1
                    continue
        # Binary fold: need two preceding const pushes in output stream
        if op in bin_ops and len(out) >= 2:
            a_it = out[-2]
            b_it = out[-1]
            if a_it["start"] not in targets and b_it["start"] not in targets:
                ok1, a = _const_value_from_instr(a_it, chunk)
                ok2, b = _const_value_from_instr(b_it, chunk)
                if ok1 and ok2:
                    try:
                        res = bin_ops[op](a, b)
                        # Replace last two and current op with single const
                        out.pop(); out.pop()
                        out.append(make_push_const(res))
                        i += 1
                        continue
                    except Exception:
                        pass
        out.append(it)
        i += 1
    _assemble_instructions(chunk, out)
    return chunk


def dead_code_elimination(chunk: Chunk) -> Chunk:
    """Remove code that cannot be reached due to prior RETURN/HALT/JUMP,
    stopping at the next existing jump target. Never removes instructions that
    are jump targets.
    """
    ins = _decode_instructions(chunk.code)
    targets = {it["target"] for it in ins if it.get("is_jump")}
    out: list[dict[str, Any]] = []
    unreachable = False
    for it in ins:
        start = it["start"]
        if start in targets:
            unreachable = False
        if unreachable:
            # skip unreachable insns
            continue
        out.append(it)
        if it["op"] in (Opcode.RETURN, Opcode.HALT, Opcode.JUMP, Opcode.JUMP_Q):
            unreachable = True
    _assemble_instructions(chunk, out)
    return chunk


def jump_target_shortening(chunk: Chunk) -> Chunk:
    """Reassemble code to recompute jump relative offsets after size changes."""
    ins = _decode_instructions(chunk.code)
    _assemble_instructions(chunk, ins)
    return chunk


def optimize_chunk(chunk: Chunk) -> Chunk:
    """
    Top-level optimizer orchestrator.

    Note: The caller is responsible for gating via ZETA_OPT; this function
    simply chains optimization passes.
    """
    chunk = quicken_opcodes(chunk)
    chunk = inline_simple_functions(chunk)
    chunk = constant_folding(chunk)
    chunk = peephole_optimize(chunk)
    chunk = dead_code_elimination(chunk)
    chunk = jump_target_shortening(chunk)

    return chunk
