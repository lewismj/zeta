from __future__ import annotations
from typing import Any

from .chunk import Chunk
from .opcodes import Opcode
from .vm import run_chunk


def call_vm_closure(env: Any, closure: Any, args: list[Any]) -> Any:
    """
    Invoke a VM Closure from outside the VM by synthesizing a tiny chunk that
    pushes the given closure and its arguments, performs a CALL, and HALTs.

    This is used to allow the classic evaluator to interoperate with VM-compiled
    closures in mixed-mode scenarios (e.g., temporary fast-paths).
    """
    chunk = Chunk()
    # Push callee first, then each argument
    cidx = chunk.add_const(closure)
    chunk.emit_op(Opcode.PUSH_CONST)
    chunk.emit_u16(cidx)
    for arg in args:
        aidx = chunk.add_const(arg)
        chunk.emit_op(Opcode.PUSH_CONST)
        chunk.emit_u16(aidx)
    chunk.emit_op(Opcode.CALL)
    chunk.emit_u8(len(args))
    chunk.emit_op(Opcode.HALT)
    return run_chunk(chunk, env)
