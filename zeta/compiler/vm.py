from __future__ import annotations

from dataclasses import dataclass
from typing import Any, List

from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil
from zeta.types.errors import ZetaUnboundSymbol

from .opcodes import Opcode
from .function import Function, Closure, Cell
from .chunk import Chunk
from zeta.evaluation.special_forms.call_cc_form import ContinuationEscape


@dataclass
class Frame:
    chunk: Chunk
    ip: int
    base: int
    env_cells: list[Cell] | None  # closure upvalues


class VM:
    def __init__(self, env: Environment, macros=None):
        self.env = env
        self.macros = macros
        self.stack: List[Any] = []
        self.frames: List[Frame] = []
        # Handlers: list of (frame_index_at_setup, target_ip, base_stack_len)
        self.handlers: List[tuple[int, int, int]] = []
        # Continuation handlers: list of (frame_index_at_setup, base_stack_at_setup)
        self.cc_handlers: List[tuple[int, int]] = []

    def _is_truthy(self, v: Any) -> bool:
        # In Zeta, only Nil and Symbol("#f") are falsey
        return not (v is Nil or (isinstance(v, Symbol) and v.id == "#f"))

    # --- Stack helpers ---
    def push(self, v: Any) -> None:
        self.stack.append(v)

    def pop(self) -> Any:
        return self.stack.pop()

    def peek(self, n: int = 0) -> Any:
        return self.stack[-1 - n]

    # --- Execution ---
    def _unwind_to_handler(self, value: Any) -> bool:
        """Unwind frames and jump to the nearest active handler. Push value for handler.
        Returns True if a handler was found and control transferred; False if none.
        """
        if not self.handlers:
            return False
        # Take the most recent handler (dynamic scope)
        frame_idx, target_ip, base_len = self.handlers.pop()
        # Pop frames until we are at the handler's frame
        while len(self.frames) - 1 > frame_idx:
            self.frames.pop()
        # Trim the operand stack to the recorded base
        if len(self.stack) > base_len:
            del self.stack[base_len:]
        # Set current frame ip to target and push the value for handler body
        frame = self.frames[-1]
        frame.ip = target_ip
        self.push(value)
        return True

    def run(self, chunk: Chunk) -> Any:
        self.frames.append(Frame(chunk=chunk, ip=0, base=0, env_cells=None))

        while True:
            frame = self.frames[-1]
            code = frame.chunk.code
            consts = frame.chunk.constants
            ip = frame.ip
            if ip >= len(code):
                # Implicit HALT at chunk end for the CURRENT frame
                return self.pop() if self.stack else Nil
            op = code[ip]
            frame.ip += 1

            if op == Opcode.NOP:
                continue
            elif op == Opcode.HALT:
                return self.pop() if self.stack else Nil



            else:
                raise RuntimeError(f"Unknown opcode: {op}")


def run_chunk(chunk: Chunk, env: Environment, macros=None) -> Any:
    vm = VM(env, macros)
    return vm.run(chunk)
