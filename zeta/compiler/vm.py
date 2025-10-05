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
    def __init__(self, env: Environment):
        self.env = env
        self.stack: List[Any] = []
        self.frames: List[Frame] = []
        self.handlers: List[tuple[int, int]] = []  # (target_ip, base)
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

            elif op == Opcode.PUSH_NIL:
                self.push(Nil)

            elif op == Opcode.PUSH_TRUE:
                self.push(True)

            elif op == Opcode.PUSH_FALSE:
                self.push(False)

            elif op == Opcode.PUSH_INT:
                v = (
                    (code[frame.ip] << 24)
                    | (code[frame.ip + 1] << 16)
                    | (code[frame.ip + 2] << 8)
                    | code[frame.ip + 3]
                )
                frame.ip += 4
                if v & 0x80000000:
                    v = v - (1 << 32)
                self.push(v)

            elif op == Opcode.PUSH_CONST:
                idx = (code[frame.ip] << 8) | code[frame.ip + 1]
                frame.ip += 2
                self.push(consts[idx])

            elif op == Opcode.DUP:
                self.push(self.peek())

            elif op == Opcode.POP:
                self.pop()

            elif op == Opcode.SWAP:
                a = self.pop()
                b = self.pop()
                self.push(a)
                self.push(b)

            # Locals (we use the value stack slots starting at base)
            elif op == Opcode.LOAD_LOCAL:
                idx = (code[frame.ip] << 8) | code[frame.ip + 1]
                frame.ip += 2
                self.push(self.stack[frame.base + idx])

            elif op == Opcode.STORE_LOCAL:
                idx = (code[frame.ip] << 8) | code[frame.ip + 1]
                frame.ip += 2
                val = self.peek()
                slot = frame.base + idx
                # Grow stack if needed
                while len(self.stack) <= slot:
                    self.stack.append(Nil)
                self.stack[slot] = val

            elif op == Opcode.LOAD_GLOBAL:
                gidx = (code[frame.ip] << 8) | code[frame.ip + 1]
                frame.ip += 2
                sym = consts[gidx]
                assert isinstance(sym, Symbol)
                self.push(self.env.lookup(sym))

            elif op == Opcode.STORE_GLOBAL:
                gidx = (code[frame.ip] << 8) | code[frame.ip + 1]
                frame.ip += 2
                sym = consts[gidx]
                assert isinstance(sym, Symbol)
                val = self.peek()
                try:
                    self.env.set(sym, val)
                except ZetaUnboundSymbol:
                    # If symbol not yet defined, create a new global binding
                    self.env.define(sym, val)

            elif op == Opcode.LOAD_UPVALUE:
                uidx = (code[frame.ip] << 8) | code[frame.ip + 1]
                frame.ip += 2
                assert frame.env_cells is not None
                self.push(frame.env_cells[uidx].value)

            elif op == Opcode.STORE_UPVALUE:
                uidx = (code[frame.ip] << 8) | code[frame.ip + 1]
                frame.ip += 2
                assert frame.env_cells is not None
                frame.env_cells[uidx].value = self.peek()

            elif op == Opcode.CLOSE_UPVALUE:
                # For MVP we skip open/close mechanics; no-op placeholder
                frame.ip += 2  # consume idx

            # Control flow
            elif op == Opcode.JUMP:
                rel = ((code[frame.ip] << 8) | code[frame.ip + 1])
                if rel & 0x8000:
                    rel = rel - (1 << 16)
                frame.ip += 2
                frame.ip += rel

            elif op == Opcode.JUMP_IF_TRUE:
                rel = ((code[frame.ip] << 8) | code[frame.ip + 1])
                if rel & 0x8000:
                    rel = rel - (1 << 16)
                frame.ip += 2
                v = self.pop()
                if self._is_truthy(v):
                    frame.ip += rel

            elif op == Opcode.JUMP_IF_FALSE:
                rel = ((code[frame.ip] << 8) | code[frame.ip + 1])
                if rel & 0x8000:
                    rel = rel - (1 << 16)
                frame.ip += 2
                v = self.pop()
                if not self._is_truthy(v):
                    frame.ip += rel

            elif op == Opcode.JUMP_IF_NIL:
                rel = ((code[frame.ip] << 8) | code[frame.ip + 1])
                if rel & 0x8000:
                    rel = rel - (1 << 16)
                frame.ip += 2
                v = self.pop()
                if v is Nil:
                    frame.ip += rel

            elif op == Opcode.RETURN:
                ret = self.pop() if self.stack else Nil
                # Unwind stack to this frame's base and pop the frame
                base = frame.base
                self.frames.pop()
                if len(self.stack) > base:
                    del self.stack[base:]
                if not self.frames:
                    return ret
                # restore previous frame, push ret
                self.push(ret)
                # If a call/cc dynamic extent ended, prune the most recent handler
                if self.cc_handlers and self.cc_handlers[-1][0] == (len(self.frames) - 1):
                    self.cc_handlers.pop()

            # Functions and calls
            elif op == Opcode.MAKE_CLOSURE:
                fidx = (code[frame.ip] << 8) | code[frame.ip + 1]
                frame.ip += 2
                ucount = code[frame.ip]
                frame.ip += 1
                fn = consts[fidx]
                cells: list[Cell] = []
                for _ in range(ucount):
                    is_local = code[frame.ip]
                    frame.ip += 1
                    idx = (code[frame.ip] << 8) | code[frame.ip + 1]
                    frame.ip += 2
                    if is_local:
                        cells.append(Cell(self.stack[frame.base + idx]))
                    else:
                        assert frame.env_cells is not None
                        cells.append(frame.env_cells[idx])
                self.push(Closure(fn=fn, upvalues=cells))

            elif op == Opcode.MAKE_LAMBDA:
                fidx = (code[frame.ip] << 8) | code[frame.ip + 1]
                frame.ip += 2
                fn = consts[fidx]
                self.push(Closure(fn=fn, upvalues=[]))

            elif op == Opcode.CALL_CC:
                # Stack: [..., callee]
                callee = self.pop()
                # Register dynamic-extent handler for this call/cc using current stack height
                self.cc_handlers.append((len(self.frames) - 1, len(self.stack)))

                # Build the continuation callable that raises ContinuationEscape
                def k_callable(_: Environment, args: list[Any]):
                    value = args[0] if len(args) >= 1 else Nil
                    raise ContinuationEscape(value)

                if isinstance(callee, Closure):
                    # Normal closure call with single argument k
                    new_frame = Frame(chunk=callee.fn.chunk, ip=0, base=len(self.stack), env_cells=callee.upvalues)
                    self.stack.append(k_callable)
                    self.frames.append(new_frame)
                elif callable(callee):
                    # Host call with try/except to catch continuation escape
                    try:
                        try:
                            result = callee(self.env, [k_callable])
                        except TypeError:
                            result = callee(k_callable)
                    except ContinuationEscape as esc:
                        depth, base0 = self.cc_handlers.pop()
                        while len(self.frames) - 1 > depth:
                            self.frames.pop()
                        if len(self.stack) > base0:
                            del self.stack[base0:]
                        self.push(esc.value)
                        continue
                    # Normal return ends dynamic extent immediately
                    self.cc_handlers.pop()
                    self.push(result)
                else:
                    raise TypeError(f"Cannot call {type(callee)} in call/cc")

            elif op == Opcode.CALL or op == Opcode.TAILCALL:
                argc = code[frame.ip]
                frame.ip += 1
                # Stack layout is: [..., callee, arg1, ..., argN]
                # Pop args first, then callee
                args = [self.pop() for _ in range(argc)]
                args.reverse()
                callee = self.pop()
                if isinstance(callee, Closure):
                    fn = callee.fn
                    if op == Opcode.TAILCALL:
                        # Proper TCO: replace the current frame in-place without growing the call stack
                        base = frame.base
                        # Trim stack to the current frame's base
                        if len(self.stack) > base:
                            del self.stack[base:]
                        # Bind arguments into the same stack segment as new locals
                        if getattr(fn, 'rest', False):
                            fixed = getattr(fn, 'arity', 0)
                            # push fixed args
                            for val in args[:fixed]:
                                self.stack.append(val)
                            # pack remaining into a list for the rest param
                            self.stack.append(list(args[fixed:]))
                        else:
                            for val in args:
                                self.stack.append(val)
                        # Reinitialize current frame to execute the callee
                        frame.chunk = fn.chunk
                        frame.ip = 0
                        frame.base = base
                        frame.env_cells = callee.upvalues
                    else:
                        # Normal call: push a new frame and grow the stack with args
                        new_frame = Frame(chunk=fn.chunk, ip=0, base=len(self.stack), env_cells=callee.upvalues)
                        if getattr(fn, 'rest', False):
                            fixed = getattr(fn, 'arity', 0)
                            for val in args[:fixed]:
                                self.stack.append(val)
                            self.stack.append(list(args[fixed:]))
                        else:
                            for val in args:
                                self.stack.append(val)
                        self.frames.append(new_frame)
                elif callable(callee):
                    # Host call: prefer Zeta builtin protocol (env, args) then fallback to positional
                    try:
                        try:
                            result = callee(self.env, args)
                        except TypeError:
                            result = callee(*args)
                    except ContinuationEscape as esc:
                        # On continuation escape, unwind to the nearest handler
                        if not self.cc_handlers:
                            raise
                        depth, base0 = self.cc_handlers.pop()
                        # Pop frames until we are back to the handler's frame depth
                        while len(self.frames) - 1 > depth:
                            self.frames.pop()
                        # Trim stack to recorded base and push the escape value
                        if len(self.stack) > base0:
                            del self.stack[base0:]
                        self.push(esc.value)
                        # Continue execution in the current frame after the original call
                        continue
                    if op == Opcode.TAILCALL:
                        # Tail-call into host callable: replace current frame
                        base = frame.base
                        self.frames.pop()
                        if len(self.stack) > base:
                            del self.stack[base:]
                        if not self.frames:
                            return result
                        self.push(result)
                    else:
                        self.push(result)
                else:
                    raise TypeError(f"Cannot call {type(callee)}")

            # Simple list ops
            elif op == Opcode.CONS:
                tail = self.pop()
                head = self.pop()
                self.push([head] + (tail if isinstance(tail, list) else [tail]))

            elif op == Opcode.LIST:
                n = code[frame.ip]
                frame.ip += 1
                items = [self.pop() for _ in range(n)]
                items.reverse()
                self.push(items)

            elif op == Opcode.APPEND:
                b = self.pop()
                a = self.pop()
                self.push((a if isinstance(a, list) else [a]) + (b if isinstance(b, list) else [b]))

            # Arithmetic via Python operators or env callables
            elif op in (Opcode.ADD, Opcode.SUB, Opcode.MUL, Opcode.DIV, Opcode.MOD,
                        Opcode.LT, Opcode.LE, Opcode.GT, Opcode.GE, Opcode.EQ, Opcode.NEQ):
                b = self.pop()
                a = self.pop()
                if op == Opcode.ADD:
                    self.push(a + b)
                elif op == Opcode.SUB:
                    self.push(a - b)
                elif op == Opcode.MUL:
                    self.push(a * b)
                elif op == Opcode.DIV:
                    self.push(a / b)
                elif op == Opcode.MOD:
                    self.push(a % b)
                elif op == Opcode.LT:
                    self.push(a < b)
                elif op == Opcode.LE:
                    self.push(a <= b)
                elif op == Opcode.GT:
                    self.push(a > b)
                elif op == Opcode.GE:
                    self.push(a >= b)
                elif op == Opcode.EQ:
                    self.push(a == b)
                elif op == Opcode.NEQ:
                    self.push(a != b)

            elif op == Opcode.NOT:
                v = self.pop()
                self.push(not v)

            elif op == Opcode.HALT:
                return self.pop() if self.stack else Nil

            else:
                raise RuntimeError(f"Unknown opcode: {op}")


def run_chunk(chunk: Chunk, env: Environment) -> Any:
    vm = VM(env)
    return vm.run(chunk)
