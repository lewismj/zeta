from __future__ import annotations

from dataclasses import dataclass
from typing import Any, List, Tuple, Callable

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
    class RunSignal:
        NORMAL = 0
        RETURN = 1
        CONTINUE = 2

    def __init__(self, env: Environment, macros=None):
        self.env = env
        self.macros = macros
        self.stack: List[Any] = []
        self.frames: List[Frame] = []
        # Handlers: list of (frame_index_at_setup, target_ip, base_stack_len)
        self.handlers: List[tuple[int, int, int]] = []
        # Continuation handlers: list of (frame_index_at_setup, base_stack_at_setup)
        self.cc_handlers: List[tuple[int, int]] = []
        # Opcode dispatch table
        self._dispatch: dict[int, Callable[[Frame, int], Tuple[int, Any | None]]] = {}
        self._init_dispatch()

    def _is_truthy(self, v: Any) -> bool:
        # In Zeta, only Nil and Symbol("#f") are falsey
        return not (v is Nil or (isinstance(v, Symbol) and v.id == "#f"))


    # TODO-
    # TODO - I know this is a bit of a mess, it should be Mixin or other classes as getattr and build the
    # TODO - dictionary on the fly etc. etc.
    # TODO - Will update once thought of some optimizations to do on the bytecode.
    # TODO -

    def _init_dispatch(self) -> None:
        d = self._dispatch
        # Stack and constants
        d[Opcode.NOP] = self.op_nop
        d[Opcode.PUSH_NIL] = self.op_push_nil
        d[Opcode.PUSH_TRUE] = self.op_push_true
        d[Opcode.PUSH_FALSE] = self.op_push_false
        d[Opcode.PUSH_INT] = self.op_push_int
        d[Opcode.PUSH_CONST] = self.op_push_const
        d[Opcode.DUP] = self.op_dup
        d[Opcode.POP] = self.op_pop
        d[Opcode.SWAP] = self.op_swap
        # Quickened variants
        d[Opcode.PUSH_CONST_Q] = self.op_push_const_q
        d[Opcode.LOAD_LOCAL_Q] = self.op_load_local_q
        d[Opcode.STORE_LOCAL_Q] = self.op_store_local_q
        d[Opcode.LOAD_UPVALUE_Q] = self.op_load_upvalue_q
        d[Opcode.STORE_UPVALUE_Q] = self.op_store_upvalue_q
        d[Opcode.JUMP_Q] = self.op_jump_q
        # Locals / upvalues / globals
        d[Opcode.LOAD_LOCAL] = self.op_load_local
        d[Opcode.STORE_LOCAL] = self.op_store_local
        d[Opcode.LOAD_GLOBAL] = self.op_load_global
        d[Opcode.STORE_GLOBAL] = self.op_store_global
        d[Opcode.LOAD_UPVALUE] = self.op_load_upvalue
        d[Opcode.STORE_UPVALUE] = self.op_store_upvalue
        d[Opcode.CLOSE_UPVALUE] = self.op_close_upvalue
        # Control flow
        d[Opcode.JUMP] = self.op_jump
        d[Opcode.JUMP_IF_TRUE] = self.op_jump_if_true
        d[Opcode.JUMP_IF_FALSE] = self.op_jump_if_false
        d[Opcode.JUMP_IF_NIL] = self.op_jump_if_nil
        d[Opcode.RETURN] = self.op_return
        d[Opcode.SETUP_CATCH] = self.op_setup_catch
        d[Opcode.POP_CATCH] = self.op_pop_catch
        d[Opcode.THROW] = self.op_throw
        # Functions / closures / calls
        d[Opcode.MAKE_CLOSURE] = self.op_make_closure
        d[Opcode.MAKE_LAMBDA] = self.op_make_lambda
        d[Opcode.CALL_CC] = self.op_call_cc
        d[Opcode.CALL] = self.op_call
        d[Opcode.TAILCALL] = self.op_tailcall
        # Lists / sequences
        d[Opcode.CONS] = self.op_cons
        d[Opcode.LIST] = self.op_list
        d[Opcode.APPEND] = self.op_append
        # Arithmetic / comparison and logical not
        d[Opcode.NOT] = self.op_not
        d[Opcode.ADD] = self.op_add
        d[Opcode.SUB] = self.op_sub
        d[Opcode.MUL] = self.op_mul
        d[Opcode.DIV] = self.op_div
        d[Opcode.MOD] = self.op_mod
        d[Opcode.LT] = self.op_lt
        d[Opcode.LE] = self.op_le
        d[Opcode.GT] = self.op_gt
        d[Opcode.GE] = self.op_ge
        d[Opcode.EQ] = self.op_eq
        d[Opcode.NEQ] = self.op_neq
        # Misc
        d[Opcode.HALT] = self.op_halt

    # --- Per-op handlers (no large if/else) ---
    # Stack and constants
    def op_nop(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return VM.RunSignal.NORMAL, None

    def op_push_nil(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        self.push(Nil)
        return VM.RunSignal.NORMAL, None

    def op_push_true(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        self.push(True)
        return VM.RunSignal.NORMAL, None

    def op_push_false(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        self.push(False)
        return VM.RunSignal.NORMAL, None

    def op_push_int(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
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
        return VM.RunSignal.NORMAL, None

    def op_push_const(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        consts = frame.chunk.constants
        idx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        self.push(consts[idx])
        return VM.RunSignal.NORMAL, None

    def op_push_const_q(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        qidx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        # Read from pre-decoded immediates if available; otherwise fallback to constants
        arr = getattr(frame.chunk, 'quick_imms', None)
        if arr and 0 <= qidx < len(arr):
            self.push(arr[qidx])
        else:
            # Fallback: treat as normal PUSH_CONST
            consts = frame.chunk.constants
            self.push(consts[qidx])
        return VM.RunSignal.NORMAL, None

    def op_dup(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        self.push(self.peek())
        return VM.RunSignal.NORMAL, None

    def op_pop(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        self.pop()
        return VM.RunSignal.NORMAL, None

    def op_swap(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        a = self.pop()
        b = self.pop()
        self.push(a)
        self.push(b)
        return VM.RunSignal.NORMAL, None

    # Locals / upvalues / globals
    def op_load_local(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        idx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        self.push(self.stack[frame.base + idx])
        return VM.RunSignal.NORMAL, None

    def op_load_local_q(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        # Quickened operand path: currently identical operand read; reserved for future pre-decode
        code = frame.chunk.code
        idx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        self.push(self.stack[frame.base + idx])
        return VM.RunSignal.NORMAL, None

    def op_store_local(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        idx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        val = self.peek()
        slot = frame.base + idx
        if slot >= len(self.stack):
            needed = (slot - len(self.stack)) + 1
            needed += 1
            for _ in range(needed):
                self.stack.append(Nil)
        elif slot == len(self.stack) - 1:
            self.stack.append(Nil)
        self.stack[slot] = val
        return VM.RunSignal.NORMAL, None

    def op_store_local_q(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        # Quickened operand path: identical semantics
        code = frame.chunk.code
        idx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        val = self.peek()
        slot = frame.base + idx
        if slot >= len(self.stack):
            needed = (slot - len(self.stack)) + 1
            needed += 1
            for _ in range(needed):
                self.stack.append(Nil)
        elif slot == len(self.stack) - 1:
            self.stack.append(Nil)
        self.stack[slot] = val
        return VM.RunSignal.NORMAL, None

    def op_load_global(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        consts = frame.chunk.constants
        gidx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        sym = consts[gidx]
        assert isinstance(sym, Symbol)
        self.push(self.env.lookup(sym))
        return VM.RunSignal.NORMAL, None

    def op_store_global(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        consts = frame.chunk.constants
        gidx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        sym = consts[gidx]
        assert isinstance(sym, Symbol)
        val = self.peek()
        try:
            self.env.set(sym, val)
        except ZetaUnboundSymbol:
            self.env.define(sym, val)
        return VM.RunSignal.NORMAL, None

    def op_load_upvalue(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        uidx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        assert frame.env_cells is not None
        self.push(frame.env_cells[uidx].value)
        return VM.RunSignal.NORMAL, None

    def op_load_upvalue_q(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        uidx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        assert frame.env_cells is not None
        self.push(frame.env_cells[uidx].value)
        return VM.RunSignal.NORMAL, None

    def op_store_upvalue(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        uidx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        assert frame.env_cells is not None
        frame.env_cells[uidx].value = self.peek()
        return VM.RunSignal.NORMAL, None

    def op_store_upvalue_q(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        uidx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        assert frame.env_cells is not None
        frame.env_cells[uidx].value = self.peek()
        return VM.RunSignal.NORMAL, None

    def op_close_upvalue(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        # consume idx (no-op for this VM)
        frame.ip += 2
        return VM.RunSignal.NORMAL, None

    # Control flow
    def _read_rel16(self, frame: Frame) -> int:
        code = frame.chunk.code
        rel = ((code[frame.ip] << 8) | code[frame.ip + 1])
        if rel & 0x8000:
            rel = rel - (1 << 16)
        frame.ip += 2
        return rel

    def op_jump(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        rel = self._read_rel16(frame)
        frame.ip += rel
        return VM.RunSignal.NORMAL, None

    def op_jump_q(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        # Quickened jump variant; same operand path for now
        rel = self._read_rel16(frame)
        frame.ip += rel
        return VM.RunSignal.NORMAL, None

    def op_setup_catch(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        rel = self._read_rel16(frame)
        target_ip = frame.ip + rel
        self.handlers.append((len(self.frames) - 1, target_ip, len(self.stack)))
        return VM.RunSignal.NORMAL, None

    def op_pop_catch(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        if self.handlers:
            self.handlers.pop()
        return VM.RunSignal.NORMAL, None

    def op_throw(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        val = self.pop() if self.stack else Nil
        if not self._unwind_to_handler(val):
            raise RuntimeError(f"Uncaught condition: {val!r}")
        return VM.RunSignal.CONTINUE, None

    def op_jump_if_true(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        rel = self._read_rel16(frame)
        v = self.pop()
        if self._is_truthy(v):
            frame.ip += rel
        return VM.RunSignal.NORMAL, None

    def op_jump_if_false(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        rel = self._read_rel16(frame)
        v = self.pop()
        if not self._is_truthy(v):
            frame.ip += rel
        return VM.RunSignal.NORMAL, None

    def op_jump_if_nil(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        rel = self._read_rel16(frame)
        v = self.pop()
        if v is Nil:
            frame.ip += rel
        return VM.RunSignal.NORMAL, None

    def op_return(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        ret = self.pop() if self.stack else Nil
        base = frame.base
        self.frames.pop()
        if len(self.stack) > base:
            del self.stack[base:]
        if not self.frames:
            return VM.RunSignal.RETURN, ret
        self.push(ret)
        if self.cc_handlers and self.cc_handlers[-1][0] == (len(self.frames) - 1):
            self.cc_handlers.pop()
        return VM.RunSignal.NORMAL, None

    # Functions / closures / calls
    def op_make_closure(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        consts = frame.chunk.constants
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
        return VM.RunSignal.NORMAL, None

    def op_make_lambda(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        consts = frame.chunk.constants
        fidx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        fn = consts[fidx]
        self.push(Closure(fn=fn, upvalues=[]))
        return VM.RunSignal.NORMAL, None

    def op_call_cc(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        callee = self.pop()
        self.cc_handlers.append((len(self.frames) - 1, len(self.stack)))

        def k_callable(_: Environment, args: list[Any]):
            value = args[0] if len(args) >= 1 else Nil
            raise ContinuationEscape(value)

        if isinstance(callee, Closure):
            new_frame = Frame(chunk=callee.fn.chunk, ip=0, base=len(self.stack), env_cells=callee.upvalues)
            self.stack.append(k_callable)
            self.frames.append(new_frame)
            return VM.RunSignal.NORMAL, None
        elif callable(callee):
            from zeta.runtime_context import set_current_macros as _set_macros
            try:
                _set_macros(self.macros)
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
                    return VM.RunSignal.CONTINUE, None
                self.cc_handlers.pop()
                self.push(result)
                return VM.RunSignal.NORMAL, None
            finally:
                _set_macros(None)
        else:
            raise TypeError(f"Cannot call {type(callee)} in call/cc")

    def _collect_args(self, frame: Frame) -> list[Any]:
        code = frame.chunk.code
        argc = code[frame.ip]
        frame.ip += 1
        args = [self.pop() for _ in range(argc)]
        args.reverse()
        return args

    def _call_closure(self, frame: Frame, callee: Closure, args: list[Any], tail: bool) -> Tuple[int, Any | None]:
        fn = callee.fn
        if tail:
            base = frame.base
            if len(self.stack) > base:
                del self.stack[base:]
            if getattr(fn, 'rest', False):
                fixed = getattr(fn, 'arity', 0)
                for val in args[:fixed]:
                    self.stack.append(val)
                self.stack.append(list(args[fixed:]))
            else:
                for val in args:
                    self.stack.append(val)
            frame.chunk = fn.chunk
            frame.ip = 0
            frame.base = base
            frame.env_cells = callee.upvalues
            return VM.RunSignal.NORMAL, None
        else:
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
            return VM.RunSignal.NORMAL, None

    def _call_callable(self, frame: Frame, callee: Callable, args: list[Any], tail: bool) -> Tuple[int, Any | None]:
        from zeta.runtime_context import set_current_macros as _set_macros
        try:
            _set_macros(self.macros)
            try:
                try:
                    result = callee(self.env, args)
                except TypeError:
                    if hasattr(callee, "__code__") and getattr(callee.__code__, "co_argcount", 0) >= 2:
                        raise
                    result = callee(*args)
            except ContinuationEscape as esc:
                if not self.cc_handlers:
                    raise
                depth, base0 = self.cc_handlers.pop()
                while len(self.frames) - 1 > depth:
                    self.frames.pop()
                if len(self.stack) > base0:
                    del self.stack[base0:]
                self.push(esc.value)
                return VM.RunSignal.CONTINUE, None
            except Exception as ex:
                if not self._unwind_to_handler(ex):
                    raise
                return VM.RunSignal.CONTINUE, None
            if tail:
                base = frame.base
                self.frames.pop()
                if len(self.stack) > base:
                    del self.stack[base:]
                if not self.frames:
                    return VM.RunSignal.RETURN, result
                self.push(result)
            else:
                self.push(result)
            return VM.RunSignal.NORMAL, None
        finally:
            _set_macros(None)

    def _do_call(self, frame: Frame, tail: bool) -> Tuple[int, Any | None]:
        args = self._collect_args(frame)
        callee = self.pop()
        if isinstance(callee, Closure):
            return self._call_closure(frame, callee, args, tail)
        elif callable(callee):
            return self._call_callable(frame, callee, args, tail)
        else:
            raise TypeError(f"Cannot call {type(callee)}")

    def op_call(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._do_call(frame, tail=False)

    def op_tailcall(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._do_call(frame, tail=True)

    # Lists / sequences
    def op_cons(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        tail = self.pop()
        head = self.pop()
        self.push([head] + (tail if isinstance(tail, list) else [tail]))
        return VM.RunSignal.NORMAL, None

    def op_list(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        code = frame.chunk.code
        n = code[frame.ip]
        frame.ip += 1
        items = [self.pop() for _ in range(n)]
        items.reverse()
        self.push(items)
        return VM.RunSignal.NORMAL, None

    def op_append(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        b = self.pop()
        a = self.pop()
        self.push((a if isinstance(a, list) else [a]) + (b if isinstance(b, list) else [b]))
        return VM.RunSignal.NORMAL, None

    # Arithmetic / comparison and logical not
    def op_not(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        v = self.pop()
        # Use Zeta truthiness: only Nil and Symbol("#f") are falsey.
        self.push(not self._is_truthy(v))
        return VM.RunSignal.NORMAL, None

    def _arith2(self, opfun: Callable[[Any, Any], Any]) -> Tuple[int, Any | None]:
        b = self.pop()
        a = self.pop()
        self.push(opfun(a, b))
        return VM.RunSignal.NORMAL, None

    def op_add(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._arith2(lambda a, b: a + b)

    def op_sub(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._arith2(lambda a, b: a - b)

    def op_mul(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._arith2(lambda a, b: a * b)

    def op_div(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._arith2(lambda a, b: a / b)

    def op_mod(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._arith2(lambda a, b: a % b)

    def op_lt(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._arith2(lambda a, b: a < b)

    def op_le(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._arith2(lambda a, b: a <= b)

    def op_gt(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._arith2(lambda a, b: a > b)

    def op_ge(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._arith2(lambda a, b: a >= b)

    def op_eq(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._arith2(lambda a, b: a == b)

    def op_neq(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return self._arith2(lambda a, b: a != b)

    # Misc
    def op_halt(self, frame: Frame, op: int) -> Tuple[int, Any | None]:
        return VM.RunSignal.RETURN, (self.pop() if self.stack else Nil)

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

            handler = self._dispatch.get(op)
            if handler is None:
                raise RuntimeError(f"Unknown opcode: {op}")
            signal, value = handler(frame, op)
            if signal == VM.RunSignal.RETURN:
                return value
            if signal == VM.RunSignal.CONTINUE:
                continue


def run_chunk(chunk: Chunk, env: Environment, macros=None) -> Any:
    vm = VM(env, macros)
    return vm.run(chunk)
