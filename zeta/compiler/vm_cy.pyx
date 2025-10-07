# cython: language_level=3
from __future__ import annotations

from typing import Any

from zeta.types.environment import Environment
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil
from zeta.types.errors import ZetaUnboundSymbol
from zeta.compiler.chunk import Chunk  # Python class (dataclass)
from zeta.compiler.opcodes import Opcode
from zeta.compiler.function import Closure, Cell
from zeta.evaluation.special_forms.call_cc_form import ContinuationEscape

# Top-level call/cc continuation function to avoid closures inside cpdef methods
# Signature matches host-call convention for built-ins: (env, args)
# It raises ContinuationEscape with the provided value (or Nil if none)
# so that the VM can unwind to the nearest call/cc handler.

def _callcc_k(env, args):
    value = args[0] if len(args) >= 1 else Nil
    raise ContinuationEscape(value)


cdef enum RunSignal:
    NORMAL = 0
    RETURN = 1
    CONTINUE = 2

cdef class Frame:
    cdef public object chunk  # Python Chunk
    cdef public int ip
    cdef public int base
    cdef public list env_cells  # list[Cell]
    def __init__(self, chunk, int base):
        self.chunk = chunk
        self.ip = 0
        self.base = base
        self.env_cells = []

cdef class Continuation:
    cdef public int base
    cdef public list stack_state
    cdef public list frames_state

    def __init__(self, int base, list stack_state, list frames_state):
        self.base = base
        self.stack_state = stack_state
        self.frames_state = frames_state

    def __call__(self, env, args):
        vm = env._vm  # Get VM instance from environment
        vm.stack = vm.stack[:self.base] + self.stack_state
        vm.frames = self.frames_state
        return args[0] if args else None


cdef class VM:
    cdef public list stack
    cdef public list frames
    cdef public list handlers
    cdef public list cc_handlers
    cdef public object env
    cdef public object macros
    cdef public dict dispatch


    def __init__(self, env, macros=None):
        self.env = env
        self.macros = macros
        self.stack = []
        self.frames = []
        self.handlers = []
        self.cc_handlers = []
        self.dispatch = {}
        self._init_dispatch()

    # --- Stack helpers (Cython inlined) ---
    cdef inline void _push(self, object v):
        self.stack.append(v)

    cdef inline object _pop(self):
        return self.stack.pop()

    cdef inline object _peek(self, int n=0):
        cdef int idx = len(self.stack) - 1 - n
        if idx < 0:
            raise IndexError("peek from empty stack")
        return (<list>self.stack)[idx]

    cdef inline bint _is_truthy(self, object v):
        # Only Nil and Symbol("#f") are falsey in Zeta
        if v is Nil:
            return False
        if isinstance(v, Symbol) and v.id == "#f":
            return False
        return True

    cdef void _init_dispatch(self):
        d = self.dispatch
        # Stack & constants
        d[Opcode.NOP] = self.op_nop
        d[Opcode.PUSH_NIL] = self.op_push_nil
        d[Opcode.PUSH_TRUE] = self.op_push_true
        d[Opcode.PUSH_FALSE] = self.op_push_false
        d[Opcode.PUSH_INT] = self.op_push_int
        d[Opcode.PUSH_CONST] = self.op_push_const
        d[Opcode.DUP] = self.op_dup
        d[Opcode.POP] = self.op_pop
        d[Opcode.SWAP] = self.op_swap
        # Arithmetic / comparison / logical
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
        d[Opcode.NOT] = self.op_not
        # Locals / upvalues
        d[Opcode.LOAD_LOCAL] = self.op_load_local
        d[Opcode.STORE_LOCAL] = self.op_store_local
        d[Opcode.LOAD_UPVALUE] = self.op_load_upvalue
        d[Opcode.STORE_UPVALUE] = self.op_store_upvalue
        # Globals
        d[Opcode.LOAD_GLOBAL] = self.op_load_global
        d[Opcode.STORE_GLOBAL] = self.op_store_global
        # Control flow
        d[Opcode.JUMP] = self.op_jump
        d[Opcode.JUMP_IF_TRUE] = self.op_jump_if_true
        d[Opcode.JUMP_IF_FALSE] = self.op_jump_if_false
        d[Opcode.RETURN] = self.op_return
        d[Opcode.TAILCALL] = self.op_tailcall
        d[Opcode.HALT] = self.op_halt
        # Functions / closures
        d[Opcode.MAKE_CLOSURE] = self.op_make_closure
        d[Opcode.MAKE_LAMBDA] = self.op_make_lambda
        # Quickened variants (subset used by optimizer)
        d[Opcode.PUSH_CONST_Q] = self.op_push_const_q
        d[Opcode.LOAD_LOCAL_Q] = self.op_load_local_q
        d[Opcode.STORE_LOCAL_Q] = self.op_store_local_q
        d[Opcode.LOAD_UPVALUE_Q] = self.op_load_upvalue_q
        d[Opcode.STORE_UPVALUE_Q] = self.op_store_upvalue_q
        d[Opcode.JUMP_Q] = self.op_jump_q
        # Calls
        d[Opcode.CALL] = self.op_call
        # Exceptions / handlers
        d[Opcode.SETUP_CATCH] = self.op_setup_catch  # 0x80 (128)
        d[Opcode.POP_CATCH] = self.op_pop_catch
        d[Opcode.THROW] = self.op_throw
        # Continuations
        d[Opcode.CALL_CC] = self.op_call_cc  # 0x92 (146)

    # --- Opcode handlers (cpdef so Python can call via dict) ---
    cpdef tuple op_nop(self, Frame frame, int op):
        return RunSignal.NORMAL, None

    cpdef tuple op_push_nil(self, Frame frame, int op):
        self._push(Nil)
        return RunSignal.NORMAL, None

    cpdef tuple op_push_true(self, Frame frame, int op):
        self._push(True)
        return RunSignal.NORMAL, None

    cpdef tuple op_push_false(self, Frame frame, int op):
        self._push(False)
        return RunSignal.NORMAL, None

    cpdef tuple op_push_const(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int idx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        self._push(frame.chunk.constants[idx])
        return RunSignal.NORMAL, None

    cpdef tuple op_push_const_q(self, Frame frame, int op):
        # Quickened constant load: read u16 qidx and fetch from chunk.quick_imms if available,
        # otherwise fall back to constants table.
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int qidx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        cdef object arr = getattr(frame.chunk, 'quick_imms', None)
        if arr is not None and 0 <= qidx < len(arr):
            self._push(arr[qidx])
        else:
            self._push(frame.chunk.constants[qidx])
        return RunSignal.NORMAL, None

    cpdef tuple op_push_int(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int b0 = code[frame.ip]
        cdef int b1 = code[frame.ip+1]
        cdef int b2 = code[frame.ip+2]
        cdef int b3 = code[frame.ip+3]
        cdef int v = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3
        frame.ip += 4
        if v & 0x80000000:
            v -= (1 << 32)
        self._push(v)
        return RunSignal.NORMAL, None

    cpdef tuple op_dup(self, Frame frame, int op):
        self._push(self._peek())
        return RunSignal.NORMAL, None

    cpdef tuple op_pop(self, Frame frame, int op):
        self._pop()
        return RunSignal.NORMAL, None

    cpdef tuple op_swap(self, Frame frame, int op):
        a = self._pop(); b = self._pop()
        self._push(a); self._push(b)
        return RunSignal.NORMAL, None

    # Arithmetic
    cpdef tuple op_add(self, Frame frame, int op):
        b = self._pop(); a = self._pop(); self._push(a + b)
        return RunSignal.NORMAL, None

    cpdef tuple op_sub(self, Frame frame, int op):
        b = self._pop(); a = self._pop(); self._push(a - b)
        return RunSignal.NORMAL, None

    cpdef tuple op_mul(self, Frame frame, int op):
        b = self._pop(); a = self._pop(); self._push(a * b)
        return RunSignal.NORMAL, None

    cpdef tuple op_div(self, Frame frame, int op):
        b = self._pop(); a = self._pop(); self._push(a / b)
        return RunSignal.NORMAL, None

    cpdef tuple op_mod(self, Frame frame, int op):
        b = self._pop(); a = self._pop(); self._push(a % b)
        return RunSignal.NORMAL, None

    # Comparison and logical
    cpdef tuple op_lt(self, Frame frame, int op):
        b = self._pop(); a = self._pop(); self._push(a < b)
        return RunSignal.NORMAL, None

    cpdef tuple op_le(self, Frame frame, int op):
        b = self._pop(); a = self._pop(); self._push(a <= b)
        return RunSignal.NORMAL, None

    cpdef tuple op_gt(self, Frame frame, int op):
        b = self._pop(); a = self._pop(); self._push(a > b)
        return RunSignal.NORMAL, None

    cpdef tuple op_ge(self, Frame frame, int op):
        b = self._pop(); a = self._pop(); self._push(a >= b)
        return RunSignal.NORMAL, None

    cpdef tuple op_eq(self, Frame frame, int op):
        b = self._pop(); a = self._pop(); self._push(a == b)
        return RunSignal.NORMAL, None

    cpdef tuple op_neq(self, Frame frame, int op):
        b = self._pop(); a = self._pop(); self._push(a != b)
        return RunSignal.NORMAL, None

    cpdef tuple op_not(self, Frame frame, int op):
        cdef object v = self._pop()
        self._push(not self._is_truthy(v))
        return RunSignal.NORMAL, None

    # Locals / Upvalues
    cpdef tuple op_load_local(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int idx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        self._push(self.stack[frame.base + idx])
        return RunSignal.NORMAL, None

    cpdef tuple op_load_local_q(self, Frame frame, int op):
        # Same operand path for now (quickened placeholder)
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int idx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        self._push(self.stack[frame.base + idx])
        return RunSignal.NORMAL, None

    cpdef tuple op_store_local(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int idx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        cdef object val = self._peek()
        cdef int slot = frame.base + idx
        # Ensure stack large enough; avoid Cython block-scoped cdef
        if slot >= len(self.stack):
            needed = (slot - len(self.stack)) + 1
            needed += 1
            self.stack.extend([Nil] * needed)
        elif slot == len(self.stack) - 1:
            self.stack.append(Nil)
        self.stack[slot] = val
        return RunSignal.NORMAL, None

    cpdef tuple op_store_local_q(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int idx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        cdef object val = self._peek()
        cdef int slot = frame.base + idx
        # Ensure stack large enough; avoid Cython block-scoped cdef
        if slot >= len(self.stack):
            needed = (slot - len(self.stack)) + 1
            needed += 1
            self.stack.extend([Nil] * needed)
        elif slot == len(self.stack) - 1:
            self.stack.append(Nil)
        self.stack[slot] = val
        return RunSignal.NORMAL, None

    cpdef tuple op_load_upvalue(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int idx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        self._push((<list>frame.env_cells)[idx].value)
        return RunSignal.NORMAL, None

    cpdef tuple op_load_upvalue_q(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int idx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        self._push((<list>frame.env_cells)[idx].value)
        return RunSignal.NORMAL, None

    cpdef tuple op_store_upvalue(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int idx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        (<list>frame.env_cells)[idx].value = self._peek()
        return RunSignal.NORMAL, None

    cpdef tuple op_store_upvalue_q(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int idx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        (<list>frame.env_cells)[idx].value = self._peek()
        return RunSignal.NORMAL, None

    # Globals
    cpdef tuple op_load_global(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int idx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        cdef object sym = frame.chunk.constants[idx]
        self._push(self.env.lookup(sym))
        return RunSignal.NORMAL, None

    cpdef tuple op_store_global(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int idx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        cdef object sym = frame.chunk.constants[idx]
        cdef object val = self._peek()
        try:
            self.env.set(sym, val)
        except ZetaUnboundSymbol:
            self.env.define(sym, val)
        return RunSignal.NORMAL, None

    # --- Functions / closures ---
    cpdef tuple op_make_closure(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int fidx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        cdef int ucount = code[frame.ip]
        frame.ip += 1
        cdef object fn = frame.chunk.constants[fidx]
        cdef list cells = []
        cdef int i
        cdef int is_local
        cdef int idx
        for i in range(ucount):
            is_local = code[frame.ip]; frame.ip += 1
            idx = (code[frame.ip] << 8) | code[frame.ip+1]; frame.ip += 2
            if is_local:
                cells.append(Cell(self.stack[frame.base + idx]))
            else:
                cells.append((<list>frame.env_cells)[idx])
        self._push(Closure(fn=fn, upvalues=cells))
        return RunSignal.NORMAL, None

    cpdef tuple op_make_lambda(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int fidx = (code[frame.ip] << 8) | code[frame.ip+1]
        frame.ip += 2
        cdef object fn = frame.chunk.constants[fidx]
        self._push(Closure(fn=fn, upvalues=[]))
        return RunSignal.NORMAL, None

    cdef tuple _call_closure(self, Frame frame, object callee, list args, bint tail):
        cdef object fn = callee.fn
        cdef int base
        cdef int fixed
        cdef Frame new_frame
        if tail:
            base = frame.base
            if len(self.stack) > base:
                del self.stack[base:]
            if getattr(fn, 'rest', False):
                fixed = getattr(fn, 'arity', 0)
                for v in args[:fixed]:
                    self.stack.append(v)
                self.stack.append(list(args[fixed:]))
            else:
                for v in args:
                    self.stack.append(v)
            frame.chunk = fn.chunk
            frame.ip = 0
            frame.base = base
            frame.env_cells = callee.upvalues
            return RunSignal.NORMAL, None
        else:
            new_frame = Frame(fn.chunk, len(self.stack))
            new_frame.env_cells = callee.upvalues
            if getattr(fn, 'rest', False):
                fixed = getattr(fn, 'arity', 0)
                for v in args[:fixed]:
                    self.stack.append(v)
                self.stack.append(list(args[fixed:]))
            else:
                for v in args:
                    self.stack.append(v)
            self.frames.append(new_frame)
            return RunSignal.NORMAL, None

    cdef tuple _call_callable(self, Frame frame, object callee, list args, bint tail):
        from zeta.runtime_context import set_current_macros as _set_macros
        cdef int base
        cdef object result
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
                # Unwind to the last call/cc handler
                if not self.cc_handlers:
                    raise
                depth, base0 = self.cc_handlers.pop()
                while len(self.frames) - 1 > depth:
                    self.frames.pop()
                if len(self.stack) > base0:
                    del self.stack[base0:]
                self._push(esc.value)
                return RunSignal.CONTINUE, None
            except Exception as ex:
                # Try condition-case handler unwind; else re-raise
                if not self._unwind_to_handler(ex):
                    raise
                return RunSignal.CONTINUE, None
            if tail:
                base = frame.base
                # pop current frame
                self.frames.pop()
                if len(self.stack) > base:
                    del self.stack[base:]
                if not self.frames:
                    return RunSignal.RETURN, result
                self._push(result)
                return RunSignal.NORMAL, None
            else:
                self._push(result)
                return RunSignal.NORMAL, None
        finally:
            _set_macros(None)

    cpdef tuple op_call(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int argc = code[frame.ip]
        frame.ip += 1
        # Collect arguments from the stack (reverse order on stack)
        cdef list args = [self._pop() for _ in range(argc)]
        args.reverse()
        cdef object callee = self._pop()
        if isinstance(callee, Closure):
            return self._call_closure(frame, callee, args, False)
        else:
            return self._call_callable(frame, callee, args, False)

    cpdef tuple op_tailcall(self, Frame frame, int op):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int argc = code[frame.ip]
        frame.ip += 1
        cdef list args = [self._pop() for _ in range(argc)]
        args.reverse()
        cdef object callee = self._pop()
        if isinstance(callee, Closure):
            return self._call_closure(frame, callee, args, True)
        else:
            return self._call_callable(frame, callee, args, True)

    # Control flow
    cdef inline int _read_rel16(self, Frame frame):
        cdef bytearray code = <bytearray>frame.chunk.code
        cdef int rel = (code[frame.ip] << 8) | code[frame.ip+1]
        if rel & 0x8000:
            rel -= (1 << 16)
        frame.ip += 2
        return rel

    cpdef tuple op_jump(self, Frame frame, int op):
        frame.ip += self._read_rel16(frame)
        return RunSignal.NORMAL, None

    cpdef tuple op_jump_q(self, Frame frame, int op):
        frame.ip += self._read_rel16(frame)
        return RunSignal.NORMAL, None

    cpdef tuple op_jump_if_true(self, Frame frame, int op):
        cdef int rel = self._read_rel16(frame)
        cdef object v = self._pop()
        if self._is_truthy(v):
            frame.ip += rel
        return RunSignal.NORMAL, None

    cpdef tuple op_jump_if_false(self, Frame frame, int op):
        cdef int rel = self._read_rel16(frame)
        cdef object v = self._pop()
        if not self._is_truthy(v):
            frame.ip += rel
        return RunSignal.NORMAL, None

    cpdef tuple op_return(self, Frame frame, int op):
        cdef object ret = self._pop() if self.stack else Nil
        cdef int base = frame.base
        # Pop current frame
        self.frames.pop()
        # Trim operand stack to base
        if len(self.stack) > base:
            del self.stack[base:]
        if not self.frames:
            return RunSignal.RETURN, ret
        # Otherwise, push return value for caller and continue
        self._push(ret)
        # If returning from a frame established by call/cc, drop its handler
        if self.cc_handlers:
            last_idx = len(self.cc_handlers) - 1
            top = self.cc_handlers[last_idx]
            if top[0] == (len(self.frames) - 1):
                self.cc_handlers.pop()
        return RunSignal.NORMAL, None

    cpdef tuple op_halt(self, Frame frame, int op):
        return RunSignal.RETURN, (self._pop() if self.stack else Nil)

    cdef bint _unwind_to_handler(self, object value):
        if not self.handlers:
            return False
        cdef object tup = self.handlers.pop()
        cdef int frame_idx = tup[0]
        cdef int target_ip = tup[1]
        cdef int base_len = tup[2]
        while len(self.frames) - 1 > frame_idx:
            self.frames.pop()
        if len(self.stack) > base_len:
            del self.stack[base_len:]
        cdef Frame f = <Frame> self.frames[len(self.frames) - 1]
        f.ip = target_ip
        self.stack.append(value)
        return True

    cpdef tuple op_setup_catch(self, Frame frame, int op):
        cdef int target = self._read_rel16(frame)
        cdef int target_ip = frame.ip + target
        self.handlers.append((len(self.frames) - 1, target_ip, len(self.stack)))
        return RunSignal.NORMAL, None

    cpdef tuple op_pop_catch(self, Frame frame, int op):
        if self.handlers:
            self.handlers.pop()
        return RunSignal.NORMAL, None

    cpdef tuple op_throw(self, Frame frame, int op):
        cdef object val = self._pop() if self.stack else Nil
        if not self._unwind_to_handler(val):
            raise RuntimeError(f"Uncaught condition: {val!r}")
        return RunSignal.CONTINUE, None

    cpdef tuple op_call_cc(self, Frame frame, int op):
        cdef object proc = self._pop()
        # Push a continuation handler capturing current depth and base
        self.cc_handlers.append((len(self.frames) - 1, len(self.stack)))

        if isinstance(proc, Closure):
            # Set up a new frame and pass the continuation function as argument
            new_frame = Frame(proc.fn.chunk, len(self.stack))
            new_frame.env_cells = proc.upvalues
            self.stack.append(_callcc_k)
            self.frames.append(new_frame)
            return RunSignal.NORMAL, None
        elif callable(proc):
            sig_val = self._call_callable(frame, proc, [_callcc_k], False)
            if sig_val[0] == RunSignal.NORMAL:
                # Normal return: pop continuation handler
                self.cc_handlers.pop()
            return sig_val
        else:
            raise TypeError(f"Cannot call {type(proc)} in call/cc")

    def run(self, chunk):
        self.frames.append(Frame(chunk, 0))
        cdef Frame frame
        cdef int op
        cdef tuple sig_val
        cdef object handler
        while True:
            frame = <Frame>self.frames[len(self.frames) - 1]
            if frame.ip >= len(frame.chunk.code):
                return self._pop() if self.stack else Nil
            op = frame.chunk.code[frame.ip]
            frame.ip += 1
            handler = self.dispatch.get(op)
            if handler is None:
                raise RuntimeError(f"Unknown opcode {op}")
            sig_val = handler(frame, op)
            if sig_val[0] == RunSignal.RETURN:
                return sig_val[1]
            if sig_val[0] == RunSignal.CONTINUE:
                continue

cpdef run_chunk(chunk, env, macros=None):
    vm = VM(env, macros)
    return vm.run(chunk)
