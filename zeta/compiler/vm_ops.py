# type: ignore

from zeta.types.nil import Nil

class VMOpsMixin:
    self.dispatch_table = {
        Opcodes.PUSH_NIL: self.op_push_nil,
        Opcodes.PUSH_TRUE: self.op_push_true,
        Opcodes.PUSH_FALSE: self.op_push_false,
        Opcodes.PUSH_INT: self.op_push_int,
        Opcodes.PUSH_CONST: self.op_push_const,
        Opcodes.PUSH_DUP: self.op_push_dup,
        Opcodes.POP: self.op_pop,
        Opcodes.SWAP: self.op_swap,
        Opcodes.LOAD_LOCAL: self.op_load_local,
        Opcodes.STORE_LOCAL: self.op_store_local,
        Opcodes.LOAD_GLOBAL: self.op_load_global,
        Opcodes.STORE_GLOBAL: self.op_store_global,
        Opcodes.LOAD_UPVALUE: self.op_load_upvalue,
        Opcodes.STORE_UPVALUE: self.op_store_upvalue,
        Opcodes.CLOSE_UPVALUE: self.op_close_upvalue,
        Opcodes.JUMP: self.op_jump,
        Opcodes.SETUP_CATCH: self.op_setup_catch,
        Opcodes.POP_CATCH: self.op_pop_catch,
    }


    def op_push_nil(self):
        self.push(Nil)

    def op_push_true(self):
        self.push(True)

    def op_push_false(self):
        self.push(False)

    def op_push_int(self):
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

    def op_push_const(self):
        idx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        self.push(consts[idx])

    def op_push_dup(self):
        self.push(self.peek())

    def op_pop(self):
        self.pop()

    def op_swap(self):
        a = self.pop()
        b = self.pop()
        self.push(a)
        self.push(b)

    # # Locals (we use the value stack slots starting at base)
    def op_load_local(self):
        idx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        self.push(self.stack[frame.base + idx])

    def op_store_local(self):
        idx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        val = self.peek()
        slot = frame.base + idx
        # Ensure that popping after STORE_LOCAL (emitted by compiler) does not remove the stored local slot.
        # We keep the operand value on the top of the stack for the subsequent POP to consume. If the
        # target slot coincides with the current top index (or is beyond the current length), we insert placeholder(s)
        # so that the stored local resides below the top and survives the POP.
        if slot >= len(self.stack):
            # Need to extend with placeholders so that len(stack) > slot after extension.
            needed = (slot - len(self.stack)) + 1  # bring length to slot+1
            # Also ensure there's at least one extra placeholder above the slot for POP to remove
            needed += 1
            for _ in range(needed):
                self.stack.append(Nil)
        elif slot == len(self.stack) - 1:
            # Slot is currently at the top; append the placeholder so POP won't remove the stored value
            self.stack.append(Nil)
        # Now it's safe to assign into the slot (which is below the top element)
        self.stack[slot] = val

    def op_load_global(self):
        idx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        sym = consts[idx]
        assert isinstance(sym, Symbol)
        self.push(self.env.lookup(sym))

    def op_store_global(self):
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

    def op_load_upvalue(self):
        uidx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        assert frame.env_cells is not None
        self.push(frame.env_cells[uidx].value)

    def op_store_upvalue(self):
        uidx = (code[frame.ip] << 8) | code[frame.ip + 1]
        frame.ip += 2
        assert frame.env_cells is not None
        frame.env_cells[uidx].value = self.peek()

    def op_close_upvalue(self):
        # For the MVP we skip open/close mechanics; no-op placeholder
        frame.ip += 2  # consume idx

    # # Control flow
    def op_jump(self):
        rel = ((code[frame.ip] << 8) | code[frame.ip + 1])
        if rel & 0x8000:
            rel = rel - (1 << 16)
        frame.ip += 2
        frame.ip += rel

    def op_setup_catch(self):
        # Operand is s16 relative target from after operand
        rel = ((code[frame.ip] << 8) | code[frame.ip + 1])
        if rel & 0x8000:
            rel = rel - (1 << 16)
        frame.ip += 2
        target_ip = frame.ip + rel
        # Record handler with current frame index and base stack pointer
        self.handlers.append((len(self.frames) - 1, target_ip, len(self.stack)))


    def op_pop_catch(self):
        if self.handlers:
            self.handlers.pop()

    def op_throw(self):
        val = self.pop() if self.stack else Nil
        if not self._unwind_to_handler(val):
            raise RuntimeError(f"Uncaught condition: {val!r}")
        # Continue loop at new ip
        continue

    def op_jump_if_true(self):
        rel = ((code[frame.ip] << 8) | code[frame.ip + 1])
        if rel & 0x8000:
            rel = rel - (1 << 16)
        frame.ip += 2
        v = self.peek()
        if self._is_truthy(v):
            frame.ip += rel


#
# elif op == Opcode.JUMP_IF_TRUE:
# rel = ((code[frame.ip] << 8) | code[frame.ip + 1])
# if rel & 0x8000:
#     rel = rel - (1 << 16)
# frame.ip += 2
# v = self.pop()
# if self._is_truthy(v):
#     frame.ip += rel
#
# elif op == Opcode.JUMP_IF_FALSE:
#     rel = ((code[frame.ip] << 8) | code[frame.ip + 1])
# if rel & 0x8000:
#     rel = rel - (1 << 16)
# frame.ip += 2
# v = self.pop()
# if not self._is_truthy(v):
#     frame.ip += rel
#
# elif op == Opcode.JUMP_IF_NIL:
#     rel = ((code[frame.ip] << 8) | code[frame.ip + 1])
# if rel & 0x8000:
#     rel = rel - (1 << 16)
# frame.ip += 2
# v = self.pop()
# if v is Nil:
#     frame.ip += rel
#
# elif op == Opcode.RETURN:
#     ret = self.pop() if self.stack else Nil
# # Unwind stack to this frame's base and pop the frame
# base = frame.base
# self.frames.pop()
# if len(self.stack) > base:
#     del self.stack[base:]
# if not self.frames:
#     return ret
# # restore previous frame, push ret
# self.push(ret)
# # If a call/cc dynamic extent ended, prune the most recent handler
# if self.cc_handlers and self.cc_handlers[-1][0] == (len(self.frames) - 1):
#     self.cc_handlers.pop()
#
# # Functions and calls
# elif op == Opcode.MAKE_CLOSURE:
#     fidx = (code[frame.ip] << 8) | code[frame.ip + 1]
# frame.ip += 2
# ucount = code[frame.ip]
# frame.ip += 1
# fn = consts[fidx]
# cells: list[Cell] = []
# for _ in range(ucount):
#     is_local = code[frame.ip]
#     frame.ip += 1
#     idx = (code[frame.ip] << 8) | code[frame.ip + 1]
#     frame.ip += 2
#     if is_local:
#         cells.append(Cell(self.stack[frame.base + idx]))
#     else:
#         assert frame.env_cells is not None
#         cells.append(frame.env_cells[idx])
# self.push(Closure(fn=fn, upvalues=cells))
#
# elif op == Opcode.MAKE_LAMBDA:
# fidx = (code[frame.ip] << 8) | code[frame.ip + 1]
# frame.ip += 2
# fn = consts[fidx]
# self.push(Closure(fn=fn, upvalues=[]))
#
# elif op == Opcode.CALL_CC:
# # Stack: [..., callee]
# callee = self.pop()
# # Register dynamic-extent handler for this call/cc using current stack height
# self.cc_handlers.append((len(self.frames) - 1, len(self.stack)))
#
#
# # Build the continuation callable that raises ContinuationEscape
# def k_callable(_: Environment, args: list[Any]):
#     value = args[0] if len(args) >= 1 else Nil
#     raise ContinuationEscape(value)
#
#
# if isinstance(callee, Closure):
#     # Normal closure call with single argument k
#     new_frame = Frame(chunk=callee.fn.chunk, ip=0, base=len(self.stack), env_cells=callee.upvalues)
#     self.stack.append(k_callable)
#     self.frames.append(new_frame)
# elif callable(callee):
#     # Host call with try/except to catch continuation escape
#     from zeta.runtime_context import set_current_macros as _set_macros
#
#     try:
#         _set_macros(self.macros)
#         try:
#             try:
#                 result = callee(self.env, [k_callable])
#             except TypeError:
#                 result = callee(k_callable)
#         except ContinuationEscape as esc:
#             depth, base0 = self.cc_handlers.pop()
#             while len(self.frames) - 1 > depth:
#                 self.frames.pop()
#             if len(self.stack) > base0:
#                 del self.stack[base0:]
#             self.push(esc.value)
#             continue
#         # Normal return ends dynamic extent immediately
#         self.cc_handlers.pop()
#         self.push(result)
#     finally:
#         _set_macros(None)
# else:
#     raise TypeError(f"Cannot call {type(callee)} in call/cc")
#
# elif op == Opcode.CALL or op == Opcode.TAILCALL:
# argc = code[frame.ip]
# frame.ip += 1
# # Stack layout is: [..., callee, arg1, ..., argN]
# # Pop args first, then callee
# args = [self.pop() for _ in range(argc)]
# args.reverse()
# callee = self.pop()
# if isinstance(callee, Closure):
#     fn = callee.fn
#     if op == Opcode.TAILCALL:
#         # Proper TCO: replace the current frame in-place without growing the call stack
#         base = frame.base
#         # Trim stack to the current frame's base
#         if len(self.stack) > base:
#             del self.stack[base:]
#         # Bind arguments into the same stack segment as new locals
#         if getattr(fn, 'rest', False):
#             fixed = getattr(fn, 'arity', 0)
#             # push fixed args
#             for val in args[:fixed]:
#                 self.stack.append(val)
#             # pack remaining into a list for the rest param
#             self.stack.append(list(args[fixed:]))
#         else:
#             for val in args:
#                 self.stack.append(val)
#         # Reinitialize current frame to execute the callee
#         frame.chunk = fn.chunk
#         frame.ip = 0
#         frame.base = base
#         frame.env_cells = callee.upvalues
#     else:
#         # Normal call: push a new frame and grow the stack with args
#         new_frame = Frame(chunk=fn.chunk, ip=0, base=len(self.stack), env_cells=callee.upvalues)
#         if getattr(fn, 'rest', False):
#             fixed = getattr(fn, 'arity', 0)
#             for val in args[:fixed]:
#                 self.stack.append(val)
#             self.stack.append(list(args[fixed:]))
#         else:
#             for val in args:
#                 self.stack.append(val)
#         self.frames.append(new_frame)
# elif callable(callee):
#     # Host call: prefer Zeta builtin protocol (env, args) then fallback to positional
#     from zeta.runtime_context import set_current_macros as _set_macros
#
#     try:
#         _set_macros(self.macros)
#         try:
#             try:
#                 result = callee(self.env, args)
#             except TypeError:
#                 # Only fallback to positional if callee doesn't look like a (env, args) function
#                 if hasattr(callee, "__code__") and getattr(callee.__code__, "co_argcount", 0) >= 2:
#                     raise
#                 result = callee(*args)
#         except ContinuationEscape as esc:
#             # On continuation escape, unwind to the nearest handler
#             if not self.cc_handlers:
#                 raise
#             depth, base0 = self.cc_handlers.pop()
#             # Pop frames until we are back to the handler's frame depth
#             while len(self.frames) - 1 > depth:
#                 self.frames.pop()
#             # Trim stack to recorded base and push the escape value
#             if len(self.stack) > base0:
#                 del self.stack[base0:]
#             self.push(esc.value)
#             # Continue execution in the current frame after the original call
#             continue
#         except Exception as ex:
#             # Map Python exceptions to a generic 'error condition; route to VM handler
#             if not self._unwind_to_handler(ex):
#                 # No handler installed: re-raise original exception
#                 raise
#             # Control transferred to handler; continue dispatch loop
#             continue
#         if op == Opcode.TAILCALL:
#             # Tail-call into host callable: replace current frame
#             base = frame.base
#             self.frames.pop()
#             if len(self.stack) > base:
#                 del self.stack[base:]
#             if not self.frames:
#                 return result
#             self.push(result)
#         else:
#             self.push(result)
#     finally:
#         _set_macros(None)
# else:
#     raise TypeError(f"Cannot call {type(callee)}")
#
# # Simple list ops
# elif op == Opcode.CONS:
# tail = self.pop()
# head = self.pop()
# self.push([head] + (tail if isinstance(tail, list) else [tail]))
#
# elif op == Opcode.LIST:
# n = code[frame.ip]
# frame.ip += 1
# items = [self.pop() for _ in range(n)]
# items.reverse()
# self.push(items)
#
# elif op == Opcode.APPEND:
# b = self.pop()
# a = self.pop()
# self.push((a if isinstance(a, list) else [a]) + (b if isinstance(b, list) else [b]))
#
# # Arithmetic via Python operators or env callables
# elif op in (Opcode.ADD, Opcode.SUB, Opcode.MUL, Opcode.DIV, Opcode.MOD,
#             Opcode.LT, Opcode.LE, Opcode.GT, Opcode.GE, Opcode.EQ, Opcode.NEQ):
# b = self.pop()
# a = self.pop()
# if op == Opcode.ADD:
#     self.push(a + b)
# elif op == Opcode.SUB:
#     self.push(a - b)
# elif op == Opcode.MUL:
#     self.push(a * b)
# elif op == Opcode.DIV:
#     self.push(a / b)
# elif op == Opcode.MOD:
#     self.push(a % b)
# elif op == Opcode.LT:
#     self.push(a < b)
# elif op == Opcode.LE:
#     self.push(a <= b)
# elif op == Opcode.GT:
#     self.push(a > b)
# elif op == Opcode.GE:
#     self.push(a >= b)
# elif op == Opcode.EQ:
#     self.push(a == b)
# elif op == Opcode.NEQ:
#     self.push(a != b)
#
# elif op == Opcode.NOT:
#     v = self.pop()
# self.push(not v)