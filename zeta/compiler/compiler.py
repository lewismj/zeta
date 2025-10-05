from __future__ import annotations

from dataclasses import dataclass
from typing import Any, List

from zeta.types.symbol import Symbol
from zeta.types.nil import Nil

from .opcodes import Opcode
from .chunk import Chunk
from .function import Function, UpvalueDescriptor


@dataclass
class CompileCtx:
    in_tail: bool = False
    locals: dict[Symbol, int] | None = None
    upvalues: dict[Symbol, int] | None = None


def compile_module(expr: Any) -> Chunk:
    """Compile a single top-level expression into a chunk that returns its value.
    For now, treat 'expr' as a single form. The caller (Interpreter) already splits.
    """
    chunk = Chunk()
    compile_expr(expr, chunk, CompileCtx(in_tail=False))
    # For top-level, implicit HALT
    chunk.emit_op(Opcode.HALT)
    return chunk


def _emit_push_const(chunk: Chunk, v: Any) -> None:
    idx = chunk.add_const(v)
    chunk.emit_op(Opcode.PUSH_CONST)
    chunk.emit_u16(idx)


def compile_expr(expr: Any, chunk: Chunk, ctx: CompileCtx) -> None:
    # Atoms
    if isinstance(expr, (int, float, str)) or expr is Nil:
        _emit_push_const(chunk, expr)
        return
    if isinstance(expr, Symbol):
        # Keywords self-evaluate
        if expr.id.startswith(":"):
            _emit_push_const(chunk, expr)
        else:
            # If symbol is a local parameter, load from local slot
            if ctx.locals is not None and expr in ctx.locals:
                chunk.emit_op(Opcode.LOAD_LOCAL)
                chunk.emit_u16(ctx.locals[expr])
            elif ctx.upvalues is not None and expr in ctx.upvalues:
                chunk.emit_op(Opcode.LOAD_UPVALUE)
                chunk.emit_u16(ctx.upvalues[expr])
            else:
                # Fallback by symbol id match to be robust against distinct-but-equal Symbol instances
                resolved = False
                if ctx.locals is not None:
                    for sym_k, idx in ctx.locals.items():
                        if isinstance(sym_k, Symbol) and sym_k.id == expr.id:
                            chunk.emit_op(Opcode.LOAD_LOCAL)
                            chunk.emit_u16(idx)
                            resolved = True
                            break
                if not resolved and ctx.upvalues is not None:
                    for sym_k, idx in ctx.upvalues.items():
                        # upvalues map is Symbol->int
                        if isinstance(sym_k, Symbol) and sym_k.id == expr.id:
                            chunk.emit_op(Opcode.LOAD_UPVALUE)
                            chunk.emit_u16(idx)
                            resolved = True
                            break
                if not resolved:
                    # load from globals for MVP
                    gidx = chunk.add_const(expr)
                    chunk.emit_op(Opcode.LOAD_GLOBAL)
                    chunk.emit_u16(gidx)
        return

    if not isinstance(expr, list) or not expr:
        _emit_push_const(chunk, expr)
        return

    head, *tail = expr
    # Special forms (subset for MVP)
    if isinstance(head, Symbol):
        if head.id in ("quote",):
            # (quote x)
            if len(tail) != 1:
                raise SyntaxError("quote takes exactly one argument")
            _emit_push_const(chunk, tail[0])
            return
        if head.id in ("progn", "begin"):
            for i, sub in enumerate(tail):
                compile_expr(sub, chunk, CompileCtx(in_tail=(ctx.in_tail and i == len(tail) - 1), locals=ctx.locals, upvalues=ctx.upvalues))
                if i < len(tail) - 1:
                    chunk.emit_op(Opcode.POP)
            return
        if head.id == "if":
            if len(tail) not in (2, 3):
                raise SyntaxError("if requires 2 or 3 arguments")
            test = tail[0]
            conseq = tail[1]
            alt = tail[2] if len(tail) == 3 else Nil
            compile_expr(test, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues))
            chunk.emit_op(Opcode.JUMP_IF_FALSE)
            jfalse_pos = len(chunk.code)  # points to first operand byte
            chunk.emit_s16(0)  # placeholder
            # then branch
            compile_expr(conseq, chunk, CompileCtx(ctx.in_tail, locals=ctx.locals, upvalues=ctx.upvalues))
            chunk.emit_op(Opcode.JUMP)
            jend_pos = len(chunk.code)  # operand start
            chunk.emit_s16(0)
            # else label target is current end
            # patch JUMP_IF_FALSE to jump to else label; rel measured from after operand
            rel = (len(chunk.code) - (jfalse_pos + 2))
            chunk.patch_s16_at(jfalse_pos, rel)
            # else branch
            compile_expr(alt, chunk, CompileCtx(ctx.in_tail, locals=ctx.locals, upvalues=ctx.upvalues))
            # end label target
            rel2 = (len(chunk.code) - (jend_pos + 2))
            chunk.patch_s16_at(jend_pos, rel2)
            return
        if head.id == "cond":
            # Rewrite cond into nested if/progn AST and compile that
            clauses = tail
            def make_progn(exprs: list[Any]) -> Any:
                if not exprs:
                    return Nil
                if len(exprs) == 1:
                    return exprs[0]
                return [Symbol("progn"), *exprs]
            def cond_to_if(cs: list[Any]) -> Any:
                if not cs:
                    return Nil
                first = cs[0]
                if not isinstance(first, list) or len(first) == 0:
                    raise SyntaxError("cond clause must be a non-empty list")
                test = first[0]
                body = first[1:]
                # default clause if test is #t
                if isinstance(test, Symbol) and test.id == "#t":
                    return make_progn(body)
                return [Symbol("if"), test, make_progn(body), cond_to_if(cs[1:])]
            rewritten = cond_to_if(clauses)
            compile_expr(rewritten, chunk, ctx)
            return
        if head.id == "and":
            # Short-circuit AND: (and a b c ...)
            if not tail:
                _emit_push_const(chunk, Symbol("#t"))
                return
            end_false_pos = None
            # Evaluate operands except the last with short-circuit on false
            for i, sub in enumerate(tail[:-1]):
                compile_expr(sub, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues))
                chunk.emit_op(Opcode.JUMP_IF_FALSE)
                jpos = len(chunk.code)
                chunk.emit_s16(0)
                # If truthy, continue; falsey jumps to end_false
                if end_false_pos is None:
                    end_false_pos = []
                end_false_pos.append(jpos)
            # Last operand produces the result when all previous were truthy
            compile_expr(tail[-1], chunk, CompileCtx(ctx.in_tail, locals=ctx.locals, upvalues=ctx.upvalues))
            # Jump to end to skip pushing #f when last is computed
            chunk.emit_op(Opcode.JUMP)
            jend = len(chunk.code)
            chunk.emit_s16(0)
            # Patch all false jumps to here: push #f
            end_label = len(chunk.code)
            _emit_push_const(chunk, Symbol("#f"))
            if end_false_pos:
                for pos in end_false_pos:
                    rel = end_label - (pos + 2)
                    chunk.patch_s16_at(pos, rel)
            # Patch end jump target
            end2 = len(chunk.code)
            rel2 = end2 - (jend + 2)
            chunk.patch_s16_at(jend, rel2)
            return
        if head.id == "or":
            # Short-circuit OR: (or a b c ...)
            if not tail:
                _emit_push_const(chunk, Symbol("#f"))
                return
            # For all but last: if truthy, keep value and jump to end; else discard and continue
            jumps_to_end: list[int] = []
            for sub in tail[:-1]:
                compile_expr(sub, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues))
                chunk.emit_op(Opcode.DUP)
                chunk.emit_op(Opcode.JUMP_IF_TRUE)
                jpos = len(chunk.code)
                chunk.emit_s16(0)
                # Not truthy: pop the remaining copy and continue
                chunk.emit_op(Opcode.POP)
                jumps_to_end.append(jpos)
            # Last operand: if truthy, jump to end keeping it; else return #f
            compile_expr(tail[-1], chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues))
            chunk.emit_op(Opcode.DUP)
            chunk.emit_op(Opcode.JUMP_IF_TRUE)
            jpos_last = len(chunk.code)
            chunk.emit_s16(0)
            # Not truthy: pop and push #f
            chunk.emit_op(Opcode.POP)
            _emit_push_const(chunk, Symbol("#f"))
            # End label position
            end_label = len(chunk.code)
            # Patch all truthy jumps to end_label
            for pos in jumps_to_end + [jpos_last]:
                rel = end_label - (pos + 2)
                chunk.patch_s16_at(pos, rel)
            # If in tail position, nothing special to do (last value already on stack)
            return
        if head.id == "call/cc":
            # (call/cc f) -> compile f then special CALL_CC
            if len(tail) != 1:
                raise SyntaxError("call/cc expects exactly 1 argument")
            compile_expr(tail[0], chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues))
            chunk.emit_op(Opcode.CALL_CC)
            return
        if head.id == "dotimes":
            # (dotimes (i n) body...)
            if not tail or not isinstance(tail[0], list) or len(tail[0]) < 2 or not isinstance(tail[0][0], Symbol):
                raise SyntaxError("dotimes syntax: (dotimes (var count) body...)")
            var_sym: Symbol = tail[0][0]
            count_expr = tail[0][1]
            body_exprs = tail[1:]
            # Allocate new local slots for loop var and count without clobbering existing locals
            local_map = dict(ctx.locals or {})
            next_slot = (max(local_map.values()) + 1) if local_map else 0
            var_slot = next_slot
            count_slot = var_slot + 1
            local_map[var_sym] = var_slot
            # initial last value
            chunk.emit_op(Opcode.PUSH_NIL)
            # init var = 0
            _emit_push_const(chunk, 0)
            chunk.emit_op(Opcode.STORE_LOCAL)
            chunk.emit_u16(var_slot)
            # evaluate count once and store in local count_slot
            compile_expr(count_expr, chunk, CompileCtx(False, locals=ctx.locals))
            chunk.emit_op(Opcode.STORE_LOCAL)
            chunk.emit_u16(count_slot)
            # check label position
            check_pos = len(chunk.code)
            # condition: i < count
            # load '<' first, then push args (callee-before-args)
            lt_sym_idx = chunk.add_const(Symbol("<"))
            chunk.emit_op(Opcode.LOAD_GLOBAL)
            chunk.emit_u16(lt_sym_idx)
            chunk.emit_op(Opcode.LOAD_LOCAL)
            chunk.emit_u16(var_slot)
            chunk.emit_op(Opcode.LOAD_LOCAL)
            chunk.emit_u16(count_slot)
            chunk.emit_op(Opcode.CALL)
            chunk.emit_u8(2)
            # if false jump to end
            chunk.emit_op(Opcode.JUMP_IF_FALSE)
            jend_pos = len(chunk.code)
            chunk.emit_s16(0)
            # loop body start: drop previous last
            loop_start = len(chunk.code)
            chunk.emit_op(Opcode.POP)
            # compile body expressions with local_map (so var resolves to local 0)
            for i, bexpr in enumerate(body_exprs):
                compile_expr(bexpr, chunk, CompileCtx(in_tail=False, locals=local_map, upvalues=ctx.upvalues))
                if i < len(body_exprs) - 1:
                    chunk.emit_op(Opcode.POP)
            # increment i: i = (+ i 1)
            plus_sym_idx = chunk.add_const(Symbol("+"))
            chunk.emit_op(Opcode.LOAD_GLOBAL)
            chunk.emit_u16(plus_sym_idx)
            chunk.emit_op(Opcode.LOAD_LOCAL)
            chunk.emit_u16(var_slot)
            _emit_push_const(chunk, 1)
            chunk.emit_op(Opcode.CALL)
            chunk.emit_u8(2)
            chunk.emit_op(Opcode.STORE_LOCAL)
            chunk.emit_u16(var_slot)
            chunk.emit_op(Opcode.POP)
            # jump back to check
            chunk.emit_op(Opcode.JUMP)
            rel_back_pos = len(chunk.code)
            chunk.emit_s16(0)
            # patch jump back to negative offset from after operand to check_pos
            rel_back = check_pos - (rel_back_pos + 2)
            if rel_back < -32768 or rel_back > 32767:
                raise OverflowError("jump offset out of range")
            chunk.patch_s16_at(rel_back_pos, rel_back)
            # patch end jump target
            end_pos = len(chunk.code)
            rel_end = end_pos - (jend_pos + 2)
            chunk.patch_s16_at(jend_pos, rel_end)
            return
        if head.id == "let":
            # (let ((v1 e1) (v2 e2) ...) body...)
            if not tail or not isinstance(tail[0], list):
                raise SyntaxError("let requires a bindings list and body")
            bindings = tail[0]
            body_exprs = tail[1:] if len(tail) > 1 else [Nil]
            # Prepare new locals mapping: allocate slots for each binding
            new_locals = dict(ctx.locals or {})
            next_slot = (max(new_locals.values()) + 1) if new_locals else 0
            slots: list[tuple[Symbol, int]] = []
            for b in bindings:
                if not isinstance(b, list) or len(b) != 2 or not isinstance(b[0], Symbol):
                    raise SyntaxError("let binding must be (name value)")
                name: Symbol = b[0]
                new_locals[name] = next_slot
                slots.append((name, next_slot))
                next_slot += 1
            # Initialize each binding in the outer context (let does not see previous bindings in this group)
            for (name, slot), b in zip(slots, bindings):
                val_expr = b[1]
                compile_expr(val_expr, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues))
                chunk.emit_op(Opcode.STORE_LOCAL)
                chunk.emit_u16(slot)
                chunk.emit_op(Opcode.POP)
            # Compile body with extended locals
            for i, bexpr in enumerate(body_exprs):
                compile_expr(bexpr, chunk, CompileCtx(in_tail=(ctx.in_tail and i == len(body_exprs) - 1), locals=new_locals, upvalues=ctx.upvalues))
                if i < len(body_exprs) - 1:
                    chunk.emit_op(Opcode.POP)
            return
        if head.id == "lambda":
            # (lambda (params...) body...)
            if not tail:
                raise SyntaxError("lambda requires parameter list and body")
            params = tail[0]
            body = tail[1:] if len(tail) > 1 else [Nil]
            if not isinstance(params, list):
                raise SyntaxError("lambda parameter list must be a list")
            # Detect &rest parameter: (... &rest name)
            rest_flag = False
            fixed_params: list[Symbol] = []
            rest_param: Symbol | None = None
            i = 0
            while i < len(params):
                p = params[i]
                if not isinstance(p, Symbol):
                    raise SyntaxError("lambda params must be symbols")
                if p.id in ("&rest", "&body"):
                    # Next item must be the rest parameter name
                    if i + 1 >= len(params) or not isinstance(params[i + 1], Symbol):
                        raise SyntaxError("&rest/&body must be followed by a symbol param name")
                    rest_flag = True
                    rest_param = params[i + 1]
                    i += 2
                    if i != len(params):
                        raise SyntaxError("&rest/&body must appear at the end of parameter list")
                    break
                else:
                    fixed_params.append(p)
                    i += 1
            # Compile function body and compute free variables for closure capture
            fn_chunk = Chunk()
            # Map parameters to local indices
            local_map: dict[Symbol, int] = {p: idx for idx, p in enumerate(fixed_params)}
            if rest_flag and rest_param is not None:
                local_map[rest_param] = len(fixed_params)

            # Determine free variables w.r.t. enclosing scope
            enclosing_locals = ctx.locals or {}
            enclosing_upvals = ctx.upvalues or {}

            def find_free_vars(expr_any: Any, bound: set[Symbol]) -> set[Symbol]:
                out: set[Symbol] = set()
                def walk(e: Any, bound_inner: set[Symbol]):
                    if isinstance(e, Symbol):
                        if e not in bound_inner and (e in enclosing_locals or e in enclosing_upvals) and not e.id.startswith(":"):
                            out.add(e)
                        return
                    if isinstance(e, list) and e:
                        h = e[0]
                        # Do not traverse into nested lambda bodies for this lambda's capture set
                        if isinstance(h, Symbol) and h.id == "lambda":
                            return
                        for x in e:
                            walk(x, bound_inner)
                    elif isinstance(e, tuple) and len(e) == 2:
                        a, b = e
                        walk(a, bound_inner)
                        walk(b, bound_inner)
                    # atoms ignored
                walk([Symbol("progn"), *body], set(local_map.keys()))
                return out
            free_vars = list(find_free_vars(body, set(local_map.keys())))
            # Build upvalue descriptors and child upvalue index map
            up_descs: list[UpvalueDescriptor] = []
            child_up_map: dict[Symbol, int] = {}
            for uv_sym in free_vars:
                if uv_sym in enclosing_locals:
                    up_descs.append(UpvalueDescriptor(is_local=True, index=enclosing_locals[uv_sym]))
                else:
                    up_descs.append(UpvalueDescriptor(is_local=False, index=enclosing_upvals[uv_sym]))
                child_up_map[uv_sym] = len(child_up_map)

            # Compile body with local and upvalue context
            for i, expr_i in enumerate(body):
                compile_expr(expr_i, fn_chunk, CompileCtx(in_tail=(i == len(body) - 1), locals=local_map, upvalues=child_up_map))
                if i < len(body) - 1:
                    fn_chunk.emit_op(Opcode.POP)
            fn_chunk.emit_op(Opcode.RETURN)
            fn = Function(chunk=fn_chunk, arity=len(fixed_params), rest=rest_flag, upvalues=up_descs, name=None)
            fidx = chunk.add_const(fn)
            if up_descs:
                # Emit MAKE_CLOSURE with capture descriptors
                chunk.emit_op(Opcode.MAKE_CLOSURE)
                chunk.emit_u16(fidx)
                chunk.emit_u8(len(up_descs))
                for desc in up_descs:
                    chunk.emit_u8(1 if desc.is_local else 0)
                    chunk.emit_u16(desc.index)
            else:
                chunk.emit_op(Opcode.MAKE_LAMBDA)
                chunk.emit_u16(fidx)
            return
        if head.id == "define":
            # (define name expr)
            if len(tail) != 2 or not isinstance(tail[0], Symbol):
                raise SyntaxError("define syntax: (define <symbol> <expr>)")
            name: Symbol = tail[0]
            val_expr = tail[1]
            compile_expr(val_expr, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues))
            gidx = chunk.add_const(name)
            chunk.emit_op(Opcode.DUP)
            chunk.emit_op(Opcode.STORE_GLOBAL)
            chunk.emit_u16(gidx)
            return
        if head.id in ("set", "set!"):
            if len(tail) != 2 or not isinstance(tail[0], Symbol):
                raise SyntaxError("set!/set syntax: (set! <symbol> <expr>)")
            name: Symbol = tail[0]
            val_expr = tail[1]
            compile_expr(val_expr, chunk, CompileCtx(False, locals=ctx.locals))
            # If the target is a local, store into the local slot; otherwise store global
            if ctx.locals is not None and name in ctx.locals:
                chunk.emit_op(Opcode.STORE_LOCAL)
                chunk.emit_u16(ctx.locals[name])
            else:
                gidx = chunk.add_const(name)
                chunk.emit_op(Opcode.STORE_GLOBAL)
                chunk.emit_u16(gidx)
            # By convention, leave the value on stack
            return

    # Function application
    # Evaluate callee then args left-to-right
    compile_expr(head, chunk, CompileCtx(False, locals=ctx.locals))
    for arg in tail:
        compile_expr(arg, chunk, CompileCtx(False, locals=ctx.locals))
    argc = len(tail)
    if ctx.in_tail:
        chunk.emit_op(Opcode.TAILCALL)
        chunk.emit_u8(argc)
    else:
        chunk.emit_op(Opcode.CALL)
        chunk.emit_u8(argc)
