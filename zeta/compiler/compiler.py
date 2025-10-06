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
    self_name: Symbol | None = None
    param_ids: set[str] | None = None
    upvalue_ids: dict[str, int] | None = None


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
                        # Match by id even if Symbol instances differ or keys are non-Symbol but stringify to same id
                        key_id = sym_k.id if isinstance(sym_k, Symbol) else (str(sym_k) if hasattr(sym_k, "__str__") else None)
                        if key_id == expr.id:
                            chunk.emit_op(Opcode.LOAD_UPVALUE)
                            chunk.emit_u16(idx)
                            resolved = True
                            break
                # If still not resolved but the symbol matches a known parameter id and we have upvalues,
                # try to resolve by id against upvalues map (defensive against instance/key mismatch)
                if not resolved and ctx.upvalues is not None and ctx.param_ids is not None and expr.id in ctx.param_ids:
                    for sym_k, idx in ctx.upvalues.items():
                        key_id = sym_k.id if isinstance(sym_k, Symbol) else (str(sym_k) if hasattr(sym_k, "__str__") else None)
                        if key_id == expr.id:
                            chunk.emit_op(Opcode.LOAD_UPVALUE)
                            chunk.emit_u16(idx)
                            resolved = True
                            break
                # Final fallback: resolve by id using precomputed upvalue id map
                if not resolved and ctx.upvalue_ids is not None:
                    idx = ctx.upvalue_ids.get(expr.id)
                    if idx is not None:
                        chunk.emit_op(Opcode.LOAD_UPVALUE)
                        chunk.emit_u16(idx)
                        resolved = True
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
        if head.id == "import":
            # Compile (import ...) to a call of builtin __import_special__ to perform Python module import
            cidx = chunk.add_const(Symbol("__import_special__"))
            chunk.emit_op(Opcode.LOAD_GLOBAL)
            chunk.emit_u16(cidx)
            for arg in tail:
                # Keep keywords 'as' and 'helpers' as literal symbols
                if isinstance(arg, Symbol) and arg.id in ("as", "helpers"):
                    _emit_push_const(chunk, arg)
                else:
                    compile_expr(arg, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues))
            argc = len(tail)
            if ctx.in_tail:
                chunk.emit_op(Opcode.TAILCALL)
                chunk.emit_u8(argc)
            else:
                chunk.emit_op(Opcode.CALL)
                chunk.emit_u8(argc)
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
                # default clause if test is #t or 'else
                if isinstance(test, Symbol) and (test.id == "#t" or test.id == "else"):
                    return make_progn(body)
                return [Symbol("if"), test, make_progn(body), cond_to_if(cs[1:])]
            rewritten = cond_to_if(clauses)
            compile_expr(rewritten, chunk, ctx)
            return
        if head.id == "condition-case":
            # Fallback to classic evaluator for complex special form
            cidx = chunk.add_const(Symbol("__eval_list__"))
            chunk.emit_op(Opcode.LOAD_GLOBAL)
            chunk.emit_u16(cidx)
            _emit_push_const(chunk, expr)
            if ctx.in_tail:
                chunk.emit_op(Opcode.TAILCALL)
                chunk.emit_u8(1)
            else:
                chunk.emit_op(Opcode.CALL)
                chunk.emit_u8(1)
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
            # Detect &rest and &optional parameters
            rest_flag = False
            fixed_params: list[Symbol] = []
            rest_param: Symbol | None = None
            optional_specs: list[tuple[Symbol, Any | None]] = []  # (name, defaultExpr or None)
            i = 0
            while i < len(params):
                p = params[i]
                if isinstance(p, Symbol) and p.id in ("&rest", "&body"):
                    # Next item must be the rest parameter name
                    if i + 1 >= len(params) or not isinstance(params[i + 1], Symbol):
                        raise SyntaxError("&rest/&body must be followed by a symbol param name")
                    rest_flag = True
                    rest_param = params[i + 1]
                    i += 2
                    if i != len(params):
                        raise SyntaxError("&rest/&body must appear at the end of parameter list")
                    break
                if isinstance(p, Symbol) and p.id == "&optional":
                    # Parse optional specs until end
                    i += 1
                    while i < len(params):
                        spec = params[i]
                        if isinstance(spec, list):
                            if len(spec) != 2 or not isinstance(spec[0], Symbol):
                                raise SyntaxError("&optional spec must be (name default)")
                            optional_specs.append((spec[0], spec[1]))
                        elif isinstance(spec, Symbol):
                            optional_specs.append((spec, None))
                        else:
                            raise SyntaxError("lambda params must be symbols")
                        i += 1
                    break
                if not isinstance(p, Symbol):
                    raise SyntaxError("lambda params must be symbols")
                fixed_params.append(p)
                i += 1
            # If we have optional specs, capture remaining args into a synthetic rest list
            synthetic_rest: Symbol | None = None
            if optional_specs and not rest_flag:
                rest_flag = True
                synthetic_rest = Symbol("__optrest")
                rest_param = synthetic_rest
            # Compile function body and compute free variables for closure capture
            fn_chunk = Chunk()
            # Map parameters to local indices
            local_map: dict[Symbol, int] = {p: idx for idx, p in enumerate(fixed_params)}
            # Assign slot for rest/synthetic rest
            if rest_flag and rest_param is not None:
                local_map[rest_param] = len(fixed_params)
            # Assign slots for optional params after rest slot (if any)
            opt_start = len(fixed_params) + (1 if rest_flag and rest_param is not None else 0)
            for j, (opt_name, _default) in enumerate(optional_specs):
                local_map[opt_name] = opt_start + j

            # Initialize optional parameters from rest list or defaults
            if optional_specs:
                # Cache indices for globals used (in function chunk)
                sym_null = fn_chunk.add_const(Symbol("null?"))
                sym_car = fn_chunk.add_const(Symbol("car"))
                sym_cdr = fn_chunk.add_const(Symbol("cdr"))
                assert rest_param is not None
                rest_slot_idx = local_map[rest_param]
                for j, (opt_name, default_expr) in enumerate(optional_specs):
                    opt_slot_idx = local_map[opt_name]
                    # if (null? rest) -> use default; else take (car rest) and rest := (cdr rest)
                    fn_chunk.emit_op(Opcode.LOAD_GLOBAL)
                    fn_chunk.emit_u16(sym_null)
                    fn_chunk.emit_op(Opcode.LOAD_LOCAL)
                    fn_chunk.emit_u16(rest_slot_idx)
                    fn_chunk.emit_op(Opcode.CALL)
                    fn_chunk.emit_u8(1)
                    fn_chunk.emit_op(Opcode.JUMP_IF_TRUE)
                    j_def = len(fn_chunk.code)
                    fn_chunk.emit_s16(0)
                    # provided path: value = (car rest)
                    fn_chunk.emit_op(Opcode.LOAD_GLOBAL)
                    fn_chunk.emit_u16(sym_car)
                    fn_chunk.emit_op(Opcode.LOAD_LOCAL)
                    fn_chunk.emit_u16(rest_slot_idx)
                    fn_chunk.emit_op(Opcode.CALL)
                    fn_chunk.emit_u8(1)
                    fn_chunk.emit_op(Opcode.STORE_LOCAL)
                    fn_chunk.emit_u16(opt_slot_idx)
                    fn_chunk.emit_op(Opcode.POP)
                    # rest = (cdr rest)
                    fn_chunk.emit_op(Opcode.LOAD_GLOBAL)
                    fn_chunk.emit_u16(sym_cdr)
                    fn_chunk.emit_op(Opcode.LOAD_LOCAL)
                    fn_chunk.emit_u16(rest_slot_idx)
                    fn_chunk.emit_op(Opcode.CALL)
                    fn_chunk.emit_u8(1)
                    fn_chunk.emit_op(Opcode.STORE_LOCAL)
                    fn_chunk.emit_u16(rest_slot_idx)
                    fn_chunk.emit_op(Opcode.POP)
                    # jump to end
                    fn_chunk.emit_op(Opcode.JUMP)
                    j_end = len(fn_chunk.code)
                    fn_chunk.emit_s16(0)
                    # default path target
                    def_tgt = len(fn_chunk.code)
                    if default_expr is None:
                        fn_chunk.emit_op(Opcode.PUSH_NIL)
                    else:
                        compile_expr(default_expr, fn_chunk, CompileCtx(False, locals=local_map))
                    fn_chunk.emit_op(Opcode.STORE_LOCAL)
                    fn_chunk.emit_u16(opt_slot_idx)
                    fn_chunk.emit_op(Opcode.POP)
                    # patch jumps
                    rel_def = def_tgt - (j_def + 2)
                    fn_chunk.patch_s16_at(j_def, rel_def)
                    end_tgt = len(fn_chunk.code)
                    rel_end = end_tgt - (j_end + 2)
                    fn_chunk.patch_s16_at(j_end, rel_end)

            # Determine free variables w.r.t. enclosing scope
            enclosing_locals = ctx.locals or {}
            enclosing_upvals = ctx.upvalues or {}

            def find_free_vars(expr_any: Any, bound: set[Symbol]) -> set[Symbol]:
                out: set[Symbol] = set()
                def walk(e: Any, bound_inner: set[Symbol]):
                    if isinstance(e, Symbol):
                        # Treat parameter names as bound even if the Symbol instance differs; compare by id
                        is_bound = (e in bound_inner) or any((isinstance(b, Symbol) and b.id == e.id) for b in bound_inner)
                        if (not is_bound) and (e in enclosing_locals or e in enclosing_upvals) and not e.id.startswith(":"):
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
            # At top-level (no enclosing locals/upvalues), no free variables can be captured
            if not enclosing_locals and not enclosing_upvals:
                free_vars = []
            else:
                free_vars = list(find_free_vars(body, set(local_map.keys())))
            # Extra safety: never treat parameters as free vars (by id)
            if free_vars:
                param_ids = {p.id for p in fixed_params}
                free_vars = [uv for uv in free_vars if not (isinstance(uv, Symbol) and uv.id in param_ids)]
            # If all detected free vars are actually parameter-name duplicates by id, ignore them
            if free_vars:
                param_ids = {p.id for p in fixed_params}
                if all(isinstance(uv, Symbol) and uv.id in param_ids for uv in free_vars):
                    free_vars = []
            # Augment free_vars by including any enclosing-local symbols referenced by id in the body (robust against instance mismatch)
            if enclosing_locals:
                ref_ids: set[str] = set()
                def collect_ids(e: Any):
                    if isinstance(e, Symbol):
                        ref_ids.add(e.id)
                        return
                    if isinstance(e, list):
                        for x in e:
                            collect_ids(x)
                    elif isinstance(e, tuple) and len(e) == 2:
                        a,b = e
                        collect_ids(a)
                        collect_ids(b)
                collect_ids([Symbol("progn"), *body])
                for sym_k in list(enclosing_locals.keys()):
                    if isinstance(sym_k, Symbol) and sym_k.id in ref_ids:
                        # ensure presence in free_vars
                        if all(not (isinstance(fv, Symbol) and fv.id == sym_k.id) for fv in free_vars):
                            free_vars.append(sym_k)
            # Exclude recursive self-reference (name being defined) from free vars
            if ctx.self_name is not None:
                free_vars = [uv for uv in free_vars if not (isinstance(uv, Symbol) and uv.id == ctx.self_name.id)]
            # Debug: print lambda capture info for simplify-bool* when disassembly is enabled
            import os as _os_dbg
            if _os_dbg.environ.get("ZETA_DISASM") and ctx.self_name is not None and isinstance(ctx.self_name, Symbol) and ctx.self_name.id == "simplify-bool*":
                try:
                    print("[DEBUG lambda] name=", ctx.self_name.id,
                          " params=", [p.id for p in fixed_params],
                          " free_vars=", [fv.id if isinstance(fv, Symbol) else str(fv) for fv in free_vars],
                          sep="")
                except Exception:
                    pass
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
            # Precompute upvalue id map for robust resolution inside the function body
            up_ids = None
            if child_up_map:
                up_ids = {}
                for k, v in child_up_map.items():
                    kid = k.id if isinstance(k, Symbol) else (str(k) if hasattr(k, "__str__") else None)
                    if kid is not None:
                        up_ids[kid] = v
            for i, expr_i in enumerate(body):
                compile_expr(expr_i, fn_chunk, CompileCtx(in_tail=(i == len(body) - 1), locals=local_map, upvalues=child_up_map, self_name=ctx.self_name, param_ids={p.id for p in fixed_params}, upvalue_ids=up_ids))
                if i < len(body) - 1:
                    fn_chunk.emit_op(Opcode.POP)
            fn_chunk.emit_op(Opcode.RETURN)
            # Optional debug: disassemble function chunk for named lambdas to aid VM debugging
            try:
                import os as _os_dbg2
                from .disasm import disassemble_chunk as _dis
                if _os_dbg2.environ.get("ZETA_DISASM_NAME") and ctx.self_name is not None:
                    print(f"=== FUNC DISASM: {ctx.self_name.id} ===")
                    print(_dis(fn_chunk))
                    print("=== END FUNC DISASM ===")
            except Exception:
                pass
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
            # Pass the self_name into lambda compilation to support proper recursion handling
            # Special-case: if compiling a top-level (no enclosing locals/upvalues) lambda, force MAKE_LAMBDA emission by
            # clearing any accidental free-var captures (ensures params are locals and avoids wrapper closures).
            if isinstance(val_expr, list) and val_expr and isinstance(val_expr[0], Symbol) and val_expr[0].id == "lambda" and not (ctx.locals or ctx.upvalues):
                # Compile lambda with an empty enclosing context and mark self_name for recursion
                compile_expr(val_expr, chunk, CompileCtx(False, locals=None, upvalues=None, self_name=name))
            else:
                compile_expr(val_expr, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues, self_name=name))
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
    # Handle non-symbol heads according to evaluator semantics:
    # - If head is a list (e.g., (lambda (...) ...) ...), evaluate it to a callable and apply.
    # - If head is a non-list atom (number, string, etc.), treat the whole form as data literal.
    if not isinstance(head, Symbol):
        if isinstance(head, list):
            # Evaluate callee expression then args, then CALL
            compile_expr(head, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues))
            for arg in tail:
                compile_expr(arg, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues))
            argc = len(tail)
            if ctx.in_tail:
                chunk.emit_op(Opcode.TAILCALL)
                chunk.emit_u8(argc)
            else:
                chunk.emit_op(Opcode.CALL)
                chunk.emit_u8(argc)
            return
        else:
            _emit_push_const(chunk, expr)
            return
    # Special-case colon-qualified heads (e.g., df:sum, np:dot): delegate to builtin __colon_call__
    if isinstance(head, Symbol) and (":" in head.id) and head.id != "/":
        # Load the helper __colon_call__ as callee
        cidx = chunk.add_const(Symbol("__colon_call__"))
        chunk.emit_op(Opcode.LOAD_GLOBAL)
        chunk.emit_u16(cidx)
        # First argument to helper is the head symbol itself
        _emit_push_const(chunk, head)
        # Then compile and push user arguments
        for arg in tail:
            compile_expr(arg, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues, self_name=ctx.self_name, param_ids=ctx.param_ids, upvalue_ids=ctx.upvalue_ids))
        argc = len(tail) + 1
        if ctx.in_tail:
            chunk.emit_op(Opcode.TAILCALL)
            chunk.emit_u8(argc)
        else:
            chunk.emit_op(Opcode.CALL)
            chunk.emit_u8(argc)
        return
    # General case: evaluate callee then args left-to-right
    compile_expr(head, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues, self_name=ctx.self_name, param_ids=ctx.param_ids, upvalue_ids=ctx.upvalue_ids))
    for arg in tail:
        compile_expr(arg, chunk, CompileCtx(False, locals=ctx.locals, upvalues=ctx.upvalues, self_name=ctx.self_name, param_ids=ctx.param_ids, upvalue_ids=ctx.upvalue_ids))
    argc = len(tail)
    if ctx.in_tail:
        chunk.emit_op(Opcode.TAILCALL)
        chunk.emit_u8(argc)
    else:
        chunk.emit_op(Opcode.CALL)
        chunk.emit_u8(argc)
