from __future__ import annotations

from zeta.interpreter.backend import Backend
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta import LispValue

from zeta.evaluation.evaluator import evaluate0  # reuse for macroexpansion guards
from zeta.types.symbol import Symbol
from zeta.evaluation.special_forms.defmacro_form import defmacro_form
from zeta.evaluation.special_forms.gensym_form import gensym_form
from .compiler import compile_module
from .vm import run_chunk
import os
from .disasm import disassemble_chunk


class BytecodeBackend:
    """
    Compiler backend: macroexpands the expression, compiles to bytecode, and runs it on the VM.
    """
    prefer_scalar_multi = True

    def _contains_call_cc(self, form) -> bool:
        if isinstance(form, list) and form:
            h = form[0]
            if isinstance(h, Symbol) and h.id == "call/cc":
                return True
            # Recurse into list elements
            return any(self._contains_call_cc(x) for x in form)
        if isinstance(form, tuple) and len(form) == 2:
            a, b = form
            return self._contains_call_cc(a) or self._contains_call_cc(b)
        return False

    def eval(self, expr, env: Environment, macros: MacroEnvironment) -> LispValue:
        # Fast-path known special forms to their handlers to avoid full evaluator dispatch
        if isinstance(expr, list) and expr and isinstance(expr[0], Symbol):
            head: Symbol = expr[0]
            tail = expr[1:]
            if head.id == "defmacro":
                # Register macro directly via special form handler
                return defmacro_form(tail, env, macros, evaluate0, False)
            if head.id == "gensym":
                # Generate symbol directly via special form handler (state in MacroEnvironment)
                return gensym_form(tail, env, macros, evaluate0, False)
            if head.id == "import":
                # Handle Python interop import as a special form in VM path too
                from zeta.evaluation.special_forms.import_form import import_form
                return import_form(tail, env, macros, evaluate0, False)
            if head.id == "condition-case":
                # Delegate to the existing special form handler for condition-case
                from zeta.evaluation.special_forms.throw_catch_form import condition_case_form
                return condition_case_form(tail, env, macros, evaluate0, False)

        # Expand macros using the existing system to preserve semantics
        expanded = expr
        if macros is not None:
            expanded = macros.macro_expand_all(expr, evaluate0, env)

        chunk = compile_module(expanded)
        if os.environ.get("ZETA_DISASM"):
            try:
                print("=== DISASM ===")
                print(disassemble_chunk(chunk))
                print("=== END DISASM ===")
            except Exception:
                pass
        result = run_chunk(chunk, env, macros)
        return result
