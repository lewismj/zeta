from __future__ import annotations

from zeta import LispValue
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.symbol import Symbol

from zeta.evaluation.evaluator import evaluate0  # reuse for macroexpansion guards
from zeta.evaluation.special_forms.defmacro_form import defmacro_form
from zeta.evaluation.special_forms.gensym_form import gensym_form
from zeta.evaluation.special_forms.import_form import import_form
from .compiler import compile_module
from .vm import run_chunk, run_chunk_backend
from .disasm import disassemble_chunk
import os


class BytecodeCompiler:
    """
    Compile S-expressions to VM bytecode chunks.
    This class performs macro expansion without requiring a caller-provided
    Environment. It should not be treated as a runtime backend.
    """

    def compile(self, expr, macros: MacroEnvironment):
        # Handle compile-time-affecting special forms directly on macros/env-less basis
        if isinstance(expr, list) and expr and isinstance(expr[0], Symbol):
            head: Symbol = expr[0]
            tail = expr[1:]
            if head.id == "defmacro":
                # Register macro directly via special form handler (no env needed)
                return defmacro_form(tail, None, macros, evaluate0, False)
            if head.id == "gensym":
                # Generate symbol directly via special form handler (state in MacroEnvironment)
                return gensym_form(tail, None, macros, evaluate0, False)
            if head.id == "import":
                # Import modifies runtime Environment; not meaningful at pure compile step.
                # Still allow it here with a fresh Environment for compatibility.
                return import_form(tail, Environment(), macros, evaluate0, False)

        # Macro expand with a temporary Environment, since expansion may evaluate templates.
        temp_env = Environment()
        expanded = macros.macro_expand_all(expr, evaluate0, temp_env) if macros is not None else expr
        chunk = compile_module(expanded)
        if os.environ.get("ZETA_DISASM"):
            try:
                print("=== DISASM ===")
                print(disassemble_chunk(chunk))
                print("=== END DISASM ===")
            except Exception:
                pass
        return chunk


def _pre_handle_special_forms(expr, env: Environment, macros: MacroEnvironment) -> LispValue | None:
    # Handle special forms that mutate macro env or runtime env before compiling
    if isinstance(expr, list) and expr and isinstance(expr[0], Symbol):
        head: Symbol = expr[0]
        tail = expr[1:]
        if head.id == "defmacro":
            return defmacro_form(tail, env, macros, evaluate0, False)
        if head.id == "gensym":
            return gensym_form(tail, env, macros, evaluate0, False)
        if head.id == "import":
            return import_form(tail, env, macros, evaluate0, False)
    return None


def _compile_expr(expr, macros: MacroEnvironment):
    compiler = BytecodeCompiler()
    return compiler.compile(expr, macros)


def eval_via_vm(expr, env: Environment, macros: MacroEnvironment) -> LispValue:
    """Adapter function to use the VM from the Interpreter by providing an eval_fn.
    Compiles expr using BytecodeCompiler and executes it on the selected VM.
    """
    handled = _pre_handle_special_forms(expr, env, macros)
    if handled is not None:
        return handled
    chunk_or_nil = _compile_expr(expr, macros)
    # If compile() returned a value (e.g., defmacro/gensym), just return it
    from zeta.types.nil import Nil
    if not hasattr(chunk_or_nil, "code"):
        return chunk_or_nil if chunk_or_nil is not None else Nil
    chunk = chunk_or_nil
    return run_chunk(chunk, env, macros)


def eval_via_vm_backend(expr, env: Environment, macros: MacroEnvironment, backend: str = "py") -> LispValue:
    """Like eval_via_vm but forcing a specific backend ('py' or 'cy')."""
    handled = _pre_handle_special_forms(expr, env, macros)
    if handled is not None:
        return handled
    chunk_or_nil = _compile_expr(expr, macros)
    from zeta.types.nil import Nil
    if not hasattr(chunk_or_nil, "code"):
        return chunk_or_nil if chunk_or_nil is not None else Nil
    chunk = chunk_or_nil
    return run_chunk_backend(chunk, env, macros, backend=backend)
