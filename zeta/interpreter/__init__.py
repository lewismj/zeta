from __future__ import annotations
from typing import Callable, Literal

from zeta import SExpression, LispValue
from zeta.reader.parser import lex, TokenStream
from zeta.types.nil import Nil
from zeta.types.macro_environment import MacroEnvironment
from zeta.types.environment import Environment
from zeta.builtin.env_builtin import register
from zeta.builtin.macro_builtin import register as register_macros



class Interpreter:
    """
    Orchestrates reading and evaluating Zeta code via a pluggable evaluator.
    Maintains an Environment and MacroEnvironment across calls.
    """

    # Class-level defaults to avoid env-variable coupling in tests
    DefaultEngine: Literal['interp', 'vm'] = 'interp'
    DefaultVM: Literal['py', 'cy'] = 'py'

    def __init__(
        self,
        eval_fn: Callable[[SExpression, Environment, MacroEnvironment], LispValue] | None = None,
        prelude: str | None | Literal['auto'] = 'auto',
        *,
        engine: Literal['interp','vm'] | None = None,
        vm_backend: Literal['py','cy'] | None = None,
    ):
        # Determine evaluator
        if eval_fn is None:
            eng = engine or self.DefaultEngine
            if eng == 'vm':
                backend = vm_backend or self.DefaultVM
                if backend == 'cy':
                    from zeta.compiler.bytecode_compiler import eval_via_vm_backend
                    eval_fn = lambda expr, env, macros: eval_via_vm_backend(expr, env, macros, 'cy')
                else:
                    from zeta.compiler.bytecode_compiler import eval_via_vm
                    eval_fn = eval_via_vm
            else:
                from zeta.evaluation.evaluator import evaluate
                eval_fn = evaluate
        self.eval_fn = eval_fn
        self.env: Environment = Environment()
        register(self.env)

        self.macros: MacroEnvironment = MacroEnvironment()
        register_macros(self.macros)

        if prelude is None:
            pass  # explicit: no prelude
        elif prelude == 'auto':
            try:
                # Lazy import to avoid circular imports
                from zeta.modules.package_loader import load_prelude
                load_prelude(self)
            except FileNotFoundError:
                # Be permissive: no prelude found -> proceed
                pass
        elif prelude:
            self.eval_prelude(prelude)

    def eval_prelude(self, code: str) -> None:
        tokens = lex(code)
        stream = TokenStream(iter(tokens))
        while (expr := stream.parse_expr()) is not None:
            self.eval_fn(expr, self.env, self.macros)

    def eval(self, code: str) -> LispValue:
        tokens = lex(code)
        stream = TokenStream(iter(tokens))
        results: list[LispValue] = []
        while (expr := stream.parse_expr()) is not None:
            results.append(self.eval_fn(expr, self.env, self.macros))
        if not results:
            return Nil
        if len(results) == 1:
            return results[0]
        return results
