import pytest

# This test configuration runs every test three times:
# 1) with the classic evaluator backend (EvalBackend) ["eval"]
# 2) with the bytecode VM using the pure-Python VM implementation ["vm_py"]
# 3) with the bytecode VM using the Cython VM implementation (if available) ["vm_cy"]
# Most existing tests instantiate Interpreter() directly. We use an autouse
# fixture to switch the default backend to VM for the VM-parameterized runs
# without changing individual test files, and to toggle ZETA_CY_VM accordingly.


@pytest.fixture(params=["eval", "vm_py", "vm_cy"]) 
def backend_mode(request):
    return request.param


@pytest.fixture(autouse=True)
def _force_interpreter_backend(backend_mode, monkeypatch):
    if backend_mode in ("vm_py", "vm_cy"):
        # Ensure the VM module sees the desired ZETA_CY_VM setting at import time
        cy_flag = "0" if backend_mode == "vm_py" else "1"
        monkeypatch.setenv("ZETA_CY_VM", cy_flag)

        # Reload VM and backend modules to apply env-switch within the same process
        import importlib, sys
        # Reload zeta.compiler.vm first so it (re)binds VM/run_chunk based on env
        if "zeta.compiler.vm" in sys.modules:
            importlib.reload(sys.modules["zeta.compiler.vm"])  # type: ignore
        else:
            import zeta.compiler.vm  # noqa: F401
        # Now reload backend_impl so its `from .vm import run_chunk` picks up new binding
        if "zeta.compiler.backend_impl" in sys.modules:
            importlib.reload(sys.modules["zeta.compiler.backend_impl"])  # type: ignore
        else:
            import zeta.compiler.backend_impl  # noqa: F401

        # For this run, make Interpreter() default to BytecodeBackend unless a
        # backend is explicitly provided by the test.
        from zeta.interpreter import Interpreter
        from zeta.compiler.backend_impl import BytecodeBackend

        original_init = Interpreter.__init__

        def patched_init(self, backend=None, prelude='auto'):
            if backend is None:
                backend = BytecodeBackend()
            # Call the original __init__ with possibly substituted backend
            return original_init(self, backend, prelude)

        monkeypatch.setattr(Interpreter, "__init__", patched_init, raising=False)
