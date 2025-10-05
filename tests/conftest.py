import pytest

# This test configuration runs every test twice: once with the classic evaluator
# backend (EvalBackend) and once with the bytecode VM backend (BytecodeBackend).
# Most existing tests instantiate Interpreter() directly. We use an autouse
# fixture to switch the default backend to VM for the "vm" parameterized run
# without changing individual test files.


@pytest.fixture(params=["eval", "vm"])
def backend_mode(request):
    return request.param


@pytest.fixture(autouse=True)
def _force_interpreter_backend(backend_mode, monkeypatch):
    if backend_mode == "vm":
        # For this run, make Interpreter() default to BytecodeBackend unless a
        # backend is explicitly provided by the test.
        from zeta.interpreter import Interpreter
        from zeta.compiler.backend_impl import BytecodeBackend

        original_init = Interpreter.__init__

        def patched_init(self, backend=None, prelude=None):
            if backend is None:
                backend = BytecodeBackend()
            # Call the original __init__ with possibly substituted backend
            return original_init(self, backend, prelude)

        monkeypatch.setattr(Interpreter, "__init__", patched_init, raising=False)
