import pytest

from zeta.interpreter import Interpreter


@pytest.fixture(params=["interp", "vm_py", "vm_cy"], scope="session", autouse=True)
def engine_mode(request):
    """
    Autouse, session-scoped parametrization that runs the entire test suite
    three times under different execution engines without monkeypatching env:
      - interp: classic interpreter
      - vm_py: Python VM backend (pure Python)
      - vm_cy: Cythonized VM backend (if available)
    """
    mode = request.param

    # Determine availability of cy backend once
    cy_available = False
    try:
        from zeta.compiler.vm_cy import VM as _  # noqa: F401
        cy_available = True
    except Exception:
        cy_available = False

    if mode == "vm_cy" and not cy_available:
        pytest.skip("Cython VM backend not available")

    if mode == "interp":
        Interpreter.DefaultEngine = 'interp'
        Interpreter.DefaultVM = 'py' # ignored if default engine is interp.
    elif mode == "vm_py":
        Interpreter.DefaultEngine = 'vm'
        Interpreter.DefaultVM = 'py'
    elif mode == "vm_cy":
        Interpreter.DefaultEngine = 'vm'
        Interpreter.DefaultVM = 'cy'
    else:
        raise RuntimeError(f"Unknown engine mode: {mode}")

    yield
