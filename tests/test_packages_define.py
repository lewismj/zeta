from zeta.interpreter import Interpreter


def test_define_into_new_package_creates_package_and_binds():
    itp = Interpreter(prelude=None)
    # Define into a non-existent package 'util'
    result = itp.eval("""
    (progn
      (define util:answer 42)
      util:answer)
    """)
    assert result == 42


def test_define_into_existing_imported_package_uses_that_package():
    itp = Interpreter(prelude=None)
    code = """
    (progn
      (import "networkx" as "nx")
      (define nx:test_const 7)
      nx:test_const)
    """
    result = itp.eval(code)
    assert result == 7


def test_define_lambda_in_package_and_call():
    itp = Interpreter(prelude=None)
    code = """
    (progn
      (define mathp:inc (lambda (x) (+ x 1)))
      (mathp:inc 41))
    """
    result = itp.eval(code)
    assert result == 42
