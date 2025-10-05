from zeta.interpreter import Interpreter


def eval_(interp: Interpreter, code: str):
    return interp.eval(code)


def test_basic_arithmetic_vm():
    itp = Interpreter()
    # Uses env builtins registered by default Interpreter
    assert eval_(itp, "(+ 1 2 3)") == 6
    assert eval_(itp, "(- 10 3)") == 7
    assert eval_(itp, "(* 2 3 4)") == 24
    assert eval_(itp, "(mod 10 3)") == 1


def test_quote_and_literals_vm():
    itp = Interpreter()
    assert eval_(itp, "'42") == 42
    assert eval_(itp, '"hello"') == "hello"
    assert eval_(itp, "(quote (1 2 3))") == [1, 2, 3]


def test_define_and_lambda_call_vm():
    itp = Interpreter()
    eval_(itp, "(define add1 (lambda (x) (+ x 1)))")
    assert eval_(itp, "(add1 41)") == 42


def test_if_and_progn_vm():
    itp = Interpreter()
    # Sequencing should return the last value
    assert eval_(itp, "(progn (define a 10) (define b 20) (+ a b))") == 30
    # If true branch
    assert eval_(itp, "(if (< 1 2) 1 2)") == 1
    # If false branch
    assert eval_(itp, "(if (> 1 2) 1 2)") == 2


def test_set_bang_vm_updates_global():
    itp = Interpreter()
    eval_(itp, "(define g 10)")
    assert eval_(itp, "g") == 10
    assert eval_(itp, "(set! g 99)") == 99
    assert eval_(itp, "g") == 99


def test_recursive_factorial_vm():
    itp = Interpreter()
    code = """
    (define fact
      (lambda (n)
        (if (<= n 1)
            1
            (* n (fact (- n 1))))))
    (fact 5)
    """
    assert eval_(itp, code)[1] == 120
