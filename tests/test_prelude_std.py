import pytest
from pathlib import Path

from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil


@pytest.fixture(scope="module")
def interp():
    # Load std and test prelude files
    root = Path(__file__).resolve().parents[1]
    std = (root / "prelude" / "std.lisp").read_text(encoding="utf-8")
    tst = (root / "prelude" / "test.lisp").read_text(encoding="utf-8")

    itp = Interpreter()
    itp.eval_prelude(std)
    itp.eval_prelude(tst)
    return itp


def eval_lisp(interp: Interpreter, code: str):
    return interp.eval(code)


def test_len_take_drop(interp):
    itp = interp
    assert eval_lisp(itp, "(len (1 2 3 4))") == 4
    assert eval_lisp(itp, "(len Nil)") == 0
    assert eval_lisp(itp, "(take 3 (range 10))") == [0, 1, 2]
    assert eval_lisp(itp, "(drop 2 (1 2 3 4 5))") == [3, 4, 5]


def test_map_filter_reverse_append(interp):
    itp = interp
    assert eval_lisp(itp, "(map (lambda (x) (* x 2)) (1 2 3))") == [2, 4, 6]
    assert eval_lisp(itp, "(filter (lambda (x) (> x 0)) (-2 -1 0 1 2 3))") == [1, 2, 3]
    assert eval_lisp(itp, "(reverse (1 2 3 4))") == [4, 3, 2, 1]
    # append should concatenate variadic lists, skipping Nil
    assert eval_lisp(itp, "(append (1 2) (3) Nil (4 5))") == [1, 2, 3, 4, 5]


def test_sum_product_range(interp):
    itp = interp
    assert eval_lisp(itp, "(sum (1 2 3 4 5))") == 15
    assert eval_lisp(itp, "(product (1 2 3 4))") == 24
    assert eval_lisp(itp, "(range 5)") == [0, 1, 2, 3, 4]
    assert eval_lisp(itp, "(range 2 6)") == [2, 3, 4, 5]


def test_any_all(interp):
    itp = interp
    # any even?
    assert eval_lisp(itp, "(any (lambda (x) (> x 5)) (1 3 5 6 7))") == Symbol("#t")
    # all positive?
    assert eval_lisp(itp, "(all (lambda (x) (> x 0)) (1 2 3))") == Symbol("#t")
    assert eval_lisp(itp, "(all (lambda (x) (> x 0)) (-1 2 3))") == Symbol("#f")


def test_function_utilities_and_sort(interp):
    itp = interp
    # compose: f(x) = (x + 1), g(x) = (2x); (f ∘ g)(3) = 7
    assert eval_lisp(itp, "((compose (lambda (x) (+ x 1)) (lambda (y) (* 2 y))) 3)") == 7

    # curry2: fix first arg of + to 2
    assert eval_lisp(itp, "((curry2 + 2) 5)") == 7

    # sort
    assert eval_lisp(itp, "(sort (3 1 4 1 5 9 2))") == [1, 1, 2, 3, 4, 5, 9]


def test_convenience_preds(interp):
    itp = interp
    assert eval_lisp(itp, "(is_empty Nil)") == Symbol("#t")
    assert eval_lisp(itp, "(is_empty (1))") == Symbol("#f")
    assert eval_lisp(itp, "(is_empty1 Nil)") == Symbol("#t")
    assert eval_lisp(itp, "(is_empty1 (1 2))") == Symbol("#f")


def test_macros_when_unless_defalias(interp):
    itp = interp
    # when: executes body and returns last value
    assert eval_lisp(itp, "(when #t (define a 1) 42)") == 42
    # unless: when condition false -> executes body
    assert eval_lisp(itp, "(unless #f (define b 2) (+ b 1))") == 3
    # defalias: create alias for function
    eval_lisp(itp, "(defun id (x) x)")
    eval_lisp(itp, "(defalias ident id)")
    assert eval_lisp(itp, "(ident 5)") == 5


def test_testing_macros_present_and_working(interp):
    itp = interp
    # assert= should evaluate and return #t on success
    assert eval_lisp(itp, "(assert= 6 (+ 1 2 3))") == Symbol("#t")
    # test macro prints and returns #t on success; we just check truthy value
    assert eval_lisp(itp, "(test \"adds\" 10 (+ 5 5))") == Symbol("#t")
