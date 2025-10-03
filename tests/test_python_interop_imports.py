import os
import math
from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol


def _mk_interp():
    # Ensure helpers (like np_helpers) are discoverable for the NumPy test
    proj_root = os.path.dirname(os.path.dirname(__file__))
    helpers_dir = os.path.join(proj_root, "ext")
    # Prepend path so import_helpers_module can find modules
    os.environ["ZETA_PACKAGE_PATH"] = helpers_dir
    return Interpreter(prelude=None)


def test_numpy_import_with_helpers_dot_to_list():
    itp = _mk_interp()

    code = """
    (progn
      (import "numpy" as "np" helpers "np_helpers")
      (np:to_list (np:dot (np:array (1 2)) (np:array (3 4)))))
    """
    result = itp.eval(code)
    # Result should be a Python list with a single numeric element 11 (np.int64 compatible)
    assert isinstance(result, list)
    assert len(result) == 1
    # Cast to int to be agnostic to numpy scalar type
    assert int(result[0]) == 11


def test_pandas_dataframe_basic_head_and_sum():
    itp = _mk_interp()

    code = """
    (progn
      (import "pandas" as "pd")
      (define data (list (list 1 2) (list 3 4)))
      (define df   (pd:DataFrame data))
      (define h    (df:head 1))
      (define sums (df:sum))
      (sums:to_dict))
    """
    result = itp.eval(code)
    # sums is a pandas Series converted to dict, something like {0: 4, 1: 6}
    assert isinstance(result, dict)
    # Keys may be ints (column indices)
    # Verify aggregate values conservatively
    vals = sorted(int(v) for v in result.values())
    assert vals == [4, 6]


def test_networkx_graph_build_and_query():
    itp = _mk_interp()

    code = """
    (progn
      (import "networkx" as "nx")
      (define g (nx:Graph))
      ;; add a small chain A-B-C
      (g:add_edge "A" "B")
      (g:add_edge "B" "C")
      ;; query basic properties
      (list (g:number_of_nodes) (g:number_of_edges) (nx:shortest_path_length g "A" "C")))
    """
    result = itp.eval(code)
    assert isinstance(result, list)
    assert len(result) == 3
    n_nodes, n_edges, sp_len = result
    assert int(n_nodes) == 3
    assert int(n_edges) == 2
    assert int(sp_len) == 2

import os
import math
from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol


def _mk_interp():
    # Ensure helpers (like np_helpers) are discoverable for the NumPy test
    proj_root = os.path.dirname(os.path.dirname(__file__))
    helpers_dir = os.path.join(proj_root, "ext")
    # Prepend path so import_helpers_module can find modules
    os.environ["ZETA_PACKAGE_PATH"] = helpers_dir
    return Interpreter(prelude=None)


def test_numpy_import_with_helpers_dot_to_list():
    itp = _mk_interp()

    code = """
    (progn
      (import "numpy" as "np" helpers "np_helpers")
      (np:to_list (np:dot (np:array (1 2)) (np:array (3 4)))))
    """
    result = itp.eval(code)
    # Result should be a Python list with a single numeric element 11 (np.int64 compatible)
    assert isinstance(result, list)
    assert len(result) == 1
    # Cast to int to be agnostic to numpy scalar type
    assert int(result[0]) == 11


def test_pandas_dataframe_basic_head_and_sum():
    itp = _mk_interp()

    code = """
    (progn
      (import "pandas" as "pd")
      (define data (list (list 1 2) (list 3 4)))
      (define df   (pd:DataFrame data))
      (define h    (df:head 1))
      (define sums (df:sum))
      (sums:to_dict))
    """
    result = itp.eval(code)
    # sums is a pandas Series converted to dict, something like {0: 4, 1: 6}
    assert isinstance(result, dict)
    # Keys may be ints (column indices)
    # Verify aggregate values conservatively
    vals = sorted(int(v) for v in result.values())
    assert vals == [4, 6]


def test_networkx_graph_build_and_query():
    itp = _mk_interp()

    code = """
    (progn
      (import "networkx" as "nx")
      (define g (nx:Graph))
      ;; add a small chain A-B-C
      (g:add_edge "A" "B")
      (g:add_edge "B" "C")
      ;; query basic properties
      (list (g:number_of_nodes) (g:number_of_edges) (nx:shortest_path_length g "A" "C")))
    """
    result = itp.eval(code)
    assert isinstance(result, list)
    assert len(result) == 3
    n_nodes, n_edges, sp_len = result
    assert int(n_nodes) == 3
    assert int(n_edges) == 2
    assert int(sp_len) == 2


def test_pandas_optional_lambda_and_method_args():
    """
    Demonstrate:
    - Optional parameters in a Lisp lambda via &optional (with a default value)
    - Calling a pandas DataFrame method that accepts optional arguments, providing them positionally
    """
    itp = _mk_interp()

    code = """
    (progn
      (import "pandas" as "pd")
      ;; Build a simple DataFrame without requiring any helpers
      (define data (list (list 1 2) (list 3 4)))
      (define df   (pd:DataFrame data))

      ;; Lambda with &optional default: (head-n df) -> df:head 1 by default
      (define head-n
        (lambda (df &optional (n 1))
          (df:head n)))

      ;; Lambda with optional axis parameter; when omitted, uses pandas' default
      (define df-sum
        (lambda (df &optional axis)
          (cond
            ((null? axis) (df:sum))
            (else (df:sum axis)))))

      ;; Use the lambdas and collect results
      (define h1 (head-n df))          ;; default n=1
      (define h2 (head-n df 2))        ;; explicit n=2

      (define s_default_series (df-sum df))  ;; default axis (columns)
      (define s_default (s_default_series:to_dict))
      (define s_axis1_series (df-sum df 1))  ;; sum across rows
      (define s_axis1 (s_axis1_series:to_list))

      (define r1_series (h1:sum 1))  ;; head(1) row sums -> [3]
      (define r1 (r1_series:to_list))
      (define r2_series (h2:sum 1))  ;; head(2) row sums -> [3, 7]
      (define r2 (r2_series:to_list))

      (list s_default s_axis1 r1 r2))
    """

    result = itp.eval(code)
    assert isinstance(result, list)
    assert len(result) == 4

    s_default, s_axis1, r1, r2 = result

    # s_default is a pandas Series converted to dict: column sums
    assert isinstance(s_default, dict)
    vals = sorted(int(v) for v in s_default.values())
    assert vals == [4, 6]

    # Row sums with axis=1
    assert list(map(int, s_axis1)) == [3, 7]

    # head-n lambda optional param default vs explicit
    assert list(map(int, r1)) == [3]
    assert list(map(int, r2)) == [3, 7]



def test_pandas_condition_case_fallback():
    """Illustrate error handling with condition-case when a pandas call fails."""
    itp = _mk_interp()

    code = """
    (progn
      (import "pandas" as "pd")
      ;; Attempt to read a non-existent CSV file; on error, fall back to empty DataFrame
      (define df
        (condition-case
          (pd:read_csv "C:/path/that/does/not/exist__zeta_demo.csv")
          (error e (pd:DataFrame ()))))

      ;; Summing an empty DataFrame yields an empty Series; convert to dict
      (define sums (df:sum))
      (sums:to_dict))
    """

    result = itp.eval(code)
    assert isinstance(result, dict)
    # Expect an empty dict as we summed an empty DataFrame
    assert result == {}
