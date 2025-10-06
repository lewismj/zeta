from timeit import timeit

from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol
from zeta.types.environment import Environment
from zeta.types.lambda_fn import Lambda
from zeta.types.nil import Nil

# Helpers to parse once, and to measure interpreter vs VM separately
from zeta.reader.parser import lex, TokenStream
from zeta.evaluation.evaluator import evaluate0
from zeta.compiler.compiler import compile_module
from zeta.compiler.vm import run_chunk


def _parse_one(code: str):
    tokens = lex(code)
    stream = TokenStream(iter(tokens))
    expr = stream.parse_expr()
    return expr


def time_interpreter(code: str, rounds: int) -> float:
    """Time the classic evaluator (no compilation phase). Parses once and
    repeatedly calls backend.eval on the same AST.
    """
    itp = Interpreter(prelude=None)  # defaults to EvalBackend
    expr = _parse_one(code)
    # Warmup
    itp.backend.eval(expr, itp.env, itp.macros)
    # Timed
    return timeit(lambda: itp.backend.eval(expr, itp.env, itp.macros), number=rounds)


def time_vm(code: str, rounds: int) -> float:
    """Time the VM execution only: parse, macroexpand, and compile once; then
    repeatedly run the precompiled chunk. This excludes compilation time.
    """
    # Reuse Interpreter just to get a populated env and macro env
    itp = Interpreter(prelude=None)
    env = itp.env
    macros = itp.macros
    expr = _parse_one(code)
    expanded = macros.macro_expand_all(expr, evaluate0, env)
    chunk = compile_module(expanded)
    # Warmup once
    run_chunk(chunk, env, macros)
    # Timed: only VM run
    return timeit(lambda: run_chunk(chunk, env, macros), number=rounds)


# Existing micro-benchmark: environment lookup chain (does not involve VM)

def bench_lookup_chain(n_envs: int = 1000, n_lookups: int = 10000) -> float:
    # Build an environment chain with a binding at the root
    root = Environment()
    key = Symbol("answer")
    root.define(key, 42)
    env = root
    for _ in range(n_envs):
        env = Environment(outer=env)
    # Warmup
    for _ in range(1000):
        env.lookup(key)
    # Timed
    t = timeit(lambda: env.lookup(key), number=n_lookups)
    return t


# A few Lisp-level benchmarks to compare Interpreter vs VM (run-time only)

LAMBDA_APPLY_CODE = "((lambda (x y) (+ x y)) 1 2)"

TAIL_RECURSION_CODE = r"""
(progn
  (defun fact (n acc)
    (if (<= n 1)
        acc
        (fact (- n 1) (* n acc))))
  (fact 100 1))
"""

# Sum 1..N using tail recursion (no dotimes dependency)
ARITH_SUM_CODE = r"""
(progn
  (defun sum-n (n acc)
    (if (<= n 0)
        acc
        (sum-n (- n 1) (+ acc n))))
  (sum-n 500 0))
"""

# Python interop microbenchmark: math.sqrt in a tight recursive loop
MATH_SQRT_CODE = r"""
(progn
  (import "math" as "m")
  (defun sqrt-acc (n acc)
    (if (<= n 0)
        acc
        (sqrt-acc (- n 1) (+ acc (m:sqrt n)))))
  (sqrt-acc 200 0))
"""


def _print_pair(name: str, code: str, rounds: int) -> None:
    tint = time_interpreter(code, rounds)
    tvm = time_vm(code, rounds)
    print(f"Benchmark: {name}")
    print(f"  interpreter: {tint:.6f}s  |  vm (exec only): {tvm:.6f}s  [rounds={rounds}]")


if __name__ == "__main__":
    # Pure environment benchmark
    print("Benchmark: environment lookup chain (pure Python env lookup)")
    print(f"  time: {bench_lookup_chain():.6f}s")

    # Compare interpreter vs VM (excluding compilation) on several workloads
    _print_pair("lambda application", LAMBDA_APPLY_CODE, rounds=20000)
    _print_pair("tail recursion (factorial)", TAIL_RECURSION_CODE, rounds=500)
    _print_pair("arithmetic sum 1..500 (tail-rec)", ARITH_SUM_CODE, rounds=1000)
    _print_pair("python interop: math.sqrt loop", MATH_SQRT_CODE, rounds=200)
