import time
from timeit import timeit

from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol
from zeta.types.environment import Environment
from zeta.types.lambda_fn import Lambda
from zeta.types.nil import Nil


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


def bench_apply_lambda(n_calls: int = 10000) -> float:
    # Simple lambda (lambda (x y) (+ x y)) simulated with Python body returning list
    # Here we just build a lambda that returns its argument to avoid arithmetic costs
    body = [Symbol("+"), Symbol("x"), Symbol("y")]
    fn = Lambda([Symbol("x"), Symbol("y")], body)
    itp = Interpreter(prelude=None)
    # Warmup a bit
    for _ in range(100):
        itp.eval("((lambda (x y) (+ x y)) 1 2)")
    # Timed
    code = "((lambda (x y) (+ x y)) 1 2)"
    t = timeit(lambda: itp.eval(code), number=n_calls)
    return t


def bench_tail_recursion(depth: int = 1000, rounds: int = 100) -> float:
    itp = Interpreter(prelude=None)
    fact_code = """
    (progn
      (defun fact (n acc)
        (if (<= n 1)
            acc
            (fact (- n 1) (* n acc))))
      (fact 100 1))
    """
    # warmup
    itp.eval(fact_code)
    # timed
    t = timeit(lambda: itp.eval(fact_code), number=rounds)
    return t


if __name__ == "__main__":
    print("Benchmark: environment lookup chain")
    print(bench_lookup_chain())
    print("Benchmark: lambda application")
    print(bench_apply_lambda())
    print("Benchmark: tail recursion")
    print(bench_tail_recursion())
