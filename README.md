## Zeta

### Summary

Zeta is a small, pragmatic Lisp system implemented in Python. 
Designed for metaprogramming and seamless Python interoperability.

 The development is still in progress:

 - Initially implemented a simple 'tree walking' evaluator: [evaluator](zeta/evaluation/evaluator.py).
 - Next a bytecode compiler: [compiler](zeta/compiler/compiler.py) and VM: [vm](zeta/compiler/vm.py).
 - A very basic optimizer was implemented: [optimizer](zeta/compiler/optimizer.py).
 - The original VM was implemented in Python. There is now a Cython-based VM for better performance: [cvm](zeta/compiler/vm_cy.pyx).
 - Tests are in [tests](tests), and run all 3 configurations (interpreter, VM, _Cythonized_ VM).
 - Module system is in place, requires documentation.

### Use Cases, Why Zeta?

- Leverage Python’s ecosystem (NumPy, Pandas, SciPy, ML/AI libraries) while using Lisp.

- Metaprogramming
    - Macros for code generation, DSLs, and syntactic abstraction.
    Examples,
    - [Term rewriting](docs/TermRewrite.md)
    - [Boolean Simplifier](docs/BooleanSimplifier.md)
    - [Lambda Calculus Beta Reducer](docs/LCBetaReduction.md)
    - Do-Calculus
  
- Data science and numerical computing
  - Leverage Python libraries (NumPy, Pandas, SciPy, ML/AI libraries) with Lisp syntax and macros.

### Immediate TODO:

In the core semantics Zeta is a Lisp, It’s not a full Common Lisp or full Scheme (by intent), 
but the interpreter now follows canonical Lisp rules in macroexpansion, binding, and evaluation. 

- [ ] Provide an extended Prelude, minimal at present.
- [ ] Provide a REPL, and LSP support for integration with editors.
- [ ] Implement cyclic check/depth limit in the recursive macro expander.
- [ ] Refactor required;
  - There are common functions are in the evaluation module that can be extracted to a common module.
  - No real requirement for the interpreter/compiler to have the same 'eval' signature.
  - The VM uses the Macro expansion via evaluator (gets the prelude macros), this is messy and can be removed.
  - Need some sort of 'embed' API where switch between interpreter, VM (Python, native) is a parameter switch.
- 

### Benchmark Comparison Interpreter vs. (Cythonized) VM.

Some simple benchmarks comparing the interpreter and the VM execution only time.
Note, these benchmarks were generated using the Cython-based VM.


```aiignore
Benchmark: environment lookup chain (pure Python env lookup)
  time: 0.001427s
Benchmark: lambda application
  interpreter: 0.461862s  |  vm (exec only): 0.170856s  [rounds=20000]
Benchmark: tail recursion (factorial)
  interpreter: 1.685704s  |  vm (exec only): 0.305634s  [rounds=500]
Benchmark: arithmetic sum 1..500 (tail-rec)
  interpreter: 16.677846s  |  vm (exec only): 2.963271s  [rounds=1000]
Benchmark: python interop: math.sqrt loop
  interpreter: 1.641686s  |  vm (exec only): 0.354214s  [rounds=200]
```
| Benchmark                                         | Interpreter Time | VM Time (exec only) | Speedup (interpreter ÷ VM) |
| ------------------------------------------------- | ---------------- | ------------------- | -------------------------- |
| Environment lookup chain (pure Python env lookup) | 0.001427 s       | —                   | —                          |
| Lambda application                                | 0.461862 s       | 0.170856 s          | 2.70×                      |
| Tail recursion (factorial)                        | 1.685704 s       | 0.305634 s          | 5.52×                      |
| Arithmetic sum 1..500 (tail-rec)                  | 16.677846 s      | 2.963271 s          | 5.63×                      |
| Python interop: math.sqrt loop                    | 1.641686 s       | 0.354214 s          | 4.63×                      |
| **Geometric mean**                                | —                | —                   | **4.49× faster**           |



#### Python interop at a glance


## Standalone mode

```lisp
    (progn
      (import "networkx" as "nx") ;; import a Python module.
      (define g (nx:Graph))
      (g:add_edge "A" "B")       ;; add a small chain A-B-C
      (g:add_edge "B" "C")
      ;; query basic properties
      (list (g:number_of_nodes) (g:number_of_edges) (nx:shortest_path_length g "A" "C"))) 
```

## Embedding into Python

Note, you don't have to use the interpreter when embedding Zeta into Python,
you can embed the VM (native or '_Cythonized_') directly. Use the interpreter
if you want to debug into it, as it may be easier than debugging the bytecode VM.
Use the VM for better performance.


```python
def _mk_interp():
    from zeta.interpreter import Interpreter  
    return Interpreter(prelude=None) # Not needed for dummy example.

interpreter = _mk_interp()
res = interpreter.eval('''
    (progn
      (import "numpy" as "np" helpers "np_helpers")
      (np:to_list (np:dot (np:array (1 2)) (np:array (3 4)))))
''')  
```
Python objects are treated as Lisp values, so you can pass them around and use them in Lisp code.
No type conversion is required in-out of the Lisp interpreter.

```
Result:
 col_a     5
 col_b    7
 col_c    9
dtype: int64, type:<class 'pandas.core.series.Series'>
```


### Language tour

#### Macros

```lisp
(defmacro unless (cond body)
  (` (if (, cond) nil (, body))))

(unless (= 1 2) 42)   ;; => 42
```

#### Lambdas: positional, `&rest`, and `&key`

```lisp
(define f (lambda (x &key y) (list x y)))
(f 10 :y 7)          ;; => (10 7)

(define g (lambda (a &rest rest) rest))
(g 1 2 3 4)          ;; => (2 3 4)
```

Partial application for simple positional lambdas:

```lisp
(define add2 (lambda (a b) (+ a b)))
(define inc (add2 1))   ;; returns a new lambda awaiting b
(inc 41)                ;; => 42
```

`apply` enforces full application and accepts a list of arguments:

```lisp
(define add2 (lambda (a b) (+ a b)))
(apply add2 (list 10 20))   ;; => 30
```

#### Tail-call optimization

```lisp
(define fact
  (lambda (n acc)
    (if (= n 0)
        acc
        (fact (- n 1) (* acc n)))))   ;; proper tail recursion via trampoline

   (fact 1500 1) ;; tail recursive native Python would fail with maximum recursion depth error.
                 ;; Python based interpreter relies on Python numeric tower though.
```

#### Continuations (call/cc)

Zeta includes Scheme-style escape continuations via the special form `call/cc` (call-with-current-continuation).
The continuation captured by `call/cc` is single-shot and delimited to the dynamic extent of the call. Invoking
it performs a non-local exit that returns its value as the value of the `call/cc` expression.

```lisp
;; Early exit
(call/cc (lambda (k)
  (k 42)   ;; escape immediately, returning 42 from call/cc
  99))     ;; never reached
;; => 42

;; Embedded in an expression: the value escapes back to the call site
(+ 1 (call/cc (lambda (k) (k 10))))  ;; => 11

;; Break out of nested calls
(progn
  (define out
    (call/cc (lambda (escape)
      ((lambda ()
         ((lambda ()
            (escape "stopped-from-deep-inside"))  ;; non-local exit
          ))
       )
      "unreached"))))
  out)
;; => "stopped-from-deep-inside"
```

Notes:
- The provided continuation is represented as a function you can call with zero or one argument; zero defaults to `Nil`.
- Invoking the continuation outside the dynamic extent of the original `call/cc` is not supported (single-shot semantics).


### Error handling

- `condition-case` to catch and handle errors:

```lisp
(condition-case
  (+ 1 "a")
  (error 99))     ;; => 99
```

- `catch`/`throw` for non-local exits:

```lisp
(catch 'any (throw 'any 42))   ;; => 42
```

### Structs

Define simple structures with constructors and accessors:

```lisp
(defstruct point x y)
(define p (make-point 10 20))
(point-x p)  ;; => 10
(point-y p)  ;; => 20
```

### Pandas Example

1. Capture Exceptions in the Lisp code itself.

```lisp
    (progn
      (import "pandas" as "pd")
      ;; Attempt to read a non-existent CSV file; on error, fall back to empty DataFrame
      (define df
        (condition-case
          (pd:read_csv "C:/path/that/does/not/exist__zeta_demo.csv")
          (error e (pd:DataFrame ())))) ;; can also use catch-throw standard Common Lisp.

      ;; Summing an empty DataFrame yields an empty Series; convert to dict
      (define sums (df:sum))
      (sums:to_dict))
```
2. Lambdas with optional and named parameters.

```lisp
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
```

### Implementation notes

- Evaluator
  - Macro expansion before ordinary application, with guarded expansion inside special forms
  - Tail-position produces `TailCall` consumed by a trampoline loop
  - Implementation for lambda/callable application, partials, and `&key`/`&rest`
- Reader/parser
  - Tokenizer and parser support Common Lisp-inspired literals, vectors, dotted lists, and reader macros



