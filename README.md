## Zeta

### Summary

Zeta is a small, pragmatic Lisp interpreter implemented in Python, designed for metaprogramming and seamless Python interoperability.
Work in progress.

### Use Cases 

- Metaprogramming
    - Macros for code generation, DSLs, and syntactic abstraction.
    Examples,
    - [Term rewriting](docs/TermRewrite.md)
    - [Boolean Simplifier](docs/BooleanSimplifier.md)
    - [Lambda Calculus Beta Reducer](docs/LCBetaReduction.md)
    - Do-Calculus
  
- Data science and numerical computing
  - Leverage Python libraries (NumPy, Pandas, SciPy, ML/AI libraries) with Lisp syntax and macros.
    
- Distributed computing, using the Lisp's ability to treat 'code as data' to serialize 
  and send code to remote workers for execution. On-the-fly updates to code and data.
- 
#### Python interop at a glance

 *Run in standalone mode or embed Lisp interpreter into Python.*

## Standalone mode

```lisp
    (progn
      (import "networkx" as "nx")
      (define g (nx:Graph))
      (g:add_edge "A" "B")       ;; add a small chain A-B-C
      (g:add_edge "B" "C")
      ;; query basic properties
      (list (g:number_of_nodes) (g:number_of_edges) (nx:shortest_path_length g "A" "C"))) 
```

## Embedding into Python

```python
def _mk_interp():
    from zeta import Interpreter
    return Interpreter(prelude=...)

interpreter = _mk_interp()
res = interpreter.eval('''
    (progn
      (import "numpy" as "np" helpers "np_helpers")
      (np:to_list (np:dot (np:array (1 2)) (np:array (3 4)))))
''')  # => 3
```
Note, the interpreter will treat classes as structs and methods as function,
you can access free functions and class methods. Python objects are treated
as Lisp values, so you can pass them around and use them in Lisp code,
no type conversion is required in-out of the Lisp interpreter.

```
Result:
 col_a     5
 col_b    7
 col_c    9
dtype: int64, type:<class 'pandas.core.series.Series'>
```

### Future Work

- REPL & LSP integration.
- Enhance Python Interop 
  - Fairly seamless, but need to consider `&optional` lambda parameters and converting
  named parameters to kwargs etc.
- Use Python for sockets, message passing, etc. A core idea is the ability to send code to remote workers for execution.
- 
- Optimization.
  - I'm currently experimenting with a Rust implementation. Would not rely on Python
  GC, but have its own NanBox/HeapObject, Numeric tower of types, its own paged Heap and GC.


### Immediate TODO:

- [ ] Provide an extended Prelude, minimal at present.
- [ ] Provide a REPL, and LSP support for integration with editors.
- [ ] Experiment with Python module interop, named and optional parameters, etc.
- [ ] Add Sphinx-based docs
- [ ] Add more macros (into the prelude).
- [ ] Over time add more Common Lisp language features.




### Features


- Lambdas and application
  - First-class `lambda` with positional parameters, `&rest`, and `&key` (named parameters like `:y 42`)
  - Multiple body forms implicitly wrapped in `progn`; empty body returns `nil`
  - Partial application for simple positional lambdas; `(apply ...)` enforces full application
- Macro system
  - `defmacro` implemented.
  - Quasiquote with `unquote` and `unquote-splicing`
  - Recursive, head-position macro expansion with hygienic-leaning substitution and gensym.
- Tail-call optimization (TCO)
  - Trampoline-based evaluator returns `TailCall` in tail position to avoid Python recursion limits
- Continuations (call/cc)
  - Scheme-style escape continuations via the special form `call/cc` (call-with-current-continuation)
  - Continuations are single-shot and delimited to the dynamic extent of the call
  - Invoking the continuation outside the dynamic extent of the original `call/cc` is not supported (single-shot semantics)
- Python interoperability
  - `(import "module" as "alias" [helpers "helper_mod"])` binds modules into Zeta
  - Qualified symbol resolution like `np:array`, `os:path.join`, or package aliases
  - Calls into Python functions/methods and receives Python objects or native values
- Reader and parser capabilities
  - Symbols, strings, integers, floats, radix numbers (`#b`, `#o`, `#x`), complex (`#C`), bitstrings (`#*`)
  - Lists and dotted lists, vectors `#(...)`, character literals `#\space`, read-time eval `#.`
  - Function shorthand `#'sym` → `("function" sym)`
- Core special forms and builtins
  - `define`, `set`, `if`, `progn`, `quote`, `quasiquote`, `unquote`, `unquote-splicing`
  - Loops: `do`, `dotimes`, `dolist`
  - Error handling: `condition-case`, `catch`, `throw`
  - Structures: `defstruct` generates constructors and accessors
  - Lists and predicates: `cons`, `car`, `cdr`, `list`, `nil?`, `symbol?`, `atom`, `join`, etc.
- Environments and packages
  - Nested lexical environments
  - Package tables and aliases enable `pkg:symbol` lookups

### Why Zeta?

- Leverage Python’s rich ecosystem (NumPy, Pandas, SciPy, ML/AI libraries) while writing expressive Lisp code and macros.
- Keep a compact, understandable interpreter for experimentation and language design.
- Mix metaprogramming and data work without bridging through ad-hoc `exec` strings.

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

(fact 5 1)  ;; => 120
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
          (error e (pd:DataFrame ())))) ;; can also use throw-catch standard Common Lisp.

      ;; Summing an empty DataFrame yields an empty Series; convert to dict
      (define sums (df:sum))
      (sums:to_dict))
```
2. Lambdas with optional and named parameters.

```
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
