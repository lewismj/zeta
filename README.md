## Zeta

### Summary

Zeta is a small, pragmatic Lisp interpreter implemented in Python, designed for metaprogramming and seamless Python interoperability.

- Import and call Python directly from Lisp using the `import` special form and qualified symbols like `np:dot` and `df:sum`.
- Write macros with quasiquote/unquote, define first-class lambdas with positional, `&rest`, and Common Lisp-style `&key` named parameters.
- Tail-call optimization (TCO) via a trampoline evaluator, partial application for simple lambdas, and a growing set of special forms and builtins.

### Notes
- This is a work in progress, alpha version. Test cases for language features included, but not exhaustive.

### Immediate TODO:

- [ ] Provide a Prelude.
- [ ] Provide a REPL, and LSP support for integration with editors.
- [ ] Experiment with Python module interop (very basic at the moment).
- [ ] Add Sphinx-based docs
- [ ] Add more macros
- [ ] Add more language features

### Use Cases & Future Work

- Metaprogramming, implementation of Do-Calculus is particularly interesting.
- Optimization.
  - Currently experimenting with Rust implementation, allows implementation of
  full Nan-Boxed, own paged heap memory management and garbage collection.

#### Python interop at a glance
```lisp
(progn
  (import "numpy" as "np" helpers "np_helpers")
  (np:to_list (np:dot (np:array (1 2)) (np:array (3 4)))))
;; => [np.int64(11)]
```

```lisp
(progn
  (import "pandas" as "pd")
  (define df
    (catch 'any
      (pd:read_csv "C://tmp//data.csv")
      (pd:DataFrame ())))          ;; fallback on error
  (df:sum))                        ;; call a Python method through a qualified symbol
```

### Features

- Macro system
  - `defmacro` with lambda-like parameters
  - Quasiquote with `unquote` and `unquote-splicing`
  - Recursive, head-position macro expansion with hygienic-leaning substitution and gensym
- Lambdas and application
  - First-class `lambda` with positional parameters, `&rest`, and `&key` (named parameters like `:y 42`)
  - Multiple body forms implicitly wrapped in `progn`; empty body returns `nil`
  - Partial application for simple positional lambdas; `(apply ...)` enforces full application
- Tail-call optimization (TCO)
  - Trampoline-based evaluator returns `TailCall` in tail position to avoid Python recursion limits
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

### Quick start

- Add Zeta to your Python project (as a package or module path) and spin up the interpreter:

```python
from zeta.interpreter import Interpreter

interp = Interpreter()
print(interp.eval('(progn (define inc (lambda (x) (+ x 1))) (inc 41))'))
# => 42
```

Or evaluate a block with Python interop:

```python
from zeta.interpreter import Interpreter

code = """
(progn
  (import "numpy" as "np")
  (np:to_list (np:array (1 2 3))))
"""
print(Interpreter().eval(code))
# => [np.int64(1), np.int64(2), np.int64(3)]
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

#### Python interop in detail

- Import module and call functions or methods via qualified symbols:

```lisp
(import "numpy" as "np")
(np:sum (np:array (1 2 3)))
```

- Use package aliases to traverse attributes: `os:path.join` or `pd:DataFrame`.
- Work with returned Python objects naturally; call methods the same way: `df:sum`.

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

### Implementation notes

- Evaluator
  - Macro expansion before ordinary application, with guarded expansion inside special forms
  - Tail-position produces `TailCall` consumed by a trampoline loop
- Application engine
  - Unified implementation for lambda/callable application, partials, and `&key`/`&rest`
- Reader/parser
  - Tokenizer and parser support Common Lisp-inspired literals, vectors, dotted lists, and reader macros
