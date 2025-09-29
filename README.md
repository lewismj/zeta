<img src="docs/resources/greek_lc_zeta.svg.png" width="77" style="vertical-align: middle;">
<span style="font-size: 22px; font-weight: bold; vertical-align: middle;">Zeta</span>

### Summary

**Zeta** is a Lisp interpreter written in Python. 

Primarily, it enables Python modules within a Lisp environment. Or, embedding Lisp within Python code.  

Why? Use of Lisp (particularly **_metaprogramming_**) techniques whilst leveraging the Python ecosystem of Numpy, DoWhy, etc.

The interpreter can import modules into first-class Lisp values. Rather than have some external 'exec' mechanism that passes a string containing Python code to an externally running interpreter.

Example, calling numpy module level functions:

```lisp
             (progn
                (import "numpy" as "np" helpers "np_helpers")
                (np:to_list (np:dot (np:array (1 2)) (np:array (3 4)))))

[np.int64(11)], type:<class 'list'>
```

Importing Pandas, and invoking data frame functions:

```lisp
        ;; CSV file is:
        ;; col_a,col_b,col_c
        ;; 1,2,3
        ;; 4,5,6

        (progn
          ;; Import Pandas
          (import "pandas" as "pd")

          ;; Try reading CSV, fallback to empty DataFrame on error
          (define df
            (catch 'any ;; ensure we have an exception handler.
              (pd:read_csv "C://tmp//data.csv")
              (pd:DataFrame ())));; optional fallback value on exception.

          ;; Sum columns
          (define col-sum (df:sum))

          ;; Return the sum
          col-sum
        )


 col_a     5
 col_b    7
 col_c    9
dtype: int64, type:<class 'pandas.core.series.Series'>

 0    4
 1    6
dtype: int64, type:<class 'pandas.core.series.Series'>            
```

- Core functionality implemented lambda functions, partial application, quoting, quasi-quoting, macro expansion, structures etc.

```lisp
(defmacro inc (x) `(+ ,x 1))
(defmacro unless (cond body) `(if (not ,cond) ,body))
```

Have built-in functions and macros, e.g.

```
(let ((var1 val1) (var2 val2) ...) body...) 
    => ((lambda (var1 var2 ...) body...) val1 val2 ...)
```

```lisp
(defun fib-iter (n)
  (let ((a 0) (b 1) (temp 0))
    (dotimes (i n a)
      (set temp b)
      (set b (+ a b))
      (set a temp))))
```

Structures:

```lisp
(defstruct person name age)
(define p (make-person "Fred" 30))
(person-name p)
(person-age p)
```

Standard functionality, Lambda, partial function application, etc.,

```lisp
(define my_add (lambda (a b) (+ a b)))
(define my_add_5 (my_add 5)) ;; partially apply my_add
(defun bar (n) ;; Lambda in a macro defn.
    (let ((f (lambda (y x) (+ 1 x))))
    (f n n)))
...
...
```

See 'playground' examples *import_examply.py* and *prelude_check.py*
Can embed the interpreter within Python code and retrieve the results into Python objects, as required.

### Use Cases,

#### Term ReWriting,

TBD

#### Neural Networks & Causal Factor Analysis,

TBD

### Prolog Query Engine

TBD

### Next Steps

- #### Functional
  
  - Formalize a Lisp _prelude_, see *prelude_check.py* as a simple example, primarily adding in list processing functions. 
    For example, we can define as part of a prelude:
    
    ```lisp
    (defun fold (f z xs)
        (if (== xs Nil)
            z
            (foldl f (f z (head xs)) (tail xs))
        ))
    
    (defun map (f xs)
        (if (== xs Nil)
            Nil
            (join (list (f (head xs))) (map f (tail xs)))
        ))
    ```
  
  - Mathematical functions can be put in the prelude, though importing Python modules allows us to expose standard math functions.

- Provide example extensions for **SciPy**, **PyTorch**, **DoWhy**, and other libraries.

- Implement use cases (as above).

- #### Technical
  
  - Provide **LSP plugin**, for integration with VSCode.
  - Provide a **_native_** interpreter, use a language such as Rust.
  - Longer term, consider a different evaluator back-end. The current system
    does not implement its own Lisp cell representation, Heap, GC, etc. It relies
    upon the Python interpreter.  Python allows easy integration of modules, such as numpy, so applications can be prototyped.
  
  However, the underlying interpreter could be re-written, provided we can
  easily interact with ```PyObject```.
  
  A full implementation could have its own Heap, GC, etc.

```rust
pub enum Tag {
    Int32 = 0,
    Bool = 1, // Will drop as we will use Nil and #t.
    String = 2,
    Symbol = 3,   
    HeapObject = 4,
    PyObject = 5,
    Nan = 6,
    Nil = 7,  
}

pub struct NaNBoxed {
    /// The underlying 64-bit representation (tag + payload).
    pub bits: u64,
}

pub trait Encode {
    fn encode(&self) -> NaNBoxed;
}

pub trait Decode: Sized {
    fn decode(nb: NaNBoxed) -> Option<Self>;
}


pub enum HeapObject {
    Int64(i64),
    UInt64(u64),
    Builtin(BuiltinFn),
    Cons(Cons)
}
```

We can write native Lisp objects into a managed Heap:

```rust
pub struct Cons {
    pub car: LispVal,
    pub cdr: LispVal,
}

impl Cons {
    pub fn new(car: LispVal, cdr: LispVal) -> Result<LispVal, EtaError> {
        let cons = Cons { car, cdr };
        let mut heap = GLOBAL_HEAP.lock().unwrap();
        let payload = heap.insert(HeapObject::Cons(cons))?;
        Ok(LispVal::new(Tag::HeapObject, payload))
    }
```

 Integration and support of `PyObject` would be necessary to allow the import of Python modules, which is a primary goal.