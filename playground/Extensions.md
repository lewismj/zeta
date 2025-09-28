## Extensions

Importing python packages can be done with the `import` keyword.
Example,
```aiignore
        (progn
            (import "numpy" as "np" helpers "np_helpers")
            (np:to_list (np:dot (np:array (1 2)) (np:array (3 4))))
        )
```
The 'helpers' are utility functions you can supply so that you may convert any
package specific format to lisp lists. That is, you'd like to pass a numpy
array to a lisp function, you'd write a helper function to convert the numpy
array to a lisp list.

These aren't required and are just a convenience. That is, if you just want to
use the numpy package, you can invoke the import statement without any helpers,
but all your 'objects' are numpy objects that ancillary Lisp functions may not
understand.
